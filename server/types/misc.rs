use serde::{Serialize, Deserialize, Serializer, Deserializer};
use std::collections::HashMap;
use std::ffi::OsString;
use std::fmt;
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::sync::{Arc, OnceLock};
use tantivy::Index;
use tantivy::schema::Field;
use time::OffsetDateTime;

use crate::consts::{DEFAULT_INITIAL_NODE_LIMIT, DEFAULT_PORT};

//
// Type Definitions
//

/// MSV = 'Maybe-Specified Vector'.
/// When the user saves a buffer, this type distinguishes
/// "not specified" (so use whatever is already on disk) from
/// "should be this value" (even if empty).
///
/// ELABORATION:
/// When a user saves an Occurrence, its claiming Occurrence children
/// always define its contents, so MSV does not apply there.
/// But other fields -- e.g. aliases --
/// the user is likely not to mention (in particular,
/// not asking for any changes). This type distinguishes the case
/// where the user did not mention the field from the case
/// where the user wants it empty.
///
/// The folder rule these values encode at save extraction
/// ('server/from_text/local_fieldintent_collection/traverse.rs'
/// enforces it; 'supplement_unspecified_fields_from_disk' consumes
/// it): a buffer with no folder for a field (no aliasFolder, subscribeeFolder
/// or overriddenFolder under the defining node) emits no intent for that
/// field, which lowers to 'Unspecified' -- no opinion, so the value
/// is filled from disk. A present-but-empty folder emits an empty list,
/// which lowers to 'Specified(vec![])' -- the user explicitly wants
/// the field empty.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum MSV<T> {
  Unspecified,
  Specified (Vec<T>),
}

/// Each node has a random ID. Skg does not check for collisions.
/// So far the IDs are Version 4 UUIDs.
/// .
/// Collisions are astronomically unlikely but not impossible.
/// If UUIDs were perfectly uniformly distributed,
/// and there are fewer than 3.3e15 v4 UUIDs --
/// equivalent to 9 billion people with 415,000 nodes each --
/// then the collision probability is less than 1 in 1e6.
/// (In reality 6 bits of a v4 UUIDs are fixed:
/// 4 for the version, and 2 for the variant indicator.
/// So there's a little less headroom, but still enough.)
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub struct ID ( pub String );

/// Comparison identity for a stored structured-relationship member.
/// Resolvable IDs compare by their canonical PID (so primary and extra IDs
/// remain one node); an unresolved raw ID compares byte-for-byte.  This key is
/// never serialization data: callers keep the original `RelPartner.member`
/// when a relationship is retained or rewritten.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum RelationshipMemberKey {
  ResolvedPid (ID),
  UnresolvedRawId (ID),
}

impl RelationshipMemberKey {
  /// The ID either variant carries.
  pub fn id (
    &self,
  ) -> &ID {
    match self {
      RelationshipMemberKey::ResolvedPid (id)
        | RelationshipMemberKey::UnresolvedRawId (id) => id, } }

  pub fn into_id (
    self,
  ) -> ID {
    match self {
      RelationshipMemberKey::ResolvedPid (id)
        | RelationshipMemberKey::UnresolvedRawId (id) => id, } }

  pub fn is_unresolved (
    &self,
  ) -> bool {
    matches! ( self, RelationshipMemberKey::UnresolvedRawId (_) ) }
}

#[derive(Serialize, Clone, PartialEq, Eq, Hash)]
pub struct Skgrepo {
  pub name         : SkgrepoName,
  pub abbreviation : Option<String>,
  pub path         : PathBuf,
  // True unless the TOML entry says 'foreign = true'. No folder
  // name bears on it. Only an owned skgrepo has a rung in the
  // privacy order; the foreign ones are an unordered set. Rust
  // constructors (tests) set it directly.
  pub owned : bool,
}

/// The TOML shape of a '[[repos]]' entry: 'name' is optional,
/// defaulting to the path as written (e.g. "eggman/eggs"), which the
/// herald-label defaulting then abbreviates for owned skgrepos.
/// 'foreign' is optional too, defaulting to false: owned.
#[derive(Deserialize)]
struct SkgrepoToml {
  name         : Option<SkgrepoName>,
  #[serde(default)]
  abbreviation : Option<String>,
  path         : PathBuf,
  #[serde(default)]
  foreign      : bool,
}

impl From<SkgrepoToml> for Skgrepo {
  fn from (
    raw : SkgrepoToml
  ) -> Skgrepo {
    let name : SkgrepoName =
      raw . name . unwrap_or_else (
        || SkgrepoName (
          raw . path . to_string_lossy () . into_owned () ));
    Skgrepo {
      name,
      abbreviation : raw . abbreviation,
      path         : raw . path,
      owned        : ! raw . foreign,
    }}}

/// One entry in a node's relationship list: the member and the
/// relationship instance's relRepo (the skgrepo whose telescope
/// section records the relationship). The relRepo is about the
/// relationship, not the member node (a public node can be the member of a
/// private membership). Its default comes from the applicable relRepo rule;
/// 'skg-set-relRepo' may move its privacy anywhere at least as
/// private as that default;
/// renormalization never lowers its privacy (the sticky rule). See
/// 'docs/telescopes.org', "Where new relationships land".
///
/// Every value records the relRepo whose section contains it.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct RelPartner<T> {
  pub relRepo   : SkgrepoName,
  pub member    : T,
}

impl<T> RelPartner<T> {
  pub fn at_relRepo (
    relRepo   : SkgrepoName,
    member    : T,
  ) -> RelPartner<T> {
    RelPartner { relRepo, member }}}

/// Tag every member of a list with one relRepo.
pub fn rel_partners_at_relRepo<T> (
  relRepo   : &SkgrepoName,
  members   : Vec<T>,
) -> Vec<RelPartner<T>> {
  members . into_iter ()
    . map ( |m| RelPartner::at_relRepo (
      relRepo . clone (), m ) )
    . collect () }

/// The values in a list of relation partners, skgrepos dropped.
pub fn members_of<T : Clone> (
  list : &[RelPartner<T>],
) -> Vec<T> {
  list . iter ()
    . map ( |m| m . member . clone () )
    . collect () }

/// 'rel_partners_at_relRepo' lifted over MSV.
pub fn rel_partners_at_relRepo_msv<T> (
  relRepo   : &SkgrepoName,
  msv       : MSV<T>,
) -> MSV<RelPartner<T>> {
  match msv {
    MSV::Unspecified     => MSV::Unspecified,
    MSV::Specified (v)   =>
      MSV::Specified ( rel_partners_at_relRepo (relRepo, v) ), }}

/// 'members_of' lifted over MSV.
pub fn members_msv<T : Clone> (
  msv : &MSV<RelPartner<T>>,
) -> MSV<T> {
  match msv {
    MSV::Unspecified     => MSV::Unspecified,
    MSV::Specified (v)   =>
      MSV::Specified ( members_of (v) ), }}

impl Skgrepo {
  pub fn herald_label (&self) -> &str {
    self . abbreviation . as_deref ()
      . unwrap_or ( &self . name ) }}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
pub struct SkgConfig {
  #[serde (skip)]
  pub config_path    : PathBuf, // Path to skgconfig.toml. Used to reload config without changing which file governs this server.

  #[serde (skip)]
  pub data_root      : PathBuf, // Directory containing skgconfig.toml. Other relative paths (tantivy_folder, skgrepo paths) are resolved against this at load time.

  #[serde ( rename = "repos", deserialize_with = "deserialize_skgrepos" )]
  pub skgrepos        : HashMap<SkgrepoName, Skgrepo>,

  // The skgrepo names in TOML declaration order ('repos' is a HashMap,
  // which loses it). Filled at parse time by the config loaders; empty
  // for dummy/test configs, where the config-order helpers fall back to
  // alphabetical. LOAD-BEARING: the owned entries' declaration order
  // is the privacy order (most public first); the foreign entries
  // have none, and may sit anywhere. The composition, the relRepo
  // defaults, the validators, and the skgrepo restriction all read it
  // through the methods below ('ordered_skgrepos', 'privacy_ladder',
  // 'skgrepo_position', 'is_strictly_more_public', 'more_private_of',
  // 'privacy_ladder_split_after'). No other code may compare skgrepo
  // positions.
  #[serde (skip)]
  pub skgrepo_order   : Vec<SkgrepoName>,

  // What each foreign skgrepo's OTHER_SKGREPO_DEPENDENCIES.toml says
  // of the order of this config's foreign skgrepos, and the fold order
  // of the foreign skgrepos that follows from it. Both are set by
  // 'set_foreign_manifest_orders', which the config loaders call once
  // paths are final; empty for dummy/test configs. They order only
  // the folding of a node no owned skgrepo has a section of.
  #[serde (skip)]
  pub foreign_manifest_orders : Vec<ForeignManifestOrder>,
  #[serde (skip)]
  pub foreign_fold_order      : Vec<SkgrepoName>,
  // Each foreign skgrepo that a foreign manifest chains to another,
  // mapped to the first in declaration order of the skgrepos so
  // chained: one owner's ladder ('skgrepo_owner'). Set with the two
  // fields above.
  #[serde (skip)]
  pub foreign_owner_groups    : HashMap<SkgrepoName, SkgrepoName>,

  // The skgrepo restriction a connection starts with: the most
  // private owned skgrepo it shows ('SkgrepoRestriction'), foreign
  // skgrepos shown. Absent, the most private owned skgrepo: no
  // skgrepo restriction. It must name an owned skgrepo.
  #[serde(rename = "default_repo_set", default)]
  pub default_skgrepo_set : Option<SkgrepoName>,

  pub tantivy_folder : PathBuf,

  #[serde(default = "default_port")]
  pub port           : u16,  // TCP port for Rust-Emacs comms.

  #[serde(default = "default_initial_node_limit")]
  pub initial_node_limit : usize, // Max nodes to render in initial content views.

  #[serde (default)] // defaults to false
  pub timing_log     : bool, // Write a JSON log, server.jsonl, beside server-to-user.log in the run subfolder ('logs_dir').

  #[serde(default = "default_beep_when_server_becomes_available")]
  pub beep_when_server_becomes_available : bool, // Play a local sound when server initialization finishes.

  #[serde(default = "default_max_roleTree_depth")]
  pub max_roleTree_depth : usize, // Max BFS depth for full containerward role tree.

  #[serde (default)]
  pub readiness_marks_file : Option<PathBuf>, // The readiness store's file, as the config states it; see 'readiness_marks_path'.

  #[serde(default = "default_repo_watch_quiet_ms")]
  pub repo_watch_quiet_ms : u64, // How long the skgrepo directories must be quiet before the files changed in them are reloaded; see 'server/repo_watch/watcher.rs'.
}

/// The foreign skgrepos of this config that one foreign skgrepo's
/// OTHER_SKGREPO_DEPENDENCIES.toml lists, in the manifest's order
/// (most public first). The manifest names its publisher's own
/// ladder, so the order is that publisher's privacy order.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ForeignManifestOrder {
  pub manifest_skgrepo : SkgrepoName,
  pub listed           : Vec<SkgrepoName>,
}

/// What the foreign manifests say of the relative order of two
/// foreign skgrepos.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ForeignPairOrder {
  /// At least one manifest lists both, and every one that does puts
  /// 'first' first.
  Agreed { first : SkgrepoName },
  /// No manifest lists both.
  Unlisted,
  /// Manifests that list both disagree: 'first_by' puts 'a' first,
  /// 'second_by' puts 'b' first. 'first' is the one folded first:
  /// as the manifest of the more private of the two says, the one
  /// whose own manifest lists the other; else ('a' and 'b' each
  /// list the other, or neither's manifest is among the disputants)
  /// the one declared first.
  Disputed { first_by  : SkgrepoName,
             second_by : SkgrepoName,
             first     : SkgrepoName },
}

/// Whose a skgrepo is: the user's (owned), or one foreign owner's,
/// named by the first in declaration order of that owner's skgrepos
/// that this config lists ('SkgConfig::skgrepo_owner').
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SkgrepoOwner {
  User,
  Foreign (SkgrepoName),
}

/// START: when this server process began, UTC, as
/// 'YYYY-MM-DDTHH-MM-SSZ' (hyphens only, so a legal file name on
/// every filesystem). Set at its first read and the same at every
/// later one, so that a config reload cannot move the run subfolder
/// ('SkgConfig::logs_dir') the log appenders already write to.
static SERVER_PROCESS_START : OnceLock<String> = OnceLock::new ();

pub fn server_process_start_as_file_name (
) -> &'static str {
  SERVER_PROCESS_START . get_or_init ( || {
    let now : OffsetDateTime = OffsetDateTime::now_utc ();
    format! ( "{:04}-{:02}-{:02}T{:02}-{:02}-{:02}Z",
              now . year (), u8::from (now . month ()), now . day (),
              now . hour (), now . minute (), now . second () ) } ) }

/// The run folder 'CONFIG.running/', CONFIG the config file's path as
/// given to the server, so a symlinked config gets its folder beside
/// the symlink, where the start script and the clients look. Set once,
/// in 'main.rs', before the config loads; never set by tests or
/// 'check-config'. Like START, it cannot move during a process.
static RUN_FOLDER : OnceLock<PathBuf> = OnceLock::new ();

/// Fix this process's run folder ('RUN_FOLDER'). A second call
/// changes nothing.
pub fn set_run_folder_from_config_path_as_given (
  config_path_as_given : &Path,
) {
  let _already_set : Result<(), PathBuf> =
    RUN_FOLDER . set ( run_folder_beside (config_path_as_given) ); }

/// 'PATH.running', a sibling of the config at PATH: '.running'
/// appended to the file name as given. No canonicalization, so for a
/// symlink it is beside the symlink, not beside its target.
pub fn run_folder_beside (
  config_path_as_given : &Path,
) -> PathBuf {
  let mut path_text : OsString =
    config_path_as_given . as_os_str () . to_os_string ();
  path_text . push (".running");
  PathBuf::from (path_text) }

impl SkgConfig {
  /// Where this server process writes its logs: 'CONFIG.running/START/',
  /// CONFIG the config file's path as given to the server
  /// ('RUN_FOLDER'), so a symlinked config gets its folder beside the
  /// symlink, where the start script and the clients look, and START
  /// this process's start ('server_process_start_as_file_name'). The
  /// run folder 'CONFIG.running/' is the one the start script keeps
  /// its stop script and script logs in; each server run gets its own
  /// subfolder there. Where 'main.rs' set no run folder (tests),
  /// CONFIG is 'config_path', which is canonical.
  pub fn logs_dir ( &self ) -> PathBuf {
    let run_folder : PathBuf =
      RUN_FOLDER . get () . cloned ()
      . unwrap_or_else ( || run_folder_beside (&self . config_path) );
    run_folder . join ( server_process_start_as_file_name () ) }

  /// The readiness store's file: 'readiness_marks_file' resolved
  /// against the data root, or else 'readiness-marks-STEM.json' in the
  /// data root, STEM the config file's name without its extension, so
  /// that servers over one data root keep separate marks.
  /// 'config_path' is canonical, so a symlink to a config file shares
  /// that file's marks. A config read from no file (a test's) uses the
  /// stem of the default config file, 'skgconfig'.
  pub fn readiness_marks_path (
    &self,
  ) -> PathBuf {
    match &self . readiness_marks_file {
      Some (stated) => self . data_root . join (stated),
      None => {
        let stem : String =
          self . config_path . file_stem ()
          . map ( |stem| stem . to_string_lossy () . to_string () )
          . unwrap_or_else ( || "skgconfig" . to_string () );
        self . data_root . join (
          format! ("readiness-marks-{}.json", stem) ) }, }} }

/// Each skgrepo has a unique name, defined in the SkgConfig,
/// used in Viewnode metadata to track provenance.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct SkgrepoName ( pub String );

impl SkgrepoName {
  /// Reserved sentinel skgrepo for an occurrence whose skgrepo
  /// nothing supplies: the parse gives it to a headline that names
  /// none where 'complete_occurrence_skgrepo' finds none either (a new
  /// headline at top level, say), and validation refuses it
  /// ('viewnode_from_metadata', 'find_buffer_errors_for_saving').
  pub const NOT_FOUND_STR : &'static str = "NOT_FOUND";
  pub fn not_found () -> Self {
    SkgrepoName ( Self::NOT_FOUND_STR . to_string () ) } }

/// A change of skgrepo, from 'old' to 'new': of a node's home, or of
/// a relationship's relRepo.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SkgrepoChange {
  pub old : SkgrepoName,
  pub new : SkgrepoName,
}

#[derive (Clone)]
pub struct TantivyIndex {
  // Associates titles and aliases to paths.
  pub index                     : Arc<Index>,
  /// Long-lived 'IndexReader' shared across all callers. Default
  /// 'ReloadPolicy::OnCommitWithDelay' refreshes automatically after
  /// each tantivy-commit, so this stays current without manual
  /// invalidation. Re-creating an 'IndexReader' on every lookup
  /// (which 'index.reader()' does) is the dominant cost of
  /// per-query Tantivy ID lookups, so callers should prefer this
  /// field over '.index.reader()'.
  pub reader                    : tantivy::IndexReader,
  pub id_field                  : Field,
  pub title_or_alias_field      : Field,
  pub raw_title_field           : Field, // Un-reduced title for is_title=true docs: preserves link syntax (e.g. "[[id:X][label]]") that 'title_or_alias_field' strips to bare labels. Populated only on primary-title docs; empty for alias docs. Used by the 'titles by ids' endpoint so clients can tell link-titles from plain titles.
  pub overPrivateText_telescope_field      : Field,
  pub no_search_matching_field  : Field,
  pub skgrepo_field             : Field,
  pub prominence_source_field : Field,
  pub is_title_field            : Field,
  pub had_id_field              : Field,
  pub body_field                : Field,
}

//
// Helper Functions
//

fn deserialize_skgrepos<'de, D> (
  deserializer : D
) -> Result <HashMap<SkgrepoName, Skgrepo>, D::Error>
where
  D : Deserializer<'de>
{
  let skgrepos_vec : Vec<SkgrepoToml> =
    Vec::deserialize (deserializer) ?;
  let mut map : HashMap<SkgrepoName, Skgrepo> =
    HashMap::new ();
  for raw in skgrepos_vec {
    let skgrepo : Skgrepo =
      Skgrepo::from (raw);
    if map . insert (
         skgrepo . name . clone (),
         skgrepo . clone () ) . is_some () {
      return Err (serde::de::Error::custom (
        format! ("Duplicate repo name '{}'", skgrepo . name))); }}
  Ok (map)
}

fn default_port() -> u16 {
  DEFAULT_PORT }

fn default_initial_node_limit() -> usize {
  DEFAULT_INITIAL_NODE_LIMIT }

fn default_beep_when_server_becomes_available() -> bool {
  true }

fn default_max_roleTree_depth() -> usize {
  20 }

pub fn default_repo_watch_quiet_ms () -> u64 {
  500 }


//
// Implementations
//

impl<T> MSV<T> {
  pub fn is_unspecified (&self) -> bool {
    matches! (self, MSV::Unspecified) }
  /// Returns the inner slice, or empty if Unspecified.
  pub fn or_default (&self) -> &[T] {
    match self {
      MSV::Unspecified  => &[],
      MSV::Specified (v) => v } }
  /// Consumes self, returning the inner Vec or empty.
  pub fn into_vec (self) -> Vec<T> {
    match self {
      MSV::Unspecified  => vec![],
      MSV::Specified (v) => v } }
  /// Ensures the value is Specified, defaulting to empty,
  /// and returns a mutable reference to the inner Vec.
  pub fn ensure_specified (&mut self) -> &mut Vec<T> {
    if matches! (self, MSV::Unspecified) {
      *self = MSV::Specified (Vec::new ()); }
    match self {
      MSV::Specified (v) => v,
      MSV::Unspecified => unreachable! () } }
  /// For serde skip_serializing_if.
  /// Skips both Unspecified and Specified([]).
  pub fn skip_serializing (&self) -> bool {
    match self {
      MSV::Unspecified  => true,
      MSV::Specified (v) => v . is_empty () } } }

impl<T> Default for MSV<T> {
  fn default () -> Self {
    MSV::Unspecified } }

impl<T: Serialize> Serialize for MSV<T> {
  fn serialize<S: Serializer> (
    &self, serializer : S
  ) -> Result<S::Ok, S::Error> {
    match self {
      MSV::Unspecified  =>
        serializer . serialize_none (),
      MSV::Specified (v) =>
        v . serialize (serializer) } } }

impl<'de, T: Deserialize<'de>> Deserialize<'de> for MSV<T> {
  fn deserialize<D: Deserializer<'de>> (
    deserializer : D
  ) -> Result<Self, D::Error> {
    Vec::<T>::deserialize (deserializer)
      . map (MSV::Specified) } }

impl ID {
  pub fn new <S : Into<String>> (s: S) -> Self {
    ID ( s . into () ) }}

impl Deref for ID {
  // lets ID be used like a String in (more?) cases
  type Target = String;
  fn deref (&self) -> &Self::Target {
    &self . 0 }}

impl AsRef<str> for ID {
  fn as_ref (&self) -> &str {
    &self . 0 }}

impl Deref for SkgrepoName {
  // lets RepoName be used like a String
  type Target = String;
  fn deref (&self) -> &Self::Target {
    &self . 0 }}

impl fmt::Display for ID {
  fn fmt ( &self,
            f: &mut fmt::Formatter<'_> )
         -> fmt::Result {
    write! ( f, "{}", self . 0 ) }}

impl fmt::Display for SkgrepoName {
  fn fmt ( &self,
            f: &mut fmt::Formatter<'_> )
         -> fmt::Result {
    write! ( f, "{}", self . 0 ) }}

impl From<String> for ID {
  fn from ( s : String ) -> Self {
    ID (s) }}

impl From<&String> for ID {
  fn from ( s : &String ) -> Self {
    ID ( s . clone () ) }}

impl From<String> for SkgrepoName {
  fn from ( s : String ) -> Self {
    SkgrepoName (s) }}

impl From<&String> for SkgrepoName {
  fn from ( s : &String ) -> Self {
    SkgrepoName ( s . clone () ) }}

impl From <&str> for ID {
  fn from(s: &str) -> Self {
    ID ( s . to_string () ) }}

impl From <&str> for SkgrepoName {
  fn from(s: &str) -> Self {
    SkgrepoName ( s . to_string () ) }}

impl SkgConfig {
  /// Creates a SkgConfig with dummy values for everything except skgrepos.
  /// Useful for tests that only need to read .skg files.
  pub fn dummyFromSkgrepos (
    skgrepos : HashMap<SkgrepoName, Skgrepo>
  ) -> Self {
    SkgConfig {
      config_path        : PathBuf::from (""),
      data_root          : PathBuf::from ("."),
      skgrepos,
      skgrepo_order       : Vec::new (),
      foreign_manifest_orders : Vec::new (),
      foreign_fold_order      : Vec::new (),
      foreign_owner_groups    : HashMap::new (),
      default_skgrepo_set : None,
      tantivy_folder     : PathBuf::from ("/tmp/unused"),
      port               : 0,
      initial_node_limit : DEFAULT_INITIAL_NODE_LIMIT,
      timing_log         : false,
      beep_when_server_becomes_available : false,
      max_roleTree_depth : default_max_roleTree_depth(),
      readiness_marks_file : None,
      repo_watch_quiet_ms : default_repo_watch_quiet_ms (), }}

  /// Creates a SkgConfig with a test-specific Tantivy folder.
  pub fn fromSkgreposAndTantivyFolder (
    skgrepos       : HashMap<SkgrepoName, Skgrepo>,
    tantivy_folder : &str,
  ) -> Self {
    SkgConfig {
      config_path        : PathBuf::from (""),
      data_root          : PathBuf::from ("."),
      skgrepos,
      skgrepo_order       : Vec::new (),
      foreign_manifest_orders : Vec::new (),
      foreign_fold_order      : Vec::new (),
      foreign_owner_groups    : HashMap::new (),
      default_skgrepo_set : None,
      tantivy_folder     : PathBuf::from (tantivy_folder),
      port               : DEFAULT_PORT,
      initial_node_limit : DEFAULT_INITIAL_NODE_LIMIT,
      timing_log         : false,
      beep_when_server_becomes_available : false,
      max_roleTree_depth : default_max_roleTree_depth(),
      readiness_marks_file : None,
      repo_watch_quiet_ms : default_repo_watch_quiet_ms (), }}

  pub fn skgrepo_is_owned (
    &self,
    skgrepo_name : &SkgrepoName
  ) -> bool {
    self . skgrepos . get (skgrepo_name)
      . map ( |s| s . owned )
      . unwrap_or (false)
  }

  /// The owned skgrepo names in privacy order, most public first: the
  /// privacy ladder (see 'privacy_ladder').
  pub fn owned_skgrepos_in_config_order (
    &self,
  ) -> Vec<SkgrepoName> {
    self . privacy_ladder () }

  /// The default owned skgrepo for a fork's clone when its skgrepo could
  /// not be inferred or user-set: the user's config-first owned skgrepo
  /// (matching the Emacs client's 'skg--default-repo', which is
  /// TOML-first). None only when the user owns no skgrepo at all -- the
  /// one case 'ForkRepoUnresolved' still fires.
  pub fn first_owned_skgrepo_in_config_order (
    &self,
  ) -> Option<SkgrepoName> {
    self . owned_skgrepos_in_config_order () . into_iter () . next () }

  /// Backwards-compatible name for the config-first owned skgrepo
  /// default; see 'first_owned_repo_in_config_order'.
  pub fn first_owned_skgrepo (
    &self,
  ) -> Option<SkgrepoName> {
    self . first_owned_skgrepo_in_config_order () }

  /// Every skgrepo name in declaration order, or alphabetical when
  /// declaration order is unavailable (a dummy/test config, whose
  /// 'skgrepo_order' is empty), so the result is always
  /// deterministic.
  pub fn skgrepos_in_declaration_order (
    &self,
  ) -> Vec<SkgrepoName> {
    if self . skgrepo_order . is_empty () {
      // No declaration order recorded: alphabetical, for determinism.
      let mut names : Vec<SkgrepoName> =
        self . skgrepos . keys () . cloned () . collect ();
      names . sort ();
      names
    } else { self . skgrepo_order . clone () }}

  /// The comparison chokepoint, with the methods below it. Every
  /// skgrepo name, in fold order: the order in which a telescope's
  /// sections fold, and so the order to walk the skgrepos in. It is
  /// declaration order, except that the foreign entries are permuted
  /// among their own places into 'foreign_fold_order'. Its owned
  /// entries are the privacy ladder, in order; where an owned entry
  /// sits relative to a foreign one says nothing, since no telescope
  /// folds an owned section with a foreign one.
  /// No code outside these methods may compare skgrepo positions.
  pub fn ordered_skgrepos (
    &self,
  ) -> Vec<SkgrepoName> {
    let declared : Vec<SkgrepoName> =
      self . skgrepos_in_declaration_order ();
    let declared_foreign : Vec<&SkgrepoName> =
      declared . iter ()
      . filter ( |name| self . skgrepo_is_foreign (name) )
      . collect ();
    let fold_order_is_current : bool = // a test may edit the config after loading it
      self . foreign_fold_order . len () == declared_foreign . len ()
      && self . foreign_fold_order . iter ()
         . all ( |name| declared_foreign . contains (&name) );
    if ! fold_order_is_current {
      return declared; }
    let mut foreign_in_fold_order =
      self . foreign_fold_order . iter () . cloned ();
    declared . into_iter ()
      . map ( |name|
        if self . skgrepo_is_foreign (&name) {
          foreign_in_fold_order . next ()
            . expect ("the fold order permutes the foreign entries") }
        else { name } )
      . collect () }

  /// The privacy ladder: the owned skgrepos in declaration order, most
  /// public first. Foreign skgrepos have no rung.
  pub fn privacy_ladder (
    &self,
  ) -> Vec<SkgrepoName> {
    self . skgrepos_in_declaration_order () . into_iter ()
      . filter ( |name| self . skgrepo_is_owned (name) )
      . collect () }

  /// Record what the foreign manifests say ('ForeignManifestOrder'),
  /// and the fold order of the foreign skgrepos that follows.
  pub fn set_foreign_manifest_orders (
    &mut self,
    orders : Vec<ForeignManifestOrder>,
  ) {
    self . foreign_manifest_orders = orders;
    self . foreign_fold_order = self . foreign_fold_order_from_manifests ();
    self . foreign_owner_groups = self . foreign_owner_groups_from_manifests (); }

  /// Each foreign skgrepo a manifest chains to another (the manifest's
  /// own skgrepo and every one it lists, transitively), mapped to the
  /// first so chained in declaration order.
  fn foreign_owner_groups_from_manifests (
    &self,
  ) -> HashMap<SkgrepoName, SkgrepoName> {
    let declared : Vec<SkgrepoName> =
      self . skgrepos_in_declaration_order () . into_iter ()
      . filter ( |name| self . skgrepo_is_foreign (name) )
      . collect ();
    let mut group_of : HashMap<SkgrepoName, usize> = // index into 'declared'
      declared . iter () . enumerate ()
      . map ( |(i, name)| ( name . clone (), i )) . collect ();
    for manifest in &self . foreign_manifest_orders {
      let members : Vec<&SkgrepoName> =
        std::iter::once (&manifest . manifest_skgrepo)
        . chain ( manifest . listed . iter () )
        . filter ( |name| group_of . contains_key (*name) )
        . collect ();
      let Some (lowest) : Option<usize> =
        members . iter () . map ( |name| group_of [*name] ) . min ()
        else { continue; };
      let merged : Vec<usize> =
        members . iter () . map ( |name| group_of [*name] ) . collect ();
      for group in group_of . values_mut () {
        if merged . contains (group) { *group = lowest; }} }
    group_of . into_iter ()
      . map ( |(name, group)| ( name, declared [group] . clone () ))
      . collect () }

  /// Whose 'skgrepo' is: the user's when owned, else its foreign
  /// owner's ('SkgrepoOwner'). Foreign skgrepos no manifest chains
  /// together have different owners.
  pub fn skgrepo_owner (
    &self,
    skgrepo : &SkgrepoName,
  ) -> SkgrepoOwner {
    if self . skgrepo_is_owned (skgrepo) { return SkgrepoOwner::User; }
    SkgrepoOwner::Foreign (
      self . foreign_owner_groups . get (skgrepo) . cloned ()
        . unwrap_or_else ( || skgrepo . clone () )) }

  /// The foreign skgrepos in the order a foreign node's sections
  /// fold in (only one owner's fold together; 'skgrepo_owner'). A
  /// pair the foreign manifests order folds in their order
  /// ('ForeignPairOrder::Agreed'), or, when they disagree, as the
  /// more private one's manifest says ('ForeignPairOrder::Disputed');
  /// every other pair, in declaration order, so far as the ordered
  /// pairs allow: repeatedly take the first foreign skgrepo in
  /// declaration order that no remaining one must precede. Should the
  /// ordered pairs form a cycle, the first remaining one is taken
  /// regardless.
  fn foreign_fold_order_from_manifests (
    &self,
  ) -> Vec<SkgrepoName> {
    let mut remaining : Vec<SkgrepoName> =
      self . skgrepos_in_declaration_order () . into_iter ()
      . filter ( |name| self . skgrepo_is_foreign (name) )
      . collect ();
    if self . foreign_manifest_orders . is_empty () {
      return remaining; }
    let must_precede = |a : &SkgrepoName, b : &SkgrepoName| -> bool {
      match self . foreign_pair_order (a, b) {
        ForeignPairOrder::Agreed { first }
          | ForeignPairOrder::Disputed { first, .. } => &first == a,
        ForeignPairOrder::Unlisted => false, } };
    let mut ordered : Vec<SkgrepoName> = Vec::new ();
    while ! remaining . is_empty () {
      let next : usize =
        remaining . iter ()
        . position ( |candidate|
          ! remaining . iter () . any ( |other|
            other != candidate && must_precede (other, candidate) ))
        . unwrap_or (0);
      ordered . push ( remaining . remove (next) ); }
    ordered }

  /// What the foreign manifests say of the order of foreign skgrepos
  /// 'a' and 'b'.
  pub fn foreign_pair_order (
    &self,
    a : &SkgrepoName,
    b : &SkgrepoName,
  ) -> ForeignPairOrder {
    let mut a_first_by : Vec<SkgrepoName> = Vec::new ();
    let mut b_first_by : Vec<SkgrepoName> = Vec::new ();
    for manifest in &self . foreign_manifest_orders {
      let position_of = |name : &SkgrepoName| -> Option<usize> {
        manifest . listed . iter () . position ( |l| l == name ) };
      if let (Some (pa), Some (pb)) = (position_of (a), position_of (b)) {
        if pa < pb { a_first_by . push ( manifest . manifest_skgrepo . clone () ); }
        else       { b_first_by . push ( manifest . manifest_skgrepo . clone () ); }} }
    match ( a_first_by . first () . cloned (), b_first_by . first () . cloned () ) {
      (None, None) => ForeignPairOrder::Unlisted,
      (Some (_), None) => ForeignPairOrder::Agreed { first : a . clone () },
      (None, Some (_)) => ForeignPairOrder::Agreed { first : b . clone () },
      (Some (first_by), Some (second_by)) => {
        // A manifest of b putting a first says b is the more private.
        let b_says_a_first : bool = a_first_by . contains (b);
        let a_says_b_first : bool = b_first_by . contains (a);
        let first : SkgrepoName = match (b_says_a_first, a_says_b_first) {
          (true, false) => a . clone (),
          (false, true) => b . clone (),
          _ => {
            let declared : Vec<SkgrepoName> =
              self . skgrepos_in_declaration_order ();
            if declared . iter () . position ( |n| n == a )
               <= declared . iter () . position ( |n| n == b )
            { a . clone () } else { b . clone () } }, };
        ForeignPairOrder::Disputed { first_by, second_by, first } }, }}

  /// Position in the fold order ('ordered_skgrepos').
  /// None for a skgrepo absent from the config. Not a privacy rank:
  /// for that, see 'privacy_rank'.
  pub fn skgrepo_position (
    &self,
    skgrepo : &SkgrepoName,
  ) -> Option<usize> {
    self . ordered_skgrepos () . iter ()
      . position ( |s| s == skgrepo ) }

  /// Position on the privacy ladder: 0 = most public. None for a
  /// foreign skgrepo, which has no rung, and for one absent from the
  /// config.
  fn privacy_rank (
    &self,
    skgrepo : &SkgrepoName,
  ) -> Option<usize> {
    self . privacy_ladder () . iter ()
      . position ( |s| s == skgrepo ) }

  /// Whether 'skgrepo' is configured and foreign.
  fn skgrepo_is_foreign (
    &self,
    skgrepo : &SkgrepoName,
  ) -> bool {
    self . skgrepos . get (skgrepo)
      . is_some_and ( |s| ! s . owned ) }

  /// True iff 'a' is strictly more public than 'b' (earlier on the
  /// privacy ladder). A skgrepo absent from the config counts as
  /// maximally private, so nothing is less public than it. A foreign
  /// skgrepo has no rung, so it is neither more nor less public than
  /// anything: false whenever either is foreign.
  pub fn is_strictly_more_public (
    &self,
    a : &SkgrepoName,
    b : &SkgrepoName,
  ) -> bool {
    if self . skgrepo_is_foreign (a) || self . skgrepo_is_foreign (b) {
      return false; }
    match ( self . privacy_rank (a),
            self . privacy_rank (b) ) {
      (Some (pa), Some (pb)) => pa < pb,
      (Some (_),  None     ) => true,
      _                      => false, }}

  /// The more private of the two (the later on the privacy ladder);
  /// 'b' on a tie. This is the relRepo default rule's core: a
  /// relationship instance normally defaults to the skgrepo of the more private
  /// of its two endpoints' homes. A foreign skgrepo has no rung, so
  /// beside an owned one the owned one is returned: a foreign
  /// skgrepo never raises an owned floor.
  pub fn more_private_of (
    &self,
    a : SkgrepoName,
    b : SkgrepoName,
  ) -> SkgrepoName {
    match ( self . skgrepo_is_foreign (&a),
            self . skgrepo_is_foreign (&b) ) {
      (true, false) => b,
      (false, true) => a,
      _ => if self . is_strictly_more_public (&b, &a) { a } else { b }, }}

  /// The default relRepo for an editable node-to-node
  /// relationship. Between owned nodes, use the more-private home.
  /// From an owned recorder to a foreign member, use the recorder's home:
  /// Skg may expose the foreign ID there, but never proposes writing
  /// a relationship into the foreign skgrepo.
  pub fn default_relRepo (
    &self,
    recorder_home : &SkgrepoName,
    member_home   : &SkgrepoName,
  ) -> SkgrepoName {
    if ( self . skgrepo_is_owned (recorder_home) &&
         ! self . skgrepo_is_owned (member_home) ) {
      recorder_home . clone ()
    } else {
      self . more_private_of (
        recorder_home . clone (), member_home . clone () ) }}

  /// The privacy ladder split after 'skgrepo': the rungs through it,
  /// inclusive (that skgrepo and every more public owned one), and
  /// the rest (every more private owned one). Errors if the skgrepo
  /// is not an owned skgrepo of the config.
  pub fn privacy_ladder_split_after (
    &self,
    skgrepo : &SkgrepoName,
  ) -> Result<(Vec<SkgrepoName>, Vec<SkgrepoName>), String> {
    let mut ordered : Vec<SkgrepoName> =
      self . privacy_ladder ();
    let position : usize =
      ordered . iter () . position ( |s| s == skgrepo )
      . ok_or_else ( || format! (
        "Repo '{}' is not an owned repo of the config", skgrepo )) ?;
    let more_private : Vec<SkgrepoName> =
      ordered . split_off (position + 1);
    Ok (( ordered, more_private )) }
}

#[cfg(test)]
#[path = "../../tests/unit/run_folder.rs"]
mod tests;
