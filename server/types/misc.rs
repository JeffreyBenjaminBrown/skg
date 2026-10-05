use serde::{Serialize, Deserialize, Serializer, Deserializer};
use std::collections::{BTreeSet, HashMap};
use std::fmt;
use std::ops::Deref;
use std::path::PathBuf;
use std::sync::Arc;
use tantivy::Index;
use tantivy::schema::Field;

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
/// When a user saves an ActiveVognode, its non-ignored ActiveVognode children
/// always define its contents, so MSV does not apply there.
/// But other fields -- e.g. aliases --
/// the user is likely not to mention (in particular,
/// not asking for any changes). This type distinguishes the case
/// where the user did not mention the field from the case
/// where the user wants it empty.
///
/// THE FOLDER RULE these values encode at save extraction
/// ('server/from_text/local_instruction_collection/traverse.rs'
/// enforces it; 'supplement_unspecified_fields_from_disk' consumes
/// it): a buffer with NO folder for a field (no AliasFolder, SubscribeeFolder
/// or OverriddenFolder under the defining node) emits no intent for that
/// field, which lowers to 'Unspecified' -- no opinion, so the value
/// is filled from disk. A PRESENT-BUT-EMPTY folder emits an empty list,
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
/// when an edge is retained or rewritten.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum RelationshipMemberKey {
  ResolvedPid (ID),
  UnresolvedRawId (ID),
}

#[derive(Serialize, Clone, PartialEq, Eq, Hash)]
pub struct SkgfileRepo {
  pub name         : RepoName,
  pub abbreviation : Option<String>,
  pub path         : PathBuf,
  // DERIVED at config load, never written in TOML (a raw
  // 'user_owns_it' key is rejected): true iff the repo's
  // as-written path sits under the config's 'owned_folder' (its
  // first component equals it). See 'derive_ownership_and_labels'
  // in server/dbs/filesystem/not_nodes.rs. Rust constructors (tests)
  // may still set it directly.
  pub user_owns_it : bool,
}

/// The TOML shape of a '[[repos]]' entry: 'name' is optional,
/// defaulting to the path as written (e.g. "eggman/eggs"), which the
/// herald-label defaulting then abbreviates for owned repos.
#[derive(Deserialize)]
struct SkgfileRepoToml {
  name         : Option<RepoName>,
  #[serde(default)]
  abbreviation : Option<String>,
  path         : PathBuf,
}

impl From<SkgfileRepoToml> for SkgfileRepo {
  fn from (
    raw : SkgfileRepoToml
  ) -> SkgfileRepo {
    let name : RepoName =
      raw . name . unwrap_or_else (
        || RepoName (
          raw . path . to_string_lossy () . into_owned () ));
    SkgfileRepo {
      name,
      abbreviation : raw . abbreviation,
      path         : raw . path,
      user_owns_it : false, // derived later; see the field's comment
    }}}

/// One entry in a node's relationship list: the member and the
/// relationship instance's relRepo (the repo whose telescope
/// section records the relationship). The relRepo is about the
/// RELATIONSHIP, not the member node (a public node can be a private
/// member). Its default comes from the applicable relRepo rule;
/// 'skg-set-relRepo' may move its privacy anywhere at least as
/// private as that default;
/// renormalization never lowers its privacy (the sticky rule). See
/// TODO/user-owned_autofork_chain/5_plan.org and
/// BUG-and-fix_make-edge-more-public.org.
///
/// Every value records the relRepo whose section contains it.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct RelPartner<T> {
  pub relRepo : RepoName,
  pub member    : T,
}

impl<T> RelPartner<T> {
  pub fn at_relRepo (
    relRepo : RepoName,
    member    : T,
  ) -> RelPartner<T> {
    RelPartner { relRepo, member }}}

/// Tag every member of a list with one relRepo.
pub fn rel_partners_at_relRepo<T> (
  relRepo : &RepoName,
  members   : Vec<T>,
) -> Vec<RelPartner<T>> {
  members . into_iter ()
    . map ( |m| RelPartner::at_relRepo (
      relRepo . clone (), m ) )
    . collect () }

/// The values in a list of relation partners, repos dropped.
pub fn members_of<T : Clone> (
  list : &[RelPartner<T>],
) -> Vec<T> {
  list . iter ()
    . map ( |m| m . member . clone () )
    . collect () }

/// 'rel_partners_at_relRepo' lifted over MSV.
pub fn rel_partners_at_relRepo_msv<T> (
  relRepo : &RepoName,
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

/// Identifies a repo-set CHOICE. Repo-sets are no longer defined
/// by hand: they are the PREFIXES of the config's repo order (most
/// public first), so a choice is either "all" (every repo) or the
/// name of a configured repo -- meaning "that repo and everything
/// more public", i.e. the most private repo to make available.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct RepoSetName ( pub String );

impl SkgfileRepo {
  pub fn herald_label (&self) -> &str {
    self . abbreviation . as_deref ()
      . unwrap_or ( &self . name ) }}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
pub struct SkgConfig {
  #[serde (skip)]
  pub config_path    : PathBuf, // Path to skgconfig.toml. Used to reload config without changing which file governs this server.

  #[serde (skip)]
  pub data_root      : PathBuf, // Directory containing skgconfig.toml. Other relative paths (tantivy_folder, repo paths) are resolved against this at load time.

  #[serde ( deserialize_with = "deserialize_repos" )]
  pub repos        : HashMap<RepoName, SkgfileRepo>,

  // The repo names in TOML declaration order ('repos' is a HashMap,
  // which loses it). Filled at parse time by the config loaders; empty
  // for dummy/test configs, where the config-order helpers fall back to
  // alphabetical. LOAD-BEARING: declaration order is the privacy order
  // (most public first); the fold, the relRepo defaults, the
  // validators, and prefix repo-sets all read it, through the
  // comparison chokepoint methods below ('ordered_repos',
  // 'repo_position', 'is_strictly_more_public', 'more_private_of',
  // 'prefix_through'). No other code may compare repo positions.
  #[serde (skip)]
  pub repo_order   : Vec<RepoName>,

  #[serde(default = "default_repo_set_name")]
  pub default_repo_set : RepoSetName,

  // The directory (as a first path component under the data root)
  // whose repos the user OWNS; every other repo is foreign
  // (write-protected). Replaces the retired per-repo 'user_owns_it'
  // TOML key. The intended layout is data/AUTHOR/REPO, with this
  // field naming the user's own author folder.
  #[serde(default = "default_owned_folder")]
  pub owned_folder : String,

  pub tantivy_folder : PathBuf,

  #[serde(default = "default_port")]
  pub port           : u16,  // TCP port for Rust-Emacs comms.

  #[serde(default = "default_initial_node_limit")]
  pub initial_node_limit : usize, // Max nodes to render in initial content views.

  #[serde (default)] // defaults to false
  pub timing_log     : bool, // Write JSON log to <data_root>/logs/server.jsonl.

  #[serde(default = "default_beep_when_server_becomes_available")]
  pub beep_when_server_becomes_available : bool, // Play a local sound when server initialization finishes.

  #[serde(default = "default_max_role_tree_depth")]
  pub max_role_tree_depth : usize, // Max BFS depth for full containerward role tree.
}

impl SkgConfig {
  pub fn logs_dir ( &self ) -> PathBuf {
    self . data_root . join ("logs") } }

/// Each repo has a unique name, defined in the SkgConfig,
/// used in Viewnode metadata to track provenance.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct RepoName ( pub String );

impl RepoName {
  /// Reserved sentinel repo for a *non-Active* viewnode (e.g. a
  /// Diff phantom) whose repo could not be determined. It renders like
  /// any other repo -- the all-caps name alone flags it to the user --
  /// so that one unresolvable reference does not abort an entire render
  /// (TODO/DONE/local-view-update/plan_v2.org §7.6). Active-vognode repo failures are caught by
  /// validation (pre-save) or are catastrophic (post-save), never this.
  pub const NOT_FOUND_STR : &'static str = "NOT_FOUND";
  pub fn not_found () -> Self {
    RepoName ( Self::NOT_FOUND_STR . to_string () ) } }

#[derive (Clone)]
pub struct TantivyIndex {
  // Associates titles and aliases to paths.
  pub index                     : Arc<Index>,
  /// Long-lived 'IndexReader' shared across all callers. Default
  /// 'ReloadPolicy::OnCommitWithDelay' refreshes automatically after
  /// writes commit, so this stays current without manual
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
  pub repo_field              : Field,
  pub context_origin_type_field : Field,
  pub is_title_field            : Field,
  pub had_id_field              : Field,
  pub body_field                : Field,
}

//
// Helper Functions
//

fn deserialize_repos<'de, D> (
  deserializer : D
) -> Result <HashMap<RepoName, SkgfileRepo>, D::Error>
where
  D : Deserializer<'de>
{
  let repos_vec : Vec<SkgfileRepoToml> =
    Vec::deserialize (deserializer) ?;
  let mut map : HashMap<RepoName, SkgfileRepo> =
    HashMap::new ();
  for raw in repos_vec {
    let repo : SkgfileRepo =
      SkgfileRepo::from (raw);
    if map . insert (
         repo . name . clone (),
         repo . clone () ) . is_some () {
      return Err (serde::de::Error::custom (
        format! ("Duplicate repo name '{}'", repo . name))); }}
  Ok (map)
}

fn default_repo_set_name () -> RepoSetName {
  RepoSetName::from ("all") }

fn default_owned_folder () -> String {
  "owned" . to_string () }

fn default_port() -> u16 {
  DEFAULT_PORT }

fn default_initial_node_limit() -> usize {
  DEFAULT_INITIAL_NODE_LIMIT }

fn default_beep_when_server_becomes_available() -> bool {
  true }

fn default_max_role_tree_depth() -> usize {
  20 }


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

impl Deref for RepoName {
  // lets RepoName be used like a String
  type Target = String;
  fn deref (&self) -> &Self::Target {
    &self . 0 }}

impl fmt::Display for ID {
  fn fmt ( &self,
            f: &mut fmt::Formatter<'_> )
         -> fmt::Result {
    write! ( f, "{}", self . 0 ) }}

impl fmt::Display for RepoName {
  fn fmt ( &self,
            f: &mut fmt::Formatter<'_> )
         -> fmt::Result {
    write! ( f, "{}", self . 0 ) }}

impl fmt::Display for RepoSetName {
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

impl From<String> for RepoName {
  fn from ( s : String ) -> Self {
    RepoName (s) }}

impl From<String> for RepoSetName {
  fn from ( s : String ) -> Self {
    RepoSetName (s) }}

impl From<&String> for RepoName {
  fn from ( s : &String ) -> Self {
    RepoName ( s . clone () ) }}

impl From<&String> for RepoSetName {
  fn from ( s : &String ) -> Self {
    RepoSetName ( s . clone () ) }}

impl From <&str> for ID {
  fn from(s: &str) -> Self {
    ID ( s . to_string () ) }}

impl From <&str> for RepoName {
  fn from(s: &str) -> Self {
    RepoName ( s . to_string () ) }}

impl From <&str> for RepoSetName {
  fn from(s: &str) -> Self {
    RepoSetName ( s . to_string () ) }}

impl SkgConfig {
  /// Creates a SkgConfig with dummy values for everything except repos.
  /// Useful for tests that only need to read .skg files.
  pub fn dummyFromRepos (
    repos : HashMap<RepoName, SkgfileRepo>
  ) -> Self {
    SkgConfig {
      config_path        : PathBuf::from (""),
      data_root          : PathBuf::from ("."),
      repos,
      repo_order       : Vec::new (),
      default_repo_set : RepoSetName::from ("all"),
      owned_folder       : "owned" . to_string (),
      tantivy_folder     : PathBuf::from ("/tmp/unused"),
      port               : 0,
      initial_node_limit : DEFAULT_INITIAL_NODE_LIMIT,
      timing_log         : false,
      beep_when_server_becomes_available : false,
      max_role_tree_depth : default_max_role_tree_depth(), }}

  /// Creates a SkgConfig with a test-specific Tantivy folder.
  pub fn fromReposAndTantivyFolder (
    repos        : HashMap<RepoName, SkgfileRepo>,
    tantivy_folder : &str,
  ) -> Self {
    SkgConfig {
      config_path        : PathBuf::from (""),
      data_root          : PathBuf::from ("."),
      repos,
      repo_order       : Vec::new (),
      default_repo_set : RepoSetName::from ("all"),
      owned_folder       : "owned" . to_string (),
      tantivy_folder     : PathBuf::from (tantivy_folder),
      port               : DEFAULT_PORT,
      initial_node_limit : DEFAULT_INITIAL_NODE_LIMIT,
      timing_log         : false,
      beep_when_server_becomes_available : false,
      max_role_tree_depth : default_max_role_tree_depth(), }}

  pub fn user_owns_repo (
    &self,
    repo_name : &RepoName
  ) -> bool {
    self . repos . get (repo_name)
      . map ( |s| s . user_owns_it )
      . unwrap_or (false)
  }

  /// The owned repo names in privacy order (see 'ordered_repos').
  pub fn owned_repos_in_config_order (
    &self,
  ) -> Vec<RepoName> {
    self . ordered_repos () . into_iter ()
      . filter ( |name| self . user_owns_repo (name) )
      . collect () }

  /// The default owned repo for a fork's clone when its repo could
  /// not be inferred or user-set: the user's CONFIG-FIRST owned repo
  /// (matching the Emacs client's 'skg--default-repo', which is
  /// TOML-first). None only when the user owns no repo at all -- the
  /// one case 'ForkRepoUnresolved' still fires.
  pub fn first_owned_repo_in_config_order (
    &self,
  ) -> Option<RepoName> {
    self . owned_repos_in_config_order () . into_iter () . next () }

  /// Backwards-compatible name for the config-first owned repo
  /// default; see 'first_owned_repo_in_config_order'.
  pub fn first_owned_repo (
    &self,
  ) -> Option<RepoName> {
    self . first_owned_repo_in_config_order () }

  pub fn default_repo_set_name (
    &self,
  ) -> &RepoSetName {
    &self . default_repo_set }

  /// THE COMPARISON CHOKEPOINT, with the methods below it. Every
  /// repo name, in privacy order: most public first, most private
  /// last (TOML declaration order). Falls back to alphabetical when
  /// declaration order is unavailable (a dummy/test config, whose
  /// 'repo_order' is empty), so the result is always deterministic.
  /// No code outside these methods may compare repo positions.
  pub fn ordered_repos (
    &self,
  ) -> Vec<RepoName> {
    if self . repo_order . is_empty () {
      // No declaration order recorded: alphabetical, for determinism.
      let mut names : Vec<RepoName> =
        self . repos . keys () . cloned () . collect ();
      names . sort ();
      names
    } else { self . repo_order . clone () }}

  /// Position in the privacy order: 0 = most public.
  /// None for a repo absent from the config.
  pub fn repo_position (
    &self,
    repo : &RepoName,
  ) -> Option<usize> {
    self . ordered_repos () . iter ()
      . position ( |s| s == repo ) }

  /// True iff 'a' is STRICTLY more public than 'b' (earlier in the
  /// privacy order). A repo absent from the config counts as
  /// maximally private, so nothing is less public than it.
  pub fn is_strictly_more_public (
    &self,
    a : &RepoName,
    b : &RepoName,
  ) -> bool {
    match ( self . repo_position (a),
            self . repo_position (b) ) {
      (Some (pa), Some (pb)) => pa < pb,
      (Some (_),  None     ) => true,
      _                      => false, }}

  /// The more private of the two (the later in the privacy order);
  /// 'b' on a tie. This is the relRepo default rule's core: a
  /// relationship instance normally defaults to the repo of the more private
  /// of its two endpoints' homes.
  pub fn more_private_of (
    &self,
    a : RepoName,
    b : RepoName,
  ) -> RepoName {
    if self . is_strictly_more_public (&b, &a) { a } else { b }}

  /// The default relRepo for a writable node-to-node
  /// relationship. Between owned nodes, use the more-private home.
  /// From an owned owner to a foreign member, use the owner's home:
  /// Skg may expose the foreign ID there, but never proposes writing
  /// a relationship into the foreign repo.
  pub fn default_relRepo (
    &self,
    owner_home : &RepoName,
    member_home : &RepoName,
  ) -> RepoName {
    if ( self . user_owns_repo (owner_home) &&
         ! self . user_owns_repo (member_home) ) {
      owner_home . clone ()
    } else {
      self . more_private_of (
        owner_home . clone (), member_home . clone () ) }}

  /// The prefix of the privacy order through 'repo', inclusive:
  /// that repo and everything more public. Errors if the repo is
  /// not configured.
  pub fn prefix_through (
    &self,
    repo : &RepoName,
  ) -> Result<Vec<RepoName>, String> {
    let ordered : Vec<RepoName> =
      self . ordered_repos ();
    let position : usize =
      ordered . iter () . position ( |s| s == repo )
      . ok_or_else ( || format! (
        "Repo '{}' not found in config", repo )) ?;
    Ok ( ordered [..= position] . to_vec () ) }

  /// The repos a repo-set choice makes available: everything for
  /// "all"; for a repo name, the prefix of the privacy order
  /// through it (that repo and everything more public).
  pub fn repo_set_repos (
    &self,
    name : &RepoSetName,
  ) -> Result<BTreeSet<RepoName>, String> {
    if name . 0 == "all" {
      return Ok ( self . repos . keys () . cloned () . collect () ); }
    Ok ( self
         . prefix_through ( &RepoName::from ( name . 0 . as_str () ))
         . map_err ( |_| format! (
           "Repo-set '{}' names no configured repo. A repo-set is 'all' or the name of the most private repo to make available.",
           name )) ?
         . into_iter () . collect () ) }
}
