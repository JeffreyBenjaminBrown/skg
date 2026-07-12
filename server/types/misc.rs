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
/// When a user saves an ActiveNode, its non-ignored ActiveNode children
/// always define its contents, so MSV does not apply there.
/// But other fields -- e.g. aliases --
/// the user is likely not to mention (in particular,
/// not asking for any changes). This type distinguishes the case
/// where the user did not mention the field from the case
/// where the user wants it empty.
///
/// THE COL RULE these values encode at save extraction
/// ('server/from_text/local_instruction_collection/traverse.rs'
/// enforces it; 'supplement_unspecified_fields_from_disk' consumes
/// it): a buffer with NO col for a field (no AliasCol, SubscribeeCol
/// or OverriddenCol under the defining node) emits no intent for that
/// field, which lowers to 'Unspecified' -- no opinion, so the value
/// is filled from disk. A PRESENT-BUT-EMPTY col emits an empty list,
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

#[derive(Serialize, Clone, PartialEq, Eq, Hash)]
pub struct SkgfileSource {
  pub name         : SourceName,
  pub abbreviation : Option<String>,
  pub path         : PathBuf,
  // DERIVED at config load, never written in TOML (a raw
  // 'user_owns_it' key is rejected): true iff the source's
  // as-written path sits under the config's 'owned_folder' (its
  // first component equals it). See 'derive_ownership_and_labels'
  // in server/dbs/filesystem/not_nodes.rs. Rust constructors (tests)
  // may still set it directly.
  pub user_owns_it : bool,
}

/// The TOML shape of a '[[sources]]' entry: 'name' is optional,
/// defaulting to the path as written (e.g. "eggman/eggs"), which the
/// herald-label defaulting then abbreviates for owned sources.
#[derive(Deserialize)]
struct SkgfileSourceToml {
  name         : Option<SourceName>,
  #[serde(default)]
  abbreviation : Option<String>,
  path         : PathBuf,
}

impl From<SkgfileSourceToml> for SkgfileSource {
  fn from (
    raw : SkgfileSourceToml
  ) -> SkgfileSource {
    let name : SourceName =
      raw . name . unwrap_or_else (
        || SourceName (
          raw . path . to_string_lossy () . into_owned () ));
    SkgfileSource {
      name,
      abbreviation : raw . abbreviation,
      path         : raw . path,
      user_owns_it : false, // derived later; see the field's comment
    }}}

/// A member of a node's relationship list, tagged with the
/// relationship instance's PRIVACY LEVEL: the source whose accordion
/// section records this edge. The level is about the EDGE, not the
/// member node (a public node can be a private member). Default
/// level = the more private of the two endpoints' homes
/// ('SkgConfig::more_private_of'); 'skg-privatize-relationship' may
/// raise it; renormalization never lowers it (the sticky rule). See
/// TODO/user-owned_autofork_chain/5_plan.org.
///
/// INTERIM (work item leveled-lists): every consumer currently runs
/// DEGENERATE semantics -- level := the owning node's (home) source
/// at load, levels dropped at the FS boundary -- so behavior is
/// unchanged until the fold lands (work item section-format-and-fold).
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct PrivaciedMember<T> {
  pub level  : SourceName,
  pub member : T,
}

impl<T> PrivaciedMember<T> {
  pub fn at (
    level  : SourceName,
    member : T,
  ) -> PrivaciedMember<T> {
    PrivaciedMember { level, member }}}

/// Tag every member of a list with one level. The degenerate-load
/// helper (level := home source), and later the natural constructor
/// for a single accordion section's slice.
pub fn privacied_all<T> (
  level   : &SourceName,
  members : Vec<T>,
) -> Vec<PrivaciedMember<T>> {
  members . into_iter ()
    . map ( |m| PrivaciedMember::at ( level . clone (), m ) )
    . collect () }

/// The members of a leveled list, levels dropped. The FS-boundary
/// projection, and the adapter for consumers that only care WHO is
/// related, not at what level.
pub fn members_of<T : Clone> (
  list : &[PrivaciedMember<T>],
) -> Vec<T> {
  list . iter ()
    . map ( |m| m . member . clone () )
    . collect () }

/// 'privacied_all' lifted over MSV.
pub fn privacied_msv<T> (
  level : &SourceName,
  msv   : MSV<T>,
) -> MSV<PrivaciedMember<T>> {
  match msv {
    MSV::Unspecified     => MSV::Unspecified,
    MSV::Specified (v)   =>
      MSV::Specified ( privacied_all (level, v) ), }}

/// 'members_of' lifted over MSV.
pub fn members_msv<T : Clone> (
  msv : &MSV<PrivaciedMember<T>>,
) -> MSV<T> {
  match msv {
    MSV::Unspecified     => MSV::Unspecified,
    MSV::Specified (v)   =>
      MSV::Specified ( members_of (v) ), }}

/// Identifies a source-set CHOICE. Source-sets are no longer defined
/// by hand: they are the PREFIXES of the config's source order (most
/// public first), so a choice is either "all" (every source) or the
/// name of a configured source -- meaning "that source and everything
/// more public", i.e. the most private source to make available.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SourceSetName ( pub String );

impl SkgfileSource {
  pub fn herald_label (&self) -> &str {
    self . abbreviation . as_deref ()
      . unwrap_or ( &self . name ) }}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
pub struct SkgConfig {
  #[serde (skip)]
  pub config_path    : PathBuf, // Path to skgconfig.toml. Used to reload config without changing which file governs this server.

  #[serde (skip)]
  pub data_root      : PathBuf, // Directory containing skgconfig.toml. Other relative paths (tantivy_folder, source paths) are resolved against this at load time.

  #[serde ( deserialize_with = "deserialize_sources" )]
  pub sources        : HashMap<SourceName, SkgfileSource>,

  // The source names in TOML declaration order ('sources' is a HashMap,
  // which loses it). Filled at parse time by the config loaders; empty
  // for dummy/test configs, where the config-order helpers fall back to
  // alphabetical. LOAD-BEARING: declaration order is the privacy order
  // (most public first); the fold, the edge-level defaults, the
  // validators, and prefix source-sets all read it, through the
  // comparison chokepoint methods below ('ordered_sources',
  // 'source_position', 'is_strictly_more_public', 'more_private_of',
  // 'prefix_through'). No other code may compare source positions.
  #[serde (skip)]
  pub source_order   : Vec<SourceName>,

  #[serde(default = "default_source_set_name")]
  pub default_source_set : SourceSetName,

  // The directory (as a first path component under the data root)
  // whose sources the user OWNS; every other source is foreign
  // (read-only). Replaces the retired per-source 'user_owns_it'
  // TOML key. The intended layout is data/AUTHOR/REPO, with this
  // field naming the user's own author folder.
  #[serde(default = "default_owned_folder")]
  pub owned_folder : String,

  pub db_name        : String,
  pub tantivy_folder : PathBuf,

  #[serde(default = "default_port")]
  pub port           : u16,  // TCP port for Rust-Emacs comms.

  #[serde(default = "default_initial_node_limit")]
  pub initial_node_limit : usize, // Max nodes to render in initial content views.

  #[serde (default)] // defaults to false
  pub delete_on_quit : bool, // Delete TypeDB db on server shutdown. Tests use it.

  #[serde (default)] // defaults to false
  pub timing_log     : bool, // Write JSON log to <data_root>/logs/server.jsonl.

  #[serde (default)] // defaults to false
  pub auto_audit_daily : bool, // If true, the server runs the in-Rust-graph-vs-TypeDB consistency audit at most once per day, backgrounded at lowest priority, and reports any mismatches via <data_root>/audits.org.

  #[serde(default = "default_beep_when_server_becomes_available")]
  pub beep_when_server_becomes_available : bool, // Play a local sound when server initialization finishes.

  #[serde(default = "default_max_ancestry_depth")]
  pub max_ancestry_depth : usize, // Max BFS depth for full containerward ancestry.
}

impl SkgConfig {
  pub fn logs_dir ( &self ) -> PathBuf {
    self . data_root . join ("logs") } }

/// Each source has a unique name, defined in the SkgConfig,
/// used in ViewNode metadata to track provenance.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct SourceName ( pub String );

impl SourceName {
  /// Reserved sentinel source for a *non-Active* viewnode (e.g. a
  /// Diff phantom) whose source could not be determined. It renders like
  /// any other source -- the all-caps name alone flags it to the user --
  /// so that one unresolvable reference does not abort an entire render
  /// (TODO/DONE/local-view-update/plan_v2.org §7.6). Active-vognode source failures are caught by
  /// validation (pre-save) or are catastrophic (post-save), never this.
  pub const NOT_FOUND_STR : &'static str = "NOT_FOUND";
  pub fn not_found () -> Self {
    SourceName ( Self::NOT_FOUND_STR . to_string () ) } }

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
  pub raw_title_field           : Field, // Un-reduced title for is_title=true docs: preserves textlink syntax (e.g. "[[id:X][label]]") that 'title_or_alias_field' strips to bare labels. Populated only on primary-title docs; empty for alias docs. Used by the 'titles by ids' endpoint so clients can tell link-titles from plain titles.
  pub source_field              : Field,
  pub context_origin_type_field : Field,
  pub is_title_field            : Field,
  pub had_id_field              : Field,
  pub body_field                : Field,
}

//
// Helper Functions
//

fn deserialize_sources<'de, D> (
  deserializer : D
) -> Result <HashMap<SourceName, SkgfileSource>, D::Error>
where
  D : Deserializer<'de>
{
  let sources_vec : Vec<SkgfileSourceToml> =
    Vec::deserialize (deserializer) ?;
  let mut map : HashMap<SourceName, SkgfileSource> =
    HashMap::new ();
  for raw in sources_vec {
    let source : SkgfileSource =
      SkgfileSource::from (raw);
    if map . insert (
         source . name . clone (),
         source . clone () ) . is_some () {
      return Err (serde::de::Error::custom (
        format! ("Duplicate source name '{}'", source . name))); }}
  Ok (map)
}

fn default_source_set_name () -> SourceSetName {
  SourceSetName::from ("all") }

fn default_owned_folder () -> String {
  "owned" . to_string () }

fn default_port() -> u16 {
  DEFAULT_PORT }

fn default_initial_node_limit() -> usize {
  DEFAULT_INITIAL_NODE_LIMIT }

fn default_beep_when_server_becomes_available() -> bool {
  true }

fn default_max_ancestry_depth() -> usize {
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

impl Deref for SourceName {
  // lets SourceName be used like a String
  type Target = String;
  fn deref (&self) -> &Self::Target {
    &self . 0 }}

impl fmt::Display for ID {
  fn fmt ( &self,
            f: &mut fmt::Formatter<'_> )
         -> fmt::Result {
    write! ( f, "{}", self . 0 ) }}

impl fmt::Display for SourceName {
  fn fmt ( &self,
            f: &mut fmt::Formatter<'_> )
         -> fmt::Result {
    write! ( f, "{}", self . 0 ) }}

impl fmt::Display for SourceSetName {
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

impl From<String> for SourceName {
  fn from ( s : String ) -> Self {
    SourceName (s) }}

impl From<String> for SourceSetName {
  fn from ( s : String ) -> Self {
    SourceSetName (s) }}

impl From<&String> for SourceName {
  fn from ( s : &String ) -> Self {
    SourceName ( s . clone () ) }}

impl From<&String> for SourceSetName {
  fn from ( s : &String ) -> Self {
    SourceSetName ( s . clone () ) }}

impl From <&str> for ID {
  fn from(s: &str) -> Self {
    ID ( s . to_string () ) }}

impl From <&str> for SourceName {
  fn from(s: &str) -> Self {
    SourceName ( s . to_string () ) }}

impl From <&str> for SourceSetName {
  fn from(s: &str) -> Self {
    SourceSetName ( s . to_string () ) }}

impl SkgConfig {
  /// Creates a SkgConfig with dummy values for everything except sources.
  /// Useful for tests that only need to read .skg files.
  pub fn dummyFromSources (
    sources : HashMap<SourceName, SkgfileSource>
  ) -> Self {
    SkgConfig {
      config_path        : PathBuf::from (""),
      data_root          : PathBuf::from ("."),
      sources,
      source_order       : Vec::new (),
      default_source_set : SourceSetName::from ("all"),
      owned_folder       : "owned" . to_string (),
      db_name            : "unused" . to_string(),
      tantivy_folder     : PathBuf::from ("/tmp/unused"),
      port               : 0,
      initial_node_limit : DEFAULT_INITIAL_NODE_LIMIT,
      delete_on_quit     : false,
      timing_log         : false,
      auto_audit_daily   : false,
      beep_when_server_becomes_available : false,
      max_ancestry_depth : default_max_ancestry_depth(), }}

  /// Creates a SkgConfig with test-appropriate values for db_name and tantivy_folder.
  /// Useful for tests that actually connect to TypeDB and create Tantivy indices.
  pub fn fromSourcesAndDbName (
    sources        : HashMap<SourceName, SkgfileSource>,
    db_name        : &str,
    tantivy_folder : &str,
  ) -> Self {
    SkgConfig {
      config_path        : PathBuf::from (""),
      data_root          : PathBuf::from ("."),
      sources,
      source_order       : Vec::new (),
      default_source_set : SourceSetName::from ("all"),
      owned_folder       : "owned" . to_string (),
      db_name            : db_name . to_string(),
      tantivy_folder     : PathBuf::from (tantivy_folder),
      port               : DEFAULT_PORT,
      initial_node_limit : DEFAULT_INITIAL_NODE_LIMIT,
      delete_on_quit     : false,
      timing_log         : false,
      auto_audit_daily   : false,
      beep_when_server_becomes_available : false,
      max_ancestry_depth : default_max_ancestry_depth(), }}

  pub fn user_owns_source (
    &self,
    source_name : &SourceName
  ) -> bool {
    self . sources . get (source_name)
      . map ( |s| s . user_owns_it )
      . unwrap_or (false)
  }

  /// The owned source names in privacy order (see 'ordered_sources').
  pub fn owned_sources_in_config_order (
    &self,
  ) -> Vec<SourceName> {
    self . ordered_sources () . into_iter ()
      . filter ( |name| self . user_owns_source (name) )
      . collect () }

  /// The default owned source for a fork's clone when its source could
  /// not be inferred or user-set: the user's CONFIG-FIRST owned source
  /// (matching the Emacs client's 'skg--default-source', which is
  /// TOML-first). None only when the user owns no source at all -- the
  /// one case 'ForkSourceUnresolved' still fires.
  pub fn first_owned_source_in_config_order (
    &self,
  ) -> Option<SourceName> {
    self . owned_sources_in_config_order () . into_iter () . next () }

  /// Backwards-compatible name for the config-first owned source
  /// default; see 'first_owned_source_in_config_order'.
  pub fn first_owned_source (
    &self,
  ) -> Option<SourceName> {
    self . first_owned_source_in_config_order () }

  pub fn default_source_set_name (
    &self,
  ) -> &SourceSetName {
    &self . default_source_set }

  /// THE COMPARISON CHOKEPOINT, with the methods below it. Every
  /// source name, in privacy order: most public first, most private
  /// last (TOML declaration order). Falls back to alphabetical when
  /// declaration order is unavailable (a dummy/test config, whose
  /// 'source_order' is empty), so the result is always deterministic.
  /// No code outside these methods may compare source positions.
  pub fn ordered_sources (
    &self,
  ) -> Vec<SourceName> {
    if self . source_order . is_empty () {
      // No declaration order recorded: alphabetical, for determinism.
      let mut names : Vec<SourceName> =
        self . sources . keys () . cloned () . collect ();
      names . sort ();
      names
    } else { self . source_order . clone () }}

  /// Position in the privacy order: 0 = most public.
  /// None for a source absent from the config.
  pub fn source_position (
    &self,
    source : &SourceName,
  ) -> Option<usize> {
    self . ordered_sources () . iter ()
      . position ( |s| s == source ) }

  /// True iff 'a' is STRICTLY more public than 'b' (earlier in the
  /// privacy order). A source absent from the config counts as
  /// maximally private, so nothing is less public than it.
  pub fn is_strictly_more_public (
    &self,
    a : &SourceName,
    b : &SourceName,
  ) -> bool {
    match ( self . source_position (a),
            self . source_position (b) ) {
      (Some (pa), Some (pb)) => pa < pb,
      (Some (_),  None     ) => true,
      _                      => false, }}

  /// The more private of the two (the later in the privacy order);
  /// 'b' on a tie. This is the edge-level default rule's core: a
  /// relationship instance defaults to the level of the more private
  /// of its two endpoints' homes.
  pub fn more_private_of (
    &self,
    a : SourceName,
    b : SourceName,
  ) -> SourceName {
    if self . is_strictly_more_public (&b, &a) { a } else { b }}

  /// The prefix of the privacy order through 'source', inclusive:
  /// that source and everything more public. Errors if the source is
  /// not configured.
  pub fn prefix_through (
    &self,
    source : &SourceName,
  ) -> Result<Vec<SourceName>, String> {
    let ordered : Vec<SourceName> =
      self . ordered_sources ();
    let position : usize =
      ordered . iter () . position ( |s| s == source )
      . ok_or_else ( || format! (
        "Source '{}' not found in config", source )) ?;
    Ok ( ordered [..= position] . to_vec () ) }

  /// The sources a source-set choice makes available: everything for
  /// "all"; for a source name, the prefix of the privacy order
  /// through it (that source and everything more public).
  pub fn source_set_sources (
    &self,
    name : &SourceSetName,
  ) -> Result<BTreeSet<SourceName>, String> {
    if name . 0 == "all" {
      return Ok ( self . sources . keys () . cloned () . collect () ); }
    Ok ( self
         . prefix_through ( &SourceName::from ( name . 0 . as_str () ))
         . map_err ( |_| format! (
           "Source-set '{}' names no configured source. A source-set is 'all' or the name of the most private source to make available.",
           name )) ?
         . into_iter () . collect () ) }
}
