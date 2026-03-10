use serde::{Serialize, Deserialize, Serializer, Deserializer};
use std::collections::HashMap;
use std::fmt;
use std::ops::Deref;
use std::path::PathBuf;
use std::sync::Arc;
use tantivy::Index;
use tantivy::schema::Field;

//
// Type Definitions
//

/// When the user saves a buffer, this type distinguishes
/// "not specified" (so use whatever is already on disk) from
/// "should be this value" (even if empty).
///
/// ELABORATION:
/// When a user saves a TrueNode, its non-ignored TrueNode children
/// always define its contents, so MaybeSpecified does not apply there.
/// But other fields -- e.g. aliases --
/// the user is likely not to mention (in particular,
/// not asking for any changes). This type distinguishes the case
/// where the user did not mention the field from the case
/// where the user wants it empty.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum MaybeSpecified<T> {
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

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Hash)]
pub struct SkgfileSource {
  pub nickname     : SourceName,
  pub path         : PathBuf,
  pub user_owns_it : bool,
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
pub struct SkgConfig {
  #[serde (skip)]
  pub data_root      : PathBuf, // Directory containing skgconfig.toml. Other relative paths (tantivy_folder, source paths) are resolved against this at load time.

  #[serde ( deserialize_with = "deserialize_sources" )]
  pub sources        : HashMap<SourceName, SkgfileSource>,
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
}

impl SkgConfig {
  pub fn logs_dir ( &self ) -> PathBuf {
    self . data_root . join ("logs") } }

/// Each source has a unique nickname, defined in the SkgConfig,
/// used in ViewNode metadata to track provenance.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct SourceName ( pub String );

#[derive (Clone)]
pub struct TantivyIndex {
  // Associates titles and aliases to paths.
  pub index                     : Arc<Index>,
  pub id_field                  : Field,
  pub title_or_alias_field      : Field,
  pub source_field              : Field,
  pub context_origin_type_field : Field,
  pub is_title_field            : Field,
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
  let sources_vec : Vec<SkgfileSource> =
    Vec::deserialize (deserializer) ?;
  let mut map : HashMap<SourceName, SkgfileSource> =
    HashMap::new ();
  for source in sources_vec {
    map . insert (
      source . nickname . clone (),
      source ); }
  Ok (map)
}

fn default_port() -> u16 {
  crate::consts::DEFAULT_PORT }

fn default_initial_node_limit() -> usize {
  crate::consts::DEFAULT_INITIAL_NODE_LIMIT }


//
// Implementations
//

impl<T> MaybeSpecified<T> {
  pub fn is_unspecified (&self) -> bool {
    matches! (self, MaybeSpecified::Unspecified) }
  /// Returns the inner slice, or empty if Unspecified.
  pub fn or_default (&self) -> &[T] {
    match self {
      MaybeSpecified::Unspecified  => &[],
      MaybeSpecified::Specified (v) => v } }
  /// Consumes self, returning the inner Vec or empty.
  pub fn into_vec (self) -> Vec<T> {
    match self {
      MaybeSpecified::Unspecified  => vec![],
      MaybeSpecified::Specified (v) => v } }
  /// Ensures the value is Specified, defaulting to empty,
  /// and returns a mutable reference to the inner Vec.
  pub fn ensure_specified (&mut self) -> &mut Vec<T> {
    if matches! (self, MaybeSpecified::Unspecified) {
      *self = MaybeSpecified::Specified (Vec::new ()); }
    match self {
      MaybeSpecified::Specified (v) => v,
      MaybeSpecified::Unspecified => unreachable! () } }
  /// For serde skip_serializing_if.
  /// Skips both Unspecified and Specified([]).
  pub fn skip_serializing (&self) -> bool {
    match self {
      MaybeSpecified::Unspecified  => true,
      MaybeSpecified::Specified (v) => v . is_empty () } } }

impl<T> Default for MaybeSpecified<T> {
  fn default () -> Self {
    MaybeSpecified::Unspecified } }

impl<T: Serialize> Serialize for MaybeSpecified<T> {
  fn serialize<S: Serializer> (
    &self, serializer : S
  ) -> Result<S::Ok, S::Error> {
    match self {
      MaybeSpecified::Unspecified  =>
        serializer . serialize_none (),
      MaybeSpecified::Specified (v) =>
        v . serialize (serializer) } } }

impl<'de, T: Deserialize<'de>> Deserialize<'de> for MaybeSpecified<T> {
  fn deserialize<D: Deserializer<'de>> (
    deserializer : D
  ) -> Result<Self, D::Error> {
    Vec::<T>::deserialize (deserializer)
      . map (MaybeSpecified::Specified) } }

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

impl From<String> for ID {
  fn from ( s : String ) -> Self {
    ID (s) }}

impl From<&String> for ID {
  fn from ( s : &String ) -> Self {
    ID ( s . clone () ) }}

impl From<String> for SourceName {
  fn from ( s : String ) -> Self {
    SourceName (s) }}

impl From<&String> for SourceName {
  fn from ( s : &String ) -> Self {
    SourceName ( s . clone () ) }}

impl From <&str> for ID {
  fn from(s: &str) -> Self {
    ID ( s . to_string () ) }}

impl From <&str> for SourceName {
  fn from(s: &str) -> Self {
    SourceName ( s . to_string () ) }}

impl SkgConfig {
  /// Creates a SkgConfig with dummy values for everything except sources.
  /// Useful for tests that only need to read .skg files.
  pub fn dummyFromSources (
    sources : HashMap<SourceName, SkgfileSource>
  ) -> Self {
    SkgConfig {
      data_root          : PathBuf::from ("."),
      sources,
      db_name            : "unused" . to_string(),
      tantivy_folder     : PathBuf::from ("/tmp/unused"),
      port               : 0,
      initial_node_limit : 100,
      delete_on_quit     : false,
      timing_log         : false, }}

  /// Creates a SkgConfig with test-appropriate values for db_name and tantivy_folder.
  /// Useful for tests that actually connect to TypeDB and create Tantivy indices.
  pub fn fromSourcesAndDbName (
    sources        : HashMap<SourceName, SkgfileSource>,
    db_name        : &str,
    tantivy_folder : &str,
  ) -> Self {
    SkgConfig {
      data_root          : PathBuf::from ("."),
      sources,
      db_name            : db_name . to_string(),
      tantivy_folder     : PathBuf::from (tantivy_folder),
      port               : crate::consts::DEFAULT_PORT,
      initial_node_limit : crate::consts::DEFAULT_INITIAL_NODE_LIMIT,
      delete_on_quit     : false,
      timing_log         : false, }}

  pub fn user_owns_source (
    &self,
    nickname : &SourceName
  ) -> bool {
    self . sources . get (nickname)
      . map ( |s| s . user_owns_it )
      . unwrap_or (false)
  }
}
