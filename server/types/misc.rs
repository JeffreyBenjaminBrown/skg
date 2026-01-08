use serde::{Serialize, Deserialize, Deserializer};
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

/* Each node has a random ID. Skg does not check for collisions.
So far the IDs are Version 4 UUIDs.
.
Collisions are astronomically unlikely but not impossible.
If there are fewer than 3.3e15 v4 UUIDs --
equivalent to 9 billion people with 415,000 nodes each --
the collision probability is less than 1 in 1e6. */
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ID ( pub String );

/// Each source has a unique nickname, defined in the SkgConfig,
/// used in OrgNode metadata to track provenance.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SourceNickname ( pub String );

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Hash)]
pub struct SkgfileSource {
  pub nickname     : String,
  pub path         : PathBuf,
  pub user_owns_it : bool,
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
pub struct SkgConfig {
  pub db_name        : String,
  pub tantivy_folder : PathBuf,

  #[serde ( deserialize_with = "deserialize_sources" )]
  pub sources        : HashMap<String, SkgfileSource>,

  #[serde(default = "default_port")]
  pub port           : u16,  // TCP port for Rust-Emacs comms.

  #[serde(default)] // defaults to false
  pub delete_on_quit : bool, // Delete TypeDB db on server shutdown.

  #[serde(default = "default_initial_node_limit")]
  pub initial_node_limit : usize, // Max nodes to render in initial content views.
}

#[derive(Clone)]
pub struct TantivyIndex {
  // Associates titles and aliases to paths.
  pub index                : Arc<Index>,
  pub id_field             : Field,
  pub title_or_alias_field : Field,
  pub source_field         : Field,
}


//
// Helper Functions
//

fn deserialize_sources<'de, D> (
  deserializer : D
) -> Result <HashMap<String, SkgfileSource>, D::Error>
where
  D : Deserializer<'de>
{
  let sources_vec : Vec<SkgfileSource> =
    Vec::deserialize ( deserializer ) ?;
  let mut map : HashMap<String, SkgfileSource> =
    HashMap::new ();
  for source in sources_vec {
    map.insert (
      source . nickname . clone (),
      source ); }
  Ok ( map )
}

fn default_port() -> u16 { 1730 }

fn default_initial_node_limit() -> usize { 1000 }


//
// Implementations
//

impl ID {
  pub fn new <S : Into<String>> (s: S) -> Self {
    ID ( s.into () ) }
  #[allow(dead_code)]
  fn as_str ( &self ) -> &str {
    &self.0 }} // a reference to the first (and only) field

impl SourceNickname {
  #[allow(dead_code)]
  fn new <S : Into<String>> (s: S) -> Self {
    SourceNickname ( s.into () ) }
  #[allow(dead_code)]
  fn as_str ( &self ) -> &str {
    &self.0 }}

impl Deref for ID {
  // lets ID be used like a String in (more?) cases
  type Target = String;
  fn deref ( &self ) -> &Self::Target {
    &self.0 }}

impl AsRef<str> for ID {
  fn as_ref(&self) -> &str {
    &self.0 }}

impl Deref for SourceNickname {
  // lets SourceNickname be used like a String in (more?) cases
  type Target = String;
  fn deref ( &self ) -> &Self::Target {
    &self.0 }}

impl fmt::Display for ID {
  fn fmt ( &self,
            f: &mut fmt::Formatter<'_> )
         -> fmt::Result {
    write! ( f, "{}", self.0 ) }}

impl fmt::Display for SourceNickname {
  fn fmt ( &self,
            f: &mut fmt::Formatter<'_> )
         -> fmt::Result {
    write! ( f, "{}", self.0 ) }}

impl From<String> for ID {
  fn from ( s : String ) -> Self {
    ID (s) }}

impl From<&String> for ID {
  fn from ( s : &String ) -> Self {
    ID ( s.clone () ) }}

impl From<String> for SourceNickname {
  fn from ( s : String ) -> Self {
    SourceNickname (s) }}

impl From<&String> for SourceNickname {
  fn from ( s : &String ) -> Self {
    SourceNickname ( s.clone () ) }}

impl From <&str> for ID {
  fn from(s: &str) -> Self {
    ID ( s.to_string () ) }}

impl From <&str> for SourceNickname {
  fn from(s: &str) -> Self {
    SourceNickname ( s.to_string () ) }}

impl SkgConfig {
  /// Creates a SkgConfig with dummy values for everything except sources.
  /// Useful for tests that only need to read .skg files.
  pub fn dummyFromSources (
    sources : HashMap<String, SkgfileSource>
  ) -> Self {
    SkgConfig {
      db_name            : "unused".to_string(),
      tantivy_folder     : PathBuf::from("/tmp/unused"),
      sources,
      port               : 0,
      delete_on_quit     : false,
      initial_node_limit : 100, }}

  /// Creates a SkgConfig with test-appropriate values for db_name and tantivy_folder.
  /// Useful for tests that actually connect to TypeDB and create Tantivy indices.
  pub fn fromSourcesAndDbName (
    sources        : HashMap<String, SkgfileSource>,
    db_name        : &str,
    tantivy_folder : &str,
  ) -> Self {
    SkgConfig {
      db_name            : db_name.to_string(),
      tantivy_folder     : PathBuf::from(tantivy_folder),
      sources,
      port               : 1730,
      delete_on_quit     : false,
      initial_node_limit : 1000, }}

  pub fn get_source (
    &self,
    nickname : &str
  ) -> Option<&SkgfileSource> {
    self . sources . get ( nickname )
  }

  pub fn get_source_path (
    &self,
    nickname : &str
  ) -> Option<&PathBuf> {
    self . sources . get ( nickname )
      . map ( |s| &s.path )
  }

  pub fn user_owns_source (
    &self,
    nickname : &str
  ) -> bool {
    self . sources . get ( nickname )
      . map ( |s| s.user_owns_it )
      . unwrap_or ( false )
  }
}
