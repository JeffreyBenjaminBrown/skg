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
  pub db_name        : String,
  pub tantivy_folder : PathBuf,

  #[serde ( deserialize_with = "deserialize_sources" )]
  pub sources        : HashMap<SourceName, SkgfileSource>,

  #[serde(default = "default_port")]
  pub port           : u16,  // TCP port for Rust-Emacs comms.

  #[serde(default)] // defaults to false
  pub delete_on_quit : bool, // Clear Neo4j on server shutdown.

  #[serde(default = "default_neo4j_uri")]
  pub neo4j_uri      : String,
  #[serde(default = "default_neo4j_user")]
  pub neo4j_user     : String,
  #[serde(default = "default_neo4j_password")]
  pub neo4j_password : String,

  #[serde(default = "default_initial_node_limit")]
  pub initial_node_limit : usize, // Max nodes to render in initial content views.

  #[serde(default)] // defaults to false
  pub timing_log     : bool, // Write timing data to <config_dir>/timing.log.

  #[serde(skip)]
  pub config_dir     : PathBuf, // Directory containing skgconfig.toml.
}

/// Each source has a unique nickname, defined in the SkgConfig,
/// used in ViewNode metadata to track provenance.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct SourceName ( pub String );

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
) -> Result <HashMap<SourceName, SkgfileSource>, D::Error>
where
  D : Deserializer<'de>
{
  let sources_vec : Vec<SkgfileSource> =
    Vec::deserialize ( deserializer ) ?;
  let mut map : HashMap<SourceName, SkgfileSource> =
    HashMap::new ();
  for source in sources_vec {
    map.insert (
      source . nickname . clone (),
      source ); }
  Ok ( map )
}

fn default_port() -> u16 { 1730 }
fn default_initial_node_limit() -> usize { 1000 }
fn default_neo4j_uri() -> String { "bolt://localhost:7687".to_string() }
fn default_neo4j_user() -> String { "neo4j".to_string() }
fn default_neo4j_password() -> String { "password".to_string() }


//
// Implementations
//

impl ID {
  pub fn new <S : Into<String>> (s: S) -> Self {
    ID ( s.into () ) }}

impl Deref for ID {
  // lets ID be used like a String in (more?) cases
  type Target = String;
  fn deref ( &self ) -> &Self::Target {
    &self.0 }}

impl AsRef<str> for ID {
  fn as_ref(&self) -> &str {
    &self.0 }}

impl Deref for SourceName {
  // lets SourceName be used like a String
  type Target = String;
  fn deref ( &self ) -> &Self::Target {
    &self.0 }}

impl fmt::Display for ID {
  fn fmt ( &self,
            f: &mut fmt::Formatter<'_> )
         -> fmt::Result {
    write! ( f, "{}", self.0 ) }}

impl fmt::Display for SourceName {
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

impl From<String> for SourceName {
  fn from ( s : String ) -> Self {
    SourceName (s) }}

impl From<&String> for SourceName {
  fn from ( s : &String ) -> Self {
    SourceName ( s.clone () ) }}

impl From <&str> for ID {
  fn from(s: &str) -> Self {
    ID ( s.to_string () ) }}

impl From <&str> for SourceName {
  fn from(s: &str) -> Self {
    SourceName ( s.to_string () ) }}

impl SkgConfig {
  /// Creates a SkgConfig with dummy values for everything except sources.
  /// Useful for tests that only need to read .skg files.
  pub fn dummyFromSources (
    sources : HashMap<SourceName, SkgfileSource>
  ) -> Self {
    SkgConfig {
      db_name            : "unused".to_string(),
      tantivy_folder     : PathBuf::from("/tmp/unused"),
      sources,
      port               : 0,
      delete_on_quit     : false,
      neo4j_uri          : default_neo4j_uri(),
      neo4j_user         : default_neo4j_user(),
      neo4j_password     : default_neo4j_password(),
      initial_node_limit : 100,
      timing_log         : false,
      config_dir         : PathBuf::from("."), }}

  /// Creates a SkgConfig with test-appropriate values for db_name and tantivy_folder.
  /// Useful for tests that actually connect to Neo4j and create Tantivy indices.
  pub fn fromSourcesAndDbName (
    sources        : HashMap<SourceName, SkgfileSource>,
    db_name        : &str,
    tantivy_folder : &str,
  ) -> Self {
    SkgConfig {
      db_name            : db_name.to_string(),
      tantivy_folder     : PathBuf::from(tantivy_folder),
      sources,
      port               : 1730,
      delete_on_quit     : false,
      neo4j_uri          : default_neo4j_uri(),
      neo4j_user         : default_neo4j_user(),
      neo4j_password     : default_neo4j_password(),
      initial_node_limit : 1000,
      timing_log         : false,
      config_dir         : PathBuf::from("."), }}

  pub fn user_owns_source (
    &self,
    nickname : &SourceName
  ) -> bool {
    self . sources . get ( nickname )
      . map ( |s| s.user_owns_it )
      . unwrap_or ( false )
  }
}
