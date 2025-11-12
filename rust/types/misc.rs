use serde::{Serialize, Deserialize, Deserializer};
use std::collections::HashMap;
use std::fmt;
use std::ops::Deref;
use std::path::PathBuf;
use std::str::FromStr;
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TextLink {
  // TextLinks are represented in, and must be parsed from, the raw text fields `title` and `body`.
  pub id: ID,
  pub label: String,
}

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
}

#[derive(Clone)]
pub struct TantivyIndex {
  // Associates titles and aliases to paths.
  pub index                : Arc<Index>,
  pub id_field             : Field,
  pub title_or_alias_field : Field,
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


//
// Implementations
//

impl ID {
  pub fn new <S : Into<String>> (s: S) -> Self {
    ID ( s.into () ) }
  pub fn as_str ( &self ) -> &str {
    &self.0 }} // a reference to the first (and only) field

impl SourceNickname {
  pub fn new <S : Into<String>> (s: S) -> Self {
    SourceNickname ( s.into () ) }
  pub fn as_str ( &self ) -> &str {
    &self.0 }}

impl Deref for ID {
  // lets ID be used like a String in (more?) cases
  type Target = String;
  fn deref ( &self ) -> &Self::Target {
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

impl TextLink {
  pub fn new ( id     : impl Into<String>,
               label  : impl Into<String>)
             -> Self {
    TextLink { id    : ID ( id.into () ),
                label : label.into (),
    }} }

impl fmt::Display for TextLink {
  // Format: [[id:ID][LABEL]], where allcaps terms are variables.
  // This is the same format org-roam uses.
  fn fmt ( &self,
            f : &mut fmt::Formatter <'_> )
            -> fmt::Result {
    write! ( f, "[[id:{}][{}]]", self.id, self.label ) }}

impl FromStr for TextLink {
  type Err = super::errors::TextLinkParseError;

  fn from_str ( text: &str )
                -> Result <Self, Self::Err> {
    use super::errors::TextLinkParseError;
    if ( !text.starts_with("[[id:") ||
          !text.ends_with("]]") ) {
      return Err(TextLinkParseError::InvalidFormat); }

    let interior : &str = &text [5 .. text.len () - 2];

    if let Some ( idx ) = interior.find ( "][" ) {
      let id    : &str = &interior [0..idx];
      let label : &str = &interior [idx+2..];
      Ok ( TextLink {
        id    : ID ( id.to_string () ),
        label : label.to_string (),
      } )
    } else {
      Err ( TextLinkParseError::MissingDivider )
    } } }

impl SkgConfig {
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
