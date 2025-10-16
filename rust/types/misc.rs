use serde::{Serialize, Deserialize};
use std::error::Error;
use std::fmt;
use std::ops::Deref;
use std::path::PathBuf;
use std::str::FromStr;
use std::sync::Arc;
use tantivy::Index;
use tantivy::schema::Field;

/* Each node has a random ID. Skg does not check for collisions.
So far the IDs are Version 4 UUIDs.
.
Collisions are astronomically unlikely but not impossible.
If there are fewer than 3.3e15 v4 UUIDs --
equivalent to 9 billion people with 415,000 nodes each --
the collision probability is less than 1 in 1e6. */
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ID ( pub String );

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Hash)]
pub struct SkgConfig {
  pub db_name        : String,
  pub skg_folder     : PathBuf, // The user's .skg files go here.
  pub tantivy_folder : PathBuf,

  #[serde(default = "default_port")]
  pub port           : u16,  // TCP port for Rust-Emacs comms.

  #[serde(default)] // defaults to false
  pub delete_on_quit : bool, // Delete TypeDB db on server shutdown.
}

fn default_port() -> u16 { 1730 }

#[derive(Clone)]
pub struct TantivyIndex {
  // Associates titles and aliases to paths.
  pub index                : Arc<Index>,
  pub id_field             : Field,
  pub title_or_alias_field : Field,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Hyperlink {
  // Hyperlinks are represented in, and must be parsed from, the raw text fields `title` and `body`.
  pub id: ID,
  pub label: String,
}

#[derive(Debug)]
pub enum HyperlinkParseError {
  InvalidFormat,
  MissingDivider,
}


//
// Implementations
//

impl ID {
  pub fn new <S : Into<String>> (s: S) -> Self {
    ID ( s.into () ) }
  pub fn as_str ( &self ) -> &str {
    &self.0 }} // a reference to the first (and only) field

impl Deref for ID {
  // lets ID be used like a String in (more?) cases
  type Target = String;
  fn deref ( &self ) -> &Self::Target {
    &self.0 }}

impl fmt::Display for ID {
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

impl From <&str> for ID {
  fn from(s: &str) -> Self {
    ID ( s.to_string () ) }}

impl Hyperlink {
  pub fn new ( id     : impl Into<String>,
               label  : impl Into<String>)
             -> Self {
    Hyperlink { id    : ID ( id.into () ),
                label : label.into (),
    }} }

impl fmt::Display for Hyperlink {
  // Format: [[id:ID][LABEL]], where allcaps terms are variables.
  // This is the same format org-roam uses.
  fn fmt ( &self,
            f : &mut fmt::Formatter <'_> )
            -> fmt::Result {
    write! ( f, "[[id:{}][{}]]", self.id, self.label ) }}

impl FromStr for Hyperlink {
  type Err = HyperlinkParseError;

  fn from_str ( text: &str )
                -> Result <Self, Self::Err> {
    if ( !text.starts_with("[[id:") ||
          !text.ends_with("]]") ) {
      return Err(HyperlinkParseError::InvalidFormat); }

    let interior : &str = &text [5 .. text.len () - 2];

    if let Some ( idx ) = interior.find ( "][" ) {
      let id    : &str = &interior [0..idx];
      let label : &str = &interior [idx+2..];
      Ok ( Hyperlink {
        id    : ID ( id.to_string () ),
        label : label.to_string (),
      } )
    } else {
      Err ( HyperlinkParseError::MissingDivider )
    } } }

impl fmt::Display for HyperlinkParseError {
  fn fmt ( &self,
            f: &mut fmt::Formatter <'_>)
            -> fmt::Result {
    match self {
      HyperlinkParseError::InvalidFormat =>
        write! (
          f, "Invalid hyperlink format. Expected [[id:ID][LABEL]]" ),
      HyperlinkParseError::MissingDivider =>
        write! (
          f, "Missing divider between ID and label. Expected ][" ),
    } } }

impl Error for HyperlinkParseError {}
