use serde::{Serialize, Deserialize};
use std::error::Error;
use std::fmt;
use std::ops::Deref;
use std::path::PathBuf;
use std::str::FromStr;


//
// Types
// (functions come later)
//

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash)]
pub struct ID(pub String);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Link {
  pub id: ID,
  pub label: String, }

#[derive(Debug)]
pub enum LinkParseError {
  InvalidFormat,
  MissingDivider, }

#[derive(Serialize, Deserialize, Debug)]
pub struct SkgNode {
  // Tantivy will receive some of this data,
  // and TypeDB some other subset.
  // At least one field, `body`, is known to neither.

  // The first title is the text displayed in a heading.
  // Other titles are just for search.
  pub titles: Vec<String>,
  pub ids: Vec<ID>, // Must be nonempty. Can be more than 1 because
  // nodes might be merged.

  // The body is all text (if any)
  // between this heading and the next (if any).
  #[serde(default, skip_serializing_if = "Option::is_none")]
  pub body: // Unknown to both Tantivy & TypeDB
  Option<String>,

  #[serde(default, skip_serializing_if = "Option::is_none")]
  pub comments_on: Option<ID>,

  #[serde(default, skip_serializing_if = "std::ops::Not::not")]
  pub no_tantivy_index: bool,

  #[serde(default, skip_serializing_if = "Vec::is_empty")]
  pub contains: Vec<ID>,

  #[serde(default, skip_serializing_if = "Vec::is_empty")]
  pub subscribes_to: Vec<ID>,

  #[serde(default, skip_serializing_if = "Vec::is_empty")]
  pub ignores: Vec<ID>,

  #[serde(default, skip_serializing_if = "Vec::is_empty")]
  pub replaces_view_of: Vec<ID>,

  #[serde(skip)] // inferred from filepath
  pub path: PathBuf,

  #[serde(skip)] // inferred from titles and body
  pub links: Vec<Link>,
}


//
// Functions
//

impl ID {
  pub fn new<S: Into<String>>(s: S) -> Self {
    ID(s.into()) }
  pub fn as_str(&self) -> &str {
    &self.0 } }

impl Deref for ID {
  // Lets ID be used like a String in (more?) cases.
  type Target = String;
  fn deref(&self) -> &Self::Target {
    &self.0 } }

impl fmt::Display for ID {
  fn fmt(&self, f: &mut fmt::Formatter<'_>)
         -> fmt::Result {
    write!(f, "{}", self.0) } }

impl From<String> for ID {
  fn from(s: String) -> Self {
    ID(s) } }

impl From<&str> for ID {
  fn from(s: &str) -> Self {
    ID(s.to_string()) } }

impl Link {
  pub fn new(id: impl Into<String>,
             label: impl Into<String>)
             -> Self {
    Link { id: ID(id.into()),
           label: label.into(),
    } } }

impl fmt::Display for Link {
  /// Format: [[id:LINK_ID][LINK_LABEL]]
  fn fmt(&self,
         f: &mut fmt::Formatter<'_>)
         -> fmt::Result {
    write!(f, "[[id:{}][{}]]", self.id, self.label)
  } }

impl FromStr for Link {
  type Err = LinkParseError;

  fn from_str(text: &str) -> Result<Self, Self::Err> {
    if ( !text.starts_with("[[id:") ||
          !text.ends_with("]]") ) {
      return Err(LinkParseError::InvalidFormat); }

    let interior = &text[5..text.len()-2];

    if let Some(idx) = interior.find("][") {
      let id = &interior[0..idx];
      let label = &interior[idx+2..];
      Ok ( Link {
        id: ID(id.to_string()),
        label: label.to_string(),
      } )
    } else {
      Err(LinkParseError::MissingDivider)
    } } }

impl fmt::Display for LinkParseError {
  fn fmt( &self,
          f: &mut fmt::Formatter<'_>)
          -> fmt::Result {
    match self {
      LinkParseError::InvalidFormat =>
        write!(f, "Invalid link format. Expected [[id:LINK_ID][LINK_LABEL]]"),
      LinkParseError::MissingDivider =>
        write!(f, "Missing divider between ID and label. Expected ]["),
    } } }

impl Error for LinkParseError {}

pub fn skgnode_example() -> SkgNode {
  SkgNode {
    titles: vec![
      "This text gets indexed.".to_string(),
      "Maybe searching other text could find this note.".to_string(),
      "YAML does not escape \"quotation marks\" in text."
        .to_string() ],
    ids: vec![ ID::new("123") ],
    body: Some( r#"This one string could span pages.
It better be okay with newlines."# . to_string() ),
    comments_on: Some(ID::new("42")),
    no_tantivy_index: true,
    contains: vec![ ID::new("1"),
                    ID::new("2"),
                    ID::new("3")],
    subscribes_to: vec![ID::new("11"),
                        ID::new("12"),
                        ID::new("13")],
    ignores: vec![],
    replaces_view_of: vec![],
    path: PathBuf::from(
      "tests/file_io/generated/example.skg"),
    links: vec![],
  } }
