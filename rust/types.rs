use serde::{Serialize, Deserialize};
use std::error::Error;
use std::fmt;
use std::ops::Deref;
use std::path::PathBuf;
use std::str::FromStr;
use std::sync::Arc;
use tantivy::{Index, schema};


//
// Types
// (functions come later)
//

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash)]
pub struct ID(pub String);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Hyperlink {
  // Hyperlinks are represented in, and must be parsed from, the raw text fields `title` and `body`.
  pub id: ID,
  pub label: String, }

#[derive(Debug)]
pub enum HyperlinkParseError {
  InvalidFormat,
  MissingDivider, }

#[derive(Debug, Clone)]
pub struct OrgNode {
  // The data that can be seen about a node in an Emacs buffer. Includes ephemeral view data ("folded", "focused", and "repeated"), and omits long-term data that a FileNode would include.
  // The same structure is used to send to and receive from Emacs. However, the `id` can only be `None` when receiving from Emacs.
  pub id       : Option<ID>,
  pub heading  : String, // a term fron org-mode
  pub body     : Option<String>, // a term fron org-mode
  pub folded   : bool, // folded in the org-roam sense
  pub focused  : bool, // where the Emacs cursor is
  pub repeated : bool, /* The second and later instances of a node are "repeated". Their body and children are not displayed in Emacs, and Rust should not update the node they refer to based on the repeated data. THis permits handling infinite data.

Emacs needs to know that the node is repeated, in order to display it differently.

Rust needs to know if it was marked repeated for Emacs. Otherwise, if the user moved a repeat of the node to before the first instance of the node, Rust would treat that as the source of information to update the new node with, and probably delete the node's branches. */
  pub branches : Vec<OrgNode>, }

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash)]
pub struct FileNode {
  // There is a 1-to-1 correspondence between FileNodes and actual files -- a file can be read to a FileNode, and a FileNode can be written to a file. The files are the only permanent data; the rest are ephemeral aspects of views of those files. FileNode is the format used to initialize the TypeDB and Tantivy databases.
  // Tantivy will receive some of this data, and TypeDB some other subset. Tantivy associates IDs with titles. TypeDB represents all the connections. At least one field, `body`, is known to neither database; it is instead read directly from the files on disk when Rust builds a document for Emacs.

  pub title: String,
  pub ids: Vec<ID>, // Must be nonempty. Can include more than 1 because nodes might be merged.

  #[serde(default, skip_serializing_if = "Option::is_none")]
  pub body: Option<String>, // Unknown to both Tantivy & TypeDB. The body is all text (if any) between the preceding heading, to which it belongs, and the next (if any).

  #[serde(default, skip_serializing_if = "Vec::is_empty")]
  pub contains: Vec<ID>,

  #[serde(default, skip_serializing_if = "Vec::is_empty")]
  pub subscribes_to: Vec<ID>,

  #[serde(default, skip_serializing_if = "Vec::is_empty")]
  pub hides_from_its_subscriptions: Vec<ID>,

  #[serde(default, skip_serializing_if = "Vec::is_empty")]
  pub overrides_view_of_of: Vec<ID>,

  #[serde(skip)] // inferred from filepath
  pub path: PathBuf,
}

#[derive(Clone)]
pub struct TantivyIndex {
  // Associates titles to paths.
  pub index: Arc<Index>,
  pub path_field: schema::Field,
  pub title_field: schema::Field,
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
  // lets ID be used like a String in (more?) case
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

impl Hyperlink {
  pub fn new(id: impl Into<String>,
             label: impl Into<String>)
             -> Self {
    Hyperlink { id: ID(id.into()),
           label: label.into(),
    } } }

impl fmt::Display for Hyperlink {
  /// Format: [[id:HYPERLINK_ID][HYPERLINK_LABEL]]
  fn fmt(&self,
         f: &mut fmt::Formatter<'_>)
         -> fmt::Result {
    write!(f, "[[id:{}][{}]]", self.id, self.label)
  } }

impl FromStr for Hyperlink {
  type Err = HyperlinkParseError;

  fn from_str(text: &str) -> Result<Self, Self::Err> {
    if ( !text.starts_with("[[id:") ||
          !text.ends_with("]]") ) {
      return Err(HyperlinkParseError::InvalidFormat); }

    let interior = &text[5..text.len()-2];

    if let Some(idx) = interior.find("][") {
      let id = &interior[0..idx];
      let label = &interior[idx+2..];
      Ok ( Hyperlink {
        id: ID(id.to_string()),
        label: label.to_string(),
      } )
    } else {
      Err(HyperlinkParseError::MissingDivider)
    } } }

impl fmt::Display for HyperlinkParseError {
  fn fmt( &self,
          f: &mut fmt::Formatter<'_>)
          -> fmt::Result {
    match self {
      HyperlinkParseError::InvalidFormat =>
        write!(f, "Invalid hyperlink format. Expected [[id:HYPERLINK_ID][HYPERLINK_LABEL]]"),
      HyperlinkParseError::MissingDivider =>
        write!(f, "Missing divider between ID and label. Expected ]["),
    } } }

impl Error for HyperlinkParseError {}

pub fn filenode_example() -> FileNode {
  FileNode {
    title: "This text gets indexed.".to_string(),
    ids: vec![ ID::new("123") ],
    body: Some( r#"This one string could span pages.
It better be okay with newlines."# . to_string() ),
    contains: vec![ ID::new("1"),
                    ID::new("2"),
                    ID::new("3")],
    subscribes_to: vec![ID::new("11"),
                        ID::new("12"),
                        ID::new("13")],
    hides_from_its_subscriptions: vec![],
    overrides_view_of_of: vec![],
    path: PathBuf::from(
      "tests/file_io/generated/example.skg"),
  } }
