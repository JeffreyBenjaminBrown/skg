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

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct SkgConfig {
  pub db_name        : String,
  pub skg_folder     : PathBuf,
  pub tantivy_folder : PathBuf,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash)]
pub struct ID ( pub String );

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Hyperlink {
  // Hyperlinks are represented in, and must be parsed from, the raw text fields `title` and `body`.
  pub id: ID,
  pub label: String, }

#[derive(Debug)]
pub enum HyperlinkParseError {
  InvalidFormat,
  MissingDivider, }

/// Raw text from Emacs is first loaded as a forest of these.
#[derive(Clone, Debug, PartialEq)]
pub struct OrgNode {
  pub title    : String,         // The title part of the headline (after asterisks and metadata). 'Headline' is a term from org-mode.
  pub body     : Option<String>, // "body" is a term fron org-mode
  pub branches : Vec<OrgNode>, }

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum OrgNodeInterp {
  // Tells Rust how to interpret -- what to do with -- an OrgNode.
  // Each org node's relationship to its org-container is determined by which of these it is. Thus org-container can relate differently to its different org-children.
  Content(ContentNode),
  Aliases(Vec<String>),
}

/// Enums (safer than strings) corresponding to
/// the constructors of OrgNodeInterp.
#[derive(Debug, Clone, PartialEq)]
pub enum OrgNodeInterpEnum {
  ContentNode,
  Aliases, }

impl Default for OrgNodeInterpEnum {
  fn default() -> Self {
    OrgNodeInterpEnum::ContentNode }}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ContentNode {
  // See also /api.md.
  // The data that can be seen about a node in an Emacs buffer. Includes ephemeral view data ("folded", "focused", and "repeated"), and omits long-term data that a FileNode would include.
  // The same structure is used to send to and receive from Emacs. However, the `id` can only be `None` when receiving from Emacs.
  pub id       : Option<ID>,
  pub title    : String,         // See comment in the type `OrgNode`
  pub aliases  : Option<Vec<String>>, // aliases in the org-roam sense
  pub body     : Option<String>, // See comment in the type `OrgNode`
  pub folded   : bool,           // folded in the org-mode sense
  pub focused  : bool,           // where the Emacs cursor is
  pub repeated : bool, /* A node might appear in multiple places in a document. When Rust sends such a document, the second and later instances of such a node are marked "repeated". Their body and children are not displayed in Emacs. Moreover when Emacs sends them back to Rust, Rust should ignore any edits made under such repeated nodes. This permits handling infinite (recursive) data.

Both Rust and Emacs need to know this, because:

Emacs has to display repeated nodes differently, and report to Rust whether the node was repeated when saving.

Rust needs to save repeated nodes differently. It should ignore their content and changes to their text, because the single source of truth lies elsewhere in the view that Emacs sent Rust to save. */
  pub branches : Vec<OrgNodeInterp>, }

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct FileNode {
  // There is a 1-to-1 correspondence between FileNodes and actual files -- a file can be read to a FileNode, and a FileNode can be written to a file. The files are the only permanent data. FileNode is the format used to initialize the TypeDB and Tantivy databases.
  // Tantivy will receive some of this data, and TypeDB some other subset. Tantivy associates IDs with titles. TypeDB represents all the connections between nodes. At least one field, `body`, is known to neither database; it is instead read directly from the files on disk when Rust builds a document for Emacs.

  pub title: String,

  #[serde(default, skip_serializing_if = "Vec::is_empty")]
  pub aliases: Vec<String>,

  pub ids: Vec<ID>, // Must be nonempty. Can have length > 1 because nodes might be merged, but will usually have length = 1.
  // TODO: Use a nonempty list type (e.g. the "nonempty" crate), or else separate fields pid : String and extra_ids : Vec<String>. I'm leaning toward the latter, as the pid is special among those ids.

  #[serde(default, skip_serializing_if = "Option::is_none")]
  pub body: Option<String>, // Unknown to both Tantivy & TypeDB. The body is all text (if any) between the preceding org headline, to which it belongs, and the next (if there is a next).

  #[serde(default, skip_serializing_if = "Vec::is_empty")]
  pub contains: Vec<ID>,

  #[serde(default, skip_serializing_if = "Vec::is_empty")]
  pub subscribes_to: Vec<ID>,

  #[serde(default, skip_serializing_if = "Vec::is_empty")]
  pub hides_from_its_subscriptions: Vec<ID>,

  #[serde(default, skip_serializing_if = "Vec::is_empty")]
  pub overrides_view_of: Vec<ID>,
}

#[derive(Clone)]
pub struct TantivyIndex {
  // Associates titles and aliases to paths.
  pub index                  : Arc<Index>,
  pub id_field               : schema::Field,
  pub title_or_alias_field   : schema::Field,
}


//
// Functions
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

impl OrgNodeInterp {
  pub fn content (content_node: ContentNode) -> Self {
    OrgNodeInterp::Content (content_node) }
  pub fn aliases (alias_list: Vec<String>) -> Self {
    OrgNodeInterp::Aliases (alias_list) }
  pub fn is_content (&self) -> bool {
    matches! (self, OrgNodeInterp::Content(_)) }
  pub fn is_aliases (&self) -> bool {
    matches! (self, OrgNodeInterp::Aliases(_)) }}


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

    let interior = &text [5 .. text.len () - 2];

    if let Some ( idx ) = interior.find ( "][" ) {
      let id    = &interior [0..idx];
      let label = &interior [idx+2..];
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

pub fn filenode_example () -> FileNode {
  FileNode {
    title: "This text gets indexed.".to_string(),
    aliases: vec![],
    ids: vec![ ID::new("example") ],
    body: Some( r#"This one string could span pages.
It better be okay with newlines."# . to_string() ),
    contains: vec![ ID::new("1"),
                    ID::new("2"),
                    ID::new("3")],
    subscribes_to: vec![ID::new("11"),
                        ID::new("12"),
                        ID::new("13")],
    hides_from_its_subscriptions: vec![],
    overrides_view_of: vec![],
  } }
