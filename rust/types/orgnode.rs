use serde::{Serialize, Deserialize};
use std::str::FromStr;
use std::fmt;

use super::skgnode::NodeWithEphem;
use super::ID;

/// Raw text from Emacs is first loaded as a forest of these.
#[derive(Clone, Debug, PartialEq)]
pub struct OrgNode {
  pub title    : String,         // The title part of the headline (after asterisks and metadata). 'Headline' is a term from org-mode.
  pub body     : Option<String>, // "Body" is a term fron org-mode.
  pub branches : Vec<OrgNode>,
}

/* The RelToOrgParent serves two purposes:
It (via heralds) tells the reader how to interpret a node on screen,
and it tells Rust what to do with that node
when the use saves their data.
.
PITFALL: The name of this type is somewhat imprecise.
All orgnodes have a RelToOrgParent,
even though the top-level nodes have no parent.
Moreover, the SearchResult value of RelToOrgParent
does not describe the node's relationship to its org parent;
a search result is a search result, no matter where it is found.
I considered using the name 'NodeType'
but that seems even less helpful. */
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RelToOrgParent {

  Content, // Most nodes are this. Upon requesting a new content view, they all are. It means the org node's relationship to its org parent (if said parent exists) is that the parent 'contains' (in the sense defined in schema.tql) the child.
  Container, // This is the reverse of 'content'. If a node has this relationship, the node 'contains' (in the sense defined in schema.tql) its org-parent.
  Aliases, // If an alias node A has org-parent P and org-children C0, C1 .. Cn, then the headline of each of the Ci is an alias of P. Any other recursive content (bodies, grandchildren, etc.) of A is ignored.
  SearchResult, // When the user searches for title/alias text, each hit is one of these. If, somehow, Rust finds a SearchResult in a saved org buffer, it ignores it, including all of its recursive content.
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MetadataItem {
  // In metadata headers, some of these are bare values,
  // and some of them are key-value pairs.
  Repeated,
  Folded,
  Focused,
  Cycle,
  MightContainMore,
  ID (String),
  RelToOrgParent (RelToOrgParent), }

/// Parsed metadata from an org-mode headline
#[derive(Debug, Clone, PartialEq)]
pub struct OrgNodeMetadata {
  pub id: Option<ID>,
  pub repeated: bool,
  pub folded: bool,
  pub focused: bool,
  pub mightContainMore: bool,
  pub relToOrgParent: RelToOrgParent,
  pub metadata: Vec<MetadataItem>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum OrgNodeInterp {
  // Tells Rust how to interpret -- what to do with -- an OrgNode it receives from Emacs when the user runs `skg-request-save-buffer` (defined in the elisp code).
  // Each org node's relationship to its org-container is determined by which of these it is. Thus org-container can relate differently to its different org-children.
  Content (NodeWithEphem), // See the definition of RelToOrgParent.
  Aliases (Vec<String>),   // See the definition of RelToOrgParent.
  Ignored, }

//
// Implementations
//

impl Default for RelToOrgParent {
  fn default() -> Self {
    RelToOrgParent::Content }}

// String conversion implementations

impl fmt::Display for RelToOrgParent {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let s = match self {
      RelToOrgParent::Content => "content",
      RelToOrgParent::Aliases => "aliases",
      RelToOrgParent::SearchResult => "searchResult",
      RelToOrgParent::Container => "container",
    };
    write!(f, "{}", s) }}

impl FromStr for RelToOrgParent {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "content" => Ok(RelToOrgParent::Content),
      "aliases" => Ok(RelToOrgParent::Aliases),
      "searchResult" => Ok(RelToOrgParent::SearchResult),
      "container" => Ok(RelToOrgParent::Container),
      _ => Err(format!("Unknown type value: {}", s)), }} }

impl fmt::Display for MetadataItem {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      MetadataItem::Repeated => write!(f, "repeated"),
      MetadataItem::Folded => write!(f, "folded"),
      MetadataItem::Focused => write!(f, "focused"),
      MetadataItem::Cycle => write!(f, "cycle"),
      MetadataItem::MightContainMore => write!(f, "mightContainMore"),
      MetadataItem::ID(id) => write!(f, "id:{}", id),
      MetadataItem::RelToOrgParent(rel) => write!(f, "relToOrgParent:{}", rel), }} }

impl FromStr for MetadataItem {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "repeated" => Ok(MetadataItem::Repeated),
      "folded" => Ok(MetadataItem::Folded),
      "focused" => Ok(MetadataItem::Focused),
      "cycle" => Ok(MetadataItem::Cycle),
      "mightContainMore" => Ok(MetadataItem::MightContainMore),
      _ => {
        if let Some((key_str, value_str)) = s.split_once(':') {
          match key_str.trim() {
            "relToOrgParent" => {
              let relToOrgParent = RelToOrgParent::from_str(value_str.trim())?;
              Ok(MetadataItem::RelToOrgParent(relToOrgParent))
            },
            "id" => {
              Ok(MetadataItem::ID(value_str.trim().to_string()))
            },
            _ => Err(format!("Unknown metadata key: {}", key_str)),
          }
        } else {
          Err(format!("Unknown metadata value: {}", s))
        }} }} }

// Helper functions for easy access
impl MetadataItem {
  pub fn is_repeated(&self) -> bool {
    matches!(self, MetadataItem::Repeated)
  }

  pub fn is_folded(&self) -> bool {
    matches!(self, MetadataItem::Folded)
  }

  pub fn is_focused(&self) -> bool {
    matches!(self, MetadataItem::Focused) }

  pub fn is_cycle(&self) -> bool {
    matches!(self, MetadataItem::Cycle)
  }

  pub fn is_mightContainMore(&self) -> bool {
    matches!(self, MetadataItem::MightContainMore)
  }

  pub fn get_id(&self) -> Option<ID> {
    match self {
      MetadataItem::ID(id) => Some(ID(id.clone())),
      _ => None, }}

  pub fn get_relToOrgParent(&self) -> Option<&RelToOrgParent> {
    match self {
      MetadataItem::RelToOrgParent(relToOrgParent) => Some(relToOrgParent),
      _ => None, }} }

/// Find the first item in a generic MetaData collection
/// that matches a generic accessor function.
///
/// Examples:
/// - `find_in_metadata_collection(&metadata, MetadataItem::get_id)`
/// - `find_in_metadata_collection(&metadata, |item| if item.is_cycle() { Some(()) } else { None })`

pub fn find_in_metadata_collection<T, F>(
  collection : &[MetadataItem],
  accessor   : F,
) -> Option<T>
where
  F : Fn(&MetadataItem) -> Option<T>,
{ collection . iter() . find_map (accessor) }

/// Special case: find ID in MetaData collection.
pub fn find_id_in_metadata_collection(
  collection : &[MetadataItem],
) -> Option<ID>
{ find_in_metadata_collection (
  collection,
  |item| item . get_id() ) }

pub fn parse_metadata_from_string(
  metadata_str: &str
) -> Result<Vec<MetadataItem>, String> {
  let mut values = Vec::new();
  for part in metadata_str.split(',') {
    let trimmed = part.trim();
    if !trimmed.is_empty() {
      let value = MetadataItem::from_str(trimmed)?;
      values.push(value); }}
  Ok (values) }
