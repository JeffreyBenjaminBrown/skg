use serde::{Serialize, Deserialize};
use std::str::FromStr;
use std::fmt;

use super::skgnode::NodeWithEphem;

/// Raw text from Emacs is first loaded as a forest of these.
#[derive(Clone, Debug, PartialEq)]
pub struct OrgNode {
  pub title    : String,         // The title part of the headline (after asterisks and metadata). 'Headline' is a term from org-mode.
  pub body     : Option<String>, // "body" is a term fron org-mode
  pub branches : Vec<OrgNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum OrgNodeType {
  Content, // most nodes are this
  Aliases,
  SearchResult, }

#[derive(Debug, Clone, PartialEq)]
pub enum MetadataItem {
  // In metadata headers, some of these are bare values,
  // and some of them are key-value pairs.
  Repeated,
  Folded,
  Focused,
  ID(String),
  Type(OrgNodeType), }

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum OrgNodeInterp {
  // Tells Rust how to interpret -- what to do with -- an OrgNode.
  // Each org node's relationship to its org-container is determined by which of these it is. Thus org-container can relate differently to its different org-children.
  Content(NodeWithEphem),
  Aliases(Vec<String>),
  Ignored, }

//
// Implementations
//

impl Default for OrgNodeType {
  fn default() -> Self {
    OrgNodeType::Content }}

// String conversion implementations

impl fmt::Display for OrgNodeType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let s = match self {
      OrgNodeType::Content => "content",
      OrgNodeType::Aliases => "aliases",
      OrgNodeType::SearchResult => "searchResult",
    };
    write!(f, "{}", s) }}

impl FromStr for OrgNodeType {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "content" => Ok(OrgNodeType::Content),
      "aliases" => Ok(OrgNodeType::Aliases),
      "searchResult" => Ok(OrgNodeType::SearchResult),
      _ => Err(format!("Unknown type value: {}", s)), }} }

impl fmt::Display for MetadataItem {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      MetadataItem::Repeated => write!(f, "repeated"),
      MetadataItem::Folded => write!(f, "folded"),
      MetadataItem::Focused => write!(f, "focused"),
      MetadataItem::ID(id) => write!(f, "id:{}", id),
      MetadataItem::Type(typ) => write!(f, "type:{}", typ), }} }

impl FromStr for MetadataItem {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "repeated" => Ok(MetadataItem::Repeated),
      "folded" => Ok(MetadataItem::Folded),
      "focused" => Ok(MetadataItem::Focused),
      _ => {
        if let Some((key_str, value_str)) = s.split_once(':') {
          match key_str.trim() {
            "type" => {
              let type_val = OrgNodeType::from_str(value_str.trim())?;
              Ok(MetadataItem::Type(type_val))
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

  pub fn get_id(&self) -> Option<&str> {
    match self {
      MetadataItem::ID(id) => Some(id),
      _ => None, }}

  pub fn get_type(&self) -> Option<&OrgNodeType> {
    match self {
      MetadataItem::Type(tv) => Some(tv),
      _ => None, }} }

/// Parse metadata string into Vec<MetadataItem>
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
