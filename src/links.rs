use regex::Regex;
use std::error::Error;
use std::fmt;
use std::str::FromStr;
use uuid::Uuid;

pub fn random_org_roam_id() -> String {
  // Kind of silly --
  // giving this another name isn't necessary.
  Uuid::new_v4().to_string() }

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Link {
  pub id: String,
  pub label: String,
}

impl Link {
  pub fn new(id: impl Into<String>,
             label: impl Into<String>)
             -> Self {
    Link { id: id.into(),
           label: label.into(),
    } } }

impl fmt::Display for Link {
  /// Format: [[id:LINK_ID][LINK_LABEL]]
  fn fmt(&self,
         f: &mut fmt::Formatter<'_>)
         -> fmt::Result {
    write!(f, "[[id:{}][{}]]", self.id, self.label)
  } }

#[derive(Debug)]
pub enum LinkParseError {
  InvalidFormat,
  MissingDivider,
}

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
        id: id.to_string(),
        label: label.to_string(),
      } )
    } else {
      Err(LinkParseError::MissingDivider)
    } } }

pub fn extract_links (text: &str)
                      -> Vec<Link> {
  // The non-greedy pattern .*? prevents capturing too much.
  let link_pattern = Regex::new(
    r"\[\[id:(.*?)\]\[(.*?)\]\]").unwrap();
  let mut links = Vec::new();
  for capture in link_pattern.captures_iter(text) {
    if capture.len() >= 3 {
      let id = capture[1].to_string();
      let label = capture[2].to_string();
      links.push(Link::new(id, label)); } }
  links }

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_link_to_string() {
    let link = Link::new("abc123", "My Link");
    assert_eq!(link.to_string(), "[[id:abc123][My Link]]");
  }

  #[test]
  fn test_link_from_str_valid() {
    let text = "[[id:abc123][My Link]]";
    let link: Link = text.parse().unwrap();
    assert_eq!(link.id, "abc123");
    assert_eq!(link.label, "My Link");
  }

  #[test]
  fn test_link_from_str_invalid_format() {
    let text = "abc123][My Link]]";
    let result = text.parse::<Link>();
    assert!(matches!(result, Err(LinkParseError::InvalidFormat)));
  }

  #[test]
  fn test_link_from_str_missing_divider() {
    let text = "[[id:abc123My Link]]";
    let result = text.parse::<Link>();
    assert!(matches!(result, Err(LinkParseError::MissingDivider)));
  }

  #[test]
  fn test_roundtrip() {
    let original = Link::new("846207ef-11d6-49e4-89b4-4558b2989a60",
                             "Some Note Title");
    let text = original.to_string();
    let parsed: Link = text.parse().unwrap();
    assert_eq!(original, parsed);
  }

  #[test]
  fn test_extract_links_empty() {
    let text = "This text has no links.";
    let links = extract_links(text);
    assert_eq!(links.len(), 0);
  }

  #[test]
  fn test_extract_links_single() {
    let text = "This text has one [[id:abc123][My Link]] in it.";
    let links = extract_links(text);
    assert_eq!(links.len(), 1);
    assert_eq!(links[0].id, "abc123");
    assert_eq!(links[0].label, "My Link");
  }

  #[test]
  fn test_extract_links_multiple() {
    let text = "This text has [[id:abc123][First Link]] and [[id:def456][Second Link]] in it.";
    let links = extract_links(text);
    assert_eq!(links.len(), 2);
    assert_eq!(links[0].id, "abc123");
    assert_eq!(links[0].label, "First Link");
    assert_eq!(links[1].id, "def456");
    assert_eq!(links[1].label, "Second Link");
  }

  #[test]
  fn test_extract_links_with_uuid() {
    let text = "Link with UUID: [[id:846207ef-11d6-49e4-89b4-4558b2989a60][My UUID Link]]";
    let links = extract_links(text);
    assert_eq!(links.len(), 1);
    assert_eq!(links[0].id, "846207ef-11d6-49e4-89b4-4558b2989a60");
    assert_eq!(links[0].label, "My UUID Link");
  }

  #[test]
  fn test_extract_links_with_nested_brackets() {
    let text = "Link with nested brackets: [[id:abc123][Link [with] brackets]]";
    let links = extract_links(text);
    assert_eq!(links.len(), 1);
    assert_eq!(links[0].id, "abc123");
    assert_eq!(links[0].label, "Link [with] brackets");
  }
}
