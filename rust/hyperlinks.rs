use regex::Regex;
use uuid::Uuid;

use crate::types::{Hyperlink, FileNode};

pub fn random_org_roam_id() -> String {
  // Kind of silly --
  // giving this another name isn't necessary.
  Uuid::new_v4().to_string() }

pub fn hyperlinks_from_filenode(
  filenode: &FileNode)
  -> Vec<Hyperlink> {
  let mut hyperlinks = Vec::new();
  hyperlinks.extend (
    hyperlinks_from_text ( &filenode.title ) );
  if let Some(text) = &filenode.body {
    hyperlinks.extend (
      hyperlinks_from_text ( text ) ); }
  hyperlinks }

pub fn hyperlinks_from_text (text: &str)
                      -> Vec<Hyperlink> {
  let hyperlink_pattern = Regex::new(
    // non-greedy .*? pattern avoids capturing too much.
    r"\[\[id:(.*?)\]\[(.*?)\]\]").unwrap();
  let mut hyperlinks = Vec::new();
  for capture in hyperlink_pattern.captures_iter(text) {
    if capture.len() >= 3 {
      let id = capture[1].to_string();
      let label = capture[2].to_string();
      hyperlinks.push(Hyperlink::new(id, label)); } }
  hyperlinks }

#[cfg(test)]
mod tests {
  use std::path::PathBuf;

  use super::*;
  use crate::types::{HyperlinkParseError, ID};

  #[test]
  fn test_hyperlink_to_string() {
    let hyperlink = Hyperlink::new("abc123", "My Hyperlink");
    assert_eq!(hyperlink.to_string(), "[[id:abc123][My Hyperlink]]");
  }

  #[test]
  fn test_hyperlink_from_str_valid() {
    let text = "[[id:abc123][My Hyperlink]]";
    let hyperlink: Hyperlink = text.parse().unwrap();
    assert_eq! ( hyperlink.id, "abc123".into() );
    assert_eq! ( hyperlink.label, "My Hyperlink");
  }

  #[test]
  fn test_hyperlink_from_str_invalid_format() {
    let text = "abc123][My Hyperlink]]";
    let result = text.parse::<Hyperlink>();
    assert!(matches!(result, Err(HyperlinkParseError::InvalidFormat)));
  }

  #[test]
  fn test_hyperlink_from_str_missing_divider() {
    let text = "[[id:abc123My Hyperlink]]";
    let result = text.parse::<Hyperlink>();
    assert!(matches!(result, Err(HyperlinkParseError::MissingDivider)));
  }

  #[test]
  fn test_roundtrip() {
    let original = Hyperlink::new("846207ef-11d6-49e4-89b4-4558b2989a60",
                             "Some Note Title");
    let text = original.to_string();
    let parsed: Hyperlink = text.parse().unwrap();
    assert_eq!(original, parsed);
  }

  #[test]
  fn test_hyperlinks_from_text_empty() {
    let text = "This text has no hyperlinks.";
    let hyperlinks = hyperlinks_from_text(text);
    assert_eq!(hyperlinks.len(), 0);
  }

  #[test]
  fn test_hyperlinks_from_text_single() {
    let text = "This text has one [[id:abc123][My Hyperlink]] in it.";
    let hyperlinks = hyperlinks_from_text(text);
    assert_eq! ( hyperlinks.len(), 1 ) ;
    assert_eq! ( hyperlinks[0].id, "abc123".into() );
    assert_eq! ( hyperlinks[0].label, "My Hyperlink" ) ;
  }

  #[test]
  fn test_hyperlinks_from_text_multiple() {
    let text = "This text has [[id:abc123][First Hyperlink]] and [[id:def456][Second Hyperlink]] in it.";
    let hyperlinks = hyperlinks_from_text(text);
    assert_eq!(hyperlinks.len(), 2);
    assert_eq!(hyperlinks[0].id, "abc123".into() );
    assert_eq!(hyperlinks[0].label, "First Hyperlink");
    assert_eq!(hyperlinks[1].id, "def456".into() );
    assert_eq!(hyperlinks[1].label, "Second Hyperlink");
  }

  #[test]
  fn test_hyperlinks_from_text_with_uuid() {
    let text = "Hyperlink with UUID: [[id:846207ef-11d6-49e4-89b4-4558b2989a60][My UUID Hyperlink]]";
    let hyperlinks = hyperlinks_from_text(text);
    assert_eq!(hyperlinks.len(), 1);
    assert_eq!(
      hyperlinks[0].id,
      "846207ef-11d6-49e4-89b4-4558b2989a60".into() );
    assert_eq!(hyperlinks[0].label, "My UUID Hyperlink");
  }

  #[test]
  fn test_hyperlinks_from_text_with_nested_brackets() {
    let text = "Hyperlink with nested brackets: [[id:abc123][Hyperlink [with] brackets]]";
    let hyperlinks = hyperlinks_from_text(text);
    assert_eq!(hyperlinks.len(), 1);
    assert_eq!(hyperlinks[0].id, "abc123".into() );
    assert_eq!(hyperlinks[0].label, "Hyperlink [with] brackets");
  }

  #[test]
  fn test_hyperlinks_from_filenode() {
    let test_node = FileNode {
      title: "Title with two hyperlinks: [[id:hyperlink1][First Hyperlink]] and [[id:hyperlink2][Second Hyperlink]]" . to_string(),
      ids: vec![ID::new("id")],
      body: Some("Some text with a link [[id:hyperlink3][Third Hyperlink]] and another [[id:hyperlink4][Fourth Hyperlink]]" . to_string()),
      contains: vec![],
      subscribes_to: vec![],
      hides_in_subscriptions: vec![],
      replaces_view_of: vec![],
      path: PathBuf::from("path"),
    };
    let hyperlinks = hyperlinks_from_filenode(&test_node);
    assert_eq!(hyperlinks.len(), 4);
    assert!(hyperlinks.iter()
            .any(|hyperlink| hyperlink.id == "hyperlink1".into() &&
                 hyperlink.label == "First Hyperlink"));
    assert!(hyperlinks.iter()
            .any(|hyperlink| hyperlink.id == "hyperlink2".into() &&
                 hyperlink.label == "Second Hyperlink"));
    assert!(hyperlinks.iter()
            .any(|hyperlink| hyperlink.id == "hyperlink3".into() &&
                 hyperlink.label == "Third Hyperlink"));
    assert!(hyperlinks.iter()
            .any(|hyperlink| hyperlink.id == "hyperlink4".into() &&
                 hyperlink.label == "Fourth Hyperlink"));
  }
}
