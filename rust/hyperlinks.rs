use regex::Regex;
use uuid::Uuid;

use crate::types::{Hyperlink};

pub fn random_org_roam_id() -> String {
  // Kind of silly --
  // giving this another name isn't necessary.
  Uuid::new_v4().to_string() }

pub fn extract_hyperlinks (text: &str)
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
  use super::*;
  use crate::types::{HyperlinkParseError};

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
  fn test_extract_hyperlinks_empty() {
    let text = "This text has no hyperlinks.";
    let hyperlinks = extract_hyperlinks(text);
    assert_eq!(hyperlinks.len(), 0);
  }

  #[test]
  fn test_extract_hyperlinks_single() {
    let text = "This text has one [[id:abc123][My Hyperlink]] in it.";
    let hyperlinks = extract_hyperlinks(text);
    assert_eq! ( hyperlinks.len(), 1 ) ;
    assert_eq! ( hyperlinks[0].id, "abc123".into() );
    assert_eq! ( hyperlinks[0].label, "My Hyperlink" ) ;
  }

  #[test]
  fn test_extract_hyperlinks_multiple() {
    let text = "This text has [[id:abc123][First Hyperlink]] and [[id:def456][Second Hyperlink]] in it.";
    let hyperlinks = extract_hyperlinks(text);
    assert_eq!(hyperlinks.len(), 2);
    assert_eq!(hyperlinks[0].id, "abc123".into() );
    assert_eq!(hyperlinks[0].label, "First Hyperlink");
    assert_eq!(hyperlinks[1].id, "def456".into() );
    assert_eq!(hyperlinks[1].label, "Second Hyperlink");
  }

  #[test]
  fn test_extract_hyperlinks_with_uuid() {
    let text = "Hyperlink with UUID: [[id:846207ef-11d6-49e4-89b4-4558b2989a60][My UUID Hyperlink]]";
    let hyperlinks = extract_hyperlinks(text);
    assert_eq!(hyperlinks.len(), 1);
    assert_eq!(
      hyperlinks[0].id,
      "846207ef-11d6-49e4-89b4-4558b2989a60".into() );
    assert_eq!(hyperlinks[0].label, "My UUID Hyperlink");
  }

  #[test]
  fn test_extract_hyperlinks_with_nested_brackets() {
    let text = "Hyperlink with nested brackets: [[id:abc123][Hyperlink [with] brackets]]";
    let hyperlinks = extract_hyperlinks(text);
    assert_eq!(hyperlinks.len(), 1);
    assert_eq!(hyperlinks[0].id, "abc123".into() );
    assert_eq!(hyperlinks[0].label, "Hyperlink [with] brackets");
  }
}
