use regex::Regex;

use crate::types::{Hyperlink, FileNode};

pub fn hyperlinks_from_filenode (
  filenode : &FileNode )
  -> Vec<Hyperlink> {
  // All hyperlinks in its title
  // and (if present) its body.

  let combined_text = format!(
    "{}{}",
    filenode.title,
    filenode . body . as_deref () . unwrap_or ("") );
  hyperlinks_from_text ( &combined_text ) }

pub fn hyperlinks_from_text (
  text: &str )
  -> Vec <Hyperlink> {

  let hyperlink_pattern = Regex::new (
    // non-greedy .*? pattern avoids capturing too much.
    r"\[\[id:(.*?)\]\[(.*?)\]\]").unwrap();
  let mut hyperlinks = Vec::new ();
  for capture in hyperlink_pattern.captures_iter ( text ) {
    if capture.len () >= 3 { // capture group 0 is the entire match
      let id    = capture [1] . to_string ();
      let label = capture [2] . to_string ();
      hyperlinks.push (
        Hyperlink::new ( id, label )); }}
  hyperlinks }

pub fn replace_each_link_with_its_label (
  text : &str )
  -> String {
  // Replaces each hyperlink with that hyperlink's label.
  // Strips some text from each hyperlink while adding nothing.

  let hyperlink_re = Regex::new (
    r"\[\[.*?\]\[(.*?)\]\]") . unwrap (); // capture the label but not the ID
  let mut result = String::from ( text );
  let mut input_offset = 0; // offset in the input string
  for cap in hyperlink_re.captures_iter ( text ) {
    let whole_match     = cap . get (0) . unwrap ();
    let hyperlink_label = cap . get (1) . unwrap ();
    let start_pos = whole_match . start () - input_offset;
    let end_pos   = whole_match . end ()   - input_offset;
    result.replace_range ( // the replacement
      start_pos .. end_pos,
      hyperlink_label.as_str () );
    input_offset += whole_match.len ()
      - hyperlink_label.len (); }
  result }

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
      hides_from_its_subscriptions: vec![],
      overrides_view_of: vec![],
      nodepath: PathBuf::from("not important"),
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

  #[test]
  fn test_replace_each_link_with_its_label() {
    // Test cases: (input, expected_output)
    let test_cases = vec![
      ( ""                   , ""),
      ( "hello"              , "hello"),
      ( "[[id:yeah][label]]" , "label"),
      ( "0 [[id:1][a]] b [[id:2][c]] d",
        "0 a b c d"), ];
    for (input, expected) in test_cases {
      let result = replace_each_link_with_its_label ( input );
      assert_eq! (
        result, expected,
        "Failed for input: '{}'. Expected: '{}', Got: '{}'",
        input, expected, result ); }}
}
