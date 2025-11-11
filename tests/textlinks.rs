// Tests for rust/textlinks.rs

use skg::media::textlinks::{
  textlinks_from_node,
  textlinks_from_text,
  replace_each_link_with_its_label,
};
use skg::types::{TextLink, TextLinkParseError, ID, SkgNode, empty_skgnode};

#[test]
fn test_textlink_to_string() {
  let textlink : TextLink =
    TextLink::new("abc123", "My TextLink");
  assert_eq!(textlink.to_string(), "[[id:abc123][My TextLink]]");
}

#[test]
fn test_textlink_from_str_valid() {
  let text : &str =
    "[[id:abc123][My TextLink]]";
  let textlink: TextLink = text.parse().unwrap();
  assert_eq! ( textlink.id, "abc123".into() );
  assert_eq! ( textlink.label, "My TextLink");
}

#[test]
fn test_textlink_from_str_invalid_format() {
  let text : &str =
    "abc123][My TextLink]]";
  let result : Result < TextLink, TextLinkParseError > =
    text.parse::<TextLink>();
  assert!(matches!(result, Err(TextLinkParseError::InvalidFormat)));
}

#[test]
fn test_textlink_from_str_missing_divider() {
  let text : &str =
    "[[id:abc123My TextLink]]";
  let result : Result < TextLink, TextLinkParseError > =
    text.parse::<TextLink>();
  assert!(matches!(result, Err(TextLinkParseError::MissingDivider)));
}

#[test]
fn test_roundtrip() {
  let original : TextLink =
    TextLink::new("846207ef-11d6-49e4-89b4-4558b2989a60",
                           "Some Note Title");
  let text : String =
    original.to_string();
  let parsed: TextLink = text.parse().unwrap();
  assert_eq!(original, parsed);
}

#[test]
fn test_textlinks_from_text_empty() {
  let text : &str =
    "This text has no textlinks.";
  let textlinks : Vec < TextLink > =
    textlinks_from_text(text);
  assert_eq!(textlinks.len(), 0);
}

#[test]
fn test_textlinks_from_text_single() {
  let text : &str =
    "This text has one [[id:abc123][My TextLink]] in it.";
  let textlinks : Vec < TextLink > =
    textlinks_from_text(text);
  assert_eq! ( textlinks.len(), 1 ) ;
  assert_eq! ( textlinks[0].id, "abc123".into() );
  assert_eq! ( textlinks[0].label, "My TextLink" ) ;
}

#[test]
fn test_textlinks_from_text_multiple() {
  let text : &str =
    "This text has [[id:abc123][First TextLink]] and [[id:def456][Second TextLink]] in it.";
  let textlinks : Vec < TextLink > =
    textlinks_from_text(text);
  assert_eq!(textlinks.len(), 2);
  assert_eq!(textlinks[0].id, "abc123".into() );
  assert_eq!(textlinks[0].label, "First TextLink");
  assert_eq!(textlinks[1].id, "def456".into() );
  assert_eq!(textlinks[1].label, "Second TextLink");
}

#[test]
fn test_textlinks_from_text_with_uuid() {
  let text : &str =
    "TextLink with UUID: [[id:846207ef-11d6-49e4-89b4-4558b2989a60][My UUID TextLink]]";
  let textlinks : Vec < TextLink > =
    textlinks_from_text(text);
  assert_eq!(textlinks.len(), 1);
  assert_eq!(
    textlinks[0].id,
    "846207ef-11d6-49e4-89b4-4558b2989a60".into() );
  assert_eq!(textlinks[0].label, "My UUID TextLink");
}

#[test]
fn test_textlinks_from_text_with_nested_brackets() {
  let text : &str =
    "TextLink with nested brackets: [[id:abc123][TextLink [with] brackets]]";
  let textlinks : Vec < TextLink > =
    textlinks_from_text(text);
  assert_eq!(textlinks.len(), 1);
  assert_eq!(textlinks[0].id, "abc123".into() );
  assert_eq!(textlinks[0].label, "TextLink [with] brackets");
}

#[test]
fn test_textlinks_from_node() {
  let mut test_node : SkgNode =
    empty_skgnode ();
  { test_node.title = "Title with two textlinks: [[id:textlink1][First TextLink]] and [[id:textlink2][Second TextLink]]" . to_string();
    test_node.ids = vec![ID::new("id")];
    test_node.body = Some("Some text with a link [[id:textlink3][Third TextLink]] and another [[id:textlink4][Fourth TextLink]]" . to_string()); }
  let textlinks : Vec < TextLink > =
    textlinks_from_node(&test_node);
  assert_eq!(textlinks.len(), 4);
  assert!(textlinks.iter()
          .any(|textlink| textlink.id == "textlink1".into() &&
               textlink.label == "First TextLink"));
  assert!(textlinks.iter()
          .any(|textlink| textlink.id == "textlink2".into() &&
               textlink.label == "Second TextLink"));
  assert!(textlinks.iter()
          .any(|textlink| textlink.id == "textlink3".into() &&
               textlink.label == "Third TextLink"));
  assert!(textlinks.iter()
          .any(|textlink| textlink.id == "textlink4".into() &&
               textlink.label == "Fourth TextLink"));
}

#[test]
fn test_replace_each_link_with_its_label() {
  // Test cases: (input, expected_output)
  let test_cases : Vec < ( &str, &str ) > =
    vec![
    ( ""                   , ""),
    ( "hello"              , "hello"),
    ( "[[id:yeah][label]]" , "label"),
    ( "0 [[id:1][a]] b [[id:2][c]] d",
      "0 a b c d"), ];
  for (input, expected) in test_cases {
    let result : String =
      replace_each_link_with_its_label ( input );
    assert_eq! (
      result, expected,
      "Failed for input: '{}'. Expected: '{}', Got: '{}'",
      input, expected, result ); }}
