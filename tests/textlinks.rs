// Tests for server/textlinks.rs

use skg::types::textlinks::{
  textlinks_from_node,
  textlinks_from_text,
  replace_each_link_with_its_label,
};
use skg::types::textlinks::TextLink;
use skg::types::errors::TextLinkParseError;
use skg::types::misc::ID;
use skg::types::nodes::complete::{NodeComplete, empty_node_complete};

#[test]
fn test_textlink_to_string() {
  let textlink : TextLink =
    TextLink::new("abc123", "My TextLink");
  assert_eq!(textlink . to_string(), "[[id:abc123][My TextLink]]");
}

#[test]
fn test_textlink_from_str_valid() {
  let text : &str =
    "[[id:abc123][My TextLink]]";
  let textlink: TextLink = text . parse() . unwrap();
  assert_eq! ( textlink . id, "abc123" . into() );
  assert_eq! ( textlink . label, "My TextLink");
}

#[test]
fn test_textlink_from_str_invalid_format() {
  let text : &str =
    "abc123][My TextLink]]";
  let result : Result < TextLink, TextLinkParseError > =
    text . parse::<TextLink>();
  assert!(matches!(result, Err (TextLinkParseError::InvalidFormat)));
}

#[test]
fn test_textlink_from_str_missing_divider() {
  let text : &str =
    "[[id:abc123My TextLink]]";
  let result : Result < TextLink, TextLinkParseError > =
    text . parse::<TextLink>();
  assert!(matches!(result, Err (TextLinkParseError::MissingDivider)));
}

#[test]
fn test_roundtrip() {
  let original : TextLink =
    TextLink::new("846207ef-11d6-49e4-89b4-4558b2989a60",
                           "Some Note Title");
  let text : String =
    original . to_string();
  let parsed: TextLink = text . parse() . unwrap();
  assert_eq!(original, parsed);
}

#[test]
fn test_textlinks_from_text_empty() {
  let text : &str =
    "This text has no textlinks.";
  let textlinks : Vec < TextLink > =
    textlinks_from_text (text);
  assert_eq!(textlinks . len(), 0);
}

#[test]
fn test_textlinks_from_text_single() {
  let text : &str =
    "This text has one [[id:abc123][My TextLink]] in it.";
  let textlinks : Vec < TextLink > =
    textlinks_from_text (text);
  assert_eq! ( textlinks . len(), 1 ) ;
  assert_eq! ( textlinks[0] . id, "abc123" . into() );
  assert_eq! ( textlinks[0] . label, "My TextLink" ) ;
}

#[test]
fn test_textlinks_from_text_multiple() {
  let text : &str =
    "This text has [[id:abc123][First TextLink]] and [[id:def456][Second TextLink]] in it.";
  let textlinks : Vec < TextLink > =
    textlinks_from_text (text);
  assert_eq!(textlinks . len(), 2);
  assert_eq!(textlinks[0] . id, "abc123" . into() );
  assert_eq!(textlinks[0] . label, "First TextLink");
  assert_eq!(textlinks[1] . id, "def456" . into() );
  assert_eq!(textlinks[1] . label, "Second TextLink");
}

#[test]
fn test_textlinks_from_text_with_uuid() {
  let text : &str =
    "TextLink with UUID: [[id:846207ef-11d6-49e4-89b4-4558b2989a60][My UUID TextLink]]";
  let textlinks : Vec < TextLink > =
    textlinks_from_text (text);
  assert_eq!(textlinks . len(), 1);
  assert_eq!(
    textlinks[0] . id,
    "846207ef-11d6-49e4-89b4-4558b2989a60" . into() );
  assert_eq!(textlinks[0] . label, "My UUID TextLink");
}

#[test]
fn test_textlinks_from_text_with_nested_brackets() {
  let text : &str =
    "TextLink with nested brackets: [[id:abc123][TextLink [with] brackets]]";
  let textlinks : Vec < TextLink > =
    textlinks_from_text (text);
  assert_eq!(textlinks . len(), 1);
  assert_eq!(textlinks[0] . id, "abc123" . into() );
  assert_eq!(textlinks[0] . label, "TextLink [with] brackets");
}

#[test]
fn test_textlinks_from_node() {
  let mut test_node : NodeComplete =
    empty_node_complete ();
  { test_node . title = "Title with two textlinks: [[id:textlink1][First TextLink]] and [[id:textlink2][Second TextLink]]" . to_string();
    test_node . pid = ID::new ("id");
    test_node . body = Some("Some text with a link [[id:textlink3][Third TextLink]] and another [[id:textlink4][Fourth TextLink]]" . to_string()); }
  let textlinks : Vec < TextLink > =
    textlinks_from_node (&test_node);
  assert_eq!(textlinks . len(), 4);
  assert!(textlinks . iter()
          . any(|textlink| textlink . id == "textlink1" . into() &&
               textlink . label == "First TextLink"));
  assert!(textlinks . iter()
          . any(|textlink| textlink . id == "textlink2" . into() &&
               textlink . label == "Second TextLink"));
  assert!(textlinks . iter()
          . any(|textlink| textlink . id == "textlink3" . into() &&
               textlink . label == "Third TextLink"));
  assert!(textlinks . iter()
          . any(|textlink| textlink . id == "textlink4" . into() &&
               textlink . label == "Fourth TextLink"));
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
      replace_each_link_with_its_label (input);
    assert_eq! (
      result, expected,
      "Failed for input: '{}'. Expected: '{}', Got: '{}'",
      input, expected, result ); }}

/// (name, text, IDs of its real links) for each case in
/// tests/shared/literal-link-cases.txt, which the Emacs and Neovim
/// clients' tests read too.
fn shared_literal_link_cases () -> Vec<(String, String, Vec<String>)> {
  let path : std::path::PathBuf = std::path::Path::new (env! ("CARGO_MANIFEST_DIR"))
    . join ("tests/shared/literal-link-cases.txt");
  let mut cases : Vec<(String, String, Vec<String>)> = Vec::new ();
  let mut current : Option<(String, Vec<String>)> = None;
  for line in std::fs::read_to_string (&path) . unwrap () . lines () {
    if let Some (name) = line . strip_prefix ("==== ") {
      current = Some ((name . to_string (), Vec::new ()));
    } else if let Some (live) = line . strip_prefix ("---- live:") {
      let (name, text) : (String, Vec<String>) = current . take () . unwrap ();
      cases . push ((name, text . join ("\n"),
                     live . split_whitespace () . map (String::from) . collect ()));
    } else if let Some ((_, text)) = current . as_mut () {
      text . push (line . to_string ()); }}
  cases }

#[test]
fn shared_literal_link_cases_hold () {
  let cases = shared_literal_link_cases ();
  assert! (cases . len () > 5, "too few cases parsed");
  for (name, text, live) in cases {
    let found : Vec<String> = textlinks_from_text (&text) . into_iter ()
      . map (|link| link . id . 0) . collect ();
    assert_eq! (found, live, "case: {}", name); }
}

#[test]
fn label_replacement_skips_example_links () {
  assert_eq! ( replace_each_link_with_its_label (
                 "real [[id:a][A]] and =[[id:b][B]]=" ),
               "real A and =[[id:b][B]]=" );
}

#[test]
fn node_body_first_line_is_a_line_start () {
  // Were title and body joined on one line, the body's opening
  // '#+begin_example' would not start a line, and its link would count.
  let mut node : NodeComplete = empty_node_complete ();
  node . title = "title [[id:t][T]]" . to_string ();
  node . body = Some (
    "#+begin_example\n[[id:x][X]]\n#+end_example" . to_string () );
  assert_eq! ( textlinks_from_node (&node),
               vec! [ TextLink::new ("t", "T") ] );
}
