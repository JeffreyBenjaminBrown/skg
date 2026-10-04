// Tests for server/links.rs

use skg::types::links::{
  links_from_node,
  links_from_text,
  replace_each_link_with_its_label,
};
use skg::types::links::Link;
use skg::types::errors::LinkParseError;
use skg::types::misc::ID;
use skg::types::nodes::complete::{NodeComplete, empty_node_complete};

#[test]
fn test_link_to_string() {
  let link : Link =
    Link::new("abc123", "My Link");
  assert_eq!(link . to_string(), "[[id:abc123][My Link]]");
}

#[test]
fn test_link_from_str_valid() {
  let text : &str =
    "[[id:abc123][My Link]]";
  let link: Link = text . parse() . unwrap();
  assert_eq! ( link . id, "abc123" . into() );
  assert_eq! ( link . label, "My Link");
}

#[test]
fn test_link_from_str_invalid_format() {
  let text : &str =
    "abc123][My Link]]";
  let result : Result < Link, LinkParseError > =
    text . parse::<Link>();
  assert!(matches!(result, Err (LinkParseError::InvalidFormat)));
}

#[test]
fn test_link_from_str_missing_divider() {
  let text : &str =
    "[[id:abc123My Link]]";
  let result : Result < Link, LinkParseError > =
    text . parse::<Link>();
  assert!(matches!(result, Err (LinkParseError::MissingDivider)));
}

#[test]
fn test_roundtrip() {
  let original : Link =
    Link::new("846207ef-11d6-49e4-89b4-4558b2989a60",
                           "Some Note Title");
  let text : String =
    original . to_string();
  let parsed: Link = text . parse() . unwrap();
  assert_eq!(original, parsed);
}

#[test]
fn test_links_from_text_empty() {
  let text : &str =
    "This text has no links.";
  let links : Vec < Link > =
    links_from_text (text);
  assert_eq!(links . len(), 0);
}

#[test]
fn test_links_from_text_single() {
  let text : &str =
    "This text has one [[id:abc123][My Link]] in it.";
  let links : Vec < Link > =
    links_from_text (text);
  assert_eq! ( links . len(), 1 ) ;
  assert_eq! ( links[0] . id, "abc123" . into() );
  assert_eq! ( links[0] . label, "My Link" ) ;
}

#[test]
fn test_links_from_text_multiple() {
  let text : &str =
    "This text has [[id:abc123][First Link]] and [[id:def456][Second Link]] in it.";
  let links : Vec < Link > =
    links_from_text (text);
  assert_eq!(links . len(), 2);
  assert_eq!(links[0] . id, "abc123" . into() );
  assert_eq!(links[0] . label, "First Link");
  assert_eq!(links[1] . id, "def456" . into() );
  assert_eq!(links[1] . label, "Second Link");
}

#[test]
fn test_links_from_text_with_uuid() {
  let text : &str =
    "Link with UUID: [[id:846207ef-11d6-49e4-89b4-4558b2989a60][My UUID Link]]";
  let links : Vec < Link > =
    links_from_text (text);
  assert_eq!(links . len(), 1);
  assert_eq!(
    links[0] . id,
    "846207ef-11d6-49e4-89b4-4558b2989a60" . into() );
  assert_eq!(links[0] . label, "My UUID Link");
}

#[test]
fn test_links_from_text_with_nested_brackets() {
  let text : &str =
    "Link with nested brackets: [[id:abc123][Link [with] brackets]]";
  let links : Vec < Link > =
    links_from_text (text);
  assert_eq!(links . len(), 1);
  assert_eq!(links[0] . id, "abc123" . into() );
  assert_eq!(links[0] . label, "Link [with] brackets");
}

#[test]
fn test_links_from_node() {
  let mut test_node : NodeComplete =
    empty_node_complete ();
  { test_node . title = "Title with two links: [[id:link1][First Link]] and [[id:link2][Second Link]]" . to_string();
    test_node . pid = ID::new ("id");
    test_node . body = Some("Some text with a link [[id:link3][Third Link]] and another [[id:link4][Fourth Link]]" . to_string()); }
  let links : Vec < Link > =
    links_from_node (&test_node);
  assert_eq!(links . len(), 4);
  assert!(links . iter()
          . any(|link| link . id == "link1" . into() &&
               link . label == "First Link"));
  assert!(links . iter()
          . any(|link| link . id == "link2" . into() &&
               link . label == "Second Link"));
  assert!(links . iter()
          . any(|link| link . id == "link3" . into() &&
               link . label == "Third Link"));
  assert!(links . iter()
          . any(|link| link . id == "link4" . into() &&
               link . label == "Fourth Link"));
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
    let found : Vec<String> = links_from_text (&text) . into_iter ()
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
  assert_eq! ( links_from_node (&node),
               vec! [ Link::new ("t", "T") ] );
}
