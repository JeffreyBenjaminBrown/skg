use skg::text_to_orgnodes::uninterpreted::parse_headline_from_sexp;
use skg::types::{ID, find_id_in_metadata_collection};

use std::error::Error;
use rand::prelude::*;

#[test]
fn test_sexp_parsing_generative() -> Result<(), Box<dyn Error>> {
  // Characters that could befuddle S-expression parsing from Emacs
  let problem_chars = [' ', '"', '(', ')', '\'', '`'];

  let mut rng: ThreadRng = // for random numbers
    thread_rng();
  for i in 0..50 { // 50 random tests
    let test_id: String = format!("test{}", i);
    let char1: char = // random problem_char
      problem_chars[rng.gen_range(0..problem_chars.len())];
    let char2: char = // random problem_char
      problem_chars[rng.gen_range(0..problem_chars.len())];
    let title: String = // has just those two characters
      format!("Title{}with{}chars", char1, char2);
    let level: usize = rng.gen_range(1..=4); // random level in [1,4]
    let asterisks: String = "*".repeat(level);
    let headline: String =
      format!("{} <skg<id:{}>> {}", asterisks, test_id, title);
    let request: String = format!(
      "((request . \"containerward view\") (headline . {}))",
      format!("{:?}", headline)
      // Rust's debug format {:?} properly escapes strings.
    );

    // Test the parsing
    match parse_headline_from_sexp(&request) {
      Ok((metadata_items, parsed_level, parsed_title)) => {
        let node_id = find_id_in_metadata_collection(&metadata_items);

        assert_eq!(node_id, Some(ID(test_id.clone())),
                   "Failed to parse ID correctly for title: '{}' (headline: '{}')",
                   title, headline);
        assert_eq!(parsed_level, level,
                   "Failed to parse level correctly for title: '{}' (headline: '{}')",
                   title, headline);
        assert_eq!(parsed_title, title,
                   "Failed to parse title correctly for title: '{}' (headline: '{}')",
                   title, headline);
      },
      Err(e) => {
        panic!("Parsing failed for title: '{}' (headline: '{}') with error: {}",
               title, headline, e); }} }
  Ok (( )) }

#[test]
fn test_sexp_parsing_edge_cases() -> Result<(), Box<dyn Error>> {
  // Test some specific edge cases that are likely to cause problems
  let test_cases = vec![
    ("quote_test", "*** <skg<id:quote_test>> Text with \"quoted\" content"),
    ("paren_test", "** <skg<id:paren_test>> Text with (parentheses) here"),
    ("tick_test", "* <skg<id:tick_test>> Text with `backticks` here"),
    ("mixed_test", "** <skg<id:mixed_test>> Text with \"quotes\" and (parens) and `ticks`"),
    ("space_test", "* <skg<id:space_test>> Text   with   multiple   spaces"),
    ("single_quote_test", "* <skg<id:single_quote_test>> Text with single \" quote"),
    ("single_open_paren_test", "** <skg<id:single_open_paren_test>> Text with single ( paren"),
    ("single_close_paren_test", "*** <skg<id:single_close_paren_test>> Text with single ) paren"),
  ];

  for (test_id, headline) in test_cases {
    let request = format!(
      "((request . \"containerward view\") (headline . {}))",
      format!("{:?}", headline) // Use Rust's Debug format which properly escapes strings
    );

    match parse_headline_from_sexp(&request) {
      Ok((metadata_items, level, _title)) => {
        // Extract ID from metadata
        let node_id = find_id_in_metadata_collection(&metadata_items);

        assert_eq!(node_id, Some(ID(test_id.to_string())),
                   "Failed to parse ID correctly for edge case: '{}'", headline);
        // Level should match the number of asterisks at start
        let expected_level = headline.chars().take_while(|&c| c == '*').count();
        assert_eq!(level, expected_level,
                   "Failed to parse level correctly for edge case: '{}'", headline);
      },
      Err(e) => {
        panic!("Parsing failed for edge case headline: '{}' with error: {}",
               headline, e); }} }
  Ok (( )) }
