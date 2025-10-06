// cargo test --test serve_containerward_view

use skg::save::parse_headline_from_sexp;
use skg::types::ID;

use std::error::Error;
use rand::prelude::*;

#[test]
fn test_sexp_parsing_generative() -> Result<(), Box<dyn Error>> {
  /* I was worried about quotation marks or unbalanced s-expressions inside of headlines confusing the parser. This test therefore generates 50 random (different each time) 2-character headlines from 'problem_chars'. */

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

    match parse_headline_from_sexp(&request) { // target of test
      Ok((headline_md, parsed_level, parsed_title)) => {
        assert_eq!(headline_md.id, Some(ID(test_id.clone())),
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
