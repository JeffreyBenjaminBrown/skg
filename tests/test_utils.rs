use skg::test_utils::{
  compare_headlines_modulo_gnid, path_for_this_process, run_with_test_stores};

use std::any::Any;
use std::error::Error;
use std::panic::catch_unwind;
use std::path::PathBuf;

#[test]
fn test_compare_headlines_modulo_gnid() {
  // Test identical headlines
  assert!(compare_headlines_modulo_gnid(
    "* Title",
    "* Title"
  ));

  // Test headlines that differ only by ID
  assert!(compare_headlines_modulo_gnid(
    "* (skg node (gnid abc123)) Title",
    "* (skg node (gnid xyz789)) Title"
  ));

  // Test headlines where one has ID and other doesn't - should be unequal
  assert!(!compare_headlines_modulo_gnid(
    "* (skg node (gnid abc123)) Title",
    "* Title"
  ));

  // Test headlines with same other metadata but different IDs
  assert!(compare_headlines_modulo_gnid(
    "* (skg node (gnid abc)) Title",
    "* (skg node (gnid xyz)) Title"
  ));

  // Test headlines that differ by title
  assert!(!compare_headlines_modulo_gnid(
    "* (skg node (gnid abc)) Title One",
    "* (skg node (gnid xyz)) Title Two"
  ));

  // Test headlines that differ by level
  assert!(!compare_headlines_modulo_gnid(
    "* (skg node (gnid abc)) Title",
    "** (skg node (gnid xyz)) Title"
  ));

  // Test headlines that differ by other metadata
  assert!(!compare_headlines_modulo_gnid(
    "* (skg node (gnid abc)) Title",
    "* (skg alias) Title"
  ));

  // Test non-headlines
  assert!(compare_headlines_modulo_gnid(
    "This is body text",
    "This is body text"
  ));

  assert!(!compare_headlines_modulo_gnid(
    "This is body text",
    "This is different text"
  ));

  // Test mixed (one headline, one not)
  assert!(!compare_headlines_modulo_gnid(
    "* Title",
    "Body text" )); }

#[test]
fn test_strip_org_comments() {
  use skg::test_utils::strip_org_comments;

  // Test the example from the docs
  let input: &str = concat!(
    "  * (skg node (gnid 1)) title 1 # here's a comment\n",
    "  Here's a body. # and a body comment\n",
    "  ** (skg node (gnid 2)) title 2 # here's another");
  let expected: &str = concat!(
    "  * (skg node (gnid 1)) title 1\n",
    "  Here's a body.\n",
    "  ** (skg node (gnid 2)) title 2");
  assert_eq!(strip_org_comments (input), expected);

  // Test no comments
  let input_no_comments: &str = "  * title\n  ** subtitle";
  assert_eq!(strip_org_comments (input_no_comments), input_no_comments);

  // Test comment at start of line
  let input_start: &str = "# full line comment";
  assert_eq!(strip_org_comments (input_start), "");

  // Test multiple # symbols (only first should trigger comment)
  let input_multi: &str = "  * title # comment with # another hash";
  assert_eq!(strip_org_comments (input_multi), "  * title");

  // Test trailing whitespace before comment is removed
  let input_trailing: &str = "  * title   # comment";
  assert_eq!(strip_org_comments (input_trailing), "  * title");

  // Test empty string
  assert_eq!(strip_org_comments (""), "");

  // Test line with only whitespace and comment
  let input_whitespace: &str = "   # just a comment";
  assert_eq!(strip_org_comments (input_whitespace), "");
}

/// A store test that panics leaves no fixture copy behind:
/// 'run_with_test_stores' removes it before the panic resumes.
#[test]
fn a_store_test_that_panics_leaves_no_fixture_copy () {
  let test_name : &str = "skg-test-a-store-test-that-panics";
  let fixture_copy : PathBuf =
    path_for_this_process ( &format! ("/tmp/{}-fixtures", test_name) );
  let outcome : Result<Result<(), Box<dyn Error>>, Box<dyn Any + Send>> =
    catch_unwind ( || run_with_test_stores (
      test_name,
      "tests/save_dirty_conflicts/fixtures",
      "/tmp/tantivy-test-a-store-test-that-panics",
      |_config, _tantivy| Box::pin (async move {
        panic! ("the test fails") } ) ));
  assert! ( outcome . is_err (), "the panic resumes" );
  assert! ( ! fixture_copy . exists (),
            "{} was left behind", fixture_copy . display () ); }
