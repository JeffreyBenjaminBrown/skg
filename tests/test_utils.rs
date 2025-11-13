use skg::test_utils::compare_headlines_modulo_id;

#[test]
fn test_compare_headlines_modulo_id() {
  // Test identical headlines
  assert!(compare_headlines_modulo_id(
    "* Title",
    "* Title"
  ));

  // Test headlines that differ only by ID
  assert!(compare_headlines_modulo_id(
    "* (skg (id abc123)) Title",
    "* (skg (id xyz789)) Title"
  ));

  // Test headlines where one has ID and other doesn't - should be unequal
  assert!(!compare_headlines_modulo_id(
    "* (skg (id abc123)) Title",
    "* Title"
  ));

  // Test headlines with same other metadata but different IDs
  assert!(compare_headlines_modulo_id(
    "* (skg (id abc) (code (relToParent content))) Title",
    "* (skg (id xyz) (code (relToParent content))) Title"
  ));

  // Test headlines that differ by title
  assert!(!compare_headlines_modulo_id(
    "* (skg (id abc)) Title One",
    "* (skg (id xyz)) Title Two"
  ));

  // Test headlines that differ by level
  assert!(!compare_headlines_modulo_id(
    "* (skg (id abc)) Title",
    "** (skg (id xyz)) Title"
  ));

  // Test headlines that differ by other metadata
  assert!(!compare_headlines_modulo_id(
    "* (skg (id abc) (code (relToParent content))) Title",
    "* (skg (id xyz) (code (relToParent alias))) Title"
  ));

  // Test non-headlines
  assert!(compare_headlines_modulo_id(
    "This is body text",
    "This is body text"
  ));

  assert!(!compare_headlines_modulo_id(
    "This is body text",
    "This is different text"
  ));

  // Test mixed (one headline, one not)
  assert!(!compare_headlines_modulo_id(
    "* Title",
    "Body text" )); }

#[test]
fn test_strip_org_comments() {
  use skg::test_utils::strip_org_comments;

  // Test the example from the docs
  let input: &str = concat!(
    "  * (skg (id 1)) title 1 # here's a comment\n",
    "  Here's a body. # and a body comment\n",
    "  ** (skg (id 2)) title 2 # here's another");
  let expected: &str = concat!(
    "  * (skg (id 1)) title 1\n",
    "  Here's a body.\n",
    "  ** (skg (id 2)) title 2");
  assert_eq!(strip_org_comments(input), expected);

  // Test no comments
  let input_no_comments: &str = "  * title\n  ** subtitle";
  assert_eq!(strip_org_comments(input_no_comments), input_no_comments);

  // Test comment at start of line
  let input_start: &str = "# full line comment";
  assert_eq!(strip_org_comments(input_start), "");

  // Test multiple # symbols (only first should trigger comment)
  let input_multi: &str = "  * title # comment with # another hash";
  assert_eq!(strip_org_comments(input_multi), "  * title");

  // Test trailing whitespace before comment is removed
  let input_trailing: &str = "  * title   # comment";
  assert_eq!(strip_org_comments(input_trailing), "  * title");

  // Test empty string
  assert_eq!(strip_org_comments(""), "");

  // Test line with only whitespace and comment
  let input_whitespace: &str = "   # just a comment";
  assert_eq!(strip_org_comments(input_whitespace), "");
}
