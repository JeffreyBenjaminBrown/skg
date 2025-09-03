use skg::save::orgfile_to_orgnodes::interpreted::metadata_inner_string_to_map_and_set;
use skg::save::orgfile_to_orgnodes::parse_skg_org_to_nodes;
use skg::types::{OrgNodeInterp, ContentNode};

#[allow(unused_imports)]
use indoc::indoc; // For a macro. The unused import checker ignores macro usage; hence the preceding `allow` directive.
use std::collections::{HashMap, HashSet};

#[cfg(test)]
mod tests {
  use super::*;

  // Helper function to extract ContentNode from OrgNodeInterp for cleaner test code
  fn extract_content_node(org_node: &OrgNodeInterp) -> &ContentNode {
    match org_node {
      OrgNodeInterp::Content(content_node) => content_node,
      OrgNodeInterp::Aliases(_) => panic!("Expected ContentNode, found AliasNode"),
    }
  }

  #[test]
  fn parses_sample() {
    let sample: &str = indoc! { r#"
  * <skg<id:1>> 1
  ** <skg<id:2>> 2, the first child of node 1, which has a body.
  The body of 2.
  *** <skg<id:3>> 3, the child of node 2, with no body.
  ** <skg<id:4>> 4, the second child of node 1, with no body.
  ** <skg<id:1,repeated>> 1
  The body of a repeated node is just a warning that it was repeated. We can ignore it.
  *** <skg<id:5>> Since this is a child of a repeated node, it is ignored!
  ** A headline with no id, the fourth child of 1.
"# };

    let forest: Vec<OrgNodeInterp> = parse_skg_org_to_nodes(sample);
    assert_eq!(forest.len(), 1);

    let n1 : &ContentNode =
      extract_content_node(&forest[0]);
    assert_eq!(n1.id.as_deref(), Some(&"1".to_string()));
    assert_eq!(n1.title, "1");
    assert_eq!(n1.branches.len(), 4);

    let n2 : &ContentNode =
      extract_content_node (&n1.branches[0]);
    assert_eq!(n2.id.as_deref(), Some(&"2".to_string()));
    assert!(n2.body.as_ref().unwrap().contains("The body of 2."));
    assert_eq!(n2.branches.len(), 1);
    let n2_child : &ContentNode =
      extract_content_node(&n2.branches[0]);
    assert_eq!(n2_child.id.as_deref(), Some(&"3".to_string()));

    let n4 : &ContentNode =
      extract_content_node(&n1.branches[1]);
    assert_eq!(n4.id.as_deref(), Some(&"4".to_string()));
    assert!(n4.body.is_none());
    assert!(n4.branches.is_empty());

    let repeated : &ContentNode =
      extract_content_node(&n1.branches[2]);
    assert_eq!(repeated.id.as_deref(), Some(&"1".to_string()));
    assert!(repeated.repeated);
    assert!(repeated.body.is_none());
    assert!(repeated.branches.is_empty());

    let n_no_id : &ContentNode =
      extract_content_node(&n1.branches[3]);
    assert_eq!(n_no_id.id, None);
    assert_eq!(n_no_id.title,
               "A headline with no id, the fourth child of 1.");
  }

  #[test]
  fn metadata_parser_examples() {
    let (m1, s1): (HashMap<String, String>, HashSet<String>) =
      metadata_inner_string_to_map_and_set("id:1, repeated");
    assert_eq!(m1.get("id").map(String::as_str), Some("1"));
    assert!(s1.contains("repeated"));

    let (m2, s2): (HashMap<String, String>, HashSet<String>) =
      metadata_inner_string_to_map_and_set("repeated:true, id:abc123");
    assert_eq!(m2.get("id").map(String::as_str), Some("abc123"));
    assert!(m2.contains_key("repeated"));
    assert!(s2.is_empty());

    let (m3, s3): (HashMap<String, String>, HashSet<String>) =
      metadata_inner_string_to_map_and_set("foo:bar, baz , qux: zip ");
    assert_eq!(m3.get("foo").map(String::as_str), Some("bar"));
    assert_eq!(m3.get("qux").map(String::as_str), Some("zip"));
    assert!(s3.contains("baz"));
  }

  #[test]
  fn parses_folded_and_focused() {
    let sample: &str = indoc! { r#"
      * <skg<id:1, folded>> Node 1 - folded
      ** <skg<id:2, focused>> Node 2 - focused
      *** <skg<id:3, folded, focused>> Node 3 - both folded and focused
      ** <skg<id:4>> Node 4 - neither
      "# };

    let forest: Vec<OrgNodeInterp> =
      parse_skg_org_to_nodes(sample);
    assert_eq!(forest.len(), 1);

    let n1: &ContentNode =
      extract_content_node(&forest[0]);
    assert_eq!(n1.id.as_deref(), Some(&"1".to_string()));
    assert!(n1.folded);
    assert!(!n1.focused);

    let n2: &ContentNode =
      extract_content_node(&n1.branches[0]);
    assert_eq!(n2.id.as_deref(), Some(&"2".to_string()));
    assert!(!n2.folded);
    assert!(n2.focused);

    let n3: &ContentNode =
      extract_content_node(&n2.branches[0]);
    assert_eq!(n3.id.as_deref(), Some(&"3".to_string()));
    assert!(n3.folded);
    assert!(n3.focused);

    let n4: &ContentNode =
      extract_content_node(&n1.branches[1]);
    assert_eq!(n4.id.as_deref(), Some(&"4".to_string()));
    assert!(!n4.folded);
    assert!(!n4.focused);
  }}
