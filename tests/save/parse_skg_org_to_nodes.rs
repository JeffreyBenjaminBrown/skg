use indoc::indoc;
use skg::save::orgfile_to_orgnodes::parse_skg_org_to_nodes;
use skg::types::{OrgNodeInterp, ID};


#[test]
fn test_parse_skg_org_to_nodes() {
  let input : &str =
    indoc! {"
            * <<id:root>> root
            ** <<id:a>> a
            *** aa
            *** <<type:aliases>> This headline is ignored.
                This body text should be ignored.
            **** alias 1
                 This body text should be ignored.
            ***** <<id:ignored>> This headline should be ignored.
            **** alias 2
            *** <<id:ab>> ab
            ** <<id:b>> b
        "};

  let parsed_nodes : Vec<OrgNodeInterp> =
    parse_skg_org_to_nodes (input);

  println! ("Parsed {} nodes", parsed_nodes.len ());

  // Verify the structure matches expected parsed_nodes
  assert_eq! (parsed_nodes.len (), 1, "Should have exactly one root node");

  let root = &parsed_nodes[0];
  if let OrgNodeInterp::Content (root_content) = root {
    assert_eq! (root_content.title, "root");
    assert_eq! (root_content.id, Some (ID::from ("root")));
    assert_eq! (root_content.aliases, None);
    assert_eq! (root_content.body, None);
    assert_eq! (root_content.folded, false);
    assert_eq! (root_content.focused, false);
    assert_eq! (root_content.repeated, false);
    assert_eq! (root_content.branches.len (), 2, "Root should have 2 children");

    // Check first child (node 'a')
    if let OrgNodeInterp::Content (node_a) = &root_content.branches[0] {
      assert_eq! (node_a.title, "a");
      println! ("node_a.title: {}", node_a.title);
      assert_eq! (node_a.id, Some (ID::from ("a")));
      assert_eq! (node_a.aliases, Some (vec!["alias 1".to_string (), "alias 2".to_string ()]));
      assert_eq! (node_a.body, None);
      assert_eq! (node_a.folded, false);
      assert_eq! (node_a.focused, false);
      assert_eq! (node_a.repeated, false);
      assert_eq! (node_a.branches.len (), 2, "Node 'a' should have 2 children");

      // Check first grandchild (node 'aa')
      if let OrgNodeInterp::Content (node_aa) = &node_a.branches[0] {
        assert_eq! (node_aa.title, "aa");
        assert_eq! (node_aa.id, None, "Node 'aa' should have no ID (None)");
        assert_eq! (node_aa.aliases, None);
        assert_eq! (node_aa.body, None);
        assert_eq! (node_aa.folded, false);
        assert_eq! (node_aa.focused, false);
        assert_eq! (node_aa.repeated, false);
        assert_eq! (node_aa.branches.len (), 0);
      } else {
        panic! ("Expected ContentNode for 'aa'");
      }

      // Check second grandchild (node 'ab')
      if let OrgNodeInterp::Content (node_ab) = &node_a.branches[1] {
        assert_eq! (node_ab.title, "ab");
        assert_eq! (node_ab.id, Some (ID::from ("ab")));
        assert_eq! (node_ab.aliases, None);
        assert_eq! (node_ab.body, None);
        assert_eq! (node_ab.folded, false);
        assert_eq! (node_ab.focused, false);
        assert_eq! (node_ab.repeated, false);
        assert_eq! (node_ab.branches.len (), 0);
      } else {
        panic! ("Expected ContentNode for 'ab'");
      }
    } else {
      panic! ("Expected ContentNode for node 'a'");
    }

    // Check second child (node 'b')
    if let OrgNodeInterp::Content (node_b) = &root_content.branches[1] {
      assert_eq! (node_b.title, "b");
      assert_eq! (node_b.id, Some (ID::from ("b")));
      assert_eq! (node_b.aliases, None);
      assert_eq! (node_b.body, None);
      assert_eq! (node_b.folded, false);
      assert_eq! (node_b.focused, false);
      assert_eq! (node_b.repeated, false);
      assert_eq! (node_b.branches.len (), 0);
    } else {
      panic! ("Expected ContentNode for node 'b'");
    }
  } else {
    panic! ("Expected ContentNode for root");
  }
}
