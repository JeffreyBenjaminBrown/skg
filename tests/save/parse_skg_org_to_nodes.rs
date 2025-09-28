use indoc::indoc;
use skg::text_to_orgnodes::parse_skg_org_to_nodes;
use skg::text_to_orgnodes::uninterpreted::parse_skg_org_to_uninterpreted_nodes;
use skg::types::{OrgNodeInterp, ID, OrgNode};


#[test]
fn test_parse_skg_org_to_nodes() {
  let input : &str =
    indoc! {"
            * <skg<id:root>> root
            ** <skg<id:a>> a
            *** aa
            *** <skg<type:aliases>> This headline is ignored.
                This body text should be ignored.
            **** alias 1
                 This body text should be ignored.
            ***** <skg<id:ignored>> This headline should be ignored.
            **** alias 2
            *** <skg<id:ab>> ab
            ** <skg<id:b>> b
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
        panic! ("Expected NodeWithEphem for 'aa'");
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
        panic! ("Expected NodeWithEphem for 'ab'");
      }
    } else {
      panic! ("Expected NodeWithEphem for node 'a'");
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
      panic! ("Expected NodeWithEphem for node 'b'");
    }
  } else {
    panic! ("Expected NodeWithEphem for root");
  }
}

#[test]
fn test_parse_skg_org_to_uninterpreted_nodes() {
  let input: &str =
    indoc! {"
            Ignored text.
            Ignored text.
            * a
            ** aa
               body of aa
            ** ab
            * b
              body of b
            ** ba
            *** baa
                body of aaa
            ** bb
        "};

  let parsed_nodes: Vec<OrgNode> =
    parse_skg_org_to_uninterpreted_nodes ( input );

  assert_eq! ( parsed_nodes.len (), 2,
               "Should have exactly 2 root nodes" );

  let node_a: &OrgNode =
    &parsed_nodes[0];
  assert_eq! ( node_a.title, "a" );
  assert_eq! ( node_a.body, None );
  assert_eq! ( node_a.branches.len (), 2 );

  let node_aa: &OrgNode =
    &node_a.branches[0];
  assert_eq! ( node_aa.title, "aa" );
  assert_eq! ( node_aa.body,
               Some ( "   body of aa".to_string () ) );
  assert_eq! ( node_aa.branches.len (), 0 );

  let node_ab: &OrgNode =
    &node_a.branches[1];
  assert_eq! ( node_ab.title, "ab" );
  assert_eq! ( node_ab.body, None );
  assert_eq! ( node_ab.branches.len (), 0 );

  let node_b: &OrgNode =
    &parsed_nodes[1];
  assert_eq! ( node_b.title, "b" );
  assert_eq! ( node_b.body,
               Some ( "  body of b".to_string () ) );
  assert_eq! ( node_b.branches.len (), 2 );

  let node_ba: &OrgNode =
    &node_b.branches[0];
  assert_eq! ( node_ba.title, "ba" );
  assert_eq! ( node_ba.body, None );
  assert_eq! ( node_ba.branches.len (), 1 );

  let node_baa: &OrgNode =
    &node_ba.branches[0];
  assert_eq! ( node_baa.title, "baa" );
  assert_eq! ( node_baa.body,
               Some ( "    body of aaa".to_string () ) );
  assert_eq! ( node_baa.branches.len (), 0 );

  let node_bb: &OrgNode =
    &node_b.branches[1];
  assert_eq! ( node_bb.title, "bb" );
  assert_eq! ( node_bb.body, None );
  assert_eq! ( node_bb.branches.len (), 0 );
}

#[test]
fn test_empty_aliases() {
  let input: &str =
    indoc! {"
            * <skg<id:a>> a has a child and no aliases.
            ** <skg<id:aa>> aa
            * <skg<id:b>> b has nothing.
            * <skg<id:c>> c has an alias and no children.
            ** <skg<type:aliases>> ignore these four words
                ignore these five words, too
            *** alias for c
            * <skg<id:d>> d has an alias and a child before it.
            ** a child with no id
            ** <skg<type:aliases>>
                Let's see if having no title after the metadata works.
            *** alias for d
            * <skg<id:e>> e has an alias and a child after it.
            ** <skg<type:aliases>>
                Let's see if having no title after the metadata works.
            *** alias for d
            ** another child with no id
            * <skg<id:f>> f has an empty but present alias list and a child before and after it.
            ** another child with no id
            ** <skg<type:aliases>> yadda yadda
                This is the main event. We should get Some( the empty vector ).
            ** another child with no id
        "};

  let parsed_nodes: Vec<OrgNodeInterp> =
    parse_skg_org_to_nodes(input);

  assert_eq!(parsed_nodes.len(), 6, "Should have exactly 6 root nodes");

  // Test node 'a': has one child and aliases are None
  if let OrgNodeInterp::Content(node_a) = &parsed_nodes[0] {
    assert_eq!(node_a.id, Some(ID::from("a")));
    assert_eq!(node_a.branches.len(), 1);
    assert_eq!(node_a.aliases, None);
  } else {
    panic!("Expected Content node for 'a'");
  }

  // Test node 'b': has no children and aliases are None
  if let OrgNodeInterp::Content(node_b) = &parsed_nodes[1] {
    assert_eq!(node_b.id, Some(ID::from("b")));
    assert_eq!(node_b.branches.len(), 0);
    assert_eq!(node_b.aliases, None);
  } else {
    panic!("Expected Content node for 'b'");
  }

  // Test node 'c': has no children but has an alias list Some(v) where length v = 1
  if let OrgNodeInterp::Content(node_c) = &parsed_nodes[2] {
    assert_eq!(node_c.id, Some(ID::from("c")));
    assert_eq!(node_c.branches.len(), 0);
    if let Some(aliases) = &node_c.aliases {
      assert_eq!(aliases.len(), 1);
    } else {
      panic!("Expected Some(aliases) for node 'c'");
    }
  } else {
    panic!("Expected Content node for 'c'");
  }

  // Test node 'd': has one child and an alias list Some(v) where length v = 1
  if let OrgNodeInterp::Content(node_d) = &parsed_nodes[3] {
    assert_eq!(node_d.id, Some(ID::from("d")));
    assert_eq!(node_d.branches.len(), 1);
    if let Some(aliases) = &node_d.aliases {
      assert_eq!(aliases.len(), 1);
    } else {
      panic!("Expected Some(aliases) for node 'd'");
    }
  } else {
    panic!("Expected Content node for 'd'");
  }

  // Test node 'e': has one child and an alias list Some(v) where length v = 1
  if let OrgNodeInterp::Content(node_e) = &parsed_nodes[4] {
    assert_eq!(node_e.id, Some(ID::from("e")));
    assert_eq!(node_e.branches.len(), 1);
    if let Some(aliases) = &node_e.aliases {
      assert_eq!(aliases.len(), 1);
    } else {
      panic!("Expected Some(aliases) for node 'e'");
    }
  } else {
    panic!("Expected Content node for 'e'");
  }

  // Test node 'f': has two children and an alias list Some(empty vector)
  if let OrgNodeInterp::Content(node_f) = &parsed_nodes[5] {
    assert_eq!(node_f.id, Some(ID::from("f")));
    assert_eq!(node_f.branches.len(), 2);
    if let Some(aliases) = &node_f.aliases {
      assert_eq!(aliases.len(), 0);
    } else {
      panic!("Expected Some(empty aliases) for node 'f'");
    }
  } else {
    panic!("Expected Content node for 'f'");
  }
}
