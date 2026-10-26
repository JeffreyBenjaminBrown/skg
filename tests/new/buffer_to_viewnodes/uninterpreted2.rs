// cargo test uninterpreted2

use indoc::indoc;
use skg::from_text::buffer_to_viewnodes::add_missing_info::IdResolution;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_viewnode_tree;
use skg::types::misc::{ID, SkgrepoName};
use skg::types::viewnode::{
  Viewnode, ViewnodeKind, Vognode};
use ego_tree::Tree;

#[test]
fn test_org_to_uninterpreted_nodes2() {
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
                body of baa
            ** bb
        "};

  let viewforest: Tree<Viewnode> =
    org_to_viewnode_tree (input, IdResolution::AsWritten) . unwrap() . 0;

  let viewforest_roots: Vec<_> = viewforest . root() . children() . collect();
  assert_eq!(viewforest_roots . len(), 2, "Should have exactly 2 tree roots");

  // Test first tree (node 'a' and its children)
  let tree_a = &viewforest_roots[0];
  let node_a = tree_a . value();
  assert_eq!(node_a . title(), "a");
  assert_eq!(node_a . body(), None);

  // Test node 'a' has children in the tree
  let node_a_children: Vec<_> = tree_a . children() . collect();
  assert_eq!(node_a_children . len(), 2, "Node 'a' should have 2 children");
  assert_eq!(node_a_children[0] . value() . title(), "aa");
  assert_eq!(node_a_children[0] . value() . body(), Some(&"   body of aa" . to_string()));
  assert_eq!(node_a_children[1] . value() . title(), "ab");

  // Test second tree (node 'b' and its children)
  let tree_b = &viewforest_roots[1];
  let node_b = tree_b . value();
  assert_eq!(node_b . title(), "b");
  assert_eq!(node_b . body(), Some(&"  body of b" . to_string()));

  // Test node 'b' has children in the tree
  let node_b_children: Vec<_> = tree_b . children() . collect();
  assert_eq!(node_b_children . len(), 2, "Node 'b' should have 2 children");
  assert_eq!(node_b_children[0] . value() . title(), "ba");
  assert_eq!(node_b_children[1] . value() . title(), "bb");

  // Test deeper nesting - node 'ba' should have child 'baa'
  let node_ba_children: Vec<_> = node_b_children[0] . children() . collect();
  assert_eq!(node_ba_children . len(), 1, "Node 'ba' should have 1 child");
  assert_eq!(node_ba_children[0] . value() . title(), "baa");
  assert_eq!(node_ba_children[0] . value() . body(), Some(&"    body of baa" . to_string()));
}

#[test]
fn test_org_to_uninterpreted_nodes2_with_metadata() {
  let input: &str =
    indoc! {"
            * (skg focused node (gnid root)) root
            Root body content
            ** (skg folded node (gnid child1)) child1
            Child1 body
            * (skg node noClaim writeProtected) independent root node
            ParentIgnores body
            * (skg node (view cycle)) cycling node
            This node has cycle flag
        "};

  let viewforest: Tree<Viewnode> =
    org_to_viewnode_tree (input, IdResolution::AsWritten) . unwrap() . 0;

  // Get tree roots (children of BufferRoot)
  let tree_roots: Vec<_> = viewforest . root() . children() . collect();
  assert_eq!(tree_roots . len(), 3, "Should have exactly 3 trees");

  // Test root node with metadata
  let root_node = tree_roots[0] . value();
  assert_eq!(root_node . title(), "root");
  assert_eq!(root_node . body(), Some(&"Root body content" . to_string()));

  // Test independent root node
  let unrel_node = tree_roots[1] . value();
  let rel_t = match &unrel_node . kind {
    ViewnodeKind::Vognode (
      Vognode::Occurrence (t)) => t,
    _ => panic!("expected Occurrence") };
  assert_eq!(unrel_node . title(), "independent root node");
  assert_eq!(rel_t . claimsMembership, false);
  assert_eq!(rel_t . is_writeProtected (), true);
  assert_eq!(unrel_node . body(), None);

  // Test cycling node
  let cycle_node = tree_roots[2] . value();
  let cycle_t = match &cycle_node . kind {
    ViewnodeKind::Vognode (
      Vognode::Occurrence (t)) => t,
    _ => panic!("expected Occurrence") };
  assert_eq!(cycle_node . title(), "cycling node");
  // 'cycle' is a derived fact, in '(view ...)', which the parser skips.
  assert_eq!(cycle_t . viewStats . cycle, false);
  assert_eq!(cycle_node . body(),
             Some(&"This node has cycle flag" . to_string()));
}

#[test]
fn test_org_to_uninterpreted_nodes2_default_values() {
  let input: &str =
    indoc! {"
            * simple node
            Simple body
            * another node
        "};

  let viewforest: Tree<Viewnode> =
    org_to_viewnode_tree (input, IdResolution::AsWritten) . unwrap() . 0;

  let tree_roots: Vec<_> = viewforest . root() . children() . collect();
  assert_eq!(tree_roots . len(), 2);

  // Test first node - should have all default values except title and body
  let first_node = tree_roots[0] . value();
  let first_t = match &first_node . kind {
    ViewnodeKind::Vognode (
      Vognode::Occurrence (t)) => t,
    _ => panic!("expected Occurrence") };
  assert_eq!(first_node . title(), "simple node");
  assert_eq!(first_node . body(), Some(&"Simple body" . to_string()));
  assert!( ! first_t . gnid . 0 . is_empty (),
           "the parse mints a gnid for a headline without one" );
  assert_eq!(first_t . home_skgrepo, SkgrepoName::not_found (),
             "nothing supplies a top-level headline's skgrepo" );
  assert_eq!(first_t . viewStats . cycle, false);
  assert!(first_t . claimsMembership);
  assert_eq!(first_node . foldAndFocus . focused, false);
  assert_eq!(first_node . foldAndFocus . folded, false);
  assert_eq!(first_t . is_writeProtected (), false);
  assert_eq!(first_t . edit_request (), None);

  // Test second node - should have no body
  let second_node = tree_roots[1] . value();
  assert_eq!(second_node . title(), "another node");
  assert_eq!(second_node . body(), None);
}

#[test]
fn test_org_to_uninterpreted_nodes2_body_spacing() {
  let input: &str =
    indoc! {"
            * node with spaced body
              body with 2 spaces
            ** child
               child body with 3 spaces
            * node with complex body
            line 1
              line 2 with 2 spaces
                line 3 with 4 spaces
        "};

  let viewforest: Tree<Viewnode> =
    org_to_viewnode_tree (input, IdResolution::AsWritten) . unwrap() . 0;
  let tree_roots: Vec<_> = viewforest . root() . children() . collect();

  assert_eq!(tree_roots . len(), 2);

  // Test first node body spacing preservation
  let first_node = tree_roots[0] . value();
  assert_eq!(first_node . title(), "node with spaced body");
  assert_eq!(first_node . body(), Some(&"  body with 2 spaces" . to_string()));

  // Test second node complex body
  let second_node = tree_roots[1] . value();
  assert_eq!(second_node . title(), "node with complex body");
  let expected_body = "line 1\n  line 2 with 2 spaces\n    line 3 with 4 spaces";
  assert_eq!(second_node . body(), Some(&expected_body . to_string()));
}

#[test]
fn test_org_to_uninterpreted_nodes2_indented_star_is_body_text() {
  let input: &str =
    indoc! {"
            * (skg node (gnid afff132a-e822-46cb-aed4-5725b7d1b0c5) (skgRepos (nodeRepo public))) [#A] pressing*
              * = org-roam transplant first needs
            ** dogfood it
        "};

  let viewforest: Tree<Viewnode> =
    org_to_viewnode_tree (input, IdResolution::AsWritten) . unwrap() . 0;
  let tree_roots: Vec<_> = viewforest . root() . children() . collect();
  let root = tree_roots[0] . value();
  let root_children: Vec<_> = tree_roots[0] . children() . collect();

  assert_eq! (tree_roots . len(), 1);
  assert_eq! (root . title(), "[#A] pressing*");
  assert_eq! (
    root . body(),
    Some (&"  * = org-roam transplant first needs" . to_string()));
  assert_eq! (root_children . len(), 1);
  assert_eq! (root_children[0] . value() . title(), "dogfood it");
}

#[test]
fn test_org_to_uninterpreted_nodes2_basic_metadata() {
  let input: &str =
    indoc! {"
            * (skg folded node (gnid test)) simple node with metadata
            Node body
            * regular node without metadata
            Regular body
        "};

  let viewforest: Tree<Viewnode> =
    org_to_viewnode_tree (input, IdResolution::AsWritten) . unwrap() . 0;

  // Get tree roots (children of BufferRoot)
  let tree_roots: Vec<_> = viewforest . root() . children() . collect();
  assert_eq!(tree_roots . len(), 2);

  // Test node with metadata
  let meta_node = tree_roots[0] . value();
  assert_eq!(meta_node . title(), "simple node with metadata");
  assert_eq!(meta_node . body(), Some(&"Node body" . to_string()));
  assert_eq!(meta_node . occurrence_gnid (), Some(&ID::from ("test")));
  assert_eq!(meta_node . foldAndFocus . folded, true);

  // Test node without metadata (should have defaults)
  let regular_node = tree_roots[1] . value();
  assert_eq!(regular_node . title(), "regular node without metadata");
  assert!(regular_node . occurrence_gnid ()
          . is_some_and ( |gnid| ! gnid . 0 . is_empty ()
                                 && *gnid != ID::from ("test") ),
          "the parse mints a fresh gnid for a headline without one" );
  assert_eq!(regular_node . foldAndFocus . folded, false);
  assert_eq!(regular_node . body(), Some(&"Regular body" . to_string()));
}

#[test]
fn test_org_to_uninterpreted_nodes2_empty_input() {
  let input = "";
  let viewforest: Tree<Viewnode> =
    org_to_viewnode_tree (input, IdResolution::AsWritten) . unwrap() . 0;
  // BufferRoot should have no children
  assert_eq!(viewforest . root() . children() . count(), 0);

  let input2 = "   \n  \n  ";
  let trees2: Tree<Viewnode> =
    org_to_viewnode_tree (input2, IdResolution::AsWritten) . unwrap() . 0;
  assert_eq!(trees2 . root() . children() . count(), 0);
}

#[test]
fn test_org_to_uninterpreted_nodes2_only_text() {
  let input: &str =
    indoc! {"
            This is just text
            with no headlines
            at all
        "};

  let viewforest: Tree<Viewnode> =
    org_to_viewnode_tree (input, IdResolution::AsWritten) . unwrap() . 0;
  assert_eq!(viewforest . root() . children() . count(), 0,
             "Should have no tree roots when there are no headlines");
}

#[test]
fn test_org_to_uninterpreted_nodes2_invalid_metadata() {
  let _input: &str =
    indoc! {"
            * (skg invalidKey:value) invalid key
            * (skg node invalidValue) invalid value
            * (skg unknownFlag) unknown flag
        "};

  // Test invalid key in node
  let input_invalid_key = "* (skg node (invalidKey value)) invalid key";
  let result = org_to_viewnode_tree (input_invalid_key, IdResolution::AsWritten);
  assert!(result . is_err());
  assert!(result . unwrap_err() . contains ("Unknown metadata key: invalidKey"));

  // Test invalid non-vognode value
  let input_invalid_value = "* (skg invalidAtom) invalid value";
  let result = org_to_viewnode_tree (input_invalid_value, IdResolution::AsWritten);
  assert!(result . is_err());
  // Error message for unknown top-level element
  let err_msg = result . unwrap_err();
  assert!(err_msg . contains ("Unknown top-level value") && err_msg . contains ("invalidAtom"));

  // Test unknown flag
  let input_unknown_flag = "* (skg unknownFlag) unknown flag";
  let result = org_to_viewnode_tree (input_unknown_flag, IdResolution::AsWritten);
  assert!(result . is_err());
  // Error message for unknown top-level value
  let err_msg = result . unwrap_err();
  assert!(err_msg . contains ("Unknown top-level value") && err_msg . contains ("unknownFlag"));
}

#[test]
fn test_org_to_uninterpreted_nodes_rejects_forest_root() {
  let input: &str = "* (skg forestRoot) illegal root";
  let result = org_to_viewnode_tree (input, IdResolution::AsWritten);
  assert!(result . is_err());
  assert!(result . unwrap_err() . contains ("forestRoot metadata is internal"));
}

#[test]
fn test_org_to_uninterpreted_nodes2_orphaned_nodes() {
  // Test that nested nodes without a level 1 parent cause an error
  let input_orphaned: &str =
    indoc! {"
            ** orphaned level 2 node
            This should cause an error
        "};

  let result = org_to_viewnode_tree (input_orphaned, IdResolution::AsWritten);
  assert!(result . is_err());
  let error_msg = result . unwrap_err();
  assert!(error_msg . contains ("jumps too far between levels"));
  assert!(error_msg . contains ("level 2"));

  // Test that level 3 without proper parents also causes an error
  let input_level3_orphaned: &str =
    indoc! {"
            *** deeply orphaned level 3 node
        "};

  let result = org_to_viewnode_tree (input_level3_orphaned, IdResolution::AsWritten);
  assert!(result . is_err());
  let error_msg = result . unwrap_err();
  assert!(error_msg . contains ("jumps too far between levels"));
  assert!(error_msg . contains ("level 3"));
}
