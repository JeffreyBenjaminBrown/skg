// Tests for to_org util functions

use skg::to_org::util::get_skgid_from_viewnode_at;
use skg::types::viewnode::{Viewnode, ViewnodeKind, Vognode, ActiveVognode, default_activeVognode};
use skg::types::viewnode::PropertyFolder;
use skg::types::misc::{ID, SkgRepoName};
use ego_tree::{NodeId,Tree};

#[test]
fn test_get_skgid_from_viewnode_at_with_skgid() {
  // ActiveVognode with ID → returns the ID
  let skgid : ID =
    ID::new ("test-id-123");
  let t : ActiveVognode =
    default_activeVognode ( skgid . clone(),
                       SkgRepoName::from ("main"),
                       "Test" . to_string() );
  let viewnode : Viewnode =
    Viewnode { focused     : false,
              folded      : false,
              body_folded : false,
              kind        : ViewnodeKind::Vognode (Vognode::Active (t)) };
  let tree : Tree<Viewnode> = Tree::new (viewnode);
  let root_skgid : NodeId = tree . root() . id();
  let result : Result<ID, Box<dyn std::error::Error>> =
    get_skgid_from_viewnode_at(&tree, root_skgid);
  assert!(result . is_ok(), "Should successfully extract ID");
  assert_eq!(result . unwrap(), skgid);
}

#[test]
fn test_get_skgid_from_viewnode_at_non_vognode() {
  // Non-vognode → returns error
  let viewnode :
    Viewnode =
    Viewnode {
      focused     : false,
      folded       : false,
      body_folded : false,
      kind        : ViewnodeKind::PropertyFolder (
        PropertyFolder::Alias) };
  let tree : Tree<Viewnode> = Tree::new (viewnode);
  let root_skgid : NodeId = tree . root() . id();
  let result :
    Result<ID, Box<dyn std::error::Error>> =
    get_skgid_from_viewnode_at(&tree, root_skgid);
  assert!(result . is_err(), "Should fail for non-vognode node");
  assert!(result . unwrap_err() . to_string()
          . contains ("caller must pass a non-phantom vognode"));
}
