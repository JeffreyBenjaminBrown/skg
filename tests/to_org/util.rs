// Tests for to_org util functions

use skg::to_org::util::get_id_from_treenode;
use skg::types::viewnode::{Viewnode, ViewnodeKind, Vognode, ActiveVognode, default_activeVognode};
use skg::types::viewnode::PropertyFolder;
use skg::types::misc::{ID, RepoName};
use ego_tree::{NodeId,Tree};

#[test]
fn test_get_id_from_treenode_with_id() {
  // ActiveVognode with ID → returns the ID
  let id : ID =
    ID::new ("test-id-123");
  let t : ActiveVognode =
    default_activeVognode ( id . clone(),
                       RepoName::from ("main"),
                       "Test" . to_string() );
  let viewnode : Viewnode =
    Viewnode { focused     : false,
              folded      : false,
              body_folded : false,
              kind        : ViewnodeKind::Vognode (Vognode::Active (t)) };
  let tree : Tree<Viewnode> = Tree::new (viewnode);
  let root_id : NodeId = tree . root() . id();
  let result : Result<ID, Box<dyn std::error::Error>> =
    get_id_from_treenode(&tree, root_id);
  assert!(result . is_ok(), "Should successfully extract ID");
  assert_eq!(result . unwrap(), id);
}

#[test]
fn test_get_id_from_treenode_non_vognode() {
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
  let root_id : NodeId = tree . root() . id();
  let result :
    Result<ID, Box<dyn std::error::Error>> =
    get_id_from_treenode(&tree, root_id);
  assert!(result . is_err(), "Should fail for non-vognode node");
  assert!(result . unwrap_err() . to_string()
          . contains ("caller must pass a non-phantom vognode"));
}
