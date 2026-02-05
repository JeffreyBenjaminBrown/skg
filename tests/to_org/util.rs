// Tests for to_org util functions

use skg::to_org::util::get_id_from_treenode;
use skg::types::viewnode::{ViewNode, ViewNodeKind, Scaffold, TrueNode, default_truenode};
use skg::types::misc::{ID, SourceName};
use ego_tree::{NodeId,Tree};

#[test]
fn test_get_id_from_treenode_with_id() {
  // TrueNode with ID → returns the ID
  let id : ID =
    ID::new("test-id-123");
  let t : TrueNode =
    default_truenode ( id.clone(),
                       SourceName::from("main"),
                       "Test".to_string() );
  let viewnode : ViewNode =
    ViewNode { focused : false,
              folded  : false,
              kind    : ViewNodeKind::True ( t ) };
  let tree : Tree<ViewNode> = Tree::new(viewnode);
  let root_id : NodeId = tree.root().id();
  let result : Result<ID, Box<dyn std::error::Error>> =
    get_id_from_treenode(&tree, root_id);
  assert!(result.is_ok(), "Should successfully extract ID");
  assert_eq!(result.unwrap(), id);
}

#[test]
fn test_get_id_from_treenode_scaffold() {
  // Scaffold → returns error
  let viewnode :
    ViewNode =
    ViewNode {
      focused : false,
      folded : false,
      kind : ViewNodeKind::Scaff(Scaffold::AliasCol),
    };
  let tree : Tree<ViewNode> = Tree::new(viewnode);
  let root_id : NodeId = tree.root().id();
  let result :
    Result<ID, Box<dyn std::error::Error>> =
    get_id_from_treenode(&tree, root_id);
  assert!(result.is_err(), "Should fail for Scaffold node");
  assert!(result.unwrap_err().to_string().contains("Scaffold"));
}
