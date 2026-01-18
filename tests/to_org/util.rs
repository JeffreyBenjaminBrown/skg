// Tests for to_org util functions

use skg::to_org::util::get_pid_in_tree;
use skg::types::orgnode::{OrgNode, OrgNodeKind, TrueNode, Scaffold};
use skg::types::misc::ID;
use ego_tree::Tree;

#[test]
fn test_get_pid_in_tree_with_id() {
  // TrueNode with ID → returns the ID
  let id :
    ID =
    ID::new("test-id-123");
  let orgnode :
    OrgNode =
    OrgNode {
      focused : false,
      folded : false,
      kind : OrgNodeKind::True(TrueNode {
        title : "Test".to_string(),
        body : None,
        id_opt : Some(id.clone()),
        source_opt : None,
        parent_ignores : false,
        indefinitive : false,
        cycle : false,
        stats : Default::default(),
        edit_request : None,
        view_requests : Default::default(),
      }),
    };

  let tree :
    Tree<OrgNode> =
    Tree::new(orgnode);
  let root_id :
    ego_tree::NodeId =
    tree.root().id();

  let result :
    Result<ID, Box<dyn std::error::Error>> =
    get_pid_in_tree(&tree, root_id);

  assert!(result.is_ok(), "Should successfully extract ID");
  assert_eq!(result.unwrap(), id);
}

#[test]
fn test_get_pid_in_tree_no_id() {
  // TrueNode without ID → returns error
  let orgnode :
    OrgNode =
    OrgNode {
      focused : false,
      folded : false,
      kind : OrgNodeKind::True(TrueNode {
        title : "Test".to_string(),
        body : None,
        id_opt : None,
        source_opt : None,
        parent_ignores : false,
        indefinitive : false,
        cycle : false,
        stats : Default::default(),
        edit_request : None,
        view_requests : Default::default(),
      }),
    };

  let tree :
    Tree<OrgNode> =
    Tree::new(orgnode);
  let root_id :
    ego_tree::NodeId =
    tree.root().id();

  let result :
    Result<ID, Box<dyn std::error::Error>> =
    get_pid_in_tree(&tree, root_id);

  assert!(result.is_err(), "Should fail for node with no ID");
  assert!(result.unwrap_err().to_string().contains("node has no ID"));
}

#[test]
fn test_get_pid_in_tree_scaffold() {
  // Scaffold → returns error
  let orgnode :
    OrgNode =
    OrgNode {
      focused : false,
      folded : false,
      kind : OrgNodeKind::Scaff(Scaffold::AliasCol),
    };

  let tree :
    Tree<OrgNode> =
    Tree::new(orgnode);
  let root_id :
    ego_tree::NodeId =
    tree.root().id();

  let result :
    Result<ID, Box<dyn std::error::Error>> =
    get_pid_in_tree(&tree, root_id);

  assert!(result.is_err(), "Should fail for Scaffold node");
  assert!(result.unwrap_err().to_string().contains("Scaffold"));
}
