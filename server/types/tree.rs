/// Type aliases for paired trees and utilities for working with ego_tree::Tree.
///
/// The SkgNode is optional because in some cases
/// it hasn't been fetched yet, and in others (e.g. an AliasCol)
/// there is no SkgNode corresponding to the OrgNode.

pub mod orgnode_skgnode;
pub mod generic;
pub mod generations;

use crate::types::orgnode::OrgNode;
use crate::types::skgnode::SkgNode;
use ego_tree::Tree;


/// PairTree is the primary way an org-buffer is represented.
/// Most OrgNodes have an associated SkgNode, but not all,
/// which is why the SkgNode is optional.
/// Each PairTree comes from an OrgNode tree,
/// and eventually becomes one before being sent to Emacs,
/// but carrying the SkgNode between those two phases
/// lets us avoid redundant disk lookups.
pub type PairTree = Tree<NodePair>;

#[derive(Clone, Debug)]
pub struct NodePair {
  pub mskgnode: Option<SkgNode>,
  pub orgnode: OrgNode,
}

impl NodePair {
  /// Get the OrgNode.
  pub fn orgnode ( &self ) -> &OrgNode {
    &self . orgnode }

  /// Get a mutable reference to the OrgNode.
  pub fn orgnode_mut ( &mut self ) -> &mut OrgNode {
    &mut self . orgnode }

}

//
// NodePair constructors
//

impl NodePair {
  /// Create a new NodePair with no SkgNode.
  pub fn from_orgnode ( orgnode : OrgNode ) -> Self {
    NodePair {
      mskgnode : None,
      orgnode,
    } }

  /// Create a new NodePair with an SkgNode.
  pub fn from_pair ( orgnode : OrgNode, skgnode : SkgNode ) -> Self {
    NodePair {
      mskgnode : Some ( skgnode ),
      orgnode,
    } } }

//
// Tests for NodePair constructors
//

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::misc::ID;
  use crate::types::orgnode::{
    OrgNode, OrgNodeKind, Scaffold, ScaffoldKind, TrueNode,
  };
  use crate::types::skgnode::SkgNode;

  // Test that NodePair::from_pair works
  #[test]
  fn test_node_pair_from_pair () {
    let true_node = TrueNode {
      title  : "Node" . to_string (),
      id     : Some ( ID::from ( "abc" ) ),
      ..TrueNode::default ()
    };
    let new_orgnode = OrgNode {
      focused : false,
      folded  : false,
      kind    : OrgNodeKind::True ( true_node ),
    };
    let skgnode = SkgNode {
      ids      : vec![ ID::from ( "abc" ) ],
      title    : "Node" . to_string (),
      source   : "test" . to_string (),
      body     : None,
      contains : None,
      aliases  : None,
      subscribes_to                : None,
      hides_from_its_subscriptions : None,
      overrides_view_of            : None,
    };
    let pair = NodePair::from_pair ( new_orgnode . clone (), skgnode . clone () );
    assert_eq! ( pair . orgnode, new_orgnode );
    assert_eq! ( pair . mskgnode, Some ( skgnode ) );
  }

  // Test that NodePair::from_orgnode works
  #[test]
  fn test_node_pair_from_orgnode () {
    let scaffold = Scaffold {
      kind : ScaffoldKind::ForestRoot,
    };
    let new_orgnode = OrgNode {
      focused : false,
      folded  : false,
      kind    : OrgNodeKind::Scaff ( scaffold ),
    };
    let pair = NodePair::from_orgnode ( new_orgnode . clone () );
    assert_eq! ( pair . orgnode, new_orgnode );
    assert! ( pair . mskgnode . is_none () );
  }

  // Test orgnode accessor
  #[test]
  fn test_orgnode_accessor () {
    let true_node = TrueNode {
      title  : "Test" . to_string (),
      id     : Some ( ID::from ( "xyz" ) ),
      ..TrueNode::default ()
    };
    let new_orgnode = OrgNode {
      focused : true,
      folded  : false,
      kind    : OrgNodeKind::True ( true_node ),
    };
    let pair = NodePair::from_orgnode ( new_orgnode );
    assert_eq! ( pair . orgnode () . title (), "Test" );
    assert! ( pair . orgnode () . focused );
  }

  // Test orgnode_mut accessor
  #[test]
  fn test_orgnode_mut_accessor () {
    let scaffold = Scaffold {
      kind : ScaffoldKind::SubscribeeCol,
    };
    let new_orgnode = OrgNode {
      focused : false,
      folded  : false,
      kind    : OrgNodeKind::Scaff ( scaffold ),
    };
    let mut pair = NodePair::from_orgnode ( new_orgnode );
    pair . orgnode_mut () . focused = true;
    assert! ( pair . orgnode () . focused );
  }
}
