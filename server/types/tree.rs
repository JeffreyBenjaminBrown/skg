/// Type aliases for paired trees and utilities for working with ego_tree::Tree.
///
/// The SkgNode is optional because in some cases
/// it hasn't been fetched yet, and in others (e.g. an AliasCol)
/// there is no SkgNode corresponding to the OrgNode.

pub mod orgnode_skgnode;
pub mod generic;
pub mod generations;

use crate::types::orgnode::OrgNode;
use crate::types::orgnode_new::NewOrgNode;
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

//
// New types using NewOrgNode (transition period)
//

/// NewPairTree uses NewOrgNode instead of OrgNode.
/// During transition, both PairTree and NewPairTree coexist.
/// After Phase 7, NewPairTree becomes the only PairTree.
pub type NewPairTree = Tree<NewNodePair>;

#[derive(Clone, Debug)]
pub struct NewNodePair {
  pub mskgnode   : Option<SkgNode>,
  pub new_orgnode : NewOrgNode,
}

impl NewNodePair {
  /// Create a new NewNodePair with no SkgNode.
  pub fn from_orgnode ( orgnode : NewOrgNode ) -> Self {
    NewNodePair {
      mskgnode    : None,
      new_orgnode : orgnode,
    } }

  /// Create a new NewNodePair with an SkgNode.
  pub fn from_pair ( orgnode : NewOrgNode, skgnode : SkgNode ) -> Self {
    NewNodePair {
      mskgnode    : Some ( skgnode ),
      new_orgnode : orgnode,
    } } }

/// Convert a NodePair to a NewNodePair.
pub fn node_pair_to_new ( old : &NodePair ) -> NewNodePair {
  use crate::types::orgnode_new::from_old_orgnode;
  NewNodePair {
    mskgnode    : old . mskgnode . clone (),
    new_orgnode : from_old_orgnode ( &old . orgnode ),
  } }

/// Convert a NewNodePair back to a NodePair.
pub fn node_pair_from_new ( new : &NewNodePair ) -> NodePair {
  use crate::types::orgnode_new::to_old_orgnode;
  NodePair {
    mskgnode : new . mskgnode . clone (),
    orgnode  : to_old_orgnode ( &new . new_orgnode ),
  } }

//
// Tests for NodePair <-> NewNodePair conversions
//

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::misc::ID;
  use crate::types::orgnode::{default_metadata, Interp, OrgNode};
  use crate::types::orgnode_new::{
    EffectOnParent, NewOrgNode, OrgNodeKind, Scaffold, ScaffoldKind, TrueNode,
  };
  use crate::types::skgnode::SkgNode;

  // Test round-trip: NodePair -> NewNodePair -> NodePair
  #[test]
  fn test_node_pair_roundtrip_content () {
    let mut md = default_metadata ();
    md . id = Some ( ID::from ( "test123" ) );
    md . source = Some ( "test.skg" . to_string () );
    md . code . interp = Interp::Content;
    let orgnode = OrgNode {
      metadata : md,
      title    : "Test Node" . to_string (),
      body     : Some ( "Body text" . to_string () ),
    };
    let skgnode = SkgNode {
      ids      : vec![ ID::from ( "test123" ) ],
      title    : "Test Node" . to_string (),
      source   : "test" . to_string (),
      body     : Some ( "Body text" . to_string () ),
      contains : Some ( vec![] ),
      aliases  : Some ( vec![] ),
      subscribes_to                : None,
      hides_from_its_subscriptions : None,
      overrides_view_of            : None,
    };
    let old = NodePair {
      mskgnode : Some ( skgnode ),
      orgnode,
    };
    let new = node_pair_to_new ( &old );
    let back = node_pair_from_new ( &new );
    assert_eq! ( old . orgnode, back . orgnode );
    assert_eq! ( old . mskgnode, back . mskgnode );
  }

  #[test]
  fn test_node_pair_roundtrip_forest_root () {
    let mut md = default_metadata ();
    md . code . interp = Interp::ForestRoot;
    let orgnode = OrgNode {
      metadata : md,
      title    : String::new (),
      body     : None,
    };
    let old = NodePair {
      mskgnode : None,
      orgnode,
    };
    let new = node_pair_to_new ( &old );
    let back = node_pair_from_new ( &new );
    assert_eq! ( old . orgnode, back . orgnode );
    assert_eq! ( old . mskgnode, back . mskgnode );
  }

  #[test]
  fn test_node_pair_roundtrip_no_skgnode () {
    let mut md = default_metadata ();
    md . code . interp = Interp::AliasCol;
    let orgnode = OrgNode {
      metadata : md,
      title    : String::new (),
      body     : None,
    };
    let old = NodePair {
      mskgnode : None,
      orgnode,
    };
    let new = node_pair_to_new ( &old );
    let back = node_pair_from_new ( &new );
    assert_eq! ( old . orgnode, back . orgnode );
    assert! ( back . mskgnode . is_none () );
  }

  // Test round-trip: NewNodePair -> NodePair -> NewNodePair
  #[test]
  fn test_new_node_pair_roundtrip_true_node () {
    use crate::types::orgnode::OrgnodeViewData;
    // Note: NewOrgNode.focused must match TrueNode.view_data.focused
    // because during conversion they get synchronized.
    let mut view_data = OrgnodeViewData::default ();
    view_data . focused = true; // Match NewOrgNode.focused
    let true_node = TrueNode {
      title            : "Test" . to_string (),
      body             : Some ( "Body" . to_string () ),
      id               : Some ( ID::from ( "xyz" ) ),
      source           : Some ( "src.skg" . to_string () ),
      effect_on_parent : EffectOnParent::Content,
      indefinitive     : false,
      view_data,
      edit_request     : None,
      view_requests    : std::collections::HashSet::new (),
    };
    let new_orgnode = NewOrgNode {
      focused : true,
      folded  : false,
      kind    : OrgNodeKind::True ( true_node ),
    };
    let new = NewNodePair::from_orgnode ( new_orgnode . clone () );
    let old = node_pair_from_new ( &new );
    let back = node_pair_to_new ( &old );
    assert_eq! ( new . new_orgnode, back . new_orgnode );
  }

  #[test]
  fn test_new_node_pair_roundtrip_scaffold () {
    let scaffold = Scaffold {
      kind : ScaffoldKind::SubscribeeCol,
    };
    let new_orgnode = NewOrgNode {
      focused : false,
      folded  : true,
      kind    : OrgNodeKind::Scaff ( scaffold ),
    };
    let new = NewNodePair::from_orgnode ( new_orgnode . clone () );
    let old = node_pair_from_new ( &new );
    let back = node_pair_to_new ( &old );
    assert_eq! ( new . new_orgnode, back . new_orgnode );
  }

  // Test that NewNodePair::from_pair works
  #[test]
  fn test_new_node_pair_from_pair () {
    let true_node = TrueNode {
      title  : "Node" . to_string (),
      id     : Some ( ID::from ( "abc" ) ),
      ..TrueNode::default ()
    };
    let new_orgnode = NewOrgNode {
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
    let pair = NewNodePair::from_pair ( new_orgnode . clone (), skgnode . clone () );
    assert_eq! ( pair . new_orgnode, new_orgnode );
    assert_eq! ( pair . mskgnode, Some ( skgnode ) );
  }

  // Test that NewNodePair::from_orgnode works
  #[test]
  fn test_new_node_pair_from_orgnode () {
    let scaffold = Scaffold {
      kind : ScaffoldKind::ForestRoot,
    };
    let new_orgnode = NewOrgNode {
      focused : false,
      folded  : false,
      kind    : OrgNodeKind::Scaff ( scaffold ),
    };
    let pair = NewNodePair::from_orgnode ( new_orgnode . clone () );
    assert_eq! ( pair . new_orgnode, new_orgnode );
    assert! ( pair . mskgnode . is_none () );
  }
}
