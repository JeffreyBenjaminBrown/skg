/// See ./orgnode.rs for what all fields mean.
/// This file defines unchecked variants of some of the types
/// from ./orgnode.rs, where 'unchecked' means
/// they might not have an ID or a source field.
/// They are only needed for a little while after reading in
/// a buffer from the client. After validation, they are converted
/// to the usual checked type.

use super::git::NodeDiffStatus;
use super::misc::ID;
use super::orgnode::{
  OrgNode, OrgNodeKind, TrueNode, Scaffold,
  GraphNodeStats, ViewNodeStats, EditRequest, ViewRequest,
};
use ego_tree::{Tree, NodeId, NodeMut};
use std::collections::HashSet;

//
// Type declarations
//

/// Unchecked version of OrgNode - for parsing/validation phase.
#[derive(Debug, Clone, PartialEq)]
pub struct UncheckedOrgNode {
  pub focused : bool,
  pub folded  : bool,
  pub kind    : UncheckedOrgNodeKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UncheckedOrgNodeKind {
  True  ( UncheckedTrueNode ),
  Scaff ( Scaffold ),  // Scaffold is shared - scaffolds never have IDs
}

/// Unchecked version of TrueNode - id and source are optional.
#[derive(Debug, Clone, PartialEq)]
pub struct UncheckedTrueNode {
  pub title          : String,
  pub body           : Option<String>,
  pub id_opt         : Option<ID>,       // optional during parsing
  pub source_opt     : Option<String>,   // optional during parsing
  pub parent_ignores : bool,
  pub indefinitive   : bool,
  pub graphStats     : GraphNodeStats,
  pub viewStats      : ViewNodeStats,
  pub edit_request   : Option<EditRequest>,
  pub view_requests  : HashSet<ViewRequest>,
  pub diff           : Option<NodeDiffStatus>,
}

//
// Conversion implementations
//

impl TryFrom<UncheckedTrueNode> for TrueNode {
  type Error = String;

  fn try_from(u: UncheckedTrueNode) -> Result<Self, Self::Error> {
    let id = u.id_opt.ok_or_else(
      || format!("Node '{}' has no ID", u.title))?;
    let source = u.source_opt.ok_or_else(
      || format!("Node '{}' has no source", u.title))?;
    Ok(TrueNode {
      title          : u.title,
      body           : u.body,
      id,
      source,
      parent_ignores : u.parent_ignores,
      indefinitive   : u.indefinitive,
      graphStats     : u.graphStats,
      viewStats      : u.viewStats,
      edit_request   : u.edit_request,
      view_requests  : u.view_requests,
      diff           : u.diff,
    })
  }
}

impl TryFrom<UncheckedOrgNodeKind> for OrgNodeKind {
  type Error = String;

  fn try_from(u: UncheckedOrgNodeKind) -> Result<Self, Self::Error> {
    match u {
      UncheckedOrgNodeKind::True(t) =>
        Ok(OrgNodeKind::True(TrueNode::try_from(t)?)),
      UncheckedOrgNodeKind::Scaff(s) =>
        Ok(OrgNodeKind::Scaff(s)),
    }
  }
}

impl TryFrom<UncheckedOrgNode> for OrgNode {
  type Error = String;

  fn try_from(u: UncheckedOrgNode) -> Result<Self, Self::Error> {
    Ok(OrgNode {
      focused : u.focused,
      folded  : u.folded,
      kind    : OrgNodeKind::try_from(u.kind)?,
    })
  }
}

// Infallible conversions from checked to unchecked types.

impl From<TrueNode> for UncheckedTrueNode {
  fn from(t: TrueNode) -> Self {
    UncheckedTrueNode {
      title          : t.title,
      body           : t.body,
      id_opt         : Some(t.id),
      source_opt     : Some(t.source),
      parent_ignores : t.parent_ignores,
      indefinitive   : t.indefinitive,
      graphStats     : t.graphStats,
      viewStats      : t.viewStats,
      edit_request   : t.edit_request,
      view_requests  : t.view_requests,
      diff           : t.diff,
    }
  }
}

impl From<OrgNodeKind> for UncheckedOrgNodeKind {
  fn from(k: OrgNodeKind) -> Self {
    match k {
      OrgNodeKind::True(t) =>
        UncheckedOrgNodeKind::True(UncheckedTrueNode::from(t)),
      OrgNodeKind::Scaff(s) =>
        UncheckedOrgNodeKind::Scaff(s),
    }
  }
}

impl From<OrgNode> for UncheckedOrgNode {
  fn from(o: OrgNode) -> Self {
    UncheckedOrgNode {
      focused : o.focused,
      folded  : o.folded,
      kind    : UncheckedOrgNodeKind::from(o.kind),
    }
  }
}

/// In the unchecked type, id and source are optional.
/// This returns an error if any TrueNode is missing either field.
/// Validation happens before this, so it should not error.
pub fn unchecked_to_checked_tree(
  unchecked: Tree<UncheckedOrgNode>
) -> Result<Tree<OrgNode>, String> {
  // Convert the root node
  let root_unchecked: &UncheckedOrgNode = unchecked.root().value();
  let root_checked: OrgNode = OrgNode::try_from(root_unchecked.clone())?;
  let mut checked: Tree<OrgNode> = Tree::new(root_checked);

  // Recursively convert children
  fn convert_children(
    unchecked_tree : &Tree<UncheckedOrgNode>,
    checked_tree   : &mut Tree<OrgNode>,
    unchecked_id   : NodeId,
    checked_id     : NodeId,
  ) -> Result<(), String> {
    let child_ids: Vec<NodeId> = unchecked_tree
      .get(unchecked_id)
      .unwrap()
      .children()
      .map(|c| c.id())
      .collect();

    for unchecked_child_id in child_ids {
      let unchecked_child: &UncheckedOrgNode = unchecked_tree
        .get(unchecked_child_id)
        .unwrap()
        .value();
      let checked_child: OrgNode =
        OrgNode::try_from(unchecked_child.clone())?;

      let checked_child_id: NodeId = {
        let mut parent_mut: NodeMut<OrgNode> =
          checked_tree.get_mut(checked_id).unwrap();
        parent_mut.append(checked_child).id()
      };

      convert_children(
        unchecked_tree,
        checked_tree,
        unchecked_child_id,
        checked_child_id,
      )?;
    }
    Ok(())
  }

  let unchecked_root_id: NodeId = unchecked.root().id();
  let checked_root_id: NodeId = checked.root().id();
  convert_children(&unchecked, &mut checked, unchecked_root_id, checked_root_id)?;

  Ok(checked)
}

/// Convert a checked OrgNode tree to an UncheckedOrgNode tree.
/// Infallible since checked types always satisfy unchecked requirements.
pub fn checked_to_unchecked_tree(
  checked: &Tree<OrgNode>
) -> Tree<UncheckedOrgNode> {
  fn convert_children(
    checked_tree   : &Tree<OrgNode>,
    unchecked_tree : &mut Tree<UncheckedOrgNode>,
    checked_id     : NodeId,
    unchecked_id   : NodeId,
  ) {
    let child_ids : Vec<NodeId> =
      checked_tree
      .get(checked_id)
      .unwrap()
      .children()
      .map(|c| c.id())
      .collect();

    for checked_child_id in child_ids {
      let checked_child : &OrgNode =
        checked_tree
        .get(checked_child_id)
        .unwrap()
        .value();
      let unchecked_child : UncheckedOrgNode =
        UncheckedOrgNode::from(checked_child.clone());

      let unchecked_child_id : NodeId =
        { let mut parent_mut : NodeMut<UncheckedOrgNode> =
           unchecked_tree.get_mut(unchecked_id).unwrap();
         parent_mut.append(unchecked_child).id() };

      convert_children(
        checked_tree,
        unchecked_tree,
        checked_child_id,
        unchecked_child_id );
    }
  }

  let root_checked : &OrgNode =
    checked.root().value();
  let root_unchecked : UncheckedOrgNode =
    UncheckedOrgNode::from(root_checked.clone());
  let mut unchecked : Tree<UncheckedOrgNode> =
    Tree::new(root_unchecked);

  let checked_root_id : NodeId =
    checked.root().id();
  let unchecked_root_id : NodeId =
    unchecked.root().id();
  convert_children(&checked, &mut unchecked, checked_root_id, unchecked_root_id);

  unchecked
}

//
// Defaults
//

impl Default for UncheckedTrueNode {
  fn default() -> Self {
    UncheckedTrueNode {
      title          : String::new(),
      body           : None,
      id_opt         : None,
      source_opt     : None,
      parent_ignores : false,
      indefinitive   : false,
      graphStats     : GraphNodeStats::default(),
      viewStats      : ViewNodeStats::default(),
      edit_request   : None,
      view_requests  : HashSet::new(),
      diff           : None,
    }
  }
}

impl Default for UncheckedOrgNode {
  fn default() -> Self {
    UncheckedOrgNode {
      focused : false,
      folded  : false,
      kind    : UncheckedOrgNodeKind::True(UncheckedTrueNode::default()),
    }
  }
}

//
// Helper methods
//

impl UncheckedOrgNode {
  /// Reasonable for both TrueNodes and Scaffolds.
  pub fn title(&self) -> &str {
    match &self.kind {
      UncheckedOrgNodeKind::True(t) => &t.title,
      UncheckedOrgNodeKind::Scaff(s) => s.title(),
    }
  }

  /// Reasonable for both TrueNodes and Scaffolds.
  pub fn body(&self) -> Option<&String> {
    match &self.kind {
      UncheckedOrgNodeKind::True(t) => t.body.as_ref(),
      UncheckedOrgNodeKind::Scaff(_) => None,
    }
  }

  /// PITFALL: Don't let this convince you a Scaff can have an ID.
  pub fn id_opt(&self) -> Option<&ID> {
    match &self.kind {
      UncheckedOrgNodeKind::True(t) => t.id_opt.as_ref(),
      UncheckedOrgNodeKind::Scaff(_) => None,
    }
  }
}

//
// Constructor functions for unchecked types
//

pub fn unchecked_forest_root_orgnode() -> UncheckedOrgNode {
  UncheckedOrgNode {
    focused : false,
    folded  : false,
    kind    : UncheckedOrgNodeKind::Scaff(Scaffold::BufferRoot),
  }
}

pub fn unchecked_orgnode_from_scaffold(scaffold: Scaffold) -> UncheckedOrgNode {
  UncheckedOrgNode {
    focused : false,
    folded  : false,
    kind    : UncheckedOrgNodeKind::Scaff(scaffold),
  }
}
