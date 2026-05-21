/// MaybePlaced variants of ViewNode and ViewNodeKind,
/// plus conversions between placed and maybePlaced trees.
/// 'MaybePlaced' means id and source might be absent.
/// Only needed briefly after parsing a buffer from the client;
/// after validation, converted to placed types.

pub use super::viewnode::MaybePlacedTruenode;
use super::misc::ID;
use super::tree::generic::do_everywhere_in_tree_dfs_readonly;
use super::git::{ExistenceAxes, MembershipAxes};
use super::viewnode::{ ViewNode, ViewNodeKind, TrueNode, Scaffold, ScaffoldKind, DeletedNode, UnknownNode, GraphNodeStats, ViewNodeStats, IndefOrDef, Birth, };

use ego_tree::{Tree, NodeId, NodeMut};
use std::collections::{HashMap, HashSet};

//
// Type declarations
//

/// Every ViewNode has an ID and a source.
/// In MaybePlacedViewnode, those two fields are optional.
/// That's the only difference.
#[derive(Debug, Clone, PartialEq)]
pub struct MaybePlacedViewnode {
  pub focused     : bool,
  pub folded      : bool,
  pub body_folded : bool,
  pub kind        : MaybePlacedViewnodeKind,
}

// MaybePlacedTruenode is defined in viewnode.rs.

#[derive(Debug, Clone, PartialEq)]
pub enum MaybePlacedViewnodeKind {
  True         (MaybePlacedTruenode),
  Scaff        (Scaffold),  // Scaffold is shared - scaffolds never have IDs
  Deleted      (DeletedNode),
  DeletedScaff (ScaffoldKind),
  Unknown      (UnknownNode),
}

//
// Conversion implementations
//

impl TryFrom<MaybePlacedTruenode> for TrueNode {
  type Error = String;

  fn try_from(u: MaybePlacedTruenode) -> Result<Self, Self::Error> {
    let id = u . id . ok_or_else(
      || format!("Node '{}' has no ID", u . title))?;
    let source = u . source . ok_or_else(
      || format!("Node '{}' has no source", u . title))?;
    Ok(TrueNode {
      title          : u . title,
      id,
      source,
      birth          : u . birth,
      graphStats     : u . graphStats,
      viewStats      : u . viewStats,
      view_requests  : u . view_requests,
      existence      : u . existence,
      membership     : u . membership,
      not_in_git     : u . not_in_git,
      indef_or_def   : u . indef_or_def,
    })
  }
}

impl TryFrom<MaybePlacedViewnodeKind> for ViewNodeKind {
  type Error = String;

  fn try_from(u: MaybePlacedViewnodeKind) -> Result<Self, Self::Error> {
    match u {
      MaybePlacedViewnodeKind::True (t) =>
        Ok(ViewNodeKind::True(TrueNode::try_from (t)?)),
      MaybePlacedViewnodeKind::Scaff (s) =>
        Ok(ViewNodeKind::Scaff (s)),
      MaybePlacedViewnodeKind::Deleted (d) =>
        Ok(ViewNodeKind::Deleted (d)),
      MaybePlacedViewnodeKind::DeletedScaff (kind) =>
        Ok (ViewNodeKind::DeletedScaff (kind)),
      MaybePlacedViewnodeKind::Unknown (u) =>
        Ok (ViewNodeKind::Unknown (u)) }}
}

impl TryFrom<MaybePlacedViewnode> for ViewNode {
  type Error = String;

  fn try_from(u: MaybePlacedViewnode) -> Result<Self, Self::Error> {
    Ok(ViewNode {
      focused     : u . focused,
      folded      : u . folded,
      body_folded : u . body_folded,
      kind        : ViewNodeKind::try_from(u . kind)?,
    })
  }
}

// Infallible conversions from placed to maybePlaced types.

impl From<TrueNode> for MaybePlacedTruenode {
  fn from(t: TrueNode) -> Self {
    MaybePlacedTruenode {
      title          : t . title,
      id             : Some(t . id),
      source         : Some(t . source),
      birth          : t . birth,
      graphStats     : t . graphStats,
      viewStats      : t . viewStats,
      view_requests  : t . view_requests,
      existence      : t . existence,
      membership     : t . membership,
      not_in_git     : t . not_in_git,
      indef_or_def   : t . indef_or_def,
    }
  }
}

impl From<ViewNodeKind> for MaybePlacedViewnodeKind {
  fn from(k: ViewNodeKind) -> Self {
    match k {
      ViewNodeKind::True (t) =>
        MaybePlacedViewnodeKind::True(MaybePlacedTruenode::from (t)),
      ViewNodeKind::Scaff (s) =>
        MaybePlacedViewnodeKind::Scaff (s),
      ViewNodeKind::Deleted (d) =>
        MaybePlacedViewnodeKind::Deleted (d),
      ViewNodeKind::DeletedScaff (kind) =>
        MaybePlacedViewnodeKind::DeletedScaff (kind),
      ViewNodeKind::Unknown (u) =>
        MaybePlacedViewnodeKind::Unknown (u) }}
}

impl From<ViewNode> for MaybePlacedViewnode {
  fn from(o: ViewNode) -> Self {
    MaybePlacedViewnode {
      focused     : o . focused,
      folded      : o . folded,
      body_folded : o . body_folded,
      kind        : MaybePlacedViewnodeKind::from(o . kind),
    }
  }
}

/// Does *not* compute missing source or ID.
/// Merely converts a Tree<MaybePlacedViewnode>
///              to a Tree<ViewNode>,
/// failing if it finds any source or ID missing.
pub fn maybePlaced_to_placed_tree (
  unchecked: Tree<MaybePlacedViewnode>
) -> Result<Tree<ViewNode>, String> {
  let unchecked_root_id: NodeId =
    unchecked . root() . id();
  let mut checked: Tree<ViewNode> =
    // This tree begins as a clone of the other's root.
    Tree::new( ViewNode::try_from(
      unchecked . root() . value() . clone() )? );
  let mut id_map: HashMap< NodeId, // key : unchecked
                           NodeId > // value : checked
    = HashMap::new();
  id_map . insert( unchecked_root_id,
                   checked . root() . id() );
  do_everywhere_in_tree_dfs_readonly(
    // PITFALL: Readonly for 'unchecked',
    // but mutates 'checked' and 'id_map'.
    &unchecked, unchecked_root_id, true,
    &mut |node_ref
    | {
      if node_ref . id() == unchecked_root_id {
        return Ok (( )); } // already converted
      let checked_node: ViewNode =
        ViewNode::try_from(
          node_ref . value() . clone() )?;
      let parent_checked_id: NodeId =
        *id_map . get (
          &node_ref . parent() . unwrap() . id()
        ) . unwrap();
      let checked_id: NodeId = {
        let mut parent_mut: NodeMut<ViewNode> =
          checked . get_mut (parent_checked_id) . unwrap();
        parent_mut . append (checked_node) . id() };
      id_map . insert( node_ref . id(),
                       checked_id );
      Ok (( )) } )?;
  Ok (checked) }

/// Convert a placed ViewNode tree to an MaybePlacedViewnode tree.
/// Infallible since checked types always satisfy maybePlaced requirements.
pub fn placed_to_maybePlaced_tree(
  checked: &Tree<ViewNode>
) -> Tree<MaybePlacedViewnode> {
  fn convert_children(
    checked_tree   : &Tree<ViewNode>,
    unchecked_tree : &mut Tree<MaybePlacedViewnode>,
    checked_id     : NodeId,
    unchecked_id   : NodeId,
  ) {
    let child_ids : Vec<NodeId> =
      checked_tree
      . get (checked_id)
      . unwrap()
      . children()
      . map(|c| c . id())
      . collect();

    for checked_child_id in child_ids {
      let checked_child : &ViewNode =
        checked_tree
        . get (checked_child_id)
        . unwrap()
        . value();
      let unchecked_child : MaybePlacedViewnode =
        MaybePlacedViewnode::from(checked_child . clone());

      let unchecked_child_id : NodeId =
        { let mut parent_mut : NodeMut<MaybePlacedViewnode> =
           unchecked_tree . get_mut (unchecked_id) . unwrap();
         parent_mut . append (unchecked_child) . id() };

      convert_children(
        checked_tree,
        unchecked_tree,
        checked_child_id,
        unchecked_child_id );
    }
  }

  let root_checked : &ViewNode =
    checked . root() . value();
  let root_unchecked : MaybePlacedViewnode =
    MaybePlacedViewnode::from(root_checked . clone());
  let mut unchecked : Tree<MaybePlacedViewnode> =
    Tree::new (root_unchecked);

  let checked_root_id : NodeId =
    checked . root() . id();
  let unchecked_root_id : NodeId =
    unchecked . root() . id();
  convert_children(&checked, &mut unchecked, checked_root_id, unchecked_root_id);

  unchecked
}

//
// Defaults
//

impl Default for MaybePlacedTruenode {
  fn default() -> Self {
    MaybePlacedTruenode {
      title          : String::new(),
      id             : None,
      source         : None,
      birth          : Birth::ContentOf,
      graphStats     : GraphNodeStats::default(),
      viewStats      : ViewNodeStats::default(),
      view_requests  : HashSet::new(),
      existence      : ExistenceAxes::default(),
      membership     : MembershipAxes::default(),
      not_in_git     : false,
      indef_or_def   : IndefOrDef::Definitive {
        body         : None,
        edit_request : None },
    }
  }
}

impl Default for MaybePlacedViewnode {
  fn default() -> Self {
    MaybePlacedViewnode {
      focused     : false,
      folded      : false,
      body_folded : false,
      kind        : MaybePlacedViewnodeKind::True(MaybePlacedTruenode::default()),
    }
  }
}

//
// Helper methods
//

impl MaybePlacedViewnode {
  /// Reasonable for both TrueNodes and Scaffolds.
  pub fn title (&self) -> &str {
    match &self . kind {
      MaybePlacedViewnodeKind::True (t)    => &t . title,
      MaybePlacedViewnodeKind::Scaff (s)   => s . title(),
      MaybePlacedViewnodeKind::Deleted (d) => &d . title,
      MaybePlacedViewnodeKind::DeletedScaff (kind) =>
        kind . default_title (),
      MaybePlacedViewnodeKind::Unknown (_) => "", }}

  /// A distinguishable label for error messages.
  pub fn error_label (&self) -> String {
    match &self . kind {
      MaybePlacedViewnodeKind::True (t)    => t . title . clone(),
      MaybePlacedViewnodeKind::Scaff (s)   => s . error_label(),
      MaybePlacedViewnodeKind::Deleted (d) =>
        format!("deleted:{}", d . id . 0),
      MaybePlacedViewnodeKind::DeletedScaff (kind) =>
        format!("deletedScaffold:{}", kind . repr_in_client ()),
      MaybePlacedViewnodeKind::Unknown (u) =>
        format!("unknown:{}", u . id . 0), }}

  /// Reasonable for both TrueNodes and Scaffolds.
  pub fn body (&self) -> Option<&String> {
    match &self . kind {
      MaybePlacedViewnodeKind::True (t)    => t . body (),
      MaybePlacedViewnodeKind::Scaff (_)   => None,
      MaybePlacedViewnodeKind::Deleted (d) => d . body . as_ref(),
      MaybePlacedViewnodeKind::DeletedScaff (_) => None,
      MaybePlacedViewnodeKind::Unknown (_) => None, }}

  /// PITFALL: Don't let this convince you a Scaff can have an ID.
  pub fn id_opt (&self) -> Option<&ID> {
    match &self . kind {
      MaybePlacedViewnodeKind::True (t)    => t . id . as_ref(),
      MaybePlacedViewnodeKind::Scaff (_)   => None,
      MaybePlacedViewnodeKind::Deleted (d) => Some(&d . id),
      MaybePlacedViewnodeKind::DeletedScaff (_) => None,
      MaybePlacedViewnodeKind::Unknown (u) => Some(&u . id), }}
}

//
// Constructor functions for maybePlaced types
//

pub fn maybePlaced_viewforest_root_viewnode() -> MaybePlacedViewnode {
  MaybePlacedViewnode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind        : MaybePlacedViewnodeKind::Scaff (Scaffold::BufferRoot), }}

pub fn maybe_placed_viewnode_from_scaffold(scaffold: Scaffold) -> MaybePlacedViewnode {
  MaybePlacedViewnode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind        : MaybePlacedViewnodeKind::Scaff (scaffold), }}
