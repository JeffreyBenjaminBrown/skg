/// Mp variants of ViewNode and ViewNodeKind,
/// plus conversions between placed and maybePlaced trees.
/// 'Mp' means id and source might be absent.
/// Only needed briefly after parsing a buffer from the client;
/// after validation, converted to placed types.

pub use super::viewnode::MpTruenode;
use super::misc::ID;
use super::tree::generic::do_everywhere_in_tree_dfs_readonly;
use super::tree::forest::{MpViewForest, ViewForest};
use super::git::{ExistenceAxes, MembershipAxes};
use super::viewnode::{ ViewNode, ViewNodeKind, TrueNode, Vognode, QualCol, Qual, RoleCol, DeletedNode, InactiveNode, UnknownNode, GraphNodeStats, ViewNodeStats, Birth, IndefOrDef, ParentIs, };

use ego_tree::{Tree, NodeId, NodeMut};
use std::collections::{HashMap, HashSet};

//
// Type declarations
//

/// Every ViewNode has an ID and a source.
/// In MpViewnode, those two fields are optional.
/// That's the only difference.
#[derive(Debug, Clone, PartialEq)]
pub struct MpViewnode {
  pub focused     : bool,
  pub folded      : bool,
  pub body_folded : bool,
  pub kind        : MpViewnodeKind,
}

// MpTruenode is defined in viewnode.rs.

#[derive(Debug, Clone, PartialEq)]
pub enum MpViewnodeKind {
  Vognode      (MpVognode),
  QualCol      (QualCol),
  Qual         (Qual),
  PartnerCol   (RoleCol),
  BufferRoot,
  DeadScaffold,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MpVognode {
  Normal   (MpTruenode),
  DiffPhantom (MpTruenode),
  Inactive (InactiveNode),
  Unknown  (UnknownNode),
  Deleted  (DeletedNode),
}

//
// Conversion implementations
//

impl TryFrom<MpTruenode> for TrueNode {
  type Error = String;

  fn try_from(u: MpTruenode) -> Result<Self, Self::Error> {
    let id = u . id . ok_or_else(
      || format!("Node '{}' has no ID", u . title))?;
    let source = u . source . ok_or_else(
      || format!("Node '{}' has no source", u . title))?;
    Ok(TrueNode {
      title          : u . title,
      id,
      source,
      parentIs          : u . parentIs,
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

impl TryFrom<MpViewnodeKind> for ViewNodeKind {
  type Error = String;

  fn try_from(u: MpViewnodeKind) -> Result<Self, Self::Error> {
    match u {
      MpViewnodeKind::Vognode (MpVognode::Normal (t)) =>
        Ok (ViewNodeKind::Vognode (
          Vognode::Normal (TrueNode::try_from (t)?))),
      MpViewnodeKind::Vognode (MpVognode::DiffPhantom (t)) =>
        Ok (ViewNodeKind::Vognode (
          Vognode::DiffPhantom (TrueNode::try_from (t)?))),
      MpViewnodeKind::Vognode (MpVognode::Deleted (d)) =>
        Ok (ViewNodeKind::Vognode (Vognode::Deleted (d))),
      MpViewnodeKind::Vognode (MpVognode::Inactive (i)) =>
        Ok (ViewNodeKind::Vognode (Vognode::Inactive (i))),
      MpViewnodeKind::Vognode (MpVognode::Unknown (u)) =>
        Ok (ViewNodeKind::Vognode (Vognode::Unknown (u))),
      MpViewnodeKind::QualCol (c) =>
        Ok (ViewNodeKind::QualCol (c)),
      MpViewnodeKind::Qual (q) =>
        Ok (ViewNodeKind::Qual (q)),
      MpViewnodeKind::PartnerCol (r) =>
        Ok (ViewNodeKind::PartnerCol (r)),
      MpViewnodeKind::BufferRoot =>
        Ok (ViewNodeKind::BufferRoot),
      MpViewnodeKind::DeadScaffold =>
        Ok (ViewNodeKind::DeadScaffold), }}
}

impl TryFrom<MpViewnode> for ViewNode {
  type Error = String;

  fn try_from(u: MpViewnode) -> Result<Self, Self::Error> {
    Ok(ViewNode {
      focused     : u . focused,
      folded      : u . folded,
      body_folded : u . body_folded,
      kind        : ViewNodeKind::try_from(u . kind)?,
    })
  }
}

// Infallible conversions from placed to maybePlaced types.

impl From<TrueNode> for MpTruenode {
  fn from(t: TrueNode) -> Self {
    MpTruenode {
      title          : t . title,
      id             : Some(t . id),
      source         : Some(t . source),
      parentIs          : t . parentIs,
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

impl From<ViewNodeKind> for MpViewnodeKind {
  fn from(k: ViewNodeKind) -> Self {
    match k {
      ViewNodeKind::Vognode (Vognode::Normal (t)) =>
        MpViewnodeKind::Vognode (
          MpVognode::Normal (MpTruenode::from (t))),
      ViewNodeKind::Vognode (Vognode::DiffPhantom (t)) =>
        MpViewnodeKind::Vognode (
          MpVognode::DiffPhantom (MpTruenode::from (t))),
      ViewNodeKind::Vognode (Vognode::Deleted (d)) =>
        MpViewnodeKind::Vognode (MpVognode::Deleted (d)),
      ViewNodeKind::Vognode (Vognode::Inactive (i)) =>
        MpViewnodeKind::Vognode (MpVognode::Inactive (i)),
      ViewNodeKind::Vognode (Vognode::Unknown (u)) =>
        MpViewnodeKind::Vognode (MpVognode::Unknown (u)),
      ViewNodeKind::QualCol (c) =>
        MpViewnodeKind::QualCol (c),
      ViewNodeKind::Qual (q) =>
        MpViewnodeKind::Qual (q),
      ViewNodeKind::PartnerCol (r) =>
        MpViewnodeKind::PartnerCol (r),
      ViewNodeKind::BufferRoot =>
        MpViewnodeKind::BufferRoot,
      ViewNodeKind::DeadScaffold =>
        MpViewnodeKind::DeadScaffold, }}
}

impl From<ViewNode> for MpViewnode {
  fn from(o: ViewNode) -> Self {
    MpViewnode {
      focused     : o . focused,
      folded      : o . folded,
      body_folded : o . body_folded,
      kind        : MpViewnodeKind::from(o . kind),
    }
  }
}

/// Does *not* compute missing source or ID.
/// Merely converts a Tree<MpViewnode>
///              to a Tree<ViewNode>,
/// failing if it finds any source or ID missing.
pub fn maybePlaced_to_placed_tree (
  unchecked: Tree<MpViewnode>
) -> Result<Tree<ViewNode>, String> {
  Ok (
    maybePlaced_to_placed_viewforest (
      MpViewForest::from_internal_tree (unchecked)) ?
    . into_internal_tree () ) }

pub fn maybePlaced_to_placed_viewforest (
  unchecked: MpViewForest
) -> Result<ViewForest, String> {
  let unchecked : Tree<MpViewnode> =
    unchecked . into_internal_tree ();
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
  Ok (ViewForest::from_internal_tree (checked)) }

/// Convert a placed ViewNode tree to an MpViewnode tree.
/// Infallible since checked types always satisfy maybePlaced requirements.
pub fn placed_to_maybePlaced_tree(
  checked: &Tree<ViewNode>
) -> Tree<MpViewnode> {
  fn convert_children(
    checked_tree   : &Tree<ViewNode>,
    unchecked_tree : &mut Tree<MpViewnode>,
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
      let unchecked_child : MpViewnode =
        MpViewnode::from(checked_child . clone());

      let unchecked_child_id : NodeId =
        { let mut parent_mut : NodeMut<MpViewnode> =
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
  let root_unchecked : MpViewnode =
    MpViewnode::from(root_checked . clone());
  let mut unchecked : Tree<MpViewnode> =
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

impl Default for MpTruenode {
  fn default() -> Self {
    MpTruenode {
      title          : String::new(),
      id             : None,
      source         : None,
      parentIs       : ParentIs::Affected,
      birth          : Birth::Unremarkable,
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

impl Default for MpViewnode {
  fn default() -> Self {
    MpViewnode {
      focused     : false,
      folded      : false,
      body_folded : false,
      kind        : MpViewnodeKind::Vognode (
        MpVognode::Normal (MpTruenode::default())),
    }
  }
}

//
// Helper methods
//

impl MpViewnode {
  pub fn title (&self) -> &str {
    match &self . kind {
      MpViewnodeKind::Vognode (MpVognode::Normal (t))
        | MpViewnodeKind::Vognode (MpVognode::DiffPhantom (t)) =>
        &t . title,
      MpViewnodeKind::Vognode (MpVognode::Deleted (d)) =>
        &d . title,
      MpViewnodeKind::Qual (q) =>
        q . title (),
      MpViewnodeKind::QualCol (_)
        | MpViewnodeKind::PartnerCol (_)
        | MpViewnodeKind::BufferRoot
        | MpViewnodeKind::DeadScaffold
        | MpViewnodeKind::Vognode (MpVognode::Inactive (_))
        | MpViewnodeKind::Vognode (MpVognode::Unknown (_)) =>
        "", }}

  /// A distinguishable label for error messages.
  pub fn error_label (&self) -> String {
    match &self . kind {
      MpViewnodeKind::Vognode (MpVognode::Normal (t))
        | MpViewnodeKind::Vognode (MpVognode::DiffPhantom (t))
        => t . title . clone(),
      MpViewnodeKind::Qual (Qual::Alias { text, .. }) =>
        format!("qual:alias({})", text),
      MpViewnodeKind::Qual (Qual::ID { id, .. }) =>
        format!("qual:id({})", id),
      MpViewnodeKind::Qual (Qual::TextChanged { .. }) =>
        "qual:textChanged" . to_string (),
      MpViewnodeKind::QualCol (col) =>
        format!("qualCol:{}", col . repr_in_client ()),
      MpViewnodeKind::PartnerCol (roleCol) =>
        format!("partnerCol:{}", roleCol . repr_in_client ()),
      MpViewnodeKind::BufferRoot =>
        "forestRoot" . to_string (),
      MpViewnodeKind::Vognode (MpVognode::Deleted (d)) =>
        format!("deleted:{}", d . id . 0),
      MpViewnodeKind::DeadScaffold =>
        "deadScaffold" . to_string (),
      MpViewnodeKind::Vognode (MpVognode::Inactive (i)) =>
        format!("inactive:{}", i . id . 0),
      MpViewnodeKind::Vognode (MpVognode::Unknown (u)) =>
        format!("unknown:{}", u . id . 0), }}

  /// The body text to render for this node, when it has one.
  pub fn body (&self) -> Option<&String> {
    match &self . kind {
      MpViewnodeKind::Vognode (MpVognode::Normal (t))
        | MpViewnodeKind::Vognode (MpVognode::DiffPhantom (t))
        => t . body (),
      MpViewnodeKind::Vognode (MpVognode::Deleted (d)) =>
        d . body . as_ref(),
      MpViewnodeKind::QualCol (_)
        | MpViewnodeKind::Qual (_)
        | MpViewnodeKind::PartnerCol (_)
        | MpViewnodeKind::BufferRoot
        | MpViewnodeKind::DeadScaffold
        | MpViewnodeKind::Vognode (MpVognode::Inactive (_))
        | MpViewnodeKind::Vognode (MpVognode::Unknown (_)) =>
        None, }}

  /// PITFALL: Don't let this convince you a Scaff can have an ID.
  pub fn id_opt (&self) -> Option<&ID> {
    match &self . kind {
      MpViewnodeKind::Vognode (MpVognode::Normal (t))
        | MpViewnodeKind::Vognode (MpVognode::DiffPhantom (t))
        => t . id . as_ref(),
      MpViewnodeKind::Vognode (MpVognode::Deleted (d)) =>
        Some (&d . id),
      MpViewnodeKind::Vognode (MpVognode::Inactive (i)) =>
        Some (&i . id),
      MpViewnodeKind::Vognode (MpVognode::Unknown (u)) =>
        Some (&u . id),
      MpViewnodeKind::QualCol (_)
        | MpViewnodeKind::Qual (_)
        | MpViewnodeKind::PartnerCol (_)
        | MpViewnodeKind::BufferRoot
        | MpViewnodeKind::DeadScaffold =>
        None, }}
}

//
// Constructor functions for maybePlaced types
//

pub fn maybePlaced_viewforest_root_viewnode() -> MpViewnode {
  MpViewnode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind        : MpViewnodeKind::BufferRoot, }}
