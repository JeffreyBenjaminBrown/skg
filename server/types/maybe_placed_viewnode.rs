/// Mp variants of Viewnode and ViewnodeKind,
/// plus conversions between placed and maybePlaced trees.
/// 'Mp' means id and repo might be absent.
/// Only needed briefly after parsing a buffer from the client;
/// after validation, converted to placed types.

pub use super::viewnode::{MpActiveVognode, MpPhantomDiff};
use super::viewnode::PhantomDiff;
use super::misc::ID;
use super::tree::generic::do_everywhere_in_tree_dfs_readonly;
use super::tree::forest::{MpViewForest, ViewForest};
use super::git::{NodeAxes, RelationshipAxes};
use super::viewnode::{ Viewnode, ViewnodeKind, ActiveVognode, Vognode, Phantom, PropertyFolder, Property, PartnerFolder, PhantomDeleted, InactiveVognode, PhantomUnknown, GraphnodeStats, ViewnodeStats, Birth, Editability, AffectsParent, };

use ego_tree::{Tree, NodeId, NodeMut};
use std::collections::{HashMap, HashSet};

//
// Type declarations
//

/// Every Viewnode has an ID and a repo.
/// In MpViewnode, those two fields are optional.
/// That's the only difference.
#[derive(Debug, Clone, PartialEq)]
pub struct MpViewnode {
  pub focused     : bool,
  pub folded      : bool,
  pub body_folded : bool,
  pub kind        : MpViewnodeKind,
}

// MpActiveVognode is defined in viewnode.rs.

#[derive(Debug, Clone, PartialEq)]
pub enum MpViewnodeKind {
  Vognode      (MpVognode),
  PropertyFolder      (PropertyFolder),
  Property         (Property),
  PartnerFolder   (PartnerFolder),
  BufferRoot,
  DeadViewnode,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MpVognode {
  Active   (MpActiveVognode),
  Inactive (InactiveVognode),
  Phantom  (MpPhantom),
}

#[derive(Debug, Clone, PartialEq)]
pub enum MpPhantom {
  Diff    (MpPhantomDiff),
  Deleted (PhantomDeleted),
  Unknown (PhantomUnknown),
}

//
// Conversion implementations
//

impl TryFrom<MpActiveVognode> for ActiveVognode {
  type Error = String;

  fn try_from(u: MpActiveVognode) -> Result<Self, Self::Error> {
    let id = u . id . ok_or_else(
      || format!("Node '{}' has no ID", u . title))?;
    let repo = u . home_repo . ok_or_else(
      || format!("Node '{}' has no repo", u . title))?;
    Ok(ActiveVognode {
      title          : u . title,
      id,
      home_repo: repo,
      affectsParent          : u . affectsParent,
      birth          : u . birth,
      graphStats     : u . graphStats,
      viewStats      : u . viewStats,
      relRepo_request : u . relRepo_request,
      view_requests  : u . view_requests,
      node_axes      : u . node_axes,
      relationship_axes     : u . relationship_axes,
      not_in_git     : u . not_in_git,
      editability   : u . editability,
    })
  }
}

impl TryFrom<MpPhantomDiff> for PhantomDiff {
  type Error = String;

  fn try_from(u: MpPhantomDiff) -> Result<Self, Self::Error> {
    let id = u . id . ok_or_else(
      || format!("Phantom '{}' has no ID", u . title))?;
    let repo = u . home_repo . ok_or_else(
      || format!("Phantom '{}' has no repo", u . title))?;
    Ok(PhantomDiff {
      title      : u . title,
      id,
      home_repo: repo,
      node_axes  : u . node_axes,
      relationship_axes : u . relationship_axes,
      not_in_git : u . not_in_git,
      graphStats : u . graphStats,
    })
  }
}

impl From<PhantomDiff> for MpPhantomDiff {
  fn from(p: PhantomDiff) -> Self {
    MpPhantomDiff {
      title      : p . title,
      id         : Some(p . id),
      home_repo     : Some(p . home_repo),
      node_axes  : p . node_axes,
      relationship_axes : p . relationship_axes,
      not_in_git : p . not_in_git,
      graphStats : p . graphStats,
    }
  }
}

impl TryFrom<MpViewnodeKind> for ViewnodeKind {
  type Error = String;

  fn try_from(u: MpViewnodeKind) -> Result<Self, Self::Error> {
    match u {
      MpViewnodeKind::Vognode (MpVognode::Active (t)) =>
        Ok (ViewnodeKind::Vognode (
          Vognode::Active (ActiveVognode::try_from (t)?))),
      MpViewnodeKind::Vognode (MpVognode::Inactive (i)) =>
        Ok (ViewnodeKind::Vognode (Vognode::Inactive (i))),
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Diff (p))) =>
        Ok (ViewnodeKind::Vognode (Vognode::Phantom (
          Phantom::Diff (PhantomDiff::try_from (p)?)))),
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Deleted (d))) =>
        Ok (ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Deleted (d)))),
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (u))) =>
        Ok (ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (u)))),
      MpViewnodeKind::PropertyFolder (c) =>
        Ok (ViewnodeKind::PropertyFolder (c)),
      MpViewnodeKind::Property (q) =>
        Ok (ViewnodeKind::Property (q)),
      MpViewnodeKind::PartnerFolder (r) =>
        Ok (ViewnodeKind::PartnerFolder (r)),
      MpViewnodeKind::BufferRoot =>
        Ok (ViewnodeKind::BufferRoot),
      MpViewnodeKind::DeadViewnode =>
        Ok (ViewnodeKind::DeadViewnode), }}
}

impl TryFrom<MpViewnode> for Viewnode {
  type Error = String;

  fn try_from(u: MpViewnode) -> Result<Self, Self::Error> {
    Ok(Viewnode {
      focused     : u . focused,
      folded      : u . folded,
      body_folded : u . body_folded,
      kind        : ViewnodeKind::try_from(u . kind)?,
    })
  }
}

// Infallible conversions from placed to maybePlaced types.

impl From<ActiveVognode> for MpActiveVognode {
  fn from(t: ActiveVognode) -> Self {
    MpActiveVognode {
      title          : t . title,
      id             : Some(t . id),
      home_repo         : Some(t . home_repo),
      affectsParent          : t . affectsParent,
      birth          : t . birth,
      graphStats     : t . graphStats,
      viewStats      : t . viewStats,
      relRepo_request : t . relRepo_request,
      view_requests  : t . view_requests,
      node_axes      : t . node_axes,
      relationship_axes     : t . relationship_axes,
      not_in_git     : t . not_in_git,
      editability   : t . editability,
    }
  }
}

impl From<ViewnodeKind> for MpViewnodeKind {
  fn from(k: ViewnodeKind) -> Self {
    match k {
      ViewnodeKind::Vognode (Vognode::Active (t)) =>
        MpViewnodeKind::Vognode (
          MpVognode::Active (MpActiveVognode::from (t))),
      ViewnodeKind::Vognode (Vognode::Inactive (i)) =>
        MpViewnodeKind::Vognode (MpVognode::Inactive (i)),
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Diff (p))) =>
        MpViewnodeKind::Vognode (MpVognode::Phantom (
          MpPhantom::Diff (MpPhantomDiff::from (p)))),
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Deleted (d))) =>
        MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Deleted (d))),
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (u))) =>
        MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (u))),
      ViewnodeKind::PropertyFolder (c) =>
        MpViewnodeKind::PropertyFolder (c),
      ViewnodeKind::Property (q) =>
        MpViewnodeKind::Property (q),
      ViewnodeKind::PartnerFolder (r) =>
        MpViewnodeKind::PartnerFolder (r),
      ViewnodeKind::BufferRoot =>
        MpViewnodeKind::BufferRoot,
      ViewnodeKind::DeadViewnode =>
        MpViewnodeKind::DeadViewnode, }}
}

impl From<Viewnode> for MpViewnode {
  fn from(o: Viewnode) -> Self {
    MpViewnode {
      focused     : o . focused,
      folded      : o . folded,
      body_folded : o . body_folded,
      kind        : MpViewnodeKind::from(o . kind),
    }
  }
}

/// Does *not* compute missing repo or ID.
/// Merely converts a Tree<MpViewnode>
///              to a Tree<Viewnode>,
/// failing if it finds any repo or ID missing.
pub fn maybePlaced_to_placed_tree (
  unchecked: Tree<MpViewnode>
) -> Result<Tree<Viewnode>, String> {
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
  let mut checked: Tree<Viewnode> =
    // This tree begins as a clone of the other's root.
    Tree::new( Viewnode::try_from(
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
      let checked_node: Viewnode =
        Viewnode::try_from(
          node_ref . value() . clone() )?;
      let parent_checked_id: NodeId =
        *id_map . get (
          &node_ref . parent() . unwrap() . id()
        ) . unwrap();
      let checked_id: NodeId = {
        let mut parent_mut: NodeMut<Viewnode> =
          checked . get_mut (parent_checked_id) . unwrap();
        parent_mut . append (checked_node) . id() };
      id_map . insert( node_ref . id(),
                       checked_id );
      Ok (( )) } )?;
  Ok (ViewForest::from_internal_tree (checked)) }


//
// Defaults
//

impl Default for MpActiveVognode {
  fn default() -> Self {
    MpActiveVognode {
      title          : String::new(),
      id             : None,
      home_repo         : None,
      affectsParent       : AffectsParent::True,
      birth          : Birth::Unremarkable,
      graphStats     : GraphnodeStats::default(),
      viewStats      : ViewnodeStats::default(),
      relRepo_request : None,
      view_requests  : HashSet::new(),
      node_axes      : NodeAxes::default(),
      relationship_axes     : RelationshipAxes::default(),
      not_in_git     : false,
      editability   : Editability::Definitive {
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
        MpVognode::Active (MpActiveVognode::default())),
    }
  }
}

//
// Helper methods
//

impl MpViewnode {
  pub fn title (&self) -> &str {
    match &self . kind {
      MpViewnodeKind::Vognode (MpVognode::Active (t)) => &t . title,
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Diff (p))) => &p . title,
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Deleted (d))) =>
        &d . title,
      MpViewnodeKind::Property (q) =>
        q . title (),
      MpViewnodeKind::PropertyFolder (folder) => folder . title (),
      MpViewnodeKind::PartnerFolder (_)
        | MpViewnodeKind::BufferRoot
        | MpViewnodeKind::DeadViewnode
        | MpViewnodeKind::Vognode (MpVognode::Inactive (_))
        | MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (_))) =>
        "", }}

  /// A distinguishable label for error messages.
  pub fn error_label (&self) -> String {
    match &self . kind {
      MpViewnodeKind::Vognode (MpVognode::Active (t))
        => t . title . clone(),
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Diff (p)))
        => p . title . clone(),
      MpViewnodeKind::Property (Property::Alias { text, .. }) =>
        format!("property:alias({})", text),
      MpViewnodeKind::Property (Property::ID { id, .. }) =>
        format!("property:id({})", id),
      MpViewnodeKind::Property (Property::Flag { flag, .. }) =>
        format!("property:flag({})", flag . wire_name ()),
      MpViewnodeKind::Property (Property::TextChanged { .. }) =>
        "property:textChanged" . to_string (),
      MpViewnodeKind::PropertyFolder (folder) =>
        format!("propertyFolder:{}", folder . repr_in_client ()),
      MpViewnodeKind::PartnerFolder (partnerFolder) =>
        format!("partnerFolder:{}", partnerFolder . repr_in_client ()),
      MpViewnodeKind::BufferRoot =>
        "forestRoot" . to_string (),
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Deleted (d))) =>
        format!("deleted:{}", d . id . 0),
      MpViewnodeKind::DeadViewnode =>
        "deadViewnode" . to_string (),
      MpViewnodeKind::Vognode (MpVognode::Inactive (_)) =>
        "inactive" . to_string (),
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (u))) =>
        format!("unknown:{}", u . id . 0), }}

  /// The body text to render for this node, when it has one.
  pub fn body (&self) -> Option<&String> {
    match &self . kind {
      MpViewnodeKind::Vognode (MpVognode::Active (t))
        => t . body (),
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Diff (p)))
        => p . body (),
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Deleted (d))) =>
        d . body . as_ref(),
      MpViewnodeKind::PropertyFolder (folder) => folder . body (),
      MpViewnodeKind::Property (property) => property . body (),
      MpViewnodeKind::PartnerFolder (_)
        | MpViewnodeKind::BufferRoot
        | MpViewnodeKind::DeadViewnode
        | MpViewnodeKind::Vognode (MpVognode::Inactive (_))
        | MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (_))) =>
        None, }}

  /// PITFALL: Don't let this convince you a Scaff can have an ID.
  pub fn id_opt (&self) -> Option<&ID> {
    match &self . kind {
      MpViewnodeKind::Vognode (MpVognode::Active (t))
        => t . id . as_ref(),
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Diff (p)))
        => p . id . as_ref(),
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Deleted (d))) =>
        Some (&d . id),
      // An inactive placeholder is anonymous: no id.
      MpViewnodeKind::Vognode (MpVognode::Inactive (_)) =>
        None,
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (u))) =>
        Some (&u . id),
      MpViewnodeKind::PropertyFolder (_)
        | MpViewnodeKind::Property (_)
        | MpViewnodeKind::PartnerFolder (_)
        | MpViewnodeKind::BufferRoot
        | MpViewnodeKind::DeadViewnode =>
        None, }}

  /// True for the two ActiveVognode-ish kinds: an Active vognode or a Diff phantom.
  pub fn is_active_or_diff_phantom (&self) -> bool {
    matches! ( &self . kind,
      MpViewnodeKind::Vognode (MpVognode::Active (_))
        | MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Diff (_))) ) }
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
