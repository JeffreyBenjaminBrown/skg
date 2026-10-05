/// Mp variants of Viewnode and ViewnodeKind,
/// plus conversions between placed and maybePlaced trees.
/// 'Mp' means id and skgrepo might be absent.
/// Only needed briefly after parsing a buffer from the client;
/// after validation, converted to placed types.

pub use super::viewnode::{MpUnrestrictedVognode, MpPhantomDiff};
use super::viewnode::PhantomDiff;
use super::misc::ID;
use super::tree::generic::do_everywhere_in_tree_dfs_readonly;
use super::tree::forest::{MpViewForest, ViewForest};
use super::git::{NodeAxes, RelationshipAxes};
use super::viewnode::{ Viewnode, ViewnodeKind, UnrestrictedVognode, Vognode, Phantom, PropertyFolder, Property, PartnerFolder, PhantomDeleted, RestrictedVognode, PhantomUnknown, GraphnodeStats, ViewnodeStats, Birth, Editability, AffectsParent, };

use ego_tree::{Tree, NodeId, NodeMut};
use std::collections::{HashMap, HashSet};

//
// Type declarations
//

/// Every Viewnode has an ID and a skgrepo.
/// In MpViewnode, those two fields are optional.
/// That's the only difference.
#[derive(Debug, Clone, PartialEq)]
pub struct MpViewnode {
  pub focused     : bool,
  pub folded      : bool,
  pub body_folded : bool,
  pub kind        : MpViewnodeKind,
}

// MpUnrestrictedVognode is defined in viewnode.rs.

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
  Unrestricted   (MpUnrestrictedVognode),
  Restricted (RestrictedVognode),
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

impl TryFrom<MpUnrestrictedVognode> for UnrestrictedVognode {
  type Error = String;

  fn try_from(u: MpUnrestrictedVognode) -> Result<Self, Self::Error> {
    let skgid = u . skgid . ok_or_else(
      || format!("Node '{}' has no ID", u . title))?;
    let skgrepo = u . home_skgrepo . ok_or_else(
      || format!("Node '{}' has no repo", u . title))?;
    Ok(UnrestrictedVognode {
      title          : u . title,
      skgid          : skgid,
      home_skgrepo: skgrepo,
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
    let skgid = u . skgid . ok_or_else(
      || format!("Phantom '{}' has no ID", u . title))?;
    let skgrepo = u . home_skgrepo . ok_or_else(
      || format!("Phantom '{}' has no repo", u . title))?;
    Ok(PhantomDiff {
      title      : u . title,
      skgid      : skgid,
      home_skgrepo: skgrepo,
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
      skgid         : Some(p . skgid),
      home_skgrepo     : Some(p . home_skgrepo),
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
      MpViewnodeKind::Vognode (MpVognode::Unrestricted (t)) =>
        Ok (ViewnodeKind::Vognode (
          Vognode::Unrestricted (UnrestrictedVognode::try_from (t)?))),
      MpViewnodeKind::Vognode (MpVognode::Restricted (i)) =>
        Ok (ViewnodeKind::Vognode (Vognode::Restricted (i))),
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

impl From<UnrestrictedVognode> for MpUnrestrictedVognode {
  fn from(t: UnrestrictedVognode) -> Self {
    MpUnrestrictedVognode {
      title          : t . title,
      skgid          : Some(t . skgid),
      home_skgrepo   : Some(t . home_skgrepo),
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
      ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
        MpViewnodeKind::Vognode (
          MpVognode::Unrestricted (MpUnrestrictedVognode::from (t))),
      ViewnodeKind::Vognode (Vognode::Restricted (i)) =>
        MpViewnodeKind::Vognode (MpVognode::Restricted (i)),
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

/// Does *not* compute missing skgrepo or ID.
/// Merely converts a Tree<MpViewnode>
///              to a Tree<Viewnode>,
/// failing if it finds any skgrepo or ID missing.
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
  let unchecked_root_skgid: NodeId =
    unchecked . root() . id();
  let mut checked: Tree<Viewnode> =
    // This tree begins as a clone of the other's root.
    Tree::new( Viewnode::try_from(
      unchecked . root() . value() . clone() )? );
  let mut id_map: HashMap< NodeId, // key : unchecked
                           NodeId > // value : checked
    = HashMap::new();
  id_map . insert( unchecked_root_skgid,
                   checked . root() . id() );
  do_everywhere_in_tree_dfs_readonly(
    // PITFALL: Readonly for 'unchecked',
    // but mutates 'checked' and 'id_map'.
    &unchecked, unchecked_root_skgid, true,
    &mut |node_ref
    | {
      if node_ref . id() == unchecked_root_skgid {
        return Ok (( )); } // already converted
      let checked_node: Viewnode =
        Viewnode::try_from(
          node_ref . value() . clone() )?;
      let parent_checked_skgid: NodeId =
        *id_map . get (
          &node_ref . parent() . unwrap() . id()
        ) . unwrap();
      let checked_skgid: NodeId = {
        let mut parent_mut: NodeMut<Viewnode> =
          checked . get_mut (parent_checked_skgid) . unwrap();
        parent_mut . append (checked_node) . id() };
      id_map . insert( node_ref . id(),
                       checked_skgid );
      Ok (( )) } )?;
  Ok (ViewForest::from_internal_tree (checked)) }


//
// Defaults
//

impl Default for MpUnrestrictedVognode {
  fn default() -> Self {
    MpUnrestrictedVognode {
      title          : String::new(),
      skgid          : None,
      home_skgrepo   : None,
      affectsParent       : AffectsParent::True,
      birth          : Birth::Unremarkable,
      graphStats     : GraphnodeStats::default(),
      viewStats      : ViewnodeStats::default(),
      relRepo_request : None,
      view_requests  : HashSet::new(),
      node_axes      : NodeAxes::default(),
      relationship_axes     : RelationshipAxes::default(),
      not_in_git     : false,
      editability    : Editability::Editable {
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
        MpVognode::Unrestricted (MpUnrestrictedVognode::default())),
    }
  }
}

//
// Helper methods
//

impl MpViewnode {
  pub fn title (&self) -> &str {
    match &self . kind {
      MpViewnodeKind::Vognode (MpVognode::Unrestricted (t)) => &t . title,
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Diff (p))) => &p . title,
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Deleted (d))) =>
        &d . title,
      MpViewnodeKind::Property (q) =>
        q . title (),
      MpViewnodeKind::PropertyFolder (folder) => folder . title (),
      MpViewnodeKind::PartnerFolder (_)
        | MpViewnodeKind::BufferRoot
        | MpViewnodeKind::DeadViewnode
        | MpViewnodeKind::Vognode (MpVognode::Restricted (_))
        | MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (_))) =>
        "", }}

  /// A distinguishable label for error messages.
  pub fn error_label (&self) -> String {
    match &self . kind {
      MpViewnodeKind::Vognode (MpVognode::Unrestricted (t))
        => t . title . clone(),
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Diff (p)))
        => p . title . clone(),
      MpViewnodeKind::Property (Property::Alias { text, .. }) =>
        format!("property:alias({})", text),
      MpViewnodeKind::Property (Property::ID { skgid, .. }) =>
        format!("property:id({})", skgid),
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
        format!("deleted:{}", d . skgid . 0),
      MpViewnodeKind::DeadViewnode =>
        "deadViewnode" . to_string (),
      MpViewnodeKind::Vognode (MpVognode::Restricted (_)) =>
        "restricted" . to_string (),
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (u))) =>
        format!("unknown:{}", u . skgid . 0), }}

  /// The body text to render for this node, when it has one.
  pub fn body (&self) -> Option<&String> {
    match &self . kind {
      MpViewnodeKind::Vognode (MpVognode::Unrestricted (t))
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
        | MpViewnodeKind::Vognode (MpVognode::Restricted (_))
        | MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (_))) =>
        None, }}

  /// PITFALL: Don't let this convince you a Scaff can have an ID.
  pub fn skgid_opt (&self) -> Option<&ID> {
    match &self . kind {
      MpViewnodeKind::Vognode (MpVognode::Unrestricted (t))
        => t . skgid . as_ref(),
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Diff (p)))
        => p . skgid . as_ref(),
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Deleted (d))) =>
        Some (&d . skgid),
      // A restricted vognode is anonymous: no id.
      MpViewnodeKind::Vognode (MpVognode::Restricted (_)) =>
        None,
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (u))) =>
        Some (&u . skgid),
      MpViewnodeKind::PropertyFolder (_)
        | MpViewnodeKind::Property (_)
        | MpViewnodeKind::PartnerFolder (_)
        | MpViewnodeKind::BufferRoot
        | MpViewnodeKind::DeadViewnode =>
        None, }}

  /// True for the two UnrestrictedVognode-ish kinds: an Unrestricted vognode or a Diff phantom.
  pub fn is_unrestricted_or_diff_phantom (&self) -> bool {
    matches! ( &self . kind,
      MpViewnodeKind::Vognode (MpVognode::Unrestricted (_))
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
