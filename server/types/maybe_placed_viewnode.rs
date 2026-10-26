/// Mp variants of Viewnode and ViewnodeKind,
/// plus conversions between placed and maybePlaced trees.
/// 'Mp' means id and skgrepo might be absent.
/// Only needed briefly after parsing a buffer from the client;
/// after validation, converted to placed types.

pub use super::viewnode::MpUnrestrictedVognode;
use super::errors::{HeadlineName, with_org_subheadlines};
use super::misc::ID;
use super::tree::generic::do_everywhere_in_tree_dfs_readonly;
use super::tree::forest::{MpViewForest, ViewForest};
use super::viewnode::{ ChangeFacts, Existence, FoldAndFocus, ReadinessRequest, ViewnodeId, Viewnode, ViewnodeKind, UnrestrictedVognode, Vognode, Folder, PropertyMember, ChangeViewnode, Ghost, Unknown, GraphnodeStats, ViewnodeStats, Editability, };

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
  pub foldAndFocus      : FoldAndFocus,
  pub kind              : MpViewnodeKind,
  pub change_facts      : ChangeFacts,
  pub readiness_request : Option<ReadinessRequest>,
  pub viewnode_id       : Option<ViewnodeId>,
}

// MpUnrestrictedVognode is defined in viewnode.rs.

#[derive(Debug, Clone, PartialEq)]
pub enum MpViewnodeKind {
  Vognode      (MpVognode),
  Folder           (Folder),
  PropertyMember   (PropertyMember),
  ChangeViewnode   (ChangeViewnode),
  BufferRoot,
  Tombstone,
}

impl MpViewnodeKind {
  /// As 'ViewnodeKind::is_partner_folder'.
  pub fn is_partner_folder (&self) -> bool {
    matches! ( self, MpViewnodeKind::Folder (folder)
                     if ! folder . members_are_properties () ) }
}

#[derive(Debug, Clone, PartialEq)]
pub enum MpVognode {
  Unrestricted (MpUnrestrictedVognode),
  Ghost        (Ghost),
  Unknown      (Unknown),
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
      claimsMembership : u . claimsMembership,
      overridesHere    : u . overridesHere,
      graphStats     : u . graphStats,
      viewStats      : u . viewStats,
      relationshipEditRequest : u . relationshipEditRequest,
      view_requests  : u . view_requests,
      editability   : u . editability,
    })
  }
}

impl TryFrom<MpViewnodeKind> for ViewnodeKind {
  type Error = String;

  fn try_from(u: MpViewnodeKind) -> Result<Self, Self::Error> {
    match u {
      MpViewnodeKind::Vognode (MpVognode::Unrestricted (t)) =>
        Ok (ViewnodeKind::Vognode (
          Vognode::Unrestricted (UnrestrictedVognode::try_from (t)?))),
      MpViewnodeKind::Vognode (MpVognode::Unknown (u)) =>
        Ok (ViewnodeKind::Vognode (Vognode::Unknown (u))),
      MpViewnodeKind::Vognode (MpVognode::Ghost (h)) =>
        Ok (ViewnodeKind::Vognode (Vognode::Ghost (h))),
      MpViewnodeKind::Folder (folder) =>
        Ok (ViewnodeKind::Folder (folder)),
      MpViewnodeKind::PropertyMember (member) =>
        Ok (ViewnodeKind::PropertyMember (member)),
      MpViewnodeKind::ChangeViewnode (change) =>
        Ok (ViewnodeKind::ChangeViewnode (change)),
      MpViewnodeKind::BufferRoot =>
        Ok (ViewnodeKind::BufferRoot),
      MpViewnodeKind::Tombstone =>
        Ok (ViewnodeKind::Tombstone), }}
}

impl TryFrom<MpViewnode> for Viewnode {
  type Error = String;

  fn try_from(u: MpViewnode) -> Result<Self, Self::Error> {
    let headline : HeadlineName = // for a refusal, which names it
      u . headline_name ();
    let kind : ViewnodeKind =
      ViewnodeKind::try_from (u . kind) . map_err (
        |e| with_org_subheadlines ( &e, &[headline] )) ?;
    Ok(Viewnode {
      foldAndFocus      : u . foldAndFocus,
      kind,
      change_facts      : u . change_facts,
      readiness_request : u . readiness_request,
      viewnode_id       : u . viewnode_id,
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
      claimsMembership : t . claimsMembership,
      overridesHere    : t . overridesHere,
      graphStats     : t . graphStats,
      viewStats      : t . viewStats,
      relationshipEditRequest : t . relationshipEditRequest,
      view_requests  : t . view_requests,
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
      ViewnodeKind::Vognode (Vognode::Unknown (u)) =>
        MpViewnodeKind::Vognode (MpVognode::Unknown (u)),
      ViewnodeKind::Vognode (Vognode::Ghost (h)) =>
        MpViewnodeKind::Vognode (MpVognode::Ghost (h)),
      ViewnodeKind::Folder (folder) =>
        MpViewnodeKind::Folder (folder),
      ViewnodeKind::PropertyMember (member) =>
        MpViewnodeKind::PropertyMember (member),
      ViewnodeKind::ChangeViewnode (change) =>
        MpViewnodeKind::ChangeViewnode (change),
      ViewnodeKind::BufferRoot =>
        MpViewnodeKind::BufferRoot,
      ViewnodeKind::Tombstone =>
        MpViewnodeKind::Tombstone, }}
}

impl From<Viewnode> for MpViewnode {
  fn from(o: Viewnode) -> Self {
    MpViewnode {
      foldAndFocus      : o . foldAndFocus,
      kind              : MpViewnodeKind::from(o . kind),
      change_facts      : o . change_facts,
      readiness_request : o . readiness_request,
      viewnode_id       : o . viewnode_id,
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
      claimsMembership : true,
      overridesHere    : None,
      graphStats     : GraphnodeStats::default(),
      viewStats      : ViewnodeStats::default(),
      relationshipEditRequest : None,
      view_requests  : HashSet::new(),
      editability    : Editability::Editable {
        body         : None,
        edit_request : None },
    }
  }
}

impl Default for MpViewnode {
  fn default() -> Self {
    MpViewnode {
      foldAndFocus      : FoldAndFocus::default (),
      kind              : MpViewnodeKind::Vognode (
        MpVognode::Unrestricted (MpUnrestrictedVognode::default())),
      change_facts      : ChangeFacts::default (),
      readiness_request : None,
      viewnode_id       : None,
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
      MpViewnodeKind::Vognode (MpVognode::Ghost (h)) =>
        &h . title,
      MpViewnodeKind::PropertyMember (member) => member . title (),
      MpViewnodeKind::ChangeViewnode (change) => change . title (),
      MpViewnodeKind::Folder (folder) => folder . title (),
      MpViewnodeKind::BufferRoot
        | MpViewnodeKind::Tombstone
        | MpViewnodeKind::Vognode (MpVognode::Unknown (_)) =>
        "", }}

  /// What a message says of this headline.
  pub fn headline_name (&self) -> HeadlineName {
    HeadlineName::of ( self . viewnode_id . as_ref (), self . skgid_opt (),
                       self . title () ) }

  /// A distinguishable label for error messages.
  pub fn error_label (&self) -> String {
    match &self . kind {
      MpViewnodeKind::Vognode (MpVognode::Unrestricted (t))
        => t . title . clone(),
      MpViewnodeKind::PropertyMember (PropertyMember::Alias { text, .. }) =>
        format!("property:alias({})", text),
      MpViewnodeKind::PropertyMember (PropertyMember::ID { skgid, .. }) =>
        format!("property:id({})", skgid),
      MpViewnodeKind::PropertyMember (PropertyMember::Flag { flag, .. }) =>
        format!("property:flag({})", flag . wire_name ()),
      MpViewnodeKind::ChangeViewnode (change) =>
        format! ("changeViewnode:{}", change . repr_in_client ()),
      MpViewnodeKind::Folder (folder) if folder . members_are_properties () =>
        format!("propertyFolder:{}", folder . repr_in_client ()),
      MpViewnodeKind::Folder (folder) =>
        format!("partnerFolder:{}", folder . repr_in_client ()),
      MpViewnodeKind::BufferRoot =>
        "forestRoot" . to_string (),
      MpViewnodeKind::Vognode (MpVognode::Ghost (h)) =>
        format!("ghost:{}", h . skgid . 0),
      MpViewnodeKind::Tombstone =>
        "tombstone" . to_string (),
      MpViewnodeKind::Vognode (MpVognode::Unknown (u)) =>
        format!("unknown:{}", u . skgid . 0), }}

  /// The body text to render for this node, when it has one.
  pub fn body (&self) -> Option<&String> {
    match &self . kind {
      MpViewnodeKind::Vognode (MpVognode::Unrestricted (t))
        => t . body (),
      MpViewnodeKind::Vognode (MpVognode::Ghost (h)) =>
        h . body . as_ref(),
      MpViewnodeKind::Folder (folder) => folder . body (),
      MpViewnodeKind::PropertyMember (member) => member . body (),
      MpViewnodeKind::ChangeViewnode (change) => change . body (),
      MpViewnodeKind::BufferRoot
        | MpViewnodeKind::Tombstone
        | MpViewnodeKind::Vognode (MpVognode::Unknown (_)) =>
        None, }}

  /// PITFALL: Don't let this convince you a Scaff can have an ID.
  pub fn skgid_opt (&self) -> Option<&ID> {
    match &self . kind {
      MpViewnodeKind::Vognode (MpVognode::Unrestricted (t))
        => t . skgid . as_ref(),
      MpViewnodeKind::Vognode (MpVognode::Ghost (h)) =>
        Some (&h . skgid),
      MpViewnodeKind::Vognode (MpVognode::Unknown (u)) =>
        Some (&u . skgid),
      MpViewnodeKind::Folder (_)
        | MpViewnodeKind::PropertyMember (_)
        | MpViewnodeKind::ChangeViewnode (_)
        | MpViewnodeKind::BufferRoot
        | MpViewnodeKind::Tombstone =>
        None, }}

  /// As 'Viewnode::is_ghostWarranted'.
  pub fn is_ghostWarranted (&self) -> bool {
    self . change_facts . change . rel == Some (Existence::Removed) }

  pub fn is_unrestricted (&self) -> bool {
    matches! ( &self . kind,
      MpViewnodeKind::Vognode (MpVognode::Unrestricted (_)) ) }
}

//
// Constructor functions for maybePlaced types
//

pub fn maybePlaced_viewforest_root_viewnode() -> MpViewnode {
  MpViewnode {
    kind : MpViewnodeKind::BufferRoot,
    .. MpViewnode::default () }}
