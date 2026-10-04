/// Local validation functions for Viewnode trees.
/// These check structural flags of individual nodes
/// without requiring global context.

use crate::types::maybe_placed_viewnode::{MpViewnode, MpViewnodeKind, MpActiveVognode, MpPhantomDiff};
use crate::types::maybe_placed_viewnode::{MpVognode, MpPhantom};
use crate::types::viewnode::{NodeEditRequest, Editability, AffectsParent, PartnerFolder, Property, PropertyFolder};
use crate::types::misc::{ID, SkgConfig};
use crate::types::tree::viewnode_graphnode::{
  generation_includes_only,
  generation_exists_and_includes,
  generation_does_not_exist,
  siblings_cannot_include,
  id_from_self_or_nearest_ancestor,
};
use ego_tree::{Tree, NodeId};
use std::collections::HashSet;

/// Error from local structure validation.
/// Contains the error message and the ID of the nearest ActiveVognode ancestor.
#[derive(Debug, Clone, PartialEq)]
pub struct LocalStructureError {
  pub message : String,
  pub id      : ID,
}

pub fn validate_local_structure (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
  config  : &SkgConfig,
) -> Result<(), LocalStructureError> {
  let Some (node_ref) = tree . get (node_id)
    else { return Err(LocalStructureError {
      message: "node not found" . to_string(),
      id: ID::from ("<unknown>"),
    }); };

  let errors : Vec<String> =
    match &node_ref . value() . kind
    { MpViewnodeKind::Vognode (MpVognode::Active (t)) =>
        validate_activeVognode(tree, node_id, t, config),
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Diff (p))) =>
        validate_phantom(tree, node_id, p),
      MpViewnodeKind::BufferRoot =>
        Vec::new (),
      MpViewnodeKind::Property (Property::Alias { .. }) =>
          validate_alias(tree, node_id),
      MpViewnodeKind::PropertyFolder (PropertyFolder::Alias) =>
          validate_aliasfolder(tree, node_id),
      MpViewnodeKind::PartnerFolder (PartnerFolder::HiddenInSubscribee) =>
          validate_hiddenInSubscribee_folder(tree, node_id),
      MpViewnodeKind::PartnerFolder (PartnerFolder::HiddenOutsideOfSubscribee) =>
          validate_hiddenOutsideOfSubscribee_folder(tree, node_id),
      MpViewnodeKind::PartnerFolder (
        role @ (PartnerFolder::Hidden
          | PartnerFolder::Hider
          | PartnerFolder::Overridden
          | PartnerFolder::Overrider
          | PartnerFolder::Subscriber))
        => validate_relation_folder(tree, node_id, *role),
      MpViewnodeKind::PartnerFolder (PartnerFolder::Subscribee) =>
          validate_subscribeefolder(tree, node_id),
      MpViewnodeKind::Property (Property::TextChanged { .. }) =>
          validate_text_changed(tree, node_id),
      MpViewnodeKind::PropertyFolder (PropertyFolder::ID) =>
          validate_idFolder(tree, node_id),
      MpViewnodeKind::Property (Property::ID { .. }) =>
          validate_id_property(tree, node_id),
      MpViewnodeKind::PropertyFolder (PropertyFolder::Flags { .. }) =>
          validate_flags_folder (tree, node_id),
      MpViewnodeKind::Property (Property::Flag { .. }) =>
          validate_flag (tree, node_id),
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Deleted (_)))
        => Vec::new(),
      MpViewnodeKind::DeadViewnode => Vec::new(),
      MpViewnodeKind::Vognode (MpVognode::Inactive (_))
        => validate_inactive_node(tree, node_id),
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (_)))
        => Vec::new() };

  if errors . is_empty() {
    Ok (( ))
  } else {
    let id : ID =
      id_from_self_or_nearest_ancestor(tree, node_id)
      . unwrap_or_else(|_| ID::from ("<no ancestor ID>"));
    Err(LocalStructureError {
      message: errors . join ("; "),
      id,
    } ) }}

fn validate_alias (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_does_not_exist(tree, node_id, 1, true) {
    errors . push("Alias must have no (non-ignored) children." . to_string()); }
  if !generation_exists_and_includes(
    tree, node_id, -1, false,
    |node| matches!(&node . kind,
                    MpViewnodeKind::PropertyFolder (PropertyFolder::Alias)))
    { errors . push("Alias must have an AliasFolder parent." . to_string()); }
  errors }

fn validate_aliasfolder (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| matches!(&node . kind,
                    MpViewnodeKind::Property (Property::Alias { .. } )))
    { errors . push("AliasFolder's (non-ignored) children must include only Aliases."
                    . to_string()); }
  if !generation_exists_and_includes(
    tree, node_id, -1, false,
    |node| matches!(&node . kind,
                    MpViewnodeKind::Vognode (
                      MpVognode::Active (_) )))
    { errors . push("AliasFolder must have an ActiveVognode parent." . to_string()); }
  if !siblings_cannot_include(
    tree, node_id,
    |node| matches!(&node . kind,
                    MpViewnodeKind::PropertyFolder (PropertyFolder::Alias)))
    { errors . push("AliasFolder must be unique among its siblings."
                    . to_string()); }
  errors }

fn validate_flag (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new ();
  if ! generation_does_not_exist (tree, node_id, 1, true) {
    errors . push ("Flag must have no (non-ignored) children."
                   . to_string ()); }
  if ! generation_exists_and_includes (
    tree, node_id, -1, false,
    |node| matches! (&node . kind,
      MpViewnodeKind::PropertyFolder (PropertyFolder::Flags { .. })))
  { errors . push ("Flag must have a FlagsFolder parent."
                   . to_string ()); }
  errors
}

fn validate_flags_folder (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new ();
  if ! generation_includes_only (
    tree, node_id, 1, true,
    |node| matches! (&node . kind,
      MpViewnodeKind::Property (Property::Flag { .. })))
  { errors . push (
      "FlagsFolder's children must include only Flags."
      . to_string ()); }
  if ! generation_exists_and_includes (
    tree, node_id, -1, false,
    |node| matches! (&node . kind,
      MpViewnodeKind::Vognode (MpVognode::Active (_))))
  { errors . push ("FlagsFolder must have an ActiveVognode parent."
                   . to_string ()); }
  if ! siblings_cannot_include (
    tree, node_id,
    |node| matches! (&node . kind,
      MpViewnodeKind::PropertyFolder (PropertyFolder::Flags { .. })))
  { errors . push (
      "FlagsFolder must be unique among its siblings." . to_string ()); }
  errors
}

/// Read the error messages to see what this validates.
fn validate_hiddenInSubscribee_folder (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, node_id, -1, false,
    |node| node . is_active_or_diff_phantom ())
    { errors . push(
        "HiddenInSubscribeeFolder must have an ActiveVognode parent (the subscribee)"
        . to_string()); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| node . is_active_or_diff_phantom ()
           || matches! ( &node . kind,
                         MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (_))) ))
    { errors . push(
        "HiddenInSubscribeeFolder's children can only be ActiveVognodes or Unknown placeholders (to hide)."
        . to_string()); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| match &node . kind {
      MpViewnodeKind::Vognode (MpVognode::Active (t))
        => t . affectsParent == AffectsParent::True,
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Diff (_)))
        => true,
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (_)))
        => true,
      _ => false, } )
    { errors . push(
        "HiddenInSubscribeeFolder ActiveVognode children must have affectsParent=true."
      . to_string()); }
  if !siblings_cannot_include(
    tree, node_id,
    |node| matches!(&node . kind,
                    MpViewnodeKind::PartnerFolder (
                      PartnerFolder::HiddenInSubscribee )))
    { errors . push("HiddenInSubscribeeFolder must be unique among its siblings."
                    . to_string()); }
  if !partnerFolder_children_have_distinct_ids(tree, node_id)
    { errors . push(
      "HiddenInSubscribeeFolder must not have duplicate ActiveVognode children."
        . to_string() ); }
  errors }

fn validate_hiddenOutsideOfSubscribee_folder (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, node_id, -1, false,
    |node| matches!(&node . kind,
                    MpViewnodeKind::PartnerFolder (
                      PartnerFolder::Subscribee)))
    { errors . push(
        "HiddenOutsideOfSubscribeeFolder must have a SubscribeeFolder parent."
        . to_string()); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| node . is_active_or_diff_phantom ()
           || matches! ( &node . kind,
                         MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (_))) ))
    { errors . push("HiddenOutsideOfSubscribeeFolder's children must include only ActiveVognodes or Unknown placeholders." . to_string()); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| match &node . kind {
      MpViewnodeKind::Vognode (MpVognode::Active (t))
        => t . affectsParent == AffectsParent::True,
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Diff (_)))
        => true,
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (_)))
        => true,
      _ => false, } )
    { errors . push(
        "HiddenOutsideOfSubscribeeFolder ActiveVognode children must be affectsParent=true."
        . to_string()); }
  if !siblings_cannot_include(
    tree, node_id,
    |node| matches!(&node . kind,
                    MpViewnodeKind::PartnerFolder (
                      PartnerFolder::HiddenOutsideOfSubscribee )))
    { errors . push(
        "HiddenOutsideOfSubscribeeFolder must be unique among its siblings."
        . to_string()); }
  if !partnerFolder_children_have_distinct_ids(tree, node_id)
    { errors . push(
        "HiddenOutsideOfSubscribeeFolder must not have duplicate ActiveVognode children."
        . to_string() ); }
  errors }

fn validate_subscribeefolder (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, node_id, -1, false,
    |node| node . is_active_or_diff_phantom ())
    { errors . push("SubscribeeFolder must have an ActiveVognode parent." . to_string()); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| node . is_active_or_diff_phantom ()
           || matches!(&node . kind,
                    MpViewnodeKind::Vognode (MpVognode::Inactive (_)) // a retained inactive subscribee may sit here as an inert display placeholder; it emits no subscribes_to membership (TODO/full-schema/9-2_repo-set-safety.org)
                      | MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (_)))
                      | MpViewnodeKind::PartnerFolder (
                          PartnerFolder::HiddenOutsideOfSubscribee) ))
    { errors . push( "SubscribeeFolder's children must include only ActiveVognodes, Unknown or inactive placeholders, or HiddenOutsideOfSubscribeeFolder." . to_string()); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| match &node . kind {
      MpViewnodeKind::Vognode (MpVognode::Active (t)) =>
        t . affectsParent == AffectsParent::True,
      MpViewnodeKind::Vognode (MpVognode::Inactive (_)) =>
        true,
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Diff (_))) =>
        true,
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (_))) =>
        true,
      MpViewnodeKind::PartnerFolder (
        PartnerFolder::HiddenOutsideOfSubscribee)
        => true,
      _ => false, } )
    { errors . push("SubscribeeFolder ActiveVognode children must have affectsParent=true."
                    . to_string() ); }
  // There is no duplicate-member check here: SubscribeeFolder is a
  // defining folder, and duplicate members of defining folders are
  // silently deduplicated at emission rather than bouncing the save.
  errors }

/// PURPOSE: See the error messages it could return.
/// PITFALL: Does not check whether the members are correct for the graph;
/// save extraction and completion decide relation meaning later.
fn validate_relation_folder (
  tree     : &Tree<MpViewnode>,
  node_id  : NodeId,
  partnerFolder  : PartnerFolder,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  let label : String = partnerFolder . repr_in_client () . to_string ();
  if !generation_exists_and_includes(
    tree, node_id, -1, false,
    |node| node . is_active_or_diff_phantom ())
    { errors . push(format!("{} must have an ActiveVognode parent.", label)); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| node . is_active_or_diff_phantom ()
           || ( partnerFolder == PartnerFolder::Overridden
                && matches! ( &node . kind,
                              MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (_))) ))
           || matches!(&node . kind,
                       MpViewnodeKind::Vognode (MpVognode::Inactive (_)))) // tolerated from stale buffers; the rerender removes it (TODO/full-schema/9-2_repo-set-safety.org)
    { errors . push(format!("{}'s children must include only ActiveVognodes, inactive placeholders, or (for OverriddenFolder) Unknown placeholders.", label)); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| match &node . kind {
      MpViewnodeKind::Vognode (MpVognode::Active (t))
        => t . affectsParent == AffectsParent::True,
      MpViewnodeKind::Vognode (MpVognode::Inactive (_))
        => true,
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Diff (_)))
        => true,
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (_)))
        if partnerFolder == PartnerFolder::Overridden => true,
      _ => false, } )
    { errors . push(format!(
        "{} ActiveVognode children must have affectsParent=true.", label)); }
  if !siblings_cannot_include(
    tree, node_id,
    |node| matches!(&node . kind,
                    MpViewnodeKind::PartnerFolder (r)
                    if *r == partnerFolder))
    { errors . push(format!("{} must be unique among its siblings.", label)); }
  if partnerFolder != PartnerFolder::Overridden
    // OverriddenFolder is a defining folder: duplicate members are silently
    // deduplicated at emission rather than bouncing the save. The
    // write-protected roles keep the check.
    && !partnerFolder_children_have_distinct_ids(tree, node_id) {
    errors . push(format!(
      "{} must not have duplicate ActiveVognode children.", label)); }
  errors }

fn validate_text_changed (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, node_id, -1, false,
    |node| node . is_active_or_diff_phantom ())
    { errors . push("TextChanged must have an ActiveVognode parent." . to_string()); }
  if !generation_does_not_exist(tree, node_id, 1, true) {
    errors . push("TextChanged must have no (non-ignored) children." . to_string()); }
  if !siblings_cannot_include(
    tree, node_id,
    |node| matches!(&node . kind,
                    MpViewnodeKind::Property (Property::TextChanged { .. })))
    { errors . push("TextChanged must be unique among its siblings." . to_string()); }
  errors }

fn validate_idFolder (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, node_id, -1, false,
    |node| node . is_active_or_diff_phantom ())
    { errors . push("IDFolder must have an ActiveVognode parent." . to_string()); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| matches!(&node . kind,
                    MpViewnodeKind::Property (Property::ID { .. } )) )
    { errors . push("IDFolder's (non-ignored) children can only be ID properties."
                    . to_string() ); }
  if !siblings_cannot_include(
    tree, node_id,
    |node| matches!(&node . kind,
                    MpViewnodeKind::PropertyFolder (PropertyFolder::ID)))
    { errors . push("IDFolder must be unique among its siblings." . to_string()); }
  errors }

fn validate_id_property (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_does_not_exist(tree, node_id, 1, true) {
    errors . push("ID property must have no (non-ignored) children." . to_string()); }
  if !generation_exists_and_includes(
    tree, node_id, -1, false,
    |node| matches!(&node . kind,
                    MpViewnodeKind::PropertyFolder (PropertyFolder::ID)))
    { errors . push("ID property must have an IDFolder parent." . to_string()); }
  errors }

fn validate_inactive_node (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  // TODO/full-schema/9-2_repo-set-safety.org: an InactiveVognode may
  // sit under a folder (a stale buffer from before a repo-set
  // switch) or under another gnode, and it may have children (the
  // retained case: an inactive node kept on screen because of its
  // active children).  Its own content stays write-protected -- the
  // parser rejects title/body text on it -- so the only structural
  // demand left is a parent that can carry it at all.
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, node_id, -1, false,
    |node| node . is_active_or_diff_phantom ()
           || matches! ( &node . kind,
                         MpViewnodeKind::Vognode (MpVognode::Inactive (_))
                         | MpViewnodeKind::PropertyFolder (_)
                         | MpViewnodeKind::PartnerFolder (_)
                         | MpViewnodeKind::DeadViewnode
                         | MpViewnodeKind::BufferRoot )) // an InactiveVognode can be a view root: a root that went inactive but was retained for its active children
    { errors . push("Inactive placeholder must have an ActiveVognode, folder or DeadViewnode parent, or be a view root."
                    . to_string()); }
  errors }

/// The identity + child-structure checks shared by an ActiveVognode and a phantom
/// (TODO/DONE/local-view-update/plan_v2.org §20.4 dedup): id present, no
/// wrong-structure child, and distinct content-child ids. `label` ("ActiveVognode"
/// / "Phantom") is woven into the messages so each kind reports itself.
/// Repo validity is NOT checked here: repo is load-bearing only for a
/// ActiveVognode (it is the node's .skg file path), so validate_activeVognode adds that
/// check; a phantom writes nothing and is ignored at save, so its repo --
/// which may be the NOT_FOUND sentinel for an unresolvable reference -- is
/// inert and goes unchecked. (validate_activeVognode also appends the
/// definitive-title check; a phantom is title-exempt, being write-protected.)
fn validate_gnode_identity_and_structure (
  tree       : &Tree<MpViewnode>,
  node_id    : NodeId,
  id_present : bool,
  label      : &str,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !id_present {
    errors . push( format!("{} must have an ID.", label) ); }
  let is_subscribee_as_such : bool =
    // A subscribee-as-such (gnode child of a SubscribeeFolder) is the
    // one gnode position that legitimately carries a
    // HiddenInSubscribeeFolder; rendering puts the folder there, so a
    // saved buffer must round-trip it.
    tree . get (node_id)
    . and_then ( |n| n . parent () )
    . map ( |p| matches! ( & p . value () . kind,
              MpViewnodeKind::PartnerFolder (PartnerFolder::Subscribee) ))
    . unwrap_or (false);
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| !cannot_be_child_of_gnode (node, is_subscribee_as_such))
    { errors . push( format!("{} has a child whose structure belongs elsewhere: BufferRoot, Alias, ID, HiddenInSubscribeeFolder (outside a subscribee-as-such), or HiddenOutsideOfSubscribeeFolder.", label) ); }
  if !nonignored_children_have_distinct_ids(tree, node_id) {
    errors . push( format!("{}'s non-ignored content children must be unique (no two sharing the same ID).", label) ); }
  errors }

fn validate_activeVognode (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
  t       : &MpActiveVognode,
  config  : &SkgConfig,
) -> Vec<String> {
  let mut errors : Vec<String> =
    validate_gnode_identity_and_structure (
      tree, node_id, has_id (t), "ActiveVognode" );
  if !has_valid_repo (t, config) {
    errors . push("ActiveVognode must have a repo that exists in the config."
                  . to_string()); }
  if t . id . is_none () && matches! (
    t . edit_request (), Some (NodeEditRequest::SetFlag { .. }))
  { errors . push (
      "A flag request requires a saved node ID; save the node first."
      . to_string ()); }
  if has_empty_title (t) {
    errors . push("Definitive node has an empty title." . to_string()); }
  errors }

/// Validate a phantom (TODO/DONE/local-view-update/plan_v2.org §11): the same
/// identity and child-structure checks as an ActiveVognode, minus the two
/// ActiveVognode-only rules. The definitive-title rule does not apply (a phantom is
/// always write-protected, hence exempt). The repo-in-config rule does not
/// apply either: a phantom writes nothing and is ignored at save, so its
/// repo -- possibly the NOT_FOUND sentinel for a reference that resolves to
/// no repo -- is inert.
fn validate_phantom (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
  p       : &MpPhantomDiff,
) -> Vec<String> {
  validate_gnode_identity_and_structure (
    tree, node_id, p . id . is_some (), "Phantom" ) }

fn cannot_be_child_of_gnode (
  node : &MpViewnode,
  affects_parent_subscribee_as_such : bool,
) -> bool {
  matches!(&node . kind,
    MpViewnodeKind::BufferRoot |
    MpViewnodeKind::Property (
      Property::Alias { .. } | Property::ID { .. } | Property::Flag { .. }) |
    MpViewnodeKind::PartnerFolder (PartnerFolder::HiddenOutsideOfSubscribee))
  || ( ! affects_parent_subscribee_as_such
       // validate_hiddenin REQUIRES a gnode parent; a
       // subscribee-as-such is the position that warrants one.
       && matches!(&node . kind,
            MpViewnodeKind::PartnerFolder (PartnerFolder::HiddenInSubscribee)) ) }

/// Check if an MpActiveVognode has an ID.
pub fn has_id ( t : &MpActiveVognode ) -> bool {
  t . id . is_some() }

/// Check if an MpActiveVognode has a repo and it exists in the config.
pub fn has_valid_repo (
  t      : &MpActiveVognode,
  config : &SkgConfig,
) -> bool {
  t . home_repo . as_ref()
    . is_some_and( |s| config . repos . contains_key (s) ) }

/// A definitive node (not marked for deletion) must have a non-empty title.
/// Nodes that are write-protected or carry a delete request are exempt.
fn has_empty_title ( t : &MpActiveVognode ) -> bool {
  let is_definitive : bool =
    matches! ( &t . editability, Editability::Definitive { .. } );
  let is_delete : bool =
    matches! ( t . edit_request (),
               Some (&NodeEditRequest::Delete) );
  is_definitive && !is_delete && t . title . trim () . is_empty () }

/// Check that all non-ignored, non-phantom content children
/// have distinct IDs.
/// "Non-ignored" means affectsParent == Affected.
/// "Non-phantom" means diff is not Removed or RemovedHere.
/// Returns true if all such children have distinct IDs,
/// or if there are no such children.
pub fn nonignored_children_have_distinct_ids (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
) -> bool {
  let Some (node_ref) = tree . get (node_id)
    else { return true; };
  let mut seen : HashSet<ID> = HashSet::new();
  for child in node_ref . children() {
    let content_id : Option<ID> =
      // collected IDs, not own IDs: distinctness guards the
      // collected contains list, and a drawn overrider can
      // legitimately appear twice when it stands for two distinct
      // originals.
      match &child . value() . kind {
        MpViewnodeKind::Vognode (MpVognode::Active (t))
          if t . affectsParent == AffectsParent::True
          => t . collected_id (),
        MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (u))) =>
          Some (u . id . clone()),
        // An inactive placeholder is not a content member (its
        // membership is owned by the disk weave), so it does not
        // participate in content-id distinctness.
        _ => None };
    if let Some (id) = content_id {
      if !seen . insert(id) {
        return false; }}}
  true }

fn partnerFolder_children_have_distinct_ids (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
) -> bool {
  let Some (node_ref) = tree . get (node_id)
    else { return true; };
  let mut seen : HashSet<ID> = HashSet::new();
  for child in node_ref . children() {
    let Some (id) =
      (match &child . value() . kind {
        MpViewnodeKind::Vognode (MpVognode::Active (t)) =>
          t . id . clone(),
        MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (u))) =>
          Some (u . id . clone()),
        _ => None,
      })
    else { continue; };
    if !seen . insert (id) {
      return false; }}
  true }
