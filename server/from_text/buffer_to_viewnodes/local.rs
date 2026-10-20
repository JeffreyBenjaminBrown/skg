/// Local validation functions for Viewnode trees.
/// These check structural flags of individual nodes
/// without requiring global context.

use crate::types::maybe_placed_viewnode::{MpViewnode, MpViewnodeKind, MpUnrestrictedVognode};
use crate::types::maybe_placed_viewnode::{MpVognode, MpPhantom};
use crate::types::viewnode::{NodeEditRequest, Editability, AffectsParent, PartnerFolder, Property, PropertyFolder};
use crate::types::viewnode::ChangesetMarkerKind;
use crate::types::misc::{ID, SkgConfig};
use crate::types::tree::viewnode_graphnode::{
  generation_includes_only,
  generation_exists_and_includes,
  generation_does_not_exist,
  siblings_cannot_include,
  skgid_from_self_or_nearest_ancestor,
};
use ego_tree::{Tree, NodeId};
use std::collections::HashSet;

/// Error from local structure validation.
/// Contains the error message and the ID of the nearest UnrestrictedVognode ancestor.
#[derive(Debug, Clone, PartialEq)]
pub struct LocalStructureError {
  pub message : String,
  pub skgid      : ID,
}

pub fn validate_local_structure (
  tree    : &Tree<MpViewnode>,
  treeid  : NodeId,
  config  : &SkgConfig,
) -> Result<(), LocalStructureError> {
  let Some (node_ref) = tree . get (treeid)
    else { return Err(LocalStructureError {
      message: "node not found" . to_string(),
      skgid: ID::from ("<unknown>"),
    }); };

  let errors : Vec<String> =
    match &node_ref . value() . kind
    { MpViewnodeKind::Vognode (MpVognode::Unrestricted (t)) =>
        validate_unrestrictedVognode(tree, treeid, t, config),
      MpViewnodeKind::BufferRoot =>
        Vec::new (),
      MpViewnodeKind::Property (
        Property::Alias { .. } | Property::RemovedAlias { .. }) =>
          validate_alias(tree, treeid),
      MpViewnodeKind::PropertyFolder (PropertyFolder::Alias) =>
          validate_aliasfolder(tree, treeid),
      MpViewnodeKind::PartnerFolder (PartnerFolder::HiddenInSubscribee) =>
          validate_hiddenInSubscribee_folder(tree, treeid),
      MpViewnodeKind::PartnerFolder (PartnerFolder::HiddenOutsideOfSubscribee) =>
          validate_hiddenOutsideOfSubscribee_folder(tree, treeid),
      MpViewnodeKind::PartnerFolder (
        role @ (PartnerFolder::Hidden
          | PartnerFolder::Hider
          | PartnerFolder::Container
          | PartnerFolder::Mentioner
          | PartnerFolder::Overridden
          | PartnerFolder::Overrider
          | PartnerFolder::Subscriber))
        => validate_relation_folder(tree, treeid, *role),
      MpViewnodeKind::PartnerFolder (PartnerFolder::Subscribee) =>
          validate_subscribeefolder(tree, treeid),
      MpViewnodeKind::PropertyFolder (PropertyFolder::ID) =>
          validate_idFolder(tree, treeid),
      MpViewnodeKind::Property (
        Property::ID { .. } | Property::RemovedID { .. }) =>
          validate_id_property(tree, treeid),
      MpViewnodeKind::PropertyFolder (PropertyFolder::Flags { .. }) =>
          validate_flags_folder (tree, treeid),
      MpViewnodeKind::Property (Property::Flag { .. }) =>
          validate_flag (tree, treeid),
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Deleted (_)))
        => Vec::new(),
      MpViewnodeKind::DeadViewnode => Vec::new(),
      MpViewnodeKind::Vognode (MpVognode::Restricted (_))
        => validate_restricted_node(tree, treeid),
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (_)))
        => Vec::new(),
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Historical (_)))
        => validate_historical_phantom (tree, treeid),
      MpViewnodeKind::PartnerFolder (PartnerFolder::AcquiredBy) =>
        validate_acquiredBy_folder (tree, treeid),
      MpViewnodeKind::PartnerFolder (PartnerFolder::Acquired) =>
        validate_acquired_folder (tree, treeid),
      MpViewnodeKind::Property (Property::OldText { .. }) =>
        validate_old_text (tree, treeid),
      MpViewnodeKind::Property (Property::BodyDiff { .. }) =>
        validate_body_diff (tree, treeid),
      MpViewnodeKind::Property (
        Property::HomeSkgrepoMove { .. } | Property::RelRepoMove { .. }) =>
        validate_skgrepo_move (tree, treeid),
      MpViewnodeKind::Property (Property::ChangesetMarker { kind, .. }) =>
        validate_changeset_marker (tree, treeid, *kind) };

  if errors . is_empty() {
    Ok (( ))
  } else {
    let skgid : ID =
      skgid_from_self_or_nearest_ancestor(tree, treeid)
      . unwrap_or_else(|_| ID::from ("<no ancestor ID>"));
    Err(LocalStructureError {
      message: errors . join ("; "),
      skgid,
    } ) }}

fn validate_alias (
  tree    : &Tree<MpViewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_does_not_exist(tree, treeid, 1, true) {
    errors . push("Alias must have no (non-ignored) children." . to_string()); }
  if !generation_exists_and_includes(
    tree, treeid, -1, false,
    |node| matches!(&node . kind,
                    MpViewnodeKind::PropertyFolder (PropertyFolder::Alias)))
    { errors . push("Alias must have an AliasFolder parent." . to_string()); }
  errors }

fn validate_aliasfolder (
  tree    : &Tree<MpViewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_includes_only(
    tree, treeid, 1, true,
    |node| matches!(&node . kind,
                    MpViewnodeKind::Property (
                      Property::Alias { .. } | Property::RemovedAlias { .. } )))
    { errors . push("AliasFolder's (non-ignored) children must include only Aliases."
                    . to_string()); }
  if !generation_exists_and_includes(
    tree, treeid, -1, false,
    |node| matches!(&node . kind,
                    MpViewnodeKind::Vognode (
                      MpVognode::Unrestricted (_) )))
    { errors . push("AliasFolder must have an UnrestrictedVognode parent." . to_string()); }
  if !siblings_cannot_include(
    tree, treeid,
    |node| matches!(&node . kind,
                    MpViewnodeKind::PropertyFolder (PropertyFolder::Alias)))
    { errors . push("AliasFolder must be unique among its siblings."
                    . to_string()); }
  errors }

fn validate_flag (
  tree    : &Tree<MpViewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new ();
  if ! generation_does_not_exist (tree, treeid, 1, true) {
    errors . push ("Flag must have no (non-ignored) children."
                   . to_string ()); }
  if ! generation_exists_and_includes (
    tree, treeid, -1, false,
    |node| matches! (&node . kind,
      MpViewnodeKind::PropertyFolder (PropertyFolder::Flags { .. })))
  { errors . push ("Flag must have a FlagsFolder parent."
                   . to_string ()); }
  errors
}

fn validate_flags_folder (
  tree    : &Tree<MpViewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new ();
  if ! generation_includes_only (
    tree, treeid, 1, true,
    |node| matches! (&node . kind,
      MpViewnodeKind::Property (Property::Flag { .. })))
  { errors . push (
      "FlagsFolder's children must include only Flags."
      . to_string ()); }
  if ! generation_exists_and_includes (
    tree, treeid, -1, false,
    |node| matches! (&node . kind,
      MpViewnodeKind::Vognode (MpVognode::Unrestricted (_))))
  { errors . push ("FlagsFolder must have an UnrestrictedVognode parent."
                   . to_string ()); }
  if ! siblings_cannot_include (
    tree, treeid,
    |node| matches! (&node . kind,
      MpViewnodeKind::PropertyFolder (PropertyFolder::Flags { .. })))
  { errors . push (
      "FlagsFolder must be unique among its siblings." . to_string ()); }
  errors
}

/// Read the error messages to see what this validates.
fn validate_hiddenInSubscribee_folder (
  tree    : &Tree<MpViewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, treeid, -1, false,
    |node| node . is_unrestricted ())
    { errors . push(
        "HiddenInSubscribeeFolder must have an UnrestrictedVognode parent (the subscribee)"
        . to_string()); }
  if !generation_includes_only(
    tree, treeid, 1, true,
    |node| node . is_unrestricted ()
           || matches! ( &node . kind,
                         MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (_))) ))
    { errors . push(
        "HiddenInSubscribeeFolder's children can only be UnrestrictedVognodes or Unknown phantoms (to hide)."
        . to_string()); }
  if !generation_includes_only(
    tree, treeid, 1, true,
    |node| match &node . kind {
      MpViewnodeKind::Vognode (MpVognode::Unrestricted (t))
        => t . affectsParent == AffectsParent::True,
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (_)))
        => true,
      _ => false, } )
    { errors . push(
        "HiddenInSubscribeeFolder UnrestrictedVognode children must have affectsParent=true."
      . to_string()); }
  if !siblings_cannot_include(
    tree, treeid,
    |node| matches!(&node . kind,
                    MpViewnodeKind::PartnerFolder (
                      PartnerFolder::HiddenInSubscribee )))
    { errors . push("HiddenInSubscribeeFolder must be unique among its siblings."
                    . to_string()); }
  if !partnerFolder_children_have_distinct_skgids(tree, treeid)
    { errors . push(
      "HiddenInSubscribeeFolder must not have duplicate UnrestrictedVognode children."
        . to_string() ); }
  errors }

fn validate_hiddenOutsideOfSubscribee_folder (
  tree    : &Tree<MpViewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, treeid, -1, false,
    |node| matches!(&node . kind,
                    MpViewnodeKind::PartnerFolder (
                      PartnerFolder::Subscribee)))
    { errors . push(
        "HiddenOutsideOfSubscribeeFolder must have a SubscribeeFolder parent."
        . to_string()); }
  if !generation_includes_only(
    tree, treeid, 1, true,
    |node| node . is_unrestricted ()
           || matches! ( &node . kind,
                         MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (_))) ))
    { errors . push("HiddenOutsideOfSubscribeeFolder's children must include only UnrestrictedVognodes or Unknown phantoms." . to_string()); }
  if !generation_includes_only(
    tree, treeid, 1, true,
    |node| match &node . kind {
      MpViewnodeKind::Vognode (MpVognode::Unrestricted (t))
        => t . affectsParent == AffectsParent::True,
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (_)))
        => true,
      _ => false, } )
    { errors . push(
        "HiddenOutsideOfSubscribeeFolder UnrestrictedVognode children must be affectsParent=true."
        . to_string()); }
  if !siblings_cannot_include(
    tree, treeid,
    |node| matches!(&node . kind,
                    MpViewnodeKind::PartnerFolder (
                      PartnerFolder::HiddenOutsideOfSubscribee )))
    { errors . push(
        "HiddenOutsideOfSubscribeeFolder must be unique among its siblings."
        . to_string()); }
  if !partnerFolder_children_have_distinct_skgids(tree, treeid)
    { errors . push(
        "HiddenOutsideOfSubscribeeFolder must not have duplicate UnrestrictedVognode children."
        . to_string() ); }
  errors }

fn validate_subscribeefolder (
  tree    : &Tree<MpViewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, treeid, -1, false,
    |node| node . is_unrestricted ())
    { errors . push("SubscribeeFolder must have an UnrestrictedVognode parent." . to_string()); }
  if !generation_includes_only(
    tree, treeid, 1, true,
    |node| node . is_unrestricted ()
           || matches!(&node . kind,
                    MpViewnodeKind::Vognode (MpVognode::Restricted (_)) // a retained restricted subscribee may sit here as an save-inert display placeholder; it emits no subscribesTo membership (TODO/DONE/full-schema/DONE/9-2_source-set-safety.org)
                      | MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (_)))
                      | MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Historical (_))) // a former member, in the total diff view; save-inert
                      | MpViewnodeKind::PartnerFolder (
                          PartnerFolder::HiddenOutsideOfSubscribee) ))
    { errors . push( "SubscribeeFolder's children must include only UnrestrictedVognodes, Unknown, historical or restricted vognodes, or HiddenOutsideOfSubscribeeFolder." . to_string()); }
  if !generation_includes_only(
    tree, treeid, 1, true,
    |node| match &node . kind {
      MpViewnodeKind::Vognode (MpVognode::Unrestricted (t)) =>
        t . affectsParent == AffectsParent::True,
      MpViewnodeKind::Vognode (MpVognode::Restricted (_)) =>
        true,
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (_))) =>
        true,
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Historical (_))) =>
        true,
      MpViewnodeKind::PartnerFolder (
        PartnerFolder::HiddenOutsideOfSubscribee)
        => true,
      _ => false, } )
    { errors . push("SubscribeeFolder UnrestrictedVognode children must have affectsParent=true."
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
  treeid   : NodeId,
  partnerFolder  : PartnerFolder,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  let label : String = partnerFolder . repr_in_client () . to_string ();
  if !generation_exists_and_includes(
    tree, treeid, -1, false,
    |node| node . is_unrestricted ())
    { errors . push(format!("{} must have an UnrestrictedVognode parent.", label)); }
  if !generation_includes_only(
    tree, treeid, 1, true,
    |node| node . is_unrestricted ()
           || ( partnerFolder == PartnerFolder::Overridden
                && matches! ( &node . kind,
                              MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (_))) ))
           || matches!(&node . kind,
                       MpViewnodeKind::Vognode (MpVognode::Restricted (_))) // tolerated from stale buffers; the rerender removes it (TODO/DONE/full-schema/DONE/9-2_source-set-safety.org)
           || matches!(&node . kind, // a former member, in the total diff view; save-inert
                       MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Historical (_)))))
    { errors . push(format!("{}'s children must include only UnrestrictedVognodes, restricted vognodes, historical phantoms, or (for OverriddenFolder) Unknown phantoms.", label)); }
  if !generation_includes_only(
    tree, treeid, 1, true,
    |node| match &node . kind {
      MpViewnodeKind::Vognode (MpVognode::Unrestricted (t))
        => t . affectsParent == AffectsParent::True,
      MpViewnodeKind::Vognode (MpVognode::Restricted (_))
        => true,
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (_)))
        if partnerFolder == PartnerFolder::Overridden => true,
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Historical (_)))
        => true,
      _ => false, } )
    { errors . push(format!(
        "{} UnrestrictedVognode children must have affectsParent=true.", label)); }
  if !siblings_cannot_include(
    tree, treeid,
    |node| matches!(&node . kind,
                    MpViewnodeKind::PartnerFolder (r)
                    if *r == partnerFolder))
    { errors . push(format!("{} must be unique among its siblings.", label)); }
  if partnerFolder != PartnerFolder::Overridden
    // OverriddenFolder is a defining folder: duplicate members are silently
    // deduplicated at emission rather than bouncing the save. The
    // write-protected roles keep the check.
    && !partnerFolder_children_have_distinct_skgids(tree, treeid) {
    errors . push(format!(
      "{} must not have duplicate UnrestrictedVognode children.", label)); }
  errors }

// The total diff view's viewnodes are save-inert: these checks only
// keep each where the total diff view draws it.

fn validate_historical_phantom (
  tree    : &Tree<MpViewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> =
    validate_vognode_identity_and_structure (
      tree, treeid,
      tree . get (treeid) . and_then ( |n| n . value () . skgid_opt () )
        . is_some_and ( |skgid| ! skgid . 0 . is_empty () ),
      "Historical phantom" );
  if !generation_includes_only(
    tree, treeid, 1, false,
    |node| matches!(&node . kind,
                    MpViewnodeKind::Vognode (_)
                    | MpViewnodeKind::PartnerFolder (PartnerFolder::AcquiredBy)))
    { errors . push("A historical phantom's children can only be vognodes (its old content) and an acquiredByFolder."
                    . to_string()); }
  errors }

fn validate_acquiredBy_folder (
  tree    : &Tree<MpViewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  validate_acquisition_folder (
    tree, treeid, "acquiredByFolder",
    |node| matches!(&node . kind,
                    MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Historical (_)))),
    |node| matches!(&node . kind,
                    MpViewnodeKind::Vognode (
                      MpVognode::Unrestricted (_)
                      | MpVognode::Phantom (MpPhantom::Unknown (_))))) }

fn validate_acquired_folder (
  tree    : &Tree<MpViewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  validate_acquisition_folder (
    tree, treeid, "acquiredFolder",
    |node| matches!(&node . kind,
                    MpViewnodeKind::Vognode (MpVognode::Unrestricted (_))),
    |node| matches!(&node . kind,
                    MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Historical (_))))) }

/// The checks shared by the two derived acquisition folders: which
/// parent, which children, one per parent.
fn validate_acquisition_folder (
  tree        : &Tree<MpViewnode>,
  treeid      : NodeId,
  label       : &str,
  valid_parent : fn (&MpViewnode) -> bool,
  valid_child  : fn (&MpViewnode) -> bool,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(tree, treeid, -1, false, valid_parent)
    { errors . push(format!("{} has the wrong kind of parent.", label)); }
  if !generation_includes_only(tree, treeid, 1, false, valid_child)
    { errors . push(format!("{} has the wrong kind of child.", label)); }
  let kind : Option<MpViewnodeKind> =
    tree . get (treeid) . map ( |n| n . value () . kind . clone () );
  if !siblings_cannot_include( tree, treeid, |node| Some (&node . kind) == kind . as_ref ())
    { errors . push(format!("{} must be unique among its siblings.", label)); }
  errors }

fn validate_old_text (
  tree    : &Tree<MpViewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, treeid, -1, false,
    |node| matches!(&node . kind, MpViewnodeKind::Vognode (_)))
    { errors . push("oldText must have a vognode parent." . to_string()); }
  if !generation_includes_only(
    tree, treeid, 1, false,
    |node| matches!(&node . kind,
                    MpViewnodeKind::Property (Property::BodyDiff { .. })))
    { errors . push("oldText's only child can be a bodyDiff." . to_string()); }
  if !siblings_cannot_include(
    tree, treeid,
    |node| matches!(&node . kind,
                    MpViewnodeKind::Property (Property::OldText { .. })))
    { errors . push("oldText must be unique among its siblings." . to_string()); }
  errors }

fn validate_body_diff (
  tree    : &Tree<MpViewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, treeid, -1, false,
    |node| matches!(&node . kind,
                    MpViewnodeKind::Property (Property::OldText { .. })))
    { errors . push("bodyDiff must have an oldText parent." . to_string()); }
  if !generation_does_not_exist(tree, treeid, 1, false) {
    errors . push("bodyDiff must have no children." . to_string()); }
  errors }

fn validate_skgrepo_move (
  tree    : &Tree<MpViewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  let is_home_move : bool =
    tree . get (treeid) . is_some_and ( |n| matches! ( &n . value () . kind,
      MpViewnodeKind::Property (Property::HomeSkgrepoMove { .. }) ));
  let label : &str = if is_home_move { "homeMove" } else { "relRepoMove" };
  if !generation_exists_and_includes(
    tree, treeid, -1, false,
    |node| matches!(&node . kind, MpViewnodeKind::Vognode (_)))
    { errors . push(format!("{} must have a vognode parent.", label)); }
  if !generation_does_not_exist(tree, treeid, 1, false) {
    errors . push(format!("{} must have no children.", label)); }
  if !siblings_cannot_include(
    tree, treeid,
    |node| match &node . kind {
      MpViewnodeKind::Property (Property::HomeSkgrepoMove { .. }) => is_home_move,
      MpViewnodeKind::Property (Property::RelRepoMove { .. }) => ! is_home_move,
      _ => false, } )
    { errors . push(format!("{} must be unique among its siblings.", label)); }
  errors }

fn validate_changeset_marker (
  tree    : &Tree<MpViewnode>,
  treeid  : NodeId,
  kind    : ChangesetMarkerKind,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, treeid, -1, false,
    |node| matches!(&node . kind, MpViewnodeKind::Vognode (_)))
    { errors . push("changesetMarker must have a vognode parent." . to_string()); }
  if !generation_does_not_exist(tree, treeid, 1, false) {
    errors . push("changesetMarker must have no children." . to_string()); }
  if !siblings_cannot_include(
    tree, treeid,
    |node| matches!(&node . kind,
                    MpViewnodeKind::Property (Property::ChangesetMarker {
                      kind : sibling_kind, .. })
                    if *sibling_kind == kind))
    { errors . push("A changesetMarker of each kind must be unique among its siblings."
                    . to_string()); }
  errors }

fn validate_idFolder (
  tree    : &Tree<MpViewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, treeid, -1, false,
    |node| node . is_unrestricted ())
    { errors . push("IDFolder must have an UnrestrictedVognode parent." . to_string()); }
  if !generation_includes_only(
    tree, treeid, 1, true,
    |node| matches!(&node . kind,
                    MpViewnodeKind::Property (
                      Property::ID { .. } | Property::RemovedID { .. } )) )
    { errors . push("IDFolder's (non-ignored) children can only be ID properties."
                    . to_string() ); }
  if !siblings_cannot_include(
    tree, treeid,
    |node| matches!(&node . kind,
                    MpViewnodeKind::PropertyFolder (PropertyFolder::ID)))
    { errors . push("IDFolder must be unique among its siblings." . to_string()); }
  errors }

fn validate_id_property (
  tree    : &Tree<MpViewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_does_not_exist(tree, treeid, 1, true) {
    errors . push("ID property must have no (non-ignored) children." . to_string()); }
  if !generation_exists_and_includes(
    tree, treeid, -1, false,
    |node| matches!(&node . kind,
                    MpViewnodeKind::PropertyFolder (PropertyFolder::ID)))
    { errors . push("ID property must have an IDFolder parent." . to_string()); }
  errors }

fn validate_restricted_node (
  tree    : &Tree<MpViewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  // TODO/DONE/full-schema/DONE/9-2_source-set-safety.org: a RestrictedVognode may
  // sit under a folder (a stale buffer from before a skgrepo-set
  // switch) or under another vognode, and it may have children (the
  // retained case: a restricted node kept on screen because of its
  // unrestricted children).  Its own content stays write-protected -- the
  // parser rejects title/body text on it -- so the only structural
  // demand left is a parent that can carry it at all.
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, treeid, -1, false,
    |node| node . is_unrestricted ()
           || matches! ( &node . kind,
                         MpViewnodeKind::Vognode (MpVognode::Restricted (_))
                         | MpViewnodeKind::PropertyFolder (_)
                         | MpViewnodeKind::PartnerFolder (_)
                         | MpViewnodeKind::DeadViewnode
                         | MpViewnodeKind::BufferRoot )) // a RestrictedVognode can be a view root: a root that went restricted but was retained for its unrestricted children
    { errors . push("Restricted vognode must have an UnrestrictedVognode, folder or DeadViewnode parent, or be a view root."
                    . to_string()); }
  errors }

/// The identity + child-structure checks shared by an UnrestrictedVognode and a phantom
/// (TODO/DONE/local-view-update/plan_v2.org §20.4 dedup): id present, no
/// wrong-structure child, and distinct content-child ids. `label` ("UnrestrictedVognode"
/// / "Phantom") is woven into the messages so each kind reports itself.
/// Repo validity is NOT checked here: skgrepo is load-bearing only for a
/// UnrestrictedVognode (it is the node's .skg file path), so validate_unrestrictedVognode adds that
/// check; a phantom writes nothing and is ignored at save, so its skgrepo --
/// which may be the NOT_FOUND sentinel for an unresolvable reference -- is
/// save-inert and goes unchecked. (validate_unrestrictedVognode also appends the
/// editable-title check; a phantom is title-exempt, being write-protected.)
fn validate_vognode_identity_and_structure (
  tree       : &Tree<MpViewnode>,
  treeid     : NodeId,
  id_present : bool,
  label      : &str,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !id_present {
    errors . push( format!("{} must have an ID.", label) ); }
  let is_subscribee_as_such : bool =
    // A subscribee-as-such (vognode child of a SubscribeeFolder) is the
    // one vognode position that legitimately carries a
    // HiddenInSubscribeeFolder; rendering puts the folder there, so a
    // saved buffer must round-trip it.
    tree . get (treeid)
    . and_then ( |n| n . parent () )
    . map ( |p| matches! ( & p . value () . kind,
              MpViewnodeKind::PartnerFolder (PartnerFolder::Subscribee) ))
    . unwrap_or (false);
  if !generation_includes_only(
    tree, treeid, 1, true,
    |node| !cannot_be_child_of_vognode (node, is_subscribee_as_such))
    { errors . push( format!("{} has a child whose structure belongs elsewhere: BufferRoot, Alias, ID, HiddenInSubscribeeFolder (outside a subscribee-as-such), or HiddenOutsideOfSubscribeeFolder.", label) ); }
  if !nonignored_children_have_distinct_skgids(tree, treeid) {
    errors . push( format!("{}'s non-ignored content children must be unique (no two sharing the same ID).", label) ); }
  errors }

fn validate_unrestrictedVognode (
  tree    : &Tree<MpViewnode>,
  treeid  : NodeId,
  t       : &MpUnrestrictedVognode,
  config  : &SkgConfig,
) -> Vec<String> {
  let mut errors : Vec<String> =
    validate_vognode_identity_and_structure (
      tree, treeid, has_skgid (t), "UnrestrictedVognode" );
  if !has_valid_skgrepo (t, config) {
    errors . push("UnrestrictedVognode must have a repo that exists in the config."
                  . to_string()); }
  if t . skgid . is_none () && matches! (
    t . edit_request (), Some (NodeEditRequest::SetFlag { .. }))
  { errors . push (
      "A flag request requires a saved node ID; save the node first."
      . to_string ()); }
  if has_empty_title (t) {
    errors . push("Editable node has an empty title." . to_string()); }
  errors }

fn cannot_be_child_of_vognode (
  node : &MpViewnode,
  affects_parent_subscribee_as_such : bool,
) -> bool {
  matches!(&node . kind,
    MpViewnodeKind::BufferRoot |
    MpViewnodeKind::Property (
      Property::Alias { .. } | Property::ID { .. } | Property::Flag { .. }
      | Property::RemovedAlias { .. } | Property::RemovedID { .. }) |
    MpViewnodeKind::PartnerFolder (PartnerFolder::HiddenOutsideOfSubscribee))
  || ( ! affects_parent_subscribee_as_such
       // validate_hiddenin REQUIRES a vognode parent; a
       // subscribee-as-such is the position that warrants one.
       && matches!(&node . kind,
            MpViewnodeKind::PartnerFolder (PartnerFolder::HiddenInSubscribee)) ) }

/// Check if an MpUnrestrictedVognode has an ID.
pub fn has_skgid ( t : &MpUnrestrictedVognode ) -> bool {
  t . skgid . is_some() }

/// Check if an MpUnrestrictedVognode has a skgrepo and it exists in the config.
pub fn has_valid_skgrepo (
  t      : &MpUnrestrictedVognode,
  config : &SkgConfig,
) -> bool {
  t . home_skgrepo . as_ref()
    . is_some_and( |s| config . skgrepos . contains_key (s) ) }

/// An editable node (not marked for deletion) must have a non-empty title.
/// Nodes that are write-protected or carry a delete request are exempt.
fn has_empty_title ( t : &MpUnrestrictedVognode ) -> bool {
  let is_editable : bool =
    matches! ( &t . editability, Editability::Editable { .. } );
  let is_delete : bool =
    matches! ( t . edit_request (),
               Some (&NodeEditRequest::Delete) );
  is_editable && !is_delete && t . title . trim () . is_empty () }

/// Check that all non-ignored content children
/// have distinct IDs.
/// "Non-ignored" means affectsParent == True.
/// Returns true if all such children have distinct IDs,
/// or if there are no such children.
pub fn nonignored_children_have_distinct_skgids (
  tree    : &Tree<MpViewnode>,
  treeid  : NodeId,
) -> bool {
  let Some (node_ref) = tree . get (treeid)
    else { return true; };
  let mut seen : HashSet<ID> = HashSet::new();
  for child in node_ref . children() {
    let content_skgid : Option<ID> =
      // collected IDs, not own IDs: distinctness guards the
      // collected contains list, and a drawn overrider can
      // legitimately appear twice when it stands for two distinct
      // originals.
      match &child . value() . kind {
        MpViewnodeKind::Vognode (MpVognode::Unrestricted (t))
          if t . affectsParent == AffectsParent::True
          => t . collected_skgid (),
        MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (u))) =>
          Some (u . skgid . clone()),
        // A restricted vognode is not a content member (its
        // membership is owned by disk supplementation), so it does not
        // participate in content-id distinctness.
        _ => None };
    if let Some (skgid) = content_skgid {
      if !seen . insert(skgid) {
        return false; }}}
  true }

fn partnerFolder_children_have_distinct_skgids (
  tree    : &Tree<MpViewnode>,
  treeid  : NodeId,
) -> bool {
  let Some (node_ref) = tree . get (treeid)
    else { return true; };
  let mut seen : HashSet<ID> = HashSet::new();
  for child in node_ref . children() {
    let Some (skgid) =
      (match &child . value() . kind {
        MpViewnodeKind::Vognode (MpVognode::Unrestricted (t)) =>
          t . skgid . clone(),
        MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (u))) =>
          Some (u . skgid . clone()),
        _ => None,
      })
    else { continue; };
    if !seen . insert (skgid) {
      return false; }}
  true }
