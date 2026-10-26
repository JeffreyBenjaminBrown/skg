/// Local validation functions for Viewnode trees.
/// These check structural flags of individual nodes
/// without requiring global context.

use crate::types::viewnode::{Viewnode, ViewnodeKind, Occurrence};
use crate::types::viewnode::{Vognode};
use crate::types::viewnode::{RelationshipEditRequest, NodeEditRequest, Editability, Folder, PropertyMember, ChangeViewnode};
use crate::types::viewnode::ChangesetMarkerKind;
use crate::types::errors::HeadlineName;
use crate::serve::parse_metadata_sexp::RESTORE_REL_WITHOUT_A_REMOVED_MEMBERSHIP;
use crate::types::misc::{ID, SkgConfig};
use crate::refresh::ancestry::parent_reads_claims;
use crate::types::tree::forest::stands_in_containerward_chain;
use crate::types::tree::viewnode_graphnode::{
  generation_includes_only,
  generation_exists_and_includes,
  generation_does_not_exist,
  siblings_cannot_include,
  gnid_from_self_or_nearest_ancestor,
};
use ego_tree::{Tree, NodeId, NodeRef};
use std::collections::HashSet;

/// Error from local structure validation.
/// Contains the error message, the ID of the nearest Occurrence ancestor,
/// and the name of the headline in violation.
#[derive(Debug, Clone, PartialEq)]
pub struct LocalStructureError {
  pub message  : String,
  pub gnid    : ID,
  pub headline : HeadlineName,
}

pub fn validate_local_structure (
  tree    : &Tree<Viewnode>,
  treeid  : NodeId,
  config  : &SkgConfig,
) -> Result<(), LocalStructureError> {
  let Some (node_ref) = tree . get (treeid)
    else { return Err(LocalStructureError {
      message: "node not found" . to_string(),
      gnid: ID::from ("<unknown>"),
      headline: HeadlineName::of ( None, None, "" ),
    }); };

  let errors : Vec<String> =
    match &node_ref . value() . kind
    { ViewnodeKind::Vognode (Vognode::Occurrence (t)) =>
        validate_occurrence(tree, treeid, t, config),
      ViewnodeKind::BufferRoot =>
        Vec::new (),
      ViewnodeKind::PropertyMember (PropertyMember::Alias { .. }) =>
          validate_alias(tree, treeid),
      ViewnodeKind::Folder (Folder::Alias) =>
          validate_aliasfolder(tree, treeid),
      ViewnodeKind::Folder (Folder::HiddenInSubscribee) =>
          validate_hiddenInSubscribee_folder(tree, treeid),
      ViewnodeKind::Folder (Folder::HiddenOutsideOfSubscribee) =>
          validate_hiddenOutsideOfSubscribee_folder(tree, treeid),
      ViewnodeKind::Folder (
        role @ (Folder::Hidden
          | Folder::Hider
          | Folder::Container
          | Folder::Mentioner
          | Folder::Overridden
          | Folder::Overrider
          | Folder::Subscriber))
        => validate_relation_folder(tree, treeid, role),
      ViewnodeKind::Folder (Folder::Subscribee) =>
          validate_subscribeefolder(tree, treeid),
      ViewnodeKind::Folder (Folder::ID) =>
          validate_idFolder(tree, treeid),
      ViewnodeKind::PropertyMember (PropertyMember::ID { .. }) =>
          validate_id_property(tree, treeid),
      ViewnodeKind::Folder (Folder::Flags { .. }) =>
          validate_flags_folder (tree, treeid),
      ViewnodeKind::PropertyMember (PropertyMember::Flag { .. }) =>
          validate_flag (tree, treeid),
      ViewnodeKind::Tombstone => Vec::new(),
      ViewnodeKind::Vognode (Vognode::Unknown (_))
        => Vec::new(),
      ViewnodeKind::Vognode (Vognode::Ghost (_))
        => validate_ghost (tree, treeid),
      ViewnodeKind::Folder (Folder::AcquiredBy) =>
        validate_acquiredBy_folder (tree, treeid),
      ViewnodeKind::Folder (Folder::Acquired) =>
        validate_acquired_folder (tree, treeid),
      ViewnodeKind::ChangeViewnode (ChangeViewnode::OldText { .. }) =>
        validate_old_text (tree, treeid),
      ViewnodeKind::ChangeViewnode (ChangeViewnode::BodyDiff { .. }) =>
        validate_body_diff (tree, treeid),
      ViewnodeKind::ChangeViewnode (
        ChangeViewnode::HomeSkgrepoMove { .. } | ChangeViewnode::RelRepoMove { .. }) =>
        validate_skgrepo_move (tree, treeid),
      ViewnodeKind::ChangeViewnode (ChangeViewnode::ChangesetMarker { kind, .. }) =>
        validate_changeset_marker (tree, treeid, *kind) };

  if errors . is_empty() {
    Ok (( ))
  } else {
    let gnid : ID =
      gnid_from_self_or_nearest_ancestor(tree, treeid)
      . unwrap_or_else(|_| ID::from ("<no ancestor ID>"));
    Err(LocalStructureError {
      message: errors . join ("; "),
      gnid,
      headline: node_ref . value () . headline_name (),
    } ) }}

/// 'predicate', or a tombstone. Every folder accepts a
/// tombstone among its children: it is what the refresh makes of a
/// member that can stand there for nothing, keeping what it holds.
fn tombstone_or<Predicate> (
  predicate : Predicate,
) -> impl Fn (&Viewnode) -> bool
where Predicate : Fn (&Viewnode) -> bool {
  move |node : &Viewnode|
    matches! ( &node . kind, ViewnodeKind::Tombstone )
    || predicate (node) }

fn validate_alias (
  tree    : &Tree<Viewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_does_not_exist(tree, treeid, 1, true) {
    errors . push("Alias must have no (non-ignored) children." . to_string()); }
  if !generation_exists_and_includes(
    tree, treeid, -1, false,
    |node| matches!(&node . kind,
                    ViewnodeKind::Folder (Folder::Alias)))
    { errors . push("Alias must have an AliasFolder parent." . to_string()); }
  errors }

fn validate_aliasfolder (
  tree    : &Tree<Viewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_includes_only(
    tree, treeid, 1, true,
    tombstone_or (|node| matches!(&node . kind,
                    ViewnodeKind::PropertyMember (
                      PropertyMember::Alias { .. } ))))
    { errors . push("AliasFolder's (non-ignored) children must include only Aliases."
                    . to_string()); }
  if !generation_exists_and_includes(
    tree, treeid, -1, false,
    |node| matches!(&node . kind,
                    ViewnodeKind::Vognode (
                      Vognode::Occurrence (_) )))
    { errors . push("AliasFolder must have an Occurrence parent." . to_string()); }
  if !siblings_cannot_include(
    tree, treeid,
    |node| matches!(&node . kind,
                    ViewnodeKind::Folder (Folder::Alias)))
    { errors . push("AliasFolder must be unique among its siblings."
                    . to_string()); }
  errors }

fn validate_flag (
  tree    : &Tree<Viewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new ();
  if ! generation_does_not_exist (tree, treeid, 1, true) {
    errors . push ("Flag must have no (non-ignored) children."
                   . to_string ()); }
  if ! generation_exists_and_includes (
    tree, treeid, -1, false,
    |node| matches! (&node . kind,
      ViewnodeKind::Folder (Folder::Flags { .. })))
  { errors . push ("Flag must have a FlagsFolder parent."
                   . to_string ()); }
  errors
}

fn validate_flags_folder (
  tree    : &Tree<Viewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new ();
  if ! generation_includes_only (
    tree, treeid, 1, true,
    tombstone_or (|node| matches! (&node . kind,
      ViewnodeKind::PropertyMember (PropertyMember::Flag { .. }))))
  { errors . push (
      "FlagsFolder's children must include only Flags."
      . to_string ()); }
  if ! generation_exists_and_includes (
    tree, treeid, -1, false,
    |node| matches! (&node . kind,
      ViewnodeKind::Vognode (Vognode::Occurrence (_))))
  { errors . push ("FlagsFolder must have an Occurrence parent."
                   . to_string ()); }
  if ! siblings_cannot_include (
    tree, treeid,
    |node| matches! (&node . kind,
      ViewnodeKind::Folder (Folder::Flags { .. })))
  { errors . push (
      "FlagsFolder must be unique among its siblings." . to_string ()); }
  errors
}

/// Read the error messages to see what this validates.
fn validate_hiddenInSubscribee_folder (
  tree    : &Tree<Viewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, treeid, -1, false,
    |node| node . is_occurrence ())
    { errors . push(
        "HiddenInSubscribeeFolder must have an Occurrence parent (the subscribee)"
        . to_string()); }
  if !generation_includes_only(
    tree, treeid, 1, true,
    tombstone_or (|node| node . is_occurrence ()
           || matches! ( &node . kind,
                         ViewnodeKind::Vognode (Vognode::Unknown (_)) )))
    { errors . push(
        "HiddenInSubscribeeFolder's children can only be Occurrences or unknowns (to hide)."
        . to_string()); }
  if !generation_includes_only(
    tree, treeid, 1, true,
    tombstone_or (|node| match &node . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (t))
        => t . claimsMembership,
      ViewnodeKind::Vognode (Vognode::Unknown (_))
        => true,
      _ => false, }) )
    { errors . push(
        "HiddenInSubscribeeFolder Occurrence children must be members (no noClaim)."
      . to_string()); }
  if !siblings_cannot_include(
    tree, treeid,
    |node| matches!(&node . kind,
                    ViewnodeKind::Folder (
                      Folder::HiddenInSubscribee )))
    { errors . push("HiddenInSubscribeeFolder must be unique among its siblings."
                    . to_string()); }
  if !partnerFolder_children_have_distinct_gnids(tree, treeid)
    { errors . push(
      "HiddenInSubscribeeFolder must not have duplicate Occurrence children."
        . to_string() ); }
  errors }

fn validate_hiddenOutsideOfSubscribee_folder (
  tree    : &Tree<Viewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, treeid, -1, false,
    |node| matches!(&node . kind,
                    ViewnodeKind::Folder (
                      Folder::Subscribee)))
    { errors . push(
        "HiddenOutsideOfSubscribeeFolder must have a SubscribeeFolder parent."
        . to_string()); }
  if !generation_includes_only(
    tree, treeid, 1, true,
    tombstone_or (|node| node . is_occurrence ()
           || matches! ( &node . kind,
                         ViewnodeKind::Vognode (Vognode::Unknown (_)) )))
    { errors . push("HiddenOutsideOfSubscribeeFolder's children must include only Occurrences or unknowns." . to_string()); }
  if !generation_includes_only(
    tree, treeid, 1, true,
    tombstone_or (|node| match &node . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (t))
        => t . claimsMembership,
      ViewnodeKind::Vognode (Vognode::Unknown (_))
        => true,
      _ => false, }) )
    { errors . push(
        "HiddenOutsideOfSubscribeeFolder Occurrence children must be members (no noClaim)."
        . to_string()); }
  if !siblings_cannot_include(
    tree, treeid,
    |node| matches!(&node . kind,
                    ViewnodeKind::Folder (
                      Folder::HiddenOutsideOfSubscribee )))
    { errors . push(
        "HiddenOutsideOfSubscribeeFolder must be unique among its siblings."
        . to_string()); }
  if !partnerFolder_children_have_distinct_gnids(tree, treeid)
    { errors . push(
        "HiddenOutsideOfSubscribeeFolder must not have duplicate Occurrence children."
        . to_string() ); }
  errors }

fn validate_subscribeefolder (
  tree    : &Tree<Viewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, treeid, -1, false,
    |node| node . is_occurrence ())
    { errors . push("SubscribeeFolder must have an Occurrence parent." . to_string()); }
  if !generation_includes_only(
    tree, treeid, 1, true,
    tombstone_or (|node| node . is_occurrence ()
           || matches!(&node . kind,
                    ViewnodeKind::Vognode (Vognode::Unknown (_))
                      | ViewnodeKind::Vognode (Vognode::Ghost (_)) // a former member, in the diff view; save-inert
                      | ViewnodeKind::Folder (
                          Folder::HiddenOutsideOfSubscribee) )))
    { errors . push( "SubscribeeFolder's children must include only Occurrences, tombstones, Unknown or historical vognodes, or HiddenOutsideOfSubscribeeFolder." . to_string()); }
  if !generation_includes_only(
    tree, treeid, 1, true,
    tombstone_or (|node| match &node . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (t)) =>
        t . claimsMembership,
      ViewnodeKind::Vognode (Vognode::Unknown (_)) =>
        true,
      ViewnodeKind::Vognode (Vognode::Ghost (_)) =>
        true,
      ViewnodeKind::Folder (
        Folder::HiddenOutsideOfSubscribee)
        => true,
      _ => false, }) )
    { errors . push("SubscribeeFolder Occurrence children must be members (no noClaim)."
                    . to_string() ); }
  // There is no duplicate-member check here: SubscribeeFolder is a
  // defining folder, and duplicate members of defining folders are
  // silently deduplicated at emission rather than bouncing the save.
  errors }

/// PURPOSE: See the error messages it could return.
/// PITFALL: Does not check whether the members are correct for the graph;
/// save extraction and completion decide relation meaning later.
fn validate_relation_folder (
  tree     : &Tree<Viewnode>,
  treeid   : NodeId,
  partnerFolder  : &Folder,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  let label : String = partnerFolder . repr_in_client () . to_string ();
  if !generation_exists_and_includes(
    tree, treeid, -1, false,
    |node| node . is_occurrence ())
    { errors . push(format!("{} must have an Occurrence parent.", label)); }
  if !generation_includes_only(
    tree, treeid, 1, true,
    tombstone_or (|node| node . is_occurrence ()
           || ( *partnerFolder == Folder::Overridden
                && matches! ( &node . kind,
                              ViewnodeKind::Vognode (Vognode::Unknown (_)) ))
           || matches!(&node . kind, // a former member, in the diff view; save-inert
                       ViewnodeKind::Vognode (Vognode::Ghost (_)))))
    { errors . push(format!("{}'s children must include only Occurrences, tombstones, ghosts, or (for OverriddenFolder) unknowns.", label)); }
  if !generation_includes_only(
    tree, treeid, 1, true,
    tombstone_or (|node| match &node . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (t))
        => t . claimsMembership,
      ViewnodeKind::Vognode (Vognode::Unknown (_))
        if *partnerFolder == Folder::Overridden => true,
      ViewnodeKind::Vognode (Vognode::Ghost (_))
        => true,
      _ => false, }) )
    { errors . push(format!(
        "{} Occurrence children must be members (no noClaim).", label)); }
  if !siblings_cannot_include(
    tree, treeid,
    |node| matches!(&node . kind,
                    ViewnodeKind::Folder (r)
                    if r == partnerFolder))
    { errors . push(format!("{} must be unique among its siblings.", label)); }
  if *partnerFolder != Folder::Overridden
    // OverriddenFolder is a defining folder: duplicate members are silently
    // deduplicated at emission rather than bouncing the save. The
    // write-protected roles keep the check.
    && !partnerFolder_children_have_distinct_gnids(tree, treeid) {
    errors . push(format!(
      "{} must not have duplicate Occurrence children.", label)); }
  errors }

// The diff view's viewnodes are save-inert: these checks only
// keep each where the diff view shows it.

fn validate_ghost (
  tree    : &Tree<Viewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> =
    validate_vognode_identity_and_structure (
      tree, treeid,
      tree . get (treeid) . is_some_and ( |n| matches! (
        &n . value () . kind,
        ViewnodeKind::Vognode (Vognode::Ghost (ghost))
          if ! ghost . gnid . 0 . is_empty () )),
      "Ghost" );
  if !generation_includes_only(
    tree, treeid, 1, false,
    |node| matches!(&node . kind,
                    ViewnodeKind::Vognode (_)
                    | ViewnodeKind::Folder (Folder::AcquiredBy)))
    { errors . push("A ghost's children can only be vognodes (its old content) and an acquiredByFolder."
                    . to_string()); }
  errors }

fn validate_acquiredBy_folder (
  tree    : &Tree<Viewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  validate_acquisition_folder (
    tree, treeid, "acquiredByFolder",
    |node| matches!(&node . kind,
                    ViewnodeKind::Vognode (Vognode::Ghost (_))),
    |node| matches!(&node . kind,
                    ViewnodeKind::Vognode (
                      Vognode::Occurrence (_)
                      | Vognode::Unknown (_)))) }

fn validate_acquired_folder (
  tree    : &Tree<Viewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  validate_acquisition_folder (
    tree, treeid, "acquiredFolder",
    |node| matches!(&node . kind,
                    ViewnodeKind::Vognode (Vognode::Occurrence (_))),
    // An acquiree's ghost, or a note the user put there: the folder
    // is write-protected, so whatever an occurrence there carries, it
    // claims nothing.
    |node| matches!(&node . kind,
                    ViewnodeKind::Vognode (
                      Vognode::Ghost (_) | Vognode::Occurrence (_)))) }

/// The checks shared by the two derived acquisition folders: which
/// parent, which children, one per parent.
fn validate_acquisition_folder (
  tree        : &Tree<Viewnode>,
  treeid      : NodeId,
  label       : &str,
  valid_parent : fn (&Viewnode) -> bool,
  valid_child  : fn (&Viewnode) -> bool,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(tree, treeid, -1, false, valid_parent)
    { errors . push(format!("{} has the wrong kind of parent.", label)); }
  if !generation_includes_only(tree, treeid, 1, false, tombstone_or (valid_child))
    { errors . push(format!("{} has the wrong kind of child.", label)); }
  let kind : Option<ViewnodeKind> =
    tree . get (treeid) . map ( |n| n . value () . kind . clone () );
  if !siblings_cannot_include( tree, treeid, |node| Some (&node . kind) == kind . as_ref ())
    { errors . push(format!("{} must be unique among its siblings.", label)); }
  errors }

fn validate_old_text (
  tree    : &Tree<Viewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, treeid, -1, false,
    |node| matches!(&node . kind, ViewnodeKind::Vognode (_)))
    { errors . push("oldText must have a vognode parent." . to_string()); }
  if !generation_includes_only(
    tree, treeid, 1, false,
    |node| matches!(&node . kind,
                    ViewnodeKind::ChangeViewnode (ChangeViewnode::BodyDiff { .. })))
    { errors . push("oldText's only child can be a bodyDiff." . to_string()); }
  if !siblings_cannot_include(
    tree, treeid,
    |node| matches!(&node . kind,
                    ViewnodeKind::ChangeViewnode (ChangeViewnode::OldText { .. })))
    { errors . push("oldText must be unique among its siblings." . to_string()); }
  errors }

fn validate_body_diff (
  tree    : &Tree<Viewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, treeid, -1, false,
    |node| matches!(&node . kind,
                    ViewnodeKind::ChangeViewnode (ChangeViewnode::OldText { .. })))
    { errors . push("bodyDiff must have an oldText parent." . to_string()); }
  if !generation_does_not_exist(tree, treeid, 1, false) {
    errors . push("bodyDiff must have no children." . to_string()); }
  errors }

fn validate_skgrepo_move (
  tree    : &Tree<Viewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  let is_homeSkgrepoMove : bool =
    tree . get (treeid) . is_some_and ( |n| matches! ( &n . value () . kind,
      ViewnodeKind::ChangeViewnode (ChangeViewnode::HomeSkgrepoMove { .. }) ));
  let label : &str = if is_homeSkgrepoMove { "homeSkgrepoMove" } else { "relRepoMove" };
  if !generation_exists_and_includes(
    tree, treeid, -1, false,
    |node| matches!(&node . kind, ViewnodeKind::Vognode (_)))
    { errors . push(format!("{} must have a vognode parent.", label)); }
  if !generation_does_not_exist(tree, treeid, 1, false) {
    errors . push(format!("{} must have no children.", label)); }
  if !siblings_cannot_include(
    tree, treeid,
    |node| match &node . kind {
      ViewnodeKind::ChangeViewnode (ChangeViewnode::HomeSkgrepoMove { .. }) => is_homeSkgrepoMove,
      ViewnodeKind::ChangeViewnode (ChangeViewnode::RelRepoMove { .. }) => ! is_homeSkgrepoMove,
      _ => false, } )
    { errors . push(format!("{} must be unique among its siblings.", label)); }
  errors }

fn validate_changeset_marker (
  tree    : &Tree<Viewnode>,
  treeid  : NodeId,
  kind    : ChangesetMarkerKind,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, treeid, -1, false,
    |node| matches!(&node . kind, ViewnodeKind::Vognode (_)))
    { errors . push("changesetMarker must have a vognode parent." . to_string()); }
  if !generation_does_not_exist(tree, treeid, 1, false) {
    errors . push("changesetMarker must have no children." . to_string()); }
  if !siblings_cannot_include(
    tree, treeid,
    |node| matches!(&node . kind,
                    ViewnodeKind::ChangeViewnode (ChangeViewnode::ChangesetMarker {
                      kind : sibling_kind, .. })
                    if *sibling_kind == kind))
    { errors . push("A changesetMarker of each kind must be unique among its siblings."
                    . to_string()); }
  errors }

fn validate_idFolder (
  tree    : &Tree<Viewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, treeid, -1, false,
    |node| node . is_occurrence ())
    { errors . push("IDFolder must have an Occurrence parent." . to_string()); }
  if !generation_includes_only(
    tree, treeid, 1, true,
    tombstone_or (|node| matches!(&node . kind,
                    ViewnodeKind::PropertyMember (
                      PropertyMember::ID { .. } ))) )
    { errors . push("IDFolder's (non-ignored) children can only be ID properties."
                    . to_string() ); }
  if !siblings_cannot_include(
    tree, treeid,
    |node| matches!(&node . kind,
                    ViewnodeKind::Folder (Folder::ID)))
    { errors . push("IDFolder must be unique among its siblings." . to_string()); }
  errors }

fn validate_id_property (
  tree    : &Tree<Viewnode>,
  treeid  : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_does_not_exist(tree, treeid, 1, true) {
    errors . push("ID property must have no (non-ignored) children." . to_string()); }
  if !generation_exists_and_includes(
    tree, treeid, -1, false,
    |node| matches!(&node . kind,
                    ViewnodeKind::Folder (Folder::ID)))
    { errors . push("ID property must have an IDFolder parent." . to_string()); }
  errors }

/// The identity + child-structure checks shared by an Occurrence and a ghost:
/// id present, no
/// wrong-structure child, and distinct content-child ids. `label` ("Occurrence"
/// / "Ghost") is woven into the messages so each kind reports itself.
/// Repo validity is not checked here: skgrepo is load-bearing only for a
/// Occurrence (it is the node's .skg file path), so validate_occurrence adds that
/// check; a ghost writes nothing and is ignored at save, so its skgrepo --
/// which may be the NOT_FOUND sentinel for an unresolvable reference -- is
/// save-inert and goes unchecked. (validate_occurrence also appends the
/// editable-title check; a ghost is title-exempt, being write-protected.)
fn validate_vognode_identity_and_structure (
  tree       : &Tree<Viewnode>,
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
              ViewnodeKind::Folder (Folder::Subscribee) ))
    . unwrap_or (false);
  if !generation_includes_only(
    tree, treeid, 1, true,
    |node| !cannot_be_child_of_vognode (node, is_subscribee_as_such))
    { errors . push( format!("{} has a child whose structure belongs elsewhere: BufferRoot, Alias, ID, HiddenInSubscribeeFolder (outside a subscribee-as-such), or HiddenOutsideOfSubscribeeFolder.", label) ); }
  if !nonignored_children_have_distinct_gnids(tree, treeid) {
    errors . push( format!("{}'s non-ignored content children must be unique (no two sharing the same ID).", label) ); }
  errors }

fn validate_occurrence (
  tree    : &Tree<Viewnode>,
  treeid  : NodeId,
  t       : &Occurrence,
  config  : &SkgConfig,
) -> Vec<String> {
  let mut errors : Vec<String> =
    validate_vognode_identity_and_structure (
      tree, treeid, true, "Occurrence" );
  if !has_valid_skgrepo (t, config) {
    errors . push("Occurrence must have a repo that exists in the config."
                  . to_string()); }
  if has_empty_title (t) {
    errors . push("Editable node has an empty title." . to_string()); }
  if t . relationshipEditRequest == Some (RelationshipEditRequest::RestoreRel)
    && ! restoreRel_stands_where_it_may (tree, treeid, t)
  { errors . push ( RESTORE_REL_WITHOUT_A_REMOVED_MEMBERSHIP . to_string () ); }
  errors }

/// Whether a restoreRel request stands where it may: on a member of a
/// partner folder (whose removed members are non-members), or on a
/// removed member, a child claiming no membership under a vognode
/// that is no node of the containerward chain
/// ('stands_in_containerward_chain'). Under a parent that does not
/// read its children's claims no child claims, whatever it carries.
/// A chain node stands for no relationship the diff view tracks.
fn restoreRel_stands_where_it_may (
  tree   : &Tree<Viewnode>,
  treeid : NodeId,
  t      : &Occurrence,
) -> bool {
  let Some (node) : Option<NodeRef<Viewnode>> = tree . get (treeid)
    else { return false; };
  let Some (parent) : Option<NodeRef<Viewnode>> = node . parent ()
    else { return false; };
  match &parent . value () . kind {
    kind if kind . is_partner_folder () => true,
    ViewnodeKind::Vognode (_) =>
      ( ! t . claimsMembership || ! parent_reads_claims (tree, parent . id ()) )
      && ! stands_in_containerward_chain (node),
    _ => false, }}

fn cannot_be_child_of_vognode (
  node : &Viewnode,
  parent_is_subscribee_as_such : bool,
) -> bool {
  matches!(&node . kind,
    ViewnodeKind::BufferRoot |
    ViewnodeKind::PropertyMember (
      PropertyMember::Alias { .. } | PropertyMember::ID { .. } | PropertyMember::Flag { .. }) |
    ViewnodeKind::Folder (Folder::HiddenOutsideOfSubscribee))
  || ( ! parent_is_subscribee_as_such
       // validate_hiddenin requires a vognode parent; a
       // subscribee-as-such is the position that warrants one.
       && matches!(&node . kind,
            ViewnodeKind::Folder (Folder::HiddenInSubscribee)) ) }

/// Check if an Occurrence has a skgrepo and it exists in the config.
pub fn has_valid_skgrepo (
  t      : &Occurrence,
  config : &SkgConfig,
) -> bool {
  config . skgrepos . contains_key (&t . home_skgrepo) }

/// An editable node (not marked for deletion) must have a non-empty title.
/// Nodes that are write-protected or carry a delete request are exempt.
fn has_empty_title ( t : &Occurrence ) -> bool {
  let is_editable : bool =
    matches! ( &t . editability, Editability::Editable { .. } );
  let is_delete : bool =
    matches! ( t . edit_request (),
               Some (&NodeEditRequest::Delete) );
  is_editable && !is_delete && t . title . trim () . is_empty () }

/// Check that all non-ignored content children
/// have distinct IDs.
/// "Non-ignored" means one that claims membership.
/// Returns true if all such children have distinct IDs,
/// or if there are no such children.
pub fn nonignored_children_have_distinct_gnids (
  tree    : &Tree<Viewnode>,
  treeid  : NodeId,
) -> bool {
  let Some (node_ref) = tree . get (treeid)
    else { return true; };
  if ! parent_reads_claims (tree, treeid) {
    // No child claims, so no list is written: duplicates are notes.
    return true; }
  let mut seen : HashSet<ID> = HashSet::new();
  for child in node_ref . children() {
    let content_gnid : Option<ID> =
      // collected IDs, not own IDs: distinctness guards the
      // collected contains list, and a shown overrider can
      // legitimately appear twice when it stands for two distinct
      // originals.
      match &child . value() . kind {
        ViewnodeKind::Vognode (Vognode::Occurrence (t))
          if t . claimsMembership
          => Some (t . collected_gnid ()),
        ViewnodeKind::Vognode (Vognode::Unknown (u)) =>
          Some (u . gnid . clone()),
        // A tombstone is not a content member (where it stands for a
        // hidden node, disk supplementation owns that membership), so
        // it does not participate in content-id distinctness.
        _ => None };
    if let Some (gnid) = content_gnid {
      if !seen . insert(gnid) {
        return false; }}}
  true }

/// Whether a folder's members have distinct IDs. As for a content
/// list ('nonignored_children_have_distinct_gnids'), only members
/// count, so that a non-member copy of a member may stand beside it,
/// and only where the folder's claims are read.
fn partnerFolder_children_have_distinct_gnids (
  tree    : &Tree<Viewnode>,
  treeid  : NodeId,
) -> bool {
  let Some (node_ref) = tree . get (treeid)
    else { return true; };
  if ! parent_reads_claims (tree, treeid) {
    // No member claims, so no list is written: duplicates are notes.
    return true; }
  let mut seen : HashSet<ID> = HashSet::new();
  for child in node_ref . children() {
    let Some (gnid) =
      (match &child . value() . kind {
        ViewnodeKind::Vognode (Vognode::Occurrence (t))
          if t . claimsMembership =>
          Some (t . gnid . clone()),
        ViewnodeKind::Vognode (Vognode::Unknown (u)) =>
          Some (u . gnid . clone()),
        _ => None,
      })
    else { continue; };
    if !seen . insert (gnid) {
      return false; }}
  true }
