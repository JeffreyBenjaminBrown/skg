/// Local validation functions for ViewNode trees.
/// These check structural properties of individual nodes
/// without requiring global context.

use crate::types::maybe_placed_viewnode::{MpViewnode, MpViewnodeKind, MpActiveNode, MpPhantomDiff};
use crate::types::maybe_placed_viewnode::{MpVognode, MpPhantom};
use crate::types::viewnode::{NodeEditRequest, IndefOrDef, ParentIs, PartnerFolder, Qual, QualFolder};
use crate::types::misc::{ID, SkgConfig};
use crate::types::tree::viewnode_nodecomplete::{
  generation_includes_only,
  generation_exists_and_includes,
  generation_does_not_exist,
  siblings_cannot_include,
  id_from_self_or_nearest_ancestor,
};
use ego_tree::{Tree, NodeId};
use std::collections::HashSet;

/// Error from local structure validation.
/// Contains the error message and the ID of the nearest ActiveNode ancestor.
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
        validate_activeNode(tree, node_id, t, config),
      MpViewnodeKind::Phantom (MpPhantom::Diff (p)) =>
        validate_phantom(tree, node_id, p),
      MpViewnodeKind::BufferRoot =>
        Vec::new (),
      MpViewnodeKind::Qual (Qual::Alias { .. }) =>
          validate_alias(tree, node_id),
      MpViewnodeKind::QualFolder (QualFolder::Alias) =>
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
      MpViewnodeKind::Qual (Qual::TextChanged { .. }) =>
          validate_text_changed(tree, node_id),
      MpViewnodeKind::QualFolder (QualFolder::ID) =>
          validate_idFolder(tree, node_id),
      MpViewnodeKind::Qual (Qual::ID { .. }) =>
          validate_idscaffold(tree, node_id),
      MpViewnodeKind::Phantom (MpPhantom::Deleted (_))
        => Vec::new(),
      MpViewnodeKind::DeadScaffold => Vec::new(),
      MpViewnodeKind::Vognode (MpVognode::Inactive (_))
        => validate_inactive_node(tree, node_id),
      MpViewnodeKind::Phantom (MpPhantom::Unknown (_))
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
                    MpViewnodeKind::QualFolder (QualFolder::Alias)))
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
                    MpViewnodeKind::Qual (Qual::Alias { .. } )))
    { errors . push("AliasFolder's (non-ignored) children must include only Aliases."
                    . to_string()); }
  if !generation_exists_and_includes(
    tree, node_id, -1, false,
    |node| matches!(&node . kind,
                    MpViewnodeKind::Vognode (
                      MpVognode::Active (_) )))
    { errors . push("AliasFolder must have an ActiveNode parent." . to_string()); }
  if !siblings_cannot_include(
    tree, node_id,
    |node| matches!(&node . kind,
                    MpViewnodeKind::QualFolder (QualFolder::Alias)))
    { errors . push("AliasFolder must be unique among its siblings."
                    . to_string()); }
  errors }

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
        "HiddenInSubscribeeFolder must have an ActiveNode parent (the subscribee)"
        . to_string()); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| node . is_active_or_diff_phantom ()
           || matches! ( &node . kind,
                         MpViewnodeKind::Phantom (MpPhantom::Unknown (_)) ))
    { errors . push(
        "HiddenInSubscribeeFolder's children can only be ActiveNodes or Unknown placeholders (to hide)."
        . to_string()); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| match &node . kind {
      MpViewnodeKind::Vognode (MpVognode::Active (t))
        => t . parentIs == ParentIs::Affected,
      MpViewnodeKind::Phantom (MpPhantom::Diff (_))
        => true,
      MpViewnodeKind::Phantom (MpPhantom::Unknown (_))
        => true,
      _ => false, } )
    { errors . push(
        "HiddenInSubscribeeFolder ActiveNode children must have parentIs=affected."
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
      "HiddenInSubscribeeFolder must not have duplicate ActiveNode children."
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
                         MpViewnodeKind::Phantom (MpPhantom::Unknown (_)) ))
    { errors . push("HiddenOutsideOfSubscribeeFolder's children must include only ActiveNodes or Unknown placeholders." . to_string()); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| match &node . kind {
      MpViewnodeKind::Vognode (MpVognode::Active (t))
        => t . parentIs == ParentIs::Affected,
      MpViewnodeKind::Phantom (MpPhantom::Diff (_))
        => true,
      MpViewnodeKind::Phantom (MpPhantom::Unknown (_))
        => true,
      _ => false, } )
    { errors . push(
        "HiddenOutsideOfSubscribeeFolder ActiveNode children must be parentIs=affected."
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
        "HiddenOutsideOfSubscribeeFolder must not have duplicate ActiveNode children."
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
    { errors . push("SubscribeeFolder must have an ActiveNode parent." . to_string()); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| node . is_active_or_diff_phantom ()
           || matches!(&node . kind,
                    MpViewnodeKind::Vognode (MpVognode::Inactive (_)) // a retained inactive subscribee may sit here as an inert display placeholder; it emits no subscribes_to membership (TODO/full-schema/9-2_source-set-safety.org)
                      | MpViewnodeKind::Phantom (MpPhantom::Unknown (_))
                      | MpViewnodeKind::PartnerFolder (
                          PartnerFolder::HiddenOutsideOfSubscribee) ))
    { errors . push( "SubscribeeFolder's children must include only ActiveNodes, Unknown or inactive placeholders, or HiddenOutsideOfSubscribeeFolder." . to_string()); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| match &node . kind {
      MpViewnodeKind::Vognode (MpVognode::Active (t)) =>
        t . parentIs == ParentIs::Affected,
      MpViewnodeKind::Vognode (MpVognode::Inactive (_)) =>
        true,
      MpViewnodeKind::Phantom (MpPhantom::Diff (_)) =>
        true,
      MpViewnodeKind::Phantom (MpPhantom::Unknown (_)) =>
        true,
      MpViewnodeKind::PartnerFolder (
        PartnerFolder::HiddenOutsideOfSubscribee)
        => true,
      _ => false, } )
    { errors . push("SubscribeeFolder ActiveNode children must have parentIs=affected."
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
    { errors . push(format!("{} must have an ActiveNode parent.", label)); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| node . is_active_or_diff_phantom ()
           || ( partnerFolder == PartnerFolder::Overridden
                && matches! ( &node . kind,
                              MpViewnodeKind::Phantom (MpPhantom::Unknown (_)) ))
           || matches!(&node . kind,
                       MpViewnodeKind::Vognode (MpVognode::Inactive (_)))) // tolerated from stale buffers; the rerender removes it (TODO/full-schema/9-2_source-set-safety.org)
    { errors . push(format!("{}'s children must include only ActiveNodes, inactive placeholders, or (for OverriddenFolder) Unknown placeholders.", label)); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| match &node . kind {
      MpViewnodeKind::Vognode (MpVognode::Active (t))
        => t . parentIs == ParentIs::Affected,
      MpViewnodeKind::Vognode (MpVognode::Inactive (_))
        => true,
      MpViewnodeKind::Phantom (MpPhantom::Diff (_))
        => true,
      MpViewnodeKind::Phantom (MpPhantom::Unknown (_))
        if partnerFolder == PartnerFolder::Overridden => true,
      _ => false, } )
    { errors . push(format!(
        "{} ActiveNode children must have parentIs=affected.", label)); }
  if !siblings_cannot_include(
    tree, node_id,
    |node| matches!(&node . kind,
                    MpViewnodeKind::PartnerFolder (r)
                    if *r == partnerFolder))
    { errors . push(format!("{} must be unique among its siblings.", label)); }
  if partnerFolder != PartnerFolder::Overridden
    // OverriddenFolder is a defining folder: duplicate members are silently
    // deduplicated at emission rather than bouncing the save. The
    // read-only roles keep the check.
    && !partnerFolder_children_have_distinct_ids(tree, node_id) {
    errors . push(format!(
      "{} must not have duplicate ActiveNode children.", label)); }
  errors }

fn validate_text_changed (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, node_id, -1, false,
    |node| node . is_active_or_diff_phantom ())
    { errors . push("TextChanged must have an ActiveNode parent." . to_string()); }
  if !generation_does_not_exist(tree, node_id, 1, true) {
    errors . push("TextChanged must have no (non-ignored) children." . to_string()); }
  if !siblings_cannot_include(
    tree, node_id,
    |node| matches!(&node . kind,
                    MpViewnodeKind::Qual (Qual::TextChanged { .. })))
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
    { errors . push("IDFolder must have an ActiveNode parent." . to_string()); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| matches!(&node . kind,
                    MpViewnodeKind::Qual (Qual::ID { .. } )) )
    { errors . push("IDFolder's (non-ignored) children can only be ID scaffolds."
                    . to_string() ); }
  if !siblings_cannot_include(
    tree, node_id,
    |node| matches!(&node . kind,
                    MpViewnodeKind::QualFolder (QualFolder::ID)))
    { errors . push("IDFolder must be unique among its siblings." . to_string()); }
  errors }

fn validate_idscaffold (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_does_not_exist(tree, node_id, 1, true) {
    errors . push("ID scaffold must have no (non-ignored) children." . to_string()); }
  if !generation_exists_and_includes(
    tree, node_id, -1, false,
    |node| matches!(&node . kind,
                    MpViewnodeKind::QualFolder (QualFolder::ID)))
    { errors . push("ID scaffold must have an IDFolder parent." . to_string()); }
  errors }

fn validate_inactive_node (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  // TODO/full-schema/9-2_source-set-safety.org: an InactiveNode may
  // sit under a folder (a stale buffer from before a source-set
  // switch) or under another gnode, and it may have children (the
  // retained case: an inactive node kept on screen because of its
  // active children).  Its own content stays read-only -- the
  // parser rejects title/body text on it -- so the only structural
  // demand left is a parent that can carry it at all.
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, node_id, -1, false,
    |node| node . is_active_or_diff_phantom ()
           || matches! ( &node . kind,
                         MpViewnodeKind::Vognode (MpVognode::Inactive (_))
                         | MpViewnodeKind::QualFolder (_)
                         | MpViewnodeKind::PartnerFolder (_)
                         | MpViewnodeKind::DeadScaffold
                         | MpViewnodeKind::BufferRoot )) // an InactiveNode can be a view root: a root that went inactive but was retained for its active children
    { errors . push("Inactive placeholder must have an ActiveNode, folder or DeadScaffold parent, or be a view root."
                    . to_string()); }
  errors }

/// The identity + child-structure checks shared by an ActiveNode and a phantom
/// (TODO/DONE/local-view-update/plan_v2.org §20.4 dedup): id present, no
/// wrong-structure child, and distinct content-child ids. `label` ("ActiveNode"
/// / "Phantom") is woven into the messages so each kind reports itself.
/// Source validity is NOT checked here: source is load-bearing only for a
/// ActiveNode (it is the node's .skg file path), so validate_activeNode adds that
/// check; a phantom writes nothing and is ignored at save, so its source --
/// which may be the NOT_FOUND sentinel for an unresolvable reference -- is
/// inert and goes unchecked. (validate_activeNode also appends the
/// definitive-title check; a phantom is title-exempt, being indefinitive.)
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

fn validate_activeNode (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
  t       : &MpActiveNode,
  config  : &SkgConfig,
) -> Vec<String> {
  let mut errors : Vec<String> =
    validate_gnode_identity_and_structure (
      tree, node_id, has_id (t), "ActiveNode" );
  if !has_valid_source (t, config) {
    errors . push("ActiveNode must have a source that exists in the config."
                  . to_string()); }
  if has_empty_title (t) {
    errors . push("Definitive node has an empty title." . to_string()); }
  errors }

/// Validate a phantom (TODO/DONE/local-view-update/plan_v2.org §11): the same
/// identity and child-structure checks as an ActiveNode, minus the two
/// ActiveNode-only rules. The definitive-title rule does not apply (a phantom is
/// always indefinitive, hence exempt). The source-in-config rule does not
/// apply either: a phantom writes nothing and is ignored at save, so its
/// source -- possibly the NOT_FOUND sentinel for a reference that resolves to
/// no source -- is inert.
fn validate_phantom (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
  p       : &MpPhantomDiff,
) -> Vec<String> {
  validate_gnode_identity_and_structure (
    tree, node_id, p . id . is_some (), "Phantom" ) }

fn cannot_be_child_of_gnode (
  node : &MpViewnode,
  parent_is_subscribee_as_such : bool,
) -> bool {
  matches!(&node . kind,
    MpViewnodeKind::BufferRoot |
    MpViewnodeKind::Qual (Qual::Alias { .. } | Qual::ID { .. }) |
    MpViewnodeKind::PartnerFolder (PartnerFolder::HiddenOutsideOfSubscribee))
  || ( ! parent_is_subscribee_as_such
       // validate_hiddenin REQUIRES a gnode parent; a
       // subscribee-as-such is the position that warrants one.
       && matches!(&node . kind,
            MpViewnodeKind::PartnerFolder (PartnerFolder::HiddenInSubscribee)) ) }

/// Check if an MpActiveNode has an ID.
pub fn has_id ( t : &MpActiveNode ) -> bool {
  t . id . is_some() }

/// Check if an MpActiveNode has a source and it exists in the config.
pub fn has_valid_source (
  t      : &MpActiveNode,
  config : &SkgConfig,
) -> bool {
  t . source . as_ref()
    . is_some_and( |s| config . sources . contains_key (s) ) }

/// A definitive node (not marked for deletion) must have a non-empty title.
/// Nodes that are indefinitive or carry a delete request are exempt.
fn has_empty_title ( t : &MpActiveNode ) -> bool {
  let is_definitive : bool =
    matches! ( &t . indef_or_def, IndefOrDef::Definitive { .. } );
  let is_delete : bool =
    matches! ( t . edit_request (),
               Some (&NodeEditRequest::Delete) );
  is_definitive && !is_delete && t . title . trim () . is_empty () }

/// Check that all non-ignored, non-phantom content children
/// have distinct IDs.
/// "Non-ignored" means parentIs == Affected.
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
          if t . parentIs == ParentIs::Affected
          => t . collected_id (),
        MpViewnodeKind::Phantom (MpPhantom::Unknown (u)) =>
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
        MpViewnodeKind::Phantom (MpPhantom::Unknown (u)) =>
          Some (u . id . clone()),
        _ => None,
      })
    else { continue; };
    if !seen . insert (id) {
      return false; }}
  true }
