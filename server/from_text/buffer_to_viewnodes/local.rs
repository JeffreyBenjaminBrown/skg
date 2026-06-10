/// Local validation functions for ViewNode trees.
/// These check structural properties of individual nodes
/// without requiring global context.

use crate::types::maybe_placed_viewnode::{MpViewnode, MpViewnodeKind, MpTruenode, MpPhantomDiff};
use crate::types::maybe_placed_viewnode::{MpVognode, MpPhantom};
use crate::types::git::Sign;
use crate::types::viewnode::{EditRequest, IndefOrDef, ParentIs, PartnerCol, Qual, QualCol};
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
/// Contains the error message and the ID of the nearest TrueNode ancestor.
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
        validate_truenode(tree, node_id, t, config),
      MpViewnodeKind::Phantom (MpPhantom::Diff (p)) =>
        validate_phantom(tree, node_id, p, config),
      MpViewnodeKind::BufferRoot =>
        Vec::new (),
      MpViewnodeKind::Qual (Qual::Alias { .. }) =>
          validate_alias(tree, node_id),
      MpViewnodeKind::QualCol (QualCol::Alias) =>
          validate_aliascol(tree, node_id),
      MpViewnodeKind::PartnerCol (PartnerCol::HiddenInSubscribee) =>
          validate_hidden_in_subscribee_col(tree, node_id),
      MpViewnodeKind::PartnerCol (PartnerCol::HiddenOutsideOfSubscribee) =>
          validate_hidden_outside_of_subscribee_col(tree, node_id),
      MpViewnodeKind::PartnerCol (
        role @ (PartnerCol::Hidden
          | PartnerCol::Hider
          | PartnerCol::Overridden
          | PartnerCol::Overrider
          | PartnerCol::Subscriber))
        => validate_relation_col(tree, node_id, *role),
      MpViewnodeKind::PartnerCol (PartnerCol::Subscribee) =>
          validate_subscribeecol(tree, node_id),
      MpViewnodeKind::Qual (Qual::TextChanged { .. }) =>
          validate_text_changed(tree, node_id),
      MpViewnodeKind::QualCol (QualCol::ID) =>
          validate_idcol(tree, node_id),
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
                    MpViewnodeKind::QualCol (QualCol::Alias)))
    { errors . push("Alias must have an AliasCol parent." . to_string()); }
  errors }

fn validate_aliascol (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| matches!(&node . kind,
                    MpViewnodeKind::Qual (Qual::Alias { .. } )))
    { errors . push("AliasCol's (non-ignored) children must include only Aliases."
                    . to_string()); }
  if !generation_exists_and_includes(
    tree, node_id, -1, false,
    |node| matches!(&node . kind,
                    MpViewnodeKind::Vognode (
                      MpVognode::Active (_) )))
    { errors . push("AliasCol must have a TrueNode parent." . to_string()); }
  if !siblings_cannot_include(
    tree, node_id,
    |node| matches!(&node . kind,
                    MpViewnodeKind::QualCol (QualCol::Alias)))
    { errors . push("AliasCol must be unique among its siblings."
                    . to_string()); }
  errors }

/// Read the error messages to see what this validates.
fn validate_hidden_in_subscribee_col (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, node_id, -1, false,
    |node| node . is_active_or_diff_phantom ())
    { errors . push(
        "HiddenInSubscribeeCol must have a TrueNode parent (the subscribee)"
        . to_string()); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| node . is_active_or_diff_phantom ())
    { errors . push(
        "HiddenInSubscribeeCol's children can only be TrueNodes (to hide)."
        . to_string()); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| match &node . kind {
      MpViewnodeKind::Vognode (MpVognode::Active (t))
        => t . parentIs == ParentIs::Affected,
      MpViewnodeKind::Phantom (MpPhantom::Diff (_))
        => true,
      _ => false, } )
    { errors . push(
        "HiddenInSubscribeeCol TrueNode children must have parentIs=affected."
      . to_string()); }
  if !siblings_cannot_include(
    tree, node_id,
    |node| matches!(&node . kind,
                    MpViewnodeKind::PartnerCol (
                      PartnerCol::HiddenInSubscribee )))
    { errors . push("HiddenInSubscribeeCol must be unique among its siblings."
                    . to_string()); }
  if !partnerCol_children_have_distinct_ids(tree, node_id)
    { errors . push(
      "HiddenInSubscribeeCol must not have duplicate TrueNode children."
        . to_string() ); }
  errors }

fn validate_hidden_outside_of_subscribee_col (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, node_id, -1, false,
    |node| matches!(&node . kind,
                    MpViewnodeKind::PartnerCol (
                      PartnerCol::Subscribee)))
    { errors . push(
        "HiddenOutsideOfSubscribeeCol must have a SubscribeeCol parent."
        . to_string()); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| node . is_active_or_diff_phantom ())
    { errors . push("HiddenOutsideOfSubscribeeCol's children must include only TrueNodes." . to_string()); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| match &node . kind {
      MpViewnodeKind::Vognode (MpVognode::Active (t))
        => t . parentIs == ParentIs::Affected,
      MpViewnodeKind::Phantom (MpPhantom::Diff (_))
        => true,
      _ => false, } )
    { errors . push(
        "HiddenOutsideOfSubscribeeCol TrueNode children must be parentIs=affected."
        . to_string()); }
  if !siblings_cannot_include(
    tree, node_id,
    |node| matches!(&node . kind,
                    MpViewnodeKind::PartnerCol (
                      PartnerCol::HiddenOutsideOfSubscribee )))
    { errors . push(
        "HiddenOutsideOfSubscribeeCol must be unique among its siblings."
        . to_string()); }
  if !partnerCol_children_have_distinct_ids(tree, node_id)
    { errors . push(
        "HiddenOutsideOfSubscribeeCol must not have duplicate TrueNode children."
        . to_string() ); }
  errors }

fn validate_subscribeecol (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, node_id, -1, false,
    |node| node . is_active_or_diff_phantom ())
    { errors . push("SubscribeeCol must have a TrueNode parent." . to_string()); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| node . is_active_or_diff_phantom ()
           || matches!(&node . kind,
                    MpViewnodeKind::Vognode (MpVognode::Inactive (_)) // a retained inactive subscribee is a positional member (TODO/full-schema/9-2_source-set-safety.org)
                      | MpViewnodeKind::PartnerCol (
                          PartnerCol::HiddenOutsideOfSubscribee) ))
    { errors . push( "SubscribeeCol's children must include only TrueNodes, inactive placeholders or HiddenOutsideOfSubscribeeCol." . to_string()); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| match &node . kind {
      MpViewnodeKind::Vognode (MpVognode::Active (t)) =>
        t . parentIs == ParentIs::Affected,
      MpViewnodeKind::Vognode (MpVognode::Inactive (_)) =>
        true,
      MpViewnodeKind::Phantom (MpPhantom::Diff (_)) =>
        true,
      MpViewnodeKind::PartnerCol (
        PartnerCol::HiddenOutsideOfSubscribee)
        => true,
      _ => false, } )
    { errors . push("SubscribeeCol TrueNode children must have parentIs=affected."
                    . to_string() ); }
  // There is no duplicate-member check here: SubscribeeCol is a
  // defining col, and duplicate members of defining cols are
  // silently deduplicated at emission rather than bouncing the save.
  errors }

/// PURPOSE: See the error messages it could return.
/// PITFALL: Does not check whether the members are correct for the graph;
/// save extraction and completion decide relation meaning later.
fn validate_relation_col (
  tree     : &Tree<MpViewnode>,
  node_id  : NodeId,
  partnerCol  : PartnerCol,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  let label : String = partnerCol . repr_in_client () . to_string ();
  if !generation_exists_and_includes(
    tree, node_id, -1, false,
    |node| node . is_active_or_diff_phantom ())
    { errors . push(format!("{} must have a TrueNode parent.", label)); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| node . is_active_or_diff_phantom ()
           || matches!(&node . kind,
                       MpViewnodeKind::Vognode (MpVognode::Inactive (_)))) // tolerated from stale buffers; the rerender removes it (TODO/full-schema/9-2_source-set-safety.org)
    { errors . push(format!("{}'s children must include only TrueNodes or inactive placeholders.", label)); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| match &node . kind {
      MpViewnodeKind::Vognode (MpVognode::Active (t))
        => t . parentIs == ParentIs::Affected,
      MpViewnodeKind::Vognode (MpVognode::Inactive (_))
        => true,
      MpViewnodeKind::Phantom (MpPhantom::Diff (_))
        => true,
      _ => false, } )
    { errors . push(format!(
        "{} TrueNode children must have parentIs=affected.", label)); }
  if !siblings_cannot_include(
    tree, node_id,
    |node| matches!(&node . kind,
                    MpViewnodeKind::PartnerCol (r)
                    if *r == partnerCol))
    { errors . push(format!("{} must be unique among its siblings.", label)); }
  if partnerCol != PartnerCol::Overridden
    // OverriddenCol is a defining col: duplicate members are silently
    // deduplicated at emission rather than bouncing the save. The
    // read-only roles keep the check.
    && !partnerCol_children_have_distinct_ids(tree, node_id) {
    errors . push(format!(
      "{} must not have duplicate TrueNode children.", label)); }
  errors }

fn validate_text_changed (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, node_id, -1, false,
    |node| node . is_active_or_diff_phantom ())
    { errors . push("TextChanged must have a TrueNode parent." . to_string()); }
  if !generation_does_not_exist(tree, node_id, 1, true) {
    errors . push("TextChanged must have no (non-ignored) children." . to_string()); }
  if !siblings_cannot_include(
    tree, node_id,
    |node| matches!(&node . kind,
                    MpViewnodeKind::Qual (Qual::TextChanged { .. })))
    { errors . push("TextChanged must be unique among its siblings." . to_string()); }
  errors }

fn validate_idcol (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, node_id, -1, false,
    |node| node . is_active_or_diff_phantom ())
    { errors . push("IDCol must have a TrueNode parent." . to_string()); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| matches!(&node . kind,
                    MpViewnodeKind::Qual (Qual::ID { .. } )) )
    { errors . push("IDCol's (non-ignored) children can only be ID scaffolds."
                    . to_string() ); }
  if !siblings_cannot_include(
    tree, node_id,
    |node| matches!(&node . kind,
                    MpViewnodeKind::QualCol (QualCol::ID)))
    { errors . push("IDCol must be unique among its siblings." . to_string()); }
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
                    MpViewnodeKind::QualCol (QualCol::ID)))
    { errors . push("ID scaffold must have an IDCol parent." . to_string()); }
  errors }

fn validate_inactive_node (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  // TODO/full-schema/9-2_source-set-safety.org: an InactiveNode may
  // sit under a col (a stale buffer from before a source-set
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
                         | MpViewnodeKind::QualCol (_)
                         | MpViewnodeKind::PartnerCol (_)
                         | MpViewnodeKind::DeadScaffold
                         | MpViewnodeKind::BufferRoot )) // an InactiveNode can be a view root: a root that went inactive but was retained for its active children
    { errors . push("Inactive placeholder must have a TrueNode, col or DeadScaffold parent, or be a view root."
                    . to_string()); }
  errors }

/// The identity + child-structure checks shared by a TrueNode and a phantom
/// (TODO/DONE/local-view-update/plan_v2.org §20.4 dedup): id present, source in config, no wrong-structure child, and
/// distinct content-child ids. `label` ("TrueNode" / "Phantom") is woven into the
/// messages so each kind reports itself. (validate_truenode appends the
/// definitive-title check; a phantom is title-exempt, being indefinitive.)
fn validate_gnode_identity_and_structure (
  tree         : &Tree<MpViewnode>,
  node_id      : NodeId,
  id_present   : bool,
  source_valid : bool,
  label        : &str,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !id_present {
    errors . push( format!("{} must have an ID.", label) ); }
  if !source_valid {
    errors . push( format!(
      "{} must have a source that exists in the config.", label) ); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| !cannot_be_child_of_gnode (node))
    { errors . push( format!("{} has a child whose structure belongs elsewhere: BufferRoot, Alias, ID, HiddenInSubscribeeCol, or HiddenOutsideOfSubscribeeCol.", label) ); }
  if !nonignored_children_have_distinct_ids(tree, node_id) {
    errors . push( format!("{}'s non-ignored content children must be unique (no two sharing the same ID).", label) ); }
  errors }

fn validate_truenode (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
  t       : &MpTruenode,
  config  : &SkgConfig,
) -> Vec<String> {
  let mut errors : Vec<String> =
    validate_gnode_identity_and_structure (
      tree, node_id, has_id (t), has_valid_source (t, config), "TrueNode" );
  if has_empty_title (t) {
    errors . push("Definitive node has an empty title." . to_string()); }
  errors }

/// Validate a phantom (TODO/DONE/local-view-update/plan_v2.org §11): same identity and child-structure checks as a
/// TrueNode, but the "definitive node must have a non-empty title" rule does
/// not apply -- a phantom is always indefinitive, hence exempt (as
/// has_empty_title would conclude for it).
fn validate_phantom (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
  p       : &MpPhantomDiff,
  config  : &SkgConfig,
) -> Vec<String> {
  validate_gnode_identity_and_structure (
    tree, node_id,
    p . id . is_some (),
    p . source . as_ref () . is_some_and (
      |s| config . sources . contains_key (s) ),
    "Phantom" ) }

fn cannot_be_child_of_gnode (
  node : &MpViewnode,
) -> bool {
  matches!(&node . kind,
    MpViewnodeKind::BufferRoot |
    MpViewnodeKind::Qual (Qual::Alias { .. } | Qual::ID { .. }) |
    MpViewnodeKind::PartnerCol (PartnerCol::HiddenInSubscribee |
                                         PartnerCol::HiddenOutsideOfSubscribee)) }

/// Check if an MpTruenode has an ID.
pub fn has_id ( t : &MpTruenode ) -> bool {
  t . id . is_some() }

/// Check if an MpTruenode has a source and it exists in the config.
pub fn has_valid_source (
  t      : &MpTruenode,
  config : &SkgConfig,
) -> bool {
  t . source . as_ref()
    . is_some_and( |s| config . sources . contains_key (s) ) }

/// A definitive node (not marked for deletion) must have a non-empty title.
/// Nodes that are indefinitive or carry a delete request are exempt.
fn has_empty_title ( t : &MpTruenode ) -> bool {
  let is_definitive : bool =
    matches! ( &t . indef_or_def, IndefOrDef::Definitive { .. } );
  let is_delete : bool =
    matches! ( t . edit_request (),
               Some (&EditRequest::Delete) );
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
      match &child . value() . kind {
        MpViewnodeKind::Vognode (MpVognode::Active (t))
          if t . parentIs == ParentIs::Affected
          => t . id . clone(),
        MpViewnodeKind::Vognode (MpVognode::Inactive (i))
          if i . membership . staged != Some (Sign::Minus)
          && i . membership . unstaged != Some (Sign::Minus)
          => Some (i . id . clone()),
        _ => None };
    if let Some (id) = content_id {
      if !seen . insert(id) {
        return false; }}}
  true }

fn partnerCol_children_have_distinct_ids (
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
        _ => None,
      })
    else { continue; };
    if !seen . insert (id) {
      return false; }}
  true }
