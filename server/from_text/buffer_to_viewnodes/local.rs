/// Local validation functions for ViewNode trees.
/// These check structural properties of individual nodes
/// without requiring global context.

use crate::types::maybe_placed_viewnode::{MaybePlacedViewnode, MaybePlacedViewnodeKind, MaybePlacedTruenode};
use crate::types::git::Sign;
use crate::types::viewnode::{Scaffold, EditRequest, IndefOrDef, ParentIs, RoleCol};
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
  tree    : &Tree<MaybePlacedViewnode>,
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
    { MaybePlacedViewnodeKind::True (t) =>
        validate_truenode(tree, node_id, t, config),
      MaybePlacedViewnodeKind::Scaff (s) => match s {
        Scaffold::BufferRoot =>
          Vec::new (),
        Scaffold::Alias { .. } =>
          validate_alias(tree, node_id),
        Scaffold::AliasCol =>
          validate_aliascol(tree, node_id),
        Scaffold::RoleCol { roleCol: RoleCol::HiddenInSubscribee } =>
          validate_hidden_in_subscribee_col(tree, node_id),
        Scaffold::RoleCol { roleCol: RoleCol::HiddenOutsideOfSubscribee } =>
          validate_hidden_outside_of_subscribee_col(tree, node_id),
        Scaffold::RoleCol { roleCol: RoleCol::Hidden }
          | Scaffold::RoleCol { roleCol: RoleCol::Hider }
          | Scaffold::RoleCol { roleCol: RoleCol::Overridden }
          | Scaffold::RoleCol { roleCol: RoleCol::Overrider }
          | Scaffold::RoleCol { roleCol: RoleCol::Subscriber } =>
          validate_relation_col(tree, node_id, s),
        Scaffold::RoleCol { roleCol: RoleCol::Subscribee } =>
          validate_subscribeecol(tree, node_id),
        Scaffold::TextChanged { .. } =>
          validate_text_changed(tree, node_id),
        Scaffold::IDCol =>
          validate_idcol(tree, node_id),
        Scaffold::ID { .. } =>
          validate_idscaffold(tree, node_id), },
      MaybePlacedViewnodeKind::Deleted (_) => Vec::new(),
      MaybePlacedViewnodeKind::DeletedScaff (_) => Vec::new(),
      MaybePlacedViewnodeKind::Inactive (_) =>
        validate_inactive_node(tree, node_id),
      MaybePlacedViewnodeKind::Unknown (_) => Vec::new() };

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
  tree    : &Tree<MaybePlacedViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_does_not_exist(tree, node_id, 1, true) {
    errors . push("Alias must have no (non-ignored) children." . to_string()); }
  if !generation_exists_and_includes(tree, node_id, -1, false, |node|
       matches!(&node . kind, MaybePlacedViewnodeKind::Scaff (Scaffold::AliasCol))) {
    errors . push("Alias must have an AliasCol parent." . to_string()); }
  errors }

fn validate_aliascol (
  tree    : &Tree<MaybePlacedViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_includes_only(tree, node_id, 1, true, |node|
       matches!(&node . kind, MaybePlacedViewnodeKind::Scaff(Scaffold::Alias { .. }))) {
    errors . push("AliasCol's (non-ignored) children must include only Aliases."
                . to_string()); }
  if !generation_exists_and_includes(tree, node_id, -1, false, |node|
       matches!(&node . kind, MaybePlacedViewnodeKind::True (_))) {
    errors . push("AliasCol must have a TrueNode parent." . to_string()); }
  if !siblings_cannot_include(tree, node_id, |node|
       matches!(&node . kind, MaybePlacedViewnodeKind::Scaff (Scaffold::AliasCol))) {
    errors . push("AliasCol must be unique among its siblings." . to_string()); }
  errors }

/// Read the error messages to see what this validates.
fn validate_hidden_in_subscribee_col (
  tree    : &Tree<MaybePlacedViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(tree, node_id, -1, false, |node|
       matches!(&node . kind, MaybePlacedViewnodeKind::True (_)))
    { errors . push(
        "HiddenInSubscribeeCol must have a TrueNode parent (the subscribee)"
        . to_string()); }
  if !generation_includes_only(tree, node_id, 1, true, |node|
       matches!(&node . kind, MaybePlacedViewnodeKind::True (_)))
    { errors . push(
        "HiddenInSubscribeeCol's children can only be TrueNodes (to hide)."
        . to_string()); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| matches!(&node . kind,
                    MaybePlacedViewnodeKind::True (t)
                    if t . parentIs == ParentIs::Collector))
    { errors . push(
        "HiddenInSubscribeeCol TrueNode children must have parentIs=collector."
      . to_string()); }
  if !siblings_cannot_include(
    tree, node_id,
    |node| matches!(&node . kind,
                    MaybePlacedViewnodeKind::Scaff (
                      Scaffold::RoleCol {
                        roleCol: RoleCol::HiddenInSubscribee } )))
    { errors . push("HiddenInSubscribeeCol must be unique among its siblings."
                    . to_string()); }
  if !relation_col_children_have_distinct_ids(tree, node_id)
    { errors . push(
      "HiddenInSubscribeeCol must not have duplicate TrueNode children."
        . to_string() ); }
  errors }

fn validate_hidden_outside_of_subscribee_col (
  tree    : &Tree<MaybePlacedViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(
    tree, node_id, -1, false,
    |node| matches!(&node . kind,
                    MaybePlacedViewnodeKind::Scaff (
                      Scaffold::RoleCol { roleCol: RoleCol::Subscribee } )))
    { errors . push(
        "HiddenOutsideOfSubscribeeCol must have a SubscribeeCol parent."
        . to_string()); }
  if !generation_includes_only(tree, node_id, 1, true, |node|
       matches!(&node . kind, MaybePlacedViewnodeKind::True (_))) {
    errors . push("HiddenOutsideOfSubscribeeCol's children must include only TrueNodes." . to_string()); }
  if !generation_includes_only(tree, node_id, 1, true, |node|
       matches!(&node . kind,
         MaybePlacedViewnodeKind::True (t)
         if t . parentIs == ParentIs::Collector)) {
    errors . push(
      "HiddenOutsideOfSubscribeeCol TrueNode children must be parentIs=collector."
      . to_string()); }
  if !siblings_cannot_include(
    tree, node_id,
    |node| matches!(&node . kind,
                    MaybePlacedViewnodeKind::Scaff (
                      Scaffold::RoleCol {
                        roleCol: RoleCol::HiddenOutsideOfSubscribee } )))
    { errors . push(
        "HiddenOutsideOfSubscribeeCol must be unique among its siblings."
        . to_string()); }
  if !relation_col_children_have_distinct_ids(tree, node_id)
    { errors . push(
        "HiddenOutsideOfSubscribeeCol must not have duplicate TrueNode children."
        . to_string() ); }
  errors }

fn validate_subscribeecol (
  tree    : &Tree<MaybePlacedViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(tree, node_id, -1, false, |node|
       matches!(&node . kind, MaybePlacedViewnodeKind::True (_))) {
    errors . push("SubscribeeCol must have a TrueNode parent." . to_string()); }
  if !generation_includes_only(tree, node_id, 1, true, |node|
       matches!(&node . kind,
         MaybePlacedViewnodeKind::True (_) |
         MaybePlacedViewnodeKind::Scaff (
           Scaffold::RoleCol {
             roleCol: RoleCol::HiddenOutsideOfSubscribee } )))
    { errors . push( "SubscribeeCol's children must include only TrueNodes or HiddenOutsideOfSubscribeeCol." . to_string()); }
  if !generation_includes_only(
    tree, node_id, 1, true,
    |node| match &node . kind {
      MaybePlacedViewnodeKind::True (t) =>
        t . parentIs == ParentIs::Collector,
      MaybePlacedViewnodeKind::Scaff (
        Scaffold::RoleCol {
          roleCol: RoleCol::HiddenOutsideOfSubscribee })
        => true,
      _ => false, } )
    { errors . push("SubscribeeCol TrueNode children must have parentIs=collector."
                    . to_string() ); }
  if !relation_col_children_have_distinct_ids(tree, node_id) {
    errors . push("SubscribeeCol must not have duplicate TrueNode children."
                  . to_string() ); }
  errors }

/// PURPOSE: It should be that the scaffold:
/// - belongs to one TrueNode
/// - has only TrueNode members marked parentIs=collector
/// - is unique among its sibling scaffolds
/// - does not repeat member IDs
/// PITFALL: Does not check whether the members are correct for the graph;
/// save extraction and completion decide relation meaning later.
fn validate_relation_col (
  tree     : &Tree<MaybePlacedViewnode>,
  node_id  : NodeId,
  scaffold : &Scaffold,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  let label : String = scaffold . repr_in_client ();
  let scaffold_for_compare : Scaffold = scaffold . clone ();
  if !generation_exists_and_includes(tree, node_id, -1, false, |node|
       matches!(&node . kind, MaybePlacedViewnodeKind::True (_))) {
    errors . push(format!("{} must have a TrueNode parent.", label)); }
  if !generation_includes_only(tree, node_id, 1, true, |node|
       matches!(&node . kind, MaybePlacedViewnodeKind::True (_))) {
    errors . push(format!("{}'s children must include only TrueNodes.", label)); }
  if !generation_includes_only(tree, node_id, 1, true, |node|
       matches!(&node . kind,
         MaybePlacedViewnodeKind::True (t)
         if t . parentIs == ParentIs::Collector)) {
    errors . push(format!(
      "{} TrueNode children must have parentIs=collector.", label)); }
  if !siblings_cannot_include(tree, node_id, |node|
       matches!(&node . kind,
         MaybePlacedViewnodeKind::Scaff (s)
         if s . matches_kind (&scaffold_for_compare))) {
    errors . push(format!("{} must be unique among its siblings.", label)); }
  if !relation_col_children_have_distinct_ids(tree, node_id) {
    errors . push(format!(
      "{} must not have duplicate TrueNode children.", label)); }
  errors }

fn validate_text_changed (
  tree    : &Tree<MaybePlacedViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(tree, node_id, -1, false, |node|
       matches!(&node . kind, MaybePlacedViewnodeKind::True (_))) {
    errors . push("TextChanged must have a TrueNode parent." . to_string()); }
  if !generation_does_not_exist(tree, node_id, 1, true) {
    errors . push("TextChanged must have no (non-ignored) children." . to_string()); }
  if !siblings_cannot_include(tree, node_id, |node|
       matches!(&node . kind, MaybePlacedViewnodeKind::Scaff (Scaffold::TextChanged { .. }))) {
    errors . push("TextChanged must be unique among its siblings." . to_string()); }
  errors }

fn validate_idcol (
  tree    : &Tree<MaybePlacedViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(tree, node_id, -1, false, |node|
       matches!(&node . kind, MaybePlacedViewnodeKind::True (_))) {
    errors . push("IDCol must have a TrueNode parent." . to_string()); }
  if !generation_includes_only(tree, node_id, 1, true, |node|
       matches!(&node . kind, MaybePlacedViewnodeKind::Scaff(Scaffold::ID { .. } )) ) {
    errors . push("IDCol's (non-ignored) children can only be ID scaffolds."
                . to_string() ); }
  if !siblings_cannot_include(tree, node_id, |node|
       matches!(&node . kind, MaybePlacedViewnodeKind::Scaff (Scaffold::IDCol))) {
    errors . push("IDCol must be unique among its siblings." . to_string()); }
  errors }

fn validate_idscaffold (
  tree    : &Tree<MaybePlacedViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_does_not_exist(tree, node_id, 1, true) {
    errors . push("ID scaffold must have no (non-ignored) children." . to_string()); }
  if !generation_exists_and_includes(tree, node_id, -1, false, |node|
       matches!(&node . kind, MaybePlacedViewnodeKind::Scaff (Scaffold::IDCol))) {
    errors . push("ID scaffold must have an IDCol parent." . to_string()); }
  errors }

fn validate_inactive_node (
  tree    : &Tree<MaybePlacedViewnode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(tree, node_id, -1, false, |node|
       matches!(&node . kind, MaybePlacedViewnodeKind::True (_))) {
    errors . push("Inactive placeholder must have a TrueNode parent."
                  . to_string()); }
  if !generation_does_not_exist(tree, node_id, 1, true) {
    errors . push("Inactive placeholder must have no active children."
                  . to_string()); }
  errors }

fn validate_truenode (
  tree    : &Tree<MaybePlacedViewnode>,
  node_id : NodeId,
  t       : &MaybePlacedTruenode,
  config  : &SkgConfig,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !has_id (t) {
    errors . push("TrueNode must have an ID." . to_string()); }
  if !has_valid_source(t, config) {
    errors . push("TrueNode must have a source that exists in the config."
                . to_string() ); }
  if !generation_includes_only(tree, node_id, 1, true, |node|
       matches!(&node . kind,
         MaybePlacedViewnodeKind::True (_)                        |
         MaybePlacedViewnodeKind::Scaff (Scaffold::AliasCol)      |
         MaybePlacedViewnodeKind::Scaff (Scaffold::IDCol)         |
         MaybePlacedViewnodeKind::Scaff (Scaffold::RoleCol { roleCol: RoleCol::Subscribee }) |
         MaybePlacedViewnodeKind::Scaff (Scaffold::RoleCol { roleCol: RoleCol::Subscriber }) |
         MaybePlacedViewnodeKind::Scaff (Scaffold::RoleCol { roleCol: RoleCol::Overridden }) |
         MaybePlacedViewnodeKind::Scaff (Scaffold::RoleCol { roleCol: RoleCol::Overrider })  |
         MaybePlacedViewnodeKind::Scaff (Scaffold::RoleCol { roleCol: RoleCol::Hider })      |
         MaybePlacedViewnodeKind::Scaff (Scaffold::RoleCol { roleCol: RoleCol::Hidden })     |
         MaybePlacedViewnodeKind::Scaff (Scaffold::TextChanged { .. })   |
         MaybePlacedViewnodeKind::Deleted (_)                     |
         MaybePlacedViewnodeKind::DeletedScaff (_)                |
         MaybePlacedViewnodeKind::Inactive (_)                    |
         MaybePlacedViewnodeKind::Unknown (_)                    )) {
    errors . push("TrueNode's children must include only TrueNode, AliasCol, IDCol, relation collection scaffolds, TextChanged, Deleted, DeletedScaff, InactiveNode, or UnknownNode" . to_string()); }
  if !nonignored_children_have_distinct_ids(tree, node_id) {
    errors . push("TrueNode's non-ignored content children must be unique (no two sharing the same ID)." . to_string()); }
  if has_empty_title (t) {
    errors . push("Definitive node has an empty title." . to_string()); }
  errors }

/// Check if an MaybePlacedTruenode has an ID.
pub fn has_id ( t : &MaybePlacedTruenode ) -> bool {
  t . id . is_some() }

/// Check if an MaybePlacedTruenode has a source and it exists in the config.
pub fn has_valid_source (
  t      : &MaybePlacedTruenode,
  config : &SkgConfig,
) -> bool {
  t . source . as_ref()
    . is_some_and( |s| config . sources . contains_key (s) ) }

/// A definitive node (not marked for deletion) must have a non-empty title.
/// Nodes that are indefinitive or carry a delete request are exempt.
fn has_empty_title ( t : &MaybePlacedTruenode ) -> bool {
  let is_definitive : bool =
    matches! ( &t . indef_or_def, IndefOrDef::Definitive { .. } );
  let is_delete : bool =
    matches! ( t . edit_request (),
               Some (&EditRequest::Delete) );
  is_definitive && !is_delete && t . title . trim () . is_empty () }

/// Check that all non-ignored, non-phantom content children
/// have distinct IDs.
/// "Non-ignored" means parentIs == Container.
/// "Non-phantom" means diff is not Removed or RemovedHere.
/// Returns true if all such children have distinct IDs,
/// or if there are no such children.
pub fn nonignored_children_have_distinct_ids (
  tree    : &Tree<MaybePlacedViewnode>,
  node_id : NodeId,
) -> bool {
  let Some (node_ref) = tree . get (node_id)
    else { return true; };
  let mut seen : HashSet<ID> = HashSet::new();
  for child in node_ref . children() {
    let content_id : Option<ID> =
      match &child . value() . kind {
        MaybePlacedViewnodeKind::True (t)
          if !t . parent_ignores_it() && !t . is_phantom () =>
          t . id . clone(),
        MaybePlacedViewnodeKind::Inactive (i)
          if i . membership . staged != Some (Sign::Minus)
          && i . membership . unstaged != Some (Sign::Minus) =>
          Some (i . id . clone()),
        _ => None,
      };
    if let Some (id) = content_id {
      if !seen . insert(id) {
        return false; }}}
  true }

fn relation_col_children_have_distinct_ids (
  tree    : &Tree<MaybePlacedViewnode>,
  node_id : NodeId,
) -> bool {
  let Some (node_ref) = tree . get (node_id)
    else { return true; };
  let mut seen : HashSet<ID> = HashSet::new();
  for child in node_ref . children() {
    let Some (id) =
      (match &child . value() . kind {
        MaybePlacedViewnodeKind::True (t)
          if !t . is_phantom () =>
          t . id . clone(),
        _ => None,
      })
    else { continue; };
    if !seen . insert (id) {
      return false; }}
  true }
