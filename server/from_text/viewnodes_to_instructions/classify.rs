use crate::types::misc::ID;
use crate::types::viewnode::{Scaffold, ViewNode, ViewNodeKind};
use ego_tree::{NodeId, NodeMut, NodeRef, Tree};
use std::collections::HashMap;

pub type SaveRoleMap = HashMap<NodeId, SaveRole>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SaveRole {
  BufferRoot,
  OrdinaryTrueNode,
  AsSubscribee { subscriber: ID },
  HiddenInSubscribeeCol { subscriber: ID, subscribee: ID },
  HiddenOutsideOfSubscribeeCol { subscriber: ID },
  AliasText,
  IdDisplay,
  DisplayOnly,
}

#[allow(non_camel_case_types)]
#[derive(Clone, Debug, PartialEq)]
pub struct ViewNode_in_Role {
  pub viewnode : ViewNode,
  pub role     : SaveRole,
}

pub fn classify_save_roles (
  viewforest : &Tree<ViewNode>,
) -> Result<SaveRoleMap, String> {
  let mut roles : SaveRoleMap = HashMap::new();
  for node_ref in viewforest . nodes() {
    let node_id : NodeId = node_ref . id();
    let role : SaveRole = saverole_for_node (node_ref) ?;
    roles . insert (node_id, role); }
  Ok (roles) }

pub fn viewforest_with_saveroles (
  viewforest : &Tree<ViewNode>,
) -> Result<Tree<ViewNode_in_Role>, String> {
  let root_ref : NodeRef<ViewNode> =
    viewforest . root();
  let mut result : Tree<ViewNode_in_Role> =
    // Right after this, 'result' only corresponds to the root of 'viewforest'.
    Tree::new (ViewNode_in_Role {
      viewnode : root_ref . value() . clone(),
      role     : saverole_for_node (root_ref)?,
    });
  let target_root_id : NodeId =
    result . root() . id();
  copy_role_children_recursive (
    // Right after this, they entirely correspond.
    viewforest,
    &mut result,
    root_ref . id(),
    target_root_id)?;
  Ok (result) }

fn copy_role_children_recursive (
  source          : &Tree<ViewNode>,
  target          : &mut Tree<ViewNode_in_Role>,
  source_parent   : NodeId,
  target_parent   : NodeId,
) -> Result<(), String> {
  let child_ids : Vec<NodeId> =
    source . get (source_parent) . unwrap()
    . children()
    . map (|child| child . id())
    . collect();
  for child_id in child_ids {
    let child_id : NodeId = child_id;
    let child_ref : NodeRef<ViewNode> =
      source . get (child_id) . unwrap();
    let target_child_id : NodeId =
      { let mut target_parent_mut : NodeMut<ViewNode_in_Role> =
          target . get_mut (target_parent) . unwrap();
        target_parent_mut . append (ViewNode_in_Role {
          viewnode : child_ref . value() . clone(),
          role     : saverole_for_node (child_ref)?,
        }) . id() };
    copy_role_children_recursive (
      source, target, child_id, target_child_id)?; }
  Ok (()) }

fn saverole_for_node (
  node_ref : NodeRef<ViewNode>,
) -> Result<SaveRole, String> {
  match &node_ref . value() . kind {
    ViewNodeKind::True (_) =>
      classify_truenode (node_ref),
    ViewNodeKind::Scaff (Scaffold::BufferRoot) =>
      Ok (SaveRole::BufferRoot),
    ViewNodeKind::Scaff (Scaffold::Alias { .. }) =>
      Ok (SaveRole::AliasText),
    ViewNodeKind::Scaff (Scaffold::ID { .. }) =>
      Ok (SaveRole::IdDisplay),
    ViewNodeKind::Scaff (_)
      | ViewNodeKind::Deleted (_)
      | ViewNodeKind::DeletedScaff (_)
      | ViewNodeKind::Unknown (_)
      => Ok (SaveRole::DisplayOnly), }}

fn classify_truenode (
  node_ref : NodeRef<ViewNode>,
) -> Result<SaveRole, String> {
  let Some (parent_ref) = node_ref . parent()
    else { return Ok (SaveRole::OrdinaryTrueNode); };
  match &parent_ref . value() . kind {
    ViewNodeKind::Scaff (Scaffold::SubscribeeCol) =>
      Ok (SaveRole::AsSubscribee {
        subscriber : truenode_id (
          parent_ref . parent(),
          "SubscribeeCol must have a TrueNode parent")?,
      }),
    ViewNodeKind::Scaff (Scaffold::HiddenInSubscribeeCol) => {
      let subscribee_ref : NodeRef<ViewNode> =
        parent_ref . parent() . ok_or (
          "HiddenInSubscribeeCol must have a TrueNode parent")?;
      let subscribee : ID =
        truenode_id (
          Some (subscribee_ref),
          "HiddenInSubscribeeCol must have a TrueNode parent")?;
      let subscribeecol_ref : NodeRef<ViewNode> =
        subscribee_ref . parent() . ok_or (
          "HiddenInSubscribeeCol subscribee must have a parent")?;
      if ! matches!(
        &subscribeecol_ref . value() . kind,
        ViewNodeKind::Scaff (Scaffold::SubscribeeCol))
      {
        return Err (
          "HiddenInSubscribeeCol subscribee must be under SubscribeeCol"
            . to_string()); }
      Ok (SaveRole::HiddenInSubscribeeCol {
        subscriber : truenode_id (
          subscribeecol_ref . parent(),
          "SubscribeeCol must have a TrueNode parent")?,
        subscribee,
      }) },
    ViewNodeKind::Scaff (Scaffold::HiddenOutsideOfSubscribeeCol) => {
      let subscribeecol_ref : NodeRef<ViewNode> =
        parent_ref . parent() . ok_or (
          "HiddenOutsideOfSubscribeeCol must have a SubscribeeCol parent")?;
      if ! matches!(
        &subscribeecol_ref . value() . kind,
        ViewNodeKind::Scaff (Scaffold::SubscribeeCol))
      {
        return Err (
          "HiddenOutsideOfSubscribeeCol must have a SubscribeeCol parent"
            . to_string()); }
      Ok (SaveRole::HiddenOutsideOfSubscribeeCol {
        subscriber : truenode_id (
          subscribeecol_ref . parent(),
          "SubscribeeCol must have a TrueNode parent")?,
      }) },
    _ => Ok (SaveRole::OrdinaryTrueNode),
  }}

fn truenode_id (
  node_ref : Option<NodeRef<ViewNode>>,
  error    : &str,
) -> Result<ID, String> {
  let node_ref : NodeRef<ViewNode> =
    node_ref . ok_or_else (|| error . to_string())?;
  match &node_ref . value() . kind {
    ViewNodeKind::True (t) =>
      Ok (t . id . clone()),
    _ =>
      Err (error . to_string()),
  }}
