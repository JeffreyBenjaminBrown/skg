use crate::types::misc::ID;
use crate::types::viewnode::{Scaffold, ViewNode, ViewNodeKind};
use ego_tree::{NodeId, NodeRef, Tree};
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

pub fn classify_save_roles (
  viewforest : &Tree<ViewNode>,
) -> Result<SaveRoleMap, String> {
  let mut roles : SaveRoleMap = HashMap::new();
  for node_ref in viewforest . nodes() {
    let node_id : NodeId = node_ref . id();
    let role : SaveRole = classify_node (node_ref) ?;
    roles . insert (node_id, role); }
  Ok (roles) }

fn classify_node (
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
