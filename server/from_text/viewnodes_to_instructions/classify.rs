use crate::types::misc::ID;
use crate::types::viewnode::{Scaffold, ViewNode, ViewNodeKind, RoleCol};
use ego_tree::{NodeId, NodeMut, NodeRef, Tree};
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SaveRole {
  BufferRoot,
  Ordinary,
  Subscribee { subscriber: ID },
  Overridden { overrider: ID },
  HiddenInSubscribeeCol { subscriber: ID, subscribee: ID },
  HiddenOutsideOfSubscribeeCol { subscriber: ID },
  AliasDisplay,
  IdDisplay,
  DisplayOnly,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ViewNode_in_Role {
  pub viewnode : ViewNode,
  pub role     : SaveRole,
}

pub fn classify_save_roles (
  viewforest : &Tree<ViewNode>,
) -> Result< HashMap<NodeId, SaveRole>,
             String> {
  let mut roles : HashMap<NodeId, SaveRole> =
    HashMap::new();
  for node_ref in viewforest . nodes() {
    let node_id : NodeId = node_ref . id();
    let role : SaveRole = saverole_for_node (node_ref) ?;
    roles . insert (node_id, role); }
  Ok (roles) }

/// The SaveRole for each node is computed using the data in it and,
/// potentially, in some of its neighbors in the tree.
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
      Ok (SaveRole::AliasDisplay),
    ViewNodeKind::Scaff (Scaffold::ID { .. }) =>
      Ok (SaveRole::IdDisplay),
    ViewNodeKind::Scaff (_)
      | ViewNodeKind::Inactive (_)
      | ViewNodeKind::Deleted (_)
      | ViewNodeKind::DeletedScaff (_)
      | ViewNodeKind::Unknown (_)
      => Ok (SaveRole::DisplayOnly), }}

fn classify_truenode (
  node_ref : NodeRef<ViewNode>,
) -> Result<SaveRole, String> {
  let Some (parent_ref) = node_ref . parent()
    else { return Ok (SaveRole::Ordinary); };
  match &parent_ref . value() . kind {
    ViewNodeKind::Scaff (Scaffold::RoleCol { roleCol: RoleCol::Subscribee }) =>
      if node_ref . value () . is_truenode_and_claims_parentIs_collector () {
        Ok (SaveRole::Subscribee {
          subscriber : truenode_id (
            parent_ref . parent(),
            "SubscribeeCol must have a TrueNode parent")?, } )
      } else { Ok (SaveRole::DisplayOnly) },
    ViewNodeKind::Scaff (Scaffold::RoleCol { roleCol: RoleCol::Overridden }) =>
      if node_ref . value () . is_truenode_and_claims_parentIs_collector () {
        Ok (SaveRole::Overridden {
          overrider : truenode_id (
            parent_ref . parent(),
            "OverriddenCol must have a TrueNode parent")?, } )
      } else { Ok (SaveRole::DisplayOnly) },
    ViewNodeKind::Scaff (Scaffold::RoleCol { roleCol: RoleCol::Subscriber })
      | ViewNodeKind::Scaff (Scaffold::RoleCol { roleCol: RoleCol::Overrider })
      | ViewNodeKind::Scaff (Scaffold::RoleCol { roleCol: RoleCol::Hider })
      | ViewNodeKind::Scaff (Scaffold::RoleCol { roleCol: RoleCol::Hidden })
      => Ok (SaveRole::DisplayOnly),
    ViewNodeKind::Scaff (Scaffold::RoleCol {
      roleCol: RoleCol::HiddenInSubscribee })
      => { let subscribee_ref : NodeRef<ViewNode> =
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
             ViewNodeKind::Scaff (Scaffold::RoleCol {
               roleCol: RoleCol::Subscribee } ))
             { return Err (
                 "HiddenInSubscribeeCol subscribee must be a child of SubscribeeCol"
                 . to_string()); }
           Ok (SaveRole::HiddenInSubscribeeCol {
             subscriber : truenode_id (
               subscribeecol_ref . parent(),
               "SubscribeeCol must have a TrueNode parent")?,
             subscribee } ) },
    ViewNodeKind::Scaff (Scaffold::RoleCol {
      roleCol: RoleCol::HiddenOutsideOfSubscribee })
      => { let subscribeecol_ref : NodeRef<ViewNode> =
             parent_ref . parent() . ok_or (
               "HiddenOutsideOfSubscribeeCol must have a SubscribeeCol parent")?;
          if ! matches!(
            &subscribeecol_ref . value() . kind,
            ViewNodeKind::Scaff (Scaffold::RoleCol { roleCol: RoleCol::Subscribee }))
          { return Err (
              "HiddenOutsideOfSubscribeeCol must have a SubscribeeCol parent"
                . to_string()); }
          Ok (SaveRole::HiddenOutsideOfSubscribeeCol {
            subscriber : truenode_id (
              subscribeecol_ref . parent(),
              "SubscribeeCol must have a TrueNode parent")? } ) },
    _ => Ok (SaveRole::Ordinary) }}

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
