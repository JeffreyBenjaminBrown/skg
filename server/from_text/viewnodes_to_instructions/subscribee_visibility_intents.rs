use crate::from_text::viewnodes_to_instructions::classify::{
  SaveRole, ViewNode_in_Role };
use crate::types::misc::ID;
use crate::types::viewnode::{EditRequest, ViewNode, ViewNodeKind};

use ego_tree::{NodeRef, Tree};

/// Raw user-authored child-list signal from a direct subscribee branch.
///
/// This is not a graph `contains` edit. Later save stages can compare
/// it with disk `subscribee.contains` and subscriber hides to infer
/// hide/unhide edits on the subscriber.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SubscribeeVisibilityIntent {
  pub subscriber      : ID,
  pub subscribee      : ID,
  pub visible_content : Vec<ID>,
}

pub fn subscribee_visibility_intents_from_tree (
  viewforest : &Tree<ViewNode_in_Role>,
) -> Result<Vec<SubscribeeVisibilityIntent>, String> {
  let mut result : Vec<SubscribeeVisibilityIntent> =
    Vec::new();
  for node_ref in viewforest . nodes() {
    let subscriber : ID =
      match &node_ref . value() . role {
        SaveRole::AsSubscribee { subscriber } =>
          subscriber . clone(),
        _ => continue,
      };
    let subscribee : ID =
      match &node_ref . value() . viewnode . kind {
        ViewNodeKind::True (t) => {
          if t . is_indefinitive() { continue; }
          t . id . clone() },
        _ => return Err (
          "AsSubscribee role assigned to non-TrueNode" . to_string()),
      };
    result . push (SubscribeeVisibilityIntent {
      subscriber,
      subscribee,
      visible_content :
        collect_direct_visible_content (&node_ref),
    }); }
  Ok (result) }

fn collect_direct_visible_content (
  node_ref : &NodeRef<ViewNode_in_Role>,
) -> Vec<ID> {
  let mut result : Vec<ID> = Vec::new();
  for child_ref in node_ref . children() {
    let child_ref : NodeRef<ViewNode_in_Role> = child_ref;
    let child : &ViewNode = &child_ref . value() . viewnode;
    match &child . kind {
      ViewNodeKind::True (t) => {
        if matches!(
             child_ref . value() . role,
             SaveRole::OrdinaryTrueNode)
           && ! t . parent_ignores_it()
           && ! t . is_phantom()
           && ! matches!(
             t . edit_request(),
             Some (&EditRequest::Delete))
        { result . push (t . id . clone()); }},
      _ => continue,
    }}
  result }
