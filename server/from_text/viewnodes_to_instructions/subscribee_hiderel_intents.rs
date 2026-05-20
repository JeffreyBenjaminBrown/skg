use crate::from_text::viewnodes_to_instructions::classify::{
  SaveRole, ViewNode_in_Role };
use crate::from_text::viewnodes_to_instructions::to_naive_instructions::{
  collect_intent_candidates, IntentCandidate, IntentCandidateKind };
use crate::types::misc::ID;
use crate::types::viewnode::{EditRequest, ViewNode, ViewNodeKind};

use ego_tree::{NodeRef, Tree};

/// Raw user-authored child-list signal from a subscribee-as-such branch.
///
/// This is not a graph `contains` edit. Later save stages can compare
/// it with disk `subscribee.contains` and subscriber hides to infer
/// hide/unhide edits on the subscriber.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SubscribeeHiderelIntent {
  pub subscriber      : ID,
  pub subscribee      : ID,
  pub visible_content : Vec<ID>, // What *not* to hide.
}

/// This only exists to avoid test churn.
/// It is called in tests but never in production.
pub fn subscribee_hiderel_intents_from_tree (
  viewforest : &Tree<ViewNode_in_Role>,
) -> Result<Vec<SubscribeeHiderelIntent>, String> {
  let candidates : Vec<IntentCandidate> =
    collect_intent_candidates (viewforest)?;
  subscribee_hiderel_intents_from_candidates (
    viewforest, &candidates) }

pub(crate) fn subscribee_hiderel_intents_from_candidates (
  viewforest : &Tree<ViewNode_in_Role>,
  candidates : &[IntentCandidate],
) -> Result<Vec<SubscribeeHiderelIntent>, String> {
  let mut result : Vec<SubscribeeHiderelIntent> =
    Vec::new();
  for candidate in candidates {
    let subscriber : ID =
      match &candidate . kind {
        IntentCandidateKind::SubscribeeAsSuch { subscriber } =>
          subscriber . clone(),
        _ => continue,
      };
    let node_ref : NodeRef<ViewNode_in_Role> =
      viewforest . get (candidate . treeid) . ok_or (
        "subscribee hiderel candidate not found")?;
    let subscribee : ID =
      match &node_ref . value() . viewnode . kind {
        ViewNodeKind::True (t) => t . id . clone(),
        _ => return Err (
          "subscribee-as-such candidate was not a TrueNode"
            . to_string()),
      };
    result . push (SubscribeeHiderelIntent {
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
