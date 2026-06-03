use crate::from_text::viewnodes_to_instructions::classify::{
  SaveRole, ViewNode_in_Role };
use crate::from_text::viewnodes_to_instructions::to_naive_instructions::{
  collect_savenode_candidates, DefinenodeCandidate, DefinenodeCandidateKind };
use crate::types::misc::ID;
use crate::types::viewnode::{EditRequest, ParentIs, ViewNode, ViewNodeKind};
use crate::types::viewnode::Vognode;

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
  let candidates : Vec<DefinenodeCandidate> =
    collect_savenode_candidates (viewforest)?;
  subscribee_hiderel_intents_from_candidates (
    viewforest, &candidates) }

pub(crate) fn subscribee_hiderel_intents_from_candidates (
  viewforest : &Tree<ViewNode_in_Role>,
  candidates : &[DefinenodeCandidate],
) -> Result<Vec<SubscribeeHiderelIntent>, String> {
  let mut result : Vec<SubscribeeHiderelIntent> =
    Vec::new();
  for candidate in candidates {
    let subscriber : ID =
      match &candidate . kind {
        DefinenodeCandidateKind::Subscribee { subscriber } =>
          subscriber . clone(),
        _ => continue,
      };
    let node_ref : NodeRef<ViewNode_in_Role> =
      viewforest . get (candidate . treeid) . ok_or (
        "subscribee hiderel candidate not found")?;
    // At-most-one-writer-per-ID: a subscribee-as-such infers
    // hide/unhide edits for its subscriber N, but only the SubscribeeCol
    // under the *definitive* instance of N may write them. The same
    // subscriber can recur indefinitively elsewhere with its own
    // SubscribeeCol; without this guard those would emit contradictory
    // hide edits for one ID. (plan_v2 §6.1.) The subscribee's own
    // definitiveness is already required by collect_savenode_candidates.
    if ! subscriber_instance_is_definitive (&node_ref) {
      continue; }
    let subscribee : ID =
      match &node_ref . value() . viewnode . kind {
        ViewNodeKind::Vognode (Vognode::Normal (t))
          => t . id . clone(),
        _ => return Err (
          "subscribee-as-such candidate was not a Normal viewnode"
            . to_string()),
      };
    result . push (SubscribeeHiderelIntent {
      subscriber,
      subscribee,
      visible_content :
        collect_visible_content (&node_ref),
    }); }
  Ok (result) }

/// True iff the subscriber instance this subscribee-as-such hangs under
/// is a *definitive* Normal vognode. The tree shape is
/// subscriber -> SubscribeeCol -> subscribee-as-such, so the subscriber
/// is the candidate's grandparent. See the at-most-one-writer-per-ID
/// note at the call site (plan_v2 §6.1).
fn subscriber_instance_is_definitive (
  subscribee_ref : &NodeRef<ViewNode_in_Role>,
) -> bool {
  subscribee_ref . parent()              // SubscribeeCol
    . and_then (|col| col . parent())    // subscriber vognode
    . map (|subscriber| matches!(
        &subscriber . value() . viewnode . kind,
        ViewNodeKind::Vognode (Vognode::Normal (t))
          if ! t . is_indefinitive() ) )
    . unwrap_or (false) }

/// Collect the children that the buffer presents as visible
/// content of one subscribee-as-such.
///
/// This list is not saved as the subscribee's 'contains'.  It is a
/// signal used to infer what the subscriber should hide or unhide for
/// this subscription.
fn collect_visible_content (
  node_ref : &NodeRef<ViewNode_in_Role>,
) -> Vec<ID> {
  let mut result : Vec<ID> = Vec::new();
  for child_ref in node_ref . children() {
    let child_ref : NodeRef<ViewNode_in_Role> = child_ref;
    let child : &ViewNode = &child_ref . value() . viewnode;
    match &child . kind {
      ViewNodeKind::Vognode (Vognode::Normal (t)) => {
        if matches!(
             child_ref . value() . role,
             SaveRole::Ordinary)
           && ( t . parentIs == ParentIs::Affected )
           && ! matches!(
             t . edit_request(),
             Some (&EditRequest::Delete))
        { result . push (t . id . clone()); }},
      _ => continue,
    }}
  result }
