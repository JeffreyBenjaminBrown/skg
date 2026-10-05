use crate::types::viewnode::{AffectsParent, Viewnode, ViewnodeKind};
use crate::types::viewnode::Vognode;
use crate::types::tree::generic::{with_node_mut, write_at_ancestor_in_tree};

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::hash::Hash;

/// Apply treatment to each child that should be treated.
pub fn treat_certain_children<Node, Treated, Treatment> (
  tree          : &mut Tree<Node>,
  parent_skgid  : NodeId,
  treated       : Treated,
  mut treatment : Treatment,
) -> Result<(), String>
where
  Treated   : Fn (&Node) -> bool,
  Treatment : FnMut (&mut Node),
{
  let child_skgids : Vec<NodeId> =
    { let node_ref : NodeRef<Node> =
        tree . get (parent_skgid)
        . ok_or ("treat_certain_children: node not found")?;
      node_ref . children() . map( |c| c . id() ) . collect() };
  for child_skgid in child_skgids {
    with_node_mut( tree, child_skgid,
                   |mut n| { if treated( n . value() )
                             { treatment( n . value() ); }
    } ) . map_err( |e| -> String { e . into() } )?; }
  Ok( () ) }

/// TODO/DONE/fork-fixes.org: a NEW hiddenInSubscribee or
/// hiddenOutsideOfSubscribee folder begins folded. Creation sites stamp
/// 'folded' on the newborn folder; this runs at the folder's own BFS visit
/// (its members now reconciled in) and transfers the stamp: every
/// child is marked folded and the folder's own flag is cleared. That
/// encoding is the client's (elisp/skg-org-fold.el): 'folded' on a
/// headline means it is hidden inside its folded PARENT, so a folder's
/// collapse lives on its members -- left on the folder itself it would
/// instead fold the folder's parent. A folder PARSED from a buffer carries
/// 'folded' only when it sat inside a folded ancestor; its members
/// then already carry the mark, so the transfer changes nothing.
pub fn fold_members_of_newborn_folder (
  tree : &mut Tree<Viewnode>,
  folder  : NodeId,
) -> Result<(), String> {
  let newborn : bool =
    tree . get (folder)
    . ok_or ("fold_members_of_newborn_folder: node not found") ?
    . value () . folded;
  if ! newborn { return Ok (( )); }
  with_node_mut ( tree, folder,
                  |mut n| { n . value () . folded = false; } )
    . map_err ( |e| -> String { e . into () } ) ?;
  treat_certain_children (
    tree, folder,
    |_vn : &Viewnode| true,
    |vn : &mut Viewnode| { vn . folded = true; } ) }

/// Classify children of a node based on a classifier function.
/// Returns a HashMap from classification key to Vec<NodeId>.
/// Order is preserved within each list.
pub fn partition_children<Node, F> (
  tree       : &Tree<Node>,
  treeid     : NodeId,
  classifier : F,
) -> Result<HashMap<i32, Vec<NodeId>>, String>
where F: Fn (&Node) -> i32 {
  let node_ref : NodeRef<Node> =
    tree . get (treeid)
    . ok_or ("partition_children: node not found")?;
  let mut groups : HashMap<i32, Vec<NodeId>> = HashMap::new();
  for child_ref in node_ref . children() {
    let key = classifier( child_ref . value() );
    groups . entry (key) . or_default() . push( child_ref . id() ); }
  Ok (groups) }

/// Returns true if this node or any descendant satisfies the predicate.
pub fn subtree_satisfies<Node, Predicate> (
  tree      : &Tree<Node>,
  treeid     : NodeId,
  predicate : &Predicate,
) -> Result<bool, String>
where Predicate: Fn (&Node) -> bool {
  let node_ref : NodeRef<Node> =
    tree . get (treeid)
    . ok_or ("subtree_satisfies: node not found")?;
  if predicate( node_ref . value() ) {
    return Ok (true); }
  for child_ref in node_ref . children() {
    if subtree_satisfies( tree, child_ref . id(), predicate )? {
      return Ok (true); } }
  Ok (false) }

/// Detach a non-vognode node, transferring focus to its parent first if
/// the detached subtree contained the focused node. Used when a
/// non-vognode collapses (e.g. SubscribeeFolder with no goal subscribees,
/// HiddenInSubscribeeFolder with no remaining hidden children).
pub fn detach_viewnode_transferring_focus (
  tree : &mut Tree<Viewnode>,
  node : NodeId,
) -> Result<(), Box<dyn Error>> {
  let has_focus : bool =
    subtree_satisfies ( tree, node, &|n : &Viewnode| n . focused ) ?;
  if has_focus {
    write_at_ancestor_in_tree (
      tree, node, 1,
      |vn : &mut Viewnode| { vn . focused = true; } )
      . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?; }
  with_node_mut ( tree, node,
                  |mut n| { n . detach (); } )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  Ok (()) }

/// Move an existing child to the end of its parent's children list.
/// Uses detach() + append_id() to move without cloning.
pub fn move_child_to_end<Node> (
  tree         : &mut Tree<Node>,
  parent_skgid : NodeId,
  child_skgid  : NodeId,
) -> Result<(), Box<dyn Error>> {
  with_node_mut( tree, child_skgid,
                 |mut n| { n . detach(); } )
    . map_err( |e| -> Box<dyn Error> { e . into() } )?;
  with_node_mut( tree, parent_skgid,
                 |mut p| { p . append_id (child_skgid); } )
    . map_err( |e| -> Box<dyn Error> { e . into() } )?;
  Ok( () ) }

/// What 'complete_relevant_children' changed while reconciling: the
/// orderkeys it created, demoted to non-member (stale branches),
/// detached as stale leaves, and detached as duplicates. Callers
/// that warn about repairs to write-protected folders consume this
/// ('CompletionWarning'); other callers ignore it.
#[derive(Debug)]
pub struct RepairSummary<Orderkey> {
  pub created            : Vec<Orderkey>,
  pub demoted            : Vec<Orderkey>,
  pub deleted_stale      : Vec<Orderkey>,
  pub deleted_duplicates : Vec<Orderkey>,
}

impl<Orderkey> RepairSummary<Orderkey> {
  fn new () -> RepairSummary<Orderkey> {
    RepairSummary {
      created            : Vec::new (),
      demoted            : Vec::new (),
      deleted_stale      : Vec::new (),
      deleted_duplicates : Vec::new (), }}
}

/// Viewnode-specialized version of complete_relevant_children.
/// See that one's description for more info.
/// This one specializes it so that:
/// - problem discards are those that would discard the focused node.
/// - if any discard is problematic, transfer focus to 'treeid'
pub fn complete_relevant_children_in_viewforest
<Orderkey, Relevant, View> (
  tree                : &mut Tree<Viewnode>,
  treeid              : NodeId,
  relevant            : Relevant,
  view_child_orderkey : View,
  goal_list           : &[Orderkey],
  create_child        : impl Fn (&Orderkey) -> Result<Viewnode, String>,
) -> Result<RepairSummary<Orderkey>, Box<dyn Error>>
where Relevant : Fn (&Viewnode) -> bool,
      View     : Fn (&Viewnode) -> Result<Orderkey, String>,
      Orderkey : Eq + Hash + Clone,
{
  let problem_discard =
    |tree: &Tree<Viewnode>, stale_treeid: NodeId| -> Result<bool, String> {
      subtree_satisfies( tree, stale_treeid, &|n: &Viewnode| n . focused ) };
  let problem_discard_response =
    |n: &mut Viewnode| { n . focused = true; };
  // TODO/DONE/local-view-update/plan_v2.org §6.0 stale-member rule: a stale member (relevant child not in the goal)
  // that is an Unrestricted, affectsParent=true *branch* (has children) is demoted to
  // non-member so the user's subtree survives; a stale RestrictedVognode
  // *branch* is deadened to a DeadViewnode instead (it has no
  // affectsParent to demote; the orphan handling then preserves its
  // subtree as independent -- TODO/DONE/full-schema/DONE/9-2_source-set-safety.org);
  // everything else stale -- a leaf, a diff-phantom, a property -- is
  // deleted by the reconciler. Returns true iff it kept the node.
  let demote_invalid =
    |tree: &mut Tree<Viewnode>, stale_treeid: NodeId|
      -> Result<bool, Box<dyn Error>> {
      enum StaleTreatment { Demote, Deaden, Detach }
      let treatment : StaleTreatment = {
        let n : NodeRef<Viewnode> = tree . get (stale_treeid)
          . ok_or ("demote_invalid: node not found") ?;
        let has_children : bool = n . children () . next () . is_some ();
        match &n . value () . kind {
          ViewnodeKind::Vognode (Vognode::Unrestricted (t))
            if has_children && t . affectsParent == AffectsParent::True
            => StaleTreatment::Demote,
          ViewnodeKind::Vognode (Vognode::Restricted (_))
            if has_children
            => StaleTreatment::Deaden,
          _ => StaleTreatment::Detach } };
      match treatment {
        StaleTreatment::Detach => Ok (false),
        StaleTreatment::Demote => {
          with_node_mut ( tree, stale_treeid, |mut n| {
            if let ViewnodeKind::Vognode (Vognode::Unrestricted (t))
              = &mut n . value () . kind
              { t . affectsParent = AffectsParent::False; } } )
            . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
          Ok (true) },
        StaleTreatment::Deaden => {
          with_node_mut ( tree, stale_treeid, |mut n| {
            n . value () . kind = ViewnodeKind::DeadViewnode; } )
            . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
          Ok (true) }, }};
  complete_relevant_children(
    tree,
    treeid,
    relevant,
    view_child_orderkey,
    goal_list,
    create_child,
    problem_discard,
    problem_discard_response,
    demote_invalid ) }

/// PURPOSE:
/// Reconciles a parent's "relevant" children against a desired list of 'orderkeys'.
/// The payload of each tree node should be some possibly-improper supertype of 'orderkey' -- that is,
/// there is a way to extract the orderkey from a node, but the node might have other information.
/// .
/// STEPS:
/// - Partitions children into "relevant" (matching a predicate) and "irrelevant".
///   It is not necessary that irrelevant children be able to provide an orderkey.
/// - Among relevant children, identifies duplicates and "stale" members (those
///   whose orderkey is not in goal_list).
///   - Duplicates are always detached.
///   - A stale member is offered to 'demote_invalid'; if that keeps it (returns
///     true, e.g. TODO/DONE/local-view-update/plan_v2.org §6.0 demote-a-branch-to-non-member) it survives, otherwise it
///     is detached.
///   Runs problem_discard on each node that is actually detached; if any returns
///   true, runs problem_discard_response on the parent afterward.
/// - Reorders remaining children: irrelevant first, then relevant in goal_list order.
///   Creates new children for any orderkeys missing from the original children.
///   USER-FACING CONSEQUENCE for PartnerFolders: a non-member
///   child a user parks inside a folder is "irrelevant" here, so it is moved
///   ABOVE the generated members on save. This is deliberate -- the
///   membership is generated and ordered, so a parked note stays but is
///   visibly separated from the live membership. Documented in docs/glossary.org
///   ("PartnerFolder") and docs/sharing-model.org.
/// - Returns a RepairSummary of what it created, demoted and detached.
pub fn complete_relevant_children
<Node, Orderkey, Relevant, View, ProblemDiscard, ProblemResponse, DemoteInvalid> (
  tree                     : &mut Tree<Node>,
  parent_skgid             : NodeId,
  relevant                 : Relevant,
  view_child_orderkey      : View,
  goal_list                : &[Orderkey],
  create_child             : impl Fn (&Orderkey) -> Result<Node, String>,
  problem_discard          : ProblemDiscard,
  mut problem_discard_response : ProblemResponse,
  demote_invalid           : DemoteInvalid,
) -> Result<RepairSummary<Orderkey>, Box<dyn Error>>
where
  Relevant        : Fn (&Node) -> bool,
  // The orderkey and create-child closures are fallible so a relevant
  // child of an unexpected kind, or an unprefetched orderkey, surfaces
  // as an Err from this function rather than a panic in a closure.
  View            : Fn (&Node) -> Result<Orderkey, String>,
  Orderkey        : Eq + Hash + Clone,
  ProblemDiscard  : Fn(&Tree<Node>, NodeId) -> Result<bool, String>,
  ProblemResponse : FnMut (&mut Node),
  DemoteInvalid   : Fn(&mut Tree<Node>, NodeId) -> Result<bool, Box<dyn Error>>,
{
  let groups : HashMap<i32, Vec<NodeId>> =
    partition_children (
      tree, parent_skgid,
      |n| if relevant (n) { 1 } else { 0 } )
        . map_err( |e| -> Box<dyn Error> { e . into() } )?;
  let relevant_skgids   : Vec<NodeId> =
    groups . get( &1 ) . cloned() . unwrap_or_default();
  let irrelevant_skgids : Vec<NodeId> =
    groups . get( &0 ) . cloned() . unwrap_or_default();
  let goal_list_as_set : HashSet<Orderkey> =
    goal_list . iter() . cloned() . collect();

  let mut orderkey_to_treeid
    : HashMap<Orderkey, NodeId> = HashMap::new();
  let mut duplicate_skgids : Vec<( NodeId, Orderkey )> = Vec::new();
  let mut invalid_skgids : Vec<( NodeId, Orderkey )> = Vec::new();
  for &treeid in &relevant_skgids { // populate the above three variables
    let node_ref : NodeRef<Node> =
    tree . get (treeid)
      . ok_or ("complete_relevant_children: node not found")?;
    let orderkey : Orderkey =
      view_child_orderkey( node_ref . value() ) ?;
    if !goal_list_as_set . contains (&orderkey) {
      invalid_skgids . push( ( treeid, orderkey ) ); // hopefully none
    } else if orderkey_to_treeid . contains_key (&orderkey) {
      duplicate_skgids . push( ( treeid, orderkey ) ); // hopefully none
    } else {
      orderkey_to_treeid . insert( orderkey, treeid ); } }
  let mut summary : RepairSummary<Orderkey> = RepairSummary::new();
  let mut discard_has_problem : bool = false;
  // Duplicates are redundant: always detach (recording focus loss).
  for ( treeid, orderkey ) in &duplicate_skgids {
    if problem_discard( tree, *treeid )? { discard_has_problem = true; }
    with_node_mut( tree, *treeid,
                   |mut n| { n . detach(); } )
      . map_err( |e| -> Box<dyn Error> { e . into() } )?;
    summary . deleted_duplicates . push( orderkey . clone() ); }
  // Stale members: demote_invalid may keep one (TODO/DONE/local-view-update/plan_v2.org §6.0 demote-a-branch); only a
  // node it does NOT keep is detached, and only that detach can lose focus.
  for ( treeid, orderkey ) in &invalid_skgids {
    if demote_invalid( tree, *treeid )? {
      summary . demoted . push( orderkey . clone() );
      continue; }
    if problem_discard( tree, *treeid )? { discard_has_problem = true; }
    with_node_mut( tree, *treeid,
                   |mut n| { n . detach(); } )
      . map_err( |e| -> Box<dyn Error> { e . into() } )?;
    summary . deleted_stale . push( orderkey . clone() ); }
  if discard_has_problem {
    // Respond to problem if any discard was problematic
    with_node_mut(
        tree, parent_skgid,
        |mut n| { problem_discard_response( n . value() ); }
      ) . map_err( |e| -> Box<dyn Error> { e . into() } )?; }
  for &child_skgid in &irrelevant_skgids {
    // Move irrelevant children to end. (They will precede the relevant ones.)
    move_child_to_end( tree, parent_skgid, child_skgid )?; }
  for orderkey in goal_list {
    // Move/create relevant children in desired order
    match orderkey_to_treeid . get (orderkey) {
      Some (&child_skgid) => {
        move_child_to_end( tree, parent_skgid, child_skgid )?; },
      None => {
        let node : Node = create_child (orderkey) ?;
        with_node_mut(
            tree, parent_skgid,
            |mut p| { p . append (node); }
          ) . map_err( |e| -> Box<dyn Error>
                      { e . into() } )?;
        summary . created . push( orderkey . clone() ); }, }}
  Ok (summary) }

#[cfg(test)]
#[path = "../../tests/unit/update_buffer_util.rs"]
mod tests;
