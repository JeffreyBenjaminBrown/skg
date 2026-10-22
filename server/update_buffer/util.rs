use crate::types::viewnode::{
  AffectsParent, Birth, ChangeFacts, Viewnode, ViewnodeKind, Vognode};
use crate::types::tree::generic::{with_node_mut, write_at_ancestor_in_tree};
use crate::types::tree::moves::move_to_last_child;

use ego_tree::{NodeId, NodeMut, NodeRef, Tree};
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

/// A NEW hiddenInSubscribee or
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
    . value () . org_folded;
  if ! newborn { return Ok (( )); }
  with_node_mut ( tree, folder,
                  |mut n| { n . value () . org_folded = false; } )
    . map_err ( |e| -> String { e . into () } ) ?;
  treat_certain_children (
    tree, folder,
    |_vn : &Viewnode| true,
    |vn : &mut Viewnode| { vn . org_folded = true; } ) }

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

/// What 'complete_managed_children' changed while reconciling: the
/// orderkeys it created, those of the stale branches it made
/// non-members, those it detached as stale leaves, and those it
/// detached as duplicates. Callers
/// that warn about repairs to write-protected folders consume this
/// ('CompletionWarning'); other callers ignore it.
#[derive(Debug)]
pub struct RepairSummary<Orderkey> {
  pub created            : Vec<Orderkey>,
  pub made_nonMembers    : Vec<Orderkey>,
  pub deleted_stale      : Vec<Orderkey>,
  pub deleted_duplicates : Vec<Orderkey>,
}

impl<Orderkey> RepairSummary<Orderkey> {
  fn new () -> RepairSummary<Orderkey> {
    RepairSummary {
      created            : Vec::new (),
      made_nonMembers    : Vec::new (),
      deleted_stale      : Vec::new (),
      deleted_duplicates : Vec::new (), }}
}

/// The rule both reconcilers obey ('docs/diff-view.org', "What a
/// reconciler keeps"): a reconciler detaches only what it drew, and
/// what the user added survives. A stale child (one the reconciler
/// manages but would no longer draw) under which 'holds_the_users'
/// finds something of the user's is kept: an unrestricted vognode as
/// a plain non-member occurrence of its node, its diff facts cleared;
/// any other kind (a restricted vognode, a phantom, a property or a
/// folder) deadened, so that the orphan pass keeps what is under it
/// ('mark_orphans_under_dead_parents_false'). Returns whether it kept
/// the child; one it did not keep is the caller's to detach.
/// 'complete_managed_children_in_viewforest' never looks below the
/// children it manages, so for it any child of a stale child is the
/// user's; the diff view's reconciler draws below the children it
/// manages, and asks 'holds_the_users' in
/// 'server/diff_view/shallow_model_subtree.rs'.
pub fn keep_stale_child_holding_the_users (
  tree            : &mut Tree<Viewnode>,
  stale           : NodeId,
  holds_the_users : &dyn Fn (NodeRef<Viewnode>) -> bool,
) -> Result<bool, Box<dyn Error>> {
  { let node : NodeRef<Viewnode> = tree . get (stale)
      . ok_or ("keep_stale_child_holding_the_users: node not found") ?;
    if node . value () . kind == ViewnodeKind::BufferRoot
       || ! holds_the_users (node) {
      return Ok (false); }}
  let mut node_mut : NodeMut<Viewnode> = tree . get_mut (stale) . unwrap ();
  let viewnode : &mut Viewnode = node_mut . value ();
  match &mut viewnode . kind {
    ViewnodeKind::Vognode (Vognode::Unrestricted (t)) => {
      t . affectsParent = AffectsParent::False;
      t . birth         = Birth::Unremarkable;
      viewnode . change_facts = ChangeFacts::default (); },
    _ => viewnode . replace_kind (ViewnodeKind::DeadViewnode), }
  Ok (true) }

/// Viewnode-specialized version of complete_managed_children.
/// See that one's description for more info.
/// This one specializes it so that:
/// - problem discards are those that would discard the focused node.
/// - if any discard is problematic, transfer focus to 'treeid'
pub fn complete_managed_children_in_viewforest
<Orderkey, Managed, View> (
  tree                : &mut Tree<Viewnode>,
  treeid              : NodeId,
  managed             : Managed,
  view_child_orderkey : View,
  goal_list           : &[Orderkey],
  create_child        : impl Fn (&Orderkey) -> Result<Viewnode, String>,
) -> Result<RepairSummary<Orderkey>, Box<dyn Error>>
where Managed  : Fn (&Viewnode) -> bool,
      View     : Fn (&Viewnode) -> Result<Orderkey, String>,
      Orderkey : Eq + Hash + Clone,
{
  let problem_discard =
    |tree: &Tree<Viewnode>, stale_treeid: NodeId| -> Result<bool, String> {
      subtree_satisfies( tree, stale_treeid, &|n: &Viewnode| n . focused ) };
  let problem_discard_response =
    |n: &mut Viewnode| { n . focused = true; };
  // A stale child (managed, not in the goal) that has children is
  // kept as the rule says (an Unrestricted one as a non-member,
  // anything else as a DeadViewnode); only a childless one is detached.
  let make_stale_a_nonMember =
    |tree: &mut Tree<Viewnode>, stale_treeid: NodeId|
      -> Result<bool, Box<dyn Error>> {
      keep_stale_child_holding_the_users (
        tree, stale_treeid,
        &|stale : NodeRef<Viewnode>| stale . children () . next () . is_some () ) };
  complete_managed_children(
    tree,
    treeid,
    managed,
    view_child_orderkey,
    goal_list,
    create_child,
    problem_discard,
    problem_discard_response,
    make_stale_a_nonMember ) }

/// PURPOSE:
/// Reconciles a parent's "managed" children against a desired list of 'orderkeys'.
/// The payload of each tree node should be some possibly-improper supertype of 'orderkey' -- that is,
/// there is a way to extract the orderkey from a node, but the node might have other information.
/// .
/// STEPS:
/// - Partitions children into "managed" (matching a predicate) and "unmanaged".
///   It is not necessary that unmanaged children be able to provide an orderkey.
/// - Among managed children, identifies duplicates and "stale" members (those
///   whose orderkey is not in goal_list).
///   - Duplicates are always detached.
///   - A stale member is offered to 'make_stale_a_nonMember'; if that keeps
///     it (returns true, e.g. by making a branch a non-member) it survives,
///     otherwise it is detached.
///   Runs problem_discard on each node that is actually detached; if any returns
///   true, runs problem_discard_response on the parent afterward.
/// - Reorders remaining children: unmanaged first, then managed in goal_list order.
///   Creates new children for any orderkeys missing from the original children.
///   USER-FACING CONSEQUENCE for PartnerFolders: a non-member
///   child a user parks inside a folder is "unmanaged" here, so it is moved
///   ABOVE the generated members on save. This is deliberate -- the
///   membership is generated and ordered, so a parked note stays but is
///   visibly separated from the live membership. Documented in docs/glossary.org
///   ("PartnerFolder") and docs/sharing-model.org.
/// - Returns a RepairSummary: which children it created, which it made
///   non-members, and which it detached.
pub fn complete_managed_children
<Node, Orderkey, Managed, View, ProblemDiscard, ProblemResponse, MakeStaleANonMember> (
  tree                     : &mut Tree<Node>,
  parent_skgid             : NodeId,
  managed                  : Managed,
  view_child_orderkey      : View,
  goal_list                : &[Orderkey],
  create_child             : impl Fn (&Orderkey) -> Result<Node, String>,
  problem_discard          : ProblemDiscard,
  mut problem_discard_response : ProblemResponse,
  make_stale_a_nonMember   : MakeStaleANonMember,
) -> Result<RepairSummary<Orderkey>, Box<dyn Error>>
where
  Managed         : Fn (&Node) -> bool,
  // The orderkey and create-child closures are fallible so a managed
  // child of an unexpected kind, or an unprefetched orderkey, surfaces
  // as an Err from this function rather than a panic in a closure.
  View            : Fn (&Node) -> Result<Orderkey, String>,
  Orderkey        : Eq + Hash + Clone,
  ProblemDiscard  : Fn(&Tree<Node>, NodeId) -> Result<bool, String>,
  ProblemResponse : FnMut (&mut Node),
  MakeStaleANonMember : Fn(&mut Tree<Node>, NodeId) -> Result<bool, Box<dyn Error>>,
{
  let groups : HashMap<i32, Vec<NodeId>> =
    partition_children (
      tree, parent_skgid,
      |n| if managed (n) { 1 } else { 0 } )
        . map_err( |e| -> Box<dyn Error> { e . into() } )?;
  let managed_skgids    : Vec<NodeId> =
    groups . get( &1 ) . cloned() . unwrap_or_default();
  let unmanaged_skgids : Vec<NodeId> =
    groups . get( &0 ) . cloned() . unwrap_or_default();
  let goal_list_as_set : HashSet<Orderkey> =
    goal_list . iter() . cloned() . collect();

  let mut orderkey_to_treeid
    : HashMap<Orderkey, NodeId> = HashMap::new();
  let mut duplicate_skgids : Vec<( NodeId, Orderkey )> = Vec::new();
  let mut invalid_skgids : Vec<( NodeId, Orderkey )> = Vec::new();
  for &treeid in &managed_skgids { // populate the above three variables
    let node_ref : NodeRef<Node> =
    tree . get (treeid)
      . ok_or ("complete_managed_children: node not found")?;
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
  // Stale members: make_stale_a_nonMember may keep one (a branch made a
  // non-member); only a node it does NOT keep is detached, and only that
  // detach can lose focus.
  for ( treeid, orderkey ) in &invalid_skgids {
    if make_stale_a_nonMember( tree, *treeid )? {
      summary . made_nonMembers . push( orderkey . clone() );
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
  for &child_skgid in &unmanaged_skgids {
    // Move unmanaged children to end. (They will precede the managed ones.)
    move_to_last_child( tree, parent_skgid, child_skgid )?; }
  for orderkey in goal_list {
    // Move/create managed children in desired order
    match orderkey_to_treeid . get (orderkey) {
      Some (&child_skgid) => {
        move_to_last_child( tree, parent_skgid, child_skgid )?; },
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
