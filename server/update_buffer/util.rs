use crate::to_org::util::make_member_children_nonMembers;
use crate::types::viewnode::{
  ChangeFacts, Viewnode, ViewnodeKind, Vognode};
use crate::types::tree::generic::{with_node_mut, write_at_ancestor_in_tree};
use crate::types::tree::moves::{
  move_to_last_child, place_created_children_after_their_model_predecessors};

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

/// A new hiddenInSubscribee or
/// hiddenOutsideOfSubscribee folder begins folded. Creation sites stamp
/// 'folded' on the newborn folder; this runs at the folder's own BFS visit
/// (its members now reconciled in) and transfers the stamp: every
/// child is marked folded and the folder's own flag is cleared. That
/// encoding is the client's (elisp/skg-org-fold.el): 'folded' on a
/// headline means it is hidden inside its folded parent, so a folder's
/// collapse lives on its members -- left on the folder itself it would
/// instead fold the folder's parent. A folder parsed from a buffer carries
/// 'folded' only when it sat inside a folded ancestor; its members
/// then already carry the mark, so the transfer changes nothing.
pub fn fold_members_of_newborn_folder (
  tree : &mut Tree<Viewnode>,
  folder  : NodeId,
) -> Result<(), String> {
  let newborn : bool =
    tree . get (folder)
    . ok_or ("fold_members_of_newborn_folder: node not found") ?
    . value () . foldAndFocus . folded;
  if ! newborn { return Ok (( )); }
  with_node_mut ( tree, folder,
                  |mut n| { n . value () . foldAndFocus . folded = false; } )
    . map_err ( |e| -> String { e . into () } ) ?;
  treat_certain_children (
    tree, folder,
    |_vn : &Viewnode| true,
    |vn : &mut Viewnode| { vn . foldAndFocus . folded = true; } ) }

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
    subtree_satisfies ( tree, node, &|n : &Viewnode| n . foldAndFocus . focused ) ?;
  if has_focus {
    write_at_ancestor_in_tree (
      tree, node, 1,
      |vn : &mut Viewnode| { vn . foldAndFocus . focused = true; } )
      . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?; }
  with_node_mut ( tree, node,
                  |mut n| { n . detach (); } )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  Ok (()) }

/// What 'complete_managed_children' changed while reconciling: the
/// orderkeys it created, those of the stale or duplicate children it
/// retained as non-members or entombstoned holding something, and
/// those of the stale and duplicate children it made childless
/// tombstones, which the prune that ends the completion removes. Callers that warn about repairs to
/// write-protected folders consume this ('CompletionWarning'); other
/// callers ignore it.
#[derive(Debug)]
pub struct RepairSummary<Orderkey> {
  pub created            : Vec<Orderkey>,
  pub retained    : Vec<Orderkey>,
  pub entombstoned_stale      : Vec<Orderkey>,
  pub entombstoned_duplicates : Vec<Orderkey>,
}

impl<Orderkey> RepairSummary<Orderkey> {
  fn new () -> RepairSummary<Orderkey> {
    RepairSummary {
      created            : Vec::new (),
      retained    : Vec::new (),
      entombstoned_stale      : Vec::new (),
      entombstoned_duplicates : Vec::new (), }}
}

/// What 'convert_unwarranted_child' made of a child.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Conversion {
  /// A tombstone with no children, which the prune removes.
  ChildlessTombstone,
  /// A vognode retained as non-member, or a tombstone holding
  /// children: either stays in the buffer.
  Retained,
}

/// Why a reconciler converts a child ('convert_unwarranted_child').
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Unwarranted {
  /// The reconciler's model lacks the child's key.
  Stale,
  /// The child is an extra copy of a key a sibling holds.
  Duplicate,
}

/// Whether 'complete_managed_children' moves the children it keeps.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum KeptChildOrder {
  /// The rule both reconcilers obey ('docs/diff-view.org', "What a
  /// reconciler keeps"): every child it keeps stays where it stands,
  /// and so does every child it does not manage; a child it creates
  /// goes after the managed child that precedes it in the goal list,
  /// or first when none does.
  Unmoved,
  /// For a list whose order is a graph fact the save writes: the
  /// content of a node and the folders whose membership the save
  /// reads (the folders whose goal is ordered,
  /// 'Folder::goal_is_ordered'). The unmanaged children go first, then the
  /// managed ones in the goal list's order, so that a view other than
  /// the one saved follows a reorder saved elsewhere. In the view just
  /// saved the two orders agree, since the save wrote the buffer's.
  FollowsGoal,
}

/// Retain as non-member or tombstone, then prune ('docs/glossary.org',
/// "retain as non-member or tombstone, then prune"): a child the graph
/// no longer warrants (a stale child, or an extra copy of a duplicate)
/// is converted, never detached. An unrestricted vognode with a
/// subtree ('is_leaf' false) is retained as non-member: it keeps its
/// title and ID, and loses its change facts and readiness request. A stale one keeps its
/// editability: an editable one keeps its members, while a
/// write-protected one's members become non-members, since a
/// write-protected occurrence holds no member. An extra copy of a
/// duplicate is write-protected whatever it was, its members made
/// non-members, since the save refuses two editable occurrences of
/// one node. Any other child -- a leaf, a non-vognode, a phantom or a
/// restricted vognode -- is entombstoned, and the prune that ends the
/// completion removes the tombstone once it is childless
/// ('prune_self_deletable_when_empty'), so that whatever stands below
/// it survives.
pub fn convert_unwarranted_child (
  tree        : &mut Tree<Viewnode>,
  child       : NodeId,
  is_leaf     : bool,
  unwarranted : Unwarranted,
) -> Result<Conversion, Box<dyn Error>> {
  let retained_writeProtected : Option<bool> = {
    // Some (whether it is write-protected) when retained as non-member.
    let mut node_mut : NodeMut<Viewnode> = tree . get_mut (child)
      . ok_or ("convert_unwarranted_child: node not found") ?;
    let viewnode : &mut Viewnode = node_mut . value ();
    match &mut viewnode . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t)) if ! is_leaf => {
        t . claimsMembership = false;
        if unwarranted == Unwarranted::Duplicate {
          t . write_protect (); }
        let writeProtected : bool = t . is_writeProtected ();
        viewnode . change_facts      = ChangeFacts::default ();
        viewnode . readiness_request = None;
        Some (writeProtected) },
      _ => { viewnode . replace_kind (ViewnodeKind::Tombstone);
             None }, }};
  if let Some (writeProtected) = retained_writeProtected {
    if writeProtected {
      make_member_children_nonMembers (tree, child) ?; }
    return Ok (Conversion::Retained); }
  let has_children : bool =
    tree . get (child) . unwrap () . children () . next () . is_some ();
  Ok ( if has_children { Conversion::Retained } else { Conversion::ChildlessTombstone } ) }

/// Viewnode-specialized version of complete_managed_children.
/// See that one's description for more info.
/// This one specializes it so that:
/// - of several managed children with one key, the editable one is
///   kept as the member, else the first;
/// - a stale or duplicate child is converted by
///   'convert_unwarranted_child', as a leaf when it has no children.
pub fn complete_managed_children_in_viewforest
<Orderkey, Managed, View> (
  tree                : &mut Tree<Viewnode>,
  treeid              : NodeId,
  kept_child_order    : KeptChildOrder,
  managed             : Managed,
  view_child_orderkey : View,
  goal_list           : &[Orderkey],
  create_child        : impl Fn (&Orderkey) -> Result<Viewnode, String>,
) -> Result<RepairSummary<Orderkey>, Box<dyn Error>>
where Managed  : Fn (&Viewnode) -> bool,
      View     : Fn (&Viewnode) -> Result<Orderkey, String>,
      Orderkey : Eq + Hash + Clone,
{
  let summary : RepairSummary<Orderkey> =
    complete_managed_children (
      tree, treeid, kept_child_order, &managed, &view_child_orderkey,
      |viewnode : &Viewnode| viewnode . editable_skgid () . is_some (),
      goal_list,
      |orderkey : &Orderkey| create_child (orderkey),
      |tree : &mut Tree<Viewnode>, child : NodeId, unwarranted : Unwarranted| {
        let is_leaf : bool = tree . get (child)
          . ok_or ("complete_managed_children_in_viewforest: child not found") ?
          . children () . next () . is_none ();
        convert_unwarranted_child (tree, child, is_leaf, unwarranted) } ) ?;
  Ok (summary) }

/// PURPOSE:
/// Reconciles a parent's "managed" children against a desired list of 'orderkeys'.
/// The payload of each tree node should be some possibly-improper supertype of 'orderkey' -- that is,
/// there is a way to extract the orderkey from a node, but the node might have other information.
/// .
/// STEPS:
/// - Partitions children into "managed" (matching a predicate) and "unmanaged".
///   It is not necessary that unmanaged children be able to provide an orderkey.
/// - Among managed children, identifies duplicates and "stale" members (those
///   whose orderkey is not in goal_list). Of several managed children
///   with one orderkey, the first that 'preferred_copy' selects is
///   kept, else the first; the others are duplicates.
/// - Hands every duplicate and every stale child to
///   'convert_unwarranted', saying which it is, and detaches nothing.
/// - Creates new children for any orderkeys missing from the original
///   children, placed as 'kept_child_order' says: with
///   'KeptChildOrder::Unmoved', each after the managed child that precedes
///   it in goal_list (or first), every other child staying where it
///   stands; with 'KeptChildOrder::FollowsGoal', the children are
///   reordered: first every child that is not a kept member (the
///   unmanaged and the converted ones, in their order), then the
///   members in goal_list order.
/// - Returns a RepairSummary: which children it created, which it
///   retained (as non-members, or as tombstones holding something),
///   and which it made childless tombstones.
pub fn complete_managed_children
<Node, Orderkey, Managed, View, PreferredCopy, ConvertUnwarranted> (
  tree                : &mut Tree<Node>,
  parent_skgid        : NodeId,
  kept_child_order    : KeptChildOrder,
  managed             : Managed,
  view_child_orderkey : View,
  preferred_copy      : PreferredCopy,
  goal_list           : &[Orderkey],
  create_child        : impl Fn (&Orderkey) -> Result<Node, String>,
  convert_unwarranted : ConvertUnwarranted,
) -> Result<RepairSummary<Orderkey>, Box<dyn Error>>
where
  Managed         : Fn (&Node) -> bool,
  // The orderkey and create-child closures are fallible so a managed
  // child of an unexpected kind, or an unprefetched orderkey, surfaces
  // as an Err from this function rather than a panic in a closure.
  View            : Fn (&Node) -> Result<Orderkey, String>,
  Orderkey        : Eq + Hash + Clone,
  PreferredCopy   : Fn (&Node) -> bool,
  ConvertUnwarranted : Fn (&mut Tree<Node>, NodeId, Unwarranted) -> Result<Conversion, Box<dyn Error>>,
{
  let managed_skgids : Vec<NodeId> =
    tree . get (parent_skgid)
    . ok_or ("complete_managed_children: node not found") ?
    . children ()
    . filter ( |child| managed ( child . value () ))
    . map ( |child| child . id () )
    . collect ();
  let goal_list_as_set : HashSet<Orderkey> =
    goal_list . iter() . cloned() . collect();
  let mut copies_by_orderkey : HashMap<Orderkey, Vec<NodeId>> = HashMap::new ();
  let mut invalid_skgids : Vec<( NodeId, Orderkey )> = Vec::new();
  for &treeid in &managed_skgids { // populate the above two variables
    let orderkey : Orderkey =
      view_child_orderkey ( tree . get (treeid) . unwrap () . value () ) ?;
    if !goal_list_as_set . contains (&orderkey) {
      invalid_skgids . push( ( treeid, orderkey ) ); // hopefully none
    } else {
      copies_by_orderkey . entry (orderkey) . or_default () . push (treeid); }}
  let mut orderkey_to_treeid : HashMap<Orderkey, NodeId> = HashMap::new ();
  let mut duplicate_skgids : Vec<( NodeId, Orderkey )> = Vec::new ();
  for orderkey in goal_list { // keep one copy of each, in goal order
    let Some (copies) = copies_by_orderkey . remove (orderkey)
      else { continue; };
    let kept : NodeId =
      copies . iter ()
      . find ( |copy| preferred_copy ( tree . get (**copy) . unwrap () . value () ))
      . copied ()
      . unwrap_or ( copies [0] );
    orderkey_to_treeid . insert ( orderkey . clone (), kept );
    for copy in copies {
      if copy != kept {
        duplicate_skgids . push ( ( copy, orderkey . clone () )); }}}
  let mut summary : RepairSummary<Orderkey> = RepairSummary::new();
  for ( treeid, orderkey ) in duplicate_skgids {
    match convert_unwarranted ( tree, treeid, Unwarranted::Duplicate ) ? {
      Conversion::ChildlessTombstone => summary . entombstoned_duplicates . push (orderkey),
      Conversion::Retained   => summary . retained    . push (orderkey), }}
  for ( treeid, orderkey ) in invalid_skgids {
    match convert_unwarranted ( tree, treeid, Unwarranted::Stale ) ? {
      Conversion::ChildlessTombstone => summary . entombstoned_stale   . push (orderkey),
      Conversion::Retained   => summary . retained . push (orderkey), }}
  let kept_members : HashSet<NodeId> =
    orderkey_to_treeid . values () . copied () . collect ();
  let other_children : Vec<NodeId> = // the unmanaged and the converted
    tree . get (parent_skgid) . unwrap () . children ()
    . map ( |child| child . id () )
    . filter ( |child| ! kept_members . contains (child) )
    . collect ();
  let mut in_goal_order : Vec<NodeId> = Vec::new ();
  let mut created : HashSet<NodeId> = HashSet::new ();
  for orderkey in goal_list { // create the missing children, at the end
    match orderkey_to_treeid . get (orderkey) {
      Some (&child_skgid) => in_goal_order . push (child_skgid),
      None => {
        let node : Node = create_child (orderkey) ?;
        let child_skgid : NodeId = tree . get_mut (parent_skgid)
          . ok_or ("complete_managed_children: node not found") ?
          . append (node) . id ();
        created . insert (child_skgid);
        in_goal_order . push (child_skgid);
        summary . created . push( orderkey . clone() ); }, }}
  match kept_child_order {
    KeptChildOrder::Unmoved =>
      place_created_children_after_their_model_predecessors (
        tree, parent_skgid, &in_goal_order, &created ) ?,
    KeptChildOrder::FollowsGoal => {
      for &child_skgid in other_children . iter () . chain (&in_goal_order) {
        // The other children, then the members in goal order.
        move_to_last_child( tree, parent_skgid, child_skgid )?; }}, }
  Ok (summary) }

#[cfg(test)]
#[path = "../../tests/unit/update_buffer_util.rs"]
mod tests;
