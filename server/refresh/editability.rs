//! Editability during the refresh: which occurrences the refresh
//! makes editable, and how a node's one editable occurrence is kept.
//!
//! Each node has at most one editable occurrence in a view. The walk
//! keeps where it stands ('Walk::editable_occurrence_of'), keyed by
//! 'Walk::editability_key': an occurrence holds its node's
//! editability while it is editable, and releases it when it is
//! write-protected, re-kinded or merged away. Before the walk
//! ('settle_before_the_walk') an editable-view request in the text
//! takes its node's editability from any other occurrence, and of
//! two editable occurrences that stand for one node the first in
//! level order keeps it.
//!
//! The node budget bounds how many occurrences the refresh makes
//! editable: each editable occurrence whose content is drawn charges
//! it once, at its visit, in either mode. It need not count exactly,
//! only stop an explosion, so an occurrence the budget could not have
//! refused (one the buffer showed editable, a node of the diff view's
//! U) is charged too.
//!
//! An occurrence the refresh draws, where its parent is editable
//! content, is drawn write-protected carrying an editable-view
//! request, and so is a write-protected child that claims membership
//! under an occurrence carrying one: the request passes on to what it
//! extends ('extend_requested_content'). At the occurrence's own
//! visit the request is consumed: the occurrence becomes editable if
//! no other occurrence of its node is, and the budget allows, and
//! stays write-protected otherwise ('consume_editableView_request').
//! In the diff view the pruned change view decides which drawn
//! occurrences may be editable: one whose node is in U, which is
//! drawn editable at once, and the budget never refuses it; and,
//! under an occurrence carrying the request, any other, as in a
//! content view. How much of an editable occurrence's goal is drawn
//! is the extent's ('extent.rs').

use crate::diff_view::pruned_change_view::prunedChangeView_sets_of_buffer;
use crate::refresh::{Walk, id_rule};
use crate::refresh::extent::children_are_editable_content;
use crate::refresh::mode::RefreshMode;
use crate::refresh::visit::children_of;
use crate::types::misc::ID;
use crate::types::nodes::rust::GraphnodeInRust;
use crate::types::viewnode::{
  Editability, ViewRequest, Viewnode, ViewnodeKind, Vognode, default_occurrence};
use crate::types::viewnode::Occurrence;

use ego_tree::{NodeId, NodeMut, NodeRef, Tree};
use std::collections::{HashMap, HashSet, VecDeque};
use std::error::Error;

impl<'a> Walk<'a> {
  /// The key of the editability of an occurrence written under
  /// 'gnid': the pid of the node 'gnid' names, else 'gnid' itself,
  /// since the ID rule points every occurrence at its node's pid. In
  /// the diff view, a gnid that named a node of HEAD's own before a
  /// nodeMerge made it an extra ID ('acquired_since_head') is its own
  /// key: the ID rule will make its occurrence that node's ghost, so
  /// it does not hold the acquirer's editability.
  pub fn editability_key (
    &self,
    gnid : &ID,
  ) -> ID {
    let pid : ID = self . truth . graph . pid_else_raw_id (gnid);
    if id_rule::acquired_since_head (gnid, &pid, self) { gnid . clone () }
    else { pid } }

  /// Let the occurrence of 'gnid' at 'treeid' hold its node's one
  /// editability, unless another occurrence already does. Whether it
  /// may.
  pub fn hold_editability (
    &mut self,
    gnid   : &ID,
    treeid : NodeId,
  ) -> bool {
    let key : ID = self . editability_key (gnid);
    *self . editable_occurrence_of . entry (key)
      . or_insert (treeid) == treeid }

  /// The occurrence of 'gnid' at 'treeid' holds its node's
  /// editability no more, if it held it.
  pub fn release_editability (
    &mut self,
    gnid   : &ID,
    treeid : NodeId,
  ) {
    let key : ID = self . editability_key (gnid);
    if self . editable_occurrence_of . get (&key) == Some (&treeid) {
      self . editable_occurrence_of . remove (&key); }}

  /// Whether some occurrence of the node 'gnid' names holds its
  /// editability.
  pub fn editability_is_held (
    &self,
    gnid : &ID,
  ) -> bool {
    self . editable_occurrence_of . contains_key (
      &self . editability_key (gnid) ) }
}

/// Before the walk: an editable-view request in the text takes its
/// node's editability ('fulfill_editableView_requests'), the first of
/// two editable occurrences of one node keeps it
/// ('keep_the_first_editable_occurrence_of_each_node'), and the walk
/// learns where each node's editable occurrence stands.
pub(super) fn settle_before_the_walk (
  tree : &mut Tree<Viewnode>,
  walk : &mut Walk,
) {
  debug_assert! ( at_most_one_editable_occurrence_per_gnid (tree),
                  "the input holds two editable occurrences of one gnid" );
  fulfill_editableView_requests (tree, walk);
  keep_the_first_editable_occurrence_of_each_node (tree, walk);
  walk . editable_occurrence_of = editable_occurrences_in (tree, walk); }

/// The input holds at most one editable occurrence per gnid:
/// validation refuses a saved buffer with two
/// ('Multiple_Defining_Viewnodes'), and every other input is the
/// server's own output.
fn at_most_one_editable_occurrence_per_gnid (
  tree : &Tree<Viewnode>,
) -> bool {
  let mut seen : HashSet<&ID> = HashSet::new ();
  tree . root () . descendants ()
    . filter_map ( |node| node . value () . editable_gnid () )
    . all ( |gnid| seen . insert (gnid) ) }

/// An editable-view request in the text makes its occurrence the
/// node's editable one, and any other editable occurrence of the node
/// write-protected, whichever of the node's IDs it is written under.
/// Done before the walk, so that an occurrence write-protected here
/// never draws content first, though it stand above the requester. (The input holds at most one such request per
/// gnid.) The request stays on the occurrence, for its own visit to
/// consume as any other ('consume_editableView_request'): its content
/// is drawn then, and the request passes on to what it extends.
fn fulfill_editableView_requests (
  tree : &mut Tree<Viewnode>,
  walk : &mut Walk,
) {
  let requested : Vec<(NodeId, ID)> =
    tree . root () . descendants ()
    . filter_map ( |node| match &node . value () . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (occurrence))
        if occurrence . view_requests . contains (&ViewRequest::Editable) =>
        Some (( node . id (), occurrence . gnid . clone () )),
      _ => None, } )
    . collect ();
  for (treeid, gnid) in requested {
    let key : ID = walk . editability_key (&gnid);
    let others : Vec<NodeId> =
      tree . root () . descendants ()
      . filter ( |node| node . id () != treeid
                        && node . value () . editable_gnid ()
                           . is_some_and ( |other| walk . editability_key (other) == key ))
      . map ( |node| node . id () )
      . collect ();
    for other in others {
      write_protect_occurrence (tree, other, walk); }
    if let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =
      &mut tree . get_mut (treeid) . unwrap () . value () . kind
    { if occurrence . is_writeProtected () {
        occurrence . editability = Editability::Editable { // the re-kind gives it its body
          body : None, edit_request : None }; }}}}

/// Two editable occurrences in the input may stand for one node when
/// a save made one's gnid an extra ID of the other's node (a
/// nodeMerge). The first in level order stays editable; the others
/// are write-protected, before any visit, so that none of them draws
/// content first. They are compared by the key of their editability
/// ('Walk::editability_key'), so that in the diff view an occurrence
/// the ID rule will make a ghost does not take the acquirer's.
fn keep_the_first_editable_occurrence_of_each_node (
  tree : &mut Tree<Viewnode>,
  walk : &mut Walk,
) {
  let mut seen : HashSet<ID> = HashSet::new ();
  let mut repeats : Vec<NodeId> = Vec::new ();
  let mut queue : VecDeque<NodeId> =
    VecDeque::from ([ tree . root () . id () ]);
  while let Some (treeid) = queue . pop_front () {
    let node : NodeRef<Viewnode> = tree . get (treeid) . unwrap ();
    if let Some (gnid) = node . value () . editable_gnid () {
      if ! seen . insert ( walk . editability_key (gnid) ) {
        repeats . push (treeid); }}
    queue . extend ( node . children () . map ( |child| child . id () )); }
  for repeat in repeats {
    write_protect_occurrence (tree, repeat, walk); }}

/// Where each node's editable occurrence stands in 'tree', by the
/// key of its editability ('Walk::editability_key'). Run after
/// 'keep_the_first_editable_occurrence_of_each_node', which leaves
/// one editable occurrence per key.
fn editable_occurrences_in (
  tree : &Tree<Viewnode>,
  walk : &Walk,
) -> HashMap<ID, NodeId> {
  tree . root () . descendants ()
    . filter_map ( |node| node . value () . editable_gnid ()
                   . map ( |gnid| ( walk . editability_key (gnid), node . id () )) )
    . collect () }

/// Make the occurrence at 'treeid' write-protected, dropping the
/// node edit request its new state has no slot for, and its
/// editable-view request, and forgetting its gnid's editable
/// occurrence.
pub(crate) fn write_protect_occurrence (
  tree   : &mut Tree<Viewnode>,
  treeid : NodeId,
  walk   : &mut Walk,
) {
  let gnid : Option<ID> =
    match &mut tree . get_mut (treeid) . unwrap () . value () . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (occurrence))
        if ! occurrence . is_writeProtected () => {
        occurrence . write_protect ();
        Some ( occurrence . gnid . clone () ) },
      ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) => {
        occurrence . write_protect (); // drops a request that would make it editable
        None },
      _ => None, };
  if let Some (gnid) = gnid {
    walk . release_editability (&gnid, treeid); }}

/// How an occurrence the refresh draws, or re-kinds to one, stands.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum DrawnEditability {
  WriteProtected,
  /// Write-protected, carrying an editable-view request, which its
  /// visit consumes ('consume_editableView_request').
  WriteProtectedRequested,
  /// Editable at once: in the diff view, a node of U drawn where no
  /// request extends.
  Editable,
}

/// How an occurrence of 'pid' the refresh draws under 'parent'
/// stands. It may be editable when its parent's children are editable
/// content, or it is a root of the view (one the ID rule makes an
/// occurrence again), and no other occurrence of the node is editable;
/// in the diff view, only when its node is in U, or the parent
/// carries an editable-view request. It is then drawn carrying the
/// request, but for a node of U under a parent that carries none,
/// which is drawn editable.
pub(crate) fn drawn_occurrence_editability (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
  pid    : &ID,
  walk   : &Walk,
) -> DrawnEditability {
  let may_be_editable : bool =
    ( children_are_editable_content (tree, parent, walk)
      || matches! ( tree . get (parent) . unwrap () . value () . kind,
                    ViewnodeKind::BufferRoot ))
    && ! walk . editability_is_held (pid);
  if ! may_be_editable { return DrawnEditability::WriteProtected; }
  let requested : bool = carries_editableView_request (tree, parent);
  match walk . mode {
    RefreshMode::ContentView => DrawnEditability::WriteProtectedRequested,
    RefreshMode::DiffView { .. } if requested => DrawnEditability::WriteProtectedRequested,
    RefreshMode::DiffView { .. } if in_the_diffViews_u (pid, walk) =>
      DrawnEditability::Editable,
    RefreshMode::DiffView { .. } => DrawnEditability::WriteProtected, } }

/// Whether the diff view's U holds 'pid'. Never in a content view.
fn in_the_diffViews_u (
  pid  : &ID,
  walk : &Walk,
) -> bool {
  match walk . mode {
    RefreshMode::ContentView => false,
    RefreshMode::DiffView { straddle, buffer_kind, .. } =>
      prunedChangeView_sets_of_buffer (straddle, buffer_kind) . u
        . contains (pid), } }

/// How an occurrence the refresh draws stands: its editability, and
/// whether it claims membership of its viewparent's list.
// TODO: The pair stands in for the 'ChildFlags' that the review of
// plan 04.1 proposed, the flags every child has (its claim and its
// warrant) beside 'FoldAndFocus', and that is not built. When it is,
// this goes.
#[derive(Clone, Copy)]
pub(crate) struct EditabilityAndClaim {
  pub editability : DrawnEditability,
  pub claims      : bool,
}

/// An occurrence of 'pid' as the truth has it: editable (with its
/// body), write-protected, or write-protected carrying an
/// editable-view request, which its visit consumes.
pub(crate) fn occurrence_from_truth (
  pid                   : &ID,
  editability_and_claim : EditabilityAndClaim,
  walk                  : &Walk,
) -> Result<Occurrence, Box<dyn Error>> {
  let node : &GraphnodeInRust = walk . truth . graph . get (pid)
    . ok_or ("occurrence_from_truth: the truth lacks the node") ?;
  let mut occurrence : Occurrence = Occurrence {
    claimsMembership : editability_and_claim . claims,
    editability      : match editability_and_claim . editability {
      DrawnEditability::Editable =>
        Editability::Editable { body : node . body . clone (), edit_request : None },
      DrawnEditability::WriteProtected | DrawnEditability::WriteProtectedRequested =>
        Editability::WriteProtected, },
    .. default_occurrence (
      pid . clone (), node . home_skgrepo . clone (), node . title . clone () ) };
  if editability_and_claim . editability == DrawnEditability::WriteProtectedRequested {
    occurrence . view_requests . insert (ViewRequest::Editable); }
  Ok (occurrence) }

/// At the visit of an occurrence, before its content is drawn: a
/// write-protected one carrying an editable-view request becomes
/// editable, with its body, if no other occurrence of its node is and
/// the budget allows (in the diff view, or its node is in U), and
/// otherwise stays write-protected. Then an editable one charges the
/// budget. The request stays until the visit ends
/// ('drop_editableView_request'), since what the visit draws asks
/// whether it extends.
pub(crate) fn consume_editableView_request (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  walk   : &mut Walk,
) {
  let (pid, requested_and_writeProtected) : (ID, bool) =
    match &tree . get (parent) . unwrap () . value () . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =>
        ( occurrence . gnid . clone (),
          occurrence . is_writeProtected ()
          && occurrence . view_requests . contains (&ViewRequest::Editable) ),
      _ => return, };
  if requested_and_writeProtected
     && ( walk . node_budget > 0 || in_the_diffViews_u (&pid, walk) )
     && walk . hold_editability (&pid, parent) {
    let body : Option<String> =
      walk . truth . graph . get (&pid)
      . and_then ( |node : &GraphnodeInRust| node . body . clone () );
    if let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =
      &mut tree . get_mut (parent) . unwrap () . value () . kind
    { occurrence . editability = Editability::Editable { body, edit_request : None }; }}
  if tree . get (parent) . unwrap () . value () . editable_gnid () . is_some () {
    walk . node_budget = walk . node_budget . saturating_sub (1); }}

/// An editable-view request extends its occurrence's content, the
/// content it shows included: each write-protected child that claims
/// membership, and that a drawn occurrence's rule would let be
/// editable ('drawn_occurrence_editability'), gets the request too,
/// which its own visit consumes, and which extends its content in
/// turn.
pub(crate) fn extend_requested_content (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  walk   : &mut Walk,
) {
  if ! carries_editableView_request (tree, parent)
     || tree . get (parent) . unwrap () . value () . editable_gnid () . is_none ()
  { return; }
  for child in children_of (tree, parent) {
    let gnid : ID = match &tree . get (child) . unwrap () . value () . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (occurrence))
        if occurrence . is_writeProtected () && occurrence . claimsMembership =>
        occurrence . gnid . clone (),
      _ => continue, };
    if drawn_occurrence_editability (tree, parent, &gnid, walk)
       != DrawnEditability::WriteProtected {
      add_editableView_request (tree, child); }}}

/// The visit of the occurrence at 'parent' is over: the editable-view
/// request it carried, consumed, goes.
pub(crate) fn drop_editableView_request (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
) {
  if let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =
    &mut tree . get_mut (parent) . unwrap () . value () . kind
  { occurrence . view_requests . remove (&ViewRequest::Editable); }}


/// Whether the occurrence at 'treeid' carries an editable-view
/// request. During the walk the request stands for this refresh's
/// first extension of the occurrence, whoever asked for it: the user,
/// a request passing on to what it extends, or the drawing of an
/// occurrence that may be editable. It is consumed at the
/// occurrence's own visit ('consume_editableView_request') and
/// dropped when that visit ends.
pub(crate) fn carries_editableView_request (
  tree   : &Tree<Viewnode>,
  treeid : NodeId,
) -> bool {
  matches! ( &tree . get (treeid) . unwrap () . value () . kind,
             ViewnodeKind::Vognode (Vognode::Occurrence (occurrence))
               if occurrence . view_requests . contains (&ViewRequest::Editable) ) }

/// Give the occurrence at 'treeid' an editable-view request.
pub(crate) fn add_editableView_request (
  tree   : &mut Tree<Viewnode>,
  treeid : NodeId,
) {
  if let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =
    &mut tree . get_mut (treeid) . unwrap () . value () . kind
  { occurrence . view_requests . insert (ViewRequest::Editable); }}

/// The merged sibling's editability, and its edit request with it,
/// passes to the result, unless the result stands for another node (a
/// node and its shown overrider stand for one gnid) whose editable
/// occurrence is elsewhere in the buffer.
pub(crate) fn merge_editability (
  tree             : &mut Tree<Viewnode>,
  result           : NodeId,
  merged_sibling   : NodeId,
  sibling_viewnode : &Viewnode,
  walk             : &mut Walk,
) {
  let ViewnodeKind::Vognode (Vognode::Occurrence (merged)) : &ViewnodeKind =
    &sibling_viewnode . kind else { return; };
  if merged . is_writeProtected () { return; }
  walk . release_editability (&merged . gnid, merged_sibling);
  let mut result_mut : NodeMut<Viewnode> = tree . get_mut (result) . unwrap ();
  let ViewnodeKind::Vognode (Vognode::Occurrence (result_occurrence)) : &mut ViewnodeKind =
    &mut result_mut . value () . kind
    else { return; };
  if result_occurrence . is_writeProtected ()
     && ! walk . editability_is_held (&result_occurrence . gnid) {
    result_occurrence . editability = merged . editability . clone (); }
  if ! result_occurrence . is_writeProtected () {
    walk . hold_editability (&result_occurrence . gnid, result); }}

/// When the vognode at 'treeid' was an editable occurrence, its node
/// has no editable occurrence now.
pub(crate) fn forget_editable_occurrence (
  vognode : &Vognode,
  treeid  : NodeId,
  walk    : &mut Walk,
) {
  if let Vognode::Occurrence (occurrence) = vognode {
    if ! occurrence . is_writeProtected () {
      walk . release_editability (&occurrence . gnid, treeid); }}}
