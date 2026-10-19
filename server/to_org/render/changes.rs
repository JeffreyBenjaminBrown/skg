//! The total diff view's step at each unrestricted vognode's visit
//! in view completion ('TODO/total-diff-view/2_plan.org', "The
//! PCSVF" and "What each kind of change looks like").
//!
//! On an editable occurrence: the content members (which the content
//! reconciler drew from the after-graph) are tagged with the presence
//! of their node and of their relationship, members outside U become
//! write-protected leaves, and the removed members are inserted where
//! 'phantom_insertion_plan' places them. A node in C also gets, ahead
//! of its content, its old text, its home move, its "it acquired"
//! folder, the alias, ID and flags folders that changed, the partner
//! folders whose membership changed (which the BFS fills from the
//! straddle), and its changeset markers. A node in U but not in C is
//! context: its body is folded. A write-protected occurrence gets
//! none of this; its viewparent tags it. U and C are those of the
//! buffer being drawn ('TdvDrawing::sets'); in the order-only buffer
//! both are empty, and each node there gets an order-only marker.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::{
  BinaryRolePosition, NodeRelation, RelationRole};
use crate::graph_straddle::types::{GraphStraddle, NodeChanges, Partner, Presence};
use crate::pcsvf::{PcsvfSets, TdvDrawing};
use crate::to_org::render::change_viewnodes::{
  append_acquired_folder, append_historical_phantom, append_old_text,
  mk_changeset_marker_viewnode, mk_home_move_viewnode,
  mk_relRepo_move_viewnode};
use crate::types::git::Sign;
use crate::types::list::{Diff_Item, compute_interleaved_diff};
use crate::types::misc::{ID, members_msv};
use crate::types::nodes::rust::GraphnodeInRust;
use crate::types::nodes::complete::Flag;
use crate::types::views_state::TdvBuffer;
use crate::types::viewnode::{
  AffectsParent, ChangeFacts, ChangesetMarkerKind, DiffPresence,
  Editability, PartnerFolder, Phantom, Property, PropertyFolder,
  ViewRequest, Viewnode, ViewnodeKind, Vognode, mk_writeProtected_viewnode};

use ego_tree::{NodeId, Tree};
use std::collections::{BTreeSet, HashMap, HashSet};
use std::error::Error;
use std::hash::Hash;

const CONTENT : RelationRole =
  RelationRole { relation : NodeRelation::Contains,
                 position : BinaryRolePosition::Second };

/// The folders of a node's recorders (its in-set changed when one
/// was added or removed), then those of the relationships it records
/// (changed when one was added, removed or moved to another skgrepo).
const IN_FOLDERS : [PartnerFolder; 5] = [
  PartnerFolder::Container, PartnerFolder::Mentioner,
  PartnerFolder::Subscriber, PartnerFolder::Overrider,
  PartnerFolder::Hider ];
const OUT_FOLDERS : [PartnerFolder; 3] = [
  PartnerFolder::Subscribee, PartnerFolder::Overridden,
  PartnerFolder::Hidden ];

/// See the module comment.
pub(crate) fn process_unrestrictedVognode_changes (
  tree    : &mut Tree<Viewnode>,
  treeid  : NodeId,
  drawing : TdvDrawing,
) -> Result<(), Box<dyn Error>> {
  let straddle : &GraphStraddle = drawing . straddle;
  let (pid, editable) : (ID, bool) =
    match &tree . get (treeid)
      . ok_or ("process_unrestrictedVognode_changes: node not found") ?
      . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
        ( t . skgid . clone (), ! t . is_writeProtected () ),
      _ => return Ok (( )), };
  if ! editable { return Ok (( )); }
  let sets : &PcsvfSets = drawing . sets ();
  remove_change_viewnodes (tree, treeid);
  tree . get_mut (treeid) . unwrap () . value () . body_folded =
    ! sets . c . contains (&pid);
  let content : Vec<Partner> = straddle . partners (&pid, CONTENT);
  tag_content_members (tree, treeid, straddle, &content, &sets . u);
  insert_removed_content_members (tree, treeid, straddle, &pid) ?;
  if drawing . tdv_buffer == TdvBuffer::OrderOnlyChanges
    && straddle . order_only_pids () . contains (&pid) {
    let marker : NodeId = append ( tree, treeid,
      mk_changeset_marker_viewnode (ChangesetMarkerKind::OrderOnly) );
    move_to_front (tree, treeid, marker); }
  if ! sets . c . contains (&pid) { return Ok (( )); }
  let mut heads : Vec<NodeId> = Vec::new ();
  heads . extend ( append_old_text (tree, treeid, straddle, &pid) );
  if let Some (home_move) = mk_home_move_viewnode (straddle, &pid) {
    heads . push ( append (tree, treeid, home_move) ); }
  heads . extend ( append_acquired_folder (tree, treeid, straddle, &pid) );
  heads . extend ( changed_property_folders (tree, treeid, straddle, &pid) );
  for kind in IN_FOLDERS . into_iter () . chain (OUT_FOLDERS) {
    let role : RelationRole = kind . relation_member_role ()
      . expect ("a relation folder has a member role");
    let outbound : bool = OUT_FOLDERS . contains (&kind);
    if straddle . partners (&pid, role) . iter () . any ( |partner|
      if outbound { partner . changed () }
      else { partner . presence != Presence::Both } ) {
      heads . push ( child_folder_or_new (
        tree, treeid, ViewnodeKind::PartnerFolder (kind) )); }}
  if content . iter () . any ( |partner| partner . presence != Presence::Both ) {
    heads . push ( append ( tree, treeid,
      mk_changeset_marker_viewnode (ChangesetMarkerKind::Content) )); }
  if content . iter () . any ( |partner| partner . relRepo_moved () ) {
    heads . push ( append ( tree, treeid,
      mk_changeset_marker_viewnode (ChangesetMarkerKind::RelRepo) )); }
  for head in heads . iter () . rev () {
    move_to_front (tree, treeid, *head); }
  Ok (( )) }

/// After view completion over the straddle: draw a node that a save
/// deleted, and that HEAD had, as the PCSVF draws a deleted node.
/// Completion turned each of its occurrences into a deleted phantom.
/// One that stands beside a historical phantom of the same node (its
/// viewparent drew the removed member) goes; any other becomes a
/// historical phantom with its old content.
pub(crate) fn redraw_deleted_phantoms_as_historical (
  tree     : &mut Tree<Viewnode>,
  straddle : &GraphStraddle,
) {
  let deleted : Vec<(NodeId, ID)> =
    tree . root () . descendants ()
    . filter_map ( |node| match &node . value () . kind {
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Deleted (d)))
        if straddle . node_presence (&d . skgid) == Some (Presence::OnlyBefore) =>
        Some (( node . id (), d . skgid . clone () )),
      _ => None, } )
    . collect ();
  for (treeid, skgid) in deleted {
    let Some (parent) : Option<NodeId> =
      tree . get (treeid) . unwrap () . parent () . map ( |p| p . id () )
      else { continue; };
    let drawn_beside : bool =
      tree . get (parent) . unwrap () . children ()
      . any ( |sibling| matches! ( &sibling . value () . kind,
        ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (h)))
          if h . skgid == skgid ));
    if ! drawn_beside {
      if let Some (historical) =
        append_historical_phantom (tree, parent, straddle, &skgid, None) {
        tree . get_mut (historical) . unwrap () . detach (); // see 'move_to_front'
        tree . get_mut (treeid) . unwrap () . insert_id_before (historical); }}
    tree . get_mut (treeid) . unwrap () . detach (); }}

/// Make 'child' the first child of 'treeid'.
/// PITFALL: ego_tree's 'prepend_id' and 'insert_id_before' read the
/// current first child (or previous sibling) before detaching the
/// node they move, so moving a node that is already there links it
/// to itself. Detaching first avoids that.
fn move_to_front (
  tree   : &mut Tree<Viewnode>,
  treeid : NodeId,
  child  : NodeId,
) {
  tree . get_mut (child) . unwrap () . detach ();
  tree . get_mut (treeid) . unwrap () . prepend_id (child); }

/// Detach what an earlier visit of this step drew under the node, so
/// that drawing it again is idempotent. Folders are reused, not
/// detached: one the user requested may be among them.
fn remove_change_viewnodes (
  tree   : &mut Tree<Viewnode>,
  treeid : NodeId,
) {
  let stale : Vec<NodeId> =
    tree . get (treeid) . unwrap () . children ()
    . filter ( |child| match &child . value () . kind {
      ViewnodeKind::Property (
        Property::OldText { .. } | Property::HomeSkgrepoMove { .. }
        | Property::ChangesetMarker { .. } ) => true,
      ViewnodeKind::PartnerFolder (PartnerFolder::Acquired) => true,
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (_))) => true,
      ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
        is_removed_member (t . affectsParent, &t . change_facts),
      _ => false, } )
    . map ( |child| child . id () )
    . collect ();
  for treeid in stale {
    tree . get_mut (treeid) . unwrap () . detach (); }}

/// A member this step drew because its relationship to its viewparent
/// is only in the before-graph.
fn is_removed_member (
  affects_parent : AffectsParent,
  change_facts   : &ChangeFacts,
) -> bool {
  affects_parent == AffectsParent::False
    && change_facts . presence . rel == Some (Sign::Minus) }

/// Tag each content member with the presence of its node and of its
/// relationship, draw a member outside U as a write-protected leaf,
/// and hang a relRepo move under a member whose relationship moved.
fn tag_content_members (
  tree     : &mut Tree<Viewnode>,
  treeid   : NodeId,
  straddle : &GraphStraddle,
  content  : &[Partner],
  u        : &BTreeSet<ID>,
) {
  let by_id : HashMap<&ID, &Partner> =
    content . iter () . map ( |partner| (&partner . id, partner) ) . collect ();
  let members : Vec<NodeId> =
    tree . get (treeid) . unwrap () . children ()
    . map ( |child| child . id () ) . collect ();
  for member in members {
    let mut moved : Option<Viewnode> = None;
    { let mut member_mut = tree . get_mut (member) . unwrap ();
      match &mut member_mut . value () . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (t))
          if t . affectsParent == AffectsParent::True => {
          let partner : Option<&&Partner> = by_id . get (&t . skgid);
          t . change_facts . presence = DiffPresence {
            node : straddle . node_presence (&t . skgid)
                   . and_then ( |presence| presence . diff_sign () ),
            rel  : partner . and_then ( |p| p . presence . diff_sign () ), };
          if ! u . contains (&t . skgid) {
            t . view_requests . remove (&ViewRequest::Editable);
            t . editability = Editability::WriteProtected; }
          moved = partner . and_then ( |p| mk_relRepo_move_viewnode (p) ); },
        ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (unknown))) => {
          unknown . change_facts . presence . rel =
            by_id . get (&unknown . skgid)
            . and_then ( |p| p . presence . diff_sign () ); },
        _ => continue, }}
    let stale_moves : Vec<NodeId> =
      tree . get (member) . unwrap () . children ()
      . filter ( |child| matches! ( &child . value () . kind,
        ViewnodeKind::Property (Property::RelRepoMove { .. }) ))
      . map ( |child| child . id () ) . collect ();
    for stale in stale_moves {
      tree . get_mut (stale) . unwrap () . detach (); }
    if let Some (moved) = moved {
      tree . get_mut (member) . unwrap () . prepend (moved); }}}

/// Insert each member of the before-graph's content list that the
/// after-graph's lacks, immediately before the next surviving member
/// of the old order: a deleted node as a historical phantom with its
/// old content, a surviving one as a write-protected vognode that
/// does not affect its parent. Membership is by set difference; the
/// LCS only positions, so a reorder shows nothing. A removed dangling
/// ID is not drawn: an unknown phantom here would be read back as a
/// member. (Its leaf is under the content changeset marker.)
fn insert_removed_content_members (
  tree     : &mut Tree<Viewnode>,
  treeid   : NodeId,
  straddle : &GraphStraddle,
  pid      : &ID,
) -> Result<(), Box<dyn Error>> {
  let after_list : Vec<ID> = content_list (&straddle . after, pid);
  let diff : Vec<Diff_Item<ID>> =
    removed_by_set_difference (
      &content_list (&straddle . before, pid), &after_list );
  let plan : Vec<(ID, Option<ID>)> = phantom_insertion_plan (&diff);
  if plan . is_empty () { return Ok (( )); }
  let member_treeids : HashMap<ID, NodeId> =
    tree . get (treeid) . unwrap () . children ()
    . filter_map ( |child| match &child . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t))
        if t . affectsParent == AffectsParent::True =>
        Some (( t . skgid . clone (), child . id () )),
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (u))) =>
        Some (( u . skgid . clone (), child . id () )),
      _ => None, } )
    . collect ();
  for (removed, anchor) in plan {
    let inserted : Option<NodeId> = match straddle . node_presence (&removed) {
      Some (Presence::OnlyBefore) =>
        append_historical_phantom (
          tree, treeid, straddle, &removed, Some (Sign::Minus) ),
      Some (presence) => {
        let node = straddle . after . get (&removed)
          . ok_or ("insert_removed_content_members: a surviving member is not in the after-graph") ?;
        let mut member : Viewnode = mk_writeProtected_viewnode (
          removed . clone (), node . home_skgrepo . clone (),
          node . title . clone (), AffectsParent::False );
        if let ViewnodeKind::Vognode (Vognode::Unrestricted (t)) = &mut member . kind {
          t . change_facts . presence = DiffPresence {
            node : presence . diff_sign (), rel : Some (Sign::Minus) }; }
        Some ( append (tree, treeid, member) ) },
      None => None, };
    if let (Some (inserted), Some (anchor_treeid)) =
      ( inserted, anchor . and_then ( |a| member_treeids . get (&a) . copied () )) {
      tree . get_mut (inserted) . unwrap () . detach (); // see 'move_to_front'
      tree . get_mut (anchor_treeid) . unwrap () . insert_id_before (inserted); }}
  Ok (( )) }

/// Decide where each removed-member phantom belongs among its surviving
/// siblings: insert it immediately before the *next* surviving child in
/// net HEAD->worktree order, or append it (anchor = None) when no surviving
/// child follows. Returns (removed_id, anchor_id) pairs in the order the
/// phantoms should be created, so consecutive removals before the same
/// survivor keep their relative order.
pub(crate) fn phantom_insertion_plan (
  net_contains : &[Diff_Item<ID>],
) -> Vec<(ID, Option<ID>)> {
  let mut plan : Vec<(ID, Option<ID>)> = Vec::new ();
  let mut next_survivor : Option<ID> = None;
  for item in net_contains . iter () . rev () {
    match item {
      Diff_Item::Unchanged (skgid) | Diff_Item::New (skgid)
        => { next_survivor = Some ( skgid . clone () ); },
      Diff_Item::Removed (skgid)
        => { plan . push ( ( skgid . clone (), next_survivor . clone () )); }, }}
  plan . reverse ();
  plan }

/// A node's content list in one graph, each member as that graph
/// resolves it.
fn content_list (
  graph : &InRustGraph,
  pid   : &ID,
) -> Vec<ID> {
  graph . outbound_rel_partners_for_relation_gated (
    pid, NodeRelation::Contains, None )
    . into_iter ()
    . map ( |member| graph . pid_of (&member . member)
            . unwrap_or (member . member) )
    . collect () }

/// The interleaved diff of two lists, minus the 'Removed' items that
/// the new list still holds (elsewhere: a reorder).
fn removed_by_set_difference<T : Clone + Eq + Hash + Ord> (
  old : &[T],
  new : &[T],
) -> Vec<Diff_Item<T>> {
  let new_set : HashSet<&T> = new . iter () . collect ();
  compute_interleaved_diff (old, new) . into_iter ()
    . filter ( |item| ! matches! ( item,
      Diff_Item::Removed (x) if new_set . contains (x) ))
    . collect () }

/// The node's alias, ID and flags folders, those whose list changed,
/// each holding the union of the list: the old order where the LCS
/// puts a removed entry, + on a gained entry, - on a lost one.
fn changed_property_folders (
  tree     : &mut Tree<Viewnode>,
  treeid   : NodeId,
  straddle : &GraphStraddle,
  pid      : &ID,
) -> Vec<NodeId> {
  let changes : NodeChanges = straddle . node_changes (pid);
  let (Some (before), Some (after)) =
    ( straddle . before . get (pid), straddle . after . get (pid) ) else {
      return Vec::new (); };
  let mut folders : Vec<NodeId> = Vec::new ();
  if ! changes . aliases_gained . is_empty () || ! changes . aliases_lost . is_empty () {
    let aliases = |node : &GraphnodeInRust| -> Vec<String> {
      members_msv (&node . aliases) . or_default () . to_vec () };
    folders . push ( fill_property_folder (
      tree, treeid, PropertyFolder::Alias,
      signed_union ( &aliases (before), &aliases (after) ) . into_iter ()
      . map ( |(text, sign)| Property::Alias {
        text, relRepo : None, relRepo_request : None,
        change_facts : facts_of_entry (sign), readiness_request : None } )
      . collect () )); }
  if ! changes . extra_ids_gained . is_empty () || ! changes . extra_ids_lost . is_empty () {
    let ids = |node : &GraphnodeInRust| -> Vec<ID> {
      std::iter::once ( pid . clone () )
        . chain ( node . extra_ids . iter () . cloned () ) . collect () };
    folders . push ( fill_property_folder (
      tree, treeid, PropertyFolder::ID,
      signed_union ( &ids (before), &ids (after) ) . into_iter ()
      . map ( |(skgid, sign)| Property::ID {
        skgid,
        change_facts : facts_of_entry (sign), readiness_request : None } )
      . collect () )); }
  if ! changes . flags_gained . is_empty () || ! changes . flags_lost . is_empty () {
    folders . push ( fill_property_folder (
      tree, treeid, PropertyFolder::flags (),
      Flag::ALL . into_iter ()
      . filter_map ( |flag| {
        let sign : Option<Sign> =
          if changes . flags_gained . contains (&flag) { Some (Sign::Plus) }
          else if changes . flags_lost . contains (&flag) { Some (Sign::Minus) }
          else if after . flags . contains (&flag) { None }
          else { return None; };
        Some ( Property::Flag {
          flag, title : String::new (), body : None,
          change_facts : facts_of_entry (sign), readiness_request : None } ) } )
      . collect () )); }
  folders }

/// The entries of the union of two lists, in the order of their
/// interleaved diff, each with + if only the new list holds it and
/// - if only the old one does.
fn signed_union<T : Clone + Eq + Hash + Ord> (
  old : &[T],
  new : &[T],
) -> Vec<(T, Option<Sign>)> {
  let old_set : HashSet<&T> = old . iter () . collect ();
  removed_by_set_difference (old, new) . into_iter ()
    . map ( |item| match item {
      Diff_Item::Removed (x) => (x, Some (Sign::Minus)),
      Diff_Item::Unchanged (x) | Diff_Item::New (x) => {
        let sign : Option<Sign> =
          ( ! old_set . contains (&x) ) . then_some (Sign::Plus);
        (x, sign) }, } )
    . collect () }

fn facts_of_entry (
  sign : Option<Sign>,
) -> ChangeFacts {
  ChangeFacts { presence : DiffPresence { node : None, rel : sign },
                ready    : None } }

/// The node's folder 'kind', created if absent, with exactly
/// 'entries' as its children.
fn fill_property_folder (
  tree    : &mut Tree<Viewnode>,
  treeid  : NodeId,
  kind    : PropertyFolder,
  entries : Vec<Property>,
) -> NodeId {
  let folder : NodeId =
    child_folder_or_new ( tree, treeid, ViewnodeKind::PropertyFolder (kind) );
  let old_entries : Vec<NodeId> =
    tree . get (folder) . unwrap () . children ()
    . map ( |child| child . id () ) . collect ();
  for entry in old_entries {
    tree . get_mut (entry) . unwrap () . detach (); }
  for entry in entries {
    tree . get_mut (folder) . unwrap () . append ( viewnode_of (
      ViewnodeKind::Property (entry) )); }
  folder }

/// The node's child of this folder kind, or a new one appended.
fn child_folder_or_new (
  tree   : &mut Tree<Viewnode>,
  treeid : NodeId,
  kind   : ViewnodeKind,
) -> NodeId {
  let same_kind = |other : &ViewnodeKind| -> bool {
    match (&kind, other) {
      ( ViewnodeKind::PropertyFolder (a), ViewnodeKind::PropertyFolder (b) ) =>
        std::mem::discriminant (a) == std::mem::discriminant (b),
      (a, b) => a == b, } };
  let existing : Option<NodeId> =
    tree . get (treeid) . unwrap () . children ()
    . find ( |child| same_kind (&child . value () . kind) )
    . map ( |child| child . id () );
  existing . unwrap_or_else ( || append ( tree, treeid, viewnode_of (kind) )) }

fn append (
  tree     : &mut Tree<Viewnode>,
  treeid   : NodeId,
  viewnode : Viewnode,
) -> NodeId {
  tree . get_mut (treeid) . unwrap () . append (viewnode) . id () }

fn viewnode_of (
  kind : ViewnodeKind,
) -> Viewnode {
  Viewnode { focused : false, folded : false, body_folded : false, kind } }

#[cfg(test)]
#[path = "../../../tests/unit/render_changes.rs"]
mod phantom_insertion_plan_tests;
