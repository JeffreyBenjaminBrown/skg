//! The refresh's diff-view mode ('TODO/simplify-refresh/
//! 04_1_plan_rewrite-refresh.org', "ghosts and the diff view" and
//! "change viewnodes"). Outside the diff view every function here
//! answers None or does nothing.
//!
//! The goal's HEAD part: over the graph straddle a parent's goal is
//! the union of its lists in the two graphs
//! ('GraphStraddle::partners', in the order of their interleaved
//! diff), and an item only HEAD's list holds is warranted 'ByHead': a
//! ghost for a node HEAD has and the worktree lacks, or a ghost
//! warrant on a node whose relationship HEAD had and the worktree
//! lacks.
//!
//! The change viewnodes of each occurrence are recomputed in place
//! from the straddle: kept while the change exists, drawn when
//! missing, entombstoned when gone. An editable occurrence in C holds,
//! ahead of its content, what 'model_of_front_change_viewnodes' says
//! (its old text, home skgrepo move, acquired folder, changed property
//! folders and changeset markers), and its partner folders whose
//! membership changed; a write-protected occurrence holds none. A
//! member whose relationship moved to another skgrepo holds its
//! relRepo move. The children are tagged with the three kinds of
//! change; the tail reads the readiness facts from the store.

use crate::diff_view::change_viewnodes::{
  mk_body_diff_viewnode, mk_relRepo_move_viewnode};
use crate::diff_view::changes::{
  ChangedAspects, changed_partner_folder_kinds, model_of_front_change_viewnodes,
  union_with_existence};
use crate::diff_view::pruned_change_view::{
  PrunedChangeViewSets, prunedChangeView_sets_of_buffer};
use crate::graph_straddle::types::{GraphStraddle, Partner};
use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::refresh::Walk;
use crate::refresh::ancestry::required_ancestor_of;
use crate::refresh::extent::is_chain_parent;
use crate::refresh::goal::{
  Goal, GoalItem, GoalMember, member_of_child};
use crate::refresh::id_rule::entombstone_gone_change;
use crate::refresh::ladder::children_of;
use crate::types::misc::{ID, SkgrepoName};
use crate::types::nodes::complete::Flag;
use crate::types::nodes::rust::GraphnodeInRust;
use crate::types::tree::forest::in_content_position;
use crate::types::tree::moves::{move_to_first_child, place_created_children_after_their_model_predecessors};
use crate::types::views_state::DiffViewBufferKind;
use crate::types::viewnode::{
  ChangeViewnode, ChangesetMarkerKind, DiffChange, Existence, Folder,
  Viewnode, ViewnodeKind, Vognode, Warrant, viewnode_of};
use crate::update_buffer::completion_mode::CompletionMode;

use ego_tree::{NodeId, NodeMut, NodeRef, Tree};
use std::collections::HashSet;
use std::error::Error;
use std::iter::once;
use std::mem::{Discriminant, discriminant};

/// The straddle and the buffer, in the diff view.
fn diff_view_of<'a> (
  walk : &Walk<'a>,
) -> Option<(&'a GraphStraddle, DiffViewBufferKind)> {
  match walk . mode {
    CompletionMode::DiffView { straddle, buffer_kind } => Some ((straddle, buffer_kind)),
    CompletionMode::ContentView => None, } }

fn sets<'a> (
  walk : &Walk<'a>,
) -> Option<&'a PrunedChangeViewSets> {
  let (straddle, buffer_kind) : (&GraphStraddle, DiffViewBufferKind) =
    diff_view_of (walk) ?;
  Some ( prunedChangeView_sets_of_buffer (straddle, buffer_kind) ) }

//
// The goal's HEAD part
//

/// The goal item of a partner over the straddle: warranted by the
/// truth when the worktree's list holds it, else by HEAD.
fn partner_item (
  partner : &Partner,
) -> GoalItem {
  GoalItem {
    member  : GoalMember::Gnid ( partner . id . clone () ),
    warrant : if partner . existence == Some (Existence::Removed) {
                Warrant::ByHead } else { Warrant::ByGraph },
    relRepo : None,
    rel     : partner . existence,
    partner : Some ( partner . clone () ), } }

/// An occurrence's content over the straddle. Of what the worktree
/// holds, a subscribee as such keeps what 'unintegrated' keeps. A
/// removed member that neither graph has is left out: its change is
/// under the content changeset marker.
pub(crate) fn content_goal (
  pid          : &ID,
  unintegrated : &dyn Fn (Vec<ID>) -> Vec<ID>,
  walk         : &Walk,
) -> Option<Goal> {
  let (straddle, _) : (&GraphStraddle, DiffViewBufferKind) = diff_view_of (walk) ?;
  let partners : Vec<Partner> = straddle . partners (pid, RelationRole::CONTENT);
  let kept : HashSet<ID> = unintegrated (
    partners . iter ()
    . filter ( |partner| partner . existence != Some (Existence::Removed) )
    . map ( |partner| partner . id . clone () )
    . collect () ) . into_iter () . collect ();
  Some ( Goal::new (
    partners . iter ()
    . filter ( |partner| match partner . existence {
      Some (Existence::Removed) => straddle . node_in_either_graph (&partner . id),
      _ => kept . contains (&partner . id), } )
    . map (partner_item)
    . collect (),
    true )) }

/// The containers of a node of the containerward chain, over the
/// straddle.
pub(crate) fn chain_goal (
  pid  : &ID,
  walk : &Walk,
) -> Option<Goal> {
  let (straddle, _) : (&GraphStraddle, DiffViewBufferKind) = diff_view_of (walk) ?;
  Some ( Goal::new (
    straddle . partners (pid, RelationRole::CONTAINER) . iter ()
    . filter ( |partner| partner . existence != Some (Existence::Removed)
                         || straddle . node_in_either_graph (&partner . id) )
    . map (partner_item)
    . collect (),
    false )) }

/// A ghost's old content, where content stands: its content at HEAD.
pub(crate) fn ghost_content_goal (
  pid  : &ID,
  walk : &Walk,
) -> Option<Goal> {
  let (straddle, _) : (&GraphStraddle, DiffViewBufferKind) = diff_view_of (walk) ?;
  Some ( Goal::new (
    straddle . partners (pid, RelationRole::CONTENT) . iter ()
    . map (partner_item) . collect (),
    true )) }

/// A partner folder's members over the straddle. In a folder whose
/// membership the save reads, a removed member that neither graph has
/// is left out.
pub(crate) fn folder_goal (
  kind        : &Folder,
  anchor      : &ID,
  member_role : RelationRole,
  walk        : &Walk,
) -> Option<Goal> {
  let (straddle, _) : (&GraphStraddle, DiffViewBufferKind) = diff_view_of (walk) ?;
  Some ( Goal::new (
    straddle . partners (anchor, member_role) . iter ()
    . filter ( |partner| partner . existence != Some (Existence::Removed)
                         || ! kind . is_editable ()
                         || straddle . node_in_either_graph (&partner . id) )
    . map (partner_item)
    . collect (),
    kind . goal_is_ordered () )) }

/// A property folder's members over the straddle: the union of the
/// node's aliases, IDs or true flags in the two graphs, each lost one
/// warranted by HEAD.
pub(crate) fn property_goal (
  kind : &Folder,
  pid  : &ID,
  walk : &Walk,
) -> Option<Goal> {
  let (straddle, _) : (&GraphStraddle, DiffViewBufferKind) = diff_view_of (walk) ?;
  let before : Option<&GraphnodeInRust> = straddle . before . get (pid);
  let after  : Option<&GraphnodeInRust> = straddle . after . get (pid);
  let item = |member : GoalMember, existence : Option<Existence>| -> GoalItem {
    GoalItem {
      warrant : if existence == Some (Existence::Removed) {
                  Warrant::ByHead } else { Warrant::ByGraph },
      rel     : existence,
      .. GoalItem::by_graph (member) } };
  Some ( match kind {
    Folder::Alias => {
      let texts = |node : Option<&GraphnodeInRust>| -> Vec<String> {
        node . map ( |node| node . alias_texts () ) . unwrap_or_default () };
      let relRepo_after = |text : &String| -> Option<SkgrepoName> {
        let node : &GraphnodeInRust = after ?;
        node . aliases . or_default () . iter ()
          . find ( |alias| alias . member == *text )
          . filter ( |alias| alias . relRepo != node . home_skgrepo )
          . map ( |alias| alias . relRepo . clone () ) };
      Goal::new (
        union_with_existence ( &texts (before), &texts (after) ) . into_iter ()
        . map ( |(text, existence)| GoalItem {
          relRepo : relRepo_after (&text),
          .. item ( GoalMember::Alias (text), existence ) } )
        . collect (),
        true ) },
    Folder::ID => {
      let ids = |node : Option<&GraphnodeInRust>| -> Vec<ID> {
        node . map ( |node| once ( node . pid . clone () )
                     . chain ( node . extra_ids . iter () . cloned () ) . collect () )
          . unwrap_or_default () };
      Goal::new (
        union_with_existence ( &ids (before), &ids (after) ) . into_iter ()
        . map ( |(id, existence)| item ( GoalMember::Id (id), existence ) )
        . collect (),
        false ) },
    _ => {
      let has = |node : Option<&GraphnodeInRust>, flag : &Flag| -> bool {
        node . is_some_and ( |node| node . flags . contains (flag) ) };
      Goal::new (
        Flag::ALL . into_iter ()
        . filter_map ( |flag| {
          let existence : Option<Existence> =
            match ( has (before, &flag), has (after, &flag) ) {
              (false, true)  => Some (Existence::Added),
              (true,  false) => Some (Existence::Removed),
              (true,  true)  => None,
              (false, false) => return None, };
          Some ( item ( GoalMember::Flag (flag), existence )) } )
        . collect (),
        false ) }, } ) }

/// The two acquisition folders' goals: under an acquirer, the ghosts
/// of the nodes it acquired since HEAD; under a ghost, the node that
/// acquired it.
pub(crate) fn acquisition_goal (
  tree   : &Tree<Viewnode>,
  folder : NodeId,
  kind   : &Folder,
  walk   : &Walk,
) -> Option<Goal> {
  let (straddle, _) : (&GraphStraddle, DiffViewBufferKind) = diff_view_of (walk) ?;
  let anchor : NodeRef<Viewnode> = tree . get ( required_ancestor_of (tree, folder, 0) ? ) ?;
  let anchor_gnid : ID = match &anchor . value () . kind {
    ViewnodeKind::Vognode (vognode) => vognode . skgid () . clone (),
    _ => return None, };
  Some ( match kind {
    Folder::Acquired => Goal::new (
      straddle . acquirees_of (&anchor_gnid) . into_iter ()
      . map ( |acquiree| GoalItem {
        warrant : Warrant::ByHead,
        .. GoalItem::by_graph ( GoalMember::Gnid (acquiree) ) } )
      . collect (),
      false ),
    _ => Goal::new (
      straddle . acquirer_of (&anchor_gnid) . into_iter ()
      . map ( |acquirer| GoalItem::by_graph ( GoalMember::Gnid (acquirer) ))
      . collect (),
      false ), } ) }

//
// The change viewnodes
//

/// What identifies a change viewnode (or a derived folder) among its
/// viewparent's: its kind, a changeset marker by what it stands for.
#[derive(Clone, Debug, PartialEq, Eq)]
enum ChangeKey {
  OldText,
  BodyDiff,
  HomeSkgrepoMove,
  RelRepoMove,
  Marker (ChangesetMarkerKind),
  Folder (Discriminant<Folder>),
}

fn change_key (
  viewnode : &Viewnode,
) -> Option<ChangeKey> {
  match &viewnode . kind {
    ViewnodeKind::ChangeViewnode (change) => Some ( match change {
      ChangeViewnode::OldText { .. }         => ChangeKey::OldText,
      ChangeViewnode::BodyDiff { .. }        => ChangeKey::BodyDiff,
      ChangeViewnode::HomeSkgrepoMove { .. } => ChangeKey::HomeSkgrepoMove,
      ChangeViewnode::RelRepoMove { .. }     => ChangeKey::RelRepoMove,
      ChangeViewnode::ChangesetMarker { kind } => ChangeKey::Marker (*kind), } ),
    ViewnodeKind::Folder (folder) => Some ( ChangeKey::Folder (discriminant (folder)) ),
    _ => None, } }

/// Whether the change viewnode, or acquisition folder, at 'child' no
/// longer stands for a change: always outside the diff view; in it,
/// when its viewparent's expected change viewnodes lack its kind. A
/// relRepo move is decided where its member is tagged
/// ('tag_children'), and other folders never are.
pub(crate) fn change_is_gone (
  tree  : &Tree<Viewnode>,
  child : NodeId,
  walk  : &Walk,
) -> bool {
  let node : NodeRef<Viewnode> = tree . get (child) . unwrap ();
  let is_derived : bool = match &node . value () . kind {
    ViewnodeKind::ChangeViewnode (ChangeViewnode::RelRepoMove { .. }) =>
      return ! walk . is_diff_view (),
    ViewnodeKind::ChangeViewnode (_) => true,
    ViewnodeKind::Folder (Folder::Acquired | Folder::AcquiredBy) => true,
    _ => false, };
  if ! is_derived { return false; }
  if ! walk . is_diff_view () { return true; }
  let Some (parent) = node . parent () else { return true; };
  let key : Option<ChangeKey> = change_key (node . value ());
  ! expected_change_viewnodes (tree, parent . id (), walk) . iter ()
    . any ( |expected| change_key (expected) == key ) }

/// The change viewnodes (and derived and changed folders) the parent
/// at 'parent' should hold, in order, as freshly drawn: for an
/// editable occurrence, its front change viewnodes, then its changed
/// partner folders, then its markers; for an old text, its body diff
/// when the body changed; for a ghost where content stands, its
/// acquired-by folder. Empty for anything else.
fn expected_change_viewnodes (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
  walk   : &Walk,
) -> Vec<Viewnode> {
  let Some ((straddle, buffer_kind)) = diff_view_of (walk) else {
    return Vec::new (); };
  let node : NodeRef<Viewnode> = tree . get (parent) . unwrap ();
  match &node . value () . kind {
    ViewnodeKind::Vognode (Vognode::Unrestricted (occurrence))
      if ! occurrence . is_writeProtected () => {
      let pid : &ID = &occurrence . skgid;
      let in_c : bool = sets (walk) . is_some_and ( |sets| sets . c . contains (pid) );
      let aspects : ChangedAspects =
        if in_c { ChangedAspects::of_node (straddle . change_leaves (), pid) }
        else { ChangedAspects::default () };
      let model : Tree<Viewnode> = model_of_front_change_viewnodes (
        straddle, pid, &aspects, in_c,
        buffer_kind == DiffViewBufferKind::OrderOnlyChanges
          && straddle . order_only_pids () . contains (pid) );
      let fronts : Vec<Viewnode> =
        model . root () . children () . map ( |child| child . value () . clone () )
        . collect ();
      let is_marker = |viewnode : &Viewnode| -> bool {
        matches! ( change_key (viewnode), Some (ChangeKey::Marker (_)) ) };
      let partner_folders : Vec<Viewnode> =
        if in_c { changed_partner_folder_kinds (&aspects) . into_iter ()
                  . map ( |kind| viewnode_of (ViewnodeKind::Folder (kind)) )
                  . collect () }
        else { Vec::new () };
      fronts . iter () . filter ( |viewnode| ! is_marker (viewnode) ) . cloned ()
        . chain (partner_folders)
        . chain ( fronts . iter () . filter ( |viewnode| is_marker (viewnode) ) . cloned () )
        . collect () },
    ViewnodeKind::ChangeViewnode (ChangeViewnode::OldText { .. }) => {
      let Some (pid) = node . parent () . and_then ( |owner| match &owner . value () . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (occurrence)) =>
          Some ( occurrence . skgid . clone () ),
        _ => None, } ) else { return Vec::new (); };
      match ( straddle . before . get (&pid), straddle . after . get (&pid) ) {
        (Some (before), Some (after)) if before . body != after . body =>
          vec! [ mk_body_diff_viewnode (
            before . body . as_deref (), after . body . as_deref () ) ],
        _ => Vec::new (), } },
    ViewnodeKind::Vognode (Vognode::Ghost (ghost)) if in_content_position (node) =>
      straddle . acquirer_of (&ghost . skgid) . into_iter ()
      . map ( |_| viewnode_of (ViewnodeKind::Folder (Folder::AcquiredBy)) )
      . collect (),
    _ => Vec::new (), } }

/// Draw the expected change viewnodes the parent lacks, each after
/// the expected one before it (or first), and bring those it keeps up
/// to date with the straddle. Folders are drawn empty: their own
/// visits fill them from their goals. A change viewnode describes the
/// straddle and is recomputed at every refresh, so drawing one is no
/// correction ('entombstone_gone_change').
pub(crate) fn draw_change_viewnodes (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  walk   : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  let expected : Vec<Viewnode> = expected_change_viewnodes (tree, parent, walk);
  if expected . is_empty () { return Ok (( )); }
  let mut order : Vec<NodeId> = Vec::new ();
  let mut created : HashSet<NodeId> = HashSet::new ();
  for model in expected {
    let key : Option<ChangeKey> = change_key (&model);
    let kept : Option<NodeId> =
      tree . get (parent) . unwrap () . children ()
      . find ( |child| change_key (child . value ()) == key )
      . map ( |child| child . id () );
    match kept {
      Some (kept) => {
        if let ViewnodeKind::ChangeViewnode (_) = &model . kind {
          tree . get_mut (kept) . unwrap () . value () . kind = model . kind; }
        order . push (kept); },
      None => {
        let drawn : NodeId =
          tree . get_mut (parent) . unwrap () . append (model) . id ();
        if matches! ( tree . get (drawn) . unwrap () . value () . kind,
                      ViewnodeKind::Folder (_) ) {
          walk . added_folders . insert (drawn); }
        created . insert (drawn);
        order . push (drawn); }, }}
  place_created_children_after_their_model_predecessors (
    tree, parent, &order, &created ) ?;
  Ok (( )) }

//
// Tagging
//

/// Tag each child of 'parent' with its change: an occurrence with its
/// node's existence, its relationship's (from the goal item it is) and
/// whether its node's other properties changed; a ghost with its
/// node's removal and its relationship's; an unknown or a property
/// member with its relationship's. A root, and an occurrence the goal
/// does not name, are tagged as a node only; a node of the
/// containerward chain carries no change. And under each
/// member whose relationship moved to another skgrepo, its relRepo
/// move.
pub(crate) fn tag_children (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  goal   : Option<&Goal>,
  walk   : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  let Some ((straddle, _)) = diff_view_of (walk) else { return Ok (( )); };
  let is_root : bool = matches! ( tree . get (parent) . unwrap () . value () . kind,
                                  ViewnodeKind::BufferRoot );
  let chain : bool = is_chain_parent (tree, parent);
  for child in children_of (tree, parent) {
    let item : Option<GoalItem> =
      member_of_child (tree . get (child) . unwrap () . value ())
      . and_then ( |member| goal ? . item_for (&member) . cloned () );
    let rel : Option<Existence> = item . as_ref () . and_then ( |item| item . rel );
    { let mut child_mut : NodeMut<Viewnode> = tree . get_mut (child) . unwrap ();
      let viewnode : &mut Viewnode = child_mut . value ();
      let change : Option<DiffChange> = match &viewnode . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (occurrence)) =>
          if chain { Some (DiffChange::default ()) }
          else { Some ( straddle . node_change (&occurrence . skgid, rel) ) },
        ViewnodeKind::Vognode (Vognode::Ghost (_)) =>
          Some ( DiffChange { node : Some (Existence::Removed),
                              rel  : if is_root { None } else { rel },
                              properties : false } ),
        ViewnodeKind::Vognode (Vognode::Unknown (_))
          | ViewnodeKind::PropertyMember (_) =>
          Some ( DiffChange { rel, .. DiffChange::default () } ),
        _ => None, };
      if let Some (change) = change {
        viewnode . change_facts . change = change; }}
    if tree . get (child) . unwrap () . value () . is_occurrence () && ! chain && ! is_root {
      let moved : Option<Viewnode> =
        item . as_ref () . and_then ( |item| item . partner . as_ref () )
        . and_then (mk_relRepo_move_viewnode);
      keep_one_relRepo_move (tree, child, moved, walk); }}
  Ok (( )) }

/// Under the member at 'member', exactly the relRepo move 'moved'
/// (kept, or drawn first), or none: any other is entombstoned.
fn keep_one_relRepo_move (
  tree   : &mut Tree<Viewnode>,
  member : NodeId,
  moved  : Option<Viewnode>,
  walk   : &mut Walk,
) {
  let present : Vec<NodeId> =
    tree . get (member) . unwrap () . children ()
    . filter ( |child| change_key (child . value ()) == Some (ChangeKey::RelRepoMove) )
    . map ( |child| child . id () )
    . collect ();
  let mut extra : Vec<NodeId> = present . clone ();
  if let Some (moved) = moved {
    match present . first () {
      Some (kept) => {
        tree . get_mut (*kept) . unwrap () . value () . kind = moved . kind;
        extra . remove (0); },
      None => {
        let drawn : NodeId = tree . get_mut (member) . unwrap () . append (moved) . id ();
        move_to_first_child (tree, member, drawn)
          . expect ("keep_one_relRepo_move: the move was just drawn"); }, }}
  for stale in extra {
    entombstone_gone_change (tree, stale, walk); }}

/// A drawn editable occurrence's body starts folded unless its node
/// is in C: it is context.
pub(crate) fn fold_body_of_drawn_occurrence (
  viewnode : &mut Viewnode,
  pid      : &ID,
  walk     : &Walk,
) {
  if let Some (sets) = sets (walk) {
    viewnode . foldAndFocus . bodyFolded = ! sets . c . contains (pid); }}
