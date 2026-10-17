//! The PCSVF ("pruned change-spanning viewforest"): the viewforest
//! the "all changes" buffer of the total diff view draws
//! ('TODO/total-diff-view/2_plan.org', "The PCSVF").
//!
//! C is the set of changed graphnodes. R is the set of content
//! origins reached from C by walking containerward through the union
//! of the graph pair; U is R, C and every node on those walks; D is R
//! minus the origins that another kept origin reaches containerward
//! inside U. The view unfolds contentward from each d in D: a node in
//! U is drawn editable with its whole content list, a member outside
//! U is a write-protected leaf. View completion does the drawing, its
//! 'tdv_drawing' set ('process_unrestrictedVognode_changes').
//!
//! The merges and skgrepo-changes buffers draw PCSVFs over subsets of
//! C ('pcsvf_sets_for_merges', 'pcsvf_sets_for_home_moves'). The
//! order-only buffer draws no PCSVF: it is a flat list of the nodes
//! whose only change is the order of a list.

use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::graph_pair::types::{GraphPair, Presence};
use crate::nodeMerge::nodeMergeInstructionTriple::acquiree_text_preserver_title;
use crate::prominence::climb_containerward_to_cycle_via;
use crate::readiness_store::ReadinessStore;
use crate::serve::handlers::readiness_requests::set_ready_facts_in_viewforest;
use crate::to_org::render::change_viewnodes::append_historical_phantom;
use crate::to_org::util::{EditableMap, build_node_branch_minus_content};
use crate::types::env::RuntimeGeneration;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::{ID, SkgrepoName};
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{
  PartnerFolder, Phantom, Viewnode, ViewnodeKind, Vognode};
use crate::types::views_state::{TdvBuffer, pids_from_viewforest};
use crate::update_buffer::{complete_viewforest_with_graph_pair, finish_viewforest};

use ego_tree::{NodeId, NodeRef};
use std::collections::{BTreeSet, HashSet};
use std::error::Error;

/// The sets that define a PCSVF. See the module comment.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct PcsvfSets {
  pub c : BTreeSet<ID>,
  pub r : BTreeSet<ID>,
  pub u : BTreeSet<ID>,
  pub d : BTreeSet<ID>,
}

/// What view completion draws a buffer of the total diff view from:
/// the graph pair, and the buffer, whose PCSVF sets it draws.
#[derive(Clone, Copy)]
pub struct TdvDrawing<'a> {
  pub pair       : &'a GraphPair,
  pub tdv_buffer : TdvBuffer,
}

impl<'a> TdvDrawing<'a> {
  pub fn sets (
    &self,
  ) -> &'a PcsvfSets {
    self . pair . pcsvf_sets_of_buffer (self . tdv_buffer) }
}

/// The sets of the PCSVF over all of C.
pub fn pcsvf_sets (
  pair : &GraphPair,
) -> PcsvfSets {
  pcsvf_sets_over ( pair, pair . changed_pids () . all () ) }

/// The sets of a PCSVF over the changed nodes 'c'.
pub fn pcsvf_sets_over (
  pair : &GraphPair,
  c    : BTreeSet<ID>,
) -> PcsvfSets {
  let containers_of = |skgid : &ID| -> Vec<ID> {
    union_containers (pair, skgid) };
  let mut r : BTreeSet<ID> = BTreeSet::new ();
  let mut u : BTreeSet<ID> = c . clone ();
  for changed in &c {
    let (walk, origins) : (Vec<ID>, HashSet<ID>) =
      climb_containerward_to_cycle_via (&containers_of, changed);
    u . extend (walk);
    r . extend (origins); }
  u . extend ( r . iter () . cloned () );
  let d : BTreeSet<ID> = kept_origins (&r, &u, &containers_of);
  PcsvfSets { c, r, u, d } }

/// The sets of the merges buffer's PCSVF, over the nodes involved in
/// nodeMerges since HEAD: for each acquiree (a node of the
/// before-graph only, which an after-graph node lists as an extra
/// ID), the acquiree, its acquirer and the acquiree's text preserver.
pub fn pcsvf_sets_for_merges (
  pair : &GraphPair,
) -> PcsvfSets {
  let mut c : BTreeSet<ID> = BTreeSet::new ();
  for acquiree in &pair . changed_pids () . rightward {
    let Some (acquirer) : Option<ID> = pair . acquirer_of (acquiree)
      else { continue; };
    c . extend ( acquiree_text_preservers (pair, acquiree, &acquirer) );
    c . insert ( acquiree . clone () );
    c . insert (acquirer); }
  pcsvf_sets_over (pair, c) }

/// The new nodes in the acquirer's content that bear the title a
/// nodeMerge gives the acquiree's text preserver.
fn acquiree_text_preservers (
  pair     : &GraphPair,
  acquiree : &ID,
  acquirer : &ID,
) -> Vec<ID> {
  let ( Some (acquiree_node), Some (acquirer_node) ) =
    ( pair . before . get (acquiree), pair . after . get (acquirer) )
    else { return Vec::new (); };
  let preserver_title : String =
    acquiree_text_preserver_title (&acquiree_node . title);
  acquirer_node . contains . iter ()
    . map ( |relationship| &relationship . member )
    . filter ( |member|
      pair . node_presence (member) == Some (Presence::OnlyAfter)
      && pair . after . get (member)
         . is_some_and ( |node| node . title == preserver_title ) )
    . cloned ()
    . collect () }

/// The sets of the skgrepo-changes buffer's PCSVF, over the nodes
/// whose home skgrepo changed.
pub fn pcsvf_sets_for_home_moves (
  pair : &GraphPair,
) -> PcsvfSets {
  let c : BTreeSet<ID> =
    pair . changed_pids () . rightward . iter ()
    . filter ( |pid| pair . node_changes (pid) . home . is_some () )
    . cloned ()
    . collect ();
  pcsvf_sets_over (pair, c) }

/// The containers of a node in either graph of the pair, sorted by ID.
pub fn union_containers (
  pair  : &GraphPair,
  skgid : &ID,
) -> Vec<ID> {
  pair . partners (skgid, RelationRole::CONTAINER) . into_iter ()
    . map ( |partner| partner . id )
    . collect () }

/// D: process R sorted by ID; discard each origin whose containerward
/// walk within U reaches an origin still kept. Of two origins on a
/// shared cycle, exactly one is kept.
fn kept_origins (
  r             : &BTreeSet<ID>,
  u             : &BTreeSet<ID>,
  containers_of : &dyn Fn (&ID) -> Vec<ID>,
) -> BTreeSet<ID> {
  let mut kept : BTreeSet<ID> = r . clone ();
  for origin in r {
    let reaches_a_kept_origin : bool = {
      let mut seen : HashSet<ID> = HashSet::from ([ origin . clone () ]);
      let mut frontier : Vec<ID> = vec! [ origin . clone () ];
      let mut found : bool = false;
      while let Some (current) = frontier . pop () {
        for container in containers_of (&current) {
          if ! u . contains (&container) || ! seen . insert (container . clone ()) {
            continue; }
          if kept . contains (&container) {
            found = true; break; }
          frontier . push (container); }
        if found { break; }}
      found };
    if reaches_a_kept_origin {
      kept . remove (origin); }}
  kept }

/// The buffer 'drawing' names, with roots 'roots' (for a PCSVF, its
/// D in order), drawn with readiness from 'store': the buffer text,
/// the pids to register, and the viewforest.
pub fn pcsvf_viewforest (
  runtime : &RuntimeGeneration,
  drawing : TdvDrawing,
  store   : &ReadinessStore,
  roots   : &[ID],
) -> Result<(String, Vec<ID>, ViewForest), Box<dyn Error>> {
  let mut viewforest : ViewForest =
    stub_viewforest_of_roots (runtime, drawing, roots) ?;
  complete_viewforest_with_graph_pair (runtime, drawing, &mut viewforest) ?;
  set_tdv_facts_in_viewforest (
    &mut viewforest, drawing, store, &runtime . graph );
  let text : String = finish_viewforest (
    &mut viewforest, &runtime . graph, &runtime . config, None ) ?;
  let pids : Vec<ID> =
    pids_from_viewforest (&viewforest) . into_iter () . collect ();
  Ok (( text, pids, viewforest )) }

/// What a buffer of the total diff view gets after view completion:
/// its '(ready ...)' facts, and in the skgrepo-changes buffer each
/// moved vognode's home move ('set_home_move_facts_in_viewforest').
pub fn set_tdv_facts_in_viewforest (
  viewforest : &mut ViewForest,
  drawing    : TdvDrawing,
  store      : &ReadinessStore,
  graph      : &InRustGraph,
) {
  set_ready_facts_in_viewforest (
    viewforest, drawing . pair . change_forest (), store, graph );
  if drawing . tdv_buffer == TdvBuffer::SkgrepoChanges {
    set_home_move_facts_in_viewforest (viewforest, drawing . pair ); }}

/// On each unrestricted vognode whose home skgrepo moved: the move,
/// and, when the nearest vognode ancestor whose home moved moved the
/// same way, that ancestor's letter. Computed like
/// 'homeSkgrepoAtBoundary', against the nearest moved ancestor.
pub fn set_home_move_facts_in_viewforest (
  viewforest : &mut ViewForest,
  pair       : &GraphPair,
) {
  let facts : Vec<(NodeId, (SkgrepoName, SkgrepoName), Option<String>)> =
    viewforest . nodes ()
    . filter_map ( |node| {
      let ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =
        &node . value () . kind else { return None; };
      let home_move : (SkgrepoName, SkgrepoName) =
        pair . node_changes (&t . skgid) . home ?;
      let moved_like_ancestor : Option<String> =
        nearest_moved_vognode_ancestor (node, pair)
        . filter ( |(_, ancestor_move)| *ancestor_move == home_move )
        . map ( |(generation, _)| ancestor_letter (generation) );
      Some (( node . id (), home_move, moved_like_ancestor )) } )
    . collect ();
  for (treeid, home_move, moved_like_ancestor) in facts {
    if let ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =
      &mut viewforest . as_internal_tree_mut () . get_mut (treeid)
      . unwrap () . value () . kind {
      t . viewStats . home_move = Some (home_move);
      t . viewStats . moved_like_ancestor = moved_like_ancestor; }}}

/// The ancestor generation (1 = viewparent, counting every viewnode
/// between) and the home move of the nearest unrestricted vognode
/// ancestor whose home skgrepo moved.
fn nearest_moved_vognode_ancestor (
  node : NodeRef<Viewnode>,
  pair : &GraphPair,
) -> Option<(usize, (SkgrepoName, SkgrepoName))> {
  node . ancestors () . enumerate ()
    . find_map ( |(index, ancestor)| match &ancestor . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
        pair . node_changes (&t . skgid) . home
        . map ( |home_move| (index + 1, home_move) ),
      _ => None, } ) }

/// The letter the relationship heralds give an ancestor generation:
/// a for 1 (the viewparent), b for 2, and so on; '{N}' past z.
fn ancestor_letter (
  generation : usize,
) -> String {
  if (1 ..= 26) . contains (&generation) {
    char::from ( b'a' + (generation - 1) as u8 ) . to_string ()
  } else { format! ( "{{{}}}", generation ) }}

/// The nodes a buffer of the total diff view draws: its pids, and
/// the deleted nodes it draws as historical phantoms. The leaves they
/// own are the changes it shows.
pub fn nodes_drawn_in_viewforest (
  viewforest : &ViewForest,
) -> HashSet<ID> {
  let mut drawn : HashSet<ID> = pids_from_viewforest (viewforest);
  drawn . extend (
    viewforest . nodes () . filter_map ( |node| match &node . value () . kind {
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (h))) =>
        Some ( h . skgid . clone () ),
      _ => None, } ));
  drawn }

/// The stub the PCSVF's view completion starts from: each root as an
/// editable stub, or, when the after-graph lacks it, as a historical
/// phantom with its old content. In a PCSVF, a multiply-contained
/// root gets a folded container folder, which completion fills with
/// its containers in either graph and their containerward forest.
/// The order-only buffer, a flat list, draws none.
fn stub_viewforest_of_roots (
  runtime : &RuntimeGeneration,
  drawing : TdvDrawing,
  roots   : &[ID],
) -> Result<ViewForest, Box<dyn Error>> {
  let pair : &GraphPair = drawing . pair;
  let mut viewforest : ViewForest = ViewForest::new ();
  let buffer_root : NodeId = viewforest . internal_root_skgid ();
  let mut stub_editable_map : EditableMap = EditableMap::new ();
  for root in roots {
    if pair . after . get (root) . is_none () {
      append_historical_phantom (
        viewforest . as_internal_tree_mut (), buffer_root, pair, root, None )
        . ok_or_else ( || format! (
          "pcsvf_viewforest: root {} is in neither graph", root )) ?;
      continue; }
    let root_treeid : NodeId = build_node_branch_minus_content (
      Some (( viewforest . as_internal_tree_mut (), buffer_root )),
      root, &runtime . graph, &runtime . config, &mut stub_editable_map,
      None ) ?;
    if drawing . tdv_buffer != TdvBuffer::OrderOnlyChanges
      && union_containers (pair, root) . len () > 1 {
      viewforest . as_internal_tree_mut () . get_mut (root_treeid)
        . ok_or ("pcsvf_viewforest: a stub root vanished") ?
        . append ( Viewnode {
          focused : false, folded : true, body_folded : false,
          kind : ViewnodeKind::PartnerFolder (PartnerFolder::Container) } ); }}
  Ok (viewforest) }

