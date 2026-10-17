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
//! 'graph_pair' set ('process_unrestrictedVognode_changes').

use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::graph_pair::types::GraphPair;
use crate::prominence::climb_containerward_to_cycle_via;
use crate::readiness_store::ReadinessStore;
use crate::serve::handlers::readiness_requests::set_ready_facts_in_viewforest;
use crate::to_org::render::change_viewnodes::append_historical_phantom;
use crate::to_org::util::{EditableMap, build_node_branch_minus_content};
use crate::types::env::RuntimeGeneration;
use crate::types::misc::ID;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{PartnerFolder, Viewnode, ViewnodeKind};
use crate::types::views_state::pids_from_viewforest;
use crate::update_buffer::{complete_viewforest_with_graph_pair, finish_viewforest};

use ego_tree::NodeId;
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

/// The PCSVF whose roots are 'roots' (a D, in order), drawn from
/// 'pair' with readiness from 'store': the buffer text, the pids to
/// register, and the viewforest.
pub fn pcsvf_viewforest (
  runtime : &RuntimeGeneration,
  pair    : &GraphPair,
  store   : &ReadinessStore,
  roots   : &[ID],
) -> Result<(String, Vec<ID>, ViewForest), Box<dyn Error>> {
  let mut viewforest : ViewForest =
    stub_viewforest_of_roots (runtime, pair, roots) ?;
  complete_viewforest_with_graph_pair (runtime, pair, &mut viewforest) ?;
  set_ready_facts_in_viewforest (
    &mut viewforest, pair . change_forest (), store, &runtime . graph );
  let text : String = finish_viewforest (
    &mut viewforest, &runtime . graph, &runtime . config, None ) ?;
  let pids : Vec<ID> =
    pids_from_viewforest (&viewforest) . into_iter () . collect ();
  Ok (( text, pids, viewforest )) }

/// The stub the PCSVF's view completion starts from: each root as an
/// editable stub, or, when the after-graph lacks it, as a historical
/// phantom with its old content. A multiply-contained root gets a
/// folded container folder, which completion fills with its
/// containers in either graph and their containerward forest.
fn stub_viewforest_of_roots (
  runtime : &RuntimeGeneration,
  pair    : &GraphPair,
  roots   : &[ID],
) -> Result<ViewForest, Box<dyn Error>> {
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
    if union_containers (pair, root) . len () > 1 {
      viewforest . as_internal_tree_mut () . get_mut (root_treeid)
        . ok_or ("pcsvf_viewforest: a stub root vanished") ?
        . append ( Viewnode {
          focused : false, folded : true, body_folded : false,
          kind : ViewnodeKind::PartnerFolder (PartnerFolder::Container) } ); }}
  Ok (viewforest) }

