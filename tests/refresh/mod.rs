// The unit suite of the refresh ('server/refresh/'): a file per rung
// of the refresh, and one each for the ancestry rows, the diff view,
// the tail and the skgrepo restriction ('restricted.rs'). Each
// test builds a small graph and an input buffer, refreshes it, and
// asserts the output, as an outline ('outline'). A refresh is checked
// for the fixpoint, that refreshing its output again changes nothing
// ('run_asserting_fixpoint_in'), unless the test needs a refresh the
// check cannot repeat ('refresh_once').

use super::*;
use crate::from_text::buffer_to_viewnodes::uninterpreted::org_to_viewforest;
use crate::org_to_text::viewforest_to_string;
use crate::from_text::buffer_to_viewnodes::add_missing_info::IdResolution;
use crate::diff_view::readiness_store::ReadinessStore;
use crate::graph_straddle::types::GraphStraddle;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::test_utils::refresh_reaches_fixpoint;
use crate::types::misc::{
  MSV, RelPartner, Skgrepo, SkgrepoName, rel_partners_at_relRepo};
use crate::types::nodes::complete::{Graphnode, empty_graphnode};
use crate::types::viewnode::{
  ChangeViewnode, PropertyMember, ViewnodeId, Warrant};
use crate::types::views_state::DiffViewBufferKind;

use ego_tree::{NodeId, NodeRef};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

mod ancestry;
mod chain;
mod claims;
mod diff_view;
mod hide_folders;
mod viewnode_merge;
mod re_kind;
mod reinstate;
mod reorder;
mod requests;
mod restricted;
mod substitution;
mod tail;
mod warrant;

/// A node of the skgrepo 'main' whose title is its pid.
pub(super) fn node (
  pid : &str,
) -> Graphnode {
  Graphnode { pid          : ID::from (pid),
              title        : pid . to_string (),
              home_skgrepo : SkgrepoName::from ("main"),
              .. empty_graphnode () } }

/// A list of members recorded in 'main'.
pub(super) fn ids (
  pids : &[&str],
) -> Vec<RelPartner<ID>> {
  rel_partners_at_relRepo (
    &SkgrepoName::from ("main"),
    pids . iter () . map ( |pid| ID::from (*pid) ) . collect () ) }

pub(super) fn msv_ids (
  pids : &[&str],
) -> MSV<RelPartner<ID>> {
  MSV::Specified ( ids (pids) ) }

/// The skgrepos 'main' and 'private', both owned.
pub(super) fn config () -> SkgConfig {
  SkgConfig::dummyFromSkgrepos ( HashMap::from ([
    ( SkgrepoName::from ("main"),
      Skgrepo { name : SkgrepoName::from ("main"), abbreviation : None,
                path : PathBuf::from ("main"), owned : true } ),
    ( SkgrepoName::from ("private"),
      Skgrepo { name : SkgrepoName::from ("private"), abbreviation : None,
                path : PathBuf::from ("private"), owned : true } ) ])) }

/// No skgrepo restriction, under 'config'.
pub(super) fn unrestricted () -> SkgrepoRestriction {
  SkgrepoRestriction::unrestricted (&config ()) }

/// The skgrepo restriction to 'main' alone.
pub(super) fn main_only () -> SkgrepoRestriction {
  SkgrepoRestriction::named (&config (), SkgrepoName::from ("main"))
    . unwrap () }

pub(super) fn graph (
  nodes : &[Graphnode],
) -> InRustGraph {
  InRustGraph::from_graphnodes (nodes) }

/// A straddle whose HEAD is 'before' and whose worktree is 'after'.
pub(super) fn straddle (
  before : &[Graphnode],
  after  : &[Graphnode],
) -> GraphStraddle {
  GraphStraddle::new ( Arc::new (graph (before)), Arc::new (graph (after)),
                       "head" . to_string (), 0 ) }

/// 'refresh_viewforest', then the render of the forest, as every
/// caller of the refresh renders it.
pub(super) fn refresh_and_render (
  forest : &mut ViewForest,
  record : &mut ViewnodeIdRecord,
  truth  : &Truth,
  mode   : RefreshMode,
  budget : usize,
) -> Result<String, Box<dyn Error>> {
  refresh_viewforest (forest, record, truth, mode, budget) ?;
  viewforest_to_string (forest) }

/// What one refresh made.
pub(super) struct Run {
  pub forest : ViewForest,
  pub text   : String,
}

impl Run {
  pub fn outline (
    &self,
  ) -> String {
    outline (&self . forest) }
}

/// Refresh 'text' as a content view over 'graph', with no
/// skgrepo restriction and a large budget, asserting the fixpoint.
pub(super) fn run_asserting_fixpoint (
  text  : &str,
  graph : &InRustGraph,
) -> Run {
  run_asserting_fixpoint_in (
    text, graph, &unrestricted (), RefreshMode::ContentView, 100 ) }

/// Refresh 'text' over 'graph' in 'mode' with 'budget', then assert
/// the fixpoint: its output, refreshed again, comes back as it was.
pub(super) fn run_asserting_fixpoint_in (
  text        : &str,
  graph       : &InRustGraph,
  restriction : &SkgrepoRestriction,
  mode        : RefreshMode,
  budget      : usize,
) -> Run {
  run_parsed_asserting_fixpoint (
    parse (text, IdResolution::ThroughGraph (graph)),
    graph, restriction, mode, budget ) }

/// As 'run_asserting_fixpoint_in' with no skgrepo restriction and a
/// large budget, for 'text' parsed as written
/// ('IdResolution::AsWritten'), as a save parses its buffer against
/// the graph before its nodeMerge: an occurrence keeps the ID it is
/// written under until the refresh points it at its node's pid.
pub(super) fn run_as_written_asserting_fixpoint_in (
  text  : &str,
  graph : &InRustGraph,
  mode  : RefreshMode,
) -> Run {
  run_parsed_asserting_fixpoint (
    parse (text, IdResolution::AsWritten), graph, &unrestricted (), mode, 100 ) }

/// Refresh 'forest' over 'graph' in 'mode' with 'budget', then assert
/// the fixpoint.
fn run_parsed_asserting_fixpoint (
  mut forest  : ViewForest,
  graph       : &InRustGraph,
  restriction : &SkgrepoRestriction,
  mode        : RefreshMode,
  budget      : usize,
) -> Run {
  let config : SkgConfig = config ();
  let truth : Truth = Truth { graph, config : &config, restriction };
  let mut record : ViewnodeIdRecord = ViewnodeIdRecord::of_viewforest (&forest);
  let text : String = refresh_and_render (
    &mut forest, &mut record, &truth, mode, budget )
    . unwrap ();
  if let Err (failure) = refresh_reaches_fixpoint (
    &text, graph,
    |again, again_record| refresh_viewforest (
      again, again_record, &truth, mode, budget )) {
    panic! ( "the refresh of a refresh's output is no fixpoint: {}", failure ); }
  Run { forest, text } }

/// Refresh 'forest' once, as a content view over 'graph' with no
/// skgrepo restriction and a large budget, asserting no fixpoint: for
/// a forest parsed as written, or a stub. Returns the rendered text.
pub(super) fn refresh_once (
  forest : &mut ViewForest,
  graph  : &InRustGraph,
) -> String {
  let config : SkgConfig = config ();
  let truth : Truth = Truth { graph, config : &config, restriction : &unrestricted () };
  refresh_and_render (
    forest, &mut ViewnodeIdRecord::default (), &truth,
    RefreshMode::ContentView, 100 )
    . unwrap () }

pub(super) fn parse (
  text          : &str,
  id_resolution : IdResolution,
) -> ViewForest {
  let (forest, errors, _warnings, _provenance) =
    org_to_viewforest (text, id_resolution) . unwrap ();
  assert! ( errors . is_empty (), "parse errors: {:?}", errors );
  forest }

/// One line per viewnode in buffer order, indented two spaces per
/// generation below the roots: its kind and what it stands for, then
/// 'wp' (write-protected), 'noClaim', '⊥' (no warrant), 'ᗣ' (a ghost
/// warrant), 'folded', and for an overrider '(overridesHere N)'.
pub(super) fn outline (
  forest : &ViewForest,
) -> String {
  let mut lines : Vec<String> = Vec::new ();
  for root in forest . roots () {
    outline_subtree (root, 0, &mut lines); }
  lines . join ("\n") }

/// The outline's line for the viewnode at 'treeid', unindented.
pub(super) fn outline_line (
  forest : &ViewForest,
  treeid : NodeId,
) -> String {
  describe ( forest . get (treeid) . unwrap () . value () ) }

fn outline_subtree (
  node  : NodeRef<Viewnode>,
  depth : usize,
  lines : &mut Vec<String>,
) {
  lines . push ( format! ( "{}{}", "  " . repeat (depth), describe (node . value ()) ));
  for child in node . children () {
    outline_subtree (child, depth + 1, lines); }}

fn describe (
  viewnode : &Viewnode,
) -> String {
  let mut parts : Vec<String> = Vec::new ();
  let warrant_atom = |warrant : Warrant| -> Option<String> { match warrant {
    Warrant::ByGraph => None,
    Warrant::ByHead  => Some ( "ᗣ" . to_string () ),
    Warrant::No      => Some ( "⊥" . to_string () ), } };
  match &viewnode . kind {
    ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) => {
      parts . push ( format! ("node {}", occurrence . gnid) );
      if occurrence . is_writeProtected () { parts . push ("wp" . into ()); }
      if ! occurrence . claimsMembership { parts . push ("noClaim" . into ()); }
      parts . extend ( warrant_atom (occurrence . viewStats . warrant) );
      if let Some (original) = &occurrence . overridesHere {
        parts . push ( format! ("(overridesHere {})", original) ); }},
    ViewnodeKind::Vognode (Vognode::Ghost (ghost)) => {
      parts . push ( format! ("ghost {}", ghost . gnid) );
      if ! ghost . claimsMembership { parts . push ("noClaim" . into ()); }
      parts . extend ( warrant_atom (ghost . warrant) ); },
    ViewnodeKind::Vognode (Vognode::GnidOnly (gnid_only)) => {
      parts . push ( format! ("{} {}", gnid_only . kind . wire_atom (), gnid_only . gnid) );
      if ! gnid_only . claimsMembership { parts . push ("noClaim" . into ()); }
      parts . extend ( warrant_atom (gnid_only . warrant) ); },
    ViewnodeKind::Tombstone => parts . push ("tombstone" . into ()),
    ViewnodeKind::BufferRoot => parts . push ("root" . into ()),
    ViewnodeKind::Folder (folder) =>
      parts . push ( folder . repr_in_client () . to_string () ),
    ViewnodeKind::PropertyMember (member) => {
      parts . push ( match member {
        PropertyMember::Flag { flag, .. } => format! ("flag {}", flag . wire_name ()),
        _ => format! ("{} {}", member . repr_in_client (), member . title ()), } );
      let (claims, warrant) : (bool, Warrant) = match member {
        PropertyMember::Alias { claimsMembership, warrant, .. }
          | PropertyMember::ID { claimsMembership, warrant, .. } =>
          (*claimsMembership, *warrant),
        PropertyMember::Flag { warrant, .. } => (true, *warrant), };
      if ! claims { parts . push ("noClaim" . into ()); }
      parts . extend ( warrant_atom (warrant) ); },
    ViewnodeKind::ChangeViewnode (change) => {
      parts . push ( change . repr_in_client () . to_string () );
      if let ChangeViewnode::ChangesetMarker { kind } = change {
        parts . push ( kind . wire_name () . to_string () ); }}, }
  if viewnode . foldAndFocus . folded { parts . push ("folded" . into ()); }
  parts . join (" ") }

/// The metadata of an occurrence of 'pid' in 'main', with 'more'
/// parts.
pub(super) fn occurrence_metadata (
  pid  : &str,
  more : &str,
) -> String {
  format! ( "(skg node (gnid {}) (skgRepos (nodeRepo main)){}{})",
            pid, if more . is_empty () { "" } else { " " }, more ) }
