/// This module implements 'local instruction collection', which is
/// how the save path extracts instructions from a buffer. The name
/// refers to its central property: extraction is one pure traversal
/// in which each buffer position reads only itself and its direct
/// children, plus a context that flows down from its ancestors. The
/// spec is TODO/DONE/local-instruction-collection/3_plan.org.
/// .
/// Beyond listing the submodules, this mod file defines the composed
/// pipeline, 'extract_nonmergeSavePlan_locally'. That function runs
/// the traversal ('traverse'), and then the downstream stages in
/// order: text-claim validation ('validate_text_claims'), lowering
/// ('lower'), visibility resolution ('resolve_visibility'), disk
/// supplementation (from 'super::supplement_from_disk'), and finally
/// the noop filter (defined below).

pub mod predicates;
pub mod types;
pub mod traverse;
pub mod lower;
pub mod resolve_visibility;
pub mod validate_text_claims;

use crate::dbs::node_lookup::graphnode_from_graph;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::from_text::supplement_from_disk::{
  build_diskSupplemented_nodeInstructions,
  NodeInstructions_with_Repomoves };
use crate::from_text::validate::{buffernode_differs_from_disknode, suppress_writes_to_inactive_nodes};
use crate::from_text::weave::member_is_visible;
use crate::skgrepo_sets::ActiveSkgRepoSet;
use crate::types::misc::{ID, SkgConfig};
use crate::types::save::{
  NodeInstruction, PostSkgsaveCommitNoticeCandidate, SaveNode, SkgRepoMove };
use crate::types::tree::forest::ViewForest;
use lower::{lower_collected_fieldIntents, nodeMerge_pairs, LoweringOutput};
use resolve_visibility::resolve_visibility;
use traverse::collect_instructions_locally;
use types::CollectedFieldIntents;
use validate_text_claims::validate_text_claims;

use std::error::Error;
use std::collections::HashSet;

pub struct NonmergeSavePlan {
  pub node_instructions : Vec<NodeInstruction>,
  pub skgrepo_moves : Vec<SkgRepoMove>,
  pub flag_targets : HashSet<ID>,
  pub warnings     : Vec<String>, // nonfatal, destined for SaveResponse.warnings (e.g. inactive-node rewrite suppression)
  pub post_skgsave_commit_notice_candidates : Vec<PostSkgsaveCommitNoticeCandidate>,
}

/// This is the whole non-nodeMerge half of save extraction, done via
/// local instruction collection. It returns the plan, plus the
/// (acquirer, acquiree) pairs that nodeMerge expansion consumes.
#[allow(non_snake_case)]
pub fn extract_nonmergeSavePlan_locally_in_graph (
  viewforest             : &ViewForest,
  graph                  : &InRustGraph,
  config                 : &SkgConfig,
  restricted_skgrepo_set : Option<&ActiveSkgRepoSet>, // None means no restriction; callers normalize 'all' to None.
) -> Result<(NonmergeSavePlan, Vec<(ID, ID)>), Box<dyn Error>> {
  let _span : tracing::span::EnteredSpan = tracing::info_span!(
    "extract_nonmergeSavePlan_locally" ). entered();
  let collected : CollectedFieldIntents =
    collect_instructions_locally (viewforest)
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  validate_text_claims (&collected, graph, config) ?;
  let flag_targets : HashSet<ID> = collected . by_pid . iter ()
    . filter_map ( |(pid, entry)|
      entry . flag . map ( |_| pid . clone () ) )
    . collect ();
  let nodeMerge_acquisitions : Vec<(ID, ID)> =
    nodeMerge_pairs (&collected);
  let (resolved, post_skgsave_commit_notice_candidates)
    : (lower::LoweredNodeIntents, Vec<PostSkgsaveCommitNoticeCandidate>) = {
    let LoweringOutput { intents, visibility, hidden_outside } =
      lower_collected_fieldIntents (collected)
      . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
    resolve_visibility (
      intents, &visibility, &hidden_outside, graph, config,
      restricted_skgrepo_set ) ? };
  let with_disk : NodeInstructions_with_Repomoves =
    build_diskSupplemented_nodeInstructions (
      resolved . into_ordered_intents(),
      graph, config, restricted_skgrepo_set ) ?;
  let sans_noops : Vec<NodeInstruction> =
    filter_wouldbe_noop_nodeInstructions (graph, with_disk . instructions);
  let (node_instructions, skgrepo_moves, suppressed_writes)
    : (Vec<NodeInstruction>, Vec<SkgRepoMove>, bool)
    = suppress_writes_to_inactive_nodes (
        sans_noops, with_disk . skgrepo_moves,
        restricted_skgrepo_set );
  let (nodeMerge_acquisitions, suppressed_merges)
    : (Vec<(ID, ID)>, bool)
    = match restricted_skgrepo_set {
        None => (nodeMerge_acquisitions, false),
        Some (active) => {
          // A nodeMerge writes both nodes' files; under a restricted
          // set it is suppressed unless both sides are provably
          // active (TODO/DONE/full-schema/DONE/9-2_source-set-safety.org).
          let before : usize = nodeMerge_acquisitions . len ();
          let kept : Vec<(ID, ID)> =
            nodeMerge_acquisitions . into_iter ()
            . filter ( |(acquirer, acquiree)|
                member_is_visible (graph, acquirer, config, active)
                && member_is_visible (graph, acquiree, config, active) )
            . collect ();
          let suppressed : bool = kept . len () < before;
          (kept, suppressed) }};
  let warnings : Vec<String> =
    if suppressed_writes || suppressed_merges {
      vec! [ "Inactive nodes present in saved buffer remain unchanged in graph."
             . to_string () ] }
    else { Vec::new () };
  Ok (( NonmergeSavePlan {
          node_instructions,
          skgrepo_moves,
          flag_targets,
          warnings,
          post_skgsave_commit_notice_candidates },
        nodeMerge_acquisitions )) }

/// Transitional compatibility for direct extraction tests.
pub fn extract_nonmergeSavePlan_locally (
  viewforest             : &ViewForest,
  config                 : &SkgConfig,
  restricted_skgrepo_set : Option<&ActiveSkgRepoSet>,
) -> Result<(NonmergeSavePlan, Vec<(ID, ID)>), Box<dyn Error>> {
  let nodes = crate::dbs::filesystem::multiple_nodes
    ::read_all_skg_files_from_skgrepos (config)?;
  let graph = InRustGraph::from_graphnodes (&nodes);
  extract_nonmergeSavePlan_locally_in_graph (
    viewforest, &graph, config, restricted_skgrepo_set ) }

/// Filters out Save instructions that would be no-ops,
/// because they match the pre-save in-Rust graph entry
/// (nothing changed). Delete instructions and new nodes (not yet
/// in the in-Rust graph) are kept. This runs after disk
/// supplementation so unspecified fields have already been restored
/// to their disk values before comparison.
fn filter_wouldbe_noop_nodeInstructions (
  graph        : &InRustGraph,
  instructions : Vec<NodeInstruction>,
) -> Vec<NodeInstruction> {
  let initial_count : usize = instructions . len();
  let filtered      : Vec<NodeInstruction> = instructions
    . into_iter()
    . filter(|instr| match instr {
      NodeInstruction::Save(SaveNode (node)) => {
        match graphnode_from_graph (graph, &node . pid) {
          Some (pre_save) =>
            buffernode_differs_from_disknode (node, &pre_save),
          None => true, }}
      NodeInstruction::Delete (_) => true, })
    . collect();
  let removed_count : usize = initial_count - filtered . len();
  tracing::debug!("filter_wouldbe_noop_nodeInstructions: \
             kept {} of {} instructions ({} unchanged filtered out)",
            filtered . len(), initial_count, removed_count);
  filtered }

#[cfg(test)]
mod flag_noop_filter_tests {
  use super::*;
  use crate::types::misc::SkgRepoName;
  use crate::types::nodes::complete::{
    Flag, Graphnode, empty_graphnode};

  fn node (
    pid   : &str,
    flags : Vec<Flag>,
  ) -> Graphnode {
    Graphnode {
      pid          : ID::from (pid),
      home_skgrepo : SkgRepoName::from ("main"),
      title        : pid . to_string (),
      flags,
      .. empty_graphnode () }}

  #[test]
  fn flag_only_changes_survive_the_noop_filter () {
    let disk_nodes : Vec<Graphnode> = ["root", "left", "right"]
      . into_iter ()
      . map (|pid| node (pid, Vec::new ()))
      . collect ();
    let graph = InRustGraph::from_graphnodes (&disk_nodes);
    let requested : Vec<NodeInstruction> = disk_nodes . iter ()
      . cloned ()
      . map (|mut candidate| {
        candidate . flags . push (Flag::NoSearchMatching);
        NodeInstruction::Save (SaveNode (candidate)) })
      . collect ();
    let kept = filter_wouldbe_noop_nodeInstructions (&graph, requested);
    assert_eq! (kept . len (), 3,
      "flag-only saves must not be discarded as unchanged");

    let unchanged : Vec<NodeInstruction> = disk_nodes . into_iter ()
      . map (|candidate| NodeInstruction::Save (SaveNode (candidate)))
      . collect ();
    assert! (filter_wouldbe_noop_nodeInstructions (&graph, unchanged) . is_empty ());
  }

  #[test]
  fn clearing_the_only_flag_survives_the_noop_filter () {
    let disk = node (
      "root", vec![Flag::NoSearchMatching]);
    let graph = InRustGraph::from_graphnodes (&[disk . clone ()]);
    let cleared = node ("root", Vec::new ());
    assert_eq! (filter_wouldbe_noop_nodeInstructions (
      &graph, vec![NodeInstruction::Save (SaveNode (cleared))]) . len (), 1);
  }
}
