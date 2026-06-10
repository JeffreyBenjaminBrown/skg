/// This module implements 'local instruction collection', which is
/// how the save path extracts instructions from a buffer. The name
/// refers to its central property: extraction is one pure traversal
/// in which each buffer position reads only itself and its direct
/// children, plus a context that flows down from its ancestors. The
/// spec is TODO/local-instruction-collection/3_plan.org.
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

use crate::dbs::node_lookup::nodecomplete_from_in_rust_graph;
use crate::from_text::supplement_from_disk::{
  build_diskSupplemented_defineNodes,
  Definenodes_with_Sourcemoves };
use crate::from_text::validate::{buffernode_differs_from_disknode, suppress_writes_to_inactive_nodes};
use crate::from_text::weave::member_is_visible;
use crate::source_sets::ActiveSourceSet;
use crate::types::misc::{ID, SkgConfig};
use crate::types::save::{DefineNode, SaveNode, SourceMove};
use crate::types::tree::forest::ViewForest;
use lower::{lower_collected_intents, nodeMerge_pairs, LoweringOutput};
use resolve_visibility::resolve_visibility;
use traverse::collect_instructions_locally;
use types::CollectedIntents;
use validate_text_claims::validate_text_claims;

use std::error::Error;
use typedb_driver::TypeDBDriver;

pub struct NonmergeSavePlan {
  pub define_nodes : Vec<DefineNode>,
  pub source_moves : Vec<SourceMove>,
  pub warnings     : Vec<String>, // nonfatal, destined for SaveResponse.warnings (e.g. inactive-node rewrite suppression)
}

/// This is the whole non-nodeMerge half of save extraction, done via
/// local instruction collection. It returns the plan, plus the
/// (acquirer, acquiree) pairs that nodeMerge expansion consumes.
#[allow(non_snake_case)]
pub async fn extract_nonmergeSavePlan_locally (
  viewforest : &ViewForest,
  config     : &SkgConfig,
  driver     : &TypeDBDriver,
  restricted_source_set : Option<&ActiveSourceSet>, // None means no restriction; callers normalize 'all' to None.
) -> Result<(NonmergeSavePlan, Vec<(ID, ID)>), Box<dyn Error>> {
  let _span : tracing::span::EnteredSpan = tracing::info_span!(
    "extract_nonmergeSavePlan_locally" ). entered();
  let collected : CollectedIntents =
    collect_instructions_locally (viewforest)
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  validate_text_claims (&collected, config, driver) . await ?;
  let nodeMerge_acquisitions : Vec<(ID, ID)> =
    nodeMerge_pairs (&collected);
  let resolved : lower::LoweredIntents = {
    let LoweringOutput { intents, visibility } =
      lower_collected_intents (collected)
      . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
    resolve_visibility (
      intents, &visibility, config, driver ) . await ? };
  let with_disk : Definenodes_with_Sourcemoves =
    build_diskSupplemented_defineNodes (
      resolved . into_ordered_intents(),
      config, driver, restricted_source_set ) . await ?;
  let sans_noops : Vec<DefineNode> =
    filter_wouldbe_noop_defineNodes (with_disk . instructions);
  let (define_nodes, source_moves, suppressed_writes)
    : (Vec<DefineNode>, Vec<SourceMove>, bool)
    = suppress_writes_to_inactive_nodes (
        sans_noops, with_disk . source_moves,
        restricted_source_set );
  let (nodeMerge_acquisitions, suppressed_merges)
    : (Vec<(ID, ID)>, bool)
    = match restricted_source_set {
        None => (nodeMerge_acquisitions, false),
        Some (active) => {
          // A nodeMerge writes both nodes' files; under a restricted
          // set it is suppressed unless both sides are provably
          // active (TODO/full-schema/9-2_source-set-safety.org).
          let before : usize = nodeMerge_acquisitions . len ();
          let kept : Vec<(ID, ID)> =
            nodeMerge_acquisitions . into_iter ()
            . filter ( |(acquirer, acquiree)|
                member_is_visible (acquirer, config, active)
                && member_is_visible (acquiree, config, active) )
            . collect ();
          let suppressed : bool = kept . len () < before;
          (kept, suppressed) }};
  let warnings : Vec<String> =
    if suppressed_writes || suppressed_merges {
      vec! [ "Inactive nodes present in saved buffer remain unchanged in graph."
             . to_string () ] }
    else { Vec::new () };
  Ok (( NonmergeSavePlan {
          define_nodes,
          source_moves,
          warnings },
        nodeMerge_acquisitions )) }

/// Filters out Save instructions that would be no-ops,
/// because they match the pre-save in-Rust graph entry
/// (nothing changed). Delete instructions and new nodes (not yet
/// in the in-Rust graph) are kept. This runs after disk
/// supplementation so unspecified fields have already been restored
/// to their disk values before comparison.
fn filter_wouldbe_noop_defineNodes (
  instructions : Vec<DefineNode>,
) -> Vec<DefineNode> {
  let initial_count : usize = instructions . len();
  let filtered : Vec<DefineNode> = instructions
    . into_iter()
    . filter(|instr| match instr {
      DefineNode::Save(SaveNode (node)) => {
        match nodecomplete_from_in_rust_graph (&node . pid) {
          Some (pre_save) =>
            buffernode_differs_from_disknode (node, &pre_save),
          None => true, }}
      DefineNode::Delete (_) => true, })
    . collect();
  let removed_count : usize = initial_count - filtered . len();
  tracing::debug!("filter_wouldbe_noop_defineNodes: \
             kept {} of {} instructions ({} unchanged filtered out)",
            filtered . len(), initial_count, removed_count);
  filtered }
