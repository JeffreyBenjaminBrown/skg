use crate::consts::TANTIVY_WRITER_BUFFER_BYTES;
use crate::context::context_origin_types_for_transition;
use crate::dbs::filesystem::one_node::{
  PreparedTelescopeWrite, prepare_nodecomplete_telescope,
};
use crate::dbs::init::wipe_then_init_typedb_db;
use crate::telescope::invariants::telescope_violations_of;
use crate::dbs::in_rust_graph::{
  InRustGraph,
  InRustGraphHandle,
  apply_definenodes_to_inRustGraph,
  override_invariants::{
    format_override_invariant_violations,
    validate_touched_override_invariants,
  },
};
use crate::dbs::tantivy::background_writer::{
  enqueue_tantivy_write,
  lock_tantivy_writes,
  wait_for_tantivy_generation,
  TantivyGenerationStatus,
  TantivyWriteTask,
};
use crate::dbs::tantivy::write::{add_documents_to_tantivy_writer, commit_with_status, delete_nodes_by_id_from_index};
use crate::dbs::typedb::nodes::create_only_nodes_with_no_ids_present;
use crate::dbs::typedb::nodes::delete_nodes_from_pids;
use crate::dbs::typedb::nodes::overwrite_extra_ids_of_node;
use crate::dbs::typedb::sources::update_node_source;
use crate::dbs::typedb::nodes::which_ids_exist;
use crate::dbs::typedb::relationships::apply_relationship_deltas_for_nodes;
use crate::dbs::typedb::relationships::create_all_relationships;
use crate::dbs::typedb::relationships::delete_all_outbound_relationships_to_nodes;
use crate::types::misc::{ID, MSV, MemberAtSource, SkgConfig, TantivyIndex};
use crate::types::errors::{BufferValidationError, SaveError};
use crate::types::nodes::rust::NodeRust;
use crate::types::nodes::tantivy::NodeTantivy;
use crate::types::nodes::typedb::NodeTypedb;
use crate::types::save::{DefineNode, SaveNode, DeleteNode, NodeMerge, SourceMove};
use crate::types::nodes::complete::NodeComplete;
use crate::types::store_state::{
  GraphGeneration,
  PathDigest,
  SelectedPathManifest,
};
use crate::dbs::tantivy::background_writer::TantivyGeneration;

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::error::Error;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tantivy::IndexWriter;
use typedb_driver::TypeDBDriver;

/// Updates **everything** from already prepared `DefineNode`s, in order:
///   1) Filesystem (source of truth)
///   2) TypeDB (with recovery: rebuild from disk on failure)
///   3) Tantivy (with recovery: rebuild from disk on failure)
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct StoreUpdateOutcome {
  pub graph_generation   : GraphGeneration,
  pub tantivy_generation : TantivyGeneration, }

pub async fn update_graph_minus_nodeMerges (
  node_defs     : Vec<DefineNode>,
  source_moves  : &[SourceMove],
  config        : SkgConfig,
  tantivy_index : &TantivyIndex,
  driver        : &TypeDBDriver,
  graph         : &InRustGraphHandle,
) -> Result < StoreUpdateOutcome, Box<dyn Error> > {
  let _write_guard = crate::write_lock::acquire_graph_write_lock () . await;
  let outcome = update_graph_minus_nodeMerges_with_hoist_approval (
    node_defs, source_moves, config, tantivy_index, driver, graph,
    &HashSet::new () ) . await ?;
  require_tantivy_commit (outcome) ?;
  Ok (outcome)
}

fn require_tantivy_commit (
  outcome : StoreUpdateOutcome,
) -> Result<(), Box<dyn Error>> {
  match wait_for_tantivy_generation (outcome . tantivy_generation) {
    TantivyGenerationStatus::Committed
    | TantivyGenerationStatus::Reconstructed (_) => Ok (()),
    TantivyGenerationStatus::Failed (reason) => Err (format! (
      "graph generation {} and TypeDB committed, but Tantivy generation {} \
       failed: {}",
      outcome . graph_generation . get (),
      outcome . tantivy_generation . get (), reason) . into ()),
    TantivyGenerationStatus::Pending => unreachable! (), }
}

async fn update_graph_minus_nodeMerges_with_hoist_approval (
  mut node_defs : Vec<DefineNode>,
  source_moves  : &[SourceMove],
  config        : SkgConfig,
  tantivy_index : &TantivyIndex,
  driver        : &TypeDBDriver,
  graph         : &InRustGraphHandle,
  hoist_approved_pids : &HashSet<ID>,
) -> Result < StoreUpdateOutcome, Box<dyn Error> > {
  tracing::info!("Updating FS, in-Rust graph, TypeDB, and Tantivy ...");
  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "apply_delete_propagation_cleanup" ). entered();
    let graph_snap : Arc<InRustGraph> =
      graph . load_full () . graph . clone ();
    apply_delete_propagation_cleanup (&mut node_defs,
                                      &graph_snap); }
  apply_define_nodes_to_stores ( node_defs,
                                 source_moves,
                                 config,
                                 tantivy_index,
                                 driver,
                                 graph,
                                 true,
                                 None,
                                 hoist_approved_pids ). await }

/// Apply prepared `DefineNode`s to the derived stores (in-Rust graph,
/// TypeDB, Tantivy), optionally writing the filesystem first.
///
/// `write_fs = true` is the save path: the filesystem is the source of
/// truth and is written first. `write_fs = false` is the RELOAD path:
/// the filesystem already holds the new state (skg is syncing FROM it),
/// so the stores are updated to match without touching any `.skg` file.
/// Callers wanting a reload MUST route here directly (never through
/// `update_graph_minus_nodeMerges`), so `apply_delete_propagation_cleanup`
/// -- which rewrites OTHER nodes' files -- never runs.
pub(crate) async fn apply_define_nodes_to_stores (
  node_defs     : Vec<DefineNode>,
  source_moves  : &[SourceMove],
  config        : SkgConfig,
  tantivy_index : &TantivyIndex,
  driver        : &TypeDBDriver,
  graph         : &InRustGraphHandle,
  write_fs      : bool,
  reload_manifest : Option<SelectedPathManifest>,
  hoist_approved_pids : &HashSet<ID>,
) -> Result < StoreUpdateOutcome, Box<dyn Error> > {
  let db_name : &str = &config . db_name;
  let old_selected = graph . load_full ();
  let old_graph_snap : Arc<InRustGraph> = old_selected . graph . clone ();
  let mut new_graph : InRustGraph = (*old_graph_snap) . clone ();
  apply_definenodes_to_inRustGraph (&mut new_graph, &node_defs);
  let mut selected_manifest : SelectedPathManifest =
    reload_manifest . unwrap_or_else ( || old_selected . manifest . clone ());

  if write_fs { // FS (source of truth)
    // TODO: Print per-source write information
    tracing::info!( "Writing {} instruction(s) to disk ...",
               { let total_input : usize = node_defs . len ();
                 total_input } );
    let prepared = prepare_fs_update (
      &node_defs, source_moves, &config, hoist_approved_pids) ?
      . with_selected_fence (&old_selected . manifest);
    prepared . validate_selected_fence ()?;
    let (deleted_count, written_count) : (usize, usize) = {
      let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "update_fs_from_savenode_defs") . entered ();
      prepared . apply (&config) ? };
    prepared . apply_to_manifest (&mut selected_manifest);
    tracing::info!( "   Deleted {} file(s), wrote {} file(s).",
              deleted_count, written_count ); }

  // Context origin types, read from the checked candidate graph, so
  // the Tantivy pass below indexes each saved doc once with its final
  // type — no separate context writer/commit. Computed here (not on the
  // Tantivy thread) so the read happens before any further mutation.
  let context_types : HashMap<ID, String> =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "context_origin_types_for_transition" ). entered();
      context_origin_types_for_transition (
        &old_graph_snap, &new_graph, &node_defs,
        &old_selected . cyclic_roots ) };
  // Context-only neighbors join the same Tantivy generation as the saved
  // documents.  They are deliberately NOT TypeDB or filesystem instructions;
  // rewriting their complete index documents is the simplest atomic way to
  // change a stored rank label (including clearing it to the empty string).
  let mut tantivy_instructions = node_defs . clone ();
  let instructed : HashSet<ID> = node_defs . iter () . map (|definition| match
    definition {
      DefineNode::Save (SaveNode (node)) => node . pid . clone (),
      DefineNode::Delete (deleted) => deleted . id . clone (),
    }) . collect ();
  for pid in context_types . keys () {
    if instructed . contains (pid) { continue; }
    if let Some (node) = new_graph . nodes . get (pid) {
      tantivy_instructions . push (DefineNode::Save (SaveNode (
        nodecomplete_from_noderust (node)))); }}

  // TypeDB (foreground): only TypeDB must finish before the save
  // responds, because the response is re-rendered from the in-Rust
  // graph (never from Tantivy).
  if let Err (e) = {
    let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "update_typedb_from_saveinstructions") . entered ();
    update_typedb_from_saveinstructions (
      db_name, driver, &node_defs, source_moves,
      Some ( old_graph_snap . as_ref () )) . await }
    { tracing::error!(
        "TypeDB incremental update failed: {}. Reconstructing candidate graph...",
        e);
      let candidate_nodes = nodecompletes_from_graph (&new_graph);
      match wipe_then_init_typedb_db (
        &config, driver, &candidate_nodes) . await {
        Ok (()) => tracing::warn! (
          "TypeDB selected the candidate graph by complete reconstruction."),
        Err (candidate_error) if !write_fs => {
          let previous_nodes = nodecompletes_from_graph (&old_graph_snap);
          match wipe_then_init_typedb_db (
            &config, driver, &previous_nodes) . await {
            Ok (()) => return Err (format! (
              "TypeDB rejected the reload candidate (incremental: {}; \
               reconstruction: {}) and was restored to graph generation {}; \
               disk was not changed and no reload mutation was published",
              e, candidate_error,
              old_selected . graph_generation . get ()) . into ()),
            Err (previous_error) => {
              let reason = format! (
                "TypeDB is poisoned: candidate reconstruction failed ({}); \
                 previous-graph reconstruction also failed ({})",
                candidate_error, previous_error);
              graph . store (Arc::new (
                old_selected . with_typedb_poisoned (reason . clone ())));
              return Err (reason . into ()); }}}
        Err (candidate_error) => {
          let reason = format! (
            "TypeDB is poisoned after filesystem save: candidate graph \
             reconstruction failed ({})", candidate_error);
          graph . store (Arc::new (
            old_selected . with_typedb_poisoned (reason . clone ())));
          return Err (reason . into ()); }}
    } else {
      tracing::info!("   TypeDB update complete."); }

  // Finish the exact index batch before selecting its graph. Existing
  // readers retain their captured Searcher throughout this preparation.
  let tantivy_generation = enqueue_tantivy_write (TantivyWriteTask {
    tantivy_index: tantivy_index . clone (),
    instructions: tantivy_instructions,
    context_types,
    recovery_graph: Arc::new (new_graph . clone ()),
    cyclic_roots: old_selected . cyclic_roots . clone (),
  });
  match wait_for_tantivy_generation (tantivy_generation) {
    TantivyGenerationStatus::Committed
    | TantivyGenerationStatus::Reconstructed (_) => {},
    TantivyGenerationStatus::Failed (reason) => {
      graph . store (Arc::new (
        old_selected . with_tantivy_poisoned (reason . clone ())));
      return Err (format! (
        "The replacement search index failed: {}. The previous graph remains selected; source effects require recovery.",
        reason) . into ()); },
    TantivyGenerationStatus::Pending => unreachable! (),
  }
  let selected = old_selected . with_selected_transition (
    new_graph, selected_manifest, tantivy_generation)
    . with_tantivy_terminal (tantivy_generation, None)
    . with_searcher (tantivy_index . reader . searcher ());
  let graph_generation = selected . graph_generation;
  graph . store (Arc::new (selected));
  Ok (StoreUpdateOutcome { graph_generation, tantivy_generation }) }

/// Runs 'update_graph_minus_nodeMerges' and then 'merge_nodes' in that
/// order, applying any Tantivy rebuild from either step to the
/// caller's '&mut TantivyIndex'. The sole place the save pipeline
/// should call when it has both save_instructions and
/// nodeMerge_instructions in hand.
pub async fn update_graph_including_nodeMerges (
  save_instructions  : Vec<DefineNode>,
  nodeMerge_instructions : &[NodeMerge],
  source_moves       : &[SourceMove],
  config             : SkgConfig,
  tantivy_index      : &mut TantivyIndex,
  driver             : &TypeDBDriver,
  graph              : &InRustGraphHandle,
  hoist_approved_pids : &HashSet<ID>,
  expected_graph_generation : u64,
) -> Result<(), Box<dyn Error>> {
  // Serialize this store mutation against any concurrent save / reload /
  // rebuild so no RCU update is lost (last-store-wins on the ArcSwap).
  let _write_guard = crate::write_lock::acquire_graph_write_lock () . await;
  let selected_before = graph . load_full ();
  if selected_before . graph_generation . get ()
     != expected_graph_generation
  {
    return Err (Box::new (SaveError::StaleViewAuthority (format! (
      "save expected graph generation {}, but writer acquired generation {}",
      expected_graph_generation,
      selected_before . graph_generation . get ())))); }
  { // Exact save fence.  It covers the union of the ordinary and merge
    // phases, including every collateral cleanup/tombstone, before either
    // phase can write its first byte.
    let all_filesystem_outputs : Vec<DefineNode> =
      save_instructions . iter () . cloned ()
      . chain (nodeMerge_instructions . iter ()
        . flat_map (|node_merge| node_merge . to_vec ()))
      . collect ();
    prepare_fs_update (
      &all_filesystem_outputs, source_moves, &config,
      hoist_approved_pids)?
      . with_selected_fence (&selected_before . manifest)
      . validate_selected_fence ()?; }
  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "validate_override_invariants_after_save" ). entered();
    validate_override_invariants_after_save (
      &save_instructions,
      nodeMerge_instructions,
      &config,
      graph ) } ?;
  let touched_pids_for_telescope_gate : Vec<ID> =
    // captured before update_graph consumes save_instructions;
    // checked after the update, against the post-save graph
    save_instructions . iter ()
    . filter_map ( |d| match d {
      DefineNode::Save (s) => Some ( s . 0 . pid . clone () ),
      DefineNode::Delete (_) => None } )
    . collect ();
  let config_for_telescope_gate : SkgConfig = config . clone ();
  let save_outcome : StoreUpdateOutcome =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "update_graph_minus_nodeMerges" ). entered();
      update_graph_minus_nodeMerges_with_hoist_approval (
        save_instructions, source_moves, config . clone(),
        tantivy_index, driver, graph,
        hoist_approved_pids ) . await } ?;
  require_tantivy_commit (save_outcome) ?;
  let nodeMerge_replacement : Option<TantivyIndex> =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "merge_nodes" ). entered();
      crate::nodeMerge::merge_nodes_with_hoist_approval (
        nodeMerge_instructions, config,
        tantivy_index, driver, graph,
        hoist_approved_pids ) . await } ?;
  if let Some (new_index) = nodeMerge_replacement {
    *tantivy_index = new_index; }
  { // The save-side telescope warning gate: same primitive as the
    // init/rebuild gate, on the touched nodes only. Warnings, never
    // failures (the write has already, deliberately, happened).
    let snap = graph . load_full ();
    for pid in &touched_pids_for_telescope_gate {
      for v in telescope_violations_of (
        &config_for_telescope_gate, &snap, pid ) {
        tracing::warn! ( pid = %pid, violation = %v,
                         "telescope warning after save" ); }} }
  Ok (( )) }

pub fn validate_override_invariants_after_save (
  save_instructions  : &[DefineNode],
  nodeMerge_instructions : &[NodeMerge],
  config             : &SkgConfig,
  graph              : &InRustGraphHandle,
) -> Result<(), Box<dyn Error>> {
  let graph_snap : Arc<InRustGraph> =
    graph . load_full () . graph . clone ();
  let mut simulated : InRustGraph =
    (*graph_snap) . clone ();
  let mut nonmerge : Vec<DefineNode> =
    save_instructions . to_vec ();
  apply_delete_propagation_cleanup (&mut nonmerge, &graph_snap);
  apply_definenodes_to_inRustGraph (&mut simulated, &nonmerge);
  let nodeMerge_definenodes : Vec<DefineNode> =
    nodeMerge_instructions . iter ()
    . flat_map ( |nodeMerge| nodeMerge . to_vec () )
    . collect ();
  apply_definenodes_to_inRustGraph (&mut simulated, &nodeMerge_definenodes);
  let touched : HashSet<ID> = // every node this save actually wrote
    nonmerge . iter () . chain ( nodeMerge_definenodes . iter () )
    . map ( |dn| match dn {
        DefineNode::Save (SaveNode (n)) => n . pid . clone (),
        DefineNode::Delete (DeleteNode { id, .. }) => id . clone (), } )
    . collect ();
  let violations =
    validate_touched_override_invariants (config, &simulated, &touched);
  if violations . is_empty () {
    Ok (( ))
  } else {
    Err (Box::new (SaveError::BufferValidationErrors {
      errors : vec![
        BufferValidationError::OverrideInvariantViolation (
          format_override_invariant_violations (&violations)) ],
      // This check runs after parsing, so no parse warnings are in
      // scope here; the buffer-validation path (from_text) carries
      // them. update_from_and_rerender_buffer also back-fills the
      // save's parse warnings onto any post-parse validation error.
      warnings : vec![], } )) }}

/// Update the DB from a batch of `DefineNode`s:
/// 1) Delete all nodes marked Delete, using delete_nodes_from_pids
/// 2) Remove deleted nodes from further processing
/// 3) Create only nodes whose IDs are not present, via
///      create_only_nodes_with_no_ids_present
/// 4) Delete all outbound `contains` from those nodes, via
///      delete_out_links
///    PITFALL: Only the primary ID from each NodeComplete is used.
/// 5) Recreate all relationships for those nodes, via
///      create_all_relationships
pub async fn update_typedb_from_saveinstructions (
  db_name      : &str,
  driver       : &TypeDBDriver,
  node_defs    : &[DefineNode],
  source_moves : &[SourceMove],
  old_graph    : Option<&InRustGraph>, // Some → write only the edge delta vs this pre-save snapshot; None → bulk delete-all + recreate-all (the nodeMerge path, where pid migration makes a set-diff subtle).
) -> Result<(), Box<dyn Error>> {

  // PITFALL: Below, each get(0) on an 'ids' field
  // is not motivated by separating the PID from the others,
  // because (see add_missing_info_to_viewforest) there are no others.
  // It is simply to turn the Vec<ID> into a bare ID.

  let ( to_delete, to_save )
    : ( Vec<DeleteNode>, Vec<SaveNode> )
    = DefineNode::partition_save_and_delete (node_defs);

  { // delete
    let to_delete_pids : Vec<ID> =
      to_delete . iter ()
      . map ( |DeleteNode { id, .. }| id . clone() )
      . collect ();
    if ! to_delete_pids . is_empty () {
      tracing::debug!("Deleting nodes with PIDs: {:?}", to_delete_pids);
      { let _span : tracing::span::EnteredSpan = tracing::info_span!(
          "delete_nodes_from_pids") . entered ();
        // PITFALL: deletions cascade in TypeDB by default,
        // so we are left with no incomplete relationships.
        delete_nodes_from_pids (
          db_name, driver, & to_delete_pids )
        . await } ?; }}

  { // create | update
    let to_write_nodecompletes : Vec<NodeComplete> =
      to_save . iter ()
      . map ( |SaveNode (node) | node . clone() )
      . collect ();
    let to_write_pids : Vec<ID> =
      to_write_nodecompletes . iter ()
      . map ( |n| n . pid . clone() )
      . collect ();
    let to_write_typedb : Vec<NodeTypedb> = // Convert to NodeTypedb (narrow) at the boundary. Parses textlinks from each node's title+body.
      to_write_nodecompletes . iter ()
      . map (NodeTypedb::from_complete_parsing_textlinks)
      . collect ();
    let pre_existing_pids : HashSet<String> = { // "Pre-existing" = not being created now. Existing ones need their has_extra_id relations re-synced below; create_only_nodes_with_no_ids_present handles extra_ids only for newly-created nodes via its internal call to 'create_node'.
      let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "which_ids_exist" ). entered();
      let pids_btreeset : BTreeSet<String> =
        to_write_pids . iter ()
        . map ( |p| p . to_string () )
        . collect ();
      which_ids_exist (db_name, driver, &pids_btreeset) . await ? };
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "create_only_nodes_with_no_ids_present") . entered ();
      create_only_nodes_with_no_ids_present (
        db_name, driver, & to_write_typedb )
      . await } ?;
    { // For existing nodes, re-sync extra_ids.
      // PITFALL: This pass must complete for every pre-existing pid before any create_all_relationships call runs, because those calls resolve target IDs through has_extra_id — a neighbor save whose target is a merged-into acquirer needs the freshly-added has_extra_id (acquiree_pid → acquirer) to exist at lookup time.
      let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "replace_extra_ids_of_existing_nodes") . entered ();
      for node in & to_write_typedb {
        if pre_existing_pids . contains (node . pid . as_str ()) {
          overwrite_extra_ids_of_node (
            db_name, driver, node ) . await ?; } } }
    match old_graph {
      Some (old_graph) => { // incremental: write only the changed edges
        let _span : tracing::span::EnteredSpan = tracing::info_span!(
          "apply_relationship_deltas_for_nodes") . entered ();
        apply_relationship_deltas_for_nodes (
          db_name, driver, old_graph, & to_write_typedb,
          & pre_existing_pids )
        . await ? ; }
      None => {
        { // Delete all 5 outbound relation types before recreating. Otherwise re-saves of existing nodes duplicate every outbound relation except 'contains'.
          let _span : tracing::span::EnteredSpan = tracing::info_span!(
            "delete_all_outbound_relationships_to_nodes") . entered ();
          delete_all_outbound_relationships_to_nodes (
            db_name, driver, & to_write_pids )
          . await ?; }
        { let _span : tracing::span::EnteredSpan = tracing::info_span!(
            "create_all_relationships") . entered ();
          create_all_relationships (
            db_name, driver, & to_write_typedb )
          . await ?; } } } }

  for sm in source_moves { // TODO ? parallelize
    update_node_source (
      db_name, driver,
      &sm . pid, &sm . new_source ) . await ?; }

  Ok (( )) }

/// Two-phase cleanup so deletes don't leave dangling references on
/// disk:
///
/// Phase 1 (cleanup-SaveNode generation): for any node N still on
/// disk that references a being-deleted pid in any of its out bound
/// list fields, *maybe* append a verbatim Save(N) to NODE_DEFS
/// so that phase 2 will rewrite N. But *don't* do that for:
/// - nodes already in NODE_DEFS as Save. Phase 2 covers them.
/// - nodes also being deleted (because why bother)
///
/// Phase 2 (strip pass): for every Save in NODE_DEFS regardless of
/// origin (which could be Phase 1 or the save itself),
/// remove every id in 'deleted_id_set' from the four
/// outbound list fields (contains, subscribes_to,
/// hides_from_its_subscriptions, overrides_view_of).
///
/// 'deleted_id_set' includes both the primary pid of each delete
/// AND the extra_ids of that node from the in-Rust graph.
/// Referencers may have stored any of those ids (TypeDB resolves
/// them all to the primary at relationship time, but the on-disk
/// list is whatever the buffer that wrote it had); we want all of
/// them gone.
///
/// Skipped fields:
/// - extra_ids of referencers: per the data model, an id can only
///   be an extra_id of ONE node, so a referencer cannot legitimately
///   carry a deleted node's id in its extra_ids.
/// - textlinks_to: lives in body text; stripping requires body
///   rewriting. Dangling textlink targets render as PhantomUnknown
///   placeholders when followed, so this is non-fatal.
pub(crate) fn apply_delete_propagation_cleanup (
  node_defs  : &mut Vec<DefineNode>,
  graph_snap : &Arc<InRustGraph>,
) {
  let deleted_primary_pids : HashSet<ID> = node_defs . iter ()
    . filter_map ( |d| match d {
      DefineNode::Delete (dn) => Some ( dn . id . clone ()),
      DefineNode::Save (_)    => None } )
    . collect ();
  if deleted_primary_pids . is_empty () { return; }

  let deleted_id_set : HashSet<ID> = { // include each deleted node's extra_ids
    let mut s : HashSet<ID> = deleted_primary_pids . clone ();
    for pid in &deleted_primary_pids {
      if let Some (rust) = graph_snap . get (pid) {
        for e in &rust . extra_ids {
          s . insert ( e . clone () ); }}}
    s };

  { // Phase 1: append "save-it-unchanged" SaveNodes for referencers not already covered by the user's instructions. As-is, these would have no effect, but phase 2 strips some IDs.
    let user_save_pids : HashSet<ID> = node_defs . iter ()
      . filter_map ( |d| match d {
        DefineNode::Save (SaveNode (n)) => Some ( n . pid . clone ()),
        DefineNode::Delete (_)          => None } )
      . collect ();
    let mut referencer_pids : HashSet<ID> = HashSet::new ();
    for deleted in &deleted_primary_pids { // Inverse indexes are keyed by primary pid (id_to_pid_if_found resolves extra_ids during index construction), so iterating the primary pids of deletes is sufficient to find every referencer.
      for inverse in [
        &graph_snap . contained_by,
        &graph_snap . subscribers_of,
        &graph_snap . hiders_of,
        &graph_snap . overriders_of, ] {
        if let Some (set) = inverse . get (deleted) {
          for pid in set {
            referencer_pids . insert ( pid . clone () ); }} } }
    referencer_pids . retain ( |p|
      ! user_save_pids . contains (p)
        && ! deleted_primary_pids . contains (p) );
    let cleanup_count : usize = referencer_pids . len ();
    for pid in referencer_pids {
      let Some (rust) = graph_snap . get (&pid) else { continue; };
      node_defs . push ( DefineNode::Save ( SaveNode (
        nodecomplete_from_noderust (rust) ))); }
    if cleanup_count > 0 {
      tracing::info!(
        "Adding {} cleanup save(s) to remove references to deleted nodes.",
        cleanup_count); } }

  { // Phase 2: strip every deleted id from every Save's outbound list fields, regardless of the Save's origin (Phase 1 or the save itself)
    for nd in node_defs . iter_mut () {
      if let DefineNode::Save ( SaveNode (nc) ) = nd {
        nc . contains . retain ( |id|
          ! deleted_id_set . contains (& id . member) );
        nc . subscribes_to = remove_from_msv (
          &nc . subscribes_to, &deleted_id_set );
        nc . hides_from_its_subscriptions = remove_from_msv (
          &nc . hides_from_its_subscriptions, &deleted_id_set );
        nc . overrides_view_of = remove_from_msv (
          &nc . overrides_view_of, &deleted_id_set ); }} } }

/// Project a NodeRust back into a NodeComplete verbatim. Stripping
/// of deleted ids happens later, in 'apply_delete_propagation_cleanup'
/// phase 2, and applies uniformly to all Saves.
pub(crate) fn nodecomplete_from_noderust (
  rust : &NodeRust,
) -> NodeComplete {
  NodeComplete {
    pid                          : rust . pid . clone (),
    source                       : rust . source . clone (),
    extra_ids                    : rust . extra_ids . clone (),
    title                        : rust . title . clone (),
    ugly_telescope               : rust . ugly_telescope,
    aliases                      : rust . aliases . clone (),
    body                         : rust . body . clone (),
    contains                     : rust . contains . clone (),
    subscribes_to                : rust . subscribes_to . clone (),
    hides_from_its_subscriptions : rust . hides_from_its_subscriptions . clone (),
    overrides_view_of            : rust . overrides_view_of . clone (),
    misc                         : rust . misc . clone (),
  }}

pub(crate) fn nodecompletes_from_graph (
  graph : &InRustGraph,
) -> Vec<NodeComplete> {
  let mut nodes : Vec<NodeComplete> = graph . nodes . values ()
    . map (nodecomplete_from_noderust)
    . collect ();
  nodes . sort_by ( |a, b| a . pid . cmp (&b . pid));
  nodes }

fn remove_from_msv (
  msv : &MSV<MemberAtSource<ID>>,
  exclude : &HashSet<ID>
) -> MSV<MemberAtSource<ID>> {
  match msv {
    MSV::Unspecified => MSV::Unspecified,
    MSV::Specified (v) => MSV::Specified (
      v . iter ()
        . filter ( |id| ! exclude . contains (& id . member) )
        . cloned ()
        . collect ()), } }

pub fn update_fs_from_saveinstructions (
  node_defs    : &[DefineNode],
  source_moves : &[SourceMove],
  config       : SkgConfig,
) -> io::Result<(usize, usize)> { // (deleted, written)
  update_fs_from_saveinstructions_with_hoist_approval (
    node_defs, source_moves, config, &HashSet::new () )
}

/// Save-only variant. Every delete target and every serialized telescope is
/// prepared before the first filesystem mutation. The approval set is an
/// explicit capability supplied only by the interactive Hoist retry; all
/// ordinary callers use 'update_fs_from_saveinstructions' above and fail
/// closed on ugly disk telescopes.
pub(crate) fn update_fs_from_saveinstructions_with_hoist_approval (
  node_defs             : &[DefineNode],
  source_moves          : &[SourceMove],
  config                : SkgConfig,
  hoist_approved_pids   : &HashSet<ID>,
) -> io::Result<(usize, usize)> { // (deleted, written)
  prepare_fs_update (
    node_defs, source_moves, &config, hoist_approved_pids ) ?
  . apply (&config)
}

/// Validate and serialize a prospective filesystem batch, then discard the
/// prepared bytes. The interactive save handler uses this on the union of its
/// ordinary and nodeMerge outputs before either phase is allowed to mutate.
pub(crate) fn preflight_fs_from_saveinstructions_with_hoist_approval (
  node_defs           : &[DefineNode],
  source_moves        : &[SourceMove],
  config              : &SkgConfig,
  hoist_approved_pids : &HashSet<ID>,
) -> io::Result<()> {
  prepare_fs_update (
    node_defs, source_moves, config, hoist_approved_pids ) ?;
  Ok (( ))
}

pub(crate) struct PreparedFilesystemUpdate {
  writes       : Vec<PreparedTelescopeWrite>,
  deleted_pids : HashSet<ID>,
  path_manifest : BTreeMap<PathBuf, PreparedPathMutation>,
}

#[derive(Clone, Debug)]
struct PreparedPathMutation {
  expected_before : Option<Option<PathDigest>>,
  proposed_after  : Option<Vec<u8>>,
}

impl PreparedFilesystemUpdate {
  /// Attach the exact selected byte/absence fact for every path this batch
  /// can create, replace, move, or delete.  The outer option distinguishes an
  /// attached expectation from an expected absence.
  pub(crate) fn with_selected_fence (
    mut self,
    selected : &SelectedPathManifest,
  ) -> Self {
    for (path, mutation) in &mut self . path_manifest {
      mutation . expected_before = Some (selected . get (path) . copied ()); }
    self
  }

  /// Re-read every prospective target immediately before the first write.
  /// A mismatch is returned as a typed save refusal so the connection layer
  /// can enqueue exact observation of the named paths.
  pub(crate) fn validate_selected_fence (
    &self,
  ) -> Result<(), SaveError> {
    let mut paths : Vec<PathBuf> = Vec::new ();
    let mut details : Vec<String> = Vec::new ();
    for (path, mutation) in &self . path_manifest {
      let Some (expected) = mutation . expected_before else {
        return Err (SaveError::DiskSelectionChanged {
          paths: vec![path . clone ()],
          details: vec![format! (
            "internal save fence omitted the selected before value for {}",
            path . display ())],
        }); };
      let actual : Result<Option<PathDigest>, String> =
        exact_regular_path_digest (path);
      match actual {
        Ok (actual) if actual == expected => {},
        Ok (actual) => {
          paths . push (path . clone ());
          details . push (format! (
            "{}: expected {}, found {}",
            path . display (), describe_digest (expected),
            describe_digest (actual))); }
        Err (reason) => {
          paths . push (path . clone ());
          details . push (format! ("{}: {}", path . display (), reason)); }
      }
    }
    if paths . is_empty () { Ok (( )) }
    else { Err (SaveError::DiskSelectionChanged { paths, details }) }
  }

  pub(crate) fn apply (
    &self,
    config : &SkgConfig,
  ) -> io::Result<(usize, usize)> {
    // Mutation starts only after the whole batch has passed ownership and
    // serialization preflight.  Apply the consolidated final path manifest,
    // so a path mentioned by more than one instruction is still written at
    // most once with the exact bytes represented by this prepared value.
    for (path, mutation) in &self . path_manifest {
      match &mutation . proposed_after {
        Some (bytes) => {
          if let Some (parent) = path . parent () {
            std::fs::create_dir_all (parent)?; }
          let unchanged = std::fs::read (path)
            . map (|old| old == *bytes) . unwrap_or (false);
          if !unchanged { std::fs::write (path, bytes)?; }}
        None => match std::fs::remove_file (path) {
          Ok (( ))                                          => {},
          Err (e) if e . kind () == io::ErrorKind::NotFound => {},
          Err (e)                                           => return Err (e), },
      }}
    for telescope in &self . writes {
      telescope . verify_hoist (config) ?; }
    Ok (( self . deleted_pids . len (), self . writes . len () ))
  }

  pub(crate) fn apply_to_manifest (
    &self,
    manifest : &mut SelectedPathManifest,
  ) {
    for (path, mutation) in &self . path_manifest {
      match &mutation . proposed_after {
        Some (bytes) => {
          manifest . insert (
            path . clone (), PathDigest::of_bytes (bytes)); }
        None => { manifest . remove (path); }
      }}
  }
}

pub(crate) fn prepare_fs_update (
  node_defs           : &[DefineNode],
  source_moves        : &[SourceMove],
  config              : &SkgConfig,
  hoist_approved_pids : &HashSet<ID>,
) -> io::Result<PreparedFilesystemUpdate> {
  let ( to_delete, to_save )
    : ( Vec<DeleteNode>, Vec<SaveNode> )
    = DefineNode::partition_save_and_delete (node_defs);
  let prepared_writes : Vec<PreparedTelescopeWrite> =
    to_save . iter ()
    . map ( |SaveNode (node)|
      prepare_nodecomplete_telescope (
        node,
        config,
        hoist_approved_pids . contains (&node . pid) ) )
    . collect::<io::Result<Vec<PreparedTelescopeWrite>>> () ?;

  // Resolve every deletion path before applying either deletion or write.
  // Only owned sections are eligible, exactly like the standalone deleter.
  let mut prepared_deletions : Vec<String> = Vec::new ();
  let mut deleted_pids : HashSet<ID> = HashSet::new ();
  for DeleteNode { id, .. } in &to_delete {
    for source in config . ordered_sources () {
      if ! config . user_owns_source (&source) { continue; }
      let path : String = crate::util::path_from_pid_and_source (
        config, &source, id . clone () )
        . map_err ( |e| io::Error::new (io::ErrorKind::NotFound, e) ) ?;
      if std::path::Path::new (&path) . is_file () {
        deleted_pids . insert ( id . clone () ); }
      prepared_deletions . push (path); }}

  let _ = source_moves;
  // Source moves need no file relocation of their own anymore: the
  // telescope write above places every section in its source and
  // sweeps owned sections whose source lost its last member. An
  // explicit old-path delete here would even be WRONG for a
  // private->public home move, where the old (more private) source
  // legitimately retains a section holding the node's private
  // memberships. (source_moves still matter to TypeDB/Tantivy,
  // handled elsewhere.)
  let mut path_manifest : BTreeMap<PathBuf, PreparedPathMutation> =
    BTreeMap::new ();
  // `apply` performs whole-node deletions first, followed by telescope
  // rewrites in this order.  Repeated paths therefore deliberately replace
  // the proposed after value here in the same order.
  for path in &prepared_deletions {
    path_manifest . insert (PathBuf::from (path), PreparedPathMutation {
      expected_before: None,
      proposed_after: None,
    }); }
  for telescope in &prepared_writes {
    for (path, proposed_after) in telescope . proposed_path_values () {
      path_manifest . insert (path, PreparedPathMutation {
        expected_before: None,
        proposed_after,
      }); }}

  Ok ( PreparedFilesystemUpdate {
    writes       : prepared_writes,
    deleted_pids,
    path_manifest,
  } ) }

fn exact_regular_path_digest (
  path : &Path,
) -> Result<Option<PathDigest>, String> {
  match std::fs::symlink_metadata (path) {
    Ok (metadata) if metadata . file_type () . is_file () =>
      std::fs::read (path)
        . map (|bytes| Some (PathDigest::of_bytes (&bytes)))
        . map_err (|error| format! ("could not read exact bytes: {}", error)),
    Ok (metadata) => Err (format! (
      "expected a regular file or absence, found filesystem type {:?}",
      metadata . file_type ())),
    Err (error) if error . kind () == io::ErrorKind::NotFound => Ok (None),
    Err (error) => Err (format! ("could not inspect path: {}", error)),
  }
}

fn describe_digest (digest : Option<PathDigest>) -> String {
  digest . map (|digest| format! ("BLAKE3 {}", digest . to_hex ()))
    . unwrap_or_else (|| "absence" . into ())
}

#[cfg(test)]
mod save_fence_tests {
  use super::*;

  fn prepared_for (
    path            : PathBuf,
    proposed_after  : Option<Vec<u8>>,
    selected_before : &SelectedPathManifest,
  ) -> PreparedFilesystemUpdate {
    PreparedFilesystemUpdate {
      writes: Vec::new (),
      deleted_pids: HashSet::new (),
      path_manifest: BTreeMap::from ([
        (path, PreparedPathMutation {
          expected_before: None,
          proposed_after,
        }),
      ]),
    } . with_selected_fence (selected_before)
  }

  #[test]
  fn exact_non_ascii_bytes_pass_and_same_length_rewrite_fails () {
    let temp = tempfile::tempdir () . unwrap ();
    let path = temp . path () . join ("n.skg");
    let before = "title: café\n" . as_bytes ();
    std::fs::write (&path, before) . unwrap ();
    let selected = SelectedPathManifest::from ([
      (path . clone (), PathDigest::of_bytes (before)),
    ]);
    let prepared = prepared_for (
      path . clone (), Some (b"title: after\n" . to_vec ()), &selected);
    prepared . validate_selected_fence () . unwrap ();

    std::fs::write (&path, "title: cafe\n" . as_bytes ()) . unwrap ();
    let error = prepared . validate_selected_fence () . unwrap_err ();
    let SaveError::DiskSelectionChanged { paths, .. } = error else {
      panic! ("wrong save-fence error"); };
    assert_eq! (paths, vec![path]);
  }

  #[test]
  fn unexpected_create_and_delete_both_fail_closed () {
    let temp = tempfile::tempdir () . unwrap ();
    let created = temp . path () . join ("created.skg");
    let create_prepared = prepared_for (
      created . clone (), Some (b"ours" . to_vec ()),
      &SelectedPathManifest::new ());
    std::fs::write (&created, b"theirs") . unwrap ();
    assert! (matches! (
      create_prepared . validate_selected_fence (),
      Err (SaveError::DiskSelectionChanged { .. })));

    let deleted = temp . path () . join ("deleted.skg");
    std::fs::write (&deleted, b"selected") . unwrap ();
    let selected = SelectedPathManifest::from ([
      (deleted . clone (), PathDigest::of_bytes (b"selected")),
    ]);
    let delete_prepared = prepared_for (deleted . clone (), None, &selected);
    std::fs::remove_file (&deleted) . unwrap ();
    assert! (matches! (
      delete_prepared . validate_selected_fence (),
      Err (SaveError::DiskSelectionChanged { .. })));
  }

  #[test]
  fn semantically_equal_external_bytes_still_fail_closed () {
    let temp = tempfile::tempdir () . unwrap ();
    let path = temp . path () . join ("semantic-no-op.skg");
    let selected_bytes = b"pid: node\ntitle: before\n";
    std::fs::write (&path, selected_bytes) . unwrap ();
    let selected = SelectedPathManifest::from ([
      (path . clone (), PathDigest::of_bytes (selected_bytes)),
    ]);
    let prepared = prepared_for (
      path . clone (), Some (b"pid: node\ntitle: saved\n" . to_vec ()),
      &selected);

    // This YAML parses to the same scalar values.  The save fence is about
    // selected bytes, so semantic equality cannot authorize overwriting it.
    let external = b"pid: node\ntitle: before  \n";
    std::fs::write (&path, external) . unwrap ();
    assert! (matches! (
      prepared . validate_selected_fence (),
      Err (SaveError::DiskSelectionChanged { .. })));
    assert_eq! (std::fs::read (&path) . unwrap (), external);
  }

  #[test]
  fn atomic_replacement_is_detected_by_exact_bytes () {
    let temp = tempfile::tempdir () . unwrap ();
    let path = temp . path () . join ("renamed.skg");
    let replacement = temp . path () . join ("replacement.tmp");
    std::fs::write (&path, b"selected") . unwrap ();
    let selected = SelectedPathManifest::from ([
      (path . clone (), PathDigest::of_bytes (b"selected")),
    ]);
    let prepared = prepared_for (
      path . clone (), Some (b"ours" . to_vec ()), &selected);

    std::fs::write (&replacement, b"atomic external replacement") . unwrap ();
    std::fs::rename (&replacement, &path) . unwrap ();
    let SaveError::DiskSelectionChanged { paths, .. } =
      prepared . validate_selected_fence () . unwrap_err ()
    else { panic! ("wrong save-fence error"); };
    assert_eq! (paths, vec![path]);
  }

  #[test]
  fn one_drifted_target_refuses_the_complete_prepared_batch () {
    let temp = tempfile::tempdir () . unwrap ();
    let first = temp . path () . join ("first.skg");
    let second = temp . path () . join ("second.skg");
    std::fs::write (&first, b"selected first") . unwrap ();
    std::fs::write (&second, b"selected second") . unwrap ();
    let selected = SelectedPathManifest::from ([
      (first . clone (), PathDigest::of_bytes (b"selected first")),
      (second . clone (), PathDigest::of_bytes (b"selected second")),
    ]);
    let prepared = PreparedFilesystemUpdate {
      writes: Vec::new (),
      deleted_pids: HashSet::new (),
      path_manifest: BTreeMap::from ([
        (first . clone (), PreparedPathMutation {
          expected_before: None,
          proposed_after: Some (b"ours first" . to_vec ()),
        }),
        (second . clone (), PreparedPathMutation {
          expected_before: None,
          proposed_after: Some (b"ours second" . to_vec ()),
        }),
      ]),
    } . with_selected_fence (&selected);

    std::fs::write (&second, b"external second") . unwrap ();
    let SaveError::DiskSelectionChanged { paths, .. } =
      prepared . validate_selected_fence () . unwrap_err ()
    else { panic! ("wrong save-fence error"); };
    assert_eq! (paths, vec![second . clone ()]);
    assert_eq! (std::fs::read (&first) . unwrap (), b"selected first");
    assert_eq! (std::fs::read (&second) . unwrap (), b"external second");
  }
}


/// Updates the index with the provided DefineNodes.
/// Deletes IDs from the index for every instruction,
/// but only adds documents for instructions where is_save.
/// Returns the number of documents processed.
pub(crate) fn update_tantivy_from_saveinstructions (
  instructions  : &[DefineNode],
  tantivy_index : &TantivyIndex,
  context_types : &HashMap<ID, String>, // pid -> context_origin_type label, so each doc is indexed once with its final type (no second context pass needed). Empty for the nodeMerge path.
) -> Result<usize, Box<dyn Error>> {

  let _wlock = // one IndexWriter per directory; serialize all Tantivy writers
    lock_tantivy_writes ();
  let mut writer: IndexWriter =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "tantivy_writer_create" ). entered();
      tantivy_index . index . writer (
        TANTIVY_WRITER_BUFFER_BYTES)? };
  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "tantivy_delete" ). entered();
    delete_nodes_by_id_from_index(
      // Delete all IDs, be they from Saves or Deletes.
      // (The entry for each Save is then recreated.)
      instructions . iter() . map(|instr| match instr {
        DefineNode::Save(SaveNode (node)) => &node . pid,
        DefineNode::Delete(DeleteNode { id, .. }) => id }),
      &mut writer,
      tantivy_index)? ; }
  // Add documents only for non-deletion instructions.
  // Convert to NodeTantivy (narrow) at the boundary.
  let nodes_to_add: Vec<NodeTantivy> =
    instructions . iter()
    . filter_map( |instr| match instr {
        DefineNode::Save(SaveNode (node)) =>
          Some ( NodeTantivy::from (node) ),
        DefineNode::Delete (_) => None } )
    . collect();
  let processed_count: usize =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "tantivy_add" ). entered();
      add_documents_to_tantivy_writer(
        & nodes_to_add, &mut writer, tantivy_index,
        context_types )? };
  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "tantivy_commit" ). entered();
    commit_with_status(
      &mut writer, tantivy_index, processed_count, "Updated")? ; }
  // Wait out the writer's background merge threads before returning, so
  // the index is fully quiescent: 'commit()' alone can leave merge
  // threads writing segment files, which (when this runs on the
  // background worker) would race a test's index-directory cleanup, and
  // makes 'wait_for_tantivy_writes_idle' mean what it says.
  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "tantivy_wait_merging_threads" ). entered();
    writer . wait_merging_threads ()
      . map_err ( |e| -> Box<dyn Error> { e . into () }) ? ; }
  Ok (processed_count) }
