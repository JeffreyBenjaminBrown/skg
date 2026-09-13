use crate::consts::TANTIVY_WRITER_BUFFER_BYTES;
use crate::context::context_origin_types_for_saved_from_in_rust_graph;
use crate::dbs::filesystem::one_node::{
  PreparedTelescopeWrite, prepare_nodecomplete_telescope,
};
use crate::telescope::invariants::{
  TelescopeViolation, affected_telescope_warnings,
};
use crate::dbs::in_rust_graph::{
  InRustGraph,
  InRustGraphHandle,
  prepared_update::{PreparedGraphUpdate, prepare_graph_update},
};
use crate::dbs::tantivy::background_writer::{enqueue_tantivy_write, lock_tantivy_writes, TantivyWriteTask};
use crate::dbs::tantivy::write::{add_documents_to_tantivy_writer, commit_with_status, delete_nodes_by_id_from_index};
use crate::types::env::MutationGate;
use crate::types::misc::{ID, MSV, MemberAtSource, SkgConfig, TantivyIndex};
use crate::types::errors::{BufferValidationError, SaveError};
use crate::types::nodes::rust::NodeRust;
use crate::types::nodes::tantivy::NodeTantivy;
use crate::types::save::{DefineNode, SaveNode, DeleteNode, NodeMerge, SourceMove};
use crate::types::nodes::complete::NodeComplete;

use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::io;
use std::sync::Arc;
use tantivy::IndexWriter;

/// Updates the authoritative and derived stores from prepared `DefineNode`s:
///   1) Filesystem (source of truth)
///   2) immutable in-Rust graph publication
///   3) Tantivy background update
/// Returns `None` for the ordinary queued-index path.
/// Returns `Some(new_index)` when Tantivy had to be rebuilt.
pub async fn update_graph_minus_nodeMerges (
  node_defs     : Vec<DefineNode>,
  source_moves  : &[SourceMove],
  config        : SkgConfig,
  tantivy_index : &TantivyIndex,
  graph         : &InRustGraphHandle,
  mutation_gate : &MutationGate,
) -> Result < Option<TantivyIndex>, Box<dyn Error> > {
  let _mutation_guard = mutation_gate . lock () . await;
  update_graph_minus_nodeMerges_with_hoist_approval (
    node_defs, source_moves, config, tantivy_index, graph,
    &HashSet::new () )
}

pub(crate) fn update_graph_minus_nodeMerges_with_hoist_approval (
  mut node_defs : Vec<DefineNode>,
  source_moves  : &[SourceMove],
  config        : SkgConfig,
  tantivy_index : &TantivyIndex,
  graph         : &InRustGraphHandle,
  hoist_approved_pids : &HashSet<ID>,
) -> Result < Option<TantivyIndex>, Box<dyn Error> > {
  tracing::info!("Updating filesystem, in-Rust graph, and Tantivy ...");
  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "apply_delete_propagation_cleanup" ). entered();
    let graph_snap : Arc<InRustGraph> = graph . load_full ();
    apply_delete_propagation_cleanup (
      &mut node_defs, &graph_snap, &config ); }
  let base : Arc<InRustGraph> = graph . load_full ();
  let prepared : PreparedGraphUpdate = prepare_graph_update (
    &config, base . clone (), node_defs)
    . map_err ( |error| -> Box<dyn Error> {
      error . to_string () . into () } ) ?;
  let prepared_filesystem : PreparedFilesystemUpdate = prepare_fs_update (
    prepared . definitions (), source_moves, &config, hoist_approved_pids) ?;
  let telescope_warnings : Vec<(ID, TelescopeViolation)> =
    affected_telescope_warnings (
      &config, &base, prepared . candidate (),
      prepared . saved_pids (), prepared . affected_ids ());
  let result : Result<Option<TantivyIndex>, Box<dyn Error>> =
    apply_defineNodes ( prepared,
                        prepared_filesystem,
                        config,
                        tantivy_index,
                        graph );
  if result . is_ok () {
    emit_telescope_warnings (&telescope_warnings); }
  result }

fn apply_defineNodes (
  prepared      : PreparedGraphUpdate,
  prepared_filesystem : PreparedFilesystemUpdate,
  config        : SkgConfig,
  tantivy_index : &TantivyIndex,
  graph         : &InRustGraphHandle,
) -> Result < Option<TantivyIndex>, Box<dyn Error> > {
  prepared . verify_base (graph)
    . map_err ( |message| -> Box<dyn Error> { message . into () } ) ?;

  { // FS (source of truth)
    // TODO: Print per-source write information
    tracing::info!( "Writing {} instruction(s) to disk ...",
               { let total_input : usize = prepared . definitions () . len ();
                 total_input } );
    let (deleted_count, written_count) : (usize, usize) =
      { let _span : tracing::span::EnteredSpan = tracing::info_span!(
          "update_fs_from_savenode_defs") . entered ();
        prepared_filesystem . apply (&config) } ?;
    tracing::info!( "   Deleted {} file(s), wrote {} file(s).",
              deleted_count, written_count ); }

  let (candidate, node_defs) : (Arc<InRustGraph>, Vec<DefineNode>) =
  { // In-Rust graph — atomic snapshot swap so readers see a
    // view that's consistent with what just landed on disk, and
    // never a mid-save half-applied state.
    let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "publish_prepared_graph_update") . entered ();
    prepared . publish (graph)
      . map_err ( |message| -> Box<dyn Error> { message . into () } ) ? };

  enqueue_tantivy_delta (
    &candidate, tantivy_index, node_defs );
  Ok (None) }

fn enqueue_tantivy_delta (
  candidate     : &InRustGraph,
  tantivy_index : &TantivyIndex,
  node_defs     : Vec<DefineNode>,
) {
  // Context origin types, read from the post-apply in-Rust graph, so
  // the Tantivy pass below indexes each saved doc once with its final
  // type — no separate context writer/commit. Computed here (not on the
  // Tantivy thread) so the read happens before any further mutation.
  let context_types : HashMap<ID, String> =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "context_origin_types_for_saved" ). entered();
      context_origin_types_for_saved_from_in_rust_graph (
        candidate, &node_defs ) };

  // Tantivy (background): the search index is a derived cache that the
  // save's response never reads, so enqueue the index update to commit
  // off the critical path, in FIFO order (a single worker). Searches
  // block on 'wait_for_tantivy_writes_idle' until it lands. A
  // background failure is logged, not propagated — the filesystem is
  // the source of truth, so 'rebuild ephemeral data stores' resyncs the index.
  enqueue_tantivy_write ( TantivyWriteTask {
    tantivy_index : tantivy_index . clone (),
    instructions  : node_defs,
    context_types, } );
}

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
  graph              : &InRustGraphHandle,
  mutation_gate      : &MutationGate,
  hoist_approved_pids : &HashSet<ID>,
) -> Result<HashMap<ID, HashSet<ID>>, Box<dyn Error>> {
  let _mutation_guard = mutation_gate . lock () . await;
  update_graph_including_nodeMerges_under_mutation_gate (
    save_instructions, nodeMerge_instructions, source_moves, config,
    tantivy_index, graph, hoist_approved_pids )
}

/// The combined save operation for a caller that already holds the shared
/// mutation gate.  The TCP save handler takes the gate before parsing so its
/// disk-derived SavePlan cannot become stale before this function publishes.
pub(crate) fn update_graph_including_nodeMerges_under_mutation_gate (
  mut save_instructions  : Vec<DefineNode>,
  nodeMerge_instructions : &[NodeMerge],
  source_moves       : &[SourceMove],
  config             : SkgConfig,
  tantivy_index      : &mut TantivyIndex,
  graph              : &InRustGraphHandle,
  hoist_approved_pids : &HashSet<ID>,
) -> Result<HashMap<ID, HashSet<ID>>, Box<dyn Error>> {
  let graph_before_save : Arc<InRustGraph> = graph . load_full ();
  apply_delete_propagation_cleanup (
    &mut save_instructions, &graph_before_save, &config );
  let all_filesystem_outputs : Vec<DefineNode> =
    save_instructions . iter () . cloned ()
    . chain ( nodeMerge_instructions . iter ()
             . flat_map ( |node_merge| node_merge . to_vec () ) )
    . collect ();
  crate::nodeMerge::error_unless_nodeMerge_hoist_is_approved (
    nodeMerge_instructions, &config, hoist_approved_pids ) ?;

  let prepared_save : Option<PreparedGraphUpdate> =
    if save_instructions . is_empty () { None }
    else { Some (prepare_graph_update (
      &config, graph_before_save . clone (), save_instructions)
      . map_err ( |error| -> Box<dyn Error> {
        Box::new (SaveError::BufferValidationErrors {
          errors : vec![BufferValidationError::Other (
            error . to_string ())],
          warnings : vec![],
        }) } ) ?) };
  let graph_after_save : Arc<InRustGraph> = prepared_save . as_ref ()
    . map ( |prepared| prepared . candidate () . clone () )
    . unwrap_or_else ( || graph_before_save . clone () );
  let nodeMerge_definitions : Vec<DefineNode> =
    nodeMerge_instructions . iter ()
    . flat_map ( |node_merge| node_merge . to_vec () )
    . collect ();
  let prepared_nodeMerge : Option<PreparedGraphUpdate> =
    if nodeMerge_definitions . is_empty () { None }
    else { Some (prepare_graph_update (
      &config, graph_after_save, nodeMerge_definitions)
      . map_err ( |error| -> Box<dyn Error> {
        Box::new (SaveError::BufferValidationErrors {
          errors : vec![BufferValidationError::Other (
            error . to_string ())],
          warnings : vec![],
        }) } ) ?) };
  // Prepare and retain both filesystem phases before either is consumed.
  // This is the save-level all-or-nothing preflight for fallible ownership,
  // shape, path, and serialization work.
  let prepared_save_filesystem : Option<PreparedFilesystemUpdate> =
    prepared_save . as_ref () . map (|prepared| prepare_fs_update (
      prepared . definitions (), source_moves, &config, hoist_approved_pids))
    . transpose () ?;
  let prepared_nodeMerge_filesystem : Option<PreparedFilesystemUpdate> =
    prepared_nodeMerge . as_ref () . map (|prepared| prepare_fs_update (
      prepared . definitions (), &[], &config, hoist_approved_pids))
    . transpose () ?;
  let final_candidate : &InRustGraph = prepared_nodeMerge . as_ref ()
    .map (|prepared| prepared . candidate () . as_ref ())
    .or_else (|| prepared_save . as_ref ()
      .map (|prepared| prepared . candidate () . as_ref ()))
    .unwrap_or (&graph_before_save);
  let saved_pids_for_telescope : HashSet<ID> = prepared_save . iter ()
    .chain (prepared_nodeMerge . iter ())
    .flat_map (|prepared| prepared . saved_pids () . iter () . cloned ())
    .collect ();
  let affected_ids_for_telescope : HashSet<ID> = prepared_save . iter ()
    .chain (prepared_nodeMerge . iter ())
    .flat_map (|prepared| prepared . affected_ids () . iter () . cloned ())
    .collect ();
  let telescope_warnings : Vec<(ID, TelescopeViolation)> =
    affected_telescope_warnings (
      &config, &graph_before_save, final_candidate,
      &saved_pids_for_telescope, &affected_ids_for_telescope);
  let deleted_by_this_save_extra_ids : HashMap<ID, HashSet<ID>> =
    all_filesystem_outputs . iter ()
    . filter_map ( |instruction| match instruction {
      DefineNode::Delete (delete) => graph_before_save . nodes . get (&delete . id)
        . map (|node| (delete . id . clone (),
                       node . extra_ids . iter () . cloned () . collect ())),
      _ => None })
    . collect ();
  if let Some ((prepared, prepared_filesystem)) =
    prepared_save . zip (prepared_save_filesystem)
  {
    let save_replacement : Option<TantivyIndex> =
      { let _span : tracing::span::EnteredSpan = tracing::info_span!(
          "apply_ordinary_defineNodes" ). entered();
        apply_defineNodes (
          prepared, prepared_filesystem, config . clone (),
          tantivy_index, graph ) } ?;
    if let Some (new_index) = save_replacement {
      *tantivy_index = new_index; }}
  let nodeMerge_replacement : Option<TantivyIndex> =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "apply_nodeMerge_defineNodes" ). entered();
      crate::nodeMerge::apply_prepared_nodeMerges (
        prepared_nodeMerge . zip (prepared_nodeMerge_filesystem), config,
        tantivy_index, graph ) } ?;
  if let Some (new_index) = nodeMerge_replacement {
    *tantivy_index = new_index; }
  emit_telescope_warnings (&telescope_warnings);
  Ok (deleted_by_this_save_extra_ids) }

pub(crate) fn emit_telescope_warnings (
  warnings : &[(ID, TelescopeViolation)],
) {
  for (pid, violation) in warnings {
    tracing::warn! (
      pid = %pid, violation = %violation,
      "telescope warning after save" ); }
}

/// Two-phase cleanup so deletes don't leave dangling references on
/// disk:
///
/// Foreign raw references deliberately survive: delete propagation never
/// manufactures or modifies a foreign telescope write.
///
/// Phase 1 (cleanup-SaveNode generation): for any owned node N still on
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
/// Referencers may have stored any of those ids (the graph resolves
/// aliases to their primary IDs, but the on-disk
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
  config     : &SkgConfig,
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
      graph_snap . get (p)
      . map ( |node| config . user_owns_source (&node . source) )
      . unwrap_or (false)
      && ! user_save_pids . contains (p)
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

  { // Strip deleted IDs only from owned saves. A foreign buffer instruction
    // remains intact so ordinary validation/preflight can reject the write.
    for nd in node_defs . iter_mut () {
      if let DefineNode::Save ( SaveNode (nc) ) = nd {
        if ! config . user_owns_source (&nc . source) { continue; }
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
    overPrivateText_telescope               : rust . overPrivateText_telescope,
    aliases                      : rust . aliases . clone (),
    body                         : rust . body . clone (),
    contains                     : rust . contains . clone (),
    subscribes_to                : rust . subscribes_to . clone (),
    hides_from_its_subscriptions : rust . hides_from_its_subscriptions . clone (),
    overrides_view_of            : rust . overrides_view_of . clone (),
    misc                         : rust . misc . clone (),
  }}

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
/// closed on overPrivateText disk telescopes.
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

pub(crate) struct PreparedFilesystemUpdate {
  writes       : Vec<PreparedTelescopeWrite>,
  deletions    : Vec<String>,
  deleted_pids : HashSet<ID>,
}

impl PreparedFilesystemUpdate {
  pub(crate) fn apply (
    self,
    config : &SkgConfig,
  ) -> io::Result<(usize, usize)> {
    // Mutation starts only after the whole batch has passed ownership and
    // serialization preflight.
    for path in self . deletions {
      match std::fs::remove_file (&path) {
        Ok (( ))                                          => {},
        Err (e) if e . kind () == io::ErrorKind::NotFound => {},
        Err (e)                                           => return Err (e), } }
    for telescope in &self . writes {
      telescope . apply (config) ?; }
    for telescope in &self . writes {
      telescope . verify_hoist (config) ?; }
    Ok (( self . deleted_pids . len (), self . writes . len () ))
  }
}

pub(crate) fn prepare_fs_update (
  node_defs           : &[DefineNode],
  source_moves        : &[SourceMove],
  config              : &SkgConfig,
  hoist_approved_pids : &HashSet<ID>,
) -> io::Result<PreparedFilesystemUpdate> {
  let ( to_delete, mut to_save )
    : ( Vec<DeleteNode>, Vec<SaveNode> )
    = DefineNode::partition_save_and_delete (node_defs);
  for SaveNode (node) in &mut to_save {
    node . normalize_ids (); }
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
  // memberships. (source_moves still matter to Tantivy, handled elsewhere.)
  Ok ( PreparedFilesystemUpdate {
    writes       : prepared_writes,
    deletions    : prepared_deletions,
    deleted_pids,
  } ) }


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
