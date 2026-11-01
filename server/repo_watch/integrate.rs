//! A batch of '.skg' paths that changed on disk becomes one runtime
//! generation, as a save's does: each changed node's telescope is read
//! again from every skgrepo, and the nodes that differ from the graph
//! are written to it (or deleted from it) as nodeInstructions, through
//! the save's incremental validation ('prepare_graph_update'). The
//! nodes so integrated take the new generation as their last change,
//! so the stale-save check refuses a buffer that showed them before.
//!
//! Nothing is written to disk: the files are already what the graph
//! should say. A file that cannot be read leaves its node as the graph
//! has it, with a problem in the notice; a batch that fails
//! validation leaves the whole graph as it was, and its paths come
//! back to be tried with the next batch.

use crate::dbs::filesystem::one_node::sections_of_pid_on_disk;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::prepared_update::{
  PreparedGraphUpdate, prepare_graph_update};
use crate::dbs::node_lookup::graphnode_from_graph;
use crate::git_ops::git_snapshot::forget_cached_git_snapshots;
use crate::repo_watch::own_writes::{canonical_directory, is_own_write_then_forget};
use crate::save::{emit_telescope_warnings, enqueue_tantivy_delta};
use crate::telescope::compose::compose_telescope_collecting_warnings;
use crate::telescope::invariants::{TelescopeViolation, affected_telescope_warnings};
use crate::telescope::types::{
  CompositionWarning, IllegalEdit, IllegalSections, Telescope, separate_illegal_sections};
use crate::types::env::{MutationGate, RuntimeGeneration, SkgEnv};
use crate::types::misc::{ID, SkgConfig, SkgrepoName};
use crate::types::nodes::complete::Graphnode;
use crate::types::nodes::fs::GraphnodeOnDisk;
use crate::types::save::{DeleteNode, NodeInstruction, SaveNode};

use futures::executor::block_on;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::MutexGuard;

/// What one batch did, for every connection to report
/// ('server/serve/handlers/repo_files_integrated.rs').
#[derive(Clone, Debug, Default)]
pub struct RepoChangeNotice {
  /// The generation the batch was swapped in as; None when nothing
  /// was integrated.
  pub generation : Option<u64>,
  /// The nodes the batch wrote to or deleted from the graph, or whose
  /// illegal edits it changed.
  pub changed_pids : HashSet<ID>,
  /// For each skgrepo, in config order, how many of its files the
  /// batch integrated.
  pub files_integrated : Vec<(SkgrepoName, usize)>,
  /// What could not be integrated.
  pub problems : Vec<RepoChangeProblem>,
  /// Whether a foreign skgrepo's file was integrated. The diff view
  /// reads foreign skgrepos from their worktree, so its graph
  /// straddle is then stale.
  pub foreign_files_integrated : bool,
}

/// One thing a batch could not integrate, and the skgrepos it
/// concerns, so that a connection whose skgrepo restriction hides one
/// of them is not told of it.
#[derive(Clone, Debug)]
pub struct RepoChangeProblem {
  pub skgrepos : Vec<SkgrepoName>,
  pub message  : String,
}

impl RepoChangeNotice {
  /// Whether there is anything to tell a connection.
  pub fn is_empty (
    &self,
  ) -> bool {
    self . files_integrated . is_empty () && self . problems . is_empty () }
}

/// A changed '.skg' file of a configured skgrepo.
#[derive(Clone, Debug)]
struct ChangedSkgFile {
  path    : PathBuf,
  skgrepo : SkgrepoName,
  pid     : ID,
}

/// What reading a changed node's telescope again says to do.
enum ReadChange {
  /// The node on disk differs from the graph's.
  Write (Graphnode),
  /// No section of the node is left, and the graph has it.
  Delete (DeleteNode),
  /// Disk and graph agree.
  Nothing,
}

/// Integrate the changed 'paths' (any paths; those that are not a
/// '.skg' file directly inside a configured skgrepo are ignored, and
/// so are those whose file is as the server itself last wrote it).
/// Holds the mutation gate throughout, so a batch never interleaves
/// with a save. Returns the notice and the paths to try again with
/// the next batch: those of a batch that failed validation.
pub fn integrate_changed_skg_files (
  env   : &SkgEnv,
  paths : &BTreeSet<PathBuf>,
) -> (RepoChangeNotice, BTreeSet<PathBuf>) {
  let mutation_gate : MutationGate = env . mutation_gate ();
  let _mutation_guard : MutexGuard<()> = block_on ( mutation_gate . lock () );
  let runtime : Arc<RuntimeGeneration> = env . runtime_snapshot ();
  let config : &SkgConfig = &runtime . config;
  let base : Arc<InRustGraph> = env . runtime . legacy_graph_handle () . load_full ();
  let changed_files : Vec<ChangedSkgFile> =
    changed_skg_files_of_configured_skgrepos (config, paths)
    . into_iter ()
    . filter ( |file| ! is_own_write_then_forget (&file . path) )
    . collect ();
  let mut notice : RepoChangeNotice = RepoChangeNotice::default ();
  if changed_files . is_empty () {
    return (notice, BTreeSet::new ()); }
  let files_by_pid : BTreeMap<ID, Vec<ChangedSkgFile>> = {
    let mut by_pid : BTreeMap<ID, Vec<ChangedSkgFile>> = BTreeMap::new ();
    for file in changed_files {
      by_pid . entry (file . pid . clone ()) . or_default () . push (file); }
    by_pid };
  let mut nodeInstructions : Vec<NodeInstruction> = Vec::new ();
  let mut illegal_edits : HashMap<ID, Vec<IllegalEdit>> = HashMap::new ();
  let mut integrated_pids : BTreeSet<ID> = BTreeSet::new ();
  for (pid, files) in &files_by_pid {
    match read_change_of_pid (config, &base, pid) {
      Err (message) =>
        notice . problems . push ( RepoChangeProblem {
          skgrepos : skgrepos_of (files),
          message  : format! (
            "Node {} changed on disk but could not be read, so the server keeps serving its previous version: {}",
            pid, message ), } ),
      Ok ((change, illegal)) => {
        let illegal_edits_changed : bool =
          base . illegal_edits . get (pid) . cloned () . unwrap_or_default ()
          != illegal;
        let wrote : bool = match change {
          ReadChange::Write (node) => {
            nodeInstructions . push ( NodeInstruction::Save (SaveNode (node)) );
            true },
          ReadChange::Delete (delete) => {
            nodeInstructions . push ( NodeInstruction::Delete (delete) );
            true },
          ReadChange::Nothing => false, };
        if wrote || illegal_edits_changed {
          illegal_edits . insert ( pid . clone (), illegal );
          integrated_pids . insert ( pid . clone () ); }}, }}
  if integrated_pids . is_empty () {
    return (notice, BTreeSet::new ()); }
  let prepared : PreparedGraphUpdate =
    match prepare_graph_update (config, base . clone (), nodeInstructions) {
      Ok (prepared) => prepared,
      Err (error) => {
        let retry : BTreeSet<PathBuf> = integrated_pids . iter ()
          . flat_map ( |pid| files_by_pid [pid] . iter () . map ( |file| file . path . clone () ))
          . collect ();
        notice . problems . push ( RepoChangeProblem {
          skgrepos : skgrepos_of ( integrated_pids . iter ()
                                   . flat_map ( |pid| files_by_pid [pid] . iter () )),
          message  : format! (
            "Files that changed on disk could not be integrated, so the server keeps serving its previous graph; they are tried again with the next change on disk:\n{}",
            error ), } );
        return (notice, retry); }, };
  let telescope_warnings : Vec<(ID, TelescopeViolation)> =
    affected_telescope_warnings (
      config, &base, prepared . candidate (),
      prepared . saved_pids (), prepared . affected_gnids ());
  let candidate : InRustGraph = {
    let mut candidate : InRustGraph = (**prepared . candidate ()) . clone ();
    for (pid, edits) in illegal_edits {
      if edits . is_empty () { candidate . illegal_edits . remove (&pid); }
      else { candidate . illegal_edits . insert (pid, edits); }}
    candidate };
  let swapped_in : Arc<RuntimeGeneration> = env . runtime . swap_in (
    runtime . config . clone (), Arc::new (candidate),
    runtime . tantivy_index . clone () );
  enqueue_tantivy_delta (
    &swapped_in . graph, &runtime . tantivy_index,
    prepared . nodeInstructions () . to_vec () );
  emit_telescope_warnings (&telescope_warnings);
  let integrated_files : Vec<&ChangedSkgFile> = integrated_pids . iter ()
    . flat_map ( |pid| files_by_pid [pid] . iter () )
    . collect ();
  notice . foreign_files_integrated = integrated_files . iter ()
    . any ( |file| ! config . skgrepo_is_owned (&file . skgrepo) );
  if notice . foreign_files_integrated {
    if let Err (e) = forget_cached_git_snapshots () {
      tracing::warn! ( error = %e, "could not forget the cached git snapshots" ); }}
  notice . files_integrated = config . ordered_skgrepos () . into_iter ()
    . filter_map ( |skgrepo| {
      let count : usize = integrated_files . iter ()
        . filter ( |file| file . skgrepo == skgrepo ) . count ();
      (count > 0) . then_some ((skgrepo, count)) } )
    . collect ();
  notice . changed_pids = integrated_pids . into_iter () . collect ();
  notice . generation = Some (swapped_in . generation);
  tracing::info! (
    generation = swapped_in . generation,
    nodes = notice . changed_pids . len (),
    "integrated .skg files that changed on disk" );
  (notice, BTreeSet::new ()) }

/// The '.skg' files among 'paths' that sit directly in a configured
/// skgrepo's directory, each with its skgrepo and the pid its name
/// gives.
fn changed_skg_files_of_configured_skgrepos (
  config : &SkgConfig,
  paths  : &BTreeSet<PathBuf>,
) -> Vec<ChangedSkgFile> {
  let skgrepo_of_directory : HashMap<PathBuf, SkgrepoName> =
    config . skgrepos . iter ()
    . map ( |(name, skgrepo)| ( canonical_directory (&skgrepo . path),
                                name . clone () ))
    . collect ();
  paths . iter ()
    . filter ( |path| path . extension () . is_some_and ( |ext| ext == "skg" ))
    . filter_map ( |path| {
      let skgrepo : &SkgrepoName =
        skgrepo_of_directory . get ( &canonical_directory ( path . parent () ? )) ?;
      let pid : ID = ID ( path . file_stem () ? . to_str () ? . to_string () );
      Some ( ChangedSkgFile {
        path : path . clone (), skgrepo : skgrepo . clone (), pid } ) } )
    . collect () }

/// What the node 'pid' on disk says to do to the graph, and its
/// illegal edits (the owned sections a foreign node's telescope
/// carries, which the graph records apart, as a load does).
fn read_change_of_pid (
  config : &SkgConfig,
  base   : &InRustGraph,
  pid    : &ID,
) -> Result<(ReadChange, Vec<IllegalEdit>), String> {
  let sections : Vec<(SkgrepoName, GraphnodeOnDisk)> =
    sections_of_pid_on_disk (config, pid)
    . map_err ( |e| e . to_string () ) ?;
  let in_graph : Option<Graphnode> = graphnode_from_graph (base, pid)
    . filter ( |node| &node . pid == pid )
    . map ( |mut node| { node . normalize_gnids (); node } );
  if sections . is_empty () {
    return Ok (( match in_graph {
      Some (node) => ReadChange::Delete ( DeleteNode {
        gnid : pid . clone (), home_skgrepo : node . home_skgrepo, } ),
      None => ReadChange::Nothing, },
      Vec::new () )); }
  let (sections, illegal) : (Vec<(SkgrepoName, GraphnodeOnDisk)>, Option<IllegalSections>) =
    separate_illegal_sections (sections, config);
  let illegal_edits : Vec<IllegalEdit> =
    illegal . map ( |illegal| illegal . owned ) . unwrap_or_default ();
  let telescope : Telescope = Telescope::try_new (pid . clone (), sections, config)
    . map_err ( |e| e . to_string () ) ?;
  let resolve = |gnid : &ID| -> ID {
    base . pid_of (gnid) . unwrap_or_else ( || gnid . clone () ) };
  let (mut node, composition_warnings) : (Graphnode, Vec<CompositionWarning>) =
    compose_telescope_collecting_warnings (telescope, &resolve)
    . map_err ( |e| e . to_string () ) ?;
  for warning in &composition_warnings {
    tracing::warn! ( pid = %pid, warning = %warning,
                     "telescope composition warning in a file changed on disk" ); }
  node . normalize_gnids ();
  let change : ReadChange =
    if in_graph . as_ref () == Some (&node) { ReadChange::Nothing }
    else { ReadChange::Write (node) };
  Ok ((change, illegal_edits)) }

/// The distinct skgrepos of 'files', in the order first met.
fn skgrepos_of<'a> (
  files : impl IntoIterator<Item = &'a ChangedSkgFile>,
) -> Vec<SkgrepoName> {
  let mut skgrepos : Vec<SkgrepoName> = Vec::new ();
  for file in files {
    if ! skgrepos . contains (&file . skgrepo) {
      skgrepos . push ( file . skgrepo . clone () ); }}
  skgrepos }
