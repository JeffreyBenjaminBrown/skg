use crate::prominence::{
  compute_and_store_prominence_sources,
  content_maps_from_nodes,
  had_id_set_from_nodes,
  mentioned_gnids_from_nodes};
use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_skgrepos_collecting_violations;
use crate::dbs::filesystem::not_nodes::load_config;
use crate::dbs::init::rebuild_tantivy_as_generation;
use crate::git_ops::git_snapshot::forget_cached_git_snapshots;
use crate::telescope::invariants::{TelescopeViolation, report_telescope_violations};
use crate::telescope::dependencies_manifest::{
  foreign_section_warnings, write_dependencies_manifests};
use crate::dbs::in_rust_graph::complete_validation::validated_graph;
use crate::types::env::SkgEnv;
use crate::serve::ViewsState;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{send_response_with_length_prefix, tag_text_response};
use crate::types::misc::{ID, SkgConfig, TantivyIndex};
use crate::types::nodes::complete::Graphnode;

use futures::executor::block_on;
use std::net::TcpStream;
use std::sync::Arc;

pub fn handle_rebuild_session_lifetime_stores_request (
  stream     : &mut TcpStream,
  env        : &mut SkgEnv,
  views_state : &mut ViewsState,
) {
  let result : Result<Vec<String>, String> =
    rebuild_session_lifetime_stores_in_place (env, views_state);
  let msg : String = match result {
    Ok (warnings) => format! (
      "{}{}",
      concat! (
        "Config reloaded; session-lifetime graph and Tantivy index ",
        "rebuilt successfully; .skg files remain authoritative."),
      warnings . iter ()
        . map ( |warning| format! ("\nWarning: {}", warning) )
        . collect::<String> () ),
    Err (e) => {
      tracing::error!("Rebuild failed: {}", e);
      format! ("Rebuild failed: {}", e) } };
  send_response_with_length_prefix (
    stream,
    & tag_text_response (
      TcpToClient::RebuildSessionLifetimeStores, &msg )); }

/// The rebuild itself, streamless, so other handlers (the
/// telescope migration) can rebuild after rewriting files. It re-reads
/// the config file too, so an edited config (a changed skgrepo list,
/// say) takes effect without a restart; an invalid one fails the
/// rebuild, leaving the old generation live. On success, returns the
/// reload's warnings: the config's unknown keys, the foreign manifests
/// it contradicts, and any changed setting that only a restart applies.
pub fn rebuild_session_lifetime_stores_in_place (
  env         : &mut SkgEnv,
  views_state : &mut ViewsState,
) -> Result<Vec<String>, String> {
  tracing::info!("Rebuilding session-lifetime stores from .skg files...");
  // Rebuild is an authoritative swap-in just like a save.  Clone the Arc
  // before locking so the guard does not borrow `env` while this function
  // replaces its config/search-index fields below.
  let mutation_gate = env . mutation_gate ();
  let _mutation_guard = block_on ( mutation_gate . lock () );
  let runtime = env . runtime_snapshot ();
  // Let any in-flight background search-index writes finish before we wipe
  // and rebuild the search index out from under them.
  crate::dbs::tantivy::background_writer::wait_for_tantivy_writes_idle ();
  let result : Result<Vec<String>, String> = (|| {
    let config_path : String =
      runtime . config . config_path . display () . to_string ();
    let (fresh_config, config_warnings) : (SkgConfig, Vec<String>) =
      // The server's startup warnings stay as they were; the reload's go into its result.
      load_config (&config_path)
      . map_err ( |e| format! (
        "Reloading config from {}: {}", config_path, e) ) ?;
    let (nodes, load_violations)
      : (Vec<Graphnode>, Vec<(ID, TelescopeViolation)>) =
      read_all_skg_files_from_skgrepos_collecting_violations (&fresh_config)
      . map_err ( |e| format! ("Reading .skg files: {}", e) ) ?;
    let (mut fresh_graph, mut graph_warnings) =
      validated_graph (&fresh_config, &nodes)
      . map_err (|e| format! ("Complete graph validation failed:\n{}", e)) ?;
    fresh_graph . record_illegal_edits (&load_violations);
    graph_warnings . extend (load_violations);
    graph_warnings . sort_by (|(pid_a, a), (pid_b, b)|
      pid_a . cmp (pid_b)
        . then_with (|| a . to_string () . cmp (&b . to_string ())));
    report_telescope_violations (
      &graph_warnings, &fresh_config . data_root )
      . map_err (|e| format! ("Writing telescope warnings: {}", e)) ?;
    let (fresh_config, new_tantivy) : (SkgConfig, TantivyIndex) =
      rebuild_tantivy_as_generation (
        &fresh_config, &nodes, runtime . generation + 1)
      . map_err ( |e| format! ("Tantivy rebuild failed: {}", e) ) ?;
    tracing::info!("Session-lifetime graph candidate and Tantivy rebuilt.");
    let had_id_set = had_id_set_from_nodes (&nodes);
    let all_node_gnids = nodes . iter ()
      . map ( |n| n . pid . clone () )
      . collect ();
    let mentioned_gnids = mentioned_gnids_from_nodes (&nodes);
    let (map_to_content, map_to_containers) =
      content_maps_from_nodes (&nodes);
    compute_and_store_prominence_sources (
      &new_tantivy, &had_id_set, &all_node_gnids,
      &mentioned_gnids, &map_to_content, &map_to_containers )
      . map_err ( |e| format! ("Context computation failed: {}", e) ) ?;
    tracing::info!("Context rankings recomputed.");
    forget_cached_git_snapshots () ?;
    let restart_only_warnings : Vec<String> =
      settings_only_a_restart_applies (&runtime . config, &fresh_config);
    let fresh_config : Arc<SkgConfig> = Arc::new (fresh_config);
    { // Swap in config, graph, and replacement search index as one generation.
      env . runtime . swap_in (
        fresh_config . clone (), Arc::new (fresh_graph), new_tantivy);
      tracing::info!("In-Rust graph rebuilt."); }
    if let Err (e) = write_dependencies_manifests (&fresh_config) {
      // As at startup: the manifests are advisory, so failing to write them fails nothing.
      tracing::warn! (
        error = %e, "could not write OTHER_SKGREPO_DEPENDENCIES.toml manifests" ); }
    let warnings : Vec<String> =
      config_warnings . into_iter ()
      . chain ( foreign_section_warnings (&fresh_config) )
      . chain ( restart_only_warnings )
      . collect ();
    for warning in &warnings {
      tracing::warn! ("{}", warning); }
    Ok (warnings)
  })();
  if result . is_ok () {
    views_state . open_views . clear ();
    // The next request of the diff view rebuilds both halves
    // of its straddle from the disk state the rebuilt live graph was
    // read from.
    views_state . graph_straddle = None; }
  result }

/// The settings 'fresh' changes from 'live' that the running server
/// cannot apply: it listens on one port and sets up its logs once.
fn settings_only_a_restart_applies (
  live  : &SkgConfig,
  fresh : &SkgConfig,
) -> Vec<String> {
  let mut warnings : Vec<String> = Vec::new ();
  if fresh . port != live . port {
    warnings . push ( format! (
      "The config's port is now {}, but the server keeps listening on {} until it restarts.",
      fresh . port, live . port )); }
  if fresh . timing_log != live . timing_log {
    warnings . push ( format! (
      "The config's timing_log is now {}, which takes effect when the server restarts.",
      fresh . timing_log )); }
  warnings }
