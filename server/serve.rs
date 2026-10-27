pub mod handlers;
pub mod parse_metadata_sexp;
pub mod protocol;
pub mod rerender_stream;
pub mod util;

use crate::graph_straddle::types::GraphStraddle;
use crate::serve::handlers::close_view::handle_close_view_request;
use crate::serve::handlers::delete_references_to_absent_node::handle_delete_references_to_absent_node_request;
use crate::serve::handlers::relRepo_info::handle_relRepo_info_request;
use crate::serve::handlers::flag_state::handle_flag_state_request;
use crate::serve::handlers::export_to_org::handle_export_to_org_request;
use crate::serve::handlers::import_md_and_org::{
  PendingImport, handle_import_md_and_org_request};
use crate::serve::handlers::get_file_path::handle_get_file_path_request_with_skgrepo_set;
use crate::serve::handlers::herald_rules::handle_herald_rules_request;
use crate::serve::handlers::investigate_unknown_node::handle_investigate_unknown_node_request;
use crate::serve::handlers::rebuild_session_lifetime_stores::handle_rebuild_session_lifetime_stores_request;
use crate::serve::handlers::rerender_all_views::handle_rerender_all_views_request;
use crate::serve::handlers::save_buffer::handle_save_buffer_request;
use crate::serve::handlers::server_warnings::handle_server_warnings_request;
use crate::serve::handlers::single_root_view::handle_single_root_view_request;
use crate::serve::handlers::skgrepo_sets::handle_skgrepo_set_request;
use crate::serve::handlers::integrate_unshown_changes::handle_integrate_unshown_changes_request;
use crate::serve::handlers::stage_and_commit::handle_stage_and_commit_request;
use crate::serve::handlers::strip_body_whitespace::handle_strip_body_whitespace_request;
use crate::serve::handlers::text_search::handle_text_search_request;
use crate::serve::handlers::titles_by_gnids::handle_titles_by_gnids_request_with_skgrepo_set;
use crate::serve::handlers::diff_view::handle_diffView_request;
use crate::serve::handlers::link_statuses::handle_link_statuses_request;
use crate::serve::protocol::{RequestType, TcpToClient};
use crate::serve::util::{ request_type_from_request, send_response_with_length_prefix, tag_text_response};
use crate::types::env::SkgEnv;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::misc::SkgrepoSetName;
use crate::types::views_state::OpenViews;

use std::io::{BufRead, BufReader};
use std::net::SocketAddr;
use std::net::TcpListener;
use std::net::TcpStream; // handles two-way communication
use std::sync::Arc;
use std::thread;
use std::time::Duration;

/// Per-connection state for the Emacs client. The in-Rust graph
/// handle is in 'SkgEnv', which is also per-connection (cloned at
/// connection acceptance), so it doesn't appear here.
pub struct ViewsState {
  pub open_views        : OpenViews,
  // Built when a diff-view buffer is requested, and brought up
  // to date by 'current_graph_straddle_in_slot'.
  pub graph_straddle    : Option<Arc<GraphStraddle>>,
  // If Emacs crashes or the TCP connection drops without sending close-view messages, OpenViews is still freed, because ViewsState is owned by handle_emacs and dropped when the connection loop exits (n == 0). There's no leak.
}

/// Pipes TCP input from Emacs into handle_emacs.
pub fn serve (
  env            : SkgEnv,
  emacs_listener : TcpListener,
) -> std::io::Result<()> {

  for stream_res in emacs_listener . incoming() { // the loop
    match stream_res {
      Ok (stream) => {
        let stream : TcpStream = stream; // for type sig
        let env_clone : SkgEnv = env . clone (); // Cloning permits the main thread to keep the env. If it were moved here, ownership would transfer into the first spawned thread, making it unavailable for the next connection.
        thread::spawn ( move || {
          handle_emacs (stream, env_clone) } ); }
      Err (e) => {
        tracing::error!(error = %e, "Connection failed"); }} }
  Ok (( )) }

/// This function directs requests from the stream to one of
///   handle_sexp_document_request
///   handle_text_search_request
/// API: See docs/api-and-formats.org
fn handle_emacs (
  mut stream : TcpStream,
  mut env    : SkgEnv,
) {
  let runtime = env . runtime_snapshot ();
  let mut views_state : ViewsState =
    ViewsState {
      open_views        : OpenViews::new (),
      graph_straddle    : None, };
  let mut skgrepo_restriction : SkgrepoRestriction =
    SkgrepoRestriction::default_from_config (
      &runtime . config )
      . unwrap_or_else ( |e| {
        tracing::error! (
          error = %e,
          "failed to initialize skgrepo restriction; falling back to all");
        SkgrepoRestriction::named (
          &runtime . config,
          SkgrepoSetName::from ("all"))
        . expect ("reserved repo-set all should always resolve") });

  let mut pending_import : Option<PendingImport> = None;

  let peer : SocketAddr =
    stream . peer_addr() . unwrap();
  tracing::info!(peer = %peer, "Emacs connected");
  stream . set_read_timeout (
    Some ( Duration::from_millis (100) ))
    . expect ("set_read_timeout failed");
  let mut reader
    : BufReader<TcpStream> // the underlying stream, but buffered
    = BufReader::new (
      stream . try_clone() . unwrap() );
  let mut request_header : String = String::new();
  loop {
    match reader . read_line (&mut request_header) {
      Ok (0) => break, // emacs disconnected
      Ok (_n) => {
        tracing::info! ( request = request_header . trim_end (), "Received request" );
        let runtime = env . runtime_snapshot ();
        match request_type_from_request (&request_header) {
          // For most types of requests, the header is the entire request, and the reader is no longer needed. For saving, though, the reader still contains the buffer content, so it is passed along.
          Ok (RequestType::SingleRootContentView) =>
            handle_single_root_view_request (
              &mut stream,
              &request_header,
              &env,
              &mut views_state,
              &skgrepo_restriction ),
          Ok (RequestType::SaveBuffer) =>
            // PITFALL: Uses the same BufReader that read the request,
            // so that any already-buffered header/payload are visible.
            handle_save_buffer_request (
              &mut reader,
              &mut stream,
              &request_header,
              &mut env,
              &mut views_state,
              &skgrepo_restriction ),
          Ok (RequestType::CloseView) =>
            handle_close_view_request (
              &mut stream,
              &request_header,
              &mut views_state ),
          Ok (RequestType::DeleteReferencesToAbsentNode) =>
            handle_delete_references_to_absent_node_request (
              &mut stream, &request_header, &mut env, &mut views_state,
              &skgrepo_restriction ),
          Ok (RequestType::TextSearch) =>
            handle_text_search_request (
              &mut stream,
              &request_header,
              &env,
              &mut views_state,
              &skgrepo_restriction ),
          Ok (RequestType::VerifyConnection) =>
            handle_verify_connection_request (
              &mut stream ),
          Ok (RequestType::Shutdown) =>
            // Never returns - exits process
            handle_shutdown_request ( &mut stream, &env ),
          Ok (RequestType::GetFilePath) =>
            handle_get_file_path_request_with_skgrepo_set ( &mut stream,
                                           &request_header,
                                           &runtime . config,
                                           &skgrepo_restriction ),
          Ok (RequestType::TitlesByIds) =>
            handle_titles_by_gnids_request_with_skgrepo_set (
              &mut stream, &request_header,
              &runtime . tantivy_index, &runtime . config,
              &skgrepo_restriction,
              &runtime . graph ),
          Ok (RequestType::LinkStatuses) =>
            handle_link_statuses_request (
              &mut stream, &request_header, &runtime . graph,
              &runtime . config, &skgrepo_restriction ),
          Ok (RequestType::InvestigateUnknownNode) =>
            handle_investigate_unknown_node_request (
              &mut stream, &request_header, &runtime,
              &skgrepo_restriction ),
          Ok (RequestType::RelRepoInfo) =>
            handle_relRepo_info_request (
              &mut stream, &request_header, &env ),
          Ok (RequestType::FlagState) =>
            handle_flag_state_request (
              &mut stream, &request_header, &env ),
          Ok (RequestType::ListSkgrepoSets)
          | Ok (RequestType::SkgrepoRestriction)
          | Ok (RequestType::SetSkgrepoRestriction) =>
            handle_skgrepo_set_request (
              &mut stream,
              &request_header,
              &env,
              &mut views_state,
              &mut skgrepo_restriction ),
          Ok (RequestType::DiffView) =>
            handle_diffView_request (
              &mut stream, &request_header, &env, &mut views_state,
              &skgrepo_restriction ),
          Ok (RequestType::StageAndCommit) =>
            handle_stage_and_commit_request (
              &mut stream, &request_header, &env, &mut views_state,
              &skgrepo_restriction ),
          Ok (RequestType::IntegrateUnshownChanges) =>
            handle_integrate_unshown_changes_request (
              &mut stream, &request_header, &env, &mut views_state,
              &skgrepo_restriction ),
          Ok (RequestType::HeraldRules) =>
            handle_herald_rules_request ( &mut stream, &runtime . config ),
          Ok (RequestType::ServerWarnings) =>
            handle_server_warnings_request (
              &mut stream, env . runtime . startup_warnings () ),
          Ok (RequestType::ExportToOrg) =>
            handle_export_to_org_request ( &mut stream,
                                           &runtime . config,
                                           &request_header ),
          Ok (RequestType::ImportMdAndOrg) =>
            handle_import_md_and_org_request (
              &mut stream, &request_header, &env, &mut pending_import ),
          Ok (RequestType::RebuildSessionLifetimeStores) =>
            handle_rebuild_session_lifetime_stores_request ( &mut stream,
                                         &mut env,
                                         &mut views_state ),
          Ok (RequestType::StripBodyWhitespace) =>
            handle_strip_body_whitespace_request ( &mut stream,
                                                   &mut env ),
          Ok (RequestType::RerenderAllViews) =>
            handle_rerender_all_views_request (
              &mut stream,
              &request_header,
              &env,
              &mut views_state,
              &skgrepo_restriction ),
          Err (err) => {
            tracing::error!(error = %err, "Error determining request type");
            send_response_with_length_prefix (
              &mut stream,
              & tag_text_response (
                TcpToClient::Error,
                & format! (
                  "Error determining request type: {}",
                  err ))); } };
        request_header . clear(); }
      Err (ref e)
        if e . kind () == std::io::ErrorKind::WouldBlock
        || e . kind () == std::io::ErrorKind::TimedOut =>
        {}, // The read timed out; no request has arrived yet.
      Err (_) => break, // real error
    }}
  tracing::info!(peer = %peer, "Emacs disconnected"); }

fn handle_verify_connection_request (
  stream: &mut std::net::TcpStream) {
  send_response_with_length_prefix (
    stream,
    & tag_text_response (
      TcpToClient::VerifyConnection,
      "This is the skg server verifying the connection." )); }

fn handle_shutdown_request (
  stream : &mut std::net::TcpStream,
  env    : &SkgEnv,
) {
  send_response_with_length_prefix (
    stream,
    & tag_text_response (
      TcpToClient::Shutdown, "Server shutting down..." ));
  cleanup_and_shutdown (env); }

/// Finish the process. Authoritative files are already durable and derived
/// stores require no shutdown-time cleanup.
fn cleanup_and_shutdown (_env : &SkgEnv) {
  tracing::info! ("Shutdown complete.");
  std::process::exit (0); }
