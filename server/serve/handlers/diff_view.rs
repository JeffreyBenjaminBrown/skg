//! The "diff view" request: one buffer of the diff view
//! ('docs/diff-view.org', "The four buffers").
//! The client sends one request per buffer, in order, and opens each
//! one the server does not answer with '(empty . t)'.

use crate::diff_view::change_leaves::readiness_response_sexp;
use crate::graph_straddle::build::current_graph_straddle_in_slot;
use crate::graph_straddle::types::GraphStraddle;
use crate::diff_view::pcsvf::{DiffViewSource, DrawnDiffViewBuffer, diffView_buffer_viewforest};
use crate::diff_view::readiness_store::ReadinessStore;
use crate::serve::ViewsState;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  format_buffer_response_sexp, send_response_with_length_prefix,
  sexp_to_client_string, tag_sexp_response, true_entry_sexp,
  value_from_request_sexp, view_id_from_request};
use crate::skgrepo_sets::{SkgrepoRestriction, require_skgrepo_set_all};
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::types::misc::ID;
use crate::types::views_state::{DiffViewBufferKind, ViewId};

use std::net::TcpStream;
use std::sync::{Arc, MutexGuard};

/// Answer a "diff view" request with the named buffer, register
/// it under the request's view-id, or answer '(empty . t)'. Refused
/// unless the skgrepo restriction is 'all', and when the graph straddle
/// cannot be built (an owned skgrepo outside git, or without a HEAD
/// commit).
pub fn handle_diffView_request (
  stream              : &mut TcpStream,
  request             : &str,
  env                 : &SkgEnv,
  views_state         : &mut ViewsState,
  skgrepo_restriction : &SkgrepoRestriction,
) {
  let runtime : Arc<RuntimeGeneration> = env . runtime_snapshot ();
  let response : Result<(Option<DrawnDiffViewBuffer>, DiffViewBufferKind, ViewId), String> =
    ( || {
      require_skgrepo_set_all ("The diff view", skgrepo_restriction) ?;
      let view_id : ViewId = view_id_from_request (request) ?;
      let buffer_name : String = value_from_request_sexp ("buffer", request) ?;
      let diffView_buffer_kind : DiffViewBufferKind = DiffViewBufferKind::from_wire_name (&buffer_name)
        . ok_or_else ( || format! (
          "Unknown diff view buffer: {}", buffer_name )) ?;
      let straddle : Arc<GraphStraddle> = current_graph_straddle_in_slot (
        &mut views_state . graph_straddle, &runtime . config,
        runtime . graph . clone () )
        . map_err ( |e| format! ( "Cannot open the diff view: {}", e )) ?;
      let store : MutexGuard<ReadinessStore> =
        env . runtime . lock_readiness_store ();
      let buffer : Option<DrawnDiffViewBuffer> =
        diffView_buffer (&runtime, &straddle, &store, diffView_buffer_kind)
        . map_err ( |e| format! ( "Error generating the diff view: {}", e )) ?;
      Ok (( buffer, diffView_buffer_kind, view_id )) } ) ();
  let sexp : String = match response {
    Err (error) =>
      format_buffer_response_sexp ( "", &[ error ], &[] ),
    Ok (( None, _, _ )) =>
      sexp_to_client_string ( &readiness_response_sexp (
        vec! [ true_entry_sexp ("empty") ], None, &[], &[] )),
    Ok (( Some (DrawnDiffViewBuffer { text, pids, viewforest }),
          diffView_buffer_kind, view_id )) => {
      views_state . open_views . register_diffView_view (
        &runtime . graph, view_id, viewforest, &pids, diffView_buffer_kind );
      format_buffer_response_sexp ( &text, &[], &[] ) }, };
  send_response_with_length_prefix (
    stream, &tag_sexp_response (TcpToClient::DiffView, &sexp) ); }

/// The named buffer, drawn from 'straddle'. The "all changes" buffer is
/// the PCSVF over all of C; the merges and skgrepo-changes buffers
/// are PCSVFs over subsets of it ('GraphStraddle::pcsvf_sets_of_buffer').
/// The order-only buffer lists the nodes whose only change is the
/// order of a list, in ID order. None when the buffer would be empty.
pub fn diffView_buffer (
  runtime              : &RuntimeGeneration,
  straddle             : &GraphStraddle,
  store                : &ReadinessStore,
  diffView_buffer_kind : DiffViewBufferKind,
) -> Result<Option<DrawnDiffViewBuffer>, Box<dyn std::error::Error>> {
  let drawing : DiffViewSource = DiffViewSource { straddle, diffView_buffer_kind };
  let roots : Vec<ID> = match diffView_buffer_kind {
    DiffViewBufferKind::OrderOnlyChanges =>
      straddle . order_only_pids () . iter () . cloned () . collect (),
    _ => drawing . sets () . d . iter () . cloned () . collect (), };
  if roots . is_empty () { return Ok (None); }
  Ok ( Some ( diffView_buffer_viewforest (runtime, drawing, store, &roots) ? )) }
