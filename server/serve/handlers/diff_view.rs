//! The "diff view" request: one buffer of the diff view
//! ('docs/diff-view.org', "The four buffers").
//! The client sends one request per buffer, in order, and opens each
//! one the server does not answer with '(empty . t)'. A buffer's answer
//! carries its readiness summary, as a save's does.

use crate::diff_view::change_leaves::{BufferReadinessSummary, readiness_response_sexp};
use crate::diff_view::readiness_requests::buffer_readiness_summary_for;
use crate::graph_straddle::build::current_graph_straddle_in_slot;
use crate::graph_straddle::types::GraphStraddle;
use crate::diff_view::buffer_rerender::{
  RenderedDiffViewBuffer, diffView_buffer_viewforest};
use crate::diff_view::pruned_change_view::prunedChangeView_sets_of_buffer;
use crate::diff_view::readiness_store::ReadinessStore;
use crate::serve::ViewsState;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  buffer_response_entries, format_buffer_response_sexp, rendered_at_entry,
  send_response_with_length_prefix, sexp_to_client_string, tag_sexp_response,
  true_entry_sexp, value_from_request_sexp, viewforest_id_from_request};
use crate::skgrepo_sets::{SkgrepoRestriction, require_unrestricted};
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::types::misc::ID;
use crate::types::views_state::{DiffViewBufferKind, ViewforestId};

use sexp::Sexp;
use std::net::TcpStream;
use std::sync::{Arc, MutexGuard};

/// The named buffer, drawn, with its readiness summary.
type DiffViewBufferWithSummary =
  (RenderedDiffViewBuffer, BufferReadinessSummary);

/// Answer a "diff view" request with the named buffer and its
/// readiness summary, register it under the request's view-id, or
/// answer '(empty . t)'. Refused
/// unless the connection is unrestricted, and when the graph straddle
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
  let response : Result<(Option<DiffViewBufferWithSummary>, DiffViewBufferKind, ViewforestId), String> =
    ( || {
      require_unrestricted ("The diff view", skgrepo_restriction) ?;
      let viewforest_id : ViewforestId = viewforest_id_from_request (request) ?;
      let buffer_name : String = value_from_request_sexp ("buffer", request) ?;
      let diffView_buffer_kind : DiffViewBufferKind = DiffViewBufferKind::from_wire_name (&buffer_name)
        . ok_or_else ( || format! (
          "Unknown diff view buffer: {}", buffer_name )) ?;
      let straddle : Arc<GraphStraddle> = current_graph_straddle_in_slot (
        &mut views_state . graph_straddle, &runtime . config,
        runtime . graph . clone () )
        . map_err ( |e| format! ( "Cannot open the diff view: {}", e )) ?;
      let buffer : Option<RenderedDiffViewBuffer> = {
        let store : MutexGuard<ReadinessStore> =
          env . runtime . lock_readiness_store ();
        diffView_buffer (&runtime, &straddle, &store, diffView_buffer_kind)
        . map_err ( |e| format! ( "Error generating the diff view: {}", e )) ? };
      let buffer_with_summary : Option<DiffViewBufferWithSummary> =
        buffer . map ( |buffer| {
          let summary : BufferReadinessSummary = // locks the store itself
            buffer_readiness_summary_for (env, &straddle, &buffer . viewforest);
          (buffer, summary) } );
      Ok (( buffer_with_summary, diffView_buffer_kind, viewforest_id )) } ) ();
  let sexp : String = match response {
    Err (error) =>
      format_buffer_response_sexp ( "", &[ error ], &[] ),
    Ok (( None, _, _ )) =>
      sexp_to_client_string ( &readiness_response_sexp (
        vec! [ true_entry_sexp ("empty") ], None, &[], &[] )),
    Ok (( Some (( RenderedDiffViewBuffer { text, pids, viewforest }, summary )),
          diffView_buffer_kind, viewforest_id )) => {
      views_state . open_views . register_diffView_view (
        &runtime . graph, viewforest_id, viewforest, &pids, diffView_buffer_kind );
      diffView_buffer_response_sexp ( &text, runtime . generation, &summary ) }, };
  send_response_with_length_prefix (
    stream, &tag_sexp_response (TcpToClient::DiffView, &sexp) ); }

/// ((content "ORG") (errors ()) (warnings ()) (rendered-at "N")
///  (readiness-summary ...)): a view response, ending as a save's does
/// when it saves a buffer of the diff view.
fn diffView_buffer_response_sexp (
  text        : &str,
  rendered_at : u64,
  summary     : &BufferReadinessSummary,
) -> String {
  let mut entries : Vec<Sexp> = buffer_response_entries (text, &[], &[]);
  entries . push ( rendered_at_entry (rendered_at) );
  entries . push ( summary . to_sexp () );
  sexp_to_client_string ( &Sexp::List (entries) ) }

/// The named buffer, rendered from 'straddle'. The "all changes" buffer is
/// the pruned change view over all of C; the merges and skgrepo-changes
/// buffers are pruned change views over subsets of it
/// ('prunedChangeView_sets_of_buffer'). The order-only buffer lists the
/// nodes whose only change is the order of a list, in ID order. None
/// when the buffer would be empty.
pub fn diffView_buffer (
  runtime              : &RuntimeGeneration,
  straddle             : &GraphStraddle,
  store                : &ReadinessStore,
  diffView_buffer_kind : DiffViewBufferKind,
) -> Result<Option<RenderedDiffViewBuffer>, Box<dyn std::error::Error>> {
  let roots : Vec<ID> = match diffView_buffer_kind {
    DiffViewBufferKind::OrderOnlyChanges =>
      straddle . order_only_pids () . iter () . cloned () . collect (),
    _ => prunedChangeView_sets_of_buffer (straddle, diffView_buffer_kind)
      . d . iter () . cloned () . collect (), };
  if roots . is_empty () { return Ok (None); }
  Ok ( Some ( diffView_buffer_viewforest (
    runtime, straddle, diffView_buffer_kind, store, &roots ) ? )) }
