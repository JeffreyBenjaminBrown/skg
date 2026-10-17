//! The "total diff view" request: one buffer of the total diff view
//! ('TODO/total-diff-view/2_plan.org', "One command, four buffers").
//! The client sends one request per buffer, in order, and opens each
//! one the server does not answer with '(empty . t)'.

use crate::graph_pair::build::current_graph_pair_in_slot;
use crate::graph_pair::types::GraphPair;
use crate::pcsvf::{TdvDrawing, pcsvf_viewforest};
use crate::readiness_store::ReadinessStore;
use crate::serve::ViewsState;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  format_buffer_response_sexp, send_response_with_length_prefix,
  tag_sexp_response, value_from_request_sexp, view_id_from_request};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::types::misc::ID;
use crate::types::tree::forest::ViewForest;
use crate::types::views_state::{TdvBuffer, ViewId};

use std::net::TcpStream;
use std::sync::{Arc, MutexGuard};

/// What one buffer of the total diff view shows.
pub enum TotalDiffViewBuffer {
  Empty,
  Content { text : String, pids : Vec<ID>, viewforest : ViewForest },
}

/// Answer a "total diff view" request with the named buffer, register
/// it under the request's view-id, or answer '(empty . t)'. Refused
/// unless the skgrepo restriction is 'all', and when the graph pair
/// cannot be built (an owned skgrepo outside git, or without a HEAD
/// commit).
pub fn handle_total_diff_view_request (
  stream              : &mut TcpStream,
  request             : &str,
  env                 : &SkgEnv,
  views_state         : &mut ViewsState,
  skgrepo_restriction : &SkgrepoRestriction,
) {
  let runtime : Arc<RuntimeGeneration> = env . runtime_snapshot ();
  let response : Result<(TotalDiffViewBuffer, TdvBuffer, ViewId), String> =
    ( || {
      if ! skgrepo_restriction . is_all () {
        return Err ( format! (
          "The total diff view requires no skgrepo restriction (the skgrepo-set all); current skgrepo restriction is {}. Switch the repo-set to all first.",
          skgrepo_restriction . name )); }
      let view_id : ViewId = view_id_from_request (request) ?;
      let buffer_name : String = value_from_request_sexp ("buffer", request) ?;
      let tdv_buffer : TdvBuffer = TdvBuffer::from_wire_name (&buffer_name)
        . ok_or_else ( || format! (
          "Unknown total diff view buffer: {}", buffer_name )) ?;
      let pair : Arc<GraphPair> = current_graph_pair_in_slot (
        &mut views_state . graph_pair, &runtime . config,
        runtime . graph . clone () ) ?;
      let store : MutexGuard<ReadinessStore> =
        env . runtime . readiness_store . lock ()
        . unwrap_or_else ( |poisoned| poisoned . into_inner () );
      let buffer : TotalDiffViewBuffer =
        total_diff_view_buffer (&runtime, &pair, &store, tdv_buffer)
        . map_err ( |e| format! ( "Error generating the total diff view: {}", e )) ?;
      Ok (( buffer, tdv_buffer, view_id )) } ) ();
  let sexp : String = match response {
    Err (error) =>
      format_buffer_response_sexp ( "", &[ error ], &[] ),
    Ok (( TotalDiffViewBuffer::Empty, _, _ )) =>
      "((empty . t) (errors ()) (warnings ()))" . to_string (),
    Ok (( TotalDiffViewBuffer::Content { text, pids, viewforest },
          tdv_buffer, view_id )) => {
      views_state . open_views . register_tdv_view (
        &runtime . graph, view_id, viewforest, &pids, tdv_buffer );
      format_buffer_response_sexp ( &text, &[], &[] ) }, };
  send_response_with_length_prefix (
    stream, &tag_sexp_response (TcpToClient::TotalDiffView, &sexp) ); }

/// The named buffer, drawn from 'pair'. The "all changes" buffer is
/// the PCSVF over all of C; the merges and skgrepo-changes buffers
/// are PCSVFs over subsets of it ('GraphPair::pcsvf_sets_of_buffer').
/// The order-only buffer lists the nodes whose only change is the
/// order of a list, in ID order.
pub fn total_diff_view_buffer (
  runtime    : &RuntimeGeneration,
  pair       : &GraphPair,
  store      : &ReadinessStore,
  tdv_buffer : TdvBuffer,
) -> Result<TotalDiffViewBuffer, Box<dyn std::error::Error>> {
  let drawing : TdvDrawing = TdvDrawing { pair, tdv_buffer };
  let roots : Vec<ID> = match tdv_buffer {
    TdvBuffer::OrderOnlyChanges =>
      pair . order_only_pids () . iter () . cloned () . collect (),
    _ => drawing . sets () . d . iter () . cloned () . collect (), };
  if roots . is_empty () { return Ok (TotalDiffViewBuffer::Empty); }
  let (text, pids, viewforest) : (String, Vec<ID>, ViewForest) =
    pcsvf_viewforest (runtime, drawing, store, &roots) ?;
  Ok ( TotalDiffViewBuffer::Content { text, pids, viewforest } ) }
