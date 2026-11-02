//! The "extend diff view" request
//! ('docs/diff-view.org', "Extending the diff view"): rerender one buffer of the diff view so
//! that every node of its recomputed U has an editable occurrence,
//! and every content origin of its recomputed D that it shows nowhere
//! is a root. A
//! save never does that on its own; the "rerender all views" that
//! follows a reload does it to each buffer the reload named
//! ('DiffViewBufferRerender::ExtendingWhereAReloadAsked').
//!
//! The answer carries the readiness summary of the rerendered buffer,
//! as a save's does, and is followed by a rerender stream: the one
//! view, or nothing when the request was refused.

use crate::diff_view::change_leaves::{BufferReadinessSummary, readiness_response_sexp};
use crate::diff_view::readiness_requests::buffer_readiness_summary_for;
use crate::graph_straddle::types::GraphStraddle;
use crate::serve::ViewsState;
use crate::serve::rerender_stream::{
  PreparedRerenders, RerenderRequest, RerenderSelector, prepare_rerender_views,
  stream_empty_rerender, stream_prepared_rerenders};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  send_response_with_length_prefix, sexp_to_client_string,
  tag_sexp_response, viewforest_id_from_request};
use crate::skgrepo_sets::{SkgrepoRestriction, require_unrestricted};
use crate::types::env::SkgEnv;
use crate::types::views_state::ViewforestId;
use crate::rerender_pass::{DiffViewBufferRerender, RerenderedView};

use std::net::TcpStream;
use std::sync::Arc;

pub fn handle_extend_diff_view_request (
  stream              : &mut TcpStream,
  request             : &str,
  env                 : &SkgEnv,
  views_state         : &mut ViewsState,
  skgrepo_restriction : &SkgrepoRestriction,
) {
  match extend_diff_view_buffer (
    env, views_state, skgrepo_restriction, request ) {
    Err (error) => {
      send_response_with_length_prefix (
        stream, &tag_sexp_response (
          TcpToClient::ExtendDiffView,
          &sexp_to_client_string ( &readiness_response_sexp (
            Vec::new (), None, &[ error ], &[] )) ));
      stream_empty_rerender (stream); },
    Ok (( prepared, summary )) => {
      for viewforest_id in &prepared . viewforest_ids {
        views_state . open_views . set_extend_at_next_rerender_all (
          viewforest_id, false ); }
      send_response_with_length_prefix (
        stream, &tag_sexp_response (
          TcpToClient::ExtendDiffView,
          &sexp_to_client_string ( &readiness_response_sexp (
            Vec::new (), Some (&summary), &[], &[] )) ));
      stream_prepared_rerenders (stream, views_state, prepared); }, }}

/// The named buffer's registered viewforest, rerendered over the
/// connection's graph straddle, brought up to date, and extended to
/// the changes it does not show ('DiffViewBufferRerender::Extending'),
/// and the readiness summary of the result. Nothing is registered or sent. Refused
/// unless the connection is unrestricted, as the diff view
/// is, and unless the view is a buffer of the diff view.
fn extend_diff_view_buffer (
  env                 : &SkgEnv,
  views_state         : &ViewsState,
  skgrepo_restriction : &SkgrepoRestriction,
  request             : &str,
) -> Result<(PreparedRerenders, BufferReadinessSummary), String> {
  require_unrestricted (
    "Extending the diff view", skgrepo_restriction ) ?;
  let viewforest_id : ViewforestId = viewforest_id_from_request (request) ?;
  if views_state . open_views . diffView_buffer_kind_of_view (&viewforest_id) . is_none () {
    return Err ( format! (
      "View {} is not a buffer of the diff view.",
      viewforest_id . repr_in_client () )); }
  let prepared : PreparedRerenders = prepare_rerender_views (
    env, env . runtime_snapshot (), views_state,
    RerenderRequest {
      restriction : skgrepo_restriction,
      selector    : RerenderSelector::One (viewforest_id),
      diffView_buffer_rerender : DiffViewBufferRerender::Extending, } );
  let (Some (view), Some (straddle)) :
    (Option<&RerenderedView>, Option<&Arc<GraphStraddle>>) =
    (prepared . views . first (), prepared . graph_straddle . as_ref ())
    else { return Err ( prepared . errors . join ("\n") ); };
  let summary : BufferReadinessSummary =
    buffer_readiness_summary_for (env, straddle, &view . viewforest);
  Ok (( prepared, summary )) }
