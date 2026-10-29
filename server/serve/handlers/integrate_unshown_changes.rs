//! The "integrate unshown changes" request
//! ('docs/diff-view.org', "Integrating unshown changes"): rerender one buffer of the diff view so
//! that every node of its recomputed U has an editable occurrence,
//! and every content origin of its recomputed D that it shows nowhere
//! is a root. A
//! save never does that on its own.
//!
//! The answer carries the readiness summary of the rerendered buffer,
//! as a save's does, and is followed by a rerender stream: the one
//! view, or nothing when the request was refused.

use crate::refresh::buffer_corrections::BufferCorrectionPolicy;
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
use crate::skgrepo_sets::{SkgrepoRestriction, require_skgrepo_set_all};
use crate::types::env::SkgEnv;
use crate::types::views_state::ViewforestId;
use crate::update_buffer::RerenderedView;

use std::net::TcpStream;
use std::sync::Arc;

pub fn handle_integrate_unshown_changes_request (
  stream              : &mut TcpStream,
  request             : &str,
  env                 : &SkgEnv,
  views_state         : &mut ViewsState,
  skgrepo_restriction : &SkgrepoRestriction,
) {
  match integrate_unshown_changes (
    env, views_state, skgrepo_restriction, request ) {
    Err (error) => {
      send_response_with_length_prefix (
        stream, &tag_sexp_response (
          TcpToClient::IntegrateUnshownChanges,
          &sexp_to_client_string ( &readiness_response_sexp (
            Vec::new (), None, &[ error ], &[] )) ));
      stream_empty_rerender (stream); },
    Ok (( prepared, summary )) => {
      send_response_with_length_prefix (
        stream, &tag_sexp_response (
          TcpToClient::IntegrateUnshownChanges,
          &sexp_to_client_string ( &readiness_response_sexp (
            Vec::new (), Some (&summary), &[], &[] )) ));
      stream_prepared_rerenders (stream, views_state, prepared); }, }}

/// The named buffer's registered viewforest, rerendered over the
/// connection's graph straddle, brought up to date, with its unshown
/// changes integrated ('RerenderSelector::IntegratingUnshownChanges'),
/// and the readiness summary of the result. Nothing is registered or sent. Refused
/// unless the skgrepo restriction is 'all', as the diff view
/// is, and unless the view is a buffer of the diff view.
fn integrate_unshown_changes (
  env                 : &SkgEnv,
  views_state         : &ViewsState,
  skgrepo_restriction : &SkgrepoRestriction,
  request             : &str,
) -> Result<(PreparedRerenders, BufferReadinessSummary), String> {
  require_skgrepo_set_all (
    "Integrating unshown changes", skgrepo_restriction ) ?;
  let viewforest_id : ViewforestId = viewforest_id_from_request (request) ?;
  if views_state . open_views . diffView_buffer_kind_of_view (&viewforest_id) . is_none () {
    return Err ( format! (
      "View {} is not a buffer of the diff view.",
      viewforest_id . repr_in_client () )); }
  let prepared : PreparedRerenders = prepare_rerender_views (
    env, env . runtime_snapshot (), views_state,
    RerenderRequest {
      restriction              : skgrepo_restriction,
      buffer_correction_policy : BufferCorrectionPolicy::ReportAll,
      selector                 : RerenderSelector::IntegratingUnshownChanges (viewforest_id), } );
  let (Some (view), Some (straddle)) :
    (Option<&RerenderedView>, Option<&Arc<GraphStraddle>>) =
    (prepared . views . first (), prepared . graph_straddle . as_ref ())
    else { return Err ( prepared . errors . join ("\n") ); };
  let summary : BufferReadinessSummary =
    buffer_readiness_summary_for (env, straddle, &view . viewforest);
  Ok (( prepared, summary )) }
