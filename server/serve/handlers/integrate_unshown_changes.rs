//! The "integrate unshown changes" request
//! ('docs/diff-view.org', "Integrating unshown changes"): redraw one buffer of the diff view so
//! that every node of its recomputed U has an editable occurrence,
//! and every content origin of its recomputed D that it draws nowhere
//! is a root. A
//! save never does that on its own.
//!
//! The answer carries the readiness summary of the redrawn buffer,
//! as a save's does, and is followed by a rerender stream: the one
//! view, or nothing when the request was refused.

use crate::diff_view::change_leaves::{BufferReadinessSummary, readiness_response_sexp};
use crate::graph_straddle::types::GraphStraddle;
use crate::diff_view::pcsvf::nodes_editable_in_diffView;
use crate::diff_view::readiness_store::ReadinessStore;
use crate::serve::ViewsState;
use crate::serve::rerender_stream::{
  PreparedRerenders, PreparedView, prepare_rerender_views_where,
  stream_empty_rerender, stream_prepared_rerenders};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  send_response_with_length_prefix, sexp_to_client_string,
  tag_sexp_response, view_id_from_request};
use crate::skgrepo_sets::{SkgrepoRestriction, require_skgrepo_set_all};
use crate::types::env::SkgEnv;
use crate::types::views_state::ViewId;
use crate::update_buffer::RerenderOptions;

use std::net::TcpStream;
use std::sync::{Arc, MutexGuard};

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
/// connection's graph straddle, brought up to date, with
/// 'integrate_unshown_changes' set ('RerenderOptions'), and the readiness
/// summary of the result. Nothing is registered or sent. Refused
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
  let view_id : ViewId = view_id_from_request (request) ?;
  if views_state . open_views . diffView_buffer_kind_of_view (&view_id) . is_none () {
    return Err ( format! (
      "View {} is not a buffer of the diff view.",
      view_id . repr_in_client () )); }
  let prepared : PreparedRerenders = prepare_rerender_views_where (
    env, env . runtime_snapshot (), views_state,
    Some (skgrepo_restriction), None,
    RerenderOptions::for_integrating_unshown_changes,
    |registered_view_id, _| *registered_view_id == view_id );
  let (Some (view), Some (straddle)) :
    (Option<&PreparedView>, Option<&Arc<GraphStraddle>>) =
    (prepared . views . first (), prepared . graph_straddle . as_ref ())
    else { return Err ( prepared . errors . join ("\n") ); };
  let summary : BufferReadinessSummary = {
    let store : MutexGuard<ReadinessStore> =
      env . runtime . lock_readiness_store ();
    straddle . change_leaves () . buffer_readiness_summary (
      &store, &nodes_editable_in_diffView (&view . viewforest) ) };
  Ok (( prepared, summary )) }
