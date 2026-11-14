//! The "set overrides followed" request: switch one open view between
//! following its root authors' overrides and following the user's
//! ('OverridesFollowed'), and rerender it.
//!
//! The answer is an 'overrides-followed' message naming the view and
//! whose overrides it now follows, followed by a rerender stream of
//! that one view. A refusal answers in the same shape, carrying the
//! view's unchanged choice and its reason in 'content', followed by an
//! empty rerender stream, since each client locks its buffers and sets
//! its stream-in-progress guard before it sends the request. Under a
//! skgrepo restriction, a rerender that would release overPrivateText
//! text is challenged ('authorize_prepared_rerenders') before the view
//! changes: the challenge and an empty rerender stream replace the
//! answer, and the view keeps its choice.
//!
//! The clients refuse to send it for a buffer with unsaved edits,
//! which the rerender would replace.

use crate::dbs::in_rust_graph::override_resolution::override_follower_of_tree_containing;
use crate::rerender_pass::DiffViewBufferRerender;
use crate::serve::ViewsState;
use crate::serve::handlers::text_release::approved_pids_from_request;
use crate::serve::protocol::TcpToClient;
use crate::serve::rerender_stream::{
  PreparedRerenders, RerenderRequest, RerenderSelector,
  authorize_prepared_rerenders, prepare_rerender_views,
  stream_empty_rerender, stream_prepared_rerenders};
use crate::serve::util::{
  send_response_with_length_prefix, value_from_request_sexp,
  viewforest_id_from_request};
use crate::sexp_string::quote_sexp_string;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::types::misc::SkgrepoOwner;
use crate::types::views_state::{OverridesFollowed, ViewState, ViewforestId};

use std::net::TcpStream;
use std::sync::Arc;

pub fn handle_set_overrides_followed_request (
  stream              : &mut TcpStream,
  request             : &str,
  env                 : &SkgEnv,
  views_state         : &mut ViewsState,
  skgrepo_restriction : &SkgrepoRestriction,
) {
  let viewforest_id : ViewforestId =
    match viewforest_id_from_request (request) {
      Ok (viewforest_id) => viewforest_id,
      Err (_) => {
        refuse_unwinding (
          stream, "", OverridesFollowed::default (),
          "The request names no view ('view-id')." );
        return; }};
  let current : OverridesFollowed =
    views_state . open_views . overrides_followed_by_view (&viewforest_id);
  let runtime : Arc<RuntimeGeneration> = env . runtime_snapshot ();
  let requested : OverridesFollowed =
    match requested_overrides_followed (request)
      . and_then ( |requested| error_unless_view_can_follow (
        views_state, &runtime, &viewforest_id, requested )
        . map ( |()| requested ))
    { Ok (requested) => requested,
      Err (reason) => {
        refuse_unwinding (
          stream, &viewforest_id . repr_in_client (), current, &reason );
        return; }};
  // The rerender reads the view's choice from what the connection
  // holds, so it is set first, and restored unless the rerender is
  // released.
  views_state . open_views . set_overrides_followed (&viewforest_id, requested);
  let mut prepared : PreparedRerenders = prepare_rerender_views (
    env, runtime, views_state,
    RerenderRequest {
      restriction : skgrepo_restriction,
      selector    : RerenderSelector::One (viewforest_id . clone ()),
      diffView_buffer_rerender : DiffViewBufferRerender::RefreshOnly, } );
  if prepared . views . is_empty () {
    views_state . open_views . set_overrides_followed (&viewforest_id, current);
    refuse_unwinding (
      stream, &viewforest_id . repr_in_client (), current,
      &prepared . errors . join ("\n") );
    return; }
  if ! authorize_prepared_rerenders (
    stream, &mut prepared, skgrepo_restriction,
    "overrides-followed-switch", &approved_pids_from_request (request) ) {
    views_state . open_views . set_overrides_followed (&viewforest_id, current);
    return; }
  send_overrides_followed_response (
    stream, &viewforest_id . repr_in_client (), requested, None );
  stream_prepared_rerenders (stream, views_state, prepared); }

/// The request's '(overrides-followed . "NAME")', which it requires.
fn requested_overrides_followed (
  request : &str,
) -> Result<OverridesFollowed, String> {
  let name : String =
    value_from_request_sexp ("overrides-followed", request)
    . map_err ( |_| "The request says nothing of whose overrides to follow ('overrides-followed')." . to_string () ) ?;
  OverridesFollowed::from_wire_name (&name)
    . ok_or_else ( || format! (
      "'overrides-followed' must be {:?} or {:?}, not {:?}.",
      OverridesFollowed::RootAuthors . wire_name (),
      OverridesFollowed::User . wire_name (),
      name )) }

/// Why 'viewforest_id' cannot be switched to follow 'requested': the
/// connection does not hold it, it is a buffer of the diff view
/// (which substitutes no overriders), or it would follow the user's
/// and already does, every one of its trees being rooted at a node
/// the user owns. A switch back to the root authors' is always
/// allowed.
fn error_unless_view_can_follow (
  views_state   : &ViewsState,
  runtime       : &RuntimeGeneration,
  viewforest_id : &ViewforestId,
  requested     : OverridesFollowed,
) -> Result<(), String> {
  let Some (state) : Option<&ViewState> =
    views_state . open_views . views . get (viewforest_id)
  else { return Err ( format! (
    "The server does not hold the view {} (it was restarted, or the client reconnected, since the view was drawn). Save the buffer, then switch again.",
    viewforest_id . repr_in_client () )); };
  if state . diffView_buffer_kind . is_some () {
    return Err ( "The diff view shows no overriders in place of what they override, so it follows no one's overrides." . to_string () ); }
  if requested == OverridesFollowed::User
     && every_tree_follows_the_user_already (state, runtime) {
    return Err ( "Every tree in this buffer is rooted at a node you own, so it shows your overrides already." . to_string () ); }
  Ok (( )) }

/// Whether every top-level tree of the view 'state' follows the user's
/// overrides when it follows its root author's: true of a view whose
/// roots are all owned (or new), and of a view with no roots.
fn every_tree_follows_the_user_already (
  state   : &ViewState,
  runtime : &RuntimeGeneration,
) -> bool {
  state . viewforest . roots ()
    . all ( |root| override_follower_of_tree_containing (
      &runtime . config, &runtime . graph,
      OverridesFollowed::RootAuthors, root ) == SkgrepoOwner::User ) }

/// The refusal: the answer carrying the view's unchanged choice and
/// 'reason', then an empty rerender stream, which unlocks the
/// client's buffers and clears its stream-in-progress guard.
fn refuse_unwinding (
  stream             : &mut TcpStream,
  view_id            : &str,
  overrides_followed : OverridesFollowed,
  reason             : &str,
) {
  send_overrides_followed_response (
    stream, view_id, overrides_followed, Some (reason) );
  stream_empty_rerender (stream); }

/// The 'overrides-followed' message: the view, whose overrides it
/// follows, and, for a refusal, why.
fn send_overrides_followed_response (
  stream             : &mut TcpStream,
  view_id            : &str,
  overrides_followed : OverridesFollowed,
  refusal            : Option<&str>,
) {
  let content : String = refusal
    . map ( |reason| format! (" (content {})", quote_sexp_string (reason)) )
    . unwrap_or_default ();
  send_response_with_length_prefix ( stream, & format! (
    "((response-type {}) (view-id {}) (overrides-followed {}){})",
    TcpToClient::OverridesFollowed . repr_in_client (),
    quote_sexp_string (view_id),
    quote_sexp_string ( overrides_followed . wire_name () ),
    content )); }
