//! The rerender pipeline every request that rerenders open views
//! outside a save shares: prepare, answer, stream. The request says
//! under which skgrepo restriction, which views, and what a rerender
//! of a buffer of the diff view does beyond its refresh
//! ('RerenderRequest'). Each rerender is prepared in memory first by
//! the rerender pass ('prepare_rerender_views', 'server/rerender_pass.rs',
//! which the save's rerender shares), so that
//! a text-release decision ('authorize_prepared_rerenders') can
//! precede any visible change, and then streamed: "rerender-lock",
//! one "rerender-view" per view, "rerender-done", which carries the
//! errors and the warnings ('stream_prepared_rerenders'). The graph straddle the rerenders
//! brought up to date is written back to the connection
//! ('write_graph_straddle_back'), as a save does with its own.

use crate::graph_straddle::types::GraphStraddle;
use crate::serve::ViewsState;
use crate::serve::handlers::text_release::{
  TextReleaseDecision,
  challenge_response,
  decide as decide_text_release};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{ format_lock_views_sexp, format_rerender_done_sexp, format_single_view_sexp, send_response_with_length_prefix, tag_sexp_response};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::types::views_state::{ViewforestId, ViewState};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::rerender_pass::{
  DiffViewBufferRerender, RerenderPassContext, RerenderedView, release_candidates,
  rerender_open_views};
use crate::types::misc::ID;

use std::net::TcpStream;
use std::collections::HashSet;
use std::sync::Arc;

pub(crate) struct PreparedRerenders {
  runtime  : Arc<RuntimeGeneration>,
  pub(crate) viewforest_ids : Vec<ViewforestId>,
  pub(crate) views    : Vec<RerenderedView>,
  pub(crate) errors   : Vec<String>,
  warnings : Vec<String>,
  pub(crate) graph_straddle : Option<Arc<GraphStraddle>>, // the connection's, as the rerenders brought it up to date
}

/// What a request that rerenders open views asks of the pass: the
/// skgrepo restriction to rerender under, which views, and what the
/// rerender of a buffer of the diff view does beyond its refresh.
pub(crate) struct RerenderRequest<'a> {
  pub restriction              : &'a SkgrepoRestriction,
  pub selector                 : RerenderSelector<'a>,
  pub diffView_buffer_rerender : DiffViewBufferRerender,
}

/// Which open views a rerender request rerenders, chosen from what
/// each already holds, before any is cloned or rerendered, so that a
/// request can leave unrelated clean and dirty buffers untouched.
pub(crate) enum RerenderSelector<'a> {
  /// Every open view.
  All,
  /// The open views the predicate selects.
  Where (Box<dyn Fn (&ViewforestId, &ViewState) -> bool + 'a>),
  /// The one open view named.
  One (ViewforestId),
}

impl RerenderSelector<'_> {
  fn selects (
    &self,
    viewforest_id : &ViewforestId,
    state         : &ViewState,
  ) -> bool {
    match self {
      RerenderSelector::All => true,
      RerenderSelector::Where (include) => include (viewforest_id, state),
      RerenderSelector::One (named) => viewforest_id == named, } }
}

/// Complete every rerender 'request' asks for in memory, against
/// 'runtime'. Nothing is sent and no registered view is changed, so
/// the text-release decision can precede the lock and the first
/// externally visible mutation. Each view is rerendered in the mode
/// its own buffer kind gives ('rerender_open_view').
pub(crate) fn prepare_rerender_views (
  env         : &SkgEnv,
  runtime     : Arc<RuntimeGeneration>,
  views_state : &ViewsState,
  request     : RerenderRequest,
) -> PreparedRerenders {
  let viewforest_ids : Vec<ViewforestId> = views_state . open_views . views . iter ()
    .filter (|(viewforest_id, state)| request . selector . selects (viewforest_id, state))
    .map (|(viewforest_id, _)| viewforest_id . clone ()) . collect ();
  let mut context : RerenderPassContext = RerenderPassContext::new (
    env, runtime, request . restriction,
    // A buffer of the diff view is refreshed over a graph straddle
    // brought up to date from the connection's. Streaming the
    // rerenders writes it back, as a save does.
    views_state . graph_straddle . clone (),
    request . diffView_buffer_rerender );
  let rendered_views : Vec<RerenderedView> =
    rerender_open_views (&viewforest_ids, views_state, &mut context);
  PreparedRerenders {
    runtime : context . runtime . clone (),
    viewforest_ids,
    views    : rendered_views,
    errors   : context . errors,
    warnings : context . warnings,
    graph_straddle : context . graph_straddle,
  }
}

/// Whether the prepared rerenders may be streamed under 'restriction'
/// ('decide_text_release'). If not, the challenge is sent, and an
/// empty rerender stream after it.
pub(crate) fn authorize_prepared_rerenders (
  stream        : &mut TcpStream,
  prepared      : &mut PreparedRerenders,
  restriction   : &SkgrepoRestriction,
  operation     : &str,
  approved_pids : &HashSet<ID>,
) -> bool {
  let candidates : Vec<ID> = release_candidates (
    prepared . views . iter () . map ( |view| &view . viewforest ));
  let release = decide_text_release (
    operation, restriction, &candidates,
    &prepared . runtime . graph, approved_pids );
  match release {
    TextReleaseDecision::Challenge { .. } => {
      send_response_with_length_prefix (
        stream, &challenge_response (&release) . unwrap () );
      stream_empty_rerender (stream);
      false },
    TextReleaseDecision::AllowWithWarning { warning } => {
      prepared . warnings . push (warning);
      true },
    TextReleaseDecision::Allow => true,
  }
}

pub(crate) fn stream_prepared_rerenders (
  stream      : &mut TcpStream,
  views_state : &mut ViewsState,
  prepared    : PreparedRerenders,
) {
  send_response_with_length_prefix (
    stream,
    & tag_sexp_response (
      TcpToClient::RerenderLock,
      & format_lock_views_sexp (&prepared . viewforest_ids) ));
  write_graph_straddle_back (views_state, prepared . graph_straddle);
  register_and_send_rerendered_views (
    stream, views_state, &prepared . runtime . graph, prepared . views,
    TcpToClient::RerenderView );
  send_response_with_length_prefix (
    stream,
    & tag_sexp_response (
      TcpToClient::RerenderDone,
      & format_rerender_done_sexp (
        &prepared . errors,
        &prepared . warnings ) )); }

/// Register each of 'views' in place of the open view it rerenders,
/// and send it to the client under 'tag'.
pub(crate) fn register_and_send_rerendered_views (
  stream      : &mut TcpStream,
  views_state : &mut ViewsState,
  graph       : &InRustGraph,
  views       : Vec<RerenderedView>,
  tag         : TcpToClient,
) {
  for view in views {
    views_state . open_views . update_view (
      graph, &view . viewforest_id, view . viewforest, view . viewnode_id_record );
    send_response_with_length_prefix (
      stream,
      & tag_sexp_response (
        tag, & format_single_view_sexp (&view . viewforest_id, &view . text) )); }}

/// Keep the graph straddle a rerender brought up to date as the
/// connection's. A rerender that rendered no buffer of the diff view
/// built none, and leaves the connection's as it was.
pub(crate) fn write_graph_straddle_back (
  views_state    : &mut ViewsState,
  graph_straddle : Option<Arc<GraphStraddle>>,
) {
  if graph_straddle . is_some () {
    views_state . graph_straddle = graph_straddle; }}

/// Send an empty rerender stream: a "rerender-lock" naming no views,
/// then "rerender-done" with no errors or warnings.  Used after a
/// refusal: each client locks every Skg buffer and sets its
/// stream-in-progress guard (Emacs's 'skg--stream-in-progress',
/// Neovim's 'stream_in_progress' in 'nvim/lua/skg/lock.lua') before
/// sending a switch of the skgrepo restriction, and only the rerender
/// stream unwinds them.  The empty lock list makes the client unlock
/// every buffer; the done message clears the guard.
pub fn stream_empty_rerender (
  stream : &mut TcpStream,
) {
  send_response_with_length_prefix (
    stream,
    & tag_sexp_response (
      TcpToClient::RerenderLock,
      & format_lock_views_sexp ( &[] ) ));
  send_response_with_length_prefix (
    stream,
    & tag_sexp_response (
      TcpToClient::RerenderDone,
      & format_rerender_done_sexp ( &[], &[] ) )); }
