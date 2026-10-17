//! The "stage and commit" request ('TODO/total-diff-view/2_plan.org',
//! "the summary and the commit"): stage everything and git-commit it,
//! in every owned skgrepo that has changes, with one message. When the
//! readiness summary is incomplete and the request lacks
//! '(confirmed . t)', the answer is the summary alone and nothing
//! changes.
//!
//! Every answer is followed by a rerender stream, as the
//! delete-references command's is: empty unless something was
//! committed, else one redrawn view per open buffer of the total diff
//! view, drawn afresh from the rebuilt graph pair, so a buffer whose
//! changes were all committed comes back empty.

use crate::change_forest::{BufferReadinessSummary, ReadinessSummary};
use crate::git_ops::stage_and_commit::{
  SkgrepoCommitOutcome, stage_and_commit_owned_skgrepos};
use crate::graph_pair::build::current_graph_pair_in_slot;
use crate::graph_pair::types::GraphPair;
use crate::readiness_store::ReadinessStore;
use crate::serve::ViewsState;
use crate::serve::handlers::rerender_all_views::stream_empty_rerender;
use crate::serve::handlers::save_buffer::readiness_summary_sexp;
use crate::serve::handlers::total_diff_view::{
  TotalDiffViewBuffer, total_diff_view_buffer};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  format_errors_warnings_sexp, format_lock_views_sexp,
  format_single_view_sexp, send_response_with_length_prefix,
  tag_sexp_response, value_from_request_sexp};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::types::misc::SkgrepoName;
use crate::types::tree::forest::ViewForest;
use crate::types::views_state::{TdvBuffer, ViewId};

use sexp::{Atom, Sexp};
use std::net::TcpStream;
use std::sync::{Arc, MutexGuard};

/// What a "stage and commit" request did.
#[derive(Debug)]
pub enum StageAndCommitAnswer {
  /// Nothing was attempted.
  Refused (String),
  /// The readiness summary is incomplete and the request was not
  /// confirmed. Nothing changed.
  NeedsConfirmation (ReadinessSummary),
  /// Each owned skgrepo's outcome, and any error met after the
  /// git-commits.
  Committed {
    outcomes : Vec<(SkgrepoName, SkgrepoCommitOutcome)>,
    errors   : Vec<String>, },
}

pub fn handle_stage_and_commit_request (
  stream              : &mut TcpStream,
  request             : &str,
  env                 : &SkgEnv,
  views_state         : &mut ViewsState,
  skgrepo_restriction : &SkgrepoRestriction,
) {
  let answer : StageAndCommitAnswer =
    match value_from_request_sexp ("message", request) {
      Err (e) => StageAndCommitAnswer::Refused (e),
      Ok (message) => stage_and_commit (
        env, views_state, skgrepo_restriction, &message,
        value_from_request_sexp ("confirmed", request)
          . is_ok_and ( |confirmed| confirmed == "t" )), };
  send_response_with_length_prefix (
    stream, &tag_sexp_response (
      TcpToClient::StageAndCommit,
      &stage_and_commit_response_sexp (&answer) ));
  match answer {
    StageAndCommitAnswer::Committed { outcomes, .. }
      if outcomes . iter () . any (
        |(_, outcome)| matches! (outcome, Ok (Some (_))) ) => {
      let (redrawn, errors) : (Vec<(ViewId, String)>, Vec<String>) =
        redraw_tdv_views (env, views_state);
      stream_redrawn_views (stream, &redrawn, &errors); },
    _ => stream_empty_rerender (stream), }}

/// See the module comment. Refused unless the skgrepo restriction is
/// 'all', as the total diff view is. After the git-commits, the
/// connection's graph pair is rebuilt (each committed HEAD moved), and
/// the marks of the changes committed are dropped from the store: a
/// commit lapses every mark.
pub fn stage_and_commit (
  env                 : &SkgEnv,
  views_state         : &mut ViewsState,
  skgrepo_restriction : &SkgrepoRestriction,
  message             : &str,
  confirmed           : bool,
) -> StageAndCommitAnswer {
  if ! skgrepo_restriction . is_all () {
    return StageAndCommitAnswer::Refused ( format! (
      "Stage and commit requires no skgrepo restriction (the skgrepo-set all); current skgrepo restriction is {}. Switch the repo-set to all first.",
      skgrepo_restriction . name )); }
  if message . trim () . is_empty () {
    return StageAndCommitAnswer::Refused (
      "A commit message is required." . to_string () ); }
  let runtime : Arc<RuntimeGeneration> = env . runtime_snapshot ();
  let pair : Arc<GraphPair> = match current_graph_pair_in_slot (
    &mut views_state . graph_pair, &runtime . config,
    runtime . graph . clone () ) {
    Ok (pair) => pair,
    Err (e) => return StageAndCommitAnswer::Refused (e), };
  { let store : MutexGuard<ReadinessStore> = lock_store (env);
    let summary : ReadinessSummary =
      pair . change_forest () . readiness_summary (&store);
    if ! summary . is_complete () && ! confirmed {
      return StageAndCommitAnswer::NeedsConfirmation (summary); }}
  let outcomes : Vec<(SkgrepoName, SkgrepoCommitOutcome)> =
    stage_and_commit_owned_skgrepos (&runtime . config, message);
  let mut errors : Vec<String> = Vec::new ();
  match current_graph_pair_in_slot (
    &mut views_state . graph_pair, &runtime . config,
    runtime . graph . clone () ) {
    Err (e) => errors . push ( format! (
      "The graph pair could not be rebuilt after the commits: {}", e )),
    Ok (rebuilt) => {
      let mut store : MutexGuard<ReadinessStore> = lock_store (env);
      store . marks . retain ( |key, _|
        rebuilt . change_forest () . leaf (key) . is_some () );
      if let Err (e) = store . save (&runtime . config . data_root) {
        errors . push ( format! (
          "The readiness marks were not written to disk: {}", e )); }}, }
  StageAndCommitAnswer::Committed { outcomes, errors } }

fn lock_store (
  env : &SkgEnv,
) -> MutexGuard<'_, ReadinessStore> {
  env . runtime . readiness_store . lock ()
    . unwrap_or_else ( |poisoned| poisoned . into_inner () ) }

/// Format, for each answer:
///   ((errors ("REFUSAL")) (warnings ()))
///   ((needs-confirmation . t) (readiness-summary ...) (errors ()) (warnings ()))
///   ((commits (("SKGREPO" "COMMIT-ID") ...)) (errors (...)) (warnings ()))
/// The readiness summary has the save response's form, with an
/// unshown count of 0. 'commits' lists the skgrepos git-committed; a
/// skgrepo without changes is in neither list, a failed one is an
/// error.
pub fn stage_and_commit_response_sexp (
  answer : &StageAndCommitAnswer,
) -> String {
  let errors_and_warnings = |errors : &[String]| -> Vec<Sexp> {
    match sexp::parse (&format_errors_warnings_sexp (errors, &[]))
      . expect ("format_errors_warnings_sexp should produce valid sexp") {
      Sexp::List (items) => items,
      atom => vec! [ atom ], } };
  let items : Vec<Sexp> = match answer {
    StageAndCommitAnswer::Refused (error) =>
      errors_and_warnings ( &[ error . clone () ] ),
    StageAndCommitAnswer::NeedsConfirmation (summary) =>
      [ sexp::parse ("(needs-confirmation . t)")
          . expect ("a literal sexp parses"),
        sexp::parse ( &readiness_summary_sexp ( &BufferReadinessSummary {
          summary : summary . clone (), unshown_changes : 0 } ))
          . expect ("readiness_summary_sexp should produce valid sexp") ]
      . into_iter () . chain ( errors_and_warnings (&[]) ) . collect (),
    StageAndCommitAnswer::Committed { outcomes, errors } => {
      let commits : Vec<Sexp> = outcomes . iter ()
        . filter_map ( |(skgrepo, outcome)| match outcome {
          Ok (Some (commit_id)) => Some ( Sexp::List ( vec! [
            Sexp::Atom ( Atom::S ( skgrepo . to_string () )),
            Sexp::Atom ( Atom::S ( commit_id . to_string () )) ] )),
          _ => None, } )
        . collect ();
      let all_errors : Vec<String> = outcomes . iter ()
        . filter_map ( |(skgrepo, outcome)| match outcome {
          Err (e) => Some ( format! ( "skgrepo {}: {}", skgrepo, e )),
          Ok (_) => None, } )
        . chain ( errors . iter () . cloned () )
        . collect ();
      std::iter::once ( Sexp::List ( vec! [
        Sexp::Atom ( Atom::S ( "commits" . to_string () )),
        Sexp::List (commits) ] ))
        . chain ( errors_and_warnings (&all_errors) ) . collect () }, };
  Sexp::List (items) . to_string () }

/// Draw each open buffer of the total diff view afresh from the
/// connection's graph pair, as the "total diff view" request draws
/// it, and register the result in its place: an empty viewforest when
/// the buffer would now be empty. Returns each view's new text, in
/// view-id order, and the errors met.
pub fn redraw_tdv_views (
  env         : &SkgEnv,
  views_state : &mut ViewsState,
) -> ( Vec<(ViewId, String)>, Vec<String> ) {
  let runtime : Arc<RuntimeGeneration> = env . runtime_snapshot ();
  let mut tdv_views : Vec<(ViewId, TdvBuffer)> =
    views_state . open_views . views . iter ()
    . filter_map ( |(view_id, state)|
      state . tdv_buffer . map ( |tdv_buffer| ( view_id . clone (), tdv_buffer )) )
    . collect ();
  tdv_views . sort_by_key ( |(view_id, _)| view_id . repr_in_client () );
  if tdv_views . is_empty () { return ( Vec::new (), Vec::new () ); }
  let pair : Arc<GraphPair> = match current_graph_pair_in_slot (
    &mut views_state . graph_pair, &runtime . config,
    runtime . graph . clone () ) {
    Ok (pair) => pair,
    Err (e) => return ( Vec::new (), vec! [ e ] ), };
  let store : MutexGuard<ReadinessStore> = lock_store (env);
  let mut redrawn : Vec<(ViewId, String)> = Vec::new ();
  let mut errors : Vec<String> = Vec::new ();
  for (view_id, tdv_buffer) in tdv_views {
    match total_diff_view_buffer (&runtime, &pair, &store, tdv_buffer) {
      Err (e) => errors . push ( format! (
        "View {}: {}", view_id . repr_in_client (), e )),
      Ok (buffer) => {
        let (text, viewforest) : (String, ViewForest) = match buffer {
          TotalDiffViewBuffer::Empty => ( String::new (), ViewForest::new () ),
          TotalDiffViewBuffer::Content { text, viewforest, .. } =>
            ( text, viewforest ), };
        views_state . open_views . update_view (
          &runtime . graph, &view_id, viewforest );
        redrawn . push (( view_id, text )); }, }}
  ( redrawn, errors ) }

/// The rerender stream: rerender-lock naming the redrawn views, one
/// rerender-view each, then rerender-done with 'errors'.
fn stream_redrawn_views (
  stream  : &mut TcpStream,
  redrawn : &[(ViewId, String)],
  errors  : &[String],
) {
  let view_ids : Vec<ViewId> =
    redrawn . iter () . map ( |(view_id, _)| view_id . clone () ) . collect ();
  send_response_with_length_prefix (
    stream, &tag_sexp_response (
      TcpToClient::RerenderLock, &format_lock_views_sexp (&view_ids) ));
  for (view_id, text) in redrawn {
    send_response_with_length_prefix (
      stream, &tag_sexp_response (
        TcpToClient::RerenderView, &format_single_view_sexp (view_id, text) )); }
  send_response_with_length_prefix (
    stream, &tag_sexp_response (
      TcpToClient::RerenderDone, &format_errors_warnings_sexp (errors, &[]) )); }
