//! The "stage and commit" request ('TODO/total-diff-view/2_plan.org',
//! "the summary and the commit"): stage everything and git-commit it,
//! in every owned skgrepo that has changes, with one message. When the
//! readiness summary is incomplete and the request lacks
//! '(confirmed . t)', the answer is the summary alone and nothing
//! changes.
//!
//! Every answer is followed by a rerender stream, as the
//! delete-references command's is: empty unless something was
//! committed, else one view per open buffer of the total diff view,
//! rerendered as any rerender is, over the rebuilt graph straddle
//! ('stream_rerender_tdv_views'). Its roots stay, so a buffer whose
//! changes were all committed comes back as plain context.

use crate::change_forest::{BufferReadinessSummary, ReadinessSummary};
use crate::git_ops::stage_and_commit::{
  SkgrepoCommitOutcome, stage_and_commit_owned_skgrepos};
use crate::graph_straddle::build::current_graph_straddle_in_slot;
use crate::graph_straddle::types::GraphStraddle;
use crate::readiness_store::ReadinessStore;
use crate::serve::ViewsState;
use crate::serve::handlers::rerender_all_views::{
  stream_empty_rerender, stream_rerender_tdv_views};
use crate::serve::handlers::save_buffer::readiness_summary_sexp;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  format_errors_warnings_sexp, send_response_with_length_prefix,
  sexp_to_client_string, tag_sexp_response, value_from_request_sexp};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::types::misc::SkgrepoName;

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
        |(_, outcome)| matches! (outcome, Ok (Some (_))) ) =>
      stream_rerender_tdv_views (
        stream, env, views_state, skgrepo_restriction ),
    _ => stream_empty_rerender (stream), }}

/// See the module comment. Refused unless the skgrepo restriction is
/// 'all', as the total diff view is. After the git-commits, the
/// connection's graph straddle is rebuilt (each committed HEAD moved), and
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
  let straddle : Arc<GraphStraddle> = match current_graph_straddle_in_slot (
    &mut views_state . graph_straddle, &runtime . config,
    runtime . graph . clone () ) {
    Ok (straddle) => straddle,
    Err (e) => return StageAndCommitAnswer::Refused (e), };
  { let store : MutexGuard<ReadinessStore> = lock_store (env);
    let summary : ReadinessSummary =
      straddle . change_forest () . readiness_summary (&store);
    if ! summary . is_complete () && ! confirmed {
      return StageAndCommitAnswer::NeedsConfirmation (summary); }}
  let outcomes : Vec<(SkgrepoName, SkgrepoCommitOutcome)> =
    stage_and_commit_owned_skgrepos (&runtime . config, message);
  let mut errors : Vec<String> = Vec::new ();
  match current_graph_straddle_in_slot (
    &mut views_state . graph_straddle, &runtime . config,
    runtime . graph . clone () ) {
    Err (e) => errors . push ( format! (
      "The graph straddle could not be rebuilt after the commits: {}", e )),
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
  sexp_to_client_string ( &Sexp::List (items) ) }
