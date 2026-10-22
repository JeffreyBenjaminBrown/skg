//! The "stage and commit" request ('docs/diff-view.org',
//! "Stage and commit"): stage everything and git-commit it,
//! in every owned skgrepo that has changes, with one message. When the
//! readiness summary is incomplete, or some owned gitrepo has entries
//! staged already, and the request lacks '(confirmed . t)', the answer
//! asks for confirmation and nothing changes.
//!
//! Every answer is followed by a rerender stream, as the
//! delete-references command's is: empty unless something was
//! committed, else one view per open buffer of the diff view,
//! rerendered as any rerender is, over the rebuilt graph straddle
//! ('stream_rerender_diffView_views'). Its roots stay, so a buffer whose
//! changes were all committed comes back as plain context.

use crate::diff_view::change_leaves::{
  BufferReadinessSummary, ReadinessSummary, readiness_response_sexp,
  readiness_response_trailing_entries};
use crate::git_ops::stage_and_commit::{
  PreStagedGitrepo, SkgrepoCommitOutcome,
  pre_staged_entries_of_owned_gitrepos, stage_and_commit_owned_skgrepos};
use crate::graph_straddle::build::current_graph_straddle_in_slot;
use crate::graph_straddle::types::GraphStraddle;
use crate::diff_view::readiness_store::ReadinessStore;
use crate::serve::ViewsState;
use crate::serve::rerender_stream::{
  PreparedRerenders, prepare_rerender_views_where,
  stream_empty_rerender, stream_prepared_rerenders};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  send_response_with_length_prefix, sexp_to_client_string,
  tag_sexp_response, true_entry_sexp, value_from_request_sexp};
use crate::sexp_string::quote_sexp_string;
use crate::skgrepo_sets::{SkgrepoRestriction, require_skgrepo_set_all};
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::types::misc::SkgrepoName;
use crate::update_buffer::RerenderOptions;

use sexp::{Atom, Sexp};
use std::iter::once;
use std::net::TcpStream;
use std::path::{Path, PathBuf};
use std::sync::{Arc, MutexGuard};

/// What a "stage and commit" request did.
#[derive(Debug)]
pub enum StageAndCommitAnswer {
  /// Nothing was attempted.
  Refused (String),
  /// The readiness summary is incomplete or 'pre_staged' is not
  /// empty, and the request was not confirmed. Nothing changed.
  NeedsConfirmation {
    summary    : ReadinessSummary,
    pre_staged : Vec<PreStagedGitrepo>, },
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
      stream_rerender_diffView_views (
        stream, env, views_state, skgrepo_restriction ),
    _ => stream_empty_rerender (stream), }}

/// After a git-commit: rerender each open buffer of the diff
/// view as any rerender does, over the graph straddle the commit
/// rebuilt. Its roots stay ('docs/diff-view.org',
/// "The fresh drawing, and what it keeps"), and what the commit made unchanged loses its
/// decorations. The commit requires the skgrepo-set all, so no text
/// is withheld and no release is asked for.
fn stream_rerender_diffView_views (
  stream              : &mut TcpStream,
  env                 : &SkgEnv,
  views_state         : &mut ViewsState,
  skgrepo_restriction : &SkgrepoRestriction,
) {
  let prepared : PreparedRerenders = prepare_rerender_views_where (
    env, env . runtime_snapshot (), views_state,
    Some (skgrepo_restriction), None, RerenderOptions::ordinary,
    |_, state| state . diffView_buffer_kind . is_some () );
  stream_prepared_rerenders (stream, views_state, prepared);
}

/// See the module comment. Refused unless the skgrepo restriction is
/// 'all', as the diff view is. A confirmed request skips both checks.
/// After the git-commits, the
/// connection's graph straddle is rebuilt (each committed HEAD moved), and
/// the store keeps exactly the marks whose change-forest leaves still
/// exist in it, so the marks of the changes committed are dropped.
pub fn stage_and_commit (
  env                 : &SkgEnv,
  views_state         : &mut ViewsState,
  skgrepo_restriction : &SkgrepoRestriction,
  message             : &str,
  confirmed           : bool,
) -> StageAndCommitAnswer {
  if let Err (refusal) =
    require_skgrepo_set_all ("Stage and commit", skgrepo_restriction) {
    return StageAndCommitAnswer::Refused (refusal); }
  if message . trim () . is_empty () {
    return StageAndCommitAnswer::Refused (
      "A commit message is required." . to_string () ); }
  let runtime : Arc<RuntimeGeneration> = env . runtime_snapshot ();
  let straddle : Arc<GraphStraddle> = match current_graph_straddle_in_slot (
    &mut views_state . graph_straddle, &runtime . config,
    runtime . graph . clone () ) {
    Ok (straddle) => straddle,
    Err (e) => return StageAndCommitAnswer::Refused ( format! (
      "Cannot stage and commit: {}", e )), };
  if ! confirmed {
    let summary : ReadinessSummary = {
      let store : MutexGuard<ReadinessStore> =
        env . runtime . lock_readiness_store ();
      straddle . change_leaves () . readiness_summary (&store) };
    let pre_staged : Vec<PreStagedGitrepo> =
      pre_staged_entries_of_owned_gitrepos (&runtime . config);
    if ! summary . is_complete () || ! pre_staged . is_empty () {
      return StageAndCommitAnswer::NeedsConfirmation {
        summary, pre_staged }; }}
  let outcomes : Vec<(SkgrepoName, SkgrepoCommitOutcome)> =
    stage_and_commit_owned_skgrepos (&runtime . config, message);
  let mut errors : Vec<String> = Vec::new ();
  match current_graph_straddle_in_slot (
    &mut views_state . graph_straddle, &runtime . config,
    runtime . graph . clone () ) {
    Err (e) => errors . push ( format! (
      "The graph straddle could not be rebuilt after the commits: {}", e )),
    Ok (rebuilt) => {
      let mut store : MutexGuard<ReadinessStore> =
        env . runtime . lock_readiness_store ();
      store . drop_marks_of_vanished_leaves (rebuilt . change_leaves ());
      if let Err (e) = store . save (&runtime . config . data_root) {
        errors . push ( format! (
          "The readiness marks were not written to disk: {}", e )); }}, }
  StageAndCommitAnswer::Committed { outcomes, errors } }

/// Format, for each answer:
///   ((errors ("REFUSAL")) (warnings ()))
///   ((needs-confirmation . t) [(pre-staged (("GITREPO" "PATH"...) ...))]
///    (readiness-summary ...) (errors ()) (warnings ()))
///   ((commits (("SKGREPO" "COMMIT-ID") ...)) (errors (...)) (warnings ()))
/// The readiness summary has the save response's form, with an
/// not-editable count of 0. 'pre-staged' is there only when some
/// gitrepo had entries staged already. 'commits' lists the skgrepos git-committed; a
/// skgrepo without changes is in neither list, a failed one is an
/// error.
pub fn stage_and_commit_response_sexp (
  answer : &StageAndCommitAnswer,
) -> String {
  match answer {
    StageAndCommitAnswer::Refused (error) =>
      sexp_to_client_string ( &readiness_response_sexp (
        Vec::new (), None, &[ error . clone () ], &[] )),
    StageAndCommitAnswer::NeedsConfirmation { summary, pre_staged } => {
      let trailing_entries : Vec<Sexp> =
        readiness_response_trailing_entries (
          Some ( &BufferReadinessSummary {
            summary : summary . clone (), changes_not_editable_here : 0 } ),
          &[], &[] );
      let entries : Vec<String> =
        once ( sexp_to_client_string (
          &true_entry_sexp ("needs-confirmation") ))
        . chain ( ( ! pre_staged . is_empty () ) . then (
          || pre_staged_sexp (pre_staged) ))
        . chain ( trailing_entries . iter () . map (sexp_to_client_string) )
        . collect ();
      format! ( "({})", entries . join (" ") ) },
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
      sexp_to_client_string ( &readiness_response_sexp (
        vec! [ Sexp::List ( vec! [
          Sexp::Atom ( Atom::S ( "commits" . to_string () )),
          Sexp::List (commits) ] ) ],
        None, &all_errors, &[] )) }, }}

/// Format: (pre-staged (("GITREPO" "PATH"...) ...))
/// Printed by hand, each path through 'quote_sexp_string': the 'sexp'
/// crate prints an atom bare unless it holds whitespace or a quote,
/// and a bare path holding ';' or '(' would not read back as one atom.
fn pre_staged_sexp (
  pre_staged : &[PreStagedGitrepo],
) -> String {
  let path_atom = |path : &Path| -> String {
    quote_sexp_string ( &path . display () . to_string () ) };
  let gitrepo_lists : Vec<String> =
    pre_staged . iter ()
    . map ( |gitrepo| format! (
      "({})",
      once ( gitrepo . gitrepo . as_path () )
        . chain ( gitrepo . paths . iter () . map ( PathBuf::as_path ) )
        . map (path_atom)
        . collect::<Vec<String>> ()
        . join (" ") ))
    . collect ();
  format! ( "(pre-staged ({}))", gitrepo_lists . join (" ") ) }
