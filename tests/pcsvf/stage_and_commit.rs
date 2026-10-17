// The "stage and commit" request ('TODO/total-diff-view/2_plan.org',
// "the summary and the commit"), over the 'buffers' fixture, whose
// owned skgrepos public and private both have changes, deletions
// among them.

use crate::pcsvf::common::git_fixture_helpers::mk_test_tcp_stream_pair;
use crate::pcsvf::common::{
  Fixture, all_changes_buffer, graph_pair, open_all_changes_buffer,
  view_id_of, with_fixture};

use skg::change_forest::{ChangeForest, LeafKey};
use skg::graph_pair::types::GraphPair;
use skg::readiness_store::ReadinessStore;
use skg::serve::handlers::stage_and_commit::{
  StageAndCommitAnswer, handle_stage_and_commit_request, stage_and_commit,
  stage_and_commit_response_sexp};
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::test_utils::skg_env_from_parts;
use skg::types::env::SkgEnv;
use skg::types::views_state::TdvBuffer;

use git2::{Oid, Repository, Status, StatusOptions};
use std::error::Error;
use std::io::Read;
use std::net::TcpStream;
use std::sync::MutexGuard;

const SKGREPOS : [&str; 2] = [ "public", "private" ];

fn all (
  f : &Fixture,
) -> Result<SkgrepoRestriction, Box<dyn Error>> {
  Ok ( SkgrepoRestriction::named (&f . config, "all" . into ()) ? ) }

fn gitrepo (
  f       : &Fixture,
  skgrepo : &str,
) -> Result<Repository, Box<dyn Error>> {
  Ok ( Repository::open ( f . root . path () . join (skgrepo) ) ? ) }

fn head_id (
  f       : &Fixture,
  skgrepo : &str,
) -> Result<Oid, Box<dyn Error>> {
  Ok ( gitrepo (f, skgrepo) ? . head () ? . peel_to_commit () ? . id () ) }

/// How many '.skg' files of the skgrepo differ between HEAD and its
/// worktree, staged or not.
fn changed_skg_files (
  f       : &Fixture,
  skgrepo : &str,
) -> Result<usize, Box<dyn Error>> {
  let gitrepo : Repository = gitrepo (f, skgrepo) ?;
  let mut options : StatusOptions = StatusOptions::new ();
  options . include_untracked (true);
  Ok ( gitrepo . statuses ( Some (&mut options) ) ? . iter ()
       . filter ( |entry| entry . status () != Status::CURRENT
                  && entry . path () . is_some_and ( |p| p . ends_with (".skg") ))
       . count () ) }

/// Mark every leaf of the change-forest ready, in the store 'env' holds.
fn mark_every_leaf (
  f   : &Fixture,
  env : &SkgEnv,
) -> Result<(), Box<dyn Error>> {
  let pair : GraphPair = graph_pair (f) ?;
  let forest : &ChangeForest = pair . change_forest ();
  let mut store : MutexGuard<ReadinessStore> =
    env . runtime . readiness_store . lock () . unwrap ();
  let keys : Vec<LeafKey> = forest . owners ()
    . flat_map ( |owner| forest . leaves_under (owner) . iter ()
                 . map ( |leaf| leaf . key . clone () ) )
    . collect ();
  for key in keys {
    let fingerprint : u64 = forest . leaf_fingerprint (&key) . unwrap ();
    store . mark (key, fingerprint); }
  Ok (( )) }

/// Each owned skgrepo now has the one message at HEAD and no changed
/// '.skg' file, and the total diff view has nothing to show.
fn assert_everything_committed (
  f       : &Fixture,
  message : &str,
) -> Result<(), Box<dyn Error>> {
  for skgrepo in SKGREPOS {
    let gitrepo : Repository = gitrepo (f, skgrepo) ?;
    assert_eq! ( gitrepo . head () ? . peel_to_commit () ? . message (),
                 Some (message), "skgrepo {}", skgrepo );
    assert_eq! ( changed_skg_files (f, skgrepo) ?, 0,
                 "skgrepo {} has uncommitted changes", skgrepo ); }
  assert_eq! ( all_changes_buffer (f) ?, None );
  Ok (( )) }

#[test]
fn an_incomplete_summary_needs_confirmation_and_commits_nothing (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("buffers", &SKGREPOS, |f| {
    let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
    let all_skgrepos : SkgrepoRestriction = all (f) ?;
    let heads : Vec<Oid> = SKGREPOS . iter ()
      . map ( |skgrepo| head_id (f, skgrepo) ) . collect::<Result<_, _>> () ?;
    let changed : Vec<usize> = SKGREPOS . iter ()
      . map ( |skgrepo| changed_skg_files (f, skgrepo) )
      . collect::<Result<_, _>> () ?;
    let answer : StageAndCommitAnswer = stage_and_commit (
      &env, &mut f . views_state, &all_skgrepos, "the message", false );
    let StageAndCommitAnswer::NeedsConfirmation (summary) = &answer
      else { panic! ("expected a request for confirmation, got {:?}", answer) };
    assert! ( summary . changed_files > 0 && summary . ready_files < summary . changed_files,
              "{:?}", summary );
    let response : String = stage_and_commit_response_sexp (&answer);
    assert! ( response . starts_with ( "((needs-confirmation . t) (readiness-summary (ready " ),
              "{}", response );
    for (i, skgrepo) in SKGREPOS . iter () . enumerate () {
      assert_eq! ( head_id (f, skgrepo) ?, heads [i], "skgrepo {}", skgrepo );
      assert_eq! ( changed_skg_files (f, skgrepo) ?, changed [i],
                   "skgrepo {}: nothing may be staged", skgrepo ); }
    Ok (( )) }) }

#[test]
fn a_confirmed_request_commits_every_owned_skgrepo_with_one_message (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("buffers", &SKGREPOS, |f| {
    let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
    let all_skgrepos : SkgrepoRestriction = all (f) ?;
    let answer : StageAndCommitAnswer = stage_and_commit (
      &env, &mut f . views_state, &all_skgrepos, "both at once", true );
    let StageAndCommitAnswer::Committed { outcomes, errors } = &answer
      else { panic! ("expected commits, got {:?}", answer) };
    assert! ( errors . is_empty (), "{:?}", errors );
    assert_eq! ( outcomes . len (), 2 );
    assert! ( outcomes . iter () . all ( |(_, outcome)| matches! (outcome, Ok (Some (_))) ),
              "{:?}", outcomes );
    assert_everything_committed (f, "both at once") ?;
    let response : String = stage_and_commit_response_sexp (&answer);
    assert! ( response . starts_with ( "((commits ((public " ),
              "{}", response );
    Ok (( )) }) }

/// When every changed file is ready no confirmation is needed, and
/// the commit lapses every mark.
#[test]
fn a_complete_summary_commits_without_confirmation_and_lapses_the_marks (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("buffers", &SKGREPOS, |f| {
    let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
    let all_skgrepos : SkgrepoRestriction = all (f) ?;
    mark_every_leaf (f, &env) ?;
    let answer : StageAndCommitAnswer = stage_and_commit (
      &env, &mut f . views_state, &all_skgrepos, "all ready", false );
    assert! ( matches! (answer, StageAndCommitAnswer::Committed { .. }),
              "{:?}", answer );
    assert_everything_committed (f, "all ready") ?;
    assert! ( env . runtime . readiness_store . lock () . unwrap () . marks . is_empty () );
    assert! ( ReadinessStore::load (&f . config . data_root) . marks . is_empty () );
    Ok (( )) }) }

#[test]
fn a_request_without_changes_commits_nothing (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("buffers", &SKGREPOS, |f| {
    let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
    let all_skgrepos : SkgrepoRestriction = all (f) ?;
    stage_and_commit ( &env, &mut f . views_state, &all_skgrepos, "first", true );
    let heads : Vec<Oid> = SKGREPOS . iter ()
      . map ( |skgrepo| head_id (f, skgrepo) ) . collect::<Result<_, _>> () ?;
    let answer : StageAndCommitAnswer = stage_and_commit (
      &env, &mut f . views_state, &all_skgrepos, "second", false );
    let StageAndCommitAnswer::Committed { outcomes, .. } = &answer
      else { panic! ("expected no confirmation, got {:?}", answer) };
    assert! ( outcomes . iter () . all ( |(_, outcome)| matches! (outcome, Ok (None)) ),
              "{:?}", outcomes );
    for (i, skgrepo) in SKGREPOS . iter () . enumerate () {
      assert_eq! ( head_id (f, skgrepo) ?, heads [i], "skgrepo {}", skgrepo ); }
    Ok (( )) }) }

/// At the handler: the confirmed resend commits, and the open "all
/// changes" buffer is redrawn empty in the rerender stream that
/// follows.
#[test]
fn a_confirmed_resend_commits_and_redraws_the_open_buffers (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("buffers", &SKGREPOS, |f| {
    let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
    open_all_changes_buffer (f) ?;
    let send = |f : &mut Fixture, request : &str| -> Result<String, Box<dyn Error>> {
      let all_skgrepos : SkgrepoRestriction = all (f) ?;
      let (mut stream, mut read_end) : (TcpStream, TcpStream) =
        mk_test_tcp_stream_pair ();
      handle_stage_and_commit_request (
        &mut stream, request, &env, &mut f . views_state, &all_skgrepos );
      drop (stream);
      let mut response : String = String::new ();
      read_end . read_to_string (&mut response) ?;
      Ok (response) };
    let first : String = send ( f,
      "((request . \"stage and commit\") (message . \"one message\"))\n" ) ?;
    assert! ( first . contains ("stage-and-commit")
              && first . contains ("(needs-confirmation . t)")
              && first . contains ("(lock-views ())")
              && ! first . contains ("rerender-view"),
              "{}", first );
    let second : String = send ( f,
      "((request . \"stage and commit\") (message . \"one message\") (confirmed . t))\n" ) ?;
    assert! ( second . contains ("(commits ((public ")
              && second . contains ("(lock-views (\"diff-view: all changes\"))")
              // The sexp crate writes an empty string as nothing.
              && second . contains (
                "(view-id \"diff-view: all changes\") (content ))" ),
              "{}", second );
    assert_everything_committed (f, "one message") ?;
    assert! ( f . views_state . open_views . views
              . get (&view_id_of (TdvBuffer::AllChanges))
              . is_some_and ( |state| state . pids . is_empty ()
                              && state . tdv_buffer == Some (TdvBuffer::AllChanges) ));
    Ok (( )) }) }

#[test]
fn a_request_is_refused_under_a_skgrepo_restriction (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("buffers", &SKGREPOS, |f| {
    let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
    let mut restriction : SkgrepoRestriction = all (f) ?;
    restriction . name = "some" . into ();
    let head : Oid = head_id (f, "public") ?;
    let answer : StageAndCommitAnswer = stage_and_commit (
      &env, &mut f . views_state, &restriction, "message", true );
    assert! ( matches! ( &answer, StageAndCommitAnswer::Refused (e)
                         if e . contains ("requires no skgrepo restriction") ),
              "{:?}", answer );
    assert_eq! ( head_id (f, "public") ?, head );
    Ok (( )) }) }
