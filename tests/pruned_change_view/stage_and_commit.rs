// The "stage and commit" request ('docs/diff-view.org', "Stage and
// commit"), over the 'buffers' fixture, whose
// owned skgrepos public and private both have changes, deletions
// among them.

use crate::git_fixture::{Fixture, graph_straddle};
use crate::pruned_change_view::common::{
  all_changes_buffer, open_all_changes_buffer, rerendered_view_content,
  send_stage_and_commit_request, viewforest_id_of, with_fixture};

use skg::diff_view::change_leaves::{ChangeLeaves, LeafKey};
use skg::git_ops::stage_and_commit::AlreadyStagedGitrepo;
use skg::graph_straddle::types::GraphStraddle;
use skg::diff_view::readiness_store::ReadinessStore;
use skg::serve::handlers::stage_and_commit::{
  StageAndCommitAnswer, stage_and_commit, stage_and_commit_response_sexp};
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::test_utils::skg_env_from_parts;
use skg::types::env::SkgEnv;
use skg::types::views_state::DiffViewBufferKind;

use git2::{Index, Oid, Repository, Status, StatusOptions, Tree};
use std::error::Error;
use std::fs;
use std::path::{Path, PathBuf};
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
                  && entry . path () . is_ok_and ( |p| p . ends_with (".skg") ))
       . count () ) }

/// Mark every leaf of the change-forest ready, in the store 'env' holds.
fn mark_every_leaf (
  f   : &Fixture,
  env : &SkgEnv,
) -> Result<(), Box<dyn Error>> {
  let straddle : GraphStraddle = graph_straddle (f) ?;
  let forest : &ChangeLeaves = straddle . change_leaves ();
  let mut store : MutexGuard<ReadinessStore> =
    env . runtime . readiness_store . lock () . unwrap ();
  let keys : Vec<LeafKey> = forest . recorders ()
    . flat_map ( |recorder| forest . leaves_under (recorder) . iter ()
                 . map ( |leaf| leaf . key . clone () ) )
    . collect ();
  for key in keys {
    let fingerprint : u64 = forest . leaf_fingerprint (&key) . unwrap ();
    store . mark (key, fingerprint); }
  Ok (( )) }

/// Each owned skgrepo now has the one message at HEAD and no changed
/// '.skg' file, and the diff view has nothing to show.
fn assert_everything_committed (
  f       : &Fixture,
  message : &str,
) -> Result<(), Box<dyn Error>> {
  for skgrepo in SKGREPOS {
    let gitrepo : Repository = gitrepo (f, skgrepo) ?;
    assert_eq! ( gitrepo . head () ? . peel_to_commit () ? . message () . ok (),
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
    let StageAndCommitAnswer::NeedsConfirmation { summary, already_staged } = &answer
      else { panic! ("expected a request for confirmation, got {:?}", answer) };
    assert! ( summary . changed_files > 0 && summary . ready_files < summary . changed_files,
              "{:?}", summary );
    assert! ( already_staged . is_empty (), "{:?}", already_staged );
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

/// A file the user staged in one gitrepo before the request stops even
/// a fully ready commit for confirmation, naming the gitrepo and the
/// file; the confirmed resend commits it with the '.skg' files.
#[test]
fn an_already_staged_entry_needs_confirmation_and_is_then_committed (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("buffers", &SKGREPOS, |f| {
    let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
    let all_skgrepos : SkgrepoRestriction = all (f) ?;
    mark_every_leaf (f, &env) ?;
    { let public : Repository = gitrepo (f, "public") ?;
      fs::write ( f . root . path () . join ("public") . join ("notes.txt"),
                  "staged by hand\n" ) ?;
      let mut index : Index = public . index () ?;
      index . add_path ( Path::new ("notes.txt") ) ?;
      index . write () ?; }
    let heads : Vec<Oid> = SKGREPOS . iter ()
      . map ( |skgrepo| head_id (f, skgrepo) ) . collect::<Result<_, _>> () ?;
    let answer : StageAndCommitAnswer = stage_and_commit (
      &env, &mut f . views_state, &all_skgrepos, "with notes", false );
    let StageAndCommitAnswer::NeedsConfirmation { summary, already_staged } = &answer
      else { panic! ("expected a request for confirmation, got {:?}", answer) };
    assert! ( summary . is_complete (), "{:?}", summary );
    let [ AlreadyStagedGitrepo { gitrepo : public_root, paths } ] = already_staged . as_slice ()
      else { panic! ("expected one gitrepo, got {:?}", already_staged) };
    assert! ( public_root . ends_with ("public"), "{:?}", public_root );
    assert_eq! ( paths, &vec! [ PathBuf::from ("notes.txt") ] );
    let response : String = stage_and_commit_response_sexp (&answer);
    assert! ( response . starts_with ( &format! (
      "((needs-confirmation . t) (already-staged ((\"{}\" \"notes.txt\"))) (readiness-summary (ready ",
      public_root . display () )),
              "{}", response );
    for (i, skgrepo) in SKGREPOS . iter () . enumerate () {
      assert_eq! ( head_id (f, skgrepo) ?, heads [i], "skgrepo {}", skgrepo ); }
    let confirmed : StageAndCommitAnswer = stage_and_commit (
      &env, &mut f . views_state, &all_skgrepos, "with notes", true );
    assert! ( matches! (confirmed, StageAndCommitAnswer::Committed { .. }),
              "{:?}", confirmed );
    assert_everything_committed (f, "with notes") ?;
    { let public : Repository = gitrepo (f, "public") ?;
      let committed_tree : Tree = public . head () ? . peel_to_tree () ?;
      assert! ( committed_tree . get_path ( Path::new ("notes.txt") ) . is_ok (),
                "the already-staged file is committed" ); }
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
/// changes" buffer is rerendered in the rerender stream that follows:
/// its roots remain, and carry no change facts. The ghost root of
/// acqd, which acq acquired, is an occurrence of acq now, where it
/// stood, since the commit left acqd only an extra ID of acq (the ID
/// rule: the truth has the node).
#[test]
fn a_confirmed_resend_commits_and_rerenders_the_open_buffers (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("buffers", &SKGREPOS, |f| {
    let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
    let buffer : String = open_all_changes_buffer (f) ?;
    let first : String = send_stage_and_commit_request ( f, &env,
      "((request . \"stage and commit\") (message . \"one message\"))\n" ) ?;
    assert! ( first . contains ("stage-and-commit")
              && first . contains ("(needs-confirmation . t)")
              && first . contains ("(lock-views ())")
              && ! first . contains ("rerender-view"),
              "{}", first );
    let roots : Vec<String> = // acqd's ghost is acq's occurrence after the commit
      buffer . lines ()
      . filter ( |line| line . starts_with ("* ") )
      . map ( |line| root_id (line) )
      . map ( |root| if root == "acqd" { "acq" . to_string () } else { root } )
      . collect ();
    let second : String = send_stage_and_commit_request ( f, &env,
      "((request . \"stage and commit\") (message . \"one message\") (confirmed . t))\n" ) ?;
    assert! ( second . contains ("(commits ((public ")
              && second . contains ("(lock-views (\"diff-view: all changes\"))")
              && second . contains ("(view-id \"diff-view: all changes\") (content \"* "),
              "{}", second );
    assert_everything_committed (f, "one message") ?;
    let content : String = rerendered_view_content (&second);
    let roots_after : Vec<String> =
      content . lines () . filter ( |line| line . starts_with ("* ") )
      . map ( |line| root_id (line) ) . collect ();
    assert_eq! ( roots_after, roots, "the roots remain:\n{}", content );
    for fact in [ "(diff ", "changed)", "changesetMarker", "oldText",
                  " ghost (gnid ", "homeSkgrepoMove", "(ready" ] {
      assert! ( ! content . contains (fact),
                "no change fact {:?} remains:\n{}", fact, content ); }
    assert! ( f . views_state . open_views . views
              . get (&viewforest_id_of (DiffViewBufferKind::AllChanges))
              . is_some_and ( |state| ! state . pids . is_empty ()
                              && state . diffView_buffer_kind == Some (DiffViewBufferKind::AllChanges) ));
    Ok (( )) }) }

/// The ID of a root line, whatever kind of vognode it is.
fn root_id (
  line : &str,
) -> String {
  let start : usize = line . find ("(gnid ") . expect ("a root has an ID") + 6;
  line [start ..] . split (')') . next () . unwrap () . to_string () }

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
