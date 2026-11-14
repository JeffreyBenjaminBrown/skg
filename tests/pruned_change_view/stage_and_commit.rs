// The "stage and commit" request ('docs/diff-view.org', "Stage and
// commit"), over the 'buffers' fixture, whose
// owned skgrepos public and private both have changes, deletions
// among them.

use crate::git_fixture::{Fixture, graph_straddle};
use crate::pruned_change_view::common::{
  all_changes_buffer, open_all_changes_buffer, rerendered_view_content,
  send_stage_and_commit_request, viewforest_id_of, with_fixture};

use skg::diff_view::change_leaves::{ChangeLeaves, LeafKey};
use skg::graph_straddle::types::GraphStraddle;
use skg::diff_view::processed_store::ProcessedStore;
use skg::serve::handlers::stage_and_commit::{
  CommitReport, CommitScope, StageAndCommitAnswer, stage_and_commit,
  stage_and_commit_response_sexp};
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::test_utils::skg_env_from_parts;
use skg::types::env::SkgEnv;
use skg::types::views_state::DiffViewBufferKind;

use git2::{Index, Oid, Repository, Status, StatusOptions, Tree};
use std::error::Error;
use std::fs;
use std::path::Path;
use std::sync::MutexGuard;

const SKGREPOS : [&str; 2] = [ "public", "private" ];

const UNCONFIRMED : CommitScope = CommitScope::Everything { confirmed : false };
const CONFIRMED   : CommitScope = CommitScope::Everything { confirmed : true };

fn all (
  f : &Fixture,
) -> Result<SkgrepoRestriction, Box<dyn Error>> {
  Ok ( SkgrepoRestriction::unrestricted (&f . config) ) }

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

/// Mark every leaf of the change-forest processed, in the store 'env' holds.
fn mark_every_leaf (
  f   : &Fixture,
  env : &SkgEnv,
) -> Result<(), Box<dyn Error>> {
  let straddle : GraphStraddle = graph_straddle (f) ?;
  let forest : &ChangeLeaves = straddle . change_leaves ();
  let mut store : MutexGuard<ProcessedStore> =
    env . runtime . processed_store . lock () . unwrap ();
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
      &env, &mut f . views_state, &all_skgrepos, "the message", UNCONFIRMED );
    let StageAndCommitAnswer::NeedsConfirmation { summary } = &answer
      else { panic! ("expected a request for confirmation, got {:?}", answer) };
    assert! ( summary . changed_nodes > 0 && summary . processed_nodes < summary . changed_nodes,
              "{:?}", summary );
    let response : String = stage_and_commit_response_sexp (&answer);
    assert! ( response . starts_with ( "((needs-confirmation . t) (processed-summary (processed " ),
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
      &env, &mut f . views_state, &all_skgrepos, "both at once", CONFIRMED );
    let StageAndCommitAnswer::Committed (CommitReport { commits, errors, left_nodes, waiting, .. }) = &answer
      else { panic! ("expected commits, got {:?}", answer) };
    assert! ( errors . is_empty (), "{:?}", errors );
    assert_eq! ( commits . len (), 2, "{:?}", commits );
    assert_eq! ( (*left_nodes, waiting . len ()), (0, 0) );
    assert_everything_committed (f, "both at once") ?;
    let response : String = stage_and_commit_response_sexp (&answer);
    assert! ( response . starts_with ( "((commits ((\"public\" " ),
              "{}", response );
    Ok (( )) }) }

/// When every changed file is processed no confirmation is needed, and
/// the commit lapses every mark.
#[test]
fn a_complete_summary_commits_without_confirmation_and_lapses_the_marks (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("buffers", &SKGREPOS, |f| {
    let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
    let all_skgrepos : SkgrepoRestriction = all (f) ?;
    mark_every_leaf (f, &env) ?;
    let answer : StageAndCommitAnswer = stage_and_commit (
      &env, &mut f . views_state, &all_skgrepos, "all processed", UNCONFIRMED );
    assert! ( matches! (answer, StageAndCommitAnswer::Committed (_)),
              "{:?}", answer );
    assert_everything_committed (f, "all processed") ?;
    assert! ( env . runtime . processed_store . lock () . unwrap () . marks . is_empty () );
    assert! ( ProcessedStore::load ( &f . config . processed_marks_path () ) . marks . is_empty () );
    Ok (( )) }) }

/// A file the user staged in one gitrepo before a confirmed commit of
/// everything is neither committed nor unstaged: the commit's tree is
/// HEAD's with the changed '.skg' files, not the git index.
#[test]
fn an_entry_staged_by_hand_is_neither_committed_nor_unstaged (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("buffers", &SKGREPOS, |f| {
    let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
    let all_skgrepos : SkgrepoRestriction = all (f) ?;
    { let public : Repository = gitrepo (f, "public") ?;
      fs::write ( f . root . path () . join ("public") . join ("notes.txt"),
                  "staged by hand\n" ) ?;
      let mut index : Index = public . index () ?;
      index . add_path ( Path::new ("notes.txt") ) ?;
      index . write () ?; }
    let answer : StageAndCommitAnswer = stage_and_commit (
      &env, &mut f . views_state, &all_skgrepos, "with notes", CONFIRMED );
    assert! ( matches! (answer, StageAndCommitAnswer::Committed (_)),
              "{:?}", answer );
    assert_everything_committed (f, "with notes") ?;
    { let public : Repository = gitrepo (f, "public") ?;
      let committed_tree : Tree = public . head () ? . peel_to_tree () ?;
      assert! ( committed_tree . get_path ( Path::new ("notes.txt") ) . is_err (),
                "the file staged by hand is not committed" );
      assert! ( public . index () ? . get_path ( Path::new ("notes.txt"), 0 ) . is_some (),
                "the file staged by hand stays staged" ); }
    Ok (( )) }) }

/// A gitrepo without a git author refuses the commit before anything
/// is committed in any skgrepo: every HEAD stays, and every git index
/// still equals its HEAD. A user.name of "" in the gitrepo's own config
/// hides any global one, so the test does not depend on the machine's
/// git config.
#[test]
fn a_missing_git_author_commits_nothing_anywhere (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("buffers", &SKGREPOS, |f| {
    let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
    let all_skgrepos : SkgrepoRestriction = all (f) ?;
    gitrepo (f, "public") ? . config () ? . set_str ("user.name", "") ?;
    let heads : Vec<Oid> = SKGREPOS . iter ()
      . map ( |skgrepo| head_id (f, skgrepo) ) . collect::<Result<_, _>> () ?;
    let changed : Vec<usize> = SKGREPOS . iter ()
      . map ( |skgrepo| changed_skg_files (f, skgrepo) )
      . collect::<Result<_, _>> () ?;
    let answer : StageAndCommitAnswer = stage_and_commit (
      &env, &mut f . views_state, &all_skgrepos, "no author", CONFIRMED );
    assert! ( matches! ( &answer, StageAndCommitAnswer::Refused (e)
                         if e . contains ("skgrepo public: no git author is configured") ),
              "{:?}", answer );
    for (i, skgrepo) in SKGREPOS . iter () . enumerate () {
      assert_eq! ( head_id (f, skgrepo) ?, heads [i], "skgrepo {}", skgrepo );
      assert_eq! ( changed_skg_files (f, skgrepo) ?, changed [i], "skgrepo {}", skgrepo );
      let gitrepo : Repository = gitrepo (f, skgrepo) ?;
      let head_tree : Tree = gitrepo . head () ? . peel_to_tree () ?;
      let staged : usize = gitrepo
        . diff_tree_to_index ( Some (&head_tree), None, None ) ? . deltas () . count ();
      assert_eq! ( staged, 0, "nothing may be staged in {}", skgrepo ); }
    Ok (( )) }) }

#[test]
fn a_request_without_changes_commits_nothing (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("buffers", &SKGREPOS, |f| {
    let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
    let all_skgrepos : SkgrepoRestriction = all (f) ?;
    stage_and_commit ( &env, &mut f . views_state, &all_skgrepos, "first", CONFIRMED );
    let heads : Vec<Oid> = SKGREPOS . iter ()
      . map ( |skgrepo| head_id (f, skgrepo) ) . collect::<Result<_, _>> () ?;
    let answer : StageAndCommitAnswer = stage_and_commit (
      &env, &mut f . views_state, &all_skgrepos, "second", UNCONFIRMED );
    assert_eq! ( stage_and_commit_response_sexp (&answer),
                 "((commits ()) (committed-nodes 0) (left-nodes 0) (waiting ()) (errors ()) (warnings ()))" );
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
      "((request . \"stage and commit\") (message-lines \"one message\"))\n" ) ?;
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
      "((request . \"stage and commit\") (message-lines \"one message\") (confirmed . t))\n" ) ?;
    assert! ( second . contains ("(commits ((\"public\" ")
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
                  " ghost (gnid ", "homeSkgrepoMove", "(processed" ] {
      assert! ( ! content . contains (fact),
                "no change fact {:?} remains:\n{}", fact, content ); }
    assert! ( f . views_state . open_views . views
              . get (&viewforest_id_of (DiffViewBufferKind::AllChanges))
              . is_some_and ( |state| ! state . pids . is_empty ()
                              && state . diffView_buffer_kind == Some (DiffViewBufferKind::AllChanges) ));
    Ok (( )) }) }

/// A message of several lines, sent one string per line, is
/// git-committed with those lines joined by newlines. A backslash and
/// an 'n' typed in a line stay those two characters.
#[test]
fn a_message_of_several_lines_is_committed_as_written (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("buffers", &SKGREPOS, |f| {
    let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
    let response : String = send_stage_and_commit_request ( f, &env,
      "((request . \"stage and commit\") (message-lines \"summary\" \"\" \"  indented \\\"quoted\\\" back\\\\n slash\") (confirmed . t))\n" ) ?;
    assert! ( response . contains ("(commits ((\"public\" "), "{}", response );
    assert_everything_committed (
      f, "summary\n\n  indented \"quoted\" back\\n slash" ) ?;
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
    let restriction : SkgrepoRestriction =
      SkgrepoRestriction::named (&f . config, "public" . into ()) ?;
    let head : Oid = head_id (f, "public") ?;
    let answer : StageAndCommitAnswer = stage_and_commit (
      &env, &mut f . views_state, &restriction, "message", CONFIRMED );
    assert! ( matches! ( &answer, StageAndCommitAnswer::Refused (e)
                         if e . contains ("requires no skgrepo restriction") ),
              "{:?}", answer );
    assert_eq! ( head_id (f, "public") ?, head );
    Ok (( )) }) }
