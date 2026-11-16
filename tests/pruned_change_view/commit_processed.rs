// The commit of the processed nodes ('docs/diff-view.org', "Stage and
// commit"), each case a scratch fixture built here: its skgrepos, each
// a folder of a gitrepo (several may share one), and its '.skg' files
// at HEAD and in the worktree. Every test asserts what HEAD's tree
// holds, what the git index holds, that the worktree is untouched,
// and which marks are left.

use crate::git_fixture::{Fixture, graph_straddle, save_cleanly};
use crate::git_fixture_helpers::configure_git_user;
use crate::pruned_change_view::common::{all_changes_view, open_all_changes_buffer};

use skg::dbs::init::create_empty_tantivy_index;
use skg::diff_view::change_leaves::{
  ChangeLeaves, LeafKey, NodeAspect, ProcessedSummary, WaitReason};
use skg::graph_straddle::types::GraphStraddle;
use skg::git_ops::stage_and_commit::{
  ChangedSkgPath, PreparedCommits, changed_skg_paths_by_stem, commit_prepared,
  prepare_commits};
use skg::serve::ViewsState;
use skg::serve::handlers::stage_and_commit::{
  CommitReport, CommitScope, StageAndCommitAnswer, commit_nodes,
  stage_and_commit, stage_and_commit_response_sexp};
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::test_utils::{cleanup_test_tantivy, graph_handle_from_config, skg_env_from_parts};
use skg::types::env::SkgEnv;
use skg::types::misc::{ID, SkgConfig, Skgrepo, SkgrepoName};
use skg::types::views_state::OpenViews;

use git2::{Index, IndexAddOption, ObjectType, Repository, Signature, Status, StatusOptions, Tree, TreeWalkMode, TreeWalkResult};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::error::Error;
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

type TestResult = Result<(), Box<dyn Error>>;

/// One '.skg' file of a case: its skgrepo, its node's pid, and its
/// contents at HEAD and in the worktree (None where it is absent).
type CaseFile<'a> = (&'a str, &'a str, Option<&'a str>, Option<&'a str>);

/// The skgrepos 'skgrepos', each '(NAME, FOLDER)', in privacy order;
/// FOLDER's first component is its gitrepo. A skgrepo named 'theirs'
/// is foreign; every other is owned. Each gitrepo commits the
/// files' HEAD contents, then the worktree contents replace them.
fn scratch (
  skgrepos : &[(&str, &str)],
  files    : &[CaseFile],
) -> Result<Fixture, Box<dyn Error>> {
  let root : TempDir = TempDir::new () ?;
  let folder_of : HashMap<&str, PathBuf> = skgrepos . iter ()
    . map ( |(name, folder)| ( *name, root . path () . join (folder) )) . collect ();
  for folder in folder_of . values () { fs::create_dir_all (folder) ?; }
  let write_side = |at_head : bool| -> TestResult {
    for file in files {
      let path : PathBuf = folder_of [file . 0] . join ( format! ("{}.skg", file . 1) );
      match if at_head { file . 2 } else { file . 3 } {
        Some (text) => fs::write (&path, text) ?,
        None => if path . exists () { fs::remove_file (&path) ?; }, }}
    Ok (( )) };
  write_side (true) ?;
  let gitrepos : BTreeSet<PathBuf> = skgrepos . iter ()
    . map ( |(_, folder)| root . path () . join (
      Path::new (folder) . components () . next () . unwrap () ))
    . collect ();
  for gitrepo_path in &gitrepos {
    let gitrepo : Repository = Repository::init (gitrepo_path) ?;
    configure_git_user (&gitrepo);
    let mut index : Index = gitrepo . index () ?;
    index . add_all ( ["*"], IndexAddOption::DEFAULT, None ) ?;
    index . write () ?;
    let tree : Tree = gitrepo . find_tree ( index . write_tree () ? ) ?;
    let signature : Signature = gitrepo . signature () ?;
    gitrepo . commit ( Some ("HEAD"), &signature, &signature, "HEAD", &tree, &[] ) ?; }
  write_side (false) ?;
  let skgrepo_map : HashMap<SkgrepoName, Skgrepo> = skgrepos . iter ()
    . map ( |(name, _)| ( SkgrepoName::from (*name), Skgrepo {
      name : SkgrepoName::from (*name), abbreviation : None,
      path : folder_of [name] . clone (), owned : *name != "theirs" } ))
    . collect ();
  let mut config : SkgConfig = SkgConfig::fromSkgreposAndTantivyFolder (
    skgrepo_map, root . path () . join ("tantivy") . to_str () . unwrap () );
  config . skgrepo_order = skgrepos . iter ()
    . map ( |(name, _)| SkgrepoName::from (*name) ) . collect ();
  config . data_root = root . path () . to_path_buf ();
  let tantivy = create_empty_tantivy_index (&config . tantivy_folder) ?;
  let graph = graph_handle_from_config (&config) ?;
  Ok ( Fixture {
    root,
    skgrepos : skgrepos . iter () . filter ( |(name, _)| *name != "theirs" )
      . map ( |(name, _)| name . to_string () ) . collect (),
    config, tantivy, graph,
    views_state : ViewsState { open_views : OpenViews::new (), graph_straddle : None } } ) }

/// Run 'body' on the 'scratch' fixture, then remove its search index.
fn with_scratch (
  skgrepos : &[(&str, &str)],
  files    : &[CaseFile],
  body     : impl FnOnce (&mut Fixture, &SkgEnv) -> TestResult,
) -> TestResult {
  let mut f : Fixture = scratch (skgrepos, files) ?;
  let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
  let result : TestResult = body (&mut f, &env);
  cleanup_test_tantivy ( Some (&f . config . tantivy_folder) ) ?;
  result }

const MAIN : &[(&str, &str)] = &[ ("main", "main") ];
const TWO_GITREPOS : &[(&str, &str)] = &[ ("public", "public"), ("private", "private") ];
const ONE_GITREPO  : &[(&str, &str)] = &[ ("public", "g/public"), ("private", "g/private") ];

fn yaml (
  pid      : &str,
  title    : &str,
  contains : &[&str],
) -> String {
  let mut text : String = format! ( "pid: {}\ntitle: {}\n", pid, title );
  if ! contains . is_empty () {
    text . push_str ("contains:\n");
    for member in contains { text . push_str ( &format! ("- {}\n", member) ); }}
  text }

/// Mark processed every leaf the nodes 'pids' record, in 'env''s store.
fn mark_nodes (
  f    : &Fixture,
  env  : &SkgEnv,
  pids : &[&str],
) -> TestResult {
  let straddle : GraphStraddle = graph_straddle (f) ?;
  let leaves : &ChangeLeaves = straddle . change_leaves ();
  let mut store = env . runtime . processed_store . lock () . unwrap ();
  for pid in pids {
    let recorded = leaves . leaves_under ( &ID::from (*pid) );
    assert! ( ! recorded . is_empty (), "{} records no leaf", pid );
    for leaf in recorded { store . mark ( leaf . key . clone (), leaf . fingerprint ); }}
  Ok (( )) }

fn marked_keys (
  env : &SkgEnv,
) -> HashSet<LeafKey> {
  env . runtime . processed_store . lock () . unwrap () . marks . keys () . cloned () . collect () }

fn commit (
  f     : &mut Fixture,
  env   : &SkgEnv,
  scope : CommitScope,
) -> StageAndCommitAnswer {
  let all : SkgrepoRestriction = SkgrepoRestriction::unrestricted (&f . config);
  stage_and_commit ( env, &mut f . views_state, &all, "the message", scope ) }

fn commit_processed (
  f   : &mut Fixture,
  env : &SkgEnv,
) -> StageAndCommitAnswer {
  commit (f, env, CommitScope::ProcessedOnly) }

fn report (
  answer : &StageAndCommitAnswer,
) -> &CommitReport {
  match answer {
    StageAndCommitAnswer::Committed (report) => {
      assert! ( report . errors . is_empty (), "{:?}", report . errors );
      report },
    other => panic! ("expected a commit's report, got {:?}", other), }}

fn gitrepo (
  f      : &Fixture,
  folder : &str,
) -> Result<Repository, Box<dyn Error>> {
  Ok ( Repository::open ( f . root . path () . join (folder) ) ? ) }

/// Every file HEAD's tree holds, by path, with its contents.
fn head_files (
  f      : &Fixture,
  folder : &str,
) -> Result<BTreeMap<String, String>, Box<dyn Error>> {
  let gitrepo : Repository = gitrepo (f, folder) ?;
  let tree : Tree = gitrepo . head () ? . peel_to_tree () ?;
  let mut files : BTreeMap<String, String> = BTreeMap::new ();
  tree . walk ( TreeWalkMode::PreOrder, |root, entry| {
    if entry . kind () == Some (ObjectType::Blob) {
      let blob = gitrepo . find_blob ( entry . id () ) . unwrap ();
      files . insert ( format! ( "{}{}", root, entry . name () . unwrap () ),
                       String::from_utf8_lossy ( blob . content () ) . to_string () ); }
    TreeWalkResult::Ok } ) ?;
  Ok (files) }

/// The paths whose git index entry differs from HEAD.
fn staged_paths (
  f      : &Fixture,
  folder : &str,
) -> Result<Vec<String>, Box<dyn Error>> {
  let gitrepo : Repository = gitrepo (f, folder) ?;
  let head_tree : Tree = gitrepo . head () ? . peel_to_tree () ?;
  Ok ( gitrepo . diff_tree_to_index ( Some (&head_tree), None, None ) ? . deltas ()
       . filter_map ( |delta| delta . new_file () . path () . or ( delta . old_file () . path () )
                      . map ( |path| path . display () . to_string () ))
       . collect () ) }

/// The paths 'git status' reports as differing anywhere, with their status.
fn statuses (
  f      : &Fixture,
  folder : &str,
) -> Result<BTreeMap<String, Status>, Box<dyn Error>> {
  let gitrepo : Repository = gitrepo (f, folder) ?;
  let mut options : StatusOptions = StatusOptions::new ();
  options . include_untracked (true) . recurse_untracked_dirs (true);
  Ok ( gitrepo . statuses ( Some (&mut options) ) ? . iter ()
       . filter ( |entry| entry . status () != Status::CURRENT )
       . map ( |entry| ( entry . path () . unwrap () . to_string (), entry . status () ))
       . collect () ) }

/// Every file under the fixture's root but the gitrepos' and the
/// search index's folders, by path, with its contents.
fn worktree_files (
  f : &Fixture,
) -> Result<BTreeMap<String, String>, Box<dyn Error>> {
  let mut files : BTreeMap<String, String> = BTreeMap::new ();
  let mut folders : Vec<PathBuf> = vec! [ f . root . path () . to_path_buf () ];
  while let Some (folder) = folders . pop () {
    for entry in fs::read_dir (&folder) ? {
      let path : PathBuf = entry ? . path ();
      let name : String = path . file_name () . unwrap () . to_string_lossy () . to_string ();
      if path . is_dir () {
        if name != ".git" && name != "tantivy" { folders . push (path); }
      } else if name . ends_with (".skg") || name . ends_with (".txt") {
        files . insert ( path . strip_prefix ( f . root . path () ) ? . display () . to_string (),
                         fs::read_to_string (&path) ? ); }} }
  Ok (files) }

/// HEAD's files in each of 'folders', and the worktree's files.
fn snapshot (
  f       : &Fixture,
  folders : &[&str],
) -> Result<(Vec<BTreeMap<String, String>>, BTreeMap<String, String>), Box<dyn Error>> {
  Ok (( folders . iter () . map ( |folder| head_files (f, folder) )
        . collect::<Result<_, _>> () ?,
        worktree_files (f) ? )) }

fn waiting_of (
  report : &CommitReport,
) -> Vec<(String, WaitReason, String)> {
  report . waiting . iter ()
    . map ( |(node, _, _)| ( node . pid . 0 . clone (), node . reason, node . waits_for . 0 . clone () ))
    . collect () }

/// Two changed nodes, a fully marked and b marked only in part: a
/// alone goes in, b's file stays modified, and b's mark is kept.
#[test]
fn of_two_changed_nodes_only_the_processed_one_goes_in () -> TestResult {
  let (a_new, b_new) = ( yaml ("a", "a2", &[]), yaml ("b", "b2", &["c"]) );
  with_scratch ( MAIN, &[
    ("main", "a", Some (&yaml ("a", "a", &[])), Some (&a_new)),
    ("main", "b", Some (&yaml ("b", "b", &[])), Some (&b_new)),
    ("main", "c", Some (&yaml ("c", "c", &[])), Some (&yaml ("c", "c", &[]))), ],
    |f, env| {
      mark_nodes (f, env, &["a"]) ?;
      let b_text : LeafKey = LeafKey::Node { pid : ID::from ("b"), aspect : NodeAspect::Text };
      { let straddle : GraphStraddle = graph_straddle (f) ?;
        let fingerprint : u64 = straddle . change_leaves () . leaf_fingerprint (&b_text) . unwrap ();
        env . runtime . processed_store . lock () . unwrap () . mark ( b_text . clone (), fingerprint ); }
      let worktree : BTreeMap<String, String> = worktree_files (f) ?;
      let answer : StageAndCommitAnswer = commit_processed (f, env);
      let report : &CommitReport = report (&answer);
      assert_eq! ( ( report . commits . len (), report . committed_nodes, report . left_nodes ),
                   ( 1, 1, 1 ) );
      assert! ( report . waiting . is_empty () );
      let head : BTreeMap<String, String> = head_files (f, "main") ?;
      assert_eq! ( head ["a.skg"], a_new );
      assert_eq! ( head ["b.skg"], yaml ("b", "b", &[]) );
      assert_eq! ( staged_paths (f, "main") ?, Vec::<String>::new () );
      assert_eq! ( statuses (f, "main") ? . keys () . collect::<Vec<_>> (), vec! ["b.skg"] );
      assert_eq! ( worktree_files (f) ?, worktree );
      assert_eq! ( marked_keys (env), HashSet::from ([ b_text ]) );
      Ok (( )) }) }

/// A new node's untracked file goes in, and its git index entry is the
/// commit's: Git reports nothing.
#[test]
fn a_new_processed_node_alone_goes_in () -> TestResult {
  let n : String = yaml ("n", "n", &[]);
  with_scratch ( MAIN, &[
    ("main", "a", Some (&yaml ("a", "a", &[])), Some (&yaml ("a", "a", &[]))),
    ("main", "n", None, Some (&n)), ],
    |f, env| {
      mark_nodes (f, env, &["n"]) ?;
      let answer : StageAndCommitAnswer = commit_processed (f, env);
      assert_eq! ( report (&answer) . committed_nodes, 1 );
      assert_eq! ( head_files (f, "main") ? . get ("n.skg"), Some (&n) );
      assert! ( statuses (f, "main") ? . is_empty (), "{:?}", statuses (f, "main") ? );
      assert! ( marked_keys (env) . is_empty () );
      Ok (( )) }) }

#[test]
fn when_nothing_is_processed_nothing_is_committed () -> TestResult {
  with_scratch ( MAIN, &[
    ("main", "a", Some (&yaml ("a", "a", &[])), Some (&yaml ("a", "a2", &[]))), ],
    |f, env| {
      let before = snapshot (f, &["main"]) ?;
      let answer : StageAndCommitAnswer = commit_processed (f, env);
      assert_eq! ( stage_and_commit_response_sexp (&answer),
                   "((commits ()) (committed-nodes 0) (left-nodes 1) (waiting ()) (errors ()) (warnings ()))" );
      assert_eq! ( snapshot (f, &["main"]) ?, before );
      Ok (( )) }) }

#[test]
fn when_nothing_has_changed_nothing_is_committed () -> TestResult {
  with_scratch ( MAIN, &[
    ("main", "a", Some (&yaml ("a", "a", &[])), Some (&yaml ("a", "a", &[]))), ],
    |f, env| {
      let before = snapshot (f, &["main"]) ?;
      let answer : StageAndCommitAnswer = commit_processed (f, env);
      assert_eq! ( stage_and_commit_response_sexp (&answer),
                   "((commits ()) (committed-nodes 0) (left-nodes 0) (waiting ()) (errors ()) (warnings ()))" );
      assert_eq! ( snapshot (f, &["main"]) ?, before );
      Ok (( )) }) }

/// m's home moves from public to private; u, unprocessed, changes too.
/// Unmarked, neither of m's files goes in; marked, both do, one commit
/// per skgrepo, and in a shared gitrepo the second's parent is the first.
fn a_home_move_goes_in_whole (
  skgrepos : &[(&str, &str)],
  public   : &str,
  private  : &str,
) -> TestResult {
  let m : String = yaml ("m", "m", &[]);
  with_scratch ( skgrepos, &[
    ("public",  "m", Some (&m), None),
    ("private", "m", None, Some (&m)),
    ("public",  "u", Some (&yaml ("u", "u", &[])), Some (&yaml ("u", "u2", &[]))), ],
    |f, env| {
      let gitrepos : Vec<&str> =
        if public == private { vec! [public] } else { vec! [public, private] };
      let before = snapshot (f, &gitrepos) ?;
      let answer : StageAndCommitAnswer = commit_processed (f, env);
      assert_eq! ( ( report (&answer) . committed_nodes, report (&answer) . left_nodes ), (0, 2) );
      assert_eq! ( snapshot (f, &gitrepos) ?, before, "neither of m's files goes in" );
      mark_nodes (f, env, &["m"]) ?;
      let answer : StageAndCommitAnswer = commit_processed (f, env);
      let report : &CommitReport = report (&answer);
      assert_eq! ( ( report . committed_nodes, report . left_nodes ), (1, 1) );
      assert_eq! ( report . commits . iter () . map ( |(skgrepo, _)| skgrepo . to_string () )
                   . collect::<Vec<_>> (), vec! ["public", "private"] );
      let in_public  : String = if public == private { "public/m.skg"  . into () } else { "m.skg" . into () };
      let in_private : String = if public == private { "private/m.skg" . into () } else { "m.skg" . into () };
      assert! ( ! head_files (f, public) ? . contains_key (&in_public) );
      assert_eq! ( head_files (f, private) ? . get (&in_private), Some (&m) );
      let u_path : String = if public == private { "public/u.skg" . into () } else { "u.skg" . into () };
      assert_eq! ( head_files (f, public) ? [&u_path], yaml ("u", "u", &[]) );
      if public == private {
        let gitrepo : Repository = gitrepo (f, public) ?;
        let head = gitrepo . head () ? . peel_to_commit () ?;
        assert_eq! ( head . id (), report . commits [1] . 1 );
        assert_eq! ( head . parent (0) ? . id (), report . commits [0] . 1 ); }
      for gitrepo in &gitrepos {
        assert_eq! ( staged_paths (f, gitrepo) ?, Vec::<String>::new () ); }
      assert_eq! ( worktree_files (f) ?, before . 1 );
      Ok (( )) }) }

#[test]
fn a_home_move_between_two_gitrepos_goes_in_whole () -> TestResult {
  a_home_move_goes_in_whole (TWO_GITREPOS, "public", "private") }

#[test]
fn a_home_move_within_one_gitrepo_goes_in_whole () -> TestResult {
  a_home_move_goes_in_whole (ONE_GITREPO, "g", "g") }

/// r, processed, now contains n, which is new and not processed.
#[test]
fn a_node_naming_a_new_node_that_does_not_go_in_waits () -> TestResult {
  with_scratch ( MAIN, &[
    ("main", "r", Some (&yaml ("r", "r", &[])), Some (&yaml ("r", "Plans", &["n"]))),
    ("main", "n", None, Some (&yaml ("n", "Seed order", &[]))), ],
    |f, env| {
      mark_nodes (f, env, &["r"]) ?;
      let marks : HashSet<LeafKey> = marked_keys (env);
      let before = snapshot (f, &["main"]) ?;
      let answer : StageAndCommitAnswer = commit_processed (f, env);
      assert_eq! ( stage_and_commit_response_sexp (&answer),
                   "((commits ()) (committed-nodes 0) (left-nodes 2) (waiting ((\"r\" \"Plans\" names-new \"n\" \"Seed order\"))) (errors ()) (warnings ()))" );
      assert_eq! ( snapshot (f, &["main"]) ?, before );
      assert_eq! ( marked_keys (env), marks, "a waiting node keeps its marks" );
      Ok (( )) }) }

/// d, processed, is deleted; r, which contained it at HEAD and does
/// not now, is not processed.
#[test]
fn a_deleted_node_still_named_at_head_waits () -> TestResult {
  with_scratch ( MAIN, &[
    ("main", "r", Some (&yaml ("r", "r", &["d"])), Some (&yaml ("r", "r", &[]))),
    ("main", "d", Some (&yaml ("d", "d", &[])), None), ],
    |f, env| {
      mark_nodes (f, env, &["d"]) ?;
      let before = snapshot (f, &["main"]) ?;
      let answer : StageAndCommitAnswer = commit_processed (f, env);
      assert_eq! ( waiting_of ( report (&answer) ),
                   vec! [ ("d" . into (), WaitReason::StillNamed, "r" . into ()) ] );
      assert_eq! ( report (&answer) . committed_nodes, 0 );
      assert_eq! ( snapshot (f, &["main"]) ?, before );
      Ok (( )) }) }

/// r contains the new n1, which contains the new n2. r and n1 are
/// processed, n2 is not: n1 waits for n2, and so r waits for n1. Once
/// n2 is processed, all three go in.
#[test]
fn a_chain_of_two_waits_and_then_all_go_in () -> TestResult {
  with_scratch ( MAIN, &[
    ("main", "r",  Some (&yaml ("r", "r", &[])), Some (&yaml ("r", "r", &["n1"]))),
    ("main", "n1", None, Some (&yaml ("n1", "n1", &["n2"]))),
    ("main", "n2", None, Some (&yaml ("n2", "n2", &[]))), ],
    |f, env| {
      mark_nodes (f, env, &["r", "n1"]) ?;
      let before = snapshot (f, &["main"]) ?;
      let answer : StageAndCommitAnswer = commit_processed (f, env);
      assert_eq! ( waiting_of ( report (&answer) ), vec! [
        ("n1" . into (), WaitReason::NamesNew, "n2" . into ()),
        ("r"  . into (), WaitReason::NamesNew, "n1" . into ()) ] );
      assert_eq! ( ( report (&answer) . committed_nodes, report (&answer) . left_nodes ), (0, 3) );
      assert_eq! ( snapshot (f, &["main"]) ?, before );
      mark_nodes (f, env, &["n2"]) ?;
      let answer : StageAndCommitAnswer = commit_processed (f, env);
      assert_eq! ( ( report (&answer) . committed_nodes, report (&answer) . left_nodes ), (3, 0) );
      assert_eq! ( head_files (f, "main") ?, before . 1 . iter ()
                   . map ( |(path, text)| ( path . trim_start_matches ("main/") . to_string (), text . clone () ))
                   . collect () );
      assert! ( statuses (f, "main") ? . is_empty () );
      assert! ( marked_keys (env) . is_empty () );
      Ok (( )) }) }

/// acq acquired acqd: acq gained acqd as an extra ID, acqd is deleted,
/// and c, whose file is unchanged, still lists acqd, which now
/// resolves to acq.
fn merge_files () -> Vec<(String, Option<String>, Option<String>)> {
  vec! [
    ( "acq" . into (), Some ( yaml ("acq", "acq", &[]) ),
      Some ( "pid: acq\ntitle: acq\nextra_ids:\n- acqd\n" . into () )),
    ( "acqd" . into (), Some ( yaml ("acqd", "acqd", &[]) ), None ),
    ( "c" . into (), Some ( yaml ("c", "c", &["acqd"]) ), Some ( yaml ("c", "c", &["acqd"]) )), ] }

fn with_merge (
  body : impl FnOnce (&mut Fixture, &SkgEnv) -> TestResult,
) -> TestResult {
  let files = merge_files ();
  let case : Vec<CaseFile> = files . iter ()
    . map ( |(pid, head, worktree)| ( "main", pid . as_str (), head . as_deref (), worktree . as_deref () ))
    . collect ();
  with_scratch (MAIN, &case, body) }

#[test]
fn a_merge_with_one_part_unprocessed_waits_whole () -> TestResult {
  with_merge ( |f, env| {
    mark_nodes (f, env, &["acq", "c"]) ?;
    let before = snapshot (f, &["main"]) ?;
    let answer : StageAndCommitAnswer = commit_processed (f, env);
    assert_eq! ( waiting_of ( report (&answer) ), vec! [
      ("acq" . into (), WaitReason::Merge, "acqd" . into ()),
      ("c"   . into (), WaitReason::Merge, "acqd" . into ()) ] );
    assert_eq! ( ( report (&answer) . committed_nodes, report (&answer) . left_nodes ), (0, 3) );
    assert_eq! ( snapshot (f, &["main"]) ?, before );
    Ok (( )) }) }

/// The only unprocessed part is c, whose file is unchanged: none of
/// the merge goes in, and 'left-nodes' counts c.
#[test]
fn a_merge_whose_unprocessed_part_is_an_unchanged_container_waits_whole () -> TestResult {
  with_merge ( |f, env| {
    mark_nodes (f, env, &["acq", "acqd"]) ?;
    let before = snapshot (f, &["main"]) ?;
    let answer : StageAndCommitAnswer = commit_processed (f, env);
    assert_eq! ( waiting_of ( report (&answer) ), vec! [
      ("acq"  . into (), WaitReason::Merge, "c" . into ()),
      ("acqd" . into (), WaitReason::Merge, "c" . into ()) ] );
    assert_eq! ( ( report (&answer) . committed_nodes, report (&answer) . left_nodes ), (0, 3) );
    assert_eq! ( snapshot (f, &["main"]) ?, before );
    Ok (( )) }) }

#[test]
fn a_merge_wholly_processed_goes_in () -> TestResult {
  with_merge ( |f, env| {
    mark_nodes (f, env, &["acq", "acqd", "c"]) ?;
    let answer : StageAndCommitAnswer = commit_processed (f, env);
    assert_eq! ( ( report (&answer) . committed_nodes, report (&answer) . left_nodes ), (3, 0) );
    let head : BTreeMap<String, String> = head_files (f, "main") ?;
    assert! ( ! head . contains_key ("acqd.skg") );
    assert! ( head ["acq.skg"] . contains ("acqd") );
    assert! ( statuses (f, "main") ? . is_empty () );
    assert! ( marked_keys (env) . is_empty () );
    Ok (( )) }) }

/// At HEAD r overrides n. In the worktree r no longer does, and n
/// contains r. Only n is processed: committing it alone would leave n
/// containing a note that forks it, so validation refuses, and nothing
/// goes in.
#[test]
fn a_commit_leaving_a_note_containing_its_forker_is_refused () -> TestResult {
  with_scratch ( MAIN, &[
    ("main", "n", Some (&yaml ("n", "n", &[])), Some (&yaml ("n", "n", &["r"]))),
    ("main", "r", Some ("pid: r\ntitle: r\noverrides:\n- n\n"), Some (&yaml ("r", "r", &[]))), ],
    |f, env| {
      mark_nodes (f, env, &["n"]) ?;
      let marks : HashSet<LeafKey> = marked_keys (env);
      let before = snapshot (f, &["main"]) ?;
      let answer : StageAndCommitAnswer = commit_processed (f, env);
      assert! ( matches! ( &answer, StageAndCommitAnswer::Refused (e)
                           if e . starts_with ("Nothing was committed: the commit would not validate:")
                              && e . contains ("node n contains node r, which forks it") ),
                "{:?}", answer );
      assert_eq! ( stage_and_commit_response_sexp (&answer) . matches ("(errors (") . count (), 1 );
      assert_eq! ( snapshot (f, &["main"]) ?, before );
      assert_eq! ( staged_paths (f, "main") ?, Vec::<String>::new () );
      assert_eq! ( marked_keys (env), marks );
      Ok (( )) }) }

/// A file staged by hand before a commit of the processed is neither
/// committed nor unstaged.
#[test]
fn an_entry_already_staged_is_neither_committed_nor_unstaged () -> TestResult {
  let a_new : String = yaml ("a", "a2", &[]);
  with_scratch ( MAIN, &[
    ("main", "a", Some (&yaml ("a", "a", &[])), Some (&a_new)), ],
    |f, env| {
      fs::write ( f . root . path () . join ("main/notes.txt"), "staged by hand\n" ) ?;
      { let gitrepo : Repository = gitrepo (f, "main") ?;
        let mut index : Index = gitrepo . index () ?;
        index . add_path ( Path::new ("notes.txt") ) ?;
        index . write () ?; }
      mark_nodes (f, env, &["a"]) ?;
      let answer : StageAndCommitAnswer = commit_processed (f, env);
      assert_eq! ( report (&answer) . committed_nodes, 1 );
      let head : BTreeMap<String, String> = head_files (f, "main") ?;
      assert_eq! ( head ["a.skg"], a_new );
      assert! ( ! head . contains_key ("notes.txt") );
      assert_eq! ( staged_paths (f, "main") ?, vec! [ "notes.txt" . to_string () ] );
      Ok (( )) }) }

/// The check of decision 4, forced to expect a's changes left when a
/// goes in: nothing is committed.
#[test]
fn a_mismatch_in_the_check_commits_nothing () -> TestResult {
  with_scratch ( MAIN, &[
    ("main", "a", Some (&yaml ("a", "a", &[])), Some (&yaml ("a", "a2", &[]))), ],
    |f, env| {
      let before = snapshot (f, &["main"]) ?;
      let straddle : GraphStraddle = graph_straddle (f) ?;
      let a : BTreeSet<ID> = BTreeSet::from ([ ID::from ("a") ]);
      let answer : StageAndCommitAnswer = commit_nodes (
        &f . config, &straddle, &a, &a, &[], "the message" );
      assert! ( matches! ( &answer, StageAndCommitAnswer::Refused (e)
                           if e . starts_with ("Nothing was committed: the commit would not leave exactly the changes of the nodes left out.") ),
                "{:?}", answer );
      assert_eq! ( snapshot (f, &["main"]) ?, before );
      assert_eq! ( staged_paths (f, "main") ?, Vec::<String>::new () );
      let _ = env;
      Ok (( )) }) }

/// x's alias moves from its public section to a private one, and
/// nothing else changes. That is no leaf, so x counts as processed, and
/// both of its files go in.
#[test]
fn an_alias_moved_between_sections_goes_in_with_both_files () -> TestResult {
  let public_x : String = yaml ("x", "x", &[]);
  let private_x : &str = "pid: x\naliases:\n- al\n";
  with_scratch ( TWO_GITREPOS, &[
    ("public",  "x", Some ("pid: x\ntitle: x\naliases:\n- al\n"), Some (&public_x)),
    ("private", "x", None, Some (private_x)),
    ("public",  "k", Some (&yaml ("k", "k", &[])), Some (&yaml ("k", "k", &[]))), ],
    |f, env| {
      let answer : StageAndCommitAnswer = commit_processed (f, env);
      assert_eq! ( report (&answer) . committed_nodes, 1, "{:?}", answer );
      assert_eq! ( head_files (f, "public") ? ["x.skg"], public_x );
      assert_eq! ( head_files (f, "private") ? . get ("x.skg") . map ( String::as_str ), Some (private_x) );
      assert! ( statuses (f, "public") ? . is_empty () && statuses (f, "private") ? . is_empty () );
      Ok (( )) }) }

/// Without '(processed-only . t)': while a changed node is not
/// processed the request asks; confirmed, it commits every changed
/// node, and a file staged by hand is neither committed nor unstaged.
#[test]
fn the_request_without_the_field_asks_then_commits_every_changed_node () -> TestResult {
  with_scratch ( MAIN, &[
    ("main", "a", Some (&yaml ("a", "a", &[])), Some (&yaml ("a", "a2", &[]))),
    ("main", "b", Some (&yaml ("b", "b", &[])), Some (&yaml ("b", "b2", &[]))),
    ("main", "n", None, Some (&yaml ("n", "n", &[]))), ],
    |f, env| {
      fs::write ( f . root . path () . join ("main/notes.txt"), "staged by hand\n" ) ?;
      { let gitrepo : Repository = gitrepo (f, "main") ?;
        let mut index : Index = gitrepo . index () ?;
        index . add_path ( Path::new ("notes.txt") ) ?;
        index . write () ?; }
      mark_nodes (f, env, &["a"]) ?;
      let before = snapshot (f, &["main"]) ?;
      let answer : StageAndCommitAnswer = commit (f, env, CommitScope::Everything { confirmed : false });
      assert_eq! ( stage_and_commit_response_sexp (&answer),
                   "((needs-confirmation . t) (processed-summary (processed 1) (changed 3) (foreign 0) (order-only 0) (waiting 0) (notEditable 0)) (errors ()) (warnings ()))" );
      assert_eq! ( snapshot (f, &["main"]) ?, before );
      let answer : StageAndCommitAnswer = commit (f, env, CommitScope::Everything { confirmed : true });
      let report : &CommitReport = report (&answer);
      assert_eq! ( ( report . committed_nodes, report . left_nodes, report . waiting . len () ), (3, 0, 0) );
      let head : BTreeMap<String, String> = head_files (f, "main") ?;
      assert_eq! ( head . keys () . collect::<Vec<_>> (), vec! ["a.skg", "b.skg", "n.skg"] );
      assert_eq! ( staged_paths (f, "main") ?, vec! [ "notes.txt" . to_string () ] );
      assert_eq! ( statuses (f, "main") ? . keys () . collect::<Vec<_>> (), vec! ["notes.txt"] );
      assert! ( marked_keys (env) . is_empty () );
      Ok (( )) }) }

/// A save of the diff view counts nodes: t, retitled in its public
/// section and gaining a member in its private one, is one changed
/// node; r, processed, names the new n, which is not, so r waits.
#[test]
fn a_save_of_the_diff_view_counts_nodes () -> TestResult {
  with_scratch ( TWO_GITREPOS, &[
    ("public",  "t", Some (&yaml ("t", "t", &[])), Some (&yaml ("t", "t2", &[]))),
    ("private", "t", None, Some ("pid: t\ncontains:\n- k\n")),
    ("public",  "k", Some (&yaml ("k", "k", &[])), Some (&yaml ("k", "k", &[]))),
    ("public",  "r", Some (&yaml ("r", "r", &[])), Some (&yaml ("r", "r", &["n"]))),
    ("public",  "n", None, Some (&yaml ("n", "n", &[]))), ],
    |f, env| {
      mark_nodes (f, env, &["r"]) ?;
      env . runtime . processed_store . lock () . unwrap ()
        . save ( &f . config . processed_marks_path () ) ?;
      let buffer : String = open_all_changes_buffer (f) ?;
      let summary : ProcessedSummary =
        save_cleanly (f, &all_changes_view (), &buffer) ? . processed_summary
        . expect ("a save of the diff view reports the summary") . summary;
      assert_eq! ( summary, ProcessedSummary {
        processed_nodes : 1, changed_nodes : 3, foreign_changed_nodes : 0,
        order_only_nodes : 0, waiting_nodes : 1 } );
      Ok (( )) }) }

/// With 'core.autocrlf' set, k's worktree file differs from HEAD's
/// blob only in line endings, which Git's filters undo: k has not
/// changed, so only a goes in.
#[test]
fn line_endings_git_normalizes_are_no_change () -> TestResult {
  let a_new : String = yaml ("a", "a2", &[]);
  with_scratch ( MAIN, &[
    ("main", "a", Some (&yaml ("a", "a", &[])), Some (&a_new)),
    ("main", "k", Some (&yaml ("k", "k", &[])), Some ("pid: k\r\ntitle: k\r\n")), ],
    |f, env| {
      gitrepo (f, "main") ? . config () ? . set_bool ("core.autocrlf", true) ?;
      mark_nodes (f, env, &["a"]) ?;
      let answer : StageAndCommitAnswer = commit_processed (f, env);
      assert_eq! ( ( report (&answer) . committed_nodes, report (&answer) . left_nodes ), (1, 0) );
      let head : BTreeMap<String, String> = head_files (f, "main") ?;
      assert_eq! ( head ["a.skg"], a_new );
      assert_eq! ( head ["k.skg"], yaml ("k", "k", &[]) );
      Ok (( )) }) }

/// The live graph reads a '.skg' file the gitrepo ignores, so its node
/// is a changed node like any other, and goes in when processed.
#[test]
fn a_new_node_whose_file_git_ignores_goes_in () -> TestResult {
  let n : String = yaml ("n", "n", &[]);
  with_scratch ( MAIN, &[
    ("main", "a", Some (&yaml ("a", "a", &[])), Some (&yaml ("a", "a", &[]))),
    ("main", "n", None, Some (&n)), ],
    |f, env| {
      fs::write ( f . root . path () . join ("main/.gitignore"), "n.skg\n" ) ?;
      mark_nodes (f, env, &["n"]) ?;
      let answer : StageAndCommitAnswer = commit_processed (f, env);
      assert_eq! ( report (&answer) . committed_nodes, 1 );
      assert_eq! ( head_files (f, "main") ? . get ("n.skg"), Some (&n) );
      Ok (( )) }) }

/// The live graph reads only a skgrepo's own folder, so a '.skg' file
/// in a subfolder (a backup, say) is no node and never goes in.
#[test]
fn a_skg_file_in_a_subfolder_is_left_alone () -> TestResult {
  let a_new : String = yaml ("a", "a2", &[]);
  with_scratch ( MAIN, &[
    ("main", "a", Some (&yaml ("a", "a", &[])), Some (&a_new)), ],
    |f, env| {
      fs::create_dir_all ( f . root . path () . join ("main/old") ) ?;
      fs::write ( f . root . path () . join ("main/old/a.skg"), yaml ("a", "a backup", &[]) ) ?;
      fs::write ( f . root . path () . join ("main/old/z.skg"), yaml ("z", "z", &[]) ) ?;
      mark_nodes (f, env, &["a"]) ?;
      let answer : StageAndCommitAnswer = commit_processed (f, env);
      assert_eq! ( report (&answer) . committed_nodes, 1 );
      assert_eq! ( head_files (f, "main") ? . keys () . collect::<Vec<_>> (), vec! ["a.skg"] );
      Ok (( )) }) }

/// When a gitrepo's HEAD moves after the commits were prepared, no
/// commit is made in any gitrepo, not even in one prepared before it.
#[test]
fn a_head_moved_after_preparing_commits_nothing_anywhere () -> TestResult {
  with_scratch ( TWO_GITREPOS, &[
    ("public",  "a", Some (&yaml ("a", "a", &[])), Some (&yaml ("a", "a2", &[]))),
    ("private", "b", Some (&yaml ("b", "b", &[])), Some (&yaml ("b", "b2", &[]))), ],
    |f, _env| {
      let paths : Vec<ChangedSkgPath> = changed_skg_paths_by_stem (&f . config) ?
        . into_values () . flatten () . collect ();
      let prepared : PreparedCommits = prepare_commits (&f . config, &paths) ?;
      assert_eq! ( prepared . commits . len (), 2 );
      { let private : Repository = gitrepo (f, "private") ?;
        let head = private . head () ? . peel_to_commit () ?;
        let signature : Signature = private . signature () ?;
        let mut builder = private . treebuilder ( Some ( &head . tree () ? )) ?;
        builder . insert ( "elsewhere.txt", private . blob (b"elsewhere\n") ?, 0o100644 ) ?;
        let tree : Tree = private . find_tree ( builder . write () ? ) ?;
        private . commit ( Some ("HEAD"), &signature, &signature, "elsewhere",
                           &tree, &[ &head ] ) ?; }
      let public_head = gitrepo (f, "public") ? . head () ? . peel_to_commit () ? . id ();
      let (commits, errors) = commit_prepared (&prepared, "the message");
      assert! ( commits . is_empty (), "{:?}", commits );
      assert! ( errors . len () == 1 && errors [0] . starts_with ("Nothing was committed: skgrepo private"),
                "{:?}", errors );
      assert_eq! ( gitrepo (f, "public") ? . head () ? . peel_to_commit () ? . id (), public_head );
      Ok (( )) }) }

/// A merge waits unprocessed, and f, in the foreign skgrepo theirs,
/// lists the acquiree's ID, so f records leaves no commit can take.
/// They hold nothing back: a, processed, goes in, and the check
/// compares only what owned skgrepos record.
#[test]
fn a_foreign_lister_of_a_pending_merge_holds_nothing_back () -> TestResult {
  let a_new : String = yaml ("a", "a2", &[]);
  let f : String = yaml ("f", "f", &["acqd"]);
  with_scratch ( &[ ("main", "main"), ("theirs", "theirs") ], &[
    ("main", "a", Some (&yaml ("a", "a", &[])), Some (&a_new)),
    ("main", "acq", Some (&yaml ("acq", "acq", &[])), Some ("pid: acq\ntitle: acq\nextra_ids:\n- acqd\n")),
    ("main", "acqd", Some (&yaml ("acqd", "acqd", &[])), None),
    ("theirs", "f", Some (&f), Some (&f)), ],
    |f, env| {
      mark_nodes (f, env, &["a"]) ?;
      let answer : StageAndCommitAnswer = commit_processed (f, env);
      assert_eq! ( ( report (&answer) . committed_nodes, report (&answer) . left_nodes ), (1, 2) );
      assert_eq! ( head_files (f, "main") ? ["a.skg"], a_new );
      Ok (( )) }) }

/// g is foreign, homed in theirs. A new file in main holding a section
/// of g is an illegal edit, which the graph sets apart: it is no
/// change of any owned node, and never goes in, though a, processed,
/// does.
#[test]
fn a_section_of_a_foreign_node_in_an_owned_skgrepo_never_goes_in () -> TestResult {
  let a_new : String = yaml ("a", "a2", &[]);
  let g : String = yaml ("g", "g", &[]);
  with_scratch ( &[ ("main", "main"), ("theirs", "theirs") ], &[
    ("main", "a", Some (&yaml ("a", "a", &[])), Some (&a_new)),
    ("main", "g", None, Some ("pid: g\ncontains:\n- a\n")),
    ("theirs", "g", Some (&g), Some (&g)), ],
    |f, env| {
      mark_nodes (f, env, &["a"]) ?;
      let answer : StageAndCommitAnswer = commit_processed (f, env);
      assert_eq! ( report (&answer) . committed_nodes, 1, "{:?}", answer );
      assert_eq! ( head_files (f, "main") ? . keys () . collect::<Vec<_>> (), vec! ["a.skg"] );
      Ok (( )) }) }
