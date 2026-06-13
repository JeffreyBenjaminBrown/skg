// cargo nextest run --test grouped_unit -E 'test(git_ops::)'

use git2::Repository;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

use skg::git_ops::find_and_stage_moves::stage_moves_script;
use skg::git_ops::read_repo::{ head_is_merge_commit, get_file_content_at_head };
use skg::types::list::{compute_interleaved_diff, Diff_Item};
use skg::types::misc::{ SkgConfig, SkgfileSource, SourceName };

fn setup_git_repo() -> (TempDir, Repository) {
  let dir : TempDir =
    TempDir::new() . unwrap();
  let repo : Repository =
    Repository::init ( dir . path() ) . unwrap();
  { // Configure user for commits
    let mut config : git2::Config =
      repo . config() . unwrap();
    config . set_str ( "user.email", "test@test.com" ) . unwrap();
    config . set_str ( "user.name", "Test" ) . unwrap(); }
  (dir, repo) }

fn create_initial_commit (
  repo : &Repository,
  dir  : &Path
) {
  fs::write ( dir . join ("test.skg"), "title: Test" ) . unwrap(); // Create a file
  { // Stage and commit
    let mut index : git2::Index =
      repo . index() . unwrap();
    index . add_path ( Path::new ("test.skg")) . unwrap();
    index . write() . unwrap();
    let tree_id : git2::Oid =
      index . write_tree() . unwrap();
    let tree : git2::Tree =
      repo . find_tree (tree_id) . unwrap();
    let sig : git2::Signature =
      repo . signature() . unwrap();
    repo . commit (
      Some ("HEAD"),
      &sig, &sig,
      "Initial commit",
      &tree,
      &[] ) . unwrap(); }}

#[test]
fn test_head_is_merge_commit() {
  let (dir, repo) : (TempDir, Repository) =
    setup_git_repo();
  create_initial_commit ( &repo, dir . path() );
  assert! ( ! head_is_merge_commit (&repo) . unwrap() ); } // Single-parent commit is not a merge commit

#[test]
fn test_get_file_content_at_head() {
  let (dir, repo) : (TempDir, Repository) =
    setup_git_repo();
  create_initial_commit ( &repo, dir . path() );
  let content : Option<String> =
    get_file_content_at_head (
      &repo,
      Path::new ("test.skg")) . unwrap();
  assert_eq! ( content, Some ( "title: Test" . to_string() ));
  let no_content : Option<String> = // Non-existent file should return None
    get_file_content_at_head (
      &repo,
      Path::new ("nonexistent.skg")) . unwrap();
  assert_eq! ( no_content, None ); }

//
// stage_moves
//

fn init_repo_with_user (
  dir : &Path
) -> Repository {
  let repo : Repository =
    Repository::init (dir) . unwrap();
  { let mut config : git2::Config =
      repo . config() . unwrap();
    config . set_str ( "user.email", "test@test.com" ) . unwrap();
    config . set_str ( "user.name", "Test" ) . unwrap(); }
  repo }

/// Write 'filename' into the repo and commit it. Works for the first
/// commit and for later ones (parents = current HEAD if any).
fn commit_file (
  repo     : &Repository,
  repo_dir : &Path,
  filename : &str,
  contents : &str,
) {
  fs::write ( repo_dir . join (filename), contents ) . unwrap();
  let mut index : git2::Index =
    repo . index() . unwrap();
  index . add_path ( Path::new (filename) ) . unwrap();
  index . write() . unwrap();
  let tree : git2::Tree =
    repo . find_tree ( index . write_tree() . unwrap() ) . unwrap();
  let sig : git2::Signature =
    repo . signature() . unwrap();
  let parent : Option<git2::Commit> =
    repo . head() . ok()
    . and_then ( |h| h . peel_to_commit() . ok() );
  let parents : Vec<&git2::Commit> =
    parent . iter() . collect();
  repo . commit (
    Some ("HEAD"),
    &sig, &sig,
    "commit",
    &tree,
    &parents ) . unwrap(); }

fn mk_source (
  name : &str,
  path : PathBuf,
) -> SkgfileSource {
  SkgfileSource {
    name         : SourceName::from (name),
    abbreviation : None,
    path,
    user_owns_it : true } }

/// Build a config whose data_root is 'root' and whose sources are the
/// named subdirectories of it, mirroring how 'make_paths_absolute'
/// leaves source paths at load time (absolute, data_root-rooted).
fn config_from_subdirs (
  root : &Path,
  names : &[&str],
) -> SkgConfig {
  let mut sources : HashMap<SourceName, SkgfileSource> =
    HashMap::new();
  for name in names {
    sources . insert (
      SourceName::from (*name),
      mk_source ( name, root . join (name) ) ); }
  let mut config : SkgConfig =
    SkgConfig::dummyFromSources (sources);
  config . data_root = root . to_path_buf();
  config }

#[test]
fn test_stage_moves_detects_single_pair_move() {
  let root : TempDir =
    TempDir::new() . unwrap();
  let alpha : PathBuf = root . path() . join ("alpha");
  let beta  : PathBuf = root . path() . join ("beta");
  fs::create_dir (&alpha) . unwrap();
  fs::create_dir (&beta)  . unwrap();
  let alpha_repo : Repository =
    init_repo_with_user (&alpha);
  let _beta_repo : Repository =
    init_repo_with_user (&beta);
  { // node-1 is committed in alpha, then removed from alpha's
    // worktree and created (untracked) in beta -- a clean alpha->beta
    // move.
    commit_file (&alpha_repo, &alpha, "node-1.skg", "title: One");
    fs::remove_file ( alpha . join ("node-1.skg") ) . unwrap();
    fs::write ( beta . join ("node-1.skg"), "title: One" ) . unwrap(); }
  { // A brand-new node that only ever appeared in beta is NOT a move
    // (it never vanished from anywhere).
    fs::write ( beta . join ("fresh.skg"), "title: Fresh" ) . unwrap(); }
  let config : SkgConfig =
    config_from_subdirs ( root . path(), &["alpha", "beta"] );
  let script : String =
    stage_moves_script (&config) . unwrap();
  assert! ( script . contains ("id=node-1"),       "script:\n{}", script );
  assert! ( script . contains ("cd alpha"),        "script:\n{}", script );
  assert! ( script . contains ("git rm $id.skg"),  "script:\n{}", script );
  assert! ( script . contains ("cd ../beta"),      "script:\n{}", script );
  assert! ( script . contains ("git add $id.skg"), "script:\n{}", script );
  assert! ( script . contains ("cd .."),           "script:\n{}", script );
  assert! ( ! script . contains ("id=fresh"),      "script:\n{}", script ); }

#[test]
fn test_stage_moves_skips_ambiguous_and_reports_none() {
  let root : TempDir =
    TempDir::new() . unwrap();
  let alpha : PathBuf = root . path() . join ("alpha");
  let beta  : PathBuf = root . path() . join ("beta");
  let gamma : PathBuf = root . path() . join ("gamma");
  for dir in [&alpha, &beta, &gamma] {
    fs::create_dir (dir) . unwrap(); }
  let alpha_repo : Repository =
    init_repo_with_user (&alpha);
  let _beta_repo : Repository =
    init_repo_with_user (&beta);
  let _gamma_repo : Repository =
    init_repo_with_user (&gamma);
  { // 'dup' vanished from alpha but appeared in BOTH beta and gamma,
    // so there are two candidate (old,new) pairs: not an unambiguous
    // move, hence skipped.
    commit_file (&alpha_repo, &alpha, "dup.skg", "title: Dup");
    fs::remove_file ( alpha . join ("dup.skg") ) . unwrap();
    fs::write ( beta  . join ("dup.skg"), "title: Dup" ) . unwrap();
    fs::write ( gamma . join ("dup.skg"), "title: Dup" ) . unwrap(); }
  let config : SkgConfig =
    config_from_subdirs ( root . path(), &["alpha", "beta", "gamma"] );
  let script : String =
    stage_moves_script (&config) . unwrap();
  assert! ( ! script . contains ("id=dup"),
            "ambiguous move should be skipped:\n{}", script );
  assert! ( script . contains ("No moves detected"),
            "script:\n{}", script ); }

#[test]
fn test_interleaved_diff_no_changes() {
  let old : Vec<&str> =
    vec!["a", "b", "c"];
  let new : Vec<&str> =
    vec!["a", "b", "c"];
  assert_eq! (
    compute_interleaved_diff ( &old, &new ),
    vec![ Diff_Item::Unchanged  ("a"),
          Diff_Item::Unchanged  ("b"),
          Diff_Item::Unchanged  ("c") ]); }

#[test]
fn test_interleaved_diff_additions() {
  let old : Vec<&str> =
    vec!["a", "c"];
  let new : Vec<&str> =
    vec!["a", "b", "c"];
  assert_eq! (
    compute_interleaved_diff ( &old, &new ),
    vec![ Diff_Item::Unchanged ("a"),
          Diff_Item::New       ("b"),
          Diff_Item::Unchanged ("c") ]); }

#[test]
fn test_interleaved_diff_removals() {
  let old : Vec<&str> =
    vec!["a", "b", "c"];
  let new : Vec<&str> =
    vec!["a", "c"];
  assert_eq! (
    compute_interleaved_diff ( &old, &new ),
    vec![ Diff_Item::Unchanged ("a"),
          Diff_Item::Removed   ("b"),
          Diff_Item::Unchanged ("c") ]); }

#[test]
fn test_interleaved_diff() {
  let old : Vec<&str> =
    vec!["a", "b", "c"];
  let new : Vec<&str> =
    vec!["a", "d", "c"];
  assert_eq! (
    compute_interleaved_diff ( &old, &new ),
    vec![ Diff_Item::Unchanged ("a"),
          Diff_Item::Removed   ("b"),
          Diff_Item::New       ("d"),
          Diff_Item::Unchanged ("c") ]); }

#[test]
fn test_interleaved_diff_many_distinct_items_do_not_collide() {
  // Pins the fix for the silent-collision bug: a per-item scalar
  // encoding would map distinct items past ~55k onto the same '?'
  // (the Unicode surrogate range), so a change among such items would
  // be invisibly reported as Unchanged. We build long lists that swap
  // two distinct items deep in that range and assert the swap is seen.
  let n : usize = 60_000;
  let old : Vec<String> =
    (0 .. n) . map ( |i| i . to_string() ) . collect();
  let new : Vec<String> = { // swap two neighbors at index 56_000
    let mut v : Vec<String> = old . clone();
    v . swap ( 56_000, 56_001 );
    v };
  let diff : Vec<Diff_Item<String>> =
    compute_interleaved_diff ( &old, &new );
  { // The two swapped items must surface as Removed and New, not be
    // silently swallowed as Unchanged.
    let removed : Vec<&String> =
      diff . iter() . filter_map ( |d| match d {
        Diff_Item::Removed (x) => Some (x),
        _ => None } ) . collect();
    let added : Vec<&String> =
      diff . iter() . filter_map ( |d| match d {
        Diff_Item::New (x) => Some (x),
        _ => None } ) . collect();
    assert! ( removed . contains ( &&"56000" . to_string() )
              || removed . contains ( &&"56001" . to_string() ),
              "a swapped item should be Removed, got {:?}", removed );
    assert! ( added . contains ( &&"56000" . to_string() )
              || added . contains ( &&"56001" . to_string() ),
              "a swapped item should be New, got {:?}", added ); }
  { // Round-trip: the New+Unchanged stream must reconstruct `new`
    // exactly, item for item -- no item dropped or mislabeled.
    let rebuilt : Vec<String> =
      diff . iter() . filter_map ( |d| match d {
        Diff_Item::Unchanged (x) | Diff_Item::New (x) => Some (x . clone()),
        Diff_Item::Removed (_) => None } ) . collect();
    assert_eq! ( rebuilt, new ); }}
