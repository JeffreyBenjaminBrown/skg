// cargo nextest run --test grouped_unit -E 'test(git_ops::)'

use git2::Repository;
use std::fs;
use std::path::Path;
use tempfile::TempDir;

use skg::git_ops::read_repo::{ head_is_merge_commit, get_file_content_at_head };
use skg::types::list::{compute_interleaved_diff, Diff_Item};

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
