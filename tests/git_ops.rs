// cargo nextest run --test git_ops

use git2::Repository;
use std::fs;
use std::path::Path;
use tempfile::TempDir;

use skg::git_ops::read_repo::{ head_is_merge_commit, get_file_content_at_head };
use skg::types::list::{diff_lists, compute_interleaved_diff, ListDiff, ListDiffEntry};

fn setup_git_repo() -> (TempDir, Repository) {
  let dir : TempDir =
    TempDir::new().unwrap();
  let repo : Repository =
    Repository::init ( dir . path() ).unwrap();
  { // Configure user for commits
    let mut config : git2::Config =
      repo . config().unwrap();
    config . set_str ( "user.email", "test@test.com" ).unwrap();
    config . set_str ( "user.name", "Test" ).unwrap(); }
  (dir, repo) }

fn create_initial_commit (
  repo : &Repository,
  dir  : &Path
) {
  fs::write ( dir . join ( "test.skg" ), "title: Test" ).unwrap(); // Create a file
  { // Stage and commit
    let mut index : git2::Index =
      repo . index().unwrap();
    index . add_path ( Path::new ( "test.skg" )) . unwrap();
    index . write().unwrap();
    let tree_id : git2::Oid =
      index . write_tree().unwrap();
    let tree : git2::Tree =
      repo . find_tree ( tree_id ).unwrap();
    let sig : git2::Signature =
      repo . signature().unwrap();
    repo . commit (
      Some ( "HEAD" ),
      &sig, &sig,
      "Initial commit",
      &tree,
      &[] ).unwrap(); }}

#[test]
fn test_head_is_merge_commit() {
  let (dir, repo) : (TempDir, Repository) =
    setup_git_repo();
  create_initial_commit ( &repo, dir . path() );
  assert! ( ! head_is_merge_commit ( &repo ).unwrap() ); } // Single-parent commit is not a merge commit

#[test]
fn test_get_file_content_at_head() {
  let (dir, repo) : (TempDir, Repository) =
    setup_git_repo();
  create_initial_commit ( &repo, dir . path() );
  let content : Option<String> =
    get_file_content_at_head (
      &repo,
      Path::new ( "test.skg" )).unwrap();
  assert_eq! ( content, Some ( "title: Test" . to_string() ));
  let no_content : Option<String> = // Non-existent file should return None
    get_file_content_at_head (
      &repo,
      Path::new ( "nonexistent.skg" )).unwrap();
  assert_eq! ( no_content, None ); }

#[test]
fn test_diff_lists_no_changes() {
  let old : Vec<&str> =
    vec!["a", "b", "c"];
  let new : Vec<&str> =
    vec!["a", "b", "c"];
  let diff : ListDiff<&str> =
    diff_lists ( &old, &new );
  assert! ( diff . added . is_empty() );
  assert! ( diff . removed . is_empty() ); }

#[test]
fn test_diff_lists_additions() {
  let old : Vec<&str> =
    vec!["a", "c"];
  let new : Vec<&str> =
    vec!["a", "b", "c"];
  let diff : ListDiff<&str> =
    diff_lists ( &old, &new );
  assert_eq! ( diff . added, vec!["b"] );
  assert! ( diff . removed . is_empty() ); }

#[test]
fn test_diff_lists_removals() {
  let old : Vec<&str> =
    vec!["a", "b", "c"];
  let new : Vec<&str> =
    vec!["a", "c"];
  let diff : ListDiff<&str> =
    diff_lists ( &old, &new );
  assert! ( diff . added . is_empty() );
  assert_eq! ( diff . removed, vec!["b"] ); }

#[test]
fn test_interleaved_diff() {
  let old : Vec<&str> =
    vec!["a", "b", "c"];
  let new : Vec<&str> =
    vec!["a", "d", "c"];
  let interleaved : Vec<ListDiffEntry<&str>> =
    compute_interleaved_diff ( &old, &new );
  let mut found_removed_b : bool = // Should have: a (unchanged), b (removed), d (new), c (unchanged)
    false;
  let mut found_new_d : bool =
    false;
  for entry in &interleaved {
    match entry {
      ListDiffEntry::RemovedHere(s) if *s == "b" => found_removed_b = true,
      ListDiffEntry::NewHere(s) if *s == "d" => found_new_d = true,
      _ => {} }}
  assert! ( found_removed_b );
  assert! ( found_new_d ); }
