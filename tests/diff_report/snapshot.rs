use skg::diff_report::git_snapshot::read_git_snapshot_pair;
use skg::diff_report::types::{DiffSelection, GitSnapshotPair};
use skg::types::misc::{ID, SkgConfig, Skgrepo, SkgrepoName};

use git2::Repository;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

fn write_node (
  skgrepo_dir : &Path,
  pid         : &str,
  title       : &str,
) {
  fs::write (
    skgrepo_dir . join (format! ("{}.skg", pid)),
    format! ("title: {}\npid: {}\n", title, pid) )
    . unwrap ();
}

fn commit_all (
  gitrepo    : &Repository,
  message : &str,
) {
  let mut index : git2::Index =
    gitrepo . index () . unwrap ();
  index . add_all (["*"].iter (), git2::IndexAddOption::DEFAULT, None)
    . unwrap ();
  index . write () . unwrap ();
  let tree_id : git2::Oid =
    index . write_tree () . unwrap ();
  let tree : git2::Tree =
    gitrepo . find_tree (tree_id) . unwrap ();
  let sig : git2::Signature =
    git2::Signature::now ("skg test", "skg@example.com") . unwrap ();
  let parent : Option<git2::Commit> =
    gitrepo . head () . ok ()
      . and_then ( |h| h . peel_to_commit () . ok () );
  match parent {
    Some (parent) => {
      gitrepo . commit (
        Some ("HEAD"), &sig, &sig, message, &tree, &[&parent] )
        . unwrap (); },
    None => {
      gitrepo . commit (
        Some ("HEAD"), &sig, &sig, message, &tree, &[] )
        . unwrap (); }}}

fn stage_all (
  gitrepo : &Repository,
) {
  let mut index : git2::Index =
    gitrepo . index () . unwrap ();
  index . add_all (["*"].iter (), git2::IndexAddOption::DEFAULT, None)
    . unwrap ();
  index . write () . unwrap ();
}

fn config_for (
  data_root   : &Path,
  skgrepo_dir : &Path,
) -> SkgConfig {
  let skgrepo_name : SkgrepoName =
    SkgrepoName::from ("main");
  let mut skgrepos : HashMap<SkgrepoName, Skgrepo> =
    HashMap::new ();
  skgrepos . insert (
    skgrepo_name . clone (),
    Skgrepo {
      name: skgrepo_name,
      abbreviation: None,
      path: skgrepo_dir . to_path_buf (),
      owned: true });
  let mut config : SkgConfig =
    SkgConfig::dummyFromSkgrepos (skgrepos);
  config . data_root = data_root . to_path_buf ();
  config
}

#[test]
fn selected_snapshots_distinguish_head_index_and_worktree () {
  let tmp : TempDir =
    tempfile::tempdir () . unwrap ();
  let gitrepo : Repository =
    Repository::init (tmp . path ()) . unwrap ();
  let skgrepo_dir : PathBuf =
    tmp . path () . join ("repo");
  fs::create_dir (&skgrepo_dir) . unwrap ();
  write_node (&skgrepo_dir, "a", "head");
  commit_all (&gitrepo, "head");
  write_node (&skgrepo_dir, "a", "index");
  stage_all (&gitrepo);
  write_node (&skgrepo_dir, "a", "worktree");
  let config : SkgConfig =
    config_for (tmp . path (), &skgrepo_dir);
  let staged_only : GitSnapshotPair =
    read_git_snapshot_pair (
      &config,
      DiffSelection {
        include_staged: true,
        include_unstaged: false }) . unwrap ();
  assert_eq! (
    staged_only . before . nodes [&ID::from ("a")] . title,
    "head" );
  assert_eq! (
    staged_only . after . nodes [&ID::from ("a")] . title,
    "index" );
  let unstaged_only : GitSnapshotPair =
    read_git_snapshot_pair (
      &config,
      DiffSelection {
        include_staged: false,
        include_unstaged: true }) . unwrap ();
  assert_eq! (
    unstaged_only . before . nodes [&ID::from ("a")] . title,
    "index" );
  assert_eq! (
    unstaged_only . after . nodes [&ID::from ("a")] . title,
    "worktree" );
}
