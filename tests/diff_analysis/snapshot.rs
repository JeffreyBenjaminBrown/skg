use skg::diff_analysis::snapshot::read_snapshot_pair;
use skg::diff_analysis::types::{DiffSelection, SnapshotPair};
use skg::types::misc::{ID, SkgConfig, SkgfileSource, SourceName};

use git2::Repository;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

fn write_node (
  source_dir : &Path,
  pid        : &str,
  title      : &str,
) {
  fs::write (
    source_dir . join (format! ("{}.skg", pid)),
    format! ("title: {}\npid: {}\n", title, pid) )
    . unwrap ();
}

fn commit_all (
  repo    : &Repository,
  message : &str,
) {
  let mut index : git2::Index =
    repo . index () . unwrap ();
  index . add_all (["*"].iter (), git2::IndexAddOption::DEFAULT, None)
    . unwrap ();
  index . write () . unwrap ();
  let tree_id : git2::Oid =
    index . write_tree () . unwrap ();
  let tree : git2::Tree =
    repo . find_tree (tree_id) . unwrap ();
  let sig : git2::Signature =
    git2::Signature::now ("skg test", "skg@example.com") . unwrap ();
  let parent : Option<git2::Commit> =
    repo . head () . ok ()
      . and_then ( |h| h . peel_to_commit () . ok () );
  match parent {
    Some (parent) => {
      repo . commit (
        Some ("HEAD"), &sig, &sig, message, &tree, &[&parent] )
        . unwrap (); },
    None => {
      repo . commit (
        Some ("HEAD"), &sig, &sig, message, &tree, &[] )
        . unwrap (); }}}

fn stage_all (
  repo : &Repository,
) {
  let mut index : git2::Index =
    repo . index () . unwrap ();
  index . add_all (["*"].iter (), git2::IndexAddOption::DEFAULT, None)
    . unwrap ();
  index . write () . unwrap ();
}

fn config_for (
  data_root  : &Path,
  source_dir : &Path,
) -> SkgConfig {
  let source_name : SourceName =
    SourceName::from ("main");
  let mut sources : HashMap<SourceName, SkgfileSource> =
    HashMap::new ();
  sources . insert (
    source_name . clone (),
    SkgfileSource {
      name: source_name,
      abbreviation: None,
      path: source_dir . to_path_buf (),
      user_owns_it: true });
  let mut config : SkgConfig =
    SkgConfig::dummyFromSources (sources);
  config . data_root = data_root . to_path_buf ();
  config
}

#[test]
fn selected_snapshots_distinguish_head_index_and_worktree () {
  let tmp : TempDir =
    tempfile::tempdir () . unwrap ();
  let repo : Repository =
    Repository::init (tmp . path ()) . unwrap ();
  let source_dir : PathBuf =
    tmp . path () . join ("source");
  fs::create_dir (&source_dir) . unwrap ();
  write_node (&source_dir, "a", "head");
  commit_all (&repo, "head");
  write_node (&source_dir, "a", "index");
  stage_all (&repo);
  write_node (&source_dir, "a", "worktree");
  let config : SkgConfig =
    config_for (tmp . path (), &source_dir);
  let staged_only : SnapshotPair =
    read_snapshot_pair (
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
  let unstaged_only : SnapshotPair =
    read_snapshot_pair (
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
