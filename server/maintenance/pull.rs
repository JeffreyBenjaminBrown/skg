//! Logical repository identity for client-owned pull operations.
//!
//! Editor and server absolute paths commonly differ across a container mount.
//! The shared authority is therefore a deterministic key over the configured
//! source names which resolve to one Git repository on each side.

use super::pull_repository_key;
use crate::types::misc::SkgConfig;

use git2::Repository;
use std::collections::BTreeMap;
use std::path::PathBuf;

pub fn repository_mapping (
  config : &SkgConfig,
) -> Result<BTreeMap<String, Vec<String>>, String> {
  let mut by_repository : BTreeMap<PathBuf, Vec<String>> = BTreeMap::new ();
  for source_name in config . ordered_sources () {
    let source = config . sources . get (&source_name)
      . expect ("ordered source exists");
    let repository = Repository::discover (&source . path) . map_err (|error|
      format! (
        "configured source '{}' is not in a Git worktree: {}",
        source_name . 0, error))?;
    if repository . workdir () . is_none () {
      return Err (format! (
        "configured source '{}' belongs to a bare Git repository",
        source_name . 0)); }
    let identity = repository . path () . canonicalize () . map_err (|error|
      format! (
        "cannot resolve Git repository for source '{}': {}",
        source_name . 0, error))?;
    by_repository . entry (identity) . or_default ()
      . push (source_name . 0 . clone ());
  }
  let mut result = BTreeMap::new ();
  for mut sources in by_repository . into_values () {
    sources . sort ();
    let key = pull_repository_key (&sources);
    if result . insert (key . clone (), sources) . is_some () {
      return Err (format! (
        "two Git repositories produced the same logical key {}", key)); }
  }
  Ok (result)
}

pub fn validate_repository_mapping (
  config       : &SkgConfig,
  repositories : &BTreeMap<String, Vec<String>>,
) -> Result<(), String> {
  let expected = repository_mapping (config)?;
  if repositories != &expected {
    return Err (format! (
      "client pull repository mapping {:?} does not match server mapping {:?}",
      repositories, expected)); }
  Ok (( ))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::misc::{SkgfileSource, SourceName};

  use std::collections::HashMap;
  use std::fs;
  use tempfile::TempDir;

  fn source (name : &str, path : PathBuf) -> (SourceName, SkgfileSource) {
    let name = SourceName::from (name);
    (name . clone (), SkgfileSource {
      name, abbreviation: None, path, user_owns_it: true,
    })
  }

  #[test]
  fn mapping_must_match_the_server_git_topology () {
    let temp = TempDir::new () . unwrap ();
    let first = temp . path () . join ("first");
    let second = temp . path () . join ("second");
    let one = first . join ("one");
    let two = first . join ("two");
    let three = second . join ("three");
    for source in [&one, &two, &three] {
      fs::create_dir_all (source) . unwrap (); }
    Repository::init (&first) . unwrap ();
    Repository::init (&second) . unwrap ();
    let config = SkgConfig::dummyFromSources (HashMap::from ([
      source ("one", one), source ("two", two), source ("three", three),
    ]));
    let expected = BTreeMap::from ([
      (pull_repository_key (&["one" . into (), "two" . into ()]),
       vec!["one" . into (), "two" . into ()]),
      (pull_repository_key (&["three" . into ()]), vec!["three" . into ()]),
    ]);
    assert_eq! (repository_mapping (&config) . unwrap (), expected);
    assert! (validate_repository_mapping (&config, &expected) . is_ok ());

    let split = BTreeMap::from ([
      (pull_repository_key (&["one" . into ()]), vec!["one" . into ()]),
      (pull_repository_key (&["two" . into ()]), vec!["two" . into ()]),
      (pull_repository_key (&["three" . into ()]), vec!["three" . into ()]),
    ]);
    assert! (validate_repository_mapping (&config, &split) . is_err ());
  }
}
