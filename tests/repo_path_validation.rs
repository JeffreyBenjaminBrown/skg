// cargo nextest run --test grouped_unit -E 'test(repo_path_validation::)'

use std::collections::HashMap;
use std::fs;
use std::io::{Result as IoResult, Error as IoError, ErrorKind as IoErrorKind};
use std::path::PathBuf;
use tempfile::{tempdir, TempDir};

use skg::dbs::filesystem::not_nodes::validate_repo_paths_creating_owned_ones_if_needed;
use skg::types::misc::{SkgfileRepo, RepoName};

#[test]
fn test_validate_existing_owned_repo() {
  // Create a temporary directory
  let dir : TempDir = tempdir() . unwrap();
  let repo_path : PathBuf = dir . path() . to_path_buf();

  let mut repos : HashMap<RepoName, SkgfileRepo> =
    HashMap::new();
  repos . insert(
    RepoName::from ("main"),
    SkgfileRepo {
      name: RepoName::from ("main"),
        abbreviation: None,
      path: repo_path . clone(),
      user_owns_it: true,
    }
  );

  // Should succeed since directory exists
  let result : IoResult<()> =
    validate_repo_paths_creating_owned_ones_if_needed (&repos);
  assert!(result . is_ok(),
          "Validation should pass for existing owned source");
}

#[test]
fn test_validate_nonexistent_owned_repo() {
  // Use a path that doesn't exist yet
  let temp_dir : TempDir = tempdir() . unwrap();
  let repo_path : PathBuf =
    temp_dir . path() . join ("nonexistent_source");

  let mut repos : HashMap<RepoName, SkgfileRepo> =
    HashMap::new();
  repos . insert(
    RepoName::from ("main"),
    SkgfileRepo {
      name: RepoName::from ("main"),
        abbreviation: None,
      path: repo_path . clone(),
      user_owns_it: true,
    }
  );

  // Should succeed and create the directory
  let result : IoResult<()> =
    validate_repo_paths_creating_owned_ones_if_needed (&repos);
  assert!(result . is_ok(), "Validation should pass and create directory for owned source");
  assert!(repo_path . exists(), "Directory should have been created");
}

#[test]
fn test_validate_existing_foreign_repo() {
  // Create a temporary directory
  let dir : TempDir = tempdir() . unwrap();
  let repo_path : PathBuf = dir . path() . to_path_buf();

  let mut repos : HashMap<RepoName, SkgfileRepo> =
    HashMap::new();
  repos . insert(
    RepoName::from ("foreign"),
    SkgfileRepo {
      name: RepoName::from ("foreign"),
        abbreviation: None,
      path: repo_path . clone(),
      user_owns_it: false,
    }
  );

  // Should succeed since directory exists
  let result : IoResult<()> =
    validate_repo_paths_creating_owned_ones_if_needed (&repos);
  assert!(result . is_ok(), "Validation should pass for existing foreign source");
}

#[test]
fn test_validate_nonexistent_foreign_repo() {
  // Use a path that doesn't exist
  let temp_dir : TempDir = tempdir() . unwrap();
  let repo_path : PathBuf =
    temp_dir . path() . join ("nonexistent_foreign");

  let mut repos : HashMap<RepoName, SkgfileRepo> =
    HashMap::new();
  repos . insert(
    RepoName::from ("foreign"),
    SkgfileRepo {
      name: RepoName::from ("foreign"),
        abbreviation: None,
      path: repo_path . clone(),
      user_owns_it: false,
    }
  );

  // Should fail since foreign repo path doesn't exist
  let result : IoResult<()> =
    validate_repo_paths_creating_owned_ones_if_needed (&repos);
  assert!(result . is_err(), "Validation should fail for nonexistent foreign source");

  let err : IoError = result . unwrap_err();
  assert_eq!(err . kind(), IoErrorKind::NotFound);
  assert!(err . to_string() . contains ("foreign"));
  assert!(err . to_string() . contains ("does not exist"));
}

#[test]
fn test_validate_multiple_repos() {
  let temp_dir : TempDir = tempdir() . unwrap();

  // Create one existing directory
  let existing_path : PathBuf = temp_dir . path() . join ("existing");
  fs::create_dir_all (&existing_path) . unwrap();

  // Path that doesn't exist yet (will be created)
  let new_owned_path : PathBuf = temp_dir . path() . join ("new_owned");

  let mut repos : HashMap<RepoName, SkgfileRepo> =
    HashMap::new();
  repos . insert(
    RepoName::from ("existing"),
    SkgfileRepo {
      name: RepoName::from ("existing"),
        abbreviation: None,
      path: existing_path . clone(),
      user_owns_it: true,
    }
  );
  repos . insert(
    RepoName::from ("new_owned"),
    SkgfileRepo {
      name: RepoName::from ("new_owned"),
        abbreviation: None,
      path: new_owned_path . clone(),
      user_owns_it: true,
    }
  );

  // Should succeed, creating the new directory
  let result : IoResult<()> =
    validate_repo_paths_creating_owned_ones_if_needed (&repos);
  assert!(result . is_ok(), "Validation should pass for multiple sources");
  assert!(existing_path . exists(), "Existing path should still exist");
  assert!(new_owned_path . exists(), "New owned path should have been created");
}

#[test]
fn test_validate_multiple_repos_with_foreign_failure() {
  let temp_dir : TempDir = tempdir() . unwrap();

  // Create one existing directory
  let existing_path : PathBuf = temp_dir . path() . join ("existing");
  fs::create_dir_all (&existing_path) . unwrap();

  // Path that doesn't exist (foreign, so should fail)
  let nonexistent_foreign : PathBuf =
    temp_dir . path() . join ("nonexistent_foreign");

  let mut repos : HashMap<RepoName, SkgfileRepo> =
    HashMap::new();
  repos . insert(
    RepoName::from ("existing"),
    SkgfileRepo {
      name: RepoName::from ("existing"),
        abbreviation: None,
      path: existing_path . clone(),
      user_owns_it: true,
    }
  );
  repos . insert(
    RepoName::from ("foreign"),
    SkgfileRepo {
      name: RepoName::from ("foreign"),
        abbreviation: None,
      path: nonexistent_foreign . clone(),
      user_owns_it: false,
    }
  );

  // Should fail because of the foreign repo
  let result : IoResult<()> =
    validate_repo_paths_creating_owned_ones_if_needed (&repos);
  assert!(result . is_err(),
          "Validation should fail when foreign source doesn't exist");

  let err : IoError = result . unwrap_err();
  assert_eq!(err . kind(), IoErrorKind::NotFound);
  assert!(err . to_string() . contains ("foreign"));
}
