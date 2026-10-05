// cargo nextest run --test grouped_unit -E 'test(repo_path_validation::)'

use std::collections::HashMap;
use std::fs;
use std::io::{Result as IoResult, Error as IoError, ErrorKind as IoErrorKind};
use std::path::PathBuf;
use tempfile::{tempdir, TempDir};

use skg::dbs::filesystem::not_nodes::validate_skgrepo_paths_creating_owned_ones_if_needed;
use skg::types::misc::{Skgrepo, SkgrepoName};

#[test]
fn test_validate_existing_owned_skgrepo() {
  // Create a temporary directory
  let dir : TempDir = tempdir() . unwrap();
  let skgrepo_path : PathBuf = dir . path() . to_path_buf();

  let mut skgrepos : HashMap<SkgrepoName, Skgrepo> =
    HashMap::new();
  skgrepos . insert(
    SkgrepoName::from ("main"),
    Skgrepo {
      name: SkgrepoName::from ("main"),
        abbreviation: None,
      path: skgrepo_path . clone(),
      owned: true,
    }
  );

  // Should succeed since directory exists
  let result : IoResult<()> =
    validate_skgrepo_paths_creating_owned_ones_if_needed (&skgrepos);
  assert!(result . is_ok(),
          "Validation should pass for existing owned repo");
}

#[test]
fn test_validate_nonexistent_owned_skgrepo() {
  // Use a path that doesn't exist yet
  let temp_dir : TempDir = tempdir() . unwrap();
  let skgrepo_path : PathBuf =
    temp_dir . path() . join ("nonexistent_repo");

  let mut skgrepos : HashMap<SkgrepoName, Skgrepo> =
    HashMap::new();
  skgrepos . insert(
    SkgrepoName::from ("main"),
    Skgrepo {
      name: SkgrepoName::from ("main"),
        abbreviation: None,
      path: skgrepo_path . clone(),
      owned: true,
    }
  );

  // Should succeed and create the directory
  let result : IoResult<()> =
    validate_skgrepo_paths_creating_owned_ones_if_needed (&skgrepos);
  assert!(result . is_ok(), "Validation should pass and create directory for owned repo");
  assert!(skgrepo_path . exists(), "Directory should have been created");
}

#[test]
fn test_validate_existing_foreign_skgrepo() {
  // Create a temporary directory
  let dir : TempDir = tempdir() . unwrap();
  let skgrepo_path : PathBuf = dir . path() . to_path_buf();

  let mut skgrepos : HashMap<SkgrepoName, Skgrepo> =
    HashMap::new();
  skgrepos . insert(
    SkgrepoName::from ("foreign"),
    Skgrepo {
      name: SkgrepoName::from ("foreign"),
        abbreviation: None,
      path: skgrepo_path . clone(),
      owned: false,
    }
  );

  // Should succeed since directory exists
  let result : IoResult<()> =
    validate_skgrepo_paths_creating_owned_ones_if_needed (&skgrepos);
  assert!(result . is_ok(), "Validation should pass for existing foreign repo");
}

#[test]
fn test_validate_nonexistent_foreign_skgrepo() {
  // Use a path that doesn't exist
  let temp_dir : TempDir = tempdir() . unwrap();
  let skgrepo_path : PathBuf =
    temp_dir . path() . join ("nonexistent_foreign");

  let mut skgrepos : HashMap<SkgrepoName, Skgrepo> =
    HashMap::new();
  skgrepos . insert(
    SkgrepoName::from ("foreign"),
    Skgrepo {
      name: SkgrepoName::from ("foreign"),
        abbreviation: None,
      path: skgrepo_path . clone(),
      owned: false,
    }
  );

  // Should fail since foreign skgrepo path doesn't exist
  let result : IoResult<()> =
    validate_skgrepo_paths_creating_owned_ones_if_needed (&skgrepos);
  assert!(result . is_err(), "Validation should fail for nonexistent foreign repo");

  let err : IoError = result . unwrap_err();
  assert_eq!(err . kind(), IoErrorKind::NotFound);
  assert!(err . to_string() . contains ("foreign"));
  assert!(err . to_string() . contains ("does not exist"));
}

#[test]
fn test_validate_multiple_skgrepos() {
  let temp_dir : TempDir = tempdir() . unwrap();

  // Create one existing directory
  let existing_path : PathBuf = temp_dir . path() . join ("existing");
  fs::create_dir_all (&existing_path) . unwrap();

  // Path that doesn't exist yet (will be created)
  let new_owned_path : PathBuf = temp_dir . path() . join ("new_owned");

  let mut skgrepos : HashMap<SkgrepoName, Skgrepo> =
    HashMap::new();
  skgrepos . insert(
    SkgrepoName::from ("existing"),
    Skgrepo {
      name: SkgrepoName::from ("existing"),
        abbreviation: None,
      path: existing_path . clone(),
      owned: true,
    }
  );
  skgrepos . insert(
    SkgrepoName::from ("new_owned"),
    Skgrepo {
      name: SkgrepoName::from ("new_owned"),
        abbreviation: None,
      path: new_owned_path . clone(),
      owned: true,
    }
  );

  // Should succeed, creating the new directory
  let result : IoResult<()> =
    validate_skgrepo_paths_creating_owned_ones_if_needed (&skgrepos);
  assert!(result . is_ok(), "Validation should pass for multiple repos");
  assert!(existing_path . exists(), "Existing path should still exist");
  assert!(new_owned_path . exists(), "New owned path should have been created");
}

#[test]
fn test_validate_multiple_skgrepos_with_foreign_failure() {
  let temp_dir : TempDir = tempdir() . unwrap();

  // Create one existing directory
  let existing_path : PathBuf = temp_dir . path() . join ("existing");
  fs::create_dir_all (&existing_path) . unwrap();

  // Path that doesn't exist (foreign, so should fail)
  let nonexistent_foreign : PathBuf =
    temp_dir . path() . join ("nonexistent_foreign");

  let mut skgrepos : HashMap<SkgrepoName, Skgrepo> =
    HashMap::new();
  skgrepos . insert(
    SkgrepoName::from ("existing"),
    Skgrepo {
      name: SkgrepoName::from ("existing"),
        abbreviation: None,
      path: existing_path . clone(),
      owned: true,
    }
  );
  skgrepos . insert(
    SkgrepoName::from ("foreign"),
    Skgrepo {
      name: SkgrepoName::from ("foreign"),
        abbreviation: None,
      path: nonexistent_foreign . clone(),
      owned: false,
    }
  );

  // Should fail because of the foreign skgrepo
  let result : IoResult<()> =
    validate_skgrepo_paths_creating_owned_ones_if_needed (&skgrepos);
  assert!(result . is_err(),
          "Validation should fail when foreign repo doesn't exist");

  let err : IoError = result . unwrap_err();
  assert_eq!(err . kind(), IoErrorKind::NotFound);
  assert!(err . to_string() . contains ("foreign"));
}
