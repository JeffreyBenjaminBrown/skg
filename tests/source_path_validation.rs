// cargo test --test source_path_validation

use std::collections::HashMap;
use std::fs;
use std::io::{Result as IoResult, Error as IoError, ErrorKind as IoErrorKind};
use std::path::PathBuf;
use tempfile::{tempdir, TempDir};

use skg::dbs::filesystem::not_nodes::validate_source_paths_creating_owned_ones_if_needed;
use skg::types::misc::{SkgfileSource, SourceName};

#[test]
fn test_validate_existing_owned_source() {
  // Create a temporary directory
  let dir : TempDir = tempdir().unwrap();
  let source_path : PathBuf = dir.path().to_path_buf();

  let mut sources : HashMap<SourceName, SkgfileSource> =
    HashMap::new();
  sources.insert(
    SourceName::from("main"),
    SkgfileSource {
      nickname: SourceName::from("main"),
      path: source_path.clone(),
      user_owns_it: true,
    }
  );

  // Should succeed since directory exists
  let result : IoResult<()> =
    validate_source_paths_creating_owned_ones_if_needed(&sources);
  assert!(result.is_ok(),
          "Validation should pass for existing owned source");
}

#[test]
fn test_validate_nonexistent_owned_source() {
  // Use a path that doesn't exist yet
  let temp_dir : TempDir = tempdir().unwrap();
  let source_path : PathBuf =
    temp_dir.path().join("nonexistent_source");

  let mut sources : HashMap<SourceName, SkgfileSource> =
    HashMap::new();
  sources.insert(
    SourceName::from("main"),
    SkgfileSource {
      nickname: SourceName::from("main"),
      path: source_path.clone(),
      user_owns_it: true,
    }
  );

  // Should succeed and create the directory
  let result : IoResult<()> =
    validate_source_paths_creating_owned_ones_if_needed(&sources);
  assert!(result.is_ok(), "Validation should pass and create directory for owned source");
  assert!(source_path.exists(), "Directory should have been created");
}

#[test]
fn test_validate_existing_foreign_source() {
  // Create a temporary directory
  let dir : TempDir = tempdir().unwrap();
  let source_path : PathBuf = dir.path().to_path_buf();

  let mut sources : HashMap<SourceName, SkgfileSource> =
    HashMap::new();
  sources.insert(
    SourceName::from("foreign"),
    SkgfileSource {
      nickname: SourceName::from("foreign"),
      path: source_path.clone(),
      user_owns_it: false,
    }
  );

  // Should succeed since directory exists
  let result : IoResult<()> =
    validate_source_paths_creating_owned_ones_if_needed(&sources);
  assert!(result.is_ok(), "Validation should pass for existing foreign source");
}

#[test]
fn test_validate_nonexistent_foreign_source() {
  // Use a path that doesn't exist
  let temp_dir : TempDir = tempdir().unwrap();
  let source_path : PathBuf =
    temp_dir.path().join("nonexistent_foreign");

  let mut sources : HashMap<SourceName, SkgfileSource> =
    HashMap::new();
  sources.insert(
    SourceName::from("foreign"),
    SkgfileSource {
      nickname: SourceName::from("foreign"),
      path: source_path.clone(),
      user_owns_it: false,
    }
  );

  // Should fail since foreign source path doesn't exist
  let result : IoResult<()> =
    validate_source_paths_creating_owned_ones_if_needed(&sources);
  assert!(result.is_err(), "Validation should fail for nonexistent foreign source");

  let err : IoError = result.unwrap_err();
  assert_eq!(err.kind(), IoErrorKind::NotFound);
  assert!(err.to_string().contains("foreign"));
  assert!(err.to_string().contains("does not exist"));
}

#[test]
fn test_validate_multiple_sources() {
  let temp_dir : TempDir = tempdir().unwrap();

  // Create one existing directory
  let existing_path : PathBuf = temp_dir.path().join("existing");
  fs::create_dir_all(&existing_path).unwrap();

  // Path that doesn't exist yet (will be created)
  let new_owned_path : PathBuf = temp_dir.path().join("new_owned");

  let mut sources : HashMap<SourceName, SkgfileSource> =
    HashMap::new();
  sources.insert(
    SourceName::from("existing"),
    SkgfileSource {
      nickname: SourceName::from("existing"),
      path: existing_path.clone(),
      user_owns_it: true,
    }
  );
  sources.insert(
    SourceName::from("new_owned"),
    SkgfileSource {
      nickname: SourceName::from("new_owned"),
      path: new_owned_path.clone(),
      user_owns_it: true,
    }
  );

  // Should succeed, creating the new directory
  let result : IoResult<()> =
    validate_source_paths_creating_owned_ones_if_needed(&sources);
  assert!(result.is_ok(), "Validation should pass for multiple sources");
  assert!(existing_path.exists(), "Existing path should still exist");
  assert!(new_owned_path.exists(), "New owned path should have been created");
}

#[test]
fn test_validate_multiple_sources_with_foreign_failure() {
  let temp_dir : TempDir = tempdir().unwrap();

  // Create one existing directory
  let existing_path : PathBuf = temp_dir.path().join("existing");
  fs::create_dir_all(&existing_path).unwrap();

  // Path that doesn't exist (foreign, so should fail)
  let nonexistent_foreign : PathBuf =
    temp_dir.path().join("nonexistent_foreign");

  let mut sources : HashMap<SourceName, SkgfileSource> =
    HashMap::new();
  sources.insert(
    SourceName::from("existing"),
    SkgfileSource {
      nickname: SourceName::from("existing"),
      path: existing_path.clone(),
      user_owns_it: true,
    }
  );
  sources.insert(
    SourceName::from("foreign"),
    SkgfileSource {
      nickname: SourceName::from("foreign"),
      path: nonexistent_foreign.clone(),
      user_owns_it: false,
    }
  );

  // Should fail because of the foreign source
  let result : IoResult<()> =
    validate_source_paths_creating_owned_ones_if_needed(&sources);
  assert!(result.is_err(),
          "Validation should fail when foreign source doesn't exist");

  let err : IoError = result.unwrap_err();
  assert_eq!(err.kind(), IoErrorKind::NotFound);
  assert!(err.to_string().contains("foreign"));
}
