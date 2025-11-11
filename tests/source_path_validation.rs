// cargo test --test source_path_validation

use std::collections::HashMap;
use std::fs;
use tempfile::tempdir;

use skg::media::file_io::misc::validate_source_paths_creating_owned_ones_if_needed;
use skg::types::misc::SkgfileSource;

#[test]
fn test_validate_existing_owned_source() {
  // Create a temporary directory
  let dir = tempdir().unwrap();
  let source_path = dir.path().to_path_buf();

  let mut sources: HashMap<String, SkgfileSource> = HashMap::new();
  sources.insert(
    "main".to_string(),
    SkgfileSource {
      nickname: "main".to_string(),
      path: source_path.clone(),
      user_owns_it: true,
    }
  );

  // Should succeed since directory exists
  let result = validate_source_paths_creating_owned_ones_if_needed(&sources);
  assert!(result.is_ok(), "Validation should pass for existing owned source");
}

#[test]
fn test_validate_nonexistent_owned_source() {
  // Use a path that doesn't exist yet
  let temp_dir = tempdir().unwrap();
  let source_path = temp_dir.path().join("nonexistent_source");

  let mut sources: HashMap<String, SkgfileSource> = HashMap::new();
  sources.insert(
    "main".to_string(),
    SkgfileSource {
      nickname: "main".to_string(),
      path: source_path.clone(),
      user_owns_it: true,
    }
  );

  // Should succeed and create the directory
  let result = validate_source_paths_creating_owned_ones_if_needed(&sources);
  assert!(result.is_ok(), "Validation should pass and create directory for owned source");
  assert!(source_path.exists(), "Directory should have been created");
}

#[test]
fn test_validate_existing_foreign_source() {
  // Create a temporary directory
  let dir = tempdir().unwrap();
  let source_path = dir.path().to_path_buf();

  let mut sources: HashMap<String, SkgfileSource> = HashMap::new();
  sources.insert(
    "foreign".to_string(),
    SkgfileSource {
      nickname: "foreign".to_string(),
      path: source_path.clone(),
      user_owns_it: false,
    }
  );

  // Should succeed since directory exists
  let result = validate_source_paths_creating_owned_ones_if_needed(&sources);
  assert!(result.is_ok(), "Validation should pass for existing foreign source");
}

#[test]
fn test_validate_nonexistent_foreign_source() {
  // Use a path that doesn't exist
  let temp_dir = tempdir().unwrap();
  let source_path = temp_dir.path().join("nonexistent_foreign");

  let mut sources: HashMap<String, SkgfileSource> = HashMap::new();
  sources.insert(
    "foreign".to_string(),
    SkgfileSource {
      nickname: "foreign".to_string(),
      path: source_path.clone(),
      user_owns_it: false,
    }
  );

  // Should fail since foreign source path doesn't exist
  let result = validate_source_paths_creating_owned_ones_if_needed(&sources);
  assert!(result.is_err(), "Validation should fail for nonexistent foreign source");

  let err = result.unwrap_err();
  assert_eq!(err.kind(), std::io::ErrorKind::NotFound);
  assert!(err.to_string().contains("foreign"));
  assert!(err.to_string().contains("does not exist"));
}

#[test]
fn test_validate_multiple_sources() {
  let temp_dir = tempdir().unwrap();

  // Create one existing directory
  let existing_path = temp_dir.path().join("existing");
  fs::create_dir_all(&existing_path).unwrap();

  // Path that doesn't exist yet (will be created)
  let new_owned_path = temp_dir.path().join("new_owned");

  let mut sources: HashMap<String, SkgfileSource> = HashMap::new();
  sources.insert(
    "existing".to_string(),
    SkgfileSource {
      nickname: "existing".to_string(),
      path: existing_path.clone(),
      user_owns_it: true,
    }
  );
  sources.insert(
    "new_owned".to_string(),
    SkgfileSource {
      nickname: "new_owned".to_string(),
      path: new_owned_path.clone(),
      user_owns_it: true,
    }
  );

  // Should succeed, creating the new directory
  let result = validate_source_paths_creating_owned_ones_if_needed(&sources);
  assert!(result.is_ok(), "Validation should pass for multiple sources");
  assert!(existing_path.exists(), "Existing path should still exist");
  assert!(new_owned_path.exists(), "New owned path should have been created");
}

#[test]
fn test_validate_multiple_sources_with_foreign_failure() {
  let temp_dir = tempdir().unwrap();

  // Create one existing directory
  let existing_path = temp_dir.path().join("existing");
  fs::create_dir_all(&existing_path).unwrap();

  // Path that doesn't exist (foreign, so should fail)
  let nonexistent_foreign = temp_dir.path().join("nonexistent_foreign");

  let mut sources: HashMap<String, SkgfileSource> = HashMap::new();
  sources.insert(
    "existing".to_string(),
    SkgfileSource {
      nickname: "existing".to_string(),
      path: existing_path.clone(),
      user_owns_it: true,
    }
  );
  sources.insert(
    "foreign".to_string(),
    SkgfileSource {
      nickname: "foreign".to_string(),
      path: nonexistent_foreign.clone(),
      user_owns_it: false,
    }
  );

  // Should fail because of the foreign source
  let result = validate_source_paths_creating_owned_ones_if_needed(&sources);
  assert!(result.is_err(), "Validation should fail when foreign source doesn't exist");

  let err = result.unwrap_err();
  assert_eq!(err.kind(), std::io::ErrorKind::NotFound);
  assert!(err.to_string().contains("foreign"));
}
