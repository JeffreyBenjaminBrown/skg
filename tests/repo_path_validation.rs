// cargo nextest run --test grouped_unit -E 'test(repo_path_validation::)'

use std::collections::HashMap;
use std::fs;
use std::io::{Result as IoResult, Error as IoError, ErrorKind as IoErrorKind};
use std::path::PathBuf;
use tempfile::{tempdir, TempDir};

use skg::dbs::filesystem::not_nodes::{load_config, validate_skgrepo_paths_creating_owned_ones_if_needed};
use skg::types::misc::{SkgConfig, Skgrepo, SkgrepoName};

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

/// Write CONFIG_TEXT as the skgconfig.toml of a fresh data root, with
/// a directory for each of SKGREPO_FOLDERS, and load it.
fn load_config_text_in_data_root (
  data_root       : &TempDir,
  skgrepo_folders : &[&str],
  config_text     : &str,
) -> Result<(SkgConfig, Vec<String>), Box<dyn std::error::Error>> {
  for folder in skgrepo_folders {
    fs::create_dir_all ( data_root . path () . join (folder) ) ?; }
  let config_path : PathBuf =
    data_root . path () . join ("skgconfig.toml");
  fs::write ( &config_path, config_text ) ?;
  load_config ( config_path . to_str () . unwrap () ) }

#[test]
fn a_skgrepo_is_owned_unless_its_entry_says_foreign (
) -> Result<(), Box<dyn std::error::Error>> {
  let data_root : TempDir = tempdir () ?;
  let (config, _warnings) : (SkgConfig, Vec<String>) =
    load_config_text_in_data_root (
      &data_root, &["eggman/eggs", "jeff/notes", "jeff/work"],
      concat! (
        "tantivy_folder = \"tantivy\"\n",
        "[[repos]]\npath = \"eggman/eggs\"\nforeign = true\n",
        "[[repos]]\npath = \"jeff/notes\"\n",
        "[[repos]]\nname = \"work\"\npath = \"jeff/work\"\n",
        "foreign = false\n" )) ?;
  let eggs : &Skgrepo =
    &config . skgrepos [&SkgrepoName::from ("eggman/eggs")];
  let notes : &Skgrepo =
    &config . skgrepos [&SkgrepoName::from ("jeff/notes")];
  let work : &Skgrepo =
    &config . skgrepos [&SkgrepoName::from ("work")];
  assert! ( ! eggs . owned );
  assert! ( notes . owned );
  assert! ( work . owned );
  { // An unnamed entry is named by its path, in the fold order too.
    assert_eq! ( config . ordered_skgrepos (),
                 vec! [ SkgrepoName::from ("eggman/eggs"),
                        SkgrepoName::from ("jeff/notes"),
                        SkgrepoName::from ("work") ] ); }
  { // The default abbreviation of an owned skgrepo drops its author
    // folder; a foreign one keeps the full path; a named one gets none.
    assert_eq! ( notes . herald_label (), "notes" );
    assert_eq! ( eggs . herald_label (), "eggman/eggs" );
    assert_eq! ( work . abbreviation, None ); }
  Ok (( )) }

#[test]
fn foreign_entries_may_sit_anywhere_and_only_owned_ones_are_ranked (
) -> Result<(), Box<dyn std::error::Error>> {
  let data_root : TempDir = tempdir () ?;
  let (config, _warnings) : (SkgConfig, Vec<String>) =
    load_config_text_in_data_root (
      &data_root,
      &["eggman/eggs", "jeff/public", "fred/notes", "jeff/private"],
      concat! (
        "tantivy_folder = \"tantivy\"\n",
        "[[repos]]\nname = \"public\"\npath = \"jeff/public\"\n",
        "[[repos]]\nname = \"eggs\"\npath = \"eggman/eggs\"\n",
        "foreign = true\n",
        "[[repos]]\nname = \"private\"\npath = \"jeff/private\"\n",
        "[[repos]]\nname = \"notes\"\npath = \"fred/notes\"\n",
        "foreign = true\n" )) ?;
  let name = |n : &str| SkgrepoName::from (n);
  assert_eq! ( config . privacy_ladder (),
               vec! [ name ("public"), name ("private") ] );
  assert_eq! ( config . ordered_skgrepos (),
               vec! [ name ("public"), name ("eggs"),
                      name ("private"), name ("notes") ],
               "no foreign manifest: declaration order" );
  { // A foreign repo has no rung: it is neither more nor less public
    // than anything, and never the more private of a pair.
    assert! ( config . is_strictly_more_public (
                &name ("public"), &name ("private") ));
    for owned in ["public", "private"] {
      assert! ( ! config . is_strictly_more_public (
                  &name ("eggs"), &name (owned) ));
      assert! ( ! config . is_strictly_more_public (
                  &name (owned), &name ("eggs") ));
      assert_eq! ( config . more_private_of ( name ("eggs"), name (owned) ),
                   name (owned) );
      assert_eq! ( config . more_private_of ( name (owned), name ("eggs") ),
                   name (owned) ); }}
  Ok (( )) }

#[test]
fn a_default_repo_set_naming_a_foreign_repo_is_refused (
) -> Result<(), Box<dyn std::error::Error>> {
  let data_root : TempDir = tempdir () ?;
  let config_text = |default : &str| -> String { format! (
    "tantivy_folder = \"tantivy\"\n\
     default_repo_set = \"{}\"\n\
     [[repos]]\nname = \"eggs\"\npath = \"eggman/eggs\"\nforeign = true\n\
     [[repos]]\nname = \"public\"\npath = \"jeff/public\"\n\
     [[repos]]\nname = \"private\"\npath = \"jeff/private\"\n",
    default ) };
  let refusal : String = load_config_text_in_data_root (
    &data_root, &["eggman/eggs", "jeff/public", "jeff/private"],
    &config_text ("eggs") )
    . err () . expect ("a default_repo_set naming a foreign repo must be refused")
    . to_string ();
  assert! ( refusal . contains ("'eggs'") && refusal . contains ("foreign"),
            "the refusal names the repo and says it is foreign: {}", refusal );
  let (config, _warnings) : (SkgConfig, Vec<String>) =
    load_config_text_in_data_root (
      &data_root, &["eggman/eggs", "jeff/public", "jeff/private"],
      &config_text ("public") ) ?;
  assert_eq! ( config . default_skgrepo_set,
               Some ( SkgrepoName::from ("public") ),
               "an owned one is accepted" );
  Ok (( )) }

#[test]
fn the_retired_owned_folder_key_is_refused (
) -> Result<(), Box<dyn std::error::Error>> {
  let data_root : TempDir = tempdir () ?;
  let refusal : String = load_config_text_in_data_root (
    &data_root, &["owned/public"],
    concat! (
      "tantivy_folder = \"tantivy\"\n",
      "owned_folder = \"owned\"\n",
      "[[repos]]\nname = \"public\"\npath = \"owned/public\"\n" ))
    . err () . expect ("a config setting owned_folder must be refused")
    . to_string ();
  assert! ( refusal . contains ("owned_folder")
            && refusal . contains ("foreign = true"),
            "the refusal names the key and the flag: {}", refusal );
  Ok (( )) }
