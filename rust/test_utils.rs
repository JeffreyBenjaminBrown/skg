use crate::save::{headline_to_triple, HeadlineInfo};
use crate::types::{OrgNode, OrgnodeMetadata, SkgNode, SkgConfig};
use crate::file_io::read_skg_files;
use crate::typedb::init::{overwrite_new_empty_db, define_schema};
use crate::typedb::nodes::create_all_nodes;
use crate::typedb::relationships::create_all_relationships;
use ego_tree::Tree;
use std::error::Error;
use std::fs;
use std::path::{Path, PathBuf};
use typedb_driver::{TypeDBDriver, Credentials, DriverOptions};

/// A helper function for tests.
pub async fn populate_test_db_from_fixtures (
  data_folder: &str,
  db_name: &str,
  driver: &TypeDBDriver
) -> Result<(), Box<dyn Error>> {
  let nodes: Vec<SkgNode> =
    read_skg_files(data_folder)?;
  overwrite_new_empty_db (
    db_name, driver ). await ?;
  define_schema (
    db_name, driver ). await?;
  create_all_nodes (
    db_name, driver, &nodes ). await ?;
  create_all_relationships (
    db_name, driver, &nodes ). await ?;
  Ok (( )) }

/// Setup a test database with fixtures from a given folder.
/// Returns (SkgConfig, TypeDBDriver) for use in tests.
///
/// The test database will be named with the given db_name prefix.
/// Call `cleanup_test_db` after the test completes to remove the database.
pub async fn setup_test_db (
  db_name: &str,
  fixtures_folder: &str,
  tantivy_folder: &str,
) -> Result<(SkgConfig, TypeDBDriver), Box<dyn Error>> {
  let config: SkgConfig = SkgConfig {
    db_name: db_name.to_string(),
    skg_folder: PathBuf::from(fixtures_folder),
    tantivy_folder: PathBuf::from(tantivy_folder),
    port: 1730,
  };

  let driver: TypeDBDriver = TypeDBDriver::new(
    "127.0.0.1:1729",
    Credentials::new("admin", "password"),
    DriverOptions::new(false, None)?
  ).await?;

  populate_test_db_from_fixtures(
    fixtures_folder,
    db_name,
    &driver
  ).await?;

  Ok((config, driver))
}

/// Clean up test database and tantivy index after a test completes.
///
/// This deletes:
/// - The TypeDB database (if it exists)
/// - The Tantivy index directory (if it exists)
///
/// Does NOT delete the .skg fixture files.
pub async fn cleanup_test_db(
  db_name: &str,
  driver: &TypeDBDriver,
  tantivy_folder: Option<&Path>,
) -> Result<(), Box<dyn Error>> {
  // Delete TypeDB database
  let databases = driver.databases();
  if databases.contains(db_name).await? {
    let database = databases.get(db_name).await?;
    database.delete().await?;
  }

  // Delete Tantivy index if path provided and exists
  if let Some(tantivy_path) = tantivy_folder {
    if tantivy_path.exists() {
      fs::remove_dir_all(tantivy_path)?;
    }
  }

  Ok(())
}

/// Compare two org-mode headlines ignoring ID differences.
/// Converts each headline to HeadlineInfo and strips ID from metadata.
pub fn compare_headlines_modulo_id(
  headline1: &str,
  headline2: &str
) -> bool {
  let info1: Result<HeadlineInfo, String> = headline_to_triple(headline1);
  let info2: Result<HeadlineInfo, String> = headline_to_triple(headline2);

  match (info1, info2) {
    (Ok((level1, metadata1, title1)),
     Ok((level2, metadata2, title2))) => {
      let has_id1: bool = metadata1.as_ref().map_or(false, |m| m.id.is_some());
      let has_id2: bool = metadata2.as_ref().map_or(false, |m| m.id.is_some());
      if has_id1 != has_id2 {
        return false; } // One has ID, other doesn't, so they are unequal.
      // Strip IDs from both (no-op if no ID present) and compare
      let stripped_metadata1: Option<OrgnodeMetadata> = strip_id_from_metadata_struct(metadata1);
      let stripped_metadata2: Option<OrgnodeMetadata> = strip_id_from_metadata_struct(metadata2);
      (level1, stripped_metadata1, title1) == (level2, stripped_metadata2, title2)
    },
    (Err(e1), Err(e2)) if e1 == "__NOT_A_HEADLINE__" && e2 == "__NOT_A_HEADLINE__" => {
      headline1 == headline2  // Both are not headlines, compare directly
    },
    _ => false,  // One is headline, other is not, or they have different errors
  }
}

/// Compare two tree forests modulo ID differences.
/// Trees are considered the same if their structure and content match,
/// ignoring ID values (but not ID presence/absence).
pub fn compare_trees_modulo_id(
  trees1: &[Tree<OrgNode>],
  trees2: &[Tree<OrgNode>]
) -> bool {
  if trees1.len() != trees2.len() {
    return false;
  }

  for (tree1, tree2) in trees1.iter().zip(trees2.iter()) {
    if !compare_nodes_modulo_id(tree1.root(), tree2.root()) {
      return false;
    }
  }

  true
}

/// Compare two nodes and their subtrees modulo ID differences.
fn compare_nodes_modulo_id(
  node1: ego_tree::NodeRef<OrgNode>,
  node2: ego_tree::NodeRef<OrgNode>
) -> bool {
  let n1 : &OrgNode = node1.value();
  let n2 : &OrgNode = node2.value();

  // Compare structure and content
  if n1.title != n2.title ||
     n1.body != n2.body ||
     n1.metadata.treatment != n2.metadata.treatment ||
     n1.metadata.cycle != n2.metadata.cycle ||
     n1.metadata.focused != n2.metadata.focused ||
     n1.metadata.folded != n2.metadata.folded ||
     n1.metadata.mightContainMore != n2.metadata.mightContainMore ||
     n1.metadata.repeat != n2.metadata.repeat ||
     n1.metadata.toDelete != n2.metadata.toDelete {
    return false;
  }

  // Compare ID presence/absence (but not values)
  match (&n1.metadata.id, &n2.metadata.id) {
    (Some(_), Some(_)) => {}, // Both have IDs, values don't matter
    (None, None) => {},       // Both lack IDs
    _ => return false,        // One has ID, other doesn't
  }

  // Compare children
  let children1: Vec<_> = node1.children().collect();
  let children2: Vec<_> = node2.children().collect();

  if children1.len() != children2.len() {
    return false;
  }

  for (child1, child2) in children1.iter().zip(children2.iter()) {
    if !compare_nodes_modulo_id(*child1, *child2) {
      return false;
    }
  }

  true
}

/// Remove ID from metadata struct while preserving other metadata
fn strip_id_from_metadata_struct(
  metadata: Option<OrgnodeMetadata>
) -> Option<OrgnodeMetadata> {
  metadata.map(|mut meta| {
    meta.id = None;
    meta
  })
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_compare_headlines_modulo_id() {
    // Test identical headlines
    assert!(compare_headlines_modulo_id(
      "* Title",
      "* Title"
    ));

    // Test headlines that differ only by ID
    assert!(compare_headlines_modulo_id(
      "* (skg (id abc123)) Title",
      "* (skg (id xyz789)) Title"
    ));

    // Test headlines where one has ID and other doesn't - should be unequal
    assert!(!compare_headlines_modulo_id(
      "* (skg (id abc123)) Title",
      "* Title"
    ));

    // Test headlines with same other metadata but different IDs
    assert!(compare_headlines_modulo_id(
      "* (skg (id abc) (treatment content)) Title",
      "* (skg (id xyz) (treatment content)) Title"
    ));

    // Test headlines that differ by title
    assert!(!compare_headlines_modulo_id(
      "* (skg (id abc)) Title One",
      "* (skg (id xyz)) Title Two"
    ));

    // Test headlines that differ by level
    assert!(!compare_headlines_modulo_id(
      "* (skg (id abc)) Title",
      "** (skg (id xyz)) Title"
    ));

    // Test headlines that differ by other metadata
    assert!(!compare_headlines_modulo_id(
      "* (skg (id abc) (treatment content)) Title",
      "* (skg (id xyz) (treatment alias)) Title"
    ));

    // Test non-headlines
    assert!(compare_headlines_modulo_id(
      "This is body text",
      "This is body text"
    ));

    assert!(!compare_headlines_modulo_id(
      "This is body text",
      "This is different text"
    ));

    // Test mixed (one headline, one not)
    assert!(!compare_headlines_modulo_id(
      "* Title",
      "Body text"
    ));
  }
}
