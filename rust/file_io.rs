// This code is tested by /tests/file_io.rs

use serde_yaml;
use std::fs::{self};
use std::io::{self};
use std::path::{Path};

use crate::types::FileNode;
use crate::links::extract_links;

pub fn read_filenode
  <P: AsRef<Path>>
  (file_path: P)
   -> io::Result<FileNode> {
    let file_path = file_path.as_ref();
    let contents = fs::read_to_string(file_path)?;
    let mut filenode: FileNode = serde_yaml::from_str(&contents)
      .map_err(
        |e| io::Error::new(
          io::ErrorKind::InvalidData, e.to_string()))?;

    // The rest of this information is not represented as a field
    // in the .skg file.

    filenode.path = file_path.to_path_buf();

    // Get links from titles and body
    let mut links = Vec::new();
    for title in &filenode.titles {
      links.extend(extract_links(title)); }
    if let Some(text) = &filenode.body {
      // Ignores the None case
      links.extend(extract_links(text)); }
    filenode.links = links;
    Ok (filenode) }

/// A line in the typedef of FileNode prevents the field `path`
/// from being part of the .skg representation.
pub fn write_filenode
  <P: AsRef<Path>>
  (filenode: &FileNode, file_path: P)
   -> io::Result<()> {
    let yaml_string = serde_yaml::to_string(filenode)
      .map_err(
        |e| io::Error::new(
          io::ErrorKind::InvalidData, e.to_string()))?;
    fs::write(file_path, yaml_string)?;
    Ok (()) }

#[cfg(test)]
mod tests {
  use super::*;
  use std::fs::File;
  use std::io::Write;
  use std::path::{PathBuf};
  use tempfile::tempdir;
  use crate::types::{ID, FileNode};

  #[test]
  fn test_links_extracted_during_read() -> io::Result<()> {
    // Create a temporary directory
    let dir = tempdir()?;
    let file_path = dir.path().join("test_node.skg");

    // Create a test node with links in titles and body
    let test_node = FileNode {
      titles: vec![
        "Title with link [[id:link1][First Link]]".to_string(),
        "Another title [[id:link2][Second Link]]".to_string(),
      ],
      ids: vec![ID::new("test123")],
      body: Some("Some text with a link [[id:link3][Third Link]] and another [[id:link4][Fourth Link]]".to_string()),
      no_tantivy_index: false,
      contains: vec![],
      subscribes_to: vec![],
      ignores: vec![],
      replaces_view_of: vec![],
      links: vec![],
      path: PathBuf::new(),
    };

    // Write the node to a file
    let yaml = serde_yaml::to_string(&test_node)
      .map_err (
        |e| io::Error::new(
          io::ErrorKind::InvalidData,
          e.to_string()))?;
    let mut file = File::create(&file_path)?;
    file.write_all(yaml.as_bytes())?;

    // Read the node back from the file
    let read_node = read_filenode(&file_path)?;

    // Verify links were extracted correctly
    assert_eq!(read_node.links.len(), 4);

    // Check links from titles
    assert!(read_node.links.iter()
            .any ( |link| link.id == "link1".into() &&
                    link.label == "First Link"));
    assert!(read_node.links.iter()
            .any ( |link| link.id == "link2".into() &&
                    link.label == "Second Link"));
    assert!(read_node.links.iter()
            .any ( |link| link.id == "link3".into() &&
                    link.label == "Third Link"));
    assert!(read_node.links.iter()
            .any ( |link| link.id == "link4".into()
                    && link.label == "Fourth Link"));
    Ok (()) } }
