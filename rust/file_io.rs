// This code is tested by /tests/file_io.rs

use serde_yaml;
use std::fs::{self};
use std::io::{self};
use std::path::{Path};

use crate::types::FileNode;
use crate::hyperlinks::extract_hyperlinks;

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

    // Get hyperlinks from titles and body
    let mut hyperlinks = Vec::new();
    for title in &filenode.titles {
      hyperlinks.extend(extract_hyperlinks(title)); }
    if let Some(text) = &filenode.body {
      // Ignores the None case
      hyperlinks.extend(extract_hyperlinks(text)); }
    filenode.hyperlinks = hyperlinks;
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
  fn test_hyperlinks_extracted_during_read() -> io::Result<()> {
    // Create a temporary directory
    let dir = tempdir()?;
    let file_path = dir.path().join("test_node.skg");

    // Create a test node with hyperlinks in titles and body
    let test_node = FileNode {
      titles: vec![
        "Title with hyperlink [[id:hyperlink1][First Hyperlink]]"
          .to_string(),
        "Another title [[id:hyperlink2][Second Hyperlink]]"
          .to_string(),
      ],
      ids: vec![ID::new("test123")],
      body: Some("Some text with a link [[id:hyperlink3][Third Hyperlink]] and another [[id:hyperlink4][Fourth Hyperlink]]".to_string()),
      no_tantivy_index: false,
      contains: vec![],
      subscribes_to: vec![],
      ignores: vec![],
      replaces_view_of: vec![],
      hyperlinks: vec![],
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

    // Verify hyperlinks were extracted correctly
    assert_eq!(read_node.hyperlinks.len(), 4);

    // Check hyperlinks from titles
    assert!(read_node.hyperlinks.iter()
            .any ( |hyperlink| hyperlink.id == "hyperlink1".into() &&
                    hyperlink.label == "First Hyperlink"));
    assert!(read_node.hyperlinks.iter()
            .any ( |hyperlink| hyperlink.id == "hyperlink2".into() &&
                    hyperlink.label == "Second Hyperlink"));
    assert!(read_node.hyperlinks.iter()
            .any ( |hyperlink| hyperlink.id == "hyperlink3".into() &&
                    hyperlink.label == "Third Hyperlink"));
    assert!(read_node.hyperlinks.iter()
            .any ( |hyperlink| hyperlink.id == "hyperlink4".into()
                    && hyperlink.label == "Fourth Hyperlink"));
    Ok (()) } }
