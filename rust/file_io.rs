// This code is tested by /tests/file_io.rs

use serde_yaml;
use std::fs::{self};
use std::io::{self};
use std::path::{Path};

use crate::types::FileNode;

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
  use tempfile::tempdir;
  use crate::types::{ID, FileNode};

  #[test]
  fn test_hyperlinks_extracted_during_read() -> io::Result<()> {
    // Create a temporary directory
    let dir = tempdir()?;
    let file_path = dir.path().join("test_node.skg");

    // Create a test node with hyperlinks in titles and body
    let test_node = FileNode {
      title: "Title with two hyperlinks: [[id:hyperlink1][First Hyperlink]] and [[id:hyperlink2][Second Hyperlink]]"
        .to_string(),
      ids: vec![ID::new("test123")],
      body: Some("Some text with a link [[id:hyperlink3][Third Hyperlink]] and another [[id:hyperlink4][Fourth Hyperlink]]".to_string()),
      contains: vec![],
      subscribes_to: vec![],
      hides_from_its_subscriptions: vec![],
      overrides_view_of_of: vec![],
      path: file_path.clone(),
    };

    // Write the node to a file
    let yaml = serde_yaml::to_string(&test_node)
      .map_err (
        |e| io::Error::new(
          io::ErrorKind::InvalidData,
          e.to_string()))?;
    let mut file = File::create( &file_path )?;
    file.write_all(yaml.as_bytes())?;

    // Read the node back from the file
    let read_node = read_filenode(&file_path)?;

    assert_eq!( test_node, read_node,
                "Nodes should have matched." );
    Ok (( )) }}
