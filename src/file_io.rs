// This code is tested by /tests/file_io.rs

use std::fs::{self};
use std::io::{self};
use std::path::{Path};
use serde_json;

use crate::types::SkgNode;
use crate::links::extract_links;

pub fn read_skgnode_from_path
  <P: AsRef<Path>>
  (file_path: P)
   -> io::Result<SkgNode>
{ let file_path = file_path.as_ref();
  let contents = fs::read_to_string(file_path)?;
  let mut skgnode: SkgNode = serde_json::from_str(&contents)
  .map_err(
    |e| io::Error::new(
      io::ErrorKind::InvalidData, e.to_string()))?;
  skgnode.path = file_path.to_path_buf(); // not part of the JSON

  // Extract links from titles and unindexed text
  let mut links = Vec::new();
  for title in &skgnode.titles {
    links.extend ( extract_links ( title ) ); }
  links.extend(
    extract_links ( &skgnode.unindexed_text ) );
  skgnode.links = links;

  Ok (skgnode) }

/// A line in the typedef of SkgNode prevents the field `path`
/// from being part of the JSON representation.
pub fn write_skgnode_to_path
  <P: AsRef<Path>>
  (skgnode: &SkgNode, file_path: P)
   -> io::Result<()>
{ let json_string = serde_json::to_string(skgnode)
  .map_err(
    |e| io::Error::new(
      io::ErrorKind::InvalidData, e.to_string()))?;
  fs::write(file_path, json_string)?;
  Ok(()) }

#[cfg(test)]
mod tests {
  use super::*;
  use std::fs::File;
  use std::io::Write;
  use std::path::{PathBuf};
  use tempfile::tempdir;
  use crate::types::{ID, SkgNode};

  #[test]
  fn test_links_extracted_during_read() -> io::Result<()> {
    // Create a temporary directory
    let dir = tempdir()?;
    let file_path = dir.path().join("test_node.skg");

    // Create a test node with links in titles and unindexed_text
    let test_node = SkgNode {
      titles: vec![
        "Title with link [[id:link1][First Link]]".to_string(),
        "Another title [[id:link2][Second Link]]".to_string(),
      ],
      ids: vec![ID::new("test123")],
      unindexed_text: "Some text with a link [[id:link3][Third Link]] and another [[id:link4][Fourth Link]]".to_string(),
      properties: vec![],
      nodes_contained: vec![],
      nodes_subscribed: vec![],
      nodes_unsubscribed: vec![],
      links: vec![],
      path: PathBuf::new(),
    };

    // Write the node to a file
    let json = serde_json::to_string(&test_node)?;
    let mut file = File::create(&file_path)?;
    file.write_all(json.as_bytes())?;

    // Read the node back from the file
    let read_node = read_skgnode_from_path(&file_path)?;

    // Verify links were extracted correctly
    assert_eq!(read_node.links.len(), 4);

    // Check links from titles
    assert!(read_node.links.iter()
            .any ( |link| link.id == "link1" &&
                    link.label == "First Link"));
    assert!(read_node.links.iter()
            .any ( |link| link.id == "link2" &&
                    link.label == "Second Link"));
    assert!(read_node.links.iter()
            .any ( |link| link.id == "link3" &&
                    link.label == "Third Link"));
    assert!(read_node.links.iter()
            .any ( |link| link.id == "link4"
                    && link.label == "Fourth Link"));
    Ok (()) } }
