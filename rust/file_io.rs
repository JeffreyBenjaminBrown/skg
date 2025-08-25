// PURPOSE & SUMMARY:
// Reads and writes the FileNode type on disk.
// As noted in types.rs, there is a 1-to-1 correspondence
// between the FileNode type and each (path, file) pair,
// which makes this pretty simple.

use serde_yaml;
use std::fs::{self};
use std::io::{self};
use std::path::{Path};

use crate::types::FileNode;

pub fn read_filenode
  <P : AsRef <Path>> // any type that can be converted to an &Path
  (file_path : P)
   -> io::Result <FileNode>
{ // The type signature explains everything.

  let file_path : &Path = file_path.as_ref ();
  let contents  : String = fs::read_to_string ( file_path )?;
  let filenode  : FileNode =
    serde_yaml::from_str ( &contents )
    . map_err (
      |e| io::Error::new (
        io::ErrorKind::InvalidData,
        e.to_string () )) ?;
  Ok ( filenode ) }

pub fn write_filenode
  <P : AsRef<Path>>
  ( filenode  : &FileNode,
    file_path : P)
    -> io::Result<()>
{ /* Writes `filenode` to `path`.
     A skip directive in the FileNode typedef
     keeps the field `path` out of the .skg representation. */

  let yaml_string =
    serde_yaml::to_string ( filenode )
    . map_err (
      |e| io::Error::new(
        io::ErrorKind::InvalidData,
        e.to_string () )) ?;
  fs::write ( file_path, yaml_string )?;
  Ok (( )) }

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
      overrides_view_of: vec![],
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
