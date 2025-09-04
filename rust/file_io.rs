// PURPOSE & SUMMARY:
// Reads and writes the Node type on disk.
// As noted in types.rs, there is a 1-to-1 correspondence
// between the Node type and each (path, file) pair,
// which makes this pretty simple.

use serde_yaml;
use std::fs::{self};
use std::io;
use std::path::{Path, PathBuf};

use crate::types::{ ID, Node, SkgConfig };


pub fn path_from_pid (
  config : &SkgConfig,
  pid    : ID,
) -> String {
  let f : PathBuf = config . skg_folder . clone() ;
  let s: String = pid.0;
  f . join (s)
    . with_extension ("skg")
    . to_string_lossy ()
    . into_owned ()
}

pub fn read_node
  <P : AsRef <Path>> // any type that can be converted to an &Path
  (file_path : P)
   -> io::Result <Node>
{ // The type signature explains everything.

 let file_path : &Path = file_path.as_ref ();
 let contents  : String = fs::read_to_string ( file_path )?;
 let node      : Node =
    serde_yaml::from_str ( &contents )
    . map_err (
      |e| io::Error::new (
        io::ErrorKind::InvalidData,
        e.to_string () )) ?;
  Ok ( node ) }

pub fn read_skg_files
  <P : AsRef<Path> > (
    dir_path : P )
  -> io::Result < Vec<Node> >
{ // Reads all relevant files from the path.

  let mut nodes : Vec<Node> = Vec::new ();
  let entries : std::fs::ReadDir = // an iterator
    fs::read_dir (dir_path) ?;
  for entry in entries {
    let entry : std::fs::DirEntry = entry ?;
    let path = entry.path () ;
    if ( path.is_file () &&
         path . extension () . map_or (
           false,                // None => no extension found
           |ext| ext == "skg") ) // Some
    { let node = read_node (&path) ?;
      nodes.push (node); }}
  Ok (nodes) }

/// Writes all given `Node`s to disk, at `config.skg_folder`,
/// using the primary ID as the filename, followed by `.skg`.
pub fn write_all_nodes (
  nodes : Vec<Node>,
  config    : SkgConfig,
) -> io::Result<usize> { // number of files written

  fs::create_dir_all (
    // Ensure entire path to folder exists
    &config.skg_folder )?;
  let mut written : usize = 0;
  for node in nodes {
    let pid : ID = node . ids . get(0)
      . ok_or_else (
         || io::Error::new (
           io::ErrorKind::InvalidInput,
           "Node has no IDs" ))?
      . clone ();
    write_node (
      & node,
      & Path::new (
        & path_from_pid (
          & config, pid )) ) ?;
    written += 1; }
  Ok (written) }

pub fn write_node
  <P : AsRef<Path>>
  ( node      : &Node,
    file_path : P
  ) -> io::Result<()>
{ // Writes `node` to `path`.

  let yaml_string =
    serde_yaml::to_string ( node )
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
  use crate::types::{ID, Node};

  #[test]
  fn test_hyperlinks_extracted_during_read() -> io::Result<()> {
    // Create a temporary directory
    let dir = tempdir()?;
    let file_path = dir.path().join("test_node.skg");

    // Create a test node with hyperlinks in titles and body
    let test_node = Node {
      title: "Title with two hyperlinks: [[id:hyperlink1][First Hyperlink]] and [[id:hyperlink2][Second Hyperlink]]"
        .to_string(),
      aliases: vec![ "alias 1" . to_string(),
                     "alias 2" . to_string() ],
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
    let read_node = read_node(&file_path)?;

    assert_eq!( test_node, read_node,
                "Nodes should have matched." );
    Ok (( )) }}
