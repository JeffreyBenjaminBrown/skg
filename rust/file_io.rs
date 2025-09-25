// PURPOSE & SUMMARY:
// Reads and writes the SkgNode type on disk.
// As noted in types.rs, there is a 1-to-1 correspondence
// between the SkgNode type and each (path, file) pair,
// which makes this pretty simple.

use serde_yaml;
use std::fs::{self};
use std::io;
use std::path::{Path, PathBuf};

use crate::types::{ ID, SkgNode, SkgConfig };
use crate::typedb::search::pid_from_id;

use std::error::Error;
use typedb_driver::TypeDBDriver;


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
   -> io::Result <SkgNode>
{ // The type signature explains everything.

 let file_path : &Path = file_path.as_ref ();
 let contents  : String = fs::read_to_string ( file_path )?;
 let node      : SkgNode =
    serde_yaml::from_str ( &contents )
    . map_err (
      |e| io::Error::new (
        io::ErrorKind::InvalidData,
        e.to_string () )) ?;
  Ok ( node ) }

pub async fn read_node_from_id (
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
  node_id : &ID
) -> Result<SkgNode, Box<dyn Error>> {

  let node_file_path = path_from_pid (
    config,
    pid_from_id (
      & config.db_name, driver, node_id
    ). await ? );
  let node : SkgNode = read_node (node_file_path) ?;
  if node.title.is_empty() {
    return Err(Box::new(std::io::Error::new(
      std::io::ErrorKind::InvalidData,
      format!("SkgNode with ID {} has an empty title", node_id),
    )) ); }
  if node.ids.is_empty() {
    return Err(Box::new(std::io::Error::new(
      std::io::ErrorKind::InvalidData,
      format!("SkgNode with ID {} has no IDs", node_id),
    )) ); }
  Ok (node) }

pub fn read_skg_files
  <P : AsRef<Path> > (
    dir_path : P )
  -> io::Result < Vec<SkgNode> >
{ // Reads all relevant files from the path.

  let mut nodes : Vec<SkgNode> = Vec::new ();
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

/// Writes all given `SkgNode`s to disk, at `config.skg_folder`,
/// using the primary ID as the filename, followed by `.skg`.
pub fn write_all_nodes (
  nodes : Vec<SkgNode>,
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
           "SkgNode has no IDs" ))?
      . clone ();
    write_node (
      & node,
      & Path::new (
        & path_from_pid (
          & config, pid )) ) ?;
    written += 1; }
  Ok (written) }

/// If there's no such .skg file at path,
/// returns the empty vector.
pub fn fetch_aliases_from_file (
  config : &SkgConfig,
  id     : ID,
) -> Vec<String> {
  let file_path : String =
    path_from_pid ( config, id );
  match read_node
    ( &Path::new ( &file_path )) {
      Ok ( node ) => node.aliases
        . unwrap_or_default (), // extract from Option
      Err ( _ )   => Vec::new(),
    }}

pub fn write_node
  <P : AsRef<Path>>
  ( node      : &SkgNode,
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
