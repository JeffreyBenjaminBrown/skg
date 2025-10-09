use crate::types::{ID, SkgNode, SkgConfig};
use crate::typedb::search::pid_from_id;
use crate::util::path_from_pid;
use std::error::Error;
use std::io;
use std::path::Path;
use std::fs;
use serde_yaml;
use typedb_driver::TypeDBDriver;

pub async fn read_node_from_id (
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
  node_id : &ID
) -> Result<SkgNode, Box<dyn Error>> {

  let pid : ID = pid_from_id (
    & config.db_name, driver, node_id
  ). await ?
    . ok_or_else ( || format! (
      "ID '{}' not found in database", node_id ) ) ?;
  let node_file_path = path_from_pid (
    config, pid );
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
