use crate::types::misc::{ID, SkgConfig};
use crate::types::skgnode::SkgNode;
use crate::media::typedb::util::pid_and_source_from_id;
use crate::util::path_from_pid_and_source;
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

  let (pid, source) : (ID, String) = pid_and_source_from_id (
    & config.db_name, driver, node_id
  ). await ?
    . ok_or_else ( || format! (
      "ID '{}' not found in database", node_id ) ) ?;
  let node_file_path : String =
    path_from_pid_and_source ( config, &source, pid );
  let mut node : SkgNode =
    read_node (node_file_path) ?;
  node.source = source;
  Ok (node) }

/// Reads a node from disk, returning None if not found
/// (either in DB or on filesystem).
/// Other errors are propagated.
pub async fn read_node_from_id_optional(
  config: &SkgConfig,
  driver: &TypeDBDriver,
  node_id: &ID
) -> Result<Option<SkgNode>, Box<dyn Error>> {
  match read_node_from_id(config, driver, node_id).await {
    Ok(node) => Ok(Some(node)),
    Err(e)   => {
      let error_msg: String = e.to_string();
      // Check if this is a "not found" error
      if error_msg.contains("not found")
        || error_msg.contains("No such file")
        || error_msg.contains("does not exist") {
          Ok(None)
        } else {
          // Propagate other errors
          Err(e) }} }}

/// If there's no such .skg file at path,
/// returns the empty vector.
pub async fn fetch_aliases_from_file (
  config : &SkgConfig,
  driver : &TypeDBDriver,
  id     : ID,
) -> Vec<String> {
  match pid_and_source_from_id (
    // Query TypeDB to get PID and source
    &config.db_name, driver, &id
  ). await {
    Ok ( Some (( pid, source )) ) => {
      let file_path : String =
        path_from_pid_and_source (
          config, &source, pid );
      match read_node ( &Path::new ( &file_path )) {
        Ok ( mut node ) => {
          node.source = source;
          node.aliases.unwrap_or_default() },
        Err ( _ )   => Vec::new(), }},
    _ => Vec::new(), }}

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
  if node.title.is_empty() {
    return Err(io::Error::new(
      io::ErrorKind::InvalidData,
      format!("SkgNode at {:?} has an empty title", file_path),
    )); }
  if node.ids.is_empty() {
    return Err(io::Error::new(
      io::ErrorKind::InvalidData,
      format!(".skg file at {:?} has no IDs", file_path),
    )); }
  Ok ( node ) }

pub fn write_node
  <P : AsRef<Path>>
  ( node      : &SkgNode,
    file_path : P
  ) -> io::Result<()>
{ // Writes `node` to `path`.

  let yaml_string : String =
    serde_yaml::to_string ( node )
    . map_err (
      |e| io::Error::new(
        io::ErrorKind::InvalidData,
        e.to_string () )) ?;
  fs::write ( file_path, yaml_string )?;
  Ok (( )) }
