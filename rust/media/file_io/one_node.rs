use crate::types::misc::{ID, SkgConfig, SourceNickname};
use crate::types::skgnode::SkgNode;
use crate::media::typedb::util::pid_and_source_from_id;
use crate::util::path_from_pid_and_source;
use std::error::Error;
use std::io;
use std::path::Path;
use std::fs;
use serde_yaml;
use typedb_driver::TypeDBDriver;

pub async fn skgnode_and_source_from_id (
  config : &SkgConfig,
  driver : &TypeDBDriver,
  skg_id : &ID
) -> Result<(SkgNode, SourceNickname), Box<dyn Error>> {

  let (pid, source) : (ID, String) =
    pid_and_source_from_id (
      & config.db_name, driver, skg_id
  ). await ?
    . ok_or_else ( || format! (
      "ID '{}' not found in database", skg_id ) ) ?;
  let node_file_path : String =
    path_from_pid_and_source (
      config, &source, pid );
  let mut skgnode : SkgNode =
    read_skgnode (node_file_path) ?;
  skgnode.source = source.clone();
  Ok ((skgnode, SourceNickname::from(source))) }

/// Reads a node from disk, returning None if not found
/// (either in DB or on filesystem).
/// Other errors are propagated.
pub async fn skgnode_and_source_from_id_optional(
  config : &SkgConfig,
  driver : &TypeDBDriver,
  skg_id : &ID
) -> Result<Option<(SkgNode, SourceNickname)>, Box<dyn Error>> {
  match skgnode_and_source_from_id(config, driver, skg_id).await {
    Ok((skgnode, source)) => Ok(Some((skgnode, source))),
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
  skg_id : ID,
) -> Vec<String> {
  match pid_and_source_from_id (
    // Query TypeDB to get PID and source
    &config.db_name, driver, &skg_id
  ). await {
    Ok ( Some (( pid, source )) ) => {
      let file_path : String =
        path_from_pid_and_source (
          config, &source, pid );
      match read_skgnode ( &Path::new ( &file_path )) {
        Ok ( mut skgnode ) => {
          skgnode.source = source;
          skgnode.aliases.unwrap_or_default() },
        Err ( _ )   => Vec::new(), }},
    _ => Vec::new(), }}

pub fn read_skgnode
  <P : AsRef <Path>> // any type that can be converted to an &Path
  (file_path : P)
   -> io::Result <SkgNode>
{ // The type signature explains everything.

  let file_path : &Path = file_path.as_ref ();
  let contents  : String = fs::read_to_string ( file_path )?;
  let skgnode   : SkgNode =
    serde_yaml::from_str ( &contents )
    . map_err (
      |e| io::Error::new (
        io::ErrorKind::InvalidData,
        e.to_string () )) ?;
  if skgnode.title.is_empty() {
    return Err(io::Error::new(
      io::ErrorKind::InvalidData,
      format!("SkgNode at {:?} has an empty title", file_path),
    )); }
  if skgnode.ids.is_empty() {
    return Err(io::Error::new(
      io::ErrorKind::InvalidData,
      format!(".skg file at {:?} has no IDs", file_path),
    )); }
  Ok ( skgnode ) }

pub fn write_skgnode
  <P : AsRef<Path>>
  ( skgnode   : &SkgNode,
    file_path : P
  ) -> io::Result<()>
{ // Writes `skgnode` to `path`.

  let yaml_string : String =
    serde_yaml::to_string ( skgnode )
    . map_err (
      |e| io::Error::new(
        io::ErrorKind::InvalidData,
        e.to_string () )) ?;
  fs::write ( file_path, yaml_string )?;
  Ok (( )) }
