use crate::types::misc::{ID, SkgConfig};
use crate::types::skgnode::SkgNode;
use crate::dbs::typedb::search::pid_and_source_from_id;
use crate::util::path_from_pid_and_source;
use std::error::Error;
use std::io;
use std::path::Path;
use std::fs;
use serde_yaml;
use typedb_driver::TypeDBDriver;

pub async fn skgnode_from_id (
  config : &SkgConfig,
  driver : &TypeDBDriver,
  skgid  : &ID
) -> Result<SkgNode, Box<dyn Error>> {
  let (pid, source) : (ID, String) =
    pid_and_source_from_id (
      & config.db_name, driver, skgid
    ). await ?
    . ok_or_else ( || format! (
      "ID '{}' not found in database", skgid ) ) ?;
  Ok ( skgnode_from_pid_and_source (
    config, pid, &source )? ) }

/// Reads a SkgNode from disk given its PID and source.
pub fn skgnode_from_pid_and_source (
  config : &SkgConfig,
  pid    : ID,
  source : &str,
) -> io::Result<SkgNode> {
  let path : String =
    path_from_pid_and_source( config, source, pid );
  let mut skgnode : SkgNode =
    read_skgnode( path )?;
  skgnode.source = // needed because it's not serialized
    source.to_string();
  Ok ( skgnode ) }

/// Reads a node from disk, returning None if not found
/// (either in DB or on filesystem).
/// Other errors are propagated.
pub async fn optskgnode_from_id (
  config : &SkgConfig,
  driver : &TypeDBDriver,
  skgid  : &ID
) -> Result<Option<SkgNode>, Box<dyn Error>> {
  match skgnode_from_id(
    config, driver, skgid
  ). await {
    Ok(skgnode) => Ok(Some(skgnode)),
    Err(e)      => {
      let error_msg: String = e.to_string();
      if error_msg.contains("not found")
        || error_msg.contains("No such file")
        || error_msg.contains("does not exist") {
          // TODO : This is kludgey. Find a better way to test for this kind of error.
          Ok (None) }
      else { Err(e) }} }}

/// If there's no such .skg file at path,
/// returns the empty vector.
pub async fn fetch_aliases_from_file (
  config : &SkgConfig,
  driver : &TypeDBDriver,
  skgid  : ID,
) -> Vec<String> {
  match optskgnode_from_id(
    config, driver, &skgid
  ). await {
    Ok ( Some ( skgnode )) =>
      skgnode.aliases.unwrap_or_default(),
    _ => Vec::new(), }}

pub fn write_skgnode_to_source (
  skgnode : &SkgNode,
  config  : &SkgConfig,
) -> io::Result<()> {
  let pid : &ID = skgnode.ids.get(0)
    .ok_or_else(|| io::Error::new(
      io::ErrorKind::InvalidInput,
      "SkgNode has no IDs"))?;
  let path : String =
    path_from_pid_and_source( config, &skgnode.source, pid.clone() );
  write_skgnode( skgnode, &path ) }

/// Effectively private.
pub(super) fn read_skgnode
  <P : AsRef <Path>> // any type that can be converted to an &Path
  (file_path : P
  ) -> io::Result <SkgNode> {

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

/// Effectively private.
pub(super) fn write_skgnode
  <P : AsRef<Path>>
  ( skgnode   : &SkgNode,
    file_path : P
  ) -> io::Result<()> {

  let yaml_string : String =
    serde_yaml::to_string ( skgnode )
    . map_err (
      |e| io::Error::new(
        io::ErrorKind::InvalidData,
        e.to_string () )) ?;
  fs::write ( file_path, yaml_string )?;
  Ok (( )) }
