use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::nodes::fs::NodeFS;
use crate::types::nodes::complete::NodeComplete;
use crate::dbs::typedb::search::pid_and_source_from_id;
use crate::util::path_from_pid_and_source;
use std::error::Error;
use std::io;
use std::path::Path;
use std::fs;
use serde_yaml;
use typedb_driver::TypeDBDriver;

pub async fn nodecomplete_from_id (
  config : &SkgConfig,
  driver : &TypeDBDriver,
  skgid  : &ID
) -> Result<NodeComplete, Box<dyn Error>> {
  let (pid, source) : (ID, SourceName) =
    pid_and_source_from_id (
      & config . db_name, driver, skgid
    ) . await ?
    . ok_or_else ( || format! (
      "ID '{}' not found in database", skgid ) ) ?;
  Ok ( nodecomplete_from_pid_and_source (
    config, pid, &source )? ) }


/// Reads a NodeComplete from disk given its PID and source.
pub fn nodecomplete_from_pid_and_source (
  config : &SkgConfig,
  pid    : ID,
  source : &SourceName,
) -> io::Result<NodeComplete> {
  let node_fs : NodeFS = {
    let path : String =
      path_from_pid_and_source( config, source, pid )
      . map_err ( |e| io::Error::new (
        io::ErrorKind::NotFound, e) ) ?;
    read_nodecomplete (path)? };
  Ok ( node_fs . into_complete ( source . clone () )) }

/// Reads a node from disk, returning None if not found
/// (either in DB or on filesystem).
/// ERRORS are propagated only if they are not of the 'not found' kind.
pub async fn optnodecomplete_from_id (
  config : &SkgConfig,
  driver : &TypeDBDriver,
  skgid  : &ID
) -> Result<Option<NodeComplete>, Box<dyn Error>> {
  match nodecomplete_from_id(
    config, driver, skgid
  ) . await {
    Ok (nodecomplete) => Ok(Some (nodecomplete)),
    Err (e)      => {
      let error_msg: String = e . to_string();
      if error_msg . contains ("not found")
        || error_msg . contains ("No such file")
        || error_msg . contains ("does not exist") {
          // TODO : This is kludgey. Find a better way to test for this kind of error.
          Ok (None) }
      else { Err (e) }} }}

/// If there's no such .skg file at path,
/// returns the empty vector.
pub async fn fetch_aliases_from_file (
  config : &SkgConfig,
  driver : &TypeDBDriver,
  skgid  : ID,
) -> Vec<String> {
  match optnodecomplete_from_id(
    config, driver, &skgid
  ) . await {
    Ok ( Some (nodecomplete)) =>
      nodecomplete . aliases . into_vec(),
    _ => Vec::new(), }}

pub fn write_nodecomplete_to_source (
  nodecomplete : &NodeComplete,
  config  : &SkgConfig,
) -> io::Result<()> {
  let pid : &ID = &nodecomplete . pid;
  let path : String =
    path_from_pid_and_source(
      config, &nodecomplete . source, pid . clone() )
    . map_err ( |e| io::Error::new (
      io::ErrorKind::NotFound, e) ) ?;
  write_nodecomplete ( nodecomplete, &path ) }


/// Checks that a node's primary ID matches the filename stem.
/// This property is assumed by `path_from_pid_and_source` and
/// elsewhere but was never validated on read.
pub(super) fn validate_pid_matches_filename (
  node : &NodeFS,
  path : &Path,
) -> io::Result<()> {
  let pid : &ID = &node . pid;
  let stem : &str = path . file_stem()
    . and_then ( |s| s . to_str() )
    . ok_or_else ( || io::Error::new (
      io::ErrorKind::InvalidData,
      format! ("Cannot extract filename stem from {:?}",
               path )) ) ?;
  if pid . as_str() != stem {
    return Err ( io::Error::new (
      io::ErrorKind::InvalidData,
      format! (
        "PID '{}' does not match filename stem '{}' in {:?}",
        pid . as_str(), stem, path )) ); }
  Ok (( )) }

/// Effectively private.
///
/// Returns a NodeFS (on-disk shape, no source). Callers attach
/// source via 'NodeFS::into_complete' based on file location.
pub(super) fn read_nodecomplete
  <P : AsRef <Path>> // any type that can be converted to an &Path
  (file_path : P
  ) -> io::Result <NodeFS> {

  let file_path : &Path = file_path . as_ref ();
  let node_fs   : NodeFS = {
    let contents : String = fs::read_to_string (file_path)?;
    serde_yaml::from_str (&contents)
    . map_err (
      |e| io::Error::new (
        io::ErrorKind::InvalidData,
        e . to_string () )) ? };
  if node_fs . title . is_empty() {
    return Err(io::Error::new(
      io::ErrorKind::InvalidData,
      format!("NodeComplete at {:?} has an empty title", file_path),
    )); }
  if node_fs . pid . as_str() . is_empty() {
    return Err(io::Error::new(
      io::ErrorKind::InvalidData,
      format!(".skg file at {:?} has no IDs", file_path),
    )); }
  Ok (node_fs) }

/// Effectively private.
///
/// Converts to 'NodeFS' (drops source) before serializing, so
/// source is not written to the YAML on disk.
pub(super) fn write_nodecomplete
  <P : AsRef<Path>>
  ( nodecomplete   : &NodeComplete,
    file_path : P
  ) -> io::Result<()> {
    let node_fs : NodeFS = NodeFS::from (nodecomplete);
    fs::write ( file_path,
                {
                  let yaml_string : String =
                    node_fs . to_yaml ()
                    . map_err (
                      |e| io::Error::new(
                        io::ErrorKind::InvalidData,
                        e . to_string () )) ?;
                  yaml_string } )?;
    Ok (( )) }
