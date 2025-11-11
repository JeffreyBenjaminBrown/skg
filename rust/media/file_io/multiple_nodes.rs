use crate::media::file_io::one_node::{read_node, write_node};
use crate::types::misc::{SkgConfig, SkgfileSource, ID};
use crate::types::skgnode::SkgNode;
use crate::util::path_from_pid;

use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};
use std::fs::{self, DirEntry, ReadDir};

pub fn read_skg_files
  <P : AsRef<Path> > (
    dir_path : P )
  -> io::Result < Vec<SkgNode> >
{ // Reads all relevant files from the path.

  let mut nodes : Vec<SkgNode> = Vec::new ();
  let entries : ReadDir = // an iterator
    fs::read_dir (dir_path) ?;
  for entry in entries {
    let entry : DirEntry = entry ?;
    let path : PathBuf = entry.path () ;
    if ( path.is_file () &&
         path . extension () . map_or (
           false,                // None => no extension found
           |ext| ext == "skg") ) // Some
    { // TODO Phase 5: Determine source from dir_path instead of hardcoding "main"
      let mut node = read_node (&path) ?;
      node.source = "main".to_string();
      nodes.push (node); }}
  Ok (nodes) }

/// Reads all .skg files from all configured sources.
/// Sets each node's source field to the appropriate source nickname.
/// Detects duplicate IDs across sources and returns an error if found.
pub fn read_all_skg_files_from_sources (
  sources: &HashMap<String, SkgfileSource>
) -> io::Result<Vec<SkgNode>> {
  let mut all_nodes: Vec<SkgNode> = Vec::new();
  let mut seen_ids: HashMap < String,  // ID
                              String > // source nickname
    // To verify no ID is in more than one source
    = HashMap::new();

  for (nickname, source) in sources.iter() {
    let path_str: &str = source.path.to_str()
      . ok_or_else( || io::Error::new(
        io::ErrorKind::InvalidInput,
        format!("Invalid UTF-8 in source '{}' path",
                nickname)) )?;
    let mut nodes: Vec<SkgNode> =
      read_skg_files (path_str)?;

    for node in &mut nodes {
      node.source = nickname.clone(); // because source is not serialized to the .skg file; it is instead inferred from that file's path
      for id in &node.ids { // check for duplicate IDs across sources
        let id_str = id.as_str();
        if let Some(existing_source) = seen_ids.get(id_str) {
          return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
              "Duplicate ID '{}' found in sources '{}' and '{}'",
              id_str, existing_source, nickname)) ); }
        seen_ids.insert ( id_str.to_string(),
                          nickname.clone() ); }}
    all_nodes.append (&mut nodes); }
  Ok (all_nodes) }

/// Writes all given `SkgNode`s to disk, at `config.skg_folder`,
/// using the primary ID as the filename, followed by `.skg`.
pub fn write_all_nodes_to_fs (
  nodes  : Vec<SkgNode>,
  config : SkgConfig,
) -> io  ::Result<usize> { // number of files written

  // TODO Phase 5: Write to appropriate source based on node.source
  // For now, use "main" source to get Phase 1 compiling
  let main_source =
    config . sources . get ( "main" )
    . expect ( "Config must have a 'main' source" );
  fs::create_dir_all (
    // Ensure entire path to folder exists
    &main_source . path )?;
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

pub fn delete_all_nodes_from_fs (
  nodes  : Vec<SkgNode>,
  config : SkgConfig,
) -> io::Result<usize> { // number of files deleted

  let mut deleted : usize = 0;
  for node in nodes {
    let pid : ID = node . ids . get(0)
      . unwrap () // Safe because we checked above
      . clone ();
    let file_path : String =
      path_from_pid (
      & config, pid );
    match fs::remove_file ( & file_path ) {
      Ok ( () ) => {
        deleted += 1; },
      Err ( e ) if e.kind () == io::ErrorKind::NotFound => {
        // File doesn't exist, which is fine.
        // The user created something and deleted it in the same pass.
      },
      Err ( e ) => {
        // TODO : Should return a list of IDs not found.
        return Err ( e ); }} }
  Ok (deleted) }
