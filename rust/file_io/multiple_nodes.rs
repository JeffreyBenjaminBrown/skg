use crate::file_io::one_node::{read_node, write_node};
use crate::types::misc::{SkgConfig, ID};
use crate::types::skgnode::SkgNode;
use crate::util::path_from_pid;

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
    { let node = read_node (&path) ?;
      nodes.push (node); }}
  Ok (nodes) }

/// Writes all given `SkgNode`s to disk, at `config.skg_folder`,
/// using the primary ID as the filename, followed by `.skg`.
pub fn write_all_nodes_to_fs (
  nodes  : Vec<SkgNode>,
  config : SkgConfig,
) -> io  ::Result<usize> { // number of files written

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
