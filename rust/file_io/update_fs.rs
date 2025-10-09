use crate::types::{ID, SkgNode, SkgConfig, SaveInstruction};
use crate::util::path_from_pid;
use crate::file_io::write_node;
use std::io;
use std::path::Path;
use std::fs;

pub fn update_fs_from_saveinstructions (
  instructions : Vec<SaveInstruction>,
  config       : SkgConfig,
) -> io::Result<(usize, usize)> { // (deleted, written)

  let ( to_delete, to_write ) : ( Vec<_>, Vec<_> ) =
    instructions . into_iter ()
    . partition ( |(_, action)| action . toDelete );

  let delete_nodes : Vec<SkgNode> =
    to_delete . into_iter ()
    . map ( |(node, _)| node )
    . collect ();

  let write_nodes : Vec<SkgNode> =
    to_write . into_iter ()
    . map ( |(node, _)| node )
    . collect ();

  let deleted = if ! delete_nodes . is_empty () {
    delete_all_nodes_from_fs (
      delete_nodes, config . clone () ) ?
  } else { 0 };

  let written = if ! write_nodes . is_empty () {
    write_all_nodes_to_fs (
      write_nodes, config ) ?
  } else { 0 };

  Ok ( (deleted, written) ) }

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
    let file_path = path_from_pid (
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
