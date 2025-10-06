// PURPOSE & SUMMARY:
// Reads and writes the SkgNode type on disk.
// As noted in types.rs, there is a 1-to-1 correspondence
// between the SkgNode type and each (path, file) pair,
// which makes this pretty simple.

use serde_yaml;
use std::fs::{self};
use std::io;
use std::path::{Path, PathBuf};

use crate::types::{ ID, SkgNode, SkgConfig, SaveInstruction };
use crate::typedb::search::pid_from_id;

use std::error::Error;
use typedb_driver::TypeDBDriver;


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
  { // Check that all files exist.
    let mut missing_files : Vec<String> = Vec::new();
    for node in &nodes {
      let pid : ID = node . ids . get(0)
        . ok_or_else (
          || io::Error::new (
            io::ErrorKind::InvalidInput,
            "SkgNode has no IDs" ))?
        . clone ();
      let file_path = path_from_pid (
        & config, pid );
      if ! Path::new ( & file_path ) . exists () {
        missing_files.push ( file_path ); }}
    if ! missing_files . is_empty () {
      return Err ( io::Error::new (
        io::ErrorKind::NotFound,
        format! ( "Files do not exist: {:?}",
                   missing_files ) )); }}

  let mut deleted : usize = 0;
  for node in nodes {
    let pid : ID = node . ids . get(0)
      . unwrap () // Safe because we checked above
      . clone ();
    let file_path = path_from_pid (
      & config, pid );
    fs::remove_file ( & file_path ) ?;
    deleted += 1; }
  Ok (deleted) }

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