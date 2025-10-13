use crate::types::{SkgNode, SkgConfig};
use crate::file_io::read_node;
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

pub fn load_config (
  path: &str )
  -> Result <SkgConfig,
             Box<dyn std::error::Error>> {

    if !Path::new(path).exists() {
      return Err(format!("Config file not found: {}",
                         path)
                 . into() ); }
    let contents: String = fs::read_to_string (path) ?;
    let config: SkgConfig = toml::from_str (&contents) ?;
    Ok (config) }
