pub mod parse;

use crate::types::misc::{ID, SourceName};
use crate::types::skgnode::SkgNode;

use std::error::Error;
use std::fmt;
use std::fs;
use std::path::Path;
use walkdir::WalkDir;

//
// Types
//

pub struct ImportStats {
  pub files_read    : usize,
  pub nodes_written : usize,
  pub errors        : Vec<String>,
}

impl fmt::Display for ImportStats {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>,
  ) -> fmt::Result {
    write! (f, "Files read: {}, nodes written: {}, errors: {}",
            self . files_read,
            self . nodes_written,
            self . errors . len() ) }}

//
// Public API
//

pub fn import_org_roam_directory (
  org_dir    : &Path,
  output_dir : &Path,
  source     : &SourceName,
) -> Result<ImportStats, Box<dyn Error>> {
  fs::create_dir_all (output_dir)?;
  let mut stats : ImportStats = ImportStats {
    files_read    : 0,
    nodes_written : 0,
    errors        : Vec::new(), };
  for entry in WalkDir::new (org_dir)
    . into_iter()
    . filter_map (|e| e . ok() )
    . filter (|e| {
      e . path() . extension()
        . map_or (false, |ext| ext == "org") }) {
    let path : &Path = entry . path();
    stats . files_read += 1;
    let nodes : Vec<SkgNode> =
      parse::parse_org_file (path);
    for mut node in nodes {
      node . source = source . clone();
      match write_skgnode_to_dir (&node, output_dir) {
        Ok (()) => { stats . nodes_written += 1; }
        Err (e) => {
          let msg : String = format! (
            "Error writing node '{}' from {:?}: {}",
            node . title, path, e);
          stats . errors . push (msg); }}}}
  Ok (stats) }

//
// File writing
//

fn write_skgnode_to_dir (
  node       : &SkgNode,
  output_dir : &Path,
) -> Result<(), Box<dyn Error>> {
  let pid : &ID = node . primary_id()
    . map_err (|e| -> Box<dyn Error> { e . into() })?;
  let filename : String =
    format! ("{}.skg", &pid . 0);
  let path : std::path::PathBuf =
    output_dir . join (&filename);
  let yaml : String = serde_yaml::to_string (node)?;
  fs::write (&path, &yaml)?;
  Ok (( )) }
