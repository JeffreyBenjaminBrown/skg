pub mod parse;

use crate::types::misc::{ID, SourceName};
use crate::types::skgnode::{FileProperty, SkgNode};

use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};
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
  for entry in fs::read_dir (output_dir)? { // Wipe existing .skg files to avoid orphans from previous runs.
    let entry : fs::DirEntry = entry?;
    let path : PathBuf = entry . path();
    if path . extension() . map_or (false, |e| e == "skg") {
      fs::remove_file (&path)?; }}
  let mut stats : ImportStats = ImportStats {
    files_read    : 0,
    nodes_written : 0,
    errors        : Vec::new(), };
  // Collect all nodes into a map keyed by primary ID.
  // When multiple org files define the same ID,
  // merge their children rather than clobbering.
  let mut node_map : HashMap<ID, SkgNode> = HashMap::new();
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
      { let pid : ID = node . pid . clone();
        if let Some (existing) = node_map . get_mut (&pid) {
          merge_into_existing (existing, &node);
          tracing::warn! (
            id = %pid,
            "Overloaded ID — multiple org-roam nodes use this ID. \
             Merging their content."); }
        else {
          node_map . insert (pid, node); }} }}
  for (_pid, node) in &node_map {
    match write_skgnode_to_dir (node, output_dir) {
      Ok (()) => { stats . nodes_written += 1; }
      Err (e) => {
        let msg : String = format! (
          "Error writing node '{}': {}",
          node . title, e);
        stats . errors . push (msg); }} }
  Ok (stats) }

//
// File writing
//

/// Merge a duplicate node into an existing one with the same ID.
/// Appends children, body text, and aliases from the newcomer.
/// Tags the result with Was_Overloaded.
fn merge_into_existing (
  existing : &mut SkgNode,
  newcomer : &SkgNode,
) {
  if ! existing . misc . contains (&FileProperty::Was_Overloaded) {
    existing . misc . push (FileProperty::Was_Overloaded); }
  { // Merge contents.
    for child in &newcomer . contains {
      if ! existing . contains . contains (child) {
        existing . contains . push (child . clone()); }} }
  { // Append the newcomer's title and body into the existing body,
    // separated by an informative marker.
    let separator : &str =
      "\n\n====== imported from a distinct org-roam node with the same ID ======";
    let mut appendage : String = String::new();
    appendage . push_str (separator);
    appendage . push_str (&format! ("\ntitle: {}", newcomer . title));
    if let Some (new_body) = &newcomer . body {
      appendage . push_str (&format! ("\nbody: {}", new_body)); }
    let body : &mut String =
      existing . body . get_or_insert_with (String::new);
    body . push_str (&appendage); }
  { // Merge aliases.
    let new_aliases : &[String] = newcomer . aliases . or_default();
    if ! new_aliases . is_empty() {
      let merged : &mut Vec<String> =
        existing . aliases . ensure_specified();
      for alias in new_aliases {
        if ! merged . contains (alias) {
          merged . push (alias . clone()); }} } }
  if newcomer . misc . contains (&FileProperty::Had_ID_Before_Import)
    && ! existing . misc . contains (&FileProperty::Had_ID_Before_Import)
    { // Preserve Had_ID_Before_Import from either side.
      existing . misc . push (FileProperty::Had_ID_Before_Import); }}

fn write_skgnode_to_dir (
  node       : &SkgNode,
  output_dir : &Path,
) -> Result<(), Box<dyn Error>> {
  let pid : &ID = &node . pid;
  let filename : String =
    format! ("{}.skg", &pid . 0);
  let path : std::path::PathBuf =
    output_dir . join (&filename);
  let yaml : String = serde_yaml::to_string (node)?;
  fs::write (&path, &yaml)?;
  Ok (( )) }
