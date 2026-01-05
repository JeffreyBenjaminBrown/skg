use crate::dbs::filesystem::one_node::{read_skgnode, write_skgnode};
use crate::types::misc::{SkgConfig, SkgfileSource, ID};
use crate::types::skgnode::SkgNode;
use crate::util::path_from_pid_and_source;

use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};
use std::fs::{self, DirEntry, ReadDir};

/// Reads all .skg files from all configured sources.
/// Sets each node's source field to the appropriate source nickname.
/// Detects duplicate IDs across sources and collects all errors.
/// If errors are found, writes detailed error reports to org files.
pub fn read_all_skg_files_from_sources (
  sources: &HashMap<String, SkgfileSource>
) -> io::Result<Vec<SkgNode>> {
  let mut all_nodes: Vec<SkgNode> = Vec::new();
  let mut seen_ids: HashMap < String,       // ID
                              Vec<String> > // source nicknames
    // Maps each ID to all sources containing it
    = HashMap::new();
  let mut load_errors: Vec<(String, // source nickname
                            String, // filename
                            String)> // error message
    = Vec::new();

  for (nickname, source) in sources.iter() {
    // Load from all sources, collecting any errors
    let path_str: &str = match source.path.to_str() {
      Some(s) => s,
      None => {
        load_errors.push((
          nickname.clone(),
          source.path.display().to_string(),
          "Invalid UTF-8 in source path".to_string() ));
        continue; }};

    match read_skg_files_from_folder (path_str) {
      Ok(mut nodes) => {
        for node in &mut nodes {
          node.source = nickname.clone();
          for id in &node.ids {
            // Track which sources contain each ID
            let id_str: String = id.as_str().to_string();
            seen_ids.entry(id_str)
              .or_insert_with(Vec::new)
              .push(nickname.clone()); }}
        all_nodes.append(&mut nodes); }
      Err(e) => {
        load_errors.push((
          nickname.clone(),
          path_str.to_string(),
          e.to_string()
        )); }} }

  // Check for duplicate IDs (IDs in multiple sources)
  let mut duplicate_ids: HashMap<String, Vec<String>> = HashMap::new();
  for (id, sources_list) in seen_ids.iter() {
    if sources_list.len() > 1 {
      duplicate_ids.insert( id.clone(),
                            sources_list.clone()); }}

  // Report errors if any were found
  let has_duplicates: bool = !duplicate_ids.is_empty();
  let has_load_errors: bool = !load_errors.is_empty();
  if has_duplicates {
    report_duplicate_ids(&duplicate_ids)?; }
  if has_load_errors {
    report_load_errors(&load_errors)?; }
  if has_duplicates || has_load_errors {
    let mut err_parts: Vec<String> = Vec::new();
    if has_duplicates {
      if duplicate_ids.len() <= 10 {
        // Include details in error message for small numbers
        let ids_list: Vec<String> = duplicate_ids.keys()
          .map(|id| format!("'{}'", id))
          .collect();
        err_parts.push(format!("Duplicate ID(s) found: {}",
                               ids_list.join(", ")));
      } else {
        err_parts.push(format!("{} duplicate IDs found (see org file)",
                               duplicate_ids.len() )); }}
    if has_load_errors {
      err_parts.push(format!("{} unreadable file(s)",
                             load_errors.len() )); }
    return Err(io::Error::new(
      io::ErrorKind::InvalidData,
      err_parts.join("; ") )); }
  Ok(all_nodes) }

fn read_skg_files_from_folder
  <P : AsRef<Path> > (
    dir_path : P )
  -> io::Result < Vec<SkgNode> >
{ // Reads all relevant files from a single directory path.

  let mut nodes : Vec<SkgNode> = Vec::new ();
  let entries : ReadDir = // an iterator
    fs::read_dir (dir_path) ?;
  for entry in entries {
    let entry : DirEntry = entry ?;
    let path : PathBuf = entry.path () ;
    if ( path.is_file () &&
         path . extension () . map_or (
           false,                  // None => no extension found
           |ext| ext == "skg") ) { // Some
      let mut skgnode: SkgNode =
        // Placeholder - caller always overwrites with actual source
        read_skgnode (&path) ?;
      skgnode.source = String::new();
      nodes.push (skgnode); }}
  Ok (nodes) }

/// Reports duplicate IDs found across sources.
/// If ≤10 duplicates: reports to stderr only.
/// If >10 duplicates: writes to org file and reports count to stderr.
fn report_duplicate_ids(
  duplicates: &HashMap<String, // ID
                       Vec<String>> // sources
) -> io::Result<()> {
  let count: usize = duplicates.len();
  if count <= 10 {
    // Report to stderr only
    eprintln!("\nError: Found {} duplicate ID(s) across sources:",
              count);
    for (id, sources) in duplicates.iter() {
      eprintln!("  - ID '{}' in sources: {}",
                id, sources.join(", ")); }
  } else { // Write to org file
    let filename: &str = "initialization-error_duplicate-ids.org";
    let mut content: String = String::new();
    content.push_str("#+title: Duplicate IDs Across Sources\n");
    content.push_str("#+date: <generated at initialization>\n\n");
    content.push_str(
      &format!("Found {} duplicate IDs across sources.\n\n",
               count));

    // Sort IDs for deterministic output
    let mut sorted_ids: Vec<(&String, &Vec<String>)> =
      duplicates.iter().collect();
    sorted_ids.sort_by_key(|(id, _)| *id);

    for (id, sources) in sorted_ids {
      content.push_str(&format!("* {}\n", id));
      // Sort sources too
      let mut sorted_sources: Vec<String> = sources.clone();
      sorted_sources.sort();
      for source in sorted_sources {
        content.push_str(&format!("** {}\n", source)); }}
    fs::write(filename, content)?;
    eprintln!("\nError: Found {} duplicate ID(s) across sources.",
              count);
    eprintln!("Details written to: {}", filename); }
  Ok (( )) }

/// Reports file loading errors.
/// Always writes to org file and reports count to stderr.
fn report_load_errors(
  errors: &[(String, String, String)]
) -> io::Result<()> {
  let count: usize = errors.len();
  let filename: &str =
    "initialization-error_unreadable-skg-files.org";

  let mut content: String = String::new();
  content.push_str("#+title: Unreadable SKG Files\n");
  content.push_str("#+date: <generated at initialization>\n\n");
  content.push_str( &format!(
    "Found {} unreadable file(s).\n\n", count));

  // Sort errors by path for deterministic output
  let mut sorted_errors: Vec<(String, String, String)> =
    errors.to_vec();
  sorted_errors.sort_by(|a, b| a.1.cmp(&b.1));

  for (source, filename_or_path, error_msg) in sorted_errors {
    content.push_str(&format!("* {}\n", filename_or_path));
    content.push_str(&format!("** {}\n", source));
    content.push_str(&format!("*** Error: {}\n", error_msg));
  }

  fs::write(filename, content)?;
  eprintln!("\nError: Found {} unreadable file(s).", count);
  eprintln!("Details written to: {}", filename);

  Ok(())
}

/// Writes all given `SkgNode`s to disk, at `config.skg_folder`,
/// using the primary ID as the filename, followed by `.skg`.
pub fn write_all_nodes_to_fs (
  nodes  : Vec<SkgNode>,
  config : SkgConfig,
) -> io  ::Result<usize> { // number of files written

  // Collect unique source directories and ensure they exist
  use std::collections::HashSet;
  let unique_sources : HashSet<&str> =
    nodes . iter()
    . map( |node| node.source.as_str() )
    . collect();
  for source_name in unique_sources {
    let source_config: &SkgfileSource =
      config . sources . get ( source_name )
      . ok_or_else( || io::Error::new(
        io::ErrorKind::NotFound,
        format!("Source '{}' not found in config",
                source_name)) )?;
    fs::create_dir_all ( &source_config . path )?; }

  let mut written : usize = 0;
  for node in nodes {
    let pid : ID = node . ids . get(0)
      . ok_or_else (
         || io::Error::new (
           io::ErrorKind::InvalidInput,
           "SkgNode has no IDs" ))?
      . clone ();
    write_skgnode (
      & node,
      & Path::new (
        & path_from_pid_and_source (
          & config, & node.source, pid )) ) ?;
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
      path_from_pid_and_source (
      & config, & node.source, pid );
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
