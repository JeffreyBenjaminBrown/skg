use crate::dbs::filesystem::one_node::{read_nodecomplete, validate_pid_matches_filename, write_nodecomplete};
use crate::types::misc::{SkgConfig, SkgfileSource, ID, SourceName};
use crate::types::nodes::fs::NodeFS;
use crate::types::nodes::complete::NodeComplete;
use crate::util::path_from_pid_and_source;

use std::collections::{HashMap, HashSet};
use std::io;
use std::path::{Path, PathBuf};
use std::fs::{self, DirEntry, ReadDir};

/// Reads all .skg files from all configured sources.
/// Sets each node's source field to the appropriate source name.
/// Delegates duplicate-ID detection across sources to
/// 'check_for_duplicate_ids_across_sources'.
/// If errors of either kind are found, writes detailed reports
/// (to stderr for small counts, to org files for large ones)
/// and returns a summary error.
pub fn read_all_skg_files_from_sources_AND_check_for_dup_ids (
  config: &SkgConfig
) -> io::Result<Vec<NodeComplete>> {
  let mut all_nodes: Vec<NodeComplete> = Vec::new();
  let mut load_errors: Vec<(String, // source name
                            String, // filename
                            String)> // error message
    = Vec::new();
  for (source_name, source) in config . sources . iter() {
    match read_skg_files_from_folder (source_name, config) {
      Ok (mut nodes) => {
        all_nodes . append (&mut nodes); }
      Err (e) => {
        load_errors . push ((
          source_name . to_string(),
          source . path . display() . to_string(),
          e . to_string()
        )); }} }
  let mut err_parts: Vec<String> = Vec::new();
  // Duplicates are reported before load errors so that the order
  // of stderr output matches the order of the err_parts message.
  if let Err (e) =
    check_for_duplicate_ids_across_sources (&all_nodes)
  { err_parts . push (e . to_string()); }
  if ! load_errors . is_empty() {
    report_load_errors (&load_errors) ?;
    err_parts . push (format! (
      "{} unreadable file(s)", load_errors . len() )); }
  if ! err_parts . is_empty() {
    return Err (io::Error::new (
      io::ErrorKind::InvalidData,
      err_parts . join ("; ") )); }
  Ok (all_nodes) }

/// Examines each node's source and all_ids(); if any ID appears
/// in more than one source, writes a detailed report (to stderr
/// for ≤10 duplicates, to an org file otherwise) and returns a
/// summary error. Otherwise returns Ok.
pub fn check_for_duplicate_ids_across_sources (
  nodes : &[NodeComplete],
) -> io::Result<()> {
  let mut seen_ids: HashMap < ID,
                              Vec<String> > // source names
    // Maps each ID to all sources containing it
    = HashMap::new();
  for node in nodes {
    for id in node . all_ids() {
      seen_ids . entry (id . clone())
        . or_insert_with (Vec::new)
        . push (node . source . to_string()); }}
  let duplicate_ids: HashMap<ID, Vec<String>> =
    seen_ids . into_iter()
    . filter ( |(_, sources)| sources . len() > 1 )
    . collect();
  if duplicate_ids . is_empty() {
    return Ok (( )); }
  report_duplicate_ids (&duplicate_ids) ?;
  let msg: String =
    if duplicate_ids . len() <= 10 {
      // Include details in error message for small numbers
      let ids_list: Vec<String> = duplicate_ids . keys()
        . map ( |id| format! ("'{}'", id) )
        . collect();
      format! ("Duplicate ID(s) found: {}",
               ids_list . join (", "))
    } else {
      format! ("{} duplicate IDs found (see org file)",
               duplicate_ids . len() ) };
  Err (io::Error::new (
    io::ErrorKind::InvalidData, msg )) }

fn read_skg_files_from_folder (
  source_name : &SourceName,
  config      : &SkgConfig,
) -> io::Result < Vec<NodeComplete> > {
  let source : &SkgfileSource =
    config . sources . get (source_name)
    . ok_or_else(|| io::Error::new(
      io::ErrorKind::NotFound,
      format!("Source '{}' not found in config", source_name)))?;
  let mut nodes : Vec<NodeComplete> = Vec::new ();
  let entries : ReadDir = // an iterator
    fs::read_dir (&source . path) ?;
  for entry in entries {
    let entry : DirEntry = entry ?;
    let path : PathBuf = entry . path () ;
    if ( path . is_file () &&
         path . extension () . map_or (
           false,                  // None => no extension found
           |ext| ext == "skg") ) { // Some
      let node_fs : NodeFS =
        read_nodecomplete (&path) ?;
      validate_pid_matches_filename (&node_fs, &path) ?;
      let nodecomplete : NodeComplete =
        node_fs . into_complete ( source_name . clone ());
      nodes . push (nodecomplete); }}
  Ok (nodes) }

/// Like `read_all_skg_files_from_sources_AND_check_for_dup_ids` but only reads files
/// whose mtime is more recent than `since`.
pub fn read_recently_modified_skgfiles_from_sources (
  config : &SkgConfig,
  since  : std::time::SystemTime,
) -> io::Result<Vec<NodeComplete>> {
  let mut all_nodes : Vec<NodeComplete> = Vec::new();
  let mut seen_ids  : HashSet<ID> = HashSet::new();
  for (source_name, source) in config . sources . iter() {
    let entries : ReadDir =
      fs::read_dir (&source . path) ?;
    for entry in entries {
      let entry : DirEntry = entry ?;
      let path  : PathBuf  = entry . path();
      if !( path . is_file() &&
            path . extension() . map_or (false,
                                         |ext| ext == "skg")) {
        continue; }
      let mtime : std::time::SystemTime =
        fs::metadata (&path) ? . modified() ?;
      if mtime <= since { continue; }
      let node_fs : NodeFS =
        read_nodecomplete (&path) ?;
      validate_pid_matches_filename (&node_fs, &path) ?;
      let pid : ID =
        node_fs . pid . clone();
      if ! seen_ids . insert (pid . clone()) {
        return Err ( io::Error::new (
          io::ErrorKind::InvalidData,
          format! ("Duplicate primary ID '{}' within batch",
                   pid )) ); }
      let nodecomplete : NodeComplete =
        node_fs . into_complete ( source_name . clone ());
      all_nodes . push (nodecomplete); }}
  Ok (all_nodes) }

/// Reports duplicate IDs found across sources.
/// If there are errors, writes a detailed report to an org file.
/// For ≤10 duplicates, also lists each one on stderr.
/// For >10 duplicates, logs only the count and the file path.
fn report_duplicate_ids(
  duplicates: &HashMap<ID,
                       Vec<String>> // sources
) -> io::Result<()> {
  let count: usize = duplicates . len();
  let filename: &str = "initialization-error_duplicate-ids.org";
  let mut content: String = String::new();
  content . push_str ("#+title: Duplicate IDs Across Sources\n");
  content . push_str ("#+date: <generated at initialization>\n\n");
  content . push_str(
    &format!("Found {} duplicate IDs across sources.\n\n",
             count));

  let mut sorted_ids: Vec<(&ID, &Vec<String>)> =
    // for deterministic output
    duplicates . iter() . collect();
  sorted_ids . sort_by_key(|(id, _)| *id);

  for (id, sources) in sorted_ids {
    content . push_str(&format!("* {}\n", id));
    let mut sorted_sources: Vec<String> =
      // for deterministic output
      sources . clone();
    sorted_sources . sort();
    for source in sorted_sources {
      content . push_str(&format!("** {}\n", source)); }}

  if count > 0 { // otherwise nothing to report
    fs::write(filename, content)?;
    if count <= 10 {
      tracing::error!("Found {} duplicate ID(s) across sources:",
                count);
      for (id, sources) in duplicates . iter() {
        tracing::error!("  - ID '{}' in sources: {}",
                  id, sources . join (", ")); }
    } else {
      tracing::error!("Found {} duplicate ID(s) across sources.",
                count);
      tracing::error!("Details written to: {}", filename); }}
  Ok (( )) }

/// Reports file loading errors.
/// Always writes to org file and reports count to stderr.
fn report_load_errors(
  errors: &[(String, String, String)]
) -> io::Result<()> {
  let count: usize = errors . len();
  let filename: &str =
    "initialization-error_unreadable-skg-files.org";

  let mut content: String = String::new();
  content . push_str ("#+title: Unreadable SKG Files\n");
  content . push_str ("#+date: <generated at initialization>\n\n");
  content . push_str( &format!(
    "Found {} unreadable file(s).\n\n", count));

  // Sort errors by path for deterministic output
  let mut sorted_errors: Vec<(String, String, String)> =
    errors . to_vec();
  sorted_errors . sort_by(|a, b| a . 1 . cmp(&b . 1));

  for (source, filename_or_path, error_msg) in sorted_errors {
    content . push_str(&format!("* {}\n", filename_or_path));
    content . push_str(&format!("** {}\n", source));
    content . push_str(&format!("*** Error: {}\n", error_msg));
  }

  fs::write(filename, content)?;
  tracing::error!("Found {} unreadable file(s).", count);
  tracing::error!("Details written to: {}", filename);

  Ok(())
}

/// Writes all given `NodeComplete`s to disk, at `config.skg_folder`,
/// using the primary ID as the filename, followed by `.skg`.
pub fn write_all_nodes_to_fs (
  nodes  : Vec<NodeComplete>,
  config : SkgConfig,
) -> io  ::Result<usize> { // number of files written

  // Collect unique source directories and ensure they exist
  for source_name in {
    let unique_sources : HashSet<&SourceName> =
      nodes . iter()
      . map( |node| &node . source )
      . collect();
    unique_sources } {
    let source_config: &SkgfileSource =
      config . sources . get (source_name)
      . ok_or_else( || io::Error::new(
        io::ErrorKind::NotFound,
        format!("Source '{}' not found in config",
                source_name)) )?;
    fs::create_dir_all ( &source_config . path )?; }

  let mut written : usize = 0;
  for node in nodes {
    let pid : ID = node . pid . clone ();
    let path : String =
      path_from_pid_and_source (
        & config, & node . source, pid )
      . map_err ( |e| io::Error::new (
        io::ErrorKind::NotFound, e) ) ?;
    write_nodecomplete ( & node, & Path::new ( &path )) ?;
    written += 1; }
  Ok (written) }

pub fn delete_all_nodes_from_fs (
  delete_targets : Vec<(ID, SourceName)>,
  config         : SkgConfig,
) -> io::Result<usize> { // number of files deleted

  let mut deleted : usize = 0;
  for (pid, source) in delete_targets {
    let path : String =
      path_from_pid_and_source (
        & config, & source, pid )
      . map_err ( |e| io::Error::new (
        io::ErrorKind::NotFound, e) ) ?;
    match fs::remove_file ( &path )
    {
      Ok ( () ) => {
        deleted += 1; },
      Err (e) if e . kind () == io::ErrorKind::NotFound => {
        // File doesn't exist, which is fine.
        // The user created something and deleted it in the same pass.
      },
      Err (e) => {
        // TODO : Should return a list of IDs not found.
        return Err (e); }} }
  Ok (deleted) }
