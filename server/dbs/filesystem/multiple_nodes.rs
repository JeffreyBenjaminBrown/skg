use crate::accordion::fold::{FoldedNode, fold_sections, nodecomplete_from_fold};
use crate::accordion::types::SectionSlices;
use crate::dbs::filesystem::one_node::{read_nodecomplete, validate_pid_matches_filename, write_nodecomplete_accordion};
use crate::types::misc::{SkgConfig, SkgfileSource, ID, SourceName};
use crate::types::nodes::fs::NodeFS;
use crate::types::nodes::complete::NodeComplete;

use std::collections::{HashMap, HashSet};
use std::io;
use std::path::{Path, PathBuf};
use std::fs::{self, DirEntry, ReadDir};

/// Reads all .skg files from all configured sources.
/// Sets each node's source field to the appropriate source name.
/// If any files fail to load, writes a detailed report to an
/// org file in the config's data_root and returns a summary error.
pub fn read_all_skg_files_from_sources (
  config: &SkgConfig
) -> io::Result<Vec<NodeComplete>> {
  let mut sections_by_pid
    : HashMap<ID, Vec<(SourceName, NodeFS)>> = HashMap::new();
  let mut pid_order : Vec<ID> = Vec::new(); // deterministic output
  let mut load_errors: Vec<(String, // source name
                            String, // filename
                            String)> // error message
    = Vec::new();
  for source_name in config . ordered_sources () {
    let Some (source) : Option<&SkgfileSource> =
      config . sources . get (&source_name) else { continue; };
    match read_skg_sections_from_folder (&source_name, config) {
      Ok (sections) => {
        for (level, node_fs) in sections {
          let pid : ID = node_fs . pid . clone ();
          if ! sections_by_pid . contains_key (&pid) {
            pid_order . push ( pid . clone () ); }
          sections_by_pid . entry (pid)
            . or_insert_with (Vec::new)
            . push ((level, node_fs)); }}
      Err (e) => {
        load_errors . push ((
          source_name . to_string(),
          source . path . display() . to_string(),
          e . to_string()
        )); }} }
  if ! load_errors . is_empty() {
    report_load_errors (&load_errors, &config . data_root) ?;
    return Err (io::Error::new (
      io::ErrorKind::InvalidData,
      format! ("{} unreadable file(s)",
               load_errors . len() ))); }
  fold_grouped_sections (sections_by_pid, pid_order) }

/// One accordion, read fresh from disk by pid (all its sections,
/// folded). Errors if no section exists or no section has a title.
pub fn nodecomplete_from_accordion_on_disk (
  config : &SkgConfig,
  pid    : &ID,
) -> io::Result<NodeComplete> {
  crate::dbs::filesystem::one_node::nodecomplete_from_pid_and_source (
    config, pid . clone (),
    & SourceName::from ("(any)") ) }

/// Fold each accordion (already grouped by pid; sections arrive in
/// privacy order because the caller iterated 'ordered_sources').
/// Anchors resolve through the extra-id map built from every
/// section, so a nodeMerge cannot dangle an anchor. A titleless
/// accordion (no home) is a hard load error; fold warnings are
/// logged for now (the accordion validators, next work item,
/// formalize their reporting).
fn fold_grouped_sections (
  mut sections_by_pid : HashMap<ID, Vec<(SourceName, NodeFS)>>,
  pid_order           : Vec<ID>,
) -> io::Result<Vec<NodeComplete>> {
  let pid_of : HashMap<ID, ID> = {
    let mut m : HashMap<ID, ID> = HashMap::new ();
    for (pid, sections) in sections_by_pid . iter () {
      for (_, node_fs) in sections {
        for extra in &node_fs . extra_ids {
          m . insert ( extra . clone (), pid . clone () ); }} }
    m };
  let resolve = |id : &ID| -> ID {
    pid_of . get (id) . cloned ()
      . unwrap_or_else ( || id . clone () ) };
  let mut all_nodes : Vec<NodeComplete> = Vec::new ();
  for pid in pid_order {
    let sections : Vec<(SourceName, NodeFS)> =
      sections_by_pid . remove (&pid)
      . expect ("pid_order tracks sections_by_pid");
    all_nodes . push (
      fold_one_accordion ( &pid, sections, &resolve ) ? ); }
  Ok (all_nodes) }

/// Fold ONE accordion's sections (in privacy order) into a
/// NodeComplete. 'resolve' maps extra ids to pids for anchor
/// resolution and must be built from the whole corpus, not just
/// this accordion. A titleless accordion (no home) is a hard error;
/// fold warnings are logged.
pub fn fold_one_accordion (
  pid      : &ID,
  sections : Vec<(SourceName, NodeFS)>,
  resolve  : &dyn Fn (&ID) -> ID,
) -> io::Result<NodeComplete> {
  let extra_ids : Vec<ID> = {
    let mut extra_ids : Vec<ID> = Vec::new ();
    for (_, node_fs) in &sections {
      for e in &node_fs . extra_ids {
        if ! extra_ids . contains (e) {
          extra_ids . push ( e . clone () ); }} }
    extra_ids };
  let misc : Vec<crate::types::nodes::complete::FileProperty> = {
    // home-section data, like extra_ids; unioned defensively
    let mut misc : Vec<crate::types::nodes::complete::FileProperty> =
      Vec::new ();
    for (_, node_fs) in &sections {
      for m in &node_fs . misc {
        if ! misc . contains (m) {
          misc . push ( m . clone () ); }} }
    misc };
  let slices : Vec<(SourceName, SectionSlices)> =
    sections . into_iter ()
    . map ( |(level, node_fs)|
            (level, node_fs . into_section_slices ()) )
    . collect ();
  let (folded, warnings) : (FoldedNode, _) =
    fold_sections ( &slices, resolve );
  for w in &warnings {
    tracing::warn! ( pid = %pid, warning = ?w,
                     "accordion fold warning" ); }
  nodecomplete_from_fold (
    pid . clone (), extra_ids, misc, folded )
    . ok_or_else ( || io::Error::new (
      io::ErrorKind::InvalidData,
      format! ("Accordion '{}' has no home: no section carries a title.",
               pid ))) }

/// Same-ID files across sources are no longer duplicates -- they
/// are the sections of one privacy accordion, grouped and folded at
/// load. What remains a CONFLICT is one id claimed by two DIFFERENT
/// nodes: an id (primary or extra) appearing among the all_ids() of
/// two nodes with distinct pids. If any exists, writes a detailed
/// report (to stderr for ≤10, to an org file otherwise) and returns
/// a summary error. (Callers pass post-fold nodes, one per
/// accordion.)
pub fn check_for_duplicate_ids_across_sources (
  nodes     : &[NodeComplete],
  data_root : &Path,
) -> io::Result<()> {
  let mut claimants: HashMap < ID, Vec<(ID, SourceName)> > =
    // Maps each ID to the (pid, home) of every node claiming it
    HashMap::new();
  for node in nodes {
    for id in node . all_ids() {
      claimants . entry (id . clone())
        . or_insert_with (Vec::new)
        . push ((node . pid . clone(), node . source . clone())); }}
  let duplicate_ids: HashMap<ID, Vec<SourceName>> =
    claimants . into_iter()
    . filter ( |(_, owners)| {
      let distinct_pids : HashSet<&ID> =
        owners . iter() . map ( |(pid, _)| pid ) . collect();
      distinct_pids . len() > 1 } )
    . map ( |(id, owners)|
            (id, owners . into_iter()
                 . map ( |(_, src)| src ) . collect()) )
    . collect();
  if duplicate_ids . is_empty() {
    return Ok (( )); }
  report_duplicate_ids (&duplicate_ids, data_root) ?;
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

pub fn read_skg_sections_from_folder (
  source_name : &SourceName,
  config      : &SkgConfig,
) -> io::Result < Vec<(SourceName, NodeFS)> > {
  let source : &SkgfileSource =
    config . sources . get (source_name)
    . ok_or_else(|| io::Error::new(
      io::ErrorKind::NotFound,
      format!("Source '{}' not found in config", source_name)))?;
  let mut sections : Vec<(SourceName, NodeFS)> = Vec::new ();
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
      sections . push (( source_name . clone (), node_fs )); }}
  Ok (sections) }

/// Like `read_all_skg_files_from_sources` but only for accordions
/// with at least one section file whose mtime is more recent than
/// `since`. A touched SECTION reloads its WHOLE accordion (all its
/// sections, however old), since the fold needs every level.
pub fn read_recently_modified_skgfiles_from_sources (
  config : &SkgConfig,
  since  : std::time::SystemTime,
) -> io::Result<Vec<NodeComplete>> {
  let mut modified_pids : Vec<ID> = Vec::new();
  let mut seen_ids      : HashSet<ID> = HashSet::new();
  for (_source_name, source) in config . sources . iter() {
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
      if seen_ids . insert (pid . clone()) {
        modified_pids . push (pid); }} }
  let mut all_nodes : Vec<NodeComplete> = Vec::new();
  for pid in modified_pids {
    all_nodes . push (
      nodecomplete_from_accordion_on_disk (config, &pid) ? ); }
  Ok (all_nodes) }

/// Reports duplicate IDs found across sources.
/// If there are errors, writes a detailed report to an org file.
/// For ≤10 duplicates, also lists each one on stderr.
/// For >10 duplicates, logs only the count and the file path.
fn report_duplicate_ids(
  duplicates : &HashMap<ID, Vec<SourceName>>,
  data_root  : &Path,
) -> io::Result<()> {
  let count: usize = duplicates . len();
  // DANGER: The report path is fixed per data_root, so two tests sharing a data_root (notably any test using SkgConfig::dummyFromSources,which defaults to ".") can still clobber each other's report.
  let report_path: PathBuf = data_root . join (
    "initialization-error_duplicate-ids.org");
  let mut content: String = String::new();
  content . push_str ("#+title: Duplicate IDs Across Sources\n");
  content . push_str ("#+date: <generated at initialization>\n\n");
  content . push_str(
    &format!("Found {} duplicate IDs across sources.\n\n",
             count));

  let mut sorted_ids: Vec<(&ID, &Vec<SourceName>)> =
    // for deterministic output
    duplicates . iter() . collect();
  sorted_ids . sort_by_key(|(id, _)| *id);

  for (id, sources) in sorted_ids {
    content . push_str(&format!("* {}\n", id));
    let mut sorted_sources: Vec<SourceName> =
      // for deterministic output
      sources . clone();
    sorted_sources . sort();
    for source in sorted_sources {
      content . push_str(&format!("** {}\n", source)); }}

  if count > 0 { // otherwise nothing to report
    fs::write(&report_path, content)?;
    if count <= 10 {
      tracing::error!("Found {} duplicate ID(s) across sources:",
                count);
      for (id, sources) in duplicates . iter() {
        let names : Vec<String> =
          sources . iter () . map (|s| s . to_string ()) . collect ();
        tracing::error!("  - ID '{}' in sources: {}",
                  id, names . join (", ")); }
    } else {
      tracing::error!("Found {} duplicate ID(s) across sources.",
                count);
      tracing::error!("Details written to: {}",
                report_path . display()); }}
  Ok (( )) }

/// Reports file loading errors.
/// Always writes to org file and reports count to stderr.
fn report_load_errors(
  errors    : &[(String, String, String)],
  data_root : &Path,
) -> io::Result<()> {
  let count: usize = errors . len();
  let report_path: PathBuf = data_root . join ( // DANGER: The report path is fixed per data_root, so two tests sharing a data_root (notably any test using SkgConfig::dummyFromSources,which defaults to ".") can still clobber each other's report.

    "initialization-error_unreadable-skg-files.org");

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

  fs::write(&report_path, content)?;
  tracing::error!("Found {} unreadable file(s).", count);
  tracing::error!("Details written to: {}", report_path . display());

  Ok(())
}

/// Writes all given `NodeComplete`s to disk as accordions: each
/// node's sections land in their levels' source directories, named
/// by the primary ID followed by `.skg`.
pub fn write_all_nodes_to_fs (
  nodes  : Vec<NodeComplete>,
  config : SkgConfig,
) -> io  ::Result<usize> { // number of nodes written

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
    write_nodecomplete_accordion ( & node, & config ) ?;
    written += 1; }
  Ok (written) }

/// Deleting a node deletes its whole ACCORDION: every owned
/// section file of that pid, in whatever source. (The SourceName in
/// each target is the caller's belief about the home; kept in the
/// signature for its callers, but every owned level is swept.)
pub fn delete_all_nodes_from_fs (
  delete_targets : Vec<(ID, SourceName)>,
  config         : SkgConfig,
) -> io::Result<usize> { // number of nodes deleted

  let mut deleted : usize = 0;
  for (pid, _source) in delete_targets {
    let mut any_removed : bool = false;
    for source_name in config . ordered_sources () {
      if ! config . user_owns_source (&source_name) { continue; }
      let path : String =
        match crate::util::path_from_pid_and_source (
          & config, & source_name, pid . clone () ) {
          Ok (p) => p,
          Err (_) => continue, };
      match fs::remove_file ( &path )
      {
        Ok ( () ) => {
          any_removed = true; },
        Err (e) if e . kind () == io::ErrorKind::NotFound => {
          // No section at this level, which is fine.
        },
        Err (e) => {
          // TODO : Should return a list of IDs not found.
          return Err (e); }} }
    if any_removed {
      deleted += 1; }}
  Ok (deleted) }
