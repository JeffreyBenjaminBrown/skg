use crate::telescope::compose::compose_telescope_collecting_warnings;
use crate::telescope::types::{
  CompositionWarning, Telescope, retain_owned_sections_when_pid_folderlides,
};
use crate::telescope::invariants::TelescopeViolation;
use crate::dbs::filesystem::one_node::{
  PreparedTelescopeWrite, prepare_graphnode_telescope,
  read_graphnode, validate_pid_matches_filename,
};
use crate::types::misc::{SkgConfig, Skgrepo, ID, SkgrepoName};
use crate::types::nodes::fs::GraphnodeOnDisk;
use crate::types::nodes::complete::Graphnode;

use std::collections::{HashMap, HashSet};
use std::io;
use std::path::{Path, PathBuf};
use std::fs::{self, DirEntry, ReadDir};

/// Reads all .skg files from all configured skgrepos.
/// Sets each node's skgrepo field to the appropriate skgrepo name.
/// If any files fail to load, writes a detailed report to an
/// org file in the config's data_root and returns a summary error.
///
/// Load-time telescope violations are LOGGED here and dropped.
/// Callers that report them to the user (init and rebuild) take
/// 'read_all_skg_files_from_repos_collecting_violations' instead.
pub fn read_all_skg_files_from_skgrepos (
  config: &SkgConfig
) -> io::Result<Vec<Graphnode>> {
  let (nodes, violations)
    : (Vec<Graphnode>, Vec<(ID, TelescopeViolation)>) =
    read_all_skg_files_from_skgrepos_collecting_violations (config) ?;
  for (pid, v) in &violations {
    tracing::warn! ( pid = %pid, violation = %v,
                     "telescope violation found at load" ); }
  Ok (nodes) }

/// Import preflight must inspect authoritative export claims without
/// creating or removing the loader's diagnostic reports.
pub(crate) fn read_all_skg_files_from_skgrepos_read_only (
  config : &SkgConfig,
) -> io::Result<Vec<Graphnode>> {
  let (nodes, violations) =
    read_all_skg_files_from_skgrepos_impl (config, false)?;
  if ! violations . is_empty () {
    return Err (io::Error::new (io::ErrorKind::InvalidData,
      "Configured repos have telescope violations; resolve them before import")); }
  Ok (nodes)
}

/// As 'read_all_skg_files_from_repos', but hands back the
/// load-time telescope violations rather than logging them, so
/// init and rebuild can report them alongside the graph-level ones
/// in DATA_ROOT/telescope-warnings.org.
///
/// Two kinds arise here and nowhere else, because only here are a
/// node's SECTION LIST and the config both in hand:
/// - every 'CompositionWarning' (wrapped as 'TelescopeViolation::Composition'),
/// - 'IgnoredForeignPidFolderlision', where owned and non-owned files
///   use the same pid. The owned telescope wins before composition.
pub fn read_all_skg_files_from_skgrepos_collecting_violations (
  config: &SkgConfig
) -> io::Result<(Vec<Graphnode>, Vec<(ID, TelescopeViolation)>)> {
  read_all_skg_files_from_skgrepos_impl (config, true)
}

fn read_all_skg_files_from_skgrepos_impl (
  config : &SkgConfig,
  report_errors : bool,
) -> io::Result<(Vec<Graphnode>, Vec<(ID, TelescopeViolation)>)> {
  let mut sections_by_pid
    : HashMap<ID, Vec<(SkgrepoName, GraphnodeOnDisk)>> = HashMap::new();
  let mut pid_order : Vec<ID> = Vec::new(); // deterministic output
  let mut load_errors: Vec<(String, // skgrepo name
                            String, // filename
                            String)> // error message
    = Vec::new();
  for skgrepo_name in config . ordered_skgrepos () {
    let Some (skgrepo) : Option<&Skgrepo> =
      config . skgrepos . get (&skgrepo_name) else { continue; };
    match read_skg_sections_from_folder (&skgrepo_name, config) {
      Ok (sections) => {
        for (skgrepo, node_fs) in sections {
          let pid : ID = node_fs . pid . clone ();
          if ! sections_by_pid . contains_key (&pid) {
            pid_order . push ( pid . clone () ); }
          sections_by_pid . entry (pid)
            . or_insert_with (Vec::new)
            . push ((skgrepo, node_fs)); }}
      Err (e) => {
        load_errors . push ((
          skgrepo_name . to_string(),
          skgrepo . path . display() . to_string(),
          e . to_string()
        )); }} }
  if report_errors {
    report_load_errors (&load_errors, &config . data_root) ?; }
  if ! load_errors . is_empty() {
    return Err (io::Error::new (
      io::ErrorKind::InvalidData,
      format! ("{} unreadable file(s)",
               load_errors . len() ))); }
  let collision_violations : Vec<(ID, TelescopeViolation)> =
    retain_owned_telescopes (
      &mut sections_by_pid, &pid_order, config );
  let (nodes, composition_violations)
    : (Vec<Graphnode>, Vec<(ID, TelescopeViolation)>) =
    compose_grouped_sections (sections_by_pid, pid_order, config) ?;
  Ok (( nodes,
        { let mut all : Vec<(ID, TelescopeViolation)> =
            collision_violations;
          all . extend (composition_violations);
          all . sort_by ( |a, b| a . 0 . cmp ( &b . 0 ));
          all } )) }

/// When owned and non-owned files use one pid, retain only the
/// owned files before composition or building the extra-id map. A pid
/// represented entirely by non-owned files remains readable.
fn retain_owned_telescopes (
  sections_by_pid : &mut HashMap<ID, Vec<(SkgrepoName, GraphnodeOnDisk)>>,
  pid_order       : &[ID],
  config          : &SkgConfig,
) -> Vec<(ID, TelescopeViolation)> {
  let mut violations : Vec<(ID, TelescopeViolation)> = Vec::new ();
  for pid in pid_order {
    let Some (sections) = sections_by_pid . remove (pid)
      else { continue; };
    let (retained, collision) =
      retain_owned_sections_when_pid_folderlides (sections, config);
    sections_by_pid . insert (pid . clone (), retained);
    if let Some (collision) = collision {
      violations . push ((
        pid . clone (),
        TelescopeViolation::IgnoredForeignPidFolderlision {
          ignored_skgrepos : collision . ignored_skgrepos,
        } )); }}
  violations }

/// One telescope, read fresh from disk by pid (all its sections,
/// composed). Errors if no section exists or no section has a title.
pub fn graphnode_from_telescope_on_disk (
  config : &SkgConfig,
  pid    : &ID,
) -> io::Result<Graphnode> {
  crate::dbs::filesystem::one_node::graphnode_from_pid_and_skgrepo (
    config, pid . clone (),
    & SkgrepoName::from ("(any)") ) }

/// Compose each telescope (already grouped by pid; sections arrive in
/// privacy order because the caller iterated 'ordered_repos').
/// Anchors resolve through the extra-id map built from every
/// section, so a nodeMerge cannot dangle an anchor. A telescope
/// with no title in any section is a hard load error; every other
/// compose complaint comes back as a violation for the caller to
/// report.
fn compose_grouped_sections (
  mut sections_by_pid : HashMap<ID, Vec<(SkgrepoName, GraphnodeOnDisk)>>,
  pid_order           : Vec<ID>,
  config              : &SkgConfig,
) -> io::Result<(Vec<Graphnode>, Vec<(ID, TelescopeViolation)>)> {
  let pid_of : HashMap<ID, ID> = {
    let mut m : HashMap<ID, ID> = HashMap::new ();
    for (pid, sections) in sections_by_pid . iter () {
      for (_, node_fs) in sections {
        for extra in &node_fs . extra_ids {
          m . insert ( extra . clone (), pid . clone () ); }} }
    m };
  let resolve = |skgid : &ID| -> ID {
    pid_of . get (skgid) . cloned ()
      . unwrap_or_else ( || skgid . clone () ) };
  let mut all_nodes : Vec<Graphnode> = Vec::new ();
  let mut all_violations : Vec<(ID, TelescopeViolation)> = Vec::new ();
  for pid in pid_order {
    let telescope : Telescope = Telescope::try_new (
      pid . clone (),
      sections_by_pid . remove (&pid)
        . expect ("pid_order tracks sections_by_pid"),
      config )
      . map_err ( |e| io::Error::new (
        io::ErrorKind::InvalidData, e ) ) ?;
    let (node, warnings) : (Graphnode, Vec<CompositionWarning>) =
      compose_telescope_collecting_warnings ( telescope, &resolve ) ?;
    all_nodes . push (node);
    for w in warnings {
      all_violations . push (
        ( pid . clone (), TelescopeViolation::Composition (w) )); }}
  Ok (( all_nodes, all_violations )) }

/// NOT AN ERROR: same-id files across skgrepos. Those are the
/// SECTIONS of one privacy telescope, grouped and composed at load,
/// and they are the feature -- see docs/telescopes.org. Sections of
/// one telescope share a pid, so they can never trip this check.
///
/// THE ERROR: one id claimed by two DIFFERENT nodes -- an id
/// (primary or extra) appearing among the all_ids() of two nodes
/// with distinct pids. Nothing about it is cross-repo; both
/// claimants can sit in one skgrepo. If any exists, writes a detailed
/// report (to stderr for ≤10, to an org file otherwise) and returns
/// a summary error. (Callers pass post-fold nodes, one per
/// telescope.)
pub fn error_unless_each_skgid_names_one_node (
  nodes     : &[Graphnode],
  data_root : &Path,
) -> io::Result<()> {
  let mut claimants: HashMap < ID, Vec<(ID, SkgrepoName)> > =
    // Maps each ID to the (pid, home) of every node claiming it
    HashMap::new();
  for node in nodes {
    for skgid in node . all_skgids() {
      claimants . entry (skgid . clone())
        . or_insert_with (Vec::new)
        . push ((node . pid . clone(), node . home_skgrepo . clone())); }}
  let contested: HashMap<ID, Vec<(ID, SkgrepoName)>> =
    claimants . into_iter()
    . filter ( |(_, recorders)| {
      let distinct_pids : HashSet<&ID> =
        recorders . iter() . map ( |(pid, _)| pid ) . collect();
      distinct_pids . len() > 1 } )
    . collect();
  report_skgids_claimed_by_two_nodes (&contested, data_root) ?;
  if contested . is_empty() {
    return Ok (( )); }
  let msg: String =
    if contested . len() <= 10 {
      // Include details in error message for small numbers
      let ids_list: Vec<String> = contested . keys()
        . map ( |skgid| format! ("'{}'", skgid) )
        . collect();
      format! ("{} id(s) claimed by more than one node: {}",
               contested . len(),
               ids_list . join (", "))
    } else {
      format! ("{} id(s) claimed by more than one node (see org file)",
               contested . len() ) };
  Err (io::Error::new (
    io::ErrorKind::InvalidData, msg )) }

pub fn read_skg_sections_from_folder (
  skgrepo_name : &SkgrepoName,
  config       : &SkgConfig,
) -> io::Result < Vec<(SkgrepoName, GraphnodeOnDisk)> > {
  let skgrepo : &Skgrepo =
    config . skgrepos . get (skgrepo_name)
    . ok_or_else(|| io::Error::new(
      io::ErrorKind::NotFound,
      format!("Repo '{}' not found in config", skgrepo_name)))?;
  let mut sections : Vec<(SkgrepoName, GraphnodeOnDisk)> = Vec::new ();
  let entries : ReadDir = // an iterator
    fs::read_dir (&skgrepo . path) ?;
  for entry in entries {
    let entry : DirEntry = entry ?;
    let path : PathBuf = entry . path () ;
    if ( path . is_file () &&
         path . extension () . map_or (
           false,                  // None => no extension found
           |ext| ext == "skg") ) { // Some
      let node_fs : GraphnodeOnDisk =
        read_graphnode (&path) ?;
      validate_pid_matches_filename (&node_fs, &path) ?;
      sections . push (( skgrepo_name . clone (), node_fs )); }}
  Ok (sections) }

/// Like `read_all_skg_files_from_repos` but only for telescopes
/// with at least one section file whose mtime is more recent than
/// `since`. A touched SECTION reloads its WHOLE telescope (all its
/// sections, however old), since the composition needs every skgrepo.
pub fn read_recently_modified_skgfiles_from_skgrepos (
  config : &SkgConfig,
  since  : std::time::SystemTime,
) -> io::Result<Vec<Graphnode>> {
  let mut modified_pids : Vec<ID> = Vec::new();
  let mut seen_skgids      : HashSet<ID> = HashSet::new();
  for (_skgrepo_name, skgrepo) in config . skgrepos . iter() {
    let entries : ReadDir =
      fs::read_dir (&skgrepo . path) ?;
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
      let node_fs : GraphnodeOnDisk =
        read_graphnode (&path) ?;
      validate_pid_matches_filename (&node_fs, &path) ?;
      let pid : ID =
        node_fs . pid . clone();
      if seen_skgids . insert (pid . clone()) {
        modified_pids . push (pid); }} }
  let mut all_nodes : Vec<Graphnode> = Vec::new();
  for pid in modified_pids {
    all_nodes . push (
      graphnode_from_telescope_on_disk (config, &pid) ? ); }
  Ok (all_nodes) }

/// Reports each id claimed by more than one node, naming every
/// CLAIMANT as "pid (home)" -- the pids are what the reader must
/// open to repair the conflict, and both claimants can share one
/// home, so the homes alone identify nothing.
/// If there are none, removes a stale report, so the file's
/// presence is meaningful.
/// Otherwise writes a detailed report to an org file, and for ≤10
/// also lists each conflict on stderr; for >10, logs the count and
/// the file path.
fn report_skgids_claimed_by_two_nodes(
  contested : &HashMap<ID, Vec<(ID, SkgrepoName)>>,
  data_root : &Path,
) -> io::Result<()> {
  let count: usize = contested . len();
  // DANGER: The report path is fixed per data_root, so two tests sharing a data_root (notably any test using SkgConfig::dummyFromRepos,which defaults to ".") can still clobber each other's report.
  let report_path: PathBuf = data_root . join (
    "initialization-error_ids-claimed-by-two-nodes.org");
  if count == 0 {
    return remove_stale_report (&report_path); }
  let claimant_lines = | claimants : &Vec<(ID, SkgrepoName)> |
                       -> Vec<String> {
    let mut lines : Vec<String> = // for deterministic output
      claimants . iter ()
      . map ( |(pid, home)| format! ("{} ({})", pid, home) )
      . collect ();
    lines . sort ();
    lines . dedup ();
    lines };
  let content: String = {
    let mut content: String = String::new();
    content . push_str ("#+title: IDs claimed by more than one node\n");
    content . push_str ("#+date: <generated at initialization>\n\n");
    content . push_str( &format!(
      "{} id(s) claimed by more than one node. Same-id files ACROSS REPOS are not this: those are the sections of one privacy telescope (docs/telescopes.org). Each id below is claimed, as a primary or extra id, by the distinct nodes listed under it.\n\n",
      count));
    let mut sorted_skgids: Vec<(&ID, &Vec<(ID, SkgrepoName)>)> =
      // for deterministic output
      contested . iter() . collect();
    sorted_skgids . sort_by_key(|(skgid, _)| *skgid);
    for (skgid, claimants) in sorted_skgids {
      content . push_str(&format!("* {}\n", skgid));
      for line in claimant_lines (claimants) {
        content . push_str(&format!("** {}\n", line)); }}
    content };
  fs::write(&report_path, content)?;
  if count <= 10 {
    tracing::error!("{} id(s) claimed by more than one node:",
              count);
    for (skgid, claimants) in contested . iter() {
      tracing::error!("  - ID '{}' claimed by: {}",
                skgid, claimant_lines (claimants) . join (", ")); }
  } else {
    tracing::error!("{} id(s) claimed by more than one node.",
              count);
    tracing::error!("Details written to: {}",
              report_path . display()); }
  Ok (( )) }

/// Delete a report whose condition no longer holds, so that a
/// report file left on disk always describes the LAST run rather
/// than some earlier one.
fn remove_stale_report (
  report_path : &Path,
) -> io::Result<()> {
  match fs::remove_file (report_path) {
    Ok (( ))                                          => Ok (( )),
    Err (e) if e . kind () == io::ErrorKind::NotFound => Ok (( )),
    Err (e)                                           => Err (e), } }

/// Reports file loading errors.
/// If there are none, removes a stale report, so the file's
/// presence is meaningful. Otherwise writes to an org file and
/// reports the count to stderr.
fn report_load_errors(
  errors    : &[(String, String, String)],
  data_root : &Path,
) -> io::Result<()> {
  let count: usize = errors . len();
  let report_path: PathBuf = data_root . join ( // DANGER: The report path is fixed per data_root, so two tests sharing a data_root (notably any test using SkgConfig::dummyFromRepos,which defaults to ".") can still clobber each other's report.

    "initialization-error_unreadable-skg-files.org");
  if count == 0 {
    return remove_stale_report (&report_path); }

  let mut content: String = String::new();
  content . push_str ("#+title: Unreadable SKG Files\n");
  content . push_str ("#+date: <generated at initialization>\n\n");
  content . push_str( &format!(
    "Found {} unreadable file(s).\n\n", count));

  // Sort errors by path for deterministic output
  let mut sorted_errors: Vec<(String, String, String)> =
    errors . to_vec();
  sorted_errors . sort_by(|a, b| a . 1 . cmp(&b . 1));

  for (skgrepo, filename_or_path, error_msg) in sorted_errors {
    content . push_str(&format!("* {}\n", filename_or_path));
    content . push_str(&format!("** {}\n", skgrepo));
    content . push_str(&format!("*** Error: {}\n", error_msg));
  }

  fs::write(&report_path, content)?;
  tracing::error!("Found {} unreadable file(s).", count);
  tracing::error!("Details written to: {}", report_path . display());

  Ok(())
}

/// Writes all given `Graphnode`s to disk as telescopes: each
/// node's sections land in their skgrepo directories, named
/// by the primary ID followed by `.skg`.
pub fn write_all_nodes_to_fs (
  nodes  : Vec<Graphnode>,
  config : SkgConfig,
) -> io  ::Result<usize> { // number of nodes written
  let prepared : Vec<PreparedTelescopeWrite> =
    nodes . iter ()
    . map ( |node|
      prepare_graphnode_telescope (node, &config, false) )
    . collect::<io::Result<Vec<PreparedTelescopeWrite>>> () ?;
  for telescope in &prepared {
    telescope . apply (&config) ?; }
  for telescope in &prepared {
    telescope . verify_hoist (&config) ?; }
  Ok (prepared . len ()) }

/// Deleting a node deletes its whole TELESCOPE: every owned
/// section file of that pid, in whatever skgrepo. (The RepoName in
/// each target is the caller's belief about the home; kept in the
/// signature for its callers, but every owned skgrepo is swept.)
pub fn delete_all_nodes_from_fs (
  delete_targets : Vec<(ID, SkgrepoName)>,
  config         : SkgConfig,
) -> io::Result<usize> { // number of nodes deleted

  let mut deleted : usize = 0;
  for (pid, _skgrepo) in delete_targets {
    let mut any_removed : bool = false;
    for skgrepo_name in config . ordered_skgrepos () {
      if ! config . skgrepo_is_owned (&skgrepo_name) { continue; }
      let path : String =
        match crate::util::path_from_pid_and_skgrepo (
          & config, & skgrepo_name, pid . clone () ) {
          Ok (p) => p,
          Err (_) => continue, };
      match fs::remove_file ( &path )
      {
        Ok ( () ) => {
          any_removed = true; },
        Err (e) if e . kind () == io::ErrorKind::NotFound => {
          // No section at this repo, which is fine.
        },
        Err (e) => {
          // TODO : Should return a list of IDs not found.
          return Err (e); }} }
    if any_removed {
      deleted += 1; }}
  Ok (deleted) }
