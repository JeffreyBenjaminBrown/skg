use crate::telescope::fold::fold_telescope_collecting_warnings;
use crate::telescope::types::{
  FoldWarning, Telescope,
};
use crate::telescope::invariants::TelescopeViolation;
use crate::dbs::filesystem::one_node::{
  PreparedTelescopeWrite, prepare_nodecomplete_telescope,
  read_nodecomplete, read_nodefs_with_bytes,
  validate_pid_matches_filename,
};
use crate::dbs::filesystem::source_files::{
  IgnoredForeignPathCollision, SelectedSourceFiles,
  selected_direct_source_files,
};
use crate::types::misc::{SkgConfig, ID, SourceName};
use crate::types::nodes::fs::NodeFS;
use crate::types::nodes::complete::NodeComplete;
use crate::types::store_state::{PathDigest, SelectedPathManifest};

use std::collections::{HashMap, HashSet};
use std::collections::{BTreeMap, BTreeSet};
use std::io;
use std::path::{Path, PathBuf};
use std::fs::{self, DirEntry, ReadDir};

pub struct LoadedCorpus {
  pub nodes      : Vec<NodeComplete>,
  pub violations : Vec<(ID, TelescopeViolation)>,
  pub manifest   : SelectedPathManifest, }

/// Reads all .skg files from all configured sources.
/// Sets each node's source field to the appropriate source name.
/// If any files fail to load, writes a detailed report to an
/// org file in the config's data_root and returns a summary error.
///
/// Load-time telescope violations are LOGGED here and dropped.
/// Callers that report them to the user (init and rebuild) take
/// 'read_all_skg_files_from_sources_collecting_violations' instead.
pub fn read_all_skg_files_from_sources (
  config: &SkgConfig
) -> io::Result<Vec<NodeComplete>> {
  let (nodes, violations)
    : (Vec<NodeComplete>, Vec<(ID, TelescopeViolation)>) =
    read_all_skg_files_from_sources_collecting_violations (config) ?;
  for (pid, v) in &violations {
    tracing::warn! ( pid = %pid, violation = %v,
                     "telescope violation found at load" ); }
  Ok (nodes) }

/// As 'read_all_skg_files_from_sources', but hands back the
/// load-time telescope violations rather than logging them, so
/// init and rebuild can report them alongside the graph-level ones
/// in DATA_ROOT/telescope-warnings.org.
///
/// Two kinds arise here and nowhere else, because only here are a
/// node's SECTION LIST and the config both in hand:
/// - every 'FoldWarning' (wrapped as 'TelescopeViolation::Fold'),
/// - 'IgnoredForeignPidCollision', where owned and non-owned files
///   use the same pid. The owned telescope wins before folding.
pub fn read_all_skg_files_from_sources_collecting_violations (
  config: &SkgConfig
) -> io::Result<(Vec<NodeComplete>, Vec<(ID, TelescopeViolation)>)> {
  let loaded = read_all_skg_files_with_manifest (config) ?;
  Ok ((loaded . nodes, loaded . violations))
}

/// Full normalized corpus plus the exact retained bytes which produced it.
/// Ignored foreign collision losers are intentionally absent: their bytes
/// were not parsed and did not select the graph.
pub fn read_all_skg_files_with_manifest (
  config: &SkgConfig
) -> io::Result<LoadedCorpus> {
  let mut sections_by_pid
    : HashMap<ID, Vec<(SourceName, NodeFS)>> = HashMap::new();
  let mut manifest : SelectedPathManifest = SelectedPathManifest::new ();
  let mut load_errors: Vec<(String, // source name
                            String, // filename or source directory
                            String)> // error message
    = Vec::new();
  // Preserve the aggregate load report for an unreadable source directory.
  // The selector itself returns `io::Error`; here we still have the source
  // identity needed to make that error actionable.
  for source_name in config . ordered_sources () {
    let source = config . sources . get (&source_name)
      . expect ("ordered source exists");
    if let Err (e) = fs::read_dir (&source . path) {
      load_errors . push ((
        source_name . to_string (),
        source . path . display () . to_string (),
        e . to_string () )); }}
  if ! load_errors . is_empty () {
    report_load_errors (&load_errors, &config . data_root) ?;
    return Err (io::Error::new (
      io::ErrorKind::InvalidData,
      format! ("{} unreadable file(s)", load_errors . len ()) )); }
  let selected : SelectedSourceFiles =
    selected_direct_source_files (config) ?;
  let pid_order : Vec<ID> = selected . pid_order;
  let collision_violations : Vec<(ID, TelescopeViolation)> =
    selected . collisions . into_iter ()
    . map (collision_violation)
    . collect ();
  for pid in &pid_order {
    for candidate in selected . by_pid . get (pid)
      . into_iter () . flatten () {
      match read_nodefs_with_bytes (&candidate . path)
        . and_then ( |(node_fs, bytes)| {
          validate_pid_matches_filename (&node_fs, &candidate . path) ?;
          Ok ((node_fs, bytes)) }) {
        Ok ((node_fs, bytes)) => {
          manifest . insert (
            candidate . path . clone (), PathDigest::of_bytes (&bytes));
          sections_by_pid . entry (pid . clone ())
            . or_default ()
            . push ((candidate . source . clone (), node_fs)); },
        Err (e) => load_errors . push ((
          candidate . source . to_string (),
          candidate . path . display () . to_string (),
          e . to_string () )), }}}
  report_load_errors (&load_errors, &config . data_root) ?;
  if ! load_errors . is_empty() {
    return Err (io::Error::new (
      io::ErrorKind::InvalidData,
      format! ("{} unreadable file(s)",
               load_errors . len() ))); }
  let (nodes, fold_violations)
    : (Vec<NodeComplete>, Vec<(ID, TelescopeViolation)>) =
    fold_grouped_sections (sections_by_pid, pid_order, config) ?;
  Ok (LoadedCorpus {
    nodes,
    violations: {
      let mut all : Vec<(ID, TelescopeViolation)> = collision_violations;
      all . extend (fold_violations);
      all . sort_by ( |a, b| a . 0 . cmp ( &b . 0 ));
      all },
    manifest, }) }

fn collision_violation (
  collision : IgnoredForeignPathCollision,
) -> (ID, TelescopeViolation) {
  let pid : ID = collision . pid;
  let winning_sources : Vec<SourceName> = collision . winners . iter ()
    . map ( |file| file . source . clone () ) . collect ();
  let winning_paths : Vec<PathBuf> = collision . winners . into_iter ()
    . map ( |file| file . path ) . collect ();
  let ignored_sources : Vec<SourceName> = collision . losers . iter ()
    . map ( |file| file . source . clone () ) . collect ();
  let ignored_paths : Vec<PathBuf> = collision . losers . into_iter ()
    . map ( |file| file . path ) . collect ();
  (pid, TelescopeViolation::IgnoredForeignPidCollision {
    winning_sources,
    winning_paths,
    ignored_sources,
    ignored_paths,
  })
}

/// One telescope, read fresh from disk by pid (all its sections,
/// folded). Errors if no section exists or no section has a title.
pub fn nodecomplete_from_telescope_on_disk (
  config : &SkgConfig,
  pid    : &ID,
) -> io::Result<NodeComplete> {
  crate::dbs::filesystem::one_node::nodecomplete_from_pid_and_source (
    config, pid . clone (),
    & SourceName::from ("(any)") ) }

/// Fold each telescope (already grouped by pid; sections arrive in
/// privacy order because the caller iterated 'ordered_sources').
/// Anchors resolve through the extra-id map built from every
/// section, so a nodeMerge cannot dangle an anchor. A telescope
/// with no title in any section is a hard load error; every other
/// fold complaint comes back as a violation for the caller to
/// report.
pub(crate) fn fold_grouped_sections (
  mut sections_by_pid : HashMap<ID, Vec<(SourceName, NodeFS)>>,
  pid_order           : Vec<ID>,
  config              : &SkgConfig,
) -> io::Result<(Vec<NodeComplete>, Vec<(ID, TelescopeViolation)>)> {
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
  let mut all_violations : Vec<(ID, TelescopeViolation)> = Vec::new ();
  for pid in pid_order {
    let telescope : Telescope = Telescope::try_new (
      pid . clone (),
      sections_by_pid . remove (&pid)
        . expect ("pid_order tracks sections_by_pid"),
      config )
      . map_err ( |e| io::Error::new (
        io::ErrorKind::InvalidData, e ) ) ?;
    let (node, warnings) : (NodeComplete, Vec<FoldWarning>) =
      fold_telescope_collecting_warnings ( telescope, &resolve ) ?;
    all_nodes . push (node);
    for w in warnings {
      all_violations . push (
        ( pid . clone (), TelescopeViolation::Fold (w) )); }}
  Ok (( all_nodes, all_violations )) }

/// NOT AN ERROR: same-id files across sources. Those are the
/// SECTIONS of one privacy telescope, grouped and folded at load,
/// and they are the feature -- see docs/telescopes.md. Sections of
/// one telescope share a pid, so they can never trip this check.
///
/// THE ERROR: one id claimed by two DIFFERENT nodes -- an id
/// (primary or extra) appearing among the all_ids() of two nodes
/// with distinct pids. Nothing about it is cross-source; both
/// claimants can sit in one source. If any exists, writes a detailed
/// report (to stderr for ≤10, to an org file otherwise) and returns
/// a summary error. (Callers pass post-fold nodes, one per
/// telescope.)
pub fn error_unless_each_id_names_one_node (
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
  let contested: HashMap<ID, Vec<(ID, SourceName)>> =
    claimants . into_iter()
    . filter ( |(_, owners)| {
      let distinct_pids : HashSet<&ID> =
        owners . iter() . map ( |(pid, _)| pid ) . collect();
      distinct_pids . len() > 1 } )
    . collect();
  report_ids_claimed_by_two_nodes (&contested, data_root) ?;
  if contested . is_empty() {
    return Ok (( )); }
  let msg: String =
    if contested . len() <= 10 {
      // Include details in error message for small numbers
      let ids_list: Vec<String> = contested . keys()
        . map ( |id| format! ("'{}'", id) )
        . collect();
      format! ("{} id(s) claimed by more than one node: {}",
               contested . len(),
               ids_list . join (", "))
    } else {
      format! ("{} id(s) claimed by more than one node (see org file)",
               contested . len() ) };
  Err (io::Error::new (
    io::ErrorKind::InvalidData, msg )) }

/// Pure distinct-node claim check for speculative transactions.  Reporting
/// wrappers may write an Org artifact later; candidate validation must not.
pub fn distinct_id_claim_conflicts (
  nodes : &[NodeComplete],
) -> BTreeMap<ID, BTreeSet<ID>> {
  let mut claims : BTreeMap<ID, BTreeSet<ID>> = BTreeMap::new ();
  for node in nodes {
    for id in node . all_ids () {
      claims . entry (id . clone ()) . or_default ()
        . insert (node . pid . clone ()); }}
  claims . retain ( |_, pids| pids . len () > 1);
  claims
}

pub fn read_skg_sections_from_folder (
  source_name : &SourceName,
  config      : &SkgConfig,
) -> io::Result < Vec<(SourceName, NodeFS)> > {
  if ! config . sources . contains_key (source_name) {
    return Err (io::Error::new (
      io::ErrorKind::NotFound,
      format! ("Source '{}' not found in config", source_name))); }
  let mut sections : Vec<(SourceName, NodeFS)> = Vec::new ();
  let selected : SelectedSourceFiles = selected_direct_source_files (config) ?;
  for pid in &selected . pid_order {
    for file in selected . by_pid . get (pid)
      . into_iter () . flatten ()
      .filter ( |file| &file . source == source_name ) {
      let node_fs : NodeFS = read_nodecomplete (&file . path) ?;
      validate_pid_matches_filename (&node_fs, &file . path) ?;
      sections . push ((source_name . clone (), node_fs)); }}
  Ok (sections) }

/// Like `read_all_skg_files_from_sources` but only for telescopes
/// with at least one section file whose mtime is more recent than
/// `since`. A touched SECTION reloads its WHOLE telescope (all its
/// sections, however old), since the fold needs every source.
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
      let Some ((_source, pid)) =
        config . sources . source_and_pid_for_direct_path (&path)
      else { continue; };
      if seen_ids . insert (pid . clone()) {
        modified_pids . push (pid); }} }
  let mut all_nodes : Vec<NodeComplete> = Vec::new();
  for pid in modified_pids {
    all_nodes . push (
      nodecomplete_from_telescope_on_disk (config, &pid) ? ); }
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
fn report_ids_claimed_by_two_nodes(
  contested : &HashMap<ID, Vec<(ID, SourceName)>>,
  data_root : &Path,
) -> io::Result<()> {
  let count: usize = contested . len();
  // DANGER: The report path is fixed per data_root, so two tests sharing a data_root (notably any test using SkgConfig::dummyFromSources,which defaults to ".") can still clobber each other's report.
  let report_path: PathBuf = data_root . join (
    "initialization-error_ids-claimed-by-two-nodes.org");
  if count == 0 {
    return remove_stale_report (&report_path); }
  let claimant_lines = | claimants : &Vec<(ID, SourceName)> |
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
      "{} id(s) claimed by more than one node. Same-id files ACROSS SOURCES are not this: those are the sections of one privacy telescope (docs/telescopes.md). Each id below is claimed, as a primary or extra id, by the distinct nodes listed under it.\n\n",
      count));
    let mut sorted_ids: Vec<(&ID, &Vec<(ID, SourceName)>)> =
      // for deterministic output
      contested . iter() . collect();
    sorted_ids . sort_by_key(|(id, _)| *id);
    for (id, claimants) in sorted_ids {
      content . push_str(&format!("* {}\n", id));
      for line in claimant_lines (claimants) {
        content . push_str(&format!("** {}\n", line)); }}
    content };
  fs::write(&report_path, content)?;
  if count <= 10 {
    tracing::error!("{} id(s) claimed by more than one node:",
              count);
    for (id, claimants) in contested . iter() {
      tracing::error!("  - ID '{}' claimed by: {}",
                id, claimant_lines (claimants) . join (", ")); }
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
  let report_path: PathBuf = data_root . join ( // DANGER: The report path is fixed per data_root, so two tests sharing a data_root (notably any test using SkgConfig::dummyFromSources,which defaults to ".") can still clobber each other's report.

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

/// Writes all given `NodeComplete`s to disk as telescopes: each
/// node's sections land in their source directories, named
/// by the primary ID followed by `.skg`.
pub fn write_all_nodes_to_fs (
  nodes  : Vec<NodeComplete>,
  config : SkgConfig,
) -> io  ::Result<usize> { // number of nodes written
  let prepared : Vec<PreparedTelescopeWrite> =
    nodes . iter ()
    . map ( |node|
      prepare_nodecomplete_telescope (node, &config, false) )
    . collect::<io::Result<Vec<PreparedTelescopeWrite>>> () ?;
  for telescope in &prepared {
    telescope . apply (&config) ?; }
  for telescope in &prepared {
    telescope . verify_hoist (&config) ?; }
  Ok (prepared . len ()) }

/// Deleting a node deletes its whole TELESCOPE: every owned
/// section file of that pid, in whatever source. (The SourceName in
/// each target is the caller's belief about the home; kept in the
/// signature for its callers, but every owned source is swept.)
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
          // No section at this source, which is fine.
        },
        Err (e) => {
          // TODO : Should return a list of IDs not found.
          return Err (e); }} }
    if any_removed {
      deleted += 1; }}
  Ok (deleted) }
