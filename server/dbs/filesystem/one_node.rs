use crate::telescope::fold::{fold_sections, nodecomplete_from_fold};
use crate::telescope::types::SectionSlices;
use crate::telescope::unfold::{UnfoldInput, unfold_node};
use crate::types::misc::{ID, SkgConfig, SourceName, members_msv};
use crate::types::nodes::fs::{NodeFS, nodefs_from_section};
use crate::types::nodes::complete::NodeComplete;
use crate::dbs::typedb::search::pid_and_source_from_id;
use crate::util::path_from_pid_and_source;
use std::error::Error;
use std::io;
use std::path::Path;
use std::fs;
use serde_yaml;
use typedb_driver::TypeDBDriver;

pub async fn nodecomplete_from_id (
  config : &SkgConfig,
  driver : &TypeDBDriver,
  skgid  : &ID
) -> Result<NodeComplete, Box<dyn Error>> {
  let (pid, source) : (ID, SourceName) =
    pid_and_source_from_id (
      & config . db_name, driver, skgid
    ) . await ?
    . ok_or_else ( || format! (
      "ID '{}' not found in database", skgid ) ) ?;
  Ok ( nodecomplete_from_pid_and_source (
    config, pid, &source )? ) }


/// Reads a NodeComplete from disk given its PID: the whole
/// TELESCOPE -- every same-pid section file across the configured
/// sources, folded. The 'source' parameter survives only as the
/// caller's belief about the home; the fold derives the true home
/// (the most public titled section), so a stale belief cannot
/// corrupt the read. Extra-id anchor resolution here is
/// identity-only (this telescope's own extra_ids are unknown until
/// read; cross-node merges resolve at the graph layer).
pub fn nodecomplete_from_pid_and_source (
  config : &SkgConfig,
  pid    : ID,
  source : &SourceName,
) -> io::Result<NodeComplete> {
  let sections : Vec<(SourceName, NodeFS)> =
    read_telescope_sections (config, &pid) ?;
  if sections . is_empty () {
    return Err ( io::Error::new (
      io::ErrorKind::NotFound,
      format! ("No .skg file for '{}' in any source (caller expected one in '{}')",
               pid, source ))); }
  let extra_ids : Vec<ID> = {
    // gathered before folding, from every section
    let mut extra_ids : Vec<ID> = Vec::new ();
    for (_, node_fs) in &sections {
      for e in &node_fs . extra_ids {
        if ! extra_ids . contains (e) {
          extra_ids . push ( e . clone () ); }} }
    extra_ids };
  let misc : Vec<crate::types::nodes::complete::FileProperty> = {
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
  let (folded, warnings) =
    fold_sections ( &slices, & |id : &ID| id . clone () );
  for w in &warnings {
    tracing::warn! ( pid = %pid, warning = ?w,
                     "telescope fold warning (single-node read)" ); }
  nodecomplete_from_fold ( pid . clone (), extra_ids, misc, folded )
    . ok_or_else ( || io::Error::new (
      io::ErrorKind::InvalidData,
      format! ("Telescope '{}' has no home: no section carries a title.",
               pid ))) }

/// Every section of PID's telescope, in privacy order: for each
/// configured source (most public first), pid.skg if present.
fn read_telescope_sections (
  config : &SkgConfig,
  pid    : &ID,
) -> io::Result<Vec<(SourceName, NodeFS)>> {
  let mut sections : Vec<(SourceName, NodeFS)> = Vec::new ();
  for source_name in config . ordered_sources () {
    let path : String =
      match path_from_pid_and_source (
        config, &source_name, pid . clone () ) {
        Ok (p) => p,
        Err (_) => continue, };
    if ! Path::new (&path) . is_file () { continue; }
    let node_fs : NodeFS = read_nodecomplete (&path) ?;
    sections . push (( source_name, node_fs )); }
  Ok (sections) }

/// Reads a node from disk, returning None if not found
/// (either in DB or on filesystem).
/// ERRORS are propagated only if they are not of the 'not found' kind.
pub async fn optnodecomplete_from_id (
  config : &SkgConfig,
  driver : &TypeDBDriver,
  skgid  : &ID
) -> Result<Option<NodeComplete>, Box<dyn Error>> {
  match nodecomplete_from_id(
    config, driver, skgid
  ) . await {
    Ok (nodecomplete) => Ok(Some (nodecomplete)),
    Err (e)      => {
      let error_msg: String = e . to_string();
      if error_msg . contains ("not found")
        || error_msg . contains ("No such file")
        || error_msg . contains ("does not exist") {
          // TODO : This is kludgey. Find a better way to test for this kind of error.
          Ok (None) }
      else { Err (e) }} }}

/// If there's no such .skg file at path,
/// returns the empty vector.
pub async fn fetch_aliases_from_file (
  config : &SkgConfig,
  driver : &TypeDBDriver,
  skgid  : ID,
) -> Vec<String> {
  match optnodecomplete_from_id(
    config, driver, &skgid
  ) . await {
    Ok ( Some (nodecomplete)) =>
      members_msv ( & nodecomplete . aliases ) . into_vec(),
    _ => Vec::new(), }}

/// Write a node as its telescope: unfold into per-level sections,
/// write each section file only when its bytes changed
/// (no-cosmetic-rewrites), and delete OWNED section files whose
/// level lost its last member. Foreign sources are never written or
/// deleted: a foreign same-pid file is the forbidden-overlay shape,
/// left for the validators to report.
pub fn write_nodecomplete_to_source (
  nodecomplete : &NodeComplete,
  config  : &SkgConfig,
) -> io::Result<()> {
  write_nodecomplete_telescope (nodecomplete, config) }

pub fn write_nodecomplete_telescope (
  nodecomplete : &NodeComplete,
  config       : &SkgConfig,
) -> io::Result<()> {
  let pid : &ID = &nodecomplete . pid;
  let sections : Vec<(SourceName, SectionSlices)> =
    unfold_node (
      & UnfoldInput {
        title    : Some ( & nodecomplete . title ),
        body     : nodecomplete . body . as_deref (),
        home     : & nodecomplete . source,
        aliases  : nodecomplete . aliases . or_default (),
        contains : & nodecomplete . contains,
        subscribes_to :
          nodecomplete . subscribes_to . or_default (),
        hides_from_its_subscriptions :
          nodecomplete . hides_from_its_subscriptions . or_default (),
        overrides_view_of :
          nodecomplete . overrides_view_of . or_default (), },
      & |level : &SourceName| config . source_position (level) );
  let mut levels_written : Vec<SourceName> = Vec::new ();
  for (level, slices) in sections {
    let is_home : bool = level == nodecomplete . source;
    let node_fs : NodeFS = nodefs_from_section (
      pid, & nodecomplete . extra_ids,
      & nodecomplete . misc, is_home, slices );
    let path : String =
      path_from_pid_and_source ( config, &level, pid . clone () )
      . map_err ( |e| io::Error::new (
        io::ErrorKind::NotFound, e) ) ?;
    let yaml : String =
      node_fs . to_yaml ()
      . map_err ( |e| io::Error::new (
        io::ErrorKind::InvalidData, e . to_string () )) ?;
    let unchanged : bool = // byte-stability
      fs::read_to_string (&path)
      . map ( |old| old == yaml )
      . unwrap_or (false);
    if ! unchanged {
      fs::write ( &path, yaml ) ?; }
    levels_written . push (level); }
  for source_name in config . ordered_sources () {
    // Remove OWNED sections this write emptied.
    if levels_written . contains (&source_name) { continue; }
    if ! config . user_owns_source (&source_name) { continue; }
    if let Ok (path) = path_from_pid_and_source (
      config, &source_name, pid . clone () ) {
      match fs::remove_file (&path) {
        Ok (( ))                                            => {},
        Err (e) if e . kind () == io::ErrorKind::NotFound   => {},
        Err (e)                                             =>
          return Err (e), }} }
  Ok (( )) }


/// Checks that a node's primary ID matches the filename stem.
/// This property is assumed by `path_from_pid_and_source` and
/// elsewhere but was never validated on read.
pub(super) fn validate_pid_matches_filename (
  node : &NodeFS,
  path : &Path,
) -> io::Result<()> {
  let pid : &ID = &node . pid;
  let stem : &str = path . file_stem()
    . and_then ( |s| s . to_str() )
    . ok_or_else ( || io::Error::new (
      io::ErrorKind::InvalidData,
      format! ("Cannot extract filename stem from {:?}",
               path )) ) ?;
  if pid . as_str() != stem {
    return Err ( io::Error::new (
      io::ErrorKind::InvalidData,
      format! (
        "PID '{}' does not match filename stem '{}' in {:?}",
        pid . as_str(), stem, path )) ); }
  Ok (( )) }

/// Effectively private.
///
/// Returns a NodeFS (on-disk shape, no source). Callers attach
/// source via 'NodeFS::into_complete' based on file location.
pub(super) fn read_nodecomplete
  <P : AsRef <Path>> // any type that can be converted to an &Path
  (file_path : P
  ) -> io::Result <NodeFS> {

  let file_path : &Path = file_path . as_ref ();
  let node_fs   : NodeFS = {
    let contents : String = fs::read_to_string (file_path)?;
    serde_yaml::from_str (&contents)
    . map_err (
      |e| io::Error::new (
        io::ErrorKind::InvalidData,
        e . to_string () )) ? };
  if node_fs . title . as_deref () == Some ("") {
    // Absent title = a non-home section, fine; PRESENT-but-empty is
    // malformed.
    return Err(io::Error::new(
      io::ErrorKind::InvalidData,
      format!("Section at {:?} has an empty title", file_path),
    )); }
  if node_fs . pid . as_str() . is_empty() {
    return Err(io::Error::new(
      io::ErrorKind::InvalidData,
      format!(".skg file at {:?} has no IDs", file_path),
    )); }
  Ok (node_fs) }
