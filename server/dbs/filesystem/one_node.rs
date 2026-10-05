use crate::telescope::compose::compose_telescope;
use crate::telescope::types::{
  Telescope, retain_owned_sections_when_pid_folderlides,
};
use crate::telescope::decompose::{
  DecompositionInput, DecomposedTelescope, decompose_node,
};
use crate::types::misc::{ID, SkgConfig, SkgrepoName, members_msv};
use crate::types::nodes::fs::GraphnodeOnDisk;
use crate::types::nodes::complete::Graphnode;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_skgrepos;
use crate::util::path_from_pid_and_skgrepo;
use std::error::Error;
use std::io;
use std::path::Path;
use std::fs;
use serde_yaml;

pub fn graphnode_from_skgid (
  config : &SkgConfig,
  skgid  : &ID
) -> Result<Graphnode, Box<dyn Error>> {
  let nodes = read_all_skg_files_from_skgrepos (config)?;
  let graph = InRustGraph::from_graphnodes (&nodes);
  let (pid, skgrepo) : (ID, SkgrepoName) =
    graph . pid_and_skgrepo (skgid)
    . ok_or_else ( || format! (
      "ID '{}' not found in graph", skgid ) ) ?;
  Ok ( graphnode_from_pid_and_skgrepo (
    config, pid, &skgrepo )? ) }


/// Reads a Graphnode from disk given its PID: the whole
/// TELESCOPE -- every same-pid section file across the configured
/// skgrepos, composed. The 'repo' parameter survives only as the
/// caller's belief about the home; the composition derives the true home
/// (the most public section), so a stale belief cannot corrupt the
/// read. Extra-id anchor resolution here is
/// identity-only (this telescope's own extra_ids are unknown until
/// read; cross-node merges resolve at the graph layer).
pub fn graphnode_from_pid_and_skgrepo (
  config  : &SkgConfig,
  pid     : ID,
  skgrepo : &SkgrepoName,
) -> io::Result<Graphnode> {
  let Some (telescope) : Option<Telescope> =
    telescope_from_disk (config, &pid) ?
  else {
    return Err ( io::Error::new (
      io::ErrorKind::NotFound,
      format! ("No .skg file for '{}' in any repo (caller expected one in '{}')",
               pid, skgrepo ))); };
  compose_telescope ( telescope, & |skgid : &ID| skgid . clone () ) }

/// PID's telescope as it sits on disk, in privacy order: for each
/// configured skgrepo (most public first), pid.skg if present. The
/// order is what makes the first section the home, so it comes from
/// 'ordered_repos' and nowhere else.
pub(crate) fn telescope_from_disk (
  config : &SkgConfig,
  pid    : &ID,
) -> io::Result<Option<Telescope>> {
  let mut sections : Vec<(SkgrepoName, GraphnodeOnDisk)> = Vec::new ();
  for skgrepo_name in config . ordered_skgrepos () {
    let path : String =
      match path_from_pid_and_skgrepo (
        config, &skgrepo_name, pid . clone () ) {
        Ok (p) => p,
        Err (_) => continue, };
    if ! Path::new (&path) . is_file () { continue; }
    let node_fs : GraphnodeOnDisk = read_graphnode (&path) ?;
    sections . push (( skgrepo_name, node_fs )); }
  if sections . is_empty () {
    return Ok (None); }
  let (sections, collision) =
    retain_owned_sections_when_pid_folderlides (sections, config);
  if let Some (collision) = collision {
    tracing::warn! (
      pid = %pid,
      ignored_skgrepos = ?collision . ignored_skgrepos,
      "owned telescope won a collision with non-owned files" ); }
  Telescope::try_new ( pid . clone (), sections, config )
    . map (Some)
    . map_err ( |e| io::Error::new (
      io::ErrorKind::InvalidData, e ) ) }

/// Reads a node from disk, returning None if not found
/// (either in DB or on filesystem).
/// ERRORS are propagated only if they are not of the 'not found' kind.
pub fn optgraphnode_from_skgid (
  config : &SkgConfig,
  skgid  : &ID
) -> Result<Option<Graphnode>, Box<dyn Error>> {
  match graphnode_from_skgid(
    config, skgid
  ) {
    Ok (graphnode) => Ok(Some (graphnode)),
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
pub fn fetch_aliases_from_file (
  config : &SkgConfig,
  skgid  : ID,
) -> Vec<String> {
  match optgraphnode_from_skgid(
    config, &skgid
  ) {
    Ok ( Some (graphnode)) =>
      members_msv ( & graphnode . aliases ) . into_vec(),
    _ => Vec::new(), }}

/// Write a node as its telescope: decompose into per-repo sections,
/// write each section file only when its bytes changed
/// (no-cosmetic-rewrites), and delete OWNED section files whose
/// skgrepo lost its last member. Foreign skgrepos are never written or
/// deleted -- 'error_unless_home_is_writable' refuses rather than
/// skipping, so a foreign home cannot silently lose the title, and
/// same-pid non-owned files are ignored when an owned telescope
/// exists, so writes cannot absorb or delete their contents.
pub fn write_graphnode_to_skgrepo (
  graphnode : &Graphnode,
  config  : &SkgConfig,
) -> io::Result<()> {
  write_graphnode_telescope (graphnode, config) }

pub fn write_graphnode_telescope (
  graphnode : &Graphnode,
  config       : &SkgConfig,
) -> io::Result<()> {
  let prepared : PreparedTelescopeWrite =
    prepare_graphnode_telescope (graphnode, config, false) ?;
  prepared . apply (config) ?;
  prepared . verify_hoist (config)
}

/// A completely validated and serialized telescope rewrite. Constructing
/// this value performs every fallible shape/ownership/serialization check;
/// applying it is the filesystem-mutation phase.
pub(crate) struct PreparedTelescopeWrite {
  pid             : ID,
  home            : SkgrepoName,
  writes          : Vec<(SkgrepoName, String, String)>,
  deletions       : Vec<String>,
  verify_as_hoist : bool,
}

impl PreparedTelescopeWrite {
  /// Serialized sections for a new telescope. Import checks all configured
  /// paths for absence and writes these with create_new rather than using
  /// the ordinary overwrite/delete mutation path.
  pub(crate) fn creation_files (
    &self,
  ) -> Vec<(std::path::PathBuf, String)> {
    self . writes . iter ()
      . map (|(_, path, yaml)| (std::path::PathBuf::from (path), yaml . clone ()))
      . collect ()
  }

  pub(crate) fn apply (
    &self,
    config : &SkgConfig,
  ) -> io::Result<()> {
    for (skgrepo, path, yaml) in &self . writes {
      assert! ( config . skgrepo_is_owned (skgrepo),
                "write preflight admitted non-owned repo '{}'", skgrepo );
      if let Some (parent) = Path::new (path) . parent () {
        fs::create_dir_all (parent) ?; }
      let unchanged : bool = // byte-stability
        fs::read_to_string (path)
        . map ( |old| old == *yaml )
        . unwrap_or (false);
      if ! unchanged {
        fs::write (path, yaml) ?; }
    }
    for path in &self . deletions {
      match fs::remove_file (path) {
        Ok (( ))                                          => {},
        Err (e) if e . kind () == io::ErrorKind::NotFound => {},
        Err (e)                                           => return Err (e), } }
    Ok (( ))
  }

  /// Hoist is not complete until a fresh disk compose proves that title and
  /// body now select from home. This runs after filesystem writes and before
  /// callers update the in-memory graph or either session-lifetime store.
  pub(crate) fn verify_hoist (
    &self,
    config : &SkgConfig,
  ) -> io::Result<()> {
    if ! self . verify_as_hoist { return Ok (( )); }
    let reread : Graphnode =
      graphnode_from_pid_and_skgrepo (
        config, self . pid . clone (), &self . home ) ?;
    if reread . overPrivateText_telescope {
      return Err ( io::Error::new (
        io::ErrorKind::InvalidData,
        format! (
          "Hoist verification failed for '{}': its freshly reread telescope still selects title or body below home '{}'. The filesystem may have changed, but the in-memory graph and session-lifetime stores were not updated.",
          self . pid, self . home ))); }
    Ok (( ))
  }
}

/// Prepare one telescope rewrite. 'allow_hoist' is deliberately a parameter
/// of this crate-private preparation boundary, not of the ordinary public
/// writer: only the interactive save pipeline may pass true after matching
/// an exact PID approval.
pub(crate) fn prepare_graphnode_telescope (
  graphnode : &Graphnode,
  config       : &SkgConfig,
  allow_hoist  : bool,
) -> io::Result<PreparedTelescopeWrite> {
  let pid : &ID = &graphnode . pid;
  let verify_as_hoist : bool =
    error_unless_home_is_writable (
      graphnode, config, allow_hoist ) ?;
  let decomposed : DecomposedTelescope =
    decompose_node (
      & DecompositionInput {
        pid      : pid,
        extra_ids : & graphnode . extra_ids,
        flags    : & graphnode . flags,
        title    : Some ( & graphnode . title ),
        body     : graphnode . body . as_deref (),
        home     : & graphnode . home_skgrepo,
        aliases  : graphnode . aliases . or_default (),
        contains : & graphnode . contains,
        subscribesTo :
          graphnode . subscribesTo . or_default (),
        hidesFromSubs :
          graphnode . hidesFromSubs . or_default (),
        overrides :
          graphnode . overrides . or_default (), },
      config )
    . map_err ( |e| io::Error::new (
      io::ErrorKind::InvalidData, e ) ) ?;

  let mut offending_skgrepos : Vec<SkgrepoName> = decomposed . sections ()
    . iter ()
    .map ( |(skgrepo, _)| skgrepo )
    . filter ( |skgrepo| ! config . skgrepo_is_owned (skgrepo) )
    . cloned ()
    . collect ();
  offending_skgrepos . sort ();
  offending_skgrepos . dedup ();
  if ! offending_skgrepos . is_empty () {
    return Err ( io::Error::new (
      io::ErrorKind::PermissionDenied,
      format! (
        "Refusing to write '{}': proposed telescope section(s) belong to non-owned repo(s) [{}]. No files were changed.",
        pid,
        offending_skgrepos . iter ()
          . map ( |skgrepo| format! ("'{}'", skgrepo) )
          . collect::<Vec<String>> () . join (", ") ))); }

  let mut prepared_writes : Vec<(SkgrepoName, String, String)> =
    Vec::new ();
  for (skgrepo, node_fs) in decomposed . sections () {
    let path : String =
      path_from_pid_and_skgrepo ( config, skgrepo, pid . clone () )
      . map_err ( |e| io::Error::new (
        io::ErrorKind::NotFound, e) ) ?;
    let yaml : String =
      node_fs . to_yaml ()
      . map_err ( |e| io::Error::new (
        io::ErrorKind::InvalidData, e . to_string () )) ?;
    prepared_writes . push (( skgrepo . clone (), path, yaml )); }
  let written_skgrepos : Vec<SkgrepoName> = prepared_writes . iter ()
    . map ( |(skgrepo, _, _)| skgrepo . clone () )
    . collect ();
  let mut prepared_deletions : Vec<String> = Vec::new ();
  for skgrepo in config . ordered_skgrepos () {
    if written_skgrepos . contains (&skgrepo) { continue; }
    if ! config . skgrepo_is_owned (&skgrepo) { continue; }
    let path : String = path_from_pid_and_skgrepo (
      config, &skgrepo, pid . clone () )
      . map_err ( |e| io::Error::new (
        io::ErrorKind::NotFound, e) ) ?;
    prepared_deletions . push (path); }

  Ok ( PreparedTelescopeWrite {
    pid             : pid . clone (),
    home            : graphnode . home_skgrepo . clone (),
    writes          : prepared_writes,
    deletions       : prepared_deletions,
    verify_as_hoist,
  } ) }


/// The two shapes 'write_graphnode_telescope' refuses, because
/// writing either would publish or destroy the node's text. Both
/// are unreachable through skg's own saves -- 'apply_sticky_relRepos'
/// clamps every relRepo to at least the recorder's home, so no save
/// creates a section more public than the home -- and arrive only
/// from hand-edited files, a pull, or a foreign overlay.
///
/// The nodes this blocks are already broken; the composition reports both
/// shapes in telescope-warnings.org with their repairs.
fn error_unless_home_is_writable (
  graphnode : &Graphnode,
  config       : &SkgConfig,
  allow_hoist  : bool,
) -> io::Result<bool> {
  let home : &SkgrepoName = &graphnode . home_skgrepo;
  if ! config . skgrepo_is_owned (home) {
    // FOREIGN HOME. Foreign sections are never written. Skipping
    // the home silently would drop the title on the floor, so
    // refuse instead. (This function is what makes the promise in
    // this module's write doc-comment true of the writer itself;
    // it was previously kept only by the writer's callers.)
    return Err ( io::Error::new (
      io::ErrorKind::PermissionDenied,
      format! (
        "Refusing to write '{}': its home is '{}', which you do not own. Foreign sections are never written, so this node cannot be saved from here. See the foreign-overlay entry in telescope-warnings.org.",
        graphnode . pid, home ))); }
  // TEXT HOIST. Compose the current disk telescope with the same title/body
  // selection used by load. Looking only for a titleless home misses the
  // equally sensitive shape "title at home, body below home".
  let disk_is_overPrivateText : bool =
    match telescope_from_disk (config, &graphnode . pid) ? {
      None => false,
      Some (telescope) => match
        compose_telescope ( telescope, & |skgid : &ID| skgid . clone () ) {
          Ok (disk_node) => disk_node . overPrivateText_telescope,
          Err (error) => return Err ( io::Error::new (
            io::ErrorKind::InvalidData,
            format! (
              "Refusing to write '{}': its current disk telescope cannot select a title ({}), so writing the buffer's text at home '{}' would publish it without a verifiable Hoist candidate. Repair the .skg sections by hand. See telescope-warnings.org.",
              graphnode . pid, error, home ))), }, };
  if disk_is_overPrivateText && ! allow_hoist {
      return Err ( io::Error::new (
        io::ErrorKind::InvalidData,
        format! (
          "Refusing to write '{}': its current disk telescope selects title or body below home '{}', so this write would publish it. An interactive save must obtain explicit Hoist approval for this PID; otherwise repair the .skg sections by hand. See telescope-warnings.org.",
          graphnode . pid, home ))); }
  Ok (disk_is_overPrivateText) }

/// Checks that a node's primary ID matches the filename stem.
/// This property is assumed by `path_from_pid_and_repo` and
/// elsewhere but was never validated on read.
pub(super) fn validate_pid_matches_filename (
  node : &GraphnodeOnDisk,
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
/// Returns a GraphnodeOnDisk (on-disk shape, no skgrepo). Callers attach
/// skgrepo via 'GraphnodeOnDisk::into_complete' based on file location.
pub(super) fn read_graphnode
  <P : AsRef <Path>> // any type that can be converted to an &Path
  (file_path : P
  ) -> io::Result <GraphnodeOnDisk> {

  let file_path : &Path = file_path . as_ref ();
  let node_fs   : GraphnodeOnDisk = {
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
