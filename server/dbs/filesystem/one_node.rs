use crate::telescope::fold::fold_telescope;
use crate::telescope::types::{
  Telescope, retain_owned_sections_when_pid_collides,
};
use crate::telescope::unfold::{
  UnfoldInput, UnfoldedTelescope, unfold_node,
};
use crate::types::misc::{ID, SkgConfig, SourceName, members_msv};
use crate::types::nodes::fs::NodeFS;
use crate::types::nodes::complete::NodeComplete;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use crate::util::path_from_pid_and_source;
use std::error::Error;
use std::io;
use std::path::Path;
use std::fs;
use serde_yaml;

pub fn nodecomplete_from_id (
  config : &SkgConfig,
  skgid  : &ID
) -> Result<NodeComplete, Box<dyn Error>> {
  let nodes = read_all_skg_files_from_sources (config)?;
  let graph = InRustGraph::from_nodecompletes (&nodes);
  let (pid, source) : (ID, SourceName) =
    graph . pid_and_source (skgid)
    . ok_or_else ( || format! (
      "ID '{}' not found in graph", skgid ) ) ?;
  Ok ( nodecomplete_from_pid_and_source (
    config, pid, &source )? ) }


/// Reads a NodeComplete from disk given its PID: the whole
/// TELESCOPE -- every same-pid section file across the configured
/// sources, folded. The 'source' parameter survives only as the
/// caller's belief about the home; the fold derives the true home
/// (the most public section), so a stale belief cannot corrupt the
/// read. Extra-id anchor resolution here is
/// identity-only (this telescope's own extra_ids are unknown until
/// read; cross-node merges resolve at the graph layer).
pub fn nodecomplete_from_pid_and_source (
  config : &SkgConfig,
  pid    : ID,
  source : &SourceName,
) -> io::Result<NodeComplete> {
  let Some (telescope) : Option<Telescope> =
    telescope_from_disk (config, &pid) ?
  else {
    return Err ( io::Error::new (
      io::ErrorKind::NotFound,
      format! ("No .skg file for '{}' in any source (caller expected one in '{}')",
               pid, source ))); };
  fold_telescope ( telescope, & |id : &ID| id . clone () ) }

/// PID's telescope as it sits on disk, in privacy order: for each
/// configured source (most public first), pid.skg if present. The
/// order is what makes the first section the home, so it comes from
/// 'ordered_sources' and nowhere else.
pub(crate) fn telescope_from_disk (
  config : &SkgConfig,
  pid    : &ID,
) -> io::Result<Option<Telescope>> {
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
  if sections . is_empty () {
    return Ok (None); }
  let (sections, collision) =
    retain_owned_sections_when_pid_collides (sections, config);
  if let Some (collision) = collision {
    tracing::warn! (
      pid = %pid,
      ignored_sources = ?collision . ignored_sources,
      "owned telescope won a collision with non-owned files" ); }
  Telescope::try_new ( pid . clone (), sections, config )
    . map (Some)
    . map_err ( |e| io::Error::new (
      io::ErrorKind::InvalidData, e ) ) }

/// Reads a node from disk, returning None if not found
/// (either in DB or on filesystem).
/// ERRORS are propagated only if they are not of the 'not found' kind.
pub fn optnodecomplete_from_id (
  config : &SkgConfig,
  skgid  : &ID
) -> Result<Option<NodeComplete>, Box<dyn Error>> {
  match nodecomplete_from_id(
    config, skgid
  ) {
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
pub fn fetch_aliases_from_file (
  config : &SkgConfig,
  skgid  : ID,
) -> Vec<String> {
  match optnodecomplete_from_id(
    config, &skgid
  ) {
    Ok ( Some (nodecomplete)) =>
      members_msv ( & nodecomplete . aliases ) . into_vec(),
    _ => Vec::new(), }}

/// Write a node as its telescope: unfold into per-source sections,
/// write each section file only when its bytes changed
/// (no-cosmetic-rewrites), and delete OWNED section files whose
/// source lost its last member. Foreign sources are never written or
/// deleted -- 'error_unless_home_is_writable' refuses rather than
/// skipping, so a foreign home cannot silently lose the title, and
/// same-pid non-owned files are ignored when an owned telescope
/// exists, so writes cannot absorb or delete their contents.
pub fn write_nodecomplete_to_source (
  nodecomplete : &NodeComplete,
  config  : &SkgConfig,
) -> io::Result<()> {
  write_nodecomplete_telescope (nodecomplete, config) }

pub fn write_nodecomplete_telescope (
  nodecomplete : &NodeComplete,
  config       : &SkgConfig,
) -> io::Result<()> {
  let prepared : PreparedTelescopeWrite =
    prepare_nodecomplete_telescope (nodecomplete, config, false) ?;
  prepared . apply (config) ?;
  prepared . verify_hoist (config)
}

/// A completely validated and serialized telescope rewrite. Constructing
/// this value performs every fallible shape/ownership/serialization check;
/// applying it is the filesystem-mutation phase.
pub(crate) struct PreparedTelescopeWrite {
  pid             : ID,
  home            : SourceName,
  writes          : Vec<(SourceName, String, String)>,
  deletions       : Vec<String>,
  verify_as_hoist : bool,
}

impl PreparedTelescopeWrite {
  pub(crate) fn apply (
    &self,
    config : &SkgConfig,
  ) -> io::Result<()> {
    for (source, path, yaml) in &self . writes {
      assert! ( config . user_owns_source (source),
                "write preflight admitted non-owned source '{}'", source );
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

  /// Hoist is not complete until a fresh disk fold proves that title and
  /// body now select from home. This runs after filesystem writes and before
  /// callers update the in-memory graph or either derived database.
  pub(crate) fn verify_hoist (
    &self,
    config : &SkgConfig,
  ) -> io::Result<()> {
    if ! self . verify_as_hoist { return Ok (( )); }
    let reread : NodeComplete =
      nodecomplete_from_pid_and_source (
        config, self . pid . clone (), &self . home ) ?;
    if reread . overPrivateText_telescope {
      return Err ( io::Error::new (
        io::ErrorKind::InvalidData,
        format! (
          "Hoist verification failed for '{}': its freshly reread telescope still selects title or body below home '{}'. The filesystem may have changed, but the in-memory graph and derived databases were not updated.",
          self . pid, self . home ))); }
    Ok (( ))
  }
}

/// Prepare one telescope rewrite. 'allow_hoist' is deliberately a parameter
/// of this crate-private preparation boundary, not of the ordinary public
/// writer: only the interactive save pipeline may pass true after matching
/// an exact PID approval.
pub(crate) fn prepare_nodecomplete_telescope (
  nodecomplete : &NodeComplete,
  config       : &SkgConfig,
  allow_hoist  : bool,
) -> io::Result<PreparedTelescopeWrite> {
  let pid : &ID = &nodecomplete . pid;
  let verify_as_hoist : bool =
    error_unless_home_is_writable (
      nodecomplete, config, allow_hoist ) ?;
  let unfolded : UnfoldedTelescope =
    unfold_node (
      & UnfoldInput {
        pid      : pid,
        extra_ids : & nodecomplete . extra_ids,
        misc      : & nodecomplete . misc,
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
      config )
    . map_err ( |e| io::Error::new (
      io::ErrorKind::InvalidData, e ) ) ?;

  let mut offending_sources : Vec<SourceName> = unfolded . sections ()
    . iter ()
    .map ( |(source, _)| source )
    . filter ( |source| ! config . user_owns_source (source) )
    . cloned ()
    . collect ();
  offending_sources . sort ();
  offending_sources . dedup ();
  if ! offending_sources . is_empty () {
    return Err ( io::Error::new (
      io::ErrorKind::PermissionDenied,
      format! (
        "Refusing to write '{}': proposed telescope section(s) belong to non-owned source(s) [{}]. No files were changed.",
        pid,
        offending_sources . iter ()
          . map ( |source| format! ("'{}'", source) )
          . collect::<Vec<String>> () . join (", ") ))); }

  let mut prepared_writes : Vec<(SourceName, String, String)> =
    Vec::new ();
  for (source, node_fs) in unfolded . sections () {
    let path : String =
      path_from_pid_and_source ( config, source, pid . clone () )
      . map_err ( |e| io::Error::new (
        io::ErrorKind::NotFound, e) ) ?;
    let yaml : String =
      node_fs . to_yaml ()
      . map_err ( |e| io::Error::new (
        io::ErrorKind::InvalidData, e . to_string () )) ?;
    prepared_writes . push (( source . clone (), path, yaml )); }
  let written_sources : Vec<SourceName> = prepared_writes . iter ()
    . map ( |(source, _, _)| source . clone () )
    . collect ();
  let mut prepared_deletions : Vec<String> = Vec::new ();
  for source in config . ordered_sources () {
    if written_sources . contains (&source) { continue; }
    if ! config . user_owns_source (&source) { continue; }
    let path : String = path_from_pid_and_source (
      config, &source, pid . clone () )
      . map_err ( |e| io::Error::new (
        io::ErrorKind::NotFound, e) ) ?;
    prepared_deletions . push (path); }

  Ok ( PreparedTelescopeWrite {
    pid             : pid . clone (),
    home            : nodecomplete . source . clone (),
    writes          : prepared_writes,
    deletions       : prepared_deletions,
    verify_as_hoist,
  } ) }


/// The two shapes 'write_nodecomplete_telescope' refuses, because
/// writing either would publish or destroy the node's text. Both
/// are unreachable through skg's own saves -- 'apply_sticky_sources'
/// clamps every recording source to at least the owner's home, so no save
/// creates a section more public than the home -- and arrive only
/// from hand-edited files, a pull, or a foreign overlay.
///
/// The nodes this blocks are already broken; the fold reports both
/// shapes in telescope-warnings.org with their repairs.
fn error_unless_home_is_writable (
  nodecomplete : &NodeComplete,
  config       : &SkgConfig,
  allow_hoist  : bool,
) -> io::Result<bool> {
  let home : &SourceName = &nodecomplete . source;
  if ! config . user_owns_source (home) {
    // FOREIGN HOME. Foreign sections are never written. Skipping
    // the home silently would drop the title on the floor, so
    // refuse instead. (This function is what makes the promise in
    // this module's write doc-comment true of the writer itself;
    // it was previously kept only by the writer's callers.)
    return Err ( io::Error::new (
      io::ErrorKind::PermissionDenied,
      format! (
        "Refusing to write '{}': its home is '{}', which you do not own. Foreign sections are never written, so this node cannot be saved from here. See the foreign-overlay entry in telescope-warnings.org.",
        nodecomplete . pid, home ))); }
  // TEXT HOIST. Fold the current disk telescope with the same title/body
  // selection used by load. Looking only for a titleless home misses the
  // equally sensitive shape "title at home, body below home".
  let disk_is_overPrivateText : bool =
    match telescope_from_disk (config, &nodecomplete . pid) ? {
      None => false,
      Some (telescope) => match
        fold_telescope ( telescope, & |id : &ID| id . clone () ) {
          Ok (disk_node) => disk_node . overPrivateText_telescope,
          Err (error) => return Err ( io::Error::new (
            io::ErrorKind::InvalidData,
            format! (
              "Refusing to write '{}': its current disk telescope cannot select a title ({}), so writing the buffer's text at home '{}' would publish it without a verifiable Hoist candidate. Repair the .skg sections by hand. See telescope-warnings.org.",
              nodecomplete . pid, error, home ))), }, };
  if disk_is_overPrivateText && ! allow_hoist {
      return Err ( io::Error::new (
        io::ErrorKind::InvalidData,
        format! (
          "Refusing to write '{}': its current disk telescope selects title or body below home '{}', so this write would publish it. An interactive save must obtain explicit Hoist approval for this PID; otherwise repair the .skg sections by hand. See telescope-warnings.org.",
          nodecomplete . pid, home ))); }
  Ok (disk_is_overPrivateText) }

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
