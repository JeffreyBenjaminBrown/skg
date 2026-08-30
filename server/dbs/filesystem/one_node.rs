use crate::telescope::fold::fold_telescope;
use crate::telescope::types::{
  Telescope,
};
use crate::dbs::filesystem::source_files::{
  IgnoredForeignPathCollision, SourceFile,
  selected_direct_source_files_for_pid,
};
use crate::telescope::unfold::{
  UnfoldInput, UnfoldedTelescope, unfold_node,
};
use crate::types::misc::{ID, SkgConfig, SourceName, members_msv};
use crate::types::nodes::fs::NodeFS;
use crate::types::nodes::complete::NodeComplete;
use crate::types::store_state::{PathDigest, SelectedPathManifest};
use crate::dbs::typedb::search::pid_and_source_from_id;
use crate::util::path_from_pid_and_source;
use std::error::Error;
use std::io;
use std::path::{Path, PathBuf};
use std::collections::BTreeMap;
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
  let (selected, collision)
    : (Vec<SourceFile>, Option<IgnoredForeignPathCollision>) =
    selected_direct_source_files_for_pid (config, pid) ?;
  let mut sections : Vec<(SourceName, NodeFS)> = Vec::new ();
  for file in selected {
    let node_fs : NodeFS = read_nodecomplete (&file . path) ?;
    validate_pid_matches_filename (&node_fs, &file . path) ?;
    sections . push (( file . source, node_fs )); }
  if sections . is_empty () {
    return Ok (None); }
  if let Some (collision) = collision {
    tracing::warn! (
      pid = %pid,
      winners = ?collision . winners,
      losers = ?collision . losers,
      "owned telescope won a collision with non-owned files" ); }
  Telescope::try_new ( pid . clone (), sections, config )
    . map (Some)
    . map_err ( |e| io::Error::new (
      io::ErrorKind::InvalidData, e ) ) }

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

/// Serialize the complete graph telescope without writing it. Every
/// configured section path is explicit: `Some(bytes)` for a section emitted
/// by unfold, `None` for its tombstone. Recovery uses the same bytes as save
/// rather than inventing a second graph-to-YAML implementation.
pub(crate) fn serialize_telescope_manifest (
  nodecomplete : &NodeComplete,
  config       : &SkgConfig,
) -> io::Result<BTreeMap<PathBuf, Option<Vec<u8>>>> {
  let writes = serialize_telescope_sections (nodecomplete, config)?;
  let mut manifest : BTreeMap<PathBuf, Option<Vec<u8>>> = config
    . ordered_sources () . into_iter () . map (|source| {
      let configured = config . sources . get (&source)
        . expect ("ordered source exists");
      (configured . path . join (format! ("{}.skg", nodecomplete . pid)),
       None)
    }) . collect ();
  for (_, path, yaml) in writes {
    manifest . insert (PathBuf::from (path), Some (yaml . into_bytes ())); }
  Ok (manifest)
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

  /// Apply this already-serialized write set to the manifest which described
  /// the pre-write graph selection.  Digests come from the exact YAML bytes
  /// passed to `fs::write`, never from a later restat of the live path.
  pub(crate) fn apply_to_manifest (
    &self,
    manifest : &mut SelectedPathManifest,
  ) {
    for (_, path, yaml) in &self . writes {
      manifest . insert (
        path . into (), PathDigest::of_bytes (yaml . as_bytes ())); }
    for path in &self . deletions {
      manifest . remove (Path::new (path)); }
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
    if reread . ugly_telescope {
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
  let verify_as_hoist : bool =
    error_unless_home_is_writable (
      nodecomplete, config, allow_hoist ) ?;
  let prepared_writes = serialize_telescope_sections (nodecomplete, config)?;
  let offending_sources : Vec<SourceName> = {
    let mut sources : Vec<SourceName> = prepared_writes . iter ()
      . map (|(source, _, _)| source)
      . filter (|source| !config . user_owns_source (source))
      . cloned () . collect ();
    sources . sort ();
    sources . dedup ();
    sources
  };
  if ! offending_sources . is_empty () {
    return Err ( io::Error::new (
      io::ErrorKind::PermissionDenied,
      format! (
        "Refusing to write '{}': proposed telescope section(s) belong to non-owned source(s) [{}]. No files were changed.",
        nodecomplete . pid,
        offending_sources . iter ()
          . map ( |source| format! ("'{}'", source) )
          . collect::<Vec<String>> () . join (", ") ))); }
  let written_sources : Vec<SourceName> = prepared_writes . iter ()
    . map ( |(source, _, _)| source . clone () )
    . collect ();
  let mut prepared_deletions : Vec<String> = Vec::new ();
  for source in config . ordered_sources () {
    if written_sources . contains (&source) { continue; }
    if ! config . user_owns_source (&source) { continue; }
    let path : String = path_from_pid_and_source (
      config, &source, nodecomplete . pid . clone () )
      . map_err ( |e| io::Error::new (
        io::ErrorKind::NotFound, e) ) ?;
    prepared_deletions . push (path); }

  Ok ( PreparedTelescopeWrite {
    pid             : nodecomplete . pid . clone (),
    home            : nodecomplete . source . clone (),
    writes          : prepared_writes,
    deletions       : prepared_deletions,
    verify_as_hoist,
  } ) }

fn serialize_telescope_sections (
  nodecomplete : &NodeComplete,
  config       : &SkgConfig,
) -> io::Result<Vec<(SourceName, String, String)>> {
  let pid = &nodecomplete . pid;
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
  Ok (prepared_writes)
}


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
  // SCALAR HOIST. Fold the current disk telescope with the same title/body
  // selection used by load. Looking only for a titleless home misses the
  // equally sensitive shape "title at home, body below home".
  let disk_is_ugly : bool =
    match telescope_from_disk (config, &nodecomplete . pid) ? {
      None => false,
      Some (telescope) => match
        fold_telescope ( telescope, & |id : &ID| id . clone () ) {
          Ok (disk_node) => disk_node . ugly_telescope,
          Err (error) => return Err ( io::Error::new (
            io::ErrorKind::InvalidData,
            format! (
              "Refusing to write '{}': its current disk telescope cannot select a title ({}), so writing the buffer's text at home '{}' would publish it without a verifiable Hoist candidate. Repair the .skg sections by hand. See telescope-warnings.org.",
              nodecomplete . pid, error, home ))), }, };
  if disk_is_ugly && ! allow_hoist {
      return Err ( io::Error::new (
        io::ErrorKind::InvalidData,
        format! (
          "Refusing to write '{}': its current disk telescope selects title or body below home '{}', so this write would publish it. An interactive save must obtain explicit Hoist approval for this PID; otherwise repair the .skg sections by hand. See telescope-warnings.org.",
          nodecomplete . pid, home ))); }
  Ok (disk_is_ugly) }

/// Checks that a node's primary ID matches the filename stem.
/// This property is assumed by `path_from_pid_and_source` and
/// elsewhere but was never validated on read.
pub(crate) fn validate_pid_matches_filename (
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
  let bytes : Vec<u8> = fs::read (file_path) ?;
  parse_nodefs_bytes (&bytes, file_path)
}

/// Read and parse once while retaining the exact bytes which selected the
/// section.  Store transactions hash these bytes; they must never reopen the
/// path after parsing and call the newer contents "selected".
pub(super) fn read_nodefs_with_bytes
  <P : AsRef<Path>>
  (file_path : P
  ) -> io::Result<(NodeFS, Vec<u8>)> {
  let file_path : &Path = file_path . as_ref ();
  let bytes : Vec<u8> = fs::read (file_path) ?;
  let node_fs = parse_nodefs_bytes (&bytes, file_path) ?;
  Ok ((node_fs, bytes))
}

pub(crate) fn parse_nodefs_bytes (
  bytes     : &[u8],
  file_path : &Path,
) -> io::Result<NodeFS> {
  let node_fs : NodeFS =
    serde_yaml::from_slice (bytes)
    . map_err (
      |e| io::Error::new (
        io::ErrorKind::InvalidData,
        e . to_string () )) ?;
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
