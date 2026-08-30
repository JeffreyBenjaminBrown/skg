//! Authoritative selection of direct source files before their bytes are
//! interpreted as YAML.
//!
//! The filename identifies the possible telescope.  If any configured owned
//! source has that filename, all foreign candidates are opaque losers: they
//! are reported, but never opened.  This boundary must therefore precede
//! every parser used by bulk load, one-pid load, reload, diff, or recovery.

use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::store_state::{PathDigest, SelectedPathManifest};

use std::collections::{HashMap, HashSet};
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SourceFile {
  pub source : SourceName,
  pub path   : PathBuf,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IgnoredForeignPathCollision {
  pub pid     : ID,
  pub winners : Vec<SourceFile>,
  pub losers  : Vec<SourceFile>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SelectedSourceFiles {
  pub pid_order  : Vec<ID>,
  pub by_pid     : HashMap<ID, Vec<SourceFile>>,
  pub collisions : Vec<IgnoredForeignPathCollision>,
}

/// Enumerate every regular direct `.skg` child, group by filename PID, and
/// apply owned/foreign selection without opening any candidate file.
pub fn selected_direct_source_files (
  config : &SkgConfig,
) -> io::Result<SelectedSourceFiles> {
  let mut candidates : Vec<(ID, SourceFile)> = Vec::new ();
  for source_name in config . ordered_sources () {
    let source = config . sources . get (&source_name)
      . expect ("ordered source exists");
    let mut paths : Vec<PathBuf> = Vec::new ();
    for entry in fs::read_dir (&source . path) ? {
      let entry = entry ?;
      if ! entry . file_type () ? . is_file () { continue; }
      let path : PathBuf = entry . path ();
      if path . extension () . and_then ( |e| e . to_str () )
         != Some ("skg") {
        continue; }
      paths . push (path); }
    paths . sort ();
    for path in paths {
      let pid : ID = pid_from_path (&path) ?;
      candidates . push ((pid, SourceFile {
        source: source_name . clone (), path })); }
  }
  Ok (select_source_file_candidates (config, candidates))
}

/// Hash the exact currently selected corpus without parsing YAML.  Used only
/// as the final byte-stability comparison after a full-corpus transaction.
pub fn selected_path_digest_manifest (
  config : &SkgConfig,
) -> io::Result<SelectedPathManifest> {
  let selected = selected_direct_source_files (config) ?;
  let mut manifest = SelectedPathManifest::new ();
  for pid in selected . pid_order {
    for file in selected . by_pid . get (&pid)
      . into_iter () . flatten () {
      let bytes = fs::read (&file . path) ?;
      manifest . insert (
        file . path . clone (), PathDigest::of_bytes (&bytes)); }}
  Ok (manifest)
}

/// Select the extant direct source files for one filename PID.  Missing paths
/// are normal (a telescope can have fewer sections than configured sources).
pub fn selected_direct_source_files_for_pid (
  config : &SkgConfig,
  pid    : &ID,
) -> io::Result<(Vec<SourceFile>, Option<IgnoredForeignPathCollision>)> {
  let mut candidates : Vec<SourceFile> = Vec::new ();
  for source_name in config . ordered_sources () {
    let source = config . sources . get (&source_name)
      . expect ("ordered source exists");
    let path : PathBuf = source . path . join (format! ("{}.skg", pid));
    match fs::symlink_metadata (&path) {
      Ok (metadata) if metadata . file_type () . is_file () =>
        candidates . push (SourceFile { source: source_name, path }),
      Ok (_) => {}, // not a regular direct source file
      Err (e) if e . kind () == io::ErrorKind::NotFound => {},
      Err (e) => return Err (e), }
  }
  Ok (select_source_file_candidates_for_pid (config, pid, candidates))
}

/// Apply the same pre-parse owned/foreign rule to a caller-provided path
/// inventory (for example paths present in a Git tree or index).
pub fn select_source_file_candidates (
  config     : &SkgConfig,
  candidates : Vec<(ID, SourceFile)>,
) -> SelectedSourceFiles {
  let mut candidates_by_pid : HashMap<ID, Vec<SourceFile>> = HashMap::new ();
  let mut pid_order : Vec<ID> = Vec::new ();
  let mut seen : HashSet<ID> = HashSet::new ();
  for (pid, candidate) in candidates {
    if seen . insert (pid . clone ()) {
      pid_order . push (pid . clone ()); }
    candidates_by_pid . entry (pid) . or_default () . push (candidate);
  }
  let mut by_pid : HashMap<ID, Vec<SourceFile>> = HashMap::new ();
  let mut collisions : Vec<IgnoredForeignPathCollision> = Vec::new ();
  for pid in &pid_order {
    let candidates : Vec<SourceFile> =
      candidates_by_pid . remove (pid) . unwrap_or_default ();
    let (selected, collision) =
      select_source_file_candidates_for_pid (config, pid, candidates);
    by_pid . insert (pid . clone (), selected);
    if let Some (collision) = collision {
      collisions . push (collision); }
  }
  SelectedSourceFiles { pid_order, by_pid, collisions }
}

pub fn select_source_file_candidates_for_pid (
  config     : &SkgConfig,
  pid        : &ID,
  candidates : Vec<SourceFile>,
) -> (Vec<SourceFile>, Option<IgnoredForeignPathCollision>) {
  let has_owned : bool = candidates . iter ()
    . any ( |candidate| config . user_owns_source (&candidate . source) );
  if ! has_owned {
    return (candidates, None); }
  let (winners, losers) : (Vec<SourceFile>, Vec<SourceFile>) =
    candidates . into_iter () . partition (
      |candidate| config . user_owns_source (&candidate . source) );
  let collision : Option<IgnoredForeignPathCollision> =
    if losers . is_empty () { None }
    else { Some (IgnoredForeignPathCollision {
      pid: pid . clone (),
      winners: winners . clone (),
      losers, }) };
  (winners, collision)
}

fn pid_from_path (path : &Path) -> io::Result<ID> {
  let stem : &str = path . file_stem ()
    . and_then ( |s| s . to_str () )
    . ok_or_else ( || io::Error::new (
      io::ErrorKind::InvalidData,
      format! ("Cannot extract filename stem from {:?}", path) )) ?;
  if stem . is_empty () {
    return Err (io::Error::new (
      io::ErrorKind::InvalidData,
      format! ("Empty .skg filename stem at {:?}", path) )); }
  Ok (ID::from (stem))
}
