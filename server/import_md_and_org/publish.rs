//! Checked, creation-only publication for one approved import batch.

use crate::dbs::filesystem::multiple_nodes::read_skg_sections_from_folder;
use crate::dbs::filesystem::one_node::prepare_nodecomplete_telescope;
use crate::dbs::in_rust_graph::prepared_update::{
  PreparedGraphUpdate, prepare_graph_update,
};
use crate::save::enqueue_tantivy_delta;
use crate::types::env::SkgEnv;
use crate::types::misc::{ID, SkgConfig};
use crate::types::nodes::complete::NodeComplete;
use crate::types::save::{DefineNode, SaveNode};
use std::collections::{HashMap, HashSet};
use std::fs::{self, File, OpenOptions};
use std::io::{self, Write};
use std::path::{Component, Path, PathBuf};

pub(crate) struct PreparedImportPublication {
  graph : PreparedGraphUpdate,
  files : Vec<(PathBuf, String)>,
}

struct CreatedFile {
  path : PathBuf,
  handle : File,
}

pub(crate) fn prepare_import_publication (
  nodes : &[NodeComplete],
  env : &SkgEnv,
) -> Result<PreparedImportPublication, String> {
  let runtime = env . runtime_snapshot ();
  let config : &SkgConfig = &runtime . config;
  let mut claims : HashMap<ID, ID> = HashMap::new ();
  for node in nodes {
    if ! config . user_owns_source (&node . source) {
      return Err (format! ("Destination source {:?} is not owned",
        node . source)); }
    for id in node . all_ids () {
      validate_safe_id (id)?;
      if let Some (owner) = claims . insert (id . clone (), node . pid . clone ()) {
        return Err (format! ("ID {} is claimed by both {} and {} in this import",
          id, owner, node . pid)); }
      if let Some ((owner, source)) = runtime . graph . pid_and_source (id) {
        return Err (format! ("ID {} already belongs to {} in source {}",
          id, owner, source)); } } }
  check_authoritative_disk_claims (&claims, config)?;
  let definitions : Vec<DefineNode> = nodes . iter () . cloned ()
    .map (|node| DefineNode::Save (SaveNode (node))) . collect ();
  let graph : PreparedGraphUpdate = prepare_graph_update (
    config, runtime . graph . clone (), definitions)
    . map_err (|error| error . to_string ())?;
  let mut files : Vec<(PathBuf, String)> = Vec::new ();
  for node in nodes {
    let prepared = prepare_nodecomplete_telescope (node, config, false)
      .map_err (|error| format! ("Preparing {}: {}", node . pid, error))?;
    files . extend (prepared . creation_files ()); }
  let mut paths : HashSet<PathBuf> = HashSet::new ();
  for (path, _) in &files {
    if ! paths . insert (path . clone ()) {
      return Err (format! ("Two imported sections would write {}",
        path . display ())); } }
  Ok (PreparedImportPublication { graph, files })
}

impl PreparedImportPublication {
  pub(crate) fn apply_under_mutation_gate (
    self,
    env : &SkgEnv,
  ) -> Result<usize, String> {
    let runtime = env . runtime_snapshot ();
    self . graph . verify_base (&env . runtime . legacy_graph_handle ())?;
    check_all_paths_absent (&self . files)?;
    let count : usize = self . files . len ();
    write_creation_files (&self . files, &mut |_, handle, yaml|
      handle . write_all (yaml . as_bytes ()))?;
    let definitions : Vec<DefineNode> = self . graph . definitions () . to_vec ();
    let candidate = self . graph . candidate () . clone ();
    env . runtime . publish (
      runtime . config . clone (), candidate . clone (),
      runtime . tantivy_index . clone ());
    enqueue_tantivy_delta (&candidate, &runtime . tantivy_index, definitions);
    Ok (count)
  }
}

fn validate_safe_id (
  id : &ID,
) -> Result<(), String> {
  let path : &Path = Path::new (&id . 0);
  let components : Vec<Component<'_>> = path . components () . collect ();
  if id . 0 . is_empty () || id . 0 . contains ('.') ||
    id . 0 . contains ('\\') || id . 0 . contains ('\0') ||
    components . len () != 1 ||
    ! matches! (components [0], Component::Normal (_)) {
    return Err (format! ("ID {:?} is unsafe as a .skg filename", id . 0)); }
  Ok (())
}

fn check_authoritative_disk_claims (
  claims : &HashMap<ID, ID>,
  config : &SkgConfig,
) -> Result<(), String> {
  for source in config . ordered_sources () {
    let sections = read_skg_sections_from_folder (&source, config)
      .map_err (|error| format! ("Reading source {}: {}", source, error))?;
    for (_, section) in sections {
      for id in std::iter::once (&section . pid)
        .chain (section . extra_ids . iter ()) {
        if let Some (new_owner) = claims . get (id) {
          return Err (format! (
            "ID {} for imported node {} is already claimed on disk by {} in source {}",
            id, new_owner, section . pid, source)); } } }
    for pid in claims . values () {
      let path : String = crate::util::path_from_pid_and_source (
        config, &source, pid . clone ())?;
      check_path_absent (Path::new (&path))?; } }
  Ok (())
}

fn check_all_paths_absent (
  files : &[(PathBuf, String)],
) -> Result<(), String> {
  for (path, _) in files { check_path_absent (path)?; }
  Ok (())
}

fn check_path_absent (
  path : &Path,
) -> Result<(), String> {
  match fs::symlink_metadata (path) {
    Ok (_) => Err (format! ("Destination {} already exists", path . display ())),
    Err (error) if error . kind () == io::ErrorKind::NotFound => Ok (()),
    Err (error) => Err (format! ("Checking {}: {}", path . display (), error)),
  }
}

fn write_creation_files (
  files : &[(PathBuf, String)],
  write : &mut impl FnMut (usize, &mut File, &str) -> io::Result<()>,
) -> Result<(), String> {
  let mut created : Vec<CreatedFile> = Vec::new ();
  for (index, (path, yaml)) in files . iter () . enumerate () {
    let result : io::Result<()> = (|| {
      let handle : File = OpenOptions::new () . write (true)
        . create_new (true) . open (path)?;
      created . push (CreatedFile { path : path . clone (), handle });
      write (index, &mut created . last_mut () . unwrap () . handle, yaml)?;
      created . last_mut () . unwrap () . handle . sync_all ()?;
      Ok (()) }) ();
    if let Err (error) = result {
      let rollback_errors : Vec<String> = rollback_created_files (created);
      let suffix : String = if rollback_errors . is_empty () {
        "All newly created files were removed." . to_string ()
      } else {
        format! ("Rollback failures: {}", rollback_errors . join ("; ")) };
      return Err (format! ("Writing {} failed: {}. {}",
        path . display (), error, suffix)); } }
  Ok (())
}

fn rollback_created_files (
  created : Vec<CreatedFile>,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new ();
  for created_file in created . into_iter () . rev () {
    let path : &Path = &created_file . path;
    let original : io::Result<fs::Metadata> = created_file . handle . metadata ();
    let current : io::Result<fs::Metadata> = fs::symlink_metadata (path);
    match (original, current) {
      (_, Err (error)) if error . kind () == io::ErrorKind::NotFound => (),
      (Ok (original), Ok (current)) if same_file (&original, &current) => {
        if let Err (error) = fs::remove_file (path) {
          errors . push (format! ("{}: {}", path . display (), error)); } }
      (Ok (_), Ok (_)) => errors . push (format! (
        "{} was replaced by another writer; left untouched", path . display ())),
      (Err (error), _) | (_, Err (error)) => errors . push (format! (
        "{}: could not verify ownership for rollback: {}", path . display (), error)),
    } }
  errors
}

#[cfg(unix)]
fn same_file (
  left : &fs::Metadata,
  right : &fs::Metadata,
) -> bool {
  use std::os::unix::fs::MetadataExt;
  left . dev () == right . dev () && left . ino () == right . ino ()
}

#[cfg(not(unix))]
fn same_file (
  _left : &fs::Metadata,
  _right : &fs::Metadata,
) -> bool { false }

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn a_failed_second_write_rolls_back_only_created_files () {
    let directory : tempfile::TempDir = tempfile::tempdir () . unwrap ();
    let first : PathBuf = directory . path () . join ("first.skg");
    let second : PathBuf = directory . path () . join ("second.skg");
    let prior : PathBuf = directory . path () . join ("prior.skg");
    fs::write (&prior, "original") . unwrap ();
    let result : Result<(), String> = write_creation_files (
      &[(first . clone (), "first" . to_string ()),
        (second . clone (), "second" . to_string ())],
      &mut |index, handle, contents| {
        if index == 1 {
          handle . write_all (b"part")?;
          return Err (io::Error::other ("injected write failure")); }
        handle . write_all (contents . as_bytes ())
      });
    assert! (result . unwrap_err () . contains ("injected write failure"));
    assert! (! first . exists ());
    assert! (! second . exists ());
    assert_eq! (fs::read_to_string (&prior) . unwrap (), "original");
  }

  #[test]
  fn destination_appearing_after_preflight_cannot_be_overwritten () {
    let directory : tempfile::TempDir = tempfile::tempdir () . unwrap ();
    let path : PathBuf = directory . path () . join ("appeared.skg");
    check_path_absent (&path) . unwrap ();
    fs::write (&path, "another writer") . unwrap ();
    let result : Result<(), String> = write_creation_files (
      &[(path . clone (), "import" . to_string ())],
      &mut |_, handle, contents| handle . write_all (contents . as_bytes ()));
    assert! (result . is_err ());
    assert_eq! (fs::read_to_string (&path) . unwrap (), "another writer");
  }

  #[test]
  fn rollback_does_not_remove_a_replaced_file () {
    let directory : tempfile::TempDir = tempfile::tempdir () . unwrap ();
    let first : PathBuf = directory . path () . join ("first.skg");
    let second : PathBuf = directory . path () . join ("second.skg");
    let result : Result<(), String> = write_creation_files (
      &[(first . clone (), "first" . to_string ()),
        (second . clone (), "second" . to_string ())],
      &mut |index, handle, contents| {
        if index == 1 {
          fs::remove_file (&first)?;
          fs::write (&first, "replacement")?;
          return Err (io::Error::other ("injected failure")); }
        handle . write_all (contents . as_bytes ())
      });
    let error : String = result . unwrap_err ();
    assert! (error . contains ("was replaced by another writer"));
    assert_eq! (fs::read_to_string (&first) . unwrap (), "replacement");
    assert! (! second . exists ());
  }
}
