use crate::consts::TYPEDB_ADDRESS;
use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use crate::dbs::filesystem::not_nodes::load_config_with_overrides;
use crate::dbs::init::{
  create_empty_tantivy_index, overwrite_new_empty_typedb_db,
  read_and_use_schema};
use crate::dbs::typedb::nodes::create_all_nodes;
use crate::dbs::typedb::relationships::create_all_relationships;
use crate::dbs::typedb::sources::create_all_sources;
use crate::org_to_text::viewforest_to_string;
use crate::types::env::find_source_with_optional_tantivy;
use crate::types::git::MembershipAxes;
use crate::types::misc::{ID, SkgConfig, SourceName, TantivyIndex};
pub use crate::types::misc::SourceSetName;
use crate::types::nodes::complete::NodeComplete;
use crate::types::nodes::typedb::NodeTypedb;
use crate::types::viewnode::{ViewNode, ViewNodeKind, mk_inactive_viewnode};
use crate::types::viewnode::Vognode;
use crate::test_utils::cleanup_test_tantivy_and_typedb_dbs;

use ego_tree::{NodeId, NodeMut, Tree};
use futures::executor::block_on;
use std::collections::{BTreeSet, HashMap};
use std::error::Error;
use std::fs;
use std::future::Future;
use std::path::Path;
use std::path::PathBuf;
use std::pin::Pin;
use std::process::Command;
use std::sync::Arc;
use typedb_driver::{Addresses, Credentials, DriverOptions, DriverTlsConfig, TypeDBDriver};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ActiveSourceSet {
  pub name    : SourceSetName,
  pub sources : BTreeSet<SourceName>,
}

impl ActiveSourceSet {
  pub fn default_from_config (
    config : &SkgConfig,
  ) -> Result<ActiveSourceSet, Box<dyn Error>> {
    ActiveSourceSet::named (
      config,
      config . default_source_set_name () . clone ()) }

  pub fn named (
    config : &SkgConfig,
    name   : SourceSetName,
  ) -> Result<ActiveSourceSet, Box<dyn Error>> {
    Ok ( ActiveSourceSet {
      sources : config . source_set_sources (&name)?,
      name } ) }

  pub fn contains_source (
    &self,
    source : &SourceName,
  ) -> bool {
    self . sources . contains (source) }

  pub fn is_all (
    &self,
  ) -> bool {
    self . name . 0 == "all" }

  pub fn id_source_is_active (
    &self,
    config : &SkgConfig,
    id     : &ID,
  ) -> Result<bool, Box<dyn Error>> {
    if self . is_all () {
      return Ok (true); }
    let deleted_since_head_pid_src_map : HashMap<ID, SourceName> =
      HashMap::new ();
    Ok ( match find_source_with_optional_tantivy (
      id, &deleted_since_head_pid_src_map, None, config ) {
      Some (source) => self . contains_source (&source),
      None          => false } ) }
}

pub fn filter_path_to_active_sources_for_test (
  config : &SkgConfig,
  active : &ActiveSourceSet,
  path   : Vec<ID>,
) -> Result<Vec<ID>, Box<dyn Error>> {
  let mut result : Vec<ID> = Vec::new ();
  let deleted_since_head_pid_src_map : HashMap<ID, SourceName> =
    HashMap::new ();
  for id in path {
    let source : SourceName =
      match find_source_with_optional_tantivy (
        &id, &deleted_since_head_pid_src_map, None, config ) {
        Some (source) => source,
        None => break };
    if active . contains_source (&source) {
      result . push (id);
    } else {
      break; }}
  Ok (result) }

pub fn filter_branches_to_active_sources_for_test (
  config   : &SkgConfig,
  active   : &ActiveSourceSet,
  branches : BTreeSet<ID>,
) -> Result<BTreeSet<ID>, Box<dyn Error>> {
  let mut result : BTreeSet<ID> = BTreeSet::new ();
  let deleted_since_head_pid_src_map : HashMap<ID, SourceName> =
    HashMap::new ();
  for id in branches {
    if let Some (source) =
      find_source_with_optional_tantivy (
        &id, &deleted_since_head_pid_src_map, None, config )
    {
      if active . contains_source (&source) {
        result . insert (id); }}}
  Ok (result) }

pub fn apply_source_set_to_viewforest (
  viewforest : &mut Tree<ViewNode>,
  active     : &ActiveSourceSet,
) {
  if active . is_all () {
    return; }
  let ids : Vec<NodeId> =
    viewforest . root () . descendants ()
    . map ( |n| n . id () )
    . collect ();
  for id in ids {
    let inactive : Option<(ID, SourceName, MembershipAxes)> =
      viewforest . get (id)
      . and_then (
        |n| match &n . value () . kind {
          ViewNodeKind::Vognode (Vognode::Normal (t))
            if ! active . contains_source (&t . source)
            => Some (( t . id . clone (),
                       t . source . clone (),
                       t . membership )),
          ViewNodeKind::Vognode (Vognode::DiffPhantom (p))
            if ! active . contains_source (&p . source)
            => Some (( p . id . clone (),
                       p . source . clone (),
                       p . membership )),
          _ => None } );
    if let Some ((pid, source, membership)) = inactive {
      let mut node_mut : NodeMut<ViewNode> =
        viewforest . get_mut (id) . unwrap ();
      node_mut . value () . kind =
        mk_inactive_viewnode (pid, source, membership) . kind; }}}

pub fn render_viewforest_with_source_set (
  viewforest : &mut Tree<ViewNode>,
  config     : &SkgConfig,
  active     : &ActiveSourceSet,
) -> Result<String, Box<dyn Error>> {
  apply_source_set_to_viewforest (viewforest, active);
  viewforest_to_string (viewforest, config) }

pub fn titles_for_source_set_for_test (
  config : &SkgConfig,
  active : &ActiveSourceSet,
  ids    : &[ID],
) -> Result<HashMap<ID, String>, Box<dyn Error>> {
  let nodes : Vec<NodeComplete> =
    read_all_skg_files_from_sources (config)?;
  let wanted : BTreeSet<ID> =
    ids . iter () . cloned () . collect ();
  let mut result : HashMap<ID, String> = HashMap::new ();
  for node in nodes {
    if active . contains_source (&node . source)
    && wanted . contains (&node . pid) {
      result . insert (node . pid, node . title); }}
  Ok (result) }

pub fn search_ids_for_source_set_for_test (
  _tantivy : &TantivyIndex,
  config   : &SkgConfig,
  active   : &ActiveSourceSet,
  terms    : &str,
  limit    : usize,
) -> Result<Vec<ID>, Box<dyn Error>> {
  let mut hits : Vec<ID> =
    read_all_skg_files_from_sources (config)?
    . into_iter ()
    . filter ( |n| active . contains_source (&n . source) )
    . filter ( |n| {
      n . title . contains (terms)
      || n . aliases . or_default () . iter () . any ( |a|
           a . contains (terms)) })
    . map ( |n| n . pid )
    . collect ();
  hits . sort ();
  hits . truncate (limit);
  Ok (hits) }

pub fn run_with_source_set_test_db<F>(
  db_name        : &str,
  config_path    : &str,
  tantivy_folder : &str,
  test_fn        : F,
) -> Result<(), Box<dyn Error>>
where
  F: for<'a>
  FnOnce(&'a SkgConfig, &'a Arc<TypeDBDriver>, &'a mut TantivyIndex)
         -> Pin<Box<dyn Future<Output = Result
                               <(), Box<dyn Error>>> + 'a>>,
{
  block_on ( async {
    let fixture_config_path : PathBuf =
      prepare_source_set_fixture_copy (db_name, config_path)?;
    let mut config : SkgConfig =
      load_config_with_overrides (
        fixture_config_path . to_str ()
        . ok_or ("fixture config path is not UTF-8")?,
        Some (db_name),
        &[])?;
    config . tantivy_folder = PathBuf::from (tantivy_folder);
    let driver : TypeDBDriver =
      TypeDBDriver::new (
        Addresses::try_from_address_str (TYPEDB_ADDRESS)?,
        Credentials::new ("admin", "password"),
        DriverOptions::new (DriverTlsConfig::disabled ()) ) . await?;
    let nodes : Vec<NodeComplete> =
      read_all_skg_files_from_sources (&config)?;
    let typedb_nodes : Vec<NodeTypedb> =
      nodes . iter ()
      . map (NodeTypedb::from_complete_parsing_textlinks)
      . collect ();
    overwrite_new_empty_typedb_db (db_name, &driver) . await?;
    read_and_use_schema (db_name, &driver) . await?;
    create_all_sources (
      db_name, &driver, &config ) . await?;
    create_all_nodes (db_name, &driver, &typedb_nodes) . await?;
    create_all_relationships (db_name, &driver, &typedb_nodes) . await?;
    let mut tantivy : TantivyIndex =
      create_empty_tantivy_index (&config . tantivy_folder)?;
    let driver_arc : Arc<TypeDBDriver> =
      Arc::new (driver);
    let result : Result<(), Box<dyn Error>> =
      test_fn (&config, &driver_arc, &mut tantivy) . await;
    cleanup_test_tantivy_and_typedb_dbs (
      db_name, &driver_arc, Some (config . tantivy_folder . as_path ())
    ) . await?;
    result }) }

fn prepare_source_set_fixture_copy (
  db_name     : &str,
  config_path : &str,
) -> Result<PathBuf, Box<dyn Error>> {
  let source_config_path : PathBuf =
    PathBuf::from (config_path);
  let source_root : &Path =
    source_config_path . parent ()
    . ok_or ("source set fixture config has no parent")?;
  let target_root : PathBuf =
    PathBuf::from (format! (
      "/tmp/skg-source-set-fixtures-{}", db_name));
  if target_root . exists () {
    fs::remove_dir_all (&target_root)?; }
  copy_dir_recursively (source_root, &target_root)?;
  prepare_git_diff_fixture (&target_root)?;
  Ok (target_root . join (
    source_config_path . file_name ()
    . ok_or ("source set fixture config has no filename")?)) }

fn copy_dir_recursively (
  source : &Path,
  target : &Path,
) -> Result<(), Box<dyn Error>> {
  fs::create_dir_all (target)?;
  for entry in fs::read_dir (source)? {
    let entry : fs::DirEntry = entry?;
    let source_path : PathBuf =
      entry . path ();
    let target_path : PathBuf =
      target . join (entry . file_name ());
    if entry . file_type ()? . is_dir () {
      copy_dir_recursively (&source_path, &target_path)?;
    } else {
      fs::copy (&source_path, &target_path)?; }}
  Ok (( )) }

fn prepare_git_diff_fixture (
  fixture_root : &Path,
) -> Result<(), Box<dyn Error>> {
  let public_source : PathBuf =
    fixture_root . join ("public");
  let diff_root : PathBuf =
    public_source . join ("diff-root.skg");
  if ! diff_root . exists () {
    return Ok (( )); }
  fs::write (&diff_root, indoc::indoc! {"
    pid: diff-root
    title: diff-root
    contains:
    - active-a
    - private-removed
  "})?;
  run_git (&public_source, &["init"])?;
  run_git (&public_source, &["config", "user.email", "tests@example.invalid"])?;
  run_git (&public_source, &["config", "user.name", "skg tests"])?;
  run_git (&public_source, &["add", "."])?;
  run_git (&public_source, &["commit", "-m", "baseline"])?;
  fs::write (&diff_root, indoc::indoc! {"
    pid: diff-root
    title: diff-root
    contains:
    - active-a
    - private-new
  "})?;
  Ok (( )) }

fn run_git (
  dir  : &Path,
  args : &[&str],
) -> Result<(), Box<dyn Error>> {
  let output : std::process::Output =
    Command::new ("git")
    . args (args)
    . current_dir (dir)
    . output ()?;
  if output . status . success () {
    Ok (( ))
  } else {
    Err (format! (
      "git {:?} failed: {}{}",
      args,
      String::from_utf8_lossy (&output . stdout),
      String::from_utf8_lossy (&output . stderr)) . into ()) }}
