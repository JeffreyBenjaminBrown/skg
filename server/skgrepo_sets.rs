use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_skgrepos;
use crate::dbs::filesystem::not_nodes::load_config_with_overrides;
use crate::dbs::init::create_empty_tantivy_index;
use crate::types::env::find_skgrepo_with_optional_tantivy;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::{ID, SkgConfig, SkgRepoName, TantivyIndex};
pub use crate::types::misc::SkgRepoSetName;
use crate::types::nodes::complete::Graphnode;
use crate::types::viewnode::{Viewnode, ViewnodeKind, mk_inactive_viewnode};
use crate::types::viewnode::{Vognode, Phantom};
use crate::test_utils::cleanup_test_tantivy;

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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ActiveSkgRepoSet {
  pub name    : SkgRepoSetName,
  pub skgrepos : BTreeSet<SkgRepoName>,
}

impl ActiveSkgRepoSet {
  pub fn default_from_config (
    config : &SkgConfig,
  ) -> Result<ActiveSkgRepoSet, Box<dyn Error>> {
    ActiveSkgRepoSet::named (
      config,
      config . default_skgrepo_set_name () . clone ()) }

  pub fn named (
    config : &SkgConfig,
    name   : SkgRepoSetName,
  ) -> Result<ActiveSkgRepoSet, Box<dyn Error>> {
    Ok ( ActiveSkgRepoSet {
      skgrepos : config . skgrepo_set_skgrepos (&name)?,
      name } ) }

  pub fn contains_skgrepo (
    &self,
    skgrepo : &SkgRepoName,
  ) -> bool {
    self . skgrepos . contains (skgrepo) }

  pub fn is_all (
    &self,
  ) -> bool {
    self . name . 0 == "all" }

  pub fn skgid_skgrepo_is_active (
    &self,
    graph  : &InRustGraph,
    config : &SkgConfig,
    skgid     : &ID,
  ) -> Result<bool, Box<dyn Error>> {
    if self . is_all () {
      return Ok (true); }
    let deleted_since_head_pid_src_map : HashMap<ID, SkgRepoName> =
      HashMap::new ();
    Ok ( match find_skgrepo_with_optional_tantivy (
      graph, skgid, &deleted_since_head_pid_src_map, None, config ) {
      Some (skgrepo) => self . contains_skgrepo (&skgrepo),
      None          => false } ) }
}

pub fn filter_path_to_active_skgrepos_for_test (
  graph  : &InRustGraph,
  config : &SkgConfig,
  active : &ActiveSkgRepoSet,
  path   : Vec<ID>,
) -> Result<Vec<ID>, Box<dyn Error>> {
  let mut result : Vec<ID> = Vec::new ();
  let deleted_since_head_pid_src_map : HashMap<ID, SkgRepoName> =
    HashMap::new ();
  for skgid in path {
    let skgrepo : SkgRepoName =
      match find_skgrepo_with_optional_tantivy (
        graph, &skgid, &deleted_since_head_pid_src_map, None, config ) {
        Some (skgrepo) => skgrepo,
        None => break };
    if active . contains_skgrepo (&skgrepo) {
      result . push (skgid);
    } else {
      break; }}
  Ok (result) }

pub fn filter_branches_to_active_skgrepos_for_test (
  graph    : &InRustGraph,
  config   : &SkgConfig,
  active   : &ActiveSkgRepoSet,
  branches : BTreeSet<ID>,
) -> Result<BTreeSet<ID>, Box<dyn Error>> {
  let mut result : BTreeSet<ID> = BTreeSet::new ();
  let deleted_since_head_pid_src_map : HashMap<ID, SkgRepoName> =
    HashMap::new ();
  for skgid in branches {
    if let Some (skgrepo) =
      find_skgrepo_with_optional_tantivy (
        graph, &skgid, &deleted_since_head_pid_src_map, None, config )
    {
      if active . contains_skgrepo (&skgrepo) {
        result . insert (skgid); }}}
  Ok (result) }

pub fn apply_skgrepo_set_to_viewforest (
  viewforest : &mut Tree<Viewnode>,
  active     : &ActiveSkgRepoSet,
) {
  if active . is_all () {
    return; }
  let skgids : Vec<NodeId> =
    viewforest . root () . descendants ()
    . map ( |n| n . id () )
    . collect ();
  for skgid in skgids {
    enum Treatment { Convert, Detach }
    let treatment : Option<Treatment> = {
      let Some (n) = viewforest . get (skgid) else { continue; }; // already detached with an ancestor
      let has_children : bool = n . has_children ();
      match &n . value () . kind {
        ViewnodeKind::Vognode (Vognode::Active (t))
          if ! active . contains_skgrepo (&t . home_skgrepo)
          => Some ( Treatment::Convert ),
        ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Diff (p)))
          if ! active . contains_skgrepo (&p . home_skgrepo)
          // TODO/DONE/full-schema/DONE/9-2_source-set-safety.org (interim,
          // until diff mode and restricted sets refuse to combine):
          // a removed-member phantom for an inactive node is
          // quietly omitted, like every other inactive member. One
          // with children (e.g. an attached role tree) is converted
          // instead, so nothing active is silently dropped.
          => if has_children { Some ( Treatment::Convert ) }
             else { Some ( Treatment::Detach ) },
        _ => None } };
    match treatment {
      None => {},
      Some (Treatment::Convert) => {
        let mut node_mut : NodeMut<Viewnode> =
          viewforest . get_mut (skgid) . unwrap ();
        node_mut . value () . kind =
          mk_inactive_viewnode () . kind; },
      Some (Treatment::Detach) => {
        let mut node_mut : NodeMut<Viewnode> =
          viewforest . get_mut (skgid) . unwrap ();
        node_mut . detach (); }, }}}


pub fn titles_for_skgrepo_set_for_test (
  config : &SkgConfig,
  active : &ActiveSkgRepoSet,
  skgids    : &[ID],
) -> Result<HashMap<ID, String>, Box<dyn Error>> {
  let nodes : Vec<Graphnode> =
    read_all_skg_files_from_skgrepos (config)?;
  let wanted : BTreeSet<ID> =
    skgids . iter () . cloned () . collect ();
  let mut result : HashMap<ID, String> = HashMap::new ();
  for node in nodes {
    if active . contains_skgrepo (&node . home_skgrepo)
    && wanted . contains (&node . pid) {
      result . insert (node . pid, node . title); }}
  Ok (result) }

pub fn search_skgids_for_skgrepo_set_for_test (
  _tantivy : &TantivyIndex,
  config   : &SkgConfig,
  active   : &ActiveSkgRepoSet,
  terms    : &str,
  limit    : usize,
) -> Result<Vec<ID>, Box<dyn Error>> {
  let mut hits : Vec<ID> =
    read_all_skg_files_from_skgrepos (config)?
    . into_iter ()
    . filter ( |n| active . contains_skgrepo (&n . home_skgrepo) )
    . filter ( |n| {
      n . title . contains (terms)
      || n . aliases . or_default () . iter () . any ( |a|
           a . member . contains (terms)) })
    . map ( |n| n . pid )
    . collect ();
  hits . sort ();
  hits . truncate (limit);
  Ok (hits) }

pub fn run_with_skgrepo_set_test_db<F>(
  test_name        : &str,
  config_path    : &str,
  tantivy_folder : &str,
  test_fn        : F,
) -> Result<(), Box<dyn Error>>
where
  F: for<'a>
  FnOnce(&'a SkgConfig, &'a mut TantivyIndex)
         -> Pin<Box<dyn Future<Output = Result
                               <(), Box<dyn Error>>> + 'a>>,
{
  block_on ( async {
    let fixture_config_path : PathBuf =
      prepare_skgrepo_set_fixture_copy (test_name, config_path)?;
    let mut config : SkgConfig =
      load_config_with_overrides (
        fixture_config_path . to_str ()
        . ok_or ("fixture config path is not UTF-8")?,
        Some (test_name),
        &[])?;
    config . tantivy_folder = PathBuf::from (tantivy_folder);
    let mut tantivy : TantivyIndex =
      create_empty_tantivy_index (&config . tantivy_folder)?;
    let result : Result<(), Box<dyn Error>> =
      test_fn (&config, &mut tantivy) . await;
    cleanup_test_tantivy (
      Some (config . tantivy_folder . as_path ())) ?;
    result }) }

fn prepare_skgrepo_set_fixture_copy (
  test_name     : &str,
  config_path : &str,
) -> Result<PathBuf, Box<dyn Error>> {
  let skgrepo_config_path : PathBuf =
    PathBuf::from (config_path);
  let skgrepo_root : &Path =
    skgrepo_config_path . parent ()
    . ok_or ("repo set fixture config has no parent")?;
  let target_root : PathBuf =
    PathBuf::from (format! (
      "/tmp/skg-repo-set-fixtures-{}", test_name));
  if target_root . exists () {
    fs::remove_dir_all (&target_root)?; }
  copy_dir_recursively (skgrepo_root, &target_root)?;
  prepare_git_diff_fixture (&target_root)?;
  Ok (target_root . join (
    skgrepo_config_path . file_name ()
    . ok_or ("repo set fixture config has no filename")?)) }

fn copy_dir_recursively (
  skgrepo : &Path,
  target : &Path,
) -> Result<(), Box<dyn Error>> {
  fs::create_dir_all (target)?;
  for entry in fs::read_dir (skgrepo)? {
    let entry : fs::DirEntry = entry?;
    let skgrepo_path : PathBuf =
      entry . path ();
    let target_path : PathBuf =
      target . join (entry . file_name ());
    if entry . file_type ()? . is_dir () {
      copy_dir_recursively (&skgrepo_path, &target_path)?;
    } else {
      fs::copy (&skgrepo_path, &target_path)?; }}
  Ok (( )) }

/// Public so repo-set diff tests can replay this prep on a
/// SharedStoreSession's fixture copy (reset_with_fixture_prep).
pub fn prepare_git_diff_fixture (
  fixture_root : &Path,
) -> Result<(), Box<dyn Error>> {
  let public_skgrepo : PathBuf =
    fixture_root . join ("owned/public");
  let diff_root : PathBuf =
    public_skgrepo . join ("diff-root.skg");
  if ! diff_root . exists () {
    return Ok (( )); }
  fs::write (&diff_root, indoc::indoc! {"
    pid: diff-root
    title: diff-root
    contains:
    - active-a
    - private-removed
  "})?;
  run_git (&public_skgrepo, &["init"])?;
  run_git (&public_skgrepo, &["config", "user.email", "tests@example.invalid"])?;
  run_git (&public_skgrepo, &["config", "user.name", "skg tests"])?;
  run_git (&public_skgrepo, &["add", "."])?;
  run_git (&public_skgrepo, &["commit", "-m", "baseline"])?;
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
