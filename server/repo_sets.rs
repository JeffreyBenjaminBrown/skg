use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_repos;
use crate::dbs::filesystem::not_nodes::load_config_with_overrides;
use crate::dbs::init::create_empty_tantivy_index;
use crate::types::env::find_repo_with_optional_tantivy;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::{ID, SkgConfig, RepoName, TantivyIndex};
pub use crate::types::misc::RepoSetName;
use crate::types::nodes::complete::NodeComplete;
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
pub struct ActiveRepoSet {
  pub name    : RepoSetName,
  pub repos : BTreeSet<RepoName>,
}

impl ActiveRepoSet {
  pub fn default_from_config (
    config : &SkgConfig,
  ) -> Result<ActiveRepoSet, Box<dyn Error>> {
    ActiveRepoSet::named (
      config,
      config . default_repo_set_name () . clone ()) }

  pub fn named (
    config : &SkgConfig,
    name   : RepoSetName,
  ) -> Result<ActiveRepoSet, Box<dyn Error>> {
    Ok ( ActiveRepoSet {
      repos : config . repo_set_repos (&name)?,
      name } ) }

  pub fn contains_repo (
    &self,
    repo : &RepoName,
  ) -> bool {
    self . repos . contains (repo) }

  pub fn is_all (
    &self,
  ) -> bool {
    self . name . 0 == "all" }

  pub fn id_repo_is_active (
    &self,
    graph  : &InRustGraph,
    config : &SkgConfig,
    id     : &ID,
  ) -> Result<bool, Box<dyn Error>> {
    if self . is_all () {
      return Ok (true); }
    let deleted_since_head_pid_src_map : HashMap<ID, RepoName> =
      HashMap::new ();
    Ok ( match find_repo_with_optional_tantivy (
      graph, id, &deleted_since_head_pid_src_map, None, config ) {
      Some (repo) => self . contains_repo (&repo),
      None          => false } ) }
}

pub fn filter_path_to_active_repos_for_test (
  graph  : &InRustGraph,
  config : &SkgConfig,
  active : &ActiveRepoSet,
  path   : Vec<ID>,
) -> Result<Vec<ID>, Box<dyn Error>> {
  let mut result : Vec<ID> = Vec::new ();
  let deleted_since_head_pid_src_map : HashMap<ID, RepoName> =
    HashMap::new ();
  for id in path {
    let repo : RepoName =
      match find_repo_with_optional_tantivy (
        graph, &id, &deleted_since_head_pid_src_map, None, config ) {
        Some (repo) => repo,
        None => break };
    if active . contains_repo (&repo) {
      result . push (id);
    } else {
      break; }}
  Ok (result) }

pub fn filter_branches_to_active_repos_for_test (
  graph    : &InRustGraph,
  config   : &SkgConfig,
  active   : &ActiveRepoSet,
  branches : BTreeSet<ID>,
) -> Result<BTreeSet<ID>, Box<dyn Error>> {
  let mut result : BTreeSet<ID> = BTreeSet::new ();
  let deleted_since_head_pid_src_map : HashMap<ID, RepoName> =
    HashMap::new ();
  for id in branches {
    if let Some (repo) =
      find_repo_with_optional_tantivy (
        graph, &id, &deleted_since_head_pid_src_map, None, config )
    {
      if active . contains_repo (&repo) {
        result . insert (id); }}}
  Ok (result) }

pub fn apply_repo_set_to_viewforest (
  viewforest : &mut Tree<Viewnode>,
  active     : &ActiveRepoSet,
) {
  if active . is_all () {
    return; }
  let ids : Vec<NodeId> =
    viewforest . root () . descendants ()
    . map ( |n| n . id () )
    . collect ();
  for id in ids {
    enum Treatment { Convert, Detach }
    let treatment : Option<Treatment> = {
      let Some (n) = viewforest . get (id) else { continue; }; // already detached with an ancestor
      let has_children : bool = n . has_children ();
      match &n . value () . kind {
        ViewnodeKind::Vognode (Vognode::Active (t))
          if ! active . contains_repo (&t . home_repo)
          => Some ( Treatment::Convert ),
        ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Diff (p)))
          if ! active . contains_repo (&p . home_repo)
          // TODO/full-schema/9-2_repo-set-safety.org (interim,
          // until diff mode and restricted sets refuse to combine):
          // a removed-member phantom for an inactive node is
          // quietly omitted, like every other inactive member. One
          // with children (e.g. an attached ancestry) is converted
          // instead, so nothing active is silently dropped.
          => if has_children { Some ( Treatment::Convert ) }
             else { Some ( Treatment::Detach ) },
        _ => None } };
    match treatment {
      None => {},
      Some (Treatment::Convert) => {
        let mut node_mut : NodeMut<Viewnode> =
          viewforest . get_mut (id) . unwrap ();
        node_mut . value () . kind =
          mk_inactive_viewnode () . kind; },
      Some (Treatment::Detach) => {
        let mut node_mut : NodeMut<Viewnode> =
          viewforest . get_mut (id) . unwrap ();
        node_mut . detach (); }, }}}


pub fn titles_for_repo_set_for_test (
  config : &SkgConfig,
  active : &ActiveRepoSet,
  ids    : &[ID],
) -> Result<HashMap<ID, String>, Box<dyn Error>> {
  let nodes : Vec<NodeComplete> =
    read_all_skg_files_from_repos (config)?;
  let wanted : BTreeSet<ID> =
    ids . iter () . cloned () . collect ();
  let mut result : HashMap<ID, String> = HashMap::new ();
  for node in nodes {
    if active . contains_repo (&node . home_repo)
    && wanted . contains (&node . pid) {
      result . insert (node . pid, node . title); }}
  Ok (result) }

pub fn search_ids_for_repo_set_for_test (
  _tantivy : &TantivyIndex,
  config   : &SkgConfig,
  active   : &ActiveRepoSet,
  terms    : &str,
  limit    : usize,
) -> Result<Vec<ID>, Box<dyn Error>> {
  let mut hits : Vec<ID> =
    read_all_skg_files_from_repos (config)?
    . into_iter ()
    . filter ( |n| active . contains_repo (&n . home_repo) )
    . filter ( |n| {
      n . title . contains (terms)
      || n . aliases . or_default () . iter () . any ( |a|
           a . member . contains (terms)) })
    . map ( |n| n . pid )
    . collect ();
  hits . sort ();
  hits . truncate (limit);
  Ok (hits) }

pub fn run_with_repo_set_test_db<F>(
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
      prepare_repo_set_fixture_copy (test_name, config_path)?;
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

fn prepare_repo_set_fixture_copy (
  test_name     : &str,
  config_path : &str,
) -> Result<PathBuf, Box<dyn Error>> {
  let repo_config_path : PathBuf =
    PathBuf::from (config_path);
  let repo_root : &Path =
    repo_config_path . parent ()
    . ok_or ("repo set fixture config has no parent")?;
  let target_root : PathBuf =
    PathBuf::from (format! (
      "/tmp/skg-repo-set-fixtures-{}", test_name));
  if target_root . exists () {
    fs::remove_dir_all (&target_root)?; }
  copy_dir_recursively (repo_root, &target_root)?;
  prepare_git_diff_fixture (&target_root)?;
  Ok (target_root . join (
    repo_config_path . file_name ()
    . ok_or ("repo set fixture config has no filename")?)) }

fn copy_dir_recursively (
  repo : &Path,
  target : &Path,
) -> Result<(), Box<dyn Error>> {
  fs::create_dir_all (target)?;
  for entry in fs::read_dir (repo)? {
    let entry : fs::DirEntry = entry?;
    let repo_path : PathBuf =
      entry . path ();
    let target_path : PathBuf =
      target . join (entry . file_name ());
    if entry . file_type ()? . is_dir () {
      copy_dir_recursively (&repo_path, &target_path)?;
    } else {
      fs::copy (&repo_path, &target_path)?; }}
  Ok (( )) }

/// Public so repo-set diff tests can replay this prep on a
/// SharedStoreSession's fixture copy (reset_with_fixture_prep).
pub fn prepare_git_diff_fixture (
  fixture_root : &Path,
) -> Result<(), Box<dyn Error>> {
  let public_repo : PathBuf =
    fixture_root . join ("owned/public");
  let diff_root : PathBuf =
    public_repo . join ("diff-root.skg");
  if ! diff_root . exists () {
    return Ok (( )); }
  fs::write (&diff_root, indoc::indoc! {"
    pid: diff-root
    title: diff-root
    contains:
    - active-a
    - private-removed
  "})?;
  run_git (&public_repo, &["init"])?;
  run_git (&public_repo, &["config", "user.email", "tests@example.invalid"])?;
  run_git (&public_repo, &["config", "user.name", "skg tests"])?;
  run_git (&public_repo, &["add", "."])?;
  run_git (&public_repo, &["commit", "-m", "baseline"])?;
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
