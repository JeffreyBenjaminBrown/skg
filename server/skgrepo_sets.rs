use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_skgrepos;
use crate::dbs::filesystem::not_nodes::load_config_with_overrides;
use crate::dbs::init::create_empty_tantivy_index;
use crate::types::env::find_skgrepo_with_optional_tantivy;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::GnidPresence;
use crate::types::misc::{ID, SkgConfig, SkgrepoName, TantivyIndex};
pub use crate::types::misc::SkgrepoSetName;
use crate::types::nodes::complete::Graphnode;
use crate::types::viewnode::{Viewnode, ViewnodeKind};
use crate::types::viewnode::Vognode;
use crate::test_utils::cleanup_test_tantivy;

use ego_tree::{NodeId, Tree};
use futures::executor::block_on;
use std::collections::{BTreeSet, HashMap};
use std::error::Error;
use std::fs;
use std::future::Future;
use std::path::Path;
use std::path::PathBuf;
use std::pin::Pin;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SkgrepoRestriction {
  pub name    : SkgrepoSetName,
  pub skgrepos : BTreeSet<SkgrepoName>,
}

impl SkgrepoRestriction {
  pub fn default_from_config (
    config : &SkgConfig,
  ) -> Result<SkgrepoRestriction, Box<dyn Error>> {
    SkgrepoRestriction::named (
      config,
      config . default_skgrepo_set_name () . clone ()) }

  pub fn named (
    config : &SkgConfig,
    name   : SkgrepoSetName,
  ) -> Result<SkgrepoRestriction, Box<dyn Error>> {
    Ok ( SkgrepoRestriction {
      skgrepos : config . skgrepo_set_skgrepos (&name)?,
      name } ) }

  pub fn contains_skgrepo (
    &self,
    skgrepo : &SkgrepoName,
  ) -> bool {
    self . skgrepos . contains (skgrepo) }

  pub fn is_all (
    &self,
  ) -> bool {
    self . name . 0 == "all" }

  pub fn gnid_skgrepo_is_unrestricted (
    &self,
    graph  : &InRustGraph,
    config : &SkgConfig,
    gnid     : &ID,
  ) -> Result<bool, Box<dyn Error>> {
    if self . is_all () {
      return Ok (true); }
    Ok ( match find_skgrepo_with_optional_tantivy (
      graph, gnid, None, config ) {
      Some (skgrepo) => self . contains_skgrepo (&skgrepo),
      None          => false } ) }
}

/// Refuses unless the restriction is 'all'. 'what' is the refused
/// command, as the subject of the refusal ("The diff view").
pub fn require_skgrepo_set_all (
  what        : &str,
  restriction : &SkgrepoRestriction,
) -> Result<(), String> {
  if restriction . is_all () { return Ok (( )); }
  Err ( format! (
    "{} requires no skgrepo restriction (the skgrepo-set all); current skgrepo restriction is {}. Switch the repo-set to all first.",
    what, restriction . name )) }

/// Entombstone every occurrence of a node the skgrepo restriction
/// hides (see 'GnidPresence'): the tombstone keeps the occurrence's
/// children and says nothing of the node. A node the graph lacks is
/// not hidden, and is left for whatever handles a vanished node.
pub fn entombstone_hidden_occurrences (
  viewforest  : &mut Tree<Viewnode>,
  graph       : &InRustGraph,
  restriction : &SkgrepoRestriction,
) {
  if restriction . is_all () {
    return; }
  let hidden : Vec<NodeId> =
    viewforest . root () . descendants ()
    . filter ( |n| match &n . value () . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
          graph . gnid_presence_gated (&t . gnid, Some (restriction))
          == GnidPresence::Hidden,
        _ => false } )
    . map ( |n| n . id () )
    . collect ();
  for treeid in hidden {
    viewforest . get_mut (treeid) . unwrap () . value ()
      . replace_kind (ViewnodeKind::Tombstone); }}

pub fn titles_for_skgrepo_set_for_test (
  config : &SkgConfig,
  restriction : &SkgrepoRestriction,
  gnids    : &[ID],
) -> Result<HashMap<ID, String>, Box<dyn Error>> {
  let nodes : Vec<Graphnode> =
    read_all_skg_files_from_skgrepos (config)?;
  let wanted : BTreeSet<ID> =
    gnids . iter () . cloned () . collect ();
  let mut result : HashMap<ID, String> = HashMap::new ();
  for node in nodes {
    if restriction . contains_skgrepo (&node . home_skgrepo)
    && wanted . contains (&node . pid) {
      result . insert (node . pid, node . title); }}
  Ok (result) }

pub fn search_gnids_for_skgrepo_set_for_test (
  _tantivy : &TantivyIndex,
  config   : &SkgConfig,
  restriction : &SkgrepoRestriction,
  terms    : &str,
  limit    : usize,
) -> Result<Vec<ID>, Box<dyn Error>> {
  let mut hits : Vec<ID> =
    read_all_skg_files_from_skgrepos (config)?
    . into_iter ()
    . filter ( |n| restriction . contains_skgrepo (&n . home_skgrepo) )
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

