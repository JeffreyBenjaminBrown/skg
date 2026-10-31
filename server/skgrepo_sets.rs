use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_skgrepos;
use crate::dbs::filesystem::not_nodes::load_config_with_overrides;
use crate::dbs::init::create_empty_tantivy_index;
use crate::types::misc::{ID, RelPartner, SkgConfig, SkgrepoName, TantivyIndex};
use crate::types::nodes::complete::Graphnode;
use crate::test_utils::{
  cleanup_test_tantivy, index_folder_for_this_process, remove_test_dir};

use futures::executor::block_on;
use std::collections::{BTreeSet, HashMap};
use std::error::Error;
use std::fs;
use std::future::Future;
use std::path::Path;
use std::path::PathBuf;
use std::pin::Pin;

/// The skgrepo restriction: the skgrepos a connection currently shows
/// ('docs/glossary.org', "skgrepo, home skgrepo, skgrepo-set,
/// gitrepo"). It is one skgrepo, the most private one shown, and
/// denotes that skgrepo and every more public one, a prefix of the
/// config's privacy order. A connection is unrestricted when the most
/// private skgrepo shown is the most private the config has
/// ('is_unrestricted'); the clients then show that skgrepo's name with
/// " (no restriction)" appended, words that never reach the wire or
/// the config.
///
/// It limits what can be edited as well as what can be
/// seen: no save, nodeMerge, fork, deletion, commit or other write may
/// remove, move, rewrite or create a record, a section or a node file
/// it keeps out, nor act on the buffer's silence about such a record.
/// Every write asks 'record_is_visible' or 'node_is_restricted' first,
/// refuses what would land out of sight, or refuses unless the
/// connection is unrestricted ('require_unrestricted').
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SkgrepoRestriction {
  pub most_private_shown : SkgrepoName,
  /// The set it denotes: 'most_private_shown' and every more public
  /// skgrepo of the config.
  pub skgrepos           : BTreeSet<SkgrepoName>,
  /// The config's skgrepos more private than 'most_private_shown', in
  /// privacy order. A skgrepo the config does not name is in neither
  /// set, and neither shown nor kept out.
  kept_out               : Vec<SkgrepoName>,
}

impl SkgrepoRestriction {
  /// The skgrepo restriction a connection starts with: the config's
  /// 'default_repo_set', else no skgrepo restriction.
  pub fn default_from_config (
    config : &SkgConfig,
  ) -> Result<SkgrepoRestriction, Box<dyn Error>> {
    match &config . default_skgrepo_set {
      Some (most_private_shown) =>
        SkgrepoRestriction::named (config, most_private_shown . clone ()),
      None => Ok ( SkgrepoRestriction::unrestricted (config) ), } }

  /// The skgrepo restriction whose most private skgrepo shown is
  /// 'most_private_shown', which the config must name.
  pub fn named (
    config             : &SkgConfig,
    most_private_shown : SkgrepoName,
  ) -> Result<SkgrepoRestriction, Box<dyn Error>> {
    let (shown, kept_out) : (Vec<SkgrepoName>, Vec<SkgrepoName>) =
      config . privacy_order_split_after (&most_private_shown)
      . map_err ( |_| format! (
        "'{}' names no skgrepo of the config. The skgrepo restriction is the most private skgrepo to show.",
        most_private_shown )) ?;
    Ok ( SkgrepoRestriction {
      skgrepos : shown . into_iter () . collect (),
      kept_out,
      most_private_shown } ) }

  /// No skgrepo restriction: the most private skgrepo of the config
  /// shown, and with it every skgrepo. A config that names no skgrepo
  /// has nothing to restrict; its skgrepo restriction shows the empty
  /// name.
  pub fn unrestricted (
    config : &SkgConfig,
  ) -> SkgrepoRestriction {
    match config . ordered_skgrepos () . last () {
      Some (most_private) =>
        SkgrepoRestriction::named (config, most_private . clone ())
        . expect ("the config names its own most private skgrepo"),
      None => SkgrepoRestriction {
        most_private_shown : SkgrepoName::from (""),
        skgrepos           : BTreeSet::new (),
        kept_out           : Vec::new (), }, } }

  /// The skgrepo restriction over the privacy order 'order' (most
  /// public first) whose most private skgrepo shown is
  /// 'most_private_shown', for a test without a config.
  pub fn of_privacy_order_for_test (
    order              : &[&str],
    most_private_shown : &str,
  ) -> SkgrepoRestriction {
    let position : usize =
      order . iter () . position ( |s| *s == most_private_shown )
      . expect ("the order names the most private skgrepo shown");
    SkgrepoRestriction {
      most_private_shown : SkgrepoName::from (most_private_shown),
      skgrepos : order [..= position] . iter ()
        . map ( |s| SkgrepoName::from (*s) ) . collect (),
      kept_out : order [position + 1 ..] . iter ()
        . map ( |s| SkgrepoName::from (*s) ) . collect (), } }

  /// Whether the set it denotes holds 'skgrepo', a skgrepo of the
  /// config.
  pub fn contains_skgrepo (
    &self,
    skgrepo : &SkgrepoName,
  ) -> bool {
    self . skgrepos . contains (skgrepo) }

  /// Whether the most private skgrepo shown is the most private the
  /// config has: no skgrepo restriction.
  pub fn is_unrestricted (
    &self,
  ) -> bool {
    self . kept_out . is_empty () }

  /// The most private skgrepo of the config, which an unrestricted
  /// connection shows.
  pub fn most_private_of_the_config (
    &self,
  ) -> &SkgrepoName {
    self . kept_out . last ()
      . unwrap_or ( &self . most_private_shown ) }

  /// Whether a record (a relationship as one node's file states it,
  /// in the section of the skgrepo 'relRepo') is visible: the
  /// skgrepo restriction does not keep its skgrepo out. A record in a
  /// skgrepo the config does not name is visible under every skgrepo
  /// restriction.
  pub fn record_is_visible (
    restriction : &SkgrepoRestriction,
    relRepo     : &SkgrepoName,
  ) -> bool {
    ! restriction . keeps_out (relRepo) }

  /// Whether a node whose home skgrepo is 'home' is restricted: the
  /// skgrepo restriction keeps its home out. A node homed in a skgrepo
  /// the config does not name is restricted under no skgrepo
  /// restriction.
  pub fn node_is_restricted (
    restriction : &SkgrepoRestriction,
    home        : &SkgrepoName,
  ) -> bool {
    restriction . keeps_out (home) }

  /// The records among 'partners' the skgrepo restriction shows
  /// ('record_is_visible'), in order.
  pub fn visible_records<'a, T> (
    restriction : &'a SkgrepoRestriction,
    partners    : &'a [RelPartner<T>],
  ) -> impl Iterator<Item = &'a RelPartner<T>> + 'a {
    partners . iter ()
      . filter ( |partner| SkgrepoRestriction::record_is_visible (
        restriction, &partner . relRepo )) }

  /// Whether 'partners' records 'member' and the skgrepo restriction
  /// shows that record.
  pub fn member_record_is_visible<T : PartialEq> (
    restriction : &SkgrepoRestriction,
    partners    : &[RelPartner<T>],
    member      : &T,
  ) -> bool {
    partners . iter ()
      . find ( |partner| &partner . member == member )
      . is_some_and ( |partner| SkgrepoRestriction::record_is_visible (
        restriction, &partner . relRepo )) }

  /// The body of both questions: whether 'skgrepo' is among the
  /// config's skgrepos the skgrepo restriction keeps out.
  fn keeps_out (
    &self,
    skgrepo : &SkgrepoName,
  ) -> bool {
    self . kept_out . contains (skgrepo) }
}

/// Refuses unless the connection is unrestricted. 'what' is the
/// refused command, as the subject of the refusal ("The diff view").
pub fn require_unrestricted (
  what        : &str,
  restriction : &SkgrepoRestriction,
) -> Result<(), String> {
  if restriction . is_unrestricted () { return Ok (( )); }
  Err ( format! (
    "{} requires no skgrepo restriction; the skgrepo restriction is {}. Switch it to {}, the most private skgrepo, first.",
    what, restriction . most_private_shown,
    restriction . most_private_of_the_config () )) }

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
    if ! SkgrepoRestriction::node_is_restricted (restriction, &node . home_skgrepo)
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
    . filter ( |n| ! SkgrepoRestriction::node_is_restricted (restriction, &n . home_skgrepo) )
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
    config . tantivy_folder =
      index_folder_for_this_process (tantivy_folder);
    let mut tantivy : TantivyIndex =
      create_empty_tantivy_index (&config . tantivy_folder)?;
    let result : Result<(), Box<dyn Error>> =
      test_fn (&config, &mut tantivy) . await;
    drop (tantivy); // before its folder goes
    cleanup_test_tantivy (
      Some (config . tantivy_folder . as_path ())) ?;
    remove_test_dir (
      fixture_config_path . parent ()
      . ok_or ("repo set fixture config has no parent")?) ?;
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
  let target_root : PathBuf = // the process ID, as the index folder has it
    PathBuf::from (format! (
      "/tmp/skg-repo-set-fixtures-{}-{}",
      test_name, std::process::id () ));
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

