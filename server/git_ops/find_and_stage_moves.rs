/// Detects nodes whose '.skg' file moved from one source's git repo
/// to another, and renders a shell script that stages each move
/// ('git rm' in the old repo, 'git add' in the new one).
///
/// A node has "moved" iff, across all configured sources, its file
/// vanished from EXACTLY one source (present in that source's git
/// HEAD, gone from its worktree) and appeared in EXACTLY one other
/// source (present in that source's worktree, absent from its HEAD).
/// If it vanished from two sources, or appeared in two, there is more
/// than one candidate (old, new) pair, so it is not an unambiguous
/// move and is skipped.

use crate::types::misc::{ID, SkgConfig, SkgfileSource, SourceName};

use super::misc::path_relative_to_repo;
use super::read_repo::open_repo;

use git2::{ErrorCode, Object, ObjectType, Repository, Tree};
use std::collections::{BTreeMap, HashSet};
use std::error::Error as StdError;
use std::fs;
use std::path::{Path, PathBuf};

/// A node whose file vanished from exactly one source and appeared in
/// exactly one (necessarily different) source.
struct NodeMove {
  id   : ID,
  from : SourceName,
  to   : SourceName,
}

/// Top-level entry: the staging script for every detected move.
pub fn stage_moves_script (
  config : &SkgConfig,
) -> Result<String, String> {
  let moves : Vec<NodeMove> =
    detect_moves (config) ?;
  Ok ( render_moves_script (config, &moves) ) }

/// Scan every source, then keep only IDs with exactly one vanished
/// source and exactly one appeared source. The two sources differ by
/// construction: a file cannot both vanish from and appear in the
/// same directory (it is either on disk there, or not).
fn detect_moves (
  config : &SkgConfig,
) -> Result<Vec<NodeMove>, String> {
  let mut vanished_in : BTreeMap<ID, Vec<SourceName>> =
    BTreeMap::new ();
  let mut appeared_in : BTreeMap<ID, Vec<SourceName>> =
    BTreeMap::new ();
  for (source_name, source) in &config . sources {
    let (vanished, appeared) : (HashSet<ID>, HashSet<ID>) =
      vanished_and_appeared_in_source (source)
      . map_err ( |e| format! (
        "Could not read git status for source '{}': {}",
        source_name, e )) ?;
    for id in vanished {
      vanished_in . entry (id) . or_default ()
        . push ( source_name . clone () ); }
    for id in appeared {
      appeared_in . entry (id) . or_default ()
        . push ( source_name . clone () ); } }
  let mut moves : Vec<NodeMove> = // BTreeMap iteration is by sorted ID,
    Vec::new ();                  // so the output is deterministic.
  for (id, froms) in &vanished_in {
    if froms . len () != 1 { continue; }
    if let Some (tos) = appeared_in . get (id) {
      if tos . len () != 1 { continue; }
      moves . push ( NodeMove {
        id   : id . clone (),
        from : froms [0] . clone (),
        to   : tos   [0] . clone () } ); } }
  Ok (moves) }

/// For one source: the IDs whose file vanished (in HEAD, gone from
/// the worktree) and the IDs whose file appeared (in the worktree,
/// absent from HEAD). A source that is not a git repo participates in
/// no move -- nothing there is committed, and 'git add'/'git rm'
/// would be meaningless -- so it contributes empty sets.
fn vanished_and_appeared_in_source (
  source : &SkgfileSource,
) -> Result<(HashSet<ID>, HashSet<ID>), Box<dyn StdError>> {
  let source_path : &Path =
    source . path . as_path ();
  let repo : Repository = match open_repo (source_path) {
    Some (r) => r,
    None     => return Ok (( HashSet::new (), HashSet::new () )), };
  let disk : HashSet<ID> =
    disk_skg_ids (source_path) ?;
  let head : HashSet<ID> =
    head_skg_ids (&repo, source_path) ?;
  let vanished : HashSet<ID> =
    head . difference (&disk) . cloned () . collect ();
  let appeared : HashSet<ID> =
    disk . difference (&head) . cloned () . collect ();
  Ok (( vanished, appeared )) }

/// The IDs of '.skg' files currently on disk in a source directory.
fn disk_skg_ids (
  source_path : &Path,
) -> Result<HashSet<ID>, Box<dyn StdError>> {
  let mut ids : HashSet<ID> =
    HashSet::new ();
  for entry in fs::read_dir (source_path) ? {
    let entry : fs::DirEntry = entry ?;
    if let Some (id) = skg_id_from_filename ( &entry . path () ) {
      ids . insert (id); } }
  Ok (ids) }

/// The IDs of '.skg' files committed (in HEAD) within a source
/// directory. The repo may span several sources, so we scope to this
/// source's subtree rather than the whole HEAD tree. A repo with no
/// HEAD yet (no commits) has nothing committed, hence an empty set.
fn head_skg_ids (
  repo        : &Repository,
  source_path : &Path,
) -> Result<HashSet<ID>, Box<dyn StdError>> {
  let head_tree : Tree = match repo . head () {
    Ok (head) => head . peel_to_tree () ?,
    Err (_)   => return Ok ( HashSet::new () ), };
  let source_tree : Option<Tree> = {
    // The source directory relative to the repo worktree. An empty
    // relative path means the source IS the repo root, so we scan the
    // whole HEAD tree. 'None' means the path is outside the worktree
    // (e.g. a bare repo), which we treat as nothing committed.
    match path_relative_to_repo (repo, source_path) {
      None                                      => None,
      Some (rel) if rel . as_os_str () . is_empty () =>
        Some ( head_tree ),
      Some (rel) =>
        subtree_at (repo, &head_tree, &rel) ?, } };
  let mut ids : HashSet<ID> =
    HashSet::new ();
  if let Some (tree) = source_tree {
    for entry in tree . iter () {
      if entry . kind () != Some (ObjectType::Blob) { continue; }
      if let Some (id) = entry . name ()
        . and_then ( |name| skg_id_from_filename ( Path::new (name) )) {
          ids . insert (id); } } }
  Ok (ids) }

/// The subtree of 'root' at relative path 'rel', or None if 'rel' is
/// absent from the tree or names a non-tree.
fn subtree_at<'repo> (
  repo : &'repo Repository,
  root : &Tree,
  rel  : &Path,
) -> Result<Option<Tree<'repo>>, Box<dyn StdError>> {
  match root . get_path (rel) {
    Ok (entry) => {
      if entry . kind () != Some (ObjectType::Tree) {
        return Ok (None); }
      let object : Object = entry . to_object (repo) ?;
      Ok ( object . into_tree () . ok () ) },
    Err (e) if e . code () == ErrorCode::NotFound =>
      Ok (None),
    Err (e) => Err ( Box::new (e) ), } }

/// The node ID of a '.skg' file path: its stem. Non-'.skg' paths
/// yield None.
fn skg_id_from_filename (
  path : &Path,
) -> Option<ID> {
  if path . extension () . and_then ( |e| e . to_str () )
     != Some ("skg") {
    return None; }
  path . file_stem ()
    . and_then ( |stem| stem . to_str () )
    . map ( |stem| ID ( stem . to_string () )) }

/// Render the whole script: a header comment, then one block per
/// move. Empty when there are no moves.
fn render_moves_script (
  config : &SkgConfig,
  moves  : &[NodeMove],
) -> String {
  let mut out : String = String::from (
    "# Run this from the skg data root. Each block moves one node's\n\
     # .skg file from one source's git repo into another, then stages\n\
     # the removal and the addition.\n" );
  if moves . is_empty () {
    out . push_str ("# No moves detected.\n");
    return out; }
  for node_move in moves {
    out . push_str ( &render_one_move (config, node_move) ); }
  out }

/// One move's block, in the template the user specified: cd into the
/// old source, 'git rm', cd into the new (a sibling), 'git add', then
/// cd back. Source folders are named relative to the data root, so
/// the script is run from there.
fn render_one_move (
  config    : &SkgConfig,
  node_move : &NodeMove,
) -> String {
  let from_dir : String =
    source_dir_relative_to_data_root (config, &node_move . from);
  let to_dir : String =
    source_dir_relative_to_data_root (config, &node_move . to);
  format! (
    "echo \"----\"\n\
     id={id}\n\
     cd {from_dir}\n\
     git rm $id.skg\n\
     cd ../{to_dir}\n\
     git add $id.skg\n\
     cd ..\n",
    id       = node_move . id . 0,
    from_dir = from_dir,
    to_dir   = to_dir ) }

/// A source's directory, named relative to the data root. Source
/// paths are made absolute (data_root-joined) at config load, so
/// stripping the data_root prefix recovers the configured folder.
/// Falls back to the absolute path if the source lies outside the
/// data root.
fn source_dir_relative_to_data_root (
  config : &SkgConfig,
  source : &SourceName,
) -> String {
  let source_path : &Path = match config . sources . get (source) {
    Some (s) => s . path . as_path (),
    None     => return source . 0 . clone (), };
  let relative_or_absolute : PathBuf =
    source_path . strip_prefix ( &config . data_root )
    . map ( Path::to_path_buf )
    . unwrap_or_else ( |_| source_path . to_path_buf () );
  relative_or_absolute . to_string_lossy () . into_owned () }
