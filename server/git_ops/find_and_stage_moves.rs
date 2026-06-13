/// Detects nodes whose '.skg' file moved from one source's git repo
/// to another, and renders a bash script -- a move list plus one loop
/// over it -- that stages each move ('git rm' in the old repo,
/// 'git add' in the new one), skipping any whose file is still
/// present in the old source (so a stale entry cannot delete it).
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

/// Render the whole script: a header comment, the move list, and one
/// bash loop that stages each move. Empty (just a comment) when there
/// are no moves. The list and the loop are kept separate so a human
/// can read the data before running it, and the same text reads
/// cleanly in the shell afterward.
fn render_moves_script (
  config : &SkgConfig,
  moves  : &[NodeMove],
) -> String {
  let mut out : String =
    String::from (STAGE_MOVES_HEADER);
  if moves . is_empty () {
    out . push_str ("# No moves detected.\n");
    return out; }
  out . push_str ("moves=(\n");
  for node_move in moves {
    out . push_str ( &format! (
      "  \"{id} {from} {to}\"\n",
      id   = node_move . id . 0,
      from = source_dir_relative_to_data_root (config, &node_move . from),
      to   = source_dir_relative_to_data_root (config, &node_move . to) )); }
  out . push_str (")\n");
  out . push_str (STAGE_MOVES_LOOP);
  out }

/// The header comment, explaining how to run the script and why the
/// existence check precedes 'git rm'.
const STAGE_MOVES_HEADER : &str =
"# Run this from the skg data root (bash). Each entry below names one\n\
 # node's .skg file, the source it moved out of, and the source it\n\
 # moved into. The loop stages each move: 'git rm' in the old source,\n\
 # 'git add' in the new one. 'git rm' deletes whatever is there, with\n\
 # no distinct output if the file is still present, so before removing\n\
 # we check that the file is ALREADY gone from the old source's\n\
 # worktree -- which is how the move was detected. A still-present\n\
 # file means the move was not actually done on disk, so we skip it\n\
 # rather than delete it.\n";

/// The constant bash loop. Kept out of 'format!' (and free of '{}')
/// so its braces and quotes need no escaping. Source dirs are named
/// relative to the data root, so 'cd ../$new' reaches a sibling and
/// the trailing 'cd ..' returns to the root for the next iteration.
const STAGE_MOVES_LOOP : &str =
r#"
for move in "${moves[@]}"; do
  read -r id old new <<< "$move"
  echo "---- moving $id : $old -> $new"
  cd "$old" || { echo "  SKIP: cannot enter $old"; continue; }
  if [ -e "$id.skg" ]; then
    echo "  SKIP: $id.skg is still present in $old; leaving it alone"
    cd ..
    continue
  fi
  echo "  removing $id from $old"
  git rm "$id.skg"
  echo "  adding $id to $new"
  cd "../$new"
  git add "$id.skg"
  cd ..
done
"#;

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
