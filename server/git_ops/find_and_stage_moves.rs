/// Detects nodes that moved from one skgrepo to another, and renders
/// a bash script -- a move list plus one loop over it -- that stages
/// each clean move ('git rm' in the old skgrepo, 'git add' in the new
/// one), skipping any whose file is still present in the old skgrepo
/// (so a stale entry cannot delete it).
///
/// Under privacy telescopes a node's sections live in several
/// skgrepos at once, so file presence alone no longer signals a move.
/// To move "the" node is to move its most public titled
/// representation: a node has "moved" iff its TITLE vanished from
/// EXACTLY one skgrepo (titled in that skgrepo's git HEAD, absent or
/// titleless in its worktree) and appeared in EXACTLY one other
/// (titled in the worktree, absent or titleless in HEAD). Titleless
/// section creations and deletions move individual relationships
/// between relRepos; they are not node moves, and stage as
/// ordinary edits.
///
/// A move is AUTO-STAGED only when it is a pure delete/create pair:
/// the old skgrepo's file is gone from its worktree and the new
/// skgrepo's file is absent from its HEAD. If instead either side
/// mixes the move into a pre-existing section file (the destination
/// already recorded relationships for the node, or the old skgrepo
/// keeps a titleless section), staging the file would drag those
/// relationship changes along -- so the pair is REPORTED for the
/// user to stage by hand, and never auto-staged.

use crate::types::misc::{ID, SkgConfig, SkgRepo, SkgRepoName};

use super::misc::path_relative_to_gitrepo;
use super::read_gitrepo::open_gitrepo;

use git2::{ErrorCode, Object, ObjectType, Repository, Tree};
use std::collections::{BTreeMap, HashMap};
use std::error::Error as StdError;
use std::fs;
use std::path::{Path, PathBuf};

/// A node whose title vanished from exactly one skgrepo and appeared
/// in exactly one (necessarily different) skgrepo.
struct NodeMove {
  skgid   : ID,
  from : SkgRepoName,
  to   : SkgRepoName,
  /// True iff the move is a pure delete/create pair, safe to stage
  /// mechanically; false means it is mixed into pre-existing
  /// section files and is only reported.
  clean : bool,
}

/// Top-level entry: the staging script for every detected move.
pub fn stage_moves_script (
  config : &SkgConfig,
) -> Result<String, String> {
  let moves : Vec<NodeMove> =
    detect_moves (config) ?;
  Ok ( render_moves_script (config, &moves) ) }

/// One skgrepo's view of one node's section file at one endpoint:
/// absent, or present with(out) a title.
#[derive(Clone, Copy, PartialEq)]
enum SectionSight {
  Absent,
  Titleless,
  Titled,
}

/// What one skgrepo holds for one id, in HEAD and on disk.
#[derive(Clone, Copy)]
struct SkgRepoSight {
  head : SectionSight,
  disk : SectionSight,
}

/// Scan every skgrepo, then keep only IDs whose TITLE vanished from
/// exactly one skgrepo and appeared in exactly one. The two skgrepos
/// differ by construction: vanishing requires the worktree title
/// absent, appearing requires it present.
fn detect_moves (
  config : &SkgConfig,
) -> Result<Vec<NodeMove>, String> {
  let mut sights : BTreeMap<ID, HashMap<SkgRepoName, SkgRepoSight>> =
    BTreeMap::new (); // BTreeMap: deterministic output order.
  for (skgrepo_name, skgrepo) in &config . skgrepos {
    for (skgid, sight) in
      sights_in_skgrepo (skgrepo)
      . map_err ( |e| format! (
        "Could not read git status for repo '{}': {}",
        skgrepo_name, e )) ? {
      sights . entry (skgid) . or_default ()
        . insert ( skgrepo_name . clone (), sight ); }}
  let mut moves : Vec<NodeMove> =
    Vec::new ();
  for (skgid, by_skgrepo) in &sights {
    let froms : Vec<&SkgRepoName> = // title vanished here
      by_skgrepo . iter ()
      . filter ( |(_, s)| s . head == SectionSight::Titled
                 && s . disk != SectionSight::Titled )
      . map ( |(name, _)| name ) . collect ();
    let tos : Vec<&SkgRepoName> = // title appeared here
      by_skgrepo . iter ()
      . filter ( |(_, s)| s . disk == SectionSight::Titled
                 && s . head != SectionSight::Titled )
      . map ( |(name, _)| name ) . collect ();
    if froms . len () != 1 || tos . len () != 1 { continue; }
    let (from, to) : (&SkgRepoName, &SkgRepoName) =
      (froms [0], tos [0]);
    let clean : bool = // a pure delete/create pair
      by_skgrepo [from] . disk == SectionSight::Absent
      && by_skgrepo [to] . head == SectionSight::Absent;
    moves . push ( NodeMove {
      skgid : skgid . clone (),
      from  : from . clone (),
      to    : to . clone (),
      clean } ); }
  Ok (moves) }

/// For one skgrepo: every id present in HEAD or on disk, with title
/// presence at both endpoints. A skgrepo that is not a gitrepo
/// participates in no move -- nothing there is committed, and
/// 'git add'/'git rm' would be meaningless -- so it contributes
/// nothing.
fn sights_in_skgrepo (
  skgrepo : &SkgRepo,
) -> Result<HashMap<ID, SkgRepoSight>, Box<dyn StdError>> {
  let skgrepo_path : &Path =
    skgrepo . path . as_path ();
  let gitrepo : Repository = match open_gitrepo (skgrepo_path) {
    Some (r) => r,
    None     => return Ok ( HashMap::new () ), };
  let disk : HashMap<ID, SectionSight> =
    disk_skg_sights (skgrepo_path) ?;
  let head : HashMap<ID, SectionSight> =
    head_skg_sights (&gitrepo, skgrepo_path) ?;
  let mut sights : HashMap<ID, SkgRepoSight> =
    HashMap::new ();
  for skgid in disk . keys () . chain ( head . keys () ) {
    sights . entry ( skgid . clone () )
      . or_insert ( SkgRepoSight {
        head : head . get (skgid) . copied ()
          . unwrap_or (SectionSight::Absent),
        disk : disk . get (skgid) . copied ()
          . unwrap_or (SectionSight::Absent) } ); }
  Ok (sights) }

/// Title presence, parsed leniently: any YAML mapping with a
/// non-null, non-empty 'title' counts as titled. Unparseable
/// content counts as titleless -- a gesture over mid-edit files
/// must not fail outright, and a file we cannot read is certainly
/// not a node's most public titled representation.
fn sight_of_content (
  content : &[u8],
) -> SectionSight {
  let titled : bool =
    std::str::from_utf8 (content) . ok ()
    . and_then ( |s|
        serde_yaml::from_str::<serde_yaml::Value> (s) . ok () )
    . as_ref ()
    . and_then ( |v| v . get ("title") )
    . map ( |t| match t {
        serde_yaml::Value::Null => false,
        serde_yaml::Value::String (s) => ! s . is_empty (),
        _ => true } )
    . unwrap_or (false);
  if titled { SectionSight::Titled }
  else      { SectionSight::Titleless }}

/// The '.skg' files currently on disk in a skgrepo directory, with
/// title presence.
fn disk_skg_sights (
  skgrepo_path : &Path,
) -> Result<HashMap<ID, SectionSight>, Box<dyn StdError>> {
  let mut sights : HashMap<ID, SectionSight> =
    HashMap::new ();
  for entry in fs::read_dir (skgrepo_path) ? {
    let entry : fs::DirEntry = entry ?;
    if let Some (skgid) = skg_skgid_from_filename ( &entry . path () ) {
      let content : Vec<u8> =
        fs::read ( entry . path () ) ?;
      sights . insert ( skgid, sight_of_content (&content) ); } }
  Ok (sights) }

/// The '.skg' files committed (in HEAD) within a skgrepo directory,
/// with title presence. The gitrepo may span several skgrepos, so we
/// scope to this skgrepo's subtree rather than the whole HEAD tree. A
/// skgrepo with no HEAD yet (no commits) has nothing committed, hence an
/// empty map.
fn head_skg_sights (
  gitrepo        : &Repository,
  skgrepo_path   : &Path,
) -> Result<HashMap<ID, SectionSight>, Box<dyn StdError>> {
  let head_tree : Tree = match gitrepo . head () {
    Ok (head) => head . peel_to_tree () ?,
    Err (_)   => return Ok ( HashMap::new () ), };
  let skgrepo_tree : Option<Tree> = {
    // The skgrepo directory relative to the gitrepo worktree. An empty
    // relative path means the skgrepo IS the gitrepo root, so we scan the
    // whole HEAD tree. 'None' means the path is outside the worktree
    // (e.g. a bare gitrepo), which we treat as nothing committed.
    match path_relative_to_gitrepo (gitrepo, skgrepo_path) {
      None                                      => None,
      Some (rel) if rel . as_os_str () . is_empty () =>
        Some ( head_tree ),
      Some (rel) =>
        subtree_at (gitrepo, &head_tree, &rel) ?, } };
  let mut sights : HashMap<ID, SectionSight> =
    HashMap::new ();
  if let Some (tree) = skgrepo_tree {
    for entry in tree . iter () {
      if entry . kind () != Some (ObjectType::Blob) { continue; }
      if let Some (skgid) = entry . name ()
        . and_then ( |name| skg_skgid_from_filename ( Path::new (name) )) {
          let blob : git2::Blob =
            gitrepo . find_blob ( entry . id () ) ?;
          sights . insert (
            skgid, sight_of_content ( blob . content () ) ); } } }
  Ok (sights) }

/// The subtree of 'root' at relative path 'rel', or None if 'rel' is
/// absent from the tree or names a non-tree.
fn subtree_at<'gitrepo> (
  gitrepo : &'gitrepo Repository,
  root : &Tree,
  rel  : &Path,
) -> Result<Option<Tree<'gitrepo>>, Box<dyn StdError>> {
  match root . get_path (rel) {
    Ok (entry) => {
      if entry . kind () != Some (ObjectType::Tree) {
        return Ok (None); }
      let object : Object = entry . to_object (gitrepo) ?;
      Ok ( object . into_tree () . ok () ) },
    Err (e) if e . code () == ErrorCode::NotFound =>
      Ok (None),
    Err (e) => Err ( Box::new (e) ), } }

/// The node ID of a '.skg' file path: its stem. Non-'.skg' paths
/// yield None.
fn skg_skgid_from_filename (
  path : &Path,
) -> Option<ID> {
  if path . extension () . and_then ( |e| e . to_str () )
     != Some ("skg") {
    return None; }
  path . file_stem ()
    . and_then ( |stem| stem . to_str () )
    . map ( |stem| ID ( stem . to_string () )) }

/// Render the whole script: a header comment, the clean-move list,
/// one bash loop that stages each clean move, and a comment block
/// reporting MIXED moves (those entangled with pre-existing section
/// files), which are never auto-staged. Empty (just a comment) when
/// there are no moves. The list and the loop are kept separate so a
/// human can read the data before running it, and the same text
/// reads cleanly in the shell afterward.
fn render_moves_script (
  config : &SkgConfig,
  moves  : &[NodeMove],
) -> String {
  let mut out : String =
    String::from (STAGE_MOVES_HEADER);
  let (clean, mixed) : (Vec<&NodeMove>, Vec<&NodeMove>) =
    moves . iter () . partition ( |m| m . clean );
  if clean . is_empty () && mixed . is_empty () {
    out . push_str ("# No moves detected.\n");
    return out; }
  if ! mixed . is_empty () {
    out . push_str (MIXED_MOVES_HEADER);
    for node_move in &mixed {
      out . push_str ( &format! (
        "#   {skgid} : {from} -> {to}\n",
        skgid   = node_move . skgid . 0,
        from = skgrepo_dir_relative_to_data_root (config, &node_move . from),
        to   = skgrepo_dir_relative_to_data_root (config, &node_move . to) )); }}
  if clean . is_empty () {
    out . push_str ("# No cleanly stageable moves detected.\n");
    return out; }
  out . push_str ("moves=(\n");
  for node_move in &clean {
    out . push_str ( &format! (
      "  \"{skgid} {from} {to}\"\n",
      skgid   = node_move . skgid . 0,
      from = skgrepo_dir_relative_to_data_root (config, &node_move . from),
      to   = skgrepo_dir_relative_to_data_root (config, &node_move . to) )); }
  out . push_str (")\n");
  out . push_str (STAGE_MOVES_LOOP);
  out }

/// Reported, never staged: staging either side's FILE would drag
/// unrelated relationship-record changes along with the move.
const MIXED_MOVES_HEADER : &str =
"# MOVES DETECTED BUT NOT STAGED: each node below moved (its title\n\
 # left one repo and arrived in another), but the move is mixed\n\
 # into section files that already existed -- the destination file\n\
 # already recorded relationships for the node, or the old repo\n\
 # keeps a titleless section. Staging those files whole could stage\n\
 # more than the move. Review and stage these by hand:\n";

/// The header comment, explaining how to run the script and why the
/// existence check precedes 'git rm'.
const STAGE_MOVES_HEADER : &str =
"# Run this from the skg data root (bash). Each entry below names one\n\
 # node's .skg file, the repo it moved out of, and the repo it\n\
 # moved into. The loop stages each move: 'git rm' in the old repo,\n\
 # 'git add' in the new one. 'git rm' deletes whatever is there, with\n\
 # no distinct output if the file is still present, so before removing\n\
 # we check that the file is ALREADY gone from the old repo's\n\
 # worktree -- which is how the move was detected. A still-present\n\
 # file means the move was not actually done on disk, so we skip it\n\
 # rather than delete it.\n";

/// The constant bash loop. Kept out of 'format!' (and free of
/// interpolation braces) so its quotes need no escaping. Repo
/// dirs are named relative to the data root and may nest at any
/// depth below it (e.g. 'owned/personal-proc'), so every 'cd' is
/// absolute, anchored at ORIGIN -- the data root, captured before
/// the loop -- rather than relative ('cd ..' would misnavigate
/// from a nested skgrepo). The final 'cd "$ORIGIN"' returns the
/// user to the data root whatever the last iteration did.
const STAGE_MOVES_LOOP : &str =
r#"
ORIGIN=$(pwd)
for move in "${moves[@]}"; do
  read -r ID OLD NEW <<< "$move"
  echo "---- moving $ID : $OLD -> $NEW"
  cd "$ORIGIN/$OLD" || { echo "  SKIP: cannot enter $OLD"; continue; }
  if [ -e "$ID.skg" ]; then
    echo "  SKIP: $ID.skg is still present in $OLD; leaving it alone"
    continue
  fi
  echo "  removing $ID from $OLD"
  git rm "$ID.skg"
  echo "  adding $ID to $NEW"
  cd "$ORIGIN/$NEW"
  git add "$ID.skg"
done
cd "$ORIGIN"
"#;

/// A skgrepo's directory, named relative to the data root. Repo
/// paths are made absolute (data_root-joined) at config load, so
/// stripping the data_root prefix recovers the configured folder.
/// Falls back to the absolute path if the skgrepo lies outside the
/// data root.
fn skgrepo_dir_relative_to_data_root (
  config : &SkgConfig,
  skgrepo : &SkgRepoName,
) -> String {
  let skgrepo_path : &Path = match config . skgrepos . get (skgrepo) {
    Some (s) => s . path . as_path (),
    None     => return skgrepo . 0 . clone (), };
  let relative_or_absolute : PathBuf =
    skgrepo_path . strip_prefix ( &config . data_root )
    . map ( Path::to_path_buf )
    . unwrap_or_else ( |_| skgrepo_path . to_path_buf () );
  relative_or_absolute . to_string_lossy () . into_owned () }
