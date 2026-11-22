//! Commits chosen nodes of the owned skgrepos ('docs/diff-view.org',
//! "Stage and commit"). A node's files are found from Git, not from
//! the graph: every '.skg' path of an owned skgrepo that is new
//! (untracked), changed or gone since HEAD, grouped by the file's
//! stem, which is its node's pid ('changed_skg_paths_by_stem').
//!
//! A commit's tree is HEAD's tree with the chosen paths taken from the
//! worktree (removed, for a deletion), not the git index: what was
//! staged by hand never goes in, and stays staged. Each owned skgrepo
//! with chosen paths gets one git-commit; in a gitrepo shared by
//! several, each commit's parent is the one before, and the last holds
//! every chosen path of the gitrepo. 'prepare_commits' builds every
//! tree and checks every author before anything is committed, so that
//! the caller can check the result first; 'commit_prepared' commits,
//! then sets each committed path's git index entry to the commit's.
//! Nothing in the worktree is written.

use crate::types::config::{SkgConfig, Skgrepo};
use crate::types::misc::{ID, SkgrepoName};

use super::git_snapshot::{path_is_skgrepo_skg, repo_prefix_in_gitrepo};
use super::read_gitrepo::open_gitrepo;

use git2::build::TreeUpdateBuilder;
use git2::{
  Commit, FileMode, Index, IndexEntry, IndexTime, ObjectType, Oid,
  Repository, Signature, Tree, TreeWalkMode, TreeWalkResult};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fs;
use std::path::{Path, PathBuf};

/// A '.skg' path of an owned skgrepo whose worktree differs from HEAD.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct ChangedSkgPath {
  pub skgrepo     : SkgrepoName,
  /// Relative to the gitrepo's root.
  pub path        : PathBuf,
  /// False when the worktree lacks it: a deletion.
  pub in_worktree : bool,
}

/// Every '.skg' path of every owned skgrepo that is new, changed or
/// gone since HEAD, whatever the git index holds, by the file's stem
/// ('skg_paths_differing_from_head'). An owned skgrepo outside a
/// gitrepo, or without a HEAD commit, refuses.
pub fn changed_skg_paths_by_stem (
  config : &SkgConfig,
) -> Result<BTreeMap<ID, Vec<ChangedSkgPath>>, String> {
  let mut by_stem : BTreeMap<ID, Vec<ChangedSkgPath>> = BTreeMap::new ();
  for skgrepo_name in config . privacy_ladder () {
    let skgrepo : &Skgrepo = config . skgrepo_or_err (&skgrepo_name) ?;
    let in_skgrepo = |e : String| -> String {
      format! ( "skgrepo {}: {}", skgrepo_name, e ) };
    let gitrepo : Repository = open_gitrepo ( Path::new (&skgrepo . path) )
      . ok_or_else ( || in_skgrepo ( "it is not in a git repository" . to_string () )) ?;
    let prefix : PathBuf =
      repo_prefix_in_gitrepo ( &gitrepo, Path::new (&skgrepo . path) )
      . map_err (in_skgrepo) ?;
    for (path, in_worktree) in
      skg_paths_differing_from_head (&gitrepo, &prefix) . map_err (in_skgrepo) ? {
      let stem : String = path . file_stem ()
        . map ( |stem| stem . to_string_lossy () . to_string () )
        . unwrap_or_default ();
      by_stem . entry ( ID::from (stem) ) . or_default () . push ( ChangedSkgPath {
        skgrepo : skgrepo_name . clone (), path, in_worktree } ); }}
  Ok (by_stem) }

/// The '.skg' files directly in the folder 'prefix' (relative to the
/// gitrepo's root) whose worktree contents differ from HEAD's, with
/// whether the worktree holds each (false: a deletion), whatever the
/// git index holds. Only the folder itself, not its subfolders, as the
/// live graph reads a skgrepo ('read_skg_sections_from_folder'); and
/// a file the gitrepo ignores counts too, since the live graph reads it.
/// A file whose raw bytes hash differently from HEAD's blob is hashed
/// again through the gitrepo's filters (line endings, say), as 'git
/// add' would store it; that writes its blob to the object database.
pub fn skg_paths_differing_from_head (
  gitrepo : &Repository,
  prefix  : &Path,
) -> Result<Vec<(PathBuf, bool)>, String> {
  let head_tree : Tree = gitrepo . head ()
    . and_then ( |head| head . peel_to_tree () )
    . map_err ( |e| format! ( "it has no HEAD commit: {}", e . message () )) ?;
  let workdir : &Path = gitrepo . workdir ()
    . ok_or_else ( || "its gitrepo has no worktree" . to_string () ) ?;
  let at_head : BTreeMap<PathBuf, Oid> = skg_blobs_in_tree (&head_tree, prefix) ?;
  let in_worktree : BTreeSet<PathBuf> = skg_files_in_worktree (workdir, prefix) ?;
  let mut differing : Vec<(PathBuf, bool)> = Vec::new ();
  for path in at_head . keys () . chain ( in_worktree . iter () )
    . collect::<BTreeSet<&PathBuf>> () {
    let in_worktree : bool = in_worktree . contains (path);
    let changed : bool = match ( at_head . get (path), in_worktree ) {
      (Some (head_blob), true) => {
        let file : PathBuf = workdir . join (path);
        let raw : Oid = Oid::hash_file ( ObjectType::Blob, &file )
          . map_err ( |e| format! ( "hashing {:?}: {}", path, e . message () )) ?;
        raw != *head_blob
          && gitrepo . blob_path (&file)
             . map_err ( |e| format! ( "hashing {:?}: {}", path, e . message () )) ?
             != *head_blob },
      _ => true, };
    if changed { differing . push (( path . clone (), in_worktree )); }}
  Ok (differing) }

/// The blob 'path' has at HEAD, and the blob its worktree file would
/// be stored as (through the gitrepo's filters, as 'git add' would
/// store it, which writes it to the object database); None where it is
/// absent.
pub fn blobs_at_head_and_in_worktree (
  config : &SkgConfig,
  path   : &ChangedSkgPath,
) -> Result<(Option<Oid>, Option<Oid>), String> {
  let skgrepo : &Skgrepo = config . skgrepo_or_err (&path . skgrepo) ?;
  let gitrepo : Repository = open_gitrepo ( Path::new (&skgrepo . path) )
    . ok_or_else ( || format! ( "skgrepo {} is not in a git repository", path . skgrepo )) ?;
  let at_head : Option<Oid> = gitrepo . head ()
    . and_then ( |head| head . peel_to_tree () ) . ok ()
    . and_then ( |tree| tree . get_path (&path . path) . ok () )
    . map ( |entry| entry . id () );
  let in_worktree : Option<Oid> =
    if ! path . in_worktree { None }
    else {
      let workdir : &Path = gitrepo . workdir ()
        . ok_or_else ( || "its gitrepo has no worktree" . to_string () ) ?;
      Some ( gitrepo . blob_path ( &workdir . join (&path . path) )
             . map_err ( |e| format! ( "hashing {:?}: {}", path . path, e . message () )) ? ) };
  Ok (( at_head, in_worktree )) }

/// The text of the blob 'blob' in the gitrepo of the skgrepo 'skgrepo'.
pub fn blob_text (
  config  : &SkgConfig,
  skgrepo : &SkgrepoName,
  blob    : Oid,
) -> Result<String, String> {
  let skgrepo_config : &Skgrepo = config . skgrepo_or_err (skgrepo) ?;
  let gitrepo : Repository = open_gitrepo ( Path::new (&skgrepo_config . path) )
    . ok_or_else ( || format! ( "skgrepo {} is not in a git repository", skgrepo )) ?;
  let found = gitrepo . find_blob (blob)
    . map_err ( |e| format! ( "reading blob {}: {}", blob, e . message () )) ?;
  String::from_utf8 ( found . content () . to_vec () )
    . map_err ( |e| format! ( "blob {} is not UTF-8: {}", blob, e )) }

/// Whether 'path' is a '.skg' file directly in the folder 'prefix'.
fn is_skg_directly_in (
  path   : &Path,
  prefix : &Path,
) -> bool {
  path . parent () == Some (prefix) && path_is_skgrepo_skg (path, prefix) }

/// The blob ID of each '.skg' file directly in the folder 'prefix' in
/// 'tree', by its path relative to the gitrepo's root.
fn skg_blobs_in_tree (
  tree   : &Tree,
  prefix : &Path,
) -> Result<BTreeMap<PathBuf, Oid>, String> {
  let mut blobs : BTreeMap<PathBuf, Oid> = BTreeMap::new ();
  tree . walk ( TreeWalkMode::PreOrder, |root, entry| {
    if entry . kind () == Some (ObjectType::Blob) {
      let path : PathBuf = PathBuf::from (root) . join ( entry . name () . unwrap_or ("") );
      if is_skg_directly_in (&path, prefix) {
        blobs . insert ( path, entry . id () ); }}
    TreeWalkResult::Ok } )
    . map_err ( |e| format! ( "reading HEAD's tree: {}", e . message () )) ?;
  Ok (blobs) }

/// The '.skg' files directly in the folder 'prefix' of the worktree,
/// by their paths relative to the gitrepo's root.
fn skg_files_in_worktree (
  workdir : &Path,
  prefix  : &Path,
) -> Result<BTreeSet<PathBuf>, String> {
  let mut files : BTreeSet<PathBuf> = BTreeSet::new ();
  for entry in fs::read_dir ( workdir . join (prefix) )
    . map_err ( |e| format! ( "reading {:?}: {}", prefix, e )) ? {
    let entry : fs::DirEntry = entry
      . map_err ( |e| format! ( "reading {:?}: {}", prefix, e )) ?;
    let path : PathBuf = prefix . join ( entry . file_name () );
    // Following a symlink, as the live graph's reader does.
    let is_file : bool = workdir . join (&path) . is_file ();
    if is_file && is_skg_directly_in (&path, prefix) { files . insert (path); }}
  Ok (files) }

/// One git-commit to be made: of 'skgrepo', in the gitrepo whose root
/// is 'gitrepo', from the tree 'parent_tree' to 'tree', taking
/// 'paths'.
#[derive(Clone, Debug)]
pub struct PreparedCommit {
  pub skgrepo     : SkgrepoName,
  pub gitrepo     : PathBuf,
  pub parent_tree : Oid,
  pub tree        : Oid,
  pub paths       : Vec<ChangedSkgPath>,
}

/// Every git-commit to be made, in declaration order, and the tree each
/// owned skgrepo's gitrepo will hold once all are made (HEAD's, for a
/// gitrepo with none).
#[derive(Clone, Debug, Default)]
pub struct PreparedCommits {
  pub commits       : Vec<PreparedCommit>,
  pub final_tree_of : HashMap<SkgrepoName, Oid>,
}

/// Build, for each owned skgrepo in declaration order, the tree of its
/// commit: the tree its gitrepo holds before it (HEAD's, or that of
/// the commit before it in the same gitrepo) with its paths among
/// 'paths' taken from the worktree. A skgrepo whose tree would not
/// change gets no commit. Blobs are written to the object database,
/// trees too, but no commit, ref or git index. Refuses, naming the
/// skgrepo, when one with a commit to make has no configured git
/// author.
pub fn prepare_commits (
  config : &SkgConfig,
  paths  : &[ChangedSkgPath],
) -> Result<PreparedCommits, String> {
  let mut prepared : PreparedCommits = PreparedCommits::default ();
  let mut tree_of_gitrepo : HashMap<PathBuf, Oid> = HashMap::new ();
  let owned : Vec<SkgrepoName> = config . privacy_ladder ();
  let mut gitrepo_of : HashMap<SkgrepoName, PathBuf> = HashMap::new ();
  for skgrepo_name in &owned {
    let in_skgrepo = |e : String| -> String {
      format! ( "skgrepo {}: {}", skgrepo_name, e ) };
    let skgrepo : &Skgrepo = config . skgrepo_or_err (skgrepo_name) ?;
    let gitrepo : Repository = open_gitrepo ( Path::new (&skgrepo . path) )
      . ok_or_else ( || in_skgrepo ( "it is not in a git repository" . to_string () )) ?;
    let root : PathBuf = gitrepo_root (&gitrepo);
    gitrepo_of . insert ( skgrepo_name . clone (), root . clone () );
    let parent_tree : Oid = match tree_of_gitrepo . get (&root) {
      Some (tree) => *tree,
      None => gitrepo . head ()
        . and_then ( |head| head . peel_to_tree () )
        . map ( |tree| tree . id () )
        . map_err ( |e| in_skgrepo ( format! (
          "it has no HEAD commit: {}", e . message () ))) ?, };
    let mine : Vec<ChangedSkgPath> = paths . iter ()
      . filter ( |path| &path . skgrepo == skgrepo_name )
      . cloned () . collect ();
    let tree : Oid = tree_with_paths (&gitrepo, parent_tree, &mine)
      . map_err ( |e| in_skgrepo ( format! (
        "building its commit's tree failed: {}", e . message () ))) ?;
    tree_of_gitrepo . insert ( root . clone (), tree );
    if tree != parent_tree {
      gitrepo . signature () . map_err ( |e| in_skgrepo ( format! (
        "no git author is configured: {}", e . message () ))) ?;
      prepared . commits . push ( PreparedCommit {
        skgrepo : skgrepo_name . clone (), gitrepo : root,
        parent_tree, tree, paths : mine } ); }}
  prepared . final_tree_of = gitrepo_of . into_iter ()
    . map ( |(skgrepo, root)| ( skgrepo, tree_of_gitrepo [&root] ))
    . collect ();
  Ok (prepared) }

/// The tree 'base' with each of 'paths' taken from the worktree: its
/// file's contents, with the mode 'base' gave the path or a plain
/// file's, or, for a deletion, nothing.
fn tree_with_paths (
  gitrepo : &Repository,
  base    : Oid,
  paths   : &[ChangedSkgPath],
) -> Result<Oid, git2::Error> {
  if paths . is_empty () { return Ok (base); }
  let base_tree : Tree = gitrepo . find_tree (base) ?;
  let workdir : &Path = gitrepo . workdir ()
    . ok_or_else ( || git2::Error::from_str ("the gitrepo has no worktree") ) ?;
  let mut update : TreeUpdateBuilder = TreeUpdateBuilder::new ();
  for path in paths {
    if path . in_worktree {
      let blob : Oid = gitrepo . blob_path ( &workdir . join (&path . path) ) ?;
      let mode : FileMode = base_tree . get_path (&path . path) . ok ()
        . and_then ( |entry| file_mode_of ( entry . filemode () ))
        . unwrap_or (FileMode::Blob);
      update . upsert ( &path . path, blob, mode );
    } else { update . remove ( &path . path ); }}
  update . create_updated (gitrepo, &base_tree) }

fn file_mode_of (
  raw : i32,
) -> Option<FileMode> {
  [ FileMode::Blob, FileMode::BlobExecutable, FileMode::BlobGroupWritable ]
    . into_iter () . find ( |mode| i32::from (*mode) == raw ) }

/// Make each prepared commit, in order, with 'message', by its
/// gitrepo's configured user, then set the git index entry of each of
/// its paths to the commit's (removing it, for a deletion), leaving
/// every other entry as it was. Returns each skgrepo committed, with
/// its commit, and the errors met. First every gitrepo's HEAD must
/// still hold the tree its first commit was built on, or nothing is
/// committed. A commit that then fails stops the rest; those made
/// stay made.
pub fn commit_prepared (
  prepared : &PreparedCommits,
  message  : &str,
) -> (Vec<(SkgrepoName, Oid)>, Vec<String>) {
  let mut commits : Vec<(SkgrepoName, Oid)> = Vec::new ();
  let mut errors : Vec<String> = Vec::new ();
  let mut gitrepos_seen : BTreeSet<&PathBuf> = BTreeSet::new ();
  for commit in &prepared . commits {
    if gitrepos_seen . insert (&commit . gitrepo) {
      if let Err (e) = head_holds_tree (&commit . gitrepo, commit . parent_tree) {
        errors . push ( format! (
          "Nothing was committed: skgrepo {}: {}", commit . skgrepo, e ));
        return (commits, errors); }}}
  for commit in &prepared . commits {
    match commit_one (commit, message) {
      Ok (commit_id) => {
        commits . push (( commit . skgrepo . clone (), commit_id ));
        if let Err (e) = set_index_entries (commit) {
          errors . push ( format! (
            "skgrepo {}: committed, but its git index was not updated: {}",
            commit . skgrepo, e )); }},
      Err (e) => {
        errors . push ( format! ( "skgrepo {}: {}", commit . skgrepo, e ));
        break; }, }}
  (commits, errors) }

/// Whether the gitrepo's HEAD commit holds the tree 'tree'.
fn head_holds_tree (
  gitrepo : &Path,
  tree    : Oid,
) -> Result<(), String> {
  let gitrepo : Repository = Repository::open (gitrepo)
    . map_err ( |e| format! ( "opening its gitrepo failed: {}", e . message () )) ?;
  let head_tree : Oid = gitrepo . head ()
    . and_then ( |head| head . peel_to_tree () )
    . map ( |tree| tree . id () )
    . map_err ( |e| format! ( "it has no HEAD commit: {}", e . message () )) ?;
  if head_tree == tree { Ok (( )) }
  else { Err ( "its HEAD moved while the commit was prepared" . to_string () ) }}

fn commit_one (
  commit  : &PreparedCommit,
  message : &str,
) -> Result<Oid, String> {
  let gitrepo : Repository = Repository::open (&commit . gitrepo)
    . map_err ( |e| format! ( "opening its gitrepo failed: {}", e . message () )) ?;
  let head_commit : Commit = gitrepo . head ()
    . and_then ( |head| head . peel_to_commit () )
    . map_err ( |e| format! ( "it has no HEAD commit: {}", e . message () )) ?;
  if head_commit . tree_id () != commit . parent_tree {
    return Err ( "its HEAD moved while the commit was prepared" . to_string () ); }
  let signature : Signature = gitrepo . signature ()
    . map_err ( |e| format! ( "no git author is configured: {}", e . message () )) ?;
  let tree : Tree = gitrepo . find_tree (commit . tree)
    . map_err ( |e| e . message () . to_string () ) ?;
  gitrepo . commit ( Some ("HEAD"), &signature, &signature, message,
                     &tree, &[ &head_commit ] )
    . map_err ( |e| format! ( "git-commit failed: {}", e . message () )) }

/// Set the git index entry of each of the commit's paths to what its
/// tree holds there, removing it where the tree holds nothing. The
/// entry's file stat is left zero, so that Git compares the worktree
/// file's contents with it rather than trusting a stat.
fn set_index_entries (
  commit : &PreparedCommit,
) -> Result<(), String> {
  let gitrepo : Repository = Repository::open (&commit . gitrepo)
    . map_err ( |e| e . message () . to_string () ) ?;
  let tree : Tree = gitrepo . find_tree (commit . tree)
    . map_err ( |e| e . message () . to_string () ) ?;
  let mut index : Index = gitrepo . index ()
    . map_err ( |e| e . message () . to_string () ) ?;
  for path in &commit . paths {
    match tree . get_path (&path . path) {
      Ok (entry) => index . add ( &IndexEntry {
        ctime : IndexTime::new (0, 0), mtime : IndexTime::new (0, 0),
        dev : 0, ino : 0, uid : 0, gid : 0, file_size : 0,
        mode           : entry . filemode () as u32,
        id             : entry . id (),
        flags          : 0,
        flags_extended : 0,
        path           : path . path . to_string_lossy () . as_bytes () . to_vec (), } ),
      Err (_) => index . remove_path (&path . path)
        . or_else ( |e| if e . code () == git2::ErrorCode::NotFound { Ok (( )) }
                        else { Err (e) } ), }
    . map_err ( |e| format! ( "{:?}: {}", path . path, e . message () )) ?; }
  index . write () . map_err ( |e| e . message () . to_string () ) }

/// The gitrepo's worktree (its git folder if it has none), without a
/// trailing separator.
fn gitrepo_root (
  gitrepo : &Repository,
) -> PathBuf {
  gitrepo . workdir () . unwrap_or ( gitrepo . path () )
    . components () . collect () }
