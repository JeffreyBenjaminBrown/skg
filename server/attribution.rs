//! A fork's attribution: what it records of the note it was forked
//! from ([[./types/nodes/attribution.rs]]), how a skgrepo is
//! identified across configs, and how the record is checked and fixed.
//! The user-facing account is 'docs/forks.org', "Attribution".
//!
//! A skgrepo's name is only a local alias, which each config may give
//! differently. Its identity is its git remote: the remote named
//! 'origin', else the first listed ('skgrepo_git_remote'), its URL
//! normalised ('normalised_remote_url') so that the ssh and https
//! spellings of one repository agree, then hashed
//! ('remote_hash_of_url'), so that a private remote's URL never
//! appears in a public note.

use crate::git_ops::read_gitrepo::{open_gitrepo, skgrepo_git_remote};
use crate::git_ops::misc::path_relative_to_gitrepo;
use crate::types::misc::{ID, SkgConfig, SkgrepoName};
use crate::types::nodes::attribution::Attribution;
use crate::types::nodes::complete::Graphnode;
use crate::types::nodes::fs::GraphnodeOnDisk;
use crate::util::path_from_pid_and_skgrepo;

use std::collections::{BTreeMap, HashMap};
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::{Mutex, OnceLock};
use std::time::{Duration, Instant};

/// A remote URL as the repository it names, whatever the transport:
/// the scheme, any user and password, and any port dropped, the host
/// lowercased, then any trailing slashes and one trailing '.git'
/// dropped. So 'git@github.com:Jeff/notes.git',
/// 'ssh://git@GitHub.com:22/Jeff/notes/' and
/// 'https://github.com/Jeff/notes' are each 'github.com/Jeff/notes'.
/// A 'file://' URL is its path; a local path is itself. The cases
/// 'tests/shared/remote-hash-cases.txt' lists hold for this function and
/// for the migration tool ('tools/migrate-attributions-2026-11/').
pub fn normalised_remote_url (
  url : &str,
) -> String {
  let url : &str = url . trim ();
  let host_and_path = |host : &str, path : &str| -> String {
    format! ( "{}/{}", host . to_lowercase (),
              path . trim_start_matches ('/') ) };
  let joined : String =
    if let Some ((scheme, rest)) = url . split_once ("://")
      . filter ( |(scheme, _)| ! scheme . is_empty ()
                 && scheme . chars () . all ( |c| c . is_ascii_alphanumeric ()
                                               || "+.-" . contains (c) ))
    { if scheme . eq_ignore_ascii_case ("file") { rest . to_string () }
      else {
        let (authority, path) : (&str, &str) =
          match rest . find ('/') {
            Some (i) => ( &rest [.. i], &rest [i ..] ),
            None     => ( rest, "" ) };
        let host_and_port : &str =
          authority . rsplit_once ('@') . map ( |(_, h)| h ) . unwrap_or (authority);
        let host : &str = match host_and_port . rsplit_once (':') {
          Some ((h, port)) if port . chars () . all ( |c| c . is_ascii_digit () )
            => h,
          _ => host_and_port };
        host_and_path (host, path) } }
    else if let Some ((before, path)) = url . split_once (':')
      . filter ( |(before, _)| ! before . contains ('/') && ! before . is_empty () )
    { // scp-like: [user@]host:path
      let host : &str =
        before . rsplit_once ('@') . map ( |(_, h)| h ) . unwrap_or (before);
      host_and_path (host, path) }
    else { url . to_string () };
  let trimmed : &str = joined . trim_end_matches ('/');
  let trimmed : &str = trimmed . strip_suffix (".git") . unwrap_or (trimmed);
  trimmed . trim_end_matches ('/') . to_string () }

/// The git blob hash (SHA-1) of the normalised URL: what
/// 'printf %s NORMALISED | git hash-object --stdin' prints.
pub fn remote_hash_of_url (
  url : &str,
) -> String {
  git2::Oid::hash_object (
    git2::ObjectType::Blob,
    normalised_remote_url (url) . as_bytes () )
    . map ( |oid| oid . to_string () )
    . expect ("hashing bytes as a git blob cannot fail") }

/// How long a skgrepo's remote hash is reused before its gitrepo is
/// read again: long enough that a redraw reads no gitrepo, short
/// enough that a remote changed while the server runs is soon seen.
const REMOTE_HASH_REUSE : Duration = Duration::from_secs (10);

/// The remote hash of the skgrepo at 'path', None when it has no
/// remote ('skgrepo_git_remote').
pub fn remote_hash_of_skgrepo_path (
  path : &Path,
) -> Option<String> {
  static RECENT : OnceLock<Mutex<HashMap<PathBuf, (Instant, Option<String>)>>>
    = OnceLock::new ();
  let recent = RECENT . get_or_init ( || Mutex::new (HashMap::new ()) );
  if let Ok (cache) = recent . lock () {
    if let Some ((when, hash)) = cache . get (path) {
      if when . elapsed () < REMOTE_HASH_REUSE { return hash . clone (); }} }
  let hash : Option<String> = skgrepo_git_remote (path)
    . map ( |(_, url)| remote_hash_of_url (&url) );
  if let Ok (mut cache) = recent . lock () {
    cache . insert ( path . to_path_buf (), (Instant::now (), hash . clone ()) ); }
  hash }

/// The remote hash of the configured skgrepo 'skgrepo'.
pub fn remote_hash_of_skgrepo (
  config  : &SkgConfig,
  skgrepo : &SkgrepoName,
) -> Option<String> {
  config . skgrepos . get (skgrepo)
    . and_then ( |s| remote_hash_of_skgrepo_path (&s . path) ) }

/// What a fork of 'original', homed in 'original_home', records,
/// when its clone is homed in 'clone_home'. None when the two homes
/// are one skgrepo: then the herald has nothing to say. Each git
/// fact is left out where it cannot be read.
pub fn attribution_at_fork (
  config        : &SkgConfig,
  original      : &ID,
  original_home : &SkgrepoName,
  clone_home    : &SkgrepoName,
) -> Option<Attribution> {
  if original_home == clone_home { return None; }
  let (commit, uncommitted) : (Option<String>, bool) =
    head_commit_and_whether_file_differs (config, original, original_home)
    . unwrap_or ((None, false));
  Some ( Attribution {
    original    : original . clone (),
    skgrepo     : original_home . clone (),
    remote_hash : remote_hash_of_skgrepo (config, original_home),
    commit,
    uncommitted, } ) }

/// The HEAD commit of the gitrepo holding the skgrepo, and whether
/// the original's home file differs from it (modified, staged, or
/// absent from HEAD).
fn head_commit_and_whether_file_differs (
  config        : &SkgConfig,
  original      : &ID,
  original_home : &SkgrepoName,
) -> Option<(Option<String>, bool)> {
  let skgrepo_path : &Path = & config . skgrepos . get (original_home) ? . path;
  let gitrepo : git2::Repository = open_gitrepo (skgrepo_path) ?;
  let commit : Option<String> = gitrepo . head () . ok ()
    . and_then ( |head| head . peel_to_commit () . ok () )
    . map ( |commit| commit . id () . to_string () );
  let file : PathBuf = PathBuf::from (
    path_from_pid_and_skgrepo (config, original_home, original . clone ()) . ok () ?);
  let file : PathBuf = fs::canonicalize (&file) . unwrap_or (file);
  let differs : bool = commit . is_some ()
    && path_relative_to_gitrepo (&gitrepo, &file)
       . and_then ( |relative| gitrepo . status_file (&relative) . ok () )
       . is_some_and ( |status| ! status . is_empty () );
  Some (( commit, differs )) }

/// The skgrepo an attribution names, as the herald shows it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AttributedSkgrepo {
  /// A loaded skgrepo is the one recorded: the one whose remote has
  /// the recorded hash, or, where none was recorded, the one of the
  /// recorded name, which has no remote either. Its current name.
  Loaded (SkgrepoName),
  /// No loaded skgrepo is known to be the one recorded: the recorded
  /// name.
  Unverified (SkgrepoName),
}

/// Which skgrepo 'attribution' names, as 'AttributedSkgrepo' says.
/// Of several loaded skgrepos with the recorded remote, the one of
/// the recorded name, else the first declared.
pub fn attributed_skgrepo (
  config      : &SkgConfig,
  attribution : &Attribution,
) -> AttributedSkgrepo {
  match &attribution . remote_hash {
    Some (hash) => {
      let with_the_remote : Vec<SkgrepoName> =
        config . ordered_skgrepos () . into_iter ()
        . filter ( |name| remote_hash_of_skgrepo (config, name) . as_ref () == Some (hash) )
        . collect ();
      if with_the_remote . contains (&attribution . skgrepo) {
        AttributedSkgrepo::Loaded (attribution . skgrepo . clone ()) }
      else { match with_the_remote . into_iter () . next () {
        Some (name) => AttributedSkgrepo::Loaded (name),
        None => AttributedSkgrepo::Unverified (attribution . skgrepo . clone ()) }} },
    None =>
      if config . skgrepos . contains_key (&attribution . skgrepo)
        && remote_hash_of_skgrepo (config, &attribution . skgrepo) . is_none ()
      { AttributedSkgrepo::Loaded (attribution . skgrepo . clone ()) }
      else { AttributedSkgrepo::Unverified (attribution . skgrepo . clone ()) }, }}

/// One line per kind of doubtful attribution among 'attributions'
/// (the attributions of notes in owned skgrepos), counting them, each
/// naming how to fix it ('rewrite_attributions'). Doubtful: a recorded
/// remote no loaded skgrepo has; a recorded name that now names a
/// skgrepo with another remote, or with none; with no remote recorded,
/// a name the config lacks, or one that now has a remote.
pub fn attribution_problems<'a> (
  config       : &SkgConfig,
  attributions : impl IntoIterator<Item = &'a Attribution>,
) -> Vec<String> {
  let config_path : String = config . config_path . display () . to_string ();
  let mut counts : BTreeMap<String, usize> = BTreeMap::new ();
  for attribution in attributions {
    if let Some (problem) = attribution_problem (config, attribution, &config_path) {
      *counts . entry (problem) . or_insert (0) += 1; }}
  counts . into_iter ()
    . map ( |(problem, count)| format! (
      "{} attribution{} of forks in your skgrepos: {}",
      count, if count == 1 { "" } else { "s" }, problem ))
    . collect () }

/// The attributions of the notes homed in owned skgrepos.
pub fn owned_attributions<'a> (
  config : &'a SkgConfig,
  nodes  : &'a [Graphnode],
) -> impl Iterator<Item = &'a Attribution> + 'a {
  nodes . iter ()
    . filter ( |node| config . skgrepo_is_owned (&node . home_skgrepo) )
    . filter_map ( |node| node . attribution . as_ref () ) }

/// Each file of an owned skgrepo that holds an attribution, read: the
/// home sections of the user's attributed forks.
pub fn owned_attributed_files (
  config : &SkgConfig,
) -> io::Result<Vec<(PathBuf, String, GraphnodeOnDisk)>> {
  let mut files : Vec<(PathBuf, String, GraphnodeOnDisk)> = Vec::new ();
  for name in config . ordered_skgrepos () {
    if ! config . skgrepo_is_owned (&name) { continue; }
    let Some (owned) = config . skgrepos . get (&name) else { continue; };
    for entry in fs::read_dir (&owned . path) ? {
      let path : PathBuf = entry ? . path ();
      if path . extension () . and_then ( |e| e . to_str () ) != Some ("skg") { continue; }
      let text : String = fs::read_to_string (&path) ?;
      if ! text . contains ("attribution:") { continue; }
      let node_fs : GraphnodeOnDisk = yaml_serde::from_str (&text)
        . map_err ( |e| io::Error::new ( io::ErrorKind::InvalidData,
                                         format! ("{}: {}", path . display (), e) )) ?;
      if node_fs . attribution . is_some () {
        files . push (( path, text, node_fs )); }}}
  Ok (files) }

fn attribution_problem (
  config      : &SkgConfig,
  attribution : &Attribution,
  config_path : &str,
) -> Option<String> {
  let name : &SkgrepoName = &attribution . skgrepo;
  let current_hash_of_name : Option<Option<String>> =
    config . skgrepos . contains_key (name)
    . then ( || remote_hash_of_skgrepo (config, name) );
  let fix = |old : &str| -> String { match config . skgrepos . contains_key (name) {
    true => format! (
      " If '{}' is the skgrepo forked from, run 'skg fix-attributions {} {} {}'.",
      name, config_path, old, name ),
    false => format! (
      " If it is loaded under another name, run 'skg fix-attributions {} {} NAME'.",
      config_path, old ) } };
  match &attribution . remote_hash {
    Some (hash) => {
      let loaded : AttributedSkgrepo = attributed_skgrepo (config, attribution);
      match (loaded, current_hash_of_name) {
        (AttributedSkgrepo::Unverified (_), _) => Some ( format! (
          "they name '{}', with a remote (hash {}) no loaded skgrepo has. It may only not be loaded, or its remote may have changed.{}",
          name, hash, fix (hash) )),
        (AttributedSkgrepo::Loaded (_), Some (now)) if now . as_ref () != Some (hash) => Some ( format! (
          "they name '{}', which now names a skgrepo with another remote; the herald shows the skgrepo with the recorded remote.",
          name )),
        _ => None } },
    None => match current_hash_of_name {
      None => Some ( format! (
        "they name '{}', which the config lacks, and record no remote.{}",
        name, fix (&name . 0) )),
      Some (Some (_)) => Some ( format! (
        "they name '{}' and record no remote, but '{}' now has one.{}",
        name, name, fix (&name . 0) )),
      Some (None) => None }, }}

/// Rewrite, in every home section of the owned skgrepos, each
/// attribution whose remote hash or skgrepo name is 'old' to name
/// 'skgrepo' as it is now: its name and its remote hash. The forked
/// note and the commit stay. Returns the files rewritten.
pub fn rewrite_attributions (
  config  : &SkgConfig,
  old     : &str,
  skgrepo : &SkgrepoName,
) -> io::Result<Vec<PathBuf>> {
  if ! config . skgrepos . contains_key (skgrepo) {
    return Err ( io::Error::new ( io::ErrorKind::NotFound, format! (
      "The config has no skgrepo '{}'.", skgrepo ))); }
  let remote_hash : Option<String> = remote_hash_of_skgrepo (config, skgrepo);
  let mut rewritten : Vec<PathBuf> = Vec::new ();
  for (path, text, mut node_fs) in owned_attributed_files (config) ? {
    let Some (attribution) = node_fs . attribution . as_mut () else { continue; };
    let matches : bool = attribution . skgrepo . 0 == old
      || attribution . remote_hash . as_deref () == Some (old);
    if ! matches { continue; }
    attribution . skgrepo = skgrepo . clone ();
    attribution . remote_hash = remote_hash . clone ();
    let yaml : String = node_fs . to_yaml ()
      . map_err ( |e| io::Error::new ( io::ErrorKind::InvalidData, e . to_string () )) ?;
    if yaml != text {
      fs::write (&path, yaml) ?;
      rewritten . push (path); }}
  Ok (rewritten) }

/// What the note a fork was forked from said then: its home file at
/// the commit the attribution records, read from the gitrepo of the
/// loaded skgrepo the attribution names. 'fork' is the fork's pid.
pub fn forked_original_at_fork_time (
  config : &SkgConfig,
  fork   : &ID,
) -> Result<String, String> {
  let attribution : Attribution = config . ordered_skgrepos () . into_iter ()
    . filter_map ( |name| path_from_pid_and_skgrepo (config, &name, fork . clone ()) . ok () )
    . filter_map ( |path| fs::read_to_string (path) . ok () )
    . filter_map ( |text| yaml_serde::from_str::<GraphnodeOnDisk> (&text) . ok () )
    . find_map ( |node_fs| node_fs . attribution )
    . ok_or_else ( || format! ( "No skgrepo holds a fork '{}' with an attribution.", fork . 0 )) ?;
  let commit : &str = attribution . commit . as_deref ()
    . ok_or ( "The attribution records no commit: the fork predates the record, or its original was in no gitrepo." ) ?;
  let skgrepo : SkgrepoName = match attributed_skgrepo (config, &attribution) {
    AttributedSkgrepo::Loaded (name) => name,
    AttributedSkgrepo::Unverified (name) => return Err ( format! (
      "No loaded skgrepo is known to be '{}', which the fork was forked from.", name )) };
  let skgrepo_path : &Path = & config . skgrepos . get (&skgrepo)
    . ok_or ( "The attributed skgrepo is not configured." ) ? . path;
  let gitrepo : git2::Repository = open_gitrepo (skgrepo_path)
    . ok_or_else ( || format! ( "Skgrepo '{}' is in no gitrepo.", skgrepo )) ?;
  let file : PathBuf = PathBuf::from (
    path_from_pid_and_skgrepo (config, &skgrepo, attribution . original . clone ()) ?);
  let file : PathBuf = fs::canonicalize (&file)
    . unwrap_or_else ( |_| fs::canonicalize (skgrepo_path)
                       . unwrap_or ( skgrepo_path . to_path_buf () )
                       . join ( file . file_name () . unwrap_or_default () ));
  let relative : PathBuf = path_relative_to_gitrepo (&gitrepo, &file)
    . ok_or ( "The original's file is outside its gitrepo." ) ?;
  let blob_text : String = gitrepo
    . revparse_single ( & format! ( "{}:{}", commit, relative . display () ))
    . map_err ( |e| format! ( "Commit {} lacks '{}': {}", commit, relative . display (), e )) ?
    . peel_to_blob () . map_err ( |e| e . to_string () )
    . map ( |blob| String::from_utf8_lossy ( blob . content () ) . into_owned () ) ?;
  let caveat : &str = if attribution . uncommitted {
    "# When the fork was made, the original's file differed from this\n\
     # commit: the text forked is the fork's own first version, not this.\n" }
    else { "" };
  Ok ( format! (
    "# '{}' in skgrepo '{}' at commit {}:\n{}{}",
    attribution . original . 0, skgrepo, commit, caveat, blob_text )) }

#[cfg(test)]
#[path = "../tests/unit/attribution.rs"]
mod tests;
