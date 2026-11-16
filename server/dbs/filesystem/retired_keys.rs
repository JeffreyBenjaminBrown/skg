//! The '.skg' keys retired in November 2026, which a server refuses
//! to read.
//!
//! 'subscribesTo' and 'overrides' became one key, 'forks', and
//! 'hidesFromSubs' became 'hides' (as did the October 2026 names of
//! the first two, 'subscribes_to' and 'overrides_view_of').
//! 'GraphnodeOnDisk' no longer has fields for them, and serde would
//! drop an unknown key silently; the next save of the section would
//! then erase it. So a worktree file holding one is unreadable, and a
//! git tree holding one is refused. The one-time tool
//! 'tools/migrate-forks-2026-11/migrate.py' rewrites old files; its
//! README.org says how to run it. This check stays until every
//! skgrepo is migrated.

use std::error::Error;
use std::fmt;
use std::path::PathBuf;

pub const RETIRED_SKG_KEYS : [&str; 5] = [
  "subscribesTo", "overrides", "hidesFromSubs",
  "subscribes_to", "overrides_view_of" ];

pub const FORKS_MIGRATION_TOOL : &str =
  "tools/migrate-forks-2026-11/migrate.py";

/// The retired keys among the top-level keys of 'yaml', in
/// 'RETIRED_SKG_KEYS' order; empty when it holds none, or does not
/// parse (whose error the ordinary read reports). Parses only a text
/// that contains a retired key's name somewhere.
pub fn retired_keys_held (
  yaml : &str,
) -> Vec<&'static str> {
  if ! RETIRED_SKG_KEYS . iter () . any ( |key| yaml . contains (key) ) {
    return Vec::new (); }
  let Ok (yaml_serde::Value::Mapping (mapping)) =
    yaml_serde::from_str::<yaml_serde::Value> (yaml)
    else { return Vec::new (); };
  RETIRED_SKG_KEYS . into_iter ()
    . filter ( |key| mapping . contains_key (*key) )
    . collect () }

/// Files that hold retired keys, each with the keys it holds, sorted
/// by path. Carried inside an 'io::Error' by the worktree reads, so
/// that a reader of a whole folder can name every such file.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RetiredKeysInFiles {
  pub files : Vec<(PathBuf, Vec<&'static str>)>,
}

impl RetiredKeysInFiles {
  pub fn into_io_error (
    mut self,
  ) -> std::io::Error {
    self . files . sort ();
    std::io::Error::new ( std::io::ErrorKind::InvalidData, self ) }

  /// The 'RetiredKeysInFiles' an 'io::Error' carries, if it carries one.
  pub fn carried_by (
    error : &std::io::Error,
  ) -> Option<&RetiredKeysInFiles> {
    error . get_ref ()
      . and_then ( |inner| inner . downcast_ref::<RetiredKeysInFiles> () ) }
}

impl fmt::Display for RetiredKeysInFiles {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>,
  ) -> fmt::Result {
    let files : Vec<String> = self . files . iter ()
      . map ( |(path, keys)| format! (
        "{} (holds {})", path . display (), quoted_keys (keys) ))
      . collect ();
    write! ( f, "{}; {}",
             files . join ("; "),
             what_to_do_about_retired_keys (None) ) }
}

impl Error for RetiredKeysInFiles {}

/// "'subscribesTo', 'overrides'"
pub fn quoted_keys (
  keys : &[&str],
) -> String {
  keys . iter ()
    . map ( |key| format! ("'{}'", key) )
    . collect::<Vec<String>> ()
    . join (", ") }

/// What to do about a file holding retired keys: its skgrepo's owner
/// migrates it. 'owned' says whose skgrepo it is, when known.
pub fn what_to_do_about_retired_keys (
  owned : Option<bool>,
) -> String {
  let retired : &str =
    "these keys were retired in November 2026";
  match owned {
    Some (true) => format! (
      "{}: run {} over your skgrepos (its README.org says how), then commit each",
      retired, FORKS_MIGRATION_TOOL ),
    Some (false) => format! (
      "{}: this skgrepo's owner should run {} over their skgrepos; until they have, you may remove it from skgconfig.toml",
      retired, FORKS_MIGRATION_TOOL ),
    None => format! (
      "{}: the skgrepo's owner should run {} (its README.org says how), or, for a foreign skgrepo, you may remove it from skgconfig.toml until its owner has",
      retired, FORKS_MIGRATION_TOOL ), }}

#[cfg(test)]
#[path = "../../../tests/unit/retired_keys.rs"]
mod tests;
