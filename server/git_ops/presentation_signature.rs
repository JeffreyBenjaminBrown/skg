//! Exact HEAD/index identity for configured direct `.skg` paths.
//!
//! Worktree bytes are deliberately absent: the selected-path manifest owns
//! that question.  This digest changes only when the Git presentation axes a
//! diff-mode view renders can change.

use crate::types::misc::SkgConfig;

use git2::{IndexEntry, Repository, Tree};
use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct PresentationSignature ([u8; 32]);

impl PresentationSignature {
  pub fn to_hex (self) -> String {
    blake3::Hash::from_bytes (self . 0) . to_hex () . to_string () }
}

#[derive(Default)]
struct RepoScopes {
  repo_path : PathBuf,
  prefixes  : BTreeSet<PathBuf>,
}

/// Hash HEAD modes/blobs and every index stage for only the configured direct
/// source children. Nested repositories are discovered from each source and
/// grouped by exact repository identity.
pub fn presentation_signature (
  config : &SkgConfig,
) -> Result<PresentationSignature, String> {
  let mut repositories : BTreeMap<PathBuf, RepoScopes> = BTreeMap::new ();
  for source_name in config . ordered_sources () {
    let source = config . sources . get (&source_name)
      . expect ("ordered source exists");
    let Ok (repo) = Repository::discover (&source . path) else { continue; };
    let Some (workdir) = repo . workdir () else { continue; };
    let prefix = source . path . strip_prefix (workdir)
      . map_err (|_| format! (
        "Configured source {} is outside repository {}",
        source . path . display (), workdir . display ()))?
      . to_path_buf ();
    let repo_path = repo . path () . canonicalize ()
      . unwrap_or_else (|_| repo . path () . to_path_buf ());
    let scopes = repositories . entry (repo_path . clone ())
      . or_insert_with (|| RepoScopes { repo_path, prefixes: BTreeSet::new () });
    scopes . prefixes . insert (prefix);
  }

  let mut hasher = blake3::Hasher::new ();
  for scopes in repositories . values () {
    let repo = Repository::open (&scopes . repo_path)
      . map_err (|error| error . to_string ())?;
    feed (&mut hasher, scopes . repo_path . to_string_lossy () . as_bytes ());
    let head_tree = repo . head () . ok ()
      . and_then (|head| head . peel_to_tree () . ok ());
    let index = repo . index () . map_err (|error| error . to_string ())?;
    let mut paths : BTreeSet<Vec<u8>> = index . iter ()
      . filter (|entry| direct_skg_in_scopes (&entry . path, &scopes . prefixes))
      . map (|entry| entry . path . clone ())
      . collect ();
    if let Some (tree) = &head_tree {
      collect_head_paths (&repo, tree, &scopes . prefixes, &mut paths)?; }
    for path_bytes in paths {
      feed (&mut hasher, &path_bytes);
      let path = Path::new (std::str::from_utf8 (&path_bytes)
        . map_err (|_| "A Git .skg path is not UTF-8" . to_string ())?);
      if let Some (tree) = &head_tree {
        if let Ok (entry) = tree . get_path (path) {
          feed (&mut hasher, b"H");
          feed (&mut hasher, &entry . filemode () . to_le_bytes ());
          feed (&mut hasher, entry . id () . as_bytes ());
        } else { feed (&mut hasher, b"h-absent"); }
      } else { feed (&mut hasher, b"h-unborn"); }
      let mut stages : Vec<IndexEntry> = index . iter ()
        . filter (|entry| entry . path == path_bytes)
        . collect ();
      stages . sort_by_key (index_stage);
      if stages . is_empty () { feed (&mut hasher, b"i-absent"); }
      for entry in stages {
        feed (&mut hasher, b"I");
        feed (&mut hasher, &[index_stage (&entry)]);
        feed (&mut hasher, &entry . mode . to_le_bytes ());
        feed (&mut hasher, entry . id . as_bytes ());
      }
    }
  }
  Ok (PresentationSignature (*hasher . finalize () . as_bytes ()))
}

fn direct_skg_in_scopes (path : &[u8], prefixes : &BTreeSet<PathBuf>) -> bool {
  let Ok (text) = std::str::from_utf8 (path) else { return false; };
  let path = Path::new (text);
  path . extension () . and_then (|extension| extension . to_str ()) == Some ("skg")
    && prefixes . iter () . any (|prefix|
      path . parent () == Some (prefix . as_path ()))
}

fn collect_head_paths (
  repo     : &Repository,
  tree     : &Tree<'_>,
  prefixes : &BTreeSet<PathBuf>,
  paths    : &mut BTreeSet<Vec<u8>>,
) -> Result<(), String> {
  for prefix in prefixes {
    let source_tree = if prefix . as_os_str () . is_empty () {
      tree . clone ()
    } else {
      let entry = match tree . get_path (prefix) {
        Ok (entry) => entry,
        Err (_) => continue,
      };
      repo . find_tree (entry . id ())
        . map_err (|error| error . to_string ())?
    };
    for entry in source_tree . iter () {
      let Some (name) = entry . name () else { continue; };
      if ! name . ends_with (".skg") { continue; }
      let path = if prefix . as_os_str () . is_empty () {
        PathBuf::from (name)
      } else { prefix . join (name) };
      paths . insert (path . to_string_lossy () . as_bytes () . to_vec ());
    }
  }
  Ok (( ))
}

fn index_stage (entry : &IndexEntry) -> u8 {
  ((entry . flags >> 12) & 0x3) as u8
}

fn feed (hasher : &mut blake3::Hasher, bytes : &[u8]) {
  hasher . update (&(bytes . len () as u64) . to_le_bytes ());
  hasher . update (bytes);
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::misc::{SkgfileSource, SourceName};
  use git2::Signature;
  use std::collections::HashMap;
  use std::fs;
  use tempfile::TempDir;

  fn config (source : &Path) -> SkgConfig {
    let name = SourceName::from ("source");
    SkgConfig::dummyFromSources (HashMap::from ([(name . clone (),
      SkgfileSource {
        name,
        abbreviation: None,
        path: source . to_path_buf (),
        user_owns_it: true,
      })]))
  }

  fn commit_index (repo : &Repository, message : &str) {
    let mut index = repo . index () . unwrap ();
    let tree_id = index . write_tree () . unwrap ();
    let tree = repo . find_tree (tree_id) . unwrap ();
    let signature = Signature::now ("test", "test@example.com") . unwrap ();
    let parents = repo . head () . ok ()
      . and_then (|head| head . peel_to_commit () . ok ())
      . into_iter () . collect::<Vec<_>> ();
    let parent_refs = parents . iter () . collect::<Vec<_>> ();
    repo . commit (Some ("HEAD"), &signature, &signature, message, &tree,
                   &parent_refs) . unwrap ();
  }

  #[test]
  fn signature_ignores_worktree_and_nested_paths_but_tracks_index () {
    let temp = TempDir::new () . unwrap ();
    let source = temp . path () . join ("source");
    fs::create_dir_all (&source) . unwrap ();
    let repo = Repository::init (temp . path ()) . unwrap ();
    fs::write (source . join ("a.skg"), "pid: a\ntitle: one\n") . unwrap ();
    { let mut index = repo . index () . unwrap ();
      index . add_path (Path::new ("source/a.skg")) . unwrap ();
      index . write () . unwrap (); }
    commit_index (&repo, "initial");
    let config = config (&source);
    let initial = presentation_signature (&config) . unwrap ();

    fs::write (source . join ("a.skg"), "pid: a\ntitle: two\n") . unwrap ();
    assert_eq! (presentation_signature (&config) . unwrap (), initial);

    { let mut index = repo . index () . unwrap ();
      index . add_path (Path::new ("source/a.skg")) . unwrap ();
      index . write () . unwrap (); }
    let staged = presentation_signature (&config) . unwrap ();
    assert_ne! (staged, initial);

    fs::create_dir_all (source . join ("nested")) . unwrap ();
    fs::write (source . join ("nested/ignored.skg"), "pid: ignored\n")
      . unwrap ();
    { let mut index = repo . index () . unwrap ();
      index . add_path (Path::new ("source/nested/ignored.skg")) . unwrap ();
      index . write () . unwrap (); }
    assert_eq! (presentation_signature (&config) . unwrap (), staged);
  }
}
