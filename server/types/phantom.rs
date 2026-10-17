/// Finding the home of a node the graph may not know,
/// such as one a phantom stands for.

use std::path::PathBuf;

use super::misc::{ID, SkgConfig, Skgrepo, SkgrepoName};

/// A node's HOME read from disk. When owned and non-owned files use
/// the same pid, the owned telescope wins; otherwise the home is
/// the most public skgrepo holding a section. Returns None if no
/// skgrepo holds one.
///
/// Walks 'ordered_repos' (the privacy order, most public first),
/// never 'config.repos' -- that is a HashMap, whose iteration
/// order Rust randomizes per process, so returning its first hit
/// answered arbitrarily for any node with more than one section.
/// Since the home is DEFINITIONALLY the most public section
/// (docs/telescopes.org), the first hit in privacy order is the
/// answer; a home whose section carries no title is a violation the
/// composition reports, not a reason to keep looking.
pub fn home_from_disk (
  skgid     : &ID,
  config : &SkgConfig,
) -> Option<SkgrepoName> {
  let filename : String = format!( "{}.skg", skgid . 0 );
  let ordered_skgrepos : Vec<SkgrepoName> = config . ordered_skgrepos ();
  for owned_only in [true, false] {
    for skgrepo_name in &ordered_skgrepos {
      if config . skgrepo_is_owned (skgrepo_name) != owned_only {
        continue; }
      let Some (skgrepo_config) : Option<&Skgrepo> =
        config . skgrepos . get (skgrepo_name) else { continue; };
      let path : PathBuf =
        PathBuf::from( &skgrepo_config . path ) . join (&filename);
      if path . exists() {
        return Some( skgrepo_name . clone () ); }} }
  None }

#[cfg(test)]
#[path = "../../tests/unit/types_phantom.rs"]
mod tests;
