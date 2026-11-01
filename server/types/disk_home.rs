//! Finding the home of a node the graph may not know,
//! such as one a ghost stands for.

use std::path::PathBuf;

use super::misc::{ID, SkgConfig, Skgrepo, SkgrepoName, SkgrepoOwner};
use crate::telescope::types::home_owner_of_sections;

/// A node's home read from disk: the first skgrepo in fold order
/// holding a section of the node's home owner
/// ('home_owner_of_sections'; sections in skgrepos of another owner
/// are illegal edits, not part of the node). Returns None if no
/// skgrepo holds one.
///
/// Walks 'ordered_skgrepos' (the fold order, most public first),
/// never 'config.repos' -- that is a HashMap, whose iteration
/// order Rust randomizes per process, so returning its first hit
/// answered arbitrarily for any node with more than one section.
/// Since the home is definitionally the most public section
/// (docs/telescopes.org), the first hit in fold order is the
/// answer; a home whose section carries no title is a violation the
/// composition reports, not a reason to keep looking.
pub fn home_from_disk (
  gnid     : &ID,
  config : &SkgConfig,
) -> Option<SkgrepoName> {
  let filename : String = format!( "{}.skg", gnid . 0 );
  let holding : Vec<SkgrepoName> =
    config . ordered_skgrepos () . into_iter ()
    . filter ( |skgrepo_name|
      config . skgrepos . get (skgrepo_name)
      . is_some_and ( |skgrepo : &Skgrepo|
        PathBuf::from ( &skgrepo . path ) . join (&filename) . exists () ))
    . collect ();
  let home_owner : SkgrepoOwner =
    home_owner_of_sections (config, holding . iter ()) ?;
  holding . into_iter ()
    . find ( |skgrepo_name| config . skgrepo_owner (skgrepo_name) == home_owner ) }

#[cfg(test)]
#[path = "../../tests/unit/types_disk_home.rs"]
mod tests;
