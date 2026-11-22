//! Privacy telescopes: a node is its same-pid '.skg' files, one section
//! per skgrepo, most public first ('docs/telescopes.org'). This module
//! turns sections into a node and back; reading and writing the files
//! is 'dbs/filesystem/'.
//! - 'types': 'Telescope', sections, and illegal sections.
//! - 'compose': sections to the node's lists ('compose_telescope').
//! - 'decompose': a node to its sections ('decompose_node').
//! - 'invariants': the telescope violations a load or save reports.
//! - 'dependencies_manifest': the fold order of foreign skgrepos
//!   (OTHER_SKGREPO_DEPENDENCIES.toml).

pub mod dependencies_manifest;
pub mod compose;
pub mod invariants;
pub mod types;
pub mod decompose;

#[cfg(test)]
#[path = "../../tests/unit/telescope.rs"]
mod tests;
