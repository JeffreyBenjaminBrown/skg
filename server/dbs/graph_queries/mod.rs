//! Reads from a supplied immutable graph, including authoritative absence.
//! Source-set arguments gate both relation provenance and visible partners.

pub mod all_graphnodestats;
pub mod ancestry;
pub mod nodes;
pub mod paths;
pub mod pids_from_ids;
pub mod relations;
pub mod subscriptions;

#[cfg(test)]
#[path = "../../../tests/unit/graph_queries.rs"]
mod tests;
