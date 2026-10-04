/// Variations on a simple theme:
/// Producing a Graphnode from different kinds of information.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::{ID, SkgConfig, RepoName};
use crate::types::nodes::complete::Graphnode;

use std::error::Error;

pub fn graphnode_by_id (
  graph  : &InRustGraph,
  _config : &SkgConfig,
  id     : &ID,
) -> Result<Graphnode, Box<dyn Error>> {
  if let Some (n) = graphnode_from_graph (graph, id) { return Ok (n); }
  Err (format! ("Node '{}' not found in captured graph generation", id) . into ()) }

/// Like graphnode_by_id, but gives None if not found.
/// id-based. Preserves 'optgraphnode_from_id' not-found behavior.
pub fn opt_graphnode_by_id (
  graph  : &InRustGraph,
  _config : &SkgConfig,
  id     : &ID,
) -> Result<Option<Graphnode>, Box<dyn Error>> {
  Ok (graphnode_from_graph (graph, id)) }

/// Transitional disk lookup for callers not yet carrying an explicit graph.
pub fn graphnode_rustFirst_by_pid_and_repo (
  graph  : &InRustGraph,
  config : &SkgConfig,
  pid    : &ID,
  repo : &RepoName,
) -> Result<Graphnode, Box<dyn Error>> {
  graphnode_graphFirst_by_pid_and_repo (graph, config, pid, repo) }

pub fn graphnode_graphFirst_by_pid_and_repo (
  graph  : &InRustGraph,
  _config : &SkgConfig,
  pid    : &ID,
  _repo : &RepoName,
) -> Result<Graphnode, Box<dyn Error>> {
  if let Some (n) = graphnode_from_graph (graph, pid) { return Ok (n); }
  Err (format! ("Node '{}' not found in captured graph generation", pid) . into ()) }

pub fn graphnode_from_graph (
  graph : &InRustGraph,
  id    : &ID,
) -> Option<Graphnode> {
  let pid : ID = graph . pid_of (id) ?;
  let rust = graph . nodes . get (&pid) ?;
  Some ( Graphnode {
    pid                          : rust . pid . clone (),
    home_repo                       : rust . home_repo . clone (),
    extra_ids                    : rust . extra_ids . clone (),
    title                        : rust . title . clone (),
    overPrivateText_telescope               : rust . overPrivateText_telescope,
    aliases                      : rust . aliases . clone (),
    body                         : rust . body . clone (),
    contains                     : rust . contains . clone (),
    subscribes_to                : rust . subscribes_to . clone (),
    hides_from_its_subscriptions : rust . hides_from_its_subscriptions . clone (),
    overrides_view_of            : rust . overrides_view_of . clone (),
    misc                         : rust . misc . clone (), } ) }
