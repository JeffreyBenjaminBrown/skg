/// Variations on a simple theme:
/// Producing a Graphnode from different kinds of information.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::{ID, SkgConfig, SkgrepoName};
use crate::types::nodes::complete::Graphnode;

use std::error::Error;

pub fn graphnode_by_gnid (
  graph  : &InRustGraph,
  _config : &SkgConfig,
  gnid     : &ID,
) -> Result<Graphnode, Box<dyn Error>> {
  if let Some (n) = graphnode_from_graph (graph, gnid) { return Ok (n); }
  Err (format! ("Node '{}' not found in captured graph generation", gnid) . into ()) }

/// Like graphnode_by_id, but gives None if not found.
/// id-based. Preserves 'optgraphnode_from_id' not-found behavior.
pub fn opt_graphnode_by_gnid (
  graph  : &InRustGraph,
  _config : &SkgConfig,
  gnid     : &ID,
) -> Result<Option<Graphnode>, Box<dyn Error>> {
  Ok (graphnode_from_graph (graph, gnid)) }

pub fn graphnode_graphFirst_by_pid_and_skgrepo (
  graph  : &InRustGraph,
  _config : &SkgConfig,
  pid    : &ID,
  _skgrepo : &SkgrepoName,
) -> Result<Graphnode, Box<dyn Error>> {
  if let Some (n) = graphnode_from_graph (graph, pid) { return Ok (n); }
  Err (format! ("Node '{}' not found in captured graph generation", pid) . into ()) }

pub fn graphnode_from_graph (
  graph : &InRustGraph,
  gnid    : &ID,
) -> Option<Graphnode> {
  let pid : ID = graph . pid_of (gnid) ?;
  let rust = graph . nodes . get (&pid) ?;
  Some ( Graphnode {
    pid                          : rust . pid . clone (),
    home_skgrepo                 : rust . home_skgrepo . clone (),
    extra_ids                    : rust . extra_ids . clone (),
    title                        : rust . title . clone (),
    overPrivateText_telescope               : rust . overPrivateText_telescope,
    aliases                      : rust . aliases . clone (),
    body                         : rust . body . clone (),
    contains                     : rust . contains . clone (),
    subscribesTo                 : rust . subscribesTo . clone (),
    hidesFromSubs                : rust . hidesFromSubs . clone (),
    overrides                    : rust . overrides . clone (),
    flags                        : rust . flags . clone (), } ) }
