/// Variations on a simple theme:
/// Producing a NodeComplete from different kinds of information.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::nodes::complete::NodeComplete;

use std::error::Error;

pub fn nodecomplete_by_id (
  graph  : &InRustGraph,
  _config : &SkgConfig,
  id     : &ID,
) -> Result<NodeComplete, Box<dyn Error>> {
  if let Some (n) = nodecomplete_from_graph (graph, id) { return Ok (n); }
  Err (format! ("Node '{}' not found in captured graph generation", id) . into ()) }

/// Like nodecomplete_by_id, but gives None if not found.
/// id-based. Preserves 'optnodecomplete_from_id' not-found behavior.
pub fn opt_nodecomplete_by_id (
  graph  : &InRustGraph,
  _config : &SkgConfig,
  id     : &ID,
) -> Result<Option<NodeComplete>, Box<dyn Error>> {
  Ok (nodecomplete_from_graph (graph, id)) }

/// Transitional disk lookup for callers not yet carrying an explicit graph.
pub fn nodecomplete_rustFirst_by_pid_and_source (
  graph  : &InRustGraph,
  config : &SkgConfig,
  pid    : &ID,
  source : &SourceName,
) -> Result<NodeComplete, Box<dyn Error>> {
  nodecomplete_graphFirst_by_pid_and_source (graph, config, pid, source) }

pub fn nodecomplete_graphFirst_by_pid_and_source (
  graph  : &InRustGraph,
  _config : &SkgConfig,
  pid    : &ID,
  _source : &SourceName,
) -> Result<NodeComplete, Box<dyn Error>> {
  if let Some (n) = nodecomplete_from_graph (graph, pid) { return Ok (n); }
  Err (format! ("Node '{}' not found in captured graph generation", pid) . into ()) }

pub fn nodecomplete_from_graph (
  graph : &InRustGraph,
  id    : &ID,
) -> Option<NodeComplete> {
  let pid : ID = graph . pid_of (id) ?;
  let rust = graph . nodes . get (&pid) ?;
  Some ( NodeComplete {
    pid                          : rust . pid . clone (),
    source                       : rust . source . clone (),
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
