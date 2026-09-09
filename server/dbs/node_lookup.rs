/// NodeComplete reads from one supplied immutable graph snapshot.
/// Absence is authoritative, including extra-ID and source lookups.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::{ID, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::nodes::rust::NodeRust;

use std::error::Error;

pub async fn nodeComplete_rustFIrst_by_id (
  graph  : &InRustGraph,
  id     : &ID,
) -> Result<NodeComplete, Box<dyn Error>> {
  nodecomplete_from_in_rust_graph (graph, id)
    . ok_or_else ( || format! (
      "Node {} not found in selected graph", id ) . into () ) }

pub async fn optNodeComplete_rustFIrst_by_id (
  graph  : &InRustGraph,
  id     : &ID,
) -> Result<Option<NodeComplete>, Box<dyn Error>> {
  Ok (nodecomplete_from_in_rust_graph (graph, id)) }

pub fn nodecomplete_rustFirst_by_pid_and_source (
  graph  : &InRustGraph,
  pid    : &ID,
  source : &SourceName,
) -> Result<NodeComplete, Box<dyn Error>> {
  nodecomplete_from_in_rust_graph (graph, pid)
    . ok_or_else ( || format! (
      "Node {} in source {} not found in selected graph", pid, source )
      . into () ) }

pub fn nodecomplete_from_in_rust_graph (
  graph : &InRustGraph,
  id    : &ID,
) -> Option<NodeComplete> {
  let pid : ID = graph . pid_of (id) ?;
  let rust : &NodeRust = graph . get (&pid) ?;
  Some ( NodeComplete {
    pid                          : rust . pid . clone (),
    source                       : rust . source . clone (),
    extra_ids                    : rust . extra_ids . clone (),
    title                        : rust . title . clone (),
    ugly_telescope               : rust . ugly_telescope,
    aliases                      : rust . aliases . clone (),
    body                         : rust . body . clone (),
    contains                     : rust . contains . clone (),
    subscribes_to                : rust . subscribes_to . clone (),
    hides_from_its_subscriptions : rust . hides_from_its_subscriptions . clone (),
    overrides_view_of            : rust . overrides_view_of . clone (),
    misc                         : rust . misc . clone (), } ) }

#[cfg(test)]
#[path = "../../tests/unit/selected_graph_reads.rs"]
mod selected_graph_reads;
