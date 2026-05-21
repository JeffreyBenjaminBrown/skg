/// Variations on a simple theme:
/// Producing a NodeComplete from different kinds of information.

use crate::dbs::filesystem::one_node::{
  nodecomplete_from_id,
  nodecomplete_from_pid_and_source};
use crate::dbs::in_rust_graph::snapshot_global;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::nodes::complete::NodeComplete;

use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Read a NodeComplete, starting from the inRustGraph,
/// falling back to disk if needed, by looking up an ID.
///
/// Async because the id->(pid, source) fallback goes through
/// 'nodecomplete_from_id', which consults TypeDB
/// when the in-Rust graph is uninitialized.
pub async fn nodeComplete_rustFIrst_by_id (
  config : &SkgConfig,
  driver : &TypeDBDriver,
  id     : &ID,
) -> Result<NodeComplete, Box<dyn Error>> {
  if let Some (n) = nodecomplete_from_in_rust_graph (id) {
    return Ok (n); }
  nodecomplete_from_id (config, driver, id) . await }

/// Like nodeComplete_rustFIrst_by_id, but gives None if not found.
/// id-based. Preserves 'optnodecomplete_from_id' not-found behavior.
pub async fn optNodeComplete_rustFIrst_by_id (
  config : &SkgConfig,
  driver : &TypeDBDriver,
  id     : &ID,
) -> Result<Option<NodeComplete>, Box<dyn Error>> {
  if let Some (n) = nodecomplete_from_in_rust_graph (id) {
    return Ok ( Some (n) ); }
  match nodecomplete_from_id (config, driver, id) . await {
    Ok (nodecomplete) => Ok ( Some (nodecomplete) ),
    Err (e) => {
      let error_msg : String = e . to_string ();
      if error_msg . contains ("not found")
        || error_msg . contains ("No such file")
        || error_msg . contains ("does not exist") {
          Ok (None) }
      else { Err (e) }} }}

/// InRustGraph-first NodeComplete read, disk fallback, given an
/// already-resolved '(pid, source)'.
/// Sync, because it never consults TypeDB.
pub fn nodecomplete_rustFirst_by_pid_and_source (
  config : &SkgConfig,
  pid    : &ID,
  source : &SourceName,
) -> Result<NodeComplete, Box<dyn Error>> {
  if let Some (n) = nodecomplete_from_in_rust_graph (pid)
    { return Ok (n); }
  Ok ( nodecomplete_from_pid_and_source (
         config, pid . clone (), source ) ? ) }

/// Synthesize a NodeComplete from the in-Rust graph if the id is
/// there (primary or extra). Returns None if in-Rust graph isn't
/// initialized or doesn't have the id.
pub fn nodecomplete_from_in_rust_graph (
  id: &ID
) -> Option<NodeComplete> {
  let graph_snap = snapshot_global () ?;
  let pid : ID = graph_snap . pid_of (id) ?;
  let rust = graph_snap . nodes . get (&pid) ?;
  Some ( NodeComplete {
    pid                          : rust . pid . clone (),
    source                       : rust . source . clone (),
    extra_ids                    : rust . extra_ids . clone (),
    title                        : rust . title . clone (),
    aliases                      : rust . aliases . clone (),
    body                         : rust . body . clone (),
    contains                     : rust . contains . clone (),
    subscribes_to                : rust . subscribes_to . clone (),
    hides_from_its_subscriptions : rust . hides_from_its_subscriptions . clone (),
    overrides_view_of            : rust . overrides_view_of . clone (),
    misc                         : rust . misc . clone (), } ) }
