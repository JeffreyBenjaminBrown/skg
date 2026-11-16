//! This file defines text-claim validation, which is async because
//! it consults disk. A 'MemberAsSuchTextClaim' is the title/body
//! that an editable foreign member as such of a forks folder (a
//! forkee or forker as such) displayed in the buffer (an owned one
//! saves them instead); since title/body edits of a foreign node in
//! that position are forbidden, each claim must match the node's disk
//! state. Claims for nodes absent from disk are ignored.

use crate::dbs::node_lookup::opt_graphnode_by_gnid;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::from_text::local_fieldintent_collection::types::CollectedFieldIntents;
use crate::types::errors::BufferValidationError;
use crate::types::misc::SkgConfig;

use std::error::Error;

pub fn validate_text_claims (
  collected : &CollectedFieldIntents,
  graph     : &InRustGraph,
  config    : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  for pid in &collected . order {
    let Some (entry) = collected . by_pid . get (pid)
      else { continue; };
    if entry . text_claims . is_empty() {
      continue; }
    let Some (from_disk) =
      opt_graphnode_by_gnid (graph, config, pid) ?
      else { continue; };
    for claim in &entry . text_claims {
      if claim . title != from_disk . title
        || claim . body != from_disk . body
      { return Err (Box::new (BufferValidationError::Other (
          format!( "Cannot edit title/body for node {} ({}), a foreign node shown as a member of a forks folder. View the node as itself to edit title/body (an edit there starts the fork operation).",
                    pid, claim . headline )) )); }}}
  Ok (( )) }
