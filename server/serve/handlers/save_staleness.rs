//! The save's refusal of a stale buffer: one rendered before a node it
//! would write last changed. Writing it would silently undo that
//! change, so the whole save is refused and nothing is written.
//!
//! Only what the buffer itself asks to write is checked: the
//! nodeInstructions the buffer's text yields once the noop filter has
//! dropped the ones identical to the graph, and the nodes its
//! nodeMerges write or delete. A node the buffer creates has no last
//! change, and is never stale. A node the buffer shows as the graph
//! has it is not written, so it saves silently however old the
//! buffer. The writes a save derives from the current graph (delete
//! propagation, strandeds resets, Hoist repairs, fork clones, restore
//! requests) undo nothing, and are not checked. A buffer no view
//! response drew, such as a client's new empty view, is dated by the
//! 'runtime generation' request its client sent on opening it.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::errors::{BufferValidationError, HeadlineName};
use crate::types::misc::ID;
use crate::types::save::{DeleteNode, NodeInstruction, NodeMerge, SaveNode};
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{ViewnodeKind, Vognode};
use crate::serve::util::value_from_request_sexp;

use std::collections::BTreeSet;

/// When the saved buffer's text was rendered, as its save request
/// says: '(rendered-at . "N")', runtime generation N, as the response
/// that last drew the buffer said.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct RenderedAt (pub u64);

/// The request's '(rendered-at . "N")'. A request without it, or with
/// anything but a number, is refused: omitting it must not bypass the
/// staleness check.
pub fn rendered_at_from_request (
  request : &str,
) -> Result<RenderedAt, String> {
  let value : String =
    value_from_request_sexp ("rendered-at", request)
    . map_err ( |_| concat! (
      "The save request lacks the field '(rendered-at . \"N\")': ",
      "the runtime generation its buffer was rendered at, which every view ",
      "response carries, and the 'runtime generation' request answers for ",
      "a new empty view. Nothing was saved.") . to_string () ) ?;
  value . parse::<u64> ()
    . map ( RenderedAt )
    . map_err ( |_| format! (
      "The save request's 'rendered-at' is {:?}, not a runtime generation number. Nothing was saved.",
      value )) }

/// One error per node the buffer would write that changed after
/// 'rendered_at', naming the buffer's occurrences of it.
pub fn stale_write_errors (
  graph            : &InRustGraph,
  rendered_at      : RenderedAt,
  viewforest       : &ViewForest,
  nodeInstructions : &[NodeInstruction],
  nodeMerges       : &[NodeMerge],
) -> Vec<BufferValidationError> {
  let rendered_at : u64 = rendered_at . 0;
  let written_pids : BTreeSet<ID> =
    // BTreeSet, so the errors come in a stable order.
    nodeInstructions . iter ()
    . map ( |nodeInstruction| match nodeInstruction {
      NodeInstruction::Save (SaveNode (node)) => node . pid . clone (),
      NodeInstruction::Delete (DeleteNode { gnid, .. }) => gnid . clone (), } )
    . chain ( nodeMerges . iter () . flat_map ( |nodeMerge| [
      nodeMerge . acquirer_gnid () . clone (),
      nodeMerge . acquiree_gnid () . clone () ] ))
    . map ( |gnid| graph . pid_else_raw_id (&gnid) )
    . collect ();
  written_pids . into_iter ()
    . filter_map ( |pid| {
      let last_change : u64 = graph . last_change_of (&pid) ?;
      ( last_change > rendered_at ) . then ( || {
        let headlines : Vec<HeadlineName> =
          occurrence_headlines_of (graph, viewforest, &pid);
        BufferValidationError::ChangedSinceRendered {
          gnid : pid, rendered_at, last_change, headlines } } ) } )
    . collect () }

/// The headline names of the occurrences of 'pid' in 'viewforest'.
fn occurrence_headlines_of (
  graph      : &InRustGraph,
  viewforest : &ViewForest,
  pid        : &ID,
) -> Vec<HeadlineName> {
  viewforest . nodes ()
    . filter ( |node| match &node . value () . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =>
        graph . pid_else_raw_id (&occurrence . gnid) == *pid,
      _ => false, } )
    . map ( |node| node . value () . headline_name () )
    . collect () }
