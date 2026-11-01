//! When each node of the in-Rust graph last changed, as runtime
//! generation numbers ('RuntimeGeneration' in 'server/types/env.rs').
//!
//! A save refuses to write a node that changed after the generation
//! its buffer was rendered at ('stale_writes' in
//! 'server/serve/handlers/save_staleness.rs'). The numbers live
//! only in the session-lifetime graph: never in '.skg' files, never in
//! a 'Graphnode'.
//!
//! Writers do not know the generation their graph will be swapped in
//! as. So a graph mutation records only which nodes it wrote
//! ('written_since_swap_in'), and 'SharedRuntime::swap_in' stamps them
//! with the generation it assigns ('stamped_at_generation').

use crate::types::misc::ID;

use std::mem::take;

#[derive(Clone, Debug, Default)]
pub struct LastChangeGenerations {
  /// The generation at which the graph was last read whole from disk
  /// (at startup, or by a rebuild): the last change of every node
  /// 'by_pid' does not list. 'None' while a graph read whole awaits
  /// its first swap-in.
  read_whole_at : Option<u64>,
  /// The nodes written since the graph was read whole, each with the
  /// generation of its last write. A deleted node keeps its entry, so
  /// a buffer rendered before the delete cannot silently recreate it.
  by_pid : im::HashMap<ID, u64>,
  /// The nodes written since the last swap-in, which the next swap-in
  /// stamps with its generation.
  written_since_swap_in : im::HashSet<ID>,
}

impl LastChangeGenerations {
  /// The generation at which 'pid' last changed, if it ever existed
  /// in this graph: a node 'by_pid' lists, or else one the graph has
  /// ('in_graph'), which last changed when the graph was read whole.
  /// A graph never swapped in (one built by a test, say) dates its
  /// nodes 0, the startup generation.
  pub fn of (
    &self,
    pid      : &ID,
    in_graph : bool,
  ) -> Option<u64> {
    match self . by_pid . get (pid) {
      Some (generation) => Some (*generation),
      None if in_graph => Some ( self . read_whole_at . unwrap_or (0) ),
      None => None, }}

  /// The latest generation this graph records a change at; None for
  /// a graph never swapped in.
  pub fn newest (
    &self,
  ) -> Option<u64> {
    self . by_pid . values () . copied ()
      . chain (self . read_whole_at)
      . max () }

  /// Note that 'pid' was written, for the next swap-in to stamp.
  pub fn note_written (
    &mut self,
    pid : &ID,
  ) {
    self . written_since_swap_in . insert ( pid . clone () ); }

  /// Each node written since the last swap-in, and the graph's
  /// read-whole generation if it has none yet, takes 'generation'.
  pub fn stamp (
    &mut self,
    generation : u64,
  ) {
    if self . read_whole_at . is_none () {
      self . read_whole_at = Some (generation); }
    for pid in take (&mut self . written_since_swap_in) {
      self . by_pid . insert (pid, generation); }}
}
