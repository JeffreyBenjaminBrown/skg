//! A checked graph candidate tied to the exact snapshot it was derived from.
//!
//! The constructor is the only way to obtain this token.  Store orchestration
//! can inspect its definitions and candidate while preflighting later work,
//! but publication consumes it and never reapplies definitions to a newer
//! graph.

use crate::dbs::in_rust_graph::{InRustGraph, InRustGraphHandle};
use crate::dbs::in_rust_graph::complete_validation::{
  CompleteGraphError, validate_complete_graph_candidate,
};
use crate::types::misc::SkgConfig;
use crate::types::save::{DefineNode, SaveNode};

use std::sync::Arc;

pub(crate) struct PreparedGraphUpdate {
  base        : Arc<InRustGraph>,
  candidate   : Arc<InRustGraph>,
  definitions : Vec<DefineNode>,
}

impl PreparedGraphUpdate {
  pub(crate) fn candidate (
    &self,
  ) -> &Arc<InRustGraph> {
    &self . candidate }

  pub(crate) fn definitions (
    &self,
  ) -> &[DefineNode] {
    &self . definitions }

  pub(crate) fn verify_base (
    &self,
    graph : &InRustGraphHandle,
  ) -> Result<(), String> {
    let current : Arc<InRustGraph> = graph . load_full ();
    if Arc::ptr_eq (&self . base, &current) {
      Ok (())
    } else {
      Err ("Refusing to apply a prepared graph update to a different base snapshot. This is an internal mutation-boundary error." . to_string ()) }
  }

  pub(crate) fn publish (
    self,
    graph : &InRustGraphHandle,
  ) -> Result<(Arc<InRustGraph>, Vec<DefineNode>), String> {
    self . verify_base (graph) ?;
    graph . store (self . candidate . clone ());
    Ok ((self . candidate, self . definitions)) }
}

pub(crate) fn prepare_graph_update (
  config      : &SkgConfig,
  base        : Arc<InRustGraph>,
  definitions : Vec<DefineNode>,
) -> Result<PreparedGraphUpdate, Vec<CompleteGraphError>> {
  let definitions : Vec<DefineNode> = definitions . into_iter ()
    . map ( |mut definition| {
      if let DefineNode::Save (SaveNode (node)) = &mut definition {
        node . normalize_ids (); }
      definition } )
    . collect ();
  let validation = validate_complete_graph_candidate (
    config, &base, &definitions);
  if ! validation . errors . is_empty () {
    return Err (validation . errors); }
  Ok (PreparedGraphUpdate {
    base,
    candidate   : Arc::new (validation . graph),
    definitions,
  })
}

#[cfg(test)]
#[path = "../../../tests/unit/prepared_graph_update.rs"]
mod tests;
