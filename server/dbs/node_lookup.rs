//! A 'Graphnode' from the in-Rust graph by gnid ('graphnode_from_graph',
//! or 'graphnode_by_gnid', which errs when it is absent). Reading one
//! from its files instead is 'dbs/filesystem/one_node.rs'.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::ID;
use crate::types::nodes::complete::Graphnode;

use std::error::Error;

pub fn graphnode_by_gnid (
  graph : &InRustGraph,
  gnid  : &ID,
) -> Result<Graphnode, Box<dyn Error>> {
  if let Some (n) = graphnode_from_graph (graph, gnid) { return Ok (n); }
  Err (format! ("Node '{}' not found in captured graph generation", gnid) . into ()) }

pub fn graphnode_from_graph (
  graph : &InRustGraph,
  gnid    : &ID,
) -> Option<Graphnode> {
  let pid : ID = graph . pid_of (gnid) ?;
  graph . nodes . get (&pid) . map (Graphnode::from) }
