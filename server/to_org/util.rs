use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::node_lookup::graphnode_graphFirst_by_pid_and_skgrepo;
use crate::types::misc::{ID, SkgConfig, SkgrepoName};
use crate::types::nodes::complete::Graphnode;
use crate::types::viewnode::{Viewnode, mk_editable_viewnode};

use ego_tree::Tree;
use ego_tree::iter::Edge;
use std::error::Error;
use std::io;

// ======================================================
// Fetching, building and modifying Graphnodes and Viewnodes
// ======================================================

/// Fetch a Graphnode from the in-Rust graph or disk. Resolves id→(pid,skgrepo)
/// via 'InRustGraph::pid_and_skgrepo', then reads. Makes a Viewnode with
/// validated title. Returns both.
/// Returns Ok(None) when GNID has no record anywhere -- not as a
/// primary pid or extra_id in the captured graph.
/// Callers should substitute a Unknown. A real
/// query error still surfaces as Err.
pub fn graphnode_and_viewnode_from_gnid (
  graph  : &InRustGraph,
  config : &SkgConfig,
  gnid  : &ID,
) -> Result < Option<( Graphnode, Viewnode )>, Box<dyn Error> > {
  let resolved : Option<(ID, SkgrepoName)> =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "graphnode_and_viewnode_from_id" ). entered();
      graph . pid_and_skgrepo (gnid) };
  match resolved {
    None => Ok (None),
    Some ((pid_resolved, skgrepo)) =>
      match graphnode_and_viewnode_from_pid_and_skgrepo (
        graph, config, &pid_resolved, &skgrepo ) {
        Ok (node) => Ok ( Some (node) ),
        // A graph member can be dangling when its file is absent. This is not
        // a render failure: callers turn `None` into Unknown.
        Err (e) if e . downcast_ref::<io::Error> ()
          . is_some_and (|io_error| io_error . kind () == io::ErrorKind::NotFound)
          => Ok (None),
        Err (e) => Err (e), } } }

/// Fetch a Graphnode from the in-Rust graph or disk given PID and skgrepo.
/// Makes an Viewnode with validated title. Returns both.
fn graphnode_and_viewnode_from_pid_and_skgrepo (
  graph   : &InRustGraph,
  config  : &SkgConfig,
  pid     : &ID,
  skgrepo : &SkgrepoName,
) -> Result < ( Graphnode, Viewnode ), Box<dyn Error> > {
  let graphnode : Graphnode =
    graphnode_graphFirst_by_pid_and_skgrepo (
      graph, config, pid, skgrepo )?;
  let title : String = graphnode . title . replace ( '\n', " " );
  if title . is_empty () {
    return Err ( Box::new ( io::Error::new (
      io::ErrorKind::InvalidData,
      format! ( "Graphnode with ID {} has an empty title",
                 pid ), )) ); }
  let viewnode : Viewnode = mk_editable_viewnode (
    pid . clone (),
    skgrepo . clone (),
    title,
    graphnode . body . clone () );
  Ok (( graphnode, viewnode )) }

/// The gnids of the occurrences in 'tree', in preorder, each as often
/// as it occurs: the nodes whose graph stats the stats pass sets.
pub fn gnids_that_can_have_graphnodestats (
  tree : &Tree<Viewnode>,
) -> Vec < ID > {
  let mut gnids : Vec < ID > = Vec::new ();
  for edge in tree . root () . traverse () {
    if let Edge::Open (node_ref) = edge {
      if let Some (gnid) =
        node_ref . value () . occurrence_gnid ()
      { gnids . push ( gnid . clone () ); }}}
  gnids }
