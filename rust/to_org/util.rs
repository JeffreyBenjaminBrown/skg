use crate::media::file_io::read_node;
use crate::util::path_from_pid_and_source;
use crate::media::typedb::util::pid_and_source_from_id;
use crate::types::{SkgNode, ID, SkgConfig, OrgNode};
use crate::types::orgnode::default_metadata;
use crate::types::trees::PairTree;

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::HashMap;
use std::error::Error;
use std::io;
use typedb_driver::TypeDBDriver;

/// Tracks which IDs have been rendered definitively and where.
/// - Key: the ID that was visited
/// - Value: ( index identifying a tree in the forest,
///            NodeId within that tree )
///
/// Uses:
/// - prevent duplicate definitive expansions
/// - locate the conflict when an earlier definitive view
///   conflicts with a new definitive view request
pub type VisitedMap =
  HashMap < ID, (usize, NodeId) >;

/// Fetch a SkgNode from disk (queries TypeDB for source).
/// Make an OrgNode from it, with validated title.
/// Return both.
pub async fn skgnode_and_orgnode_from_id (
  config : &SkgConfig,
  driver : &TypeDBDriver,
  id     : &ID,
) -> Result < ( SkgNode, OrgNode ), Box<dyn Error> > {
  let (pid_resolved, source) : (ID, String) =
    pid_and_source_from_id( // Query TypeDB for them
      &config.db_name, driver, id).await?
    . ok_or_else( || format!(
      "ID '{}' not found in database", id ))?;
  skgnode_and_orgnode_from_pid_and_source (
    config, &pid_resolved, &source ) }

/// Fetch a SkgNode from disk given PID and source.
/// Make an OrgNode from it, with validated title.
/// Return both.
pub fn skgnode_and_orgnode_from_pid_and_source (
  config : &SkgConfig,
  pid    : &ID,
  source : &str,
) -> Result < ( SkgNode, OrgNode ), Box<dyn Error> > {
  let path : String =
    path_from_pid_and_source (
      config, source, pid.clone() );
  let mut skgnode : SkgNode = read_node ( path ) ?;
  skgnode.source = source.to_string();
  let orgnode : OrgNode = OrgNode {
    metadata : { let mut md = default_metadata ();
                 md . id = Some ( pid . clone () );
                 md . source = Some ( source . to_string () );
                 md },
    title : newline_to_space ( & skgnode . title ),
    body  : skgnode . body . clone (), };
  if orgnode . title . is_empty () { // Validate title
    return Err ( Box::new ( io::Error::new (
      io::ErrorKind::InvalidData,
      format! ( "SkgNode with ID {} has an empty title",
                 pid ), )) ); }
  Ok (( skgnode, orgnode )) }

pub fn newline_to_space ( s: &str ) -> String {
  s.replace ( '\n', " " ) }

pub fn org_bullet ( level: usize ) -> String {
  "*" . repeat ( level.max ( 1 )) }

/// Check if `target_id` appears in the ancestor path of `node_id`.
/// Used for cycle detection.
///
/// The `get_id` closure extracts an `Option<&ID>` from a node value,
/// allowing this to work with different tree types:
/// - `Tree<OrgNode>`: `|n| n.metadata.id.as_ref()`
/// - `Tree<(SkgNode, OrgNode)>`: `|n| n.1.metadata.id.as_ref()`
pub fn is_ancestor_id < T, F > (
  tree      : &Tree < T >,
  node_id   : NodeId,
  target_id : &ID,
  get_id    : F,
) -> Result < bool, Box<dyn Error> >
where F : Fn ( &T ) -> Option < &ID >
{
  let node_ref : NodeRef < T > =
    tree . get ( node_id )
    . ok_or ( "is_ancestor_id: NodeId not in tree" ) ?;
  let mut current : Option < NodeRef < T > > =
    node_ref . parent ();
  while let Some ( parent ) = current {
    if let Some ( parent_id ) = get_id ( parent . value () ) {
      if parent_id == target_id {
        return Ok ( true ); }}
    current = parent . parent (); }
  Ok ( false ) }

/// Extract the PID from a node in a tree.
/// Returns an error if the node is not found or has no ID.
///
/// The `get_id` closure extracts an `Option<&ID>` from a node value,
/// allowing this to work with different tree types:
/// - `Tree<OrgNode>`: `|n| n.metadata.id.as_ref()`
/// - `Tree<(SkgNode, OrgNode)>`: `|n| n.1.metadata.id.as_ref()`
pub fn get_node_pid_generic < T, F > (
  tree    : &Tree < T >,
  node_id : NodeId,
  get_id  : F,
) -> Result < ID, Box<dyn Error> >
where F : Fn ( &T ) -> Option < &ID >
{
  let node_ref : NodeRef < T > =
    tree . get ( node_id )
    . ok_or ( "get_node_pid_generic: NodeId not in tree" ) ?;
  get_id ( node_ref . value () )
    . cloned ()
    . ok_or_else ( || "get_node_pid_generic: node has no ID" . into () ) }

/// Extract the PID from a node in a PairTree.
/// Returns an error if the node is not found or has no ID.
/// (Convenience wrapper around get_node_pid_generic for PairTree.)
pub fn get_pid_in_pairtree (
  tree    : &PairTree,
  node_id : NodeId,
) -> Result < ID, Box<dyn Error> > {
  get_node_pid_generic (
    tree, node_id,
    |n| n . 1 . metadata . id . as_ref () ) }

/// Extract the content child IDs from an SkgNode.
/// Returns an empty Vec if there are no contents.
pub fn content_ids_from_skgnode (
  skgnode : &SkgNode
) -> Vec < ID > {
  skgnode . contains . clone () . unwrap_or_default () }
