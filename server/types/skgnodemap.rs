use crate::dbs::filesystem::one_node::{skgnodes_from_ids, skgnode_from_pid_and_source};
use crate::types::viewnode::{ViewNode, ViewNodeKind};
use crate::types::save::{DefineNode, SaveNode};
use crate::types::skgnode::SkgNode;
use super::misc::{ID, SkgConfig, SourceName};

use ego_tree::{Tree, NodeId, NodeRef};
use std::collections::HashMap;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Type alias for the new map-based approach.
/// Maps node IDs to their corresponding SkgNodes.
pub type SkgNodeMap = HashMap<ID, SkgNode>;

/// Extract SkgNode for an ViewNode from the map (tried first) or disk.
/// Updates the map if needed.
/// Returns None for Scaffolds.
pub fn skgnode_for_viewnode<'a> (
  viewnode : &ViewNode,
  map     : &'a mut SkgNodeMap,
  config  : &SkgConfig,
) -> Result<Option<&'a SkgNode>, Box<dyn Error>>
{ match &viewnode.kind {
    ViewNodeKind::True(t) => {
      let skgnode : &SkgNode =
        skgnode_from_map_or_disk(
          &t.id, &t.source, map, config)?;
      Ok(Some(skgnode)) },
    ViewNodeKind::Scaff(_) => Ok(None),
  }}

/// Build a SkgNodeMap from DefineNodes.
/// Each SkgNode is indexed by its first ID.
/// PITFALL: Does not include every node in the buffer --
/// just the ones that generated instructions.
/// Hence the importance of 'skgnode_from_map_or_disk'.
/// PITFALL: Only Save instructions contribute;
/// Delete instructions carry no SkgNode data.
pub fn skgnode_map_from_save_instructions (
  instructions : &Vec<DefineNode>,
) -> SkgNodeMap
{ instructions.iter()
    . filter_map( |instr| match instr {
        DefineNode::Save(SaveNode(skgnode)) =>
          skgnode . ids . first()
            . map( |id : &ID| (id . clone(),
                               skgnode . clone() )),
        DefineNode::Delete(_) => None } )
    . collect() }

/// Fetches (batched) SkgNodes from disk for all IDs in the tree.
pub async fn skgnode_map_from_forest (
  forest : &Tree<ViewNode>,
  config : &SkgConfig,
  driver : &TypeDBDriver,
) -> Result<SkgNodeMap, Box<dyn Error>>
{ let mut all_tree_ids : Vec<ID> = Vec::new();
  collect_ids_from_subtree (
    forest, forest . root () . id (), &mut all_tree_ids );
  let nodes : Vec<SkgNode> = skgnodes_from_ids (
    config, driver, &all_tree_ids ) . await ?;
  let mut map : SkgNodeMap = SkgNodeMap::new ();
  for node in nodes {
    if let Some ( id ) = node . ids . first () {
    map . insert ( id . clone (), node ); }}
  Ok ( map ) }

/// Get a SkgNode from the map, or from disk if it's not there.
/// Updates the map.
pub fn skgnode_from_map_or_disk<'a>(
  id: &ID,
  source: &SourceName,
  map: &'a mut SkgNodeMap,
  config: &SkgConfig,
) -> Result<&'a SkgNode, Box<dyn Error>> {
  if !map.contains_key(id) {
    let skgnode: SkgNode = skgnode_from_pid_and_source(
      config, id.clone(), source)?;
    map.insert(id.clone(), skgnode); }
  map . get(id) . ok_or_else(
    || "SkgNode should be in map after fetch".into( )) }

fn collect_ids_from_subtree (
  tree    : &Tree<ViewNode>,
  node_id : NodeId,
  ids_out : &mut Vec<ID>, )
{ let node_ref : NodeRef<ViewNode> =
    tree . get ( node_id ) . unwrap ();
  if let ViewNodeKind::True ( t )
    = &node_ref . value () . kind
    { ids_out . push ( t . id . clone () ); }
  for child in node_ref . children () {
    collect_ids_from_subtree (
      tree, child . id (), ids_out ); }}
