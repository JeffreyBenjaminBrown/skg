use crate::dbs::filesystem::one_node::{skgnodes_from_ids, skgnode_from_pid_and_source};
use crate::types::orgnode::{OrgNode, OrgNodeKind};
use crate::types::save::NonMerge_NodeAction;
use crate::types::skgnode::SkgNode;
use super::misc::{ID, SkgConfig};

use ego_tree::{Tree, NodeId, NodeRef};
use std::collections::HashMap;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Type alias for the new map-based approach.
/// Maps node IDs to their corresponding SkgNodes.
pub type SkgNodeMap = HashMap<ID, SkgNode>;

/// Extract SkgNode for an OrgNode from the map (tried first) or disk.
/// Updates the map if needed.
/// Returns None for Scaffolds, TrueNodes without IDs, or TrueNodes without sources.
pub fn skgnode_for_orgnode<'a> (
  orgnode : &OrgNode,
  map     : &'a mut SkgNodeMap,
  config  : &SkgConfig,
) -> Result<Option<&'a SkgNode>, Box<dyn Error>>
{ match &orgnode.kind {
    OrgNodeKind::True(t) => {
      match (&t.id_opt, &t.source_opt) {
        (Some(id), Some(source)) => {
          let skgnode : &SkgNode =
            skgnode_from_map_or_disk(
              id, map, config, source)?;
          Ok(Some(skgnode)) },
        _ => Ok(None) }},  // No ID or no source
    OrgNodeKind::Scaff(_) => Ok(None),
  }}

/// Build a SkgNodeMap from SaveInstructions.
/// Each SkgNode is indexed by its first ID.
pub fn skgnode_map_from_save_instructions (
  instructions : &Vec<(SkgNode, NonMerge_NodeAction)>,
) -> SkgNodeMap
{ instructions.iter()
    . filter_map( |(skgnode, _action)|
                  { skgnode . ids . first()
                      . map( |id| (id . clone(),
                                   skgnode . clone() )) } )
    . collect() }

/// Fetches (batched) SkgNodes from disk for all IDs in the tree.
pub async fn skgnode_map_from_forest (
  forest : &Tree<OrgNode>,
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
  map: &'a mut SkgNodeMap,
  config: &SkgConfig,
  source: &str,
) -> Result<&'a SkgNode, Box<dyn Error>> {
  if !map.contains_key(id) {
    let skgnode: SkgNode = skgnode_from_pid_and_source(
      config, id.clone(), source)?;
    map.insert(id.clone(), skgnode); }
  map . get(id) . ok_or_else(
    || "SkgNode should be in map after fetch".into( )) }

fn collect_ids_from_subtree (
  tree    : &Tree<OrgNode>,
  node_id : NodeId,
  ids_out : &mut Vec<ID>, )
{ let node_ref : NodeRef<OrgNode> =
    tree . get ( node_id ) . unwrap ();
  if let OrgNodeKind::True ( t )
    = &node_ref . value () . kind
    { if let Some ( id ) = &t . id_opt {
        ids_out . push ( id . clone () ); }}
  for child in node_ref . children () {
    collect_ids_from_subtree (
      tree, child . id (), ids_out ); }}
