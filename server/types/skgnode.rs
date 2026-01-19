use serde::{Serialize, Deserialize};
use std::collections::HashMap;
use std::error::Error;

use crate::dbs::filesystem::one_node::{skgnodes_from_ids, skgnode_from_pid_and_source};
use crate::types::orgnode::{OrgNode, OrgNodeKind};
use crate::types::save::NonMerge_NodeAction;
use crate::util::option_vec_is_empty_or_none;
use super::misc::{ID, SkgConfig};
use ego_tree::{Tree, NodeId, NodeRef};
use typedb_driver::TypeDBDriver;

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct SkgNode {
  // There is a 1-to-1 correspondence between SkgNodes and actual .skg files -- a file can be read to a SkgNode, and a SkgNode can be written to a file. The files are the only permanent data. SkgNode is the format used to initialize the TypeDB and Tantivy databases.
  // Tantivy will receive some of this data, and TypeDB some other subset. Tantivy associates IDs with titles. TypeDB represents all the connections between nodes (see 'schema.tql' for how). At least one field, `body`, is known to neither database; it is instead read directly from the files on disk when Rust builds a document for Emacs.
  // PITFALL: Some([]) vs. None in the Optional lists:
  // - On disk the distinction is not needed. Both Some([]) and None are both rendered on disk as a missing field.
  // - When multiple SkgNodes need to be reconciled (as in reconcile_same_id_instructions or clobber_none_fields_with_data_from_disk), the distinction matters. This is because a SkgNode can be built from an OrgNode, which might or might not say something about the relevant field. If the OrgNode intends to convey "this field *should* be empty", then it reads 'Some([])'. If instead the OrgNode did not say anything about that field, we use None -- and later clobber it with whatever was on disk for that field, via 'clobber_none_fields_with_data_from_disk'.

  pub title: String,

  #[serde(default, skip_serializing_if = "option_vec_is_empty_or_none")]
  pub aliases: Option<Vec<String>>, // A node can be searched for using its title or any of its aliases, and so far using its body text too. (I might later decide not to index bodies, or to give the choice to the user.)

  #[serde(skip_serializing, skip_deserializing)]
  pub source: String, // source nickname, inferred from file location and SkgConfig

  pub ids: Vec<ID>, // Must be nonempty. Can have length > 1 because nodes might be merged, but will usually have length = 1.
  // TODO: Use a nonempty list type (e.g. the "nonempty" crate), or else separate fields pid : String and extra_ids : Vec<String>. I'm leaning toward the latter, as the pid is special among those ids.

  #[serde(default, skip_serializing_if = "Option::is_none")]
  pub body: Option<String>, // Unknown to both Tantivy & TypeDB. The body is all text (if any) between the preceding org headline, to which it belongs, and the next (if there is a next).

  #[serde(default, skip_serializing_if = "option_vec_is_empty_or_none")]
  pub contains: Option<Vec<ID>>, // See schema.tql.

  #[serde(default, skip_serializing_if = "option_vec_is_empty_or_none")]
  pub subscribes_to: Option<Vec<ID>>, // See schema.tql.

  #[serde(default, skip_serializing_if = "option_vec_is_empty_or_none")]
  pub hides_from_its_subscriptions: Option<Vec<ID>>, // See schema.tql.

  #[serde(default, skip_serializing_if = "option_vec_is_empty_or_none")]
  pub overrides_view_of: Option<Vec<ID>>, // See schema.tql.
}

impl SkgNode {
  /// Returns the primary ID, or an error if ids is empty.
  pub fn primary_id(&self) -> Result<&ID, String> {
    self.ids.get(0).ok_or_else(||
      format!("SkgNode '{}' has no IDs", self.title)) }
}


//
// Map-based helpers for Tree<OrgNode> + SkgNodeMap architecture
//

/// Type alias for the new map-based approach.
/// Maps node IDs to their corresponding SkgNodes.
pub type SkgNodeMap = HashMap<ID, SkgNode>;


//
// Functions
//

pub fn skgnode_example () -> SkgNode {
  SkgNode {
    title: "This text gets indexed.".to_string(),
    aliases: None,
    source: "main".to_string(),
    ids: vec![ ID::new("example") ],
    body: Some( r#"This one string could span pages.
It better be okay with newlines."# . to_string() ),
    contains: Some(vec![ ID::new("1"),
                         ID::new("2"),
                         ID::new("3")]),
    subscribes_to: Some(vec![ID::new("11"),
                             ID::new("12"),
                             ID::new("13")]),
    hides_from_its_subscriptions: None,
    overrides_view_of: None, }}

/// Useful for making tests more readable.
pub fn empty_skgnode () -> SkgNode {
  SkgNode {
    title                        : String::new (),
    aliases                      : None,
    source                       : "main".to_string(),
    ids                          : vec![],
    body                         : None,
    contains                     : None,
    subscribes_to                : None,
    hides_from_its_subscriptions : None,
    overrides_view_of            : None,
  }}

/// Extract SkgNode for an OrgNode from the map, if applicable.
/// Returns None for Scaffolds or TrueNodes without IDs.
pub fn skgnode_for_orgnode<'a> (
  orgnode : &OrgNode,
  map     : &'a SkgNodeMap,
) -> Option<&'a SkgNode>
{
  match &orgnode.kind {
    OrgNodeKind::True(t) =>
      t.id_opt.as_ref()
        .and_then(|id| map.get(id)),
    OrgNodeKind::Scaff(_) => None,
  }
}

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

/// Build a SkgNodeMap by fetching SkgNodes from disk for all IDs in the tree.
/// This is the v2 equivalent of orgnode_forest_to_paired for tests.
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
  { let mut map : SkgNodeMap = SkgNodeMap::new ();
    for node in nodes {
      if let Some ( id ) = node . ids . first () {
        map . insert ( id . clone (), node ); }}
    Ok ( map ) }}

/// Tries to return a SkgNode from the map.
/// If it's absent, fetches from disk and updates the map.
pub fn skgnode_from_map_or_disk<'a>(
  id     : &ID,
  map    : &'a mut SkgNodeMap,
  config : &SkgConfig,
  source : &str,
) -> Result<&'a SkgNode, Box<dyn Error>> {
  if !map.contains_key(id) {
    let skgnode: SkgNode =
      skgnode_from_pid_and_source(
        config, id.clone(), source)?;
    map.insert(id.clone(), skgnode); }
  map . get(id) . ok_or_else(
    || "SkgNode should be in map after fetch".into( )) }

fn collect_ids_from_subtree (
  tree    : &Tree<OrgNode>,
  node_id : NodeId,
  ids_out : &mut Vec<ID>,
) {
  let node_ref : NodeRef<OrgNode> =
    tree . get ( node_id ) . unwrap ();
  if let OrgNodeKind::True ( t ) = &node_ref . value () . kind
  { if let Some ( id ) = &t . id_opt { // collect one
      ids_out . push ( id . clone () ); }}
  for child in node_ref . children ()
  { collect_ids_from_subtree ( // recurse
      tree, child . id (), ids_out ); }}
