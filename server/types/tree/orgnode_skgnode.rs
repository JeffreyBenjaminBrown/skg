/// Node access utilities for ego_tree::Tree<OrgNode>

use crate::to_org::util::{skgnode_and_orgnode_from_id, get_id_from_treenode};
use crate::types::misc::{ID, SkgConfig};
use crate::types::orgnode::{
    OrgNode, OrgNodeKind, Scaffold,
    mk_indefinitive_orgnode,
    orgnode_from_scaffold };
use crate::types::skgnode::SkgNode;
use crate::types::skgnodemap::SkgNodeMap;
use crate::util::dedup_vector;
use super::generic::with_node_mut;

use ego_tree::{Tree, NodeId, NodeRef};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Find the unique child of a node with a given Scaffold.
/// Returns None if no child has the kind,
/// Some(child_id) if exactly one does,
/// or an error if multiple children have it.
pub fn unique_scaffold_child (
  tree          : &Tree<OrgNode>,
  node_id       : NodeId,
  scaffold_kind : &Scaffold,
) -> Result<Option<NodeId>, Box<dyn Error>> {
  let node_ref : NodeRef<OrgNode> =
    tree . get(node_id) . ok_or(
      "unique_scaffold_child: node not found")?;
  let matches : Vec<NodeId> = node_ref.children()
    .filter(|c| matches!(&c.value().kind,
                         OrgNodeKind::Scaff(s)
                         if s.matches_kind(scaffold_kind)) )
    .map(|c| c.id())
    .collect();
  match matches.len() {
    0 => Ok(None),
    1 => Ok(Some(matches[0])),
    n => Err(format!(
      "Expected at most one {:?} child, found {}", scaffold_kind, n).into()),
  }
}

/// Extract PIDs for the subscriber and its subscribees.
/// Returns an error if the node has no SkgNode.
pub fn pids_for_subscriber_and_its_subscribees (
  tree    : &Tree<OrgNode>,
  map     : &SkgNodeMap,
  node_id : NodeId,
) -> Result < ( ID, Vec < ID > ),
              Box<dyn Error> > {
  let pid : ID = get_id_from_treenode ( tree, node_id ) ?;
  let skgnode : &SkgNode =
    map . get ( &pid ) . ok_or ( "pids_for_subscriber_and_its_subscribees: SkgNode should exist in map" ) ?;
  Ok (( skgnode . ids [0] . clone (),
        ( skgnode . subscribes_to . clone ()
          . unwrap_or_default () )) ) }

/// Extract PIDs for a Subscribee and its grandparent (the subscriber).
/// Expects: subscriber -> SubscribeeCol -> Subscribee (this node)
pub fn pid_for_subscribee_and_its_subscriber_grandparent (
  tree    : &Tree<OrgNode>,
  map     : &SkgNodeMap,
  node_id : NodeId,
) -> Result < ( ID, ID ), Box<dyn Error> > {
  let subscribee_pid : ID = get_id_from_treenode ( tree, node_id ) ?;
  let node_ref : NodeRef < OrgNode > =
    tree . get ( node_id ) . ok_or (
      "pid_for_subscribee_and_its_subscriber_grandparent: node not found" ) ?;
  let parent_ref : NodeRef < OrgNode > =
    node_ref . parent ()
    . ok_or ( "Subscribee has no parent (SubscribeeCol)" ) ?;
  if ! matches! ( &parent_ref . value () . kind,
                  OrgNodeKind::Scaff ( Scaffold::SubscribeeCol )) {
    return Err ( "Subscribee's parent is not a SubscribeeCol" .
                 into () ); }
  let grandparent_ref : NodeRef < OrgNode > =
    parent_ref . parent ()
    . ok_or ( "SubscribeeCol has no parent (subscriber)" ) ?;
  let subscriber_id : ID =
    get_id_from_treenode ( tree, grandparent_ref . id () ) ?;
  let skgnode : &SkgNode =
    map . get ( &subscriber_id )
    . ok_or ( "Subscriber SkgNode not in map" ) ?;
  Ok (( subscribee_pid,
        skgnode . ids[0] . clone() )) }

pub fn insert_scaffold_as_child (
  tree          : &mut Tree<OrgNode>,
  parent_id     : NodeId,
  scaffold_kind : Scaffold,
  prepend       : bool, // otherwise, append
) -> Result < NodeId, Box<dyn Error> > {
  let orgnode : OrgNode =
    orgnode_from_scaffold ( scaffold_kind );
  let col_id : NodeId = with_node_mut (
    tree, parent_id,
    |mut parent_mut| {
      if prepend { parent_mut . prepend ( orgnode ) . id () }
      else       { parent_mut . append  ( orgnode ) . id () } } )
    . map_err ( |e| -> Box<dyn Error> { e.into() } ) ?;
  Ok ( col_id ) }

/// Fetch a node from disk and append it as an indefinitive child.
/// Also adds the SkgNode to the map.
pub async fn append_indefinitive_from_disk_as_child (
  tree           : &mut Tree<OrgNode>,
  map            : &mut SkgNodeMap,
  parent_id      : NodeId,
  node_id        : &ID,
  parent_ignores : bool,
  config         : &SkgConfig,
  driver         : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let ( _skgnode, content_orgnode ) : ( SkgNode, OrgNode ) =
    skgnode_and_orgnode_from_id (
      config, driver, node_id, map ) . await ?;
  let (id, source, title) : (ID, String, String)
  = match &content_orgnode.kind
  { OrgNodeKind::True(t) => (
      t . id_opt . as_ref() . ok_or("append_indefinitive_from_disk_as_child: node has no ID")?
        . clone(),
      t . source_opt . as_ref() . ok_or("append_indefinitive_from_disk_as_child: node has no source")?
        . clone(),
      t . title . clone( )),
    OrgNodeKind::Scaff(_) => return Err("append_indefinitive_from_disk_as_child: expected TrueNode".into()) };
  let orgnode : OrgNode = mk_indefinitive_orgnode (
    id, source, title, parent_ignores );
  with_node_mut (
    tree, parent_id,
    |mut parent_mut| {
      parent_mut . append ( orgnode ); } )
    . map_err ( |e| -> Box<dyn Error> { e.into() } ) ?;
  Ok (( )) }

/// Collect titles from Alias children of an AliasCol.
/// Removes Duplicate (preserving order of first occurrence).
/// Errors if any non-Alias children are found.
pub(crate) fn collect_child_aliases_at_aliascol (
  tree             : &Tree<OrgNode>,
  aliascol_node_id : NodeId,
) -> Result < Vec < String >, Box<dyn Error> > {
  let mut aliases : Vec < String > =
    Vec::new ();
  let aliascol_ref : NodeRef < OrgNode > =
    tree . get ( aliascol_node_id )
    . ok_or ( "AliasCol node not found" ) ?;
  for child_ref in aliascol_ref . children() {
    let child : &OrgNode = child_ref . value();
    if ! matches! ( &child . kind,
                    OrgNodeKind::Scaff ( Scaffold::Alias(_) )) {
      return Err (
        format! ( "AliasCol has non-Alias child with kind: {:?}",
                  child . kind )
        . into () ); }
    aliases . push (
      child . title () . to_string () ); }
  Ok ( aliases ) }

/// Reads from disk the SkgNode
/// for a node or for one of its tree-ancestors.
/// Collect aliases for a node:
/// - find the unique AliasCol child (error if multiple)
/// - for each Alias child of the AliasCol, collect its title
/// Duplicates are removed (preserving order of first occurrence).
/// Returns None ("no opinion") if no AliasCol found.
/// Returns Some(vec) if AliasCol found, even if empty.
pub fn collect_grandchild_aliases_for_orgnode (
  tree: &Tree<OrgNode>,
  node_id: NodeId,
) -> Result<Option<Vec<String>>, String> {
  let alias_col_id : Option<NodeId> =
    unique_scaffold_child (
      tree, node_id, &Scaffold::AliasCol )
    . map_err ( |e| e.to_string() ) ?;
  match alias_col_id {
    None => Ok(None),
    Some(col_id) => {
      let aliases : Vec<String> = {
        let col_ref : NodeRef<OrgNode> = tree.get(col_id).expect( "collect_grandchild_aliases_for_orgnode: AliasCol not found");
        let mut aliases : Vec<String> = Vec::new();
        for alias_child in col_ref.children() {
          { // check for invalid state
            if ! matches!(&alias_child.value().kind,
                          OrgNodeKind::Scaff(Scaffold::Alias(_))) {
              return Err ( format! (
                "AliasCol has non-Alias child with kind: {:?}",
                alias_child.value().kind )); }}
          aliases . push(
            alias_child . value() . title() . to_string() ); }
        aliases };
      Ok(Some(dedup_vector(aliases))) }} }

/// Find a child node by its ID.
/// Returns the NodeId of the child if found, None otherwise.
pub fn find_child_by_id (
  tree          : & Tree<OrgNode>,
  parent_treeid : NodeId,
  target_skgid  : & ID,
) -> Option < NodeId > {
  let singleton : HashSet<ID> =
    std::iter::once( target_skgid.clone() )
    . collect();
  find_children_by_ids( tree, parent_treeid, &singleton)
    . remove(target_skgid) }

/// Find child nodes by their IDs.
/// Returns a map from ID to NodeId for children that were found.
/// IDs not found as children are not included in the result.
pub fn find_children_by_ids (
  tree          : & Tree<OrgNode>,
  parent_treeid : NodeId,
  target_skgids : & HashSet < ID >,
) -> HashMap < ID, NodeId > {
  let mut result : HashMap < ID, NodeId > = HashMap::new();
  for child in tree.get(parent_treeid).unwrap().children() {
    if let OrgNodeKind::True(t) = &child.value().kind {
      if let Some(child_id) = &t.id_opt {
        if target_skgids.contains(child_id) {
          result.insert(child_id.clone(), child.id()); }}}}
  result }
