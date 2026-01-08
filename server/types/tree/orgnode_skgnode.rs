/// Node access utilities for ego_tree::Tree.

use crate::dbs::filesystem::one_node::skgnode_from_id;
use crate::to_org::util::skgnode_and_orgnode_from_id;
use crate::types::misc::{ID, SkgConfig};
use crate::types::orgnode::{Interp, OrgNode, default_metadata};
use crate::types::orgnode_new::{from_old_orgnode, NewOrgNode, ScaffoldKind};
use crate::types::skgnode::SkgNode;
use crate::util::dedup_vector;
use super::{NodePair, PairTree};
use super::generic::{read_at_ancestor_in_tree, read_at_node_in_tree, with_node_mut};

use ego_tree::{Tree, NodeId, NodeRef};
use std::error::Error;
use typedb_driver::TypeDBDriver;

///
/// accessors specific to trees of OrgNodes and (maybe) SkgNodes
///

/// Find the unique child of a node with a given Interp (for PairTree).
/// Returns None if no child has the interp,
/// Some(child_id) if exactly one does,
/// or an error if multiple children have it.
pub fn unique_child_with_interp (
  tree    : &PairTree,
  node_id : NodeId,
  interp  : Interp,
) -> Result<Option<NodeId>, Box<dyn Error>> {
  let node_ref : ego_tree::NodeRef<super::NodePair> =
    tree.get(node_id)
    .ok_or("unique_child_with_interp: node not found")?;
  let matches : Vec<NodeId> = node_ref.children()
    .filter(|c| c.value().orgnode().matches_interp ( &interp ))
    .map(|c| c.id())
    .collect();
  match matches.len() {
    0 => Ok(None),
    1 => Ok(Some(matches[0])),
    n => Err(format!(
      "Expected at most one {:?} child, found {}", interp, n).into()),
  }
}

/// Find the unique child of a node with a given Interp (for Tree<OrgNode>).
/// Returns None if no child has the interp,
/// Some(child_id) if exactly one does,
/// or an error if multiple children have it.
pub fn unique_orgnode_child_with_interp (
  tree    : &Tree<OrgNode>,
  node_id : NodeId,
  interp  : Interp,
) -> Result<Option<NodeId>, Box<dyn Error>> {
  let node_ref : ego_tree::NodeRef<OrgNode> =
    tree . get(node_id) . ok_or(
      "unique_orgnode_child_with_interp: node not found")?;
  unique_orgnode_child_with_interp_from_ref (
    &node_ref, interp ) }

/// Like unique_orgnode_child_with_interp, but takes a NodeRef directly.
/// Useful when you already have the NodeRef and don't want to look it up again.
pub fn unique_orgnode_child_with_interp_from_ref (
  node_ref : &ego_tree::NodeRef<OrgNode>,
  interp   : Interp,
) -> Result<Option<NodeId>, Box<dyn Error>> {
  let matches : Vec<NodeId> = node_ref.children()
    .filter(|c| c.value().metadata.code.interp == interp)
    .map(|c| c.id())
    .collect();
  match matches.len() {
    0 => Ok(None),
    1 => Ok(Some(matches[0])),
    n => Err(format!(
      "Expected at most one {:?} child, found {}", interp, n).into()),
  }
}

/// Extract PIDs for the subscriber and its subscribees.
/// Returns an error if the node has no SkgNode.
pub fn pids_for_subscriber_and_its_subscribees (
  tree    : &PairTree,
  node_id : NodeId,
) -> Result < ( ID, Vec < ID > ),
              Box<dyn Error> > {
  read_at_node_in_tree (
    tree, node_id,
    |np| np . mskgnode . as_ref ()
      . map ( |skgnode|
              ( skgnode . ids [0] . clone (),
                skgnode . subscribes_to . clone ()
                . unwrap_or_default () ))
  )? . ok_or_else (
    || "pids_for_subscriber_and_its_subscribees: SkgNode should exist"
    . into () ) }

/// Extract PIDs for a Subscribee and its grandparent (the subscriber).
/// Expects: subscriber -> SubscribeeCol -> Subscribee (this node)
pub fn pid_for_subscribee_and_its_subscriber_grandparent (
  tree    : &PairTree,
  node_id : NodeId,
) -> Result < ( ID, ID ), Box<dyn Error> > {
  let node_ref : NodeRef < NodePair > =
    tree . get ( node_id ) . ok_or (
      "pid_for_subscribee_and_its_subscriber_grandparent: node not found" ) ?;
  let subscribee_pid : ID =
    node_ref . value () . orgnode () . id () . cloned ()
    . ok_or ( "Subscribee has no ID" ) ?;
  let parent_ref : NodeRef < NodePair > =
    node_ref . parent ()
    . ok_or ( "Subscribee has no parent (SubscribeeCol)" ) ?;
  if ! parent_ref . value () . orgnode ()
      . matches_interp ( &Interp::SubscribeeCol ) {
      return Err ( "Subscribee's parent is not a SubscribeeCol" .
                    into () ); }
  let grandparent_ref : NodeRef < NodePair > =
    parent_ref . parent ()
    . ok_or ( "SubscribeeCol has no parent (subscriber)" ) ?;
  let skgnode : &SkgNode =
    grandparent_ref . value () . mskgnode . as_ref ()
    . ok_or ( "Subscriber has no SkgNode" ) ?;
  Ok ( ( subscribee_pid,
         skgnode . ids [ 0 ] . clone () ) ) }

/// Insert into parent_id's children
///   a node with no associated SkgNode.
/// Most Interps are like this (Alias and all the *Cols.)
///   but the most common ones are not (Content, Subscribee).
pub fn insert_sourceless_node (
  tree      : &mut PairTree,
  parent_id : NodeId,
  interp    : Interp,
  title     : &str,
  prepend   : bool, // otherwise, append
) -> Result < NodeId, Box<dyn Error> > {
  let col_orgnode : OrgNode = {
    let mut md = default_metadata ();
    md . code . interp = interp;
    OrgNode {
      metadata : md,
      title : title . to_string (),
      body : None, } };
  let new_orgnode = from_old_orgnode ( &col_orgnode );
  let col_id : NodeId = with_node_mut (
    tree, parent_id,
    |mut parent_mut| {
      let pair = NodePair { mskgnode : None,
                            orgnode  : new_orgnode };
      if prepend { parent_mut . prepend ( pair ) . id () }
      else       { parent_mut . append  ( pair ) . id () } } ) ?;
  Ok ( col_id ) }

/// Fetch a node from disk and append it as an indefinitive child with the given Interp.
pub async fn append_indefinitive_node (
  tree      : &mut PairTree,
  parent_id : NodeId,
  node_id   : &ID,
  interp    : Interp,
  config    : &SkgConfig,
  driver    : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let ( skgnode, mut orgnode ) : ( SkgNode, OrgNode ) =
    skgnode_and_orgnode_from_id (
      config, driver, node_id ) . await ?;
  orgnode . metadata . code . interp = interp;
  orgnode . metadata . code . indefinitive = true;
  orgnode . body = None;
  let new_orgnode = from_old_orgnode ( &orgnode );
  with_node_mut (
    tree, parent_id,
    |mut parent_mut| {
      parent_mut . append (
        NodePair { mskgnode : Some ( skgnode ),
                   orgnode  : new_orgnode } ); } ) ?;
  Ok (( )) }

/// Reads from disk the SkgNode
/// for a node or for one of its tree-ancestors.
pub async fn ancestor_skgnode_from_disk (
  tree       : &PairTree,
  treeid     : NodeId,
  generation : usize, // 0 = self, 1 = parent, etc.
  config     : &SkgConfig,
  driver     : &TypeDBDriver,
) -> Result < SkgNode, Box<dyn Error> > {
  let ancestor_skgid : ID =
    read_at_ancestor_in_tree( tree, treeid, generation,
      |np| np . orgnode () . id () . cloned () )
    . map_err( |e| -> Box<dyn Error> { e . into () })?
    . ok_or_else(
      || -> Box<dyn Error> {
        "Ancestor node has no ID" . into () })?;
  Ok ( { let skgnode : SkgNode =
           skgnode_from_id(
             config, driver, &ancestor_skgid ) . await?;
         skgnode } ) }

/// Collect titles from Alias children of an AliasCol node (for PairTree).
/// Duplicates are removed (preserving order of first occurrence).
/// Errors if any non-Alias children are found.
pub fn collect_child_aliases_at_nodepair_aliascol (
  tree             : &PairTree,
  aliascol_node_id : NodeId,
) -> Result < Vec < String >, Box<dyn Error> > {
  let mut aliases : Vec < String > =
    Vec::new ();
  let aliascol_ref : NodeRef < NodePair > =
    tree . get ( aliascol_node_id )
    . ok_or ( "AliasCol node not found" ) ?;
  for child in aliascol_ref . children () {
    let child_new = child . value () . orgnode ();
    if ! child_new . matches_interp ( &Interp::Alias ) {
      return Err (
        format! ( "AliasCol has non-Alias child with interp: {:?}",
                  child_new . interp () )
        . into () ); }
    aliases . push (
      child_new . title () . to_string () ); }
  Ok ( dedup_vector ( aliases ) ) }

/// Collect aliases for a node (for Tree<OrgNode>):
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
    unique_orgnode_child_with_interp (
      tree, node_id, Interp::AliasCol )
    . map_err ( |e| e.to_string() ) ?;
  match alias_col_id {
    None => Ok(None),
    Some(col_id) => {
      let aliases : Vec<String> = {
        let col_ref : NodeRef<OrgNode> = tree.get(col_id).expect(
          "collect_aliases_at_orgnode: AliasCol not found");
        let mut aliases : Vec<String> = Vec::new();
        for alias_child in col_ref.children() {
          { // check for invalid state
            let child_interp : &Interp =
              &alias_child . value() . metadata . code.interp;
            if *child_interp != Interp::Alias {
              return Err ( format! (
                "AliasCol has non-Alias child with interp: {:?}",
                child_interp )); }}
          aliases . push(
            alias_child . value() . title . clone() ); }
        aliases };
      Ok(Some(dedup_vector(aliases))) }} }

/// Find the unique child of a node with a given ScaffoldKind (for Tree<NewOrgNode>).
/// Returns None if no child has the kind,
/// Some(child_id) if exactly one does,
/// or an error if multiple children have it.
pub fn unique_neworgnode_child_with_interp (
  tree    : &Tree<NewOrgNode>,
  node_id : NodeId,
  interp  : Interp,
) -> Result<Option<NodeId>, Box<dyn Error>> {
  let node_ref : ego_tree::NodeRef<NewOrgNode> =
    tree . get(node_id) . ok_or(
      "unique_neworgnode_child_with_interp: node not found")?;
  let matches : Vec<NodeId> = node_ref.children()
    .filter(|c| c.value().matches_interp ( &interp ))
    .map(|c| c.id())
    .collect();
  match matches.len() {
    0 => Ok(None),
    1 => Ok(Some(matches[0])),
    n => Err(format!(
      "Expected at most one {:?} child, found {}", interp, n).into()),
  }
}

/// Collect aliases for a node (for Tree<NewOrgNode>):
/// - find the unique AliasCol child (error if multiple)
/// - for each Alias child of the AliasCol, collect its title
/// Duplicates are removed (preserving order of first occurrence).
/// Returns None ("no opinion") if no AliasCol found.
/// Returns Some(vec) if AliasCol found, even if empty.
pub fn collect_grandchild_aliases_for_neworgnode (
  tree: &Tree<NewOrgNode>,
  node_id: NodeId,
) -> Result<Option<Vec<String>>, String> {
  let alias_col_id : Option<NodeId> =
    unique_neworgnode_child_with_interp (
      tree, node_id, Interp::AliasCol )
    . map_err ( |e| e.to_string() ) ?;
  match alias_col_id {
    None => Ok(None),
    Some(col_id) => {
      let aliases : Vec<String> = {
        let col_ref : NodeRef<NewOrgNode> = tree.get(col_id).expect(
          "collect_grandchild_aliases_for_neworgnode: AliasCol not found");
        let mut aliases : Vec<String> = Vec::new();
        for alias_child in col_ref.children() {
          { // check for invalid state
            if ! alias_child.value().is_scaffold(
                   &ScaffoldKind::Alias(String::new())) {
              return Err ( format! (
                "AliasCol has non-Alias child with interp: {:?}",
                alias_child.value().interp() )); }}
          aliases . push(
            alias_child . value() . title() . to_string() ); }
        aliases };
      Ok(Some(dedup_vector(aliases))) }} }
