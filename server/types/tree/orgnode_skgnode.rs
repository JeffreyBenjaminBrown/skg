/// Node access utilities for ego_tree::Tree<OrgNode|NodePair>

use crate::dbs::filesystem::one_node::skgnode_from_id;
use crate::to_org::util::skgnode_and_orgnode_from_id;
use crate::types::misc::{ID, SkgConfig};
use crate::types::orgnode::{
    OrgNode, OrgNodeKind, Scaffold,
    mk_indefinitive_orgnode,
    orgnode_from_scaffold };
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

/// Find the unique child of a node with a given Scaffold (for PairTree).
/// Returns None if no child has the kind,
/// Some(child_id) if exactly one does,
/// or an error if multiple children have it.
pub fn unique_scaffold_child (
  tree          : &PairTree,
  node_id       : NodeId,
  scaffold_kind : &Scaffold,
) -> Result<Option<NodeId>, Box<dyn Error>> {
  let node_ref : ego_tree::NodeRef<super::NodePair> =
    tree.get(node_id)
    .ok_or("unique_scaffold_child: node not found")?;
  let matches : Vec<NodeId> = node_ref.children()
    .filter(|c| matches!(&c.value().orgnode.kind,
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

/// Find the unique child of a node with a given Scaffold (for Tree<OrgNode>).
/// Returns None if no child has the kind,
/// Some(child_id) if exactly one does,
/// or an error if multiple children have it.
pub fn unique_orgnode_scaffold_child (
  tree          : &Tree<OrgNode>,
  node_id       : NodeId,
  scaffold_kind : &Scaffold,
) -> Result<Option<NodeId>, Box<dyn Error>> {
  let node_ref : ego_tree::NodeRef<OrgNode> =
    tree . get(node_id) . ok_or(
      "unique_orgnode_scaffold_child: node not found")?;
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
    match &node_ref . value () .orgnode . kind {
      OrgNodeKind::True ( t ) => t . id_opt . clone () . expect (
        "Subscribee should have an ID." ),
      OrgNodeKind::Scaff ( _ ) => return Err (
        "Subscribee is not a true node." . into() ) };
  let parent_ref : NodeRef < NodePair > =
    node_ref . parent ()
    . ok_or ( "Subscribee has no parent (SubscribeeCol)" ) ?;
  if ! matches! ( &parent_ref . value () . orgnode . kind,
                  OrgNodeKind::Scaff ( Scaffold::SubscribeeCol )) {
    return Err ( "Subscribee's parent is not a SubscribeeCol" .
                 into () ); }
  let grandparent_ref : NodeRef < NodePair > =
    parent_ref . parent ()
    . ok_or ( "SubscribeeCol has no parent (subscriber)" ) ?;
  let skgnode : &SkgNode =
    grandparent_ref . value () . mskgnode . as_ref ()
    . ok_or ( "Subscriber has no SkgNode" ) ?;
  Ok (( subscribee_pid,
        skgnode . ids[0] . clone() )) }

pub fn insert_scaffold_as_child (
  tree          : &mut PairTree,
  parent_id     : NodeId,
  scaffold_kind : Scaffold,
  prepend       : bool, // otherwise, append
) -> Result < NodeId, Box<dyn Error> > {
  let orgnode : OrgNode =
    orgnode_from_scaffold ( scaffold_kind );
  let col_id : NodeId = with_node_mut (
    tree, parent_id,
    |mut parent_mut| {
      let pair = NodePair { mskgnode : None,
                            orgnode  : orgnode };
      if prepend { parent_mut . prepend ( pair ) . id () }
      else       { parent_mut . append  ( pair ) . id () } } ) ?;
  Ok ( col_id ) }

/// Fetch a node from disk and append it as an indefinitive child.
pub async fn append_indefinitive_from_disk_as_child (
  tree           : &mut PairTree,
  parent_id      : NodeId,
  node_id        : &ID,
  parent_ignores : bool,
  config         : &SkgConfig,
  driver         : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let ( skgnode, content_orgnode ) : ( SkgNode, OrgNode ) =
    skgnode_and_orgnode_from_id (
      config, driver, node_id ) . await ?;
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
      parent_mut . append (
        NodePair { mskgnode : Some ( skgnode ),
                   orgnode  : orgnode } ); } ) ?;
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
    read_at_ancestor_in_tree(
      tree, treeid, generation,
      |np| match &np.orgnode.kind {
        OrgNodeKind::True(t) => t.id_opt.clone()
          .ok_or("Ancestor TrueNode has no ID"),
        OrgNodeKind::Scaff(_) => Err(
          "Ancestor  is a Scaffold, not a TrueNode") } )??;
  let skgnode : SkgNode =
    skgnode_from_id( config, driver, &ancestor_skgid ) . await?;
  Ok ( skgnode ) }

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
  for child_ref in aliascol_ref . children() {
    let child : &OrgNode = &child_ref . value() . orgnode;
    if ! matches! ( &child . kind,
                    OrgNodeKind::Scaff ( Scaffold::Alias(_) )) {
      return Err (
        format! ( "AliasCol has non-Alias child with kind: {:?}",
                  child . kind )
        . into () ); }
    aliases . push (
      child . title () . to_string () ); }
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
    unique_orgnode_scaffold_child (
      tree, node_id, &Scaffold::AliasCol )
    . map_err ( |e| e.to_string() ) ?;
  match alias_col_id {
    None => Ok(None),
    Some(col_id) => {
      let aliases : Vec<String> = {
        let col_ref : NodeRef<OrgNode> = tree.get(col_id).expect(
          "collect_grandchild_aliases_for_orgnode: AliasCol not found");
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
