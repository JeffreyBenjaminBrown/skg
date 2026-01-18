/// Node access utilities for ego_tree::Tree<OrgNode|NodePair>

use crate::dbs::filesystem::one_node::skgnode_from_id;
use crate::to_org::util::skgnode_and_orgnode_from_id;
use crate::types::misc::{ID, SkgConfig};
use crate::types::orgnode::{
    OrgNode, OrgNodeKind, Scaffold,
    mk_indefinitive_orgnode,
    orgnode_from_scaffold };
use crate::types::skgnode::{SkgNode, SkgNodeMap};
use crate::util::dedup_vector;
use super::{NodePair, PairTree};
use super::generic::{read_at_ancestor_in_tree, read_at_node_in_tree, with_node_mut};

use ego_tree::{Tree, NodeId, NodeRef, NodeMut};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use typedb_driver::TypeDBDriver;

///
/// Conversion between PairTree and (Tree<OrgNode>, SkgNodeMap)
///

/// Convert Tree<OrgNode> and SkgNodeMap into a PairTree.
/// For each OrgNode, looks up the SkgNode in the map by ID.
pub fn pairtree_from_tree_and_map (
  tree : &Tree<OrgNode>,
  map  : &SkgNodeMap,
) -> PairTree {
  fn clone_subtree (
    node_ref   : NodeRef<OrgNode>,
    map        : &SkgNodeMap,
    parent_mut : &mut NodeMut<NodePair>,
  ) {
    for child_ref in node_ref . children() {
      let orgnode : OrgNode = child_ref . value() . clone();
      let mskgnode : Option<SkgNode> = match &orgnode.kind {
        OrgNodeKind::True(t) =>
          t . id_opt . as_ref() . and_then( |id| map.get(id) . cloned() ),
        OrgNodeKind::Scaff(_) => None,
      };
      let pair : NodePair = NodePair { mskgnode, orgnode };
      let mut child_mut : NodeMut<NodePair> = parent_mut . append ( pair );
      clone_subtree ( child_ref, map, &mut child_mut ); } }

  // Start with root
  let root_ref : NodeRef<OrgNode> = tree . root();
  let root_orgnode : OrgNode = root_ref . value() . clone();
  let root_mskgnode : Option<SkgNode> = match &root_orgnode.kind {
    OrgNodeKind::True(t) =>
      t . id_opt . as_ref() . and_then( |id| map.get(id) . cloned() ),
    OrgNodeKind::Scaff(_) => None,
  };
  let root_pair : NodePair = NodePair {
    mskgnode : root_mskgnode,
    orgnode  : root_orgnode,
  };
  let mut result : PairTree = Tree::new ( root_pair );
  { let mut root_mut : NodeMut<NodePair> = result . root_mut();
    clone_subtree ( tree.root(), map, &mut root_mut ); }
  result }

/// Extract Tree<OrgNode> and SkgNodeMap from a PairTree.
/// Returns a new tree and map built from the PairTree.
pub fn tree_and_map_from_pairtree (
  pairtree : &PairTree,
) -> (Tree<OrgNode>, SkgNodeMap) {
  fn clone_subtree (
    node_ref   : NodeRef<NodePair>,
    parent_mut : &mut NodeMut<OrgNode>,
    map        : &mut SkgNodeMap,
  ) {
    for child_ref in node_ref . children() {
      let pair : &NodePair = child_ref . value();
      let orgnode : OrgNode = pair . orgnode . clone();

      // Add SkgNode to map if present
      if let Some(skgnode) = &pair.mskgnode {
        if let Some(id) = skgnode . ids . first() {
          map . insert ( id . clone(), skgnode . clone() ); }}

      let mut child_mut : NodeMut<OrgNode> =
        parent_mut . append ( orgnode );
      clone_subtree ( child_ref, &mut child_mut, map ); } }

  let mut map : SkgNodeMap = SkgNodeMap::new();

  // Start with root
  let root_ref : NodeRef<NodePair> = pairtree . root();
  let root_pair : &NodePair = root_ref . value();
  let root_orgnode : OrgNode = root_pair . orgnode . clone();

  // Add root SkgNode to map if present
  if let Some(skgnode) = &root_pair.mskgnode {
    if let Some(id) = skgnode . ids . first() {
      map . insert ( id . clone(), skgnode . clone() ); }}

  let mut tree : Tree<OrgNode> = Tree::new ( root_orgnode );
  { let mut root_mut : NodeMut<OrgNode> = tree . root_mut();
    clone_subtree ( pairtree.root(), &mut root_mut, &mut map ); }

  ( tree, map ) }

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

/// Extract PIDs for subscriber and subscribees from Tree<OrgNode> + SkgNodeMap.
pub fn pids_for_subscriber_and_its_subscribees_in_orgtree (
  tree    : &Tree<OrgNode>,
  map     : &SkgNodeMap,
  node_id : NodeId,
) -> Result < ( ID, Vec < ID > ),
              Box<dyn Error> > {
  // Get the node's ID
  let pid : ID =
    read_at_node_in_tree (
      tree, node_id,
      |orgnode| match &orgnode.kind {
        OrgNodeKind::True(t) => t . id_opt . clone()
          . ok_or("pids_for_subscriber_and_its_subscribees_in_orgtree: node has no ID"),
        OrgNodeKind::Scaff(_) => Err (
          "pids_for_subscriber_and_its_subscribees_in_orgtree: expected TrueNode" ),
      } )
    . map_err ( |e| -> Box<dyn Error> { e.into() } ) ??;

  // Look up SkgNode in map
  let skgnode : &SkgNode =
    map . get ( &pid )
    . ok_or ( "pids_for_subscriber_and_its_subscribees_in_orgtree: SkgNode should exist in map" ) ?;

  Ok (( skgnode . ids [0] . clone (),
        skgnode . subscribes_to . clone ()
        . unwrap_or_default () )) }

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

/// V2: Tree<OrgNode> + SkgNodeMap version of pid_for_subscribee_and_its_subscriber_grandparent.
pub fn pid_for_subscribee_and_its_subscriber_grandparent_in_orgtree (
  tree    : &Tree<OrgNode>,
  map     : &SkgNodeMap,
  node_id : NodeId,
) -> Result < ( ID, ID ), Box<dyn Error> > {
  let node_ref : NodeRef < OrgNode > =
    tree . get ( node_id ) . ok_or (
      "pid_for_subscribee_and_its_subscriber_grandparent_in_orgtree: node not found" ) ?;
  let subscribee_pid : ID =
    match &node_ref . value () . kind {
      OrgNodeKind::True ( t ) => t . id_opt . clone () . expect (
        "Subscribee should have an ID." ),
      OrgNodeKind::Scaff ( _ ) => return Err (
        "Subscribee is not a true node." . into() ) };
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
    match &grandparent_ref . value () . kind {
      OrgNodeKind::True ( t ) => t . id_opt . clone ()
        . ok_or ( "Subscriber has no ID" ) ?,
      OrgNodeKind::Scaff ( _ ) => return Err (
        "Subscriber is not a true node." . into() ) };
  let skgnode : &SkgNode =
    map . get ( &subscriber_id )
    . ok_or ( "Subscriber SkgNode not in map" ) ?;
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

/// Insert a scaffold node as a child in Tree<OrgNode>.
pub fn insert_scaffold_as_child_in_orgtree (
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

/// Fetch a node from disk and append it as an indefinitive child to Tree<OrgNode>.
/// Also adds the SkgNode to the map.
pub async fn append_indefinitive_from_disk_as_child_in_orgtree (
  tree           : &mut Tree<OrgNode>,
  map            : &mut SkgNodeMap,
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
      t . id_opt . as_ref() . ok_or("append_indefinitive_from_disk_as_child_in_orgtree: node has no ID")?
        . clone(),
      t . source_opt . as_ref() . ok_or("append_indefinitive_from_disk_as_child_in_orgtree: node has no source")?
        . clone(),
      t . title . clone( )),
    OrgNodeKind::Scaff(_) => return Err("append_indefinitive_from_disk_as_child_in_orgtree: expected TrueNode".into()) };

  // Add SkgNode to map
  map . insert ( id.clone(), skgnode );

  // Create indefinitive OrgNode and append to tree
  let orgnode : OrgNode = mk_indefinitive_orgnode (
    id, source, title, parent_ignores );
  with_node_mut (
    tree, parent_id,
    |mut parent_mut| {
      parent_mut . append ( orgnode ); } )
    . map_err ( |e| -> Box<dyn Error> { e.into() } ) ?;

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

/// Find a child node by its ID.
/// Returns the NodeId of the child if found, None otherwise.
pub fn find_child_by_id (
  tree          : & PairTree,
  parent_treeid : ego_tree::NodeId,
  target_skgid  : & ID,
) -> Option < ego_tree::NodeId > {
  let singleton : HashSet<ID> =
    std::iter::once( target_skgid.clone() )
    . collect();
  find_children_by_ids( tree, parent_treeid, &singleton)
    . remove(target_skgid) }

/// Find child nodes by their IDs.
/// Returns a map from ID to NodeId for children that were found.
/// IDs not found as children are not included in the result.
pub fn find_children_by_ids (
  tree          : & PairTree,
  parent_treeid : ego_tree::NodeId,
  target_skgids : & HashSet < ID >,
) -> HashMap < ID, ego_tree::NodeId > {
  let mut result : HashMap < ID, ego_tree::NodeId > = HashMap::new();
  for child in tree.get(parent_treeid).unwrap().children() {
    if let OrgNodeKind::True(t) = &child.value().orgnode.kind {
      if let Some(child_id) = &t.id_opt {
        if target_skgids.contains(child_id) {
          result.insert(child_id.clone(), child.id()); }}}}
  result }

/// V2: Find a child node by ID in Tree<OrgNode>.
pub fn find_child_by_id_in_orgtree (
  tree          : & Tree<OrgNode>,
  parent_treeid : ego_tree::NodeId,
  target_skgid  : & ID,
) -> Option < ego_tree::NodeId > {
  let singleton : HashSet<ID> =
    std::iter::once( target_skgid.clone() )
    . collect();
  find_children_by_ids_in_orgtree( tree, parent_treeid, &singleton)
    . remove(target_skgid) }

/// V2: Find child nodes by their IDs in Tree<OrgNode>.
pub fn find_children_by_ids_in_orgtree (
  tree          : & Tree<OrgNode>,
  parent_treeid : ego_tree::NodeId,
  target_skgids : & HashSet < ID >,
) -> HashMap < ID, ego_tree::NodeId > {
  let mut result : HashMap < ID, ego_tree::NodeId > = HashMap::new();
  for child in tree.get(parent_treeid).unwrap().children() {
    if let OrgNodeKind::True(t) = &child.value().kind {
      if let Some(child_id) = &t.id_opt {
        if target_skgids.contains(child_id) {
          result.insert(child_id.clone(), child.id()); }}}}
  result }

/// Reorder children of a parent node.
/// Takes two vectors of NodeIds: first and second.
/// All children in 'first' will be placed before all children in 'second'.
pub fn reorder_children (
  tree          : &mut PairTree,
  parent_treeid : NodeId,
  first         : &Vec < NodeId >,
  second        : &Vec < NodeId >,
) -> Result < (), Box<dyn Error> > {
  let mut desired_order : Vec < NodeId > =
    Vec::new ();
  desired_order . extend ( first );
  desired_order . extend ( second );

  for child_treeid in & desired_order {
    // Detach all children
    with_node_mut ( tree, *child_treeid,
                    |mut child_mut| child_mut . detach () ) ?; }

  for child_treeid in & desired_order {
    // Re-append in desired order.
    // ('append_id' preserves entire subtrees.)
    with_node_mut (
      tree, parent_treeid,
      |mut parent_mut| {
        parent_mut . append_id ( *child_treeid ); } ) ?; }
  Ok (( )) }
