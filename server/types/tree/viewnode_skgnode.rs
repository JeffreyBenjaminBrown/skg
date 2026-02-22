/// Node access utilities for ego_tree::Tree<ViewNode> and Tree<UncheckedViewNode>

use crate::to_org::util::{skgnode_and_viewnode_from_id, get_id_from_treenode};
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::viewnode::{
    ViewNode, ViewNodeKind, Scaffold,
    mk_indefinitive_viewnode,
    viewnode_from_scaffold };
use crate::types::unchecked_viewnode::{
    UncheckedViewNode, UncheckedViewNodeKind };
use crate::types::skgnode::SkgNode;
use crate::types::skgnodemap::SkgNodeMap;
use crate::types::list::dedup_vector;
use super::generic::with_node_mut;

use ego_tree::{Tree, NodeId, NodeRef};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use neo4rs::Graph;

/// Extract (ID, source) from a TrueNode in the tree.
/// Returns an error if the node is not found or not a TrueNode.
pub fn pid_and_source_from_treenode (
  tree        : &Tree<ViewNode>,
  treeid      : NodeId,
  caller_name : &str,
) -> Result<(ID, SourceName), Box<dyn Error>> {
  let node_ref : NodeRef<ViewNode> =
    tree . get ( treeid ) . ok_or_else ( ||
      format! ( "{}: node not found", caller_name ) ) ?;
  let ViewNodeKind::True ( t ) : &ViewNodeKind =
    &node_ref . value() . kind
    else { return Err ( format! ( "{}: expected TrueNode",
                                  caller_name ) . into() ) };
  Ok (( t.id.clone(), t.source.clone() )) }

/// Get the ID from this node if it's an UncheckedTrueNode with an ID,
/// otherwise recursively try ancestors.
/// Returns an error if no ancestor has an ID (e.g., reached BufferRoot).
pub fn id_from_self_or_nearest_ancestor (
  tree    : &Tree<UncheckedViewNode>,
  node_id : NodeId,
) -> Result<ID, String> {
  let mut node : NodeRef<UncheckedViewNode> =
    tree.get(node_id)
    .ok_or("id_from_self_or_nearest_ancestor: node not found")?;
  loop {
    if let UncheckedViewNodeKind::True(t) = &node.value().kind {
      if let Some(id) = &t.id_opt {
        return Ok(id.clone()); }}
    node = node.parent()
      . ok_or("id_from_self_or_nearest_ancestor: reached root without finding ID")?; }}

/// Find the unique child of a node with a given Scaffold.
/// Returns None if no child has the kind,
/// Some(child_id) if exactly one does,
/// or an error if multiple children have it.
pub fn unique_scaffold_child (
  tree          : &Tree<ViewNode>,
  node_id       : NodeId,
  scaffold_kind : &Scaffold,
) -> Result<Option<NodeId>, Box<dyn Error>> {
  let node_ref : NodeRef<ViewNode> =
    tree . get(node_id) . ok_or(
      "unique_scaffold_child: node not found")?;
  let matches : Vec<NodeId> = node_ref.children()
    .filter(|c| matches!(&c.value().kind,
                         ViewNodeKind::Scaff(s)
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
  tree    : &Tree<ViewNode>,
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
  tree    : &Tree<ViewNode>,
  map     : &SkgNodeMap,
  node_id : NodeId,
) -> Result < ( ID, ID ), Box<dyn Error> > {
  let subscribee_pid : ID = get_id_from_treenode ( tree, node_id ) ?;
  let node_ref : NodeRef < ViewNode > =
    tree . get ( node_id ) . ok_or (
      "pid_for_subscribee_and_its_subscriber_grandparent: node not found" ) ?;
  let parent_ref : NodeRef < ViewNode > =
    node_ref . parent ()
    . ok_or ( "Subscribee has no parent (SubscribeeCol)" ) ?;
  if ! matches! ( &parent_ref . value () . kind,
                  ViewNodeKind::Scaff ( Scaffold::SubscribeeCol )) {
    return Err ( "Subscribee's parent is not a SubscribeeCol" .
                 into () ); }
  let grandparent_ref : NodeRef < ViewNode > =
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
  tree          : &mut Tree<ViewNode>,
  parent_id     : NodeId,
  scaffold_kind : Scaffold,
  prepend       : bool, // otherwise, append
) -> Result < NodeId, Box<dyn Error> > {
  let viewnode : ViewNode =
    viewnode_from_scaffold ( scaffold_kind );
  let col_id : NodeId = with_node_mut (
    tree, parent_id,
    |mut parent_mut| {
      if prepend { parent_mut . prepend ( viewnode ) . id () }
      else       { parent_mut . append  ( viewnode ) . id () } } )
    . map_err ( |e| -> Box<dyn Error> { e.into() } ) ?;
  Ok ( col_id ) }

/// Fetch a node from disk and append it as an indefinitive child.
/// Also adds the SkgNode to the map.
pub async fn append_indefinitive_from_disk_as_child (
  tree           : &mut Tree<ViewNode>,
  map            : &mut SkgNodeMap,
  parent_id      : NodeId,
  node_id        : &ID,
  parent_ignores : bool,
  config         : &SkgConfig,
  graph          : &Graph,
) -> Result < (), Box<dyn Error> > {
  let ( _skgnode, content_viewnode ) : ( SkgNode, ViewNode ) =
    skgnode_and_viewnode_from_id (
      config, graph, node_id, map ) . await ?;
  let (id, source, title) : (ID, SourceName, String)
  = match &content_viewnode.kind
  { ViewNodeKind::True(t) => (
      t . id . clone(),
      t . source . clone(),
      t . title . clone( )),
    ViewNodeKind::Scaff(_) => return Err("append_indefinitive_from_disk_as_child: expected TrueNode".into()) };
  let viewnode : ViewNode = mk_indefinitive_viewnode (
    id, source, title, parent_ignores );
  with_node_mut (
    tree, parent_id,
    |mut parent_mut| {
      parent_mut . append ( viewnode ); } )
    . map_err ( |e| -> Box<dyn Error> { e.into() } ) ?;
  Ok (( )) }

/// Collect titles from Alias children of an AliasCol.
/// Removes Duplicate (preserving order of first occurrence).
/// Errors if any non-Alias children are found.
pub(crate) fn collect_child_aliases_at_aliascol (
  tree             : &Tree<ViewNode>,
  aliascol_node_id : NodeId,
) -> Result < Vec < String >, Box<dyn Error> > {
  let mut aliases : Vec < String > =
    Vec::new ();
  let aliascol_ref : NodeRef < ViewNode > =
    tree . get ( aliascol_node_id )
    . ok_or ( "AliasCol node not found" ) ?;
  for child_ref in aliascol_ref . children() {
    let child : &ViewNode = child_ref . value();
    if ! matches! ( &child . kind,
                    ViewNodeKind::Scaff ( Scaffold::Alias { .. } )) {
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
pub fn collect_grandchild_aliases_for_viewnode (
  tree: &Tree<ViewNode>,
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
        let col_ref : NodeRef<ViewNode> = tree.get(col_id).expect( "collect_grandchild_aliases_for_viewnode: AliasCol not found");
        let mut aliases : Vec<String> = Vec::new();
        for alias_child in col_ref.children() {
          { // check for invalid state
            if ! matches!(&alias_child.value().kind,
                          ViewNodeKind::Scaff(Scaffold::Alias { .. })) {
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
  tree          : & Tree<ViewNode>,
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
  tree          : & Tree<ViewNode>,
  parent_treeid : NodeId,
  target_skgids : & HashSet < ID >,
) -> HashMap < ID, NodeId > {
  let mut result : HashMap < ID, NodeId > = HashMap::new();
  for child in tree.get(parent_treeid).unwrap().children() {
    if let ViewNodeKind::True(t) = &child.value().kind {
      if target_skgids.contains(&t.id) {
        result.insert(t.id.clone(), child.id()); }}}
  result }

/// Check if all nodes at the specified generation satisfy the predicate.
/// Returns true if the generation is empty (vacuously true).
/// Negative generations = ancestors; positive = descendants.
/// If skip_parent_ignores, excludes TrueNodes with parent_ignores=true.
pub fn generation_includes_only<F> (
  tree                : &Tree<UncheckedViewNode>,
  node_id             : NodeId,
  generation          : i32,
  skip_parent_ignores : bool,
  predicate           : F,
) -> bool
where F: Fn(&UncheckedViewNode) -> bool
{ collect_generation( tree, node_id, generation, skip_parent_ignores )
    . iter()
    . all ( |&id| predicate(
      tree . get(id) . unwrap() . value() )) }

/// Check if the generation is nonempty and all nodes satisfy the predicate.
/// Negative generations = ancestors; positive = descendants.
/// If skip_parent_ignores, excludes TrueNodes with parent_ignores=true.
pub fn generation_exists_and_includes<F> (
  tree                : &Tree<UncheckedViewNode>,
  node_id             : NodeId,
  generation          : i32,
  skip_parent_ignores : bool,
  predicate           : F,
) -> bool
where F: Fn(&UncheckedViewNode) -> bool
{ let nodes = collect_generation(
    tree, node_id, generation, skip_parent_ignores);
  !nodes.is_empty() &&
    nodes . iter() . all(
      |&id| predicate(
        tree . get(id) . unwrap() . value() )) }

/// Check if the specified generation is empty.
/// Negative generations = ancestors; positive = descendants.
/// If skip_parent_ignores, excludes TrueNodes with parent_ignores=true.
pub fn generation_does_not_exist (
  tree                : &Tree<UncheckedViewNode>,
  node_id             : NodeId,
  generation          : i32,
  skip_parent_ignores : bool,
) -> bool {
  collect_generation( tree, node_id, generation, skip_parent_ignores
                    ). is_empty() }

/// Collect NodeIds at a specified generation relative to the given node.
/// Negative generation = ancestors (-1 = parent, -2 = grandparent, etc.)
/// Positive generation = descendants (1 = children, 2 = grandchildren, etc.)
/// Generation 0 returns just the node itself.
/// If 'skip_parent_ignores' is true and generation > 0,
///   then we exclude TrueNodes with parent_ignores=true.
fn collect_generation (
  tree               : &Tree<UncheckedViewNode>,
  node_id            : NodeId,
  generation         : i32,
  skip_parent_ignores : bool,
) -> Vec<NodeId> {
  if generation == 0 {
    return vec![node_id]; }
  let Some(node_ref) = tree.get(node_id)
    else { return vec![]; };
  if generation <= 0 {
    let mut current : NodeRef<'_, UncheckedViewNode> =
      node_ref;
    for _ in 0..(-generation) {
      match current.parent() {
        Some(parent) => current = parent,
        None => return vec![], }}
    vec![current.id()] }
  else {
    let mut current_gen : Vec<NodeId> =
      vec![node_id];
    for _ in 0..generation {
      let mut next_gen : Vec<NodeId> =
        vec![];
      for id in current_gen {
        if let Some(n) = tree.get(id) {
          next_gen.extend(
            n.children()
              .filter(|c| !skip_parent_ignores ||
                          !matches!(&c.value().kind,
                                    UncheckedViewNodeKind::True(t)
                                    if t.parent_ignores))
              .map(|c| c.id()) ); }}
      current_gen = next_gen; }
    current_gen } }

/// Check that no sibling satisfies the predicate.
/// Returns true if the node has no siblings,
/// or if the predicate returns false for all siblings.
/// Short-circuits on the first sibling where the predicate returns true.
pub fn siblings_cannot_include<F> (
  tree      : &Tree<UncheckedViewNode>,
  node_id   : NodeId,
  predicate : F,
) -> bool
where F: Fn(&UncheckedViewNode) -> bool
{ let Some(node_ref) = tree.get(node_id)
    else { return true; };
  let Some(parent_ref) = node_ref.parent()
    else { return true; };
  ! parent_ref.children()
      . filter ( |c| c.id() != node_id )
      . any    ( |c| predicate( c.value() )) }
