use crate::dbs::filesystem::one_node::skgnodes_from_ids;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::skgnode::SkgNode;
use crate::types::skgnodemap::SkgNodeMap;
use crate::types::viewnode::{
    ViewNode, ViewNodeKind, Scaffold,
    mk_indefinitive_viewnode};
use crate::types::tree::generic::{
    error_unless_node_satisfies,
    read_at_ancestor_in_tree,
    write_at_ancestor_in_tree,
    with_node_mut};
use crate::types::tree::viewnode_skgnode::{
    unique_scaffold_child,
    insert_scaffold_as_child};
use crate::update_buffer::util::{
    move_child_to_end,
    subtree_satisfies,
    complete_relevant_children_in_viewnodetree};

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::HashMap;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// SubscribeeCol completion.
///
/// INTENDED USE: Use in the first, DFS preorder (parent-first)
/// buffer-update pass through the tree.
///
/// WHAT IT DOES:
/// - Error unless it's a SubscribeeCol.
/// - Read parent's skg ID and indefinitive flag.
/// - Look up parent's subscribees.
/// - If no subscribees: transfer focus if needed, then delete.
/// - If parent is indefinitive: reconcile subscribee children.
/// - Ensure HiddenOutsideOfSubscribeeCol exists and is last.
pub async fn complete_subscribee_col_preorder (
  node   : NodeId,
  tree   : &mut Tree<ViewNode>,
  map    : &mut SkgNodeMap,
  config : &SkgConfig,
  driver : &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  error_unless_node_satisfies(
      tree, node,
      |vn : &ViewNode| matches!( &vn.kind,
                                 ViewNodeKind::Scaff(
                                   Scaffold::SubscribeeCol )),
      "complete_subscribee_col_preorder: expected SubscribeeCol"
    ). map_err( |e| -> Box<dyn Error> { e.into() } ) ?;
  let (parent_skgid, parent_indefinitive) : (ID, bool) =
    read_at_ancestor_in_tree(
      tree, node, 1,
      |vn : &ViewNode| match &vn.kind {
        ViewNodeKind::True( t ) =>
          Some(( t.id.clone(), t.indefinitive )),
        _ => None } )
    . map_err( |e| -> Box<dyn Error> { e.into() } ) ?
    . ok_or( "complete_subscribee_col_preorder: parent is not a TrueNode" ) ?;
  let subscribees : Vec<ID> =
    map.get( &parent_skgid )
      .map( |skg| skg.subscribes_to.clone().unwrap_or_default() )
      .unwrap_or_default();
  if subscribees.is_empty() { // delete SubscribeeCol, handling focus
    let has_focus : bool =
      subtree_satisfies( tree, node, &|n : &ViewNode| n.focused ) ?;
    if has_focus {
      write_at_ancestor_in_tree( tree, node, 1,
        |vn : &mut ViewNode| { vn.focused = true; } )
      .map_err( |e| -> Box<dyn Error> { e.into() } ) ?; }
    with_node_mut( tree, node, |mut n| { n.detach(); } )
      .map_err( |e| -> Box<dyn Error> { e.into() } ) ?;
    return Ok(( )); }
  if parent_indefinitive { // If the parent is indefinitive (this case), then its SubscribeeCol might not reflect the truth, so we have to complete its children. We don't do this if the parent is definitive, because in that case the SubscribeeCol just *defined* the parent's subscriptions, and therefore does not need to be adjusted to reflect them.
    let missing : Vec<ID> = subscribees.iter()
      .filter( |id| !map.contains_key( id ) )
      .cloned()
      .collect();
    if !missing.is_empty() { // Ensure all subscribees are in the map.
      let fetched_missing : Vec<SkgNode> =
        skgnodes_from_ids( config, driver, &missing ).await ?;
      for skg in fetched_missing {
        if let Some( id ) = skg.ids.first() {
          map.insert( id.clone(), skg ); } } }
    let child_data : HashMap<ID, (SourceName,
                                  String)> = // title
      // Pre-compute child creation data so that the create_child closure argument to complete_relevant_children_in_viewnodetree captures only owned data and does not conflict with the &mut Tree borrow in complete_relevant_children_in_viewnodetree.
      build_subscribee_child_data(
        tree, node, &subscribees, map ) ?;
    complete_relevant_children_in_viewnodetree(
      tree, node,
      |vn : &ViewNode| matches!( &vn.kind,
                                 ViewNodeKind::True( t )
                                 if !t.parent_ignores ),
      |vn : &ViewNode| match &vn.kind {
        ViewNodeKind::True( t ) => t.id.clone(),
        _ => panic!( "complete_subscribee_col_preorder: \
                      relevant child not TrueNode" ) },
      &subscribees,
      |id : &ID| {
        let ( source, title ) =
          child_data . get( id ) . expect( "complete_subscribee_col_preorder: child data not pre-fetched" );
        mk_indefinitive_viewnode(
          id.clone(), source.clone(),
          title.clone(), false ) }, ) ?; }
  { // Ensure HiddenOutsideOfSubscribeeCol exists and is last.
    let hidden_outside : Option<NodeId> =
      unique_scaffold_child(
        tree, node, &Scaffold::HiddenOutsideOfSubscribeeCol ) ?;
    match hidden_outside {
      Some( child ) => { move_child_to_end(
                           tree, node, child ) ?; },
      None => { insert_scaffold_as_child(
                  tree, node,
                  Scaffold::HiddenOutsideOfSubscribeeCol,
                  false ) ?; }, }}
  Ok(( )) }

/// Build a map from subscribee ID to (source, title)
/// for use in the create_child closure argument to
/// complete_relevant_children_in_viewnodetree.
/// By this point all subscribees should be in the map
/// (loaded in the previous step if they were missing).
/// For each subscribee ID:
/// 1. If already an existing TrueNode child -> use its source/title.
/// 2. Else if in map -> use skgnode.source and skgnode.title.
/// 3. Else -> Err.
fn build_subscribee_child_data (
  tree        : &Tree<ViewNode>,
  node        : NodeId,
  subscribees : &[ID],
  map         : &SkgNodeMap,
) -> Result<HashMap<ID, (SourceName, String)>, // id -> (source, title)
            Box<dyn Error>> {
  let existing_children : HashMap<ID, (SourceName, String)> =
    { let node_ref : NodeRef<ViewNode> =
        tree.get( node )
          .ok_or( "build_subscribee_child_data: node not found" ) ?;
      let mut m : HashMap<ID, (SourceName, String)> = HashMap::new();
      for child_ref in node_ref.children() {
        if let ViewNodeKind::True( t ) = &child_ref.value().kind {
          m.insert( t.id.clone(),
                    ( t.source.clone(), t.title.clone() )); }}
      m };
  let mut result : HashMap<ID, (SourceName, String)> = HashMap::new();
  for id in subscribees {
    if result.contains_key( id ) { continue; }
    if let Some( data ) = existing_children.get( id ) {
      result.insert( id.clone(), data.clone() );
    } else if let Some( skg ) = map.get( id ) {
      result.insert( id.clone(),
                     ( skg.source.clone(), skg.title.clone() ));
    } else {
      return Err( format!(
        "build_subscribee_child_data: \
         subscribee {} not found in children or map",
        id.0 ).into() ); } }
  Ok( result ) }
