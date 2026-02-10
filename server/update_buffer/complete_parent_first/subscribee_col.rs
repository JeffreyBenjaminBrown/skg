use crate::dbs::filesystem::one_node::skgnodes_from_ids;
use crate::git_ops::read_repo::skgnode_from_git_head;
use crate::types::viewnode::mk_phantom_viewnode;
use crate::types::git::{SourceDiff, NodeDiffStatus};
use crate::types::list::{compute_interleaved_diff,
                          itemlist_and_removedset_from_diff,
                          Diff_Item};
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::phantom::{source_for_phantom, title_for_phantom, phantom_diff_status};
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
use std::collections::{HashMap, HashSet};
use std::error::Error;
use typedb_driver::TypeDBDriver;

struct SubscribeeChildData {
  source  : SourceName,
  title   : String,
  phantom : Option<NodeDiffStatus>, // None = normal subscribee
}

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
  node               : NodeId,
  tree               : &mut Tree<ViewNode>,
  map                : &mut SkgNodeMap,
  source_diffs       : &Option<HashMap<SourceName, SourceDiff>>,
  config             : &SkgConfig,
  driver             : &TypeDBDriver,
  deleted_id_src_map : &HashMap<ID, SourceName>,
) -> Result<(), Box<dyn Error>> {
  error_unless_node_satisfies(
      tree, node,
      |vn : &ViewNode| matches!( &vn.kind,
                                 ViewNodeKind::Scaff(
                                   Scaffold::SubscribeeCol )),
      "complete_subscribee_col_preorder: expected SubscribeeCol"
    ). map_err( |e| -> Box<dyn Error> { e.into() } ) ?;
  let (parent_skgid, parent_source, parent_indefinitive)
    : (ID, SourceName, bool)
    = read_at_ancestor_in_tree(
      tree, node, 1,
      |vn : &ViewNode| match &vn.kind {
        ViewNodeKind::True( t ) =>
          Some(( t.id.clone(), t.source.clone(), t.indefinitive )),
        _ => None } )
    . map_err( |e| -> Box<dyn Error> { e.into() } ) ?
    . ok_or( "complete_subscribee_col_preorder: parent is not a TrueNode" ) ?;
  let worktree_subscribees : Vec<ID> =
    map.get( &parent_skgid )
      .map( |skg| skg.subscribes_to.clone().unwrap_or_default() )
      .unwrap_or_default();
  let (goal_list, removed_ids) : (Vec<ID>, HashSet<ID>) =
    diff_aware_goal_list(
      source_diffs, &parent_source, &parent_skgid,
      config, &worktree_subscribees );
  if goal_list.is_empty() { // delete SubscribeeCol, handling focus
    let has_focus : bool =
      subtree_satisfies( tree, node, &|n : &ViewNode| n.focused ) ?;
    if has_focus {
      write_at_ancestor_in_tree( tree, node, 1,
        |vn : &mut ViewNode| { vn.focused = true; } )
      .map_err( |e| -> Box<dyn Error> { e.into() } ) ?; }
    with_node_mut( tree, node, |mut n| { n.detach(); } )
      .map_err( |e| -> Box<dyn Error> { e.into() } ) ?;
    return Ok(( )); }
  if parent_indefinitive || source_diffs.is_some() { // If the parent is indefinitive, its SubscribeeCol might not reflect the truth, so we complete its children. If source_diffs is present (diff view), even a definitive parent needs reconciliation, so that removed subscribees appear as phantoms. (If the parent is definitive and the diff view is off, though, then the parent just *defined* what its children should be, so they don't need completion.)
    let missing : Vec<ID> = worktree_subscribees.iter()
      .filter( |id| !map.contains_key( id ) )
      .cloned()
      .collect();
    if !missing.is_empty() { // Ensure all subscribees are in the map.
      let fetched_missing : Vec<SkgNode> =
        skgnodes_from_ids( config, driver, &missing ).await ?;
      for skg in fetched_missing {
        if let Some( id ) = skg.ids.first() {
          map.insert( id.clone(), skg ); } } }
    let child_data : HashMap<ID, SubscribeeChildData> =
      // Pre-compute child creation data so that the create_child closure argument to complete_relevant_children_in_viewnodetree captures only owned data and does not conflict with the &mut Tree borrow in complete_relevant_children_in_viewnodetree.
      build_subscribee_child_data(
        tree, node, &goal_list, &removed_ids,
        source_diffs, map, deleted_id_src_map, config ) ?;
    complete_relevant_children_in_viewnodetree(
      tree, node,
      |vn : &ViewNode| matches!( &vn.kind,
                                 ViewNodeKind::True( t )
                                 if !t.parent_ignores ),
      |vn : &ViewNode| match &vn.kind {
        ViewNodeKind::True( t ) => t.id.clone(),
        _ => panic!( "complete_subscribee_col_preorder: \
                      relevant child not TrueNode" ) },
      &goal_list,
      |id : &ID| {
        let d : &SubscribeeChildData =
          child_data.get( id ).expect(
            "complete_subscribee_col_preorder: child data not pre-fetched" );
        match d.phantom {
          None =>
            mk_indefinitive_viewnode(
              id.clone(), d.source.clone(),
              d.title.clone(), false ),
          Some( diff_status ) =>
            mk_phantom_viewnode(
              id.clone(), d.source.clone(),
              d.title.clone(), diff_status ) } }, ) ?; }
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

/// Compute the goal list and removed set for subscribee children.
/// In diff view, diffs HEAD subscribees against worktree subscribees
/// so that removed subscriptions appear as phantoms.
/// Outside diff view, returns the worktree subscribees unchanged.
fn diff_aware_goal_list (
  source_diffs          : &Option<HashMap<SourceName, SourceDiff>>,
  parent_source         : &SourceName,
  parent_skgid          : &ID,
  config                : &SkgConfig,
  worktree_subscribees  : &[ID],
) -> (Vec<ID>, HashSet<ID>) {
  let head_subscribees : Option<Vec<ID>> =
    source_diffs . as_ref()
      . and_then( |diffs| diffs.get( parent_source ) )
      . filter( |sd| sd.is_git_repo )
      . and_then(
         |_| skgnode_from_git_head(
               parent_skgid, parent_source, config )
             . ok()
             . and_then( |skg| skg.subscribes_to ) );
  match head_subscribees {
    None =>
      (worktree_subscribees.to_vec(), HashSet::new()),
    Some( head ) => {
      let diff : Vec<Diff_Item<ID>> =
        compute_interleaved_diff( &head, worktree_subscribees );
      itemlist_and_removedset_from_diff( &diff ) } } }

/// Build a map from subscribee ID to SubscribeeChildData
/// for use in the create_child closure argument to
/// complete_relevant_children_in_viewnodetree.
/// By this point all worktree subscribees should be in the map
/// (loaded in the previous step if they were missing).
/// For each ID in goal_list:
/// - If NOT in removed_ids: look up from existing children or map
///   (current behavior), set phantom: None.
/// - If in removed_ids: look up title/source from existing children,
///   then map, then source_diffs deleted_nodes across all sources.
///   Determine phantom status: check if the subscribee's source's
///   SourceDiff.deleted_nodes contains the ID -> Some(Removed),
///   otherwise Some(RemovedHere).
fn build_subscribee_child_data (
  tree               : &Tree<ViewNode>,
  node               : NodeId,
  goal_list          : &[ID],
  removed_ids        : &HashSet<ID>,
  source_diffs       : &Option<HashMap<SourceName, SourceDiff>>,
  map                : &SkgNodeMap,
  deleted_id_src_map : &HashMap<ID, SourceName>,
  config             : &SkgConfig,
) -> Result<HashMap<ID, SubscribeeChildData>,
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
  let child_sources : HashMap<ID, SourceName> =
    existing_children.iter()
      .map( |(id, (s, _))| (id.clone(), s.clone()) )
      .collect();
  let mut result : HashMap<ID, SubscribeeChildData> = HashMap::new();
  for child_skgid in goal_list {
    if result.contains_key( child_skgid ) { continue; }
    if removed_ids.contains( child_skgid ) {
      let child_src : SourceName =
        source_for_phantom(
          child_skgid, &child_sources,
          deleted_id_src_map, map, config )
        .map_err( |e| -> Box<dyn Error> { e.into() } ) ?;
      let phantom : NodeDiffStatus =
        phantom_diff_status( child_skgid, &child_src, source_diffs.as_ref() );
      let child_title : String =
        title_for_phantom( child_skgid, &child_src,
                           source_diffs.as_ref(), map, config );
      result.insert( child_skgid.clone(),
                     SubscribeeChildData { source: child_src,
                                           title: child_title,
                                           phantom: Some( phantom ) } );
    } else { // Normal subscribee: exists in worktree subscriptions.
      if let Some( (s, t) ) = existing_children.get( child_skgid ) {
        result.insert( child_skgid.clone(),
                       SubscribeeChildData { source: s.clone(),
                                             title: t.clone(),
                                             phantom: None } );
      } else if let Some( skg ) = map.get( child_skgid ) {
        result.insert( child_skgid.clone(),
                       SubscribeeChildData { source: skg.source.clone(),
                                             title: skg.title.clone(),
                                             phantom: None } );
      } else {
        return Err( format!(
          "build_subscribee_child_data: \
           subscribee {} not found in children or map",
          child_skgid.0 ).into() ); } } }
  Ok( result ) }
