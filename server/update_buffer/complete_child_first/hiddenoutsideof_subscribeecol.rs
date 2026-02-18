use crate::git_ops::read_repo::skgnode_from_git_head;
use crate::types::viewnode::mk_phantom_viewnode;
use crate::types::git::{SourceDiff, NodeDiffStatus, NodeChanges, node_changes_for_truenode};
use crate::types::list::{compute_interleaved_diff, itemlist_and_removedset_from_diff, Diff_Item};
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::phantom::{source_for_phantom, title_for_phantom, phantom_diff_status};
use crate::types::skgnode::SkgNode;
use crate::types::skgnodemap::SkgNodeMap;
use crate::types::tree::generic::{error_unless_node_satisfies, pid_and_source_from_ancestor, read_at_ancestor_in_tree, write_at_ancestor_in_tree, with_node_mut};
use crate::types::viewnode::{ViewNode, ViewNodeKind, Scaffold, mk_indefinitive_viewnode};
use crate::update_buffer::util::{complete_relevant_children_in_viewnodetree, treat_certain_children, subtree_satisfies};

use ego_tree::{NodeId, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;

struct HiddenChildData {
  source  : SourceName,
  title   : String,
  phantom : Option<NodeDiffStatus>,
}

/// HiddenOutsideOfSubscribeeCol completion (child-first / postorder pass).
///
/// Tree structure:
///   Subscriber (TrueNode)                       <- ancestor 2
///     └─ SubscribeeCol (Scaffold)               <- ancestor 1 = parent
///          ├─ Subscribee_A (TrueNode)           <- sibling
///          ├─ Subscribee_B (TrueNode)           <- sibling
///          └─ HiddenOutsideOfSubscribeeCol      <- self
///               └─ [hidden TrueNode children]
///
/// Collects nodes that the subscriber hides from its subscriptions
/// but that are NOT top-level content of any subscribee.
pub fn complete_hiddenoutsideofsubscribeecol (
  node               : NodeId,
  tree               : &mut Tree<ViewNode>,
  map                : &SkgNodeMap,
  source_diffs       : &Option<HashMap<SourceName, SourceDiff>>,
  config             : &SkgConfig,
  deleted_id_src_map : &HashMap<ID, SourceName>,
) -> Result<(), Box<dyn Error>> {
  { // verify node and parent are the right kinds
    error_unless_node_satisfies(
        tree, node,
        |vn : &ViewNode| matches!(
          &vn.kind,
          ViewNodeKind::Scaff(
            Scaffold::HiddenOutsideOfSubscribeeCol )),
        "complete_hiddenoutsideofsubscribeecol: \
         expected HiddenOutsideOfSubscribeeCol"
      ). map_err( |e| -> Box<dyn Error> { e.into() } ) ?;
    read_at_ancestor_in_tree(
        tree, node, 1,
        |vn : &ViewNode| match &vn.kind {
          ViewNodeKind::Scaff( Scaffold::SubscribeeCol ) => Ok(( )),
          _ => Err( "complete_hiddenoutsideofsubscribeecol: \
                     ancestor 1 is not a SubscribeeCol" ) } )
      . map_err( |e| -> Box<dyn Error> { e.into() } ) ?
      . map_err( |e| -> Box<dyn Error> { e.into() } ) ?; }
  let (subscriber_pid, subscriber_source) : (ID, SourceName) =
    pid_and_source_from_ancestor(
      tree, node, 2,
      "complete_hiddenoutsideofsubscribeecol" ) ?;
  let wt_subscriber_skgnode : &SkgNode =
    map.get( &subscriber_pid )
    .ok_or( "complete_hiddenoutsideofsubscribeecol: \
             subscriber SkgNode not in map" ) ?;
  let wt_subscriber_hides : Vec<ID> =
    wt_subscriber_skgnode.hides_from_its_subscriptions
      .clone().unwrap_or_default();
  let wt_subscribees : Vec<ID> =
    wt_subscriber_skgnode.subscribes_to
      .clone().unwrap_or_default();
  let wt_all_subscribee_content : HashSet<ID> =
    // everything contained by any subscribee
    wt_subscribees.iter()
      .filter_map( |pid| map.get( pid ) )
      .flat_map( |skg| skg.contains.clone().unwrap_or_default() )
      .collect();
  let wt_content : Vec<ID> =
    // what the subscriber hides that no subscribee contains
    wt_subscriber_hides.iter()
      .filter( |id| !wt_all_subscribee_content.contains( id ) )
      .cloned().collect();
  let (goal_list, removed_ids) : (Vec<ID>, HashSet<ID>) =
    match source_diffs {
      None => (wt_content.clone(),
               HashSet::new()),
      Some( _ ) => {
        let (head_subscriber_hides, head_subscribees)
          : (Vec<ID>, Vec<ID>)
          = skgnode_from_git_head(
                &subscriber_pid, &subscriber_source, config )
              .ok()
              .map( |skg| (skg.hides_from_its_subscriptions
                              .unwrap_or_default(),
                           skg.subscribes_to
                              .unwrap_or_default() ))
              .unwrap_or_default();
        let head_all_subscribee_content : HashSet<ID> =
          head_subscribee_contains_from_map_and_diffs(
            &head_subscribees, map, source_diffs );
        let head_content : Vec<ID> =
          head_subscriber_hides.iter()
            .filter( |id| !head_all_subscribee_content.contains( id ) )
            .cloned().collect();
        let diff : Vec<Diff_Item<ID>> =
          compute_interleaved_diff( &head_content, &wt_content );
        itemlist_and_removedset_from_diff( &diff ) } };
  let child_data : HashMap<ID, HiddenChildData> = // Pre-compute this, so that the create_child closure captures only owned data and does not conflict with the &mut Tree borrow in complete_relevant_children_in_viewnodetree.
    build_hidden_child_data(
      tree, node, &goal_list, &removed_ids,
      source_diffs, map, deleted_id_src_map, config ) ?;
  complete_relevant_children_in_viewnodetree(
    tree, node,
    |vn : &ViewNode| matches!( &vn.kind,
                               ViewNodeKind::True( t )
                               if !t.parent_ignores ),
    |vn : &ViewNode| match &vn.kind {
      ViewNodeKind::True( t ) => t.id.clone(),
      _ => panic!( "complete_hiddenoutsideofsubscribeecol: \
                    relevant child not TrueNode" ) },
    &goal_list,
    |id : &ID| {
      let d : &HiddenChildData =
        child_data.get( id ).expect(
          "complete_hiddenoutsideofsubscribeecol: \
           child data not pre-fetched" );
      match d.phantom {
        None => mk_indefinitive_viewnode(
          id.clone(), d.source.clone(),
          d.title.clone(), false ),
        Some( diff_status ) => mk_phantom_viewnode(
          id.clone(), d.source.clone(),
          d.title.clone(), diff_status ) }}, ) ?;
  { // Mark erroneous content children as parentIgnores. A child is erroneous if it is a non-parentIgnored TrueNode not in the goal_list and not a phantom.
    let goal_set : HashSet<ID> =
      goal_list.iter().cloned().collect();
    treat_certain_children(
      tree, node,
      |vn : &ViewNode| match &vn.kind {
        ViewNodeKind::True( t ) =>
          !t.parent_ignores
          && !goal_set.contains( &t.id )
          && !t.is_phantom(),
        _ => false },
      |vn : &mut ViewNode| {
        if let ViewNodeKind::True( ref mut t ) = vn.kind {
          t.parent_ignores = true; } },
    ).map_err( |e| -> Box<dyn Error> { e.into() } ) ?; }
  { // Remove if empty.
    let has_children : bool =
      tree . get(node) . ok_or(
        "complete_hiddenoutsideofsubscribeecol: node not found" )?
        . children() . next() . is_some();
    if !has_children {
      let has_focus : bool =
        subtree_satisfies( tree, node, &|n : &ViewNode| n.focused ) ?;
      if has_focus {
        write_at_ancestor_in_tree( tree, node, 1,
          |vn : &mut ViewNode| { vn.focused = true; } )
        .map_err( |e| -> Box<dyn Error> { e.into() } ) ?; }
      with_node_mut( tree, node, |mut n| { n.detach(); } )
        .map_err( |e| -> Box<dyn Error> { e.into() } ) ?; } }
  Ok(( )) }

/// From HEAD, compute the union of some subscribees' content.
/// For each subscribee pid:
/// - Look up its source from the map. If missing, skip.
/// - Get node_changes via node_changes_for_truenode. If present,
///     HEAD contains = Unchanged + Removed items from contains_diff.
///   If absent, HEAD = worktree, so use contains from the map.
fn head_subscribee_contains_from_map_and_diffs (
  head_subscribees : &[ID],
  map              : &SkgNodeMap,
  source_diffs     : &Option<HashMap<SourceName, SourceDiff>>,
) -> HashSet<ID> {
  let mut result : HashSet<ID> = HashSet::new();
  for pid in head_subscribees {
    let subscribee_source : SourceName =
      match map.get( pid ) {
        Some( skg ) => skg.source.clone(),
        None => continue };
    let nc : Option<&NodeChanges> =
      node_changes_for_truenode(
        source_diffs, pid, &subscribee_source );
    match nc {
      Some( nc ) => {
        for d in &nc.contains_diff {
          match d {
            Diff_Item::Unchanged( id ) |
              Diff_Item::Removed( id ) =>
                { result.insert( id.clone() ); },
            Diff_Item::New( _ ) => {} } } },
      None => {
        if let Some( skg ) = map.get( pid ) {
          for id in skg.contains.clone().unwrap_or_default() {
            result.insert( id ); } } } } }
  result }

/// Build a map from child ID to HiddenChildData.
/// For each ID in goal_list, pre-compute the source, title, and
/// phantom status so the create_child closure argument for
/// complete_relevant_children_in_viewnodetree
/// captures only owned data.
fn build_hidden_child_data (
  tree               : &Tree<ViewNode>,
  node               : NodeId,
  goal_list          : &[ID],
  removed_ids        : &HashSet<ID>,
  source_diffs       : &Option<HashMap<SourceName, SourceDiff>>,
  map                : &SkgNodeMap,
  deleted_id_src_map : &HashMap<ID, SourceName>,
  config             : &SkgConfig,
) -> Result<HashMap<ID, HiddenChildData>, Box<dyn Error>> {
  let existing_children : HashMap<ID, (SourceName, String)> =
    { let node_ref : ego_tree::NodeRef<ViewNode> =
        tree.get( node )
          .ok_or( "build_hidden_child_data: node not found" ) ?;
      let mut m : HashMap<ID, (SourceName, String)> = HashMap::new();
      for child_ref in node_ref.children() {
        if let ViewNodeKind::True( t ) = &child_ref.value().kind {
          m.insert( t.id.clone(),
                    ( t.source.clone(), t.title.clone() )); } }
      m };
  let child_sources : HashMap<ID, SourceName> =
    existing_children.iter()
      .map( |(id, (s, _))| (id.clone(), s.clone()) )
      .collect();
  let mut result : HashMap<ID, HiddenChildData> = HashMap::new();
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
                     HiddenChildData { source: child_src,
                                       title: child_title,
                                       phantom: Some( phantom ) } );
    } else {
      if let Some( (s, t) ) = existing_children.get( child_skgid ) {
        result.insert( child_skgid.clone(),
                       HiddenChildData { source: s.clone(),
                                         title: t.clone(),
                                         phantom: None } );
      } else if let Some( skg ) = map.get( child_skgid ) {
        result.insert( child_skgid.clone(),
                       HiddenChildData { source: skg.source.clone(),
                                         title: skg.title.clone(),
                                         phantom: None } );
      } else {
        return Err( format!(
          "build_hidden_child_data: \
           child {} not found in children or map",
          child_skgid.0 ).into() ); } } }
  Ok( result ) }
