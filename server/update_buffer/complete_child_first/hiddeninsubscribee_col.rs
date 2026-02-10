use crate::git_ops::read_repo::skgnode_from_git_head;
use crate::types::viewnode::mk_phantom_viewnode;
use crate::types::git::{SourceDiff, NodeDiffStatus, NodeChanges, node_changes_for_truenode};
use crate::types::list::{compute_interleaved_diff, itemlist_and_removedset_from_diff, Diff_Item};
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::skgnode::SkgNode;
use crate::types::skgnodemap::SkgNodeMap;
use crate::types::tree::generic::{error_unless_node_satisfies, read_at_ancestor_in_tree, write_at_ancestor_in_tree, with_node_mut};
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

/// HiddenInSubscribeeCol completion (child-first / postorder pass).
///
/// Tree structure:
///   Subscriber (TrueNode)            <- ancestor 3
///     └─ SubscribeeCol (Scaffold)    <- ancestor 2
///          └─ Subscribee (TrueNode)  <- ancestor 1
///               └─ HiddenInSubscribeeCol (Scaffold) <- self
///                    └─ [hidden TrueNode children]
///
/// The HiddenInSubscribeeCol collects nodes that the subscriber
/// hides from its subscriptions AND that are top-level content
/// of the subscribee.
pub fn complete_hiddeninsubscribee_col (
  node         : NodeId,
  tree         : &mut Tree<ViewNode>,
  map          : &SkgNodeMap,
  source_diffs : &Option<HashMap<SourceName, SourceDiff>>,
  config       : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  error_unless_node_satisfies(
      tree, node,
      |vn : &ViewNode| matches!( &vn.kind,
                                 ViewNodeKind::Scaff(
                                   Scaffold::HiddenInSubscribeeCol )),
      "complete_hiddeninsubscribee_col: expected HiddenInSubscribeeCol"
    ). map_err( |e| -> Box<dyn Error> { e.into() } ) ?;
  let (subscribee_pid, subscribee_source) : (ID, SourceName) =
    read_at_ancestor_in_tree(
      tree, node, 1,
      |vn : &ViewNode| match &vn.kind {
        ViewNodeKind::True( t ) =>
          Ok(( t.id.clone(), t.source.clone() )),
        _ => Err( "complete_hiddeninsubscribee_col: \
                   ancestor 1 is not a TrueNode" ) } )
    . map_err( |e| -> Box<dyn Error> { e.into() } ) ?
    . map_err( |e| -> Box<dyn Error> { e.into() } ) ?;
  let (subscriber_pid, subscriber_source) : (ID, SourceName) =
    read_at_ancestor_in_tree(
      tree, node, 3,
      |vn : &ViewNode| match &vn.kind {
        ViewNodeKind::True( t ) =>
          Ok(( t.id.clone(), t.source.clone() )),
        _ => Err( "complete_hiddeninsubscribee_col: \
                   ancestor 3 is not a TrueNode" ) } )
    . map_err( |e| -> Box<dyn Error> { e.into() } ) ?
    . map_err( |e| -> Box<dyn Error> { e.into() } ) ?;
  let subscribee_contains : Vec<ID> = {
    let subscribee_skgnode : &SkgNode =
      map.get( &subscribee_pid )
      .ok_or( "complete_hiddeninsubscribee_col: \
               subscribee SkgNode not in map" ) ?;
    subscribee_skgnode.contains.clone().unwrap_or_default() };
  let subscriber_hides : Vec<ID> = {
    let subscriber_skgnode : &SkgNode =
      map.get( &subscriber_pid )
      .ok_or( "complete_hiddeninsubscribee_col: \
               subscriber SkgNode not in map" ) ?;
    subscriber_skgnode.hides_from_its_subscriptions
      . clone() . unwrap_or_default() };
  let worktree_content : Vec<ID> =
    { // Intersection of subscriber_hides and subscribee_contains,
      // preserving order from subscriber_hides.
      let subscribee_contains_set : HashSet<ID> =
        subscribee_contains.iter().cloned().collect();
      subscriber_hides.iter()
        .filter( |id| subscribee_contains_set.contains( id ) )
        .cloned().collect() };
  let (goal_list, removed_ids) : (Vec<ID>, HashSet<ID>) =
    match source_diffs {
      None => (worktree_content.clone(),
               HashSet::new()),
      Some( _ ) => {
        let subscribee_node_changes : Option<&NodeChanges>
          = node_changes_for_truenode(
              source_diffs, &subscribee_pid, &subscribee_source );
        let head_subscribee_contains : Vec<ID> =
          match subscribee_node_changes {
            Some( nc ) =>
              nc.contains_diff.iter().filter_map(
                |d| match d {
                  Diff_Item::Unchanged( id ) |
                    Diff_Item::Removed( id ) => Some( id.clone() ),
                  Diff_Item::New( _ ) => None } )
              .collect(),
            None => subscribee_contains.clone() };
        let head_subscriber_hides : Vec<ID> =
          skgnode_from_git_head(
              &subscriber_pid, &subscriber_source, config )
            .ok()
            .and_then( |skg| skg.hides_from_its_subscriptions )
            .unwrap_or_default();
        let old_list : Vec<ID> =
          { let head_subscribee_contains_set : HashSet<ID> =
              head_subscribee_contains.iter().cloned().collect();
            head_subscriber_hides.iter() .filter(
                |id| head_subscribee_contains_set.contains( id )
              ). cloned() . collect() };
        let diff : Vec<Diff_Item<ID>> =
          compute_interleaved_diff( &old_list, &worktree_content );
        itemlist_and_removedset_from_diff( &diff ) } };
  let child_data : HashMap<ID, HiddenChildData> = // Pre-compute this, so that the create_child closure argument to complete_relevant_children_in_viewnodetree captures only owned data and does not conflict with the &mut Tree borrow in complete_relevant_children_in_viewnodetree.
    build_hidden_child_data(
      tree, node, &goal_list, &removed_ids,
      source_diffs, map ) ?;
  complete_relevant_children_in_viewnodetree(
    tree, node,
    |vn : &ViewNode| matches!( &vn.kind,
                               ViewNodeKind::True( t )
                               if !t.parent_ignores ),
    |vn : &ViewNode| match &vn.kind {
      ViewNodeKind::True( t ) => t.id.clone(),
      _ => panic!( "complete_hiddeninsubscribee_col: \
                    relevant child not TrueNode" ) },
    &goal_list,
    |id : &ID| {
      let d : &HiddenChildData =
        child_data.get( id ).expect(
          "complete_hiddeninsubscribee_col: \
           child data not pre-fetched" );
      match d.phantom {
        None => mk_indefinitive_viewnode(
          id.clone(), d.source.clone(),
          d.title.clone(), false ),
        Some( diff_status ) => mk_phantom_viewnode(
          id.clone(), d.source.clone(),
          d.title.clone(), diff_status ) }}, ) ?;
  { // Mark erroneous content children as parentIgnores. Non-parentIgnored TrueNode children that are not in subscribee_contains and not phantoms are erroneous.
    // TODO: I'm not sure this is right, in the git diff view case.
    let subscribee_contains_set : HashSet<ID> =
      subscribee_contains.iter().cloned().collect();
    treat_certain_children(
      tree, node,
      |vn : &ViewNode| match &vn.kind {
        ViewNodeKind::True( t ) =>
          !t.parent_ignores
          && !subscribee_contains_set.contains( &t.id )
          && !t.is_phantom(),
        _ => false },
      |vn : &mut ViewNode| {
        if let ViewNodeKind::True( ref mut t ) = vn.kind {
          t.parent_ignores = true; } },
    ).map_err( |e| -> Box<dyn Error> { e.into() } ) ?; }
  { // Remove if empty.
    let has_children : bool =
      tree.get( node )
        .ok_or( "complete_hiddeninsubscribee_col: node not found" ) ?
        .children().next().is_some();
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

/// Build a map from child ID to HiddenChildData.
/// For each ID in goal_list, pre-compute the source, title, and
/// phantom status so the create_child closure captures only owned data.
fn build_hidden_child_data (
  tree         : &Tree<ViewNode>,
  node         : NodeId,
  goal_list    : &[ID],
  removed_ids  : &HashSet<ID>,
  source_diffs : &Option<HashMap<SourceName, SourceDiff>>,
  map          : &SkgNodeMap,
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
  let mut result : HashMap<ID, HiddenChildData> = HashMap::new();
  for child_skgid in goal_list {
    if result.contains_key( child_skgid ) { continue; }
    if removed_ids.contains( child_skgid ) {
      let (child_src, child_title) : (SourceName, String) =
        if let Some( (s, t) ) = existing_children.get( child_skgid ) {
          (s.clone(), t.clone())
        } else if let Some( skgnode ) = map.get( child_skgid ) {
          (skgnode.source.clone(), skgnode.title.clone())
        } else if let Some( (s, t) )
            = source_diffs.as_ref().and_then(
                |diffs| diffs.iter().find_map(
                  |(src, sd)| sd.deleted_nodes.get( child_skgid )
                    .map( |skg| (src.clone(),
                                 skg.title.clone() )) )) {
            (s, t)
        } else {
          return Err( format!(
            "build_hidden_child_data: \
             removed child {} not found in children, map, or deleted_nodes",
            child_skgid.0 ).into() ); };
      let phantom : NodeDiffStatus =
        if source_diffs.as_ref()
          .and_then( |diffs| diffs.get( &child_src ) )
          .map( |sd| sd.deleted_nodes.contains_key( child_skgid ) )
          .unwrap_or( false )
        { NodeDiffStatus::Removed } else { NodeDiffStatus::RemovedHere };
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
