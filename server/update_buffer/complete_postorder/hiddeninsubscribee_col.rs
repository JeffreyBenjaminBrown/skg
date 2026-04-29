use crate::git_ops::read_repo::nodecomplete_from_git_head;
use crate::to_org::complete::sharing::child_data::{ChildData, build_child_data};
use crate::types::viewnode::mk_phantom_viewnode;
use crate::types::git::{SourceDiff, NodeChanges, net_diff_from_per_stage, per_stage_node_changes_for_truenode};
use crate::types::list::{compute_interleaved_diff, itemlist_and_removedset_from_diff, Diff_Item};
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::memory::nodecomplete_from_memory_or_disk;
use crate::types::nodes::complete::NodeComplete;
use crate::types::tree::generic::{error_unless_node_satisfies, pid_and_source_from_ancestor, write_at_ancestor_in_tree, with_node_mut};
use crate::types::viewnode::{ViewNode, ViewNodeKind, Scaffold, Birth, mk_indefinitive_viewnode};
use crate::update_buffer::util::{complete_relevant_children_in_viewnodetree, treat_certain_children, subtree_satisfies};

use ego_tree::{NodeId, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;

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
  node               : NodeId,
  tree               : &mut Tree<ViewNode>,
  source_diffs       : &Option<HashMap<SourceName, SourceDiff>>,
  config             : &SkgConfig,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
) -> Result<(), Box<dyn Error>> {
  error_unless_node_satisfies(
      tree, node,
      |vn : &ViewNode| matches!( &vn . kind,
                                 ViewNodeKind::Scaff(
                                   Scaffold::HiddenInSubscribeeCol )),
      "complete_hiddeninsubscribee_col: expected HiddenInSubscribeeCol"
    ) . map_err( |e| -> Box<dyn Error> { e . into() } ) ?;
  let (subscribee_pid, subscribee_source) : (ID, SourceName) =
    pid_and_source_from_ancestor(
      tree, node, 1,
      "complete_hiddeninsubscribee_col" ) ?;
  let (subscriber_pid, subscriber_source) : (ID, SourceName) =
    pid_and_source_from_ancestor(
      tree, node, 3,
      "complete_hiddeninsubscribee_col" ) ?;
  let subscribee_contains : Vec<ID> = {
    let subscribee_nodecomplete : NodeComplete =
      nodecomplete_from_memory_or_disk (
        config, &subscribee_pid, &subscribee_source ) ?;
    subscribee_nodecomplete . contains . clone() };
  let subscriber_hides : Vec<ID> = {
    let subscriber_nodecomplete : NodeComplete =
      nodecomplete_from_memory_or_disk (
        config, &subscriber_pid, &subscriber_source ) ?;
    subscriber_nodecomplete . hides_from_its_subscriptions
      . or_default() . to_vec() };
  let worktree_content : Vec<ID> =
    { // Intersection of subscriber_hides and subscribee_contains,
      // preserving order from subscriber_hides.
      let subscribee_contains_set : HashSet<ID> =
        subscribee_contains . iter() . cloned() . collect();
      subscriber_hides . iter()
        . filter( |id| subscribee_contains_set . contains (id) )
        . cloned() . collect() };
  let (goal_list, removed_ids) : (Vec<ID>, HashSet<ID>) =
    match source_diffs {
      None => ( worktree_content . clone(),
                HashSet::new() ),
      Some (_) =>
        goallist_and_removedids_for_hiddeninsubscribeecol_with_diff(
          source_diffs, &subscribee_pid, &subscribee_source,
          &subscriber_pid, &subscriber_source,
          &subscribee_contains, &worktree_content, config ) };
  let child_data : HashMap<ID, ChildData> = // Pre-compute this, so that the create_child closure argument to complete_relevant_children_in_viewnodetree captures only owned data and does not conflict with the &mut Tree borrow in complete_relevant_children_in_viewnodetree.
    build_child_data(
      tree, node, &subscribee_pid, &subscribee_source,
      &goal_list, &removed_ids,
      source_diffs, deleted_since_head_pid_src_map, config ) ?;
  complete_relevant_children_in_viewnodetree(
    tree, node,
    |vn : &ViewNode| matches!( &vn . kind,
                               ViewNodeKind::True (t)
                               if !t . parent_ignores_it() ),
    |vn : &ViewNode| match &vn . kind {
      ViewNodeKind::True (t) => t . id . clone(),
      _ => panic!( "complete_hiddeninsubscribee_col: \
                    relevant child not TrueNode" ) },
    &goal_list,
    |id : &ID| {
      let d : &ChildData =
        child_data . get (id) . expect(
          "complete_hiddeninsubscribee_col: \
           child data not pre-fetched" );
      match d . phantom {
        None => mk_indefinitive_viewnode(
          id . clone(), d . source . clone(),
          d . title . clone(), Birth::ContentOf ),
        Some ((ex, mem)) => mk_phantom_viewnode(
          id . clone(), d . source . clone(),
          d . title . clone(), ex, mem ) }}, ) ?;
  { // Change birth of erroneous content children to Independent. "Erroneous content" are TrueNode children marked birth=ContentOf that are not part of its content.
    let subscribee_contains_set : HashSet<ID> =
      subscribee_contains . iter() . cloned() . collect();
    treat_certain_children(
      tree, node,
      |vn : &ViewNode| match &vn . kind {
        ViewNodeKind::True (t) =>
          ! t . parent_ignores_it()
          && ! subscribee_contains_set . contains( &t . id )
          && ! t . is_phantom(),
        _ => false },
      |vn : &mut ViewNode| {
        if let ViewNodeKind::True( ref mut t ) = vn . kind {
          t . birth = Birth::Independent; } },
    ) . map_err( |e| -> Box<dyn Error> { e . into() } ) ?; }
  { // Remove if empty.
    let has_children : bool =
      tree . get (node)
        . ok_or ("complete_hiddeninsubscribee_col: node not found") ?
        . children() . next() . is_some();
    if !has_children {
      let has_focus : bool =
        subtree_satisfies( tree, node, &|n : &ViewNode| n . focused ) ?;
      if has_focus {
        write_at_ancestor_in_tree( tree, node, 1,
          |vn : &mut ViewNode| { vn . focused = true; } )
        . map_err( |e| -> Box<dyn Error> { e . into() } ) ?; }
      with_node_mut( tree, node, |mut n| { n . detach(); } )
        . map_err( |e| -> Box<dyn Error> { e . into() } ) ?; } }
  Ok(( )) }

/// Reconstruct the HEAD version of hidden-in-subscribee content,
/// diff it against the worktree version,
/// and return the goal list and removed set.
///
/// Uses per-stage contains_diff (via 'net_diff_from_per_stage')
/// rather than the merged 'node_changes_for_truenode' view, so that
/// staged-only contains-list changes still produce phantoms.
fn goallist_and_removedids_for_hiddeninsubscribeecol_with_diff (
  source_diffs        : &Option<HashMap<SourceName, SourceDiff>>,
  subscribee_pid      : &ID,
  subscribee_source   : &SourceName,
  subscriber_pid      : &ID,
  subscriber_source   : &SourceName,
  subscribee_contains : &[ID],
  worktree_content    : &[ID],
  config              : &SkgConfig,
) -> (Vec<ID>, HashSet<ID>) {
  let (staged_nc, unstaged_nc)
    : (Option<&NodeChanges>, Option<&NodeChanges>) =
    per_stage_node_changes_for_truenode (
      source_diffs, subscribee_pid, subscribee_source );
  let head_subscribee_contains : Vec<ID> =
    if staged_nc . is_none () && unstaged_nc . is_none () {
      subscribee_contains . to_vec ()
    } else {
      let net : Vec<Diff_Item<ID>> = net_diff_from_per_stage (
        staged_nc   . map ( |c| c . contains_diff . as_slice () ),
        unstaged_nc . map ( |c| c . contains_diff . as_slice () ));
      net . iter () . filter_map (
          |d| match d { // Items that were in HEAD: Unchanged or Removed.
            Diff_Item::Unchanged (id) |
              Diff_Item::Removed (id) => Some ( id . clone () ),
            Diff_Item::New (_) => None } )
        . collect () };
  let head_subscriber_hides : Vec<ID> =
    nodecomplete_from_git_head(
        subscriber_pid, subscriber_source, config )
      . ok()
      . map( |skg| skg . hides_from_its_subscriptions . into_vec() )
      . unwrap_or_default();
  let old_list : Vec<ID> =
    { let head_subscribee_contains_set : HashSet<ID> =
        head_subscribee_contains . iter() . cloned() . collect();
      head_subscriber_hides . iter() . filter(
          |id| head_subscribee_contains_set . contains (id)
        ) . cloned() . collect() };
  let diff : Vec<Diff_Item<ID>> =
    compute_interleaved_diff( &old_list, worktree_content );
  itemlist_and_removedset_from_diff (&diff) }

