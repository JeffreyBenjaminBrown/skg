use crate::git_ops::read_repo::nodecomplete_from_git_head;
use crate::to_org::complete::sharing::child_data::{ChildData, build_child_data};
use crate::types::viewnode::mk_phantom_viewnode;
use crate::types::git::{SourceDiff, NodeChanges, net_diff_from_per_stage, per_stage_node_changes_for_truenode};
use crate::types::list::{compute_interleaved_diff, itemlist_and_removedset_from_diff, Diff_Item};
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::memory::nodecomplete_from_memory_or_disk;
use crate::types::nodes::complete::NodeComplete;
use crate::types::tree::generic::{error_unless_node_satisfies, pid_and_source_from_ancestor, read_at_ancestor_in_tree, write_at_ancestor_in_tree, with_node_mut};
use crate::types::viewnode::{ViewNode, ViewNodeKind, Scaffold, Birth, mk_indefinitive_viewnode};
use crate::update_buffer::util::{complete_relevant_children_in_viewnodetree, treat_certain_children, subtree_satisfies};

use ego_tree::{NodeId, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;

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
  source_diffs       : &Option<HashMap<SourceName, SourceDiff>>,
  config             : &SkgConfig,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
) -> Result<(), Box<dyn Error>> {
  { // verify node and parent are the right kinds
    error_unless_node_satisfies(
        tree, node,
        |vn : &ViewNode| matches!(
          &vn . kind,
          ViewNodeKind::Scaff(
            Scaffold::HiddenOutsideOfSubscribeeCol )),
        "complete_hiddenoutsideofsubscribeecol: \
         expected HiddenOutsideOfSubscribeeCol"
      ) . map_err( |e| -> Box<dyn Error> { e . into() } ) ?;
    read_at_ancestor_in_tree(
        tree, node, 1,
        |vn : &ViewNode| match &vn . kind {
          ViewNodeKind::Scaff (Scaffold::SubscribeeCol) => Ok(( )),
          _ => Err( "complete_hiddenoutsideofsubscribeecol: \
                     ancestor 1 is not a SubscribeeCol" ) } )
      . map_err( |e| -> Box<dyn Error> { e . into() } ) ?
      . map_err( |e| -> Box<dyn Error> { e . into() } ) ?; }
  let (subscriber_pid, subscriber_source) : (ID, SourceName) =
    pid_and_source_from_ancestor(
      tree, node, 2,
      "complete_hiddenoutsideofsubscribeecol" ) ?;
  let wt_subscriber_nodecomplete : NodeComplete =
    nodecomplete_from_memory_or_disk (
      config, &subscriber_pid, &subscriber_source ) ?;
  let wt_subscriber_hides : Vec<ID> =
    wt_subscriber_nodecomplete . hides_from_its_subscriptions
      . or_default() . to_vec();
  let wt_subscribees : Vec<ID> =
    wt_subscriber_nodecomplete . subscribes_to
      . or_default() . to_vec();
  let wt_all_subscribee_content : HashSet<ID> =
    // everything contained by any subscribee
    wt_subscribees . iter()
      . flat_map ( |pid| {
        match snapshot_global_source (pid, config) {
          Some (src) =>
            nodecomplete_from_memory_or_disk ( config, pid, &src )
              . ok ()
              . map ( |skg| skg . contains )
              . unwrap_or_default (),
          None => Vec::new () } } )
      . collect();
  let wt_content : Vec<ID> =
    // what the subscriber hides that no subscribee contains
    wt_subscriber_hides . iter()
      . filter( |id| !wt_all_subscribee_content . contains (id) )
      . cloned() . collect();
  let (goal_list, removed_ids) : (Vec<ID>, HashSet<ID>) =
    match source_diffs {
      None => (wt_content . clone(),
               HashSet::new()),
      Some (_) => {
        let (head_subscriber_hides, head_subscribees)
          : (Vec<ID>, Vec<ID>)
          = nodecomplete_from_git_head(
                &subscriber_pid, &subscriber_source, config )
              . ok()
              . map( |skg| (skg . hides_from_its_subscriptions
                              . into_vec(),
                           skg . subscribes_to
                              . into_vec() ))
              . unwrap_or_default();
        let head_all_subscribee_content : HashSet<ID> =
          head_subscribee_contains_from_memory_and_diffs(
            &head_subscribees, source_diffs, config );
        let head_content : Vec<ID> =
          head_subscriber_hides . iter()
            . filter( |id| !head_all_subscribee_content . contains (id) )
            . cloned() . collect();
        let diff : Vec<Diff_Item<ID>> =
          compute_interleaved_diff( &head_content, &wt_content );
        itemlist_and_removedset_from_diff (&diff) } };
  let child_data : HashMap<ID, ChildData> = // Pre-compute this, so that the create_child closure captures only owned data and does not conflict with the &mut Tree borrow in complete_relevant_children_in_viewnodetree.
    build_child_data(
      tree, node, &subscriber_pid, &subscriber_source,
      &goal_list, &removed_ids,
      source_diffs, deleted_since_head_pid_src_map, config ) ?;
  complete_relevant_children_in_viewnodetree(
    tree, node,
    |vn : &ViewNode| matches!( &vn . kind,
                               ViewNodeKind::True (t)
                               if !t . parent_ignores_it() ),
    |vn : &ViewNode| match &vn . kind {
      ViewNodeKind::True (t) => t . id . clone(),
      _ => panic!( "complete_hiddenoutsideofsubscribeecol: \
                    relevant child not TrueNode" ) },
    &goal_list,
    |id : &ID| {
      let d : &ChildData =
        child_data . get (id) . expect(
          "complete_hiddenoutsideofsubscribeecol: \
           child data not pre-fetched" );
      match d . phantom {
        None => mk_indefinitive_viewnode(
          id . clone(), d . source . clone(),
          d . title . clone(), Birth::ContentOf ),
        Some ((ex, mem)) => mk_phantom_viewnode(
          id . clone(), d . source . clone(),
          d . title . clone(), ex, mem ) }}, ) ?;
  { // Mark erroneous content children birth=Independent. A child is erroneous if it is a non-phantom TrueNode marked birth=Content but not in the goal_list.
    let goal_set : HashSet<ID> =
      goal_list . iter() . cloned() . collect();
    treat_certain_children(
      tree, node,
      |vn : &ViewNode| match &vn . kind {
        ViewNodeKind::True (t) =>
          !t . parent_ignores_it()
          && !goal_set . contains( &t . id )
          && !t . is_phantom(),
        _ => false },
      |vn : &mut ViewNode| {
        if let ViewNodeKind::True( ref mut t ) = vn . kind {
          t . birth = Birth::Independent; } },
    ) . map_err( |e| -> Box<dyn Error> { e . into() } ) ?; }
  { // Remove if empty.
    let has_children : bool =
      tree . get (node) . ok_or(
        "complete_hiddenoutsideofsubscribeecol: node not found" )?
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

/// From HEAD, compute the union of some subscribees' content.
/// For each subscribee pid:
/// - Look up its source from memory. If missing, skip.
/// - Read per-stage contains_diff via
///   'per_stage_node_changes_for_truenode' and compose the net
///   HEAD→worktree view. Items that were in HEAD are Unchanged +
///   Removed in the net diff. If neither stage has changes for
///   this file, HEAD = worktree, so read contains from memory/disk.
fn head_subscribee_contains_from_memory_and_diffs (
  head_subscribees : &[ID],
  source_diffs     : &Option<HashMap<SourceName, SourceDiff>>,
  config           : &SkgConfig,
) -> HashSet<ID> {
  let mut result : HashSet<ID> = HashSet::new();
  for pid in head_subscribees {
    let subscribee_source : SourceName =
      match snapshot_global_source (pid, config) {
        Some (s) => s,
        None => continue };
    let (staged_nc, unstaged_nc)
      : (Option<&NodeChanges>, Option<&NodeChanges>) =
      per_stage_node_changes_for_truenode (
        source_diffs, pid, &subscribee_source );
    if staged_nc . is_none () && unstaged_nc . is_none () {
      if let Ok (skg) = nodecomplete_from_memory_or_disk (
        config, pid, &subscribee_source )
      { for id in skg . contains . into_iter () {
          result . insert (id); } }
    } else {
      let net : Vec<Diff_Item<ID>> = net_diff_from_per_stage (
        staged_nc   . map ( |c| c . contains_diff . as_slice () ),
        unstaged_nc . map ( |c| c . contains_diff . as_slice () ));
      for d in &net {
        match d {
          Diff_Item::Unchanged (id) |
            Diff_Item::Removed (id) =>
              { result . insert ( id . clone () ); },
          Diff_Item::New (_) => {} } } } }
  result }

/// Resolve a node's source: try the in-Rust memory snapshot first, fall back
/// to scanning source directories for the matching '.skg' file. Tests that
/// bypass 'init_global_handle_for_first_time_or_panic' rely on the disk fallback.
fn snapshot_global_source (
  pid    : &ID,
  config : &SkgConfig,
) -> Option<SourceName> {
  if let Some (s) =
    crate::dbs::memory::snapshot_global ()
      . as_deref ()
      . and_then ( |g| g . pid_and_source (pid) . map ( |(_, s)| s ) )
  { return Some (s); }
  crate::types::phantom::source_from_disk (pid, config) }

