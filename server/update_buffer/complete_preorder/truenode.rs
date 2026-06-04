use crate::to_org::complete::contents::clobberIndefinitiveViewnode;
use crate::source_sets::ActiveSourceSet;
use crate::types::viewnode::{mk_inactive_viewnode, mk_phantom_viewnode};
use crate::to_org::util::{DefinitiveMap, make_indef_if_repeat_then_extend_defmap};
use crate::types::git::{ExistenceAxes, MembershipAxes, Sign, SourceDiff, NodeChanges, net_diff_from_per_stage, per_stage_node_changes_for_truenode};
use crate::types::list::{Diff_Item, compute_interleaved_diff, itemlist_and_removedset_from_diff};
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::phantom::{title_for_phantom, phantom_axes};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::env::find_source_with_optional_tantivy;
#[cfg(test)]
use crate::types::git::{GitDiffStatus, NodeCompleteDiff};
use crate::types::nodes::complete::NodeComplete;
use crate::git_ops::read_repo::nodecomplete_from_git_head;
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_source;
use crate::util::setlike_vector_subtraction;
use crate::types::viewnode::{
    ViewNode, ViewNodeKind, DeletedNode, IndefOrDef,
    ParentIs, ViewRequest, mk_definitive_viewnode};
use crate::types::viewnode::{Vognode, QualCol, Qual, RoleCol};
use crate::types::tree::generic::{error_unless_node_satisfies, pid_and_source_from_ancestor, read_at_ancestor_in_tree, read_at_node_in_tree, write_at_node_in_tree};
use crate::types::tree::viewnode_nodecomplete::{
    pid_and_source_from_treenode,
    write_at_truenode_in_tree,
    unique_scaffold_child_of_viewnode,
    insert_scaffold_as_child};
use crate::update_buffer::util::{
    complete_relevant_children_in_viewnodetree,
    partition_children,
    treat_certain_children,
    move_child_to_end};

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::path::PathBuf;
use std::sync::Arc;

enum ContentReality {
  Real, // Real content: Exists in worktree, and parent contains it at this position.
  Phantom (ExistenceAxes, MembershipAxes), // Is not contained by parent at this position, and might not exist at all.
  Inactive,
}

struct ChildData {
  title  : String,
  source : SourceName,
  body   : Option<String>,
  kind   : ContentReality,
}

#[derive(Clone, Copy)]
struct CompletionMode {
  saved_view : bool, // false => collateral buffer
}

impl CompletionMode {
  fn new (
    is_saved_view : bool,
  ) -> CompletionMode {
    CompletionMode {
      saved_view : is_saved_view,
    }}

  /// The saved (definitive) view of a node
  /// *defines* the title, body, and source.
  /// Collateral views, though, need those fields updated.
  fn should_sync_from_disk_even_though_definitive (
    self,
  ) -> bool {
    ! self . saved_view }

}

/// TrueNode content reconcile + content-child creation, for one node, in the
/// plan_v2 §3 level-order BFS visit. The driver settles the node's
/// Finalizable state *before* calling this (via 'apply_definitive_draw_rule')
/// and passes:
/// - `settled`: the §5.2 draw rule already ran for this node (it carried a
///   ViewRequest::Definitive), so its map entry and indef/def are already
///   correct -- skip 'make_indef_if_repeat_then_extend_defmap', which would
///   otherwise indefinitize a just-made-Final node against its own entry.
/// - `cascade`: this node is Final (DVR-made); per §5.3 it hands a
///   ViewRequest::Definitive to each of its affected content children so the
///   BFS draws each Final (clobbering competing Tentative occurrences).
/// - `node_budget`: the §5.5 remaining budget of new ViewNodes; content-child
///   creation is capped against it.
pub fn expand_true_content_at_truenode (
  node               : NodeId,
  tree               : &mut Tree<ViewNode>,
  defmap             : &mut DefinitiveMap,
  source_diffs       : &Option<HashMap<SourceName, SourceDiff>>,
  config             : &SkgConfig,
  graph_snap                     : &Arc<InRustGraph>,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  deleted_by_this_save_pids      : &HashSet<ID>,
  active_source_set              : Option<&ActiveSourceSet>,
  is_saved_view                  : bool,
  settled                        : bool,
  cascade                        : bool,
  node_budget                    : &mut usize,
) -> Result<(), Box<dyn Error>> {
  // Phantoms are diff placeholders; this pass mutates and expands
  // real content nodes.
  error_unless_node_satisfies(
    tree, node,
    |vn : &ViewNode| matches!( &vn . kind,
                                ViewNodeKind::Vognode (Vognode::Normal (_))),
    "expand_true_content_at_truenode: expected normal vognode" ) ?;
  if ! settled {
    // A DVR node was already resolved by apply_definitive_draw_rule; running
    // the dedup here would indefinitize it against its own (just-inserted)
    // map entry. Ordinary nodes still dedup first-wins (Tentative).
    make_indef_if_repeat_then_extend_defmap(
      tree, node, defmap ) ?; }
  let (pid, initial_source) : (ID, SourceName) =
    pid_and_source_from_treenode( tree, node,
                                  "expand_true_content_at_truenode" ) ?;
  set_diff_status (
    // Must precede the clobber-and-early-return done to indefinitive nodes.
    tree, node, &pid, source_diffs, &initial_source) ?;
  { let is_indefinitive : bool =
      read_at_node_in_tree( tree, node,
        |vn : &ViewNode| match &vn . kind {
          ViewNodeKind::Vognode (Vognode::Normal (t))
            => t . is_indefinitive (),
          _ => false } ) ?;
    if is_indefinitive {
      clobberIndefinitiveViewnode( tree, node, config ) ?;
      return Ok (( )); }}
  if deleted_by_this_save_pids . contains (&pid) {
    mutate_truenode_to_deletednode (
      tree, node, &pid, &initial_source ) ?;
    return Ok (( )); }
  clear_edit_request (tree, node) ?;
  let nodecomplete : NodeComplete =
    nodecomplete_rustFirst_by_pid_and_source (
      config, &pid, &initial_source ) ?;
  let source : SourceName =
    nodecomplete . source . clone ();
  let mode : CompletionMode =
    CompletionMode::new (is_saved_view);
  if mode . should_sync_from_disk_even_though_definitive () {
    sync_truenode_from_disk (tree, node, &nodecomplete) ?; }
  reconcile_content_children (
    tree, node, &nodecomplete, &pid, &source,
    source_diffs, config, graph_snap,
    deleted_since_head_pid_src_map,
    active_source_set, node_budget ) ?;
  if cascade {
    attach_cascade_dvrs_to_affected_content( tree, node ) ?; }
  order_children_as_scaffolds_then_ignored_then_content(
    tree, node ) ?;
  Ok(( )) }

/// §5.3 cascade: hand a ViewRequest::Definitive to each affected,
/// non-phantom Normal content child of a Final node -- new and existing
/// alike -- so the main BFS draws each Final (and it in turn cascades to its
/// own content). The cascade does *not* flow through scaffolds, so only
/// parentIs=Affected content children receive it.
fn attach_cascade_dvrs_to_affected_content (
  tree : &mut Tree<ViewNode>,
  node : NodeId,
) -> Result<(), Box<dyn Error>> {
  let child_ids : Vec<NodeId> =
    tree . get (node) . unwrap ()
    . children ()
    . filter ( |c| matches!( &c . value () . kind,
        ViewNodeKind::Vognode (Vognode::Normal (t))
          if t . parentIs == ParentIs::Affected
             && ! t . should_be_phantom () ) )
    . map ( |c| c . id () )
    . collect ();
  for cid in child_ids {
    write_at_truenode_in_tree (
      tree, cid,
      |t| { t . view_requests . insert ( ViewRequest::Definitive ); } )
      . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?; }
  Ok (( )) }

/// The edit request should have been used by now.
fn clear_edit_request (
  tree : &mut Tree<ViewNode>,
  node : NodeId,
) -> Result<(), Box<dyn Error>> {
  write_at_truenode_in_tree (
    tree, node,
    |t| { if let IndefOrDef::Definitive { edit_request, .. }
          = &mut t . indef_or_def
          { *edit_request = None; }} ) ?;
  Ok (( )) }

/// Phase 3 (non-saved views only): overwrite the viewnode's title
/// and body with the fresh values from disk. The saved view is the
/// one that *defines* those fields, so it is excluded.
fn sync_truenode_from_disk (
  tree         : &mut Tree<ViewNode>,
  node         : NodeId,
  nodecomplete : &NodeComplete,
) -> Result<(), Box<dyn Error>> {
  let disk_title : String = nodecomplete . title . clone ();
  let disk_body  : Option<String> = nodecomplete . body . clone ();
  let disk_source : SourceName = nodecomplete . source . clone ();
  write_at_truenode_in_tree (
    tree, node,
    |t| { t . title = disk_title;
          t . source = disk_source;
          if let IndefOrDef::Definitive { body, .. }
            = &mut t . indef_or_def
            { *body = disk_body; }} ) ?;
  Ok (( )) }

/// Compute the content goal list (diff-aware), reconcile
/// children against it, and mark any surviving non-goal children
/// as ParentIs::Independent.
fn reconcile_content_children (
  tree                           : &mut Tree<ViewNode>,
  node                           : NodeId,
  nodecomplete                   : &NodeComplete,
  pid                            : &ID,
  source                         : &SourceName,
  source_diffs                   : &Option<HashMap<SourceName, SourceDiff>>,
  config                         : &SkgConfig,
  graph_snap                     : &Arc<InRustGraph>,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  active_source_set              : Option<&ActiveSourceSet>,
  node_budget                    : &mut usize,
) -> Result<(), Box<dyn Error>> {
  // Resolve each id through the in-Rust-graph extra_id map so that an
  // id appearing in a node's 'contains' after merging into another
  // node is redirected to the acquirer's pid. Without this, the
  // rerender would carry the raw (now deleted) acquiree through to
  // the goal list and display it as (deleted ...) in its parent's
  // subtree, even though the parent's containment logically points
  // at the acquirer. (Fresh views already got this for free from
  // 'pid_and_source_from_id'; this makes the rerender consistent.)
  let content_ids : Vec<ID> =
    nodecomplete . contains . iter ()
    . map ( |id| graph_snap . pid_of (id)
                 . unwrap_or_else ( || id . clone () ))
    . collect ();
  let (staged_nc, unstaged_nc)
    : (Option<&NodeChanges>, Option<&NodeChanges>) =
    per_stage_node_changes_for_truenode (
      source_diffs, pid, source );
  let is_sub : bool = is_subscribee (tree, node) ?;
  // §6.1: a definitive subscribee-as-such regenerates its content as
  // contains-minus-hides, saved and collateral views alike. (The former
  // "do not regenerate a saved subscribee's children" carve-out is gone:
  // view-update is strictly post-extraction, so the hide edits are already
  // in the graph and regenerating is correct -- and required, since the
  // §5.3 cascade now draws subscribee content through this path rather than
  // the old extendDefinitiveSubtreeFromLeaf.)
  let (goal_list, removed_ids, apparent_content_ids) =
    // git diff view makes a difference
    content_goal_list(
      tree, node, &content_ids,
      staged_nc, unstaged_nc,
      is_sub, config ) ?;
  // §5.5: cap how many *new* content children this node may create against
  // the per-buffer budget (existing children and diff phantoms are free).
  let goal_list : Vec<ID> =
    cap_content_goal_to_budget(
      tree, node, &goal_list, &removed_ids, node_budget );
  // A content child this save deleted is no longer detached here (the former
  // detach_stale_deleted_content stopgap is gone, death-leafward build order
  // 2/3): it stays, and at its own BFS visit becomes a DeletedNode whose cols
  // generalized-orphan and deaden -- so no col reconciles against a missing
  // NodeComplete -- while any user subtree under it is preserved (demoted), and
  // a now-childless DeletedNode is removed by the §6.6 prune sweep.
  complete_content_children(
    tree, node, &goal_list, &removed_ids,
    source_diffs, config, deleted_since_head_pid_src_map,
    active_source_set ) ?;
  mark_erroneous_content_children_as_indep(
    tree, node, &apparent_content_ids ) ?;
  Ok (( )) }

/// §5.5 node-limit: cap a content goal list so this node creates at most
/// `*node_budget` *new* content children (ids not already present as
/// children). Existing children cost nothing (no new node), and removed-id
/// diff phantoms are exempt (the diff overlay is not budget-bound, §5.5), so
/// both are always kept. Decrements `*node_budget` by the number of new ids
/// kept and drops the rest from the goal list (so they are simply not
/// created -- and, in a cascade, not expanded). Preserves order.
fn cap_content_goal_to_budget (
  tree        : &Tree<ViewNode>,
  node        : NodeId,
  goal_list   : &[ID],
  removed_ids : &HashSet<ID>,
  node_budget : &mut usize,
) -> Vec<ID> {
  let existing : HashSet<ID> =
    tree . get (node) . unwrap () . children ()
    . filter_map ( |c| match &c . value () . kind {
        ViewNodeKind::Vognode (Vognode::Normal (t)
                               | Vognode::DiffPhantom (t)) =>
          Some ( t . id . clone () ),
        ViewNodeKind::Vognode (Vognode::Inactive (i)) =>
          Some ( i . id . clone () ),
        _ => None } )
    . collect ();
  let mut kept : Vec<ID> = Vec::with_capacity ( goal_list . len () );
  for id in goal_list {
    if existing . contains (id) || removed_ids . contains (id) {
      kept . push ( id . clone () ); // free: not a new ordinary node
    } else if *node_budget > 0 {
      *node_budget -= 1;
      kept . push ( id . clone () );
    } // else: budget exhausted -- drop this new id
  }
  kept }


fn mutate_truenode_to_deletednode (
  tree   : &mut Tree<ViewNode>,
  node   : NodeId,
  pid    : &ID,
  source : &SourceName,
) -> Result<(), Box<dyn Error>> {
  let (title, body) : (String, Option<String>) =
    read_at_node_in_tree ( tree, node,
      |vn : &ViewNode| match &vn . kind {
        ViewNodeKind::Vognode (Vognode::Normal (t))
          => ( t . title . clone(),
               t . body () . cloned() ),
        _ => ( String::new(), None ) } ) ?;
  write_at_node_in_tree ( tree, node,
    |vn : &mut ViewNode| {
      vn . kind = ViewNodeKind::Vognode (
        Vognode::Deleted ( DeletedNode {
        id     : pid . clone(),
        source : source . clone(),
        title,
        body, } ) ); }
  ) . map_err ( |e| -> Box<dyn Error> { e . into() } ) }

/// Whether this node claims parentIs=affected
/// and is a child of SubscribeeCol (and not a phantom).
fn is_subscribee (
  tree : &Tree<ViewNode>,
  node : NodeId,
) -> Result<bool, Box<dyn Error>> {
  let is_member_of_parent : bool =
    read_at_node_in_tree( tree, node,
      |vn : &ViewNode| match &vn . kind {
        ViewNodeKind::Vognode (Vognode::Normal (t))
          => t . parentIs == ParentIs::Affected,
        _ => false } ) ?;
  let parent_is_subscribee_col : bool =
    read_at_ancestor_in_tree( tree, node, 1,
      |vn : &ViewNode| matches!( &vn . kind,
        ViewNodeKind::PartnerCol (RoleCol::Subscribee)))
    . unwrap_or (false);
  Ok( is_member_of_parent && parent_is_subscribee_col ) }

/// Reads per-stage contains_diff (rather than the merged view from
/// 'node_changes_for_truenode') so that a contains-list removal that
/// lives on only the staged side still produces phantoms. The net
/// HEAD→worktree diff is recomposed via 'net_diff_from_per_stage'
/// before being fed to the goal-list logic.
fn content_goal_list (
  tree          : &Tree<ViewNode>,
  node          : NodeId,
  content_ids   : &[ID],
  staged_nc     : Option<&NodeChanges>,
  unstaged_nc   : Option<&NodeChanges>,
  is_subscribee : bool,
  config        : &SkgConfig,
) -> Result<(Vec<ID>, HashSet<ID>, Vec<ID>), Box<dyn Error>> {
  let no_diff : bool =
    staged_nc . is_none () && unstaged_nc . is_none ();
  if !is_subscribee {
    let apparent_content_ids : Vec<ID> = content_ids . to_vec();
    let (goal_list, removed_ids) : (Vec<ID>, HashSet<ID>) =
      if no_diff {
        ( content_ids . to_vec (), HashSet::new () )
      } else {
        let net : Vec<Diff_Item<ID>> = net_diff_from_per_stage (
          staged_nc   . map ( |c| c . contains_diff . as_slice () ),
          unstaged_nc . map ( |c| c . contains_diff . as_slice () ));
        itemlist_and_removedset_from_diff (&net) };
    Ok(( goal_list, removed_ids, apparent_content_ids ))
  } else {
    let (grandparent_pid, grandparent_source) : (ID, SourceName) =
      pid_and_source_from_ancestor( tree, node, 2,
                                    "content_goal_list" ) ?;
    let worktree_hidden : Vec<ID> =
      { let grandparent_nodecomplete : NodeComplete =
          nodecomplete_rustFirst_by_pid_and_source (
            config, &grandparent_pid, &grandparent_source ) ?;
        grandparent_nodecomplete . hides_from_its_subscriptions
          . or_default() . to_vec() };
    let apparent_content_ids : Vec<ID> =
      setlike_vector_subtraction (
        content_ids . to_vec(), &worktree_hidden );
    let (goal_list, removed_ids) : (Vec<ID>, HashSet<ID>) =
      if no_diff {
        ( apparent_content_ids . clone (), HashSet::new () )
      } else {
        let net : Vec<Diff_Item<ID>> = net_diff_from_per_stage (
          staged_nc   . map ( |c| c . contains_diff . as_slice () ),
          unstaged_nc . map ( |c| c . contains_diff . as_slice () ));
        let head_hidden : Vec<ID> =
          nodecomplete_from_git_head(
              &grandparent_pid, &grandparent_source, config )
            . ok()
            . map( |skg| skg . hides_from_its_subscriptions . into_vec() )
            . unwrap_or_default();
        let head_visible : Vec<ID> =
          setlike_vector_subtraction(
            net . iter () . filter_map (
                |d| match d { // Items that were in HEAD: Unchanged or Removed.
                  Diff_Item::Unchanged (id) |
                    Diff_Item::Removed (id) => Some( id . clone() ),
                  Diff_Item::New (_) => None } )
              . collect(),
            &head_hidden );
        let visible_diff : Vec<Diff_Item<ID>> =
          compute_interleaved_diff(
            &head_visible, &apparent_content_ids );
        itemlist_and_removedset_from_diff (&visible_diff) };
    Ok(( goal_list, removed_ids, apparent_content_ids ))
  } }

/// Reconcile the node's non-parentIgnored TrueNode children
/// against the goal list (content IDs, possibly interleaved with
/// phantom IDs in diff view). Missing children are created as
/// indefinitive ViewNodes or phantom ViewNodes as appropriate.
fn complete_content_children (
  tree               : &mut Tree<ViewNode>,
  node               : NodeId,
  goal_list          : &[ID],
  removed_ids        : &HashSet<ID>,
  source_diffs       : &Option<HashMap<SourceName, SourceDiff>>,
  config             : &SkgConfig,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  active_source_set  : Option<&ActiveSourceSet>,
) -> Result<(), Box<dyn Error>> {
  let child_data : HashMap<ID, ChildData> =
    build_child_creation_data(
      tree, node, goal_list, removed_ids,
      source_diffs, config, deleted_since_head_pid_src_map,
      active_source_set ) ?;
  complete_relevant_children_in_viewnodetree(
    tree, node,
    |vn : &ViewNode| match &vn . kind {
      ViewNodeKind::Vognode (Vognode::Normal (t))
        => t . parentIs == ParentIs::Affected,
      ViewNodeKind::Vognode (Vognode::DiffPhantom (_))
        // Existing phantoms are reordered or replaced, not duplicated.
        => true,
      ViewNodeKind::Vognode (Vognode::Inactive (_))
        => true,
      _ => false },
    |vn : &ViewNode| match &vn . kind {
      ViewNodeKind::Vognode (Vognode::Normal (t)
                             | Vognode::DiffPhantom (t))
        // Both kinds participate in child-list reconciliation.
        => t . id . clone(),
      ViewNodeKind::Vognode (Vognode::Inactive (i))
        => i . id . clone(),
      _ => panic!(
        "complete_content_children: relevant child had no content ID" ) },
    goal_list,
    |id : &ID| {
      let d : &ChildData = child_data . get (id) . expect(
        "complete_content_children: child data not pre-fetched" );
      match d . kind {
        ContentReality::Real =>
          mk_definitive_viewnode(
            id . clone(), d . source . clone(),
            d . title . clone(), d . body . clone() ),
        ContentReality::Phantom (ex, mem) =>
          mk_phantom_viewnode(
            id . clone(), d . source . clone(),
            d . title . clone(), ex, mem ),
        ContentReality::Inactive =>
          mk_inactive_viewnode (
            id . clone(), d . source . clone(), MembershipAxes::default ()) } },
  ) }

/// 'erroneous content children' are children that look like content,
/// but are not actually content.
/// This marks them parentIs=Independent.
///
/// `content_ids` describes the parent's current, saved/worktree
/// `contains` list. A removed-here phantom deliberately is *not* in that
/// list: the point of the phantom is to show content that used to be under
/// this parent in HEAD, but was removed from this parent in the worktree.
/// If we treated that missing worktree membership as erroneous view
/// placement, we would rewrite the phantom to `ParentIs::Independent` and
/// lose the information that the diff is about removed content.
fn mark_erroneous_content_children_as_indep (
  tree        : &mut Tree<ViewNode>,
  node        : NodeId,
  content_ids : &[ID],
) -> Result<(), Box<dyn Error>> {
  let content_id_set : HashSet<ID> =
    content_ids . iter() . cloned() . collect();
  treat_certain_children(
    tree, node,
    |vn : &ViewNode| match &vn . kind {
      ViewNodeKind::Vognode (Vognode::Normal (t)) =>
        t . parentIs == ParentIs::Affected
        && !content_id_set . contains( &t . id )
        && !t . should_be_phantom(), // see this function's docstring
      _ => false },
    |vn : &mut ViewNode| {
      if let ViewNodeKind::Vognode (Vognode::Normal ( ref mut t ))
        = vn . kind
        { t . parentIs = ParentIs::Independent; }},
  ) . map_err( |e| -> Box<dyn Error> { e . into() } ) }

/// Reorder children into three groups:
/// - scaffolds first
/// - parentIgnored TrueNodes
/// - non-ignored TrueNodes last
/// Preserves relative order within each group.
fn order_children_as_scaffolds_then_ignored_then_content (
  tree    : &mut Tree<ViewNode>,
  node : NodeId,
) -> Result<(), Box<dyn Error>> {
  let groups : HashMap<i32, Vec<NodeId>> =
    partition_children( tree, node,
      |vn : &ViewNode| match &vn . kind {
        ViewNodeKind::QualCol (_)
          | ViewNodeKind::Qual (_)
          | ViewNodeKind::PartnerCol (_)
          | ViewNodeKind::BufferRoot
          | ViewNodeKind::DeadScaffold                => 0,
        ViewNodeKind::Vognode (Vognode::Normal (t))
          if t . parentIs != ParentIs::Affected                  => 1,
        ViewNodeKind::Vognode (Vognode::DiffPhantom (_))  => 2,
        ViewNodeKind::Vognode (Vognode::Normal (_))   => 2,
        ViewNodeKind::Vognode (Vognode::Deleted (_))  => 2,
        ViewNodeKind::Vognode (Vognode::Inactive (_)) => 2,
        // UnknownNode is a content-position placeholder: order it
        // alongside the Deleted/True content children rather than as
        // a scaffold.
        ViewNodeKind::Vognode (Vognode::Unknown (_))  => 2,
      } ) . map_err( |e| -> Box<dyn Error> { e . into() } ) ?;
  let empty : Vec<NodeId> = Vec::new();
  for &cid in groups . get( &0 ) . unwrap_or (&empty) . iter()
    . chain( groups . get( &1 ) . unwrap_or (&empty) . iter() )
    . chain( groups . get( &2 ) . unwrap_or (&empty) . iter() )
  { move_child_to_end( tree, node, cid ) ?; }
  Ok(( )) }

/// In diff view, prepend scaffolds for changed fields:
/// - TextChanged (if title/body changed)
/// - IDCol with ID children (if IDs changed)
/// - AliasCol with Alias children (if aliases changed)
/// Each is only prepended if absent.
///
/// Reads per-stage NodeChanges directly rather than going through the
/// merged view, so that a change present on only the staged side (or
/// only the unstaged side) still triggers scaffold emission. See
/// 'per_stage_node_changes_for_truenode' for why the merged view
/// would drop such changes post-save.
pub(in crate::update_buffer) fn maybe_prepend_diff_view_scaffolds (
  tree         : &mut Tree<ViewNode>,
  node         : NodeId,
  source_diffs : &Option<HashMap<SourceName, SourceDiff>>,
  pid          : &ID,
  source       : &SourceName,
) -> Result<(), Box<dyn Error>> {
  let (staged_nc, unstaged_nc)
    : (Option<&NodeChanges>, Option<&NodeChanges>) =
    per_stage_node_changes_for_truenode (
      source_diffs, pid, source );
  if staged_nc . is_none () && unstaged_nc . is_none () {
    return Ok (( )); }
  let staged_text   : bool =
    staged_nc   . map ( |nc| nc . text_changed ) . unwrap_or (false);
  let unstaged_text : bool =
    unstaged_nc . map ( |nc| nc . text_changed ) . unwrap_or (false);
  if staged_text || unstaged_text {
    if unique_scaffold_child_of_viewnode(
      tree, node,
      &ViewNodeKind::Qual ( Qual::TextChanged { staged: false,
                                                unstaged: false } )
    ) ?. is_none() {
      insert_scaffold_as_child(
        tree, node,
        ViewNodeKind::Qual ( Qual::TextChanged { staged: staged_text,
                                                 unstaged: unstaged_text } ),
        true ) ?; }}
  let ids_changed : bool =
    has_list_changes (
      staged_nc   . map ( |nc| nc . ids_diff . as_slice () ),
      unstaged_nc . map ( |nc| nc . ids_diff . as_slice () ) );
  if ids_changed {
    if unique_scaffold_child_of_viewnode(
      tree, node,
      &ViewNodeKind::QualCol (QualCol::ID)
    ) ?. is_none() {
      insert_scaffold_as_child(
        tree, node,
        ViewNodeKind::QualCol (QualCol::ID),
        true ) ?; } }
  let aliases_changed : bool =
    has_list_changes (
      staged_nc   . map ( |nc| nc . aliases_diff . as_slice () ),
      unstaged_nc . map ( |nc| nc . aliases_diff . as_slice () ) );
  if aliases_changed {
    if unique_scaffold_child_of_viewnode(
      tree, node,
      &ViewNodeKind::QualCol (QualCol::Alias)
    ) ?. is_none() {
      insert_scaffold_as_child(
        tree, node,
        ViewNodeKind::QualCol (QualCol::Alias),
        true ) ?; }}
  Ok(( )) }

/// True iff either stage's diff slice contains a New or Removed item.
/// Generic over the diff item type so the same helper works for
/// ids_diff, aliases_diff, etc.
fn has_list_changes<T> (
  staged   : Option<&[Diff_Item<T>]>,
  unstaged : Option<&[Diff_Item<T>]>,
) -> bool {
  let non_trivial = | slice : Option<&[Diff_Item<T>]> | -> bool {
    slice . map ( |s| s . iter () . any ( |d|
      ! matches! ( d, Diff_Item::Unchanged (_) ))) . unwrap_or (false) };
  non_trivial (staged) || non_trivial (unstaged) }

/// WHAT IT DOES: Maps each child of 'node' to a ChildData:
/// determines title, source, and phantom|normal status
/// (where phantom = removed from this list of children,
/// which only applies in the git diff view).
///
/// MOTIVATION: We pre-extract what will be needed
/// by the last argument to complete_content_children,
/// that is, by the closure that creates children,
/// This way the closure captures only owned/pre-computed data
/// and does not conflict with the &mut tree borrow
/// in complete_relevant_children_in_viewnodetree.
fn build_child_creation_data (
  tree               : &Tree<ViewNode>,
  node               : NodeId,
  goal_list          : &[ID],
  removed_ids        : &HashSet<ID>,
  source_diffs       : &Option<HashMap<SourceName, SourceDiff>>,
  config             : &SkgConfig,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  active_source_set  : Option<&ActiveSourceSet>,
) -> Result<HashMap<ID, ChildData>, Box<dyn Error>> {
  let (parent_pid, parent_source) : (ID, SourceName) =
    pid_and_source_from_treenode (
      tree, node, "build_child_creation_data" ) ?;
  let child_sources : HashMap<ID, SourceName> =
    { let node_ref : NodeRef<ViewNode> =
        tree . get (node)
          . ok_or ("build_child_creation_data: node not found") ?;
      let mut m : HashMap<ID, SourceName> = HashMap::new();
      for child_ref in node_ref . children() {
        match &child_ref . value() . kind {
          ViewNodeKind::Vognode (Vognode::Normal (t)
                                 | Vognode::DiffPhantom (t))
            => { m . insert( t . id . clone(),
                             t . source . clone()); },
          ViewNodeKind::Vognode (Vognode::Inactive (i))
            => { m . insert( i . id . clone(),
                             i . source . clone()); },
          _ => {}, }}
      m };
  let mut result : HashMap<ID, ChildData> = HashMap::new();
  for id in goal_list {
    if result . contains_key (id) { continue; }
    // Skip IDs already present as children in the viewnode tree.
    // 'complete_relevant_children_in_viewnodetree' only needs
    // ChildData for children it creates from scratch; existing
    // children keep their in-tree state. Eagerly reading disk for
    // every goal_list ID would fail (ENOENT) for children that
    // this save just deleted, since their .skg file is gone.
    if child_sources . contains_key (id) { continue; }
    let is_phantom : bool = removed_ids . contains (id);
    if is_phantom {
      let phantom_source : SourceName =
        find_source_with_optional_tantivy (
          id, deleted_since_head_pid_src_map, None, config )
          . ok_or_else ( || -> Box<dyn Error> { format! (
            "find_source: no source for {}", id . 0 ) . into () } ) ?;
      let (ex, mem) : (ExistenceAxes, MembershipAxes) =
        phantom_axes ( id, &phantom_source,
                       &parent_pid, &parent_source,
                       source_diffs . as_ref () );
      let kind : ContentReality = ContentReality::Phantom (ex, mem);
      let title : String = title_for_phantom(
        id, &phantom_source, source_diffs . as_ref(), config );
      result . insert( id . clone(),
                       ChildData { title,
                                   source: phantom_source,
                                   body: None,
                                   kind } );
    } else {
      let child_source : SourceName =
        find_source_with_optional_tantivy (
          id, deleted_since_head_pid_src_map, None, config )
          . ok_or_else ( || -> Box<dyn Error> { format! (
            "find_source: no source for {}", id . 0 ) . into () } ) ?;
      if active_source_set
        . is_some_and ( |active| !active . contains_source (&child_source) )
      {
        result . insert( id . clone(),
                       ChildData { title: String::new (),
                                   source: child_source,
                                   body: None,
                                   kind: ContentReality::Inactive } );
        continue; }
      let skg : NodeComplete =
        nodecomplete_rustFirst_by_pid_and_source ( config, id, &child_source ) ?;
      result . insert( id . clone(),
                     ChildData { title: skg . title . clone(),
                                 source: skg . source . clone(),
                                 body: skg . body . clone(),
                                 kind: ContentReality::Real } ); }}
  Ok (result) }

/// In diff view, set the TrueNode's per-stage diff axes.
fn set_diff_status (
  tree         : &mut Tree<ViewNode>,
  node         : NodeId,
  pid          : &ID,
  source_diffs : &Option<HashMap<SourceName, SourceDiff>>,
  source       : &SourceName,
) -> Result<(), Box<dyn Error>> {
  let source_diff : &SourceDiff =
    match source_diffs . as_ref() . and_then(|d| d . get (source)) {
      Some (sd) => sd,
      None => return Ok(( )) };
  if !source_diff . is_git_repo {
    return write_truenode_diff (
      tree, node,
      ExistenceAxes::default (),
      MembershipAxes::default (),
      true /* not_in_git */ ); }
  let file_path : PathBuf =
    PathBuf::from(format!("{}.skg", pid . 0));
  let staged_x : Option<Sign> = source_diff . staged . get (&file_path)
    . and_then ( |d| d . status . to_existence_sign () );
  let unstaged_x : Option<Sign> = source_diff . unstaged . get (&file_path)
    . and_then ( |d| d . status . to_existence_sign () );
  let (staged_m, unstaged_m) : (Option<Sign>, Option<Sign>) = {
    // Membership axes derive from the parent's per-stage contains_diff:
    // if pid is New(pid) in a stage, that stage's M is Plus.
    let is_non_content : bool =
      read_at_node_in_tree( tree, node,
        |vn : &ViewNode| match &vn . kind {
          // Called for normal vognodes only; phantom parentIs is not
          // part of diff-status inheritance.
          ViewNodeKind::Vognode (Vognode::Normal (t))
            => t . parentIs != ParentIs::Affected,
          _ => true } ) ?;
    if is_non_content { (None, None) }
    else {
      let parent : Option<(ID, SourceName)> =
        read_at_ancestor_in_tree( tree, node, 1,
          |vn : &ViewNode| match &vn . kind {
            // Only normal ancestors define content membership axes;
            // phantom ancestors are display placeholders.
            ViewNodeKind::Vognode (Vognode::Normal (t))
              => Ok(( t . id . clone(), t . source . clone() )),
            _ => Err ("not a TrueNode") }
        ) . ok () . and_then ( |r| r . ok () );
      match parent {
        None => (None, None),
        Some ((parent_pid, parent_source)) => {
          membership_signs_from_parent_contains (
            source_diff, &parent_pid, &parent_source, pid ) }} } };
  let existence  : ExistenceAxes  = ExistenceAxes  {
    staged: staged_x, unstaged: unstaged_x };
  let membership : MembershipAxes = MembershipAxes {
    staged: staged_m, unstaged: unstaged_m };
  write_truenode_diff (tree, node, existence, membership, false) }

/// Look up per-stage M-axis signs for a child within a parent's contains.
/// Returns (staged_m, unstaged_m); each is Some(Plus) iff the child was
/// added to the parent's contains in that stage.
fn membership_signs_from_parent_contains (
  source_diff   : &SourceDiff,
  parent_pid    : &ID,
  parent_source : &SourceName,
  child_pid     : &ID,
) -> (Option<Sign>, Option<Sign>) {
  // Both stages live under the same parent_source; we ignore other
  // sources here. Look up the parent file in each stage's map and
  // inspect its contains_diff.
  let _ = parent_source; // signature symmetry; not used yet
  let parent_file : PathBuf =
    PathBuf::from ( format! ( "{}.skg", parent_pid . 0 ));
  let staged_m : Option<Sign> = source_diff . staged . get (&parent_file)
    . and_then ( |d| d . node_changes . as_ref () )
    . and_then ( |nc| if nc . contains_diff . iter ()
                          . any ( |d| matches! ( d, Diff_Item::New (id) if id == child_pid ))
                       { Some (Sign::Plus) } else { None } );
  let unstaged_m : Option<Sign> = source_diff . unstaged . get (&parent_file)
    . and_then ( |d| d . node_changes . as_ref () )
    . and_then ( |nc| if nc . contains_diff . iter ()
                          . any ( |d| matches! ( d, Diff_Item::New (id) if id == child_pid ))
                       { Some (Sign::Plus) } else { None } );
  (staged_m, unstaged_m) }

fn write_truenode_diff (
  tree       : &mut Tree<ViewNode>,
  node       : NodeId,
  existence  : ExistenceAxes,
  membership : MembershipAxes,
  not_in_git : bool,
) -> Result<(), Box<dyn Error>> {
  write_at_node_in_tree( tree, node,
    |vn : &mut ViewNode| {
      // Normal nodes get file/member diff axes here.
      // Phantom axes are set when the phantom is constructed.
      if let ViewNodeKind::Vognode (Vognode::Normal ( ref mut t ))
        = vn . kind
      { t . existence  = existence;
        t . membership = membership;
        t . not_in_git = not_in_git; }
      vn . normal_to_phantom (); }
  ) . map_err( |e| -> Box<dyn Error> { e . into() } ) }

#[cfg(test)]
mod tests {
  use super::*;

  fn source_name (s: &str) -> SourceName { SourceName ( s . to_string () ) }
  fn id          (s: &str) -> ID          { ID ( s . to_string () ) }

  fn make_diff_entry (text_changed: bool) -> NodeCompleteDiff {
    NodeCompleteDiff {
      status: GitDiffStatus::Modified,
      node_changes: Some ( NodeChanges {
        text_changed,
        aliases_diff:  Vec::new (),
        ids_diff:      Vec::new (),
        contains_diff: Vec::new (), } ),
      before_node: None,
      after_node: None, } }

  fn sd_with (
    pid     : &ID,
    staged  : Option<bool>,
    unstag  : Option<bool>,
  ) -> SourceDiff {
    let file : PathBuf = PathBuf::from ( format! ( "{}.skg", pid . 0 ) );
    let mut s : HashMap<PathBuf, NodeCompleteDiff> = HashMap::new ();
    let mut u : HashMap<PathBuf, NodeCompleteDiff> = HashMap::new ();
    if let Some (t) = staged { s . insert (file . clone (), make_diff_entry (t)); }
    if let Some (t) = unstag { u . insert (file,           make_diff_entry (t)); }
    SourceDiff {
      is_git_repo: true,
      staged: s, unstaged: u,
      added_nodes: HashMap::new (),
      deleted_nodes: HashMap::new (), } }

  fn diffs_with (src: &SourceName, sd: SourceDiff)
    -> Option<HashMap<SourceName, SourceDiff>> {
    let mut m : HashMap<SourceName, SourceDiff> = HashMap::new ();
    m . insert (src . clone (), sd);
    Some (m) }

  fn text_changed_both (
    diffs : &Option<HashMap<SourceName, SourceDiff>>,
    pid   : &ID,
    src   : &SourceName,
  ) -> (bool, bool) {
    let (s, u) = per_stage_node_changes_for_truenode (diffs, pid, src);
    ( s . map ( |n| n . text_changed ) . unwrap_or (false),
      u . map ( |n| n . text_changed ) . unwrap_or (false) )
  }

  #[test]
  fn text_change_only_staged () {
    let src = source_name ("public");
    let pid = id ("n");
    let diffs = diffs_with (&src, sd_with (&pid, Some (true), None));
    assert_eq! ( text_changed_both (&diffs, &pid, &src), (true, false) );
  }

  #[test]
  fn text_change_only_unstaged () {
    let src = source_name ("public");
    let pid = id ("n");
    let diffs = diffs_with (&src, sd_with (&pid, None, Some (true)));
    assert_eq! ( text_changed_both (&diffs, &pid, &src), (false, true) );
  }

  #[test]
  fn text_change_both_stages () {
    let src = source_name ("public");
    let pid = id ("n");
    let diffs = diffs_with (&src, sd_with (&pid, Some (true), Some (true)));
    assert_eq! ( text_changed_both (&diffs, &pid, &src), (true, true) );
  }

  #[test]
  fn no_diff_at_all () {
    let src = source_name ("public");
    let pid = id ("n");
    assert_eq! ( text_changed_both (&None, &pid, &src), (false, false) );
  }
}
