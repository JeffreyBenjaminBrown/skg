use crate::to_org::complete::contents::clobberIndefinitiveViewnode;
use crate::source_sets::ActiveSourceSet;
use crate::types::viewnode::{mk_inactive_viewnode, mk_unknown_viewnode};
use crate::to_org::util::{DefinitiveMap, make_indef_if_repeat_then_extend_defmap};
use crate::types::git::MembershipAxes;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::env::find_source_with_optional_tantivy;
use crate::types::nodes::complete::NodeComplete;
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_source;
use crate::util::setlike_vector_subtraction;
use crate::types::viewnode::{
    ViewNode, ViewNodeKind, DeletedNode, IndefOrDef,
    ParentIs, ViewRequest, mk_definitive_viewnode};
use crate::types::viewnode::{Vognode, RoleCol};
use crate::types::tree::generic::{error_unless_node_satisfies, pid_and_source_from_ancestor, read_at_ancestor_in_tree, read_at_node_in_tree, write_at_node_in_tree};
use crate::types::tree::viewnode_nodecomplete::{
    pid_and_source_from_treenode,
    write_at_truenode_in_tree};
use crate::update_buffer::util::{
    complete_relevant_children_in_viewnodetree,
    partition_children,
    treat_certain_children,
    move_child_to_end};

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::sync::Arc;

enum ContentReality {
  Real, // Real content: Exists in worktree, and parent contains it at this position.
  Inactive,
  Unknown, // TODO/DONE/local-view-update/plan_v2.org §7.6: a content id that resolves to nothing (a dangling reference) -> an Unknown placeholder, rather than aborting the whole view.
}

struct ChildData {
  title  : String,
  source : SourceName,
  body   : Option<String>,
  kind   : ContentReality,
}

/// TrueNode content reconcile + content-child creation, for one node, in the
/// TODO/DONE/local-view-update/plan_v2.org §3 level-order BFS visit. View completion (dispatch_node_update)
/// settles the node's
/// Finalizable state *before* calling this (via 'apply_definitive_draw_rule')
/// and passes:
/// - `settled`: the TODO/DONE/local-view-update/plan_v2.org §5.2 draw rule already ran for this node (it carried a
///   ViewRequest::Definitive), so its map entry and indef/def are already
///   correct -- skip 'make_indef_if_repeat_then_extend_defmap', which would
///   otherwise indefinitize a just-made-Final node against its own entry.
/// - `cascade`: this node is Final (DVR-made); per TODO/DONE/local-view-update/plan_v2.org §5.3 it hands a
///   ViewRequest::Definitive to each of its affected content children so the
///   BFS draws each Final (clobbering competing Tentative occurrences).
/// - `node_budget`: the TODO/DONE/local-view-update/plan_v2.org §5.5 remaining budget of new ViewNodes; content-child
///   creation is capped against it.
pub fn expand_true_content_at_truenode (
  node               : NodeId,
  tree               : &mut Tree<ViewNode>,
  defmap             : &mut DefinitiveMap,
  config             : &SkgConfig,
  graph_snap                     : &Arc<InRustGraph>,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  deleted_by_this_save_pids      : &HashSet<ID>,
  active_source_set              : Option<&ActiveSourceSet>,
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
  // This content path produces the pure worktree view; the node's git diff
  // (axes, phantom flip, diff scaffolds) is applied by process_truenode_diff at
  // the end of the node's BFS visit (TODO/DONE/local-view-update/plan_v2.org §9 reversal / #3).
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
  // TODO/DONE/local-view-update/plan_v2.org §5.5: this vognode is definitive and about to expand -- draw its whole
  // content group, and (via the BFS) its cols. Each expansion costs ONE budget
  // unit; an indefinitive node (returned above) costs nothing, and a col fills
  // for free. visit_normal_node already forced this node indefinitive if the
  // budget was 0, so here it is > 0; saturating_sub is defensive.
  *node_budget = node_budget . saturating_sub (1);
  clear_edit_request (tree, node) ?;
  let nodecomplete : NodeComplete =
    nodecomplete_rustFirst_by_pid_and_source (
      config, &pid, &initial_source ) ?;
  // TODO/DONE/local-view-update/plan_v2.org §8.3: EVERY definitive node re-syncs title/body/source from the snapshot,
  // saved and collateral alike. (After extraction the snapshot already reflects
  // the saved buffer's text, so re-syncing the saved node yields the same
  // content it just defined -- a no-op.)
  sync_truenode_from_disk (tree, node, &nodecomplete) ?;
  reconcile_content_children (
    tree, node, &nodecomplete, config, graph_snap,
    deleted_since_head_pid_src_map,
    active_source_set ) ?;
  if cascade {
    attach_cascade_dvrs_to_affected_content( tree, node ) ?; }
  order_children_as_scaffolds_then_ignored_then_content(
    tree, node ) ?;
  Ok(( )) }

/// TODO/DONE/local-view-update/plan_v2.org §5.3 cascade: hand a ViewRequest::Definitive to each affected,
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

/// Overwrite the viewnode's title, source, and body with the fresh values from
/// the snapshot. TODO/DONE/local-view-update/plan_v2.org §8.3: every definitive node re-syncs, saved and collateral
/// alike -- after extraction the snapshot already holds the saved buffer's text,
/// so the saved node re-syncs to the same content it just defined.
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
  config                         : &SkgConfig,
  graph_snap                     : &Arc<InRustGraph>,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  active_source_set              : Option<&ActiveSourceSet>,
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
  let is_sub : bool = is_subscribee (tree, node) ?;
  // TODO/DONE/local-view-update/plan_v2.org §6.1: a definitive subscribee-as-such regenerates its content as
  // contains-minus-hides, saved and collateral views alike. (View-update is
  // strictly post-extraction, so the hide edits are already in the graph and
  // regenerating contains-minus-hides is correct. The TODO/DONE/local-view-update/plan_v2.org §5.3 cascade draws
  // subscribee content through this same path.)
  let apparent_content_ids : Vec<ID> =
    content_goal_list( tree, node, &content_ids, is_sub, config ) ?;
  // TODO/DONE/local-view-update/plan_v2.org §5.5: the content group is drawn WHOLE -- never truncated mid-group. The
  // budget is spent once per expanding vognode (in expand_true_content_at_truenode),
  // not per child, so a node either fully expands or is left indefinitive; we
  // never create a silent partial sibling set.
  // A content child this save deleted stays here; at its own BFS visit it
  // becomes a DeletedNode whose cols generalized-orphan and deaden -- so no col
  // reconciles against a missing NodeComplete -- while any user subtree under it
  // is preserved (demoted), and a now-childless DeletedNode is removed by the
  // TODO/DONE/local-view-update/plan_v2.org §6.6 prune sweep.
  complete_content_children(
    tree, node, &apparent_content_ids, config,
    deleted_since_head_pid_src_map, active_source_set ) ?;
  mark_erroneous_content_children_as_indep(
    tree, node, &apparent_content_ids ) ?;
  convert_nonmember_unknown_children_to_dead(
    tree, node, &apparent_content_ids ) ?;
  Ok (( )) }

/// TODO/DONE/local-view-update/plan_v2.org §6.5: an Unknown content placeholder (a dangling reference the parent kept
/// rather than failing the whole view) that is *no longer a member* of the
/// parent's contains converts to a DeadScaffold; the TODO/DONE/local-view-update/plan_v2.org §3.4 postorder prune sweep
/// then removes it. An Unknown still in contains is retained (a present-but-
/// unresolvable reference). Self-deletion is the sweep's job -- this only
/// decides member-vs-convert, so a Dead -> Unknown -> Dead chain collapses in
/// one sweep.
fn convert_nonmember_unknown_children_to_dead (
  tree       : &mut Tree<ViewNode>,
  node       : NodeId,
  member_ids : &[ID],
) -> Result<(), Box<dyn Error>> {
  let member_set : HashSet<ID> =
    member_ids . iter () . cloned () . collect ();
  treat_certain_children(
    tree, node,
    |vn : &ViewNode| match &vn . kind {
      ViewNodeKind::Vognode (Vognode::Unknown (u)) =>
        ! member_set . contains (&u . id),
      _ => false },
    |vn : &mut ViewNode| { vn . kind = ViewNodeKind::DeadScaffold; },
  ) . map_err( |e| -> Box<dyn Error> { e . into() } ) }

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

/// The worktree content goal list for a node: the (extra-id-resolved) contains,
/// in order. For a subscribee-as-such (TODO/DONE/local-view-update/plan_v2.org §6.1) it is contains MINUS what the
/// subscriber hides -- the hidden remainder shows in the HiddenInSubscribeeCol.
/// The git-diff decorations (removed-member phantoms, membership axes) are NOT
/// computed here: they are applied per node by process_truenode_diff at its BFS
/// visit (TODO/DONE/local-view-update/plan_v2.org §9 reversal / #3), so the main content path produces only the pure
/// worktree view.
fn content_goal_list (
  tree          : &Tree<ViewNode>,
  node          : NodeId,
  content_ids   : &[ID],
  is_subscribee : bool,
  config        : &SkgConfig,
) -> Result<Vec<ID>, Box<dyn Error>> {
  if !is_subscribee {
    Ok ( content_ids . to_vec () )
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
    Ok ( setlike_vector_subtraction (
           content_ids . to_vec(), &worktree_hidden ) )
  } }

/// Reconcile the node's non-parentIgnored TrueNode children
/// against the goal list (content IDs, possibly interleaved with
/// phantom IDs in diff view). Missing children are created as
/// indefinitive ViewNodes or phantom ViewNodes as appropriate.
fn complete_content_children (
  tree               : &mut Tree<ViewNode>,
  node               : NodeId,
  goal_list          : &[ID],
  config             : &SkgConfig,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  active_source_set  : Option<&ActiveSourceSet>,
) -> Result<(), Box<dyn Error>> {
  let child_data : HashMap<ID, ChildData> =
    build_child_creation_data(
      tree, node, goal_list, config,
      deleted_since_head_pid_src_map, active_source_set ) ?;
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
      // Both kinds participate in child-list reconciliation.
      ViewNodeKind::Vognode (Vognode::Normal (t))
        => t . id . clone(),
      ViewNodeKind::Vognode (Vognode::DiffPhantom (p))
        => p . id . clone(),
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
        ContentReality::Inactive =>
          mk_inactive_viewnode (
            id . clone(), d . source . clone(), MembershipAxes::default ()),
        ContentReality::Unknown =>
          mk_unknown_viewnode ( id . clone() ) } },
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
  config             : &SkgConfig,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  active_source_set  : Option<&ActiveSourceSet>,
) -> Result<HashMap<ID, ChildData>, Box<dyn Error>> {
  let child_sources : HashMap<ID, SourceName> =
    { let node_ref : NodeRef<ViewNode> =
        tree . get (node)
          . ok_or ("build_child_creation_data: node not found") ?;
      let mut m : HashMap<ID, SourceName> = HashMap::new();
      for child_ref in node_ref . children() {
        match &child_ref . value() . kind {
          // Only an Affected Normal child counts as "already present" -- the
          // same predicate complete_content_children's reconcile applies. An
          // Independent same-id child is a distinct occurrence (e.g. a
          // containerward ancestor, or a TODO/DONE/local-view-update/plan_v2.org §6.0-demoted branch), so its goal id
          // must still be pre-fetched here; otherwise the reconcile sends it to
          // the create closure and child_data.get(id).expect(..) panics.
          ViewNodeKind::Vognode (Vognode::Normal (t))
            if t . parentIs == ParentIs::Affected
            => { m . insert( t . id . clone(),
                             t . source . clone()); },
          ViewNodeKind::Vognode (Vognode::DiffPhantom (p))
            => { m . insert( p . id . clone(),
                             p . source . clone()); },
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
    let child_source : SourceName =
      match find_source_with_optional_tantivy (
        id, deleted_since_head_pid_src_map, None, config )
      { Some (s) => s,
        None => {
          // TODO/DONE/local-view-update/plan_v2.org §7.6: the id resolves to nothing (a dangling reference). Render an
          // Unknown placeholder rather than aborting the whole view. This is
          // what lets the one view completion serve de-novo (which must tolerate
          // dangling refs) and makes post-save robust to them too.
          result . insert ( id . clone (),
            ChildData { title  : String::new (),
                        source : SourceName::not_found (),
                        body   : None,
                        kind   : ContentReality::Unknown } );
          continue; } };
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
                               kind: ContentReality::Real } ); }
  Ok (result) }

#[cfg(test)]
#[path = "../../../tests/unit/reconcile_content.rs"]
mod tests;
