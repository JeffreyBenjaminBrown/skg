use crate::to_org::complete::contents::clobberIndefinitiveViewnode;
use crate::types::viewnode::mk_phantom_viewnode;
use crate::to_org::util::{DefinitiveMap, make_indef_if_repeat_then_extend_defmap};
use crate::types::git::{ExistenceAxes, MembershipAxes, Sign, SourceDiff, NodeChanges, net_diff_from_per_stage, per_stage_node_changes_for_truenode};
use crate::types::list::{Diff_Item, compute_interleaved_diff, itemlist_and_removedset_from_diff};
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::phantom::{title_for_phantom, phantom_axes};
use crate::types::memory::find_source_many_ways;
use crate::types::nodes::complete::NodeComplete;
use crate::git_ops::read_repo::nodecomplete_from_git_head;
use crate::types::memory::nodecomplete_from_memory_or_disk;
use crate::util::setlike_vector_subtraction;
use crate::types::viewnode::{
    ViewNode, ViewNodeKind, Scaffold, DeletedNode, IndefOrDef,
    Birth, mk_definitive_viewnode};
use crate::types::tree::generic::{error_unless_node_satisfies, pid_and_source_from_ancestor, read_at_ancestor_in_tree, read_at_node_in_tree, write_at_node_in_tree};
use crate::types::tree::viewnode_nodecomplete::{
    pid_and_source_from_treenode,
    write_at_truenode_in_tree,
    unique_scaffold_child,
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

enum ContentReality {
  Real, // Real content: Exists in worktree, and parent contains it at this position.
  Phantom (ExistenceAxes, MembershipAxes), // Is not contained by parent at this position, and might not exist at all.
}

struct ChildData {
  title  : String,
  source : SourceName,
  kind   : ContentReality,
}

/// Outcome of dispatching on the TrueNode's kind and state.
/// Stop means the node has been handled (phantom/indefinitive/
/// deleted-by-this-save) and no further completion is needed.
/// Continue carries the pid/source extracted once for the rest
/// of the pipeline.
enum TruenodeKindOutcome {
  Stop,
  Continue { pid : ID, source : SourceName },
}

/// TrueNode completion.
///
/// INTENDED USE: Use in the first, DFS preorder (parent-first)
/// buffer-update pass through the tree.
/// That's because the second, DFS-postorder (child-first)
/// pass will not be correct if any truenode is missing children.
///
/// WHAT IT DOES (high-level phases):
/// 1. resolve_truenode_kind -- phantom/indefinitive/deleted-by-save
///    short-circuits; otherwise yields (pid, source).
/// 2. clear_consumed_edit_request
/// 3. Load NodeComplete; sync title/body from disk for non-saved views.
/// 4. reconcile_content_children -- goal list + phantoms + indep-mark.
/// 5. order_children_as_scaffolds_then_ignored_then_content
/// 6. maybe_prepend_subscribee_col
/// 7. maybe_prepend_diff_view_scaffolds
pub fn complete_truenode_preorder (
  node               : NodeId,
  tree               : &mut Tree<ViewNode>,
  defmap             : &mut DefinitiveMap,
  source_diffs       : &Option<HashMap<SourceName, SourceDiff>>,
  config             : &SkgConfig,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  deleted_by_this_save_pids       : &HashSet<ID>,
  is_saved_view                   : bool,
) -> Result<(), Box<dyn Error>> {
  error_unless_node_satisfies(
    tree, node, |vn : &ViewNode| matches!( &vn . kind,
                                           ViewNodeKind::True (_)),
    "complete_truenode_preorder: expected TrueNode" ) ?;
  make_indef_if_repeat_then_extend_defmap(
    tree, node, defmap ) ?;
  let (pid, source) : (ID, SourceName) =
    match resolve_truenode_kind (
      tree, node, source_diffs, config,
      deleted_by_this_save_pids ) ?
    { TruenodeKindOutcome::Stop => return Ok (( )),
      TruenodeKindOutcome::Continue { pid, source } => (pid, source) };
  clear_consumed_edit_request (tree, node) ?;
  let nodecomplete : NodeComplete =
    nodecomplete_from_memory_or_disk ( config, &pid, &source ) ?;
  if ! is_saved_view { // The saved (definitive) view of a node *defines* the title and body, but other views need those fields updated.
    sync_truenode_text_from_disk (tree, node, &nodecomplete) ?; }
  let subscribes_to : Vec<ID> =
    reconcile_content_children (
      tree, node, &nodecomplete, &pid, &source,
      source_diffs, config, deleted_since_head_pid_src_map ) ?;
  order_children_as_scaffolds_then_ignored_then_content(
    tree, node ) ?;
  maybe_prepend_subscribee_col(
    tree, node, &subscribes_to ) ?;
  maybe_prepend_diff_view_scaffolds(
    tree, node,
    source_diffs, &pid, &source ) ?;
  Ok(( )) }

/// Phase 1 of complete_truenode_preorder: phantom/indefinitive/
/// deleted-by-this-save nodes are finalised in place here and
/// the function returns Stop; everything else yields Continue
/// with the (pid, source) that later phases need.
fn resolve_truenode_kind (
  tree                       : &mut Tree<ViewNode>,
  node                       : NodeId,
  source_diffs               : &Option<HashMap<SourceName, SourceDiff>>,
  config                     : &SkgConfig,
  deleted_by_this_save_pids  : &HashSet<ID>,
) -> Result<TruenodeKindOutcome, Box<dyn Error>> {
  // Handle git diff view *before* the clobber-and-early-return
  // that happens to indefinitive nodes.
  let is_phantom : bool =
    // Phantoms (nodes with diff status already set, e.g. Removed,
    // RemovedHere) are display-only placeholders created during
    // content reconciliation. They need no further completion.
    read_at_node_in_tree( tree, node,
      |vn : &ViewNode| match &vn . kind {
        ViewNodeKind::True (t) => t . is_phantom(),
        _ => false } ) ?;
  if is_phantom { return Ok (TruenodeKindOutcome::Stop); }
  let (pid, source) : (ID, SourceName) =
    pid_and_source_from_treenode( tree, node,
                                  "resolve_truenode_kind" ) ?;
  maybe_change_node_diff_status(
    tree, node, &pid, source_diffs, &source) ?;
  let is_indefinitive : bool =
    read_at_node_in_tree( tree, node,
      |vn : &ViewNode| match &vn . kind {
        ViewNodeKind::True (t) => t . is_indefinitive (),
        _ => false } ) ?;
  if is_indefinitive {
    clobberIndefinitiveViewnode( tree, node, config ) ?;
    return Ok (TruenodeKindOutcome::Stop); }
  if deleted_by_this_save_pids . contains (&pid) {
    mutate_truenode_to_deletednode (
      tree, node, &pid, &source ) ?;
    return Ok (TruenodeKindOutcome::Stop); }
  Ok (TruenodeKindOutcome::Continue { pid, source }) }

/// Phase 2: clear the edit_request that this preorder visit has
/// consumed. Analogous to 'remove_completed_view_request' for
/// view requests.
fn clear_consumed_edit_request (
  tree : &mut Tree<ViewNode>,
  node : NodeId,
) -> Result<(), Box<dyn Error>> {
  write_at_truenode_in_tree ( tree, node, |t| {
    if let IndefOrDef::Definitive { edit_request, .. }
      = &mut t . indef_or_def
      { *edit_request = None; } } ) ?;
  Ok (( )) }

/// Phase 3 (non-saved views only): overwrite the viewnode's title
/// and body with the fresh values from disk. The saved view is the
/// one that *defines* those fields, so it is excluded.
fn sync_truenode_text_from_disk (
  tree         : &mut Tree<ViewNode>,
  node         : NodeId,
  nodecomplete : &NodeComplete,
) -> Result<(), Box<dyn Error>> {
  let disk_title : String = nodecomplete . title . clone ();
  let disk_body  : Option<String> = nodecomplete . body . clone ();
  write_at_truenode_in_tree ( tree, node, |t| {
    t . title = disk_title;
    if let IndefOrDef::Definitive { body, .. } = &mut t . indef_or_def {
      *body = disk_body; } }
  ) ?;
  Ok (( )) }

/// Phase 4: compute the content goal list (diff-aware), reconcile
/// children against it, and mark any surviving non-goal children
/// as Birth::Independent. Returns 'subscribes_to' so phase 6 can
/// decide whether to prepend a SubscribeeCol.
fn reconcile_content_children (
  tree                           : &mut Tree<ViewNode>,
  node                           : NodeId,
  nodecomplete                   : &NodeComplete,
  pid                            : &ID,
  source                         : &SourceName,
  source_diffs                   : &Option<HashMap<SourceName, SourceDiff>>,
  config                         : &SkgConfig,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
) -> Result<Vec<ID>, Box<dyn Error>> {
  let content_ids : Vec<ID> =
    nodecomplete . contains . clone();
  let subscribes_to : Vec<ID> =
    nodecomplete . subscribes_to . or_default() . to_vec();
  let (staged_nc, unstaged_nc)
    : (Option<&NodeChanges>, Option<&NodeChanges>) =
    per_stage_node_changes_for_truenode (
      source_diffs, pid, source );
  let is_sub : bool = is_subscribee (tree, node) ?;
  let (goal_list, removed_ids, apparent_content_ids) =
    // git diff view makes a difference
    content_goal_list(
      tree, node, &content_ids,
      staged_nc, unstaged_nc,
      is_sub, config ) ?;
  complete_content_children(
    tree, node, &goal_list, &removed_ids,
    source_diffs, config, deleted_since_head_pid_src_map ) ?;
  mark_erroneous_content_children_as_indep(
    tree, node, &apparent_content_ids ) ?;
  Ok (subscribes_to) }

fn mutate_truenode_to_deletednode (
  tree   : &mut Tree<ViewNode>,
  node   : NodeId,
  pid    : &ID,
  source : &SourceName,
) -> Result<(), Box<dyn Error>> {
  let (title, body) : (String, Option<String>) =
    read_at_node_in_tree ( tree, node,
      |vn : &ViewNode| match &vn . kind {
        ViewNodeKind::True (t) =>
          ( t . title . clone(), t . body () . cloned() ),
        _ => ( String::new(), None ) } ) ?;
  write_at_node_in_tree ( tree, node,
    |vn : &mut ViewNode| {
      vn . kind = ViewNodeKind::Deleted ( DeletedNode {
        id     : pid . clone(),
        source : source . clone(),
        title,
        body, } ); }
  ) . map_err ( |e| -> Box<dyn Error> { e . into() } ) }

/// Whether this is a non-ignored child of a SubscribeeCol.
fn is_subscribee (
  tree : &Tree<ViewNode>,
  node : NodeId,
) -> Result<bool, Box<dyn Error>> {
  let is_content_of_parent : bool =
    read_at_node_in_tree( tree, node,
      |vn : &ViewNode| match &vn . kind {
        ViewNodeKind::True (t) => !t . parent_ignores_it(),
        _ => false } ) ?;
  let parent_is_subscribee_col : bool =
    read_at_ancestor_in_tree( tree, node, 1,
      |vn : &ViewNode| matches!( &vn . kind,
        ViewNodeKind::Scaff (Scaffold::SubscribeeCol)))
    . unwrap_or (false);
  Ok( is_content_of_parent && parent_is_subscribee_col ) }

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
          nodecomplete_from_memory_or_disk (
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
) -> Result<(), Box<dyn Error>> {
  let child_data : HashMap<ID, ChildData> =
    build_child_creation_data(
      tree, node, goal_list, removed_ids,
      source_diffs, config, deleted_since_head_pid_src_map ) ?;
  complete_relevant_children_in_viewnodetree(
    tree, node,
    |vn : &ViewNode| matches!( &vn . kind,
                               ViewNodeKind::True (t)
                               if !t . parent_ignores_it() ),
    |vn : &ViewNode| match &vn . kind {
      ViewNodeKind::True (t) => t . id . clone(),
      _ => panic!(
        "complete_content_children: relevant child not TrueNode" ) },
    goal_list,
    |id : &ID| {
      let d : &ChildData = child_data . get (id) . expect(
        "complete_content_children: child data not pre-fetched" );
      match d . kind {
        ContentReality::Real =>
          mk_definitive_viewnode(
            id . clone(), d . source . clone(),
            d . title . clone(), None ),
        ContentReality::Phantom (ex, mem) =>
          mk_phantom_viewnode(
            id . clone(), d . source . clone(),
            d . title . clone(), ex, mem ) } },
  ) }

/// 'erroneous content children' are children that look like content,
/// but that are not actually content.
/// This marks them birth=Independent.
/// (Note that phantom nodes are not content in the worktree,
/// but they are content in HEAD.
/// This does not mark such phantoms as Independent.)
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
      ViewNodeKind::True (t) =>
        !t . parent_ignores_it()
        && !content_id_set . contains( &t . id )
        && !t . is_phantom(), // phantoms, though not content in the worktree, are content in HEAD, and should not be parent-ignored
      _ => false },
    |vn : &mut ViewNode| {
      if let ViewNodeKind::True( ref mut t ) = vn . kind {
        t . birth = Birth::Independent; }},
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
        ViewNodeKind::Scaff (_)                           => 0,
        ViewNodeKind::DeletedScaff (_)                    => 0,
        ViewNodeKind::True (t) if t . parent_ignores_it() => 1,
        ViewNodeKind::True (_)                            => 2,
        ViewNodeKind::Deleted (_)                         => 2,
      } ) . map_err( |e| -> Box<dyn Error> { e . into() } ) ?;
  let empty : Vec<NodeId> = Vec::new();
  for &cid in groups . get( &0 ) . unwrap_or (&empty) . iter()
    . chain( groups . get( &1 ) . unwrap_or (&empty) . iter() )
    . chain( groups . get( &2 ) . unwrap_or (&empty) . iter() )
  { move_child_to_end( tree, node, cid ) ?; }
  Ok(( )) }

/// If the NodeComplete subscribes to anything and no SubscribeeCol
/// child exists yet, prepend an empty one.
/// (It will be populated when processing visits the SubscribeeCol.)
fn maybe_prepend_subscribee_col (
  tree          : &mut Tree<ViewNode>,
  node          : NodeId,
  subscribes_to : &[ID],
) -> Result<(), Box<dyn Error>> {
  if !subscribes_to . is_empty() {
    if unique_scaffold_child(
      tree, node, &Scaffold::SubscribeeCol
    ) ?. is_none() {
      insert_scaffold_as_child(
        tree, node, Scaffold::SubscribeeCol, true ) ?; } }
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
fn maybe_prepend_diff_view_scaffolds (
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
  let staged_text   : bool = staged_nc   . map ( |nc| nc . text_changed ) . unwrap_or (false);
  let unstaged_text : bool = unstaged_nc . map ( |nc| nc . text_changed ) . unwrap_or (false);
  if staged_text || unstaged_text {
    if unique_scaffold_child(
      tree, node,
      &Scaffold::TextChanged { staged: false, unstaged: false }
    ) ?. is_none() {
      insert_scaffold_as_child(
        tree, node,
        Scaffold::TextChanged { staged: staged_text,
                                unstaged: unstaged_text },
        true ) ?; } }
  let ids_changed : bool =
    has_list_changes (
      staged_nc   . map ( |nc| nc . ids_diff . as_slice () ),
      unstaged_nc . map ( |nc| nc . ids_diff . as_slice () ) );
  if ids_changed {
    if unique_scaffold_child(
      tree, node, &Scaffold::IDCol
    ) ?. is_none() {
      insert_scaffold_as_child(
        tree, node, Scaffold::IDCol, true ) ?; } }
  let aliases_changed : bool =
    has_list_changes (
      staged_nc   . map ( |nc| nc . aliases_diff . as_slice () ),
      unstaged_nc . map ( |nc| nc . aliases_diff . as_slice () ) );
  if aliases_changed {
    if unique_scaffold_child(
      tree, node, &Scaffold::AliasCol
    ) ?. is_none() {
      insert_scaffold_as_child(
        tree, node, Scaffold::AliasCol, true ) ?; } }
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
        if let ViewNodeKind::True (t) = &child_ref . value() . kind {
          m . insert( t . id . clone(),
                      t . source . clone()); }}
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
        find_source_many_ways(
          id, &child_sources, deleted_since_head_pid_src_map, config )
        . map_err( |e| -> Box<dyn Error> { e . into() } ) ?;
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
                                   kind } );
    } else {
      let child_source : SourceName =
        find_source_many_ways( id, &child_sources,
                     deleted_since_head_pid_src_map, config )
        . map_err( |e| -> Box<dyn Error> { e . into() } ) ?;
      let skg : NodeComplete =
        nodecomplete_from_memory_or_disk ( config, id, &child_source ) ?;
      result . insert( id . clone(),
                     ChildData { title: skg . title . clone(),
                                 source: skg . source . clone(),
                                 kind: ContentReality::Real } ); }}
  Ok (result) }

/// In diff view, set the TrueNode's per-stage diff axes.
fn maybe_change_node_diff_status (
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
  // Membership axes derive from the parent's per-stage contains_diff:
  // if pid is New(pid) in a stage, that stage's M is Plus.
  let (staged_m, unstaged_m) : (Option<Sign>, Option<Sign>) = {
    let is_non_content : bool =
      read_at_node_in_tree( tree, node,
        |vn : &ViewNode| match &vn . kind {
          ViewNodeKind::True (t) => t . parent_ignores_it(),
          _ => true } ) ?;
    if is_non_content { (None, None) }
    else {
      let parent : Option<(ID, SourceName)> =
        read_at_ancestor_in_tree( tree, node, 1,
          |vn : &ViewNode| match &vn . kind {
            ViewNodeKind::True (t) =>
              Ok(( t . id . clone(), t . source . clone() )),
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
      if let ViewNodeKind::True( ref mut t ) = vn . kind {
        t . existence  = existence;
        t . membership = membership;
        t . not_in_git = not_in_git; }}
  ) . map_err( |e| -> Box<dyn Error> { e . into() } ) }

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::git::{GitDiffStatus, NodeCompleteDiff};

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
      before_node: None, } }

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
