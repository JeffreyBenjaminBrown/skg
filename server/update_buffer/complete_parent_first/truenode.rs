use crate::to_org::complete::contents::clobberIndefinitiveViewnode;
use crate::types::viewnode::mk_phantom_viewnode;
use crate::to_org::util::{DefinitiveMap, make_indef_if_repeat_then_extend_defmap};
use crate::types::git::{SourceDiff, NodeDiffStatus, NodeChanges, node_changes_for_truenode, GitDiffStatus};
use crate::types::list::{Diff_Item, compute_interleaved_diff, itemlist_and_removedset_from_diff};
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::phantom::{source_for_phantom, title_for_phantom, phantom_diff_status};
use crate::types::skgnode::SkgNode;
use crate::git_ops::read_repo::skgnode_from_git_head;
use crate::types::skgnodemap::{SkgNodeMap, skgnode_from_map_or_disk};
use crate::util::setlike_vector_subtraction;
use crate::types::viewnode::{
    ViewNode, ViewNodeKind, Scaffold,
    mk_definitive_viewnode};
use crate::types::tree::generic::{error_unless_node_satisfies, read_at_ancestor_in_tree, read_at_node_in_tree, write_at_node_in_tree};
use crate::types::tree::viewnode_skgnode::{
    pid_and_source_from_treenode,
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

enum ChildKind {
  Normal,      // in worktree, and in parent's content
  RemovedHere, // in worktree, but removed from parent's content
  Removed,     // not in worktree
}

struct ChildData {
  title  : String,
  source : SourceName,
  kind   : ChildKind,
}

/// TrueNode completion.
///
/// INTENDED USE: Use in the first, DFS preorder (parent-first)
/// buffer-update pass through the tree.
/// That's because the second, DFS-postorder (child-first)
/// pass will not be correct if any truenode is missing children.
///
/// WHAT IT DOES:
/// (Beware, this comment could easily go stale.)
/// - Error unless it's a truenode.
/// - make_indef_if_repeat_then_extend_defmap
/// - If it's indefinitive:
///   - clobberIndefinitiveViewnode.
/// - If it's definitive, run (in order):
///   - complete_content_children
///   - mark_erroneous_content_children_as_parent_ignores
///   - order_children_as_scaffolds_then_ignored_then_content
///   - maybe_prepend_subscribee_col
///   - maybe_prepend_diff_view_scaffolds
pub fn complete_truenode_preorder (
  node               : NodeId,
  tree               : &mut Tree<ViewNode>,
  map                : &mut SkgNodeMap,
  defmap             : &mut DefinitiveMap,
  source_diffs       : &Option<HashMap<SourceName, SourceDiff>>,
  config             : &SkgConfig,
  deleted_id_src_map : &HashMap<ID, SourceName>,
) -> Result<(), Box<dyn Error>> {
  error_unless_node_satisfies(
    tree, node, |vn : &ViewNode| matches!( &vn.kind,
                                           ViewNodeKind::True( _ )),
    "complete_truenode_preorder: expected TrueNode" ) ?;
  make_indef_if_repeat_then_extend_defmap(
    tree, node, defmap ) ?;
  let (pid, source) : (ID, SourceName) = {
    // Handle git diff view *before* the clobber-and-early-return
    // that happens to indefinitive nodes.
    let is_phantom : bool =
      // Phantoms (nodes with diff status already set, e.g. Removed,
      // RemovedHere) are display-only placeholders created during
      // content reconciliation. They need no further completion.
      read_at_node_in_tree( tree, node,
        |vn : &ViewNode| match &vn.kind {
          ViewNodeKind::True( t ) => t.is_phantom(),
          _ => false } ) ?;
    if is_phantom { return Ok(( )); }
    let (pid, source) : (ID, SourceName) =
      pid_and_source_from_treenode( tree, node,
                                    "complete_truenode_preorder" ) ?;
    maybe_change_node_diff_status(
      tree, node, &pid, source_diffs, &source)?;
    (pid, source) };
  { let is_indefinitive : bool =
      read_at_node_in_tree( tree, node,
        |vn : &ViewNode| match &vn.kind {
          ViewNodeKind::True( t ) => t.indefinitive,
          _ => false } ) ?;
    if is_indefinitive {
      clobberIndefinitiveViewnode( tree, map, node, config ) ?;
      return Ok(( )); }}
  let skgnode : &SkgNode =
    skgnode_from_map_or_disk( &pid, &source, map, config ) ?;
  let content_ids : Vec<ID> =
    skgnode.contains.clone().unwrap_or_default();
  let subscribes_to : Vec<ID> =
    skgnode.subscribes_to.clone().unwrap_or_default();
  let node_changes : Option<&NodeChanges> =
    node_changes_for_truenode( source_diffs, &pid, &source );
  let is_sub : bool = is_subscribee( tree, node ) ?;
  { let (goal_list, removed_ids, apparent_content_ids) =
      // git diff view makes a difference
      content_goal_list(
        tree, node, &content_ids, node_changes,
        is_sub, map, config ) ?;
    complete_content_children(
      tree, node, &goal_list, &removed_ids,
      source_diffs, map, config, deleted_id_src_map ) ?;
    mark_erroneous_content_children_as_parent_ignores(
      tree, node, &apparent_content_ids ) ?; }
  order_children_as_scaffolds_then_ignored_then_content(
    tree, node ) ?;
  maybe_prepend_subscribee_col(
    tree, node, &subscribes_to ) ?;
  maybe_prepend_diff_view_scaffolds(
    tree, node, node_changes ) ?;
  Ok(( )) }

/// Whether this is a non-ignored child of a SubscribeeCol.
fn is_subscribee (
  tree : &Tree<ViewNode>,
  node : NodeId,
) -> Result<bool, Box<dyn Error>> {
  let not_parent_ignores : bool =
    read_at_node_in_tree( tree, node,
      |vn : &ViewNode| match &vn.kind {
        ViewNodeKind::True( t ) => !t.parent_ignores,
        _ => false } ) ?;
  let parent_is_subscribee_col : bool =
    read_at_ancestor_in_tree( tree, node, 1,
      |vn : &ViewNode| matches!( &vn.kind,
        ViewNodeKind::Scaff( Scaffold::SubscribeeCol )))
    .unwrap_or( false );
  Ok( not_parent_ignores && parent_is_subscribee_col ) }

fn content_goal_list (
  tree          : &Tree<ViewNode>,
  node          : NodeId,
  content_ids   : &[ID],
  node_changes  : Option<&NodeChanges>,
  is_subscribee : bool,
  map           : &mut SkgNodeMap,
  config        : &SkgConfig,
) -> Result<(Vec<ID>, HashSet<ID>, Vec<ID>), Box<dyn Error>> {
  if !is_subscribee {
    let apparent_content_ids : Vec<ID> = content_ids.to_vec();
    let (goal_list, removed_ids) : (Vec<ID>, HashSet<ID>) =
      match node_changes {
        None => (content_ids.to_vec(),
                 HashSet::new()),
        Some( nc ) =>
          itemlist_and_removedset_from_diff( &nc.contains_diff ) };
    Ok(( goal_list, removed_ids, apparent_content_ids ))
  } else {
    // TODO: Currently RemovedHere means both 'removed from subscribee content' and 'hidden by subscriber'. Newhere is similarly ambiguous.
    // Better would be to introduce two new diff values, 'RemovedByHiding' and 'NewByUnhiding'. It would be dangerous to make those available where diff values are currently used, so this means adding a new type, 'Diff_Item_Hiding'.
    let (grandparent_pid, grandparent_source) : (ID, SourceName) =
      read_at_ancestor_in_tree( tree, node, 2,
        |vn : &ViewNode| match &vn.kind {
          ViewNodeKind::True( t ) =>
            Ok(( t.id.clone(), t.source.clone() )),
          _ => Err( "content_goal_list: grandparent is not a TrueNode" ) } )
      . map_err( |e| -> Box<dyn Error> { e.into() } ) ? ?;
    let worktree_hidden : Vec<ID> =
      { let grandparent_skgnode : &SkgNode =
          skgnode_from_map_or_disk(
            &grandparent_pid, &grandparent_source, map, config ) ?;
        grandparent_skgnode.hides_from_its_subscriptions
          . clone() . unwrap_or_default() };
    let apparent_content_ids : Vec<ID> =
      setlike_vector_subtraction (
        content_ids.to_vec(), &worktree_hidden );
    let (goal_list, removed_ids) : (Vec<ID>, HashSet<ID>) =
      match node_changes {
        None => (apparent_content_ids.clone(),
                 HashSet::new()),
        Some( nc ) => {
          let head_hidden : Vec<ID> =
            skgnode_from_git_head(
                &grandparent_pid, &grandparent_source, config )
              . ok()
              . and_then( |skg| skg.hides_from_its_subscriptions )
              . unwrap_or_default();
          let head_visible : Vec<ID> =
            setlike_vector_subtraction(
              nc . contains_diff . iter() .filter_map (
                  |d| match d { // If it's New, it was not in HEAD.
                    Diff_Item::Unchanged( id ) |
                      Diff_Item::Removed( id ) => Some( id.clone() ),
                    Diff_Item::New( _ ) => None } )
                . collect(),
              &head_hidden );
          let visible_diff : Vec<Diff_Item<ID>> =
            compute_interleaved_diff(
              &head_visible, &apparent_content_ids );
          itemlist_and_removedset_from_diff( &visible_diff ) }, };
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
  map                : &mut SkgNodeMap,
  config             : &SkgConfig,
  deleted_id_src_map : &HashMap<ID, SourceName>,
) -> Result<(), Box<dyn Error>> {
  let child_data : HashMap<ID, ChildData> =
    build_child_creation_data(
      tree, node, goal_list, removed_ids,
      source_diffs, map, config, deleted_id_src_map ) ?;
  complete_relevant_children_in_viewnodetree(
    tree, node,
    |vn : &ViewNode| matches!( &vn.kind,
                               ViewNodeKind::True( t )
                               if !t.parent_ignores ),
    |vn : &ViewNode| match &vn.kind {
      ViewNodeKind::True( t ) => t.id.clone(),
      _ => panic!(
        "complete_content_children: relevant child not TrueNode" ) },
    goal_list,
    |id : &ID| {
      let d : &ChildData = child_data.get( id ).expect(
        "complete_content_children: child data not pre-fetched" );
      match d.kind {
        ChildKind::Normal =>
          mk_definitive_viewnode(
            id.clone(), d.source.clone(),
            d.title.clone(), None ),
        ChildKind::RemovedHere =>
          mk_phantom_viewnode(
            id.clone(), d.source.clone(),
            d.title.clone(), NodeDiffStatus::RemovedHere ),
        ChildKind::Removed =>
          mk_phantom_viewnode(
            id.clone(), d.source.clone(),
            d.title.clone(), NodeDiffStatus::Removed ) } },
  ) }

/// 'erroneous content children' are children that look like content,
/// but that are not actually content.
/// This marks them parentIgnores.
/// (Note that phantom nodes are not content in the worktree,
/// but they are content in HEAD.
/// This does not mark such phantoms parentIgnores.)
fn mark_erroneous_content_children_as_parent_ignores (
  tree        : &mut Tree<ViewNode>,
  node        : NodeId,
  content_ids : &[ID],
) -> Result<(), Box<dyn Error>> {
  let content_id_set : HashSet<ID> =
    content_ids.iter().cloned().collect();
  treat_certain_children(
    tree, node,
    |vn : &ViewNode| match &vn.kind {
      ViewNodeKind::True( t ) =>
        !t.parent_ignores
        && !content_id_set.contains( &t.id )
        && !t.is_phantom(), // phantoms, though not content in the worktree, are content in HEAD, and should not be parent-ignored
      _ => false },
    |vn : &mut ViewNode| {
      if let ViewNodeKind::True( ref mut t ) = vn.kind {
        t.parent_ignores = true; }},
  ).map_err( |e| -> Box<dyn Error> { e.into() } ) }

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
      |vn : &ViewNode| match &vn.kind {
        ViewNodeKind::Scaff( _ )                    => 0,
        ViewNodeKind::True( t ) if t.parent_ignores => 1,
        ViewNodeKind::True( _ )                     => 2,
      } ).map_err( |e| -> Box<dyn Error> { e.into() } ) ?;
  let empty : Vec<NodeId> = Vec::new();
  for &cid in groups.get( &0 ).unwrap_or( &empty ).iter()
    .chain( groups.get( &1 ).unwrap_or( &empty ).iter() )
    .chain( groups.get( &2 ).unwrap_or( &empty ).iter() )
  { move_child_to_end( tree, node, cid ) ?; }
  Ok(( )) }

/// If the SkgNode subscribes to anything and no SubscribeeCol
/// child exists yet, prepend an empty one.
/// (It will be populated when processing visits the SubscribeeCol.)
fn maybe_prepend_subscribee_col (
  tree          : &mut Tree<ViewNode>,
  node          : NodeId,
  subscribes_to : &[ID],
) -> Result<(), Box<dyn Error>> {
  if !subscribes_to.is_empty() {
    if unique_scaffold_child(
      tree, node, &Scaffold::SubscribeeCol
    ) ?.is_none() {
      insert_scaffold_as_child(
        tree, node, Scaffold::SubscribeeCol, true ) ?; } }
  Ok(( )) }

/// In diff view, prepend scaffolds for changed fields:
/// - TextChanged (if title/body changed)
/// - IDCol with ID children (if IDs changed)
/// - AliasCol with Alias children (if aliases changed)
/// Each is only prepended if absent.
fn maybe_prepend_diff_view_scaffolds (
  tree         : &mut Tree<ViewNode>,
  node      : NodeId,
  node_changes : Option<&NodeChanges>,
) -> Result<(), Box<dyn Error>> {
  if let Some( nc ) = node_changes {
    if nc.text_changed {
      if unique_scaffold_child(
        tree, node, &Scaffold::TextChanged
      ) ?.is_none() {
        insert_scaffold_as_child(
          tree, node, Scaffold::TextChanged, true ) ?; } }
    maybe_prepend_id_col(
      tree, node, nc ) ?;
    maybe_prepend_alias_col(
      tree, node, nc ) ?; }
  Ok(( )) }

/// If the node's IDs differ between disk and HEAD,
/// prepend an empty IDCol scaffold.
/// (It will be populated when processing visits the IDCol.)
fn maybe_prepend_id_col (
  tree         : &mut Tree<ViewNode>,
  node         : NodeId,
  node_changes : &NodeChanges,
) -> Result<(), Box<dyn Error>> {
  let has_id_changes : bool = node_changes.ids_diff.iter()
    .any( |d| !matches!( d, Diff_Item::Unchanged( _ )) );
  if has_id_changes {
    if unique_scaffold_child(
      tree, node, &Scaffold::IDCol
    ) ?.is_none() {
      insert_scaffold_as_child(
        tree, node, Scaffold::IDCol, true ) ?; } }
  Ok(( )) }

/// If the node's aliases differ between disk and HEAD,
/// prepend an empty AliasCol scaffold.
/// (It will be populated when processing visits the AliasCol.)
fn maybe_prepend_alias_col (
  tree         : &mut Tree<ViewNode>,
  node         : NodeId,
  node_changes : &NodeChanges,
) -> Result<(), Box<dyn Error>> {
  let has_alias_changes : bool = node_changes.aliases_diff.iter()
    .any( |d| !matches!( d, Diff_Item::Unchanged( _ )) );
  if has_alias_changes {
    if unique_scaffold_child(
      tree, node, &Scaffold::AliasCol
    ) ?.is_none() {
      insert_scaffold_as_child(
        tree, node, Scaffold::AliasCol, true ) ?; } }
  Ok(( )) }

/// WHAT IT DOES: Maps each ID to a ChildData:
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
  map                : &mut SkgNodeMap,
  config             : &SkgConfig,
  deleted_id_src_map : &HashMap<ID, SourceName>,
) -> Result<HashMap<ID, ChildData>, Box<dyn Error>> {
  let child_sources : HashMap<ID, SourceName> =
    { let node_ref : NodeRef<ViewNode> =
        tree.get( node )
          .ok_or( "build_child_creation_data: node not found" ) ?;
      let mut m : HashMap<ID, SourceName> = HashMap::new();
      for child_ref in node_ref.children() {
        if let ViewNodeKind::True( t ) = &child_ref.value().kind {
          m.insert( t.id.clone(), t.source.clone()); }}
      m };
  let mut result : HashMap<ID, ChildData> = HashMap::new();
  for id in goal_list {
    if result.contains_key( id ) { continue; }
    let is_phantom : bool = removed_ids.contains( id );
    if is_phantom {
      let phantom_source : SourceName =
        source_for_phantom(
          id, &child_sources, deleted_id_src_map, map, config )
        .map_err( |e| -> Box<dyn Error> { e.into() } ) ?;
      let kind : ChildKind =
        match phantom_diff_status( id, &phantom_source, source_diffs.as_ref() ) {
          NodeDiffStatus::Removed     => ChildKind::Removed,
          NodeDiffStatus::RemovedHere => ChildKind::RemovedHere,
          _                           => ChildKind::RemovedHere };
      let title : String =
        title_for_phantom( id, &phantom_source, source_diffs.as_ref(), map, config );
      result.insert( id.clone(),
                     ChildData { title,
                                 source: phantom_source,
                                 kind } );
    } else {
      let child_source : &SourceName = child_sources.get( id )
        .ok_or( format!(
          "build_child_creation_data: \
           no source for {} (not in map, not a child)",
          id.0 )) ?;
      let skg : &SkgNode =
        skgnode_from_map_or_disk( id, child_source, map, config ) ?;
      result.insert( id.clone(),
                     ChildData { title: skg.title.clone(),
                                 source: skg.source.clone(),
                                 kind: ChildKind::Normal } ); }}
  Ok( result ) }

/// In diff view, set the TrueNode's 'diff' field.
fn maybe_change_node_diff_status (
  tree         : &mut Tree<ViewNode>,
  node         : NodeId,
  pid          : &ID,
  source_diffs : &Option<HashMap<SourceName, SourceDiff>>,
  source       : &SourceName,
) -> Result<(), Box<dyn Error>> {
  let source_diff : &SourceDiff =
    match source_diffs.as_ref().and_then(|d| d.get(source)) {
      Some(sd) => sd,
      None => return Ok(( )) };
  if !source_diff.is_git_repo {
    return set_truenode_diff(
      tree, node, NodeDiffStatus::NotInGit); }
  let file_path : PathBuf =
    PathBuf::from(format!("{}.skg", pid.0));
  if let Some(skgnode_diff)
    = source_diff.skgnode_diffs.get(&file_path)
    { match skgnode_diff.status {
        GitDiffStatus::Added =>
          return set_truenode_diff( tree, node,
                                    NodeDiffStatus::New),
        GitDiffStatus::Deleted =>
          return set_truenode_diff( tree, node,
                                    NodeDiffStatus::Removed),
        GitDiffStatus::Modified => { // This case that falls through to the NewHere logic. The same happens if there's no skgnode_diff (i.e. if fetching this file_path gave None).
        } }}
  { // NewHere: Node was added to its parent's contains list,
    // but the file already existed (Added would have returned above).
    let is_parent_ignores : bool =
      read_at_node_in_tree( tree, node,
        |vn : &ViewNode| match &vn.kind {
          ViewNodeKind::True( t ) => t.parent_ignores,
          _ => true } ) ?;
    if is_parent_ignores { return Ok(( )); }
    let (parent_pid, parent_source) : (ID, SourceName) =
      match read_at_ancestor_in_tree( tree, node, 1,
        |vn : &ViewNode| match &vn.kind {
          ViewNodeKind::True( t ) =>
            Ok(( t.id.clone(), t.source.clone() )),
          _ => Err( "not a TrueNode" ) })
      { Ok( Ok( r ) ) => r,
        Ok( Err( _ ) ) | Err( _ ) => return Ok(( )) };
    let parent_changes : Option<&NodeChanges> =
      node_changes_for_truenode(
        source_diffs, &parent_pid, &parent_source );
    let nc : &NodeChanges = match parent_changes {
      Some( nc ) => nc,
      None => return Ok(( )) };
    let is_new_in_contains : bool =
      nc.contains_diff.iter().any(
        |d| matches!( d, Diff_Item::New( id ) if id == pid ) );
    if is_new_in_contains {
      set_truenode_diff(
        tree, node, NodeDiffStatus::NewHere )?; }
    Ok(( )) }}

fn set_truenode_diff (
  tree   : &mut Tree<ViewNode>,
  node   : NodeId,
  status : NodeDiffStatus,
) -> Result<(), Box<dyn Error>> {
  write_at_node_in_tree( tree, node,
    |vn : &mut ViewNode| {
      if let ViewNodeKind::True( ref mut t ) = vn.kind {
        t.diff = Some( status ); }}
  ).map_err( |e| -> Box<dyn Error> { e.into() } ) }
