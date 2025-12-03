/// PURPOSE: BFS rendering of content views with node count limit.
/// This avoids complexities in 'completeOrgnodeForest',
/// because in an initial view the only kind of child is content.
///
/// THE ALGORITHM
/// Start with a list of roots.
/// Render one generation at a time as definitive orgnodes.
/// Once the node limit is hit, stop rendering that generation,
/// and re-render it as indefinitive orgnodes.
/// (Re-render sounds like a lot of work but it just changes 2 fields.)
/// Once the last orgnode has been re-rendered as indefinitive,
/// continue rendering any of its remaining siblings the same way,
/// but omit the rest of its generation.
/// Then navigate up to its parent P,
/// and navigate every node in P's generation after P as indefinitive.

mod truncate;

use crate::media::tree::collect_generation_ids_in_forest;
use crate::to_org::content_view::stub_forest_from_root_ids;
use crate::types::{SkgConfig, ID, OrgNode, SkgNode};
use crate::to_org::util::skgnode_and_orgnode_from_id;

use ego_tree::{Tree, NodeId, NodeMut, NodeRef};
use std::cmp::min;
use std::collections::HashSet;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Render initial content views using BFS traversal with node limit.
/// Renders content one generation at a time, in BFS order.
/// Creates stub forest from root IDs, then builds it out.
/// Uses (SkgNode, OrgNode) trees to avoid redundant DB fetches.
pub async fn render_initial_forest_bfs (
  root_ids : &[ID],
  config   : &SkgConfig,
  driver   : &TypeDBDriver,
) -> Result < Vec < Tree < (SkgNode, OrgNode) > >, Box<dyn Error> > {
  let mut forest : Vec < Tree < (SkgNode, OrgNode) > > =
    stub_forest_from_root_ids (
      root_ids, config, driver ) . await ?;
  let mut visited : HashSet<ID> = HashSet::new();
  let mut generation : usize = 1;
  let mut nodes_rendered : usize = 0;
  let limit : usize = config.initial_node_limit;
  loop {
    let nodes_in_gen_by_tree : Vec<(usize, Vec<NodeId>)> =
      collect_generation_ids_in_forest (
        &forest, generation)?;
    if nodes_in_gen_by_tree.is_empty() {
      break; }
    for (tree_idx, node_ids) in &nodes_in_gen_by_tree {
      for node_id in node_ids {
        process_cycles_and_repeats(
          &mut forest,
          *tree_idx,
          *node_id,
          &mut visited, )?;
        nodes_rendered += 1; }}
    let children_to_add : Vec<(usize, NodeId, ID)> =
      collect_children_from_generation(
        &forest, &nodes_in_gen_by_tree);
    let children_count : usize =
      children_to_add.len();
    if nodes_rendered + children_count >= limit {
      add_children_with_truncation (
        &mut forest,
        generation,
        &children_to_add,
        nodes_rendered,
        limit,
        config,
        driver,
        &mut visited,
      ).await?;
      break; }
    for (tree_idx, parent_id, child_id) in children_to_add {
      let (child_skgnode, child_orgnode) : (SkgNode, OrgNode) =
        skgnode_and_orgnode_from_id(
          config, driver, &child_id).await?;
      let mut parent_mut : NodeMut<(SkgNode, OrgNode)> =
        forest[tree_idx] . get_mut(parent_id) . unwrap();
      parent_mut . append((child_skgnode, child_orgnode)); }
    generation += 1; }
  Ok ( forest ) }

fn process_cycles_and_repeats (
  forest     : &mut Vec<Tree<(SkgNode, OrgNode)>>,
  tree_idx   : usize,
  node_id    : NodeId,
  visited    : &mut HashSet<ID>,
) -> Result<(), Box<dyn Error>> {
  let pid_opt : Option<ID> = {
    let node_ref =
      forest[tree_idx] . get(node_id) . unwrap();
    node_ref . value() . 1 . metadata.id . clone() };
  let Some(pid) = pid_opt else {
    return Err("Node has no ID".into()); };
  { // Check for cycles
    let is_cycle : bool = is_ancestor(
      &forest[tree_idx], node_id, &pid);
    let mut node_mut : NodeMut<(SkgNode, OrgNode)> =
      forest[tree_idx].get_mut(node_id).unwrap();
    node_mut . value() . 1 . metadata.viewData.cycle = is_cycle; }
  if visited . contains (&pid) { // It's a repeat.
    let mut node_mut : NodeMut<(SkgNode, OrgNode)> =
      forest[tree_idx] . get_mut(node_id) . unwrap();
    node_mut . value() . 1 . metadata.code.indefinitive = true;
    node_mut . value() . 1 . body = None;
    return Ok(( )); }
  visited.insert(pid);
  Ok (( )) }

/// Collect all children IDs from nodes in the current generation.
/// Returns (tree_idx, parent_node_id, child_id) tuples.
fn collect_children_from_generation(
  forest : &[Tree<(SkgNode, OrgNode)>],
  nodes_in_gen : &[(usize, // an index into the forest
                    Vec<NodeId>)], // all in the same generation
) -> Vec<(usize, // an index into the forest
          NodeId, // a parent from 'nodes_in_gen'
          ID)> { // a child of that parent
  let mut children : Vec<(usize, NodeId, ID)> = Vec::new();
  for (tree_idx, node_ids) in nodes_in_gen {
    for node_id in node_ids {
      let node_ref =
        forest[*tree_idx] . get(*node_id) . unwrap();
      if node_ref . value() . 1 . metadata.code.indefinitive {
        continue; } // OrgNode (1) is indefinitive, so ignore it.
      else if let Some(ref child_ids) =
        node_ref . value() . 0 . contains {
          // SkgNode (0) has contents. Include them.
          for child_id in child_ids {
            children . push(
              (*tree_idx, *node_id, child_id.clone() )); }} }}
  children }

/// Add children with truncation when limit is exceeded. In odrer:
/// - fetch and add children up to the limit as indefinitive nodes
/// - complete the sibling group of the limit node
/// - omit rest of generation (later would-be siblin groups)
async fn add_children_with_truncation(
  forest           : &mut Vec<Tree<(SkgNode, OrgNode)>>,
  generation       : usize,
  children_to_add  : &[(usize, NodeId, ID)],
  nodes_rendered   : usize,
  limit            : usize,
  config           : &SkgConfig,
  driver           : &TypeDBDriver,
  visited          : &mut HashSet<ID>,
) -> Result<(), Box<dyn Error>> {
  let space_left_before_limit : usize = limit - nodes_rendered;
  if space_left_before_limit < 1 { // shouldn't happen
    return Ok (( )); }
  let last_addable_index =
    min (space_left_before_limit, children_to_add . len()) - 1;
  let (limit_tree_idx, limit_parent_id) : (usize, NodeId) = {
    let (tree_idx, // index of the node to end at
         parent_id, // the parent of that node
         _child_id) =
      &children_to_add [last_addable_index];
    (*tree_idx, *parent_id) };
  for (idx, (tree_idx, parent_id, child_id))
    in children_to_add . iter() . enumerate() {
      if idx > last_addable_index && // past the index
        ( *tree_idx != limit_tree_idx || // in a new tree
           *parent_id != limit_parent_id ) // in a new sibling group
      // That 'in a new tree' condition is needed because ego_tree does not permit comparison of node_ids from different trees.
      { break; }
      let (child_skgnode, child_orgnode) : (SkgNode, OrgNode) =
        skgnode_and_orgnode_from_id(
          config, driver, child_id ). await?;
      let mut parent_mut : NodeMut<(SkgNode, OrgNode)> =
        forest[*tree_idx] . get_mut(*parent_id) . unwrap();
      let child_node_id : NodeId = // append it
        parent_mut . append((child_skgnode, child_orgnode)) . id();
      { let mut child_mut : NodeMut<(SkgNode, OrgNode)> =
          forest[*tree_idx] . get_mut(child_node_id) . unwrap();
        child_mut . value() . 1 . metadata.code.indefinitive = true;
        child_mut . value() . 1 . body = None; }
      if let Some(ref child_pid) =
        forest[*tree_idx] . get(child_node_id)
        . unwrap() . value() . 1 . metadata.id {
          visited.insert(child_pid.clone()); }}
  truncate::truncate_after_node_in_generation(
    forest,
    generation,
    limit_tree_idx,
    limit_parent_id, )?;
  Ok (( )) }

/// Check if a node with a given ID
/// is in the ancestor path of the current node.
/// This is used for cycle detection.
fn is_ancestor (
  tree : &Tree<(SkgNode, OrgNode)>,
  node_id : NodeId,
  target_id : &ID,
) -> bool {
  let node_ref : NodeRef<(SkgNode, OrgNode)> =
    tree . get (node_id) . unwrap();
  let mut current : Option<NodeRef<(SkgNode, OrgNode)>> =
    node_ref . parent();
  while let Some(parent) = current {
    if let Some(ref parent_id)
      = parent . value() . 1 . metadata.id {
        if parent_id == target_id {
          return true; }}
    current = parent . parent(); }
  false }
