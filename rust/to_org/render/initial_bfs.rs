/// PURPOSE: BFS rendering of content views with node count limit.
///
/// MOTIVATION: Avoids some complexities of
/// 'completeOrgnodeForest_collectingDefinitiveRequests',
/// because in an initial view the only kind of child is content.
///
/// THE ALGORITHM:
/// Start with a list of roots.
/// Render one generation at a time as definitive orgnodes.
/// Once the node limit is hit, stop rendering that generation,
/// and re-render it as indefinitive orgnodes.
/// ('Re-rendering' might sound like a lot of work,
/// but it only changes 2 fields.)
/// Once the last orgnode L has been re-rendered as indefinitive,
/// continue rendering any of its remaining siblings the same way,
/// but ignore (omit, leave unrendered) the rest of its generation.
/// Then navigate up to L's parent P,
/// and re-render as indefinitive every node in P's generation after P.

use crate::to_org::util::stub_forest_from_root_ids;
use crate::to_org::render::truncate_after_node_in_gen::truncate_after_node_in_generation_in_forest;
use crate::to_org::util::{
  content_ids_if_definitive_else_empty,
  fetch_and_append_child_pair,
  get_pid_in_pairtree, is_ancestor_id,
  rewrite_to_indefinitive,
  VisitedMap };
use crate::types::{SkgConfig, ID};
use crate::types::trees::{NodePair, PairTree};

use ego_tree::{NodeId, NodeMut};
use std::cmp::min;
use std::error::Error;
use std::pin::Pin;
use std::future::Future;
use typedb_driver::TypeDBDriver;

/// See file header comment.
pub async fn render_initial_forest_bfs (
  root_ids : &[ID],
  config   : &SkgConfig,
  driver   : &TypeDBDriver,
) -> Result < Vec < PairTree >,
  Box<dyn Error> > {
  let mut visited : VisitedMap = VisitedMap::new();
  let mut forest : Vec < PairTree > =
    stub_forest_from_root_ids (
      root_ids, config, driver ) . await ?;
  let root_nodes : Vec<(usize, Vec<NodeId>)> =
    forest . iter() . enumerate()
    . map(|(idx, tree)|
          // 'enumerate' creates 'idx', which starts at 0.
          (idx, vec![tree.root().id()]))
    . collect();
  render_generation_and_recurse (
    &mut forest,
    root_nodes,
    1,  // generation
    0,  // nodes_rendered
    config.initial_node_limit,
    &mut visited,
    config,
    driver,
  ) . await ?;
  Ok ( forest ) }

/// Returns when the generation is empty or the limit is reached.
fn render_generation_and_recurse<'a> (
  forest         : &'a mut Vec<PairTree>,
  gen_nodeids    : Vec<(usize, // which tree (index into forest)
                        Vec<NodeId>)>,
  gen_int        : usize,
  rendered_count : usize,
  limit          : usize,
  visited        : &'a mut VisitedMap,
  config         : &'a SkgConfig,
  driver         : &'a TypeDBDriver,
) -> Pin<Box<dyn Future<
    Output = Result<(), Box<dyn Error>>> + 'a>> {
  Box::pin ( async move {
    if gen_nodeids.is_empty() {
      return Ok(( )); }
    let mut rendered_count : usize = rendered_count;
    for (tree_idx, node_ids) in &gen_nodeids {
      for node_id in node_ids {
        mark_visited_or_repeat_or_cycle(
          forest, *tree_idx, *node_id, visited)?;
        rendered_count += 1; }}
    let parent_child_rels_to_add : Vec<(usize, // the tree
                                        NodeId, // the parent
                                        ID)> = ( // the child
      collect_rels_to_children_from_generation(
        forest, &gen_nodeids ));
    let next_gen_count : usize =
      parent_child_rels_to_add . len();
    if rendered_count + next_gen_count >= limit {
      add_last_generation_truncated (
        forest, gen_int, &parent_child_rels_to_add,
        rendered_count, limit, config, driver, visited,
      ) . await ?;
      return Ok(( )); }
    else {
      let next_gen : Vec<(usize, Vec<NodeId>)> =
        add_children_and_collect_their_ids (
          forest, parent_child_rels_to_add, config, driver
        ) . await ?;
      render_generation_and_recurse (
        forest, next_gen, gen_int + 1,
        rendered_count, limit, visited, config, driver,
      ) . await }} ) }

/// Add children to the forest.
/// Return their NodeIds grouped by tree.
async fn add_children_and_collect_their_ids (
  forest      : &mut Vec<PairTree>,
  rels_to_add : Vec<( usize,  // which tree
                      NodeId, // parent
                      ID )>,  // child of that parent
  config      : &SkgConfig,
  driver      : &TypeDBDriver,
) -> Result<Vec<( usize,         // which tree
                  Vec<NodeId>)>, // children added
            Box<dyn Error>> {
  let mut child_gen : Vec<(usize, Vec<NodeId>)> =
    (0 .. forest.len())
    . map(|idx| (idx, Vec::new()))
    . collect();
  for (tree_idx, parent_id, child_id) in rels_to_add {
    let child_node_id : NodeId =
      fetch_and_append_child_pair (
        &mut forest[tree_idx], parent_id, &child_id, config, driver
      ) . await ?;
    child_gen[tree_idx] . 1 . push(child_node_id); }
  let child_gen : Vec<(usize, Vec<NodeId>)> = (
    // Filter out trees with no children added,
    // so that the caller can use .is_empty() to detect termination.
    child_gen . into_iter()
      . filter(|(_, ids)| ! ids.is_empty())
      . collect() );
  Ok(child_gen) }

/// If node repeat, call rewrite_to_indefinitive on it.
/// If it's a cycle, do that but also mark it a cycle.
/// Otherwise add it to 'visited'.
fn mark_visited_or_repeat_or_cycle (
  forest     : &mut Vec<PairTree>,
  tree_idx   : usize,
  node_id    : NodeId,
  visited    : &mut VisitedMap,
) -> Result<(), Box<dyn Error>> {
  let tree = &mut forest[tree_idx];
  let pid : ID = get_pid_in_pairtree ( tree, node_id ) ?;
  let is_cycle : bool =
    is_ancestor_id ( tree, node_id, &pid,
                     |n| n . 1 . metadata . id . as_ref () ) ?;
  { let mut node_mut : NodeMut < NodePair > =
      tree . get_mut ( node_id )
      . ok_or ( "mark_visited_or_repeat_or_cycle: node not found" ) ?;
    node_mut . value () . 1 . metadata . viewData . cycle = is_cycle; }
  if visited . contains_key ( &pid ) {
    rewrite_to_indefinitive ( tree, node_id ) ?; }
  else {
    visited . insert ( pid, (tree_idx, node_id) ); }
  Ok (( )) }

/// Collect all children IDs from nodes in the current generation.
/// Passes over indefinitive nodes,
///   as their contents do not need rendering.
/// Returns (tree_idx, parent_node_id, child_id) tuples.
fn collect_rels_to_children_from_generation(
  forest : &[PairTree],
  nodes_in_gen : &[(usize,         // which tree of the forest
                    Vec<NodeId>)], // These are all in the same generation -- even across trees, not just within them.
) -> Vec<(usize,  // an index into the forest
          NodeId, // a parent from 'nodes_in_gen'
          ID)> {  // a child of that parent
  let mut children : Vec<(usize, NodeId, ID)> = Vec::new();
  for (tree_idx, node_ids) in nodes_in_gen {
    for node_id in node_ids {
      if let Ok ( child_ids ) =
        content_ids_if_definitive_else_empty (
          &forest[*tree_idx], *node_id )
      { for child_id in child_ids {
        children . push ( (*tree_idx,
                           *node_id, child_id) ); }} }}
  children }

/// Render the last generation because limit is hit. In order:
/// - fetch and add children up to the limit as indefinitive nodes
/// - complete the sibling group of the limit node
/// - omit rest of generation (later would-be sibling groups)
/// - truncate the preceding generation after the limit-hitting node's parent
async fn add_last_generation_truncated (
  forest           : &mut Vec<PairTree>,
  generation       : usize,
  children_to_add  : &[(usize, NodeId, ID)],
  nodes_rendered   : usize,
  limit            : usize,
  config           : &SkgConfig,
  driver           : &TypeDBDriver,
  visited          : &mut VisitedMap,
) -> Result<(), Box<dyn Error>> {
  let space_left_before_limit : usize = limit - nodes_rendered;
  if space_left_before_limit < 1 { // shouldn't happen
    return Ok (( )); }
  let last_addable_index =
    min (space_left_before_limit, children_to_add . len()) - 1;
  let (limit_tree_idx, limit_parent_id) : (usize, NodeId) = {
    // identify where limit is hit
    let (tree_idx, parent_id, _child_id) =
      &children_to_add [last_addable_index];
    (*tree_idx, *parent_id) };
  for (idx, (tree_idx, parent_id, child_id))
    in children_to_add . iter() . enumerate() {
      if idx > last_addable_index &&
        ( *tree_idx != limit_tree_idx ||
           *parent_id != limit_parent_id )
      // That 'in a new tree' condition is needed because ego_tree does not permit comparison of node_ids from different trees.
      { break; }
      let child_node_id : NodeId =
        fetch_and_append_child_pair (
          &mut forest[*tree_idx], *parent_id, child_id, config, driver
        ). await ?;
      rewrite_to_indefinitive (
        &mut forest[*tree_idx], child_node_id ) ?;
      if let Some(ref child_pid) =
        forest[*tree_idx] . get(child_node_id)
        . unwrap() . value() . 1 . metadata.id
      { visited.insert( child_pid.clone(),
                        (*tree_idx, child_node_id)); }}
  truncate_after_node_in_generation_in_forest(
    forest, generation, limit_tree_idx, limit_parent_id )?;
  Ok (( )) }
