/// PURPOSE: BFS rendering of content views with node count limit.
///
/// MOTIVATION: Avoids some complexities of 'completeOrgnodeForest_collectingDefinitiveRequests',
/// because in an initial view the only kind of child is content.
///
/// THE ALGORITHM:
/// Start with a list of roots.
/// Render one generation at a time as definitive orgnodes.
/// Once the node limit is hit, stop rendering that generation,
/// and re-render it as indefinitive orgnodes.
/// (Re-render sounds like a lot of work but it just changes 2 fields.)
/// Once the last orgnode L has been re-rendered as indefinitive,
/// continue rendering any of its remaining siblings the same way,
/// but ignore (omit, leave unrendered) the rest of its generation.
/// Then navigate up to L's parent P,
/// and re-render every node in P's generation after P as indefinitive.

use crate::to_org::util::stub_forest_from_root_ids;
use crate::to_org::render::truncate::truncate_after_node_in_generation_in_forest;
use crate::to_org::util::{
  get_pid_in_pairtree, is_ancestor_id,
  collect_content_children, fetch_and_append_child_pair,
  rewrite_to_indefinitive };
use crate::types::{SkgConfig, ID};
use crate::types::trees::{NodePair, PairTree};

use ego_tree::{NodeId, NodeMut};
use std::cmp::min;
use std::collections::HashSet;
use std::error::Error;
use std::pin::Pin;
use std::future::Future;
use typedb_driver::TypeDBDriver;

/// The file header comment describes the algorithm.
pub async fn render_initial_forest_bfs (
  root_ids : &[ID],
  config   : &SkgConfig,
  driver   : &TypeDBDriver,
) -> Result < Vec < PairTree >,
  Box<dyn Error> > {
  let mut visited : HashSet<ID> = HashSet::new();
  let mut forest : Vec < PairTree > =
    stub_forest_from_root_ids (
      root_ids, config, driver ) . await ?;
  let root_nodes : Vec<(usize, Vec<NodeId>)> =
    forest . iter() . enumerate()
    . map(|(idx, tree)| (idx, vec![tree.root().id()]))
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
  current_gen    : Vec<(usize, Vec<NodeId>)>,
  generation     : usize,
  nodes_rendered : usize,
  limit          : usize,
  visited        : &'a mut HashSet<ID>,
  config         : &'a SkgConfig,
  driver         : &'a TypeDBDriver,
) -> Pin<Box<dyn Future<
    Output = Result<(), Box<dyn Error>>> + 'a>> {
  Box::pin ( async move {
    if current_gen.is_empty() {
      return Ok(( )); }
    let mut nodes_rendered : usize = nodes_rendered;
    for (tree_idx, node_ids) in &current_gen {
      // mark cycles and repeats
      for node_id in node_ids {
        mark_cycles_and_repeats(
          forest, *tree_idx, *node_id, visited)?;
        nodes_rendered += 1; }}
    let parent_child_rels_to_add : Vec<(usize, // the tree
                                        NodeId, // the parent
                                        ID)> = ( // the child
      collect_rels_to_children_from_generation(
        forest, &current_gen ));
    let next_gen_count : usize =
      parent_child_rels_to_add . len();
    if nodes_rendered + next_gen_count >= limit {
      add_children_with_truncation (
        forest, generation, &parent_child_rels_to_add,
        nodes_rendered, limit, config, driver, visited,
      ) . await ?;
      return Ok(( )); }
    else {
      let next_gen : Vec<(usize, Vec<NodeId>)> =
        add_children_and_collect_their_ids (
          forest, parent_child_rels_to_add, config, driver
        ) . await ?;
      render_generation_and_recurse (
        forest, next_gen, generation + 1,
        nodes_rendered, limit, visited, config, driver,
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
  let mut child_gen : Vec<(usize, Vec<NodeId>)> = Vec::new();
  for (tree_idx, parent_id, child_id) in rels_to_add {
    let child_node_id : NodeId =
      fetch_and_append_child_pair (
        &mut forest[tree_idx], parent_id, &child_id, config, driver
      ) . await ?;
    if let Some((_, ids)) =
      child_gen . iter_mut() . find(|(idx, _)| *idx == tree_idx) {
        // Group by tree_idx
        // todo ? Using 'find' is inefficient, but arguably safer,
        // than simply accessing the 'idx'th element.
        ids . push(child_node_id);
    } else {
      child_gen . push((tree_idx, vec![child_node_id])); }}
  Ok(child_gen) }

/// If node is a cycle or a repeat,
/// call rewrite_to_indefinitive on it.
/// Otherwise add it to 'visited'.
fn mark_cycles_and_repeats (
  forest     : &mut Vec<PairTree>,
  tree_idx   : usize,
  node_id    : NodeId,
  visited    : &mut HashSet<ID>,
) -> Result<(), Box<dyn Error>> {
  let tree = &mut forest[tree_idx];
  let pid : ID = get_pid_in_pairtree ( tree, node_id ) ?;
  let is_cycle : bool =
    is_ancestor_id ( tree, node_id, &pid,
                     |n| n . 1 . metadata . id . as_ref () ) ?;
  { let mut node_mut : NodeMut < NodePair > =
      tree . get_mut ( node_id )
      . ok_or ( "mark_cycles_and_repeats: node not found" ) ?;
    node_mut . value () . 1 . metadata . viewData . cycle = is_cycle; }
  if visited . contains ( &pid ) {
    rewrite_to_indefinitive ( tree, node_id ) ?; }
  else {
    visited . insert ( pid ); }
  Ok (( )) }

/// Collect all children IDs from nodes in the current generation.
/// Passes over indefinitive nodes,
///   as their contents do not need rendering.
/// Returns (tree_idx, parent_node_id, child_id) tuples.
fn collect_rels_to_children_from_generation(
  forest : &[PairTree],
  nodes_in_gen :  &[(usize,         // which tree of the forest
                     Vec<NodeId>)], // all in the same generation (even across trees, not just within them)
) -> Vec<(usize,  // an index into the forest
          NodeId, // a parent from 'nodes_in_gen'
          ID)> {  // a child of that parent
  let mut children : Vec<(usize, NodeId, ID)> = Vec::new();
  for (tree_idx, node_ids) in nodes_in_gen {
    for node_id in node_ids {
      // collect_content_children returns empty vec if indefinitive
      if let Ok ( child_ids ) =
        collect_content_children (
          &forest[*tree_idx], *node_id )
      { for child_id in child_ids {
          children . push ( (*tree_idx, *node_id, child_id) ); }}}}
  children }

/// Add children with truncation when limit is exceeded. In order:
/// - fetch and add children up to the limit as indefinitive nodes
/// - complete the sibling group of the limit node
/// - omit rest of generation (later would-be sibling groups)
async fn add_children_with_truncation(
  forest           : &mut Vec<PairTree>,
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
    let (tree_idx, parent_id, _child_id) =
      &children_to_add [last_addable_index];
    (*tree_idx, *parent_id) };
  for (idx, (tree_idx, parent_id, child_id))
    in children_to_add . iter() . enumerate() {
      if idx > last_addable_index &&
        ( *tree_idx != limit_tree_idx ||
           *parent_id != limit_parent_id )
      // That 'in a new tree' condition is needed because
      // ego_tree does not permit comparison of
      // node_ids from different trees.
      { break; }
      let child_node_id : NodeId =
        fetch_and_append_child_pair (
          &mut forest[*tree_idx], *parent_id, child_id, config, driver
        ) . await ?;
      rewrite_to_indefinitive (
        &mut forest[*tree_idx], child_node_id ) ?;
      if let Some(ref child_pid) =
        forest[*tree_idx] . get(child_node_id)
        . unwrap() . value() . 1 . metadata.id
      { visited.insert(child_pid.clone()); }}
  truncate_after_node_in_generation_in_forest(
    forest, generation, limit_tree_idx, limit_parent_id )?;
  Ok (( )) }
