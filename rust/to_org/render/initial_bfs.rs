/// PURPOSE: BFS rendering of content views with node count limit.
///
/// MOTIVATION: Avoids some complexities of
/// 'completeAndRestoreForest_collectingViewRequests',
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
use crate::to_org::render::truncate_after_node_in_gen::add_last_generation_and_edit_previous_in_forest;
use crate::to_org::util::{
  content_ids_if_definitive_else_empty,
  build_node_branch_minus_content,
  VisitedMap };
use crate::types::{SkgConfig, ID};
use crate::types::trees::PairTree;

use ego_tree::NodeId;
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
      root_ids, config, driver, &mut visited ) . await ?;
  let root_nodes : Vec<(usize, Vec<NodeId>)> =
    forest . iter() . enumerate()
    . map(|(idx, tree)|
          // 'enumerate' creates 'idx', which starts at 0.
          (idx, vec![tree.root().id()]))
    . collect();
  render_generation_and_recurse (
    &mut forest,
    root_nodes, // the last complete generation
    1,          // the last complete generation
    0, // nodes_rendered
    config.initial_node_limit,
    &mut visited,
    config,
    driver,
  ) . await ?;
  Ok ( forest ) }

/// Returns when the generation is empty or the limit is reached.
fn render_generation_and_recurse<'a> (
  forest         : &'a mut Vec<PairTree>,
  gen_nodeids    : Vec< // deepest generation rendered so far
      (usize, // which tree (index into forest)
       Vec<NodeId>)>,
  gen_int        : usize, // deepest generation rendered so far
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
    let nodes_in_gen : usize =
      gen_nodeids . iter ()
      . map ( |(_, ids)| ids . len () )
      . sum ();
    let rendered_count : usize = rendered_count + nodes_in_gen;
    let parent_child_rels_to_add : Vec<(usize, // the tree
                                        NodeId, // the parent
                                        ID)> = ( // the child
      collect_rels_to_children_from_generation(
        forest, &gen_nodeids ));
    let next_gen_count : usize =
      parent_child_rels_to_add . len();
    if rendered_count + next_gen_count < limit {
      let next_gen : Vec<(usize, Vec<NodeId>)> =
        add_children_and_collect_their_ids (
          forest, parent_child_rels_to_add, visited, config, driver
        ) . await ?;
      render_generation_and_recurse (
        forest, next_gen, gen_int + 1,
        rendered_count, limit, visited, config, driver,
      ) . await }
    else {
      add_last_generation_and_edit_previous_in_forest (
        forest, gen_int, &parent_child_rels_to_add,
        rendered_count, limit, config, driver,
      ) . await ?;
      return Ok(( )); }
  } ) }

/// Add children to the forest.
/// Return their NodeIds grouped by tree.
async fn add_children_and_collect_their_ids (
  forest      : &mut Vec<PairTree>,
  rels_to_next_gen_to_add : Vec<( usize,  // which tree
                      NodeId, // parent
                      ID )>,  // child of that parent
  visited     : &mut VisitedMap,
  config      : &SkgConfig,
  driver      : &TypeDBDriver,
) -> Result<Vec<( usize,         // which tree
                  Vec<NodeId>)>, // children added
            Box<dyn Error>> {
  let mut child_gen : Vec<(usize, Vec<NodeId>)> =
    (0 .. forest.len())
    . map(|idx| (idx, Vec::new()))
    . collect();
  for (tree_idx, parent_id, child_id) in rels_to_next_gen_to_add {
    let (_tree, child_node_id) =
      build_node_branch_minus_content (
        Some((&mut forest[tree_idx], parent_id)),
        tree_idx, &child_id, config, driver, visited ) . await ?;
    child_gen[tree_idx] . 1 . push(child_node_id); }
  let child_gen : Vec<(usize, Vec<NodeId>)> = (
    // Filter out trees with no children added,
    // so that the caller can use .is_empty() to detect termination.
    child_gen . into_iter()
      . filter(|(_, ids)| ! ids.is_empty())
      . collect() );
  Ok(child_gen) }

/// Collect all children IDs
///   from defiitive nodes in the current generation.
///   (Indefinitive nodes' contents do not need rendering.)
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
