/// PURPOSE: BFS rendering of content views with node count limit.
///
/// MOTIVATION: Avoids some complexities of
/// 'completeAndRestoreForest_collectingViewRequests',
/// because in an initial view the only kind of child is content.
///
/// THE ALGORITHM:
/// Start with a list of roots.
/// Render one generation at a time as definitive viewnodes.
/// Once the node limit is hit, stop rendering that generation,
/// and re-render it as indefinitive viewnodes.
/// ('Re-rendering' might sound like a lot of work,
/// but it only changes 2 fields.)
/// Once the last viewnode L has been re-rendered as indefinitive,
/// continue rendering any of its remaining siblings the same way,
/// but ignore (omit, leave unrendered) the rest of its generation.
/// Then navigate up to L's parent P,
/// and re-render as indefinitive every node in P's generation after P.

use crate::dbs::neo4j::search::pids_and_sources_from_ids;
use crate::to_org::util::{stub_forest_from_root_ids, content_ids_if_definitive_else_empty, build_node_branch_minus_content_from_pid_and_source, DefinitiveMap};
use crate::to_org::render::truncate_after_node_in_gen::add_last_generation_and_truncate_some_of_previous;
use crate::types::misc::{SkgConfig, ID, SourceName};
use crate::types::viewnode::ViewNode;
use crate::types::skgnodemap::{SkgNodeMap, IdToPidAndSource};

use ego_tree::{NodeId, Tree};
use std::error::Error;
use std::pin::Pin;
use std::future::Future;
use neo4rs::Graph;

/// See file header comment.
/// Returns a "forest" (tree with BufferRoot).
pub async fn render_initial_forest_bfs (
  root_ids : &[ID],
  config   : &SkgConfig,
  graph    : &Graph,
) -> Result < (Tree<ViewNode>, SkgNodeMap), Box<dyn Error> > {
  let mut visited : DefinitiveMap = DefinitiveMap::new();
  let (mut forest, mut map) : (Tree<ViewNode>, SkgNodeMap) =
    stub_forest_from_root_ids (
      root_ids, config, graph, &mut visited ) . await ?;
  let forest_root_id : NodeId = forest . root () . id ();
  let root_nodes : Vec < NodeId > =
    forest . root () . children ()
    . map ( |c| c . id () )
    . collect ();
  render_generation_and_recurse (
    &mut forest,
    &mut map,
    root_nodes, // the last complete generation
    1,          // the last complete generation's number
    0, // nodes_rendered (BufferRoot doesn't count)
    config.initial_node_limit,
    forest_root_id, // effective_root for truncation
    &mut visited,
    config,
    graph,
  ) . await ?;
  Ok ( (forest, map) ) }

/// Returns when the generation is empty or the limit is reached.
fn render_generation_and_recurse<'a> (
  forest         : &'a mut Tree<ViewNode>,
  map            : &'a mut SkgNodeMap,
  gen_nodeids    : Vec < NodeId >, // nodes of deepest generation rendered so far
  gen_int        : usize,          // number of deepest generation rendered so far (0 = root)
  rendered_count : usize,
  limit          : usize,
  effective_root : NodeId,         // BufferRoot for initial rendering
  visited        : &'a mut DefinitiveMap,
  config         : &'a SkgConfig,
  graph          : &'a Graph,
) -> Pin<Box<dyn Future<
    Output = Result<(), Box<dyn Error>>> + 'a>> {
  Box::pin ( async move {
    if gen_nodeids.is_empty() {
      return Ok(( )); }
    let nodes_in_gen : usize = gen_nodeids . len ();
    let rendered_count : usize = rendered_count + nodes_in_gen;
    let parent_child_rels_to_add : Vec < (NodeId, ID) > =
      collect_rels_to_children_from_generation (
        forest, map, &gen_nodeids );
    let next_gen_count : usize =
      parent_child_rels_to_add . len();
    if rendered_count + next_gen_count < limit {
      let next_gen : Vec < NodeId > =
        add_children_and_collect_their_ids (
          forest, map, parent_child_rels_to_add, visited, config, graph
        ) . await ?;
      render_generation_and_recurse (
        forest, map, next_gen, gen_int + 1,
        rendered_count, limit, effective_root,
        visited, config, graph,
      ) . await }
    else {
      add_last_generation_and_truncate_some_of_previous (
        forest, map, gen_int + 1, &parent_child_rels_to_add,
        limit - rendered_count, effective_root,
        visited, config, graph,
      ) . await ?;
      return Ok(( )); }
  } ) }

/// Add children to the forest.
/// Return their NodeIds.
/// Batch-resolves all child IDs via a single Neo4j query,
/// then builds each child without further Neo4j lookups.
async fn add_children_and_collect_their_ids (
  forest      : &mut Tree<ViewNode>,
  map         : &mut SkgNodeMap,
  rels_to_add : Vec < (NodeId, ID) >,
  visited     : &mut DefinitiveMap,
  config      : &SkgConfig,
  graph       : &Graph,
) -> Result < Vec < NodeId >, Box<dyn Error> > {
  let all_child_ids : Vec<ID> =
    rels_to_add . iter ()
    . map ( |( _, child_skgid )| child_skgid . clone () )
    . collect ();
  let pid_source_map : IdToPidAndSource =
    pids_and_sources_from_ids ( graph, &all_child_ids ) . await ?;
  let mut child_treeids : Vec < NodeId > = Vec::new ();
  for (parent_treeid, child_skgid) in rels_to_add {
    let (pid, source) : &(ID, SourceName) =
      pid_source_map . get ( &child_skgid )
      . ok_or_else ( || format! (
        "ID '{}' not found in database", child_skgid ) ) ?;
    let child_treeid : NodeId =
      build_node_branch_minus_content_from_pid_and_source (
        forest, map, parent_treeid, pid, source,
        config, graph, visited ) . await ?;
    child_treeids . push ( child_treeid ); }
  Ok ( child_treeids ) }

/// Collect all children IDs
///   from definitive nodes in the input generation.
///   (Indefinitive nodes' contents do not need rendering.)
/// Returns (parent_treeid, child_skgid) tuples.
fn collect_rels_to_children_from_generation (
  forest       : &Tree<ViewNode>,
  map          : &SkgNodeMap,
  nodes_in_gen : &[NodeId],
) -> Vec < (NodeId, ID) > {
  let mut children : Vec < (NodeId, ID) > = Vec::new ();
  for treeid in nodes_in_gen {
    if let Ok ( child_skgids ) =
      content_ids_if_definitive_else_empty (
        forest, map, *treeid )
    { for child_skgid in child_skgids {
      children . push ( (*treeid, child_skgid) ); }} }
  children }
