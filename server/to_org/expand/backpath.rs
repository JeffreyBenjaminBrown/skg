/// PURPOSE: "Integrate" a "path" into an ViewNode tree.
/// PITFALL: Both of those terms are tricky.
/// - The 'path' is actually more general than that:
///   If at the end it forks, it includes the first layer of branches,
///   and if it cycles,
///   the first node to cycle is duplicated at the end.
/// - I say 'integrate' rather than 'insert' because some of the path,
///   maybe even all of it, might already be there.

use crate::dbs::graph_queries::ancestry::{ AncestryTree, full_containerward_ancestry};
use crate::dbs::graph_queries::paths::{paths_to_first_nonlinearities, PathToFirstNonlinearity};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::source_sets::ActiveSourceSet;
use crate::to_org::util::{ get_id_from_treenode, nodecomplete_and_viewnode_from_id, remove_completed_view_request};

use crate::types::env::find_source;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::tree::viewnode_nodecomplete::{ find_child_by_id, find_children_by_ids};
use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::types::viewnode::ViewRequest;
use crate::types::viewnode::{ Birth, ViewNode, ViewNodeKind, ParentIs, mk_indefinitive_from_viewnode, mk_unknown_viewnode };
use crate::types::viewnode::Vognode;

use ego_tree::{NodeId,Tree};
use std::collections::{HashSet, HashMap};
use std::error::Error;
use std::pin::Pin;
use std::future::Future;


/// Fulfill a '(viewRequests (path ROLENAME))' request: build the
/// backpath for 'role' and drop the request. The typed role selects
/// the relation and direction, so one call site serves all nine
/// partner roles.
pub async fn build_and_integrate_path_view_then_drop_request (
  graph : &InRustGraph,
  tree          : &mut Tree<ViewNode>,
  node_id       : NodeId,
  role          : RelationRole,
  config        : &SkgConfig,
  errors        : &mut Vec < String >,
  active        : Option<&ActiveSourceSet>,
) -> Result < (), Box<dyn Error> > {
  let result : Result<(), Box<dyn Error>> =
    build_and_integrate_path_with_source_set (
      graph, tree, node_id, role, config, active ) . await;
  remove_completed_view_request (
    tree, node_id,
    ViewRequest::Path (role),
    "Failed to integrate path view",
    errors, result ) }

/// Build the backpath for one partner 'role', and -- for every role
/// EXCEPT the container role -- attach each grafted partner's
/// containerward ancestry beneath it, so the partner is shown in its
/// own container context (as sourceward has always done for link
/// sources). The container role itself IS that ancestry, so it does not
/// re-attach.
pub async fn build_and_integrate_path_with_source_set (
  graph : &InRustGraph,
  tree      : &mut Tree<ViewNode>,
  node_id   : NodeId,
  role      : RelationRole,
  config    : &SkgConfig,
  active    : Option<&ActiveSourceSet>,
) -> Result < (), Box<dyn Error> > {
  let _ : Vec<ID> = build_and_integrate_backpaths (
    graph, tree, node_id, config, role,
    Birth::Backpath (role),
    active ) . await ?;
  if role != RelationRole::CONTAINER {
    attach_containerward_ancestries_for_birth_role (
      graph, tree, node_id, role, config, active ) . await ?; }
  Ok (( )) }

/// Integrate a containerward path into a ViewNode tree (no ancestry
/// re-attach). Thin wrapper kept for callers/tests; the engine is the
/// generic 'build_and_integrate_path_with_source_set'.
pub async fn build_and_integrate_containerward_path (
  graph : &InRustGraph,
  tree      : &mut Tree<ViewNode>,
  node_id   : NodeId,
  config    : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  build_and_integrate_path_with_source_set (
    graph, tree, node_id, RelationRole::CONTAINER, config, None ) . await }

pub async fn build_and_integrate_containerward_path_with_source_set (
  graph : &InRustGraph,
  tree      : &mut Tree<ViewNode>,
  node_id   : NodeId,
  config    : &SkgConfig,
  active    : Option<&ActiveSourceSet>,
) -> Result < (), Box<dyn Error> > {
  build_and_integrate_path_with_source_set (
    graph, tree, node_id, RelationRole::CONTAINER, config, active ) . await }

/// Integrate sourceward paths (link sources of the node), attaching
/// each source's containerward ancestry. Thin wrapper over the generic
/// engine with the linkSource role.
pub async fn build_and_integrate_sourceward_path (
  graph : &InRustGraph,
  tree      : &mut Tree<ViewNode>,
  node_id   : NodeId,
  config    : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  build_and_integrate_path_with_source_set (
    graph, tree, node_id, RelationRole::LINK_SOURCE, config, None ) . await }

/// Plural 'backpaths' because if the origin
/// immediately forks in the backward direction,
/// this will generate a path at each fork.
/// Otherwise it will only generate one path.
///
/// RETURNS the deduplicated set of pids that appear anywhere in
/// the integrated paths (including branches and cycle nodes).
/// Sourceward callers use this to fetch ancestries for each
/// link source; containerward callers can ignore it.
async fn build_and_integrate_backpaths (
  graph : &InRustGraph,
  tree        : &mut Tree<ViewNode>,
  node_id     : NodeId,
  config      : &SkgConfig,
  role        : RelationRole,
  birth       : Birth,
  active      : Option<&ActiveSourceSet>,
) -> Result < Vec<ID>, Box<dyn Error> > {
  let terminus_pid : ID =
    get_id_from_treenode ( tree, node_id ) ?;
  let paths : Vec<PathToFirstNonlinearity> =
    paths_to_first_nonlinearities (graph, &terminus_pid, role, active);
  let pids : Vec<ID> =
    extract_pids_from_paths ( &paths );
  integrate_backpaths (
    graph, node_id, tree, paths, birth, config, active
  ) . await ?;
  Ok (pids) }

/// At 'node_id' in 'tree', integrate 'paths' of homogenous birth 'birth'.
async fn integrate_backpaths (
  graph : &InRustGraph,
  node_id : NodeId,
  tree    : &mut Tree<ViewNode>,
  paths   : Vec<PathToFirstNonlinearity>,
  birth   : Birth,
  config  : &SkgConfig,
  active  : Option<&ActiveSourceSet>,
) -> Result < (), Box<dyn Error> > {
  for p in paths {
    integrate_path_that_might_fork_or_cycle_with_source_set (
      graph, tree, node_id,
      p.path, p.branches, p.cycle_nodes,
      config, birth, active
    ) . await ?; }
  Ok(()) }

/// Integrate a (maybe forked or cyclic) path into an ViewNode tree,
/// using provided backpath data.
pub async fn integrate_path_that_might_fork_or_cycle (
  graph : &InRustGraph,
  tree        : &mut Tree<ViewNode>,
  node_id     : NodeId,
  path        : Vec < ID >,
  branches    : HashSet < ID >,
  cycle_nodes : HashSet < ID >,
  config      : &SkgConfig,
  birth       : Birth,
) -> Result < (), Box<dyn Error> > {
  integrate_path_that_might_fork_or_cycle_with_source_set (
    graph, tree, node_id, path, branches, cycle_nodes,
    config, birth, None ) . await
}

pub async fn integrate_path_that_might_fork_or_cycle_with_source_set (
  graph : &InRustGraph,
  tree        : &mut Tree<ViewNode>,
  node_id     : NodeId,
  path        : Vec < ID >,
  branches    : HashSet < ID >,
  cycle_nodes : HashSet < ID >,
  config      : &SkgConfig,
  birth       : Birth,
  active      : Option<&ActiveSourceSet>,
) -> Result < (), Box<dyn Error> > {
  let last_node_id : NodeId =
    integrate_linear_portion_of_path (
      graph, tree, node_id, &path, config, birth, active
    ). await ?;
  if ! branches . is_empty () {
    integrate_branches_in_node (
      graph, tree, last_node_id, branches, config, birth
      , active ). await ?;
  } else if ! cycle_nodes . is_empty () {
    // PITFALL: If there are branches, cycle nodes are ignored.
    integrate_cycle_nodes (
      graph, tree, last_node_id, cycle_nodes, config, birth
      , active ). await ?; }
  Ok (( )) }

/// Recursively integrate the remaining path into the tree.
/// Operates on a specific node and the remaining path.
/// Returns the NodeId of the last node in the path.
fn integrate_linear_portion_of_path<'a> (
  graph : &'a InRustGraph,
  tree    : &'a mut Tree<ViewNode>,
  node_id : NodeId,
  path    : &'a [ID],
  config  : &'a SkgConfig,
  birth   : Birth,
  active  : Option<&'a ActiveSourceSet>,
) -> Pin<Box<dyn Future<Output = Result<NodeId,
                                        Box<dyn Error>>> + 'a>> {
  Box::pin(async move {
    if path . is_empty () {
      return Ok (node_id); }
    let path_head : &ID = &path[0];
    let path_tail : &[ID] = &path[1..];
    let next_node_id : NodeId =
      match find_child_by_id ( tree, node_id, path_head ) {
        Some (child_treeid) => child_treeid,
        None => {
          match
            prepend_indef_indep_child_with_source_set (
                    graph, tree, node_id, path_head, config, birth
                    , active ) . await ?
          {
            Some (child_id) => child_id,
            None => return Ok (node_id), } } };
    integrate_linear_portion_of_path (graph,  // recurse
      tree,
      next_node_id, // we just found or inserted this
      path_tail,
      config,
      birth,
      active ). await } ) }

/// Add branch nodes as children of the specified node.
/// Branches are added in sorted order (reversed for prepending).
/// Branches that are already children are skipped.
async fn integrate_branches_in_node (
  graph : &InRustGraph,
  tree     : &mut Tree<ViewNode>,
  node_id  : NodeId,
  branches : HashSet < ID >,
  config   : &SkgConfig,
  birth    : Birth,
  active   : Option<&ActiveSourceSet>,
) -> Result < (), Box<dyn Error> > {
  let found_children : HashMap < ID, NodeId > =
    find_children_by_ids ( tree, node_id, &branches );
  let mut branches_to_add : Vec < ID > =
    branches . into_iter ()
    . filter ( | b |
                 ! found_children . contains_key (b) )
    . collect ();
  { // Simplifies testing. Not necessary in production.
    branches_to_add . sort (); }
  for branch_id in branches_to_add {
    prepend_indef_indep_child_with_source_set (
      graph, tree, node_id, &branch_id, config, birth
      , active ). await ?; }
  Ok (( )) }

/// Add cycle nodes as children of the specified node.
/// Cycle nodes already present as children are skipped.
async fn integrate_cycle_nodes (
  graph : &InRustGraph,
  tree        : &mut Tree<ViewNode>,
  node_id     : NodeId,
  cycle_nodes : HashSet < ID >,
  config      : &SkgConfig,
  birth       : Birth,
  active      : Option<&ActiveSourceSet>,
) -> Result < (), Box<dyn Error> > {
  let found_children : HashMap < ID, NodeId > =
    find_children_by_ids ( tree, node_id, &cycle_nodes );
  let mut to_add : Vec < ID > =
    cycle_nodes . into_iter ()
    . filter ( | c |
                 ! found_children . contains_key (c) )
    . collect ();
  { to_add . sort (); }
  for cycle_id in to_add {
    prepend_indef_indep_child_with_source_set (
      graph, tree, node_id, &cycle_id, config, birth
      , active ). await ?; }
  Ok (( )) }

/// Extract every PID from a Vec<PathToFirstNonlinearity>,
/// deduplicated.
fn extract_pids_from_paths (
  paths : &[PathToFirstNonlinearity],
) -> Vec<ID> {
  let mut seen : HashSet<ID> = HashSet::new ();
  let mut result : Vec<ID> = Vec::new ();
  for p in paths {
    for id in &p.path {
      if seen . insert ( id . clone () ) {
        result . push ( id . clone () ); } }
    for id in &p.branches {
      if seen . insert ( id . clone () ) {
        result . push ( id . clone () ); } }
    for id in &p.cycle_nodes {
      if seen . insert ( id . clone () ) {
        result . push ( id . clone () ); } } }
  result }

/// Walk the subtree under node_id to find every Birth::Backpath(role)
/// node grafted by this path build. For each, insert its containerward
/// ancestry as subheadlines with Birth::Backpath(CONTAINER), so the
/// partner is shown in its own container context.
async fn attach_containerward_ancestries_for_birth_role (
  graph : &InRustGraph,
  tree    : &mut Tree<ViewNode>,
  node_id : NodeId,
  role    : RelationRole,
  config  : &SkgConfig,
  active  : Option<&ActiveSourceSet>,
) -> Result<(), Box<dyn Error>> {
  // Collect the role's grafted partner nodes before mutating the tree.
  let role_nodeids : Vec<NodeId> = {
    let mut result : Vec<NodeId> = Vec::new ();
    for edge in tree . get (node_id) . unwrap () . traverse () {
      if let ego_tree::iter::Edge::Open (node_ref) = edge {
        if let ViewNodeKind::Vognode (Vognode::Active (t))
          = &node_ref . value () . kind
        { if t . birth == Birth::Backpath (role) {
            result . push ( node_ref . id () ); }} }}
    result };
  attach_containerward_ancestries_at_nodeids_with_source_set (
    graph, tree, &role_nodeids, config, active ) . await }

/// For each NodeId, look up its ActiveNode pid in the tree, fetch
/// every such pid's containerward ancestry from the graph (in
/// parallel via `ancestry_by_id_from_ids_async`), and prepend any
/// `Inner`-shaped ancestry under that NodeId as indefinitive
/// `Birth::Backpath(CONTAINER)` children. NodeIds that aren't ActiveNodes,
/// or whose ancestry is `Root`/`Repeated`/`DepthTruncated`, are
/// skipped.
pub async fn attach_containerward_ancestries_at_nodeids (
  graph : &InRustGraph,
  tree    : &mut Tree<ViewNode>,
  nodeids : &[NodeId],
  config  : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  attach_containerward_ancestries_at_nodeids_with_source_set (
    graph, tree, nodeids, config, None ) . await
}

pub async fn attach_containerward_ancestries_at_nodeids_with_source_set (
  graph : &InRustGraph,
  tree    : &mut Tree<ViewNode>,
  nodeids : &[NodeId],
  config  : &SkgConfig,
  active  : Option<&ActiveSourceSet>,
) -> Result<(), Box<dyn Error>> {
  let pairs : Vec<(NodeId, ID)> =
    nodeids . iter ()
      . filter_map ( |nid|
        tree . get (*nid) . and_then ( |n|
          match & n . value () . kind {
            ViewNodeKind::Vognode (Vognode::Active (t))
              => Some ( (*nid, t . id . clone ()) ),
            _ => None } ) )
      . collect ();
  if pairs . is_empty () { return Ok (( )); }
  let ids : Vec<ID> =
    pairs . iter () . map ( |(_, id)| id . clone () ) . collect ();
  let ancestry_map : HashMap<ID, AncestryTree> =
    ids . iter () . map ( |id| (id . clone (),
      full_containerward_ancestry (
        graph, id, config . max_ancestry_depth, active)) ) . collect ();
  attach_containerward_ancestries_from_map (
    graph, tree, &pairs, &ancestry_map, config, active ) . await }

/// Inner helper: given pre-collected pairs and a pre-fetched map,
/// prepend each pair's `Inner` ancestry. Pulled out only because
/// `attach_containerward_ancestries_at_nodeids` and the surrounding
/// recursive insertion both call into the same rev-prepend loop.
async fn attach_containerward_ancestries_from_map (
  graph : &InRustGraph,
  tree         : &mut Tree<ViewNode>,
  pairs        : &[(NodeId, ID)],
  ancestry_map : &HashMap<ID, AncestryTree>,
  config       : &SkgConfig,
  active       : Option<&ActiveSourceSet>,
) -> Result<(), Box<dyn Error>> {
  for ( treeid, pid ) in pairs {
    let ancestry : &AncestryTree = match ancestry_map . get (pid) {
      Some (a) => a,
      None     => continue, };
    if let AncestryTree::Inner ( _, children ) = ancestry {
      for child in children . iter () . rev () {
        insert_containerward_ancestry_tree_recursive (
          graph, child, *treeid,
          tree, config, active ) . await ?; }} }
  Ok (( )) }

/// Recursively insert an AncestryTree as indefinitive
/// Content subheadlines under the given parent.
/// Iterates children in reverse so that prepending
/// preserves the original order.
pub fn insert_containerward_ancestry_tree_recursive<'a> (
  graph : &'a InRustGraph,
  node       : &'a AncestryTree,
  parent_nid : NodeId,
  tree       : &'a mut Tree<ViewNode>,
  config     : &'a SkgConfig,
  active     : Option<&'a ActiveSourceSet>,
) -> Pin<Box<dyn Future<Output = Result<(),
                                        Box<dyn Error>>> + 'a>> {
  Box::pin ( async move {
    let child_nid : NodeId = match
      prepend_indef_indep_child_with_source_set (
        graph, tree, parent_nid, node . id (),
        config, Birth::Backpath (RelationRole::CONTAINER), active
      ) . await ?
    {
      Some (child_nid) => child_nid,
      None => return Ok (()), };
    if let AncestryTree::Inner ( _, children ) = node {
      for child in children . iter () . rev () {
        insert_containerward_ancestry_tree_recursive (
          graph, child, child_nid,
          tree, config, active
        ) . await ?; } }
    Ok (()) } ) }

pub async fn prepend_indef_indep_child (
  graph : &InRustGraph,
  tree          : &mut Tree<ViewNode>,
  parent_treeid : NodeId,
  child_skgid   : &ID,
  _config        : &SkgConfig,
  birth         : Birth,
) -> Result < NodeId, Box<dyn Error> > {
  let viewnode : ViewNode = match
    nodecomplete_and_viewnode_from_id (
      graph, child_skgid
    ) . await ? {
      Some ((_nc, child_viewnode)) =>
        mk_indefinitive_from_viewnode (
          child_viewnode, ParentIs::Independent, birth )
          . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?,
      None => mk_unknown_viewnode (child_skgid . clone ()), };
  let new_child_treeid : NodeId =
    tree . get_mut (parent_treeid) . unwrap ()
    . prepend (viewnode) . id ();
  Ok (new_child_treeid) }

pub async fn prepend_indef_indep_child_with_source_set (
  graph : &InRustGraph,
  tree          : &mut Tree<ViewNode>,
  parent_treeid : NodeId,
  child_skgid   : &ID,
  config        : &SkgConfig,
  birth         : Birth,
  active        : Option<&ActiveSourceSet>,
) -> Result < Option<NodeId>, Box<dyn Error> > {
  if let Some (active) = active {
    if ! active . is_all () {
      let deleted_since_head_pid_src_map : HashMap<ID, SourceName> =
        HashMap::new ();
      if let Some (source) =
        find_source (
          child_skgid, &deleted_since_head_pid_src_map, graph )
      { if ! active . contains_source (&source)
        { return Ok (None); }} }
    // Edge-source gating (render-and-gating, 5_plan.org): the partner
    // NODE's source (above) is not enough -- the EDGE grafting it
    // here can be recorded in a more private source than either
    // endpoint's home (a private reading-list membership between two
    // public nodes). 'birth' names the role the partner plays toward
    // whatever sits at 'parent_treeid' (the origin, for the first
    // hop; a previously-grafted partner, for a later hop or an
    // ancestry step), so the edge and its owner are derivable.
    // The same selected graph supplies edge provenance and node homes.
    if let Birth::Backpath (role) = birth {
      if let Ok (parent_pid) = get_id_from_treenode (tree, parent_treeid) {
          let source_active : bool =
            backpath_edge_source (graph, &parent_pid, child_skgid, role)
            . map ( |source| active . contains_source (&source) )
            . unwrap_or (false);
          if ! source_active { return Ok (None); }}}}
  let new_child_treeid : NodeId =
    prepend_indef_indep_child (
      graph, tree, parent_treeid, child_skgid, config, birth )
    . await ?;
  Ok (Some (new_child_treeid)) }

/// The source of the edge grafting 'partner' at backpath role 'role'
/// toward 'origin' (the node the partner is being attached under).
/// 'role' names the role the PARTNER plays (per RelationRole's doc:
/// "output_role is THIS (partner) role") -- the inverse of
/// 'other_member_pids_gated's convention, where the role belongs to
/// the node asking the question. When the partner plays the relation's
/// FIRST position (e.g. CONTAINER), the partner owns the edge (an
/// inbound partner of origin, in the "someone else's outbound list
/// names me" sense); otherwise origin owns it.
fn backpath_edge_source (
  graph   : &InRustGraph,
  origin  : &ID,
  partner : &ID,
  role    : RelationRole,
) -> Option<SourceName> {
  if role . is_first_role () {
    graph . edge_source ( partner, role . relation, origin )
  } else {
    graph . edge_source ( origin, role . relation, partner )
  } }
