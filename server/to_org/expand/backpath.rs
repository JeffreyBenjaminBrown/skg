/// PURPOSE: "Integrate" a "path" into an ViewNode tree.
/// PITFALL: Both of those terms are tricky.
/// - The 'path' is actually more general than that:
///   If at the end it forks, it includes the first layer of branches,
///   and if it cycles,
///   the first node to cycle is duplicated at the end.
/// - I say 'integrate' rather than 'insert' because some of the path,
///   maybe even all of it, might already be there.

use crate::dbs::typedb::ancestry::{ AncestryTree, ancestry_by_id_from_ids_async};
use crate::dbs::typedb::paths::{ paths_to_first_nonlinearities, PathToFirstNonlinearity};
use crate::source_sets::ActiveSourceSet;
use crate::to_org::util::{ get_id_from_treenode, nodecomplete_and_viewnode_from_id, remove_completed_view_request};

use crate::types::env::find_source_with_optional_tantivy;
use crate::types::git::MembershipAxes;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::tree::viewnode_nodecomplete::{ find_child_by_id, find_children_by_ids};
use crate::types::viewnode::ViewRequest;
use crate::types::viewnode::{ ViewNode, ViewNodeKind, ParentIs, mk_inactive_viewnode, mk_indefinitive_from_viewnode, mk_unknown_viewnode };

use ego_tree::{NodeId,Tree};
use std::collections::{HashSet, HashMap};
use std::error::Error;
use std::pin::Pin;
use std::future::Future;
use typedb_driver::TypeDBDriver;

pub async fn build_and_integrate_containerward_view_then_drop_request (
  tree          : &mut Tree<ViewNode>,

  node_id       : NodeId,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  errors        : &mut Vec < String >,
) -> Result < (), Box<dyn Error> > {
  build_and_integrate_containerward_view_then_drop_request_with_source_set (
    tree, node_id, config, typedb_driver, errors, None ) . await
}

pub async fn build_and_integrate_containerward_view_then_drop_request_with_source_set (
  tree          : &mut Tree<ViewNode>,
  node_id       : NodeId,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  errors        : &mut Vec < String >,
  active        : Option<&ActiveSourceSet>,
) -> Result < (), Box<dyn Error> > {
  let result : Result<(), Box<dyn Error>> =
    build_and_integrate_containerward_path_with_source_set (
      tree, node_id, config, typedb_driver, active ) . await;
  remove_completed_view_request (
    tree, node_id,
    ViewRequest::Containerward,
    "Failed to integrate containerward path",
    errors, result ) }

/// Integrate a containerward path into an ViewNode tree.
/// This is called on a specific node in the tree,
/// and integrates the containerward path from that node.
pub async fn build_and_integrate_containerward_path (
  tree      : &mut Tree<ViewNode>,
  node_id   : NodeId,
  config    : &SkgConfig,
  driver    : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  build_and_integrate_containerward_path_with_source_set (
    tree, node_id, config, driver, None ) . await
}

pub async fn build_and_integrate_containerward_path_with_source_set (
  tree      : &mut Tree<ViewNode>,
  node_id   : NodeId,
  config    : &SkgConfig,
  driver    : &TypeDBDriver,
  active    : Option<&ActiveSourceSet>,
) -> Result < (), Box<dyn Error> > {
  let _ : Vec<ID> = build_and_integrate_backpaths (
    tree, node_id, config, driver,
    "contains", "contained", "container",
    ParentIs::Content,
    active
  ) . await ?;
  Ok (( )) }

pub async fn build_and_integrate_sourceward_view_then_drop_request (
  tree          : &mut Tree<ViewNode>,
  node_id       : NodeId,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  errors        : &mut Vec < String >,
) -> Result < (), Box<dyn Error> > {
  build_and_integrate_sourceward_view_then_drop_request_with_source_set (
    tree, node_id, config, typedb_driver, errors, None ) . await
}

pub async fn build_and_integrate_sourceward_view_then_drop_request_with_source_set (
  tree          : &mut Tree<ViewNode>,
  node_id       : NodeId,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  errors        : &mut Vec < String >,
  active        : Option<&ActiveSourceSet>,
) -> Result < (), Box<dyn Error> > {
  let result : Result<(), Box<dyn Error>> =
    build_and_integrate_sourceward_path_with_source_set (
      tree, node_id, config, typedb_driver, active ) . await;
  remove_completed_view_request (
    tree, node_id,
    ViewRequest::Sourceward,
    "Failed to integrate sourceward path",
    errors, result ) }

/// Integrate sourceward paths into a ViewNode tree,
/// then attach containerward ancestry beneath each source node.
pub async fn build_and_integrate_sourceward_path (
  tree      : &mut Tree<ViewNode>,
  node_id   : NodeId,
  config    : &SkgConfig,
  driver    : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  build_and_integrate_sourceward_path_with_source_set (
    tree, node_id, config, driver, None ) . await
}

pub async fn build_and_integrate_sourceward_path_with_source_set (
  tree      : &mut Tree<ViewNode>,
  node_id   : NodeId,
  config    : &SkgConfig,
  driver    : &TypeDBDriver,
  active    : Option<&ActiveSourceSet>,
) -> Result < (), Box<dyn Error> > {
  let _source_pids : Vec<ID> =
    build_and_integrate_backpaths (
      tree, node_id, config, driver,
      "textlinks_to", "dest", "source",
      ParentIs::LinkTarget,
      active ) . await ?;
  attach_containerward_ancestries_to_link_sources (
    tree, node_id, config, driver, active ) . await }

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
  tree        : &mut Tree<ViewNode>,
  node_id     : NodeId,
  config      : &SkgConfig,
  driver      : &TypeDBDriver,
  relation    : &str,
  input_role  : &str,
  output_role : &str,
  parentIs       : ParentIs,
  active      : Option<&ActiveSourceSet>,
) -> Result < Vec<ID>, Box<dyn Error> > {
  let terminus_pid : ID =
    get_id_from_treenode ( tree, node_id ) ?;
  let paths : Vec<PathToFirstNonlinearity> =
    paths_to_first_nonlinearities (
      &config.db_name, driver, &terminus_pid,
      relation, input_role, output_role
    ) . await ?;
  let pids : Vec<ID> =
    extract_pids_from_paths ( &paths );
  integrate_backpaths (
    node_id, tree, paths, parentIs, config, driver, active
  ) . await ?;
  Ok (pids) }

/// At 'node_id' in 'tree', integrate 'paths' of homogenous parentIs 'parentIs'.
async fn integrate_backpaths (
  node_id : NodeId,
  tree    : &mut Tree<ViewNode>,
  paths   : Vec<PathToFirstNonlinearity>,
  parentIs   : ParentIs,

  config  : &SkgConfig,
  driver  : &TypeDBDriver,
  active  : Option<&ActiveSourceSet>,
) -> Result < (), Box<dyn Error> > {
  for p in paths {
    integrate_path_that_might_fork_or_cycle_with_source_set (
      tree, node_id,
      p.path, p.branches, p.cycle_nodes,
      config, driver, parentIs, active
    ) . await ?; }
  Ok(()) }

/// Integrate a (maybe forked or cyclic) path into an ViewNode tree,
/// using provided backpath data.
pub async fn integrate_path_that_might_fork_or_cycle (
  tree        : &mut Tree<ViewNode>,
  node_id     : NodeId,
  path        : Vec < ID >,
  branches    : HashSet < ID >,
  cycle_nodes : HashSet < ID >,
  config      : &SkgConfig,
  driver      : &TypeDBDriver,
  parentIs       : ParentIs,
) -> Result < (), Box<dyn Error> > {
  integrate_path_that_might_fork_or_cycle_with_source_set (
    tree, node_id, path, branches, cycle_nodes,
    config, driver, parentIs, None ) . await
}

pub async fn integrate_path_that_might_fork_or_cycle_with_source_set (
  tree        : &mut Tree<ViewNode>,
  node_id     : NodeId,
  path        : Vec < ID >,
  branches    : HashSet < ID >,
  cycle_nodes : HashSet < ID >,
  config      : &SkgConfig,
  driver      : &TypeDBDriver,
  parentIs       : ParentIs,
  active      : Option<&ActiveSourceSet>,
) -> Result < (), Box<dyn Error> > {
  let last_node_id : NodeId =
    integrate_linear_portion_of_path (
      tree, node_id, &path, config, driver, parentIs, active
    ). await ?;
  if ! branches . is_empty () {
    integrate_branches_in_node (
      tree, last_node_id, branches, config, driver, parentIs
      , active ). await ?;
  } else if ! cycle_nodes . is_empty () {
    // PITFALL: If there are branches, cycle nodes are ignored.
    integrate_cycle_nodes (
      tree, last_node_id, cycle_nodes, config, driver, parentIs
      , active ). await ?; }
  Ok (( )) }

/// Recursively integrate the remaining path into the tree.
/// Operates on a specific node and the remaining path.
/// Returns the NodeId of the last node in the path.
fn integrate_linear_portion_of_path<'a> (
  tree    : &'a mut Tree<ViewNode>,
  node_id : NodeId,
  path    : &'a [ID],
  config  : &'a SkgConfig,
  driver  : &'a TypeDBDriver,
  parentIs   : ParentIs,
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
          let (child_id, should_descend) =
            prepend_indefinitive_child_with_source_set (
                    tree, node_id, path_head, config, driver, parentIs
                    , active ) . await ?;
          if ! should_descend {
            return Ok (child_id); }
          child_id } };
    integrate_linear_portion_of_path ( // recurse
      tree,
      next_node_id, // we just found or inserted this
      path_tail,
      config,
      driver,
      parentIs,
      active ). await } ) }

/// Add branch nodes as children of the specified node.
/// Branches are added in sorted order (reversed for prepending).
/// Branches that are already children are skipped.
async fn integrate_branches_in_node (
  tree     : &mut Tree<ViewNode>,

  node_id  : NodeId,
  branches : HashSet < ID >,
  config   : &SkgConfig,
  driver   : &TypeDBDriver,
  parentIs    : ParentIs,
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
    prepend_indefinitive_child_with_source_set (
      tree, node_id, &branch_id, config, driver, parentIs
      , active ). await ?; }
  Ok (( )) }

/// Add cycle nodes as children of the specified node.
/// Cycle nodes already present as children are skipped.
async fn integrate_cycle_nodes (
  tree        : &mut Tree<ViewNode>,

  node_id     : NodeId,
  cycle_nodes : HashSet < ID >,
  config      : &SkgConfig,
  driver      : &TypeDBDriver,
  parentIs       : ParentIs,
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
    prepend_indefinitive_child_with_source_set (
      tree, node_id, &cycle_id, config, driver, parentIs
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

/// Walk the subtree under node_id to find every ParentIs::LinkTarget node.
/// For each, insert its containerward ancestry as subheadlines
/// with ParentIs::Content.
async fn attach_containerward_ancestries_to_link_sources (
  tree    : &mut Tree<ViewNode>,
  node_id : NodeId,
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
  active  : Option<&ActiveSourceSet>,
) -> Result<(), Box<dyn Error>> {
  // Collect LinkTarget nodes before mutating the tree.
  let links_to_nodeids : Vec<NodeId> = {
    let mut result : Vec<NodeId> = Vec::new ();
    for edge in tree . get (node_id) . unwrap () . traverse () {
      if let ego_tree::iter::Edge::Open (node_ref) = edge {
        if let ViewNodeKind::True (t) = &node_ref . value () . kind {
          if t . parentIs == ParentIs::LinkTarget {
            result . push ( node_ref . id () ); }} }}
    result };
  attach_containerward_ancestries_at_nodeids_with_source_set (
    tree, &links_to_nodeids, config, driver, active ) . await }

/// For each NodeId, look up its TrueNode pid in the tree, fetch
/// every such pid's containerward ancestry from the graph (in
/// parallel via `ancestry_by_id_from_ids_async`), and prepend any
/// `Inner`-shaped ancestry under that NodeId as indefinitive
/// `ParentIs::Content` children. NodeIds that aren't TrueNodes,
/// or whose ancestry is `Root`/`Repeated`/`DepthTruncated`, are
/// skipped.
pub async fn attach_containerward_ancestries_at_nodeids (
  tree    : &mut Tree<ViewNode>,
  nodeids : &[NodeId],
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  attach_containerward_ancestries_at_nodeids_with_source_set (
    tree, nodeids, config, driver, None ) . await
}

pub async fn attach_containerward_ancestries_at_nodeids_with_source_set (
  tree    : &mut Tree<ViewNode>,
  nodeids : &[NodeId],
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
  active  : Option<&ActiveSourceSet>,
) -> Result<(), Box<dyn Error>> {
  let pairs : Vec<(NodeId, ID)> =
    nodeids . iter ()
      . filter_map ( |nid|
        tree . get (*nid) . and_then ( |n|
          match & n . value () . kind {
            ViewNodeKind::True (t) =>
              Some ( (*nid, t . id . clone ()) ),
            _ => None } ) )
      . collect ();
  if pairs . is_empty () { return Ok (( )); }
  let ids : Vec<ID> =
    pairs . iter () . map ( |(_, id)| id . clone () ) . collect ();
  let ancestry_map : HashMap<ID, AncestryTree> =
    ancestry_by_id_from_ids_async (
      &ids, & config . db_name, driver,
      config . max_ancestry_depth ) . await;
  attach_containerward_ancestries_from_map (
    tree, &pairs, &ancestry_map, config, driver, active ) . await }

/// Inner helper: given pre-collected pairs and a pre-fetched map,
/// prepend each pair's `Inner` ancestry. Pulled out only because
/// `attach_containerward_ancestries_at_nodeids` and the surrounding
/// recursive insertion both call into the same rev-prepend loop.
async fn attach_containerward_ancestries_from_map (
  tree         : &mut Tree<ViewNode>,
  pairs        : &[(NodeId, ID)],
  ancestry_map : &HashMap<ID, AncestryTree>,
  config       : &SkgConfig,
  driver       : &TypeDBDriver,
  active       : Option<&ActiveSourceSet>,
) -> Result<(), Box<dyn Error>> {
  for ( treeid, pid ) in pairs {
    let ancestry : &AncestryTree = match ancestry_map . get (pid) {
      Some (a) => a,
      None     => continue, };
    if let AncestryTree::Inner ( _, children ) = ancestry {
      for child in children . iter () . rev () {
        insert_containerward_ancestry_tree_recursive (
          child, *treeid,
          tree, config, driver, active ) . await ?; }} }
  Ok (( )) }

/// Recursively insert an AncestryTree as indefinitive
/// Content subheadlines under the given parent.
/// Iterates children in reverse so that prepending
/// preserves the original order.
pub fn insert_containerward_ancestry_tree_recursive<'a> (
  node       : &'a AncestryTree,
  parent_nid : NodeId,
  tree       : &'a mut Tree<ViewNode>,
  config     : &'a SkgConfig,
  driver     : &'a TypeDBDriver,
  active     : Option<&'a ActiveSourceSet>,
) -> Pin<Box<dyn Future<Output = Result<(),
                                        Box<dyn Error>>> + 'a>> {
  Box::pin ( async move {
    let (child_nid, should_descend) =
      prepend_indefinitive_child_with_source_set (
        tree, parent_nid, node . id (),
        config, driver, ParentIs::Content, active
      ) . await ?;
    if ! should_descend {
      return Ok (()); }
    if let AncestryTree::Inner ( _, children ) = node {
      for child in children . iter () . rev () {
        insert_containerward_ancestry_tree_recursive (
          child, child_nid,
          tree, config, driver, active
        ) . await ?; } }
    Ok (()) } ) }

pub async fn prepend_indefinitive_child (
  tree           : &mut Tree<ViewNode>,

  parent_treeid  : NodeId,
  child_skgid    : &ID,
  config         : &SkgConfig,
  driver         : &TypeDBDriver,
  parentIs          : ParentIs,
) -> Result < NodeId, Box<dyn Error> > {
  let viewnode : ViewNode = match
    nodecomplete_and_viewnode_from_id (
      config, driver, child_skgid
    ) . await ? {
      Some ((_nc, child_viewnode)) =>
        mk_indefinitive_from_viewnode (
          child_viewnode, parentIs )
          . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?,
      None => mk_unknown_viewnode (child_skgid . clone ()), };
  let new_child_treeid : NodeId =
    tree . get_mut (parent_treeid) . unwrap ()
    . prepend (viewnode) . id ();
  Ok (new_child_treeid) }

pub async fn prepend_indefinitive_child_with_source_set (
  tree           : &mut Tree<ViewNode>,
  parent_treeid  : NodeId,
  child_skgid    : &ID,
  config         : &SkgConfig,
  driver         : &TypeDBDriver,
  parentIs       : ParentIs,
  active         : Option<&ActiveSourceSet>,
) -> Result < (NodeId, bool), Box<dyn Error> > {
  if let Some (active) = active {
    if ! active . is_all () {
      let deleted_since_head_pid_src_map : HashMap<ID, SourceName> =
        HashMap::new ();
      if let Some (source) =
        find_source_with_optional_tantivy (
          child_skgid, &deleted_since_head_pid_src_map, None, config )
      {
        if ! active . contains_source (&source) {
          let viewnode : ViewNode =
            mk_inactive_viewnode (
              child_skgid . clone (), source, MembershipAxes::default () );
          let new_child_treeid : NodeId =
            tree . get_mut (parent_treeid) . unwrap ()
            . prepend (viewnode) . id ();
          return Ok ((new_child_treeid, false)); }}}}
  let new_child_treeid : NodeId =
    prepend_indefinitive_child (
      tree, parent_treeid, child_skgid, config, driver, parentIs )
    . await ?;
  Ok ((new_child_treeid, true)) }
