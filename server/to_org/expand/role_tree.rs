/// PURPOSE: "Integrate" a role tree into a Viewnode tree (see "role
/// tree" in docs/glossary.org). Its nodes other than the origin are
/// role grafts.
/// PITFALL: The tree is drawn as paths to their first nonlinearity
/// (server/dbs/in_rust_graph/paths.rs): each follows single partners
/// until it branches, then includes the first layer of branches (so the
/// path's last viewnode is a branching viewnode: one with several
/// viewchildren), and if it cycles, the first node to cycle is
/// duplicated at the end.
/// I say 'integrate' rather than 'insert' because some of the tree,
/// maybe even all of it, might already be there.

use crate::dbs::in_rust_graph::containerward_role_tree::{ ContainerwardRoleTree, containerward_role_trees_by_skgid_from_skgids};
use crate::dbs::in_rust_graph::paths::{
  paths_to_first_nonlinearities_in_graph, PathToFirstNonlinearity};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::skgrepo_sets::ActiveSkgRepoSet;
use crate::to_org::util::{ get_skgid_from_viewnode_at, graphnode_and_viewnode_from_skgid, remove_completed_view_request};

use crate::types::misc::{ID, SkgConfig, SkgRepoName};
use crate::types::tree::viewnode_graphnode::{ find_child_by_skgid, find_children_by_skgids};
use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::types::viewnode::ViewRequest;
use crate::types::viewnode::{ Birth, Viewnode, ViewnodeKind, AffectsParent, mk_writeProtected_from_viewnode, mk_unknown_viewnode };
use crate::types::viewnode::Vognode;

use ego_tree::{NodeId,Tree};
use std::collections::{HashSet, HashMap};
use std::error::Error;


/// Fulfill a '(viewRequests (roleTree ROLENAME))' request: build the
/// role tree for 'role' and drop the request. Relation-generic -- the
/// '(relation, input_role, output_role)' triple comes from the role
/// ('RelationRole::role_tree_triple'), so one call site serves all nine
/// partner roles.
pub fn build_and_integrate_role_tree_then_drop_request (
  tree          : &mut Tree<Viewnode>,
  treeid        : NodeId,
  graph         : &InRustGraph,
  role          : RelationRole,
  config        : &SkgConfig,
  errors        : &mut Vec < String >,
  active        : Option<&ActiveSkgRepoSet>,
) -> Result < (), Box<dyn Error> > {
  let result : Result<(), Box<dyn Error>> =
    build_and_integrate_role_tree_with_skgrepo_set (
      tree, treeid, graph, role, config, active );
  remove_completed_view_request (
    tree, treeid,
    ViewRequest::RoleTree (role),
    "Failed to integrate path view",
    errors, result ) }

/// Build the role tree for one partner 'role', and -- for every role
/// EXCEPT the container role -- attach each grafted partner's
/// containerward role tree beneath it, so the partner is shown in its
/// own container context (as mentionerward has always done for link
/// skgrepos). The container role itself IS that role tree, so it does not
/// re-attach.
pub fn build_and_integrate_role_tree_with_skgrepo_set (
  tree      : &mut Tree<Viewnode>,
  treeid    : NodeId,
  graph     : &InRustGraph,
  role      : RelationRole,
  config    : &SkgConfig,
  active    : Option<&ActiveSkgRepoSet>,
) -> Result < (), Box<dyn Error> > {
  let (relation, input_role, output_role)
    : (&'static str, &'static str, &'static str) =
    role . role_tree_triple ();
  let _ : Vec<ID> = build_and_integrate_role_trees (
    tree, treeid, graph, config,
    relation, input_role, output_role,
    Birth::RoleGraft (role),
    active ) ?;
  if role != RelationRole::CONTAINER {
    attach_full_containerward_role_trees_for_birth_role (
      tree, treeid, role, graph, config, active ) ?; }
  Ok (( )) }

/// Integrate a containerward role tree into a Viewnode tree (no role tree
/// re-attach). Thin wrapper kept for callers/tests; the engine is the
/// generic 'build_and_integrate_role_tree_with_repo_set'.
pub fn build_and_integrate_containerward_role_tree (
  tree      : &mut Tree<Viewnode>,
  treeid    : NodeId,
  graph     : &InRustGraph,
  config    : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  build_and_integrate_role_tree_with_skgrepo_set (
    tree, treeid, graph, RelationRole::CONTAINER, config, None ) }

pub fn build_and_integrate_containerward_role_tree_with_skgrepo_set (
  tree      : &mut Tree<Viewnode>,
  treeid    : NodeId,
  graph     : &InRustGraph,
  config    : &SkgConfig,
  active    : Option<&ActiveSkgRepoSet>,
) -> Result < (), Box<dyn Error> > {
  build_and_integrate_role_tree_with_skgrepo_set (
    tree, treeid, graph, RelationRole::CONTAINER, config, active ) }

/// Integrate mentionerward paths (link skgrepos of the node), attaching
/// each skgrepo's containerward role tree. Thin wrapper over the generic
/// engine with the mentioner role.
pub fn build_and_integrate_mentionerward_role_tree (
  tree      : &mut Tree<Viewnode>,
  treeid    : NodeId,
  graph     : &InRustGraph,
  config    : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  build_and_integrate_role_tree_with_skgrepo_set (
    tree, treeid, graph, RelationRole::MENTIONER, config, None ) }

/// Plural 'role trees' because if the origin
/// immediately branches in the backward direction,
/// this will generate a path at each branch.
/// Otherwise it will only generate one path.
///
/// RETURNS the deduplicated set of pids that appear anywhere in
/// the integrated paths (including branches and cycle nodes).
/// Mentionerward callers use this to fetch role trees for each
/// link skgrepo; containerward callers can ignore it.
fn build_and_integrate_role_trees (
  tree        : &mut Tree<Viewnode>,
  treeid      : NodeId,
  graph       : &InRustGraph,
  config      : &SkgConfig,
  relation    : &str,
  input_role  : &str,
  output_role : &str,
  birth       : Birth,
  active      : Option<&ActiveSkgRepoSet>,
) -> Result < Vec<ID>, Box<dyn Error> > {
  let terminus_pid : ID =
    get_skgid_from_viewnode_at ( tree, treeid ) ?;
  let paths : Vec<PathToFirstNonlinearity> =
    paths_to_first_nonlinearities_in_graph (
      graph, active, &terminus_pid, relation, input_role, output_role )?;
  let pids : Vec<ID> =
    extract_pids_from_paths ( &paths );
  integrate_role_trees (
    treeid, tree, graph, paths, birth, config, active
  ) ?;
  Ok (pids) }

/// At 'treeid' in 'tree', integrate 'paths' of homogenous birth 'birth'.
fn integrate_role_trees (
  treeid  : NodeId,
  tree    : &mut Tree<Viewnode>,
  graph   : &InRustGraph,
  paths   : Vec<PathToFirstNonlinearity>,
  birth   : Birth,
  config  : &SkgConfig,
  active  : Option<&ActiveSkgRepoSet>,
) -> Result < (), Box<dyn Error> > {
  for p in paths {
    integrate_path_that_might_branch_or_cycle_with_skgrepo_set (
      tree, treeid,
      p.path, p.branches, p.cycle_nodes,
      graph, config, birth, active
    ) ?; }
  Ok(()) }

/// Integrate a (maybe branching or cyclic) path into an Viewnode tree,
/// using provided role tree data.
pub fn integrate_path_that_might_branch_or_cycle (
  tree        : &mut Tree<Viewnode>,
  treeid      : NodeId,
  path        : Vec < ID >,
  branches    : HashSet < ID >,
  cycle_nodes : HashSet < ID >,
  graph       : &InRustGraph,
  config      : &SkgConfig,
  birth       : Birth,
) -> Result < (), Box<dyn Error> > {
  integrate_path_that_might_branch_or_cycle_with_skgrepo_set (
    tree, treeid, path, branches, cycle_nodes,
    graph, config, birth, None )
}

pub fn integrate_path_that_might_branch_or_cycle_with_skgrepo_set (
  tree        : &mut Tree<Viewnode>,
  treeid      : NodeId,
  path        : Vec < ID >,
  branches    : HashSet < ID >,
  cycle_nodes : HashSet < ID >,
  graph       : &InRustGraph,
  config      : &SkgConfig,
  birth       : Birth,
  active      : Option<&ActiveSkgRepoSet>,
) -> Result < (), Box<dyn Error> > {
  let last_treeid : NodeId =
    integrate_linear_portion_of_path (
      tree, treeid, &path, graph, config, birth, active
    ) ?;
  if ! branches . is_empty () {
    integrate_branches_in_node (
      tree, last_treeid, branches, graph, config, birth
      , active ) ?;
  } else if ! cycle_nodes . is_empty () {
    // PITFALL: If there are branches, cycle nodes are ignored.
    integrate_cycle_nodes (
      tree, last_treeid, cycle_nodes, graph, config, birth
      , active ) ?; }
  Ok (( )) }

/// Recursively integrate the remaining path into the tree.
/// Operates on a specific node and the remaining path.
/// Returns the NodeId of the last node in the path.
fn integrate_linear_portion_of_path (
  tree    : &mut Tree<Viewnode>,
  treeid  : NodeId,
  path    : &[ID],
  graph   : &InRustGraph,
  config  : &SkgConfig,
  birth   : Birth,
  active  : Option<&ActiveSkgRepoSet>,
) -> Result<NodeId, Box<dyn Error>> {
    if path . is_empty () {
      return Ok (treeid); }
    let path_head : &ID = &path[0];
    let path_tail : &[ID] = &path[1..];
    let next_treeid : NodeId =
      match find_child_by_skgid ( tree, treeid, path_head ) {
        Some (child_treeid) => child_treeid,
        None => {
          match
            prepend_writeProtected_indep_child_with_skgrepo_set (
                    tree, treeid, path_head, graph, config, birth
                    , active ) ?
          {
            Some (child_skgid) => child_skgid,
            None => return Ok (treeid), } } };
    integrate_linear_portion_of_path ( // recurse
      tree,
      next_treeid, // we just found or inserted this
      path_tail,
      graph,
      config,
      birth,
      active ) }

/// Add branch nodes as children of the specified node, which thereby
/// becomes a branching viewnode (if there are at least two).
/// Branches are added in sorted order (reversed for prepending).
/// Branches that are already children are skipped.
fn integrate_branches_in_node (
  tree     : &mut Tree<Viewnode>,

  treeid   : NodeId,
  branches : HashSet < ID >,
  graph    : &InRustGraph,
  config   : &SkgConfig,
  birth    : Birth,
  active   : Option<&ActiveSkgRepoSet>,
) -> Result < (), Box<dyn Error> > {
  let found_children : HashMap < ID, NodeId > =
    find_children_by_skgids ( tree, treeid, &branches );
  let mut branches_to_add : Vec < ID > =
    branches . into_iter ()
    . filter ( | b |
                 ! found_children . contains_key (b) )
    . collect ();
  { // Simplifies testing. Not necessary in production.
    branches_to_add . sort (); }
  for branch_skgid in branches_to_add {
    prepend_writeProtected_indep_child_with_skgrepo_set (
      tree, treeid, &branch_skgid, graph, config, birth
      , active ) ?; }
  Ok (( )) }

/// Add cycle nodes as children of the specified node.
/// Cycle nodes already present as children are skipped.
fn integrate_cycle_nodes (
  tree        : &mut Tree<Viewnode>,
  treeid      : NodeId,
  cycle_nodes : HashSet < ID >,
  graph       : &InRustGraph,
  config      : &SkgConfig,
  birth       : Birth,
  active      : Option<&ActiveSkgRepoSet>,
) -> Result < (), Box<dyn Error> > {
  let found_children : HashMap < ID, NodeId > =
    find_children_by_skgids ( tree, treeid, &cycle_nodes );
  let mut to_add : Vec < ID > =
    cycle_nodes . into_iter ()
    . filter ( | c |
                 ! found_children . contains_key (c) )
    . collect ();
  { to_add . sort (); }
  for cycle_skgid in to_add {
    prepend_writeProtected_indep_child_with_skgrepo_set (
      tree, treeid, &cycle_skgid, graph, config, birth
      , active ) ?; }
  Ok (( )) }

/// Extract every PID from a Vec<PathToFirstNonlinearity>,
/// deduplicated.
fn extract_pids_from_paths (
  paths : &[PathToFirstNonlinearity],
) -> Vec<ID> {
  let mut seen : HashSet<ID> = HashSet::new ();
  let mut result : Vec<ID> = Vec::new ();
  for p in paths {
    for skgid in &p.path {
      if seen . insert ( skgid . clone () ) {
        result . push ( skgid . clone () ); } }
    for skgid in &p.branches {
      if seen . insert ( skgid . clone () ) {
        result . push ( skgid . clone () ); } }
    for skgid in &p.cycle_nodes {
      if seen . insert ( skgid . clone () ) {
        result . push ( skgid . clone () ); } } }
  result }

/// Walk the subtree under treeid to find every Birth::RoleGraft(role)
/// node grafted by this path build. For each, insert its containerward
/// role tree as subheadlines with Birth::RoleGraft(CONTAINER), so the
/// partner is shown in its own container context.
fn attach_full_containerward_role_trees_for_birth_role (
  tree    : &mut Tree<Viewnode>,
  treeid  : NodeId,
  role    : RelationRole,
  graph   : &InRustGraph,
  config  : &SkgConfig,
  active  : Option<&ActiveSkgRepoSet>,
) -> Result<(), Box<dyn Error>> {
  // Collect the role's grafted partner nodes before mutating the tree.
  let role_treeids : Vec<NodeId> = {
    let mut result : Vec<NodeId> = Vec::new ();
    for edge in tree . get (treeid) . unwrap () . traverse () {
      if let ego_tree::iter::Edge::Open (node_ref) = edge {
        if let ViewnodeKind::Vognode (Vognode::Active (t))
          = &node_ref . value () . kind
        { if t . birth == Birth::RoleGraft (role) {
            result . push ( node_ref . id () ); }} }}
    result };
  attach_full_containerward_role_trees_at_treeids_with_skgrepo_set (
    tree, &role_treeids, graph, config, active ) }

/// For each NodeId, look up its ActiveVognode pid in the tree, fetch
/// every such pid's containerward role tree from the graph (in
/// parallel via `containerward_role_trees_by_id_from_ids`), and prepend any
/// `Inner`-shaped role tree under that NodeId as write-protected
/// `Birth::RoleGraft(CONTAINER)` children. NodeIds that aren't ActiveVognodes,
/// or whose role tree is `Root`/`Repeated`/`DepthTruncated`, are
/// skipped.
pub fn attach_full_containerward_role_trees_at_treeids (
  tree    : &mut Tree<Viewnode>,
  treeids : &[NodeId],
  graph   : &InRustGraph,
  config  : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  attach_full_containerward_role_trees_at_treeids_with_skgrepo_set (
    tree, treeids, graph, config, None )
}

pub fn attach_full_containerward_role_trees_at_treeids_with_skgrepo_set (
  tree    : &mut Tree<Viewnode>,
  treeids : &[NodeId],
  graph   : &InRustGraph,
  config  : &SkgConfig,
  active  : Option<&ActiveSkgRepoSet>,
) -> Result<(), Box<dyn Error>> {
  let pairs : Vec<(NodeId, ID)> =
    treeids . iter ()
      . filter_map ( |treeid|
        tree . get (*treeid) . and_then ( |n|
          match & n . value () . kind {
            ViewnodeKind::Vognode (Vognode::Active (t))
              => Some ( (*treeid, t . skgid . clone ()) ),
            _ => None } ) )
      . collect ();
  if pairs . is_empty () { return Ok (( )); }
  let skgids : Vec<ID> =
    pairs . iter () . map ( |(_, skgid)| skgid . clone () ) . collect ();
  let role_trees_by_skgid : HashMap<ID, ContainerwardRoleTree> =
    containerward_role_trees_by_skgid_from_skgids (
      graph, &skgids, config . max_role_tree_depth );
  attach_full_containerward_role_trees_from_map (
    tree, &pairs, &role_trees_by_skgid, graph, config, active ) }

/// Inner helper: given pre-collected pairs and a pre-fetched map,
/// prepend each pair's `Inner` role tree. Pulled out only because
/// `attach_full_containerward_role_trees_at_treeids` and the surrounding
/// recursive insertion both call into the same rev-prepend loop.
fn attach_full_containerward_role_trees_from_map (
  tree                : &mut Tree<Viewnode>,
  pairs               : &[(NodeId, ID)],
  role_trees_by_skgid : &HashMap<ID, ContainerwardRoleTree>,
  graph               : &InRustGraph,
  config              : &SkgConfig,
  active              : Option<&ActiveSkgRepoSet>,
) -> Result<(), Box<dyn Error>> {
  for ( treeid, pid ) in pairs {
    let role_tree : &ContainerwardRoleTree = match role_trees_by_skgid . get (pid) {
      Some (a) => a,
      None     => continue, };
    if let ContainerwardRoleTree::Inner ( _, children ) = role_tree {
      for child in children . iter () . rev () {
        insert_full_containerward_role_tree_recursive (
          child, *treeid,
          tree, graph, config, active ) ?; }} }
  Ok (( )) }

/// Recursively insert an ContainerwardRoleTree as write-protected
/// Content subheadlines under the given parent.
/// Iterates children in reverse so that prepending
/// preserves the original order.
pub fn insert_full_containerward_role_tree_recursive (
  node       : &ContainerwardRoleTree,
  parent_treeid : NodeId,
  tree       : &mut Tree<Viewnode>,
  graph      : &InRustGraph,
  config     : &SkgConfig,
  active     : Option<&ActiveSkgRepoSet>,
) -> Result<(), Box<dyn Error>> {
    let child_treeid : NodeId = match
      prepend_writeProtected_indep_child_with_skgrepo_set (
        tree, parent_treeid, node . skgid (),
        graph, config, Birth::RoleGraft (RelationRole::CONTAINER), active
      ) ?
    {
        Some (child_treeid) => child_treeid,
      None => return Ok (()), };
    if let ContainerwardRoleTree::Inner ( _, children ) = node {
      for child in children . iter () . rev () {
        insert_full_containerward_role_tree_recursive (
          child, child_treeid,
          tree, graph, config, active
        ) ?; } }
    Ok (()) }

pub fn prepend_writeProtected_indep_child (
  tree          : &mut Tree<Viewnode>,
  parent_treeid : NodeId,
  child_skgid   : &ID,
  graph         : &InRustGraph,
  config        : &SkgConfig,
  birth         : Birth,
) -> Result < NodeId, Box<dyn Error> > {
  let viewnode : Viewnode = match
    graphnode_and_viewnode_from_skgid (
      graph, config, child_skgid
    ) ? {
      Some ((_nc, child_viewnode)) =>
        mk_writeProtected_from_viewnode (
          child_viewnode, AffectsParent::False, birth )
          . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?,
      None => mk_unknown_viewnode (child_skgid . clone ()), };
  let new_child_treeid : NodeId =
    tree . get_mut (parent_treeid) . unwrap ()
    . prepend (viewnode) . id ();
  Ok (new_child_treeid) }

pub fn prepend_writeProtected_indep_child_with_skgrepo_set (
  tree          : &mut Tree<Viewnode>,
  parent_treeid : NodeId,
  child_skgid   : &ID,
  graph         : &InRustGraph,
  config        : &SkgConfig,
  birth         : Birth,
  active        : Option<&ActiveSkgRepoSet>,
) -> Result < Option<NodeId>, Box<dyn Error> > {
  if let Some (active) = active {
    if ! active . is_all () {
      if let Some (skgrepo) = graph . pid_and_skgrepo (child_skgid)
        . map (|(_, skgrepo)| skgrepo)
        . or_else (|| crate::types::phantom::home_from_disk (child_skgid, config))
      { if ! active . contains_skgrepo (&skgrepo)
        { return Ok (None); }} }
    // relRepo gating (render-and-gating, 5_plan.org): the partner
    // NODE's skgrepo (above) is not enough -- the RELATIONSHIP grafting it
    // here can be recorded in a more private skgrepo than either
    // endpoint's home (a private reading-list membership between two
    // public nodes). 'birth' names the role the partner plays toward
    // whatever sits at 'parent_treeid' (the origin, for the first
    // hop; a previously-grafted partner, for a later hop or an
    // role-tree step), so the relationship and its recorder are derivable.
    // The captured graph is the authoritative home of relationship relRepos.
    if let Birth::RoleGraft (role) = birth {
      if let Ok (parent_pid) = get_skgid_from_viewnode_at (tree, parent_treeid) {
          let skgrepo_active : bool =
            role_graft_relRepo (graph, &parent_pid, child_skgid, role)
            . map ( |skgrepo| active . contains_skgrepo (&skgrepo) )
            . unwrap_or (false);
          if ! skgrepo_active { return Ok (None); }}}}
  let new_child_treeid : NodeId =
    prepend_writeProtected_indep_child (
      tree, parent_treeid, child_skgid, graph, config, birth )
 ?;
  Ok (Some (new_child_treeid)) }

/// The relRepo of the relationship grafting 'partner' at role tree role 'role'
/// toward 'origin' (the node the partner is being attached under).
/// 'role' names the role the PARTNER plays (per RelationRole's doc:
/// "output_role is THIS (partner) role") -- the inverse of
/// 'other_member_pids_gated's convention, where the role belongs to
/// the node asking the question. When the partner plays the relation's
/// FIRST position (e.g. CONTAINER), the partner owns the relationship (an
/// inbound partner of origin, in the "someone else's outbound list
/// names me" sense); otherwise origin owns it.
fn role_graft_relRepo (
  graph   : &InRustGraph,
  origin  : &ID,
  partner : &ID,
  role    : RelationRole,
) -> Option<SkgRepoName> {
  if role . is_first_role () {
    graph . relRepo ( partner, role . relation, origin )
  } else {
    graph . relRepo ( origin, role . relation, partner )
  } }
