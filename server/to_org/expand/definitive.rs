use crate::dbs::filesystem::one_node::optnodecomplete_from_id;
use crate::dbs::typedb::nodes::which_ids_exist;
use crate::dbs::typedb::paths::containerward_path_stats_bulk;
use crate::git_ops::read_repo::nodecomplete_from_index_or_head;
use crate::to_org::complete::sharing::{ maybe_add_hiddenInSubscribeeCol_branch, type_and_parent_type_consistent_with_subscribee };
use crate::to_org::expand::aliases::build_and_integrate_aliases_view_then_drop_request;
use crate::to_org::expand::backpath::{ build_and_integrate_containerward_view_then_drop_request, build_and_integrate_sourceward_view_then_drop_request};
use crate::to_org::render::truncate_after_node_in_gen::add_last_generation_and_truncate_some_of_previous;
use crate::to_org::util::{ DefinitiveMap, build_node_branch_minus_content, get_id_from_treenode, makeIndefinitiveAndClobber, truenode_in_tree_is_indefinitive, content_ids_if_definitive_else_empty };
use crate::types::git::{ExistenceAxes, MembershipAxes, Sign};
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::viewnode::{ ViewNode, ViewNodeKind, ViewRequest, ContainerwardPathStats, IndefOrDef, Birth, mk_indefinitive_viewnode };
use crate::types::nodes::complete::NodeComplete;
use crate::types::memory::nodecomplete_from_memory_or_disk;
use crate::types::tree::generic::read_at_node_in_tree;
use crate::types::tree::viewnode_nodecomplete::{write_at_truenode_in_tree, pid_and_source_from_treenode};

use ego_tree::{Tree, NodeId, NodeRef, NodeMut};
use std::collections::{BTreeSet,HashSet,HashMap};
use std::error::Error;
use typedb_driver::TypeDBDriver;

pub async fn execute_view_requests (
  forest        : &mut Tree<ViewNode>,
  requests      : Vec < (NodeId, ViewRequest) >,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  visited       : &mut DefinitiveMap,
  errors        : &mut Vec < String >,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
) -> Result < (), Box<dyn Error> > {
  for (node_id, request) in requests {
    match request {
      ViewRequest::Aliases => {
        build_and_integrate_aliases_view_then_drop_request (
          forest, node_id, config, typedb_driver, errors )
          . await ?; },
      ViewRequest::Containerward => {
        build_and_integrate_containerward_view_then_drop_request (
          forest, node_id, config, typedb_driver, errors )
          . await ?; },
      ViewRequest::Sourceward => {
        build_and_integrate_sourceward_view_then_drop_request (
          forest, node_id, config, typedb_driver, errors )
          . await ?; },
      ViewRequest::Definitive => {
        execute_definitive_view_request (
          forest, node_id, config, typedb_driver,
          visited, errors,
          deleted_since_head_pid_src_map ) . await ?; },
      ViewRequest::ContainerwardStats => {
        execute_containerwardstats_request (
          forest, node_id, config, typedb_driver ) . await ?; }, }}
  Ok (( )) }

/// Compute containerward path stats for a node and store them,
/// then remove the view request.
async fn execute_containerwardstats_request (
  tree          : &mut Tree<ViewNode>,
  node_id       : NodeId,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let node_pid : ID =
    get_id_from_treenode ( tree, node_id ) ?;
  let cp_map : HashMap<ID, ContainerwardPathStats> =
    containerward_path_stats_bulk (
      & config . db_name, typedb_driver,
      & [ node_pid . clone () ] ) . await ?;
  let cp : ContainerwardPathStats =
    cp_map . get (& node_pid)
    . cloned ()
    . unwrap_or ( ContainerwardPathStats {
      length : 0,
      forks  : 1,
      cycles : false } );
  write_at_truenode_in_tree (
    tree, node_id, |t| {
      t . graphStats . containerwardPath = Some (cp);
      t . view_requests . remove (& ViewRequest::ContainerwardStats); } )
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  Ok (( )) }

/// BEHAVIOR:
/// In serial, this function:
/// - 'Indefinitizes' any previously definitive ViewNode with that ID, and its *entire content subtree*. (Otherwise the requested definitive view would have indefinitive children and expand no further.)
/// - marks this ViewNode definitive
/// - expands (BFS) its content from disk (or from latest git commit for removed nodes), applying the node limit
/// - removes its ViewRequest::Definitive
/// For Subscribee nodes: the subscriber's 'hides_from_its_subscriptions' list is used to filter out content that should be hidden.
/// For nodes with `diff: Some(Removed)`: loads content from git HEAD instead of disk, and marks children as removed if they don't exist on disk.
/// .
/// PITFALL: The indefinitization step can miss things: If definitive A contains indefinitive B, and elsewhere there is a definitive B, then when one requests a definitive view of A somewhere else, it will indefinitize the earlier A, but it will not indefinitize the earlier B. This seems fine -- searching through the whole tree for duplicates of A's recursive content would be expensive, and unless the user has strange habits they will at most rarely encounter this behavior.
async fn execute_definitive_view_request (
  forest        : &mut Tree<ViewNode>, // "forest" = tree with BufferRoot
  node_id       : NodeId,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  visited       : &mut DefinitiveMap,
  _errors       : &mut Vec < String >,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
) -> Result < (), Box<dyn Error> > {
  let node_pid : ID = get_id_from_treenode (
    forest, node_id ) ?;
  let is_removed_node : bool = // for git diff view
    read_at_node_in_tree (
      forest, node_id,
      |n| match &n . kind {
        ViewNodeKind::True (t) =>
        // The truenode's '.skg' file is gone in the worktree.
        // Body must be loaded from index (preferred) or HEAD.
          t . existence . unstaged == Some (Sign::Minus),
        _ => false } ) ?;
  let hidden_ids : HashSet < ID > =
    // If node is a subscribee, we may need to hide some content.
    get_hidden_ids_if_subscribee ( forest, node_id, config ) ?;
  if let Some (&preexisting_node_id) =
    visited . get (& node_pid)
  { if preexisting_node_id != node_id {
    indefinitize_content_subtree ( forest,
                                   preexisting_node_id,
                                   visited, config ) ?; }}
  { // Remove request, mark definitive, replace title/body, add to visited.
    write_at_truenode_in_tree (
      forest, node_id, |t| {
        t . view_requests . remove (& ViewRequest::Definitive);
        t . indef_or_def = IndefOrDef::Definitive {
          body         : None,
          edit_request : None }; } )
      . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
    if is_removed_node
      { from_git_replace_title_body (
          // PITFALL: This per-node git lookup might be slow. Hard to see how to batch the lookoup, though, since it's from git.
          forest, node_id, config ) ?; }
      else { from_disk_replace_title_body_and_nodecomplete (
               forest, node_id, config ) ?; }
    visited . insert ( node_pid . clone(), node_id ); }
  if is_removed_node {
    extendDefinitiveSubtree_fromGit (
      forest, node_id, config . initial_node_limit,
      visited, config, &hidden_ids, typedb_driver,
      deleted_since_head_pid_src_map ) . await ?; }
  else {
    extendDefinitiveSubtreeFromLeaf (
      forest, node_id, config . initial_node_limit,
      visited, config, typedb_driver, &hidden_ids ) . await ?;
    if type_and_parent_type_consistent_with_subscribee (
      forest, node_id ) ?
    { maybe_add_hiddenInSubscribeeCol_branch (
        forest, node_id, config, typedb_driver
      ) . await ?; } }
  Ok (( )) }

/// If the node is a Subscribee
/// (child of SubscribeeCol, grandchild of Subscriber),
/// returns the Subscriber's 'hides_from_its_subscriptions'.
/// Otherwise returns the empty set.
fn get_hidden_ids_if_subscribee (
  tree    : &Tree<ViewNode>,
  node_id : NodeId,
  config  : &SkgConfig,
) -> Result < HashSet < ID >, Box<dyn Error> > {
  let node_ref : NodeRef < ViewNode > =
    tree . get (node_id)
    . ok_or ("get_hidden_ids_if_subscribee: node not found") ?;
  if !type_and_parent_type_consistent_with_subscribee (
       tree, node_id )?
  { // Don't throw an error: 'if_subscribee' is in the function name.
    return Ok ( HashSet::new () ); }
  else {
    let subscribee_col : NodeRef < ViewNode > =
      node_ref . parent ()
      . ok_or ("get_hidden_ids_if_subscribee: Subscribee has no parent (SubscribeeCol)") ?;
    let subscriber : NodeRef < ViewNode > =
      subscribee_col . parent ()
      . ok_or ("get_hidden_ids_if_subscribee: SubscribeeCol has no parent (Subscriber)") ?;
    let (subscriber_id, subscriber_source) : (ID, SourceName) =
      pid_and_source_from_treenode (
        tree, subscriber . id(),
        "get_hidden_ids_if_subscribee" ) ?;
    let nodecomplete : NodeComplete =
      nodecomplete_from_memory_or_disk (
        config, &subscriber_id, &subscriber_source ) ?;
    let hidden_ids : HashSet < ID > =
      nodecomplete . hides_from_its_subscriptions
        . or_default ()
        . iter () . cloned () . collect ();
    Ok (hidden_ids) }}

/// Does two things:
/// - Mark a node, and its entire content subtree, as indefinitive.
/// - Remove them from `visited`.
/// Only recurses into non-ignored TrueNode children;
///   ignored and scaffold children persist unchanged.
/// TODO : This will need complication to properly handle
///   sharing-related nodes among the input node's descendents.
fn indefinitize_content_subtree (
  tree    : &mut Tree<ViewNode>,
  node_id : NodeId,
  visited : &mut DefinitiveMap,
  config  : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  let (node_pid, content_child_treeids)
    : (ID, Vec <NodeId>) =
    { let node_ref : NodeRef < ViewNode > =
        tree . get (node_id) . ok_or (
          "indefinitize_content_subtree: NodeId not in tree" ) ?;
      let node_pid : ID =
        get_id_from_treenode ( tree, node_id ) ?;
      let content_child_treeids : Vec < NodeId > =
        node_ref . children ()
        . filter ( |c| matches! ( &c . value() . kind,
                                  ViewNodeKind::True (t)
                                  if !t . parent_ignores_it() ))
        . map ( |c| c . id () )
        . collect ();
      (node_pid, content_child_treeids) };
  if ! truenode_in_tree_is_indefinitive ( tree, node_id ) ? {
    visited . remove (&node_pid);
    makeIndefinitiveAndClobber ( tree, node_id, config ) ?; }
  for child_treeid in content_child_treeids { // recurse
    indefinitize_content_subtree (
      tree, child_treeid, visited, config ) ?; }
  Ok (( )) }

/// Expands content for a node using BFS with a node limit.
///
/// This is similar to `render_initial_forest_bfs`
/// but operates on an existing PairTree.
///
/// The starting node is already definitive. This function:
/// - Reads content children from the NodeComplete in the tree
/// - Adds them as children using BFS
/// - Marks repeats (nodes already in `visited`) as indefinitive
/// - Adds new definitive nodes to `visited`
/// - Truncates when the node limit is exceeded:
///   - Adds children in that last gen up to the limit as indefinitive
///   - Completes the sibling group of the limit-hitting node
///   - After limit-hitting node's parent, rewrite nodes in that second-to-last gen as indefinitive
///
/// Generations are relative to the effective root (generation 0),
/// *not* the tree's true root. This way, truncation only affects
/// nodes within this subtree, not siblings of ancestors.
///
/// `hidden_ids` contains IDs to filter from a subscribee's
/// top-level children (without recursing into grandchildren, etc.)
/// If the view was requested from a non-subscribee,
/// 'hidden_ids' will be empty.
async fn extendDefinitiveSubtreeFromLeaf (
  tree           : &mut Tree<ViewNode>,
  effective_root : NodeId, // It contained the request and is already in the tree. it was indefinitive when the request was issued, but was made definitive by 'execute_definitive_view_request'.
  limit          : usize,
  visited        : &mut DefinitiveMap,
  config         : &SkgConfig,
  driver         : &TypeDBDriver,
  hidden_ids     : &HashSet < ID >,
) -> Result < (), Box<dyn Error> > {
  let mut gen_with_children : Vec < (NodeId, // effective root
                                     ID) > = // one of its children
    content_ids_if_definitive_else_empty (
      tree, effective_root, config ) ?
    . into_iter ()
    . filter ( |skgid| ! hidden_ids . contains (skgid) )
    . map ( |child_skgid| (effective_root, child_skgid) )
    . collect ();
  let mut nodes_rendered : usize = 0;
  let mut generation : usize = 1; // children of effective root
  while ! gen_with_children . is_empty () {
    if nodes_rendered + gen_with_children . len () >= limit {
      // Limit hit. Complete this sibling group
      // and truncate later parents.
      let space_left : usize = limit - nodes_rendered;
      add_last_generation_and_truncate_some_of_previous (
        tree, generation, &gen_with_children,
        space_left, effective_root, visited, config, driver ) . await ?;
      return Ok (( )); }
    let mut next_gen : Vec < (NodeId, ID) > = Vec::new ();
    for (parent_treeid, child_skgid) in gen_with_children {
      let new_treeid : NodeId = build_node_branch_minus_content (
        Some((tree, parent_treeid)),
        &child_skgid, config, driver, visited ) . await ?;
      nodes_rendered += 1;
      if ! truenode_in_tree_is_indefinitive ( tree, new_treeid ) ? {
        // No filtering here; 'hidden_ids' only applies to top-level.
        let grandchild_skgids : Vec < ID > =
          content_ids_if_definitive_else_empty (
            tree, new_treeid, config ) ?;
        for grandchild_skgid in grandchild_skgids {
          next_gen . push ( (new_treeid, grandchild_skgid) ); }} }
    gen_with_children = next_gen;
    generation += 1; }
  Ok (( )) }

/// Fetches NodeComplete from memory or disk.
/// Updates title and body.
/// Preserves all other ViewNode data.
fn from_disk_replace_title_body_and_nodecomplete (
  tree    : &mut Tree<ViewNode>,
  node_id : NodeId,
  config  : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  let (pid, src) : (ID, SourceName) =
    pid_and_source_from_treenode ( tree, node_id,
      "from_disk_replace_title_body_and_nodecomplete" ) ?;
  let nodecomplete : NodeComplete = nodecomplete_from_memory_or_disk (
    config, &pid, &src ) ?;
  let title : String = nodecomplete . title . clone();
  if title . is_empty () {
    return Err ( format! ( "NodeComplete {} has empty title", pid ) . into () ); }
  let body : Option < String > = nodecomplete . body . clone ();
  write_at_truenode_in_tree ( tree, node_id, |t| {
    t . title = title;
    if let IndefOrDef::Definitive { body: ref mut b, .. }
      = t . indef_or_def
      { *b = body; } } )
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  Ok (( )) }

/// Expand children for a removed node,
/// by loading content from git HEAD.
/// Children that exist in TypeDB are marked as RemovedHere.
/// Children that don't exist in TypeDB are marked as Removed.
async fn extendDefinitiveSubtree_fromGit (
  tree           : &mut Tree<ViewNode>,
  effective_root : NodeId,
  limit          : usize,
  visited        : &mut DefinitiveMap,
  config         : &SkgConfig,
  hidden_ids     : &HashSet<ID>,
  typedb_driver  : &TypeDBDriver,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
) -> Result<(), Box<dyn Error>> {
  let (pid, src) : (ID, SourceName) =
    pid_and_source_from_treenode ( tree, effective_root,
      "extendDefinitiveSubtree_fromGit" ) ?;
  let (contents, contents_in_worktree)
    : (Vec<ID>, HashSet<String>) =
    { let nodecomplete : NodeComplete =
        nodecomplete_from_index_or_head ( &pid, &src, config ) ?;
      let contents : Vec<ID> =
        nodecomplete . contains;
      let not_hidden : BTreeSet<String> =
        contents . iter()
        . filter ( |id| ! hidden_ids . contains (id) )
        . take (limit)
        . map ( |id| id . 0 . clone() )
        . collect();
      let contents_in_worktree : HashSet<String> =
        which_ids_exist (
          &config . db_name, typedb_driver, &not_hidden
        ) . await ?;
      (contents, contents_in_worktree) };
  for child_id in contents . iter() . take (limit) {
    if hidden_ids . contains (child_id) { continue; }
    let child_viewnode : ViewNode =
      mk_removed_child_viewnode (
        child_id, &src, &contents_in_worktree,
        deleted_since_head_pid_src_map,
        config, typedb_driver ) . await ?;
    let mut parent_mut : NodeMut<ViewNode> = // Add child to tree
      tree . get_mut (effective_root) . ok_or (
        "Parent not found" ) ?;
    parent_mut . append (child_viewnode);
    visited . insert ( child_id . clone(), effective_root ); }
  Ok (( )) }

/// Build an ViewNode for a child of
/// a removed (in the git diff sense) node.
/// The child is loaded from the worktree if it exists there,
/// and otherwise from git HEAD.
async fn mk_removed_child_viewnode (
  child_id           : &ID,
  parent_src         : &SourceName,
  contents_in_worktree : &HashSet<String>,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  config             : &SkgConfig,
  typedb_driver      : &TypeDBDriver,
) -> Result<ViewNode, Box<dyn Error>> {
  let in_worktree : bool =
    contents_in_worktree . contains ( &child_id . 0 );
  // The child is a phantom either way
  // (absent from its parent's worktree content).
  // Existence depends on whether the child's
  // own file is also gone in the worktree:
  //   in worktree -> file exists; just M is removed.
  //   not in worktree -> file gone; both X and M are removed.
  let (existence, child_opt_nodecomplete)
    : (ExistenceAxes, Option<NodeComplete>)
    = if in_worktree
      { ( ExistenceAxes::default (),
          optnodecomplete_from_id (
            config, typedb_driver, child_id ) . await ? ) }
      else
      { ( ExistenceAxes { staged: None, unstaged: Some (Sign::Minus) },
          nodecomplete_from_index_or_head ( child_id, parent_src, config
                                     ) . ok() ) };
  let membership : MembershipAxes =
    MembershipAxes { staged: None, unstaged: Some (Sign::Minus) };
  let child_nodecomplete : &NodeComplete =
    child_opt_nodecomplete . as_ref()
    . ok_or_else ( || format! (
      "mk_removed_child_viewnode: no NodeComplete for child {}",
      child_id ) ) ?;
  let child_source : Option<SourceName> =
    if in_worktree { Some ( child_nodecomplete . source . clone() ) }
    else           { deleted_since_head_pid_src_map . get (child_id)
                       . cloned() };
  let mut child_viewnode : ViewNode =
    mk_indefinitive_viewnode (
      child_id . clone(),
      child_nodecomplete . source . clone(),
      child_nodecomplete . title . clone(),
      Birth::ContentOf );
  if let ViewNodeKind::True ( ref mut t ) = child_viewnode . kind {
    if let Some (source) = child_source {
      t . source = source; }
    t . existence  = existence;
    t . membership = membership; }
  Ok (child_viewnode) }

/// Load title and body from index (preferred) or HEAD for a node
/// that no longer exists on disk. This is used when expanding a
/// definitive view for a node that exists in HEAD or the index but
/// not in the worktree.
fn from_git_replace_title_body (
  tree    : &mut Tree<ViewNode>,
  node_id : NodeId,
  config  : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  let (pid, src) : (ID, SourceName) =
    pid_and_source_from_treenode ( tree, node_id,
      "from_git_replace_title_body" ) ?;
  let nodecomplete : NodeComplete =
    nodecomplete_from_index_or_head ( &pid, &src, config ) ?;
  write_at_truenode_in_tree (
    tree, node_id, |t| {
      t . title = nodecomplete . title;
      if let IndefOrDef::Definitive { body: ref mut b, .. }
        = t . indef_or_def
        { *b = nodecomplete . body; } } )
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  Ok (( )) }
