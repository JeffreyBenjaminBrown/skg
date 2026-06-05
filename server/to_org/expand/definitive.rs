use crate::dbs::filesystem::one_node::optnodecomplete_from_id;
use crate::dbs::typedb::nodes::which_ids_exist;
use crate::git_ops::read_repo::nodecomplete_from_index_or_head;
use crate::source_sets::ActiveSourceSet;
use crate::to_org::complete::sharing::type_and_parent_type_consistent_with_subscribee;
use crate::to_org::expand::aliases::build_and_integrate_aliases_view_then_drop_request;
use crate::to_org::expand::backpath::{ build_and_integrate_containerward_view_then_drop_request_with_source_set, build_and_integrate_sourceward_view_then_drop_request_with_source_set};
use crate::to_org::util::{ DefinitiveMap, Finalizable, get_id_from_treenode, makeIndefinitiveAndClobber, truenode_in_tree_is_indefinitive };
use crate::types::git::{ExistenceAxes, MembershipAxes, Sign, SourceDiff, file_existence_axes_from_source_diff};
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::viewnode::{ ViewNode, ViewNodeKind, ViewRequest, IndefOrDef, ParentIs, mk_phantom_viewnode };
use crate::types::viewnode::Vognode;
use crate::types::nodes::complete::NodeComplete;
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_source;
use crate::types::tree::generic::read_at_node_in_tree;
use crate::types::tree::viewnode_nodecomplete::{write_at_truenode_in_tree, pid_and_source_from_treenode};

use ego_tree::{Tree, NodeId, NodeRef, NodeMut};
use std::collections::{BTreeSet,HashSet,HashMap};
use std::error::Error;
use typedb_driver::TypeDBDriver;

pub async fn execute_view_requests (
  viewforest    : &mut Tree<ViewNode>,
  requests      : Vec < (NodeId, ViewRequest) >,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  errors        : &mut Vec < String >,
  active_source_set : Option<&ActiveSourceSet>,
) -> Result < (), Box<dyn Error> > {
  for (node_id, request) in requests {
    match request {
      ViewRequest::Aliases => {
        build_and_integrate_aliases_view_then_drop_request (
          viewforest, node_id, config, typedb_driver, errors )
          . await ?; },
      ViewRequest::Containerward => {
        build_and_integrate_containerward_view_then_drop_request_with_source_set (
          viewforest, node_id, config, typedb_driver, errors,
          active_source_set )
          . await ?; },
      ViewRequest::Sourceward => {
        build_and_integrate_sourceward_view_then_drop_request_with_source_set (
          viewforest, node_id, config, typedb_driver, errors,
          active_source_set )
          . await ?; },
      ViewRequest::Definitive =>
        // The BFS driver settles every Definitive request at the node's own
        // visit (apply_definitive_draw_rule, the §5.2 draw rule + §5.3 cascade),
        // before this post-content view-request pass runs, so none should reach
        // here. Fail loudly if one does, rather than silently dropping it.
        return Err ( "execute_view_requests: a ViewRequest::Definitive survived \
          to the view-request pass; it should have been consumed by the draw \
          rule at the node's visit" . into () ), }}
  Ok (( )) }

/// The result of applying the §5.2 Tentative/Final draw rule to a node that
/// carries a 'ViewRequest::Definitive' (a user DVR, or -- once the §5.3
/// cascade lands -- a cascade DVR).
pub enum DrawOutcome {
  /// An existing Final occurrence of this id won: the DVR was dropped and
  /// the node left indefinitive. No content expansion should follow.
  Deferred,
  /// The node was made Final (and any prior Tentative occurrence of its id
  /// indefinitized). The caller draws its content next -- via the §5.3
  /// cascade for a present node, or 'extendDefinitiveSubtree_fromGit' for a
  /// removed one. 'hidden_ids' is the subscriber's hide set when the node is
  /// a subscribee, else empty.
  MadeFinal { is_removed_node : bool,
              hidden_ids      : HashSet < ID >, },
}

/// Apply the plan_v2 §5.2 draw rule for a node carrying
/// 'ViewRequest::Definitive', WITHOUT expanding its content:
/// - defer to an existing Final occurrence (drop the DVR, stay
///   indefinitive) -> 'DrawOutcome::Deferred';
/// - otherwise indefinitize any prior Tentative occurrence of the id, mark
///   this node Final (resyncing title/body from disk or git), register it in
///   the map, and clear the request -> 'DrawOutcome::MadeFinal'.
/// Content drawing is the caller's job (cascade vs fromGit), so the same
/// rule serves both today's self-expanding path and the BFS driver, which
/// must settle Final-ness before it draws (and cascades) content.
pub fn apply_definitive_draw_rule (
  viewforest : &mut Tree<ViewNode>,
  node_id    : NodeId,
  config     : &SkgConfig,
  visited    : &mut DefinitiveMap,
) -> Result < DrawOutcome, Box<dyn Error> > {
  let node_pid : ID = get_id_from_treenode (
    viewforest, node_id ) ?;
  if let Some (&prior) = visited . get (& node_pid) {
    if prior . is_final () && prior . node_id () != node_id {
      // §5.2: an existing Final occurrence wins; discard this DVR and make
      // the node indefinitive. (Setting indefinitive matters for a §5.3
      // cascade DVR landing on a freshly-created definitive child whose id
      // is already Final elsewhere; for a user DVR on an already-indefinitive
      // node it is a no-op. The expand step then clobbers/refreshes it.)
      write_at_truenode_in_tree (
        viewforest, node_id,
        |t| { t . view_requests . remove (& ViewRequest::Definitive);
              t . indef_or_def = IndefOrDef::Indefinitive; } )
        . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
      return Ok ( DrawOutcome::Deferred ); }
    if prior . node_id () != node_id {
      indefinitize_content_subtree ( viewforest,
                                     prior . node_id (),
                                     visited, config ) ?; }}
  // Past here the node WILL be made Final, so the git-diff existence read
  // happens now -- after the Deferred early return above, so a deferring node
  // does no extra work.
  let is_removed_node : bool = // for git diff view
    read_at_node_in_tree (
      viewforest, node_id,
      |n| match &n . kind {
        ViewNodeKind::Vognode (Vognode::Normal (t))
          => truenode_file_absent_from_worktree (&t . existence),
        ViewNodeKind::Vognode (Vognode::DiffPhantom (p))
          => truenode_file_absent_from_worktree (&p . existence),
        _ => false } ) ?;
  { // Remove request, mark definitive, replace title/body, add to visited.
    write_at_truenode_in_tree (
      viewforest, node_id, |t| {
        t . view_requests . remove (& ViewRequest::Definitive);
        t . indef_or_def = IndefOrDef::Definitive {
          body         : None,
          edit_request : None }; } )
      . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
    if is_removed_node
      { from_git_replace_title_body (
          // PITFALL: This per-node git lookup might be slow. Hard to see how to batch the lookoup, though, since it's from git.
          viewforest, node_id, config ) ?; }
      else { from_disk_replace_title_body_and_nodecomplete (
               viewforest, node_id, config ) ?; }
    // A DVR target is Final (§5.2): later DVRs for this ID defer to it.
    visited . insert ( node_pid . clone(), Finalizable::Final (node_id) ); }
  let hidden_ids : HashSet < ID > =
    // Only the removed-node branch (extendDefinitiveSubtree_fromGit) consumes
    // this, and computing it is a NodeComplete fetch (a possible disk read), so
    // a present node -- the common case -- skips it.
    if is_removed_node { get_hidden_ids_if_subscribee ( viewforest, node_id, config ) ? }
    else               { HashSet::new () };
  Ok ( DrawOutcome::MadeFinal { is_removed_node, hidden_ids } ) }

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
      nodecomplete_rustFirst_by_pid_and_source (
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
                                  ViewNodeKind::Vognode (Vognode::Normal (t))
                                  if t . parentIs == ParentIs::Affected ))
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

/// Fetches NodeComplete from the in-Rust graph or disk.
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
  let nodecomplete : NodeComplete = nodecomplete_rustFirst_by_pid_and_source (
    config, &pid, &src ) ?;
  let title : String = nodecomplete . title . clone();
  if title . is_empty () {
    return Err ( format! ( "NodeComplete {} has empty title", pid ) . into () ); }
  let body : Option < String > = nodecomplete . body . clone ();
  write_at_truenode_in_tree
    ( tree, node_id,
      |t| { t . title = title;
            if let IndefOrDef::Definitive { body: ref mut b, .. }
              = t . indef_or_def
              { *b = body; }} )
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  Ok (( )) }

fn truenode_file_absent_from_worktree (
  existence : &ExistenceAxes,
) -> bool {
  match existence . unstaged {
    Some (Sign::Minus) => true,
    Some (Sign::Plus)  => false,
    None               => matches! (existence . staged, Some (Sign::Minus)),
  } }

/// Expand children for a removed node, by loading content from git HEAD. Each
/// child is a DiffPhantom; whether it still exists in the worktree (a
/// removed-here member) or not (fully removed) is encoded in its
/// existence/membership axes, not in a named state.
pub async fn extendDefinitiveSubtree_fromGit (
  tree           : &mut Tree<ViewNode>,
  effective_root : NodeId,
  limit          : usize,
  visited        : &mut DefinitiveMap,
  source_diffs   : &Option<HashMap<SourceName, SourceDiff>>,
  config         : &SkgConfig,
  hidden_ids     : &HashSet<ID>,
  typedb_driver  : &TypeDBDriver,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  _active_source_set : Option<&ActiveSourceSet>,
) -> Result<(), Box<dyn Error>> {
  let (pid, src) : (ID, SourceName) =
    pid_and_source_from_treenode ( tree, effective_root,
      "extendDefinitiveSubtree_fromGit" ) ?;
  // The parent's file is gone from the worktree; its per-stage
  // file status determines in which stage its deletion happened,
  // which in turn determines the child's membership-removal stage.
  let parent_deletion_axes : ExistenceAxes =
    file_existence_axes_from_source_diff (
      source_diffs, &pid, &src );
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
        &parent_deletion_axes, source_diffs,
        deleted_since_head_pid_src_map,
        config, typedb_driver ) . await ?;
    let mut parent_mut : NodeMut<ViewNode> = // Add child to tree
      tree . get_mut (effective_root) . ok_or (
        "Parent not found" ) ?;
    parent_mut . append (child_viewnode);
    visited . insert ( child_id . clone(),
                       Finalizable::Tentative (effective_root) ); }
  Ok (( )) }

/// Derive child-membership-removal axes from parent-existence-removal
/// axes. When the parent's file is deleted in a given stage, any
/// child it used to contain is "removed from its contains" in that
/// same stage. Plus signs on existence (file re-added) don't
/// propagate — this is a deletion-only mapping.
fn membership_minus_from_existence_minus (
  ex : &ExistenceAxes,
) -> MembershipAxes {
  let sign_minus = | s : Option<Sign> |
    if matches! (s, Some (Sign::Minus)) { Some (Sign::Minus) } else { None };
  MembershipAxes {
    staged:   sign_minus (ex . staged),
    unstaged: sign_minus (ex . unstaged), } }

/// Build a ViewNode for a child of
/// a removed (in the git diff sense) node.
/// The child is loaded from the worktree if it exists there,
/// and otherwise from git HEAD.
async fn mk_removed_child_viewnode (
  child_id           : &ID,
  parent_src         : &SourceName,
  contents_in_worktree : &HashSet<String>,
  parent_deletion_axes : &ExistenceAxes,
  source_diffs       : &Option<HashMap<SourceName, SourceDiff>>,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  config             : &SkgConfig,
  typedb_driver      : &TypeDBDriver,
) -> Result<ViewNode, Box<dyn Error>> {
  let in_worktree : bool =
  // The child is a phantom (absent from its parent's worktree content)
  // regardless of the value of 'in_worktree'. Existence depends
  // on whether the child's own file is gone; if gone, stage(s) of
  // its deletion come from the source_diff for the child's file.
    contents_in_worktree . contains ( &child_id . 0 );
  let (existence, child_opt_nodecomplete)
    : (ExistenceAxes, Option<NodeComplete>)
    = if in_worktree
      { ( ExistenceAxes::default (),
          optnodecomplete_from_id (
            config, typedb_driver, child_id ) . await ? ) }
      else
      { ( file_existence_axes_from_source_diff (
            source_diffs, child_id, parent_src ),
          nodecomplete_from_index_or_head (
            child_id, parent_src, config
          ) . ok() ) };
  // Child's membership in this removed parent's contains mirrors
  // the stages in which the parent was removed: if the parent's
  // file was deleted staged, the child's membership-as-contained-by
  // this parent is also removed staged, and similarly for unstaged.
  let membership : MembershipAxes =
    membership_minus_from_existence_minus (parent_deletion_axes);
  let child_nodecomplete : &NodeComplete =
    child_opt_nodecomplete . as_ref()
    . ok_or_else ( || format! (
      "mk_removed_child_viewnode: no NodeComplete for child {}",
      child_id ) ) ?;
  let child_source : SourceName =
    if in_worktree { Some ( child_nodecomplete . source . clone() ) }
    else           { deleted_since_head_pid_src_map . get (child_id)
                       . cloned() }
    . unwrap_or_else (|| child_nodecomplete . source . clone());
  let child_viewnode : ViewNode =
    mk_phantom_viewnode (
      child_id . clone(),
      child_source,
      child_nodecomplete . title . clone(),
      existence,
      membership );
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
