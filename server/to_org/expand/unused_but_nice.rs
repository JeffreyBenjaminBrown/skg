//! Held for someday, not currently wired in.
//!
//! This is the machinery for a feature we'd like to have but don't use yet:
//! opening a node that git says has been DELETED, and drawing its content as of
//! the last commit (HEAD) -- each child shown as a Diff phantom, marked removed.
//! It works; it's just not reachable in the current renderer. We're keeping it
//! because it would be a nice thing to use someday, and rebuilding it from
//! scratch would be a waste.
//!
//! How it used to plug in (so a future reviver knows the wiring that was
//! removed): the draw rule (apply_definitive_draw_rule in definitive.rs) would
//! detect a removed node with `truenode_file_absent_from_worktree`, load its
//! title/body from git with `from_git_replace_title_body`, compute the
//! subscriber hide-set with `get_hidden_ids_if_subscribee`, and report
//! `is_removed_node = true`; view completion (complete_nodes_in_level_order in
//! complete.rs) would then call
//! `extendDefinitiveSubtree_fromGit` instead of expanding live content. That
//! path was unreachable in the inline-diff architecture -- a node's git-existence
//! axes are stamped at the END of its visit, but the draw rule runs at the
//! START, so `is_removed_node` was always false by the time it was checked.
//! Reviving the feature means stamping existence before the draw rule (or
//! detecting removal another way) and re-attaching these calls.

#![allow(dead_code)]
#![allow(non_snake_case)]

use crate::dbs::filesystem::one_node::optnodecomplete_from_id;
use crate::dbs::typedb::nodes::which_ids_exist;
use crate::git_ops::read_repo::nodecomplete_from_index_or_head;
use crate::source_sets::ActiveSourceSet;
use crate::to_org::complete::partner_col::type_and_parent_type_consistent_with_subscribee;
use crate::to_org::util::{ DefinitiveMap, Finalizable };
use crate::types::git::{ExistenceAxes, MembershipAxes, Sign, SourceDiff, file_existence_axes_from_source_diff};
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::viewnode::{ ViewNode, IndefOrDef, mk_phantom_viewnode };
use crate::types::nodes::complete::NodeComplete;
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_source;
use crate::types::tree::viewnode_nodecomplete::{write_at_truenode_in_tree, pid_and_source_from_treenode};

use ego_tree::{Tree, NodeId, NodeRef, NodeMut};
use std::collections::{BTreeSet, HashSet, HashMap};
use std::error::Error;
use typedb_driver::TypeDBDriver;

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

fn truenode_file_absent_from_worktree (
  existence : &ExistenceAxes,
) -> bool {
  match existence . unstaged {
    Some (Sign::Minus) => true,
    Some (Sign::Plus)  => false,
    None               => matches! (existence . staged, Some (Sign::Minus)),
  } }

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

/// Expand children for a removed node, by loading content from git HEAD. Each
/// child is a Diff phantom; whether it still exists in the worktree (a
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
