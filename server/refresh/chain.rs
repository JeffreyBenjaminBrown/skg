//! PURPOSE: Draw the containerward chain under the members of a
//! partner folder ('Folder::members_carry_containerward_chain'):
//! under each member, its containers, write-protected and claiming nothing;
//! under each of those, its containers; and so on breadth-first to
//! 'max_roleTree_depth', up to the roots.
//! A node the walk met before (a cycle or a diamond) is shown once
//! more, as a leaf ('ContainerwardRoleTree::Repeated'). Each chain
//! node is what the ID rule makes of its gnid under its parent's chain
//! goal ('chain_goal'), built as the refresh builds that kind, from the
//! truth and with no disk read; the chain goes on only under an
//! occurrence.
//! The refresh draws the chain under a member it draws into such a
//! folder ('after_drawing' in 'server/refresh/visit.rs'), and never
//! reinstates a chain node once drawn; no request extends a chain,
//! and a request on a chain node for its containers draws a
//! containers folder under it like any other. A chain node carries no
//! change facts and takes no commit-readiness marker
//! ('stands_in_containerward_chain').

use crate::dbs::in_rust_graph::containerward_role_tree::{
  ContainerwardRoleTree, containers_in_inRustGraph, full_containerward_role_tree};
use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::refresh::Walk;
use crate::refresh::editability::{DrawnEditability, EditabilityAndClaim};
use crate::refresh::goal::{Goal, GoalItem, GoalMember, chain_goal};
use crate::refresh::id_rule::{WhereTheHeadlineStands, id_rule, vognode_for_answer};
use crate::refresh::mode::RefreshMode;
use crate::types::misc::ID;
use crate::types::viewnode::{Viewnode, ViewnodeKind, Vognode};

use ego_tree::{NodeId, Tree};
use std::collections::HashSet;
use std::error::Error;

/// Draw the chain under the occurrence at 'member': its containers'
/// role trees, each prepended so that the first container comes
/// first. The role tree gives the chain its shape (the order, the
/// depth, a node met before shown as a leaf); the chain goal of each
/// chain node's parent says which of its containers stand
/// ('chain_goal'), and the ID rule what each is. Anything but an
/// occurrence at 'member' is skipped.
pub(crate) fn draw_containerward_chain (
  tree   : &mut Tree<Viewnode>,
  member : NodeId,
  walk   : &Walk,
) -> Result<(), Box<dyn Error>> {
  let Some (pid) : Option<ID> = tree . get (member) . and_then ( |node|
    match &node . value () . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =>
        Some ( occurrence . gnid . clone () ),
      _ => None, } )
    else { return Ok (( )); };
  if let ContainerwardRoleTree::Inner ( _, containers ) =
    containerward_role_tree (&pid, walk)
  { insert_containers (tree, member, &pid, &containers, walk) ?; }
  Ok (( )) }

/// The containerward role tree of 'pid', over the live graph, or in
/// the diff view over the union of the straddle.
fn containerward_role_tree (
  pid  : &ID,
  walk : &Walk,
) -> ContainerwardRoleTree {
  let max_depth : usize = walk . truth . config . max_roleTree_depth;
  match walk . mode {
    RefreshMode::ContentView =>
      full_containerward_role_tree (
        &containers_in_inRustGraph (walk . truth . graph ), pid, max_depth ),
    RefreshMode::DiffView { straddle, .. } =>
      full_containerward_role_tree (
        &|node : &ID, role : RelationRole| -> HashSet<ID> {
          straddle . partners (node, role) . into_iter ()
            . map ( |partner| partner . id )
            . collect () },
        pid, max_depth ), } }

/// Prepend under 'parent', an occurrence of 'parent_pid', each of
/// 'containers' that stands ('prepend_containerward_chain_node'), in
/// reverse so that they keep their order; and under each that is an
/// occurrence, its own containers.
fn insert_containers (
  tree       : &mut Tree<Viewnode>,
  parent     : NodeId,
  parent_pid : &ID,
  containers : &[ContainerwardRoleTree],
  walk       : &Walk,
) -> Result<(), Box<dyn Error>> {
  let goal : Goal = chain_goal (parent_pid, walk);
  for container in containers . iter () . rev () {
    let Some ((chain_node, pid)) : Option<(NodeId, Option<ID>)> =
      prepend_containerward_chain_node (
        tree, parent, container . gnid (), &goal, walk ) ?
      else { continue; };
    if let ( Some (pid), ContainerwardRoleTree::Inner ( _, its_containers )) =
      (pid, container)
    { insert_containers (tree, chain_node, &pid, its_containers, walk) ?; }}
  Ok (( )) }

/// Prepend under 'parent' the chain node for 'container', if 'goal',
/// the chain goal of 'parent', names it, as the ID rule answers it and
/// as the refresh builds each kind: an occurrence write-protected, a
/// ghost or a gnid-only vognode, each claiming nothing; for a
/// tombstone, nothing. A container whose record the
/// skgrepo restriction keeps out the goal does not name
/// ('docs/telescopes.org', "What lives where"); one whose record is
/// visible and whose node is restricted is a gnid-only vognode.
/// Returns the chain node and, for an occurrence, its pid: the chain
/// goes on only under an occurrence.
fn prepend_containerward_chain_node (
  tree      : &mut Tree<Viewnode>,
  parent    : NodeId,
  container : &ID,
  goal      : &Goal,
  walk      : &Walk,
) -> Result<Option<(NodeId, Option<ID>)>, Box<dyn Error>> {
  let Some (item) : Option<&GoalItem> =
    goal . item_for ( &GoalMember::Gnid ( container . clone () ))
    else { return Ok (None); };
  let Some (viewnode) : Option<Viewnode> =
    vognode_for_answer (
      id_rule (container, WhereTheHeadlineStands::Under (goal), walk),
      container, item,
      EditabilityAndClaim { editability : DrawnEditability::WriteProtected,
                            claims      : false },
      walk ) ?
    else { return Ok (None); };
  let pid : Option<ID> = match &viewnode . kind {
    ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =>
      Some ( occurrence . gnid . clone () ),
    _ => None, };
  let chain_node : NodeId = tree . get_mut (parent)
    . ok_or ("prepend_containerward_chain_node: parent not found") ?
    . prepend (viewnode)
    . id ();
  Ok ( Some (( chain_node, pid )) ) }
