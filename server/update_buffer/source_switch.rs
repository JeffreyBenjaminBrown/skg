/// Applying a source-set switch to an already-drawn view
/// (TODO/full-schema/9-2_source-set-safety.org).  Two passes:
/// convert every Active viewnode from a now-inactive source into an
/// InactiveNode, then prune (DFS postorder, so emptied parents prune
/// in the same sweep) every:
/// - InactiveNode leaf;
/// - Qual leaf whose owning gnode (grandparent) is inactive;
/// - write-protected leaf partner (child of a PartnerFolder), active or
///   inactive: a write-protected partner defines nothing, and
///   completion regenerates current membership afterward;
/// - empty QualFolder or PartnerFolder;
/// - DeadScaffold leaf.
/// What survives includes active nodes, inactive nodes with
/// surviving children (the retained case), and definitive partners
/// (the user may be mid-edit inside them).  Completion (run with folder
/// creation enabled) then rebuilds folders and members for the new
/// active set.

use crate::source_sets::ActiveSourceSet;
use crate::types::viewnode::{
  mk_inactive_viewnode, PartnerFolder, QualFolder, ViewNode, ViewNodeKind,
  Vognode };
use crate::update_buffer::util::subtree_satisfies;

use ego_tree::{NodeId, NodeRef, Tree};
use std::error::Error;

pub fn convert_and_prune_for_source_switch (
  tree   : &mut Tree<ViewNode>,
  active : &ActiveSourceSet,
) -> Result<(), Box<dyn Error>> {
  convert_now_inactive_actives (tree, active);
  let root : NodeId = tree . root () . id ();
  prune_children_postorder (tree, root) ?;
  Ok (( )) }

fn convert_now_inactive_actives (
  tree   : &mut Tree<ViewNode>,
  active : &ActiveSourceSet,
) {
  if active . is_all () { return; }
  let ids : Vec<NodeId> =
    tree . root () . descendants ()
    . map ( |n| n . id () )
    . collect ();
  for id in ids {
    let conversion : Option<ViewNodeKind> =
      tree . get (id)
      . and_then ( |n| match &n . value () . kind {
          ViewNodeKind::Vognode (Vognode::Active (t))
            if ! active . contains_source (&t . source)
            => Some ( mk_inactive_viewnode () . kind ),
          _ => None } );
    if let Some (kind) = conversion {
      tree . get_mut (id) . unwrap () . value () . kind = kind; }}}

/// Recursively prune 'node's descendants, then report whether 'node'
/// itself should be detached (the parent's loop detaches it, so the
/// forest root is never detached).  If a detached subtree contained
/// the focused node, focus transfers to 'node' (the surviving
/// parent), mirroring 'detach_scaffold_transferring_focus'.
fn prune_children_postorder (
  tree : &mut Tree<ViewNode>,
  node : NodeId,
) -> Result<bool, Box<dyn Error>> {
  let child_ids : Vec<NodeId> =
    tree . get (node)
    . ok_or ("prune_children_postorder: node not found") ?
    . children () . map ( |c| c . id () ) . collect ();
  for child in child_ids {
    if prune_children_postorder (tree, child) ? {
      let had_focus : bool =
        subtree_satisfies (
          tree, child, &|vn : &ViewNode| vn . focused ) ?;
      if had_focus {
        tree . get_mut (node) . unwrap ()
          . value () . focused = true; }
      tree . get_mut (child) . unwrap () . detach (); }}
  should_prune (tree, node) }

fn should_prune (
  tree : &Tree<ViewNode>,
  node : NodeId,
) -> Result<bool, Box<dyn Error>> {
  let node_ref : NodeRef<ViewNode> =
    tree . get (node)
    . ok_or ("should_prune: node not found") ?;
  let is_leaf : bool =
    ! node_ref . has_children ();
  let affects_parent_partnerFolder : bool =
    node_ref . parent ()
    . map ( |p| matches! ( &p . value () . kind,
                           ViewNodeKind::PartnerFolder (_) ))
    . unwrap_or (false);
  let grandaffects_parent_inactive : bool =
    node_ref . parent ()
    . and_then ( |p| p . parent () )
    . map ( |gp| matches! ( &gp . value () . kind,
                            ViewNodeKind::Vognode (Vognode::Inactive (_)) ))
    . unwrap_or (false);
  Ok ( match &node_ref . value () . kind {
    ViewNodeKind::Vognode (Vognode::Inactive (_)) =>
      is_leaf,
    ViewNodeKind::Qual (_) =>
      is_leaf && grandaffects_parent_inactive,
    ViewNodeKind::Vognode (Vognode::Active (t)) =>
      is_leaf && affects_parent_partnerFolder && t . is_writeProtected (),
    ViewNodeKind::QualFolder (QualFolder::ID)
      | ViewNodeKind::QualFolder (QualFolder::Alias)
      | ViewNodeKind::PartnerFolder (PartnerFolder::Subscribee)
      | ViewNodeKind::PartnerFolder (PartnerFolder::Subscriber)
      | ViewNodeKind::PartnerFolder (PartnerFolder::Overridden)
      | ViewNodeKind::PartnerFolder (PartnerFolder::Overrider)
      | ViewNodeKind::PartnerFolder (PartnerFolder::Hider)
      | ViewNodeKind::PartnerFolder (PartnerFolder::Hidden)
      | ViewNodeKind::PartnerFolder (PartnerFolder::HiddenInSubscribee)
      | ViewNodeKind::PartnerFolder (PartnerFolder::HiddenOutsideOfSubscribee) =>
      is_leaf, // empty folder (children, if any, were pruned first)
    ViewNodeKind::DeadScaffold =>
      is_leaf,
    ViewNodeKind::Phantom (_)
      | ViewNodeKind::BufferRoot =>
      false, } ) }

#[cfg(test)]
#[path = "../../tests/unit/source_switch.rs"]
mod tests;
