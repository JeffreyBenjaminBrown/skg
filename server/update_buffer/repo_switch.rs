/// Applying a repo-set switch to an already-drawn view
/// (TODO/full-schema/9-2_repo-set-safety.org).  Two passes:
/// convert every Active viewnode from a now-inactive repo into an
/// InactiveVognode, then prune (DFS postorder, so emptied parents prune
/// in the same sweep) every:
/// - InactiveVognode leaf;
/// - Property leaf whose owning gnode (grandparent) is inactive;
/// - write-protected leaf partner (child of a PartnerFolder), active or
///   inactive: a write-protected partner defines nothing, and
///   completion regenerates current membership afterward;
/// - empty PropertyFolder or PartnerFolder;
/// - DeadViewnode leaf.
/// What survives includes active nodes, inactive nodes with
/// surviving children (the retained case), and definitive partners
/// (the user may be mid-edit inside them).  Completion (run with folder
/// creation enabled) then rebuilds folders and members for the new
/// active set.

use crate::repo_sets::ActiveRepoSet;
use crate::types::viewnode::{
  mk_inactive_viewnode, PartnerFolder, PropertyFolder, Viewnode, ViewnodeKind,
  Vognode };
use crate::update_buffer::util::subtree_satisfies;

use ego_tree::{NodeId, NodeRef, Tree};
use std::error::Error;

pub fn convert_and_prune_for_repo_switch (
  tree   : &mut Tree<Viewnode>,
  active : &ActiveRepoSet,
) -> Result<(), Box<dyn Error>> {
  convert_now_inactive_actives (tree, active);
  let root : NodeId = tree . root () . id ();
  prune_children_postorder (tree, root) ?;
  Ok (( )) }

fn convert_now_inactive_actives (
  tree   : &mut Tree<Viewnode>,
  active : &ActiveRepoSet,
) {
  if active . is_all () { return; }
  let ids : Vec<NodeId> =
    tree . root () . descendants ()
    . map ( |n| n . id () )
    . collect ();
  for id in ids {
    let conversion : Option<ViewnodeKind> =
      tree . get (id)
      . and_then ( |n| match &n . value () . kind {
          ViewnodeKind::Vognode (Vognode::Active (t))
            if ! active . contains_repo (&t . home_repo)
            => Some ( mk_inactive_viewnode () . kind ),
          _ => None } );
    if let Some (kind) = conversion {
      tree . get_mut (id) . unwrap () . value () . kind = kind; }}}

/// Recursively prune 'node's descendants, then report whether 'node'
/// itself should be detached (the parent's loop detaches it, so the
/// forest root is never detached).  If a detached subtree contained
/// the focused node, focus transfers to 'node' (the surviving
/// parent), mirroring 'detach_viewnode_transferring_focus'.
fn prune_children_postorder (
  tree : &mut Tree<Viewnode>,
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
          tree, child, &|vn : &Viewnode| vn . focused ) ?;
      if had_focus {
        tree . get_mut (node) . unwrap ()
          . value () . focused = true; }
      tree . get_mut (child) . unwrap () . detach (); }}
  should_prune (tree, node) }

fn should_prune (
  tree : &Tree<Viewnode>,
  node : NodeId,
) -> Result<bool, Box<dyn Error>> {
  let node_ref : NodeRef<Viewnode> =
    tree . get (node)
    . ok_or ("should_prune: node not found") ?;
  let is_leaf : bool =
    ! node_ref . has_children ();
  let affects_parent_partnerFolder : bool =
    node_ref . parent ()
    . map ( |p| matches! ( &p . value () . kind,
                           ViewnodeKind::PartnerFolder (_) ))
    . unwrap_or (false);
  let grandaffects_parent_inactive : bool =
    node_ref . parent ()
    . and_then ( |p| p . parent () )
    . map ( |gp| matches! ( &gp . value () . kind,
                            ViewnodeKind::Vognode (Vognode::Inactive (_)) ))
    . unwrap_or (false);
  Ok ( match &node_ref . value () . kind {
    ViewnodeKind::Vognode (Vognode::Inactive (_)) =>
      is_leaf,
    ViewnodeKind::Property (_) =>
      is_leaf && grandaffects_parent_inactive,
    ViewnodeKind::Vognode (Vognode::Active (t)) =>
      is_leaf && affects_parent_partnerFolder && t . is_writeProtected (),
    ViewnodeKind::PropertyFolder (PropertyFolder::ID)
      | ViewnodeKind::PropertyFolder (PropertyFolder::Alias)
      | ViewnodeKind::PropertyFolder (PropertyFolder::Flags { .. })
      | ViewnodeKind::PartnerFolder (PartnerFolder::Subscribee)
      | ViewnodeKind::PartnerFolder (PartnerFolder::Subscriber)
      | ViewnodeKind::PartnerFolder (PartnerFolder::Overridden)
      | ViewnodeKind::PartnerFolder (PartnerFolder::Overrider)
      | ViewnodeKind::PartnerFolder (PartnerFolder::Hider)
      | ViewnodeKind::PartnerFolder (PartnerFolder::Hidden)
      | ViewnodeKind::PartnerFolder (PartnerFolder::HiddenInSubscribee)
      | ViewnodeKind::PartnerFolder (PartnerFolder::HiddenOutsideOfSubscribee) =>
      is_leaf, // empty folder (children, if any, were pruned first)
    ViewnodeKind::DeadViewnode =>
      is_leaf,
    ViewnodeKind::Vognode (Vognode::Phantom (_))
      | ViewnodeKind::BufferRoot =>
      false, } ) }

#[cfg(test)]
#[path = "../../tests/unit/repo_switch.rs"]
mod tests;
