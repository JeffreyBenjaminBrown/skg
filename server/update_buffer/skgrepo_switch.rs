/// Applying a repo-set switch to an already-rendered view
/// (the API doc's "Restricted-skgrepo nodes").  Two passes:
/// convert every Unrestricted viewnode from a now-restricted skgrepo into an
/// RestrictedVognode, and every deleted or unknown phantom whose
/// relationship or node a now-restricted skgrepo records (a deleted
/// phantom's home skgrepo; an unknown phantom's relRepo, or, when it
/// has none, its recorder's home) into a tombstone, then prune (DFS
/// postorder, so emptied parents prune in the same sweep) the leaves
/// the restriction hides, and only those:
/// - RestrictedVognode leaf;
/// - Property leaf whose owning vognode (grandparent) is restricted;
/// - Tombstone leaf;
/// - PropertyFolder or PartnerFolder emptied by this prune (one that
///   was empty before it stays).
/// Everything else stays as it was, with its viewnode ID: folders and
/// their members, write-protected or not, the completion (run with
/// folder creation enabled) then refreshes under the new skgrepo
/// restriction, as it refreshes them at a save.

use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::misc::SkgrepoName;
use crate::types::viewnode::{
  mk_restricted_viewnode, PartnerFolder, Phantom, PropertyFolder, Viewnode,
  ViewnodeKind, Vognode };
use crate::update_buffer::util::subtree_satisfies;

use ego_tree::{NodeId, NodeRef, Tree};
use std::error::Error;

pub fn convert_and_prune_for_skgrepo_switch (
  tree   : &mut Tree<Viewnode>,
  restriction : &SkgrepoRestriction,
) -> Result<(), Box<dyn Error>> {
  // The phantoms first, while their recorders are still unrestricted.
  convert_now_restricted_phantoms (tree, restriction);
  anonymize_now_restricted_vognodes (tree, restriction);
  let root : NodeId = tree . root () . id ();
  prune_children_postorder (tree, root) ?;
  Ok (( )) }

fn anonymize_now_restricted_vognodes (
  tree   : &mut Tree<Viewnode>,
  restriction : &SkgrepoRestriction,
) {
  if restriction . is_all () { return; }
  let skgids : Vec<NodeId> =
    tree . root () . descendants ()
    . map ( |n| n . id () )
    . collect ();
  for skgid in skgids {
    let conversion : Option<ViewnodeKind> =
      tree . get (skgid)
      . and_then ( |n| match &n . value () . kind {
          ViewnodeKind::Vognode (Vognode::Unrestricted (t))
            if ! restriction . contains_skgrepo (&t . home_skgrepo)
            => Some ( mk_restricted_viewnode () . kind ),
          _ => None } );
    if let Some (kind) = conversion {
      tree . get_mut (skgid) . unwrap () . value () . replace_kind (kind); }}}

/// A phantom shows its node's ID, and a deleted one its last title and
/// body, so one that a now-restricted skgrepo records becomes a tombstone:
/// the prune below removes it if it is a leaf, and otherwise
/// it stays, titleless, holding what is under it.
fn convert_now_restricted_phantoms (
  tree        : &mut Tree<Viewnode>,
  restriction : &SkgrepoRestriction,
) {
  if restriction . is_all () { return; }
  let phantoms : Vec<NodeId> =
    tree . root () . descendants ()
    . filter ( |node| match &node . value () . kind {
        ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Deleted (d))) =>
          ! restriction . contains_skgrepo (&d . home_skgrepo),
        ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (u))) =>
          u . relRepo . clone ()
          . or_else ( || recorder_home_skgrepo (*node) )
          . is_some_and ( |skgrepo| ! restriction . contains_skgrepo (&skgrepo) ),
        _ => false } )
    . map ( |node| node . id () )
    . collect ();
  for phantom in phantoms {
    tree . get_mut (phantom) . unwrap () . value ()
      . replace_kind (ViewnodeKind::Tombstone); }}

/// The home skgrepo of the unrestricted vognode that records the
/// relationship an unknown phantom without a relRepo stands in: its
/// viewparent, or, in a folder, the folder's viewparent.
fn recorder_home_skgrepo (
  node : NodeRef<Viewnode>,
) -> Option<SkgrepoName> {
  let parent : NodeRef<Viewnode> = node . parent () ?;
  let recorder : NodeRef<Viewnode> = match &parent . value () . kind {
    ViewnodeKind::PartnerFolder (_) | ViewnodeKind::PropertyFolder (_) =>
      parent . parent () ?,
    _ => parent, };
  match &recorder . value () . kind {
    ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
      Some ( t . home_skgrepo . clone () ),
    _ => None, }}

/// Recursively prune 'node's descendants, then report whether 'node'
/// itself should be detached (the parent's loop detaches it, so the
/// forest root is never detached).  If a detached subtree contained
/// the focused node, focus transfers to 'node' (the surviving
/// parent), mirroring 'detach_viewnode_transferring_focus'.
fn prune_children_postorder (
  tree : &mut Tree<Viewnode>,
  node : NodeId,
) -> Result<bool, Box<dyn Error>> {
  let child_skgids : Vec<NodeId> =
    tree . get (node)
    . ok_or ("prune_children_postorder: node not found") ?
    . children () . map ( |c| c . id () ) . collect ();
  let had_children : bool = ! child_skgids . is_empty ();
  for child in child_skgids {
    if prune_children_postorder (tree, child) ? {
      let had_focus : bool =
        subtree_satisfies (
          tree, child, &|vn : &Viewnode| vn . focused ) ?;
      if had_focus {
        tree . get_mut (node) . unwrap ()
          . value () . focused = true; }
      tree . get_mut (child) . unwrap () . detach (); }}
  should_prune (tree, node, had_children) }

fn should_prune (
  tree         : &Tree<Viewnode>,
  node         : NodeId,
  had_children : bool, // before this prune
) -> Result<bool, Box<dyn Error>> {
  let node_ref : NodeRef<Viewnode> =
    tree . get (node)
    . ok_or ("should_prune: node not found") ?;
  let is_leaf : bool =
    ! node_ref . has_children ();
  let grandaffects_parent_restricted : bool =
    node_ref . parent ()
    . and_then ( |p| p . parent () )
    . map ( |gp| matches! ( &gp . value () . kind,
                            ViewnodeKind::Vognode (Vognode::Restricted (_)) ))
    . unwrap_or (false);
  Ok ( match &node_ref . value () . kind {
    ViewnodeKind::Vognode (Vognode::Restricted (_)) =>
      is_leaf,
    ViewnodeKind::Property (_) =>
      is_leaf && grandaffects_parent_restricted,
    ViewnodeKind::PropertyFolder (PropertyFolder::ID)
      | ViewnodeKind::PropertyFolder (PropertyFolder::Alias)
      | ViewnodeKind::PropertyFolder (PropertyFolder::Flags { .. })
      | ViewnodeKind::PartnerFolder (PartnerFolder::Subscribee)
      | ViewnodeKind::PartnerFolder (PartnerFolder::Subscriber)
      | ViewnodeKind::PartnerFolder (PartnerFolder::Overridden)
      | ViewnodeKind::PartnerFolder (PartnerFolder::Overrider)
      | ViewnodeKind::PartnerFolder (PartnerFolder::Hider)
      | ViewnodeKind::PartnerFolder (PartnerFolder::Hidden)
      | ViewnodeKind::PartnerFolder (PartnerFolder::Container)
      | ViewnodeKind::PartnerFolder (PartnerFolder::Mentioner)
      | ViewnodeKind::PartnerFolder (PartnerFolder::AcquiredBy)
      | ViewnodeKind::PartnerFolder (PartnerFolder::Acquired)
      | ViewnodeKind::PartnerFolder (PartnerFolder::HiddenInSubscribee)
      | ViewnodeKind::PartnerFolder (PartnerFolder::HiddenOutsideOfSubscribee) =>
      is_leaf && had_children, // emptied: its children were pruned first
    ViewnodeKind::Tombstone =>
      is_leaf,
    ViewnodeKind::Vognode (Vognode::Unrestricted (_))
      | ViewnodeKind::Vognode (Vognode::Phantom (_))
      | ViewnodeKind::BufferRoot =>
      false, } ) }

#[cfg(test)]
#[path = "../../tests/unit/repo_switch.rs"]
mod tests;
