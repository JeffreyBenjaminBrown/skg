/// Applying a repo-set switch to an already-rendered view
/// (the API doc's "Restricted-skgrepo nodes").  Two passes:
/// entombstone every occurrence of a node the new restriction hides,
/// and every unknown whose relationship a now-restricted skgrepo
/// records (its relRepo, or, when it has none, its recorder's
/// home), then prune (DFS postorder, so emptied parents prune in the
/// same sweep) the leaves the restriction hides, and only those:
/// - Tombstone leaf;
/// - property member or change viewnode leaf whose grandparent (the
///   viewnode it describes) is a tombstone;
/// - folder emptied by this prune (one that was empty before it
///   stays).
/// Everything else stays as it was, with its viewnode ID: folders and
/// their members, write-protected or not, the completion (run with
/// folder creation enabled) then refreshes under the new skgrepo
/// restriction, as it refreshes them at a save.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::skgrepo_sets::{SkgrepoRestriction, entombstone_hidden_occurrences};
use crate::types::misc::SkgrepoName;
use crate::types::viewnode::{
  Folder, Viewnode, ViewnodeKind, Vognode };
use crate::update_buffer::util::subtree_satisfies;

use ego_tree::{NodeId, NodeRef, Tree};
use std::error::Error;

pub fn convert_and_prune_for_skgrepo_switch (
  tree        : &mut Tree<Viewnode>,
  graph       : &InRustGraph,
  restriction : &SkgrepoRestriction,
) -> Result<(), Box<dyn Error>> {
  // The unknowns first, while their recorders are still occurrences.
  entombstone_now_restricted_unknowns (tree, restriction);
  entombstone_hidden_occurrences (tree, graph, restriction);
  let root : NodeId = tree . root () . id ();
  prune_children_postorder (tree, root) ?;
  Ok (( )) }

/// An unknown shows its node's ID, so one that a now-restricted
/// skgrepo records becomes a tombstone:
/// the prune below removes it if it is a leaf, and otherwise
/// it stays, titleless, holding what is under it.
fn entombstone_now_restricted_unknowns (
  tree        : &mut Tree<Viewnode>,
  restriction : &SkgrepoRestriction,
) {
  if restriction . is_all () { return; }
  let unknowns : Vec<NodeId> =
    tree . root () . descendants ()
    . filter ( |node| match &node . value () . kind {
        ViewnodeKind::Vognode (Vognode::Unknown (u)) =>
          u . relRepo . clone ()
          . or_else ( || recorder_home_skgrepo (*node) )
          . is_some_and ( |skgrepo| ! restriction . contains_skgrepo (&skgrepo) ),
        _ => false } )
    . map ( |node| node . id () )
    . collect ();
  for unknown in unknowns {
    tree . get_mut (unknown) . unwrap () . value ()
      . replace_kind (ViewnodeKind::Tombstone); }}

/// The home skgrepo of the occurrence that records the
/// relationship an unknown without a relRepo stands in: its
/// viewparent, or, in a folder, the folder's viewparent.
fn recorder_home_skgrepo (
  node : NodeRef<Viewnode>,
) -> Option<SkgrepoName> {
  let parent : NodeRef<Viewnode> = node . parent () ?;
  let recorder : NodeRef<Viewnode> = match &parent . value () . kind {
    ViewnodeKind::Folder (_) =>
      parent . parent () ?,
    _ => parent, };
  match &recorder . value () . kind {
    ViewnodeKind::Vognode (Vognode::Occurrence (t)) =>
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
  let child_gnids : Vec<NodeId> =
    tree . get (node)
    . ok_or ("prune_children_postorder: node not found") ?
    . children () . map ( |c| c . id () ) . collect ();
  let had_children : bool = ! child_gnids . is_empty ();
  for child in child_gnids {
    if prune_children_postorder (tree, child) ? {
      let had_focus : bool =
        subtree_satisfies (
          tree, child, &|vn : &Viewnode| vn . foldAndFocus . focused ) ?;
      if had_focus {
        tree . get_mut (node) . unwrap ()
          . value () . foldAndFocus . focused = true; }
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
  let grandparent_is_tombstone : bool =
    node_ref . parent ()
    . and_then ( |p| p . parent () )
    . map ( |gp| matches! ( &gp . value () . kind,
                            ViewnodeKind::Tombstone ))
    . unwrap_or (false);
  Ok ( match &node_ref . value () . kind {
    ViewnodeKind::PropertyMember (_) | ViewnodeKind::ChangeViewnode (_) =>
      is_leaf && grandparent_is_tombstone,
    ViewnodeKind::Folder (Folder::ID)
      | ViewnodeKind::Folder (Folder::Alias)
      | ViewnodeKind::Folder (Folder::Flags { .. })
      | ViewnodeKind::Folder (Folder::Subscribee)
      | ViewnodeKind::Folder (Folder::Subscriber)
      | ViewnodeKind::Folder (Folder::Overridden)
      | ViewnodeKind::Folder (Folder::Overrider)
      | ViewnodeKind::Folder (Folder::Hider)
      | ViewnodeKind::Folder (Folder::Hidden)
      | ViewnodeKind::Folder (Folder::Container)
      | ViewnodeKind::Folder (Folder::Mentioner)
      | ViewnodeKind::Folder (Folder::AcquiredBy)
      | ViewnodeKind::Folder (Folder::Acquired)
      | ViewnodeKind::Folder (Folder::HiddenInSubscribee)
      | ViewnodeKind::Folder (Folder::HiddenOutsideOfSubscribee) =>
      is_leaf && had_children, // emptied: its children were pruned first
    ViewnodeKind::Tombstone =>
      is_leaf,
    ViewnodeKind::Vognode (_)
      | ViewnodeKind::BufferRoot =>
      false, } ) }

#[cfg(test)]
#[path = "../../tests/unit/repo_switch.rs"]
mod tests;
