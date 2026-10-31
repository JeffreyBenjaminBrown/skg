// Helpers that parse a test's buffer as written, no graph supplying
// IDs or skgrepos, and read the parsed tree. Declared in
// 'tests/grouped_repos.rs' and 'tests/grouped_saves.rs'.

use skg::from_text::buffer_to_viewnodes::add_missing_info::IdResolution;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_viewnode_tree;
use skg::types::misc::ID;
use skg::types::tree::forest::ViewForest;
use skg::types::viewnode::{Folder, Viewnode, ViewnodeKind, Vognode, VognodeKind};

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::BTreeSet;

/// The viewnode tree of 'input', read as written. Panics if the
/// parse fails.
pub fn viewnode_tree_as_written (
  input : &str,
) -> Tree<Viewnode> {
  org_to_viewnode_tree (input, IdResolution::AsWritten) . unwrap () . 0 }

/// The viewforest of 'input', read as written. Panics if the parse
/// fails.
pub fn viewforest_as_written (
  input : &str,
) -> ViewForest {
  ViewForest::from_internal_tree ( viewnode_tree_as_written (input) ) }

/// The tree ID of the first root.
pub fn first_root (
  tree : &Tree<Viewnode>,
) -> NodeId {
  tree . root () . first_child () . unwrap () . id () }

/// The gnids of the occurrences among the children of 'parent'.
pub fn occurrence_children_gnids (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
) -> BTreeSet<ID> {
  tree . get (parent) . unwrap () . children ()
    . filter_map ( |child| match &child . value () . kind {
      ViewnodeKind::Vognode ( Vognode::Occurrence (occurrence) )
        => Some (occurrence . gnid . clone ()),
      _ => None, })
    . collect () }

/// The tree ID of the first viewnode, in the buffer's order, that is a
/// folder of kind 'folder'; None if there is none.
pub fn first_folder (
  tree   : &Tree<Viewnode>,
  folder : &Folder,
) -> Option<NodeId> {
  tree . root () . descendants ()
    . find ( |node_ref| matches! ( &node_ref . value () . kind,
                                   ViewnodeKind::Folder (kind) if kind == folder ))
    . map ( |node_ref| node_ref . id () ) }

/// What each child of 'parent' stands as, "KIND GNID": a vognode's
/// kind as the wire writes it ('node', 'ghost', 'unknown',
/// 'restricted') and its gnid. A child that is no vognode is written
/// by its error label ('tombstone', a folder's).
pub fn kind_and_gnid_of_each_child (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
) -> Vec<String> {
  tree . get (parent) . unwrap () . children ()
    . map ( |child| match &child . value () . kind {
      ViewnodeKind::Vognode (vognode) => {
        let kind : &str = match vognode . kind () {
          VognodeKind::Occurrence         => "node",
          VognodeKind::Ghost              => "ghost",
          VognodeKind::GnidOnly (gnid_only) => gnid_only . wire_atom (), };
        format! ("{} {}", kind, vognode . gnid ()) },
      _ => child . value () . error_label (), })
    . collect () }

/// The occurrences titled 'title', in the buffer's order.
pub fn occurrences_titled<'a> (
  tree  : &'a Tree<Viewnode>,
  title : &str,
) -> Vec<NodeRef<'a, Viewnode>> {
  tree . root () . descendants ()
    . filter ( |node_ref| node_ref . value () . is_occurrence ()
                          && node_ref . value () . title () == title )
    . collect () }
