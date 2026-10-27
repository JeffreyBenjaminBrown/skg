// Helpers that parse a test's buffer as written, no graph supplying
// IDs or skgrepos, and read the parsed tree. Declared in
// 'tests/grouped_repos.rs' and 'tests/grouped_saves.rs'.

use skg::from_text::buffer_to_viewnodes::add_missing_info::IdResolution;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_viewnode_tree;
use skg::types::misc::ID;
use skg::types::tree::forest::ViewForest;
use skg::types::viewnode::{Viewnode, ViewnodeKind, Vognode};

use ego_tree::{NodeId, Tree};
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
