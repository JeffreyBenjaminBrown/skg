use crate::types::misc::ID;
use crate::types::viewnode::{
  Viewnode, ViewnodeKind, Vognode, viewforest_root_viewnode};

use ego_tree::{NodeId, NodeMut, NodeRef, Tree};
use std::collections::HashSet;
use std::ops::{Deref, DerefMut};

/// ego_tree does not permit moving nodes from one tree to another.
/// Skg does that. This type makes it convenient. A Forest is a tree,
/// but with helpers that make it feel like a List<Tree> --
/// in particular, keeping the 'forest root' out of the way,
/// and treating its children like roots.
#[derive(Clone, Debug, PartialEq)]
pub struct Forest<T> {
  tree : Tree<T> }

pub type ViewForest = Forest<Viewnode>;

impl Forest<Viewnode> {
  pub fn new () -> Self {
    Forest { tree : Tree::new (
      viewforest_root_viewnode () ) }}

  /// Drop save-only edit requests from every occurrence in a successfully
  /// skgsave-committed saved view. View requests have a different lifecycle and are
  /// intentionally preserved for completion to execute.
  pub fn consume_edit_requests_after_save (
    &mut self,
  ) {
    for viewnode in self . values_mut () {
      viewnode . consume_edit_request_after_save (); }}}

/// The IDs of the nodes that have an editable occurrence in the
/// viewforest ('Viewnode::editable_gnid').
pub fn editable_occurrences (
  viewforest : &ViewForest,
) -> HashSet<ID> {
  viewforest . nodes ()
    . filter_map ( |node| node . value () . editable_gnid () . cloned () )
    . collect () }

/// Whether 'node' stands where content does: its parent is the buffer
/// root, an occurrence, or a historical phantom that is
/// itself in content position. (Whether it is a member there, its
/// 'membership', is a separate question.)
pub fn in_content_position (
  node : NodeRef<Viewnode>,
) -> bool {
  match node . parent () {
    Some (parent) => match &parent . value () . kind {
      ViewnodeKind::BufferRoot
        | ViewnodeKind::Vognode (Vognode::Occurrence (_)) => true,
      ViewnodeKind::Vognode (Vognode::Ghost (_)) =>
        in_content_position (parent),
      _ => false, },
    None => false, }}

/// Whether the vognode children of 'node' are nodes of the
/// containerward chain: 'node' is a write-protected occurrence that is
/// a member of a folder whose members carry the chain
/// ('Folder::members_carry_containerward_chain'), or is itself a node
/// of the chain. The chain is known by where it stands. An editable
/// member's children are its content.
pub fn holds_containerward_chain (
  node : NodeRef<Viewnode>,
) -> bool {
  let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =
    &node . value () . kind else { return false; };
  if ! occurrence . is_writeProtected () { return false; }
  match node . parent () {
    Some (parent) => match &parent . value () . kind {
      ViewnodeKind::Folder (folder) =>
        folder . members_carry_containerward_chain (),
      ViewnodeKind::Vognode (Vognode::Occurrence (_)) =>
        holds_containerward_chain (parent),
      _ => false, },
    None => false, }}

/// Whether 'node' is a node of the containerward chain: a vognode
/// whose viewparent holds the chain ('holds_containerward_chain').
pub fn stands_in_containerward_chain (
  node : NodeRef<Viewnode>,
) -> bool {
  matches! ( node . value () . kind, ViewnodeKind::Vognode (_) )
  && node . parent () . is_some_and (holds_containerward_chain) }

/// The 'internal' functions expose the awkward internal representation
/// that Forest is designed to hide when possible.
impl<T> Forest<T> {
  pub fn internal_root_gnid (
    &self,
  ) -> NodeId {
    self . tree . root () . id () }

  pub fn from_internal_tree (
    tree : Tree<T>,
  ) -> Self {
    Forest { tree }}

  pub fn into_internal_tree (
    self,
  ) -> Tree<T> {
    self . tree }


  pub fn as_internal_tree_mut (
    &mut self,
  ) -> &mut Tree<T> {
    &mut self . tree }

  pub fn is_empty (
    &self,
  ) -> bool {
    self . tree . root () . children () . next () . is_none () }

  pub fn roots<'a> (
    &'a self,
  ) -> impl Iterator<Item = NodeRef<'a, T>> + 'a {
    self . tree . root () . children () }

  pub fn root_gnids (
    &self,
  ) -> Vec<NodeId> {
    tree_forest_root_gnids (&self . tree) }

  pub fn nodes<'a> (
    &'a self,
  ) -> impl Iterator<Item = NodeRef<'a, T>> + 'a {
    self . roots () . flat_map ( |root|
      root . descendants () ) } // 'descendants' begins with 'root' itself.

  pub fn first_root<'a> (
    &'a self,
  ) -> Option<NodeRef<'a, T>> {
    self . roots () . next () }

  pub fn append_root (
    &mut self,
    value : T,
  ) -> NodeId {
    self . tree . root_mut () . append (value) . id () }

  pub fn get<'a> (
    &'a self,
    gnid : NodeId,
  ) -> Option<NodeRef<'a, T>> {
    self . tree . get (gnid) }

  pub fn get_mut<'a> (
    &'a mut self,
    gnid : NodeId,
  ) -> Option<NodeMut<'a, T>> {
    self . tree . get_mut (gnid) }}

impl<T> From<Tree<T>> for Forest<T> {
  fn from (
    tree : Tree<T>,
  ) -> Self {
    Forest::from_internal_tree (tree) }}

impl<T> From<Forest<T>> for Tree<T> {
  fn from (
    forest : Forest<T>,
  ) -> Self {
    forest . into_internal_tree () }}

impl<T> Deref for Forest<T> {
  type Target = Tree<T>;

  fn deref (
    &self,
  ) -> &Self::Target {
    &self . tree }}

impl<T> DerefMut for Forest<T> {
  fn deref_mut (
    &mut self,
  ) -> &mut Self::Target {
    &mut self . tree }}

/// Compatibility helper for code that still holds the old
/// single-tree forest representation directly.
pub fn tree_forest_root_gnids<T> (
  tree : &Tree<T>,
) -> Vec<NodeId> {
  tree . root () . children ()
    . map ( |root| root . id () )
    . collect () }

#[cfg(test)]
#[path = "../../../tests/unit/tree_forest.rs"]
mod tests;
