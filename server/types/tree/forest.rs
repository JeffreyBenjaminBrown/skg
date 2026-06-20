use crate::types::maybe_placed_viewnode::{
  MpViewnode,
  maybePlaced_viewforest_root_viewnode};
use crate::types::viewnode::{ViewNode, viewforest_root_viewnode};

use ego_tree::{NodeId, NodeMut, NodeRef, Tree};
use std::ops::{Deref, DerefMut};

/// ego_tree does not permit moving nodes from one tree to another.
/// Skg does that. This type makes it convenient. A Forest is a tree,
/// but with helpers that make it feel like a List<Tree> --
/// in particular, keeping the 'forest root' out of the way,
/// and treating its children like roots.
#[derive(Clone, Debug, PartialEq)]
pub struct Forest<T> {
  tree : Tree<T> }

pub type ViewForest = Forest<ViewNode>;
pub type MpViewForest = Forest<MpViewnode>;

impl Forest<ViewNode> {
  pub fn new () -> Self {
    Forest { tree : Tree::new (
      viewforest_root_viewnode () ) }}}

impl Forest<MpViewnode> {
  pub fn new () -> Self {
    Forest { tree : Tree::new (
      maybePlaced_viewforest_root_viewnode () ) }}}

/// The 'internal' functions expose the awkward internal representation
/// that Forest is designed to hide when possible.
impl<T> Forest<T> {
  pub fn internal_root_id (
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

  pub fn root_ids (
    &self,
  ) -> Vec<NodeId> {
    tree_forest_root_ids (&self . tree) }

  pub fn nodes<'a> (
    &'a self,
  ) -> impl Iterator<Item = NodeRef<'a, T>> + 'a {
    self . roots () . flat_map ( |root| {
      std::iter::once (root) . chain (root . descendants ()) } ) }

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
    id : NodeId,
  ) -> Option<NodeRef<'a, T>> {
    self . tree . get (id) }

  pub fn get_mut<'a> (
    &'a mut self,
    id : NodeId,
  ) -> Option<NodeMut<'a, T>> {
    self . tree . get_mut (id) }}

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
pub fn tree_forest_root_ids<T> (
  tree : &Tree<T>,
) -> Vec<NodeId> {
  tree . root () . children ()
    . map ( |root| root . id () )
    . collect () }
