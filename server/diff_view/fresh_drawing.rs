//! Reconciling what the diff view drew under a viewnode with a
//! fresh drawing of it ('docs/diff-view.org',
//! "The fresh drawing, and what it keeps").
//! The fresh drawing is built in a scratch tree. Each existing
//! viewnode it also produces is kept, with its fold state, and its
//! kind and facts are updated in place; each it does not is
//! detached; what only it has is copied in. A viewnode that stands
//! for a graphnode is matched by the ID it stands for, any other by
//! its kind (and a folder entry by the entry): within one parent each
//! key appears at most once.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::to_org::complete::partner_folder::child_data::skgid_a_folder_child_stands_for;
use crate::types::git::Sign;
use crate::types::misc::ID;
use crate::types::tree::moves::arrange_last_children;
use crate::types::viewnode::{
  AffectsParent, Birth, ChangeFacts, Property, PropertyFolder,
  Viewnode, ViewnodeKind, Vognode};
use crate::update_buffer::util::detach_viewnode_transferring_focus;

use ego_tree::{NodeId, NodeRef, Tree};
use std::error::Error;

/// What identifies a viewnode among its siblings.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum MatchKey {
  /// A viewnode that stands for a graphnode: the ID it stands for, as
  /// 'skgid_a_folder_child_stands_for' reads it, else its skgid.
  Graphnode (ID),
  /// One that stands for no graphnode: its kind, and for a folder
  /// entry the entry.
  Kind (String),
}

/// None for a viewnode that carries no identity (a restricted
/// vognode, a dead viewnode, the buffer root). 'graph' is the live
/// graph, which resolves a member's extra ID.
pub fn match_key (
  viewnode : &Viewnode,
  graph    : &InRustGraph,
) -> Option<MatchKey> {
  match &viewnode . kind {
    ViewnodeKind::Vognode (vognode) =>
      skgid_a_folder_child_stands_for (viewnode, graph)
      . or_else ( || vognode . skgid () . cloned () )
      . map (MatchKey::Graphnode),
    ViewnodeKind::Property (property) =>
      Some ( MatchKey::Kind ( property_kind (property) )),
    ViewnodeKind::PropertyFolder (folder) =>
      Some ( MatchKey::Kind ( match folder {
        PropertyFolder::ID          => "idFolder",
        PropertyFolder::Alias       => "aliasFolder",
        PropertyFolder::Flags { .. } => "flagsFolder", } . to_string () )),
    ViewnodeKind::PartnerFolder (folder) =>
      Some ( MatchKey::Kind ( format! ("{:?}Folder", folder) )),
    ViewnodeKind::BufferRoot | ViewnodeKind::DeadViewnode => None, }}

fn property_kind (
  property : &Property,
) -> String {
  match property {
    // A removed entry has its live twin's key, so an entry the
    // worktree regains (or loses) keeps its viewnode.
    Property::Alias { text, .. } | Property::RemovedAlias { text, .. } =>
      format! ("alias {}", text),
    Property::ID { skgid, .. } | Property::RemovedID { skgid, .. } =>
      format! ("id {}", skgid . 0),
    Property::Flag { flag, .. }    => format! ("flag {:?}", flag),
    Property::OldText { .. }       => "oldText" . to_string (),
    Property::BodyDiff { .. }      => "bodyDiff" . to_string (),
    Property::HomeSkgrepoMove { .. } => "homeSkgrepoMove" . to_string (),
    Property::RelRepoMove { .. }   => "relRepoMove" . to_string (),
    Property::ChangesetMarker { kind, .. } =>
      format! ("changesetMarker {:?}", kind), }}

/// Under 'parent', make the children that 'managed' selects match
/// the children of 'fresh_parent' in 'fresh'. A kept child whose
/// fresh twin 'descend' selects has its own children reconciled the
/// same way, every keyed child managed, and ordered as the fresh
/// drawing orders them. Returns the kept and created children, in
/// the fresh drawing's order; where they stand among the others is
/// the caller's to decide. 'graph' is the live graph ('match_key').
pub fn reconcile_children_with_fresh_drawing (
  tree         : &mut Tree<Viewnode>,
  parent       : NodeId,
  fresh        : &Tree<Viewnode>,
  fresh_parent : NodeId,
  graph        : &InRustGraph,
  managed      : &dyn Fn (&Viewnode) -> bool,
  descend      : &dyn Fn (&Viewnode) -> bool,
) -> Result<Vec<NodeId>, Box<dyn Error>> {
  let mut unmatched : Vec<(NodeId, MatchKey)> =
    tree . get (parent)
    . ok_or ("reconcile_children_with_fresh_drawing: parent not found") ?
    . children ()
    . filter ( |child| managed ( child . value () ))
    . filter_map ( |child| match_key ( child . value (), graph )
                   . map ( |key| (child . id (), key) ))
    . collect ();
  let fresh_children : Vec<NodeId> =
    fresh . get (fresh_parent)
    . ok_or ("reconcile_children_with_fresh_drawing: fresh parent not found") ?
    . children () . map ( |child| child . id () ) . collect ();
  let mut result : Vec<NodeId> = Vec::new ();
  for fresh_child in fresh_children {
    let fresh_viewnode : &Viewnode =
      fresh . get (fresh_child) . unwrap () . value ();
    let key : MatchKey = match_key (fresh_viewnode, graph)
      . ok_or ("reconcile_children_with_fresh_drawing: a fresh viewnode has no match key") ?;
    let matched : Option<usize> =
      unmatched . iter () . position ( |(_, existing)| *existing == key );
    match matched {
      Some (index) => {
        let (existing, _) : (NodeId, MatchKey) = unmatched . remove (index);
        adopt_fresh_facts (
          tree . get_mut (existing) . unwrap () . value (), fresh_viewnode );
        if descend (fresh_viewnode) {
          let kept : Vec<NodeId> = reconcile_children_with_fresh_drawing (
            tree, existing, fresh, fresh_child, graph,
            &|viewnode| match_key (viewnode, graph) . is_some (), descend ) ?;
          arrange_last_children (tree, existing, &kept) ?; }
        result . push (existing); },
      None => result . push (
        append_copy_of_subtree (tree, parent, fresh, fresh_child) ?), }}
  for (stale, _) in unmatched {
    detach_viewnode_transferring_focus (tree, stale) ?; }
  Ok (result) }

/// Give 'existing' the kind and facts of 'fresh', keeping its fold
/// state and focus. An unrestricted vognode keeps what the view
/// decides about it (its view requests, its stats), and becomes
/// write-protected when the fresh one is.
pub fn adopt_fresh_facts (
  existing : &mut Viewnode,
  fresh    : &Viewnode,
) {
  match ( &mut existing . kind, &fresh . kind ) {
    ( ViewnodeKind::Vognode (Vognode::Unrestricted (e)),
      ViewnodeKind::Vognode (Vognode::Unrestricted (f)) ) => {
      e . title         = f . title . clone ();
      e . home_skgrepo  = f . home_skgrepo . clone ();
      e . affectsParent = f . affectsParent;
      e . birth         = f . birth; // a buffer does not carry it back
      e . change_facts  = ChangeFacts {
        presence : f . change_facts . presence,
        ready    : None,
        leaves   : None };
      if f . is_writeProtected () {
        e . write_protect (); }},
    (kind, fresh_kind) => *kind = fresh_kind . clone (), }}

/// Append under 'parent' a copy of the subtree of 'fresh' at
/// 'fresh_node'.
pub fn append_copy_of_subtree (
  tree       : &mut Tree<Viewnode>,
  parent     : NodeId,
  fresh      : &Tree<Viewnode>,
  fresh_node : NodeId,
) -> Result<NodeId, Box<dyn Error>> {
  let fresh_ref : NodeRef<Viewnode> = fresh . get (fresh_node)
    . ok_or ("append_copy_of_subtree: fresh node not found") ?;
  let copy : NodeId =
    tree . get_mut (parent) . ok_or ("append_copy_of_subtree: parent not found") ?
    . append ( fresh_ref . value () . clone () ) . id ();
  let fresh_children : Vec<NodeId> =
    fresh_ref . children () . map ( |child| child . id () ) . collect ();
  for fresh_child in fresh_children {
    append_copy_of_subtree (tree, copy, fresh, fresh_child) ?; }
  Ok (copy) }

/// A member the diff view drew, outside the content list,
/// because its relationship to its viewparent is only in the
/// before-graph (or, while a visit reconciles, is about to be).
pub fn is_unrestricted_removed_member (
  viewnode : &Viewnode,
) -> bool {
  matches! ( &viewnode . kind,
    ViewnodeKind::Vognode (Vognode::Unrestricted (t))
      if t . affectsParent == AffectsParent::False
         && t . birth == Birth::Unremarkable
         && t . change_facts . presence . rel == Some (Sign::Minus) ) }
