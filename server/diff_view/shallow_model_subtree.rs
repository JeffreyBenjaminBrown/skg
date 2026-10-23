//! Reconciling what the diff view added under a viewnode with a
//! shallow model subtree of it ('docs/diff-view.org',
//! "The shallow model subtree, and what it keeps").
//! The shallow model subtree is built in a scratch tree. Each existing
//! viewnode it also produces is kept where it stands, with its fold
//! state, and its kind and facts are updated in place; each it does
//! not is converted by the one rule ('convert_unwarranted_child'), with
//! what the diff view made below it ('convert_unwarranted_subtree');
//! what only it has is copied in, after the child that precedes it in
//! the shallow model subtree. A reconciler manages only the children
//! it owns ('Owner'), as the view's record of owners says. A viewnode
//! that stands for a graphnode is matched by the ID it stands for, any
//! other by its kind (and a folder entry by the entry): within one
//! parent each key appears at most once.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::to_org::complete::partner_folder::child_data::skgid_a_folder_child_stands_for;
use crate::types::git::Sign;
use crate::types::misc::ID;
use crate::types::tree::moves::place_created_children_after_their_model_predecessors;
use crate::types::viewnode::{
  AffectsParent, Birth, ChangeFacts, Owner, Property, PropertyFolder,
  Viewnode, ViewnodeKind, Vognode};
use crate::update_buffer::util::{Conversion, convert_unwarranted_child};

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::HashSet;
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

/// Under 'parent', make the children that 'owner' owns and 'selects'
/// selects match the children of 'model_parent' in 'model'. 'owner' is
/// the calling reconciler: it owns every child it creates, and
/// manages no child another owns (the user's, or another
/// reconciler's, such as a content list's members, which are the
/// content reconciler's); 'selects' says, by kind, which of its own
/// children this model concerns, where one reconciler runs several
/// models under one parent. A kept child stays where it stands; a
/// created one goes after the kept or created child that precedes it
/// in the shallow model subtree, or first when none does
/// ('place_created_children_after_their_model_predecessors'). A child
/// the model lacks is converted ('convert_unwarranted_subtree'). A
/// kept child whose model twin 'descend' selects has its own children
/// reconciled the same way, every keyed child 'owner' owns managed.
/// Returns the kept and created children, in the shallow model
/// subtree's order. 'graph' is the live graph ('match_key').
pub fn reconcile_children_with_shallowModelSubtree (
  tree         : &mut Tree<Viewnode>,
  parent       : NodeId,
  model        : &Tree<Viewnode>,
  model_parent : NodeId,
  graph        : &InRustGraph,
  owner        : Owner,
  selects      : &dyn Fn (&Viewnode) -> bool,
  descend      : &dyn Fn (&Viewnode) -> bool,
) -> Result<Vec<NodeId>, Box<dyn Error>> {
  let mut unmatched : Vec<(NodeId, MatchKey)> =
    tree . get (parent)
    . ok_or ("reconcile_children_with_shallowModelSubtree: parent not found") ?
    . children ()
    . filter ( |child| child . value () . owner == owner
                       && selects ( child . value () ))
    . filter_map ( |child| match_key ( child . value (), graph )
                   . map ( |key| (child . id (), key) ))
    . collect ();
  let model_children : Vec<NodeId> =
    model . get (model_parent)
    . ok_or ("reconcile_children_with_shallowModelSubtree: model parent not found") ?
    . children () . map ( |child| child . id () ) . collect ();
  let mut result : Vec<NodeId> = Vec::new ();
  let mut created : HashSet<NodeId> = HashSet::new ();
  for model_child in model_children {
    let model_viewnode : &Viewnode =
      model . get (model_child) . unwrap () . value ();
    let key : MatchKey = match_key (model_viewnode, graph)
      . ok_or ("reconcile_children_with_shallowModelSubtree: a model viewnode has no match key") ?;
    let matched : Option<usize> =
      unmatched . iter () . position ( |(_, existing)| *existing == key );
    match matched {
      Some (index) => {
        let (existing, _) : (NodeId, MatchKey) = unmatched . remove (index);
        adopt_model_facts (
          tree . get_mut (existing) . unwrap () . value (), model_viewnode );
        if descend (model_viewnode) {
          reconcile_children_with_shallowModelSubtree (
            tree, existing, model, model_child, graph, owner, &|_| true,
            descend ) ?; }
        result . push (existing); },
      None => {
        let copy : NodeId =
          append_copy_of_subtree (tree, parent, model, model_child, owner) ?;
        created . insert (copy);
        result . push (copy); }, }}
  for (stale, _) in unmatched {
    convert_unwarranted_subtree (tree, stale) ?; }
  place_created_children_after_their_model_predecessors (
    tree, parent, &result, &created ) ?;
  Ok (result) }

/// Convert 'node', a child the model no longer warrants, by the one
/// rule ('convert_unwarranted_child'), and, first, what was made below
/// it for the same model: postorder, every child of a viewnode being
/// converted is converted too, except the user's and the content
/// reconciler's, which stay where they stand (a parked vognode keeps
/// its content, as the older reconciler's does). So a folder's members
/// go with the folder, and a member's role tree with the member. A
/// viewnode holding nothing but childless dead viewnodes is converted
/// as a leaf, so the prune that ends the completion removes the lot.
/// Detaches nothing.
pub fn convert_unwarranted_subtree (
  tree : &mut Tree<Viewnode>,
  node : NodeId,
) -> Result<Conversion, Box<dyn Error>> {
  let children : Vec<(NodeId, Owner)> =
    tree . get (node) . ok_or ("convert_unwarranted_subtree: node not found") ?
    . children () . map ( |child| (child . id (), child . value () . owner) )
    . collect ();
  let mut holds_something : bool = false;
  for (child, child_owner) in children {
    let stays : bool =
      matches! ( child_owner, Owner::User | Owner::ContentReconciler );
    let conversion : Conversion =
      if stays { Conversion::Parked }
      else { convert_unwarranted_subtree (tree, child) ? };
    if conversion == Conversion::Parked { holds_something = true; }}
  convert_unwarranted_child (tree, node, ! holds_something) ?;
  Ok ( if holds_something { Conversion::Parked } else { Conversion::DeadLeaf } ) }

/// Give 'existing' the kind and facts of 'model', keeping its fold
/// state and focus. Of the facts, it takes the presence, and its
/// '(ready ...)' and '(leaves ...)' are cleared, for the pass after
/// completion to set. An unrestricted vognode keeps what the view
/// decides about it (its view requests, its stats, its readiness
/// request), and becomes write-protected when the model one is.
pub fn adopt_model_facts (
  existing : &mut Viewnode,
  model    : &Viewnode,
) {
  existing . change_facts = ChangeFacts {
    presence : model . change_facts . presence,
    ready    : None,
    leaves   : None };
  match ( &mut existing . kind, &model . kind ) {
    ( ViewnodeKind::Vognode (Vognode::Unrestricted (e)),
      ViewnodeKind::Vognode (Vognode::Unrestricted (f)) ) => {
      e . title         = f . title . clone ();
      e . home_skgrepo  = f . home_skgrepo . clone ();
      e . affectsParent = f . affectsParent;
      e . birth         = f . birth; // the model's, over what the buffer carried back
      if f . is_writeProtected () {
        e . write_protect (); }},
    (kind, model_kind) => {
      *kind = model_kind . clone ();
      existing . readiness_request = model . readiness_request . clone (); }, }}

/// Append under 'parent' a copy of the subtree of 'model' at
/// 'model_node', every viewnode of it 'owner''s.
pub fn append_copy_of_subtree (
  tree       : &mut Tree<Viewnode>,
  parent     : NodeId,
  model      : &Tree<Viewnode>,
  model_node : NodeId,
  owner      : Owner,
) -> Result<NodeId, Box<dyn Error>> {
  let model_ref : NodeRef<Viewnode> = model . get (model_node)
    . ok_or ("append_copy_of_subtree: model node not found") ?;
  let copy : NodeId =
    tree . get_mut (parent) . ok_or ("append_copy_of_subtree: parent not found") ?
    . append ( Viewnode { owner, .. model_ref . value () . clone () } ) . id ();
  let model_children : Vec<NodeId> =
    model_ref . children () . map ( |child| child . id () ) . collect ();
  for model_child in model_children {
    append_copy_of_subtree (tree, copy, model, model_child, owner) ?; }
  Ok (copy) }

/// A removed member: a viewnode the diff view made as a non-member
/// with '(presence rel-)', because its relationship to its viewparent is
/// only in the before-graph (or, while a visit reconciles, is about
/// to be). That is the one encoding of a removed relationship, in
/// content, in a partner folder and in a historical phantom's old
/// content ('mk_removed_member_viewnode', 'FolderGoalShape::Removed'),
/// and this is its one reader.
pub fn is_unrestricted_removed_member (
  viewnode : &Viewnode,
) -> bool {
  matches! ( &viewnode . kind,
    ViewnodeKind::Vognode (Vognode::Unrestricted (t))
      if t . affectsParent == AffectsParent::False
         && t . birth == Birth::Unremarkable
         && viewnode . change_facts . presence . rel == Some (Sign::Minus) ) }
