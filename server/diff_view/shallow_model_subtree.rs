//! Reconciling what the diff view added under a viewnode with a
//! shallow model subtree of it ('docs/diff-view.org',
//! "The shallow model subtree, and what it keeps").
//! The shallow model subtree is built in a scratch tree. Each existing
//! viewnode it also produces is kept where it stands, with its fold
//! state, and its kind and facts are updated in place; each it does
//! not is converted by the one rule ('convert_unwarranted_child'), with
//! what the diff view made below it ('convert_unwarranted_subtree');
//! what only it has is copied in, after the child that precedes it in
//! the shallow model subtree. A viewnode that stands for a graphnode is
//! matched by the ID it stands for, any other by its kind (and a folder
//! entry by the entry), among all the children of the parent, whoever
//! drew them ('docs/glossary.org', "key, match key"): a child of a key
//! the model has is adopted, and a second child of a matched key is a
//! duplicate, converted by the one rule.
//! A stale child is converted only when it is the reconciler's, known
//! by its kind and change facts ('is_diffView_change_viewnode'); what
//! the user or another reconciler drew stays where it stands.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::to_org::complete::partner_folder::child_data::skgid_a_folder_child_stands_for;
use crate::to_org::extend::containerward_chain::is_drawn_chain_node;
use crate::types::misc::ID;
use crate::types::tree::moves::place_created_children_after_their_model_predecessors;
use crate::types::viewnode::{
  Membership, ChangeFacts, Existence, Phantom, Property, PropertyFolder,
  Viewnode, ViewnodeKind, Vognode};
use crate::update_buffer::util::{Conversion, Unwarranted, convert_unwarranted_child};

use ego_tree::{NodeId, NodeMut, NodeRef, Tree};
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

/// Under 'parent', make the children that 'selects' selects match the
/// children of 'model_parent' in 'model'. 'selects' says, by kind,
/// which children this model concerns, where one reconciler runs
/// several models under one parent; it is matched against every child
/// it selects, whoever drew it. Of several selected children
/// of a key the model has, the editable one is adopted, else the first
/// in buffer order; the others are duplicates, each converted by the
/// one rule ('convert_unwarranted_child') with its subtree where it
/// stands. A kept child stays where it stands; a created one goes
/// after the kept or created child that precedes it in the shallow
/// model subtree, or first when none does
/// ('place_created_children_after_their_model_predecessors'). A
/// selected child whose key the model lacks is converted
/// ('convert_unwarranted_subtree') when 'converts_when_stale' says it
/// is the calling reconciler's, by its kind and change facts, and
/// stays as it is otherwise. A kept child whose model twin 'descend' selects has
/// its own children reconciled the same way, every keyed child
/// selected. Returns the kept and created children, in the shallow
/// model subtree's order. 'graph' is the live graph ('match_key').
pub fn reconcile_children_with_shallowModelSubtree (
  tree         : &mut Tree<Viewnode>,
  parent       : NodeId,
  model        : &Tree<Viewnode>,
  model_parent : NodeId,
  graph        : &InRustGraph,
  converts_when_stale : &dyn Fn (NodeRef<Viewnode>) -> bool,
  selects      : &dyn Fn (&Viewnode) -> bool,
  descend      : &dyn Fn (&Viewnode) -> bool,
) -> Result<Vec<NodeId>, Box<dyn Error>> {
  let candidates : Vec<(NodeId, MatchKey)> =
    tree . get (parent)
    . ok_or ("reconcile_children_with_shallowModelSubtree: parent not found") ?
    . children ()
    . filter ( |child| selects ( child . value () ))
    . filter_map ( |child| match_key ( child . value (), graph )
                   . map ( |key| (child . id (), key) ))
    . collect ();
  let model_children : Vec<NodeId> =
    model . get (model_parent)
    . ok_or ("reconcile_children_with_shallowModelSubtree: model parent not found") ?
    . children () . map ( |child| child . id () ) . collect ();
  let mut taken : HashSet<NodeId> = HashSet::new (); // adopted, or duplicates
  let mut result : Vec<NodeId> = Vec::new ();
  let mut created : HashSet<NodeId> = HashSet::new ();
  for model_child in model_children {
    let model_viewnode : &Viewnode =
      model . get (model_child) . unwrap () . value ();
    let key : MatchKey = match_key (model_viewnode, graph)
      . ok_or ("reconcile_children_with_shallowModelSubtree: a model viewnode has no match key") ?;
    let copies : Vec<NodeId> =
      candidates . iter ()
      . filter ( |(child, existing)| *existing == key && ! taken . contains (child) )
      . map ( |(child, _)| *child )
      . collect ();
    taken . extend ( copies . iter () . copied () );
    match adopted_copy (tree, &copies) {
      Some (existing) => {
        for duplicate in copies . iter () . filter ( |copy| **copy != existing ) {
          convert_duplicate (tree, *duplicate) ?; }
        { let mut existing_mut : NodeMut<Viewnode> = tree . get_mut (existing) . unwrap ();
          adopt_model_facts (existing_mut . value (), model_viewnode); }
        if descend (model_viewnode) {
          reconcile_children_with_shallowModelSubtree (
            tree, existing, model, model_child, graph, converts_when_stale,
            &|_| true, descend ) ?; }
        result . push (existing); },
      None => {
        let copy : NodeId =
          append_copy_of_subtree (tree, parent, model, model_child) ?;
        created . insert (copy);
        result . push (copy); }, }}
  for (stale, _) in candidates {
    if ! taken . contains (&stale)
       && converts_when_stale ( tree . get (stale) . unwrap () ) {
      convert_unwarranted_subtree (tree, stale) ?; }}
  place_created_children_after_their_model_predecessors (
    tree, parent, &result, &created ) ?;
  Ok (result) }

/// Of the copies of one key, in buffer order, the one a reconciler
/// adopts: the editable one, else the first.
fn adopted_copy (
  tree   : &Tree<Viewnode>,
  copies : &[NodeId],
) -> Option<NodeId> {
  copies . iter ()
    . find ( |copy| tree . get (**copy) . unwrap () . value ()
                    . editable_skgid () . is_some () )
    . or ( copies . first () )
    . copied () }

/// Convert an extra copy of an adopted key by the one rule's duplicate
/// clause: retained write-protected when it has a subtree, which stays
/// under it, a tombstone when it has none.
fn convert_duplicate (
  tree      : &mut Tree<Viewnode>,
  duplicate : NodeId,
) -> Result<Conversion, Box<dyn Error>> {
  let is_leaf : bool =
    tree . get (duplicate) . ok_or ("convert_duplicate: node not found") ?
    . children () . next () . is_none ();
  convert_unwarranted_child (tree, duplicate, is_leaf, Unwarranted::Duplicate) }

/// Convert 'node', a child the model no longer warrants, by the one
/// rule ('convert_unwarranted_child'), and, first, what was made below
/// it for the same model: postorder, every child of a viewnode being
/// converted that 'goes_with_a_stale_viewparent' is converted too; the
/// rest (the content reconciler's members, what the user typed or
/// pasted) stay where they stand, so a vognode retained as non-member
/// keeps its content, as the older reconciler's does. So a folder's
/// members go with the folder, and a member's chain with the member.
/// A viewnode holding nothing but childless dead viewnodes is
/// converted as a leaf, so the prune that ends the completion removes
/// the lot. Detaches nothing.
pub fn convert_unwarranted_subtree (
  tree : &mut Tree<Viewnode>,
  node : NodeId,
) -> Result<Conversion, Box<dyn Error>> {
  let children : Vec<(NodeId, bool)> =
    tree . get (node) . ok_or ("convert_unwarranted_subtree: node not found") ?
    . children ()
    . map ( |child| (child . id (), goes_with_a_stale_viewparent (child)) )
    . collect ();
  let mut holds_something : bool = false;
  for (child, goes) in children {
    let conversion : Conversion =
      if goes { convert_unwarranted_subtree (tree, child) ? }
      else { Conversion::Parked };
    if conversion == Conversion::Parked { holds_something = true; }}
  convert_unwarranted_child (tree, node, ! holds_something, Unwarranted::Stale) ?;
  Ok ( if holds_something { Conversion::Parked } else { Conversion::DeadLeaf } ) }

/// Whether the diff view's reconciler drew 'node', known by its kind
/// and its change facts as they stand at the completion that finds it
/// stale: a property viewnode (an old text, a move, a changeset
/// marker, a folder entry), a folder, a historical phantom, a removed
/// member (it still carries '(rel removed)'), or a member under a
/// phantom, which has no content list, so that the member is one the
/// content reconciler drew while the node was live. A content member,
/// tagged or not, and a non-member the user typed or pasted are not
/// the diff view's.
pub fn is_diffView_change_viewnode (
  node : NodeRef<Viewnode>,
) -> bool {
  let viewnode : &Viewnode = node . value ();
  match &viewnode . kind {
    ViewnodeKind::Property (_)
      | ViewnodeKind::PropertyFolder (_)
      | ViewnodeKind::PartnerFolder (_)
      | ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (_))) => true,
    ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
      is_unrestricted_removed_member (viewnode)
      || ( t . membership == Membership::Member
           && node . parent () . is_some_and ( |parent| matches! (
                parent . value () . kind,
                ViewnodeKind::Vognode (Vognode::Phantom (_)) ))),
    _ => false, }}

/// Whether 'node', under a viewnode being converted, goes with it
/// ('convert_unwarranted_subtree'): when the diff view drew it
/// ('is_diffView_change_viewnode'), when it is a member of a folder
/// (every child of a partner folder but a non-member the user put
/// there), or when the role-tree attach drew it as a chain node
/// ('is_drawn_chain_node').
fn goes_with_a_stale_viewparent (
  node : NodeRef<Viewnode>,
) -> bool {
  let is_folder_member : bool =
    node . parent () . is_some_and ( |parent| matches! (
      parent . value () . kind, ViewnodeKind::PartnerFolder (_) ))
    && match &node . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
        t . membership == Membership::Member,
      ViewnodeKind::Vognode (_) => true,
      _ => false, };
  is_diffView_change_viewnode (node) || is_folder_member
    || is_drawn_chain_node (node) }

/// Give 'existing' the kind and facts of 'model', keeping its fold
/// state and focus. Of the facts, it takes the kinds of change, and its
/// '(ready ...)' and '(leaves ...)' are cleared, for the pass after
/// completion to set. An unrestricted vognode keeps what the view
/// decides about it (its view requests, its stats, its readiness
/// request), and becomes write-protected when the model one is.
pub fn adopt_model_facts (
  existing : &mut Viewnode,
  model    : &Viewnode,
) {
  existing . change_facts = ChangeFacts {
    change   : model . change_facts . change,
    ready    : None,
    leaves   : None };
  match ( &mut existing . kind, &model . kind ) {
    ( ViewnodeKind::Vognode (Vognode::Unrestricted (e)),
      ViewnodeKind::Vognode (Vognode::Unrestricted (f)) ) => {
      e . title         = f . title . clone ();
      e . home_skgrepo  = f . home_skgrepo . clone ();
      e . membership = f . membership;
      if f . is_writeProtected () {
        e . write_protect (); }},
    (kind, model_kind) => {
      *kind = model_kind . clone ();
      existing . readiness_request = model . readiness_request . clone (); }, }}

/// Append under 'parent' a copy of the subtree of 'model' at
/// 'model_node'.
pub fn append_copy_of_subtree (
  tree       : &mut Tree<Viewnode>,
  parent     : NodeId,
  model      : &Tree<Viewnode>,
  model_node : NodeId,
) -> Result<NodeId, Box<dyn Error>> {
  let model_ref : NodeRef<Viewnode> = model . get (model_node)
    . ok_or ("append_copy_of_subtree: model node not found") ?;
  let copy : NodeId =
    tree . get_mut (parent) . ok_or ("append_copy_of_subtree: parent not found") ?
    . append ( model_ref . value () . clone () ) . id ();
  let model_children : Vec<NodeId> =
    model_ref . children () . map ( |child| child . id () ) . collect ();
  for model_child in model_children {
    append_copy_of_subtree (tree, copy, model, model_child) ?; }
  Ok (copy) }

/// A removed member: a viewnode the diff view made as a non-member
/// with '(rel removed)', because its relationship to its viewparent is
/// only in the before-graph (or, while a visit reconciles, is about
/// to be). That is the one encoding of a removed relationship, in
/// content, in a partner folder and in a historical phantom's old
/// content ('mk_removed_member_viewnode', 'FolderGoalShape::Removed'),
/// and this is its one reader. Where it stands is not read: the
/// diff view gives change facts to no other non-member, and a node of
/// the containerward chain, the one other non-member it draws, carries
/// none.
pub fn is_unrestricted_removed_member (
  viewnode : &Viewnode,
) -> bool {
  matches! ( &viewnode . kind,
    ViewnodeKind::Vognode (Vognode::Unrestricted (t))
      if t . membership == Membership::NonMember
         && viewnode . change_facts . change . rel == Some (Existence::Removed) ) }
