//! Reconciling what the diff view drew under a viewnode with a
//! fresh drawing of it ('docs/diff-view.org',
//! "The fresh drawing, and what it keeps").
//! The fresh drawing is built in a scratch tree. Each existing
//! viewnode it also produces is kept, with its fold state, and its
//! kind and facts are updated in place; each it does not is
//! detached, unless something the user added stands below it
//! ('keep_stale_child_holding_the_users', the rule both reconcilers
//! obey); what only it has is copied in. A viewnode that stands
//! for a graphnode is matched by the ID it stands for, any other by
//! its kind (and a folder entry by the entry): within one parent each
//! key appears at most once.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::to_org::complete::partner_folder::child_data::skgid_a_folder_child_stands_for;
use crate::types::git::Sign;
use crate::types::misc::ID;
use crate::types::tree::moves::arrange_last_children;
use crate::types::viewnode::{
  AffectsParent, Birth, ChangeFacts, DiffPresence, Property, PropertyFolder,
  Viewnode, ViewnodeKind, Vognode};
use crate::update_buffer::util::{
  detach_viewnode_transferring_focus, keep_stale_child_holding_the_users};

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
/// same way, every keyed child but the user's managed
/// ('is_drawn_by_the_diff_view'), and ordered as the fresh drawing
/// orders them. Returns the kept and created children, in
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
          let in_folder : bool =
            is_folder ( tree . get (existing) . unwrap () . value () );
          let kept : Vec<NodeId> = reconcile_children_with_fresh_drawing (
            tree, existing, fresh, fresh_child, graph,
            &|viewnode| is_drawn_by_the_diff_view (viewnode, in_folder, graph),
            descend ) ?;
          arrange_last_children (tree, existing, &kept) ?; }
        result . push (existing); },
      None => result . push (
        append_copy_of_subtree (tree, parent, fresh, fresh_child) ?), }}
  for (stale, _) in unmatched {
    if keep_stale_child_holding_the_users (tree, stale, &holds_the_users) ? {
      detach_the_servers_below (tree, stale) ?;
    } else {
      detach_viewnode_transferring_focus (tree, stale) ?; }}
  Ok (result) }

/// Below a stale viewnode kept for what the user added under it,
/// detach what the diff view drew there that holds nothing of the
/// user's, as it would have been with the viewnode.
fn detach_the_servers_below (
  tree : &mut Tree<Viewnode>,
  node : NodeId,
) -> Result<(), Box<dyn Error>> {
  let (in_folder, children) : (bool, Vec<NodeId>) = {
    let node_ref : NodeRef<Viewnode> = tree . get (node)
      . ok_or ("detach_the_servers_below: node not found") ?;
    ( is_folder ( node_ref . value () ),
      node_ref . children () . map ( |child| child . id () ) . collect () ) };
  for child in children {
    let (users, holds_users) : (bool, bool) = {
      let child_ref : NodeRef<Viewnode> = tree . get (child) . unwrap ();
      ( is_the_users_viewnode ( child_ref . value (), in_folder ),
        holds_the_users (child_ref) ) };
    if users { continue; }
    if holds_users { detach_the_servers_below (tree, child) ?; }
    else { detach_viewnode_transferring_focus (tree, child) ?; }}
  Ok (( )) }

/// Whether a viewnode below what the diff view drew is the user's:
/// an unrestricted vognode born 'Unremarkable' that carries no
/// '(diff ...)' fact and is not a member of the folder it stands in
/// ('in_folder'). A role graft ('Birth::RoleGraft', which a buffer
/// carries back), a removed member and a folder's member are the
/// server's, as is every viewnode of another kind.
pub fn is_the_users_viewnode (
  viewnode  : &Viewnode,
  in_folder : bool,
) -> bool {
  matches! ( &viewnode . kind,
    ViewnodeKind::Vognode (Vognode::Unrestricted (t))
      if t . birth == Birth::Unremarkable
         && t . change_facts . presence == DiffPresence::default ()
         && ! ( in_folder && t . affectsParent == AffectsParent::True )) }

/// What the diff view's reconciler manages below what it drew,
/// where it descends and under a member of a changed folder or a
/// historical phantom: every viewnode with a match key except the
/// user's, which stays where it stands. 'in_folder' is whether its
/// parent is a folder.
pub fn is_drawn_by_the_diff_view (
  viewnode  : &Viewnode,
  in_folder : bool,
  graph     : &InRustGraph,
) -> bool {
  match_key (viewnode, graph) . is_some ()
  && ! is_the_users_viewnode (viewnode, in_folder) }

/// Whether something of the user's ('is_the_users_viewnode') stands
/// anywhere below 'node'.
pub fn holds_the_users (
  node : NodeRef<Viewnode>,
) -> bool {
  node . descendants () . skip (1)
    . any ( |below| is_the_users_viewnode (
      below . value (),
      below . parent () . is_some_and ( |parent| is_folder ( parent . value () )) )) }

fn is_folder (
  viewnode : &Viewnode,
) -> bool {
  matches! ( viewnode . kind,
    ViewnodeKind::PartnerFolder (_) | ViewnodeKind::PropertyFolder (_) ) }

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

/// A removed member: a viewnode the diff view drew as a non-member
/// with '(diff rel-)', because its relationship to its viewparent is
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
         && t . change_facts . presence . rel == Some (Sign::Minus) ) }
