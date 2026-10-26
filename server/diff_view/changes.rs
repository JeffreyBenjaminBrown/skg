//! The diff view's step at each unrestricted vognode's visit
//! in view completion ('docs/diff-view.org', "The
//! per-visit step" and "What each kind of change looks like"), and at each
//! historical phantom's.
//!
//! On an editable occurrence: the content members (which the content
//! reconciler made from the after-graph) are tagged with the three
//! kinds of change (their node's existence and other properties, and
//! their relationship's existence), members outside U that
//! were not editable already become write-protected members
//! without their content, and the former members stand where
//! 'phantom_insertion_plan' places them. A node in C also gets, ahead
//! of its content, its old text, its home skgrepo move, its "it acquired"
//! folder, the alias, ID and flags folders that changed, the partner
//! folders whose membership changed (which the BFS fills from the
//! straddle), and its changeset markers. A node in U but not in C is
//! context: its body starts folded. A write-protected occurrence gets
//! none of this; its viewparent tags it. U and C are those of the
//! buffer being rendered ('prunedChangeView_sets_of_buffer'); in the
//! order-only buffer both are empty, and each node there gets an
//! order-only marker.
//!
//! Nothing that exists is created again ('docs/diff-view.org',
//! "The shallow model subtree, and what it keeps"):
//! each step builds a shallow model subtree and reconciles what the node
//! holds with it ('server/diff_view/shallow_model_subtree.rs'), so every
//! viewnode that stays keeps its fold state and where it stands; only
//! a former member is moved to where it belongs
//! ('reconcile_removed_content_members'). The server sets a fold
//! state only on a viewnode it creates.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::{
  NodeRelation, RelationRole};
use crate::graph_straddle::types::{GraphStraddle, Partner};
use crate::diff_view::change_leaves::{
  ChangeLeaves, LeafKey, NodeAspect, RelationshipAspect};
use crate::diff_view::pruned_change_view::{
  PrunedChangeViewSets, prunedChangeView_sets_of_buffer};
use crate::diff_view::change_viewnodes::{
  append_acquired_folder, append_historical_phantom_content, append_old_text,
  mk_changeset_marker_viewnode, mk_historical_viewnode, mk_homeSkgrepoMove_viewnode,
  mk_relRepo_move_viewnode, mk_removed_member_viewnode};
use crate::diff_view::shallow_model_subtree::{
  give_model_facts_to, is_diffView_change_viewnode,
  reconcile_children_with_shallowModelSubtree};
use crate::types::list::{Diff_Item, interleaved_diff_without_reorders};
use crate::types::misc::ID;
use crate::types::nodes::rust::GraphnodeInRust;
use crate::types::nodes::complete::Flag;
use crate::types::tree::forest::in_content_position;
use crate::types::tree::moves::{
  move_before, move_to_last_child,
  place_created_children_after_their_model_predecessors};
use crate::types::views_state::DiffViewBufferKind;
use crate::types::viewnode::{
  ChangeFacts, ChangesetMarkerKind, DiffChange, Existence,
  Editability, Folder, PropertyMember, ChangeViewnode, Viewnode, ViewnodeKind, Vognode,
  mk_editable_viewnode, viewforest_root_viewnode,
  viewnode_of};
use crate::update_buffer::util::{Unwarranted, convert_unwarranted_child};

use ego_tree::{NodeId, NodeMut, NodeRef, Tree};
use std::collections::{BTreeSet, HashMap, HashSet};
use std::error::Error;
use std::hash::Hash;
use std::mem::discriminant;

/// The folders of a node's recorders (its in-set changed when one
/// was added or removed), then those of the relationships it records
/// (changed when one was added, removed or moved to another skgrepo).
const IN_FOLDERS : [Folder; 5] = [
  Folder::Container, Folder::Mentioner,
  Folder::Subscriber, Folder::Overrider,
  Folder::Hider ];
const OUT_FOLDERS : [Folder; 3] = [
  Folder::Subscribee, Folder::Overridden,
  Folder::Hidden ];

/// What the change leaves that mention a node say changed about it,
/// grouped by aspect: what the per-visit step adds ahead of its
/// content and as its changed partner folders. A new aspect of a leaf
/// is added here once, and C, the extension and readiness follow.
#[derive(Debug, Default)]
pub(crate) struct ChangedAspects {
  text         : bool,
  home_skgrepo : bool,
  aliases      : bool,
  extra_ids    : bool,
  flags        : bool,
  /// The relations of the relationships the node records that were
  /// added or removed (existence) or moved (relRepo).
  recorded     : HashSet<(NodeRelation, RelationshipAspect)>,
  /// The relations by which a recorder gained or lost the node.
  recorders    : HashSet<NodeRelation>,
  /// Mentions are no leaves; see 'ChangeLeaves::mentioner_sets_changed'.
  mentioners   : bool,
}

impl ChangedAspects {
  pub(crate) fn of_node (
    leaves : &ChangeLeaves,
    pid    : &ID,
  ) -> ChangedAspects {
    let mut aspects : ChangedAspects = ChangedAspects {
      mentioners : leaves . mentioner_sets_changed () . contains (pid),
      .. ChangedAspects::default () };
    for leaf in leaves . leaves_under (pid) {
      match &leaf . key {
        LeafKey::Node { aspect, .. } => match aspect {
          NodeAspect::Existence    => {},
          NodeAspect::Text         => aspects . text         = true,
          NodeAspect::HomeSkgrepo  => aspects . home_skgrepo = true,
          NodeAspect::Alias (_)    => aspects . aliases      = true,
          NodeAspect::ExtraId (_)  => aspects . extra_ids    = true,
          NodeAspect::Flag (_)     => aspects . flags        = true, },
        LeafKey::Relationship { relationship, aspect } => {
          aspects . recorded . insert (( relationship . relation, *aspect )); }, }}
    for leaf in leaves . leaves_naming_member (pid) {
      if let LeafKey::Relationship {
        relationship, aspect : RelationshipAspect::Existence } = &leaf . key {
        aspects . recorders . insert ( relationship . relation ); }}
    aspects }

  /// Whether a recorded relationship of this relation changed in this
  /// aspect.
  fn recorded_changed (
    &self,
    relation : NodeRelation,
    aspect   : RelationshipAspect,
  ) -> bool {
    self . recorded . contains (&( relation, aspect )) }
}

/// See the module comment.
/// Two reconcilers run at an editable node's visit. The older one,
/// 'complete_managed_children' ('update_buffer/util.rs'), reconciles
/// the content list first, as in every view (and each folder's
/// members at the folder's own visit); then this step runs the diff
/// view's, 'reconcile_children_with_shallowModelSubtree', over the change
/// viewnodes, the removed members and what stands below them, matching
/// by key whoever drew a child. What the user added survives, under a
/// stale child too: both convert only the stale children they own
/// ('convert_unwarranted_child') and detach nothing. A property folder
/// the step's model lacks is never converted: it is unmarked
/// ('unmark_property_folders_except'), and its own visit refreshes it
/// from the after-graph. What this step keeps stays where it stands;
/// what it creates goes after what precedes it here: the change
/// viewnodes ahead of the markers, then the changed partner folders,
/// then the markers.
/// 'created_in_this_completion' is whether
/// view completion created the viewnode at 'treeid': only then does
/// its body's fold state come from C. 'editable_before_completion'
/// holds the viewnodes that were editable when completion began.
pub(crate) fn process_unrestrictedVognode_changes (
  tree                       : &mut Tree<Viewnode>,
  treeid                     : NodeId,
  straddle                   : &GraphStraddle,
  diffView_buffer_kind       : DiffViewBufferKind,
  created_in_this_completion : bool,
  editable_before_completion : &HashSet<NodeId>,
) -> Result<(), Box<dyn Error>> {
  let (pid, editable) : (ID, bool) =
    match &tree . get (treeid)
      . ok_or ("process_unrestrictedVognode_changes: node not found") ?
      . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
        ( t . skgid . clone (), ! t . is_writeProtected () ),
      _ => return Ok (( )), };
  tag_root (tree, treeid, straddle, &pid) ?;
  let content : Vec<Partner> =
    straddle . partners (&pid, RelationRole::CONTENT);
  if ! editable {
    // What an earlier visit added here while it was editable goes, and
    // the members it still holds are tagged as they stand now.
    convert_change_viewnodes (tree, treeid, straddle) ?;
    unmark_property_folders_except (tree, treeid, &[]) ?;
    tag_content_members (tree, treeid, straddle, &content, None) ?;
    return Ok (( )); }
  let sets : &PrunedChangeViewSets =
    prunedChangeView_sets_of_buffer (straddle, diffView_buffer_kind);
  let in_c : bool = sets . c . contains (&pid);
  if created_in_this_completion {
    tree . get_mut (treeid) . unwrap () . value () . foldAndFocus . bodyFolded = ! in_c; }
  tag_content_members (
    tree, treeid, straddle, &content,
    Some (( &sets . u, editable_before_completion )) ) ?;
  reconcile_removed_content_members (tree, treeid, straddle, &pid, &sets . u) ?;
  let aspects : ChangedAspects = // read only for a node in C
    if in_c { ChangedAspects::of_node (straddle . change_leaves (), &pid) }
    else { ChangedAspects::default () };
  let model : Tree<Viewnode> = model_of_front_change_viewnodes (
    straddle, &pid, &aspects, in_c,
    diffView_buffer_kind == DiffViewBufferKind::OrderOnlyChanges
      && straddle . order_only_pids () . contains (&pid) );
  let children_before : HashSet<NodeId> =
    tree . get (treeid) . unwrap () . children ()
    . map ( |child| child . id () ) . collect ();
  let model_property_folders : Vec<ViewnodeKind> = // the folders that changed
    model . root () . children ()
    . filter ( |child| child . value () . kind . is_property_folder () )
    . map ( |child| child . value () . kind . clone () ) . collect ();
  let fronts : Vec<NodeId> = reconcile_children_with_shallowModelSubtree (
    tree, treeid, &model, model . root () . id (), &straddle . after,
    &is_diffView_change_viewnode,
    &|viewnode| is_front_change_viewnode (viewnode)
      && ( ! viewnode . kind . is_property_folder ()
           || model_property_folders . iter () . any ( |kind|
                same_property_folder_kind (kind, &viewnode . kind) )),
    &|viewnode| ! matches! ( viewnode . kind, ViewnodeKind::Vognode (_) )) ?;
  unmark_property_folders_except (tree, treeid, &fronts) ?;
  let partner_folders : Vec<NodeId> =
    if in_c { changed_partner_folders (tree, treeid, &aspects) }
    else { Vec::new () };
  let is_marker = |treeid : &NodeId| -> bool {
    matches! ( tree . get (*treeid) . unwrap () . value () . kind,
               ViewnodeKind::ChangeViewnode (ChangeViewnode::ChangesetMarker { .. }) ) };
  let order : Vec<NodeId> =
    fronts . iter () . filter ( |front| ! is_marker (front) ) . copied ()
    . chain (partner_folders)
    . chain ( fronts . iter () . filter ( |front| is_marker (front) ) . copied () )
    . collect ();
  let created : HashSet<NodeId> = order . iter ()
    . filter ( |front| ! children_before . contains (front) ) . copied () . collect ();
  place_created_children_after_their_model_predecessors (
    tree, treeid, &order, &created ) ?;
  Ok (( )) }

/// A root is tagged at its own visit, as a member is at its
/// viewparent's ('tag_content_members'): '(node added)' when new,
/// '(properties changed)' when modified ('GraphStraddle::node_change').
/// Nothing for a non-root.
fn tag_root (
  tree     : &mut Tree<Viewnode>,
  treeid   : NodeId,
  straddle : &GraphStraddle,
  pid      : &ID,
) -> Result<(), Box<dyn Error>> {
  let is_root : bool =
    tree . get (treeid) . ok_or ("tag_root: node not found") ?
    . parent () . is_some_and ( |parent|
      matches! ( parent . value () . kind, ViewnodeKind::BufferRoot ));
  if is_root {
    tree . get_mut (treeid) . unwrap () . value ()
      . change_facts . change = straddle . node_change (pid, None); }
  Ok (( )) }

/// Before the content reconciler runs at an editable node's visit,
/// align its members with the straddle, so that the reconciler keeps
/// each viewnode instead of creating another: a historical phantom
/// whose relationship is in the after-graph is a member, and so is the
/// first non-member of a node in the after-graph's list when no member
/// of that node stands there (a removed member whose relationship
/// returned, or a copy the user pasted, whoever drew it); a member, or
/// any non-member, whose relationship is only in the before-graph is
/// retained as non-member, for the diff view's step after the content
/// reconciler to find and show as removed. So a copy the user pasted
/// is retained as non-member and shown as the removed member, not
/// doubled by it.
pub(crate) fn align_content_members_with_straddle (
  tree     : &mut Tree<Viewnode>,
  treeid   : NodeId,
  straddle : &GraphStraddle,
) -> Result<(), Box<dyn Error>> {
  let Some (pid) : Option<ID> = tree . get (treeid)
    . ok_or ("align_content_members_with_straddle: node not found") ?
    . value () . editable_skgid () . cloned ()
    else { return Ok (( )); };
  let after_list : Vec<ID> = content_list (&straddle . after, &pid);
  let after_set : HashSet<&ID> = after_list . iter () . collect ();
  let removed : HashSet<ID> = removed_content_members (straddle, &pid);
  let children : Vec<NodeId> =
    tree . get (treeid) . unwrap () . children ()
    . map ( |child| child . id () ) . collect ();
  let mut nodes_with_a_member : HashSet<ID> =
    tree . get (treeid) . unwrap () . children ()
    . filter_map ( |child| match &child . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t))
        if t . claimsMembership => Some ( t . skgid . clone () ),
      _ => None, } )
    . collect ();
  for child in children {
    let mut child_mut : NodeMut<Viewnode> = tree . get_mut (child) . unwrap ();
    let viewnode : &mut Viewnode = child_mut . value ();
    let returned : Option<Viewnode> = match &mut viewnode . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t)) => {
        if removed . contains (&t . skgid) {
          t . claimsMembership = false; }
        else if ! t . claimsMembership
                && after_set . contains (&t . skgid)
                && nodes_with_a_member . insert ( t . skgid . clone () ) {
          t . claimsMembership = true; }
        None },
      ViewnodeKind::Vognode (Vognode::Ghost (h))
        if after_set . contains (&h . skgid) =>
        straddle . after . get (&h . skgid) . map ( |node|
          mk_editable_viewnode (
            h . skgid . clone (), node . home_skgrepo . clone (),
            node . title . clone (), node . body . clone () )),
      _ => None, };
    if let Some (member) = returned {
      viewnode . replace_kind (member . kind); }}
  Ok (( )) }

/// What a historical phantom's visit did.
#[derive(Debug, PartialEq, Eq)]
pub(crate) enum HistoricalPhantomVisit {
  Done,
  /// The node is back, so the root is an editable vognode again; the
  /// caller visits it as one.
  RootIsLive,
}

/// The visit of a ghost in a diff view. Where
/// content stands (under a vognode, or as a root), it holds what
/// 'append_historical_phantom_content' adds, reconciled with what it
/// holds already; a node standing above itself holds nothing. In a
/// folder it holds nothing new. A root whose node is back becomes an
/// editable vognode, and one whose node is in neither graph (a
/// deletion since committed) a tombstone ('entombstone_ghost'). A removed member of
/// its old content is created editable when its node is in U of the
/// buffer being completed ('prunedChangeView_sets_of_buffer'); one that
/// stands already keeps its editability ('give_model_facts_to'). A
/// ghost has no content list, so its old content adopts
/// the content reconciler's former members under it, and converts the
/// rest of them ('is_diffView_change_viewnode').
pub(crate) fn process_historical_phantom_visit (
  tree                 : &mut Tree<Viewnode>,
  treeid               : NodeId,
  straddle             : &GraphStraddle,
  diffView_buffer_kind : DiffViewBufferKind,
) -> Result<HistoricalPhantomVisit, Box<dyn Error>> {
  let (skgid, at_root, in_content_position) : (ID, bool, bool) = {
    let node : NodeRef<Viewnode> = tree . get (treeid)
      . ok_or ("process_historical_phantom_visit: node not found") ?;
    let ViewnodeKind::Vognode (Vognode::Ghost (h)) =
      &node . value () . kind else {
        return Ok (HistoricalPhantomVisit::Done); };
    ( h . skgid . clone (),
      node . parent () . is_some_and ( |parent|
        parent . value () . kind == ViewnodeKind::BufferRoot ),
      in_content_position (node) ) };
  if at_root {
    if let Some (node) = straddle . after . get (&skgid) {
      tree . get_mut (treeid) . unwrap () . value () . replace_kind (
        mk_editable_viewnode (
          skgid . clone (), node . home_skgrepo . clone (),
          node . title . clone (), node . body . clone () ) . kind );
      return Ok (HistoricalPhantomVisit::RootIsLive); }}
  let Some (model_phantom) : Option<Viewnode> =
    mk_historical_viewnode (straddle, &skgid, None) else {
      // In neither graph.
      entombstone_ghost (tree, treeid, straddle) ?;
      return Ok (HistoricalPhantomVisit::Done); };
  if at_root {
    give_model_facts_to (
      tree . get_mut (treeid) . unwrap () . value (), &model_phantom ); }
  if ! in_content_position { return Ok (HistoricalPhantomVisit::Done); }
  let standing_above_itself : bool =
    tree . get (treeid) . unwrap () . ancestors ()
    . any ( |ancestor| matches! ( &ancestor . value () . kind,
      ViewnodeKind::Vognode (Vognode::Ghost (h))
        if h . skgid == skgid ));
  let mut model : Tree<Viewnode> = Tree::new (model_root ());
  if ! standing_above_itself {
    let root : NodeId = model . root () . id ();
    append_historical_phantom_content (
      &mut model, root, straddle, &skgid,
      & prunedChangeView_sets_of_buffer (straddle, diffView_buffer_kind) . u ); }
  reconcile_children_with_shallowModelSubtree (
    tree, treeid, &model, model . root () . id (), &straddle . after,
    &is_diffView_change_viewnode, &|_| true,
    &|viewnode| ! matches! ( viewnode . kind, ViewnodeKind::Vognode (_) )) ?;
  Ok (HistoricalPhantomVisit::Done) }

/// When the graph lacks the node of the occurrence at 'treeid' and the
/// straddle holds it as only-before, the occurrence becomes the node's
/// ghost, in place. Returns whether it did.
pub(crate) fn vanished_occurrence_becomes_ghost (
  tree     : &mut Tree<Viewnode>,
  treeid   : NodeId,
  straddle : &GraphStraddle,
) -> Result<bool, Box<dyn Error>> {
  let (skgid, parent_pid) : (ID, Option<ID>) = {
    let node : NodeRef<Viewnode> = tree . get (treeid)
      . ok_or ("vanished_occurrence_becomes_ghost: node not found") ?;
    let ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =
      &node . value () . kind else { return Ok (false); };
    ( t . skgid . clone (),
      node . parent () . and_then ( |parent| match &parent . value () . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
          Some ( t . skgid . clone () ),
        _ => None, } )) };
  if straddle . node_existence (&skgid) != Some (Existence::Removed) {
    return Ok (false); }
  let rel : Option<Existence> = // its relationship to a viewparent that contained it
    parent_pid . and_then ( |parent_pid|
      straddle . partners (&parent_pid, RelationRole::CONTENT) . into_iter ()
      . find ( |partner| partner . id == skgid )
      . and_then ( |partner| partner . existence ));
  let Some (ghost) : Option<Viewnode> =
    mk_historical_viewnode (straddle, &skgid, rel) else { return Ok (false); };
  let mut node_mut : NodeMut<Viewnode> = tree . get_mut (treeid) . unwrap ();
  let viewnode : &mut Viewnode = node_mut . value ();
  viewnode . replace_kind (ghost . kind);
  viewnode . change_facts = ghost . change_facts;
  Ok (true) }

/// Make the ghost at 'treeid' a tombstone, which keeps what the user
/// put under it; what the diff view drew there goes
/// ('is_diffView_change_viewnode'). A tombstone left childless is
/// pruned.
pub(crate) fn entombstone_ghost (
  tree     : &mut Tree<Viewnode>,
  treeid   : NodeId,
  straddle : &GraphStraddle,
) -> Result<(), Box<dyn Error>> {
  if let ViewnodeKind::Vognode (Vognode::Ghost (ghost)) =
    &tree . get (treeid) . ok_or ("entombstone_ghost: node not found") ?
    . value () . kind
  { // The parser skips the change facts by which the diff view knows
    // what it drew, so first match the ghost's children with its
    // content at HEAD, which gives each such child its facts again.
    let skgid : ID = ghost . skgid . clone ();
    let mut model : Tree<Viewnode> = Tree::new (model_root ());
    let root : NodeId = model . root () . id ();
    append_historical_phantom_content (
      &mut model, root, straddle, &skgid, &BTreeSet::new () );
    reconcile_children_with_shallowModelSubtree (
      tree, treeid, &model, root, &straddle . after,
      &|_| false, &|_| true, &|_| false ) ?; }
  { let mut node_mut : NodeMut<Viewnode> = tree . get_mut (treeid)
      . ok_or ("entombstone_ghost: node not found") ?;
    let viewnode : &mut Viewnode = node_mut . value ();
    viewnode . replace_kind (ViewnodeKind::Tombstone);
    viewnode . change_facts = ChangeFacts::default ();
    viewnode . readiness_request = None; }
  let empty : Tree<Viewnode> = Tree::new (model_root ());
  reconcile_children_with_shallowModelSubtree (
    tree, treeid, &empty, empty . root () . id (), &straddle . after,
    &is_diffView_change_viewnode, &|_| true, &|_| false ) ?;
  Ok (( )) }

/// The root of a scratch tree that holds a shallow model subtree.
fn model_root (
) -> Viewnode {
  viewforest_root_viewnode () }

/// Of the diff view's viewnodes, those an editable node holds ahead of
/// its content: its change viewnodes. Partner folders other than the
/// derived "it acquired" folder are not among them: the node's own
/// visit finds or makes them, and their own visits reconcile them.
fn is_front_change_viewnode (
  viewnode : &Viewnode,
) -> bool {
  viewnode . kind . is_property_folder ()
  || matches! ( &viewnode . kind,
    ViewnodeKind::ChangeViewnode (
      ChangeViewnode::OldText { .. } | ChangeViewnode::HomeSkgrepoMove { .. }
      | ChangeViewnode::ChangesetMarker { .. } )
    | ViewnodeKind::Folder (Folder::Acquired) ) }

/// Convert what this step adds under an editable node: its change
/// viewnodes, its removed members and the historical phantoms of its
/// deleted ones. Its property folders stay, for the caller to unmark.
fn convert_change_viewnodes (
  tree     : &mut Tree<Viewnode>,
  treeid   : NodeId,
  straddle : &GraphStraddle,
) -> Result<(), Box<dyn Error>> {
  let empty : Tree<Viewnode> = Tree::new (model_root ());
  reconcile_children_with_shallowModelSubtree (
    tree, treeid, &empty, empty . root () . id (), &straddle . after,
    &is_diffView_change_viewnode,
    &|viewnode| ( is_front_change_viewnode (viewnode)
                  && ! viewnode . kind . is_property_folder () )
      || matches! ( &viewnode . kind,
        ViewnodeKind::Vognode (
          Vognode::Unrestricted (_) | Vognode::Ghost (_) )),
    &|_| false ) ?;
  Ok (( )) }

/// Whether two kinds are property folders of one kind, whatever
/// their flags.
fn same_property_folder_kind (
  a : &ViewnodeKind,
  b : &ViewnodeKind,
) -> bool {
  match (a, b) {
    ( ViewnodeKind::Folder (a), ViewnodeKind::Folder (b) )
      if a . members_are_properties () && b . members_are_properties () =>
      discriminant (a) == discriminant (b),
    _ => false, }}

/// Unmark every property folder under 'treeid' but those in 'kept'
/// (the changed ones, which the per-visit step just reconciled with
/// the union of the straddle): a lost alias or ID it holds is
/// converted ('convert_unwarranted_child'), and every other entry
/// loses its change facts. The folder stays, whoever drew it, until
/// the user deletes it; its own visit refreshes it from the
/// after-graph ('diffView_visit_before_extension').
fn unmark_property_folders_except (
  tree   : &mut Tree<Viewnode>,
  treeid : NodeId,
  kept   : &[NodeId],
) -> Result<(), Box<dyn Error>> {
  let entries : Vec<NodeId> =
    tree . get (treeid)
    . ok_or ("unmark_property_folders_except: node not found") ?
    . children ()
    . filter ( |child| child . value () . kind . is_property_folder ()
                       && ! kept . contains (&child . id ()) )
    . flat_map ( |folder| folder . children () . map ( |entry| entry . id () ))
    . collect ();
  for entry in entries {
    let lost : bool = {
      let mut entry_mut : NodeMut<Viewnode> = tree . get_mut (entry) . unwrap ();
      let viewnode : &mut Viewnode = entry_mut . value ();
      let lost : bool = // an entry HEAD alone warrants
        viewnode . is_ghostWarranted ();
      viewnode . change_facts = ChangeFacts::default ();
      lost };
    if lost {
      let is_leaf : bool =
        tree . get (entry) . unwrap () . children () . next () . is_none ();
      convert_unwarranted_child (tree, entry, is_leaf, Unwarranted::Stale) ?; }}
  Ok (( )) }

/// The shallow model subtree of what an editable node holds ahead of its
/// content, in order, partner folders aside: for a node in C, its old
/// text, its home skgrepo move, its "it acquired" folder, its alias, ID and
/// flags folders that changed, and its content and relRepo markers,
/// each as 'aspects' says; then, in the order-only buffer, its
/// order-only marker.
pub(crate) fn model_of_front_change_viewnodes (
  straddle   : &GraphStraddle,
  pid        : &ID,
  aspects    : &ChangedAspects,
  in_c       : bool,
  order_only : bool,
) -> Tree<Viewnode> {
  let mut model : Tree<Viewnode> = Tree::new (model_root ());
  let root : NodeId = model . root () . id ();
  if in_c {
    if aspects . text {
      append_old_text (&mut model, root, straddle, pid); }
    if aspects . home_skgrepo {
      if let Some (homeSkgrepoMove) = mk_homeSkgrepoMove_viewnode (straddle, pid) {
        append (&mut model, root, homeSkgrepoMove); }}
    append_acquired_folder (&mut model, root, straddle, pid);
    append_changed_property_folders (&mut model, root, straddle, pid, aspects);
    if aspects . recorded_changed (
      NodeRelation::Contains, RelationshipAspect::Existence ) {
      append ( &mut model, root,
        mk_changeset_marker_viewnode (ChangesetMarkerKind::Content) ); }
    if aspects . recorded_changed (
      NodeRelation::Contains, RelationshipAspect::RelRepo ) {
      append ( &mut model, root,
        mk_changeset_marker_viewnode (ChangesetMarkerKind::RelRepo) ); }}
  if order_only {
    append ( &mut model, root,
      mk_changeset_marker_viewnode (ChangesetMarkerKind::OrderOnly) ); }
  model }

/// The node's partner folders whose membership changed, each found
/// or created: the folders of its recorders (changed when one was
/// added or removed), then those of the relationships it records
/// (changed when one was added, removed or moved to another skgrepo).
/// The view completion's visit of each fills it from the straddle.
fn changed_partner_folders (
  tree    : &mut Tree<Viewnode>,
  treeid  : NodeId,
  aspects : &ChangedAspects,
) -> Vec<NodeId> {
  changed_partner_folder_kinds (aspects) . iter ()
    . map ( |kind| child_folder_or_new (tree, treeid, kind) )
    . collect () }

/// The kinds of the node's partner folders whose membership changed,
/// in the order 'changed_partner_folders' finds or creates them.
pub(crate) fn changed_partner_folder_kinds (
  aspects : &ChangedAspects,
) -> Vec<Folder> {
  IN_FOLDERS . into_iter () . chain (OUT_FOLDERS)
    . filter ( |kind| {
      let relation : NodeRelation = kind . relation_member_role ()
        . expect ("a relation folder has a member role") . relation;
      if OUT_FOLDERS . contains (kind) {
        aspects . recorded_changed (relation, RelationshipAspect::Existence)
        || aspects . recorded_changed (relation, RelationshipAspect::RelRepo) }
      else if relation == NodeRelation::LinksTo { aspects . mentioners }
      else { aspects . recorders . contains (&relation) } } )
    . collect () }

/// Tag each content member, and each non-member child whose node is
/// content, with the three kinds of change of its node and its relationship,
/// show a member outside U as a write-protected member without its
/// content unless it was
/// editable before completion ('u_and_editable', when given), and
/// keep a relRepo move under exactly the members whose relationship
/// moved. A member's view requests stay: one that
/// carries an editable-view request is extended as the request says at
/// its own visit.
fn tag_content_members (
  tree     : &mut Tree<Viewnode>,
  treeid   : NodeId,
  straddle : &GraphStraddle,
  content  : &[Partner],
  u_and_editable : Option<( &BTreeSet<ID>, &HashSet<NodeId> )>,
) -> Result<(), Box<dyn Error>> {
  let by_id : HashMap<&ID, &Partner> =
    content . iter () . map ( |partner| (&partner . id, partner) ) . collect ();
  let members : Vec<NodeId> =
    tree . get (treeid) . unwrap () . children ()
    . map ( |child| child . id () ) . collect ();
  for member in members {
    let mut moved : Option<Viewnode> = None;
    { let mut member_mut = tree . get_mut (member) . unwrap ();
      let viewnode : &mut Viewnode = member_mut . value ();
      match &mut viewnode . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (t))
          if t . claimsMembership
          || by_id . contains_key (&t . skgid) => {
          // A non-member whose node is content (as under a
          // write-protected node) is tagged too, but never recreated.
          let partner : Option<&&Partner> = by_id . get (&t . skgid);
          viewnode . change_facts . change = straddle . node_change (
            &t . skgid, partner . and_then ( |p| p . existence ));
          if t . claimsMembership
             && u_and_editable . is_some_and ( |(u, writable)|
            ! u . contains (&t . skgid) && ! writable . contains (&member) ) {
            // Not 'write_protect': the member keeps its view requests.
            t . editability = Editability::WriteProtected; }
          moved = partner . and_then ( |p| mk_relRepo_move_viewnode (p) ); },
        ViewnodeKind::Vognode (Vognode::Unknown (unknown)) => {
          viewnode . change_facts . change . rel =
            by_id . get (&unknown . skgid)
            . and_then ( |p| p . existence ); },
        _ => continue, }}
    let mut model : Tree<Viewnode> = Tree::new (model_root ());
    if let Some (moved) = moved {
      model . root_mut () . append (moved); }
    reconcile_children_with_shallowModelSubtree ( // a new move goes first
      tree, member, &model, model . root () . id (), &straddle . after,
      &is_diffView_change_viewnode,
      &|viewnode| matches! ( &viewnode . kind,
        ViewnodeKind::ChangeViewnode (ChangeViewnode::RelRepoMove { .. }) ),
      &|_| false ) ?; }
  Ok (( )) }

/// The members of the before-graph's content list that the
/// after-graph's lacks, each where 'phantom_insertion_plan' places it
/// (immediately before the next surviving member of the old order): a
/// deleted node as its historical phantom (whose own visit adds its
/// old content), a surviving one as a removed member
/// ('mk_removed_member_viewnode'), created editable when its node is
/// in 'u'. Each is matched by its skgid to what
/// stands under the node already, and moved, not recreated. Membership
/// is by set difference; the LCS only positions, so a reorder shows
/// nothing. A removed dangling ID is not shown: an unknown phantom
/// here would be read back as a member. (Its change-forest leaf is
/// under the content changeset marker.)
fn reconcile_removed_content_members (
  tree     : &mut Tree<Viewnode>,
  treeid   : NodeId,
  straddle : &GraphStraddle,
  pid      : &ID,
  u        : &BTreeSet<ID>,
) -> Result<(), Box<dyn Error>> {
  let plan : Vec<(ID, Option<ID>)> = removal_plan (straddle, pid);
  let mut model : Tree<Viewnode> = Tree::new (model_root ());
  let mut anchors : Vec<Option<ID>> = Vec::new ();
  for (removed, anchor) in plan {
    let removed_viewnode : Option<Viewnode> =
      if straddle . node_existence (&removed) == Some (Existence::Removed) {
        mk_historical_viewnode (straddle, &removed, Some (Existence::Removed))
      } else if straddle . node_in_either_graph (&removed) {
        Some ( mk_removed_member_viewnode (straddle, &removed, u)
               . ok_or ("reconcile_removed_content_members: a surviving member is not in the after-graph") ? )
      } else { None };
    if let Some (removed_viewnode) = removed_viewnode {
      model . root_mut () . append (removed_viewnode);
      anchors . push (anchor); }}
  let kept : Vec<NodeId> = reconcile_children_with_shallowModelSubtree (
    tree, treeid, &model, model . root () . id (), &straddle . after,
    &is_diffView_change_viewnode,
    &|viewnode| matches! ( &viewnode . kind,
      ViewnodeKind::Vognode (
        Vognode::Unrestricted (_) | Vognode::Ghost (_) ))
      && ! viewnode . is_unrestrictedVognode_and_member (),
    &|_| false ) ?;
  let member_treeids : HashMap<ID, NodeId> =
    tree . get (treeid) . unwrap () . children ()
    . filter_map ( |child| match &child . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t))
        if t . claimsMembership =>
        Some (( t . skgid . clone (), child . id () )),
      ViewnodeKind::Vognode (Vognode::Unknown (u)) =>
        Some (( u . skgid . clone (), child . id () )),
      _ => None, } )
    . collect ();
  for (removed_treeid, anchor) in kept . into_iter () . zip (anchors) {
    match anchor . and_then ( |a| member_treeids . get (&a) . copied () ) {
      Some (anchor_treeid) => move_before (tree, anchor_treeid, removed_treeid) ?,
      None => move_to_last_child (tree, treeid, removed_treeid) ?, }}
  Ok (( )) }

/// 'phantom_insertion_plan' of the node's content lists.
fn removal_plan (
  straddle : &GraphStraddle,
  pid      : &ID,
) -> Vec<(ID, Option<ID>)> {
  phantom_insertion_plan ( &interleaved_diff_without_reorders (
    &content_list (&straddle . before, pid),
    &content_list (&straddle . after, pid) )) }

/// The members of the node's content list in the before-graph that
/// its list in the after-graph lacks.
fn removed_content_members (
  straddle : &GraphStraddle,
  pid      : &ID,
) -> HashSet<ID> {
  removal_plan (straddle, pid) . into_iter ()
    . map ( |(removed, _)| removed ) . collect () }

/// Decide where each removed-member phantom belongs among its surviving
/// siblings: insert it immediately before the *next* surviving child in
/// net HEAD->worktree order, or append it (anchor = None) when no surviving
/// child follows. Returns (removed_id, anchor_id) pairs in the order the
/// phantoms should be created, so consecutive removals before the same
/// survivor keep their relative order.
pub(crate) fn phantom_insertion_plan (
  net_contains : &[Diff_Item<ID>],
) -> Vec<(ID, Option<ID>)> {
  let mut plan : Vec<(ID, Option<ID>)> = Vec::new ();
  let mut next_survivor : Option<ID> = None;
  for item in net_contains . iter () . rev () {
    match item {
      Diff_Item::Unchanged (skgid) | Diff_Item::New (skgid)
        => { next_survivor = Some ( skgid . clone () ); },
      Diff_Item::Removed (skgid)
        => { plan . push ( ( skgid . clone (), next_survivor . clone () )); }, }}
  plan . reverse ();
  plan }

/// A node's content list in one graph, each member as that graph
/// resolves it.
fn content_list (
  graph : &InRustGraph,
  pid   : &ID,
) -> Vec<ID> {
  graph . outbound_member_keys_for_relation_gated (
    pid, NodeRelation::Contains, None )
    . into_iter ()
    . map ( |member| member . member . into_id () )
    . collect () }

/// Under 'treeid', the node's alias, ID and flags folders, those whose
/// list changed ('aspects'), each holding the union of the list: the
/// old order where the LCS puts a removed entry, + on a gained entry,
/// - on a lost one. A lost alias or ID is a member with a ghost
/// warrant, known by its '(rel removed)', which claims no membership
/// ('PropertyMember').
fn append_changed_property_folders (
  tree     : &mut Tree<Viewnode>,
  treeid   : NodeId,
  straddle : &GraphStraddle,
  pid      : &ID,
  aspects  : &ChangedAspects,
) {
  let (Some (before), Some (after)) =
    ( straddle . before . get (pid), straddle . after . get (pid) ) else {
      return; };
  if aspects . aliases {
    append_property_folder (
      tree, treeid, Folder::Alias,
      union_with_existence ( &before . alias_texts (), &after . alias_texts () )
      . into_iter ()
      . map ( |(text, existence)| entry_viewnode (
        PropertyMember::alias ( text, None, None ),
        existence ) )
      . collect () ); }
  if aspects . extra_ids {
    let ids = |node : &GraphnodeInRust| -> Vec<ID> {
      std::iter::once ( pid . clone () )
        . chain ( node . extra_ids . iter () . cloned () ) . collect () };
    append_property_folder (
      tree, treeid, Folder::ID,
      union_with_existence ( &ids (before), &ids (after) ) . into_iter ()
      . map ( |(skgid, existence)| entry_viewnode (
        PropertyMember::id (skgid), existence ) )
      . collect () ); }
  if aspects . flags {
    append_property_folder (
      tree, treeid, Folder::flags (),
      Flag::ALL . into_iter ()
      . filter_map ( |flag| {
        let existence : Option<Existence> =
          match ( before . flags . contains (&flag),
                  after  . flags . contains (&flag) ) {
            (false, true)  => Some (Existence::Added),
            (true,  false) => Some (Existence::Removed),
            (true,  true)  => None,
            (false, false) => return None, };
        Some ( entry_viewnode (
          PropertyMember::flag ( flag, String::new (), None ),
          existence )) } )
      . collect () ); }
  }

/// The entries of the union of two lists, in the order of their
/// interleaved diff, each added if only the new list holds it and
/// removed if only the old one does.
pub(crate) fn union_with_existence<T : Clone + Eq + Hash + Ord> (
  old : &[T],
  new : &[T],
) -> Vec<(T, Option<Existence>)> {
  let old_set : HashSet<&T> = old . iter () . collect ();
  interleaved_diff_without_reorders (old, new) . into_iter ()
    . map ( |item| match item {
      Diff_Item::Removed (x) => (x, Some (Existence::Removed)),
      Diff_Item::Unchanged (x) | Diff_Item::New (x) => {
        let existence : Option<Existence> =
          ( ! old_set . contains (&x) ) . then_some (Existence::Added);
        (x, existence) }, } )
    . collect () }

/// A property folder's entry, whose existence in the folder changed
/// by 'existence'.
fn entry_viewnode (
  property : PropertyMember,
  existence : Option<Existence>,
) -> Viewnode {
  Viewnode {
    change_facts : ChangeFacts {
      change : DiffChange { rel : existence, .. DiffChange::default () },
      ready    : None,
      leaves   : None,
      .. ChangeFacts::default () },
    .. viewnode_of ( ViewnodeKind::PropertyMember (property) ) } }

/// Under 'treeid', a folder 'kind' holding 'entries'.
fn append_property_folder (
  tree    : &mut Tree<Viewnode>,
  treeid  : NodeId,
  kind    : Folder,
  entries : Vec<Viewnode>,
) {
  let folder : NodeId =
    append ( tree, treeid, viewnode_of ( ViewnodeKind::Folder (kind) ));
  for entry in entries {
    append ( tree, folder, entry ); }}

/// The node's partner folder of this kind, or a new one appended.
fn child_folder_or_new (
  tree   : &mut Tree<Viewnode>,
  treeid : NodeId,
  kind   : &Folder,
) -> NodeId {
  let existing : Option<NodeId> =
    tree . get (treeid) . unwrap () . children ()
    . find ( |child| matches! ( &child . value () . kind,
                                ViewnodeKind::Folder (folder) if folder == kind ))
    . map ( |child| child . id () );
  existing . unwrap_or_else ( ||
    append ( tree, treeid, viewnode_of ( ViewnodeKind::Folder (kind . clone ()) ))) }

/// Append 'viewnode' under 'treeid'.
fn append (
  tree     : &mut Tree<Viewnode>,
  treeid   : NodeId,
  viewnode : Viewnode,
) -> NodeId {
  tree . get_mut (treeid) . unwrap () . append (viewnode) . id () }

#[cfg(test)]
#[path = "../../tests/unit/render_changes.rs"]
mod phantom_insertion_plan_tests;
