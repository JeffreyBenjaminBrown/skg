//! What changed about a node, read from the change leaves that
//! mention it ('ChangedAspects'), and the change viewnodes that say it
//! ahead of its content ('model_of_front_change_viewnodes'): its old
//! text, its home skgrepo move, its "it acquired" folder, its alias,
//! ID and flags folders that changed, and its changeset markers; and
//! which of its partner folders changed ('changed_partner_folder_kinds').
//! The refresh's diff-view mode ('server/refresh/diff_view.rs') draws
//! them ('docs/diff-view.org', "What each kind of change looks like").

use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::graph_straddle::types::GraphStraddle;
use crate::diff_view::change_leaves::{
  ChangeLeaves, LeafKey, NodeAspect, RelationshipAspect};
use crate::diff_view::change_viewnodes::{
  append_acquired_folder, append_old_text,
  mk_changeset_marker_viewnode, mk_homeSkgrepoMove_viewnode};
use crate::types::list::{Diff_Item, interleaved_diff_without_reorders};
use crate::types::misc::ID;
use crate::types::viewnode::{
  ChangesetMarkerKind, Existence, Folder, Viewnode, ViewnodeKind,
  viewforest_root_viewnode, viewnode_of};

use ego_tree::{NodeId, Tree};
use std::collections::HashSet;
use std::hash::Hash;

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
/// grouped by aspect: what the diff view draws ahead of its content
/// and as its changed partner folders. A new aspect of a leaf
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

/// A scratch tree whose root holds what an editable node holds ahead
/// of its content, in order, partner folders aside: for a node in C, its old
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
    append_changed_property_folders (&mut model, root, aspects);
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

/// The kinds of the node's partner folders whose membership changed,
/// in the order the diff view draws them: the folders of its recorders
/// (changed when one was added or removed), then those of the
/// relationships it records (changed when one was added, removed or
/// moved to another skgrepo).
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

/// The node's alias, ID and flags folders whose list changed, drawn
/// empty: the refresh fills each from its goal, the union of the two
/// graphs' lists.
fn append_changed_property_folders (
  tree    : &mut Tree<Viewnode>,
  treeid  : NodeId,
  aspects : &ChangedAspects,
) {
  for (changed, kind) in [ ( aspects . aliases,   Folder::Alias ),
                           ( aspects . extra_ids, Folder::ID ),
                           ( aspects . flags,     Folder::flags () ) ] {
    if changed {
      append ( tree, treeid, viewnode_of ( ViewnodeKind::Folder (kind) )); }}}

/// The root of a scratch tree.
fn model_root (
) -> Viewnode {
  viewforest_root_viewnode () }

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

/// Append 'viewnode' under 'treeid'.
fn append (
  tree     : &mut Tree<Viewnode>,
  treeid   : NodeId,
  viewnode : Viewnode,
) -> NodeId {
  tree . get_mut (treeid) . unwrap () . append (viewnode) . id () }
