//! Folder metadata used by the folder completion paths.
//!
//! The rerender-time completers share enough structure (build child
//! data, reconcile against a goal list) that several pieces of
//! per-kind metadata are worth capturing as methods: the
//! caller-label string used in error messages, the corresponding
//! relation role, and a self-type guard.
//!
//! These methods live here rather than beside the type because they
//! depend on 'RelationRole' (the dbs layer), which
//! 'server/types/viewnode.rs' should not import. The pure per-kind
//! facts, such as 'Folder::goal_is_ordered', live with the type.

use crate::types::tree::generic::error_unless_node_satisfies;
use crate::types::viewnode::{Folder, Viewnode, ViewnodeKind};
use crate::dbs::in_rust_graph::relation_accessors::{
  BinaryRolePosition,
  NodeRelation,
  RelationRole,
};

use ego_tree::{NodeId, Tree};
use std::error::Error;

impl Folder {
  /// Stable label used in panic messages from
  /// 'reconcile_partnerFolder_children_against_goal_list' and similar helpers.
  /// Mirrors the function name of the corresponding completer so a
  /// crash gives the reader an immediately greppable hit.
  pub fn caller_label (&self) -> &'static str {
    match self {
      Folder::Subscribee
        | Folder::Subscriber
        | Folder::Overridden
        | Folder::Overrider
        | Folder::Hider
        | Folder::Hidden
        | Folder::Container
        | Folder::Mentioner =>
        "reconcile_partnerFolder_children",
      Folder::HiddenInSubscribee =>
        "reconcile_hiddenInSubscribeeFolder_children",
      Folder::HiddenOutsideOfSubscribee =>
        "reconcile_hiddenoutsideSubscribeeFolder_children",
      // Derived from the graph straddle, and drawn by the diff
      // view's reconciler rather than by view completion.
      Folder::AcquiredBy
        | Folder::Acquired =>
        "acquisition_folder",
      Folder::Alias => "reconcile_aliasFolder_children",
      Folder::ID    => "reconcile_idFolder_children",
      Folder::Flags { .. } => "reconcile_flags_folder_children", } }

  pub fn relation_member_role (&self) -> Option<RelationRole> {
    match self {
      Folder::Subscribee =>
        Some (RelationRole::new (
          NodeRelation::SubscribesTo, BinaryRolePosition::Second)),
      Folder::Subscriber =>
        Some (RelationRole::new (
          NodeRelation::SubscribesTo, BinaryRolePosition::First)),
      Folder::Overridden =>
        Some (RelationRole::new (
          NodeRelation::Overrides, BinaryRolePosition::Second)),
      Folder::Overrider =>
        Some (RelationRole::new (
          NodeRelation::Overrides, BinaryRolePosition::First)),
      Folder::Hider =>
        Some (RelationRole::new (
          NodeRelation::HidesFromSubs, BinaryRolePosition::First)),
      Folder::Hidden =>
        Some (RelationRole::new (
          NodeRelation::HidesFromSubs, BinaryRolePosition::Second)),
      Folder::Container =>
        Some (RelationRole::new (
          NodeRelation::Contains, BinaryRolePosition::First)),
      Folder::Mentioner =>
        Some (RelationRole::new (
          NodeRelation::LinksTo, BinaryRolePosition::First)),
      Folder::HiddenInSubscribee |
      Folder::HiddenOutsideOfSubscribee =>
        None,
      // A nodeMerge is no relation: these come from extra IDs.
      Folder::AcquiredBy |
      Folder::Acquired =>
        None,
      // A property is no relation.
      Folder::Alias | Folder::ID | Folder::Flags { .. } =>
        None,
    } }

  /// Error if 'node' is not a folder of this kind.
  /// Wraps 'error_unless_node_satisfies' with
  /// - a Box<dyn Error> result
  /// - a per-kind error string of the form
  ///   "<caller_label>: expected <folder-kind>".
  pub fn error_unless_node_is_this_kind (
    &self,
    tree : &Tree<Viewnode>,
    node : NodeId,
  ) -> Result<(), Box<dyn Error>> {
    error_unless_node_satisfies (
      tree, node,
      |vn : &Viewnode| matches! (
        &vn . kind,
        ViewnodeKind::Folder (folder) if folder == self ),
      &format! ( "{}: expected {:?}",
                 self . caller_label (),
                 self ),
    ) . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
    Ok (( )) }
}
