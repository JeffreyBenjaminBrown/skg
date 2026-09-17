//! PartnerFolder metadata used by PartnerFolder completion paths.
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
//! fact, 'PartnerFolder::policy', lives with the type.

use crate::types::tree::generic::error_unless_node_satisfies;
use crate::types::viewnode::{PartnerFolder, ViewNode, ViewNodeKind};
use crate::dbs::in_rust_graph::relation_accessors::{
  BinaryRolePosition,
  NodeRelation,
  RelationRole,
};

use ego_tree::{NodeId, Tree};
use std::error::Error;

impl PartnerFolder {
  /// Stable label used in panic messages from
  /// 'reconcile_partnerFolder_children_against_goal_list' and similar helpers.
  /// Mirrors the function name of the corresponding completer so a
  /// crash gives the reader an immediately greppable hit.
  pub fn caller_label (self) -> &'static str {
    match self {
      PartnerFolder::Subscribee =>
        "reconcile_subscribeeFolder_children",
      PartnerFolder::Subscriber
        | PartnerFolder::Overridden
        | PartnerFolder::Overrider
        | PartnerFolder::Hider
        | PartnerFolder::Hidden =>
        "reconcile_partnerFolder_children",
      PartnerFolder::HiddenInSubscribee =>
        "reconcile_hiddenInSubscribeeFolder_children",
      PartnerFolder::HiddenOutsideOfSubscribee =>
        "reconcile_hiddenoutsideSubscribeeFolder_children", } }

  pub fn relation_member_role (self) -> Option<RelationRole> {
    match self {
      PartnerFolder::Subscribee =>
        Some (RelationRole::new (
          NodeRelation::Subscribes, BinaryRolePosition::Second)),
      PartnerFolder::Subscriber =>
        Some (RelationRole::new (
          NodeRelation::Subscribes, BinaryRolePosition::First)),
      PartnerFolder::Overridden =>
        Some (RelationRole::new (
          NodeRelation::OverridesViewOf, BinaryRolePosition::Second)),
      PartnerFolder::Overrider =>
        Some (RelationRole::new (
          NodeRelation::OverridesViewOf, BinaryRolePosition::First)),
      PartnerFolder::Hider =>
        Some (RelationRole::new (
          NodeRelation::HidesFromItsSubscriptions, BinaryRolePosition::First)),
      PartnerFolder::Hidden =>
        Some (RelationRole::new (
          NodeRelation::HidesFromItsSubscriptions, BinaryRolePosition::Second)),
      PartnerFolder::HiddenInSubscribee |
      PartnerFolder::HiddenOutsideOfSubscribee =>
        None,
    } }

  /// Error if 'node' is not a PartnerFolder of this kind.
  /// Wraps 'error_unless_node_satisfies' with
  /// - a Box<dyn Error> result
  /// - a per-kind error string of the form
  ///   "<caller_label>: expected <folder-kind>".
  pub fn error_unless_node_is_this_kind (
    self,
    tree : &Tree<ViewNode>,
    node : NodeId,
  ) -> Result<(), Box<dyn Error>> {
    error_unless_node_satisfies (
      tree, node,
      |vn : &ViewNode| matches! (
        &vn . kind,
        ViewNodeKind::PartnerFolder (partnerFolder) if *partnerFolder == self ),
      &format! ( "{}: expected {:?}",
                 self . caller_label (),
                 self ),
    ) . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
    Ok (( )) }
}
