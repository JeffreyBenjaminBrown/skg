//! PartnerCol metadata used by PartnerCol completion paths.
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
//! fact, 'PartnerCol::policy', lives with the type.

use crate::types::tree::generic::error_unless_node_satisfies;
use crate::types::viewnode::{PartnerCol, ViewNode, ViewNodeKind};
use crate::dbs::in_rust_graph::relation_accessors::{
  BinaryRolePosition,
  NodeRelation,
  RelationRole,
};

use ego_tree::{NodeId, Tree};
use std::error::Error;

impl PartnerCol {
  /// Stable label used in panic messages from
  /// 'reconcile_partnerCol_children_against_goal_list' and similar helpers.
  /// Mirrors the function name of the corresponding completer so a
  /// crash gives the reader an immediately greppable hit.
  pub fn caller_label (self) -> &'static str {
    match self {
      PartnerCol::Subscribee =>
        "reconcile_subscribee_col_children",
      PartnerCol::Subscriber
        | PartnerCol::Overridden
        | PartnerCol::Overrider
        | PartnerCol::Hider
        | PartnerCol::Hidden =>
        "reconcile_partnerCol_children",
      PartnerCol::HiddenInSubscribee =>
        "reconcile_hiddenin_subscribee_col_children",
      PartnerCol::HiddenOutsideOfSubscribee =>
        "reconcile_hiddenoutside_subscribee_col_children", } }

  pub fn relation_member_role (self) -> Option<RelationRole> {
    match self {
      PartnerCol::Subscribee =>
        Some (RelationRole::new (
          NodeRelation::Subscribes, BinaryRolePosition::Second)),
      PartnerCol::Subscriber =>
        Some (RelationRole::new (
          NodeRelation::Subscribes, BinaryRolePosition::First)),
      PartnerCol::Overridden =>
        Some (RelationRole::new (
          NodeRelation::OverridesViewOf, BinaryRolePosition::Second)),
      PartnerCol::Overrider =>
        Some (RelationRole::new (
          NodeRelation::OverridesViewOf, BinaryRolePosition::First)),
      PartnerCol::Hider =>
        Some (RelationRole::new (
          NodeRelation::HidesFromItsSubscriptions, BinaryRolePosition::First)),
      PartnerCol::Hidden =>
        Some (RelationRole::new (
          NodeRelation::HidesFromItsSubscriptions, BinaryRolePosition::Second)),
      PartnerCol::HiddenInSubscribee |
      PartnerCol::HiddenOutsideOfSubscribee =>
        None,
    } }

  /// Error if 'node' is not a PartnerCol of this kind.
  /// Wraps 'error_unless_node_satisfies' with
  /// - a Box<dyn Error> result
  /// - a per-kind error string of the form
  ///   "<caller_label>: expected <col-kind>".
  pub fn error_unless_node_is_this_kind (
    self,
    tree : &Tree<ViewNode>,
    node : NodeId,
  ) -> Result<(), Box<dyn Error>> {
    error_unless_node_satisfies (
      tree, node,
      |vn : &ViewNode| matches! (
        &vn . kind,
        ViewNodeKind::PartnerCol (partnerCol) if *partnerCol == self ),
      &format! ( "{}: expected {:?}",
                 self . caller_label (),
                 self ),
    ) . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
    Ok (( )) }
}
