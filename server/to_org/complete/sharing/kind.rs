//! RoleCol metadata used by sharing-scaffold completion paths.
//!
//! The three rerender-time completers — for SubscribeeCol,
//! HiddenInSubscribeeCol, and HiddenOutsideOfSubscribeeCol — share
//! enough structure (build child data, reconcile against a goal
//! list) that several pieces of per-kind metadata are worth
//! capturing as methods: the caller-label string used in panic
//! messages, the corresponding PartnerCol variant, the number of
//! ancestor links to the subscriber, and a self-type guard.

use crate::types::tree::generic::error_unless_node_satisfies;
use crate::types::viewnode::{RoleCol, ViewNode, ViewNodeKind};
use crate::dbs::in_rust_graph::relation_accessors::{
  BinaryRolePosition,
  NodeRelation,
  RelationRole,
};

use ego_tree::{NodeId, Tree};
use std::error::Error;

impl RoleCol {
  /// Stable label used in panic messages from
  /// 'reconcile_sharing_scaffold_children' and similar helpers.
  /// Mirrors the function name of the corresponding completer so a
  /// crash gives the reader an immediately greppable hit.
  pub fn caller_label (self) -> &'static str {
    match self {
      RoleCol::Subscribee =>
        "reconcile_subscribee_col_children",
      RoleCol::Subscriber
        | RoleCol::Overridden
        | RoleCol::Overrider
        | RoleCol::Hider
        | RoleCol::Hidden =>
        "reconcile_relation_col_children",
      RoleCol::HiddenInSubscribee =>
        "reconcile_hiddenin_subscribee_col_children",
      RoleCol::HiddenOutsideOfSubscribee =>
        "reconcile_hiddenoutside_subscribee_col_children", } }

  /// How many ancestor links *should* separate this scaffold
  /// from the subscriber TrueNode.
  ///
  /// Tree shape:
  ///   subscriber (TrueNode)
  ///     └─ SubscribeeCol                       ← distance 1
  ///          ├─ Subscribee (TrueNode)          ← distance 2
  ///          │    └─ HiddenInSubscribeeCol     ← distance 3
  ///          └─ HiddenOutsideOfSubscribeeCol   ← distance 2
  pub fn correct_subscriber_ancestor_distance (self) -> usize {
    match self {
      RoleCol::Subscribee
        | RoleCol::Subscriber
        | RoleCol::Overridden
        | RoleCol::Overrider
        | RoleCol::Hider
        | RoleCol::Hidden => 1,
      RoleCol::HiddenInSubscribee => 3,
      RoleCol::HiddenOutsideOfSubscribee => 2, } }

  pub fn relation_member_role (self) -> Option<RelationRole> {
    match self {
      RoleCol::Subscribee =>
        Some (RelationRole::new (
          NodeRelation::Subscribes, BinaryRolePosition::Second)),
      RoleCol::Subscriber =>
        Some (RelationRole::new (
          NodeRelation::Subscribes, BinaryRolePosition::First)),
      RoleCol::Overridden =>
        Some (RelationRole::new (
          NodeRelation::OverridesViewOf, BinaryRolePosition::Second)),
      RoleCol::Overrider =>
        Some (RelationRole::new (
          NodeRelation::OverridesViewOf, BinaryRolePosition::First)),
      RoleCol::Hider =>
        Some (RelationRole::new (
          NodeRelation::HidesFromItsSubscriptions, BinaryRolePosition::First)),
      RoleCol::Hidden =>
        Some (RelationRole::new (
          NodeRelation::HidesFromItsSubscriptions, BinaryRolePosition::Second)),
      RoleCol::HiddenInSubscribee |
      RoleCol::HiddenOutsideOfSubscribee =>
        None,
    } }

  /// Error if 'node' is not a PartnerCol of this kind.
  /// Wraps 'error_unless_node_satisfies' with
  /// - a Box<dyn Error> result
  /// - a per-kind error string of the form
  ///   "<caller_label>: expected <Scaffold>".
  pub fn error_unless_node_is_this_kind (
    self,
    tree : &Tree<ViewNode>,
    node : NodeId,
  ) -> Result<(), Box<dyn Error>> {
    error_unless_node_satisfies (
      tree, node,
      |vn : &ViewNode| matches! (
        &vn . kind,
        ViewNodeKind::PartnerCol (roleCol) if *roleCol == self ),
      &format! ( "{}: expected {:?}",
                 self . caller_label (),
                 self ),
    ) . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
    Ok (( )) }
}
