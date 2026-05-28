//! 'SharingScaffoldKind': a discriminator for the three sharing-scaffold
//! completion paths.
//!
//! The three rerender-time completers — for SubscribeeCol,
//! HiddenInSubscribeeCol, and HiddenOutsideOfSubscribeeCol — share
//! enough structure (build child data, reconcile against a goal
//! list) that several pieces of per-kind metadata are worth
//! capturing as an enum: the caller-label string used in panic
//! messages, the corresponding 'Scaffold' variant, the number of
//! ancestor links to the subscriber, and a self-type guard.

use crate::types::tree::generic::error_unless_node_satisfies;
use crate::types::viewnode::{Scaffold, ViewNode, ViewNodeKind};
use crate::dbs::in_rust_graph::relation_accessors::{
  NodeRelation,
  RelationRole,
};

use ego_tree::{NodeId, Tree};
use std::error::Error;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SharingScaffoldKind {
  SubscribeeCol,
  SubscriberCol,
  OverriddenCol,
  OverriderCol,
  HiderCol,
  HiddenCol,
  HiddenInSubscribeeCol,
  HiddenOutsideOfSubscribeeCol,
}

impl SharingScaffoldKind {
  /// Stable label used in panic messages from
  /// 'reconcile_sharing_scaffold_children' and similar helpers.
  /// Mirrors the function name of the corresponding completer so a
  /// crash gives the reader an immediately greppable hit.
  pub fn caller_label (self) -> &'static str {
    match self {
      Self::SubscribeeCol =>
        "reconcile_subscribee_col_children",
      Self::SubscriberCol
        | Self::OverriddenCol
        | Self::OverriderCol
        | Self::HiderCol
        | Self::HiddenCol =>
        "reconcile_relation_col_children",
      Self::HiddenInSubscribeeCol =>
        "reconcile_hiddenin_subscribee_col_children",
      Self::HiddenOutsideOfSubscribeeCol =>
        "reconcile_hiddenoutside_subscribee_col_children", } }

  /// The 'Scaffold' variant a node of this kind carries.
  pub fn scaffold (self) -> Scaffold {
    match self {
      Self::SubscribeeCol =>
        Scaffold::SubscribeeCol,
      Self::SubscriberCol =>
        Scaffold::SubscriberCol,
      Self::OverriddenCol =>
        Scaffold::OverriddenCol,
      Self::OverriderCol =>
        Scaffold::OverriderCol,
      Self::HiderCol =>
        Scaffold::HiderCol,
      Self::HiddenCol =>
        Scaffold::HiddenCol,
      Self::HiddenInSubscribeeCol =>
        Scaffold::HiddenInSubscribeeCol,
      Self::HiddenOutsideOfSubscribeeCol =>
        Scaffold::HiddenOutsideOfSubscribeeCol, } }

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
      Self::SubscribeeCol
        | Self::SubscriberCol
        | Self::OverriddenCol
        | Self::OverriderCol
        | Self::HiderCol
        | Self::HiddenCol                => 1,
      Self::HiddenInSubscribeeCol        => 3,
      Self::HiddenOutsideOfSubscribeeCol => 2, } }

  pub fn relation_member_role (self) -> Option<RelationRole> {
    match self {
      Self::SubscribeeCol =>
        Some (RelationRole::new (
          NodeRelation::Subscribes, "subscribee") . unwrap ()),
      Self::SubscriberCol =>
        Some (RelationRole::new (
          NodeRelation::Subscribes, "subscriber") . unwrap ()),
      Self::OverriddenCol =>
        Some (RelationRole::new (
          NodeRelation::OverridesViewOf, "overridden") . unwrap ()),
      Self::OverriderCol =>
        Some (RelationRole::new (
          NodeRelation::OverridesViewOf, "overrider") . unwrap ()),
      Self::HiderCol =>
        Some (RelationRole::new (
          NodeRelation::HidesFromItsSubscriptions, "hider") . unwrap ()),
      Self::HiddenCol =>
        Some (RelationRole::new (
          NodeRelation::HidesFromItsSubscriptions, "hidden") . unwrap ()),
      Self::HiddenInSubscribeeCol |
      Self::HiddenOutsideOfSubscribeeCol =>
        None,
    } }

  pub fn from_scaffold (scaffold : &Scaffold) -> Option<SharingScaffoldKind> {
    match scaffold {
      Scaffold::SubscribeeCol =>
        Some (Self::SubscribeeCol),
      Scaffold::SubscriberCol =>
        Some (Self::SubscriberCol),
      Scaffold::OverriddenCol =>
        Some (Self::OverriddenCol),
      Scaffold::OverriderCol =>
        Some (Self::OverriderCol),
      Scaffold::HiderCol =>
        Some (Self::HiderCol),
      Scaffold::HiddenCol =>
        Some (Self::HiddenCol),
      Scaffold::HiddenInSubscribeeCol =>
        Some (Self::HiddenInSubscribeeCol),
      Scaffold::HiddenOutsideOfSubscribeeCol =>
        Some (Self::HiddenOutsideOfSubscribeeCol),
      _ => None,
    } }

  /// Error if 'node' is not a 'Scaff' of this kind.
  /// Wraps 'error_unless_node_satisfies' with
  /// - a Box<dyn Error> result
  /// - a per-kind error string of the form
  ///   "<caller_label>: expected <Scaffold>".
  pub fn error_unless_node_is_this_kind (
    self,
    tree : &Tree<ViewNode>,
    node : NodeId,
  ) -> Result<(), Box<dyn Error>> {
    let scaffold : Scaffold = self . scaffold ();
    error_unless_node_satisfies (
      tree, node,
      |vn : &ViewNode| matches! (
        &vn . kind,
        ViewNodeKind::Scaff (s) if *s == scaffold ),
      &format! ( "{}: expected {:?}",
                 self . caller_label (),
                 scaffold ),
    ) . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
    Ok (( )) }
}
