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

use ego_tree::{NodeId, Tree};
use std::error::Error;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SharingScaffoldKind {
  SubscribeeCol,
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
      Self::HiddenInSubscribeeCol =>
        "reconcile_hiddenin_subscribee_col_children",
      Self::HiddenOutsideOfSubscribeeCol =>
        "reconcile_hiddenoutside_subscribee_col_children", } }

  /// The 'Scaffold' variant a node of this kind carries.
  pub fn scaffold (self) -> Scaffold {
    match self {
      Self::SubscribeeCol =>
        Scaffold::SubscribeeCol,
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
      Self::SubscribeeCol                => 1,
      Self::HiddenInSubscribeeCol        => 3,
      Self::HiddenOutsideOfSubscribeeCol => 2, } }

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
