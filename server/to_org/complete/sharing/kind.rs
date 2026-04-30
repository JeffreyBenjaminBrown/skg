//! 'SharingScaffoldKind': a discriminator for the three sharing-scaffold
//! completion paths.
//!
//! The three rerender-time completers — for SubscribeeCol,
//! HiddenInSubscribeeCol, and HiddenOutsideOfSubscribeeCol — share
//! enough structure (build child data, reconcile against a goal
//! list) that several pieces of per-kind metadata are worth
//! capturing as an enum: the caller-label string used in panic
//! messages, the corresponding 'Scaffold' variant.

use crate::types::viewnode::Scaffold;

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
        "complete_subscribee_col_preorder",
      Self::HiddenInSubscribeeCol =>
        "complete_hiddeninsubscribee_col",
      Self::HiddenOutsideOfSubscribeeCol =>
        "complete_hiddenoutsideofsubscribeecol", } }

  /// The 'Scaffold' variant a node of this kind carries.
  pub fn scaffold (self) -> Scaffold {
    match self {
      Self::SubscribeeCol =>
        Scaffold::SubscribeeCol,
      Self::HiddenInSubscribeeCol =>
        Scaffold::HiddenInSubscribeeCol,
      Self::HiddenOutsideOfSubscribeeCol =>
        Scaffold::HiddenOutsideOfSubscribeeCol, } }
}
