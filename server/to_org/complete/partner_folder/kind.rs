//! The relation role a folder's members play ('Folder::relation_member_role').
//!
//! It lives here rather than beside the type because it depends on
//! 'RelationRole' (the dbs layer), which 'server/types/viewnode.rs'
//! should not import. The pure per-kind rules, such as
//! 'Folder::goal_is_ordered', live with the type.

use crate::types::viewnode::Folder;
use crate::dbs::in_rust_graph::relation_accessors::{
  BinaryRolePosition,
  NodeRelation,
  RelationRole,
};

impl Folder {
  /// The role its members play in the relation the folder shows, of
  /// which the node it describes plays the other; None for a folder that shows no
  /// one relation.
  pub fn relation_member_role (
    &self,
  ) -> Option<RelationRole> {
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
}
