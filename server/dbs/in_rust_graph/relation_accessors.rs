use std::collections::HashSet;

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::typedb::relationships::OUTBOUND_RELATIONSHIP_TYPES;
use crate::types::git::NodeChanges;
use crate::types::list::Diff_Item;
use crate::types::misc::ID;
use crate::types::nodes::rust::NodeRust;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum NodeRelation {
  Contains,
  TextlinksTo,
  Subscribes,
  HidesFromItsSubscriptions,
  OverridesViewOf,
}

impl NodeRelation {
  pub fn typeql_name (self) -> &'static str {
    match self {
      Self::Contains =>
        "contains",
      Self::TextlinksTo =>
        "textlinks_to",
      Self::Subscribes =>
        "subscribes",
      Self::HidesFromItsSubscriptions =>
        "hides_from_its_subscriptions",
      Self::OverridesViewOf =>
        "overrides_view_of",
    } }

  pub fn from_typeql_name (name : &str) -> Option<NodeRelation> {
    match name {
      "contains" =>
        Some (Self::Contains),
      "textlinks_to" =>
        Some (Self::TextlinksTo),
      "subscribes" =>
        Some (Self::Subscribes),
      "hides_from_its_subscriptions" =>
        Some (Self::HidesFromItsSubscriptions),
      "overrides_view_of" =>
        Some (Self::OverridesViewOf),
      _ => None,
    } }

  /// The per-stage diff of this relation's outbound list within a
  /// NodeChanges, or None for a relation NodeChanges does not diff
  /// (textlinks are inferred from node text, not stored as a list).
  /// Membership-sign consumers (e.g. 'phantom_axes') call this with
  /// the one relation their col represents, so a sign can never be
  /// read from a different relation that involves the same ID.
  pub fn diff_in_nodechanges<'a> (
    self,
    nc : &'a NodeChanges,
  ) -> Option<&'a [Diff_Item<ID>]> {
    match self {
      Self::Contains =>
        Some ( & nc . contains_diff ),
      Self::Subscribes =>
        Some ( & nc . subscribes_to_diff ),
      Self::HidesFromItsSubscriptions =>
        Some ( & nc . hides_diff ),
      Self::OverridesViewOf =>
        Some ( & nc . overrides_view_of_diff ),
      Self::TextlinksTo =>
        None, } }

  pub fn roles (self) -> (&'static str, &'static str) {
    let relation_name : &'static str = self . typeql_name ();
    OUTBOUND_RELATIONSHIP_TYPES . iter ()
      . find ( |(candidate, _, _)| *candidate == relation_name )
      . map ( |(_, first, second)| (*first, *second) )
      . expect ("OUTBOUND_RELATIONSHIP_TYPES should cover NodeRelation")
  }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RelationRole {
  pub relation : NodeRelation,
  pub position : BinaryRolePosition,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BinaryRolePosition {
  First,
  Second,
}

impl RelationRole {
  pub fn new (
    relation : NodeRelation,
    position : BinaryRolePosition,
  ) -> RelationRole {
    RelationRole { relation, position } }

  pub fn from_typeql_role_name (
    relation : NodeRelation,
    role     : &'static str,
  ) -> Result<RelationRole, String> {
    let (first_role, second_role) : (&'static str, &'static str) =
      relation . roles ();
    if role == first_role {
      Ok (RelationRole::new (relation, BinaryRolePosition::First))
    } else if role == second_role {
      Ok (RelationRole::new (relation, BinaryRolePosition::Second))
    } else {
      Err (format!(
        "role '{}' is not part of relation '{}'",
        role, relation . typeql_name ())) } }

  pub fn typeql_role_name (self) -> &'static str {
    let (first_role, second_role) : (&'static str, &'static str) =
      self . relation . roles ();
    match self . position {
      BinaryRolePosition::First  => first_role,
      BinaryRolePosition::Second => second_role,
    } }

  pub fn opposite_position (self) -> BinaryRolePosition {
    match self . position {
      BinaryRolePosition::First  => BinaryRolePosition::Second,
      BinaryRolePosition::Second => BinaryRolePosition::First,
    } }

  pub fn opposite_role (self) -> RelationRole {
    RelationRole::new (self . relation, self . opposite_position ()) }

  pub fn is_first_role (self) -> bool {
    self . position == BinaryRolePosition::First }
}

impl InRustGraph {
  pub fn outbound_ids_for_relation (
    &self,
    pid      : &ID,
    relation : NodeRelation,
  ) -> Vec<ID> {
    self . nodes . get (pid)
      . map ( |node| outbound_ids_from_node (node, relation) )
      . unwrap_or_default () }

  pub fn outbound_pids_for_relation (
    &self,
    pid      : &ID,
    relation : NodeRelation,
  ) -> Vec<ID> {
    self . outbound_ids_for_relation (pid, relation)
      . iter ()
      . filter_map ( |id| self . pid_of (id) )
      . collect () }

  pub fn inbound_pids_for_relation (
    &self,
    pid      : &ID,
    relation : NodeRelation,
  ) -> Vec<ID> {
    // The inbound members are stored as a set, so their iteration order is
    // nondeterministic (run-to-run). Sort by ID so every consumer gets a stable
    // order: a node's inbound PartnerCol (e.g. thousands of subscribers) then
    // renders the same way every time, rather than in an arbitrary shuffle.
    // Inbound relation order is user-irrelevant, unlike the outbound relations,
    // whose meaningful Vec order (e.g. a node's hides list) is left untouched.
    let mut pids : Vec<ID> =
      inbound_pid_set (self, pid, relation)
      . into_iter ()
      . collect ();
    pids . sort_by ( |a, b| a . 0 . cmp (&b . 0) );
    pids }

  pub fn other_member_pids (
    &self,
    pid  : &ID,
    role : RelationRole,
  ) -> Vec<ID> {
    if role . is_first_role () {
      self . outbound_pids_for_relation (pid, role . relation)
    } else {
      self . inbound_pids_for_relation (pid, role . relation) } }

  pub fn relation_membership_is_real (
    &self,
    owner_pid  : &ID,
    member_pid : &ID,
    member_role : RelationRole,
  ) -> bool {
    let members : Vec<ID> =
      self . other_member_pids (owner_pid, member_role . opposite_role ());
    members . contains (member_pid) }
}

fn outbound_ids_from_node (
  node     : &NodeRust,
  relation : NodeRelation,
) -> Vec<ID> {
  match relation {
    NodeRelation::Contains =>
      node . contains . clone (),
    NodeRelation::TextlinksTo =>
      node . textlinks_to . clone (),
    NodeRelation::Subscribes =>
      node . subscribes_to . or_default () . to_vec (),
    NodeRelation::HidesFromItsSubscriptions =>
      node . hides_from_its_subscriptions . or_default () . to_vec (),
    NodeRelation::OverridesViewOf =>
      node . overrides_view_of . or_default () . to_vec (),
  } }

fn inbound_pid_set (
  graph    : &InRustGraph,
  pid      : &ID,
  relation : NodeRelation,
) -> HashSet<ID> {
  let ids : Vec<ID> = match relation {
    NodeRelation::Contains =>
      graph . contained_by . get (pid)
      . map ( |s| s . iter () . cloned () . collect () )
      . unwrap_or_default (),
    NodeRelation::TextlinksTo =>
      graph . textlinks_in . get (pid)
      . map ( |s| s . iter () . cloned () . collect () )
      . unwrap_or_default (),
    NodeRelation::Subscribes =>
      graph . subscribers_of . get (pid)
      . map ( |s| s . iter () . cloned () . collect () )
      . unwrap_or_default (),
    NodeRelation::HidesFromItsSubscriptions =>
      graph . hiders_of . get (pid)
      . map ( |s| s . iter () . cloned () . collect () )
      . unwrap_or_default (),
    NodeRelation::OverridesViewOf =>
      graph . overriders_of . get (pid)
      . map ( |s| s . iter () . cloned () . collect () )
      . unwrap_or_default (),
  };
  ids . into_iter () . collect () }
