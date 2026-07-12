use std::collections::HashSet;

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::typedb::relationships::OUTBOUND_RELATIONSHIP_TYPES;
use crate::types::git::NodeChanges;
use crate::types::list::Diff_Item;
use crate::types::misc::{ID, members_of};
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

//
// The partner-role vocabulary: ROLENAME <-> RelationRole <-> backpath
// triple <-> glyph. The single source of truth shared by the request
// layer ('(path ROLENAME)'), the backpath engine, and the birth herald.
//

impl RelationRole {
  // The nine partner roles a backpath/path can graft, named for the
  // role the grafted partner plays toward the origin (= toward its
  // org-parent). 'Contains, Second' ("content") is intentionally
  // absent -- the recursive content view already serves it.
  pub const CONTAINER : RelationRole =
    RelationRole { relation : NodeRelation::Contains,
                   position : BinaryRolePosition::First };
  pub const LINK_SOURCE : RelationRole =
    RelationRole { relation : NodeRelation::TextlinksTo,
                   position : BinaryRolePosition::First };
  pub const LINK_DEST : RelationRole =
    RelationRole { relation : NodeRelation::TextlinksTo,
                   position : BinaryRolePosition::Second };
  pub const OVERRIDER : RelationRole =
    RelationRole { relation : NodeRelation::OverridesViewOf,
                   position : BinaryRolePosition::First };
  pub const OVERRIDDEN : RelationRole =
    RelationRole { relation : NodeRelation::OverridesViewOf,
                   position : BinaryRolePosition::Second };
  pub const HIDER : RelationRole =
    RelationRole { relation : NodeRelation::HidesFromItsSubscriptions,
                   position : BinaryRolePosition::First };
  pub const HIDDEN : RelationRole =
    RelationRole { relation : NodeRelation::HidesFromItsSubscriptions,
                   position : BinaryRolePosition::Second };
  pub const SUBSCRIBER : RelationRole =
    RelationRole { relation : NodeRelation::Subscribes,
                   position : BinaryRolePosition::First };
  pub const SUBSCRIBEE : RelationRole =
    RelationRole { relation : NodeRelation::Subscribes,
                   position : BinaryRolePosition::Second };

  /// The single ROLENAME token naming this partner role in the wire
  /// grammar ('(path ROLENAME)', '(birth backpath ROLENAME)').
  /// Panics for a role absent from PARTNER_ROLE_VOCAB (only
  /// 'Contains, Second' -- "content", never a path/birth role).
  pub fn rolename (
    self,
  ) -> &'static str {
    PARTNER_ROLE_VOCAB . iter ()
      . find ( |(_, role, _)| *role == self )
      . map ( |(name, _, _)| *name )
      . expect ( "RelationRole::rolename: role absent from PARTNER_ROLE_VOCAB" ) }

  pub fn from_rolename (
    s : &str,
  ) -> Option<RelationRole> {
    PARTNER_ROLE_VOCAB . iter ()
      . find ( |(name, _, _)| *name == s )
      . map ( |(_, role, _)| *role ) }

  /// The birth/path glyph for this partner role (orange when emitted
  /// as a birth). Panics for a role absent from PARTNER_ROLE_VOCAB.
  pub fn glyph (
    self,
  ) -> &'static str {
    PARTNER_ROLE_VOCAB . iter ()
      . find ( |(_, role, _)| *role == self )
      . map ( |(_, _, glyph)| *glyph )
      . expect ( "RelationRole::glyph: role absent from PARTNER_ROLE_VOCAB" ) }

  /// The '(relation, input_role, output_role)' triple the backpath
  /// engine consumes: output_role is THIS (partner) role, input_role
  /// is the origin's (opposite) role.
  pub fn backpath_triple (
    self,
  ) -> (&'static str, &'static str, &'static str) {
    let (first, second) : (&'static str, &'static str) =
      self . relation . roles ();
    let relation : &'static str = self . relation . typeql_name ();
    match self . position {
      BinaryRolePosition::First  => (relation, second, first),
      BinaryRolePosition::Second => (relation, first, second), } }
}

/// Each row is (ROLENAME, RelationRole, glyph). The backpath triple is
/// DERIVED from the RelationRole ('RelationRole::backpath_triple'), so
/// it is not stored here. A col is named by its RELATION (spanning both
/// roles); a path and a birth are named by the one ROLE the grafted
/// partner plays. 'Contains, Second' is absent (see the consts above).
pub const PARTNER_ROLE_VOCAB
  : &[ (&'static str, RelationRole, &'static str) ] = &[
  ("container",  RelationRole::CONTAINER,   "}"),
  ("linkSource", RelationRole::LINK_SOURCE, "←"),
  ("linkDest",   RelationRole::LINK_DEST,   "→"),
  ("overrider",  RelationRole::OVERRIDER,   "Op"),
  ("overridden", RelationRole::OVERRIDDEN,  "pO"),
  ("hider",      RelationRole::HIDER,       "Hp"),
  ("hidden",     RelationRole::HIDDEN,      "pH"),
  ("subscriber", RelationRole::SUBSCRIBER,  "Sp"),
  ("subscribee", RelationRole::SUBSCRIBEE,  "pS"),
];

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
      members_of ( &node . contains ),
    NodeRelation::TextlinksTo =>
      node . textlinks_to . clone (),
    NodeRelation::Subscribes =>
      members_of ( node . subscribes_to . or_default () ),
    NodeRelation::HidesFromItsSubscriptions =>
      members_of ( node . hides_from_its_subscriptions . or_default () ),
    NodeRelation::OverridesViewOf =>
      members_of ( node . overrides_view_of . or_default () ),
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

#[cfg(test)]
#[allow(non_snake_case)]
mod tests {
  use super::*;

  /// Every PARTNER_ROLE_VOCAB row round-trips ROLENAME <-> RelationRole,
  /// resolves a glyph, and yields the expected backpath triple.
  #[test]
  fn partner_role_vocab_round_trips_and_derives_triples () {
    // (rolename, RelationRole, glyph, (relation, input_role, output_role))
    let expected : [(&str, RelationRole, &str,
                     (&str, &str, &str)); 9] = [
      ("container",  RelationRole::CONTAINER,   "}",
       ("contains", "contained", "container")),
      ("linkSource", RelationRole::LINK_SOURCE, "←",
       ("textlinks_to", "dest", "source")),
      ("linkDest",   RelationRole::LINK_DEST,   "→",
       ("textlinks_to", "source", "dest")),
      ("overrider",  RelationRole::OVERRIDER,   "Op",
       ("overrides_view_of", "overridden", "overrider")),
      ("overridden", RelationRole::OVERRIDDEN,  "pO",
       ("overrides_view_of", "overrider", "overridden")),
      ("hider",      RelationRole::HIDER,       "Hp",
       ("hides_from_its_subscriptions", "hidden", "hider")),
      ("hidden",     RelationRole::HIDDEN,      "pH",
       ("hides_from_its_subscriptions", "hider", "hidden")),
      ("subscriber", RelationRole::SUBSCRIBER,  "Sp",
       ("subscribes", "subscribee", "subscriber")),
      ("subscribee", RelationRole::SUBSCRIBEE,  "pS",
       ("subscribes", "subscriber", "subscribee")),
    ];
    for (name, role, glyph, triple) in expected {
      assert_eq! ( role . rolename (), name );
      assert_eq! ( RelationRole::from_rolename (name), Some (role) );
      assert_eq! ( role . glyph (), glyph );
      assert_eq! ( role . backpath_triple (), triple,
        "wrong backpath triple for {}", name ); } }

  #[test]
  fn from_rolename_rejects_unknown_and_the_absent_content_role () {
    assert_eq! ( RelationRole::from_rolename (""), None );
    assert_eq! ( RelationRole::from_rolename ("contained"), None );
    assert_eq! ( RelationRole::from_rolename ("bogus"), None ); }
}
