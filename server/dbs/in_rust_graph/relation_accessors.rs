use std::collections::HashSet;

use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::git::NodeChanges;
use crate::types::list::Diff_Item;
use crate::types::misc::{ID, MemberAtSource, RelationshipMemberKey, SourceName, members_of};
use crate::types::nodes::rust::NodeRust;

/// The five stored outbound relationship types and their endpoint roles.
/// This is domain vocabulary; storage adapters consume it rather than own it.
pub const OUTBOUND_RELATIONSHIP_TYPES : &[(&str, &str, &str)] = &[
  ("contains",                      "container",  "contained"),
  ("textlinks_to",                  "source",     "dest"),
  ("subscribes",                    "subscriber", "subscribee"),
  ("hides_from_its_subscriptions",  "hider",      "hidden"),
  ("overrides_view_of",             "overrider",  "overridden"),
];

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum NodeRelation {
  Contains,
  TextlinksTo,
  Subscribes,
  HidesFromItsSubscriptions,
  OverridesViewOf,
}

impl NodeRelation {
  pub fn relation_name (self) -> &'static str {
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
    let relation_name : &'static str = self . relation_name ();
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
    let relation : &'static str = self . relation . relation_name ();
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
  /// Comparison identity for a relationship member without mutating its raw
  /// stored spelling.  See `RelationshipMemberKey` for the two cases.
  pub fn relationship_member_key (
    &self,
    raw_member : &ID,
  ) -> RelationshipMemberKey {
    match self . pid_of (raw_member) {
      Some (pid) => RelationshipMemberKey::ResolvedPid (pid),
      None => RelationshipMemberKey::UnresolvedRawId (raw_member . clone ()), } }

  /// Stored outbound members for one relationship. Unlike the PID-oriented
  /// accessors, this preserves an unresolved raw ID and its recording source.
  /// Callers that require a current graph node should keep using the existing
  /// canonical-PID accessors instead.
  pub fn outbound_members_at_sources_for_relation_gated (
    &self,
    pid      : &ID,
    relation : NodeRelation,
    active   : Option<&crate::source_sets::ActiveSourceSet>,
  ) -> Vec<MemberAtSource<ID>> {
    let Some (node) = self . nodes . get (pid) else {
      return Vec::new (); };
    let members : Vec<MemberAtSource<ID>> = match relation {
      NodeRelation::Contains =>
        node . contains . clone (),
      NodeRelation::Subscribes =>
        node . subscribes_to . or_default () . to_vec (),
      NodeRelation::HidesFromItsSubscriptions =>
        node . hides_from_its_subscriptions . or_default () . to_vec (),
      NodeRelation::OverridesViewOf =>
        node . overrides_view_of . or_default () . to_vec (),
      NodeRelation::TextlinksTo =>
        return Vec::new (),
    };
    members . into_iter ()
      . filter ( |member| match active {
        None => true,
        Some (set) => set . is_all ()
          || set . contains_source (&member . source), } )
      . collect () }

  /// The SOURCE of the edge from OWNER to TARGET under RELATION, read
  /// from the owner's outbound list (where every edge's source
  /// lives). None when no such edge exists. This is how INBOUND
  /// surfaces gate: an inbound partner P of X is visible at the
  /// active set iff edge_source(P, R, X) is active -- private
  /// memberships must not surface through ancestry, backpaths, or
  /// inbound cols when the content direction hides them
  /// (render-and-gating, 5_plan.org).
  pub fn edge_source (
    &self,
    owner    : &ID,
    relation : NodeRelation,
    target   : &ID,
  ) -> Option<SourceName> {
    let target_key : ID = self . pid_of (target) ? ;
    let node : &NodeRust = self . nodes . get (owner) ? ;
    let members_at_sources : Vec<MemberAtSource<ID>> = match relation {
      NodeRelation::Contains =>
        node . contains . clone (),
      NodeRelation::Subscribes =>
        node . subscribes_to . or_default () . to_vec (),
      NodeRelation::HidesFromItsSubscriptions =>
        node . hides_from_its_subscriptions . or_default () . to_vec (),
      NodeRelation::OverridesViewOf =>
        node . overrides_view_of . or_default () . to_vec (),
      NodeRelation::TextlinksTo =>
        // textlinks derive from the body, which is home-only, so
        // their source is the owner's home by construction.
        return self . nodes . get (owner)
          . map ( |n| n . source . clone () ), };
    members_at_sources . iter ()
      . find ( |m| self . pid_of ( &m . member )
               . as_ref () == Some (&target_key) )
      . map ( |m| m . source . clone () ) }

  /// The recording source of one exact, stored outbound member ID.
  /// Unlike 'edge_source', this deliberately does not canonicalize the
  /// target: an unresolved raw ID has no PID, but is still a real stored
  /// relationship member and can be edited from an Unknown placeholder.
  pub fn edge_source_for_stored_member (
    &self,
    owner    : &ID,
    relation : NodeRelation,
    raw_member : &ID,
  ) -> Option<SourceName> {
    self . outbound_members_at_sources_for_relation_gated (
      owner, relation, None ) . into_iter ()
      . find ( |member| &member . member == raw_member )
      . map ( |member| member . source ) }

  /// Outbound members whose EDGE source is in the active set: the
  /// visible fold of one relation. Pass None for the full fold.
  pub fn outbound_pids_for_relation_gated (
    &self,
    pid      : &ID,
    relation : NodeRelation,
    active   : Option<&crate::source_sets::ActiveSourceSet>,
  ) -> Vec<ID> {
    if relation == NodeRelation::TextlinksTo {
      return self . outbound_pids_for_relation (pid, relation); }
    self . outbound_members_at_sources_for_relation_gated (
      pid, relation, active ) . iter ()
      . filter_map ( |member| self . pid_of (&member . member) )
      . collect () }

  /// Inbound partners whose EDGES to this node are visible at the
  /// active set (see 'edge_source'). Pass None for all of them.
  pub fn inbound_pids_for_relation_gated (
    &self,
    pid      : &ID,
    relation : NodeRelation,
    active   : Option<&crate::source_sets::ActiveSourceSet>,
  ) -> Vec<ID> {
    self . inbound_pids_for_relation (pid, relation)
      . into_iter ()
      . filter ( |partner| match active {
        None => true,
        Some (a) => a . is_all ()
          || self . edge_source (partner, relation, pid)
             . map ( |source| a . contains_source (&source) )
             . unwrap_or (false) } )
      . collect () }

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
    self . other_member_pids_gated (pid, role, None) }

  /// 'other_member_pids' with edge-source gating: partners whose
  /// EDGE is above the active prefix are omitted, in both
  /// directions (see 'edge_source').
  pub fn other_member_pids_gated (
    &self,
    pid    : &ID,
    role   : RelationRole,
    active : Option<&crate::source_sets::ActiveSourceSet>,
  ) -> Vec<ID> {
    if role . is_first_role () {
      self . outbound_pids_for_relation_gated (
        pid, role . relation, active )
    } else {
      self . inbound_pids_for_relation_gated (
        pid, role . relation, active ) } }

  pub fn relation_membership_is_real (
    &self,
    owner_pid  : &ID,
    member_pid : &ID,
    member_role : RelationRole,
  ) -> bool {
    self . relation_membership_is_visible (
      owner_pid, member_pid, member_role, None ) }

  /// 'relation_membership_is_real' with edge-source gating: an edge
  /// whose recording source is outside the active prefix does not count
  /// as a membership (see 'edge_source'). Pass None to ask about the
  /// full fold.
  pub fn relation_membership_is_visible (
    &self,
    owner_pid   : &ID,
    member_pid  : &ID,
    member_role : RelationRole,
    active      : Option<&crate::source_sets::ActiveSourceSet>,
  ) -> bool {
    let members : Vec<ID> =
      self . other_member_pids_gated (
        owner_pid, member_role . opposite_role (), active );
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
