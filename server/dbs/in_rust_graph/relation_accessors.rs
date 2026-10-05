use std::collections::HashSet;

use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::git::NodeChanges;
use crate::types::list::Diff_Item;
use crate::types::misc::{ID, RelPartner, RelationshipMemberKey, SkgRepoName, members_of};
use crate::types::nodes::rust::GraphnodeInRust;

/// The five stored outbound relationship types and their endpoint roles.
/// This is domain vocabulary; storage adapters consume it rather than own it.
pub const OUTBOUND_RELATIONSHIP_TYPES : &[(&str, &str, &str)] = &[
  ("contains",                      "container",  "content"),
  ("linksTo",                       "mentioner",  "mentioned"),
  ("subscribesTo",                  "subscriber", "subscribee"),
  ("hidesFromSubs",                 "hider",      "hidden"),
  ("overrides",                     "overrider",  "overridden"),
];

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum NodeRelation {
  Contains,
  LinksTo,
  SubscribesTo,
  HidesFromSubs, // "hides from its subscriptions": the hider keeps the hidden node out of what it shows from its subscribees.
  Overrides,
}

impl NodeRelation {
  pub fn relation_name (self) -> &'static str {
    match self {
      Self::Contains =>
        "contains",
      Self::LinksTo =>
        "linksTo",
      Self::SubscribesTo =>
        "subscribesTo",
      Self::HidesFromSubs =>
        "hidesFromSubs",
      Self::Overrides =>
        "overrides",
    } }


  /// The per-stage diff of this relation's outbound list within a
  /// NodeChanges, or None for a relation NodeChanges does not diff
  /// (links are inferred from node text, not stored as a list).
  /// Membership-sign consumers (e.g. 'phantom_axes') call this with
  /// the one relation their folder represents, so a sign can never be
  /// read from a different relation that involves the same ID.
  pub fn diff_in_nodechanges<'a> (
    self,
    nc : &'a NodeChanges,
  ) -> Option<&'a [Diff_Item<ID>]> {
    match self {
      Self::Contains =>
        Some ( & nc . contains_diff ),
      Self::SubscribesTo =>
        Some ( & nc . subscribesTo_diff ),
      Self::HidesFromSubs =>
        Some ( & nc . hides_diff ),
      Self::Overrides =>
        Some ( & nc . overrides_diff ),
      Self::LinksTo =>
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
// The partner-role vocabulary: ROLENAME <-> RelationRole <-> role tree
// triple <-> glyph. The single source of truth shared by the request
// layer ('(roleTree ROLENAME)'), the role-tree engine, and the birth herald.
//

impl RelationRole {
  // The nine partner roles a role tree can graft, named for the
  // role the grafted partner plays toward the origin (= toward its
  // viewparent). 'Contains, Second' ("content") is intentionally
  // absent -- the recursive content view already serves it.
  pub const CONTAINER : RelationRole =
    RelationRole { relation : NodeRelation::Contains,
                   position : BinaryRolePosition::First };
  pub const MENTIONER : RelationRole =
    RelationRole { relation : NodeRelation::LinksTo,
                   position : BinaryRolePosition::First };
  pub const MENTIONED : RelationRole =
    RelationRole { relation : NodeRelation::LinksTo,
                   position : BinaryRolePosition::Second };
  pub const OVERRIDER : RelationRole =
    RelationRole { relation : NodeRelation::Overrides,
                   position : BinaryRolePosition::First };
  pub const OVERRIDDEN : RelationRole =
    RelationRole { relation : NodeRelation::Overrides,
                   position : BinaryRolePosition::Second };
  pub const HIDER : RelationRole =
    RelationRole { relation : NodeRelation::HidesFromSubs,
                   position : BinaryRolePosition::First };
  pub const HIDDEN : RelationRole =
    RelationRole { relation : NodeRelation::HidesFromSubs,
                   position : BinaryRolePosition::Second };
  pub const SUBSCRIBER : RelationRole =
    RelationRole { relation : NodeRelation::SubscribesTo,
                   position : BinaryRolePosition::First };
  pub const SUBSCRIBEE : RelationRole =
    RelationRole { relation : NodeRelation::SubscribesTo,
                   position : BinaryRolePosition::Second };

  /// The single ROLENAME token naming this partner role in the wire
  /// grammar ('(roleTree ROLENAME)', '(birth roleGraft ROLENAME)').
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

  /// The path glyph for this partner role. Birth styling is applied
  /// by the clients. Panics for a role absent from PARTNER_ROLE_VOCAB.
  pub fn glyph (
    self,
  ) -> &'static str {
    PARTNER_ROLE_VOCAB . iter ()
      . find ( |(_, role, _)| *role == self )
      . map ( |(_, _, glyph)| *glyph )
      . expect ( "RelationRole::glyph: role absent from PARTNER_ROLE_VOCAB" ) }

  /// The '(relation, input_role, output_role)' triple the role tree
  /// engine consumes: output_role is THIS (partner) role, input_role
  /// is the origin's (opposite) role.
  pub fn role_tree_triple (
    self,
  ) -> (&'static str, &'static str, &'static str) {
    let (first, second) : (&'static str, &'static str) =
      self . relation . roles ();
    let relation : &'static str = self . relation . relation_name ();
    match self . position {
      BinaryRolePosition::First  => (relation, second, first),
      BinaryRolePosition::Second => (relation, first, second), } }
}

/// Each row is (ROLENAME, RelationRole, glyph). The role-tree triple is
/// DERIVED from the RelationRole ('RelationRole::role_tree_triple'), so
/// it is not stored here. A folder is named by its RELATION (spanning both
/// roles); a path and a birth are named by the one ROLE the grafted
/// partner plays. 'Contains, Second' is absent (see the consts above).
pub const PARTNER_ROLE_VOCAB
  : &[ (&'static str, RelationRole, &'static str) ] = &[
  ("container",  RelationRole::CONTAINER,   "}"),
  ("mentioner", RelationRole::MENTIONER, "←"),
  ("mentioned",   RelationRole::MENTIONED,   "→"),
  ("overrider",  RelationRole::OVERRIDER,   "Op"),
  ("overridden", RelationRole::OVERRIDDEN,  "pO"),
  ("hider",      RelationRole::HIDER,       "Hp"),
  ("hidden",     RelationRole::HIDDEN,      "pH"),
  ("subscriber", RelationRole::SUBSCRIBER,  "Sp"),
  ("subscribee", RelationRole::SUBSCRIBEE,  "pS"),
];

impl InRustGraph {
  /// IDs whose rendered relationship context can change when any seed changes.
  /// Ordinary relations contribute one hop in either direction. Override
  /// relations instead contribute complete, independently visited walks in
  /// each direction, beginning only at the seeds.
  pub fn update_relevant_neighborhood (
    &self,
    seeds : impl IntoIterator<Item = ID>,
  ) -> HashSet<ID> {
    let seed_skgids : HashSet<ID> = seeds . into_iter ()
      . map (|skgid| self . pid_of (&skgid) . unwrap_or (skgid))
      . collect ();
    let mut result : HashSet<ID> = seed_skgids . clone ();
    for seed in &seed_skgids {
      for relation in [
        NodeRelation::Contains,
        NodeRelation::LinksTo,
        NodeRelation::SubscribesTo,
        NodeRelation::HidesFromSubs,
      ] {
        result . extend (
          self . outbound_skgids_for_relation (seed, relation)
            . into_iter ()
            . map (|skgid| self . pid_of (&skgid) . unwrap_or (skgid)));
        result . extend (
          self . inbound_pids_for_relation (seed, relation)); }}
    result . extend (self . override_walk_from_seeds (
      &seed_skgids, BinaryRolePosition::First));
    result . extend (self . override_walk_from_seeds (
      &seed_skgids, BinaryRolePosition::Second));
    result
  }

  fn override_walk_from_seeds (
    &self,
    seeds         : &HashSet<ID>,
    seed_position : BinaryRolePosition,
  ) -> HashSet<ID> {
    let mut visited : HashSet<ID> = seeds . clone ();
    let mut pending : Vec<ID> = seeds . iter () . cloned () . collect ();
    while let Some (node) = pending . pop () {
      let next : Vec<ID> = match seed_position {
        BinaryRolePosition::First =>
          self . outbound_skgids_for_relation (
            &node, NodeRelation::Overrides),
        BinaryRolePosition::Second =>
          self . inbound_pids_for_relation (
            &node, NodeRelation::Overrides), };
      for raw_skgid in next {
        let skgid : ID = self . pid_of (&raw_skgid) . unwrap_or (raw_skgid);
        if visited . insert (skgid . clone ()) {
          pending . push (skgid); }} }
    visited
  }

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
  /// accessors, this preserves an unresolved raw ID and its relRepo.
  /// Callers that require a current graphnode should keep using the existing
  /// canonical-PID accessors instead.
  pub fn outbound_rel_partners_for_relation_gated (
    &self,
    pid      : &ID,
    relation : NodeRelation,
    active   : Option<&crate::skgrepo_sets::ActiveSkgRepoSet>,
  ) -> Vec<RelPartner<ID>> {
    let Some (node) = self . nodes . get (pid) else {
      return Vec::new (); };
    let members : Vec<RelPartner<ID>> = match relation {
      NodeRelation::Contains =>
        node . contains . clone (),
      NodeRelation::SubscribesTo =>
        node . subscribesTo . or_default () . to_vec (),
      NodeRelation::HidesFromSubs =>
        node . hidesFromSubs . or_default () . to_vec (),
      NodeRelation::Overrides =>
        node . overrides . or_default () . to_vec (),
      NodeRelation::LinksTo =>
        return Vec::new (),
    };
    members . into_iter ()
      . filter ( |member| match active {
        None => true,
        Some (set) => set . is_all ()
          || set . contains_skgrepo (&member . relRepo), } )
      . collect () }

  /// Raw outbound member IDs whose relRepo is in the active set. Unlike the
  /// PID-oriented accessor, this retains unresolved stored IDs so rendering
  /// can preserve them as Unknown phantoms.
  pub fn outbound_skgids_for_relation_gated (
    &self,
    pid      : &ID,
    relation : NodeRelation,
    active   : Option<&crate::skgrepo_sets::ActiveSkgRepoSet>,
  ) -> Vec<ID> {
    if relation == NodeRelation::LinksTo {
      return self . outbound_skgids_for_relation (pid, relation); }
    self . outbound_rel_partners_for_relation_gated (
      pid, relation, active ) . into_iter ()
      . map ( |member| member . member )
      . collect () }

  /// The relRepo of the relationship from RECORDER to TARGET under RELATION,
  /// read from the recorder's outbound list. None when no such relationship
  /// exists. This is how INBOUND
  /// surfaces gate: an inbound partner P of X is visible at the
  /// active set iff relRepo(P, R, X) is active -- private
  /// memberships must not surface through ancestry, role trees, or
  /// inbound folders when the content direction hides them
  /// (render-and-gating, 5_plan.org).
  pub fn relRepo (
    &self,
    recorder : &ID,
    relation : NodeRelation,
    target   : &ID,
  ) -> Option<SkgRepoName> {
    let target_key : ID = self . pid_of (target) ? ;
    let node : &GraphnodeInRust = self . nodes . get (recorder) ? ;
    let rel_partners : Vec<RelPartner<ID>> = match relation {
      NodeRelation::Contains =>
        node . contains . clone (),
      NodeRelation::SubscribesTo =>
        node . subscribesTo . or_default () . to_vec (),
      NodeRelation::HidesFromSubs =>
        node . hidesFromSubs . or_default () . to_vec (),
      NodeRelation::Overrides =>
        node . overrides . or_default () . to_vec (),
      NodeRelation::LinksTo =>
        // links derive from the body, which is home-only, so
        // their skgrepo is the recorder's home by construction.
        return self . nodes . get (recorder)
          . map ( |n| n . home_skgrepo . clone () ), };
    rel_partners . iter ()
      . find ( |m| self . pid_of ( &m . member )
               . as_ref () == Some (&target_key) )
      . map ( |m| m . relRepo . clone () ) }

  /// The relRepo of one exact, stored outbound member ID.
  /// Unlike 'relRepo', this deliberately does not canonicalize the
  /// target: an unresolved raw ID has no PID, but is still a real stored
  /// relationship member and can be edited from an Unknown phantom.
  pub fn relRepo_for_stored_member (
    &self,
    recorder    : &ID,
    relation : NodeRelation,
    raw_member : &ID,
  ) -> Option<SkgRepoName> {
    self . outbound_rel_partners_for_relation_gated (
      recorder, relation, None ) . into_iter ()
      . find ( |member| &member . member == raw_member )
      . map ( |member| member . relRepo ) }

  /// Outbound members whose relRepo is in the active set: the
  /// visible composition of one relation. Pass None for the full composition.
  pub fn outbound_pids_for_relation_gated (
    &self,
    pid      : &ID,
    relation : NodeRelation,
    active   : Option<&crate::skgrepo_sets::ActiveSkgRepoSet>,
  ) -> Vec<ID> {
    self . outbound_skgids_for_relation_gated (
      pid, relation, active ) . iter ()
      . filter_map ( |member| self . pid_of (member) )
      . collect () }

  /// Inbound partners whose RELATIONSHIPS to this node are visible at the
  /// active set (see 'relRepo'). Pass None for all of them.
  pub fn inbound_pids_for_relation_gated (
    &self,
    pid      : &ID,
    relation : NodeRelation,
    active   : Option<&crate::skgrepo_sets::ActiveSkgRepoSet>,
  ) -> Vec<ID> {
    self . inbound_pids_for_relation (pid, relation)
      . into_iter ()
      . filter ( |partner| match active {
        None => true,
        Some (a) => a . is_all ()
          || self . relRepo (partner, relation, pid)
             . map ( |skgrepo| a . contains_skgrepo (&skgrepo) )
             . unwrap_or (false) } )
      . collect () }

  pub fn outbound_skgids_for_relation (
    &self,
    pid      : &ID,
    relation : NodeRelation,
  ) -> Vec<ID> {
    self . nodes . get (pid)
      . map ( |node| outbound_skgids_from_node (node, relation) )
      . unwrap_or_default () }

  pub fn outbound_pids_for_relation (
    &self,
    pid      : &ID,
    relation : NodeRelation,
  ) -> Vec<ID> {
    self . outbound_skgids_for_relation (pid, relation)
      . iter ()
      . filter_map ( |skgid| self . pid_of (skgid) )
      . collect () }

  pub fn inbound_pids_for_relation (
    &self,
    pid      : &ID,
    relation : NodeRelation,
  ) -> Vec<ID> {
    // The inbound members are stored as a set, so their iteration order is
    // nondeterministic (run-to-run). Sort by ID so every consumer gets a stable
    // order: a node's inbound PartnerFolder (e.g. thousands of subscribers) then
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

  /// 'other_member_pids' with relRepo gating: partners whose
  /// RELATIONSHIP is above the active prefix are omitted, in both
  /// directions (see 'relRepo').
  pub fn other_member_pids_gated (
    &self,
    pid    : &ID,
    role   : RelationRole,
    active : Option<&crate::skgrepo_sets::ActiveSkgRepoSet>,
  ) -> Vec<ID> {
    if role . is_first_role () {
      self . outbound_pids_for_relation_gated (
        pid, role . relation, active )
    } else {
      self . inbound_pids_for_relation_gated (
        pid, role . relation, active ) } }

  pub fn relation_membership_is_real (
    &self,
    recorder_pid  : &ID,
    member_pid : &ID,
    member_role : RelationRole,
  ) -> bool {
    self . relation_membership_is_visible (
      recorder_pid, member_pid, member_role, None ) }

  /// 'relation_membership_is_real' with relRepo gating: a relationship
  /// whose relRepo is outside the active prefix does not count
  /// as a membership (see 'relRepo'). Pass None to ask about the
  /// full composition.
  pub fn relation_membership_is_visible (
    &self,
    recorder_pid : &ID,
    member_pid   : &ID,
    member_role  : RelationRole,
    active       : Option<&crate::skgrepo_sets::ActiveSkgRepoSet>,
  ) -> bool {
    let members : Vec<ID> =
      self . other_member_pids_gated (
        recorder_pid, member_role . opposite_role (), active );
    members . contains (member_pid) }
}

fn outbound_skgids_from_node (
  node     : &GraphnodeInRust,
  relation : NodeRelation,
) -> Vec<ID> {
  match relation {
    NodeRelation::Contains =>
      members_of ( &node . contains ),
    NodeRelation::LinksTo =>
      node . linksTo . clone (),
    NodeRelation::SubscribesTo =>
      members_of ( node . subscribesTo . or_default () ),
    NodeRelation::HidesFromSubs =>
      members_of ( node . hidesFromSubs . or_default () ),
    NodeRelation::Overrides =>
      members_of ( node . overrides . or_default () ),
  } }

fn inbound_pid_set (
  graph    : &InRustGraph,
  pid      : &ID,
  relation : NodeRelation,
) -> HashSet<ID> {
  let skgids : Vec<ID> = match relation {
    NodeRelation::Contains =>
      graph . contained_by . get (pid)
      . map ( |s| s . iter () . cloned () . collect () )
      . unwrap_or_default (),
    NodeRelation::LinksTo =>
      graph . mentioners_of . get (pid)
      . map ( |s| s . iter () . cloned () . collect () )
      . unwrap_or_default (),
    NodeRelation::SubscribesTo =>
      graph . subscribers_of . get (pid)
      . map ( |s| s . iter () . cloned () . collect () )
      . unwrap_or_default (),
    NodeRelation::HidesFromSubs =>
      graph . hiders_of . get (pid)
      . map ( |s| s . iter () . cloned () . collect () )
      . unwrap_or_default (),
    NodeRelation::Overrides =>
      graph . overriders_of . get (pid)
      . map ( |s| s . iter () . cloned () . collect () )
      . unwrap_or_default (),
  };
  skgids . into_iter () . collect () }

#[cfg(test)]
#[allow(non_snake_case)]
mod tests {
  use super::*;

  /// Every NodeRelation. The match makes a new variant a compile error
  /// here until it is listed.
  fn every_node_relation () -> Vec<NodeRelation> {
    let every : Vec<NodeRelation> = vec! [
      NodeRelation::Contains, NodeRelation::LinksTo,
      NodeRelation::SubscribesTo, NodeRelation::HidesFromSubs,
      NodeRelation::Overrides ];
    for relation in &every {
      match relation {
        NodeRelation::Contains | NodeRelation::LinksTo
          | NodeRelation::SubscribesTo
          | NodeRelation::HidesFromSubs
          | NodeRelation::Overrides => (), } }
    every }

  /// shared/relations.json, which both clients read, names the same
  /// relations and roles, in the same order, as the enum and
  /// OUTBOUND_RELATIONSHIP_TYPES.
  #[test]
  fn relations_match_the_shared_relations_file () {
    let path : std::path::PathBuf =
      std::path::Path::new ( env! ("CARGO_MANIFEST_DIR") )
      . join ("shared/relations.json");
    let file : serde_json::Value =
      serde_json::from_str ( &std::fs::read_to_string (&path) . unwrap () )
      . unwrap ();
    let from_file : Vec<(String, String, String)> =
      file ["relations"] . as_array () . unwrap () . iter ()
      . map ( |relation| {
        let roles : &Vec<serde_json::Value> =
          relation ["roles"] . as_array () . unwrap ();
        ( relation ["name"] . as_str () . unwrap () . to_string (),
          roles [0] . as_str () . unwrap () . to_string (),
          roles [1] . as_str () . unwrap () . to_string () ) } )
      . collect ();
    let from_rust : Vec<(String, String, String)> =
      OUTBOUND_RELATIONSHIP_TYPES . iter ()
      . map ( |(name, first, second)| (
        name . to_string (), first . to_string (), second . to_string () ) )
      . collect ();
    assert_eq! ( from_file, from_rust );
    let mut enum_names : Vec<&str> =
      every_node_relation () . into_iter ()
      . map ( |relation| relation . relation_name () ) . collect ();
    let mut file_names : Vec<&str> =
      from_file . iter () . map ( |(name, _, _)| name . as_str () ) . collect ();
    enum_names . sort ();
    file_names . sort ();
    assert_eq! ( enum_names, file_names ); }

  /// Every PARTNER_ROLE_VOCAB row round-trips ROLENAME <-> RelationRole,
  /// resolves a glyph, and yields the expected role-tree triple.
  #[test]
  fn partner_role_vocab_round_trips_and_derives_triples () {
    // (rolename, RelationRole, glyph, (relation, input_role, output_role))
    let expected : [(&str, RelationRole, &str,
                     (&str, &str, &str)); 9] = [
      ("container",  RelationRole::CONTAINER,   "}",
       ("contains", "content", "container")),
      ("mentioner", RelationRole::MENTIONER, "←",
       ("linksTo",  "mentioned", "mentioner")),
      ("mentioned",   RelationRole::MENTIONED,   "→",
       ("linksTo",  "mentioner", "mentioned")),
      ("overrider",  RelationRole::OVERRIDER,   "Op",
       ("overrides",         "overridden", "overrider")),
      ("overridden", RelationRole::OVERRIDDEN,  "pO",
       ("overrides",         "overrider", "overridden")),
      ("hider",      RelationRole::HIDER,       "Hp",
       ("hidesFromSubs",                "hidden", "hider")),
      ("hidden",     RelationRole::HIDDEN,      "pH",
       ("hidesFromSubs",                "hider", "hidden")),
      ("subscriber", RelationRole::SUBSCRIBER,  "Sp",
       ("subscribesTo",  "subscribee", "subscriber")),
      ("subscribee", RelationRole::SUBSCRIBEE,  "pS",
       ("subscribesTo",  "subscriber", "subscribee")),
    ];
    for (name, role, glyph, triple) in expected {
      assert_eq! ( role . rolename (), name );
      assert_eq! ( RelationRole::from_rolename (name), Some (role) );
      assert_eq! ( role . glyph (), glyph );
      assert_eq! ( role . role_tree_triple (), triple,
        "wrong role-tree triple for {}", name ); } }

  #[test]
  fn from_rolename_rejects_unknown_and_the_absent_content_role () {
    assert_eq! ( RelationRole::from_rolename (""), None );
    assert_eq! ( RelationRole::from_rolename ("content"), None );
    assert_eq! ( RelationRole::from_rolename ("bogus"), None ); }
}
