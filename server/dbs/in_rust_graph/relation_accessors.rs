use std::collections::{HashMap, HashSet};

use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::{ID, RelPartner, RelationshipMemberKey, SkgrepoName, members_of};
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum NodeRelation {
  Contains,
  LinksTo,
  SubscribesTo,
  HidesFromSubs, // "hides from its subscriptions": the hider keeps the hidden node out of what it shows from its subscribees.
  Overrides,
}

/// Where the relRepos of a recorder's relationships under one relation
/// are stored: in its list, member by member, or, for links, nowhere
/// but its home ('InRustGraph::stored_rel_partners').
enum StoredRelPartners<'a> {
  Listed    (&'a [RelPartner<ID>]),
  AllAtHome (SkgrepoName),
}

/// What the gated graph answers for a gnid ('gnid_presence_gated').
/// A restricted node is one the graph has and the skgrepo restriction
/// keeps out of view; it is not an absent one.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GnidPresence {
  Present,
  Restricted,
  Absent,
}

/// One relationship: 'recorder' lists 'member' (a primary ID, or the
/// stored ID when it resolves to no node) under 'relation'. The save's
/// strandeds computation and the change leaves both name relationships so.
/// Serialized in readiness marks files.
#[derive(Clone, Debug, Eq, Hash, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct RelationshipKey {
  pub relation : NodeRelation,
  pub recorder : ID,
  pub member   : ID,
}

impl NodeRelation {
  pub const ALL : [NodeRelation; 5] = [
    NodeRelation::Contains, NodeRelation::LinksTo,
    NodeRelation::SubscribesTo, NodeRelation::HidesFromSubs,
    NodeRelation::Overrides ];

  /// The inverse of 'relation_name'.
  pub fn from_relation_name (
    name : &str,
  ) -> Option<NodeRelation> {
    NodeRelation::ALL . into_iter ()
      . find ( |relation| relation . relation_name () == name ) }

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
// The roles, by name.
//

impl RelationRole {
  // The nine partner roles, named for the role the partner plays
  // toward the origin (as a member of a partner folder plays toward
  // the folder's viewparent), and CONTENT, which the recursive content
  // view serves and the diff view compares content lists by.
  pub const CONTAINER : RelationRole =
    RelationRole { relation : NodeRelation::Contains,
                   position : BinaryRolePosition::First };
  pub const CONTENT : RelationRole =
    RelationRole { relation : NodeRelation::Contains,
                   position : BinaryRolePosition::Second };
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
}

impl InRustGraph {
  /// IDs whose rendered relationship context can change when any seed changes.
  /// Ordinary relations contribute one hop in either direction. Override
  /// relations instead contribute complete, independently visited walks in
  /// each direction, beginning only at the seeds.
  pub fn update_relevant_neighborhood (
    &self,
    seeds : impl IntoIterator<Item = ID>,
  ) -> HashSet<ID> {
    let seed_gnids : HashSet<ID> = seeds . into_iter ()
      . map (|gnid| self . pid_of (&gnid) . unwrap_or (gnid))
      . collect ();
    let mut result : HashSet<ID> = seed_gnids . clone ();
    for seed in &seed_gnids {
      for relation in [
        NodeRelation::Contains,
        NodeRelation::LinksTo,
        NodeRelation::SubscribesTo,
        NodeRelation::HidesFromSubs,
      ] {
        result . extend (
          self . outbound_gnids_for_relation (seed, relation)
            . into_iter ()
            . map (|gnid| self . pid_of (&gnid) . unwrap_or (gnid)));
        result . extend (
          self . inbound_pids_for_relation (seed, relation)); }}
    result . extend (self . override_walk_from_seeds (
      &seed_gnids, BinaryRolePosition::First));
    result . extend (self . override_walk_from_seeds (
      &seed_gnids, BinaryRolePosition::Second));
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
          self . outbound_gnids_for_relation (
            &node, NodeRelation::Overrides),
        BinaryRolePosition::Second =>
          self . inbound_pids_for_relation (
            &node, NodeRelation::Overrides), };
      for raw_gnid in next {
        let gnid : ID = self . pid_of (&raw_gnid) . unwrap_or (raw_gnid);
        if visited . insert (gnid . clone ()) {
          pending . push (gnid); }} }
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

  /// Whether the gated graph has the node 'gnid' names (a pid or an
  /// extra ID): 'Present' when its home skgrepo is in the skgrepo
  /// restriction, or there is none ('restriction' None or 'all');
  /// 'Restricted' when the graph has it and the restriction excludes its
  /// home; 'Absent' when the graph has no such node.
  pub fn gnid_presence_gated (
    &self,
    gnid        : &ID,
    restriction : Option<&crate::skgrepo_sets::SkgrepoRestriction>,
  ) -> GnidPresence {
    match self . pid_and_skgrepo (gnid) {
      None => GnidPresence::Absent,
      Some ((_pid, home)) => match restriction {
        Some (set) if ! set . is_all ()
                      && ! set . contains_skgrepo (&home) =>
          GnidPresence::Restricted,
        _ => GnidPresence::Present, }, }}

  /// Whether the skgrepo restriction keeps 'gnid' out of a view: the
  /// graph has its node and the restriction excludes its home, or,
  /// under a restriction other than 'all', the graph has no node it
  /// names, which might be private. The graph holds every node on
  /// disk, so it alone answers; every render-side question of the
  /// restriction about a gnid asks here.
  pub fn restriction_keeps_out (
    &self,
    gnid        : &ID,
    restriction : Option<&crate::skgrepo_sets::SkgrepoRestriction>,
  ) -> bool {
    match self . gnid_presence_gated (gnid, restriction) {
      GnidPresence::Present    => false,
      GnidPresence::Restricted => true,
      GnidPresence::Absent     =>
        restriction . is_some_and ( |set| ! set . is_all () ), } }

  /// Stored outbound members for one relationship. Unlike the PID-oriented
  /// accessors, this preserves an unresolved raw ID and its relRepo.
  /// Callers that require a current graphnode should keep using the existing
  /// canonical-PID accessors instead.
  pub fn outbound_rel_partners_for_relation_gated (
    &self,
    pid      : &ID,
    relation : NodeRelation,
    restriction : Option<&crate::skgrepo_sets::SkgrepoRestriction>,
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
      . filter ( |member| match restriction {
        None => true,
        Some (set) => set . is_all ()
          || set . contains_skgrepo (&member . relRepo), } )
      . collect () }

  /// The members 'pid' records under 'relation', each with its relRepo
  /// and its comparison identity ('relationship_member_key'): the pid
  /// when this graph resolves the listed ID, else the raw ID tagged
  /// unresolved. A mention's relRepo is the recorder's home, because
  /// links live in the text. Members whose relRepo is outside the
  /// skgrepo restriction are left out; pass None for every member.
  /// Rendering code reads the tag (an unresolved member is shown as an
  /// unknown); comparisons take the ID from either variant.
  pub fn outbound_member_keys_for_relation_gated (
    &self,
    pid         : &ID,
    relation    : NodeRelation,
    restriction : Option<&crate::skgrepo_sets::SkgrepoRestriction>,
  ) -> Vec<RelPartner<RelationshipMemberKey>> {
    let rel_partners : Vec<RelPartner<ID>> =
      if relation == NodeRelation::LinksTo {
        self . nodes . get (pid) . map ( |node|
          node . linksTo . iter ()
          . map ( |member| RelPartner::at_relRepo (
            node . home_skgrepo . clone (), member . clone () ))
          . filter ( |member| match restriction {
            None => true,
            Some (set) => set . is_all ()
              || set . contains_skgrepo (&member . relRepo), } )
          . collect () )
        . unwrap_or_default ()
      } else { self . outbound_rel_partners_for_relation_gated (
                 pid, relation, restriction ) };
    rel_partners . into_iter ()
      . map ( |member| RelPartner::at_relRepo (
        member . relRepo, self . relationship_member_key (&member . member) ))
      . collect () }

  /// Raw outbound member IDs whose relRepo is in the skgrepo restriction. Unlike the
  /// PID-oriented accessor, this retains unresolved stored IDs so rendering
  /// can preserve them as unknowns.
  pub fn outbound_gnids_for_relation_gated (
    &self,
    pid      : &ID,
    relation : NodeRelation,
    restriction : Option<&crate::skgrepo_sets::SkgrepoRestriction>,
  ) -> Vec<ID> {
    if relation == NodeRelation::LinksTo {
      return self . outbound_gnids_for_relation (pid, relation); }
    self . outbound_rel_partners_for_relation_gated (
      pid, relation, restriction ) . into_iter ()
      . map ( |member| member . member )
      . collect () }

  /// The relRepo of the relationship from RECORDER to TARGET under RELATION,
  /// read from the recorder's outbound list. None when no such relationship
  /// exists. This is how inbound
  /// surfaces gate: an inbound partner P of X is visible at the
  /// skgrepo restriction iff relRepo(P, R, X) is unrestricted -- private
  /// memberships must not surface through ancestry, role trees, or
  /// inbound folders when the content direction hides them
  /// ('docs/telescopes.org', "What lives where").
  pub fn relRepo (
    &self,
    recorder : &ID,
    relation : NodeRelation,
    target   : &ID,
  ) -> Option<SkgrepoName> {
    let target_key : ID = self . pid_of (target) ? ;
    match self . stored_rel_partners (recorder, relation) ? {
      StoredRelPartners::Listed (rel_partners) =>
        rel_partners . iter ()
        . find ( |m| self . pid_of ( &m . member )
                 . as_ref () == Some (&target_key) )
        . map ( |m| m . relRepo . clone () ),
      StoredRelPartners::AllAtHome (home) => Some (home), } }

  /// What 'relRepo' answers for each target of RECORDER's list under
  /// RELATION, keyed by the target's pid, built in one pass over the
  /// list: the first member that resolves to a pid gives that pid's
  /// relRepo. None for LinksTo, whose relRepo is the recorder's home
  /// whatever the target, or for a recorder the graph lacks. Built
  /// once per recorder, it spares a caller that asks of every member
  /// a scan of the list per member.
  pub fn relRepos_by_target_pid (
    &self,
    recorder : &ID,
    relation : NodeRelation,
  ) -> Option<HashMap<ID, SkgrepoName>> {
    let StoredRelPartners::Listed (rel_partners) =
      self . stored_rel_partners (recorder, relation) ?
      else { return None; };
    let mut relRepos : HashMap<ID, SkgrepoName> = HashMap::new ();
    for member in rel_partners {
      if let Some (pid) = self . pid_of (&member . member) {
        relRepos . entry (pid) . or_insert_with ( || member . relRepo . clone () ); }}
    Some (relRepos) }

  /// RECORDER's stored list under RELATION, or, for LinksTo, its home:
  /// links derive from the body, which is home-only, so their skgrepo
  /// is the recorder's home by construction. None for a recorder the
  /// graph lacks.
  fn stored_rel_partners (
    &self,
    recorder : &ID,
    relation : NodeRelation,
  ) -> Option<StoredRelPartners<'_>> {
    let node : &GraphnodeInRust = self . nodes . get (recorder) ? ;
    Some ( match relation {
      NodeRelation::Contains =>
        StoredRelPartners::Listed (&node . contains),
      NodeRelation::SubscribesTo =>
        StoredRelPartners::Listed (node . subscribesTo . or_default ()),
      NodeRelation::HidesFromSubs =>
        StoredRelPartners::Listed (node . hidesFromSubs . or_default ()),
      NodeRelation::Overrides =>
        StoredRelPartners::Listed (node . overrides . or_default ()),
      NodeRelation::LinksTo =>
        StoredRelPartners::AllAtHome (node . home_skgrepo . clone ()), } ) }

  /// The relRepo of one exact, stored outbound member ID.
  /// Unlike 'relRepo', this deliberately does not canonicalize the
  /// target: an unresolved raw ID has no PID, but is still a real stored
  /// relationship member and can be edited from an unknown.
  pub fn relRepo_for_stored_member (
    &self,
    recorder    : &ID,
    relation : NodeRelation,
    raw_member : &ID,
  ) -> Option<SkgrepoName> {
    self . outbound_rel_partners_for_relation_gated (
      recorder, relation, None ) . into_iter ()
      . find ( |member| &member . member == raw_member )
      . map ( |member| member . relRepo ) }

  /// Outbound members whose relRepo is in the skgrepo restriction: the
  /// visible composition of one relation. Pass None for the full composition.
  pub fn outbound_pids_for_relation_gated (
    &self,
    pid      : &ID,
    relation : NodeRelation,
    restriction : Option<&crate::skgrepo_sets::SkgrepoRestriction>,
  ) -> Vec<ID> {
    self . outbound_gnids_for_relation_gated (
      pid, relation, restriction ) . iter ()
      . filter_map ( |member| self . pid_of (member) )
      . collect () }

  /// Inbound partners whose relationships to this node are visible at the
  /// skgrepo restriction (see 'relRepo'). Pass None for all of them.
  pub fn inbound_pids_for_relation_gated (
    &self,
    pid      : &ID,
    relation : NodeRelation,
    restriction : Option<&crate::skgrepo_sets::SkgrepoRestriction>,
  ) -> Vec<ID> {
    self . inbound_pids_for_relation (pid, relation)
      . into_iter ()
      . filter ( |partner| match restriction {
        None => true,
        Some (a) => a . is_all ()
          || self . relRepo (partner, relation, pid)
             . map ( |skgrepo| a . contains_skgrepo (&skgrepo) )
             . unwrap_or (false) } )
      . collect () }

  pub fn outbound_gnids_for_relation (
    &self,
    pid      : &ID,
    relation : NodeRelation,
  ) -> Vec<ID> {
    self . nodes . get (pid)
      . map ( |node| outbound_gnids_from_node (node, relation) )
      . unwrap_or_default () }

  pub fn outbound_pids_for_relation (
    &self,
    pid      : &ID,
    relation : NodeRelation,
  ) -> Vec<ID> {
    self . outbound_gnids_for_relation (pid, relation)
      . iter ()
      . filter_map ( |gnid| self . pid_of (gnid) )
      . collect () }

  pub fn inbound_pids_for_relation (
    &self,
    pid      : &ID,
    relation : NodeRelation,
  ) -> Vec<ID> {
    // The inbound members are stored as a set, so their iteration order is
    // nondeterministic (run-to-run). Sort by ID so every consumer gets a stable
    // order: a node's inbound partner folder (e.g. thousands of subscribers) then
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
  /// relationship is above the skgrepo restriction are omitted, in both
  /// directions (see 'relRepo').
  pub fn other_member_pids_gated (
    &self,
    pid    : &ID,
    role   : RelationRole,
    restriction : Option<&crate::skgrepo_sets::SkgrepoRestriction>,
  ) -> Vec<ID> {
    if role . is_first_role () {
      self . outbound_pids_for_relation_gated (
        pid, role . relation, restriction )
    } else {
      self . inbound_pids_for_relation_gated (
        pid, role . relation, restriction ) } }

  pub fn relation_membership_is_real (
    &self,
    recorder_pid  : &ID,
    member_pid : &ID,
    member_role : RelationRole,
  ) -> bool {
    self . relation_membership_is_visible (
      recorder_pid, member_pid, member_role, None ) }

  /// 'relation_membership_is_real' with relRepo gating: a relationship
  /// whose relRepo is outside the skgrepo restriction does not count
  /// as a membership (see 'relRepo'). Pass None to ask about the
  /// full composition.
  pub fn relation_membership_is_visible (
    &self,
    recorder_pid : &ID,
    member_pid   : &ID,
    member_role  : RelationRole,
    restriction  : Option<&crate::skgrepo_sets::SkgrepoRestriction>,
  ) -> bool {
    let members : Vec<ID> =
      self . other_member_pids_gated (
        recorder_pid, member_role . opposite_role (), restriction );
    members . contains (member_pid) }
}

fn outbound_gnids_from_node (
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
  let gnids : Vec<ID> = match relation {
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
  gnids . into_iter () . collect () }

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
}
