//! The union walk: questions about the union of the graph straddle.
//!
//! Each graph identifies a relationship member by its own resolution
//! of the stored ID ('InRustGraph::pid_of'; an unresolvable ID stays
//! raw), the same identity its recorderward relmaps use, so outbound
//! and inbound answers agree within each graph. A queried ID is taken
//! as a primary ID, unresolved: once a nodeMerge makes B an extra ID
//! of A, the after-graph has no node B, and B's relationships there
//! are A's.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::{
  NodeRelation, RelationRole};
use crate::graph_straddle::types::{
  ChangedPids, GraphStraddle, NodeChanges, Partner, Presence};
use crate::types::misc::{ID, SkgrepoChange, SkgrepoName};
use crate::types::nodes::complete::Flag;
use crate::types::nodes::rust::GraphnodeInRust;
use crate::util::setlike_vector_subtraction;

use std::collections::{BTreeSet, HashMap, HashSet};

/// The roles that the members of a node's own relationship lists
/// play: a change among them is recorded in the node's files.
/// (Mentions are not in a list; they change with the text.)
pub(crate) const ROLES_OF_LISTED_MEMBERS : [RelationRole; 4] = [
  RelationRole::CONTENT,
  RelationRole::SUBSCRIBEE,
  RelationRole::HIDDEN,
  RelationRole::OVERRIDDEN, ];

/// The roles a node's recorded partners play toward it, mentions
/// included: every relationship the node records.
const ROLES_OF_RECORDED_PARTNERS : [RelationRole; 5] = [
  RelationRole::CONTENT,
  RelationRole::MENTIONED,
  RelationRole::SUBSCRIBEE,
  RelationRole::HIDDEN,
  RelationRole::OVERRIDDEN, ];

impl GraphStraddle {
  /// Which graphs hold a node with this primary ID; None if neither.
  pub fn node_presence (
    &self,
    pid : &ID,
  ) -> Option<Presence> {
    Presence::from_membership (
      self . before . nodes . contains_key (pid),
      self . after  . nodes . contains_key (pid) ) }

  /// Empty unless the node is in both graphs: a created or deleted
  /// node's change is its existence ('node_presence').
  pub fn node_changes (
    &self,
    pid : &ID,
  ) -> NodeChanges {
    match ( self . before . get (pid), self . after . get (pid) ) {
      (Some (before), Some (after)) =>
        node_changes_between (before, after),
      _ => NodeChanges::default (), } }

  /// The partners that play 'role' toward the node, in either graph.
  /// 'role' names the role the PARTNER plays, as in role trees
  /// ('PARTNER_ROLE_VOCAB'): CONTAINER gives the node's containers,
  /// '(Contains, Second)' its content. Partners the node records come
  /// in the after-graph's list order, then those only in the
  /// before-graph in its order; recorders of the node come sorted by
  /// ID.
  pub fn partners (
    &self,
    pid  : &ID,
    role : RelationRole,
  ) -> Vec<Partner> {
    let before : Vec<(ID, SkgrepoName)> =
      partners_in_graph (&self . before, pid, role);
    let after : Vec<(ID, SkgrepoName)> =
      partners_in_graph (&self . after, pid, role);
    let before_relRepos : HashMap<&ID, &SkgrepoName> =
      before . iter () . map ( |(id, relRepo)| (id, relRepo) ) . collect ();
    let after_relRepos : HashMap<&ID, &SkgrepoName> =
      after . iter () . map ( |(id, relRepo)| (id, relRepo) ) . collect ();
    let mut partners : Vec<Partner> =
      after . iter ()
      . chain ( before . iter ()
                . filter ( |(id, _)| ! after_relRepos . contains_key (id) ))
      . map ( |(id, _)| {
        let relRepo_before : Option<SkgrepoName> =
          before_relRepos . get (id) . map ( |r| (*r) . clone () );
        let relRepo_after : Option<SkgrepoName> =
          after_relRepos . get (id) . map ( |r| (*r) . clone () );
        Partner {
          id : id . clone (),
          presence : Presence::from_membership (
            relRepo_before . is_some (), relRepo_after . is_some () )
            . expect ("a partner comes from one graph or the other"),
          relRepo_before,
          relRepo_after, }} )
      . collect ();
    if role . is_first_role () {
      partners . sort_by ( |a, b| a . id . cmp (&b . id) ); }
    partners }

  /// The node that acquired 'pid' by a nodeMerge since HEAD: the
  /// after-graph node that lists 'pid' as an extra ID, when 'pid' is
  /// a node of the before-graph only.
  pub fn acquirer_of (
    &self,
    pid : &ID,
  ) -> Option<ID> {
    if self . node_presence (pid) != Some (Presence::OnlyBefore) {
      return None; }
    self . after . pid_of (pid) }

  /// The nodes 'pid' acquired by nodeMerges since HEAD: those of its
  /// after-graph extra IDs that name a node of the before-graph only.
  pub fn acquirees_of (
    &self,
    pid : &ID,
  ) -> Vec<ID> {
    self . after . get (pid)
      . map ( |node| node . extra_ids . iter ()
              . filter ( |extra_id| self . node_presence (extra_id)
                                    == Some (Presence::OnlyBefore) )
              . cloned ()
              . collect () )
      . unwrap_or_default () }

  /// C, split into its out-changed and in-changed halves. Computed once
  /// per straddle.
  pub fn changed_pids (
    &self,
  ) -> &ChangedPids {
    self . changed_pids . get_or_init (
      || self . compute_changed_pids () ) }

  fn compute_changed_pids (
    &self,
  ) -> ChangedPids {
    let all_pids : HashSet<&ID> =
      self . before . nodes . keys ()
      . chain ( self . after . nodes . keys () )
      . collect ();
    let out_changed : BTreeSet<ID> =
      all_pids . into_iter ()
      . filter ( |pid| self . changed_out (pid) )
      . cloned ()
      . collect ();
    let in_changed : BTreeSet<ID> =
      { // Every change to a set of recorders is a change to some
        // recorder's relationships, so it is found from that recorder.
        let mut in_changed : BTreeSet<ID> = BTreeSet::new ();
        for recorder in &out_changed {
          for role in ROLES_OF_RECORDED_PARTNERS {
            for partner in self . partners (recorder, role) {
              if partner . presence != Presence::Both
                && self . node_presence (&partner . id) . is_some () {
                in_changed . insert (partner . id); }} }}
        in_changed };
    ChangedPids { out_changed, in_changed } }

  fn changed_out (
    &self,
    pid : &ID,
  ) -> bool {
    self . node_presence (pid) != Some (Presence::Both)
      || ! self . node_changes (pid) . is_empty ()
      || ROLES_OF_LISTED_MEMBERS . iter () . any (
        |role| self . partners (pid, *role) . iter ()
          . any ( |partner| partner . changed () )) }
}

fn node_changes_between (
  before : &GraphnodeInRust,
  after  : &GraphnodeInRust,
) -> NodeChanges {
  // Gained entries in the after-list's order, lost ones in the
  // before-list's.
  let (aliases_before, aliases_after) : (Vec<String>, Vec<String>) =
    ( before . alias_texts (), after . alias_texts () );
  let aliases_gained : Vec<String> =
    setlike_vector_subtraction (aliases_after . clone (), &aliases_before);
  let aliases_lost : Vec<String> =
    setlike_vector_subtraction (aliases_before, &aliases_after);
  let extra_ids_gained : Vec<ID> =
    setlike_vector_subtraction (after . extra_ids . clone (), &before . extra_ids);
  let extra_ids_lost : Vec<ID> =
    setlike_vector_subtraction (before . extra_ids . clone (), &after . extra_ids);
  let flags_gained : Vec<Flag> =
    setlike_vector_subtraction (after . flags . clone (), &before . flags);
  let flags_lost : Vec<Flag> =
    setlike_vector_subtraction (before . flags . clone (), &after . flags);
  NodeChanges {
    title_changed : before . title != after . title,
    body_changed  : before . body  != after . body,
    aliases_gained, aliases_lost,
    extra_ids_gained, extra_ids_lost,
    flags_gained, flags_lost,
    home : ( before . home_skgrepo != after . home_skgrepo )
      . then ( || SkgrepoChange { old : before . home_skgrepo . clone (),
                                  new : after  . home_skgrepo . clone () } ), } }

/// The partners playing 'role' toward 'pid' in one graph, each with
/// its relationship's relRepo, without duplicates.
fn partners_in_graph (
  graph : &InRustGraph,
  pid   : &ID,
  role  : RelationRole,
) -> Vec<(ID, SkgrepoName)> {
  let mut seen : HashSet<ID> = HashSet::new ();
  let partners : Vec<(ID, SkgrepoName)> =
    if role . is_first_role () { // the partner records the relationship
      graph . inbound_pids_for_relation (pid, role . relation)
        . into_iter ()
        . filter_map ( |recorder| {
          let relRepo : SkgrepoName =
            relRepo_of_recorded_relationship (
              graph, &recorder, role . relation, pid ) ?;
          Some ((recorder, relRepo)) } )
        . collect ()
    } else { // the node records the relationship
      graph . outbound_member_keys_for_relation_gated (pid, role . relation, None)
        . into_iter ()
        . map ( |rel_partner| ( rel_partner . member . into_id (),
                                rel_partner . relRepo ))
        . collect () };
  partners . into_iter ()
    . filter ( |(id, _)| seen . insert (id . clone ()) )
    . collect () }

/// The relRepo of the relationship by which 'recorder' names 'member'
/// (a member ID as this graph resolves it).
fn relRepo_of_recorded_relationship (
  graph    : &InRustGraph,
  recorder : &ID,
  relation : NodeRelation,
  member   : &ID,
) -> Option<SkgrepoName> {
  graph . outbound_member_keys_for_relation_gated (recorder, relation, None)
    . into_iter ()
    . find ( |rel_partner| rel_partner . member . id () == member )
    . map ( |rel_partner| rel_partner . relRepo ) }
