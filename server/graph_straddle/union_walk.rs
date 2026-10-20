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
  BinaryRolePosition, NodeRelation, RelationRole};
use crate::graph_straddle::types::{
  ChangedPids, GraphStraddle, NodeChanges, Partner, Presence};
use crate::types::misc::{ID, RelPartner, SkgrepoName, members_msv};
use crate::types::nodes::complete::Flag;
use crate::types::nodes::rust::GraphnodeInRust;

use std::collections::{BTreeSet, HashMap, HashSet};

const CONTENT : RelationRole =
  RelationRole { relation : NodeRelation::Contains,
                 position : BinaryRolePosition::Second };

/// The roles that the members of a node's own relationship lists
/// play: a change among them is recorded in the node's files.
/// (Mentions are not in a list; they change with the text.)
pub(crate) const ROLES_OF_LISTED_MEMBERS : [RelationRole; 4] = [
  CONTENT,
  RelationRole::SUBSCRIBEE,
  RelationRole::HIDDEN,
  RelationRole::OVERRIDDEN, ];

/// The roles a node's recorded partners play toward it, mentions
/// included: every relationship the node records.
const ROLES_OF_RECORDED_PARTNERS : [RelationRole; 5] = [
  CONTENT,
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
  let alias_texts = |node : &GraphnodeInRust| -> Vec<String> {
    members_msv (&node . aliases) . or_default () . to_vec () };
  let (aliases_gained, aliases_lost) : (Vec<String>, Vec<String>) =
    gained_and_lost (&alias_texts (before), &alias_texts (after));
  let (extra_ids_gained, extra_ids_lost) : (Vec<ID>, Vec<ID>) =
    gained_and_lost (&before . extra_ids, &after . extra_ids);
  let (flags_gained, flags_lost) : (Vec<Flag>, Vec<Flag>) =
    gained_and_lost (&before . flags, &after . flags);
  NodeChanges {
    title_changed : before . title != after . title,
    body_changed  : before . body  != after . body,
    aliases_gained, aliases_lost,
    extra_ids_gained, extra_ids_lost,
    flags_gained, flags_lost,
    home : ( before . home_skgrepo != after . home_skgrepo )
      . then ( || ( before . home_skgrepo . clone (),
                    after  . home_skgrepo . clone () )), } }

/// (in 'after' but not 'before', in 'before' but not 'after'),
/// each in its own list's order.
fn gained_and_lost<T : Clone + PartialEq> (
  before : &[T],
  after  : &[T],
) -> (Vec<T>, Vec<T>) {
  let missing_from = |list : &[T], other : &[T]| -> Vec<T> {
    list . iter ()
      . filter ( |item| ! other . contains (item) )
      . cloned ()
      . collect () };
  ( missing_from (after, before),
    missing_from (before, after) ) }

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
      recorded_rel_partners (graph, pid, role . relation)
        . into_iter ()
        . map ( |rel_partner| (
          graph . pid_of (&rel_partner . member)
            . unwrap_or (rel_partner . member),
          rel_partner . relRepo ))
        . collect () };
  partners . into_iter ()
    . filter ( |(id, _)| seen . insert (id . clone ()) )
    . collect () }

/// Every relationship of 'relation' that 'recorder' records, with its
/// stored member ID. A mention's relRepo is the recorder's home,
/// because links live in the text.
fn recorded_rel_partners (
  graph    : &InRustGraph,
  recorder : &ID,
  relation : NodeRelation,
) -> Vec<RelPartner<ID>> {
  if relation != NodeRelation::LinksTo {
    return graph . outbound_rel_partners_for_relation_gated (
      recorder, relation, None ); }
  let Some (node) = graph . get (recorder) else {
    return Vec::new (); };
  node . linksTo . iter ()
    . map ( |member| RelPartner::at_relRepo (
      node . home_skgrepo . clone (), member . clone () ))
    . collect () }

/// The relRepo of the relationship by which 'recorder' names 'member'
/// (a member ID as this graph resolves it).
fn relRepo_of_recorded_relationship (
  graph    : &InRustGraph,
  recorder : &ID,
  relation : NodeRelation,
  member   : &ID,
) -> Option<SkgrepoName> {
  recorded_rel_partners (graph, recorder, relation)
    . into_iter ()
    . find ( |rel_partner|
      graph . pid_of (&rel_partner . member) . as_ref ()
        . unwrap_or (&rel_partner . member) == member )
    . map ( |rel_partner| rel_partner . relRepo ) }
