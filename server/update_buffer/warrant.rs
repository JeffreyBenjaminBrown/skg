/// Whether the graph relates a viewnode to its viewparent as the
/// viewnode's position implies ('Warrant'). The stats pass stores the
/// answer on every occurrence ('ViewnodeStats::isWarranted').

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::{
  BinaryRolePosition, NodeRelation, RelationRole };
use crate::dbs::node_lookup::graphnode_from_graph;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::to_org::complete::partner_folder::goal_list::goal_list_for_hiddenOutsideOfSubscribee_folder;
use crate::to_org::extend::containerward_chain::is_in_containerward_chain;
use crate::types::misc::{ID, SkgConfig, members_of};
use crate::types::nodes::complete::{Graphnode, flag_is_true};
use crate::types::viewnode::{
  Folder, PropertyMember, Viewnode, ViewnodeKind, Vognode, Warrant };
use crate::update_buffer::ancestry::required_ancestor;
use ego_tree::{NodeId, NodeRef};

/// The warrant of 'node', by its position:
/// - a root: warranted;
/// - a node of a containerward chain: it contains the node above;
/// - content under an occurrence: the occurrence contains it;
/// - a partner folder's member: the folder's relation holds between
///   it and the folder's viewparent (for the two compound partner
///   folders, the hide and the containment, or its absence, that
///   define them);
/// - a property member: the node above the property folder has it;
/// - anything else: warranted, since its position implies no
///   relationship.
/// An occurrence stands for the gnid its parent's list would hold
/// ('collected_skgid'), so a shown overrider is judged as the node it
/// overrides.
pub fn warrant_of_viewnode (
  node        : NodeRef<Viewnode>,
  graph       : &InRustGraph,
  config      : &SkgConfig,
  restriction : Option<&SkgrepoRestriction>,
) -> Warrant {
  let warranted : bool = match node . parent () {
    None => true,
    Some (parent) => match &node . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (occurrence)) => {
        let gnid : ID = pid_or_itself (graph, &occurrence . collected_skgid ());
        if is_in_containerward_chain (node) {
          match vognode_gnid (parent) {
            Some (gnid_above) => graph . relation_membership_is_visible (
              &gnid_above, &gnid,
              RelationRole::new (NodeRelation::Contains, BinaryRolePosition::First),
              restriction ),
            None => true, }
        } else {
          match &parent . value () . kind {
            ViewnodeKind::Vognode (Vognode::Unrestricted (container)) =>
              graph . relation_membership_is_visible (
                &pid_or_itself (graph, &container . skgid), &gnid,
                RelationRole::new (NodeRelation::Contains, BinaryRolePosition::Second),
                restriction ),
            ViewnodeKind::Folder (folder)
              if ! folder . members_are_properties () =>
              folder_member_is_warranted (
                parent, folder, &gnid, graph, config, restriction ),
            _ => true, }}},
      ViewnodeKind::PropertyMember (property) =>
        // HEAD alone warrants a member with a ghost warrant; 'Warrant'
        // has no word for that yet, so it reads as warranted.
        node . value () . is_ghostWarranted ()
        || property_member_is_warranted (property, parent, graph),
      _ => true, }};
  if warranted { Warrant::ByGraph } else { Warrant::No } }

/// Whether the folder's relation holds between 'member' and the
/// folder's viewparent. True when an ancestor the folder needs is not
/// an occurrence, or the folder names no relation.
fn folder_member_is_warranted (
  folder      : NodeRef<Viewnode>,
  kind        : &Folder,
  member      : &ID,
  graph       : &InRustGraph,
  config      : &SkgConfig,
  restriction : Option<&SkgrepoRestriction>,
) -> bool {
  let ancestor_gnid = |i : usize| -> Option<ID> {
    // The folder's i-th required ancestor, if it is an occurrence.
    let ancestor : NodeId =
      required_ancestor (folder . tree (), folder . id (), i)
      . ok () . flatten () ?;
    occurrence_gnid (folder . tree () . get (ancestor) ?)
      . map ( |gnid| pid_or_itself (graph, &gnid) ) };
  let relates = |recorder : &ID, relation : NodeRelation| -> bool {
    graph . relation_membership_is_visible (
      recorder, member,
      RelationRole::new (relation, BinaryRolePosition::Second),
      restriction ) };
  match kind {
    Folder::HiddenInSubscribee =>
      // subscriber > subscribeeFolder > subscribee > this folder
      match (ancestor_gnid (0), ancestor_gnid (2)) {
        (Some (subscribee), Some (subscriber)) =>
          relates (&subscriber, NodeRelation::HidesFromSubs)
          && relates (&subscribee, NodeRelation::Contains),
        _ => true, },
    Folder::HiddenOutsideOfSubscribee =>
      // subscriber > subscribeeFolder > this folder
      match ancestor_gnid (1) {
        Some (subscriber) => {
          let goal : Vec<ID> = goal_list_for_hiddenOutsideOfSubscribee_folder (
            graph,
            &graph . outbound_pids_for_relation_gated (
              &subscriber, NodeRelation::HidesFromSubs, restriction ),
            &graph . outbound_pids_for_relation_gated (
              &subscriber, NodeRelation::SubscribesTo, restriction ),
            config );
          goal . iter () . any ( |gnid|
            graph . relationship_member_key (gnid)
            == graph . relationship_member_key (member) ) },
        None => true, },
    _ => match (kind . relation_member_role (), ancestor_gnid (0)) {
      (Some (member_role), Some (recorder)) =>
        graph . relation_membership_is_visible (
          &recorder, member, member_role, restriction ),
      _ => true, }, } }

/// Whether the node above the property folder holding 'property' has
/// it: the alias, the ID (its pid or an extra ID), or the flag set
/// true. True when the node above is not an occurrence the graph has.
fn property_member_is_warranted (
  property : &PropertyMember,
  folder   : NodeRef<Viewnode>,
  graph    : &InRustGraph,
) -> bool {
  let Some (node) : Option<Graphnode> =
    folder . parent () . and_then (occurrence_gnid)
    . and_then ( |gnid| graphnode_from_graph (graph, &gnid) )
    else { return true; };
  match property {
    PropertyMember::Alias { text, .. } =>
      members_of (node . aliases . or_default ()) . contains (text),
    PropertyMember::ID { skgid } =>
      node . pid == *skgid || node . extra_ids . contains (skgid),
    PropertyMember::Flag { flag, .. } =>
      flag_is_true (&node . flags, *flag), } }

fn occurrence_gnid (
  node : NodeRef<Viewnode>,
) -> Option<ID> {
  match &node . value () . kind {
    ViewnodeKind::Vognode (Vognode::Unrestricted (occurrence)) =>
      Some (occurrence . skgid . clone ()),
    _ => None, } }

/// The gnid of any vognode but a restricted one.
fn vognode_gnid (
  node : NodeRef<Viewnode>,
) -> Option<ID> {
  match &node . value () . kind {
    ViewnodeKind::Vognode (vognode) => Some ( vognode . skgid () . clone () ),
    _ => None, } }

/// The pid of the node 'gnid' names, or 'gnid' itself when the graph
/// has no such node.
fn pid_or_itself (
  graph : &InRustGraph,
  gnid  : &ID,
) -> ID {
  graph . pid_of (gnid) . unwrap_or_else ( || gnid . clone () ) }

#[cfg(test)]
#[path = "../../tests/unit/warrant.rs"]
mod tests;
