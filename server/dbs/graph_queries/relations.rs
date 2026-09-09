use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::{
  BinaryRolePosition, NodeRelation, RelationRole};
use crate::source_sets::ActiveSourceSet;
use crate::types::misc::ID;

use std::collections::{HashMap, HashSet};

/// Each relation's name, first role, and second role.
pub const OUTBOUND_RELATIONSHIP_TYPES : &[(&str, &str, &str)] = &[
  ("contains",                    "container",  "contained"),
  ("textlinks_to",                "source",     "dest"),
  ("subscribes",                  "subscriber", "subscribee"),
  ("hides_from_its_subscriptions", "hider",      "hidden"),
  ("overrides_view_of",           "overrider",  "overridden"),
];

/// Resolve a relation-name triple to the input endpoint's typed role.
/// Unknown names and identical input/output roles have no meaning.
pub fn input_role_from_names (
  relation    : &str,
  input_role  : &str,
  output_role : &str,
) -> Option<RelationRole> {
  let relations : [NodeRelation; 5] = [
    NodeRelation::Contains, NodeRelation::TextlinksTo, NodeRelation::Subscribes,
    NodeRelation::HidesFromItsSubscriptions, NodeRelation::OverridesViewOf];
  OUTBOUND_RELATIONSHIP_TYPES . iter () . zip (relations)
    . find_map ( |((name, first, second), typed)| {
      if *name != relation { return None; }
      let position : BinaryRolePosition =
        if input_role == *first && output_role == *second {
          BinaryRolePosition::First
        } else if input_role == *second && output_role == *first {
          BinaryRolePosition::Second
        } else { return None; };
      Some (RelationRole::new (typed, position)) } ) }

/// Union of the selected nodes' partners. The role belongs to each
/// input node; partners occupy its opposite role.
pub fn find_related_nodes (
  graph : &InRustGraph,
  nodes : &[ID],
  role : RelationRole,
  active : Option<&ActiveSourceSet>,
) -> HashSet<ID> {
  nodes . iter () . flat_map ( |id|
    visible_related_pids (graph, id, role, active)) . collect () }

/// Preserve outbound list order and deterministic inbound ID order.
/// Both the edge's recording source and the partner's home must be
/// visible. Unknown IDs never trigger another store lookup.
pub fn visible_related_pids (
  graph : &InRustGraph,
  id : &ID,
  role : RelationRole,
  active : Option<&ActiveSourceSet>,
) -> Vec<ID> {
  let Some (pid) : Option<ID> = graph . pid_of (id)
    else { return Vec::new (); };
  if !pid_source_is_active (graph, active, &pid) { return Vec::new (); }
  graph . other_member_pids_gated (&pid, role, active)
    . into_iter ()
    . filter ( |partner| pid_source_is_active (graph, active, partner))
    . collect () }

pub fn find_container_ids_of_pid (
  graph : &InRustGraph,
  pid : &ID,
  active : Option<&ActiveSourceSet>,
) -> HashSet<ID> {
  visible_related_pids (
    graph, pid, RelationRole::CONTAINER . opposite_role (), active)
    . into_iter () . collect () }

/// Containment restricted to the supplied primary IDs. Empty entries
/// are omitted; both maps are derived from the same visible edges.
pub fn contains_from_pids (
  graph : &InRustGraph,
  pids : &[ID],
  active : Option<&ActiveSourceSet>,
) -> (HashMap<ID, HashSet<ID>>, HashMap<ID, HashSet<ID>>) {
  let selected : HashSet<ID> = pids . iter () . cloned () . collect ();
  let mut containers : HashMap<ID, HashSet<ID>> = HashMap::new ();
  let mut contents : HashMap<ID, HashSet<ID>> = HashMap::new ();
  for pid in pids {
    for child in visible_related_pids (graph, pid, RelationRole::CONTAINER, active) {
      if selected . contains (&child) {
        containers . entry (pid . clone ()) . or_default () . insert (child . clone ());
        contents . entry (child) . or_default () . insert (pid . clone ()); }} }
  (containers, contents) }

/// Node-home visibility used alongside relation provenance gates.
pub(crate) fn pid_source_is_active (
  graph : &InRustGraph,
  active : Option<&ActiveSourceSet>,
  pid : &ID,
) -> bool {
  graph . get (pid) . is_some_and ( |node| active . is_none_or ( |active|
    active . is_all () || active . contains_source (&node . source))) }
