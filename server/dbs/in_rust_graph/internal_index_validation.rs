//! Pure consistency validation for `InRustGraph`'s derived indexes.
//!
//! Recomputes every index from node forward data without calling the graph's
//! incremental index-maintenance helpers.  This makes it useful both as a
//! regression oracle and as a precise diagnostic when a published graph is
//! suspected of internal corruption.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::prepared_update::GraphChangeSet;
use crate::types::misc::{ID, members_of};
use crate::types::nodes::rust::NodeRust;
use crate::types::save::{DefineNode, DeleteNode, SaveNode};

use std::collections::{BTreeMap, BTreeSet};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct InternalIndexMismatch {
  pub index : &'static str,
  pub key : ID,
  pub expected : Vec<ID>,
  pub actual : Vec<ID>,
}

type ExpectedIndex = BTreeMap<ID, BTreeSet<ID>>;

#[derive(Debug)]
#[allow(dead_code)]
pub(crate) struct LocalIndexValidation {
  pub(crate) errors                         : Vec<InternalIndexMismatch>,
  pub(crate) node_checks                    : usize,
  pub(crate) identity_checks                : usize,
  pub(crate) relationship_membership_checks : usize,
}

/// Check only nodes and index memberships whose truth can have changed in one
/// prepared batch. Unlike `validate_internal_indexes`, this is bounded by the
/// delta and is suitable as an always-on save guard.
pub(crate) fn validate_local_internal_indexes (
  base        : &InRustGraph,
  candidate   : &InRustGraph,
  definitions : &[DefineNode],
  changes     : &GraphChangeSet,
) -> LocalIndexValidation {
  let mut result : Vec<InternalIndexMismatch> = Vec::new ();
  let mut relationship_membership_checks : usize = 0;
  validate_local_nodes (candidate, definitions, &mut result);
  validate_local_identity (base, candidate, changes, &mut result);
  for owner in &changes . owners_to_reindex {
    let old_keys : BTreeMap<&'static str, BTreeSet<ID>> = base . nodes
      . get (owner)
      . map (|node| relationship_keys (base, node))
      . unwrap_or_default ();
    let final_keys : BTreeMap<&'static str, BTreeSet<ID>> = candidate . nodes
      . get (owner)
      . map (|node| relationship_keys (candidate, node))
      . unwrap_or_default ();
    for index_name in [
      "contained_by", "subscribers_of", "hiders_of", "overriders_of",
      "textlinks_in",
    ] {
      let keys : BTreeSet<ID> = old_keys . get (index_name) . into_iter ()
        . flatten () . chain (
          final_keys . get (index_name) . into_iter () . flatten ())
        . cloned () . collect ();
      for key in keys {
        relationship_membership_checks += 1;
        let expected : bool = final_keys . get (index_name)
          . is_some_and (|set| set . contains (&key));
        let actual_set : Option<&im::HashSet<ID>> =
          relationship_index (candidate, index_name) . get (&key);
        let actual : bool = actual_set
          . is_some_and (|set| set . contains (owner));
        if expected != actual {
          result . push (membership_mismatch (
            index_name, key . clone (), owner, expected, actual)); }
        if actual_set . is_some_and (|set| set . is_empty ()) {
          result . push (InternalIndexMismatch {
            index    : index_name,
            key,
            expected : Vec::new (),
            actual   : Vec::new (),
          }); }} }}
  result . sort_by (|left, right|
    left . index . cmp (right . index)
      . then_with (|| left . key . cmp (&right . key))
      . then_with (|| left . expected . cmp (&right . expected))
      . then_with (|| left . actual . cmp (&right . actual)));
  LocalIndexValidation {
    errors : result,
    node_checks : definitions . len (),
    identity_checks : changes . affected_ids . len (),
    relationship_membership_checks,
  }
}

fn validate_local_nodes (
  candidate   : &InRustGraph,
  definitions : &[DefineNode],
  result      : &mut Vec<InternalIndexMismatch>,
) {
  for definition in definitions {
    match definition {
      DefineNode::Save (SaveNode (node)) => {
        let expected : NodeRust = NodeRust::from (node);
        if candidate . nodes . get (&node . pid) != Some (&expected) {
          result . push (InternalIndexMismatch {
            index    : "nodes",
            key      : node . pid . clone (),
            expected : vec![node . pid . clone ()],
            actual   : candidate . nodes . get (&node . pid)
              . map (|actual| vec![actual . pid . clone ()])
              . unwrap_or_default (),
          }); }}
      DefineNode::Delete (DeleteNode { id, .. }) => {
        if candidate . nodes . contains_key (id) {
          result . push (InternalIndexMismatch {
            index    : "nodes",
            key      : id . clone (),
            expected : Vec::new (),
            actual   : vec![id . clone ()],
          }); }} }
  }
}

fn validate_local_identity (
  base      : &InRustGraph,
  candidate : &InRustGraph,
  changes   : &GraphChangeSet,
  result    : &mut Vec<InternalIndexMismatch>,
) {
  for id in &changes . affected_ids {
    let final_owner : Option<ID> =
      match changes . canonicalization_changes . iter ()
        . find (|change| &change . id == id) {
        Some (change) => change . new_owner . clone (),
        None          => base . pid_of (id),
      };
    let expected_extra_owner : Option<ID> = match &final_owner {
      Some (owner) if owner != id => Some (owner . clone ()),
      _                           => None,
    };
    let actual_extra_owner : Option<ID> =
      candidate . extra_id_to_pid . get (id) . cloned ();
    if expected_extra_owner != actual_extra_owner {
      result . push (InternalIndexMismatch {
        index    : "extra_id_to_pid",
        key      : id . clone (),
        expected : expected_extra_owner . into_iter () . collect (),
        actual   : actual_extra_owner . into_iter () . collect (),
      }); }
  }
}

fn relationship_keys (
  identity : &InRustGraph,
  node     : &NodeRust,
) -> BTreeMap<&'static str, BTreeSet<ID>> {
  let canonical = |raw : &ID| -> ID {
    identity . pid_of (raw) . unwrap_or_else (|| raw . clone ()) };
  BTreeMap::from ([
    ("contained_by", members_of (&node . contains) . into_iter ()
      . map (|raw| canonical (&raw)) . collect ()),
    ("subscribers_of", members_of (node . subscribes_to . or_default ())
      . into_iter () . map (|raw| canonical (&raw)) . collect ()),
    ("hiders_of", members_of (node . hides_from_its_subscriptions . or_default ())
      . into_iter () . map (|raw| canonical (&raw)) . collect ()),
    ("overriders_of", members_of (node . overrides_view_of . or_default ())
      . into_iter () . map (|raw| canonical (&raw)) . collect ()),
    ("textlinks_in", node . textlinks_to . iter ()
      . map (canonical) . collect ()),
  ])
}

fn relationship_index<'a> (
  graph : &'a InRustGraph,
  name  : &str,
) -> &'a im::HashMap<ID, im::HashSet<ID>> {
  match name {
    "contained_by"   => &graph . contained_by,
    "subscribers_of" => &graph . subscribers_of,
    "hiders_of"      => &graph . hiders_of,
    "overriders_of"  => &graph . overriders_of,
    "textlinks_in"   => &graph . textlinks_in,
    _ => unreachable! ("known relationship index"),
  }
}

fn membership_mismatch (
  index    : &'static str,
  key      : ID,
  owner    : &ID,
  expected : bool,
  actual   : bool,
) -> InternalIndexMismatch {
  InternalIndexMismatch {
    index,
    key,
    expected : if expected { vec![owner . clone ()] } else { Vec::new () },
    actual   : if actual { vec![owner . clone ()] } else { Vec::new () },
  }
}

/// Return deterministic, exact differences between all stored derived indexes
/// and indexes independently recomputed from `graph.nodes`.
pub fn validate_internal_indexes (
  graph : &InRustGraph,
) -> Vec<InternalIndexMismatch> {
  let mut expected_extra_owners : ExpectedIndex = BTreeMap::new ();
  for node in graph . nodes . values () {
    for extra in &node . extra_ids {
      expected_extra_owners . entry (extra . clone ()) . or_default ()
        . insert (node . pid . clone ()); }}

  let canonical = |raw : &ID| -> ID {
    match expected_extra_owners . get (raw) {
      Some (owners) if owners . len () == 1 =>
        owners . first () . unwrap () . clone (),
      _ => raw . clone (), } };
  let mut contained_by : ExpectedIndex = BTreeMap::new ();
  let mut subscribers_of : ExpectedIndex = BTreeMap::new ();
  let mut hiders_of : ExpectedIndex = BTreeMap::new ();
  let mut overriders_of : ExpectedIndex = BTreeMap::new ();
  let mut textlinks_in : ExpectedIndex = BTreeMap::new ();
  let record = |index : &mut ExpectedIndex, member : &ID, owner : &ID| {
    index . entry (canonical (member)) . or_default ()
      . insert (owner . clone ()); };
  for node in graph . nodes . values () {
    for member in members_of (&node . contains) {
      record (&mut contained_by, &member, &node . pid); }
    for member in members_of (node . subscribes_to . or_default ()) {
      record (&mut subscribers_of, &member, &node . pid); }
    for member in members_of (node . hides_from_its_subscriptions . or_default ()) {
      record (&mut hiders_of, &member, &node . pid); }
    for member in members_of (node . overrides_view_of . or_default ()) {
      record (&mut overriders_of, &member, &node . pid); }
    for member in &node . textlinks_to {
      record (&mut textlinks_in, member, &node . pid); }}

  let mut result : Vec<InternalIndexMismatch> = Vec::new ();
  compare_set_index (
    "contained_by", &contained_by, &graph . contained_by, &mut result);
  compare_set_index (
    "subscribers_of", &subscribers_of, &graph . subscribers_of, &mut result);
  compare_set_index (
    "hiders_of", &hiders_of, &graph . hiders_of, &mut result);
  compare_set_index (
    "overriders_of", &overriders_of, &graph . overriders_of, &mut result);
  compare_set_index (
    "textlinks_in", &textlinks_in, &graph . textlinks_in, &mut result);

  let actual_extra_owners : ExpectedIndex = graph . extra_id_to_pid . iter ()
    . map (|(extra, pid)| {
      (extra . clone (), std::iter::once (pid . clone ()) . collect ()) })
    . collect ();
  compare_btree_indexes (
    "extra_id_to_pid", &expected_extra_owners, &actual_extra_owners, &mut result);
  result
}

fn compare_set_index (
  name : &'static str,
  expected : &ExpectedIndex,
  actual : &im::HashMap<ID, im::HashSet<ID>>,
  result : &mut Vec<InternalIndexMismatch>,
) {
  let actual : ExpectedIndex = actual . iter ()
    .map (|(key, values)| (
      key . clone (), values . iter () . cloned () . collect ()))
    .collect ();
  compare_btree_indexes (name, expected, &actual, result);
}

fn compare_btree_indexes (
  name : &'static str,
  expected : &ExpectedIndex,
  actual : &ExpectedIndex,
  result : &mut Vec<InternalIndexMismatch>,
) {
  let keys : BTreeSet<ID> = expected . keys () . chain (actual . keys ())
    . cloned () . collect ();
  for key in keys {
    let expected_values : Vec<ID> = expected . get (&key)
      .map (|set| set . iter () . cloned () . collect ())
      .unwrap_or_default ();
    let actual_values : Vec<ID> = actual . get (&key)
      .map (|set| set . iter () . cloned () . collect ())
      .unwrap_or_default ();
    if expected_values != actual_values {
      result . push (InternalIndexMismatch {
        index : name,
        key,
        expected : expected_values,
        actual : actual_values,
      }); }}
}

pub fn format_internal_index_mismatches (
  mismatches : &[InternalIndexMismatch],
) -> String {
  if mismatches . is_empty () {
    return "All in-Rust graph internal indexes are coherent." . to_string (); }
  let mut output = format! (
    "{} in-Rust graph internal-index mismatch(es):\n", mismatches . len ());
  for mismatch in mismatches {
    output . push_str (&format! (
      "  index={} key={} expected={:?} actual={:?}\n",
      mismatch . index, mismatch . key,
      mismatch . expected, mismatch . actual)); }
  output
}
