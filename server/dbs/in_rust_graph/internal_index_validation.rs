//! Pure consistency validation for `InRustGraph`'s derived indexes.
//!
//! Recomputes every index from node forward data without calling the graph's
//! incremental index-maintenance helpers.  This makes it useful both as a
//! regression oracle and as a precise diagnostic when a published graph is
//! suspected of internal corruption.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::{ID, members_of};

use std::collections::{BTreeMap, BTreeSet};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct InternalIndexMismatch {
  pub index : &'static str,
  pub key : ID,
  pub expected : Vec<ID>,
  pub actual : Vec<ID>,
}

type ExpectedIndex = BTreeMap<ID, BTreeSet<ID>>;

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
