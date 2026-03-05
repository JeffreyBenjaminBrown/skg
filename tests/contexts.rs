use std::collections::{HashMap, HashSet};

use skg::context::{
  ContextOriginType,
  ContainsMap,
  ReverseContainsMap,
  multiplier_for_label,
  had_id_set_from_nodes,
  link_targets_from_nodes,
  find_roots_from_maps,
  find_multiply_contained_from_maps,
  grow_context_from_origin,
  handle_cycles,
};
use skg::types::misc::ID;
use skg::types::skgnode::{FileProperty, SkgNode, empty_skgnode};

#[test]
fn test_multiplier_for_label_known_types () {
  assert_eq! (multiplier_for_label ("Root"),           100.0);
  assert_eq! (multiplier_for_label ("CycleMember"),     30.0);
  assert_eq! (multiplier_for_label ("Target"),          10.0);
  assert_eq! (multiplier_for_label ("HadID"),           10.0);
  assert_eq! (multiplier_for_label ("MultiContained"),   3.0); }

#[test]
fn test_multiplier_for_label_empty () {
  assert_eq! (multiplier_for_label (""),  1.0); }

#[test]
fn test_multiplier_for_label_unknown () {
  assert_eq! (multiplier_for_label ("Bogus"),  1.0); }

#[test]
fn test_context_origin_type_label_roundtrip () {
  // Every ContextOriginType's label produces the correct multiplier.
  let types : Vec<ContextOriginType> = vec![
    ContextOriginType::Root,
    ContextOriginType::CycleMember,
    ContextOriginType::Target,
    ContextOriginType::HadID,
    ContextOriginType::MultiContained ];
  for ct in types {
    assert_eq! (
      multiplier_for_label (ct . label ()),
      ct . multiplier () ); } }

#[test]
fn test_had_id_set_from_nodes_empty () {
  let nodes : Vec<SkgNode> = vec![];
  let result : HashSet<ID> = had_id_set_from_nodes (&nodes);
  assert! (result . is_empty ()); }

#[test]
fn test_had_id_set_from_nodes_mixed () {
  let mut node_with : SkgNode = empty_skgnode ();
  node_with . ids = vec![ID::new ("has-id")];
  node_with . misc = vec![FileProperty::Had_ID_Before_Import];
  let mut node_without : SkgNode = empty_skgnode ();
  node_without . ids = vec![ID::new ("no-id")];
  let nodes : Vec<SkgNode> = vec![node_with, node_without];
  let result : HashSet<ID> = had_id_set_from_nodes (&nodes);
  assert_eq! (result . len (), 1);
  assert! (result . contains (&ID::new ("has-id"))); }

#[test]
fn test_link_targets_from_nodes () {
  let mut node1 : SkgNode = empty_skgnode ();
  node1 . ids = vec![ID::new ("src")];
  node1 . title =
    "see [[id:tgt1][target one]]" . to_string ();
  node1 . body = Some (
    "also [[id:tgt2][target two]]" . to_string () );
  let mut node2 : SkgNode = empty_skgnode ();
  node2 . ids = vec![ID::new ("other")];
  node2 . title = "no links here" . to_string ();
  let nodes : Vec<SkgNode> = vec![node1, node2];
  let targets : HashSet<ID> =
    link_targets_from_nodes (&nodes);
  assert_eq! (targets . len (), 2);
  assert! (targets . contains (&ID::new ("tgt1")));
  assert! (targets . contains (&ID::new ("tgt2"))); }

#[test]
fn test_origin_type_priority () {
  // When a node qualifies as multiple types,
  // the highest-priority (largest multiplier) wins.
  // Root (100) > Target (10) > MultiContained (3).
  let mut origin_types : HashMap<ID, ContextOriginType> =
    HashMap::new ();
  // Simulate: start from lowest priority.
  origin_types . insert (
    ID::new ("a"), ContextOriginType::MultiContained);
  origin_types . insert (
    ID::new ("a"), ContextOriginType::Target);
  origin_types . insert (
    ID::new ("a"), ContextOriginType::Root);
  // Last insert wins (Root).
  assert_eq! (
    origin_types . get (&ID::new ("a")),
    Some (&ContextOriginType::Root) ); }

#[test]
fn test_find_roots_from_maps () {
  // a → b, a → c. d is a root (not contained).
  let all_ids : HashSet<ID> =
    HashSet::from ([
      ID::new ("a"), ID::new ("b"),
      ID::new ("c"), ID::new ("d") ]);
  let mut reverse : ReverseContainsMap = HashMap::new ();
  reverse . insert (
    ID::new ("b"), vec![ID::new ("a")] );
  reverse . insert (
    ID::new ("c"), vec![ID::new ("a")] );
  let roots : HashSet<ID> =
    find_roots_from_maps (&all_ids, &reverse);
  assert_eq! (roots . len (), 2);
  assert! (roots . contains (&ID::new ("a")));
  assert! (roots . contains (&ID::new ("d"))); }

#[test]
fn test_find_multiply_contained_from_maps () {
  // b is contained by both a and c.
  let mut reverse : ReverseContainsMap = HashMap::new ();
  reverse . insert (
    ID::new ("b"), vec![ID::new ("a"), ID::new ("c")] );
  reverse . insert (
    ID::new ("d"), vec![ID::new ("a")] );
  let multi : HashSet<ID> =
    find_multiply_contained_from_maps (&reverse);
  assert_eq! (multi . len (), 1);
  assert! (multi . contains (&ID::new ("b"))); }

#[test]
fn test_grow_context_simple_tree () {
  // a (root/origin) → b → c
  // d (root/origin) → e
  // Growing from a should give {a, b, c}, truncated at d.
  let mut contains_map : ContainsMap = HashMap::new ();
  contains_map . insert (
    ID::new ("a"), vec![ID::new ("b")] );
  contains_map . insert (
    ID::new ("b"), vec![ID::new ("c")] );
  contains_map . insert (
    ID::new ("d"), vec![ID::new ("e")] );
  let origins : HashSet<ID> =
    HashSet::from ([ID::new ("a"), ID::new ("d")]);
  let ctx : HashSet<ID> =
    grow_context_from_origin (
      &ID::new ("a"), &origins, &contains_map );
  assert_eq! (ctx . len (), 3);
  assert! (ctx . contains (&ID::new ("a")));
  assert! (ctx . contains (&ID::new ("b")));
  assert! (ctx . contains (&ID::new ("c"))); }

#[test]
fn test_grow_context_truncates_at_other_origin () {
  // a (origin) → b → c (origin) → d
  // Growing from a should give {a, b}, stopping before c.
  let mut contains_map : ContainsMap = HashMap::new ();
  contains_map . insert (
    ID::new ("a"), vec![ID::new ("b")] );
  contains_map . insert (
    ID::new ("b"), vec![ID::new ("c")] );
  contains_map . insert (
    ID::new ("c"), vec![ID::new ("d")] );
  let origins : HashSet<ID> =
    HashSet::from ([ID::new ("a"), ID::new ("c")]);
  let ctx_a : HashSet<ID> =
    grow_context_from_origin (
      &ID::new ("a"), &origins, &contains_map );
  assert_eq! (ctx_a . len (), 2);
  assert! (ctx_a . contains (&ID::new ("a")));
  assert! (ctx_a . contains (&ID::new ("b")));
  let ctx_c : HashSet<ID> =
    grow_context_from_origin (
      &ID::new ("c"), &origins, &contains_map );
  assert_eq! (ctx_c . len (), 2);
  assert! (ctx_c . contains (&ID::new ("c")));
  assert! (ctx_c . contains (&ID::new ("d"))); }

#[test]
fn test_handle_cycles_detects_cycle () {
  // a → b → c → a (cycle: a, b, c)
  // d is a root (origin), not in the cycle.
  let mut contains_map : ContainsMap = HashMap::new ();
  contains_map . insert (
    ID::new ("a"), vec![ID::new ("b")] );
  contains_map . insert (
    ID::new ("b"), vec![ID::new ("c")] );
  contains_map . insert (
    ID::new ("c"), vec![ID::new ("a")] );
  contains_map . insert (
    ID::new ("d"), vec![] );
  let mut reverse_map : ReverseContainsMap = HashMap::new ();
  reverse_map . insert (
    ID::new ("b"), vec![ID::new ("a")] );
  reverse_map . insert (
    ID::new ("c"), vec![ID::new ("b")] );
  reverse_map . insert (
    ID::new ("a"), vec![ID::new ("c")] );
  let all_node_ids : HashSet<ID> =
    HashSet::from ([
      ID::new ("a"), ID::new ("b"),
      ID::new ("c"), ID::new ("d") ]);
  let mut origin_types : HashMap<ID, ContextOriginType> =
    HashMap::new ();
  origin_types . insert (
    ID::new ("d"), ContextOriginType::Root );
  let mut all_contexts : Vec<HashSet<ID>> =
    vec![HashSet::from ([ID::new ("d")])];
  handle_cycles (
    &all_node_ids,
    &contains_map,
    &reverse_map,
    &mut origin_types,
    &mut all_contexts );
  // All cycle members should now be CycleMember origins.
  assert_eq! (
    origin_types . get (&ID::new ("a")),
    Some (&ContextOriginType::CycleMember) );
  assert_eq! (
    origin_types . get (&ID::new ("b")),
    Some (&ContextOriginType::CycleMember) );
  assert_eq! (
    origin_types . get (&ID::new ("c")),
    Some (&ContextOriginType::CycleMember) );
  // All nodes should be covered.
  let covered : HashSet<ID> =
    all_contexts . iter ()
    . flat_map ( |ctx| ctx . iter () . cloned () )
    . collect ();
  assert! (covered . contains (&ID::new ("a")));
  assert! (covered . contains (&ID::new ("b")));
  assert! (covered . contains (&ID::new ("c")));
  assert! (covered . contains (&ID::new ("d"))); }
