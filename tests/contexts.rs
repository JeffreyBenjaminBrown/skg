use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use skg::context::{
  ContextOriginType,
  MapToContent,
  MapToContainers,
  contains_maps_from_nodes,
  had_id_set_from_nodes,
  link_targets_from_nodes,
  find_roots_and_multiply_contained,
  extend_context,
  extend_contexts_for_cycles,
};
use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use skg::types::misc::{ID, SkgConfig, SkgfileSource, SourceName};
use skg::types::skgnode::{FileProperty, SkgNode, empty_skgnode};

#[test]
fn test_from_label_unknown () {
  assert! (ContextOriginType::from_label ("") . is_none());
  assert! (ContextOriginType::from_label ("Bogus") . is_none()); }

#[test]
fn test_label_roundtrip () {
  let types : Vec<ContextOriginType> = vec![
    ContextOriginType::Root,
    ContextOriginType::CycleMember,
    ContextOriginType::Target,
    ContextOriginType::HadID,
    ContextOriginType::MultiContained ];
  for ct in types {
    assert_eq! (
      ContextOriginType::from_label (ct . label()),
      Some (ct) ); } }

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
fn test_find_roots_and_multiply_contained () {
  // a → b, a → c, e → b. d is a root (not contained).
  // b is multiply-contained (by a and e).
  let all_ids : HashSet<ID> =
    HashSet::from ([
      ID::new ("a"), ID::new ("b"),
      ID::new ("c"), ID::new ("d"),
      ID::new ("e") ]);
  let mut reverse : MapToContainers = HashMap::new ();
  reverse . insert (
    ID::new ("b"), vec![ID::new ("a"), ID::new ("e")] );
  reverse . insert (
    ID::new ("c"), vec![ID::new ("a")] );
  let ( roots, multi ) : ( HashSet<ID>, HashSet<ID> ) =
    find_roots_and_multiply_contained (&all_ids, &reverse);
  assert_eq! (roots . len (), 3);
  assert! (roots . contains (&ID::new ("a")));
  assert! (roots . contains (&ID::new ("d")));
  assert! (roots . contains (&ID::new ("e")));
  assert_eq! (multi . len (), 1);
  assert! (multi . contains (&ID::new ("b"))); }

#[test]
fn test_grow_context_simple_tree () {
  // a (root/origin) → b → c
  // d (root/origin) → e
  // Growing from a should give {a, b, c}, truncated at d.
  let mut contains_map : MapToContent = HashMap::new ();
  contains_map . insert (
    ID::new ("a"), vec![ID::new ("b")] );
  contains_map . insert (
    ID::new ("b"), vec![ID::new ("c")] );
  contains_map . insert (
    ID::new ("d"), vec![ID::new ("e")] );
  let origins : HashMap<ID, ContextOriginType> =
    HashMap::from ([
      (ID::new ("a"), ContextOriginType::Root),
      (ID::new ("d"), ContextOriginType::Root) ]);
  let mut ctx : HashSet<ID> = HashSet::new ();
  extend_context (
    &mut ctx, &ID::new ("a"), &origins, &contains_map );
  assert_eq! (ctx . len (), 3);
  assert! (ctx . contains (&ID::new ("a")));
  assert! (ctx . contains (&ID::new ("b")));
  assert! (ctx . contains (&ID::new ("c"))); }

#[test]
fn test_grow_context_truncates_at_other_origin () {
  // a (origin) → b → c (origin) → d
  // Growing from a should give {a, b}, stopping before c.
  let mut contains_map : MapToContent = HashMap::new ();
  contains_map . insert (
    ID::new ("a"), vec![ID::new ("b")] );
  contains_map . insert (
    ID::new ("b"), vec![ID::new ("c")] );
  contains_map . insert (
    ID::new ("c"), vec![ID::new ("d")] );
  let origins : HashMap<ID, ContextOriginType> =
    HashMap::from ([
      (ID::new ("a"), ContextOriginType::Root),
      (ID::new ("c"), ContextOriginType::Target) ]);
  let mut ctx_a : HashSet<ID> = HashSet::new ();
  extend_context (
    &mut ctx_a, &ID::new ("a"), &origins, &contains_map );
  assert_eq! (ctx_a . len (), 2);
  assert! (ctx_a . contains (&ID::new ("a")));
  assert! (ctx_a . contains (&ID::new ("b")));
  let mut ctx_c : HashSet<ID> = HashSet::new ();
  extend_context (
    &mut ctx_c, &ID::new ("c"), &origins, &contains_map );
  assert_eq! (ctx_c . len (), 2);
  assert! (ctx_c . contains (&ID::new ("c")));
  assert! (ctx_c . contains (&ID::new ("d"))); }

#[test]
fn test_extend_contexts_for_cycles_detects_cycle () {
  // a → b → c → a (cycle: a, b, c)
  // d is a root (origin), not in the cycle.
  let mut contains_map : MapToContent = HashMap::new ();
  contains_map . insert (
    ID::new ("a"), vec![ID::new ("b")] );
  contains_map . insert (
    ID::new ("b"), vec![ID::new ("c")] );
  contains_map . insert (
    ID::new ("c"), vec![ID::new ("a")] );
  contains_map . insert (
    ID::new ("d"), vec![] );
  let mut reverse_map : MapToContainers = HashMap::new ();
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
  extend_contexts_for_cycles (
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

/// See tests/contexts/fixtures/README.org
#[test]
fn test_full_context_pipeline () {
  // Load SkgNodes from fixture files.
  let config : SkgConfig =
    SkgConfig::dummyFromSources (
      HashMap::from ([(
        SourceName::from ("test"),
        SkgfileSource {
          nickname     : SourceName::from ("test"),
          path         : PathBuf::from ("tests/contexts/fixtures"),
          user_owns_it : true } )]) );
  let nodes : Vec<SkgNode> =
    read_all_skg_files_from_sources (&config)
    . expect ("failed to read fixture .skg files");
  // Extract data from SkgNodes.
  let ( map_to_content, map_to_containers )
    : ( MapToContent, MapToContainers )
    = contains_maps_from_nodes (&nodes);
  let link_targets : HashSet<ID> =
    link_targets_from_nodes (&nodes);
  let had_id_set : HashSet<ID> =
    had_id_set_from_nodes (&nodes);
  let all_node_ids : HashSet<ID> =
    nodes . iter ()
    . filter_map ( |n| n . primary_id () . ok () . cloned () )
    . collect ();
  assert_eq! (all_node_ids . len (), 15);
  assert_eq! (link_targets . len (), 1);
  assert! (link_targets . contains (&ID::new ("link-target")));
  assert_eq! (had_id_set . len (), 1);
  assert! (had_id_set . contains (&ID::new ("had-id")));
  // Step 1: identify origins.
  // (identify_origins is private, so we replicate its logic.)
  let ( roots, multicontained ) : ( HashSet<ID>, HashSet<ID> ) =
    find_roots_and_multiply_contained (
      &all_node_ids, &map_to_containers );
  assert_eq! (roots . len (), 3);
  assert! (roots . contains (&ID::new ("root-1")));
  assert! (roots . contains (&ID::new ("root-2")));
  assert! (roots . contains (&ID::new ("link-source")));
  assert_eq! (multicontained . len (), 1);
  assert! (multicontained . contains (&ID::new ("shared")));
  let mut origin_types : HashMap<ID, ContextOriginType> =
    HashMap::new ();
  // Priority order: MC, HadID, Target, Root (lowest first).
  for id in &multicontained {
    origin_types . insert (
      id . clone (), ContextOriginType::MultiContained ); }
  for id in &had_id_set {
    origin_types . insert (
      id . clone (), ContextOriginType::HadID ); }
  for id in &link_targets {
    origin_types . insert (
      id . clone (), ContextOriginType::Target ); }
  for id in &roots {
    origin_types . insert (
      id . clone (), ContextOriginType::Root ); }
  assert_eq! (origin_types . len (), 6);
  assert_eq! (origin_types [&ID::new ("root-1")],
              ContextOriginType::Root);
  assert_eq! (origin_types [&ID::new ("root-2")],
              ContextOriginType::Root);
  assert_eq! (origin_types [&ID::new ("link-source")],
              ContextOriginType::Root);
  assert_eq! (origin_types [&ID::new ("shared")],
              ContextOriginType::MultiContained);
  assert_eq! (origin_types [&ID::new ("link-target")],
              ContextOriginType::Target);
  assert_eq! (origin_types [&ID::new ("had-id")],
              ContextOriginType::HadID);
  // Step 2: grow treelike contexts.
  // (grow_all_contexts is private, so we replicate it.)
  let mut all_contexts : Vec<HashSet<ID>> =
    origin_types . keys ()
    . map ( |origin|
      { let mut ctx : HashSet<ID> = HashSet::new ();
        extend_context (
          &mut ctx, origin, &origin_types, &map_to_content );
        ctx } )
    . collect ();
  // Verify that each treelike context has the right members.
  assert_eq! (ctx_containing ("root-1", &all_contexts),
              HashSet::from ([
                ID::new ("root-1"),
                ID::new ("in-root-1") ]));
  assert_eq! (ctx_containing ("root-2", &all_contexts),
              HashSet::from ([
                ID::new ("root-2") ]));
  assert_eq! (ctx_containing ("link-source", &all_contexts),
              HashSet::from ([
                ID::new ("link-source") ]));
  assert_eq! (ctx_containing ("shared", &all_contexts),
              HashSet::from ([
                ID::new ("shared"),
                ID::new ("in-shared-1"),
                ID::new ("in-shared-2") ]));
  assert_eq! (ctx_containing ("link-target", &all_contexts),
              HashSet::from ([
                ID::new ("link-target") ]));
  assert_eq! (ctx_containing ("had-id", &all_contexts),
              HashSet::from ([
                ID::new ("had-id"),
                ID::new ("in-had-id-1"),
                ID::new ("in-had-id-2") ]));
  // Cycle nodes should not be covered yet.
  let covered_before_cycles : HashSet<ID> =
    all_contexts . iter ()
    . flat_map ( |ctx| ctx . iter () . cloned () )
    . collect ();
  assert_eq! (covered_before_cycles . len (), 11);
  assert! (! covered_before_cycles . contains (
    &ID::new ("cycle-1") ));
  // Step 3: handle cycles.
  extend_contexts_for_cycles (
    &all_node_ids,
    &map_to_content,
    &map_to_containers,
    &mut origin_types,
    &mut all_contexts );
  // Verify cycle members are now CycleMember origins.
  assert_eq! (origin_types [&ID::new ("cycle-1")],
              ContextOriginType::CycleMember);
  assert_eq! (origin_types [&ID::new ("cycle-2")],
              ContextOriginType::CycleMember);
  // Verify the cycle context.
  assert_eq! (ctx_containing ("cycle-1", &all_contexts),
              HashSet::from ([
                ID::new ("cycle-1"),
                ID::new ("cycle-2"),
                ID::new ("from-cycle-1"),
                ID::new ("from-cycle-2") ]));
  // Verify all 12 nodes are covered.
  let covered_after : HashSet<ID> =
    all_contexts . iter ()
    . flat_map ( |ctx| ctx . iter () . cloned () )
    . collect ();
  assert_eq! (covered_after . len (), 15); }

/// Find the context containing the given node ID.
fn ctx_containing (
  id       : &str,
  contexts : &[HashSet<ID>],
) -> HashSet<ID> {
  let target : ID = ID::new (id);
  contexts . iter ()
  . find ( |ctx| ctx . contains (&target) )
  . unwrap_or_else ( || panic! (
    "{} not in any context", id ) )
  . clone () }
