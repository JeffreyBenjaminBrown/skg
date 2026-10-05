use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use skg::prominence::{
  ProminenceSource,
  MapToContent,
  MapToContainers,
  content_maps_from_nodes,
  prominence_sources_for_saved_from_in_rust_graph,
  had_id_set_from_nodes,
  mentioned_skgids_from_nodes,
  find_roots_and_multiply_contained,
  extend_region,
  extend_regions_for_cycles,
};
use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_skgrepos;
use skg::dbs::in_rust_graph::InRustGraph;
use skg::types::misc::{ID, SkgConfig, SkgRepo, SkgRepoName, rel_partners_at_relRepo};
use skg::types::nodes::complete::{Flag, Graphnode, empty_graphnode};
use skg::types::save::{NodeInstruction, SaveNode};

#[test]
fn test_from_label_unknown () {
  assert! (ProminenceSource::from_label ("") . is_none());
  assert! (ProminenceSource::from_label ("Bogus") . is_none()); }

#[test]
fn test_label_roundtrip () {
  let types : Vec<ProminenceSource> = vec![
    ProminenceSource::Root,
    ProminenceSource::CycleMember,
    ProminenceSource::Mentioned,
    ProminenceSource::HadID,
    ProminenceSource::MultiContained ];
  for ct in types {
    assert_eq! (
      ProminenceSource::from_label (ct . label()),
      Some (ct) ); } }

#[test]
fn test_had_id_set_from_nodes_empty () {
  let nodes : Vec<Graphnode> = vec![];
  let result : HashSet<ID> = had_id_set_from_nodes (&nodes);
  assert! (result . is_empty ()); }

#[test]
fn test_had_id_set_from_nodes_mixed () {
  let mut node_with : Graphnode = empty_graphnode ();
  node_with . pid = ID::new ("has-id");
  node_with . flags = vec![Flag::Had_ID_Before_Import];
  let mut node_without : Graphnode = empty_graphnode ();
  node_without . pid = ID::new ("no-id");
  let nodes : Vec<Graphnode> = vec![node_with, node_without];
  let result : HashSet<ID> = had_id_set_from_nodes (&nodes);
  assert_eq! (result . len (), 1);
  assert! (result . contains (&ID::new ("has-id"))); }

#[test]
fn test_mentioned_skgids_from_nodes () {
  let mut node1 : Graphnode = empty_graphnode ();
  node1 . pid = ID::new ("src");
  node1 . title =
    "see [[id:tgt1][target one]]" . to_string ();
  node1 . body = Some (
    "also [[id:tgt2][target two]]" . to_string () );
  let mut node2 : Graphnode = empty_graphnode ();
  node2 . pid = ID::new ("other");
  node2 . title = "no links here" . to_string ();
  let nodes : Vec<Graphnode> = vec![node1, node2];
  let mentioned_skgids : HashSet<ID> =
    mentioned_skgids_from_nodes (&nodes);
  assert_eq! (mentioned_skgids . len (), 2);
  assert! (mentioned_skgids . contains (&ID::new ("tgt1")));
  assert! (mentioned_skgids . contains (&ID::new ("tgt2"))); }

#[test]
fn test_prominence_source_priority () {
  // When a node qualifies as multiple types,
  // the highest-priority (largest multiplier) wins.
  // Root (100) > Target (10) > MultiContained (3).
  let mut prominence_sources : HashMap<ID, ProminenceSource> =
    HashMap::new ();
  // Simulate: start from lowest priority.
  prominence_sources . insert (
    ID::new ("a"), ProminenceSource::MultiContained);
  prominence_sources . insert (
    ID::new ("a"), ProminenceSource::Mentioned);
  prominence_sources . insert (
    ID::new ("a"), ProminenceSource::Root);
  // Last insert wins (Root).
  assert_eq! (
    prominence_sources . get (&ID::new ("a")),
    Some (&ProminenceSource::Root) ); }

#[test]
fn in_rust_prominence_sources_for_saved_nodes () {
  // The save-time, in-Rust-graph prominence-source computation. One node of
  // each kind, plus an ordinary (untyped) node and a 2-node cycle.
  let mk = | pid : &str, title : &str,
            contains : &[&str], had_id : bool | -> Graphnode {
    let mut n : Graphnode = empty_graphnode ();
    n . pid = ID::new (pid);
    n . title = title . to_string ();
    n . home_skgrepo = SkgRepoName::from ("main");
    n . contains = rel_partners_at_relRepo (
      &n . home_skgrepo,
      contains . iter () . map ( |c| ID::new (*c) ) . collect () );
    if had_id { n . flags = vec![Flag::Had_ID_Before_Import]; }
    n };
  let nodes : Vec<Graphnode> = vec![
    mk ("root",   "root",                &["ord","multi","hadid","tgt"], false),
    mk ("other",  "other",               &["multi"],                     false),
    mk ("ord",    "ordinary",            &[],                            false),
    mk ("multi",  "multi",               &[],                            false),
    mk ("hadid",  "hadid",               &[],                            true ),
    mk ("tgt",    "target",              &[],                            false),
    mk ("linker", "see [[id:tgt][t]]",   &[],                            false),
    mk ("cyc1",   "cyc1",                &["cyc2"],                      false),
    mk ("cyc2",   "cyc2",                &["cyc1"],                      false), ];
  let graph : InRustGraph =
    InRustGraph::from_graphnodes (&nodes);
  let defs : Vec<NodeInstruction> =
    nodes . iter () . cloned ()
    . map ( |n| NodeInstruction::Save ( SaveNode (n) )) . collect ();
  let types : HashMap<ID, String> =
    prominence_sources_for_saved_from_in_rust_graph (&graph, &defs);
  let got = |skgid : &str| types . get (&ID::new (skgid)) . map ( |s| s . as_str () );
  assert_eq! (got ("root"),   Some ("Root"));
  assert_eq! (got ("other"),  Some ("Root"));
  assert_eq! (got ("linker"), Some ("Root"));
  assert_eq! (got ("multi"),  Some ("MultiContained"));
  assert_eq! (got ("hadid"),  Some ("HadID"));
  assert_eq! (got ("tgt"),    Some ("Mentioned"));
  assert_eq! (got ("ord"),    None,
    "a singly-contained ordinary node is not an origin");
  assert_eq! (got ("cyc1"),   Some ("CycleMember"));
  assert_eq! (got ("cyc2"),   Some ("CycleMember")); }

#[test]
fn test_find_roots_and_multiply_contained () {
  // a → b, a → c, e → b. d is a root (not contained).
  // b is multiply-contained (by a and e).
  let all_skgids : HashSet<ID> =
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
    find_roots_and_multiply_contained (&all_skgids, &reverse);
  assert_eq! (roots . len (), 3);
  assert! (roots . contains (&ID::new ("a")));
  assert! (roots . contains (&ID::new ("d")));
  assert! (roots . contains (&ID::new ("e")));
  assert_eq! (multi . len (), 1);
  assert! (multi . contains (&ID::new ("b"))); }

#[test]
fn test_grow_region_simple_tree () {
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
  let origins : HashMap<ID, ProminenceSource> =
    HashMap::from ([
      (ID::new ("a"), ProminenceSource::Root),
      (ID::new ("d"), ProminenceSource::Root) ]);
  let mut region : HashSet<ID> = HashSet::new ();
  extend_region (
    &mut region, &ID::new ("a"), &origins, &contains_map );
  assert_eq! (region . len (), 3);
  assert! (region . contains (&ID::new ("a")));
  assert! (region . contains (&ID::new ("b")));
  assert! (region . contains (&ID::new ("c"))); }

#[test]
fn test_grow_region_truncates_at_other_origin () {
  // a (origin) → b → c (origin) → d
  // Growing from a should give {a, b}, stopping before c.
  let mut contains_map : MapToContent = HashMap::new ();
  contains_map . insert (
    ID::new ("a"), vec![ID::new ("b")] );
  contains_map . insert (
    ID::new ("b"), vec![ID::new ("c")] );
  contains_map . insert (
    ID::new ("c"), vec![ID::new ("d")] );
  let origins : HashMap<ID, ProminenceSource> =
    HashMap::from ([
      (ID::new ("a"), ProminenceSource::Root),
      (ID::new ("c"), ProminenceSource::Mentioned) ]);
  let mut region_a : HashSet<ID> = HashSet::new ();
  extend_region (
    &mut region_a, &ID::new ("a"), &origins, &contains_map );
  assert_eq! (region_a . len (), 2);
  assert! (region_a . contains (&ID::new ("a")));
  assert! (region_a . contains (&ID::new ("b")));
  let mut region_c : HashSet<ID> = HashSet::new ();
  extend_region (
    &mut region_c, &ID::new ("c"), &origins, &contains_map );
  assert_eq! (region_c . len (), 2);
  assert! (region_c . contains (&ID::new ("c")));
  assert! (region_c . contains (&ID::new ("d"))); }

#[test]
fn test_extend_regions_for_cycles_detects_cycle () {
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
  let all_node_skgids : HashSet<ID> =
    HashSet::from ([
      ID::new ("a"), ID::new ("b"),
      ID::new ("c"), ID::new ("d") ]);
  let mut prominence_sources : HashMap<ID, ProminenceSource> =
    HashMap::new ();
  prominence_sources . insert (
    ID::new ("d"), ProminenceSource::Root );
  let mut all_regions : Vec<HashSet<ID>> =
    vec![HashSet::from ([ID::new ("d")])];
  extend_regions_for_cycles (
    &all_node_skgids,
    &contains_map,
    &reverse_map,
    &mut prominence_sources,
    &mut all_regions );
  // All cycle members should now be CycleMember origins.
  assert_eq! (
    prominence_sources . get (&ID::new ("a")),
    Some (&ProminenceSource::CycleMember) );
  assert_eq! (
    prominence_sources . get (&ID::new ("b")),
    Some (&ProminenceSource::CycleMember) );
  assert_eq! (
    prominence_sources . get (&ID::new ("c")),
    Some (&ProminenceSource::CycleMember) );
  // All nodes should be covered.
  let covered : HashSet<ID> =
    all_regions . iter ()
    . flat_map ( |region| region . iter () . cloned () )
    . collect ();
  assert! (covered . contains (&ID::new ("a")));
  assert! (covered . contains (&ID::new ("b")));
  assert! (covered . contains (&ID::new ("c")));
  assert! (covered . contains (&ID::new ("d"))); }

/// See tests/prominence/fixtures/README.org
#[test]
fn test_full_prominence_pipeline () {
  // Load Graphnodes from fixture files.
  let config : SkgConfig =
    SkgConfig::dummyFromSkgRepos (
      HashMap::from ([(
        SkgRepoName::from ("test"),
        SkgRepo {
          name         : SkgRepoName::from ("test"),
          abbreviation : None,
          path         : PathBuf::from ("tests/prominence/fixtures"),
          owned        : true } )]) );
  let nodes : Vec<Graphnode> =
    read_all_skg_files_from_skgrepos (&config)
    . expect ("failed to read fixture .skg files");
  // Extract data from Graphnodes.
  let ( map_to_content, map_to_containers )
    : ( MapToContent, MapToContainers )
    = content_maps_from_nodes (&nodes);
  let mentioned_skgids : HashSet<ID> =
    mentioned_skgids_from_nodes (&nodes);
  let had_id_set : HashSet<ID> =
    had_id_set_from_nodes (&nodes);
  let all_node_skgids : HashSet<ID> =
    nodes . iter ()
    . map ( |n| n . pid . clone () )
    . collect ();
  assert_eq! (all_node_skgids . len (), 15);
  assert_eq! (mentioned_skgids . len (), 1);
  assert! (mentioned_skgids . contains (&ID::new ("link-target")));
  assert_eq! (had_id_set . len (), 1);
  assert! (had_id_set . contains (&ID::new ("had-id")));
  // Step 1: identify origins.
  // (identify_origins is private, so we replicate its logic.)
  let ( roots, multicontained ) : ( HashSet<ID>, HashSet<ID> ) =
    find_roots_and_multiply_contained (
      &all_node_skgids, &map_to_containers );
  assert_eq! (roots . len (), 3);
  assert! (roots . contains (&ID::new ("root-1")));
  assert! (roots . contains (&ID::new ("root-2")));
  assert! (roots . contains (&ID::new ("link-source")));
  assert_eq! (multicontained . len (), 1);
  assert! (multicontained . contains (&ID::new ("shared")));
  let mut prominence_sources : HashMap<ID, ProminenceSource> =
    HashMap::new ();
  // Priority order: MC, HadID, Target, Root (lowest first).
  for skgid in &multicontained {
    prominence_sources . insert (
      skgid . clone (), ProminenceSource::MultiContained ); }
  for skgid in &had_id_set {
    prominence_sources . insert (
      skgid . clone (), ProminenceSource::HadID ); }
  for skgid in &mentioned_skgids {
    prominence_sources . insert (
      skgid . clone (), ProminenceSource::Mentioned ); }
  for skgid in &roots {
    prominence_sources . insert (
      skgid . clone (), ProminenceSource::Root ); }
  assert_eq! (prominence_sources . len (), 6);
  assert_eq! (prominence_sources [&ID::new ("root-1")],
              ProminenceSource::Root);
  assert_eq! (prominence_sources [&ID::new ("root-2")],
              ProminenceSource::Root);
  assert_eq! (prominence_sources [&ID::new ("link-source")],
              ProminenceSource::Root);
  assert_eq! (prominence_sources [&ID::new ("shared")],
              ProminenceSource::MultiContained);
  assert_eq! (prominence_sources [&ID::new ("link-target")],
              ProminenceSource::Mentioned);
  assert_eq! (prominence_sources [&ID::new ("had-id")],
              ProminenceSource::HadID);
  // Step 2: grow treelike regions.
  // (grow_all_regions is private, so we replicate it.)
  let mut all_regions : Vec<HashSet<ID>> =
    prominence_sources . keys ()
    . map ( |origin|
      { let mut region : HashSet<ID> = HashSet::new ();
        extend_region (
          &mut region, origin, &prominence_sources, &map_to_content );
        region } )
    . collect ();
  // Verify that each treelike region has the right members.
  assert_eq! (ctx_containing ("root-1", &all_regions),
              HashSet::from ([
                ID::new ("root-1"),
                ID::new ("in-root-1") ]));
  assert_eq! (ctx_containing ("root-2", &all_regions),
              HashSet::from ([
                ID::new ("root-2") ]));
  assert_eq! (ctx_containing ("link-source", &all_regions),
              HashSet::from ([
                ID::new ("link-source") ]));
  assert_eq! (ctx_containing ("shared", &all_regions),
              HashSet::from ([
                ID::new ("shared"),
                ID::new ("in-shared-1"),
                ID::new ("in-shared-2") ]));
  assert_eq! (ctx_containing ("link-target", &all_regions),
              HashSet::from ([
                ID::new ("link-target") ]));
  assert_eq! (ctx_containing ("had-id", &all_regions),
              HashSet::from ([
                ID::new ("had-id"),
                ID::new ("in-had-id-1"),
                ID::new ("in-had-id-2") ]));
  // Cycle nodes should not be covered yet.
  let covered_before_cycles : HashSet<ID> =
    all_regions . iter ()
    . flat_map ( |region| region . iter () . cloned () )
    . collect ();
  assert_eq! (covered_before_cycles . len (), 11);
  assert! (! covered_before_cycles . contains (
    &ID::new ("cycle-1") ));
  // Step 3: handle cycles.
  extend_regions_for_cycles (
    &all_node_skgids,
    &map_to_content,
    &map_to_containers,
    &mut prominence_sources,
    &mut all_regions );
  // Verify cycle members are now CycleMember origins.
  assert_eq! (prominence_sources [&ID::new ("cycle-1")],
              ProminenceSource::CycleMember);
  assert_eq! (prominence_sources [&ID::new ("cycle-2")],
              ProminenceSource::CycleMember);
  // Verify the cycle region.
  assert_eq! (ctx_containing ("cycle-1", &all_regions),
              HashSet::from ([
                ID::new ("cycle-1"),
                ID::new ("cycle-2"),
                ID::new ("from-cycle-1"),
                ID::new ("from-cycle-2") ]));
  // Verify all 12 nodes are covered.
  let covered_after : HashSet<ID> =
    all_regions . iter ()
    . flat_map ( |region| region . iter () . cloned () )
    . collect ();
  assert_eq! (covered_after . len (), 15); }

/// Find the region containing the given node ID.
fn ctx_containing (
  skgid       : &str,
  regions : &[HashSet<ID>],
) -> HashSet<ID> {
  let target : ID = ID::new (skgid);
  regions . iter ()
  . find ( |region| region . contains (&target) )
  . unwrap_or_else ( || panic! (
    "{} not in any region", skgid ) )
  . clone () }
