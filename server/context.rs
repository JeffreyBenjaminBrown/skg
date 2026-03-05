// PURPOSE: Compute "context origin types" for nodes in the graph.
// A context is a structurally significant region of the containment graph.
// Origins (roots, link targets, multiply-contained nodes, cycle members,
// nodes with Had_ID_Before_Import) get score multipliers at search time.

use crate::dbs::tantivy::update_context_origin_types;
use crate::types::misc::{ID, TantivyIndex};
use crate::types::skgnode::{FileProperty, SkgNode};
use crate::types::textlinks::textlinks_from_node;

use std::collections::{HashMap, HashSet};
use std::error::Error;

//
// Types
//

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ContextOriginType {
  Root,
  CycleMember,
  Target,
  HadID,
  MultiContained,
}

impl ContextOriginType {
  pub fn label (
    &self,
  ) -> &'static str {
    match self {
      ContextOriginType::Root           => "Root",
      ContextOriginType::CycleMember    => "CycleMember",
      ContextOriginType::Target         => "Target",
      ContextOriginType::HadID          => "HadID",
      ContextOriginType::MultiContained => "MultiContained", } }
  pub fn multiplier (
    &self,
  ) -> f32 {
    match self {
      ContextOriginType::Root           => 100.0,
      ContextOriginType::CycleMember    =>  30.0,
      ContextOriginType::Target         =>  10.0,
      ContextOriginType::HadID          =>  10.0,
      ContextOriginType::MultiContained =>   3.0, } } }

pub fn multiplier_for_label (
  label : &str,
) -> f32 {
  match label {
    "Root"           => 100.0,
    "CycleMember"    =>  30.0,
    "Target"         =>  10.0,
    "HadID"          =>  10.0,
    "MultiContained" =>   3.0,
    _                =>   1.0, } }

//
// In-memory data extraction from SkgNodes
//

/// Collect all link target IDs from the text of all nodes.
/// Parses [[id:...][...]] patterns from titles and bodies,
/// eliminating the TypeDB link_targets query (~2.5s on 28k nodes).
pub fn link_targets_from_nodes (
  nodes : &[SkgNode],
) -> HashSet<ID> {
  nodes . iter ()
  . flat_map ( |node| {
    textlinks_from_node (node)
    . into_iter ()
    . map ( |tl| tl . id ) } )
  . collect () }

//
// In-memory origin identification
// Roots and multiply-contained are derived from the contains maps,
// eliminating two TypeDB queries (~17s saved on 28k-node dataset).
//

/// Derive roots from all_node_ids and reverse_map.
/// A root is a node that never appears as contained.
fn find_roots_from_maps (
  all_node_ids : &HashSet<ID>,
  reverse_map  : &ReverseContainsMap,
) -> HashSet<ID> {
  all_node_ids . iter ()
  . filter ( |id| ! reverse_map . contains_key (id) )
  . cloned ()
  . collect () }

/// Derive multiply-contained nodes from reverse_map.
/// A node is multiply-contained if it has >1 distinct container.
fn find_multiply_contained_from_maps (
  reverse_map : &ReverseContainsMap,
) -> HashSet<ID> {
  reverse_map . iter ()
  . filter ( |(_, containers)| {
    // Deduplicate containers (in case of duplicate edges).
    let unique : HashSet<&ID> =
      containers . iter () . collect ();
    unique . len () > 1 } )
  . map ( |(id, _)| id . clone () )
  . collect () }

/// Build origin_types map from in-memory data + one TypeDB query result.
/// Priority order: Root > CycleMember > Target = HadID > MultiContained.
/// CycleMember is assigned later (step 3).
fn identify_origins (
  all_node_ids : &HashSet<ID>,
  reverse_map  : &ReverseContainsMap,
  targets      : &HashSet<ID>,
  had_id_set   : &HashSet<ID>,
) -> HashMap<ID, ContextOriginType> {
  let roots : HashSet<ID> =
    find_roots_from_maps (all_node_ids, reverse_map);
  let multi : HashSet<ID> =
    find_multiply_contained_from_maps (reverse_map);
  // Start from lowest priority and overwrite with higher.
  let mut origin_types : HashMap<ID, ContextOriginType> =
    HashMap::new ();
  for id in &multi {
    origin_types . insert (
      id . clone (), ContextOriginType::MultiContained ); }
  for id in had_id_set {
    origin_types . insert (
      id . clone (), ContextOriginType::HadID ); }
  for id in targets {
    origin_types . insert (
      id . clone (), ContextOriginType::Target ); }
  for id in &roots {
    origin_types . insert (
      id . clone (), ContextOriginType::Root ); }
  origin_types }

//
// Contains maps: built from SkgNodes or loaded from TypeDB
//

/// Container → contained mapping.
pub type ContainsMap = HashMap<ID, Vec<ID>>;
/// Contained → container mapping (reverse of ContainsMap).
pub type ReverseContainsMap = HashMap<ID, Vec<ID>>;

/// Build (forward, reverse) contains maps from loaded SkgNodes.
/// This avoids a ~22s TypeDB query on 28k-node datasets.
pub fn contains_maps_from_nodes (
  nodes : &[SkgNode],
) -> (ContainsMap, ReverseContainsMap) {
  let mut forward : ContainsMap = HashMap::new ();
  let mut reverse : ReverseContainsMap = HashMap::new ();
  for node in nodes {
    if let Ok (pid) = node . primary_id () {
      if let Some (children) = &node . contains {
        for child_id in children {
          forward . entry (pid . clone ())
            . or_insert_with (Vec::new)
            . push (child_id . clone ());
          reverse . entry (child_id . clone ())
            . or_insert_with (Vec::new)
            . push (pid . clone ()); } } } }
  ( forward, reverse ) }

//
// Step 2: grow treelike contexts (in-memory)
//

/// Grow one context contentward from an origin using the in-memory map.
/// Returns the set of member IDs (including the origin itself).
fn grow_context_from_origin (
  origin       : &ID,
  origins      : &HashSet<ID>,
  contains_map : &ContainsMap,
) -> HashSet<ID> {
  let mut members : HashSet<ID> = HashSet::new ();
  members . insert (origin . clone ());
  let mut frontier : Vec<ID> = vec![origin . clone ()];
  while ! frontier . is_empty () {
    let mut next_frontier : Vec<ID> = Vec::new ();
    for pid in &frontier {
      if let Some (children) = contains_map . get (pid) {
        for child in children {
          // Truncate at other origins.
          if origins . contains (child)
             && child != origin {
            continue; }
          if members . contains (child) {
            continue; }
          members . insert (child . clone ());
          next_frontier . push (child . clone ()); } } }
    frontier = next_frontier; }
  members }

/// Grow contexts from all origins using the in-memory map.
fn grow_all_contexts (
  origin_types : &HashMap<ID, ContextOriginType>,
  contains_map : &ContainsMap,
) -> Vec<HashSet<ID>> {
  let origins_set : HashSet<ID> =
    origin_types . keys () . cloned () . collect ();
  origin_types . keys ()
  . map ( |origin|
    grow_context_from_origin (
      origin, &origins_set, contains_map ) )
  . collect () }

//
// Step 3: handle cycles (in-memory)
//

/// After growing treelike contexts, find any nodes not covered
/// (these are in or adjacent to containment cycles).
/// For each connected component, detect the cycle,
/// mark cycle members as origins, and grow their tails.
fn handle_cycles (
  all_node_ids  : &HashSet<ID>,
  contains_map  : &ContainsMap,
  reverse_map   : &ReverseContainsMap,
  origin_types  : &mut HashMap<ID, ContextOriginType>,
  all_contexts  : &mut Vec<HashSet<ID>>,
) {
  let covered : HashSet<ID> =
    all_contexts . iter ()
    . flat_map ( |ctx| ctx . iter () . cloned () )
    . collect ();
  let remainder : HashSet<ID> =
    all_node_ids . difference (&covered) . cloned () . collect ();
  if remainder . is_empty () { return; }
  let mut unvisited : HashSet<ID> = remainder . clone ();
  while ! unvisited . is_empty () {
    let start : ID =
      unvisited . iter () . next () . unwrap () . clone ();
    // Climb containerward from start until revisiting a node.
    let mut path : Vec<ID> = vec![start . clone ()];
    let mut path_set : HashSet<ID> =
      HashSet::from ([start . clone ()]);
    let mut current : ID = start . clone ();
    let cycle_members : HashSet<ID>;
    loop {
      let containers : &[ID] =
        reverse_map . get (&current)
        . map ( |v| v . as_slice () )
        . unwrap_or (&[]);
      if containers . is_empty () || containers . len () > 1 {
        // This shouldn't happen for true remainder nodes
        // (they have exactly 1 container, not a root, not multiply contained).
        // If it does, treat the whole path as a degenerate context.
        cycle_members = HashSet::new ();
        break; }
      let container : &ID = &containers[0];
      if path_set . contains (container) {
        // Found a cycle. The cycle is from the first occurrence
        // of container to the end of path.
        let cycle_start_idx : usize =
          path . iter ()
          . position ( |id| id == container )
          . unwrap ();
        cycle_members =
          path[cycle_start_idx ..] . iter ()
          . cloned () . collect ();
        break;
      } else {
        path . push (container . clone ());
        path_set . insert (container . clone ());
        current = container . clone (); } }
    // Mark cycle members as origins.
    for cm in &cycle_members {
      if ! origin_types . contains_key (cm) {
        origin_types . insert (
          cm . clone (), ContextOriginType::CycleMember ); } }
    // Grow contentward from cycle members (same truncation rules).
    let origins_set : HashSet<ID> =
      origin_types . keys () . cloned () . collect ();
    for cm in &cycle_members {
      let ctx : HashSet<ID> =
        grow_context_from_origin (
          cm, &origins_set, contains_map );
      for member in &ctx {
        unvisited . remove (member); }
      all_contexts . push (ctx); }
    // Also remove the path nodes (tail leading to the cycle).
    for node in &path {
      unvisited . remove (node); }
    // If cycle_members was empty (degenerate case),
    // still remove path from unvisited.
    if cycle_members . is_empty () {
      for node in &path {
        unvisited . remove (node); } } } }

//
// Top-level: compute all context origin types and store in Tantivy
//

/// Compute context origin types for all nodes and update Tantivy.
/// Returns the map from node ID to context origin type label.
///
/// Fully in-memory: all data is pre-computed from SkgNodes at init.
/// No TypeDB queries, no async. On a 28k-node dataset this is
/// near-instantaneous (sub-second).
pub fn compute_and_store_context_types (
  tantivy_index  : &TantivyIndex,
  had_id_set     : &HashSet<ID>,
  all_node_ids   : &HashSet<ID>,
  link_targets   : &HashSet<ID>,
  contains_map   : &ContainsMap,
  reverse_map    : &ReverseContainsMap,
) -> Result<HashMap<ID, String>, Box<dyn Error>> {
  println! ("Computing context origin types...");
  let edge_count : usize =
    contains_map . values () . map ( |v| v . len () ) . sum ();
  println! ("  {} nodes, {} edges, {} link targets.",
            all_node_ids . len (), edge_count, link_targets . len ());
  // Derive origins in-memory.
  let mut origin_types : HashMap<ID, ContextOriginType> =
    identify_origins (
      all_node_ids, reverse_map, link_targets, had_id_set );
  println! ("  {} origins identified.", origin_types . len ());
  // Grow treelike contexts (in-memory).
  let mut all_contexts : Vec<HashSet<ID>> =
    grow_all_contexts (&origin_types, contains_map);
  println! ("  {} treelike contexts grown.", all_contexts . len ());
  // Handle cycles (in-memory).
  handle_cycles (
    all_node_ids,
    contains_map,
    reverse_map,
    &mut origin_types,
    &mut all_contexts );
  println! ("  {} total contexts after cycle detection.",
            all_contexts . len ());
  // Build the map from ID to label for Tantivy.
  let context_types_by_id : HashMap<ID, String> =
    origin_types . iter ()
    . map ( |(id, ct)| (id . clone (), ct . label () . to_string ()) )
    . collect ();
  // Store in Tantivy.
  let updated : usize =
    update_context_origin_types (
      tantivy_index, &context_types_by_id ) ?;
  println! ("  {} Tantivy documents updated with context types.",
            updated);
  Ok (context_types_by_id) }

/// Collect the set of IDs of nodes whose misc field
/// contains Had_ID_Before_Import.
pub fn had_id_set_from_nodes (
  nodes : &[SkgNode],
) -> HashSet<ID> {
  nodes . iter ()
  . filter ( |n|
    n . misc . contains (&FileProperty::Had_ID_Before_Import) )
  . filter_map ( |n| n . primary_id () . ok () . cloned () )
  . collect () }

//
// Tests
//

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::skgnode::empty_skgnode;

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
}
