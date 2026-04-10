/// PURPOSE: Compute "context origin types" for nodes in the graph.
/// A context is a region of the containment graph,
/// consisting of 'origins' (usually a single rootlike node,
/// but maybe a cycle) and potentially a lot of other nodes
/// in the recursive content of the origin(s).
/// Origins (roots, link targets, multiply-contained nodes, cycle members,
/// nodes with Had_ID_Before_Import) get score multipliers at search time.

use crate::dbs::tantivy::{subset_with_hadid, update_context_origin_types};
use crate::dbs::typedb::paths::containerward_path_stats_bulk;
use crate::dbs::typedb::search::find_container_ids_of_pid;
use crate::dbs::typedb::util::ConceptRowStream;
use crate::types::misc::{ID, TantivyIndex};
use crate::types::save::{DefineNode, SaveNode};
use crate::types::skgnode::{FileProperty, SkgNode};
use crate::types::textlinks::textlinks_from_node;
use crate::types::viewnode::ContainerwardPathStats;

use futures::stream::{self, StreamExt};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use typedb_driver as tdbd;

//
// Types
//

// This enum's constructors are listed in priority order.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ContextOriginType {
  Root,
  CycleMember, // A treelike set might have no single root, but rather a rootlike cycle. In that case every member of the cycle should be almost as prominent in search results as a true root. The multipliers reflect that.
  Target,
  HadID, // If something had an org-roam ID when imported, even if it was not linked to, it was probably considered important enough at some point to be worth linking to.
  MultiContained,
}

impl ContextOriginType {
  pub fn label ( &self, ) -> &'static str {
    match self {
      ContextOriginType::Root           => "Root",
      ContextOriginType::CycleMember    => "CycleMember",
      ContextOriginType::Target         => "Target",
      ContextOriginType::HadID          => "HadID",
      ContextOriginType::MultiContained => "MultiContained", } }
  pub fn from_label ( label : &str, )
                      -> Option<ContextOriginType> {
    match label {
      "Root"           => Some (ContextOriginType::Root),
      "CycleMember"    => Some (ContextOriginType::CycleMember),
      "Target"         => Some (ContextOriginType::Target),
      "HadID"          => Some (ContextOriginType::HadID),
      "MultiContained" => Some (ContextOriginType::MultiContained),
      _                => None, } }
  pub fn multiplier ( &self, ) -> f32 {
    use crate::consts::*;
    match self {
      ContextOriginType::Root           => MULTIPLIER_ROOT,
      ContextOriginType::CycleMember    => MULTIPLIER_CYCLE_MEMBER,
      ContextOriginType::Target         => MULTIPLIER_TARGET,
      ContextOriginType::HadID          => MULTIPLIER_HAD_ID,
      ContextOriginType::MultiContained => MULTIPLIER_MULTI_CONTAINED, }} }

/// Maps each node to the nodes it contains.
pub type MapToContent = HashMap<ID, Vec<ID>>;
/// Maps each node to the nodes containing it.
pub type MapToContainers = HashMap<ID, Vec<ID>>;

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
  tantivy_index : &TantivyIndex,
  had_id_set    : &HashSet<ID>,
  all_node_ids  : &HashSet<ID>,
  link_targets  : &HashSet<ID>,
  map_to_content  : &MapToContent,
  map_to_containers   : &MapToContainers,
) -> Result<HashMap<ID, String>, Box<dyn Error>> {
  tracing::info! ("Computing context origin types...");
  let edge_count : usize =
    map_to_content . values () . map ( |v| v . len () ) . sum ();
  tracing::info! ("  {} nodes, {} edges, {} link targets.",
            all_node_ids . len (), edge_count, link_targets . len ());
  let mut origin_types : HashMap<ID, ContextOriginType> =
    identify_origins (
      all_node_ids, map_to_containers, link_targets, had_id_set );
  tracing::info! ("  {} origins identified.", origin_types . len ());
  let mut all_contexts : Vec<HashSet<ID>> =
    grow_all_contexts (&origin_types, map_to_content);
  tracing::info! ("  {} treelike contexts grown.", all_contexts . len ());
  extend_contexts_for_cycles (
    all_node_ids,
    map_to_content,
    map_to_containers,
    &mut origin_types,
    &mut all_contexts );
  tracing::info! ("  {} total contexts after cycle detection.",
            all_contexts . len ());
  let context_types_by_id : HashMap<ID, String> =
    // Converts ContextOriginType too String for Tantivy.
    origin_types . iter ()
    . map ( |(id, ct)| (id . clone (),
                        ct . label () . to_string ()) )
    . collect ();
  let updated : usize = // update Tantivy
    update_context_origin_types (
      tantivy_index, &context_types_by_id ) ?;
  tracing::info! ("  {} Tantivy documents updated with context types.",
            updated);
  Ok (context_types_by_id) }

/// Recompute context origin types
/// (used to rank search results) for saved nodes,
pub async fn update_context_types_for_saved_nodes (
  tantivy_index : &TantivyIndex,
  db_name       : &str,
  driver        : &tdbd::TypeDBDriver,
  instructions  : &[DefineNode],
) -> Result<(), Box<dyn Error>> {
  let saved_ids : HashSet<ID> =
    instructions . iter ()
    . filter_map ( |instr| match instr {
      DefineNode::Save (SaveNode (node)) =>
        Some (node . pid . clone ()),
      DefineNode::Delete (_) => None } )
    . collect ();
  if saved_ids . is_empty () { return Ok (( )); }

  // Build the inputs that `identify_origins` expects,
  // using TypeDB for graph structure and Tantivy for had_id.
  let map_to_containers : MapToContainers =
    map_to_containers_from_typedb (
      db_name, driver, &saved_ids ) . await ?;
  let link_targets : HashSet<ID> =
    link_targets_from_typedb (
      db_name, driver, &saved_ids ) . await ?;
  let hadid_set : HashSet<ID> =
    subset_with_hadid (tantivy_index, &saved_ids);
  let mut origin_types : HashMap<ID, ContextOriginType> =
    identify_origins (
      &saved_ids, &map_to_containers, &link_targets, &hadid_set );

  { // identify_origins does not handle cycles. (The init path uses extend_contexts_for_cycles, which covers the whole graph and hence won't work here). For nodes that still have no origin type and are singly contained, check for cycles via the existing bulk BFS in paths.rs.
    let ids_to_check : Vec<ID> =
      saved_ids . iter ()
      . filter ( |id| ! origin_types . contains_key (id) )
      . cloned () . collect ();
    let path_stats : HashMap<ID, ContainerwardPathStats> =
      containerward_path_stats_bulk (
        db_name, driver, &ids_to_check ) . await ?;
    for (id, stats) in &path_stats {
      if stats . cycles {
        origin_types . insert (
          id . clone (), ContextOriginType::CycleMember ); }} }
  let origin_types_typedb_can_read : HashMap<ID, String> =
    // convert each ContextOriginType to String
    origin_types . iter ()
    . map ( |(id, ct)| (id . clone (),
                        ct . label () . to_string () ))
    . collect ();
  if ! origin_types_typedb_can_read . is_empty () {
    update_context_origin_types (
      tantivy_index, &origin_types_typedb_can_read ) ?; }
  Ok (( )) }

/// Build a content-to-containers map for a set of node IDs
/// by querying TypeDB in parallel.
async fn map_to_containers_from_typedb (
  db_name : &str,
  driver  : &tdbd::TypeDBDriver,
  ids     : &HashSet<ID>,
) -> Result<MapToContainers, Box<dyn Error>> {
  let results : Vec<Result<(ID, HashSet<ID>),
                           Box<dyn Error>>> =
    stream::iter ( ids . iter () . map ( |id| {
      let id : ID = id . clone ();
      async move {
        let containers : HashSet<ID> =
          find_container_ids_of_pid (
            db_name, driver, &id ) . await ?;
        Ok ((id, containers)) }} ))
    . buffer_unordered (
      crate::consts::TYPEDB_CONCURRENT_TRANSACTIONS )
    . collect () . await;
  let mut map_to_containers : MapToContainers =
    HashMap::new ();
  for r in results {
    let (id, containers) : (ID, HashSet<ID>) = r ?;
    if ! containers . is_empty () {
      map_to_containers . insert (
        id, containers . into_iter () . collect () ); }}
  Ok (map_to_containers) }

/// Returns a subset of the input IDs
/// that are textlink targets, querying TypeDB in parallel.
async fn link_targets_from_typedb (
  db_name : &str,
  driver  : &tdbd::TypeDBDriver,
  ids     : &HashSet<ID>,
) -> Result<HashSet<ID>, Box<dyn Error>> {
  let results : Vec<Result<(ID, bool),
                           Box<dyn Error>>> =
    stream::iter ( ids . iter () . map ( |id| {
      let id : ID = id . clone ();
      async move {
        let hit : bool =
          is_textlink_target (
            db_name, driver, &id ) . await ?;
        Ok ((id, hit)) }} ))
    . buffer_unordered (
      crate::consts::TYPEDB_CONCURRENT_TRANSACTIONS )
    . collect () . await;
  let mut targets : HashSet<ID> = HashSet::new ();
  for r in results {
    let (id, hit) : (ID, bool) = r ?;
    if hit { targets . insert (id); }}
  Ok (targets) }

async fn is_textlink_target (
  db_name : &str,
  driver  : &tdbd::TypeDBDriver,
  pid     : &ID,
) -> Result<bool, Box<dyn Error>> {
  use futures::StreamExt;
  let tx : tdbd::Transaction =
    driver . transaction (
      db_name, tdbd::TransactionType::Read
    ) . await ?;
  let answer : tdbd::answer::QueryAnswer =
    tx . query ( format! (
      r#"match
         $dest isa node, has id "{}";
         $rel isa textlinks_to ( source: $src,
                                 dest:   $dest );
         select $rel; limit 1;"#,
      pid ) ) . await ?;
  let mut stream : ConceptRowStream =
    answer . into_rows ();
  let has_any : bool =
    stream . next () . await . is_some ();
  Ok (has_any) }

//
// Step 1: identify origins (in-memory)
//

/// Build origin_types map from in-memory data + one TypeDB query result.
/// We impose priority order: If something is a Root,
/// it doesn't matter that it's a Target, etc.
/// Therefore higher-priority origin types are processed later.
/// CycleMember is assigned later (step 3).
/// (That's safe because the only higher-priority thing is a Root,
/// and a Root cannot be a CycleMember.)
fn identify_origins (
  all_node_ids : &HashSet<ID>,
  map_to_containers  : &MapToContainers,
  targets      : &HashSet<ID>,
  had_id_set   : &HashSet<ID>,
) -> HashMap<ID, ContextOriginType> {
  let ( roots, multicontained ) : ( HashSet<ID>, HashSet<ID> ) =
    find_roots_and_multiply_contained (all_node_ids, map_to_containers);
  let mut origin_types : HashMap<ID, ContextOriginType> =
    HashMap::new ();
  { // Important: Start at least priority, work up to highest.
    for id in &multicontained {
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
        id . clone (), ContextOriginType::Root ); }}
  origin_types }

/// Partition nodes into roots and multiply-contained
/// based on how many distinct containers each has:
/// Roots have none, multiply-contained have more than one.
pub fn find_roots_and_multiply_contained (
  all_node_ids : &HashSet<ID>,
  map_to_containers  : &MapToContainers,
) -> ( HashSet<ID>, HashSet<ID> ) {
  let mut roots : HashSet<ID> = HashSet::new();
  let mut multi : HashSet<ID> = HashSet::new();
  for id in all_node_ids {
    match map_to_containers . get (id) {
      None => { roots . insert (id . clone()); },
      Some (containers) => {
        let unique_containers : HashSet<&ID> =
          containers . iter () . collect ();
        if unique_containers . len () > 1 {
          multi . insert (id . clone() ); }}, }}
  ( roots, multi ) }

//
// Step 2: grow treelike contexts (in-memory)
//

/// Grow contexts from all origins using the in-memory map.
fn grow_all_contexts (
  origin_types : &HashMap<ID, ContextOriginType>,
  map_to_content : &MapToContent,
) -> Vec<HashSet<ID>> {
  origin_types . keys ()
    . map ( |origin|
      { let mut ctx : HashSet<ID> = HashSet::new ();
        extend_context (
          &mut ctx, origin, origin_types, map_to_content );
        ctx } )
    . collect () }

//
// Step 3: handle cycles (in-memory)
//

/// The treelike contexts, grown first, might not cover all nodes.
/// Stragglers are in, or recursively contained in, containment cycles.
/// For each connected component of uncovered nodes,
/// this will climb containerward to find a cycle,
/// mark cycle members as origins, and grow their tails.
pub fn extend_contexts_for_cycles (
  all_node_ids      : &HashSet<ID>,
  map_to_content    : &MapToContent,
  map_to_containers : &MapToContainers,
  origins  : &mut HashMap<ID, ContextOriginType>, // we grow this
  contexts : &mut Vec<HashSet<ID>>,               // we grow this
) {
  let covered : HashSet<ID> = // nodes already assigned a context
    contexts . iter ()
    . flat_map ( |ctx| ctx . iter () . cloned () )
    . collect ();
  let mut uncovered : HashSet<ID> =
    all_node_ids . difference (&covered) . cloned () . collect ();
  while ! uncovered . is_empty () { // consume some of 'uncovered'
    let start : ID = // picks a random node
      uncovered . iter () . next () . unwrap () . clone ();
    let ( _path, cycle_members ) : ( Vec<ID>, HashSet<ID> ) =
      climb_containerward_to_cycle (&start, map_to_containers);
    for cm in &cycle_members {
      if origins . get (cm) != Some (&ContextOriginType::Root) {
        // This should never execute: We already handled Roots and MultiContained, so each cycle member should be contained exactly once.
        // But to be safe, we explicitly avoid clobbering Root with CycleMember. (Recall that in priority, Root > Cycle > others.)
        origins . insert ( cm . clone (),
                           ContextOriginType::CycleMember ); }}
    let mut ctx : HashSet<ID> = HashSet::new ();
    for cm in &cycle_members {
      extend_context (
        &mut ctx, cm, origins, map_to_content ); }
    for member in &ctx { // 'path' is a subset of 'ctx'
      uncovered . remove (member); }
    contexts . push (ctx); }}

//
// Shared callee of steps 2 and 3
//

/// Grow a context contentward from `grow_from`, merging results
/// into `context`. Used for both single-origin contexts (where
/// `context` starts empty) and cycle contexts (where multiple
/// cycle members each extend the same context).
/// Growth is truncated without inclusion
/// wherever it encounters another context origin.
pub fn extend_context (
  context      : &mut HashSet<ID>,
  grow_from    : &ID,
  origins      : &HashMap<ID, ContextOriginType>,
  map_to_content : &MapToContent,
) {
  context . insert (grow_from . clone ());
  let mut frontier : Vec<ID> = vec![grow_from . clone ()];
  while ! frontier . is_empty () {
    let mut next_frontier : Vec<ID> = Vec::new ();
    for pid in &frontier {
      if let Some (children) = map_to_content . get (pid) {
        for child in children {
          if origins . contains_key (child) && child != grow_from { // The origin of a different context. Don't include it, and don't recurse into it.
            continue; }
          if context . contains (child) { // Already visited. Do not recurse into it.
            continue; }
          context . insert (child . clone ());
          next_frontier . push (child . clone ()); }} }
    frontier = next_frontier; } }

/// Climb containerward from `start` until revisiting a node.
/// Returns the full path walked and the set of cycle members
/// (empty if a degenerate dead end is reached).
fn climb_containerward_to_cycle (
  start             : &ID,
  map_to_containers : &MapToContainers,
) -> ( Vec<ID>, HashSet<ID> ) {
  let mut path : Vec<ID> = vec![ start . clone () ];
  let mut path_set : HashSet<ID> =
    HashSet::from ( [ start . clone () ] );
  let mut current : ID = start . clone ();
  loop {
    let containers : &[ID] =
      map_to_containers . get (&current)
      . map ( |v| v . as_slice () )
      . unwrap_or (&[]);
    if containers . len () != 1 { // Should not happen, because roots and multiply-contained nodes were already handled.
      return ( path, HashSet::from ( [current . clone () ] )); }
    let container : &ID = &containers[0];
    if path_set . contains (container) {
      let cycle_start_idx : usize =
        path . iter ()
        . position ( // returns the first match
          |id| id == container )
        . unwrap ();
      let cycle_members : HashSet<ID> =
        path [cycle_start_idx ..] . iter ()
        . cloned () . collect ();
      return ( path, cycle_members );
    } else {
      path . push (container . clone ());
      path_set . insert (container . clone ());
      current = container . clone (); } } }

//
// Standalone public helpers (called from outside this module)
//

/// Build (map-to-content, map-to-containers) maps
/// from loaded SkgNodes.
/// This avoids a ~22s TypeDB query on 28k-node datasets.
pub fn contains_maps_from_nodes (
  nodes : &[SkgNode],
) -> (MapToContent, MapToContainers) {
  let mut to_content    : MapToContent    = HashMap::new ();
  let mut to_containers : MapToContainers = HashMap::new ();
  for node in nodes {
    { let pid : &ID = &node . pid;
      for child_id in &node . contains {
        to_content . entry (pid . clone ())
          . or_insert_with (Vec::new)
          . push (child_id . clone ());
        to_containers . entry (child_id . clone ())
          . or_insert_with (Vec::new)
          . push (pid . clone ()); } }}
  ( to_content, to_containers ) }

/// Collect all link target IDs from titles and bodies.
/// (The previous implementation, which queried TypeDB,
/// took ~2.5 seconds for 28k nodes.)
pub fn link_targets_from_nodes (
  nodes : &[SkgNode],
) -> HashSet<ID> {
  nodes . iter ()
  . flat_map ( |node| {
    textlinks_from_node (node)
    . into_iter ()
    . map ( |tl| tl . id ) } )
  . collect () }

/// Collect the set of IDs of nodes whose misc field
/// contains Had_ID_Before_Import.
pub fn had_id_set_from_nodes (
  nodes : &[SkgNode],
) -> HashSet<ID> {
  nodes . iter ()
  . filter ( |n|
    n . misc . contains (&FileProperty::Had_ID_Before_Import) )
  . map ( |n| n . pid . clone () )
  . collect () }
