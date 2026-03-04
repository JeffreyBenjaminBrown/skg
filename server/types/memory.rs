use crate::dbs::filesystem::one_node::{skgnodes_from_ids, skgnode_from_pid_and_source};
use crate::types::many_to_many::ManyToMany;
use crate::types::save::{DefineNode, SaveNode};
use crate::types::skgnode::SkgNode;
use crate::types::viewnode::{ViewUri, ViewNode, ViewNodeKind};
use super::misc::{ID, SkgConfig, SourceName};
use super::phantom::source_from_disk;

use ego_tree::{Tree, NodeId, NodeRef};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use typedb_driver::TypeDBDriver;

//
// Type declarations
//

pub type SkgNodeMap = HashMap<ID, SkgNode>;

/// Persistent cross-request cache of all SkgNodes currently displayed
/// in any Emacs view. The pool is the authoritative in-memory store;
/// SkgNodeMap is the per-request transactional layer that shadows it.
/// See also: SkgNodeMap (the per-request working set).
pub struct SkgnodesInMemory {
  pub pool        : HashMap<ID, SkgNode>,
  views           : HashMap<ViewUri, ViewState>, // TODO ? OPTIMIZE:
  // The reverse lookup (PID -> views) is computed by scanning `views` via views_containing(). This is O(views) per call, fine for < 10 views. If the number of views grows large, consider a bijective map (HashMap<ID, HashSet<ViewUri>> maintained alongside this one) for O(1) reverse lookups.
  pub id_resolver : HashMap<ID, (ID, SourceName)>, // TODO ? OPTIMIZE:
  // id_resolver is not yet wired into the render pipeline (which would require threading &mut SkgnodesInMemory through render_initial_forest_bfs and complete_viewtree). If pid_and_source_from_id remains a bottleneck after pool seeding, wire id_resolver into skgnode_and_viewnode_from_id, or apply the batch-query optimization from typedb-batching.org.
  root_ids        : ManyToMany<ID, ViewUri>, // Maps every root ID (primary + extra) ↔ ViewUri. Supports many-to-many because a view can have multiple roots, and an ID could be a root in multiple views. Maintained by register_view / update_view / unregister_view.
}

/// Invariant: all forest mutations must go through register_view /
/// update_view, which maintain pids in sync with the forest.
/// Direct forest mutation would make pids stale.
pub struct ViewState {
  pub forest : Tree<ViewNode>,
  pub pids   : HashSet<ID>, // all the TrueNodes (and DeletedNodes) in the buffer
}

//
// Implementations
//

impl SkgnodesInMemory {
  pub fn new () -> Self {
    SkgnodesInMemory {
      pool        : HashMap::new (),
      views       : HashMap::new (),
      id_resolver : HashMap::new (),
      root_ids    : ManyToMany::new () }}

  pub fn viewuri_to_pids (
    &self,
    uri : &ViewUri,
  ) -> Vec<ID> {
    self . views . get (uri)
      . map ( |vs| vs . pids . iter () . cloned () . collect () )
      . unwrap_or_default () }

  pub fn viewuri_to_view (
    &self,
    uri : &ViewUri,
  ) -> Option<&Tree<ViewNode>> {
    self . views . get (uri)
      . map ( |vs| &vs . forest ) }

  pub fn view_uri_for_root_id (
    &self,
    id : &ID,
  ) -> Option<&ViewUri> {
    self . root_ids . get_right (id)
      . and_then ( |uris| uris . iter () . next () ) }

  pub fn register_view (
    &mut self,
    uri    : ViewUri,
    forest : Tree<ViewNode>,
    pids   : &[ID],
  ) { let rids : HashSet<ID> =
        root_ids_from_forest ( &forest, &self . pool );
      for rid in &rids {
        self . root_ids . insert (
          rid . clone (), uri . clone () ); }
      let state : ViewState =
        ViewState { forest,
                    pids : pids . iter () . cloned () . collect () };
      self . views . insert ( uri, state ); }

  pub fn update_view (
    &mut self,
    uri        : &ViewUri,
    new_forest : Tree<ViewNode>,
  ) { let pids : HashSet<ID> =
        new_forest . root () . descendants ()
        . filter_map ( |n| match &n . value () . kind {
          ViewNodeKind::True (t)    => Some ( t . id . clone () ),
          ViewNodeKind::Deleted (d) => Some ( d . id . clone () ),
          _ => None } )
        . collect ();
      self . root_ids . remove_right (uri);
      let rids : HashSet<ID> =
        root_ids_from_forest ( &new_forest, &self . pool );
      for rid in &rids {
        self . root_ids . insert (
          rid . clone (), uri . clone () ); }
      if let Some (vs)
        = self . views . get_mut (uri)
        { vs . forest = new_forest;
          vs . pids = pids; }
      else { self . views . insert (
               uri . clone (),
               ViewState { forest : new_forest,
                           pids } ); }
      self . gc (); }

  pub fn unregister_view (
    &mut self,
    uri : &ViewUri,
  ) { self . root_ids . remove_right (uri);
      self . views . remove (uri);
      self . gc (); }

  pub fn views_containing (
    &self,
    pid : &ID,
  ) -> Vec<ViewUri> {
    self . views . iter ()
      . filter ( |(_, vs)| vs . pids . contains (pid) )
      . map ( |(uri, _)| uri . clone () )
      . collect () }

  // SCALING NOTE: gc() is O(views * pids_per_view). For typical usage
  // (< 10 views, < 1000 nodes each) this is negligible. If it becomes
  // a bottleneck, replace with reference counting: increment on
  // register/update, decrement on update/unregister, remove at zero.
  fn gc (&mut self) {
    let referenced : HashSet<ID> =
      self . views . values ()
      . flat_map ( |vs| vs . pids . iter () . cloned () )
      . collect ();
    self . pool . retain ( |pid, _| referenced . contains (pid));
    self . id_resolver . retain (
      |_, (target_pid, _)| referenced . contains (target_pid)); }
}

//
// Functions
//

/// Collect all IDs (primary + extras) for every root TrueNode in a forest.
/// A root TrueNode is a direct child of the BufferRoot scaffold.
fn root_ids_from_forest (
  forest : &Tree<ViewNode>,
  pool   : &HashMap<ID, SkgNode>,
) -> HashSet<ID> {
  let mut ids : HashSet<ID> = HashSet::new ();
  for child in forest . root () . children () {
    if let ViewNodeKind::True (t) = &child . value () . kind {
      ids . insert ( t . id . clone () );
      if let Some (skgnode) = pool . get ( &t . id ) {
        for extra_id in &skgnode . ids {
          ids . insert ( extra_id . clone () ); }}}}
  ids }

/// Extract SkgNode for an ViewNode from the map (tried first) or disk.
/// Updates the map if needed.
/// Returns None for Scaffolds.
pub fn skgnode_for_viewnode<'a> (
  viewnode : &ViewNode,
  map     : &'a mut SkgNodeMap,
  config  : &SkgConfig,
) -> Result<Option<&'a SkgNode>, Box<dyn Error>>
{ match &viewnode . kind {
    ViewNodeKind::True (t) => {
      let skgnode : &SkgNode =
        skgnode_from_map_or_disk(
          &t . id, &t . source, map, config)?;
      Ok(Some (skgnode)) },
    _ => Ok (None) }}

/// Build a SkgNodeMap from DefineNodes.
/// Each SkgNode is indexed by its first ID.
/// PITFALL: Does not include every node in the buffer --
/// just the ones that generated instructions.
/// Hence the importance of 'skgnode_from_map_or_disk'.
/// PITFALL: Only Save instructions contribute;
/// Delete instructions carry no SkgNode data.
pub fn skgnode_map_from_save_instructions (
  instructions : &Vec<DefineNode>,
) -> SkgNodeMap
{ instructions . iter()
    . filter_map( |instr| match instr {
        DefineNode::Save(SaveNode (skgnode)) =>
          skgnode . ids . first()
            . map( |id : &ID| (id . clone(),
                               skgnode . clone() )),
        DefineNode::Delete (_) => None } )
    . collect() }

/// Fetches (batched) SkgNodes from disk for all IDs in the tree.
pub async fn skgnode_map_from_forest (
  forest : &Tree<ViewNode>,
  config : &SkgConfig,
  driver : &TypeDBDriver,
) -> Result<SkgNodeMap, Box<dyn Error>>
{ let mut all_tree_ids : Vec<ID> = Vec::new();
  collect_ids_from_subtree (
    forest, forest . root () . id (), &mut all_tree_ids );
  let nodes : Vec<SkgNode> = skgnodes_from_ids (
    config, driver, &all_tree_ids ) . await ?;
  let mut map : SkgNodeMap = SkgNodeMap::new ();
  for node in nodes {
    if let Some (id) = node . ids . first () {
    map . insert ( id . clone (), node ); }}
  Ok (map) }

/// Get a SkgNode from the map, or from disk if it's not there.
/// Updates the map.
pub fn skgnode_from_map_or_disk<'a>(
  id: &ID,
  source: &SourceName,
  map: &'a mut SkgNodeMap,
  config: &SkgConfig,
) -> Result<&'a SkgNode, Box<dyn Error>> {
  if !map . contains_key (id) {
    let skgnode: SkgNode = skgnode_from_pid_and_source(
      config, id . clone(), source)?;
    map . insert(id . clone(), skgnode); }
  map . get (id) . ok_or_else(
    || "SkgNode should be in map after fetch" . into( )) }

fn collect_ids_from_subtree (
  tree    : &Tree<ViewNode>,
  node_id : NodeId,
  ids_out : &mut Vec<ID>, )
{ let node_ref : NodeRef<ViewNode> =
    tree . get (node_id) . unwrap ();
  match &node_ref . value () . kind {
    ViewNodeKind::True (t)    =>
      ids_out . push ( t . id . clone () ),
    ViewNodeKind::Deleted (d) =>
      ids_out . push ( d . id . clone () ),
    _ => {} }
  for child in node_ref . children () {
    collect_ids_from_subtree (
      tree, child . id (), ids_out ); }}

/// Lookup a node's source by trying, in order:
/// - sources_from_siblings
/// - deleted_since_head_pid_src_map
/// - map
/// - disk (which, if used, updates the SkgnodeMap)
pub fn find_source_many_ways (
  id                             : &ID,
  sources_from_siblings          : &HashMap<ID, SourceName>, // When 'find_source_many_ways' is called from a parent, the parent's child viewnodes can be scanned for IDs and put here. If that's not done and this is empty, the source will still probably be found.
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>, // built by deleted_ids_to_source
  map                            : &mut SkgNodeMap,
  config                         : &SkgConfig,
) -> Result<SourceName, String> {
  if let Some (s) = sources_from_siblings . get (id)
    { return Ok ( s . clone () ); }
  if let Some (s) = deleted_since_head_pid_src_map . get (id)
    { return Ok ( s . clone () ); }

  // PITFALL: The next eight lines, just to look up from the map or disk, looks overcomplex but is necessary. (1) map.get is needed for correctness, not just speed — if the node is in memory but its .skg file isn't on disk (e.g. newly created in this save), source_from_disk would fail, even though the answer is sitting in the map. (2) source_from_disk is needed because skgnode_from_map_or_disk requires a source parameter — it can't find the file without knowing which  source directory to look in. (3) The only genuine redundancy is that skgnode_from_map_or_disk re-checks map.contains_key(id) even though we just verified the node isn't there. That's harmless.
  if let Some (s) = map . get (id) . map ( |n| n . source . clone () )
    { return Ok (s); }
  let source : SourceName =
    source_from_disk ( id, config )
    . ok_or_else ( || format! (
      "find_source_many_ways: no source found for {}", id . 0 )) ?;
  skgnode_from_map_or_disk ( id, &source, map, config )
    . map_err ( |e| e . to_string () ) ?;
  Ok (source) }
