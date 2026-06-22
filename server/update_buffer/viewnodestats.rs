use crate::dbs::in_rust_graph::{InRustGraph, snapshot_global};
use crate::dbs::in_rust_graph::relation_accessors::{
  BinaryRolePosition, NodeRelation, RelationRole };
use crate::herald_tokens::{AncestorFlags, assemble_active, HeraldStrings};
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::viewnode::{
  Birth, GraphNodeStats, ParentIs, PartnerCol, ViewNode, ViewNodeKind, Vognode };
use crate::update_buffer::ancestry::required_ancestor;
use ego_tree::{Tree, NodeId};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

/// The five graph relations whose flags the H/S/O/L checks consult via
/// the in-Rust graph (contains is checked via the containment maps).
const GRAPH_RELATIONS : [NodeRelation; 4] = [
  NodeRelation::TextlinksTo,
  NodeRelation::HidesFromItsSubscriptions,
  NodeRelation::Subscribes,
  NodeRelation::OverridesViewOf, ];

pub fn set_viewnodestats_in_viewforest (
  viewforest            : &mut Tree<ViewNode>,
  container_to_contents : &HashMap<ID, HashSet<ID>>,
  content_to_containers : &HashMap<ID, HashSet<ID>>,
  config                : &SkgConfig,
) {
  let multi_source : bool = config . sources . len () > 1;
  let graph : Option<Arc<InRustGraph>> =
    // None only on paths that bypass the global handle (some tests);
    // then the relation flags stay empty and only contains-based
    // heralds (via the maps) can appear.
    snapshot_global ();
  let mut ancestor_ids : HashSet<ID> = HashSet::new ();
  let root_treeid : NodeId = viewforest . root () . id ();
  set_viewnodestats_recursive (
    viewforest,
    root_treeid,
    multi_source,
    graph . as_deref (),
    &mut ancestor_ids,
    container_to_contents,
    content_to_containers ); }

fn set_viewnodestats_recursive (
  tree                  : &mut Tree<ViewNode>,
  treeid                : NodeId,
  multi_source          : bool,
  graph                 : Option<&InRustGraph>,
  ancestor_ids          : &mut HashSet<ID>,
  container_to_contents : &HashMap<ID, HashSet<ID>>,
  content_to_containers : &HashMap<ID, HashSet<ID>>,
) {
  let opt_pid : Option<ID> =
    if let ViewNodeKind::Vognode (Vognode::Active (t)) =
      & tree . get (treeid) . unwrap () . value () . kind
    { let node_pid : ID = t . id . clone ();
      detect_and_mark_cycle_v2 (
        tree, treeid, &node_pid, ancestor_ids );
      if multi_source {
        set_source_at_boundary (tree, treeid); }
      set_herald_strings_in_viewnode (
        tree, treeid, &node_pid, graph,
        container_to_contents, content_to_containers );
      Some (node_pid)
    } else { None };
  let was_new : bool =
    if let Some ( ref pid ) = opt_pid
    { ancestor_ids . insert ( pid . clone() ) }
    else { false };
  let child_treeids : Vec<NodeId> =
    tree . get (treeid) . unwrap ()
    . children () . map ( |c| c . id () ) . collect ();
  for child_treeid in child_treeids {
    set_viewnodestats_recursive (
      tree,
      child_treeid,
      multi_source,
      graph,
      ancestor_ids,
      container_to_contents,
      content_to_containers ); }
  if was_new {
    if let Some ( ref pid ) = opt_pid
    { ancestor_ids . remove (pid); } } }

/// What the active vognode at treeid is born of, and which ancestors to
/// flag. The visible PARENT is a generation-1 ancestor (a scaffold col
/// carries no flag); a col member additionally flags the col's
/// required-ancestry gnodes (owner = the last entry) at their tree-gen
/// distances.
enum ParentKind {
  Gnode (ID),                 // an Active vognode parent
  Col (PartnerCol, NodeId),   // a PartnerCol parent (its treeid)
  Other,
}

/// Compute and store the orange birth herald and blue rels herald for
/// the active vognode at treeid.
fn set_herald_strings_in_viewnode (
  tree                  : &mut Tree<ViewNode>,
  treeid                : NodeId,
  node_pid              : &ID,
  graph                 : Option<&InRustGraph>,
  container_to_contents : &HashMap<ID, HashSet<ID>>,
  content_to_containers : &HashMap<ID, HashSet<ID>>,
) {
  let (gstats, parentIs, birth) : (GraphNodeStats, ParentIs, Birth) = {
    let ViewNodeKind::Vognode (Vognode::Active (t)) =
      & tree . get (treeid) . unwrap () . value () . kind
    else { return; };
    ( t . graphStats . clone (), t . parentIs, t . birth ) };
  let counts = match & gstats . rels {
    Some (c) => c . clone (),
    None => return, }; // no stats -> no heralds
  let parent_kind : ParentKind = parent_kind_of (tree, treeid);
  // Gather tracked ancestors (pid, generation), then flag relations.
  let mut flags : AncestorFlags = AncestorFlags::default ();
  for (anc_pid, generation) in
    tracked_ancestors (tree, &parent_kind) {
    flag_ancestor_relations (
      &mut flags, graph, container_to_contents, content_to_containers,
      node_pid, &anc_pid, generation ); }
  let birth_rels : Vec<NodeRelation> =
    birth_relations (&parent_kind, parentIs, birth, &flags);
  let strings : HeraldStrings = assemble_active (
    &counts, gstats . aliases, gstats . extra_ids, &flags, &birth_rels );
  if let ViewNodeKind::Vognode (Vognode::Active (t)) =
    &mut tree . get_mut (treeid) . unwrap () . value () . kind
  { t . viewStats . birth_herald = strings . birth;
    t . viewStats . rels_herald  = strings . rels; } }

fn parent_kind_of (
  tree   : &Tree<ViewNode>,
  treeid : NodeId,
) -> ParentKind {
  let parent_ref = match tree . get (treeid) . unwrap () . parent () {
    Some (p) => p, None => return ParentKind::Other, };
  match & parent_ref . value () . kind {
    ViewNodeKind::Vognode (Vognode::Active (t)) =>
      ParentKind::Gnode ( t . id . clone () ),
    ViewNodeKind::PartnerCol (col) =>
      ParentKind::Col ( *col, parent_ref . id () ),
    _ => ParentKind::Other, } }

/// The (pid, generation) of each tracked ancestor: the visible parent
/// gnode (gen 1), or -- for a col member -- the col's required-ancestry
/// gnodes (gen i+2 for the i-th required ancestor, since the col itself
/// is gen 1). Scaffold ancestors in the chain carry no flag and are
/// skipped.
fn tracked_ancestors (
  tree        : &Tree<ViewNode>,
  parent_kind : &ParentKind,
) -> Vec<(ID, usize)> {
  match parent_kind {
    ParentKind::Gnode (pid) => vec![ (pid . clone (), 1) ],
    ParentKind::Col (_col, col_treeid) => {
      let mut out : Vec<(ID, usize)> = Vec::new ();
      let mut i : usize = 0;
      loop {
        match required_ancestor (tree, *col_treeid, i) {
          Ok (Some (anc_id)) => {
            if let Some (pid) = active_vognode_pid (tree, anc_id) {
              out . push ( (pid, i + 2) ); }
            i += 1; }
          _ => break, } }
      out }
    ParentKind::Other => Vec::new (), } }

fn active_vognode_pid (
  tree   : &Tree<ViewNode>,
  treeid : NodeId,
) -> Option<ID> {
  match & tree . get (treeid) ? . value () . kind {
    ViewNodeKind::Vognode (Vognode::Active (t)) => Some ( t . id . clone () ),
    _ => None, } }

/// Record, for the tracked ancestor 'anc_pid' at 'generation', every
/// relation it is a member of on each side relative to 'node_pid'.
/// Contains is read from the source-filtered containment maps; the other
/// four relations from the in-Rust graph.
fn flag_ancestor_relations (
  flags                 : &mut AncestorFlags,
  graph                 : Option<&InRustGraph>,
  container_to_contents : &HashMap<ID, HashSet<ID>>,
  content_to_containers : &HashMap<ID, HashSet<ID>>,
  node_pid              : &ID,
  anc_pid               : &ID,
  generation            : usize,
) {
  // Contains, via the maps. inbound: ancestor contains node.
  if content_to_containers . get (node_pid)
    . map_or (false, |s| s . contains (anc_pid)) {
    flags . record (NodeRelation::Contains, true, generation); }
  // outbound: node contains ancestor.
  if container_to_contents . get (node_pid)
    . map_or (false, |s| s . contains (anc_pid)) {
    flags . record (NodeRelation::Contains, false, generation); }
  let graph = match graph { Some (g) => g, None => return, };
  for rel in GRAPH_RELATIONS {
    // inbound: ancestor R's node (ancestor plays the first role).
    if graph . relation_membership_is_real (
      node_pid, anc_pid,
      RelationRole::new (rel, BinaryRolePosition::First) ) {
      flags . record (rel, true, generation); }
    // outbound: node R's ancestor (ancestor plays the second role).
    if graph . relation_membership_is_real (
      node_pid, anc_pid,
      RelationRole::new (rel, BinaryRolePosition::Second) ) {
      flags . record (rel, false, generation); } } }

/// The birth relation(s) -- which relation token(s) lead in orange,
/// and in what order. Usually a singleton; a HiddenInSubscribee member
/// is [Hides, Contains].
fn birth_relations (
  parent_kind : &ParentKind,
  parentIs    : ParentIs,
  birth       : Birth,
  flags       : &AncestorFlags,
) -> Vec<NodeRelation> {
  // A backpath graft's birth is its role's relation, regardless of
  // parentIs (grafts are typically Independent/Indefinitive).
  if let Birth::Backpath (role) = birth {
    return vec![ role . relation ]; }
  if parentIs != ParentIs::Affected { return Vec::new (); }
  match parent_kind {
    ParentKind::Gnode (_) =>
      // Ordinary content: born of its parent containing it.
      if flags . contains_in . contains (&1) {
        vec![ NodeRelation::Contains ]
      } else { Vec::new () },
    ParentKind::Col (col, _) => birth_relations_for_col (*col),
    ParentKind::Other => Vec::new (), } }

fn birth_relations_for_col (
  col : PartnerCol,
) -> Vec<NodeRelation> {
  match col {
    PartnerCol::Subscribee | PartnerCol::Subscriber
    | PartnerCol::Overridden | PartnerCol::Overrider
    | PartnerCol::Hider | PartnerCol::Hidden =>
      match col . relation_member_role () {
        Some (role) => vec![ role . relation ],
        None => Vec::new (), },
    // Filter cols: the subscriber-owner HIDES the member; a
    // HiddenInSubscribee member is also CONTAINED by the subscribee.
    PartnerCol::HiddenInSubscribee =>
      vec![ NodeRelation::HidesFromItsSubscriptions,
            NodeRelation::Contains ],
    PartnerCol::HiddenOutsideOfSubscribee =>
      vec![ NodeRelation::HidesFromItsSubscriptions ], } }

/// Sets sourceAtBoundary on the active vognode at treeid.
/// True if no active vognode ancestor exists (i.e. a root),
/// or if the nearest active vognode ancestor has a different source.
fn set_source_at_boundary (
  tree   : &mut Tree<ViewNode>,
  treeid : NodeId,
) {
  let node_source : SourceName = {
    let ViewNodeKind::Vognode (Vognode::Active (t)) =
      & tree . get (treeid) . unwrap () . value () . kind
    else { return; };
    t . source . clone () };
  let ancestor_source : Option<SourceName> =
    nearest_activeNode_ancestor_source (tree, treeid);
  let at_boundary : bool =
    match ancestor_source {
      None => true,
      Some (s) => s != node_source };
  if let ViewNodeKind::Vognode (Vognode::Active (t)) =
    &mut tree . get_mut (treeid) . unwrap () . value () . kind
  { t . viewStats . sourceAtBoundary = at_boundary; }}

/// Walk rootward from treeid (exclusive) to find
/// the nearest active vognode ancestor's source.
fn nearest_activeNode_ancestor_source (
  tree   : &Tree<ViewNode>,
  treeid : NodeId,
) -> Option<SourceName> {
  let mut current : NodeId = treeid;
  while let Some (parent_ref)
    = tree . get (current) . unwrap () . parent ()
    { current = parent_ref . id ();
      if let ViewNodeKind::Vognode (Vognode::Active (t))
        = & parent_ref . value () . kind
        { return Some ( t . source . clone () ); }}
  None }

/// The node's 'cycle' field becomes equal to
/// whether the 'ancestor_ids' argument contains its ID.
fn detect_and_mark_cycle_v2 (
  tree         : &mut Tree<ViewNode>,
  treeid       : NodeId,
  node_pid     : &ID,
  ancestor_ids : &HashSet<ID>,
) {
  if let ViewNodeKind::Vognode (Vognode::Active (t)) =
    &mut tree . get_mut (treeid) . unwrap () . value () . kind
  { t . viewStats . cycle = ancestor_ids . contains (node_pid); } }
