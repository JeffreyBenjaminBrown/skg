use crate::dbs::in_rust_graph::{InRustGraph, snapshot_global};
use crate::dbs::in_rust_graph::relation_accessors::{
  BinaryRolePosition, NodeRelation, RelationRole };
use crate::herald_tokens::{AncestorFlags, assemble_active};
use crate::source_sets::ActiveSourceSet;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::viewnode::{
  Birth, GraphNodeStats, HeraldSpan, ParentIs, PartnerCol, ViewNode, ViewNodeKind, Vognode };
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
  active                : Option<&ActiveSourceSet>,
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
    config,
    active,
    &mut ancestor_ids,
    container_to_contents,
    content_to_containers ); }

fn set_viewnodestats_recursive (
  tree                  : &mut Tree<ViewNode>,
  treeid                : NodeId,
  multi_source          : bool,
  graph                 : Option<&InRustGraph>,
  config                : &SkgConfig,
  active                : Option<&ActiveSourceSet>,
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
        tree, treeid, &node_pid, graph, active,
        container_to_contents, content_to_containers );
      set_hidden_body (tree, treeid, &node_pid, graph);
      set_rel_source (tree, treeid, graph, config);
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
      config,
      active,
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
  active                : Option<&ActiveSourceSet>,
  container_to_contents : &HashMap<ID, HashSet<ID>>,
  content_to_containers : &HashMap<ID, HashSet<ID>>,
) {
  let (gstats, parentIs, birth, overridesHere)
    : (GraphNodeStats, ParentIs, Birth, bool) = {
    let ViewNodeKind::Vognode (Vognode::Active (t)) =
      & tree . get (treeid) . unwrap () . value () . kind
    else { return; };
    ( t . graphStats . clone (), t . parentIs, t . birth,
      t . viewStats . overridesHere . is_some () ) };
  let counts = match & gstats . rels {
    Some (c) => c . clone (),
    None => return, }; // no stats -> no heralds
  let parent_kind : ParentKind = parent_kind_of (tree, treeid);
  // Gather tracked ancestors (pid, generation), then flag relations.
  let mut flags : AncestorFlags = AncestorFlags::default ();
  for (anc_pid, generation) in
    tracked_ancestors (tree, &parent_kind) {
    flag_ancestor_relations (
      &mut flags, graph, active,
      container_to_contents, content_to_containers,
      node_pid, &anc_pid, generation ); }
  let birth_rels : Vec<NodeRelation> =
    birth_relations (&parent_kind, parentIs, birth, &flags,
                     overridesHere);
  let spans : Vec<HeraldSpan> = assemble_active (
    &counts, gstats . aliases, gstats . extra_ids, &flags, &birth_rels );
  if let ViewNodeKind::Vognode (Vognode::Active (t)) =
    &mut tree . get_mut (treeid) . unwrap () . value () . kind
  { t . viewStats . rel_spans =
      if spans . is_empty () { None } else { Some (spans) }; } }

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
/// Contains membership is read from the source-filtered containment
/// maps; the other four relations from the in-Rust graph. Every flag
/// is EDGE-LEVEL gated: an edge recorded above the active prefix
/// must not tint an ancestor herald at a more public level (it would
/// reveal the very relationship the user privatized). The contains
/// gate needs the graph (the maps carry no levels); without one
/// (some test paths, which never restrict) it degrades to ungated.
fn flag_ancestor_relations (
  flags                 : &mut AncestorFlags,
  graph                 : Option<&InRustGraph>,
  active                : Option<&ActiveSourceSet>,
  container_to_contents : &HashMap<ID, HashSet<ID>>,
  content_to_containers : &HashMap<ID, HashSet<ID>>,
  node_pid              : &ID,
  anc_pid               : &ID,
  generation            : usize,
) {
  let contains_edge_visible = |owner : &ID, target : &ID| -> bool {
    match (graph, active) {
      (Some (g), Some (a)) if ! a . is_all () =>
        g . edge_level (owner, NodeRelation::Contains, target)
          . map ( |level| a . contains_source (&level) )
          . unwrap_or (false),
      _ => true }};
  // Contains, via the maps. inbound: ancestor contains node.
  if content_to_containers . get (node_pid)
    . map_or (false, |s| s . contains (anc_pid))
    && contains_edge_visible (anc_pid, node_pid) {
    flags . record (NodeRelation::Contains, true, generation); }
  // outbound: node contains ancestor.
  if container_to_contents . get (node_pid)
    . map_or (false, |s| s . contains (anc_pid))
    && contains_edge_visible (node_pid, anc_pid) {
    flags . record (NodeRelation::Contains, false, generation); }
  let graph = match graph { Some (g) => g, None => return, };
  for rel in GRAPH_RELATIONS {
    // inbound: ancestor R's node (ancestor plays the first role).
    if graph . relation_membership_is_visible (
      node_pid, anc_pid,
      RelationRole::new (rel, BinaryRolePosition::First), active ) {
      flags . record (rel, true, generation); }
    // outbound: node R's ancestor (ancestor plays the second role).
    if graph . relation_membership_is_visible (
      node_pid, anc_pid,
      RelationRole::new (rel, BinaryRolePosition::Second), active ) {
      flags . record (rel, false, generation); } } }

/// The birth relation(s) -- which relation token(s) lead in orange,
/// and in what order. Usually a singleton; a HiddenInSubscribee member
/// is [Hides, Contains].
fn birth_relations (
  parent_kind : &ParentKind,
  parentIs    : ParentIs,
  birth       : Birth,
  flags       : &AncestorFlags,
  overridesHere : bool, // whether the node is drawn in place of a node it overrides
) -> Vec<NodeRelation> {
  let mut rels : Vec<NodeRelation> = {
    // A backpath graft's birth is its role's relation, regardless of
    // parentIs (grafts are typically Independent/Indefinitive).
    if let Birth::Backpath (role) = birth {
      vec![ role . relation ]
    } else if parentIs != ParentIs::Affected { Vec::new ()
    } else {
      match parent_kind {
        ParentKind::Gnode (_) =>
          // Ordinary content: born of its parent containing it.
          if flags . contains_in . contains (&1) {
            vec![ NodeRelation::Contains ]
          } else { Vec::new () },
        ParentKind::Col (col, _) => birth_relations_for_col (*col),
        ParentKind::Other => Vec::new (), }}};
  if overridesHere
    && ! rels . contains (&NodeRelation::OverridesViewOf) {
    // A drawn overrider (drawn in place of a node it overrides) is
    // born of that override: it leads with the O herald, like every
    // other birth relation.
    rels . insert (0, NodeRelation::OverridesViewOf); }
  rels }

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

/// Sets hidden_body on the active vognode at treeid: true iff the node
/// is drawn INDEFINITIVE here while its graph node has a body -- one
/// the rendering hides. Herald "B" on the ☮ (TODO/more.org). False
/// without a graph handle (some tests): better no B than a wrong one.
fn set_hidden_body (
  tree     : &mut Tree<ViewNode>,
  treeid   : NodeId,
  node_pid : &ID,
  graph    : Option<&InRustGraph>,
) {
  let hidden_body : bool = {
    let ViewNodeKind::Vognode (Vognode::Active (t)) =
      & tree . get (treeid) . unwrap () . value () . kind
    else { return; };
    t . is_indefinitive ()
      && graph . map_or ( false, |g| {
           let pid : ID = g . pid_of (node_pid)
             . unwrap_or_else ( || node_pid . clone () );
           g . nodes . get (&pid)
             . map_or ( false, |n| n . body . is_some () ) } ) };
  if let ViewNodeKind::Vognode (Vognode::Active (t)) =
    &mut tree . get_mut (treeid) . unwrap () . value () . kind
  { t . viewStats . hidden_body = hidden_body; }}

/// Sets rel_source on the active vognode at treeid (render-and-gating,
/// 5_plan.org; see 'ViewNodeStats::rel_source' for the full contract).
/// Computes the (owner, relation, target) triple that identifies the
/// binding edge this position represents -- contains for an ordinary
/// Gnode-parent content child; the col's relation for a simple
/// PartnerCol member, oriented by which side owns the outbound edge
/// (see 'RelationRole::is_first_role') -- then compares the edge's
/// actual level ('InRustGraph::edge_level') against its default (the
/// more private of the two endpoints' homes). None on any of: no
/// graph handle; parentIs != Affected or a backpath graft (not a
/// genuine member here); a compound filter col
/// (HiddenInSubscribee / HiddenOutsideOfSubscribee: no single
/// 'relation_member_role'); no recorded edge; unresolvable homes;
/// or the level equalling the default.
fn set_rel_source (
  tree   : &mut Tree<ViewNode>,
  treeid : NodeId,
  graph  : Option<&InRustGraph>,
  config : &SkgConfig,
) {
  let rel_source : Option<SourceName> = 'compute : {
    let graph : &InRustGraph = match graph {
      Some (g) => g, None => break 'compute None, };
    let (node_pid, parentIs, birth) : (ID, ParentIs, Birth) = {
      let ViewNodeKind::Vognode (Vognode::Active (t)) =
        & tree . get (treeid) . unwrap () . value () . kind
      else { break 'compute None; };
      ( t . collected_id (), t . parentIs, t . birth ) };
    if parentIs != ParentIs::Affected
      || birth != Birth::Unremarkable {
      // Not a genuine member of the collection at this position (a
      // self-writer parked under a col, or a backpath graft): there
      // is no binding edge here to have a level at all.
      break 'compute None; }
    let (owner_pid, relation, target_pid) : (ID, NodeRelation, ID) =
      match parent_kind_of (tree, treeid) {
        ParentKind::Gnode (parent_pid) =>
          (parent_pid, NodeRelation::Contains, node_pid),
        ParentKind::Col (col, col_treeid) => {
          let Some (role) = col . relation_member_role ()
          else { break 'compute None; }; // compound filter cols
          let Some (anchor_pid) =
            tree . get (col_treeid) . unwrap () . parent ()
            . and_then ( |p| active_vognode_pid (tree, p . id ()) )
          else { break 'compute None; };
          if role . is_first_role () {
            // This position's own node OWNS the outbound edge (e.g.
            // a subscriberCol member, which itself subscribes to
            // the col's anchor).
            (node_pid . clone (), role . relation, anchor_pid)
          } else {
            // The col's anchor owns the outbound edge (e.g. a
            // subscribeeCol member, which the anchor subscribes to).
            (anchor_pid, role . relation, node_pid . clone ())
          }},
        ParentKind::Other => break 'compute None, };
    let level : SourceName =
      match graph . edge_level (&owner_pid, relation, &target_pid) {
        Some (l) => l, None => break 'compute None, };
    let default : SourceName = {
      let owner_home : Option<SourceName> =
        graph . pid_and_source (&owner_pid) . map ( |(_, s)| s );
      let target_home : Option<SourceName> =
        graph . pid_and_source (&target_pid) . map ( |(_, s)| s );
      match (owner_home, target_home) {
        (Some (a), Some (b)) => config . more_private_of (a, b),
        _ => break 'compute None, }};
    if level == default { None } else { Some (level) } };
  if let ViewNodeKind::Vognode (Vognode::Active (t)) =
    &mut tree . get_mut (treeid) . unwrap () . value () . kind
  { t . viewStats . rel_source = rel_source; }}

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
