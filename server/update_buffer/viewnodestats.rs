use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::stats::link_source_is_interesting;
use crate::dbs::in_rust_graph::relation_accessors::{
  BinaryRolePosition, NodeRelation, RelationRole };
use crate::herald_tokens::{AncestorFlags, relationship_heralds_sexp};
use crate::source_sets::ActiveSourceSet;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::viewnode::{
  Birth, GraphNodeStats, AffectsParent, PartnerFolder, ViewNode, ViewNodeKind, Vognode };
use crate::update_buffer::ancestry::required_ancestor;
use crate::update_buffer::reconcile::content::unintegrated_content_ids;
use ego_tree::{Tree, NodeId};
use std::collections::{HashMap, HashSet};

/// The five graph relations whose flags the H/S/O/L checks consult via
/// the in-Rust graph (contains is checked via the containment maps).
const GRAPH_RELATIONS : [NodeRelation; 4] = [
  NodeRelation::TextlinksTo,
  NodeRelation::HidesFromItsSubscriptions,
  NodeRelation::Subscribes,
  NodeRelation::OverridesViewOf, ];

pub fn set_viewnodestats_in_viewforest (
  viewforest            : &mut Tree<ViewNode>,
  graph                 : &InRustGraph,
  container_to_contents : &HashMap<ID, HashSet<ID>>,
  content_to_containers : &HashMap<ID, HashSet<ID>>,
  config                : &SkgConfig,
  active                : Option<&ActiveSourceSet>,
) {
  let multi_source : bool = config . sources . len () > 1;
  let mut ancestor_ids : HashSet<ID> = HashSet::new ();
  let root_treeid : NodeId = viewforest . root () . id ();
  set_viewnodestats_recursive (
    viewforest,
    root_treeid,
    multi_source,
    Some (graph),
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
      set_relSource (tree, treeid, graph, config);
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
/// flag. The visible PARENT is a generation-1 ancestor (a scaffold folder
/// carries no flag); a folder member additionally flags the folder's
/// required-ancestry gnodes (owner = the last entry) at their tree-gen
/// distances.
enum ParentKind {
  Gnode (ID),                 // an Active vognode parent
  Folder (PartnerFolder, NodeId),   // a PartnerFolder parent (its treeid)
  Other,
}

/// Compute and store semantic relationship and birth facts for
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
  let (gstats, affectsParent, birth, overridesHere)
    : (GraphNodeStats, AffectsParent, Birth, bool) = {
    let ViewNodeKind::Vognode (Vognode::Active (t)) =
      & tree . get (treeid) . unwrap () . value () . kind
    else { return; };
    ( t . graphStats . clone (), t . affectsParent, t . birth,
      t . viewStats . overridesHere . is_some () ) };
  let counts = match & gstats . rels {
    Some (c) => c . clone (),
    None => return, }; // no stats -> no heralds
  let parent_kind : ParentKind = parent_kind_of (tree, treeid);
  // Gather tracked ancestors (pid, generation), then flag relations.
  let mut flags : AncestorFlags = AncestorFlags::default ();
  let ancestors : Vec<(ID, usize)> = tracked_ancestors (tree, &parent_kind);
  for (anc_pid, generation) in &ancestors {
    flag_ancestor_relations (
      &mut flags, graph, active,
      container_to_contents, content_to_containers,
      node_pid, anc_pid, *generation ); }
  let unintegrated : Option<usize> =
    if affectsParent == AffectsParent::True
      && matches! (parent_kind, ParentKind::Folder (PartnerFolder::Subscribee, _)) {
      graph . and_then (|g| {
        let subscriber_pid : &ID = &ancestors . iter ()
          . find (|(_, generation)| *generation == 2) ? . 0;
        let visible = |id : &ID| -> bool {
          match active {
            None => true,
            Some (a) if a . is_all () => true,
            Some (a) => g . nodes . get (id)
              .is_some_and (|n| a . contains_source (&n . source)), }};
        let contents : Vec<ID> = g . outbound_pids_for_relation_gated (
          node_pid, NodeRelation::Contains, active )
          . into_iter () . filter (|id| visible (id)) . collect ();
        let hidden : Vec<ID> = g . outbound_pids_for_relation_gated (
          subscriber_pid, NodeRelation::HidesFromItsSubscriptions, active );
        let contained : Vec<ID> = g . outbound_pids_for_relation_gated (
          subscriber_pid, NodeRelation::Contains, active );
        let members : HashSet<ID> = unintegrated_content_ids (
          g, &contents, &hidden, &contained )
          . into_iter () . collect ();
        for (id, generation) in &ancestors {
          if members . contains (id)
            && flags . contains_out . contains (generation) {
            flags . contents_unintegrated_out . push (*generation); } }
        Some (members . len ()) })
    } else { None };
  let birth_rels : Vec<NodeRelation> =
    birth_relations (&parent_kind, affectsParent, birth, &flags,
                     overridesHere);
  let rel_heralds : Option<String> = relationship_heralds_sexp (
    &counts, gstats . aliases, gstats . extra_ids, gstats . properties,
    &flags, &birth_rels, unintegrated );
  if let ViewNodeKind::Vognode (Vognode::Active (t)) =
    &mut tree . get_mut (treeid) . unwrap () . value () . kind
  { t . viewStats . rel_heralds = rel_heralds; } }

fn parent_kind_of (
  tree   : &Tree<ViewNode>,
  treeid : NodeId,
) -> ParentKind {
  let parent_ref = match tree . get (treeid) . unwrap () . parent () {
    Some (p) => p, None => return ParentKind::Other, };
  match & parent_ref . value () . kind {
    ViewNodeKind::Vognode (Vognode::Active (t)) =>
      ParentKind::Gnode ( t . id . clone () ),
    ViewNodeKind::PartnerFolder (folder) =>
      ParentKind::Folder ( *folder, parent_ref . id () ),
    _ => ParentKind::Other, } }

/// The (pid, generation) of each tracked ancestor: the visible parent
/// gnode (gen 1), or -- for a folder member -- the folder's required-ancestry
/// gnodes (gen i+2 for the i-th required ancestor, since the folder itself
/// is gen 1). Scaffold ancestors in the chain carry no flag and are
/// skipped.
fn tracked_ancestors (
  tree        : &Tree<ViewNode>,
  parent_kind : &ParentKind,
) -> Vec<(ID, usize)> {
  match parent_kind {
    ParentKind::Gnode (pid) => vec![ (pid . clone (), 1) ],
    ParentKind::Folder (_folder, folder_treeid) => {
      let mut out : Vec<(ID, usize)> = Vec::new ();
      let mut i : usize = 0;
      loop {
        match required_ancestor (tree, *folder_treeid, i) {
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
/// is relSource gated: an edge recorded outside the active prefix
/// must not tint an ancestor herald in a more public view (it would
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
  let contains_rel_is_visible = |owner : &ID, target : &ID| -> bool {
    match (graph, active) {
      (Some (g), Some (a)) if ! a . is_all () =>
        g . relSource (owner, NodeRelation::Contains, target)
          . map ( |source| a . contains_source (&source) )
          . unwrap_or (false),
      _ => true }};
  // Contains, via the maps. inbound: ancestor contains node.
  if content_to_containers . get (node_pid)
    . map_or (false, |s| s . contains (anc_pid))
    && contains_rel_is_visible (anc_pid, node_pid) {
    flags . record (NodeRelation::Contains, true, generation); }
  // outbound: node contains ancestor.
  if container_to_contents . get (node_pid)
    . map_or (false, |s| s . contains (anc_pid))
    && contains_rel_is_visible (node_pid, anc_pid) {
    flags . record (NodeRelation::Contains, false, generation); }
  let graph = match graph { Some (g) => g, None => return, };
  for rel in GRAPH_RELATIONS {
    // inbound: ancestor R's node (ancestor plays the first role).
    if graph . relation_membership_is_visible (
      node_pid, anc_pid,
      RelationRole::new (rel, BinaryRolePosition::First), active ) {
      flags . record (rel, true, generation);
      if rel == NodeRelation::TextlinksTo
        && link_source_is_interesting (graph, active, anc_pid) {
        flags . links_interesting_in . push (generation); } }
    // outbound: node R's ancestor (ancestor plays the second role).
    if graph . relation_membership_is_visible (
      node_pid, anc_pid,
      RelationRole::new (rel, BinaryRolePosition::Second), active ) {
      flags . record (rel, false, generation); } } }

/// The birth relation(s) -- which relation token(s) explain this
/// occurrence. Usually a singleton; a HiddenInSubscribee member
/// is [Hides, Contains].
fn birth_relations (
  parent_kind : &ParentKind,
  affectsParent    : AffectsParent,
  birth       : Birth,
  flags       : &AncestorFlags,
  overridesHere : bool, // whether the node is drawn in place of a node it overrides
) -> Vec<NodeRelation> {
  let mut rels : Vec<NodeRelation> = {
    // A backpath graft's birth is its role's relation, regardless of
    // affectsParent (grafts are typically Independent/WriteProtected).
    if let Birth::Backpath (role) = birth {
      vec![ role . relation ]
    } else if affectsParent != AffectsParent::True { Vec::new ()
    } else {
      match parent_kind {
        ParentKind::Gnode (_) =>
          // Ordinary content: born of its parent containing it.
          if flags . contains_in . contains (&1) {
            vec![ NodeRelation::Contains ]
          } else { Vec::new () },
        ParentKind::Folder (folder, _) => birth_relations_for_folder (*folder),
        ParentKind::Other => Vec::new (), }}};
  if overridesHere
    && ! rels . contains (&NodeRelation::OverridesViewOf) {
    // A drawn overrider (drawn in place of a node it overrides) is
    // born of that override: it leads with the O herald, like every
    // other birth relation.
    rels . insert (0, NodeRelation::OverridesViewOf); }
  rels }

fn birth_relations_for_folder (
  folder : PartnerFolder,
) -> Vec<NodeRelation> {
  match folder {
    PartnerFolder::Subscribee | PartnerFolder::Subscriber
    | PartnerFolder::Overridden | PartnerFolder::Overrider
    | PartnerFolder::Hider | PartnerFolder::Hidden =>
      match folder . relation_member_role () {
        Some (role) => vec![ role . relation ],
        None => Vec::new (), },
    // Filter folders: the subscriber-owner HIDES the member; a
    // HiddenInSubscribee member is also CONTAINED by the subscribee.
    PartnerFolder::HiddenInSubscribee =>
      vec![ NodeRelation::HidesFromItsSubscriptions,
            NodeRelation::Contains ],
    PartnerFolder::HiddenOutsideOfSubscribee =>
      vec![ NodeRelation::HidesFromItsSubscriptions ], } }

/// Sets hidden_body on the active vognode at treeid: true iff the node
/// is drawn WRITE_PROTECTED here while its graph node has a body -- one
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
    t . is_writeProtected ()
      && graph . map_or ( false, |g| {
           let pid : ID = g . pid_of (node_pid)
             . unwrap_or_else ( || node_pid . clone () );
           g . nodes . get (&pid)
             . map_or ( false, |n| n . body . is_some () ) } ) };
  if let ViewNodeKind::Vognode (Vognode::Active (t)) =
    &mut tree . get_mut (treeid) . unwrap () . value () . kind
  { t . viewStats . hidden_body = hidden_body; }}

/// Sets relSource on the active vognode at treeid (render-and-gating,
/// 5_plan.org; see 'ViewNodeStats::relSource' for the full contract).
/// Computes the (owner, relation, target) triple that identifies the
/// binding edge this position represents -- contains for an ordinary
/// Gnode-parent content child; the folder's relation for a simple
/// PartnerFolder member, oriented by which side owns the outbound edge
/// (see 'RelationRole::is_first_role') -- then compares the edge's
/// actual source ('InRustGraph::relSource') against its applicable
/// relationship default. None on any of: no
/// graph handle; affectsParent != Affected or a backpath graft (not a
/// genuine member here); a compound filter folder
/// (HiddenInSubscribee / HiddenOutsideOfSubscribee: no single
/// 'relation_member_role'); no recorded edge; unresolvable homes;
/// or the source equalling the default.
fn set_relSource (
  tree   : &mut Tree<ViewNode>,
  treeid : NodeId,
  graph  : Option<&InRustGraph>,
  config : &SkgConfig,
) {
  let relSource : Option<SourceName> = 'compute : {
    let graph : &InRustGraph = match graph {
      Some (g) => g, None => break 'compute None, };
    let (node_pid, affectsParent, birth) : (ID, AffectsParent, Birth) = {
      let ViewNodeKind::Vognode (Vognode::Active (t)) =
        & tree . get (treeid) . unwrap () . value () . kind
      else { break 'compute None; };
      ( t . collected_id (), t . affectsParent, t . birth ) };
    if affectsParent != AffectsParent::True
      || birth != Birth::Unremarkable {
      // Not a genuine member at this position (a
      // self-writer parked under a folder, or a backpath graft): there
      // is no binding edge here to have a source at all.
      break 'compute None; }
    let (owner_pid, relation, target_pid) : (ID, NodeRelation, ID) =
      match parent_kind_of (tree, treeid) {
        ParentKind::Gnode (parent_pid) =>
          (parent_pid, NodeRelation::Contains, node_pid),
        ParentKind::Folder (folder, folder_treeid) => {
          let Some (role) = folder . relation_member_role ()
          else { break 'compute None; }; // compound filter folders
          let Some (anchor_pid) =
            tree . get (folder_treeid) . unwrap () . parent ()
            . and_then ( |p| active_vognode_pid (tree, p . id ()) )
          else { break 'compute None; };
          if role . is_first_role () {
            // This position's own node OWNS the outbound edge (e.g.
            // a subscriberFolder member, which itself subscribes to
            // the folder's anchor).
            (node_pid . clone (), role . relation, anchor_pid)
          } else {
            // The folder's anchor owns the outbound edge (e.g. a
            // subscribeeFolder member, which the anchor subscribes to).
            (anchor_pid, role . relation, node_pid . clone ())
          }},
        ParentKind::Other => break 'compute None, };
    let source : SourceName =
      match graph . relSource (&owner_pid, relation, &target_pid) {
        Some (l) => l, None => break 'compute None, };
    let default : SourceName = {
      let owner_home : Option<SourceName> =
        graph . pid_and_source (&owner_pid) . map ( |(_, s)| s );
      let target_home : Option<SourceName> =
        graph . pid_and_source (&target_pid) . map ( |(_, s)| s );
      match (owner_home, target_home) {
        (Some (a), Some (b)) =>
          config . default_relSource (&a, &b),
        _ => break 'compute None, }};
    if source == default { None } else { Some (source) } };
  if let ViewNodeKind::Vognode (Vognode::Active (t)) =
    &mut tree . get_mut (treeid) . unwrap () . value () . kind
  { t . viewStats . relSource = relSource; }}

#[cfg(test)]
mod relationship_default_tests {
  use super::*;
  use crate::types::misc::{RelPartner, SkgfileSource};
  use crate::types::nodes::complete::{empty_node_complete, NodeComplete};
  use crate::types::viewnode::{
    mk_definitive_viewnode, viewforest_root_viewnode};
  use std::path::PathBuf;

  #[test]
  fn owned_to_foreign_owner_home_edge_has_no_override_herald () {
    let mut config : SkgConfig = {
      let mut sources : HashMap<SourceName, SkgfileSource> =
        HashMap::new ();
      for (name, owned) in
          [("public", true), ("foreign", false), ("private", true)] {
        sources . insert (
          SourceName::from (name),
          SkgfileSource {
            name         : SourceName::from (name),
            abbreviation : None,
            path         : PathBuf::from (name),
            user_owns_it : owned, } ); }
      SkgConfig::dummyFromSources (sources) };
    config . source_order = ["public", "foreign", "private"]
      . into_iter () . map (SourceName::from) . collect ();

    let mut owner : NodeComplete = empty_node_complete ();
    owner . pid = ID::new ("owner");
    owner . title = "owner" . to_string ();
    owner . source = SourceName::from ("public");
    owner . contains = vec! [ RelPartner::at_relSource (
      SourceName::from ("public"), ID::new ("member") ) ];
    let mut member : NodeComplete = empty_node_complete ();
    member . pid = ID::new ("member");
    member . title = "member" . to_string ();
    member . source = SourceName::from ("foreign");
    let graph : InRustGraph =
      InRustGraph::from_nodecompletes (&[owner, member]);

    let mut tree : Tree<ViewNode> =
      Tree::new (viewforest_root_viewnode ());
    let owner_treeid : NodeId = tree . root_mut () . append (
      mk_definitive_viewnode (
        ID::new ("owner"), SourceName::from ("public"),
        "owner" . to_string (), None ) ) . id ();
    let member_treeid : NodeId = tree . get_mut (owner_treeid)
      . unwrap () . append ( mk_definitive_viewnode (
        ID::new ("member"), SourceName::from ("foreign"),
        "member" . to_string (), None ) ) . id ();

    set_relSource (
      &mut tree, member_treeid, Some (&graph), &config );
    let ViewNodeKind::Vognode (Vognode::Active (rendered_member)) =
      & tree . get (member_treeid) . unwrap () . value () . kind
    else { panic! ("member should be active"); };
    assert_eq! ( rendered_member . viewStats . relSource, None,
      "the owner-home default must not render a fake relSource override" );
  }
}

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
