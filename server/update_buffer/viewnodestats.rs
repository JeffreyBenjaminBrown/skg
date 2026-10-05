use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::stats::mentioner_is_substantive;
use crate::dbs::in_rust_graph::relation_accessors::{
  BinaryRolePosition, NodeRelation, RelationRole };
use crate::herald_tokens::{AncestorFlags, BirthFact, Side, relationship_heralds_sexp};
use crate::skgrepo_sets::ActiveSkgRepoSet;
use crate::types::misc::{ID, SkgConfig, SkgRepoName};
use crate::types::viewnode::{
  Birth, GraphnodeStats, AffectsParent, PartnerFolder, Viewnode, ViewnodeKind, Vognode };
use crate::update_buffer::ancestry::required_ancestor;
use crate::update_buffer::reconcile::content::unintegrated_content_skgids;
use ego_tree::{Tree, NodeId};
use std::collections::{HashMap, HashSet};

/// The five graph relations whose flags the H/S/O/L checks consult via
/// the in-Rust graph (contains is checked via the containment maps).
const GRAPH_RELATIONS : [NodeRelation; 4] = [
  NodeRelation::LinksTo,
  NodeRelation::HidesFromSubs,
  NodeRelation::SubscribesTo,
  NodeRelation::Overrides, ];

pub fn set_viewnodestats_in_viewforest (
  viewforest            : &mut Tree<Viewnode>,
  graph                 : &InRustGraph,
  container_to_contents : &HashMap<ID, HashSet<ID>>,
  content_to_containers : &HashMap<ID, HashSet<ID>>,
  config                : &SkgConfig,
  active                : Option<&ActiveSkgRepoSet>,
) {
  let multi_skgrepo : bool = config . skgrepos . len () > 1;
  let mut ancestor_skgids : HashSet<ID> = HashSet::new ();
  let root_treeid : NodeId = viewforest . root () . id ();
  set_viewnodestats_recursive (
    viewforest,
    root_treeid,
    multi_skgrepo,
    Some (graph),
    config,
    active,
    &mut ancestor_skgids,
    container_to_contents,
    content_to_containers ); }

fn set_viewnodestats_recursive (
  tree                  : &mut Tree<Viewnode>,
  treeid                : NodeId,
  multi_skgrepo         : bool,
  graph                 : Option<&InRustGraph>,
  config                : &SkgConfig,
  active                : Option<&ActiveSkgRepoSet>,
  ancestor_skgids       : &mut HashSet<ID>,
  container_to_contents : &HashMap<ID, HashSet<ID>>,
  content_to_containers : &HashMap<ID, HashSet<ID>>,
) {
  let opt_pid : Option<ID> =
    if let ViewnodeKind::Vognode (Vognode::Active (t)) =
      & tree . get (treeid) . unwrap () . value () . kind
    { let node_pid : ID = t . skgid . clone ();
      detect_and_mark_cycle_v2 (
        tree, treeid, &node_pid, ancestor_skgids );
      if multi_skgrepo {
        set_skgrepo_at_boundary (tree, treeid); }
      set_herald_strings_in_viewnode (
        tree, treeid, &node_pid, graph, active,
        container_to_contents, content_to_containers );
      set_omitted_body (tree, treeid, &node_pid, graph);
      set_relRepo (tree, treeid, graph, config);
      Some (node_pid)
    } else { None };
  let was_new : bool =
    if let Some ( ref pid ) = opt_pid
    { ancestor_skgids . insert ( pid . clone() ) }
    else { false };
  let child_treeids : Vec<NodeId> =
    tree . get (treeid) . unwrap ()
    . children () . map ( |c| c . id () ) . collect ();
  for child_treeid in child_treeids {
    set_viewnodestats_recursive (
      tree,
      child_treeid,
      multi_skgrepo,
      graph,
      config,
      active,
      ancestor_skgids,
      container_to_contents,
      content_to_containers ); }
  if was_new {
    if let Some ( ref pid ) = opt_pid
    { ancestor_skgids . remove (pid); } } }

/// What the active vognode at treeid is born of, and which ancestors to
/// flag. The viewparent is a generation-1 ancestor (a non-vognode folder
/// carries no flag); a folder member additionally flags the folder's
/// required-ancestry vognodes (recorder = the last entry) at their tree-gen
/// distances.
enum ParentKind {
  Vognode (ID),                 // an Active vognode parent
  Folder (PartnerFolder, NodeId),   // a PartnerFolder parent (its treeid)
  Other,
}

/// Compute and store semantic relationship and birth facts for
/// the active vognode at treeid.
fn set_herald_strings_in_viewnode (
  tree                  : &mut Tree<Viewnode>,
  treeid                : NodeId,
  node_pid              : &ID,
  graph                 : Option<&InRustGraph>,
  active                : Option<&ActiveSkgRepoSet>,
  container_to_contents : &HashMap<ID, HashSet<ID>>,
  content_to_containers : &HashMap<ID, HashSet<ID>>,
) {
  let (gstats, affectsParent, birth, overridesHere)
    : (GraphnodeStats, AffectsParent, Birth, bool) = {
    let ViewnodeKind::Vognode (Vognode::Active (t)) =
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
        let visible = |skgid : &ID| -> bool {
          match active {
            None => true,
            Some (a) if a . is_all () => true,
            Some (a) => g . nodes . get (skgid)
              .is_some_and (|n| a . contains_skgrepo (&n . home_skgrepo)), }};
        let contents : Vec<ID> = g . outbound_pids_for_relation_gated (
          node_pid, NodeRelation::Contains, active )
          . into_iter () . filter (|skgid| visible (skgid)) . collect ();
        let hidden : Vec<ID> = g . outbound_pids_for_relation_gated (
          subscriber_pid, NodeRelation::HidesFromSubs, active );
        let contained : Vec<ID> = g . outbound_pids_for_relation_gated (
          subscriber_pid, NodeRelation::Contains, active );
        let members : HashSet<ID> = unintegrated_content_skgids (
          g, &contents, &hidden, &contained )
          . into_iter () . collect ();
        for (skgid, generation) in &ancestors {
          if members . contains (skgid)
            && flags . contains_out . contains (generation) {
            flags . contents_unintegrated_out . push (*generation); } }
        Some (members . len ()) })
    } else { None };
  let birth_facts : Vec<BirthFact> =
    birth_facts (&parent_kind, affectsParent, birth, &flags,
                 overridesHere);
  let rel_heralds : Option<String> = relationship_heralds_sexp (
    &counts, gstats . aliases, gstats . extra_ids, gstats . flags,
    &flags, &birth_facts, unintegrated );
  if let ViewnodeKind::Vognode (Vognode::Active (t)) =
    &mut tree . get_mut (treeid) . unwrap () . value () . kind
  { t . viewStats . rel_heralds = rel_heralds; } }

fn parent_kind_of (
  tree   : &Tree<Viewnode>,
  treeid : NodeId,
) -> ParentKind {
  let parent_ref = match tree . get (treeid) . unwrap () . parent () {
    Some (p) => p, None => return ParentKind::Other, };
  match & parent_ref . value () . kind {
    ViewnodeKind::Vognode (Vognode::Active (t)) =>
      ParentKind::Vognode ( t . skgid . clone () ),
    ViewnodeKind::PartnerFolder (folder) =>
      ParentKind::Folder ( *folder, parent_ref . id () ),
    _ => ParentKind::Other, } }

/// The (pid, generation) of each tracked ancestor: the viewparent
/// vognode (gen 1), or -- for a folder member -- the folder's required-ancestry
/// vognodes (gen i+2 for the i-th required ancestor, since the folder itself
/// is gen 1). Non-vognode ancestors in the chain carry no flag and are
/// skipped.
fn tracked_ancestors (
  tree        : &Tree<Viewnode>,
  parent_kind : &ParentKind,
) -> Vec<(ID, usize)> {
  match parent_kind {
    ParentKind::Vognode (pid) => vec![ (pid . clone (), 1) ],
    ParentKind::Folder (_folder, folder_treeid) => {
      let mut out : Vec<(ID, usize)> = Vec::new ();
      let mut i : usize = 0;
      loop {
        match required_ancestor (tree, *folder_treeid, i) {
          Ok (Some (anc_skgid)) => {
            if let Some (pid) = active_vognode_pid (tree, anc_skgid) {
              out . push ( (pid, i + 2) ); }
            i += 1; }
          _ => break, } }
      out }
    ParentKind::Other => Vec::new (), } }

fn active_vognode_pid (
  tree   : &Tree<Viewnode>,
  treeid : NodeId,
) -> Option<ID> {
  match & tree . get (treeid) ? . value () . kind {
    ViewnodeKind::Vognode (Vognode::Active (t)) => Some ( t . skgid . clone () ),
    _ => None, } }

/// Record, for the tracked ancestor 'anc_pid' at 'generation', every
/// relation it is a member of on each side relative to 'node_pid'.
/// Contains membership is read from the repo-filtered containment
/// maps; the other four relations from the in-Rust graph. Every flag
/// is relRepo gated: a relationship recorded outside the active prefix
/// must not tint an ancestor herald in a more public view (it would
/// reveal the very relationship the user privatized). The contains
/// gate needs the graph (the maps carry no levels); without one
/// (some test paths, which never restrict) it degrades to ungated.
fn flag_ancestor_relations (
  flags                 : &mut AncestorFlags,
  graph                 : Option<&InRustGraph>,
  active                : Option<&ActiveSkgRepoSet>,
  container_to_contents : &HashMap<ID, HashSet<ID>>,
  content_to_containers : &HashMap<ID, HashSet<ID>>,
  node_pid              : &ID,
  anc_pid               : &ID,
  generation            : usize,
) {
  let contains_rel_is_visible = |recorder : &ID, target : &ID| -> bool {
    match (graph, active) {
      (Some (g), Some (a)) if ! a . is_all () =>
        g . relRepo (recorder, NodeRelation::Contains, target)
          . map ( |skgrepo| a . contains_skgrepo (&skgrepo) )
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
      if rel == NodeRelation::LinksTo
        && mentioner_is_substantive (graph, active, anc_pid) {
        flags . links_substantive_in . push (generation); } }
    // outbound: node R's ancestor (ancestor plays the second role).
    if graph . relation_membership_is_visible (
      node_pid, anc_pid,
      RelationRole::new (rel, BinaryRolePosition::Second), active ) {
      flags . record (rel, false, generation); } } }

/// The birth facts -- which relations explain this occurrence, on which
/// side of it, accounting for which ancestor. Usually a singleton; a
/// HiddenInSubscribee member has two.
fn birth_facts (
  parent_kind : &ParentKind,
  affectsParent    : AffectsParent,
  birth       : Birth,
  flags       : &AncestorFlags,
  overridesHere : bool, // whether the node is drawn in place of a node it overrides
) -> Vec<BirthFact> {
  let mut facts : Vec<BirthFact> = {
    // A role role graft's birth is its role's relation, regardless of
    // affectsParent (role grafts are typically non-members and write-protected).
    // The role graft relates to its viewparent, one generation up.
    if let Birth::RoleGraft (role) = birth {
      vec![ BirthFact::new ( role . relation, side_of_role (role), Some (1) ) ]
    } else if affectsParent != AffectsParent::True { Vec::new ()
    } else {
      match parent_kind {
        ParentKind::Vognode (_) =>
          // Ordinary content: born of its parent containing it.
          if flags . contains_in . contains (&1) {
            vec![ BirthFact::new ( NodeRelation::Contains, Side::In, Some (1) ) ]
          } else { Vec::new () },
        ParentKind::Folder (folder, _) => birth_facts_for_folder (*folder),
        ParentKind::Other => Vec::new (), }}};
  if overridesHere
    && ! facts . iter () . any ( |fact|
           fact . relation == NodeRelation::Overrides ) {
    // A drawn overrider (drawn in place of a node it overrides) is
    // born of that override: it leads with the O herald, like every
    // other birth relation. The overridden node is not an ancestor.
    facts . insert ( 0, BirthFact::new (
      NodeRelation::Overrides, Side::Out, None ) ); }
  facts }

/// The side of a node playing ROLE: the first role states the relationship
/// ("it RELATIONs N nodes"), so it is on the out side.
fn side_of_role (
  role : RelationRole,
) -> Side {
  if role . is_first_role () { Side::Out } else { Side::In } }

/// A partner-folder member's birth facts. The folder's recorder is the
/// member's grandparent; the filter folders sit one or two levels
/// deeper under the subscriber. (The table of birth facts is in
/// docs/api-and-formats.org.)
fn birth_facts_for_folder (
  folder : PartnerFolder,
) -> Vec<BirthFact> {
  match folder {
    PartnerFolder::Subscribee | PartnerFolder::Subscriber
    | PartnerFolder::Overridden | PartnerFolder::Overrider
    | PartnerFolder::Hider | PartnerFolder::Hidden =>
      match folder . relation_member_role () {
        Some (role) => vec![ BirthFact::new (
          role . relation, side_of_role (role), Some (2) ) ],
        None => Vec::new (), },
    // Filter folders. In
    //   subscriber > subscribeeFolder > subscribee
    //     > hiddenInSubscribeeFolder > member
    // the subscriber (generation 4) HIDES the member, and the
    // subscribee (generation 2) CONTAINS it. In
    //   subscriber > subscribeeFolder
    //     > hiddenOutsideOfSubscribeeFolder > member
    // the subscriber (generation 3) hides it.
    PartnerFolder::HiddenInSubscribee =>
      vec![ BirthFact::new (
              NodeRelation::HidesFromSubs, Side::In, Some (4) ),
            BirthFact::new ( NodeRelation::Contains, Side::In, Some (2) ) ],
    PartnerFolder::HiddenOutsideOfSubscribee =>
      vec![ BirthFact::new (
              NodeRelation::HidesFromSubs, Side::In, Some (3) ) ], } }

/// Sets omitted_body on the active vognode at treeid: true iff the node
/// is drawn WRITE_PROTECTED here while its graphnode has a body -- one
/// the rendering hides. Herald "B" on the ☮ (TODO/more.org). False
/// without a graph handle (some tests): better no B than a wrong one.
fn set_omitted_body (
  tree     : &mut Tree<Viewnode>,
  treeid   : NodeId,
  node_pid : &ID,
  graph    : Option<&InRustGraph>,
) {
  let omitted_body : bool = {
    let ViewnodeKind::Vognode (Vognode::Active (t)) =
      & tree . get (treeid) . unwrap () . value () . kind
    else { return; };
    t . is_writeProtected ()
      && graph . map_or ( false, |g| {
           let pid : ID = g . pid_of (node_pid)
             . unwrap_or_else ( || node_pid . clone () );
           g . nodes . get (&pid)
             . map_or ( false, |n| n . body . is_some () ) } ) };
  if let ViewnodeKind::Vognode (Vognode::Active (t)) =
    &mut tree . get_mut (treeid) . unwrap () . value () . kind
  { t . viewStats . omitted_body = omitted_body; }}

/// Sets relRepo on the active vognode at treeid (render-and-gating,
/// 5_plan.org; see 'ViewnodeStats::relRepo' for the full contract).
/// Computes the (recorder, relation, target) triple that identifies the
/// binding relationship this position represents -- contains for an ordinary
/// Vognode-parent content child; the folder's relation for a simple
/// PartnerFolder member, oriented by which side owns the outbound relationship
/// (see 'RelationRole::is_first_role') -- then compares the relationship's
/// actual skgrepo ('InRustGraph::relRepo') against its applicable
/// relationship default. None on any of: no
/// graph handle; affectsParent != True or a role graft (not a
/// genuine member here); a compound filter folder
/// (HiddenInSubscribee / HiddenOutsideOfSubscribee: no single
/// 'relation_member_role'); no recorded relationship; unresolvable homes;
/// or the skgrepo equalling the default.
fn set_relRepo (
  tree   : &mut Tree<Viewnode>,
  treeid : NodeId,
  graph  : Option<&InRustGraph>,
  config : &SkgConfig,
) {
  let relRepo : Option<SkgRepoName> = 'compute : {
    let graph : &InRustGraph = match graph {
      Some (g) => g, None => break 'compute None, };
    let (node_pid, affectsParent, birth) : (ID, AffectsParent, Birth) = {
      let ViewnodeKind::Vognode (Vognode::Active (t)) =
        & tree . get (treeid) . unwrap () . value () . kind
      else { break 'compute None; };
      ( t . collected_skgid (), t . affectsParent, t . birth ) };
    if affectsParent != AffectsParent::True
      || birth != Birth::Unremarkable {
      // Not a genuine member at this position (a
      // self-writer parked under a folder, or a role graft): there
      // is no binding relationship here to have a skgrepo at all.
      break 'compute None; }
    let (recorder_pid, relation, target_pid) : (ID, NodeRelation, ID) =
      match parent_kind_of (tree, treeid) {
        ParentKind::Vognode (parent_pid) =>
          (parent_pid, NodeRelation::Contains, node_pid),
        ParentKind::Folder (folder, folder_treeid) => {
          let Some (role) = folder . relation_member_role ()
          else { break 'compute None; }; // compound filter folders
          let Some (anchor_pid) =
            tree . get (folder_treeid) . unwrap () . parent ()
            . and_then ( |p| active_vognode_pid (tree, p . id ()) )
          else { break 'compute None; };
          if role . is_first_role () {
            // This position's own node OWNS the outbound relationship (e.g.
            // a subscriberFolder member, which itself subscribes to
            // the folder's anchor).
            (node_pid . clone (), role . relation, anchor_pid)
          } else {
            // The folder's anchor owns the outbound relationship (e.g. a
            // subscribeeFolder member, which the anchor subscribes to).
            (anchor_pid, role . relation, node_pid . clone ())
          }},
        ParentKind::Other => break 'compute None, };
    let skgrepo : SkgRepoName =
      match graph . relRepo (&recorder_pid, relation, &target_pid) {
        Some (l) => l, None => break 'compute None, };
    let default : SkgRepoName = {
      let recorder_home : Option<SkgRepoName> =
        graph . pid_and_skgrepo (&recorder_pid) . map ( |(_, s)| s );
      let target_home : Option<SkgRepoName> =
        graph . pid_and_skgrepo (&target_pid) . map ( |(_, s)| s );
      match (recorder_home, target_home) {
        (Some (a), Some (b)) =>
          config . default_relRepo (&a, &b),
        _ => break 'compute None, }};
    if skgrepo == default { None } else { Some (skgrepo) } };
  if let ViewnodeKind::Vognode (Vognode::Active (t)) =
    &mut tree . get_mut (treeid) . unwrap () . value () . kind
  { t . viewStats . relRepo = relRepo; }}

#[cfg(test)]
mod relationship_default_tests {
  use super::*;
  use crate::types::misc::{RelPartner, SkgRepo};
  use crate::types::nodes::complete::{empty_graphnode, Graphnode};
  use crate::types::viewnode::{
    mk_editable_viewnode, viewforest_root_viewnode};
  use std::path::PathBuf;

  #[test]
  fn owned_to_foreign_recorder_home_relationship_has_no_override_herald () {
    let mut config : SkgConfig = {
      let mut skgrepos : HashMap<SkgRepoName, SkgRepo> =
        HashMap::new ();
      for (name, owned) in
          [("public", true), ("foreign", false), ("private", true)] {
        skgrepos . insert (
          SkgRepoName::from (name),
          SkgRepo {
            name         : SkgRepoName::from (name),
            abbreviation : None,
            path         : PathBuf::from (name),
            owned        : owned, } ); }
      SkgConfig::dummyFromSkgRepos (skgrepos) };
    config . skgrepo_order = ["public", "foreign", "private"]
      . into_iter () . map (SkgRepoName::from) . collect ();

    let mut recorder : Graphnode = empty_graphnode ();
    recorder . pid = ID::new ("recorder");
    recorder . title = "recorder" . to_string ();
    recorder . home_skgrepo = SkgRepoName::from ("public");
    recorder . contains = vec! [ RelPartner::at_relRepo (
      SkgRepoName::from ("public"), ID::new ("member") ) ];
    let mut member : Graphnode = empty_graphnode ();
    member . pid = ID::new ("member");
    member . title = "member" . to_string ();
    member . home_skgrepo = SkgRepoName::from ("foreign");
    let graph : InRustGraph =
      InRustGraph::from_graphnodes (&[recorder, member]);

    let mut tree : Tree<Viewnode> =
      Tree::new (viewforest_root_viewnode ());
    let recorder_treeid : NodeId = tree . root_mut () . append (
      mk_editable_viewnode (
        ID::new ("recorder"), SkgRepoName::from ("public"),
        "recorder" . to_string (), None ) ) . id ();
    let member_treeid : NodeId = tree . get_mut (recorder_treeid)
      . unwrap () . append ( mk_editable_viewnode (
        ID::new ("member"), SkgRepoName::from ("foreign"),
        "member" . to_string (), None ) ) . id ();

    set_relRepo (
      &mut tree, member_treeid, Some (&graph), &config );
    let ViewnodeKind::Vognode (Vognode::Active (rendered_member)) =
      & tree . get (member_treeid) . unwrap () . value () . kind
    else { panic! ("member should be active"); };
    assert_eq! ( rendered_member . viewStats . relRepo, None,
      "the recorder-home default must not render a fake relRepo override" );
  }
}

/// Sets homeRepoAtBoundary on the active vognode at treeid.
/// True if no active vognode ancestor exists (i.e. a root),
/// or if the nearest active vognode ancestor has a different skgrepo.
fn set_skgrepo_at_boundary (
  tree   : &mut Tree<Viewnode>,
  treeid : NodeId,
) {
  let node_skgrepo : SkgRepoName = {
    let ViewnodeKind::Vognode (Vognode::Active (t)) =
      & tree . get (treeid) . unwrap () . value () . kind
    else { return; };
    t . home_skgrepo . clone () };
  let ancestor_skgrepo : Option<SkgRepoName> =
    nearest_activeVognode_ancestor_skgrepo (tree, treeid);
  let at_boundary : bool =
    match ancestor_skgrepo {
      None => true,
      Some (s) => s != node_skgrepo };
  if let ViewnodeKind::Vognode (Vognode::Active (t)) =
    &mut tree . get_mut (treeid) . unwrap () . value () . kind
  { t . viewStats . homeSkgRepoAtBoundary = at_boundary; }}

/// Walk rootward from treeid (exclusive) to find
/// the nearest active vognode ancestor's skgrepo.
fn nearest_activeVognode_ancestor_skgrepo (
  tree   : &Tree<Viewnode>,
  treeid : NodeId,
) -> Option<SkgRepoName> {
  let mut current : NodeId = treeid;
  while let Some (parent_ref)
    = tree . get (current) . unwrap () . parent ()
    { current = parent_ref . id ();
      if let ViewnodeKind::Vognode (Vognode::Active (t))
        = & parent_ref . value () . kind
        { return Some ( t . home_skgrepo . clone () ); }}
  None }

/// The node's 'cycle' field becomes equal to
/// whether the 'ancestor_ids' argument contains its ID.
fn detect_and_mark_cycle_v2 (
  tree            : &mut Tree<Viewnode>,
  treeid          : NodeId,
  node_pid        : &ID,
  ancestor_skgids : &HashSet<ID>,
) {
  if let ViewnodeKind::Vognode (Vognode::Active (t)) =
    &mut tree . get_mut (treeid) . unwrap () . value () . kind
  { t . viewStats . cycle = ancestor_skgids . contains (node_pid); } }
