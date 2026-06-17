use crate::dbs::in_rust_graph::{InRustGraph, snapshot_global};
use crate::dbs::in_rust_graph::relation_accessors::{
  BinaryRolePosition, NodeRelation, RelationRole};
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::viewnode::{PartnerCol, ViewNode, ViewNodeKind};
use crate::types::viewnode::Vognode;
use ego_tree::{Tree, NodeId};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

/// What an Active vognode's visible parent is, for the
/// position-relative view stats. 'Gnode' drives the containment
/// stats and 'overridesParent'; 'Col' drives the grandparent stats
/// ('owner' being the col's own parent gnode); everything else
/// ('Other', including the BufferRoot above view roots) drives none.
#[derive(Clone)]
enum VisibleParent {
  Gnode (ID),
  Col { col : PartnerCol, owner : Option<ID> },
  Other,
}

pub fn set_viewnodestats_in_viewforest (
  viewforest                : &mut Tree<ViewNode>,
  container_to_contents : &HashMap<ID, HashSet<ID>>,
  content_to_containers : &HashMap<ID, HashSet<ID>>,
  config                : &SkgConfig,
) {
  let multi_source : bool = config . sources . len () > 1;
  let graph : Option<Arc<InRustGraph>> =
    // None only on paths that bypass the global handle (some tests);
    // then the relation-relative stats (gO, gS, Op) stay false.
    snapshot_global ();
  let mut ancestor_ids : HashSet<ID> = HashSet::new ();
  let root_treeid : NodeId = viewforest . root () . id ();
  set_viewnodestats_recursive (
    viewforest,
    root_treeid,
    &VisibleParent::Other,
    multi_source,
    graph . as_deref (),
    &mut ancestor_ids,
    container_to_contents,
    content_to_containers ); }

fn set_viewnodestats_recursive (
  tree                  : &mut Tree<ViewNode>,
  treeid                : NodeId,
  parent                : &VisibleParent,
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
      set_parent_containment_stats_in_viewnode (
        tree, treeid, &node_pid,
        match parent { VisibleParent::Gnode (pid) => Some (pid),
                       _ => None },
        container_to_contents, content_to_containers );
      if multi_source {
        set_source_at_boundary (tree, treeid); }
      if let Some (graph) = graph {
        set_relation_relative_stats (
          tree, treeid, &node_pid, parent, graph ); }
      Some (node_pid)
    } else { None };
  let context_for_children : VisibleParent =
    match & tree . get (treeid) . unwrap () . value () . kind {
      ViewNodeKind::Vognode (Vognode::Active (_)) =>
        VisibleParent::Gnode (
          opt_pid . clone () . expect ("just set for an Active vognode") ),
      ViewNodeKind::PartnerCol (col) =>
        VisibleParent::Col {
          col   : *col,
          owner : match parent {
            VisibleParent::Gnode (pid) => Some ( pid . clone () ),
            _ => None } },
      _ => VisibleParent::Other };
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
      &context_for_children,
      multi_source,
      graph,
      ancestor_ids,
      container_to_contents,
      content_to_containers ); }
  if was_new {
    if let Some ( ref pid ) = opt_pid
    { ancestor_ids . remove (pid); } } }

/// Sets the stats that relate the node to its visible parent through
/// the sharing relations:
/// - 'overridesParent' ("Op"): the parent is a gnode this node
///   overrides. Computed at every such position; the org-relative
///   analogue of 'containsParent'.
/// - 'grandparentOverrides' ("gO"): this node is a subscribee-as-such
///   (Affected child of a SubscribeeCol) whose col owner also
///   overrides it.
/// - 'grandparentSubscribes' ("gS"): this node is an Affected child
///   of an OverriddenCol whose col owner also subscribes to it.
/// The redundant cases are deliberately not computed (no gS under a
/// subscribeeCol, no gO under an overriddenCol): the col itself
/// already says it.
fn set_relation_relative_stats (
  tree     : &mut Tree<ViewNode>,
  treeid   : NodeId,
  node_pid : &ID,
  parent   : &VisibleParent,
  graph    : &InRustGraph,
) {
  let (overrides_parent, grandparent_overrides, grandparent_subscribes)
    : (bool, bool, bool) = {
    let node_is_affected : bool =
      tree . get (treeid) . unwrap () . value ()
      . is_activeNode_and_parentIs_affected ();
    match parent {
      VisibleParent::Gnode (parent_pid) =>
        ( graph . relation_membership_is_real (
            parent_pid, node_pid, // the node overrides; the parent is overridden
            RelationRole::new ( NodeRelation::OverridesViewOf,
                                BinaryRolePosition::First )),
          false, false ),
      VisibleParent::Col { col : PartnerCol::Subscribee,
                           owner : Some (owner_pid) }
        if node_is_affected =>
        ( false,
          graph . relation_membership_is_real (
            owner_pid, node_pid, // the owner overrides; the node is overridden
            RelationRole::new ( NodeRelation::OverridesViewOf,
                                BinaryRolePosition::Second )),
          false ),
      VisibleParent::Col { col : PartnerCol::Overridden,
                           owner : Some (owner_pid) }
        if node_is_affected =>
        ( false, false,
          graph . relation_membership_is_real (
            owner_pid, node_pid, // the owner subscribes; the node is the subscribee
            RelationRole::new ( NodeRelation::Subscribes,
                                BinaryRolePosition::Second )) ),
      _ => (false, false, false) } };
  if let ViewNodeKind::Vognode (Vognode::Active (t)) =
    &mut tree . get_mut (treeid) . unwrap () . value () . kind
  { t . viewStats . overridesParent       = overrides_parent;
    t . viewStats . grandparentOverrides  = grandparent_overrides;
    t . viewStats . grandparentSubscribes = grandparent_subscribes; }}

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

fn set_parent_containment_stats_in_viewnode (
  tree                  : &mut Tree<ViewNode>,
  treeid                : NodeId,
  node_pid              : &ID,
  parent_pid_opt        : Option<&ID>,
  container_to_contents : &HashMap<ID, HashSet<ID>>,
  content_to_containers : &HashMap<ID, HashSet<ID>>,
) {
  let (parent_is_container, parent_is_content) : (bool, bool) =
    if let Some (parent_pid) = parent_pid_opt {
      ( content_to_containers
          . get (node_pid)
          . map_or ( false, |containers|
                     containers . contains (parent_pid)),
        container_to_contents
          . get (node_pid)
          . map_or ( false, |contents|
                     contents . contains (parent_pid)) )
    } else { (true, false) }; // TODO ? PITFALL: Not ideal. If the parent is not an activeNode, this suggests the node is its parent's content and not its container. In truth those concepts simply don't apply. But in that case, using these values for parent_is_container and parent_is_content has the desired effect on the node's metadata: It won't make any noise about either relationship.
  if let ViewNodeKind::Vognode (Vognode::Active (t)) =
    &mut tree . get_mut (treeid) . unwrap () . value () . kind
  { t . viewStats . parentIsContainer = parent_is_container;
    t . viewStats . parentIsContent = parent_is_content; }}
