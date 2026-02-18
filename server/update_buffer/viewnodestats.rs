use crate::types::misc::ID;
use crate::types::viewnode::{ViewNode, ViewNodeKind};
use ego_tree::{Tree, NodeId};
use std::collections::{HashMap, HashSet};

pub fn set_viewnodestats_in_forest (
  forest                : &mut Tree<ViewNode>,
  container_to_contents : &HashMap<ID, HashSet<ID>>,
  content_to_containers : &HashMap<ID, HashSet<ID>>,
) {
  let mut ancestor_ids : HashSet<ID> = HashSet::new ();
  let root_treeid : NodeId = forest . root () . id ();
  set_viewnodestats_recursive (
    forest,
    root_treeid,
    None,
    &mut ancestor_ids,
    container_to_contents,
    content_to_containers ); }

fn set_viewnodestats_recursive (
  tree                  : &mut Tree<ViewNode>,
  treeid                : NodeId,
  parent_opt_pid        : Option<&ID>,
  ancestor_ids          : &mut HashSet<ID>,
  container_to_contents : &HashMap<ID, HashSet<ID>>,
  content_to_containers : &HashMap<ID, HashSet<ID>>,
) {
  let opt_pid : Option<ID> =
    if let ViewNodeKind::True ( t ) =
      &mut tree . get_mut ( treeid ) . unwrap () . value () . kind
    { let node_pid : ID = t . id . clone();
      detect_and_mark_cycle_v2 (
        tree, treeid, &node_pid, ancestor_ids );
      set_parent_containment_stats_in_viewnode (
        tree, treeid, &node_pid, parent_opt_pid,
        container_to_contents, content_to_containers );
      Some ( node_pid )
    } else { None };
  let was_new : bool =
    if let Some ( ref pid ) = opt_pid
    { ancestor_ids . insert ( pid.clone() ) }
    else { false };
  let child_treeids : Vec<NodeId> =
    tree . get ( treeid ) . unwrap ()
    . children () . map ( |c| c . id () ) . collect ();
  for child_treeid in child_treeids {
    set_viewnodestats_recursive (
      tree,
      child_treeid,
      opt_pid . as_ref (),
      ancestor_ids,
      container_to_contents,
      content_to_containers ); }
  if was_new {
    if let Some ( ref pid ) = opt_pid
    { ancestor_ids . remove ( pid ); } } }

/// The node's 'cycle' field becomes equal to
/// whether the 'ancestor_ids' argument contains its ID.
fn detect_and_mark_cycle_v2 (
  tree         : &mut Tree<ViewNode>,
  treeid       : NodeId,
  node_pid     : &ID,
  ancestor_ids : &HashSet<ID>,
) {
  if let ViewNodeKind::True ( t ) =
    &mut tree . get_mut ( treeid ) . unwrap () . value () . kind
  { t . viewStats . cycle = ancestor_ids . contains ( node_pid ); } }

fn set_parent_containment_stats_in_viewnode (
  tree                  : &mut Tree<ViewNode>,
  treeid                : NodeId,
  node_pid              : &ID,
  parent_pid_opt        : Option<&ID>,
  container_to_contents : &HashMap<ID, HashSet<ID>>,
  content_to_containers : &HashMap<ID, HashSet<ID>>,
) {
  let (parent_is_container, parent_is_content) : (bool, bool) =
    if let Some ( parent_pid ) = parent_pid_opt {
      ( content_to_containers
          . get ( node_pid )
          . map_or ( false, |containers|
                     containers . contains ( parent_pid )),
        container_to_contents
          . get ( node_pid )
          . map_or ( false, |contents|
                     contents . contains ( parent_pid )) )
    } else { (true, false) }; // TODO ? PITFALL: Not ideal. If the parent is not a truenode, this suggests the node is its parent's content and not its container. In truth those concepts simply don't apply. But in that case, using these values for parent_is_container and parent_is_content has the desired effect on the node's metadata: It won't make any noise about either relationship.
  if let ViewNodeKind::True (t) =
    &mut tree . get_mut (treeid) . unwrap () . value () . kind
  { t . viewStats . parentIsContainer = parent_is_container;
    t . viewStats . parentIsContent = parent_is_content; }}
