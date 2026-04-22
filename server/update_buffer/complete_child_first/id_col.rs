use crate::types::git::{MembershipAxes, NodeChanges, Sign};
use crate::types::list::Diff_Item;
use crate::types::memory::nodecomplete_from_memory_or_disk;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::git::{SourceDiff, node_changes_for_truenode};
use crate::types::tree::generic::{error_unless_node_satisfies, pid_and_source_from_ancestor};
use crate::types::viewnode::{ViewNode, ViewNodeKind, Scaffold, viewnode_from_scaffold};
use crate::update_buffer::util::complete_relevant_children_in_viewnodetree;
use ego_tree::{NodeId, Tree};
use std::collections::HashMap;
use std::error::Error;

/// Reconciles an IDCol's children against
///   the IDs on disk (via the map) for its parent TrueNode.
///
/// - Verify this node is an IDCol
/// - Verify its parent is a TrueNode
/// - Fetch the corresponding NodeComplete from the map
/// - Read its IDs into a goal list
/// - In diff view, also build a diff-status map from NodeChanges.ids_diff
/// - Reconcile children via complete_relevant_children_in_viewnodetree
#[allow (non_snake_case)]
pub fn completeIDCol (
  idcol_node_id : NodeId,
  tree          : &mut Tree<ViewNode>,
  source_diffs  : &Option<HashMap<SourceName, SourceDiff>>,
  config        : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  error_unless_node_satisfies(
    tree, idcol_node_id,
    |viewnode| matches!( &viewnode . kind,
                         ViewNodeKind::Scaff (Scaffold::IDCol) ),
    "completeIDCol: Node is not an IDCol" )
    . map_err( |e| -> Box<dyn Error> { e . into() } )?;
  let (parent_pid, parent_source) : (ID, SourceName) =
    pid_and_source_from_ancestor(
      tree, idcol_node_id, 1,
      "completeIDCol" ) ?;
  let parent_nodecomplete : NodeComplete =
    nodecomplete_from_memory_or_disk (
      config, &parent_pid, &parent_source )
    . map_err ( |_| "completeIDCol: parent NodeComplete not found" ) ?;
  let node_changes : Option<&NodeChanges> =
    node_changes_for_truenode(
      source_diffs, &parent_pid, &parent_source );
  let (goal_list, axes_map)
    : (Vec<ID>, HashMap<ID, MembershipAxes>)
    = match node_changes {
      None => { // No git diff view: Easy.
        let goals : Vec<ID> =
          parent_nodecomplete . all_ids()
            . cloned()
            . collect();
        ( goals, HashMap::new() ) }
      Some (nc) => { // Git diff view.
        // node_changes_for_truenode returns one NodeChanges (preferring
        // unstaged); the membership change is therefore stamped on the
        // unstaged axis. Per-stage merging would happen here if/when
        // node_changes_for_truenode is generalised.
        let mut goals : Vec<ID> = Vec::new();
        let mut amap : HashMap<ID, MembershipAxes> =
          HashMap::new();
        for entry in &nc . ids_diff {
          let (id, m) : (ID, MembershipAxes) = match entry {
            Diff_Item::Unchanged (id) =>
              ( id . clone(), MembershipAxes::default () ),
            Diff_Item::New (id) =>
              ( id . clone(),
                MembershipAxes { staged: None,
                                 unstaged: Some (Sign::Plus) } ),
            Diff_Item::Removed (id) =>
              ( id . clone(),
                MembershipAxes { staged: None,
                                 unstaged: Some (Sign::Minus) } ), };
          goals . push (id . clone ());
          amap . insert (id, m); }
        ( goals, amap ) } };
  let is_id : fn (&ViewNode) -> bool =
    |viewnode| matches!( &viewnode . kind,
                         ViewNodeKind::Scaff( Scaffold::ID { .. } ) );
  let view_id_text : fn (&ViewNode) -> ID =
    |viewnode| match &viewnode . kind {
      ViewNodeKind::Scaff( Scaffold::ID { id, .. } ) =>
        id . clone(),
      _ => unreachable!(), };
  let create_id = |id: &ID| -> ViewNode {
    let membership : MembershipAxes =
      axes_map . get (id) . copied () . unwrap_or_default ();
    viewnode_from_scaffold(
      Scaffold::ID { id: id . clone(), membership } ) };
  complete_relevant_children_in_viewnodetree(
    tree,
    idcol_node_id,
    is_id,
    view_id_text,
    &goal_list,
    create_id )?;
  Ok( () ) }
