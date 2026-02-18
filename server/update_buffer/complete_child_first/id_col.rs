use crate::types::git::{FieldDiffStatus, NodeChanges};
use crate::types::list::Diff_Item;
use crate::types::misc::{ID, SourceName};
use crate::types::skgnode::SkgNode;
use crate::types::skgnodemap::SkgNodeMap;
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
/// - Fetch the corresponding SkgNode from the map
/// - Read its IDs into a goal list
/// - In diff view, also build a diff-status map from NodeChanges.ids_diff
/// - Reconcile children via complete_relevant_children_in_viewnodetree
#[allow(non_snake_case)]
pub fn completeIDCol (
  idcol_node_id : NodeId,
  tree          : &mut Tree<ViewNode>,
  map           : &SkgNodeMap,
  source_diffs  : &Option<HashMap<SourceName, SourceDiff>>,
) -> Result<(), Box<dyn Error>> {
  error_unless_node_satisfies(
    tree, idcol_node_id,
    |viewnode| matches!( &viewnode.kind,
                         ViewNodeKind::Scaff( Scaffold::IDCol ) ),
    "completeIDCol: Node is not an IDCol" )
    .map_err( |e| -> Box<dyn Error> { e.into() } )?;
  let (parent_pid, parent_source) : (ID, SourceName) =
    pid_and_source_from_ancestor(
      tree, idcol_node_id, 1,
      "completeIDCol" ) ?;
  let parent_skgnode : &SkgNode =
    map.get( &parent_pid )
    .ok_or( "completeIDCol: Parent SkgNode not in map" )?;
  let node_changes : Option<&NodeChanges> =
    node_changes_for_truenode(
      source_diffs, &parent_pid, &parent_source );
  let (goal_list, diff_map)
    : (Vec<ID>, HashMap<ID, Option<FieldDiffStatus>>)
    = match node_changes {
      None => { // No git diff view: Easy.
        let goals : Vec<ID> =
          parent_skgnode.ids.iter()
            .cloned()
            .collect();
        ( goals, HashMap::new() ) }
      Some( nc ) => { // Git diff view.
        let mut goals : Vec<ID> = Vec::new();
        let mut dmap : HashMap<ID, Option<FieldDiffStatus>> =
          HashMap::new();
        for entry in &nc.ids_diff {
          let (id, diff) : (ID, Option<FieldDiffStatus>) =
            match entry {
              Diff_Item::Unchanged( id ) =>
                ( id.clone(), None ),
              Diff_Item::New( id ) =>
                ( id.clone(), Some( FieldDiffStatus::New ) ),
              Diff_Item::Removed( id ) =>
                ( id.clone(), Some( FieldDiffStatus::Removed ) ), };
          goals.push( id.clone() );
          dmap.insert( id, diff ); }
        ( goals, dmap ) } };
  let is_id : fn(&ViewNode) -> bool =
    |viewnode| matches!( &viewnode.kind,
                         ViewNodeKind::Scaff( Scaffold::ID { .. } ) );
  let view_id_text : fn(&ViewNode) -> ID =
    |viewnode| match &viewnode.kind {
      ViewNodeKind::Scaff( Scaffold::ID { id, .. } ) =>
        id.clone(),
      _ => unreachable!(), };
  let create_id = |id: &ID| -> ViewNode {
    let diff : Option<FieldDiffStatus> =
      diff_map.get( id ).copied().flatten();
    viewnode_from_scaffold(
      Scaffold::ID { id: id.clone(), diff } ) };
  complete_relevant_children_in_viewnodetree(
    tree,
    idcol_node_id,
    is_id,
    view_id_text,
    &goal_list,
    create_id )?;
  Ok( () ) }
