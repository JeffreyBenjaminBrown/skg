use crate::types::git::{MembershipAxes, NodeChanges, SourceDiff, axes_from_per_stage_diffs, per_stage_node_changes_for_truenode};
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_source;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::viewnode::{ViewNode, ViewNodeKind, ParentIs};
use crate::types::viewnode::{Vognode, QualCol, Qual};
use crate::types::tree::generic::read_at_ancestor_in_tree;
use crate::update_buffer::ancestry::pid_and_source_from_required_ancestor;
use crate::update_buffer::util::{complete_relevant_children_in_viewnodetree, treat_certain_children};
use ego_tree::{NodeId, Tree};
use std::collections::HashMap;
use std::error::Error;

/// Reconciles an AliasCol's children against
///   the aliases on disk (via the map) for its parent TrueNode.
///
/// Per the spec in buffer-update.org:
/// - Verify this node is an AliasCol
/// - Verify its parent is a TrueNode
/// - Fetch the corresponding NodeComplete from the map
/// - Read its aliases into 'aliases'
/// - Partition the AliasCol's children into:
///   - TrueNodes with parentIs != Affected
///   - Alias scaffold nodes
///   (Error if any child does not fit these categories.)
/// - Reorder children: ignored TrueNodes first, then Alias nodes
/// - Among the Alias children, discard any not in 'aliases'
/// - Create new Alias nodes for values in 'aliases' not already present
/// - Order the final Alias children to match the order in 'aliases'
pub fn reconcile_alias_col_children (
  tree             : &mut Tree<ViewNode>,
  aliascol_node_id : NodeId,
  source_diffs     : &Option<HashMap<SourceName, SourceDiff>>,
  config           : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  { let is_aliascol : bool = // barf if not an aliascol
      read_at_ancestor_in_tree(
        tree, aliascol_node_id, 0,
        |viewnode| matches!( &viewnode . kind,
                            ViewNodeKind::QualCol (QualCol::Alias)) )
      . map_err( |e| -> Box<dyn Error> { e . into() } )?;
    if !is_aliascol { return Err(
      "reconcile_alias_col_children: Node is not an AliasCol" . into() ); }}
  // TODO/DONE/local-view-update/propagate-death-leafward/plan.org §4: parent Normal vognode read through the TODO/DONE/local-view-update/propagate-death-leafward/plan.org §3 ancestry table (index 0).
  let (parent_pid, parent_source) : (ID, SourceName) =
    pid_and_source_from_required_ancestor(
      tree, aliascol_node_id, 0,
      "reconcile_alias_col_children" ) ?;
  let parent_nodecomplete : NodeComplete =
    nodecomplete_rustFirst_by_pid_and_source (
      config, &parent_pid, &parent_source )
    . map_err ( |_| "reconcile_alias_col_children: parent NodeComplete not found" ) ?;
  let (staged_nc, unstaged_nc)
    : (Option<&NodeChanges>, Option<&NodeChanges>) =
    per_stage_node_changes_for_truenode (
      source_diffs, &parent_pid, &parent_source );
  let (goal_list, axes_map)
    : (Vec<String>, HashMap<String, MembershipAxes>) =
    if staged_nc . is_none () && unstaged_nc . is_none () {
      let goals : Vec<String> =
        parent_nodecomplete . aliases . or_default() . to_vec();
      ( goals, HashMap::new() )
    } else {
      let merged : Vec<(String, MembershipAxes)> =
        axes_from_per_stage_diffs (
          staged_nc   . map ( |c| c . aliases_diff . as_slice () ),
          unstaged_nc . map ( |c| c . aliases_diff . as_slice () ) );
      let goals : Vec<String> =
        merged . iter () . map ( |(t, _)| t . clone () ) . collect ();
      let amap : HashMap<String, MembershipAxes> =
        merged . into_iter () . collect ();
      ( goals, amap ) };
  let is_alias : fn (&ViewNode) -> bool =
    // relevance to complete_relevant_children
    |viewnode| matches!( &viewnode . kind,
                        ViewNodeKind::Qual (Qual::Alias { .. } ) );
  let view_alias_text : fn (&ViewNode) -> String =
    |viewnode| match &viewnode . kind {
      ViewNodeKind::Qual (Qual::Alias { text, .. } ) =>
        text . clone(),
      _ => unreachable!(), }; // relevance means Qual::Alias
  let create_alias = |text: &String| -> ViewNode {
    let membership : MembershipAxes =
      axes_map . get (text) . copied () . unwrap_or_default ();
    ViewNode {
      focused     : false,
      folded      : false,
      body_folded : false,
      kind : ViewNodeKind::Qual (Qual::Alias { text : text . clone(),
                                               membership } ), }};
  complete_relevant_children_in_viewnodetree(
    tree,
    aliascol_node_id,
    is_alias,
    view_alias_text,
    &goal_list,
    create_alias )?;
  treat_certain_children(
      // Currently unreachable: validation rejects normal vognode
      // children of AliasCol. If that is later relaxed, only Normal
      // Vognodes need repair; parentIs is a vestigial field in Phantoms.
      tree, aliascol_node_id,
      |vn : &ViewNode| matches!( &vn . kind,
                                  ViewNodeKind::Vognode (Vognode::Active (_)) ),
      |vn : &mut ViewNode| {
        if let ViewNodeKind::Vognode (Vognode::Active ( ref mut t ))
          = vn . kind
          { t . parentIs = ParentIs::Independent; }},
    ) . map_err( |e| -> Box<dyn Error> { e . into() } )?;
  Ok( () ) }
