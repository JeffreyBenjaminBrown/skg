use crate::types::git::{RelationshipAxes, NodeChanges, SkgrepoDiff, axes_from_per_stage_diffs, per_stage_node_changes_for_unrestrictedVognode};
use crate::dbs::node_lookup::graphnode_graphFirst_by_pid_and_skgrepo;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::{ID, SkgConfig, SkgrepoName, members_of};
use crate::types::nodes::complete::Graphnode;
use crate::types::viewnode::{Viewnode, ViewnodeKind, AffectsParent};
use crate::types::viewnode::{Vognode, PropertyFolder, Property};
use crate::types::tree::generic::read_at_ancestor_in_tree;
use crate::update_buffer::ancestry::pid_and_skgrepo_from_required_ancestor;
use crate::update_buffer::util::{complete_relevant_children_in_viewforest, treat_certain_children};
use ego_tree::{NodeId, Tree};
use std::collections::HashMap;
use std::error::Error;

/// Reconciles an AliasFolder's children against
///   the aliases on disk (via the map) for its parent UnrestrictedVognode.
///
/// Per the spec in buffer-update.org:
/// - Verify this node is an AliasFolder
/// - Verify its parent is an UnrestrictedVognode
/// - Fetch the corresponding Graphnode from the map
/// - Read its aliases into 'aliases'
/// - Partition the AliasFolder's children into:
///   - UnrestrictedVognodes with affectsParent != True
///   - Alias property nodes
///   (Error if any child does not fit these categories.)
/// - Reorder children: ignored UnrestrictedVognodes first, then Alias nodes
/// - Among the Alias children, discard any not in 'aliases'
/// - Create new Alias nodes for values in 'aliases' not already present
/// - Order the final Alias children to match the order in 'aliases'
pub fn reconcile_aliasFolder_children (
  tree               : &mut Tree<Viewnode>,
  aliasfolder_treeid : NodeId,
  graph              : &InRustGraph,
  skgrepo_diffs      : &Option<HashMap<SkgrepoName, SkgrepoDiff>>,
  config             : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  { let is_aliasFolder : bool = // barf if not an aliasFolder
      read_at_ancestor_in_tree(
        tree, aliasfolder_treeid, 0,
        |viewnode| matches!( &viewnode . kind,
                            ViewnodeKind::PropertyFolder (PropertyFolder::Alias)) )
      . map_err( |e| -> Box<dyn Error> { e . into() } )?;
    if !is_aliasFolder { return Err(
      "reconcile_aliasFolder_children: Node is not an AliasFolder" . into() ); }}
  // TODO/DONE/local-view-update/propagate-death-leafward/plan.org §4: parent Unrestricted vognode read through the TODO/DONE/local-view-update/propagate-death-leafward/plan.org §3 ancestry table (index 0).
  let (parent_pid, parent_skgrepo) : (ID, SkgrepoName) =
    pid_and_skgrepo_from_required_ancestor(
      tree, aliasfolder_treeid, 0,
      "reconcile_aliasFolder_children" ) ?;
  let parent_graphnode : Graphnode =
    graphnode_graphFirst_by_pid_and_skgrepo (
      graph, config, &parent_pid, &parent_skgrepo )
    . map_err ( |_| "reconcile_aliasFolder_children: parent Graphnode not found" ) ?;
  let alias_relRepos : HashMap<String, SkgrepoName> =
    parent_graphnode . aliases . or_default () . iter ()
    .map ( |alias| (alias . member . clone (), alias . relRepo . clone ()) )
    .collect ();
  let (staged_nc, unstaged_nc)
    : (Option<&NodeChanges>, Option<&NodeChanges>) =
    per_stage_node_changes_for_unrestrictedVognode (
      skgrepo_diffs, &parent_pid, &parent_skgrepo );
  let (goal_list, axes_map)
    : (Vec<String>, HashMap<String, RelationshipAxes>) =
    if staged_nc . is_none () && unstaged_nc . is_none () {
      let goals : Vec<String> =
        members_of ( parent_graphnode . aliases . or_default() );
      ( goals, HashMap::new() )
    } else {
      let merged : Vec<(String, RelationshipAxes)> =
        axes_from_per_stage_diffs (
          staged_nc   . map ( |c| c . aliases_diff . as_slice () ),
          unstaged_nc . map ( |c| c . aliases_diff . as_slice () ) );
      let goals : Vec<String> =
        merged . iter () . map ( |(t, _)| t . clone () ) . collect ();
      let amap : HashMap<String, RelationshipAxes> =
        merged . into_iter () . collect ();
      ( goals, amap ) };
  let is_alias : fn (&Viewnode) -> bool =
    // relevance to complete_relevant_children
    |viewnode| matches!( &viewnode . kind,
                        ViewnodeKind::Property (Property::Alias { .. } ) );
  let view_alias_text : fn (&Viewnode) -> Result<String, String> =
    |viewnode| match &viewnode . kind {
      ViewnodeKind::Property (Property::Alias { text, .. } ) =>
        Ok ( text . clone() ),
      _ => Err ( "reconcile_aliasFolder_children: relevant child is not an alias"
                 . to_string() ), }; // relevance means Property::Alias
  let create_alias = |text: &String| -> Result<Viewnode, String> {
    let relationship_axes : RelationshipAxes =
      axes_map . get (text) . copied () . unwrap_or_default ();
    Ok ( Viewnode {
      focused     : false,
      folded      : false,
      body_folded : false,
      kind : ViewnodeKind::Property (Property::Alias { text : text . clone(),
                                               relRepo : alias_relRepos
                                                 . get (text)
                                                 . and_then ( |relRepo|
                                                   if relRepo == &parent_graphnode . home_skgrepo { None }
                                                   else { Some (relRepo . clone ()) } ),
                                               relRepo_request : None,
                                               relationship_axes } ), })};
  complete_relevant_children_in_viewforest(
    tree,
    aliasfolder_treeid,
    is_alias,
    view_alias_text,
    &goal_list,
    create_alias )?;
  treat_certain_children(
      // Currently unreachable: validation rejects unrestricted vognode
      // children of AliasFolder. If that is later relaxed, only Unrestricted
      // Vognodes need repair; affectsParent is a vestigial field in Phantoms.
      tree, aliasfolder_treeid,
      |vn : &Viewnode| matches!( &vn . kind,
                                  ViewnodeKind::Vognode (Vognode::Unrestricted (_)) ),
      |vn : &mut Viewnode| {
        if let ViewnodeKind::Vognode (Vognode::Unrestricted ( ref mut t ))
          = vn . kind
          { t . affectsParent = AffectsParent::False; }},
    ) . map_err( |e| -> Box<dyn Error> { e . into() } )?;
  Ok( () ) }
