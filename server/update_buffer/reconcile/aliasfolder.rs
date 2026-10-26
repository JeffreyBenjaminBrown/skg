use crate::dbs::node_lookup::graphnode_graphFirst_by_pid_and_skgrepo;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::{ID, SkgConfig, SkgrepoName, members_of};
use crate::types::nodes::complete::Graphnode;
use crate::types::viewnode::{Viewnode, ViewnodeKind};
use crate::types::viewnode::{Vognode, PropertyFolder, PropertyMember, viewnode_of};
use crate::types::tree::generic::read_at_ancestor_in_tree;
use crate::update_buffer::ancestry::pid_and_skgrepo_from_required_ancestor;
use crate::update_buffer::util::{KeptChildOrder, complete_managed_children_in_viewforest, treat_certain_children};
use ego_tree::{NodeId, Tree};
use std::collections::HashMap;
use std::error::Error;

/// Reconciles an AliasFolder's children against
///   the aliases on disk (via the map) for its parent UnrestrictedVognode.
///
/// In order:
/// - Verify this node is an AliasFolder
/// - Verify its parent is an UnrestrictedVognode
/// - Fetch the corresponding Graphnode from the map
/// - Read its aliases into 'aliases'
/// - Partition the AliasFolder's children into:
///   - UnrestrictedVognodes that are non-members
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
  // The parent Unrestricted vognode, read through the required-ancestry table of 'update_buffer/ancestry.rs' (index 0).
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
  let goal_list : Vec<String> =
    members_of ( parent_graphnode . aliases . or_default() );
  let is_alias : fn (&Viewnode) -> bool =
    // managedness for complete_managed_children
    |viewnode| matches!( &viewnode . kind,
                        ViewnodeKind::PropertyMember (PropertyMember::Alias { .. } ) );
  let view_alias_text : fn (&Viewnode) -> Result<String, String> =
    |viewnode| match &viewnode . kind {
      ViewnodeKind::PropertyMember (PropertyMember::Alias { text, .. } ) =>
        Ok ( text . clone() ),
      _ => Err ( "reconcile_aliasFolder_children: managed child is not an alias"
                 . to_string() ), }; // managed means PropertyMember::Alias
  let create_alias = |text: &String| -> Result<Viewnode, String> {
    Ok ( viewnode_of (ViewnodeKind::PropertyMember (PropertyMember::Alias { text : text . clone(),
                                               relRepo : alias_relRepos
                                                 . get (text)
                                                 . and_then ( |relRepo|
                                                   if relRepo == &parent_graphnode . home_skgrepo { None }
                                                   else { Some (relRepo . clone ()) } ),
                                               relationshipEditRequest : None } )))};
  complete_managed_children_in_viewforest(
    tree,
    aliasfolder_treeid,
    KeptChildOrder::FollowsGoal, // the save writes the aliases' order
    is_alias,
    view_alias_text,
    &goal_list,
    create_alias )?;
  treat_certain_children(
      // Validation accepts an unrestricted vognode child of an AliasFolder
      // only as a non-member, so this changes nothing today; it would make a
      // member child a non-member. Only UnrestrictedVognodes need it;
      // membership is a vestigial field in Phantoms.
      tree, aliasfolder_treeid,
      |vn : &Viewnode| matches!( &vn . kind,
                                  ViewnodeKind::Vognode (Vognode::Unrestricted (_)) ),
      |vn : &mut Viewnode| {
        if let ViewnodeKind::Vognode (Vognode::Unrestricted ( ref mut t ))
          = vn . kind
          { t . claimsMembership = false; }},
    ) . map_err( |e| -> Box<dyn Error> { e . into() } )?;
  Ok( () ) }
