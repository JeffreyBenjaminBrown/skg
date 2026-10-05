use crate::dbs::node_lookup::graphnode_from_graph;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::to_org::util::{get_skgid_from_viewnode_at, remove_completed_view_request};
use crate::types::git::RelationshipAxes;
use crate::types::misc::{ID, RelPartner, SkgConfig, SkgRepoName};
use crate::types::nodes::complete::Graphnode;
use crate::types::viewnode::{Viewnode, ViewnodeKind, ViewRequest, FolderRelation};
use crate::types::viewnode::{PropertyFolder, Property};
use crate::types::tree::viewnode_graphnode::{
  insert_non_vognode_as_child, unique_non_vognode_child_of_viewnode};

use ego_tree::Tree;
use std::error::Error;

pub fn build_and_integrate_aliases_view_then_drop_request (
  tree          : &mut Tree<Viewnode>,
  treeid        : ego_tree::NodeId,
  graph         : &InRustGraph,
  config        : &SkgConfig,
  errors        : &mut Vec < String >,
) -> Result < (), Box<dyn Error> > {
  let result : Result<(), Box<dyn Error>> =
    build_and_integrate_aliases (
      tree, treeid, graph, config );
  remove_completed_view_request (
    tree, treeid,
    ViewRequest::Folder (FolderRelation::Aliases),
    "Failed to integrate aliases view",
    errors, result ) }

/// Integrate an AliasFolder child with its Alias grandchildren
/// into the Viewnode tree containing the target node.
///
/// PITFALL: This function fetches aliases from disk and
/// populates them immediately, whereas 'reconcile_aliasFolder_children' (in
/// update_buffer) is only called on an AliasFolder already in the tree.
/// These two distinct ways of populating an AliasFolder are necessary,
/// because in 'complete_or_restore_each_node_in_branch',
/// view requests are only processed AFTER recursing to children
/// (for reasons explained in that function's header comment),
/// so any newly-created empty AliasFolder
/// would not be visited in the same save cycle.
pub fn build_and_integrate_aliases (
  tree      : &mut Tree<Viewnode>,
  treeid    : ego_tree::NodeId,
  graph     : &InRustGraph,
  _config    : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  let node_id_val : ID =
    get_skgid_from_viewnode_at ( tree, treeid ) ?;
  if unique_non_vognode_child_of_viewnode (
    tree, treeid,
    &ViewnodeKind::PropertyFolder (PropertyFolder::Alias) )? . is_some ()
  { // If it already has an AliasFolder child,
    // then reconcile_aliasFolder_children (in update_buffer) already handled it.
    return Ok (( )); }
  let node : Option<Graphnode> =
    graphnode_from_graph (graph, &node_id_val);
  let home : Option<SkgRepoName> =
    node . as_ref () . map ( |node| node . home_skgrepo . clone () );
  let aliases : Vec<RelPartner<String>> = node
    . map ( |node| node . aliases . or_default () . to_vec () )
    . unwrap_or_default ();
  let aliasfolder_skgid : ego_tree::NodeId =
    insert_non_vognode_as_child ( tree, treeid,
      ViewnodeKind::PropertyFolder (PropertyFolder::Alias), true ) ?;
  for alias in & aliases {
    insert_non_vognode_as_child (
      tree, aliasfolder_skgid,
      ViewnodeKind::Property (
        Property::Alias { text: alias . member . clone (),
                      relRepo: home . as_ref ()
                        .and_then ( |home|
                          if &alias . relRepo == home { None }
                          else { Some (alias . relRepo . clone ()) } ),
                      relRepo_request: None,
                      relationship_axes: RelationshipAxes::default () } ),
      false ) ?; }
  Ok (( )) }
