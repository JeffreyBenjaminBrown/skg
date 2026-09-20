use crate::dbs::node_lookup::nodecomplete_from_graph;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::to_org::util::{get_id_from_treenode, remove_completed_view_request};
use crate::types::git::MembershipAxes;
use crate::types::misc::{ID, RelPartner, SkgConfig, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::viewnode::{ViewNode, ViewNodeKind, ViewRequest, FolderRelation};
use crate::types::viewnode::{QualFolder, Qual};
use crate::types::tree::viewnode_nodecomplete::{
  insert_scaffold_as_child, unique_scaffold_child_of_viewnode};

use ego_tree::Tree;
use std::error::Error;

pub fn build_and_integrate_aliases_view_then_drop_request (
  tree          : &mut Tree<ViewNode>,
  node_id       : ego_tree::NodeId,
  graph         : &InRustGraph,
  config        : &SkgConfig,
  errors        : &mut Vec < String >,
) -> Result < (), Box<dyn Error> > {
  let result : Result<(), Box<dyn Error>> =
    build_and_integrate_aliases (
      tree, node_id, graph, config );
  remove_completed_view_request (
    tree, node_id,
    ViewRequest::Folder (FolderRelation::Aliases),
    "Failed to integrate aliases view",
    errors, result ) }

/// Integrate an AliasFolder child with its Alias grandchildren
/// into the ViewNode tree containing the target node.
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
  tree      : &mut Tree<ViewNode>,
  node_id   : ego_tree::NodeId,
  graph     : &InRustGraph,
  _config    : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  let node_id_val : ID =
    get_id_from_treenode ( tree, node_id ) ?;
  if unique_scaffold_child_of_viewnode (
    tree, node_id,
    &ViewNodeKind::QualFolder (QualFolder::Alias) )? . is_some ()
  { // If it already has an AliasFolder child,
    // then reconcile_aliasFolder_children (in update_buffer) already handled it.
    return Ok (( )); }
  let node : Option<NodeComplete> =
    nodecomplete_from_graph (graph, &node_id_val);
  let home : Option<SourceName> =
    node . as_ref () . map ( |node| node . source . clone () );
  let aliases : Vec<RelPartner<String>> = node
    . map ( |node| node . aliases . or_default () . to_vec () )
    . unwrap_or_default ();
  let aliasfolder_id : ego_tree::NodeId =
    insert_scaffold_as_child ( tree, node_id,
      ViewNodeKind::QualFolder (QualFolder::Alias), true ) ?;
  for alias in & aliases {
    insert_scaffold_as_child (
      tree, aliasfolder_id,
      ViewNodeKind::Qual (
        Qual::Alias { text: alias . member . clone (),
                      relSource: home . as_ref ()
                        .and_then ( |home|
                          if &alias . relSource == home { None }
                          else { Some (alias . relSource . clone ()) } ),
                      relSource_request: None,
                      membership: MembershipAxes::default () } ),
      false ) ?; }
  Ok (( )) }
