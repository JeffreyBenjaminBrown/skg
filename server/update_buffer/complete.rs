use crate::types::orgnode::{OrgNode, OrgNodeKind, Scaffold};
use crate::types::tree::generic::read_at_ancestor_in_tree;
use crate::types::misc::ID;
use crate::types::skgnode::SkgNode;
use crate::types::skgnodemap::SkgNodeMap;
use crate::update_buffer::util::complete_relevant_children_in_orgnodetree;
use ego_tree::{NodeId, Tree};
use std::error::Error;

/// Reconciles an AliasCol's children against
///   the aliases on disk (via the map) for its parent TrueNode.
///
/// Per the spec in buffer-update.org:
/// - Verify this node is an AliasCol
/// - Verify its parent is a TrueNode
/// - Fetch the corresponding SkgNode from the map
/// - Read its aliases into 'aliases'
/// - Partition the AliasCol's children into:
///   - TrueNodes with parent_ignores=true
///   - Alias scaffold nodes
///   (Error if any child does not fit these categories.)
/// - Reorder children: ignored TrueNodes first, then Alias nodes
/// - Among the Alias children, discard any not in 'aliases'
/// - Create new Alias nodes for values in 'aliases' not already present
/// - Order the final Alias children to match the order in 'aliases'
pub fn completeAliasCol (
  tree             : &mut Tree<OrgNode>,
  map              : &SkgNodeMap,
  aliascol_node_id : NodeId,
) -> Result<(), Box<dyn Error>> {
  { let is_aliascol : bool = // barf if not an aliascol
      read_at_ancestor_in_tree(
        tree, aliascol_node_id, 0,
        |orgnode| matches!( &orgnode.kind,
                            OrgNodeKind::Scaff( Scaffold::AliasCol )) )
      .map_err( |e| -> Box<dyn Error> { e.into() } )?;
    if !is_aliascol { return Err(
      "completeAliasCol: Node is not an AliasCol".into() ); }}
  let parent_id : ID =
    read_at_ancestor_in_tree(
      // Verify parent is a TrueNode and get its ID
      tree, aliascol_node_id, 1,
      |orgnode| match &orgnode.kind {
        OrgNodeKind::True( t ) => Ok( t.id.clone() ),
        OrgNodeKind::Scaff( _ ) =>
          Err( "completeAliasCol: Parent is not a TrueNode" ), } )
    .map_err( |e| -> Box<dyn Error> { e.into() } )?
    .map_err( |e| -> Box<dyn Error> { e.into() } )? ;
  let parent_skgnode : &SkgNode =
    map.get( &parent_id )
    .ok_or( "completeAliasCol: Parent SkgNode not in map" )?;
  let aliases : Vec<String> = // Source of truth from disk
    parent_skgnode.aliases.clone().unwrap_or_default();
  let is_alias : fn(&OrgNode) -> bool =
    // relevance to complete_relevant_children
    |orgnode| matches!( &orgnode.kind,
                        OrgNodeKind::Scaff( Scaffold::Alias { .. } ) );
  let view_alias_text : fn(&OrgNode) -> String =
    |orgnode| match &orgnode.kind {
      OrgNodeKind::Scaff( Scaffold::Alias { text, .. } ) =>
        text.clone(),
      _ => unreachable!(), }; // relevance means Scaffold::Alias
  let create_alias : fn(&String) -> OrgNode =
    |text| OrgNode {
      focused : false,
      folded  : false,
      kind    : OrgNodeKind::Scaff(
        Scaffold::Alias { text : text.clone(),
                          diff : None } ), };
  complete_relevant_children_in_orgnodetree(
    tree,
    aliascol_node_id,
    is_alias,
    view_alias_text,
    &aliases,
    create_alias )?;
  Ok( () ) }
