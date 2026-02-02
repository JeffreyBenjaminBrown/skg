use crate::types::git::NodeDiffStatus;
use crate::types::orgnode::EditRequest;
use crate::types::orgnode::{OrgNode, OrgNodeKind, TrueNode, Scaffold};
use crate::types::misc::ID;
use crate::types::skgnode::SkgNode;
use crate::types::save::{DefineOneNode, SaveSkgnode, DeleteSkgnode};
use crate::types::tree::generic::read_at_node_in_tree;
use crate::types::tree::orgnode_skgnode::{
  collect_grandchild_aliases_for_orgnode, unique_scaffold_child };
use crate::util::dedup_vector;
use ego_tree::{NodeId, NodeRef, Tree};

/// Converts a forest of OrgNodes to DefineOneNodes,
/// taking them all at face value. In particular,
/// it does *not*:
/// - reconcile DefineOneNodes with the same ID
/// - clobber None fields with data from disk
/// (Its caller 'orgnode_forest_to_nonmerge_save_instructions' does.)
pub fn naive_saveinstructions_from_forest (
  mut forest: Tree<OrgNode> // "forest" = tree with BufferRoot
) -> Result<Vec<DefineOneNode>, String> {
  let mut result: Vec<DefineOneNode> =
    Vec::new();
  let root_id : NodeId = forest.root().id();
  naive_saveinstructions_from_tree ( &mut forest, root_id,
                                     &mut result )?;
  Ok(result) }

/// Appends another DefineOneNode to 'result' and recurses (DFS order).
/// Skips some nodes, because:
/// - indefinitive nodes don't generate instructions
/// - aliases     are handled by
///   'collect_grandchild_aliases_for_orgnode'
/// - subscribees are handled by
///   'collect_subscribees'
fn naive_saveinstructions_from_tree(
  tree: &mut Tree<OrgNode>,
  node_id: NodeId,
  result: &mut Vec<DefineOneNode>
) -> Result<(), String> {
  fn recurse ( // because it's called in two places
    tree: &mut Tree<OrgNode>,
    node_id: NodeId,
    result: &mut Vec<DefineOneNode>
  ) -> Result<(), String> {
    for child_treeid in {
      let child_treeids: Vec<NodeId> =
        tree.get(node_id).unwrap().children()
        .map(|c| c.id()).collect();
      child_treeids }
    { naive_saveinstructions_from_tree(
        tree, child_treeid, result)?; }
    Ok(( )) }
  let node_kind: OrgNodeKind =
    read_at_node_in_tree(tree, node_id, |node| node.kind.clone())?;
  match node_kind {
    OrgNodeKind::Scaff ( Scaffold::BufferRoot ) =>
      recurse( tree, node_id, result )?,
    OrgNodeKind::Scaff ( _ ) => {
      // Other scaffolds currently produce no DefineOneNodes.
      // TODO: Recurse into SubscribeeCols.
    },
    OrgNodeKind::True ( t ) => {
      if ! t.indefinitive {
        let instruction: DefineOneNode =
          if t.edit_request == Some(EditRequest::Delete)
          { DefineOneNode::Delete(DeleteSkgnode {
              id: t.id.clone(),
              source: t.source.clone() })
          } else {
            let aliases: Option<Vec<String>> =
              collect_grandchild_aliases_for_orgnode(tree, node_id)?;
            let subscribees: Option<Vec<ID>> =
              collect_subscribees(tree, node_id)?;
            let node_ref: NodeRef<OrgNode> = tree.get(node_id).ok_or(
              "saveinstructions_from_tree: node not found")?;
            let skg_node: SkgNode = skgnode_for_orgnode_in_tree(
              node_ref.value(), &node_ref, aliases, subscribees)?;
            DefineOneNode::Save(SaveSkgnode(skg_node)) };
        result.push(instruction); }
      recurse( tree, node_id, result )?; }}
  Ok(( )) }

fn skgnode_for_orgnode_in_tree<'a> (
  orgnode: &OrgNode,
  noderef: &NodeRef<'a, OrgNode>, // the same node, but in the tree
  aliases: Option<Vec<String>>,
  subscribees: Option<Vec<ID>>,
) -> Result<SkgNode, String> {
  let t : &TrueNode = match &orgnode.kind {
    OrgNodeKind::True(t) => t,
    OrgNodeKind::Scaff(_) => return Err(
      "skgnode_for_orgnode_in_tree: expected TrueNode, got Scaffold"
      . to_string()) };
  Ok ( SkgNode {
    title: t.title.clone(),
    aliases,
    source: t.source.clone(),
    ids: vec![t.id.clone()],
    body: t.body.clone(),
    contains: Some(collect_contents_that_are_not_to_delete(noderef)),
    subscribes_to: subscribees,
    hides_from_its_subscriptions: None,
    overrides_view_of: None,
  }) }

/// Treats the input tree as the source of truth; does not read dbs.
/// Returns None if no SubscribeeCol found,
///   because in this case the user has expressed no opinion.
/// Returns Some(vec) if SubscribeeCol found.
///   Empty means the user wants no subscribees.
///   Deduplicates the output, preserving order of first occurrence.
fn collect_subscribees (
  tree: &Tree<OrgNode>,
  node_id: NodeId,
) -> Result<Option<Vec<ID>>, String> {
  let subscribee_col_id : Option<NodeId> =
    unique_scaffold_child (
      tree, node_id, &Scaffold::SubscribeeCol )
    . map_err ( |e| e.to_string() ) ?;
  match subscribee_col_id {
    None => Ok(None),
    Some(col_id) => {
      let subscribees : Vec<ID> = {
        let col_ref : NodeRef<OrgNode> = tree.get(col_id).expect(
          "collect_subscribees: SubscribeeCol not found");
        let mut subscribees : Vec<ID> = Vec::new();
        for subscribeecol_child in col_ref.children() {
          let child_node : &OrgNode = subscribeecol_child.value();
          match &child_node.kind {
            OrgNodeKind::True(t) => {
              if !t.parent_ignores {
                subscribees.push(t.id.clone()); }},
            OrgNodeKind::Scaff(Scaffold::HiddenOutsideOfSubscribeeCol) =>
              continue, // valid child of SubscribeeCol, but not a subscribee
            OrgNodeKind::Scaff(s) => return Err(format!( "SubscribeeCol has unexpected Scaffold child: {:?}", s)), }}
        subscribees };
      Ok(Some(dedup_vector(subscribees))) }} }

/// Returns IDs of all children for which treatment = Content.
/// Not a recursive traversal;
///   it is only concerned with this node's contents.
fn collect_contents_that_are_not_to_delete<'a> (
  node_ref: &NodeRef<'a, OrgNode>
) -> Vec<ID> {
  let mut contents: Vec<ID> =
    Vec::new();
  for child_ref in node_ref.children() {
    let child : &OrgNode = child_ref . value();
    if let OrgNodeKind::True(t) = &child.kind {
      let is_phantom : bool =
        // In diff view, skip phantom (Removed/RemovedHere) nodes
        matches!( t . diff,
                  Some(NodeDiffStatus::Removed) |
                  Some(NodeDiffStatus::RemovedHere) );
      if !t.parent_ignores
         && !is_phantom
         && ! matches!( t . edit_request,
                        Some(EditRequest::Delete))
      { contents.push(t.id.clone()); } } }
  contents }
