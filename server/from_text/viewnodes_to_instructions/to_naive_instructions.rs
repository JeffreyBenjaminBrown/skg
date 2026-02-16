use crate::types::git::NodeDiffStatus;
use crate::types::viewnode::EditRequest;
use crate::types::viewnode::{ViewNode, ViewNodeKind, Scaffold};
use crate::types::misc::ID;
use crate::types::skgnode::SkgNode;
use crate::types::save::{DefineNode, SaveNode, DeleteNode};
use crate::types::tree::generic::read_at_node_in_tree;
use crate::types::tree::viewnode_skgnode::{
  collect_grandchild_aliases_for_viewnode, unique_scaffold_child };
use crate::types::list::dedup_vector;
use ego_tree::{NodeId, NodeRef, Tree};

/// Converts a forest of ViewNodes to DefineNodes, 'naively' --
/// that is, leaving the following work to be handled elsewhere:
/// - reconcile DefineNodes with the same ID
/// - clobber None fields with data from disk
/// (Its caller, 'viewnode_forest_to_nonmerge_save_instructions',
/// does those things.)
pub fn naive_saveinstructions_from_tree (
  mut forest: Tree<ViewNode> // "forest" = tree with BufferRoot
) -> Result<Vec<DefineNode>, String> {

  /// Might appends a DefineNode to 'result', and might recurse.
  /// Skips some nodes, because:
  /// - indefinitive nodes don't generate instructions
  /// - aliases     are handled by
  ///   'collect_grandchild_aliases_for_viewnode'
  /// - subscribees are handled by
  ///   'collect_subscribees'
  fn mauybe_defineonenode_and_maybe_recurse (
    tree    : &mut Tree<ViewNode>,
    node_id : NodeId,
    result  : &mut Vec<DefineNode>
  ) -> Result<(), String> {

    /// Defined separately because it's called in two places.
    fn recurse_on_children (
      tree: &mut Tree<ViewNode>,
      node_id: NodeId,
      result: &mut Vec<DefineNode>
    ) -> Result<(), String> {
      for child_treeid in
        { let child_treeids: Vec<NodeId> =
            tree . get(node_id) . unwrap() . children()
            . map(|c| c.id()) . collect();
          child_treeids }
        { mauybe_defineonenode_and_maybe_recurse(
            tree, child_treeid, result)?; }
      Ok(( )) }

    let node_kind: ViewNodeKind = read_at_node_in_tree(
        tree, node_id, |node| node.kind.clone())?;
    match node_kind {
      ViewNodeKind::Scaff ( Scaffold::BufferRoot ) =>
        recurse_on_children( tree, node_id, result )?,
      ViewNodeKind::Scaff ( Scaffold::SubscribeeCol ) =>
        recurse_on_children( tree, node_id, result )?,
      ViewNodeKind::Scaff ( _ ) => {
        // TODO ? Recurse into more Scaffolds.
      },
      ViewNodeKind::True ( t ) => {
        if let Some(instruction)
          = maybe_instruction_from_treenode( tree, node_id, &t )?
          { result . push(instruction); }
        recurse_on_children( tree, node_id, result )?; }}
    Ok(( )) }

  let mut result: Vec<DefineNode> = Vec::new();
  let root_id : NodeId = forest . root() . id();
  mauybe_defineonenode_and_maybe_recurse (
    &mut forest, root_id, &mut result ) ?;
  Ok (result) }

/// Returns Some(Delete) or Some(Save) for definitive nodes.
/// Returns None for indefinitive nodes.
fn maybe_instruction_from_treenode (
  tree    : &Tree<ViewNode>,
  node_id : NodeId,
  t       : &crate::types::viewnode::TrueNode
) -> Result<Option<DefineNode>, String> {
  if t.indefinitive { return Ok(None); }
  if t.edit_request == Some(EditRequest::Delete) {
    return Ok(Some(DefineNode::Delete(DeleteNode {
      id     : t.id    .clone(),
      source : t.source.clone() } )) ); }
  let node_ref : NodeRef<ViewNode> =
    tree . get(node_id) . ok_or(
      "maybe_instruction_from_treenode: node not found")?;
  Ok(Some(DefineNode::Save(SaveNode(SkgNode {
    title:   t.title.clone(),
    aliases: collect_grandchild_aliases_for_viewnode(
      tree, node_id)?,
    source:  t.source.clone(),
    ids:     vec![t.id.clone()],
    body:    t.body.clone(),
    contains: Some(
      collect_contents_to_save_from_children(&node_ref) ),
    subscribes_to:
      collect_subscribees( tree, node_id )?,
    hides_from_its_subscriptions: None,
    overrides_view_of: None } )) )) }

/// Treats the input tree as the source of truth; does not read dbs.
/// Returns None if no SubscribeeCol found,
///   because in this case the user has expressed no opinion.
/// Returns Some(vec) if SubscribeeCol found.
///   Empty means the user wants no subscribees.
///   Deduplicates the output, preserving order of first occurrence.
fn collect_subscribees (
  tree: &Tree<ViewNode>,
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
        let col_ref : NodeRef<ViewNode> = tree.get(col_id).expect(
          "collect_subscribees: SubscribeeCol not found");
        let mut subscribees : Vec<ID> = Vec::new();
        for subscribeecol_child in col_ref.children() {
          let child_node : &ViewNode = subscribeecol_child.value();
          match &child_node.kind {
            ViewNodeKind::True(t) => {
              if !t.parent_ignores {
                subscribees.push(t.id.clone()); }},
            ViewNodeKind::Scaff(Scaffold::HiddenOutsideOfSubscribeeCol) =>
              continue, // valid child of SubscribeeCol, but not a subscribee
            ViewNodeKind::Scaff(s) => return Err(format!( "SubscribeeCol has unexpected Scaffold child: {:?}", s)), }}
        subscribees };
      Ok( Some(dedup_vector(subscribees)) ) }} }

/// The following kinds of TrueNode children
/// should be excluded from their parent's content:
/// - anything marked parentIgnores
/// - any phantom content ('Removed' or 'RemovedHere')
/// - anything about to be deleted
fn collect_contents_to_save_from_children<'a> (
  node_ref: &NodeRef<'a, ViewNode>
) -> Vec<ID> {
  let mut contents: Vec<ID> =
    Vec::new();
  for child_ref in node_ref.children() {
    let child : &ViewNode = child_ref . value();
    if let ViewNodeKind::True(t) = &child.kind {
      let is_phantom : bool =
        // In diff view, skip phantom (Removed/RemovedHere) nodes
        matches!( t . diff,
                  Some(NodeDiffStatus::Removed) |
                  Some(NodeDiffStatus::RemovedHere) );
      if ! t.parent_ignores
         && ! is_phantom
         && ! matches!( t . edit_request,
                        Some(EditRequest::Delete))
      { contents.push( t.id.clone() ); }} }
  contents }
