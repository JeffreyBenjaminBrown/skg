use crate::types::orgnode::EditRequest;
use crate::types::orgnode::{OrgNode, OrgNodeKind, ScaffoldKind, EffectOnParent};
use crate::types::misc::ID;
use crate::types::skgnode::SkgNode;
use crate::types::save::{NonMerge_NodeAction, SaveInstruction};
use crate::types::tree::generic::read_at_node_in_tree;
use crate::types::tree::orgnode_skgnode::{
  collect_grandchild_aliases_for_orgnode, unique_orgnode_scaffold_child };
use crate::util::dedup_vector;
use ego_tree::{NodeId, NodeRef, Tree};

/// Converts a forest of OrgNodes to SaveInstructions,
/// taking them all at face value.
///
/// PITFALL: Leaves important work undone,
/// which its caller 'orgnodes_to_reconciled_save_instructions'
/// does after calling it.
pub fn naive_saveinstructions_from_forest (
  mut forest: Tree<OrgNode> // "forest" = tree with ForestRoot
) -> Result<Vec<SaveInstruction>, String> {
  let mut result: Vec<SaveInstruction> =
    Vec::new();
  let root_id = forest.root().id();
  naive_saveinstructions_from_tree ( &mut forest, root_id,
                                     &mut result )?;
  Ok(result) }

/// Appends another pair to 'result' and recurses (in DFS order).
/// Skips some nodes, because:
/// - indefinitive nodes don't generate instructions
/// - aliases     are handled by 'collect_aliases'
/// - subscribees are handled by 'collect_subscribees'
fn naive_saveinstructions_from_tree(
  tree: &mut Tree<OrgNode>,
  node_id: NodeId,
  result: &mut Vec<SaveInstruction>
) -> Result<(), String> {
  let (node_kind, is_indefinitive): (OrgNodeKind, bool) =
    read_at_node_in_tree(tree, node_id, |node| {
      ( node.kind.clone(),
        node.is_indefinitive() )
    })?;
  // Handle ForestRoot: just recurse to children
  if matches!(&node_kind, OrgNodeKind::Scaff(s) if s.kind == ScaffoldKind::ForestRoot) {
    for child_treeid in {
      let child_treeids: Vec<NodeId> =
        tree . get(node_id) . unwrap() . children()
        . map( |c| c.id() )
        . collect();
      child_treeids }
    { naive_saveinstructions_from_tree (
        tree, child_treeid, result)?; }
    return Ok(( )); }
  // Skip all other scaffolds - they don't generate SaveInstructions
  if matches!(&node_kind, OrgNodeKind::Scaff(_)) {
    return Ok(()); }
  let aliases =
    collect_grandchild_aliases_for_orgnode(tree, node_id)?;
  let subscribees =
    collect_subscribees(tree, node_id)?;
  let skg_node_opt = if !is_indefinitive {
    let node_ref = tree.get(node_id) . ok_or(
      "saveinstructions_from_tree: node not found after deletion")?;
    Some(skgnode_for_orgnode_in_tree(
      node_ref.value(), &node_ref, aliases, subscribees )?)
  } else { None };
  if let Some(skg_node) = skg_node_opt {
    // Push SaveInstruction if applicable
    result.push((skg_node, {
      let save_action: NonMerge_NodeAction =
        read_at_node_in_tree(tree, node_id, |node| {
          if matches!( node.edit_request(),
                       Some(EditRequest::Delete) )
          { NonMerge_NodeAction::Delete
          } else { NonMerge_NodeAction::Save } })?;
      save_action } )); }
  { // recurse
    for child_treeid in {
      let child_treeids: Vec<NodeId> =
        tree . get(node_id) . unwrap() . children()
        . map(|c| c.id()) . collect();
      child_treeids }
    { naive_saveinstructions_from_tree(
        tree, child_treeid, result )?; }}
  Ok (( )) }

fn skgnode_for_orgnode_in_tree<'a> (
  orgnode: &OrgNode,
  noderef: &NodeRef<'a, OrgNode>, // the same node, but in the tree
  aliases: Option<Vec<String>>,
  subscribees: Option<Vec<ID>>,
) -> Result<SkgNode, String> {
  let title: String =
    orgnode . title() . to_string();
  let body: Option<String> =
    orgnode . body() . cloned();
  let ids: Vec<ID> =
    match orgnode.id() {
      Some(id) => vec![id.clone()],
      None => return Err(format!(
        "Node entitled '{}' has no ID", title)), };
  let source: String =
    match orgnode.source() {
      Some(s) => s.clone(),
      None => return Err(format!(
        "Node entitled '{}' has no source", title)), };
  Ok ( SkgNode {
    title: title,
    aliases: aliases,
    source: source,
    ids: ids,
    body: body,
    contains: Some(collect_contents_that_are_not_to_delete(noderef)),
    subscribes_to: subscribees,
    hides_from_its_subscriptions: None,
    overrides_view_of: None,
  } ) }

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
    unique_orgnode_scaffold_child (
      tree, node_id, &ScaffoldKind::SubscribeeCol )
    . map_err ( |e| e.to_string() ) ?;
  match subscribee_col_id {
    None => Ok(None),
    Some(col_id) => {
      let subscribees : Vec<ID> = {
        let col_ref : NodeRef<OrgNode> = tree.get(col_id).expect(
          "collect_subscribees: SubscribeeCol not found");
        let mut subscribees : Vec<ID> = Vec::new();
        for subscribee_child in col_ref.children() {
          let child_node = subscribee_child . value();
          // HiddenOutsideOfSubscribeeCol is allowed as a child of SubscribeeCol,
          // but it's not a subscribee, so skip it.
          if child_node . is_scaffold ( &ScaffoldKind::HiddenOutsideOfSubscribeeCol ) {
            continue; }
          // Must be a Subscribee (TrueNode with Subscribee effect)
          if ! child_node . has_effect ( EffectOnParent::Subscribee ) {
            return Err ( format! (
              "SubscribeeCol has non-Subscribee child: {:?}",
              child_node . kind )); }
          match child_node.id() {
            Some(id) => subscribees . push( id . clone() ),
            None => return Err ( format! (
              "Subscribee '{}' has no ID",
              child_node.title() )), }
        }
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
  for child in node_ref.children() {
    let child_node = child . value();
    if child_node . has_effect ( EffectOnParent::Content )
       && ! matches!( child_node . edit_request(), Some(EditRequest::Delete) )
    {
      if let Some(id) = child_node.id() {
        contents.push(id.clone()); }} }
  contents }
