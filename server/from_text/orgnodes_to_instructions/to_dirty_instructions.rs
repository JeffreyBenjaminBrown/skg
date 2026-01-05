use crate::types::orgnode::{OrgNode, Interp, EditRequest};
use crate::types::misc::ID;
use crate::types::skgnode::SkgNode;
use crate::types::save::{NonMerge_NodeAction, SaveInstruction};
use crate::types::tree::accessors::read_at_node_in_tree;
use crate::util::dedup_vector;
use ego_tree::{NodeId, NodeRef, Tree};

/// Converts a forest of OrgNodes to SaveInstructions,
/// taking them all at face value.
///
/// PITFALL: Leaves important work undone,
/// which its caller 'orgnodes_to_reconciled_save_instructions'
/// does after calling it.
pub fn saveinstructions_from_forest (
  mut forest: Tree<OrgNode> // "forest" = tree with ForestRoot
) -> Result<Vec<SaveInstruction>, String> {
  let mut result: Vec<SaveInstruction> =
    Vec::new();
  let root_id = forest.root().id();
  saveinstructions_from_tree ( &mut forest, root_id,
                               &mut result )?;
  Ok(result) }

/// Appends another pair to 'result' and recurses (in DFS order).
/// Skips some nodes, because:
/// - indefinitive nodes don't generate instructions
/// - aliases     are handled by 'collect_aliases'
/// - subscribees are handled by 'collect_subscribees'
fn saveinstructions_from_tree(
  tree: &mut Tree<OrgNode>,
  node_id: NodeId,
  result: &mut Vec<SaveInstruction>
) -> Result<(), String> {
  let (interp, is_indefinitive): (Interp, bool) =
    read_at_node_in_tree(tree, node_id, |node| {
      ( node.metadata.code.interp.clone(),
        node.metadata.code.indefinitive )
    })?;
  if interp == Interp::ForestRoot {
    let child_treeids: Vec<NodeId> = tree.get(node_id).unwrap()
      .children().map(|c| c.id()).collect();
    for child_treeid in child_treeids {
      saveinstructions_from_tree(tree, child_treeid, result)?; }
    return Ok(()); }
  if matches!(interp, Interp::AliasCol                     |
                      Interp::Alias                        |
                      Interp::SubscribeeCol                |
                      Interp::Subscribee                   |
                      Interp::HiddenOutsideOfSubscribeeCol |
                      Interp::HiddenInSubscribeeCol        |
                      Interp::HiddenFromSubscribees ) {
    return Ok(()); } // Skip - these don't generate SaveInstructions
  let aliases =
    collect_aliases(tree, node_id)?;
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
    let save_action: NonMerge_NodeAction =
      read_at_node_in_tree(tree, node_id, |node| {
        if matches!(node.metadata.code.editRequest,
                    Some(EditRequest::Delete)) {
          NonMerge_NodeAction::Delete
        } else {
          NonMerge_NodeAction::Save }
      })?;
    result.push((skg_node, save_action)); }
  { // recurse
    let child_treeids: Vec<NodeId> = tree.get(node_id).unwrap()
      .children().map(|c| c.id()).collect();
    for child_treeid in child_treeids {
      saveinstructions_from_tree(tree, child_treeid, result)?; }}
  Ok (()) }

fn skgnode_for_orgnode_in_tree<'a> (
  orgnode: &OrgNode,
  noderef: &NodeRef<'a, OrgNode>, // the same node, but in the tree
  aliases: Option<Vec<String>>,
  subscribees: Option<Vec<ID>>,
) -> Result<SkgNode, String> {
  let title: String =
    orgnode . title . clone();
  let body: Option<String> =
    orgnode . body . clone();
  let ids: Vec<ID> =
    match &orgnode.metadata.id {
      Some(id) => vec![id.clone()],
      None => return Err(format!(
        "Node entitled '{}' has no ID", title)), };
  let source: String =
    match &orgnode.metadata.source {
      Some(s) => s.clone(),
      None => return Err(format!(
        "Node entitled '{}' has no source", title)), };
  Ok ( SkgNode {
    title: title,
    aliases: aliases,
    source: source,
    ids: ids,
    body: body,
    contains: Some(collect_contents(noderef)),
    subscribes_to: subscribees,
    hides_from_its_subscriptions: None,
    overrides_view_of: None,
  } ) }

/// Collect aliases for a node, then delete its AliasCol branch(es):
/// - for each child CA of N such that CA has treatment=AliasCol,
///   - for each child A of CA such that A has treatment=Alias,
///     - collect A into the list of aliases for N.
///   - delete CA from the tree.
/// This is programmed defensively:
///   'validate_tree' will not currently permit multiple 'AliasCol'
///   children under the same node,
///   but this function will work even if there are.
/// Duplicates are removed (preserving order of first occurrence).
/// Returns None ("no opinion") if no AliasCol found.
/// Returns Some(vec) if AliasCol found, even if empty.
fn collect_aliases (
  tree: &mut Tree<OrgNode>,
  node_id: NodeId,
) -> Result<Option<Vec<String>>, String> {
  let (aliases, alias_col_ids): (Vec<String>, Vec<NodeId>) = {
    let node_ref = tree.get(node_id).expect(
      "collect_aliases: node not found");
    let mut aliases: Vec<String> = Vec::new();
    let mut alias_col_ids: Vec<NodeId> = Vec::new();
    for alias_col_child in node_ref.children()
    { if ( alias_col_child . value() . metadata . code.interp
           == Interp::AliasCol ) // child of interest
      { alias_col_ids.push(alias_col_child.id());
        for alias_child in alias_col_child.children() {
          let child_interp =
            &alias_child . value() . metadata . code.interp;
          if *child_interp != Interp::Alias {
            return Err ( format! (
              "AliasCol has non-Alias child with interp: {:?}",
              child_interp )); }
          aliases . push(
            alias_child . value() . title . clone() ); } }}
    (aliases, alias_col_ids) };
  { // return
    if alias_col_ids.is_empty() { Ok(None) }
    else { Ok(Some(dedup_vector(aliases))) }} }

/// Collect a node's subscribees, then delete the colecting branch(es):
/// - for each child SC of N such that SC has treatment=SubscribeeCol,
///   - for each child S of SC such that S has treatment=Subscribee,
///     - collect S's ID into the list of subscribees for N.
///   - delete SC from the tree.
/// This is programmed defensively:
///   'validate_tree' will not currently permit multiple 'SubscribeeCol'
///   children under the same node,
///   but this function will work even if there are.
/// Duplicates are removed (preserving order of first occurrence).
/// Returns None if no SubscribeeCol found (no opinion).
/// Returns Some(vec) if SubscribeeCol found - even if empty (user wants no subscribees).
fn collect_subscribees (
  tree: &mut Tree<OrgNode>,
  node_id: NodeId,
) -> Result<Option<Vec<ID>>, String> {
  let (subscribees, subscribee_col_ids): (Vec<ID>, Vec<NodeId>) = {
    let node_ref = tree.get(node_id).expect(
      "collect_subscribees: node not found");
    let mut subscribees: Vec<ID> = Vec::new();
    let mut subscribee_col_ids: Vec<NodeId> = Vec::new();
    for subscribee_col_child in node_ref.children()
    { if ( subscribee_col_child . value() . metadata . code.interp
           == Interp::SubscribeeCol ) // child of interest
      { subscribee_col_ids.push(subscribee_col_child.id());
        for subscribee_child in subscribee_col_child.children() {
          let child_interp =
            &subscribee_child . value() . metadata . code.interp;
          // Skip HiddenOutsideOfSubscribeeCol - it's allowed as a child of a SubscribeeCol, but it's not a subscribee
          if *child_interp == Interp::HiddenOutsideOfSubscribeeCol {
            continue; }
          if *child_interp != Interp::Subscribee {
            return Err ( format! (
              "SubscribeeCol has non-Subscribee child with interp: {:?}",
              child_interp )); }
          match &subscribee_child.value().metadata.id {
            Some(id) => subscribees . push( id . clone() ),
            None => return Err ( format! (
              "Subscribee '{}' has no ID",
              subscribee_child.value().title )),
          }} }}
    (subscribees, subscribee_col_ids) };
  { // return
    if subscribee_col_ids.is_empty() { Ok(None) }
    else { Ok(Some(dedup_vector(subscribees) )) }} }

/// Returns IDs of all children for which treatment = Content.
/// Excludes children for which metadata.toDelete is true.
/// This is not a recursive traversal;
/// it is only concerned with this node's contents.
fn collect_contents<'a> (
  node_ref: &NodeRef<'a, OrgNode>
) -> Vec<ID> {
  let mut contents: Vec<ID> =
    Vec::new();
  for child in node_ref.children() {
    if (( child . value() . metadata . code.interp
          == Interp::Content )
        && ( ! matches!(
          child . value() . metadata . code . editRequest,
          Some(EditRequest::Delete)) )) {
      if let Some(id) = &child.value().metadata.id {
        contents.push(id.clone()); }} }
  contents }
