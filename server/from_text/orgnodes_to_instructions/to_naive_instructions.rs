use crate::types::orgnode::{OrgNode, Interp, EditRequest};
use crate::types::misc::ID;
use crate::types::skgnode::SkgNode;
use crate::types::save::{NonMerge_NodeAction, SaveInstruction};
use crate::types::tree::accessors::{ read_at_node_in_tree, unique_orgnode_child_with_interp };
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
  let (interp, is_indefinitive): (Interp, bool) =
    read_at_node_in_tree(tree, node_id, |node| {
      ( node.metadata.code.interp.clone(),
        node.metadata.code.indefinitive )
    })?;
  if interp == Interp::ForestRoot {
    for child_treeid in {
      let child_treeids: Vec<NodeId> =
        tree . get(node_id) . unwrap() . children()
        . map( |c| c.id() )
        . collect();
      child_treeids }
    { naive_saveinstructions_from_tree (
        tree, child_treeid, result)?; }
    return Ok(( )); }
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
    result.push((skg_node, {
      let save_action: NonMerge_NodeAction =
        read_at_node_in_tree(tree, node_id, |node| {
          if matches!( node.metadata.code.editRequest,
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

/// Collect aliases for a node, then delete its AliasCol branch:
/// - find the unique AliasCol child (error if multiple)
/// - for each Alias child of the AliasCol, collect its title
/// - delete the AliasCol from the tree
/// Duplicates are removed (preserving order of first occurrence).
/// Returns None ("no opinion") if no AliasCol found.
/// Returns Some(vec) if AliasCol found, even if empty.
fn collect_aliases (
  tree: &mut Tree<OrgNode>,
  node_id: NodeId,
) -> Result<Option<Vec<String>>, String> {
  let alias_col_id : Option<NodeId> =
    unique_orgnode_child_with_interp (
      tree, node_id, Interp::AliasCol )
    . map_err ( |e| e.to_string() ) ?;
  match alias_col_id {
    None => Ok(None),
    Some(col_id) => {
      let aliases : Vec<String> = {
        let col_ref : NodeRef<OrgNode> = tree.get(col_id).expect(
          "collect_aliases: AliasCol not found");
        let mut aliases : Vec<String> = Vec::new();
        for alias_child in col_ref.children() {
          let child_interp : &Interp =
            &alias_child . value() . metadata . code.interp;
          if *child_interp != Interp::Alias {
            return Err ( format! (
              "AliasCol has non-Alias child with interp: {:?}",
              child_interp )); }
          aliases . push(
            alias_child . value() . title . clone() ); }
        aliases };
      Ok(Some(dedup_vector(aliases))) }} }

/// Collect a node's subscribees, then delete the SubscribeeCol branch:
/// - find the unique SubscribeeCol child (error if multiple)
/// - for each Subscribee child, collect its ID
/// - skip HiddenOutsideOfSubscribeeCol children (they're not subscribees)
/// Duplicates are removed (preserving order of first occurrence).
/// Returns None if no SubscribeeCol found (no opinion).
/// Returns Some(vec) if SubscribeeCol found - even if empty (user wants no subscribees).
fn collect_subscribees (
  tree: &mut Tree<OrgNode>,
  node_id: NodeId,
) -> Result<Option<Vec<ID>>, String> {
  let subscribee_col_id : Option<NodeId> =
    unique_orgnode_child_with_interp (
      tree, node_id, Interp::SubscribeeCol )
    . map_err ( |e| e.to_string() ) ?;
  match subscribee_col_id {
    None => Ok(None),
    Some(col_id) => {
      let subscribees : Vec<ID> = {
        let col_ref : NodeRef<OrgNode> = tree.get(col_id).expect(
          "collect_subscribees: SubscribeeCol not found");
        let mut subscribees : Vec<ID> = Vec::new();
        for subscribee_child in col_ref.children() {
          let child_interp : &Interp =
            &subscribee_child . value() . metadata . code.interp;
          if *child_interp == Interp::HiddenOutsideOfSubscribeeCol {
            // This Interp is allowed as a child of a SubscribeeCol,
            // but it's not a subscribee, so skip it.
            continue; }
          if *child_interp != Interp::Subscribee {
            return Err ( format! (
              "SubscribeeCol has non-Subscribee child with interp: {:?}",
              child_interp )); }
          match &subscribee_child.value().metadata.id {
            Some(id) => subscribees . push( id . clone() ),
            None => return Err ( format! (
              "Subscribee '{}' has no ID",
              subscribee_child.value().title )), }}
        subscribees };
      Ok(Some(dedup_vector(subscribees))) }} }

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
