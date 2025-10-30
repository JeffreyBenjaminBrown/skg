use crate::types::{OrgNode, RelToParent, ID, SkgNode, NodeSaveAction, SkgConfig, SaveInstruction};
use crate::save::reconcile_dup_instructions;
use ego_tree::{NodeRef, Tree};
use typedb_driver::TypeDBDriver;
use std::error::Error;

/// Converts a forest of OrgNode2s to SaveInstructions,
/// reconciling duplicates via 'reconcile_dup_instructions'.
pub async fn orgnodes_to_save_instructions (
  forest  : &Vec<Tree<OrgNode>>,
  config : &SkgConfig,
  driver : &TypeDBDriver
) -> Result<Vec<SaveInstruction>, Box<dyn Error>> {
  let instructions : Vec<SaveInstruction> =
    orgnodes_to_dirty_save_instructions (
      forest . clone () ) ?;
  let reconciled_instructions : Vec<SaveInstruction> =
    reconcile_dup_instructions (
      config, driver, instructions ) . await ?;
  Ok (reconciled_instructions) }

/// PITFALL: Leaves important work undone,
/// which orgnodes_to_save_instructions does.
/// This is only public for testing.
///
/// Converts a forest of OrgNode2s to SaveInstructions,
/// taking them all at face value.
pub fn orgnodes_to_dirty_save_instructions (
  trees: Vec<Tree<OrgNode>>
) -> Result<Vec<SaveInstruction>, String> {
  let mut result: Vec<(SkgNode, NodeSaveAction)> =
    Vec::new();
  for tree in trees {
    interpret_node_dfs ( tree.root(),
                         &mut result )?; }
  Ok(result) }

/// Appends another pair to 'result' and recurses.
fn interpret_node_dfs(
  node_ref: NodeRef<OrgNode>,
  result: &mut Vec<(SkgNode, NodeSaveAction)>
) -> Result<(), String> {
  { // push another pair
    let node_data = node_ref.value();
    let save_action: NodeSaveAction = NodeSaveAction {
      indefinitive : ( node_data.metadata.code.indefinitive ||
                       node_data.metadata.code.repeat ),
      toDelete     : node_data.metadata.code.toDelete, };
    let skg_node: SkgNode =
      mk_skgnode(node_data, &node_ref)?;
    result.push (( skg_node, save_action )); }
  { // Recurse into everything except aliases.
    for child in node_ref.children() {
      let child_rel = &child.value().metadata.code.relToParent;
      match child_rel {
        RelToParent::AliasCol | RelToParent::Alias => {
          // These are ignored.
        },
        _ => {
          interpret_node_dfs(
            child, result)?; }} } }
  Ok (( )) }

fn mk_skgnode (
  orgnode: &OrgNode,
  noderef: &NodeRef<OrgNode> // the same node, but in the tree
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
  Ok ( SkgNode {
    title: title,
    aliases: collect_aliases(noderef),
    ids: ids,
    body: body,
    contains: Some(collect_contents(noderef)),
    subscribes_to: None,
    hides_from_its_subscriptions: None,
    overrides_view_of: None,
  } ) }

/* Collect aliases for a node using a double-loop:
- For each child CA of N such that CA has treatment=AliasCol,
  - for each child A of CA such that A has treatment=Alias,
    - collect A into the list of aliases for N.
This is programmed defensively:
  'validate_tree' will not currently permit multiple AliasCol
  children under the same node,
  but this function will work even if there are. */
fn collect_aliases (
  node_ref: &NodeRef<OrgNode>
) -> Option<Vec<String>> {
  let mut aliases: Vec<String> = Vec::new();
  for alias_col_child in node_ref.children()
  { if ( alias_col_child . value() . metadata . code.relToParent
         == RelToParent::AliasCol ) // child of interest
    { for alias_child in alias_col_child.children() {
      if ( alias_child . value() . metadata . code.relToParent
           == RelToParent::Alias ) // grandchild of interest
      { aliases . push(
        alias_child . value() . title . clone() ); }} }}
  if aliases.is_empty() { None
  } else { Some(aliases) }}

/// Returns IDs of all children for which treatment = Content.
/// Excludes children for which metadata.toDelete is true.
/// This is not a recursive traversal;
/// it is only concerned with this node's contents.
fn collect_contents (
  node_ref: &NodeRef<OrgNode>
) -> Vec<ID> {
  let mut contents: Vec<ID> =
    Vec::new();
  for child in node_ref.children() {
    if ( child . value() . metadata . code.relToParent
         == RelToParent::Content
         && ! child . value() . metadata . code.toDelete ) {
      if let Some(id) = &child.value().metadata.id {
        contents.push(id.clone());
      }} }
  contents }
