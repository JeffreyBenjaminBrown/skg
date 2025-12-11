use crate::types::{OrgNode, RelToParent, ID, SkgNode, NonMerge_NodeAction, SaveInstruction, EditRequest};
use ego_tree::{NodeRef, Tree};

/// Converts a forest of OrgNodes to SaveInstructions,
/// taking them all at face value.
///
/// PITFALL: Leaves important work undone,
/// which its caller 'orgnodes_to_reconciled_save_instructions'
/// does after calling it.
pub fn interpret_orgnode_forest (
  trees: Vec<Tree<OrgNode>>
) -> Result<Vec<SaveInstruction>, String> {
  let mut result: Vec<SaveInstruction> =
    Vec::new();
  for tree in trees {
    interpret_node_dfs ( tree.root(),
                         &mut result )?; }
  Ok(result) }

/// Appends another pair to 'result' and recurses.
/// Skips AliasCol, Alias, and indefinitive nodes.
/// (Aliases are handled by 'collect_aliases' in 'mk_skgnode',
/// when 'interpret_node_dfs' is called on the orgnode ancestor they describe.)
/// (Indefinitive nodes represent views and don't contribute to saves.)
fn interpret_node_dfs(
  node_ref: NodeRef<OrgNode>,
  result: &mut Vec<SaveInstruction>
) -> Result<(), String> {
  let node_data = node_ref.value();
  let rel_to_parent = &node_data.metadata.code.relToParent;
  if matches!(rel_to_parent, ( RelToParent::AliasCol |
                               RelToParent::Alias )) {
    // Skip. Should never execute, because a predecessor in the tree should have already processed any alias node.
    return Ok(( )); }
  if !node_data.metadata.code.indefinitive {
    // push another pair
    let save_action : NonMerge_NodeAction =
      if matches!(node_data.metadata.code.editRequest,
                  Some(EditRequest::Delete)) {
        NonMerge_NodeAction::Delete
      } else {
        NonMerge_NodeAction::Save };
    let skg_node: SkgNode =
      mk_skgnode ( node_data, &node_ref )?;
    result . push (( skg_node, save_action )); }
  for child in node_ref.children() {
    // Recurse over children, even if their parent is indefinitive.
    interpret_node_dfs ( child, result)?; }
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
  let source: String =
    match &orgnode.metadata.source {
      Some(s) => s.clone(),
      None => return Err(format!(
        "Node entitled '{}' has no source", title)), };
  Ok ( SkgNode {
    title: title,
    aliases: collect_aliases(noderef),
    source: source,
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
  'validate_tree' will not currently permit multiple 'AliasCol'
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
  if aliases.is_empty() { None }
  else { Some (aliases) }}

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
    if (( child . value() . metadata . code.relToParent
          == RelToParent::Content )
        && ( ! matches!(
          child . value() . metadata . code . editRequest,
          Some(EditRequest::Delete)) )) {
      if let Some(id) = &child.value().metadata.id {
        contents.push(id.clone()); }} }
  contents }
