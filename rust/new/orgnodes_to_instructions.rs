use crate::types::{OrgNode2, RelToOrgParent2, ID, SkgNode, NodeSaveAction};

use ego_tree::{NodeRef,Tree};

/// Converts a forest of OrgNode2s to a forest of instructions.
pub fn interpret_forest (
  trees: Vec<Tree<OrgNode2>>
) -> Result<Vec<(SkgNode,
                 NodeSaveAction)>,
            String> {
  let mut result: Vec<(SkgNode, NodeSaveAction)> =
    Vec::new();
  for tree in trees {
    interpret_node_dfs( tree.root(),
                        &mut result)?; }
  Ok(result) }

/// Appends another pair to 'result' and recurses.
fn interpret_node_dfs(
  node_ref: NodeRef<OrgNode2>,
  result: &mut Vec<(SkgNode, NodeSaveAction)>
) -> Result<(), String> {
  { // push another pair
    let node_data = node_ref.value();
    let save_action: NodeSaveAction = NodeSaveAction {
      mightContainMore : node_data.metadata.mightContainMore,
      toDelete         : node_data.metadata.toDelete, };
    let skg_node: SkgNode =
      mk_skgnode(node_data, &node_ref)?;
    result.push((skg_node, save_action)); }
  { // Recurse into everything except aliases.
    for child in node_ref.children() {
      let child_rel = &child.value().metadata.relToOrgParent;
      match child_rel {
        RelToOrgParent2::AliasCol | RelToOrgParent2::Alias => {
          // These are ignored.
        },
        _ => {
          interpret_node_dfs(
            child, result)?; }} } }
  Ok (( )) }

fn mk_skgnode (
  orgnode: &OrgNode2,
  noderef: &NodeRef<OrgNode2> // the same node, but in the tree
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
    contains: collect_contents(noderef),
    subscribes_to: vec![],
    hides_from_its_subscriptions: vec![],
    overrides_view_of: vec![],
  } ) }

/* Collect aliases for a node using a double-loop:
- For each child CA of N such that CA has relToOrgParent=AliasCol,
  - for each child A of CA such that A has relToOrgParent=Alias,
    - collect A into the list of aliases for N.
This is programmed defensively:
  'validate_tree' will not currently permit multiple AliasCol
  children under the same node,
  but this function will work even if there are. */
fn collect_aliases (
  node_ref: &NodeRef<OrgNode2>
) -> Option<Vec<String>> {
  let mut aliases: Vec<String> = Vec::new();
  for alias_col_child in node_ref.children() {
    if ( alias_col_child . value() . metadata . relToOrgParent
         == RelToOrgParent2::AliasCol ) // child of interest
    { for alias_child in alias_col_child.children() {
      if ( alias_child . value() . metadata . relToOrgParent
           == RelToOrgParent2::Alias ) // grandchild of interest
      { aliases . push(
        alias_child . value() . title . clone() ); }} }}
  if aliases.is_empty() { None
  } else { Some(aliases) }}

/// Returns IDs of all children for which relToOrgParent = Content
fn collect_contents (
  node_ref: &NodeRef<OrgNode2>
) -> Vec<ID> {
  let mut contents: Vec<ID> =
    Vec::new();
  for child in node_ref.children() {
    if ( child . value() . metadata . relToOrgParent
         == RelToOrgParent2::Content ) {
      if let Some(id) = &child.value().metadata.id {
        contents.push(id.clone());
      }} }
  contents }
