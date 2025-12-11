use crate::types::{OrgNode, orgnodemd_to_string};

use ego_tree::Tree;

/// Render a forest of OrgNode trees to org-mode text.
/// Each tree's root starts at level 1.
/// Assumes metadata has already been enriched with relationship data.
pub fn orgnode_forest_to_string (
  forest : &[Tree < OrgNode >],
) -> String {
  fn render_node_subtree_to_org (
    node_ref : ego_tree::NodeRef < OrgNode >,
    level    : usize,
  ) -> String {
    let node : &OrgNode = node_ref . value ();
    let mut out : String =
      orgnode_to_text ( level, node );
    for child in node_ref . children () {
      out . push_str (
        & render_node_subtree_to_org (
          child,
          level + 1 )); }
    out }
  let mut result : String =
    String::new ();
  for tree in forest {
    result . push_str (
      & render_node_subtree_to_org (
        tree . root (),
        1 )); }
  result }

/// Renders an OrgNode as org-mode formatted text.
/// Not recursive -- just stars, metadata, title, and maybe a body.
pub fn orgnode_to_text (
  level : usize,
  node  : &OrgNode
) -> String {
  let metadata_str : String =
    orgnodemd_to_string ( &node.metadata );
  if ( metadata_str . is_empty() &&
       node.title   . is_empty() ) {
    panic! (
      "orgnode_to_text called with both empty metadata and empty title"
    ); }
  let mut result : String =
    String::new ();
  result . push_str (
    // Leading bullet is mandatory.
    &org_bullet ( level ));
  if ! metadata_str.is_empty () {
    // Maybe add metadata.
    result . push ( ' ' );
    result . push_str ( "(skg " );
    result . push_str ( &metadata_str );
    result . push ( ')' ); }
  if ! node.title.is_empty () {
    // Maybe add title.
    // PITFALL: Title can be missing, for the right metadata.
    result . push ( ' ' );
    result . push_str ( &node.title ); }
  result . push ( '\n' );
  if let Some ( ref body_text ) = node.body {
    // Maybe add body
    if ! body_text . is_empty () {
      result . push_str ( body_text );
      if ! body_text . ends_with ( '\n' ) {
        result . push ( '\n' ); }} }
  result }

pub fn org_bullet ( level: usize ) -> String {
  "*" . repeat ( level.max ( 1 )) }
