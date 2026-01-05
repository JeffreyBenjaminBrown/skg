use crate::types::{OrgNode, orgnodemd_to_string, Interp};
use crate::types::trees::{NodePair, PairTree};

use ego_tree::NodeRef;
use std::error::Error;

/// PURPOSE: Render a "forest" -- a tree with ForestRoot at root
/// -- to org-mode text.
/// ForestRoot is not rendered; its children start at level 1.
///
/// ASSUMES: metadata has already been enriched with relationship data.
/// ERRORS: if root is not a ForestRoot.
pub fn orgnode_forest_to_string (
  forest : &PairTree,
) -> Result < String, Box<dyn Error> > {
  fn render_node_subtree_to_org (
    node_ref : NodeRef < NodePair >,
    level    : usize,
  ) -> String {
    let orgnode : &OrgNode = & node_ref . value () . orgnode;
    let mut out : String =
      orgnode_to_text ( level, orgnode );
    for child in node_ref . children () {
      out . push_str (
        & render_node_subtree_to_org (
          child,
          level + 1 )); }
    out }
  let root_ref = forest . root ();
  if root_ref . value () . orgnode . metadata . code . interp
     != Interp::ForestRoot {
    return Err (
      "orgnode_forest_to_string: root is not a ForestRoot".into() ); }
  let mut result : String =
    String::new ();
  for child in root_ref . children () {
    result . push_str (
      & render_node_subtree_to_org ( child, 1 )); }
  Ok ( result ) }

/// Renders an OrgNode as org-mode formatted text.
/// Not recursive -- just stars, metadata, title, and maybe a body.
pub fn orgnode_to_text (
  level   : usize,
  orgnode : &OrgNode
) -> String {
  let metadata_str : String =
    orgnodemd_to_string ( &orgnode.metadata );
  if ( metadata_str . is_empty() &&
       orgnode.title   . is_empty() ) {
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
  if ! orgnode.title.is_empty () {
    // Maybe add title.
    // PITFALL: Title can be missing, for the right metadata.
    result . push ( ' ' );
    result . push_str ( &orgnode.title ); }
  result . push ( '\n' );
  if let Some ( ref body_text ) = orgnode.body {
    // Maybe add body
    if ! body_text . is_empty () {
      result . push_str ( body_text );
      if ! body_text . ends_with ( '\n' ) {
        result . push ( '\n' ); }} }
  result }

pub fn org_bullet ( level: usize ) -> String {
  "*" . repeat ( level.max ( 1 )) }
