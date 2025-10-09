use crate::types::{OrgNode, headlinemd_to_string};
use crate::mk_org_text::util::org_bullet;

/// Renders an OrgNode as an org-mode formatted string.
pub fn render_org_node_from_text (
  level : usize,
  node  : &OrgNode
) -> String {
  let metadata_str : String =
    headlinemd_to_string ( &node.metadata );
  if ( metadata_str . is_empty() &&
       node.title   . is_empty() ) {
    panic! (
      "render_org_node_from_text called with both empty metadata and empty title" ); }
  let mut result : String =
    String::new ();
  result . push_str (
    // Leading bullet is mandatory.
    &org_bullet ( level ));
  if ! metadata_str.is_empty () {
    // Maybe add metadata.
    result . push ( ' ' );
    result . push_str ( "<skg<" );
    result . push_str ( &metadata_str );
    result . push_str ( ">>" ); }
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
