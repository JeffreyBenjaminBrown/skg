use crate::types::{OrgNode, RelToOrgParent};
use crate::types::orgnode::default_metadata;
use crate::render::orgnode::render_org_node_from_text;

/// Returns not an entire org buffer,
/// but rather just a passage,
/// which should replace the current headline and its body.
/// The new passage include the current headline
/// with potentially different metadata (if it is part of a cycle),
/// the same body, a child 'aliases' headline,
/// and a grandchild under it corresponding to each alias.
/// If there are no aliases, the child will still be included --
/// that's how a user can add aliases when there are none.
/// Neither child nor grandchildren will include bodies.
/// Prior children are retained
/// (not that this function knows about them).
pub fn aliases_to_org (
  aliases : Vec<String>,
  level   : usize,
) -> String {
  let header_level : usize = level + 1;
  let alias_level  : usize = level + 2;
  let aliases_header_node : OrgNode =
    OrgNode {
      metadata : { let mut md = default_metadata ();
                   md.relToOrgParent = RelToOrgParent::AliasCol;
                   md },
      title : "".to_string (),
      // The only child node, the 'aliases' headline, has no title.
      body : None, };
  let mut result : String =
    render_org_node_from_text (
      header_level,
      &aliases_header_node );
  for alias in aliases {
    // Each alias grandchild, if any.
    let alias_node : OrgNode =
      OrgNode {
        metadata : { let mut md = default_metadata ();
                     md.relToOrgParent = RelToOrgParent::Alias;
                     md },
        title : alias,
        body : None, };
    result.push_str (
      & render_org_node_from_text (
        alias_level,
        &alias_node )); }
  result }
