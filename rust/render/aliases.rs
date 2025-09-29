use crate::types::{MetadataItem, RelToOrgParent};
use crate::render::orgnode::render_org_node_from_text;
use std::collections::HashSet;

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

  // build inputs
  let header_level : usize = level + 1;
  let alias_level  : usize = level + 2;
  let mut aliases_metadata = HashSet::new();
  aliases_metadata.insert (
    MetadataItem::RelToOrgParent (
      RelToOrgParent::Aliases ));

  // build result
  let mut result = render_org_node_from_text(
    // The only child node, the 'aliases' headline, has no title.
    header_level, "", None, &aliases_metadata);
  for alias in aliases { // Each each alias grandchild, if any.
    result.push_str (
      & render_org_node_from_text(
        alias_level, & alias, None, & HashSet::new()
      ) ); }

  result }
