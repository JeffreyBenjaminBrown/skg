use crate::types::{MetadataItem, OrgNodeType};

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
  let aliases_metadata = MetadataItem::Type(
    OrgNodeType::Aliases);
  let mut result : String =
    format! ( "{} <skg<{}>>\n",
              "*".repeat ( header_level ),
              aliases_metadata );
  for alias in aliases {
    result.push_str (
      & format! ( "{} {}",
                  "*".repeat ( alias_level ),
                  alias )) ;
    result.push ( '\n' ); }
  result }
