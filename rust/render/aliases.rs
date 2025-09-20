use crate::types::{MetadataItem, OrgNodeType};

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