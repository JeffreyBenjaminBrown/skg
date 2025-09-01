// Transform 'OrgNodeUninterpreted's to 'OrgNode's

use crate::types::{ID, OrgNode, OrgNodeUninterpreted};
use std::collections::{HashMap, HashSet};

pub fn interpret_org_node (
  uninterpreted : OrgNodeUninterpreted
) -> OrgNode {
  let (id_opt, is_repeated, is_folded, is_focused, title) =
    parse_separating_metadata_and_title (
      & uninterpreted.heading );
  OrgNode {
    id       : id_opt,
    heading  : title,
    aliases  : None,
    body     : ( if is_repeated { None }
                 else { uninterpreted.body } ),
    folded   : is_folded,
    focused  : is_focused,
    repeated : is_repeated,
    branches : ( if is_repeated { Vec::new() }
                 else { uninterpreted.branches
                        .into_iter()
                        .map(interpret_org_node)
                        .collect() } ) }}

/// Parse the *heading line* into `(id, repeated, folded, focused, title)`.
///
/// Steps:
/// 1) Confirm the line is a heading and isolate the post-marker content.
/// 2) If a leading `<<...>>` block exists, parse as metadata (order-agnostic).
/// 3) Remaining text (after `>>`) is the trimmed title.
/// 4) If no metadata block, the whole remainder is the trimmed title.
fn parse_separating_metadata_and_title (
  line_after_bullet: &str
) -> (Option<ID>, bool, bool, bool, String) {

  let heading_with_metadata: &str = line_after_bullet.trim_start();
  if let Some(meta_start) = heading_with_metadata.strip_prefix("<<") {
    if let Some(end) = meta_start.find(">>") {
      let inner: &str = &meta_start[..end]; // between "<<" and ">>"
      let (kv, bare): (HashMap<String, String>, HashSet<String>) =
        parse_metadata_block(inner);
      let id_opt: Option<ID> = kv.get("id").map(|s| s.into());
      let repeated: bool = bare.contains("repeated");
      let folded: bool = bare.contains("folded");
      let focused: bool = bare.contains("focused");
      let title_rest: &str = &meta_start[end + 2..]; // skip ">>"
      let title: String = title_rest.trim().to_string();
      return (id_opt, repeated, folded, focused, title);
    }
    // If "<<" with no matching ">>", fall through to default case
  }
  // Default case: no (well-formed) metadata block
  let title: String = line_after_bullet.trim().to_string();
  (None, false, false, false, title)
}

/// Parse the content inside a `<< ... >>` metadata block.
/// Each token is either a key-value pair or a bare value.
/// Tokens are separated by commas.
/// Keys and values are separated by the first colon.
/// Whitespace is stripped.
pub fn parse_metadata_block (
  inner: &str
) -> (HashMap<String, String>, HashSet<String>) {

  let mut map: HashMap<String, String> =
    HashMap::new();
  let mut set: HashSet<String> =
    HashSet::new();
  for raw in inner.split(',') {
    let tok: &str = raw.trim();
    if tok.is_empty() { continue }
    if let Some(colon) = tok.find(':') {
      // Split key from value on the first colon
      let split          : (&str, &str) = tok.split_at(colon);
      let key_raw        : &str = split.0;
      let val_with_colon : &str = split.1;
      let key            : &str = key_raw.trim();
      let value          : &str = // drop leading ':'
        val_with_colon[1..].trim();
      if !key.is_empty() && !value.is_empty() {
        let k: String = key.to_string();
        let v: String = value.to_string();
        map.insert(k, v); }
    } else { let v: String = tok.to_string();
             set.insert(v);
    }}
  (map, set) }
