// Transform 'OrgNodeUninterpreted's to 'OrgNode's

use crate::types::AliasNode;
use crate::types::ContentNode;
use crate::types::ID;
use crate::types::OrgNode;
use crate::types::OrgNodeUninterpreted;

use std::collections::{HashMap, HashSet};

pub fn interpret_org_node (
  uninterpreted : OrgNodeUninterpreted
) -> OrgNode {

  let (id_opt, is_repeated, is_folded, is_focused, title, node_type) =
    parse_separating_metadata_and_title (
      & uninterpreted.heading );
  match node_type {
    None => { // Default case: ContentNode.
      let interpreted_branches: Vec<OrgNode> =
        if is_repeated { Vec::new()
        } else { uninterpreted.branches
                 . into_iter()
                 . map (interpret_org_node)
                 . collect()
        };
      let aliases: Option<AliasNode> =
        interpreted_branches
        . iter()
        . find_map ( /* Finds the first value for which the lambda returns Some. PITFALL: There should be at most one AliasNode in a given set of siblings, but the user could create more; if they do, all but the first are ignored. */
          |child| {
            if let OrgNode::Aliases (alias_node) = child {
              Some (alias_node.clone())
            } else { None
            }} );
      OrgNode::Content(ContentNode {
        id       : id_opt,
        heading  : title,
        aliases  : aliases,
        body     : if is_repeated { None } else { uninterpreted.body },
        folded   : is_folded,
        focused  : is_focused,
        repeated : is_repeated,
        branches : interpreted_branches
          . iter()
          . filter_map ( |child| {
            if let OrgNode::Content(content_node) = child {
              Some(OrgNode::Content(content_node.clone()))
            } else { None // Filter out AliasNode values
            }} )
          . collect(), }) },
    Some (ref type_str) if type_str == "aliases" => {
      let branches: Vec<OrgNode> =
      { uninterpreted.branches
        . into_iter()
        . map (interpret_org_node)
        . collect() };
      let aliases: Vec<String> = branches
        . iter()
        . filter_map ( |child| {
          // collect aliases only from ContentNodes
          if let OrgNode::Content (content_node) = child {
            Some (content_node.heading.clone() )
          } else { None }} )
      . collect();
      OrgNode::Aliases(AliasNode {
        aliases,
        branches,
      })
    },
    Some(type_str) => {
      panic! ( "unrecognized 'type' field in OrgNode: {}",
                type_str ); }} }


/// Parse the *heading line* into `(id, repeated, folded, focused, title, type)`.
/// .
/// Steps:
/// 1) Confirm the line is a heading and isolate the post-marker content.
/// 2) If a leading `<<...>>` block exists, parse as metadata (order-agnostic).
/// 3) Remaining text (after `>>`) is the trimmed title.
/// 4) If no metadata block, the whole remainder is the trimmed title.
/// 5) Extract the `type` field from metadata if present.
fn parse_separating_metadata_and_title (
  line_after_bullet: &str
) -> (Option<ID>, bool, bool, bool, String, Option<String>) {

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
      let node_type: Option<String> = kv.get("type").cloned();
      let title_rest: &str = &meta_start[end + 2..]; // skip ">>"
      let title: String = title_rest.trim().to_string();
      return (id_opt, repeated, folded, focused, title, node_type);
    }
    // If "<<" with no matching ">>", fall through to default case
  }
  // Default case: no (well-formed) metadata block
  let title: String = line_after_bullet.trim().to_string();
  (None, false, false, false, title, None)
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
