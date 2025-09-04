// Transform 'OrgNode's to 'OrgNodeInterp's

use crate::types::NodeWithEphem;
use crate::types::ID;
use crate::types::OrgNodeInterp;
use crate::types::{OrgNodeInterpEnum,OrgNode};
use super::types::{OrgNodeMetadata};

use std::collections::{HashMap, HashSet};

pub fn interpret_org_node (
  uninterpreted : OrgNode
) -> OrgNodeInterp { // TODO ? Currently this returns a tree of OrgNodeInterps. But since AliasNodes are filtered out, it ought to return instead a tree of NodeWithEphems. This requires using generic trees, and redefining NodeWithEphem and AliasNode to not include their branches.

  let (metadata, title): (OrgNodeMetadata, String) =
    parse_separating_metadata_and_title (
      & uninterpreted.title );
  match metadata.node_type {
    OrgNodeInterpEnum::NodeWithEphem => {
      let interpreted_branches: Vec<OrgNodeInterp> =
        if metadata.repeated { Vec::new()
        } else { uninterpreted.branches
                 . into_iter()
                 . map (interpret_org_node) // recurse
                 . collect()
        };
      let aliases: Option<Vec<String>> =
      // Uses aliases from the first OrgNodeInterp::AliasNode.
      // PITFALL: There should be at most one AliasNode in a given set of siblings, but the user could create more. If they do, all but the first are ignored. */
        interpreted_branches
        . iter()
        . find_map ( // Returns the first Some.
          |child| {
            if let OrgNodeInterp::Aliases (alias_list) = child {
              Some (alias_list.clone())
            } else { None }} );
      OrgNodeInterp::Content ( NodeWithEphem {
        id       : metadata.id,
        title    : title,
        aliases  : aliases,
        body     : ( if metadata.repeated { None }
                     else { uninterpreted.body } ),
        folded   : metadata.folded,
        focused  : metadata.focused,
        repeated : metadata.repeated,
        branches : interpreted_branches
          . iter()
          . filter_map ( |child| {
            if let OrgNodeInterp::Content(content_node)
              = child
            { Some ( OrgNodeInterp::Content (
              content_node.clone() ))
            } else { None // Filter out AliasNode values
            }} )
          . collect(), }) },
    OrgNodeInterpEnum::Aliases => {
      // PITFALL: Perhaps counterintuitively, this recurses into all of the AliasNode's descendents, then collects the headlines of its top-level children and discards everything else. That's because there should not be other contents. (The user can make other contents, but it's not clear why they would want to.)
      let branches: Vec<OrgNodeInterp> =
      { uninterpreted.branches
        . into_iter()
        . map (interpret_org_node) // recurse
        . collect() };
      let aliases: Vec<String> = branches
        . iter()
        . filter_map ( |child| {
          // collect aliases only from NodeWithEphems
          if let OrgNodeInterp::Content (content_node) =
          // PITFALL: You could argue this is an abuse of the NodeWithEphem type, which is intended to correspond to a node in the graph, whereas this corresponds to an alias of its grandparent in the org file.
            child { Some ( content_node . title . clone() )
            } else { None }} )
        . collect();
      OrgNodeInterp::Aliases (aliases) }} }

/// Parse a headline into structured metadata and title.
/// .
/// Steps:
/// 1) Confirm the line is a headline and isolate the post-marker content.
/// 2) If a leading `<skg<...>>` block exists, parse as metadata (order-agnostic).
/// 3) Remaining text (after `>>`) is the trimmed title.
/// 4) If no metadata block, the whole remainder is the trimmed title.
/// 5) Extract the `type` field from metadata if present.
fn parse_separating_metadata_and_title (
  line_after_bullet: &str
) -> (OrgNodeMetadata,
      String) { // the title

  let headline_with_metadata: &str = line_after_bullet.trim_start();
  if let Some(meta_start) = headline_with_metadata.strip_prefix("<skg<") {
    if let Some(end) = meta_start.find(">>") {
      let inner: &str = &meta_start[..end]; // between "<skg<" and ">>"
      let (kv, bare): (HashMap<String, String>, HashSet<String>) =
        metadata_inner_string_to_map_and_set(inner);
      let id_opt: Option<ID> = kv.get("id").map(|s| s.into());
      let repeated: bool = bare.contains("repeated");
      let folded: bool = bare.contains("folded");
      let focused: bool = bare.contains("focused");
      let node_type: OrgNodeInterpEnum
        = match kv . get ("type") . map ( |s|
                                           s.as_str() )
      { Some("aliases") => OrgNodeInterpEnum::Aliases,
        None => OrgNodeInterpEnum::NodeWithEphem,
        Some (other) => panic!( "unrecognized 'type' field: {}",
                                 other), };
      let title_rest: &str = &meta_start[end + 2..]; // skip ">>"
      let title: String = title_rest.trim().to_string();
      return ( OrgNodeMetadata { id: id_opt,
                                 repeated,
                                 folded,
                                 focused,
                                 node_type, },
               title ); }
    // If "<skg<" with no matching ">>", fall through to default case
  }
  { // Default case: no (well-formed) metadata block
    let title: String = line_after_bullet.trim().to_string();
    ( OrgNodeMetadata
      { id: None,
        repeated: false,
        folded: false,
        focused: false,
        node_type: OrgNodeInterpEnum::NodeWithEphem, },
      title ) }}

/// Parse the content inside a `<skg< ... >>` metadata block.
/// Each token is either a key-value pair or a bare value.
/// Tokens are separated by commas.
/// Keys and values are separated by the first colon.
/// Whitespace is stripped.
pub fn metadata_inner_string_to_map_and_set (
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
