pub mod cursor;
pub mod uninterpreted;
pub mod interpreted;

use crate::save::orgfile_to_orgnodes::uninterpreted::parse_skg_org_to_uninterpreted_nodes;
use crate::save::orgfile_to_orgnodes::interpreted::interpret_org_node;use crate::types::{OrgNode, OrgNodeUninterpreted};

/* This function parses a "skg org" document
into a forest of `OrgNode`s.
.
- Lines before the first heading are ignored.
- A heading is `*+ <space>? <content>` where `<content>` is not all whitespace.
- If a heading begins with a metadata block `<<...>>`, it is parsed into (map, set).
  - `id` comes from the map's `"id"` key if present.
  - Each of `repeated`, `folded` and `focused` is true if a bare value of the same name appears in the metadata.
- For a `repeated` node, the body and the *entire subtree* are skipped.
- Body is the consecutive non-heading lines immediately after a heading.
- Children are consecutive headings with level exactly `parent_level + 1`. */
pub fn parse_skg_org_to_nodes (
  input : &str
) -> Vec<OrgNode> { // TODO: As noted in a TODO comment in the definition of `interpret_org_node`, this return type could be better.

  let uninterpreted_nodes : Vec<OrgNodeUninterpreted> =
    parse_skg_org_to_uninterpreted_nodes (input);
  uninterpreted_nodes.into_iter ()
    .map (interpret_org_node)
    .collect () }
