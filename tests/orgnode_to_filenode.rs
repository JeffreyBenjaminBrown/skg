// cargo test --test orgnode_to_filenode -- --nocapture

use sexp::parse;

use skg::types::{ID,OrgNode,FileNode};
use skg::save::orgnode_to_filenode::{
  orgnodes_to_filenodes,
  orgnode_to_filenodes };
use skg::save::sexp_to_vector::{
  node_sexp_to_orgnode,
  content_sexp_to_orgnodes,
  content_sexps_to_orgnodes };

#[test]
fn test_convert_sexp_to_filenode() {
  let sexp_str = r#"(
(id . "1")
(heading . "a title") )"#;
  let sexp = parse(sexp_str)
    .expect("Failed to parse S-expression");
  let org_node = node_sexp_to_orgnode(
    sexp)
    .expect("Failed to convert S-expression to OrgNode");
  let file_nodes = orgnode_to_filenodes(
    &org_node);

  assert_eq!(file_nodes.len(), 1,
             "Expected exactly one FileNode");
  let file_node = &file_nodes[0];
  assert_eq!(file_node.titles.len(), 1);
  assert_eq!(file_node.titles[0], "a title");
  assert_eq!(file_node.ids.len(), 1);
  assert_eq!(file_node.ids[0].as_str(), "1");
  assert_eq!(file_node.body, None);
  assert_eq!(file_node.no_tantivy_index, false);
  assert!(file_node.contains.is_empty());
  assert!(file_node.subscribes_to.is_empty());
  assert!(file_node.ignores.is_empty());
  assert!(file_node.replaces_view_of.is_empty());
  assert!(file_node.links.is_empty());
}
