// cargo test --test orgnode_to_filenode -- --nocapture

use sexp::parse;

use skg::types::{ID,OrgNode,FileNode};
use skg::save::orgnode_to_filenode::{
  orgnodes_to_filenodes,
  orgnode_to_filenodes };
use skg::save::sexp_to_orgnodes::{
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
  assert!(file_node.contains.is_empty());
  assert!(file_node.subscribes_to.is_empty());
  assert!(file_node.ignores.is_empty());
  assert!(file_node.replaces_view_of.is_empty());
  assert!(file_node.hyperlinks.is_empty());
}

#[test]
fn test_convert_circular_sexp_to_filenode() {
  let sexp_str = r#"
( (id . "1")
  (heading . "1")
  (content
   . ( ( ( id . "2" )
         ( heading . "2" )
         ( content
           . ( ( ( id . "1" )
                 ( heading . "irrelevant" )
                 ( content
                   . ( ( ( id . "3" )
                         ( heading . "also irrelevant" )
                         ) ) ) ) ) ) ) ) ) )"#;
  let sexp = parse(sexp_str)
    .expect("Failed to parse S-expression");
  let org_node = node_sexp_to_orgnode(sexp)
    .expect("Failed to convert S-expression to OrgNode");

  // Verify the top-level OrgNode is correct
  assert_eq!(org_node.id.as_ref().map(|id| id.as_str()),
             Some("1"));
  assert_eq!(org_node.heading, "1");
  assert_eq!(org_node.branches.len(), 1,
             "Top node should have one child node");

  // Verify the child node
  let child = &org_node.branches[0];
  assert_eq!(child.id.as_ref().map(|id| id.as_str()),
             Some("2"));
  assert_eq!(child.heading, "2");
  assert_eq!(child.branches.len(), 1,
             "Child should itself have one child");

  // Verify the nested repeated node
  let nested = &child.branches[0];
  assert_eq!(nested.id.as_ref().map(|id| id.as_str()),
             Some("1"));
  assert_eq!(nested.heading, "irrelevant");

  let file_nodes = orgnode_to_filenodes(&org_node);
  assert_eq!(file_nodes.len(), 2,
             "Expected exactly two FileNodes");

  let node1 = file_nodes.iter().find(
    |n| n.ids[0].as_str() == "1")
    . expect("Node with ID '1' not found");
  let node2 = file_nodes.iter().find(
    |n| n.ids[0].as_str() == "2")
    . expect("Node with ID '2' not found");

  assert_eq!(node1.titles[0], "1");
  assert_eq!(node1.contains.len(), 1,
             "Node 1 should contain node 2");
  assert_eq!(node1.contains[0].as_str(), "2");

  assert_eq!(node2.titles[0], "2");
  assert_eq!(node2.contains.len(), 1,
             "Node 2 should contain node 1");
  assert_eq!(node2.contains[0].as_str(), "1");
}
