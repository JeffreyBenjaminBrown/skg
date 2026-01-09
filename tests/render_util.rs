// cargo test --test render_util

use skg::org_to_text::orgnode_to_text;
use skg::types::orgnode::{OrgnodeViewData, OrgnodeRelationships};
use skg::types::orgnode::{
    OrgNode, OrgNodeKind, TrueNode, ScaffoldKind,
    orgnode_from_scaffold_kind };
use skg::types::misc::ID;

#[test]
fn test_orgnode_to_text_no_metadata () {
  let node = OrgNode {
    kind: OrgNodeKind::True ( TrueNode {
      title: "Test Title".to_string(),
      .. TrueNode::default() }),
    .. OrgNode::default() };
  let result : String =
    orgnode_to_text ( 1, &node );
  assert_eq! ( result, "* Test Title\n" ); }

#[test]
fn test_orgnode_to_text_with_body () {
  let node = OrgNode {
    kind: OrgNodeKind::True ( TrueNode {
      title: "Test Title".to_string(),
      body: Some ( "Test body content".to_string() ),
      .. TrueNode::default() }),
    .. OrgNode::default() };
  let result : String =
    orgnode_to_text ( 2, &node );
  assert_eq! ( result, "** Test Title\nTest body content\n" ); }

#[test]
fn test_orgnode_to_text_with_metadata () {
  let mut node = orgnode_from_scaffold_kind ( ScaffoldKind::AliasCol );
  node.folded = true;
  let result : String =
    orgnode_to_text ( 1, &node );
  assert_eq! ( result, "* (skg (view folded) (code (interp aliasCol))) its aliases\n" ); }

#[test]
fn test_orgnode_to_text_with_id_metadata () {
  let node = OrgNode {
    kind: OrgNodeKind::True ( TrueNode {
      title: "Test Title".to_string(),
      id: Some ( ID::from ( "test123" )),
      indefinitive: true,
      .. TrueNode::default() }),
    .. OrgNode::default() };
  let result : String =
    orgnode_to_text ( 3, &node );
  assert_eq! ( result, "*** (skg (id test123) (code indefinitive)) Test Title\n" ); }

#[test]
fn test_metadata_ordering () {
  let node = OrgNode {
    kind: OrgNodeKind::True ( TrueNode {
      title: "Test".to_string(),
      id: Some ( ID::from ( "xyz" )),
      view_data: OrgnodeViewData {
        cycle: true,
        relationships: OrgnodeRelationships {
          parentIsContainer: false,
          .. OrgnodeRelationships::default() },
        .. OrgnodeViewData::default() },
      .. TrueNode::default() }),
    .. OrgNode::default() };
  let result : String =
    orgnode_to_text ( 1, &node );
  assert_eq! ( result, "* (skg (id xyz) (view cycle (rels notInParent))) Test\n" ); }

#[test]
#[should_panic ( expected = "orgnode_to_text called with both empty metadata and empty title" )]
fn test_orgnode_to_text_empty_metadata_and_title () {
  orgnode_to_text ( 1, &OrgNode::default() ); }
