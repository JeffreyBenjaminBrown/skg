// cargo test --test render_util

use skg::mk_org_text::orgnode::render_org_node_from_text;
use skg::types::{OrgNode, OrgnodeMetadata, OrgnodeViewData, OrgnodeCode, OrgnodeRelationships, RelToParent, ID};
use skg::types::orgnode::default_metadata;
use std::collections::HashSet;

#[test]
fn test_render_org_node_from_text_no_metadata () {
  let node : OrgNode =
    OrgNode {
      metadata : default_metadata (),
      title : "Test Title".to_string (),
      body : None,
    };
  let result : String =
    render_org_node_from_text ( 1, &node );
  assert_eq! ( result, "* Test Title\n" ); }

#[test]
fn test_render_org_node_from_text_with_body () {
  let node : OrgNode =
    OrgNode {
      metadata : default_metadata (),
      title : "Test Title".to_string (),
      body : Some ( "Test body content".to_string () ),
    };
  let result : String =
    render_org_node_from_text ( 2, &node );
  assert_eq! ( result, "** Test Title\nTest body content\n" ); }

#[test]
fn test_render_org_node_from_text_with_metadata () {
  let node : OrgNode =
    OrgNode {
      metadata : { let mut md = default_metadata ();
                   md.code.relToParent = RelToParent::AliasCol;
                   md.viewData.folded = true;
                   md },
      title : "Test Title".to_string (),
      body : None, };
  let result : String =
    render_org_node_from_text ( 1, &node );
  assert_eq! ( result, "* (skg (view folded) (code (relToParent aliasCol))) Test Title\n" ); }

#[test]
fn test_render_org_node_from_text_with_id_metadata () {
  let node : OrgNode =
    OrgNode {
      metadata : { let mut md = default_metadata ();
                   md.id = Some ( ID::from ( "test123" ));
                   md.viewData.repeat = true;
                   md },
      title : "Test Title".to_string (),
      body : None, };
  let result : String =
    render_org_node_from_text ( 3, &node );
  assert_eq! ( result, "*** (skg (id test123) (view repeated)) Test Title\n" ); }

#[test]
fn test_metadata_ordering () {
  let node : OrgNode =
    OrgNode {
      metadata :
        OrgnodeMetadata {
          id : Some ( ID::from ( "xyz" )),
          viewData : OrgnodeViewData {
            cycle : true,
            focused : false,
            folded : false,
            repeat : true,
            relationships : OrgnodeRelationships {
              parentIsContainer : false,
              .. OrgnodeRelationships::default () }, },
          code : OrgnodeCode {
            relToParent : RelToParent::Content,
            indefinitive : false,
            toDelete     : false,
            nodeRequests : HashSet::new (), }, },
      title : "Test".to_string (),
      body : None, };
  let result : String =
    render_org_node_from_text ( 1, &node );
  assert_eq! ( result, "* (skg (id xyz) (view cycle repeated (rels notInParent))) Test\n" ); }

#[test]
#[should_panic ( expected = "render_org_node_from_text called with both empty metadata and empty title" )]
fn test_render_org_node_from_text_empty_metadata_and_title () {
  let node : OrgNode =
    OrgNode {
      metadata : default_metadata (),
      title : "".to_string (),
      body : None,
    };
  render_org_node_from_text ( 1, &node ); }
