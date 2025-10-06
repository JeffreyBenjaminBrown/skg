// cargo test --test render_util

use skg::render::orgnode::render_org_node_from_text;
use skg::types::{OrgNode2, HeadlineMd2, RelToOrgParent2, ID};

fn default_metadata () -> HeadlineMd2 {
  HeadlineMd2 {
    id : None,
    relToOrgParent : RelToOrgParent2::Content,
    cycle : false,
    focused : false,
    folded : false,
    mightContainMore : false,
    repeat : false,
    toDelete : false,
  } }

#[test]
fn test_render_org_node_from_text_no_metadata () {
  let node : OrgNode2 =
    OrgNode2 {
      metadata : default_metadata (),
      title : "Test Title".to_string (),
      body : None,
    };
  let result : String =
    render_org_node_from_text ( 1, &node );
  assert_eq! ( result, "* Test Title\n" ); }

#[test]
fn test_render_org_node_from_text_with_body () {
  let node : OrgNode2 =
    OrgNode2 {
      metadata : default_metadata (),
      title : "Test Title".to_string (),
      body : Some ( "Test body content".to_string () ),
    };
  let result : String =
    render_org_node_from_text ( 2, &node );
  assert_eq! ( result, "** Test Title\nTest body content\n" ); }

#[test]
fn test_render_org_node_from_text_with_metadata () {
  let node : OrgNode2 =
    OrgNode2 {
      metadata :
        HeadlineMd2 {
          id : None,
          relToOrgParent : RelToOrgParent2::AliasCol,
          cycle : false,
          focused : false,
          folded : true,
          mightContainMore : false,
          repeat : false,
          toDelete : false,
        },
      title : "Test Title".to_string (),
      body : None,
    };
  let result : String =
    render_org_node_from_text ( 1, &node );
  assert_eq! ( result, "* <skg<relToOrgParent:aliasCol,folded>> Test Title\n" ); }

#[test]
fn test_render_org_node_from_text_with_id_metadata () {
  let node : OrgNode2 =
    OrgNode2 {
      metadata :
        HeadlineMd2 {
          id : Some ( ID::from ( "test123" )),
          relToOrgParent : RelToOrgParent2::Content,
          cycle : false,
          focused : false,
          folded : false,
          mightContainMore : false,
          repeat : true,
          toDelete : false,
        },
      title : "Test Title".to_string (),
      body : None,
    };
  let result : String =
    render_org_node_from_text ( 3, &node );
  assert_eq! ( result, "*** <skg<id:test123,repeated>> Test Title\n" ); }

#[test]
fn test_metadata_ordering () {
  let node : OrgNode2 =
    OrgNode2 {
      metadata :
        HeadlineMd2 {
          id : Some ( ID::from ( "xyz" )),
          relToOrgParent : RelToOrgParent2::Content,
          cycle : true,
          focused : false,
          folded : false,
          mightContainMore : false,
          repeat : true,
          toDelete : false,
        },
      title : "Test".to_string (),
      body : None,
    };
  let result : String =
    render_org_node_from_text ( 1, &node );
  assert_eq! ( result, "* <skg<id:xyz,repeated,cycle>> Test\n" ); }

#[test]
#[should_panic ( expected = "render_org_node_from_text called with both empty metadata and empty title" )]
fn test_render_org_node_from_text_empty_metadata_and_title () {
  let node : OrgNode2 =
    OrgNode2 {
      metadata : default_metadata (),
      title : "".to_string (),
      body : None,
    };
  render_org_node_from_text ( 1, &node ); }
