// cargo test --test render_util

use skg::org_to_text::orgnode_to_text;
use skg::types::orgnode::{OrgnodeViewData, OrgnodeRelationships};
use skg::types::orgnode::{OrgNode, OrgNodeKind, TrueNode, Scaffold, ScaffoldKind, EffectOnParent};
use skg::types::misc::ID;
use std::collections::HashSet;

#[test]
fn test_orgnode_to_text_no_metadata () {
  let node : OrgNode = OrgNode {
    focused: false,
    folded: false,
    kind: OrgNodeKind::True(TrueNode {
      title: "Test Title".to_string(),
      body: None,
      id: None,
      source: None,
      effect_on_parent: EffectOnParent::Content,
      indefinitive: false,
      view_data: OrgnodeViewData::default(),
      edit_request: None,
      view_requests: HashSet::new(),
    }),
  };
  let result : String =
    orgnode_to_text ( 1, &node );
  assert_eq! ( result, "* Test Title\n" ); }

#[test]
fn test_orgnode_to_text_with_body () {
  let node : OrgNode = OrgNode {
    focused: false,
    folded: false,
    kind: OrgNodeKind::True(TrueNode {
      title: "Test Title".to_string(),
      body: Some("Test body content".to_string()),
      id: None,
      source: None,
      effect_on_parent: EffectOnParent::Content,
      indefinitive: false,
      view_data: OrgnodeViewData::default(),
      edit_request: None,
      view_requests: HashSet::new(),
    }),
  };
  let result : String =
    orgnode_to_text ( 2, &node );
  assert_eq! ( result, "** Test Title\nTest body content\n" ); }

#[test]
fn test_orgnode_to_text_with_metadata () {
  let node : OrgNode = OrgNode {
    focused: false,
    folded: true,
    kind: OrgNodeKind::Scaff(Scaffold {
      kind: ScaffoldKind::AliasCol,
    }),
  };
  let result : String =
    orgnode_to_text ( 1, &node );
  assert_eq! ( result, "* (skg (view folded) (code (interp aliasCol))) its aliases\n" ); }

#[test]
fn test_orgnode_to_text_with_id_metadata () {
  let node : OrgNode = OrgNode {
    focused: false,
    folded: false,
    kind: OrgNodeKind::True(TrueNode {
      title: "Test Title".to_string(),
      body: None,
      id: Some(ID::from("test123")),
      source: None,
      effect_on_parent: EffectOnParent::Content,
      indefinitive: true,
      view_data: OrgnodeViewData::default(),
      edit_request: None,
      view_requests: HashSet::new(),
    }),
  };
  let result : String =
    orgnode_to_text ( 3, &node );
  assert_eq! ( result, "*** (skg (id test123) (code indefinitive)) Test Title\n" ); }

#[test]
fn test_metadata_ordering () {
  let node : OrgNode = OrgNode {
    focused: false,
    folded: false,
    kind: OrgNodeKind::True(TrueNode {
      title: "Test".to_string(),
      body: None,
      id: Some(ID::from("xyz")),
      source: None,
      effect_on_parent: EffectOnParent::Content,
      indefinitive: false,
      view_data: OrgnodeViewData {
        cycle: true,
        focused: false,
        folded: false,
        relationships: OrgnodeRelationships {
          parentIsContainer: false,
          .. OrgnodeRelationships::default()
        },
      },
      edit_request: None,
      view_requests: HashSet::new(),
    }),
  };
  let result : String =
    orgnode_to_text ( 1, &node );
  assert_eq! ( result, "* (skg (id xyz) (view cycle (rels notInParent))) Test\n" ); }

#[test]
#[should_panic ( expected = "orgnode_to_text called with both empty metadata and empty title" )]
fn test_orgnode_to_text_empty_metadata_and_title () {
  let node : OrgNode = OrgNode {
    focused: false,
    folded: false,
    kind: OrgNodeKind::True(TrueNode {
      title: "".to_string(),
      body: None,
      id: None,
      source: None,
      effect_on_parent: EffectOnParent::Content,
      indefinitive: false,
      view_data: OrgnodeViewData::default(),
      edit_request: None,
      view_requests: HashSet::new(),
    }),
  };
  orgnode_to_text ( 1, &node ); }
