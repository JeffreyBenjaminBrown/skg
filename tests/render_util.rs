// cargo test --test render_util

use skg::org_to_text::orgnode_to_text;
use skg::types::orgnode::ViewNodeStats;
use skg::types::orgnode::{
    OrgNode, OrgNodeKind, Scaffold, TrueNode,
    orgnode_from_scaffold, default_truenode };
use skg::types::misc::ID;

#[test]
fn test_orgnode_to_text_no_metadata () {
  let node : OrgNode = OrgNode {
    focused : false,
    folded  : false,
    kind    : OrgNodeKind::True (
      default_truenode ( ID::from("test"),
                         "main".to_string(),
                         "Test Title".to_string() )) };
  let result : String =
    orgnode_to_text ( 1, &node )
    . expect ( "TrueNode rendering never fails" );
  assert_eq! ( result, "* (skg (node (id test) (source main))) Test Title\n" ); }

#[test]
fn test_orgnode_to_text_with_body () {
  let t : TrueNode = TrueNode {
    body : Some ( "Test body content".to_string() ),
    .. default_truenode ( ID::from("test"),
                          "main".to_string(),
                          "Test Title".to_string() ) };
  let node : OrgNode = OrgNode {
    focused : false,
    folded  : false,
    kind    : OrgNodeKind::True ( t ), };
  let result : String =
    orgnode_to_text ( 2, &node )
    . expect ( "TrueNode rendering never fails" );
  assert_eq! ( result, "** (skg (node (id test) (source main))) Test Title\nTest body content\n" ); }

#[test]
fn test_orgnode_to_text_with_metadata () {
  let mut node = orgnode_from_scaffold ( Scaffold::AliasCol );
  node.folded = true;
  let result : String =
    orgnode_to_text ( 1, &node )
    . expect ( "AliasCol rendering never fails" );
  assert_eq! ( result, "* (skg folded aliasCol) its aliases\n" ); }

#[test]
fn test_orgnode_to_text_with_id_metadata () {
  let t : TrueNode = TrueNode {
    indefinitive : true,
    .. default_truenode ( ID::from ( "test123" ),
                          "main".to_string(),
                          "Test Title".to_string() ) };
  let node : OrgNode = OrgNode {
    focused : false,
    folded  : false,
    kind    : OrgNodeKind::True ( t ), };
  let result : String =
    orgnode_to_text ( 3, &node )
    . expect ( "TrueNode rendering never fails" );
  assert_eq! ( result, "*** (skg (node (id test123) (source main) indefinitive)) Test Title\n" ); }

#[test]
fn test_metadata_ordering () {
  let t : TrueNode = TrueNode {
    viewStats : ViewNodeStats {
      cycle             : true,
      parentIsContainer : false,
      .. ViewNodeStats::default() },
    .. default_truenode ( ID::from ( "xyz" ),
                          "main".to_string(),
                          "Test".to_string() ) };
  let node : OrgNode = OrgNode {
    focused : false,
    folded  : false,
    kind    : OrgNodeKind::True ( t ), };
  let result : String =
    orgnode_to_text ( 1, &node )
    . expect ( "TrueNode rendering never fails" );
  assert_eq! ( result, "* (skg (node (id xyz) (source main) (viewStats cycle notInParent))) Test\n" ); }
