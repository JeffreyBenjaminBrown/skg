/// These tests pin the membership predicates of
/// server/from_text/local_instruction_collection/predicates.rs.
/// There is one test per condition each predicate encodes
/// (TODO/local-instruction-collection/3_plan.org, "testing").

use skg::from_text::local_instruction_collection::predicates::{
  active_child_counts_as_content,
  active_child_counts_as_visible_content,
  inactiveNode_is_phantom,
  member_counts_for_partnerCol };
use skg::types::git::{MembershipAxes, Sign};
use skg::types::misc::{ID, SourceName};
use skg::types::viewnode::{
  default_truenode, EditRequest, IndefOrDef, InactiveNode, ParentIs,
  TrueNode };

fn base_truenode (
) -> TrueNode {
  default_truenode (
    ID::from ("n"),
    SourceName::from ("main"),
    "n" . to_string() ) }

fn with_edit_request (
  edit_request : EditRequest,
) -> TrueNode {
  let mut t : TrueNode =
    base_truenode ();
  t . indef_or_def = IndefOrDef::Definitive {
    body         : None,
    edit_request : Some (edit_request) };
  t }

#[test]
fn relation_collection_membership_conditions () {
  assert!( member_counts_for_partnerCol (
    &base_truenode () ));
  { // parentIs != Affected excludes.
    let mut t : TrueNode = base_truenode ();
    t . parentIs = ParentIs::Independent;
    assert!( ! member_counts_for_partnerCol (&t) ); }
  { // A negative staged membership axis (would-be diff phantom) excludes.
    let mut t : TrueNode = base_truenode ();
    t . membership . staged = Some (Sign::Minus);
    assert!( ! member_counts_for_partnerCol (&t) ); }
  { // A negative unstaged membership axis excludes.
    let mut t : TrueNode = base_truenode ();
    t . membership . unstaged = Some (Sign::Minus);
    assert!( ! member_counts_for_partnerCol (&t) ); }
  { // A negative unstaged existence axis (file deleted) excludes.
    let mut t : TrueNode = base_truenode ();
    t . existence . unstaged = Some (Sign::Minus);
    assert!( ! member_counts_for_partnerCol (&t) ); }
  { // A positive axis does not exclude.
    let mut t : TrueNode = base_truenode ();
    t . membership . unstaged = Some (Sign::Plus);
    assert!( member_counts_for_partnerCol (&t) ); }
  // A Delete edit request excludes; a NodeMerge edit request does not.
  assert!( ! member_counts_for_partnerCol (
    &with_edit_request (EditRequest::Delete) ));
  assert!( member_counts_for_partnerCol (
    &with_edit_request (EditRequest::NodeMerge (ID::from ("other"))) )); }

#[test]
fn content_membership_coincides_with_relation_collection_membership () {
  // The two predicates encode one condition today; if they ever
  // diverge, this test should be split per condition.
  let cases : Vec<TrueNode> = {
    let mut cases : Vec<TrueNode> =
      vec![ base_truenode (),
            with_edit_request (EditRequest::Delete),
            with_edit_request (EditRequest::NodeMerge (ID::from ("other"))) ];
    { let mut t : TrueNode = base_truenode ();
      t . parentIs = ParentIs::Independent;
      cases . push (t); }
    { let mut t : TrueNode = base_truenode ();
      t . membership . unstaged = Some (Sign::Minus);
      cases . push (t); }
    { let mut t : TrueNode = base_truenode ();
      t . existence . unstaged = Some (Sign::Minus);
      cases . push (t); }
    cases };
  for t in &cases {
    assert_eq!( active_child_counts_as_content (t),
                member_counts_for_partnerCol (t) ); }}

#[test]
fn visible_content_membership_conditions () {
  assert!( active_child_counts_as_visible_content (
    &base_truenode () ));
  { // parentIs != Affected excludes.
    let mut t : TrueNode = base_truenode ();
    t . parentIs = ParentIs::Independent;
    assert!( ! active_child_counts_as_visible_content (&t) ); }
  // A Delete edit request excludes; a NodeMerge edit request does not.
  assert!( ! active_child_counts_as_visible_content (
    &with_edit_request (EditRequest::Delete) ));
  assert!( active_child_counts_as_visible_content (
    &with_edit_request (EditRequest::NodeMerge (ID::from ("other"))) ));
  { // This pins an asymmetry: negative diff axes do NOT exclude
    // here, unlike in the contains and PartnerCol
    // predicates.
    let mut t : TrueNode = base_truenode ();
    t . membership . staged   = Some (Sign::Minus);
    t . membership . unstaged = Some (Sign::Minus);
    t . existence  . unstaged = Some (Sign::Minus);
    assert!( active_child_counts_as_visible_content (&t) ); }}

#[test]
fn inactive_phantomhood_conditions () {
  let inactive = | membership : MembershipAxes | -> InactiveNode {
    InactiveNode {
      id         : ID::from ("i"),
      source     : SourceName::from ("private"),
      membership } };
  assert!( ! inactiveNode_is_phantom ( &inactive (
    MembershipAxes::default() )));
  assert!( inactiveNode_is_phantom ( &inactive (
    MembershipAxes { staged   : Some (Sign::Minus),
                     unstaged : None } )));
  assert!( inactiveNode_is_phantom ( &inactive (
    MembershipAxes { staged   : None,
                     unstaged : Some (Sign::Minus) } )));
  // Positive axes are not phantomhood.
  assert!( ! inactiveNode_is_phantom ( &inactive (
    MembershipAxes { staged   : Some (Sign::Plus),
                     unstaged : Some (Sign::Plus) } ))); }
