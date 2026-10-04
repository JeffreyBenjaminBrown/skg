/// These tests pin the membership predicates of
/// server/from_text/local_instruction_collection/predicates.rs.
/// There is one test per condition each predicate encodes
/// (TODO/local-instruction-collection/3_plan.org, "testing").

use skg::from_text::local_instruction_collection::predicates::{
  active_child_counts_as_content,
  active_child_counts_as_visible_content,
  member_counts_for_partnerFolder };
use skg::types::git::Sign;
use skg::types::misc::{ID, RepoName};
use skg::types::viewnode::{
  default_activeVognode, NodeEditRequest, Editability, AffectsParent,
  ActiveVognode };

fn base_activeVognode (
) -> ActiveVognode {
  default_activeVognode (
    ID::from ("n"),
    RepoName::from ("main"),
    "n" . to_string() ) }

fn with_edit_request (
  edit_request : NodeEditRequest,
) -> ActiveVognode {
  let mut t : ActiveVognode =
    base_activeVognode ();
  t . editability = Editability::Definitive {
    body         : None,
    edit_request : Some (edit_request) };
  t }

#[test]
fn relation_folder_membership_conditions () {
  assert!( member_counts_for_partnerFolder (
    &base_activeVognode () ));
  { // affectsParent != Affected excludes.
    let mut t : ActiveVognode = base_activeVognode ();
    t . affectsParent = AffectsParent::False;
    assert!( ! member_counts_for_partnerFolder (&t) ); }
  { // A negative staged relationship axis (would-be diff phantom) excludes.
    let mut t : ActiveVognode = base_activeVognode ();
    t . relationship_axes . staged = Some (Sign::Minus);
    assert!( ! member_counts_for_partnerFolder (&t) ); }
  { // A negative unstaged relationship axis excludes.
    let mut t : ActiveVognode = base_activeVognode ();
    t . relationship_axes . unstaged = Some (Sign::Minus);
    assert!( ! member_counts_for_partnerFolder (&t) ); }
  { // A negative unstaged node axis (file deleted) excludes.
    let mut t : ActiveVognode = base_activeVognode ();
    t . node_axes . unstaged = Some (Sign::Minus);
    assert!( ! member_counts_for_partnerFolder (&t) ); }
  { // A positive axis does not exclude.
    let mut t : ActiveVognode = base_activeVognode ();
    t . relationship_axes . unstaged = Some (Sign::Plus);
    assert!( member_counts_for_partnerFolder (&t) ); }
  // A Delete edit request excludes; a NodeMerge edit request does not.
  assert!( ! member_counts_for_partnerFolder (
    &with_edit_request (NodeEditRequest::Delete) ));
  assert!( member_counts_for_partnerFolder (
    &with_edit_request (NodeEditRequest::NodeMerge (ID::from ("other"))) )); }

#[test]
fn content_membership_coincides_with_relation_folder_membership () {
  // The two predicates encode one condition today; if they ever
  // diverge, this test should be split per condition.
  let cases : Vec<ActiveVognode> = {
    let mut cases : Vec<ActiveVognode> =
      vec![ base_activeVognode (),
            with_edit_request (NodeEditRequest::Delete),
            with_edit_request (NodeEditRequest::NodeMerge (ID::from ("other"))) ];
    { let mut t : ActiveVognode = base_activeVognode ();
      t . affectsParent = AffectsParent::False;
      cases . push (t); }
    { let mut t : ActiveVognode = base_activeVognode ();
      t . relationship_axes . unstaged = Some (Sign::Minus);
      cases . push (t); }
    { let mut t : ActiveVognode = base_activeVognode ();
      t . node_axes . unstaged = Some (Sign::Minus);
      cases . push (t); }
    cases };
  for t in &cases {
    assert_eq!( active_child_counts_as_content (t),
                member_counts_for_partnerFolder (t) ); }}

#[test]
fn visible_content_membership_conditions () {
  assert!( active_child_counts_as_visible_content (
    &base_activeVognode () ));
  { // affectsParent != Affected excludes.
    let mut t : ActiveVognode = base_activeVognode ();
    t . affectsParent = AffectsParent::False;
    assert!( ! active_child_counts_as_visible_content (&t) ); }
  // A Delete edit request excludes; a NodeMerge edit request does not.
  assert!( ! active_child_counts_as_visible_content (
    &with_edit_request (NodeEditRequest::Delete) ));
  assert!( active_child_counts_as_visible_content (
    &with_edit_request (NodeEditRequest::NodeMerge (ID::from ("other"))) ));
  { // This pins an asymmetry: negative diff axes do NOT exclude
    // here, unlike in the contains and PartnerFolder
    // predicates.
    let mut t : ActiveVognode = base_activeVognode ();
    t . relationship_axes . staged   = Some (Sign::Minus);
    t . relationship_axes . unstaged = Some (Sign::Minus);
    t . node_axes  . unstaged = Some (Sign::Minus);
    assert!( active_child_counts_as_visible_content (&t) ); }}
