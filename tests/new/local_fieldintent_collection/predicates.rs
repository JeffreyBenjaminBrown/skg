/// These tests pin the membership predicates of
/// server/from_text/local_fieldintent_collection/predicates.rs.
/// There is one test per condition each predicate encodes
/// (TODO/DONE/local-fieldintent-collection/3_plan.org, "testing").

use skg::from_text::local_fieldintent_collection::predicates::{
  unrestricted_child_counts_as_content,
  unrestricted_child_counts_as_visible_content,
  member_counts_for_partnerFolder };
use skg::types::git::Sign;
use skg::types::misc::{ID, SkgRepoName};
use skg::types::viewnode::{
  default_unrestrictedVognode, NodeEditRequest, Editability, AffectsParent,
  UnrestrictedVognode };

fn base_unrestrictedVognode (
) -> UnrestrictedVognode {
  default_unrestrictedVognode (
    ID::from ("n"),
    SkgRepoName::from ("main"),
    "n" . to_string() ) }

fn with_edit_request (
  edit_request : NodeEditRequest,
) -> UnrestrictedVognode {
  let mut t : UnrestrictedVognode =
    base_unrestrictedVognode ();
  t . editability = Editability::Editable {
    body         : None,
    edit_request : Some (edit_request) };
  t }

#[test]
fn relation_folder_membership_conditions () {
  assert!( member_counts_for_partnerFolder (
    &base_unrestrictedVognode () ));
  { // affectsParent != True excludes.
    let mut t : UnrestrictedVognode = base_unrestrictedVognode ();
    t . affectsParent = AffectsParent::False;
    assert!( ! member_counts_for_partnerFolder (&t) ); }
  { // A negative staged relationship axis (would-be diff phantom) excludes.
    let mut t : UnrestrictedVognode = base_unrestrictedVognode ();
    t . relationship_axes . staged = Some (Sign::Minus);
    assert!( ! member_counts_for_partnerFolder (&t) ); }
  { // A negative unstaged relationship axis excludes.
    let mut t : UnrestrictedVognode = base_unrestrictedVognode ();
    t . relationship_axes . unstaged = Some (Sign::Minus);
    assert!( ! member_counts_for_partnerFolder (&t) ); }
  { // A negative unstaged node axis (file deleted) excludes.
    let mut t : UnrestrictedVognode = base_unrestrictedVognode ();
    t . node_axes . unstaged = Some (Sign::Minus);
    assert!( ! member_counts_for_partnerFolder (&t) ); }
  { // A positive axis does not exclude.
    let mut t : UnrestrictedVognode = base_unrestrictedVognode ();
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
  let cases : Vec<UnrestrictedVognode> = {
    let mut cases : Vec<UnrestrictedVognode> =
      vec![ base_unrestrictedVognode (),
            with_edit_request (NodeEditRequest::Delete),
            with_edit_request (NodeEditRequest::NodeMerge (ID::from ("other"))) ];
    { let mut t : UnrestrictedVognode = base_unrestrictedVognode ();
      t . affectsParent = AffectsParent::False;
      cases . push (t); }
    { let mut t : UnrestrictedVognode = base_unrestrictedVognode ();
      t . relationship_axes . unstaged = Some (Sign::Minus);
      cases . push (t); }
    { let mut t : UnrestrictedVognode = base_unrestrictedVognode ();
      t . node_axes . unstaged = Some (Sign::Minus);
      cases . push (t); }
    cases };
  for t in &cases {
    assert_eq!( unrestricted_child_counts_as_content (t),
                member_counts_for_partnerFolder (t) ); }}

#[test]
fn visible_content_membership_conditions () {
  assert!( unrestricted_child_counts_as_visible_content (
    &base_unrestrictedVognode () ));
  { // affectsParent != True excludes.
    let mut t : UnrestrictedVognode = base_unrestrictedVognode ();
    t . affectsParent = AffectsParent::False;
    assert!( ! unrestricted_child_counts_as_visible_content (&t) ); }
  // A Delete edit request excludes; a NodeMerge edit request does not.
  assert!( ! unrestricted_child_counts_as_visible_content (
    &with_edit_request (NodeEditRequest::Delete) ));
  assert!( unrestricted_child_counts_as_visible_content (
    &with_edit_request (NodeEditRequest::NodeMerge (ID::from ("other"))) ));
  { // This pins an asymmetry: negative diff axes do NOT exclude
    // here, unlike in the contains and PartnerFolder
    // predicates.
    let mut t : UnrestrictedVognode = base_unrestrictedVognode ();
    t . relationship_axes . staged   = Some (Sign::Minus);
    t . relationship_axes . unstaged = Some (Sign::Minus);
    t . node_axes  . unstaged = Some (Sign::Minus);
    assert!( unrestricted_child_counts_as_visible_content (&t) ); }}
