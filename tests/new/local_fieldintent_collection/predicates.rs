/// These tests pin the membership predicates of
/// server/from_text/local_fieldintent_collection/predicates.rs.
/// There is one test per condition each predicate encodes.

use skg::from_text::local_fieldintent_collection::predicates::{
  occurrence_child_counts_as_content,
  occurrence_child_counts_as_visible_content,
  member_counts_for_partnerFolder };
use skg::types::misc::{ID, SkgrepoName};
use skg::types::viewnode::{
  default_occurrence, NodeEditRequest, Editability,
  Occurrence };

fn base_occurrence (
) -> Occurrence {
  default_occurrence (
    ID::from ("n"),
    SkgrepoName::from ("main"),
    "n" . to_string() ) }

fn with_edit_request (
  edit_request : NodeEditRequest,
) -> Occurrence {
  let mut t : Occurrence =
    base_occurrence ();
  t . editability = Editability::Editable {
    body         : None,
    edit_request : Some (edit_request) };
  t }

#[test]
fn relation_folder_membership_conditions () {
  assert!( member_counts_for_partnerFolder (
    &base_occurrence () ));
  { // a child claiming nothing excludes.
    let mut t : Occurrence = base_occurrence ();
    t . claimsMembership = false;
    assert!( ! member_counts_for_partnerFolder (&t) ); }
  // A Delete edit request excludes; a NodeMerge edit request does not.
  assert!( ! member_counts_for_partnerFolder (
    &with_edit_request (NodeEditRequest::Delete) ));
  assert!( member_counts_for_partnerFolder (
    &with_edit_request (NodeEditRequest::NodeMerge (ID::from ("other"))) )); }

#[test]
fn content_membership_coincides_with_relation_folder_membership () {
  // The two predicates encode one condition today; if they ever
  // diverge, this test should be split per condition.
  let cases : Vec<Occurrence> = {
    let mut cases : Vec<Occurrence> =
      vec![ base_occurrence (),
            with_edit_request (NodeEditRequest::Delete),
            with_edit_request (NodeEditRequest::NodeMerge (ID::from ("other"))) ];
    { let mut t : Occurrence = base_occurrence ();
      t . claimsMembership = false;
      cases . push (t); }
    cases };
  for t in &cases {
    assert_eq!( occurrence_child_counts_as_content (t),
                member_counts_for_partnerFolder (t) ); }}

#[test]
fn visible_content_membership_conditions () {
  assert!( occurrence_child_counts_as_visible_content (
    &base_occurrence () ));
  { // a child claiming nothing excludes.
    let mut t : Occurrence = base_occurrence ();
    t . claimsMembership = false;
    assert!( ! occurrence_child_counts_as_visible_content (&t) ); }
  // A Delete edit request excludes; a NodeMerge edit request does not.
  assert!( ! occurrence_child_counts_as_visible_content (
    &with_edit_request (NodeEditRequest::Delete) ));
  assert!( occurrence_child_counts_as_visible_content (
    &with_edit_request (NodeEditRequest::NodeMerge (ID::from ("other"))) )); }
