// cargo nextest run --test grouped_saves -E 'test(messages_name_view_ids::)'
//
// A message about one headline names its view id after the node's ID;
// a message about several occurrences of a node lists their view
// ids; a metadata parse error ends by naming its headline; and a
// headline without a view id (typed, or a copy) is named by its
// title ('docs/api-and-formats.org', "Save buffer").

use indoc::indoc;
use skg::from_text::buffer_to_viewnodes::uninterpreted::{
  headline_to_triple, org_to_uninterpreted_viewforest};
use skg::from_text::buffer_to_viewnodes::validate_tree::contradictory_instructions::find_inconsistent_instructions;
use skg::from_text::buffer_to_viewnodes::validate_tree::find_buffer_errors_for_saving;
use skg::test_utils::run_with_test_stores;
use skg::types::errors::{BufferValidationError, HeadlineName, SaveError};
use skg::types::misc::{ID, SkgConfig, TantivyIndex};
use skg::types::save::format_save_error_as_org;
use skg::types::tree::forest::MpViewForest;
use skg::types::viewnode::Vid;
use std::error::Error;

const FIXTURES : &str =
  "tests/merge/merge_nodes/fixtures";

fn view_id (
  text : &str,
) -> HeadlineName {
  HeadlineName::ViewId ( Vid (text . to_string ()) ) }

/// The org form a save refusal of 'errors' shows the client.
fn org_form_of (
  errors : &[BufferValidationError],
) -> String {
  format_save_error_as_org (
    &SaveError::BufferValidationErrors {
      errors   : errors . to_vec (),
      warnings : Vec::new () } ) }

/// The parse errors and warnings of 'buffer'.
fn parse_errors_and_warnings (
  buffer : &str,
) -> (Vec<BufferValidationError>, Vec<String>) {
  let (_viewforest, errors, warnings)
    : (MpViewForest, Vec<BufferValidationError>, Vec<String>)
    = org_to_uninterpreted_viewforest (buffer) . unwrap ();
  (errors, warnings) }

#[test]
fn messages_about_one_headline_name_its_view_id_at_parse_time () {
  let (errors, warnings) = parse_errors_and_warnings ( indoc! {"
    * (skg (node (id root) (repo main)) (vid 0a)) root
    ** (skg (node (id gone) (repo main) writeProtected (editRequest delete)) (vid 0b)) gone
    ** (skg aliasFolder (vid 0c)) typed on a folder
    *** (skg alias (vid 0d)) an alias
    with a body
    "} );
  let edit_request : &BufferValidationError =
    errors . iter () . find ( |e| matches! (
      e, BufferValidationError::EditRequestOnWriteProtectedOccurrence (..) ))
    . expect ("an edit request on a write-protected node");
  assert_eq! ( edit_request,
               &BufferValidationError::EditRequestOnWriteProtectedOccurrence (
                 ID::from ("gone"), view_id ("0b") ));
  assert! ( edit_request . to_string () . contains (
              "node ID(\"gone\") (view id 0b)" ),
            "{}", edit_request );
  let body : &BufferValidationError =
    errors . iter () . find ( |e| matches! (
      e, BufferValidationError::Body_of_NonVognode (..) ))
    . expect ("a body on an alias");
  assert! ( body . to_string () . starts_with (
              "alias node (view id 0d) should not have a body" ),
            "{}", body );
  let org_form : String = org_form_of (&errors);
  assert! ( org_form . contains ("- view id: 0b\n"), "{}", org_form );
  assert! ( org_form . contains ("- view id: 0d\n"), "{}", org_form );
  assert! ( warnings . iter () . any ( |w| w . starts_with (
              "Headline text on a aliasFolder (view id 0c) is not saved" )),
            "{:?}", warnings ); }

#[test]
fn messages_about_one_headline_name_its_view_id_at_validation (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-messages-name-view-ids-validation", FIXTURES,
    "/tmp/tantivy-test-messages-name-view-ids-validation",
    |config, tantivy| Box::pin (
      one_headline_at_validation (config, tantivy) )) }

async fn one_headline_at_validation (
  config   : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let buffer : &str = indoc! {"
    * (skg (node (id parent) (repo main) (viewRequests editableView writeProtectedView)) (vid 0e)) parent
    ** (skg alias (vid 0f)) a stray alias
    "};
  let errors : Vec<BufferValidationError> =
    find_buffer_errors_for_saving (
      &org_to_uninterpreted_viewforest (buffer) . unwrap () . 0,
      config ) ?;
  assert! ( errors . contains (
              &BufferValidationError::WriteProtectAndEditableViewRequestsOnOneOccurrence (
                ID::from ("parent"), view_id ("0e") )),
            "{:?}", errors );
  assert! ( errors . iter () . any ( |e| matches! (
              e, BufferValidationError::LocalStructureViolation (..) )
              && e . to_string () . starts_with (
                "Local structure violation at ID ID(\"parent\") (view id 0f): " )),
            "an alias outside an alias folder: {:?}", errors );
  let org_form : String = org_form_of (&errors);
  assert! ( org_form . contains ("- view id: 0e\n"), "{}", org_form );
  assert! ( org_form . contains ("- view id: 0f\n"), "{}", org_form );
  Ok (( )) }

#[test]
fn messages_about_several_occurrences_list_their_view_ids () {
  let viewforest : MpViewForest =
    org_to_uninterpreted_viewforest ( indoc! {"
      * (skg (node (id a) (repo main)) (vid 0a)) a
      * (skg (node (id a) (repo main) (editRequest delete)) (vid 0b)) a again
      "} ) . unwrap () . 0;
  let (ambiguous_deletions, multiple_definers) =
    find_inconsistent_instructions (&viewforest);
  let both : Vec<HeadlineName> =
    vec! [ view_id ("0a"), view_id ("0b") ];
  assert_eq! ( ambiguous_deletions, vec! [ (ID::from ("a"), both . clone ()) ] );
  assert_eq! ( multiple_definers, vec! [ (ID::from ("a"), both . clone ()) ] );
  let error : BufferValidationError =
    BufferValidationError::Multiple_Defining_Viewnodes (
      ID::from ("a"), both );
  assert! ( error . to_string () . ends_with ("are editable (view ids 0a, 0b)"),
            "{}", error );
  let org_form : String = org_form_of (&[error]);
  assert! ( org_form . contains ("- view ids: 0a, 0b\n"), "{}", org_form ); }

#[test]
fn a_metadata_parse_error_names_its_headline () {
  let with_vid : String =
    headline_to_triple (
      "* (skg (node (id a) (repo main) (bogus 1)) (vid 0c)) a" )
    . unwrap_err ();
  assert! ( with_vid . ends_with ("(view id 0c)"), "{}", with_vid );
  let without_vid : String =
    headline_to_triple ( "* (skg (node (id a) (repo main) (bogus 1))) a typed" )
    . unwrap_err ();
  assert! ( without_vid . ends_with ("(headline \"a typed\")"),
            "{}", without_vid ); }

#[test]
fn a_headline_without_a_view_id_is_named_by_its_title () {
  // The second headline is a copy of the first, so it loses the vid
  // it carries; the third the user typed.
  let viewforest : MpViewForest =
    org_to_uninterpreted_viewforest ( indoc! {"
      * (skg (node (id a) (repo main)) (vid 0a)) a
      * (skg (node (id a) (repo main)) (vid 0a)) a copied
      * (skg (node (id a) (repo main))) a typed
      "} ) . unwrap () . 0;
  let (_, multiple_definers) =
    find_inconsistent_instructions (&viewforest);
  let names : Vec<HeadlineName> = vec! [
    view_id ("0a"),
    HeadlineName::Title ("a copied" . to_string ()),
    HeadlineName::Title ("a typed" . to_string ()) ];
  assert_eq! ( multiple_definers, vec! [ (ID::from ("a"), names . clone ()) ] );
  let error : BufferValidationError =
    BufferValidationError::Multiple_Defining_Viewnodes (
      ID::from ("a"), names );
  assert! ( error . to_string () . ends_with (
              "(view id 0a, headline \"a copied\", headline \"a typed\")" ),
            "{}", error );
  let org_form : String = org_form_of (&[error]);
  assert! ( org_form . contains (
              "- occurrences: view id 0a, headline \"a copied\", headline \"a typed\"\n" ),
            "{}", org_form ); }
