// cargo nextest run --test grouped_saves -E 'test(messages_name_viewnode_ids::)'
//
// A message names each headline it concerns as an org subheadline
// under the message's headline: the title, then the graphnode ID, then
// the viewnode ID with the command that finds it, or "not yet saved"
// for a headline without one (typed, or a copy). A validation error's
// subheadlines nest under its '** Error N', after the error's own
// lines; a parse error and a warning end with theirs
// ('docs/api-and-formats.org', "How a message names a headline").

use skg::from_text::buffer_to_viewnodes::add_missing_info::EnrichmentProvenance;
use skg::dbs::in_rust_graph::InRustGraph;
use indoc::indoc;
use skg::from_text::buffer_to_viewnodes::uninterpreted::{
  headline_to_triple, org_to_viewforest};
use skg::from_text::buffer_to_viewnodes::validate_tree::contradictory_instructions::find_inconsistent_instructions;
use skg::from_text::buffer_to_viewnodes::validate_tree::find_buffer_errors_for_saving;
use skg::test_utils::run_with_test_stores;
use skg::types::errors::{BufferValidationError, HeadlineName, SaveError};
use skg::types::misc::{ID, SkgConfig, TantivyIndex};
use skg::types::save::format_save_error_as_org;
use skg::types::tree::forest::ViewForest;
use skg::types::viewnode::ViewnodeId;
use std::error::Error;

const FIXTURES : &str =
  "tests/merge/merge_nodes/fixtures";

/// The name of a headline titled 'title', showing graphnode 'gnid',
/// with viewnode ID 'viewnode_id'.
fn named (
  title       : &str,
  gnid       : &str,
  viewnode_id : &str,
) -> HeadlineName {
  HeadlineName::of ( Some ( &ViewnodeId (viewnode_id . to_string ()) ),
                     Some ( &ID::from (gnid) ),
                     title ) }

/// The subheadline naming a headline with a graphnode ID and a
/// viewnode ID.
fn subheadline (
  title       : &str,
  gnid       : &str,
  viewnode_id : &str,
) -> String {
  format! ( "*** {}\n- graphnode ID: {}\n- viewnode ID: {}. To find it: C-c g v {}\n",
            title, gnid, viewnode_id, viewnode_id ) }

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
  let (_viewforest, errors, warnings, _provenance)
    : (ViewForest, Vec<BufferValidationError>, Vec<String>, EnrichmentProvenance)
    = org_to_viewforest (buffer, &InRustGraph::new ()) . unwrap ();
  (errors, warnings) }

#[test]
fn messages_about_one_headline_name_it_as_a_subheadline_at_parse_time () {
  let (errors, warnings) = parse_errors_and_warnings ( indoc! {"
    * (skg (vnid 0a) node (gnid root) (skgRepos (nodeRepo main))) root
    ** (skg (vnid 0b) node (gnid gone) (skgRepos (nodeRepo main)) writeProtected (editRequest delete)) gone
    ** (skg (vnid 0c) aliasFolder) typed on a folder
    *** (skg (vnid 0d) alias) an alias
    with a body
    "} );
  assert! ( errors . contains (
              &BufferValidationError::EditRequestOnWriteProtectedOccurrence (
                ID::from ("gone"), named ("gone", "gone", "0b") )),
            "{:?}", errors );
  let org_form : String = org_form_of (&errors);
  assert! ( org_form . starts_with ("NOTHING WAS SAVED\n"), "{}", org_form );
  assert! ( org_form . contains ( &format! (
              "- To delete or merge this node, visit a editable view of it first (C-c g RET).\n{}",
              subheadline ("gone", "gone", "0b") )),
            "the subheadline follows the error's own lines:\n{}", org_form );
  assert! ( org_form . contains (
              "*** an alias\n- viewnode ID: 0d. To find it: C-c g v 0d\n" ),
            "an alias carries no graphnode ID:\n{}", org_form );
  assert! ( warnings . iter () . any ( |w|
              w . starts_with ( "Headline text on a aliasFolder is not saved" )
              && w . ends_with (
                "\n*** typed on a folder\n- viewnode ID: 0c. To find it: C-c g v 0c\n" )),
            "{:?}", warnings ); }

#[test]
fn messages_about_one_headline_name_it_as_a_subheadline_at_validation (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-messages-name-viewnode-ids-validation", FIXTURES,
    "/tmp/tantivy-test-messages-name-viewnode-ids-validation",
    |config, tantivy| Box::pin (
      one_headline_at_validation (config, tantivy) )) }

async fn one_headline_at_validation (
  config   : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let buffer : &str = indoc! {"
    * (skg (vnid 0e) node (gnid parent) (skgRepos (nodeRepo main)) (viewRequests editableView writeProtectedView)) parent
    ** (skg (vnid 0f) alias) a stray alias
    "};
  let errors : Vec<BufferValidationError> =
    find_buffer_errors_for_saving (
      &org_to_viewforest (buffer, &InRustGraph::new ()) . unwrap () . 0,
      config ) ?;
  assert! ( errors . contains (
              &BufferValidationError::WriteProtectAndEditableViewRequestsOnOneOccurrence (
                ID::from ("parent"), named ("parent", "parent", "0e") )),
            "{:?}", errors );
  assert! ( errors . iter () . any ( |e| matches! (
              e, BufferValidationError::LocalStructureViolation (..) )),
            "an alias outside an alias folder: {:?}", errors );
  let org_form : String = org_form_of (&errors);
  assert! ( org_form . contains ( &format! (
              "on another (C-c s d).\n{}", subheadline ("parent", "parent", "0e") )),
            "{}", org_form );
  assert! ( org_form . contains (
              "*** a stray alias\n- viewnode ID: 0f. To find it: C-c g v 0f\n" ),
            "{}", org_form );
  Ok (( )) }

#[test]
fn messages_about_several_occurrences_name_each_as_a_subheadline () {
  let viewforest : ViewForest =
    org_to_viewforest (indoc! {"
      * (skg (vnid 0a) node (gnid a) (skgRepos (nodeRepo main))) a
      * (skg (vnid 0b) node (gnid a) (skgRepos (nodeRepo main)) (editRequest delete)) a again
      "}, &InRustGraph::new ()) . unwrap () . 0;
  let (ambiguous_deletions, multiple_definers) =
    find_inconsistent_instructions (&viewforest);
  let both : Vec<HeadlineName> =
    vec! [ named ("a", "a", "0a"), named ("a again", "a", "0b") ];
  assert_eq! ( ambiguous_deletions, vec! [ (ID::from ("a"), both . clone ()) ] );
  assert_eq! ( multiple_definers, vec! [ (ID::from ("a"), both . clone ()) ] );
  let org_form : String = org_form_of ( &[
    BufferValidationError::Multiple_Defining_Viewnodes (
      ID::from ("a"), both ) ] );
  assert! ( org_form . contains ( &format! (
              "** Error 1\nID has multiple defining containers:\n- ID: a\n{}{}",
              subheadline ("a", "a", "0a"),
              subheadline ("a again", "a", "0b") )),
            "{}", org_form ); }

#[test]
fn a_metadata_parse_error_ends_with_its_headline () {
  let with_viewnode_id : String =
    headline_to_triple (
      "* (skg (vnid 0c) node (gnid a) (skgRepos (nodeRepo main)) (bogus 1)) a" )
    . unwrap_err ();
  assert! ( with_viewnode_id . ends_with ( &format! (
              "\n{}", subheadline ("a", "a", "0c") )),
            "{}", with_viewnode_id );
  let without_viewnode_id : String =
    headline_to_triple ( "* (skg node (gnid a) (skgRepos (nodeRepo main)) (bogus 1)) a typed" )
    . unwrap_err ();
  assert! ( without_viewnode_id . ends_with (
              "\n*** a typed\n- graphnode ID: a\n- not yet saved\n" ),
            "{}", without_viewnode_id ); }

#[test]
fn a_headline_without_a_viewnode_id_is_not_yet_saved () {
  // The second headline is a copy of the first, so it loses the viewnode ID
  // it carries; the third the user typed.
  let viewforest : ViewForest =
    org_to_viewforest (indoc! {"
      * (skg (vnid 0a) node (gnid a) (skgRepos (nodeRepo main))) a
      * (skg (vnid 0a) node (gnid a) (skgRepos (nodeRepo main))) a copied
      * (skg node (gnid a) (skgRepos (nodeRepo main))) a typed
      "}, &InRustGraph::new ()) . unwrap () . 0;
  let (_, multiple_definers) =
    find_inconsistent_instructions (&viewforest);
  let names : Vec<HeadlineName> = vec! [
    named ("a", "a", "0a"),
    HeadlineName::of ( None, Some (&ID::from ("a")), "a copied" ),
    HeadlineName::of ( None, Some (&ID::from ("a")), "a typed" ) ];
  assert_eq! ( multiple_definers, vec! [ (ID::from ("a"), names . clone ()) ] );
  let org_form : String = org_form_of ( &[
    BufferValidationError::Multiple_Defining_Viewnodes (
      ID::from ("a"), names ) ] );
  assert! ( org_form . contains ( &format! (
              "{}*** a copied\n- graphnode ID: a\n- not yet saved\n*** a typed\n- graphnode ID: a\n- not yet saved\n",
              subheadline ("a", "a", "0a") )),
            "{}", org_form ); }
