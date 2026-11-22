use super::*;

use crate::heralds::rule_texts_at_path;
use crate::serve::handlers::describe_heralds::describe_heralds_response;
use crate::types::misc::Skgrepo;

use std::collections::HashMap;
use std::path::PathBuf;

// The help cannot fall behind the heralds: every path the rule table
// renders text at has sentences, and none of them is the fallback.
#[test]
fn every_herald_the_rule_table_renders_has_a_sentence () {
  let mut problems : Vec<String> = Vec::new ();
  for path in text_emitting_rule_paths () . keys () {
    if folder_of_atom (path) . is_some () { continue; } // the next test's
    match sentences_covering (path) {
      None => problems . push ( format! ("{}: no sentence", path) ),
      Some (sentences) => for sentence in sentences {
        if sentence . contains ("No description yet") {
          problems . push ( format! ("{}: {}", path, sentence) ); }}, }}
  assert! ( problems . is_empty (), "heralds without a description: {:#?}", problems ); }

// Every folder has a sentence, which begins with its label as the rule
// table renders it.
#[test]
fn every_folder_has_a_sentence_beginning_with_its_label () {
  for folder in Folder::all () {
    let label : String = rule_texts_at_path ( folder . repr_in_client () ) . join (" ");
    assert! ( ! label . is_empty (), "{} has no label", folder . repr_in_client () );
    assert! ( folder_sentence (&folder) . starts_with ( &format! ("{}: ", label) ),
              "the sentence of {} does not begin with its label {:?}: {}",
              folder . repr_in_client (), label, folder_sentence (&folder) ); }}

// A herald added to the rule table without a sentence says so, rather
// than failing or vanishing.
#[test]
fn a_herald_without_a_sentence_says_so () {
  let config : SkgConfig = config_with_skgrepos ();
  let mut rule_paths : BTreeMap<String, Option<&'static str>> = text_emitting_rule_paths ();
  rule_paths . insert ( "view/somethingNew" . to_string (), Some ("high") );
  let metadata : Sexp = sexp::parse ("(view somethingNew)") . unwrap ();
  let parts : Vec<Sexp> = vec! [];
  let context : HeadlineContext = HeadlineContext::of (&parts, &config);
  let mut out : Vec<(usize, String)> = Vec::new ();
  describe_element ( &metadata, &[], &context, &rule_paths, &mut out );
  assert_eq! ( out . into_iter () . map ( |(_, s)| s ) . collect::<Vec<String>> (),
               vec! [ "No description yet for view/somethingNew." . to_string () ] ); }

#[test]
fn a_forks_folder_says_what_its_members_are () {
  let lines : Vec<String> = describe_heralds (
    "(skg (vnid 3) forksFolder)", None, &config_with_skgrepos () ) . unwrap ();
  assert_eq! ( lines . len (), 1 );
  assert! ( lines [0] . starts_with (
    "Its mirrors are these.: its mirrors: the notes this one forks or is \
     forked by, and the notes joined to those by forks." ));
  assert! ( lines [0] . contains ("'bK'") && lines [0] . contains ("'Kb'") ); }

#[test]
fn a_forkee_in_a_forks_folder () {
  let lines : Vec<String> = describe_heralds (
    "(skg (vnid 4) node (gnid g4) (skgRepos (nodeRepo theirs)) \
          (view (rels (contains (in 1) (out 8 (unintegrated 2))) \
                      (forks (in 1 (ancestors 2)))) \
                (birth (forks in 2)) repoBoundary))",
    None, &config_with_skgrepos () ) . unwrap ();
  assert_eq! ( lines, vec! [
    "1C2/8: 1 note contains this one; this note contains 8 notes; 2 of them \
     are new to you (neither the folder's note nor another note of yours \
     holds or hides them).",
    "bK: the headline two levels up (b) forks this one. This relationship \
     is why it is drawn here.",
    "⌂:th: it lives in the skgrepo 'th', which you do not own.",
  ] ); }

// A side with no members shows nothing in the token, so no clause.
#[test]
fn a_forker_with_no_content_says_nothing_of_its_content () {
  let lines : Vec<String> = describe_heralds (
    "(skg (vnid 8) node (gnid g8) \
          (view (rels (contains (in 1) (out 0 (unintegrated 0))) \
                      (forks (out 1 (ancestors 2)))) \
                (birth (forks out 2))))",
    None, &config_with_skgrepos () ) . unwrap ();
  assert_eq! ( lines, vec! [
    "1C: 1 note contains this one.",
    "Kb: this note forks the headline two levels up (b). This relationship \
     is why it is drawn here.",
  ] ); }

#[test]
fn an_ordinary_headline_with_several_heralds () {
  let lines : Vec<String> = describe_heralds (
    "(skg (vnid 5) node (gnid g5) writeProtected \
          (skgRepos (nodeRepo mine)) \
          (view omittedBody repoBoundary (attribution theirs) \
                (rels (contains (in 2 (ancestors 1)) (out 3)) \
                      (linksTo (in 5 (substantive 2))) \
                      (forks (in 1 (ancestors 1)))) \
                (props (aliases 2)) (birth (contains in 1))) \
          (editRequest delete))",
    None, &config_with_skgrepos () ) . unwrap ();
  assert_eq! ( lines, vec! [
    "☮: this occurrence is write-protected: a save refuses an edit to it \
     here, though the note may be yours and editable elsewhere in the view.",
    "B: it has a body, which this write-protected occurrence leaves out; \
     visit the note to read it.",
    "2/5L: 5 notes link to this one; 2 of them are substantive (they have a \
     body, content, or link to other notes too).",
    "aK: the headline above it (a) forks this one.",
    "delete: a pending request: the next save deletes this note.",
    "2aC3: 2 notes, among them the headline above it (a), contain this one; \
     this note contains 3 notes. This relationship is why it is drawn here.",
    "⌂:<mine/th>: it lives in your skgrepo 'mine' and was forked from a \
     note of 'th'.",
    "A2: it has 2 aliases, other names for it.",
  ] ); }

#[test]
fn a_headline_with_no_heralds_says_so () {
  assert_eq! ( describe_heralds ( "(skg (vnid 6) node (gnid g6))", None,
                                  &config_with_skgrepos () ) . unwrap (),
               vec! [ NO_HERALDS_SENTENCE . to_string () ] ); }

#[test]
fn the_endpoint_answers_lines_or_an_error () {
  let config : SkgConfig = config_with_skgrepos ();
  assert_eq! (
    describe_heralds_response (
      r#"((request . "describe heralds") (metadata . "(skg (vnid 7) node pendingDetachment)"))"#,
      &config ),
    "((response-type describe-heralds) (lines \"detach: the next save takes \
     this headline out of its parent's list (in a forks folder, ends the \
     fork), leaving the note itself; attaching it again (C-c s a; \
     :SkgAttach) first cancels that.\"))" );
  assert! ( describe_heralds_response (
      r#"((request . "describe heralds") (metadata . "(view)"))"#, &config )
    . contains ("(error ") ); }

/// A config with an owned skgrepo 'mine' and a foreign one 'theirs',
/// abbreviated 'th'.
fn config_with_skgrepos () -> SkgConfig {
  let skgrepo = |name : &str, abbreviation : Option<&str>, owned : bool| -> (SkgrepoName, Skgrepo) {
    ( SkgrepoName::from (name),
      Skgrepo { name : SkgrepoName::from (name),
                abbreviation : abbreviation . map (str::to_string),
                path : PathBuf::from (name), owned } ) };
  SkgConfig::dummyFromSkgrepos ( HashMap::from ( [
    skgrepo ("mine", None, true),
    skgrepo ("theirs", Some ("th"), false) ] )) }
