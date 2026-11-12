use super::*;

use crate::serve::util::sexp_to_client_string;

const STRIP_CASES : &str =
  include_str! ("../shared/fold-and-focus-atom-strip-cases.txt");

/// The cases of the shared file, as (line, stripped form) pairs; see
/// its header for the format.
fn strip_cases (
) -> Vec<(String, String)> {
  let mut cases : Vec<(String, String)> = Vec::new ();
  let mut block : Vec<&str> = Vec::new ();
  for line in STRIP_CASES . lines () . chain (once ("")) {
    if line . starts_with ('#') { continue; }
    if ! line . is_empty () { block . push (line);
                              continue; }
    if block . is_empty () { continue; }
    assert_eq! ( block . len (), 2,
                 "a case is two lines, not {:?}", block );
    cases . push (( block [0] . to_string (), block [1] . to_string () ));
    block . clear (); }
  cases }

fn viewnode_ids (
  ids : &[&str],
) -> Vec<ViewnodeId> {
  ids . iter () . map ( |id| ViewnodeId (id . to_string ()) ) . collect () }

#[test]
fn every_shared_case_strips_as_the_file_says () {
  let cases : Vec<(String, String)> = strip_cases ();
  assert! ( cases . len () >= 20, "the file holds {} cases", cases . len () );
  for (line, expected) in cases {
    assert_eq! ( stripped_line (&line), expected, "stripping {:?}", line ); }}

const TEXT : &str = "\
preamble line
* (skg (vnid aa) node (gnid a)) a
a's body
** (skg (vnid ab) folded node (gnid b)) b
** (skg (vnid ac) node (gnid c)) c
c's body
";

#[test]
fn nothing_changed_is_an_empty_delta () {
  assert_eq! ( changed_headlines (TEXT, TEXT), Some (Vec::new ()) ); }

#[test]
fn a_changed_headline_is_listed () {
  let new_text : String = TEXT . replace ("gnid c)) c", "gnid c)) c, retitled");
  assert_eq! ( changed_headlines (TEXT, &new_text),
               Some (viewnode_ids (&["ac"])) ); }

#[test]
fn changed_headlines_are_listed_in_the_new_texts_order () {
  let new_text : String = TEXT
    . replace ("gnid c)) c", "gnid c)) c, retitled")
    . replace ("gnid a)) a", "gnid a) (view (containers 2))) a");
  assert_eq! ( changed_headlines (TEXT, &new_text),
               Some (viewnode_ids (&["aa", "ac"])) ); }

#[test]
fn a_fold_or_focus_atom_alone_is_not_listed () {
  let new_text : String = TEXT
    . replace ("(vnid ab) folded node", "(vnid ab) node")
    . replace ("(vnid ac) node", "(vnid ac) focused bodyFolded node");
  assert_eq! ( changed_headlines (TEXT, &new_text), Some (Vec::new ()) ); }

#[test]
fn a_changed_level_is_listed () {
  let new_text : String = TEXT . replace ("** (skg (vnid ac)", "*** (skg (vnid ac)");
  assert_eq! ( changed_headlines (TEXT, &new_text),
               Some (viewnode_ids (&["ac"])) ); }

#[test]
fn a_changed_body_is_whole () {
  let new_text : String = TEXT . replace ("c's body", "c's new body");
  assert_eq! ( changed_headlines (TEXT, &new_text), None );
  let body_added : String = TEXT . replace ("gnid b)) b\n", "gnid b)) b\nb's body\n");
  assert_eq! ( changed_headlines (TEXT, &body_added), None ); }

#[test]
fn a_changed_preamble_is_whole () {
  let new_text : String = TEXT . replace ("preamble line", "another preamble");
  assert_eq! ( changed_headlines (TEXT, &new_text), None ); }

#[test]
fn a_headline_without_a_viewnode_id_is_whole () {
  { // added
    let reference : String = TEXT . replace ("(skg (vnid ab) folded node", "(skg folded node");
    let new_text : String = TEXT . replace ("(skg (vnid ab) folded node", "(skg (vnid ad) folded node");
    assert_eq! ( changed_headlines (&reference, &new_text), None ); }
  { // missing
    let new_text : String = TEXT . replace ("(skg (vnid ab) folded node", "(skg folded node");
    assert_eq! ( changed_headlines (TEXT, &new_text), None ); }
  { // missing in both
    let both : String = TEXT . replace ("** (skg (vnid ac) node (gnid c)) c", "** c");
    assert_eq! ( changed_headlines (&both, &both), None ); }}

#[test]
fn a_different_viewnode_id_or_record_count_is_whole () {
  let renumbered : String = TEXT . replace ("(vnid ac)", "(vnid ad)");
  assert_eq! ( changed_headlines (TEXT, &renumbered), None );
  let one_more : String = format! ( "{}* (skg (vnid ad) node (gnid d)) d\n", TEXT );
  assert_eq! ( changed_headlines (TEXT, &one_more), None ); }

#[test]
fn a_last_headline_losing_its_newline_is_whole () {
  let reference : &str = "* (skg (vnid aa) node (gnid a)) a\n";
  let new_text  : &str = "* (skg (vnid aa) node (gnid a)) a";
  assert_eq! ( changed_headlines (reference, new_text), None ); }

#[test]
fn empty_texts_are_an_empty_delta () {
  assert_eq! ( changed_headlines ("", ""), Some (Vec::new ()) ); }

#[test]
fn the_entry_prints_bare_viewnode_ids () {
  assert_eq! ( sexp_to_client_string (
                 &changed_headlines_entry (&viewnode_ids (&["ab", "1c"])) ),
               "(changed-headlines (ab 1c))" );
  assert_eq! ( sexp_to_client_string ( &changed_headlines_entry (&[]) ),
               "(changed-headlines ())" ); }
