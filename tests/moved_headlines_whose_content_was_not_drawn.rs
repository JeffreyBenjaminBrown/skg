// cargo nextest run --test grouped_forks -E 'test(moved_headlines_whose_content_was_not_drawn::)'
//
// A headline the view drew without its node's content, or with only
// part of it, moved by the user to a position where it stands as
// editable content: the save never reads that node's content from the
// headline, and keeps it as on disk until a view draws it whole and the
// user edits it there ('docs/forks.org', "Moving a member out of the
// folder"). Each move takes the headline with its metadata and what
// stands under it, as an editor moves a subtree.
// The fixture: "main" is owned, "theirs" foreign.
// - N forks F, and hides k; F contains k and j; k contains c.
// - N2 forks F2; F2 contains k2 and j2; k2 contains c2; Y2 contains k2.
// - N3 forks F3 (theirs), and hides k3; F3 contains k3 and j3; k3
//   contains c3.
// - N4 contains g1 and z; G forks N4, and contains g1 and g2.
// The sub-tests:
// - k, moved out of F's folder of what N hides in it to stand under F,
//   is unhidden and keeps c; drawn write-protected there, it shows c
//   once the user asks for its editable view, and a cut of c there
//   empties it;
// - k2, moved out of F2's folder of what you keep elsewhere to stand
//   under F2, writes nothing;
// - F, moved out of N's forks folder into N's content (without its
//   folder of what N hides in it, which the save refuses outside the
//   folder), keeps k and j, and its new title; drawn whole there, a cut
//   of j empties it of j;
// - F3, foreign, moved likewise, keeps its content and forks nothing;
// - G, a forker as such, moved under z, keeps its content;
// - a headline typed under F, moved out, joins no list: the save warns
//   of the orphan, and F keeps its content.

use std::collections::BTreeMap;
use std::error::Error;

use crate::view_and_save_helpers::{
  contains_of, depth, forks_of, hides_of, ids, line_of, node, refusal_of,
  save, save_cleanly, skg_files, view_of, with_line_after_subtree_of};
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::test_utils::run_with_shared_test_stores;
use skg::types::config::SkgConfig;
use skg::types::misc::{ID, TantivyIndex};

const FIXTURES : &str = "tests/moved_headlines_whose_content_was_not_drawn/fixtures";

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-moved-headlines-whose-content-was-not-drawn",
    |s| Box::pin ( async move {
      s . reset ("a_child_moved_out_of_the_hidden_in_folder_keeps_its_content", FIXTURES) ?;
      a_child_moved_out_of_the_hidden_in_folder_keeps_its_content (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_child_moved_out_of_the_integrated_elsewhere_folder_keeps_its_content", FIXTURES) ?;
      a_child_moved_out_of_the_integrated_elsewhere_folder_keeps_its_content (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_member_moved_out_of_the_forks_folder_keeps_its_content", FIXTURES) ?;
      a_member_moved_out_of_the_forks_folder_keeps_its_content (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_foreign_member_moved_out_forks_nothing", FIXTURES) ?;
      a_foreign_member_moved_out_forks_nothing (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_forker_moved_out_keeps_its_content", FIXTURES) ?;
      a_forker_moved_out_keeps_its_content (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_headline_typed_under_a_moved_member_joins_no_list", FIXTURES) ?;
      a_headline_typed_under_a_moved_member_joins_no_list (&s . config, &mut s . tantivy) . await ?;
      Ok (( )) } )) }

//
// Helpers
//

/// The index of the first line of 'lines' showing 'gnid'.
fn index_of (
  lines : &[&str],
  gnid  : &str,
) -> usize {
  lines . iter ()
    . position ( |line| line . contains (&format! ("(gnid {})", gnid)) )
    . unwrap_or_else ( || panic! ( "no line shows {} in:\n{}", gnid, lines . join ("\n") )) }

/// The end of the subtree whose headline is 'lines [at]': the index
/// after its last line.
fn end_of_subtree (
  lines : &[&str],
  at    : usize,
) -> usize {
  let top : usize = depth (lines [at]);
  at + 1 + lines [at + 1 ..] . iter ()
    . take_while ( |line| depth (line) > top )
    . count () }

/// 'buf' with the first headline showing 'moved', and what stands
/// under it, moved to be the last child of the first headline showing
/// 'target' (found once the subtree is out), its stars shifted to its
/// new depth, its metadata kept: as an editor moves a subtree.
fn moved_under (
  buf    : &str,
  moved  : &str,
  target : &str,
) -> String {
  let lines : Vec<&str> = buf . lines () . collect ();
  let from : usize = index_of (&lines, moved);
  let to : usize = end_of_subtree (&lines, from);
  let subtree : Vec<&str> = lines [from .. to] . to_vec ();
  let rest : Vec<&str> = [ &lines [.. from], &lines [to ..] ] . concat ();
  let at : usize = index_of (&rest, target);
  let insert_at : usize = end_of_subtree (&rest, at);
  let new_depth : usize = depth (rest [at]) + 1;
  let old_depth : usize = depth (subtree [0]);
  let shifted : Vec<String> = subtree . iter ()
    . map ( |line| format! ( "{}{}", "*" . repeat ( depth (line) + new_depth - old_depth ),
                             &line [ depth (line) .. ] ))
    . collect ();
  let mut out : Vec<String> = rest [.. insert_at] . iter () . map ( |line| line . to_string () ) . collect ();
  out . extend (shifted);
  out . extend ( rest [insert_at ..] . iter () . map ( |line| line . to_string () ));
  out . join ("\n") + "\n" }

/// 'buf' less the folders directly under the first headline showing
/// 'member' that set its content aside, and what stands under them.
fn without_set_aside_folders_under (
  buf    : &str,
  member : &str,
) -> String {
  let lines : Vec<&str> = buf . lines () . collect ();
  let at : usize = index_of (&lines, member);
  let end : usize = end_of_subtree (&lines, at);
  let member_depth : usize = depth (lines [at]);
  let mut kept : Vec<&str> = lines [.. at + 1] . to_vec ();
  let mut i : usize = at + 1;
  while i < end {
    let is_set_aside_folder : bool =
      depth (lines [i]) == member_depth + 1
      && [ "integratedElsewhereFolder", "hiddenInForkPartnerFolder", "hiddenByForkerFolder" ]
         . iter () . any ( |atom| lines [i] . contains (&format! ("(skg {})", atom)) );
    if is_set_aside_folder { i = end_of_subtree (&lines, i); }
    else { kept . push (lines [i]); i += 1; }}
  kept . extend ( &lines [end ..] );
  kept . join ("\n") + "\n" }

/// The line at depth 'at_depth' showing 'gnid', and the lines of its
/// content: the headlines one deeper, up to the next line no deeper
/// than it.
fn headline_and_content_at_depth<'a> (
  buf      : &'a str,
  gnid     : &str,
  at_depth : usize,
) -> (&'a str, Vec<&'a str>) {
  let lines : Vec<&str> = buf . lines () . collect ();
  let at : usize = lines . iter ()
    . position ( |line| depth (line) == at_depth
                        && line . contains (&format! ("(gnid {})", gnid)) )
    . unwrap_or_else ( || panic! ( "no line at depth {} shows {} in:\n{}", at_depth, gnid, buf ));
  let content : Vec<&str> = lines [at + 1 .. end_of_subtree (&lines, at)] . iter ()
    . filter ( |line| depth (line) == at_depth + 1 )
    . copied ()
    . collect ();
  (lines [at], content) }

/// Whether the lines 'content' show 'gnids' in that order, and nothing
/// else.
fn shows (
  content : &[&str],
  gnids   : &[&str],
) -> bool {
  content . len () == gnids . len ()
  && content . iter () . zip (gnids)
     . all ( |(line, gnid)| line . contains (&format! ("(gnid {})", gnid)) ) }

/// The member 'member' of 'note''s forks folder moved, with what
/// stands under it but its set-aside folders, to be the last child of
/// 'target'. Moved with those folders the save refuses it, writing
/// nothing.
async fn member_moved_under (
  note    : &str,
  member  : &str,
  target  : &str,
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<String, Box<dyn Error>> {
  let view : String = view_of (config, tantivy, note) ?;
  assert! ( line_of (&view, member) . contains ("contentInPart"),
            "the member shows its content in part:\n{}", view );
  let before : BTreeMap<String, String> = skg_files (config) ?;
  let refusal : String = refusal_of (
    &moved_under (&view, member, target), note, config, tantivy ) . await ?;
  assert! ( refusal . contains ("Occurrence has a child whose structure belongs elsewhere"),
            "{}", refusal );
  assert_eq! ( skg_files (config) ?, before, "nothing is written" );
  Ok ( moved_under ( &without_set_aside_folders_under (&view, member), member, target )) }

//
// The sub-tests
//

/// The reported case. N hides k, which F contains; the view draws k
/// write-protected and folded, without c, in F's folder of what N hides
/// in it. Moved from there to stand under F, it is unhidden, and keeps
/// c. Still write-protected, it is drawn showing c once the user asks
/// for its editable view; then a cut of c there empties it.
async fn a_child_moved_out_of_the_hidden_in_folder_keeps_its_content (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  assert! ( line_of (&view, "k") . contains ("writeProtected")
            && view . contains ("hiddenInForkPartnerFolder")
            && ! view . contains ("(gnid c)"),
            "k stands write-protected in the hidden-in folder, without c:\n{}", view );
  let saved : String = save_cleanly (
    &moved_under (&view, "k", "F"), "N", config, tantivy ) . await ?;
  assert_eq! ( hides_of (config, "N") ?, Vec::<ID>::new (), "k is unhidden" );
  assert_eq! ( contains_of (config, "k") ?, ids (&["c"]), "k keeps c" );
  assert_eq! ( contains_of (config, "F") ?, ids (&["k", "j"]) );
  assert_eq! ( contains_of (config, "N") ?, Vec::<ID>::new (), "N files nothing" );
  let (k_line, _) : (&str, Vec<&str>) = headline_and_content_at_depth (&saved, "k", 4);
  assert! ( k_line . contains ("writeProtected"), "k stays write-protected:\n{}", saved );
  let requested : String = saved . replacen (
    k_line, &k_line . replacen ("writeProtected", "writeProtected (viewRequests editableView)", 1), 1 );
  let drawn : String = save_cleanly (&requested, "N", config, tantivy) . await ?;
  let (k_line, k_content) : (&str, Vec<&str>) = headline_and_content_at_depth (&drawn, "k", 4);
  assert! ( ! k_line . contains ("writeProtected") && shows (&k_content, &["c"]),
            "k is drawn editable, showing c:\n{}", drawn );
  save_cleanly ( &drawn . replacen (&format! ("{}\n", k_content [0]), "", 1),
                 "N", config, tantivy ) . await ?;
  assert_eq! ( contains_of (config, "k") ?, Vec::<ID>::new (),
               "a cut of drawn content is read" );
  Ok (( )) }

/// Y2 keeps k2, so the view draws k2 write-protected and folded,
/// without c2, in F2's folder of what you keep elsewhere. Moved from
/// there to stand under F2, with its containers folder, it writes
/// nothing: k2 keeps c2.
async fn a_child_moved_out_of_the_integrated_elsewhere_folder_keeps_its_content (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N2") ?;
  assert! ( line_of (&view, "k2") . contains ("writeProtected")
            && view . contains ("integratedElsewhereFolder")
            && ! view . contains ("(gnid c2)"),
            "k2 stands write-protected in the integrated-elsewhere folder, without c2:\n{}", view );
  let before : BTreeMap<String, String> = skg_files (config) ?;
  save_cleanly ( &moved_under (&view, "k2", "F2"), "N2", config, tantivy ) . await ?;
  assert_eq! ( skg_files (config) ?, before, "nothing is written" );
  assert_eq! ( contains_of (config, "k2") ?, ids (&["c2"]) );
  Ok (( )) }

/// F, an editable member as such, shows j, its content less what N
/// hides (k). Moved into N's content, retitled, it keeps k and j, and
/// takes the new title; N stops forking it and contains it. Drawn there
/// whole, without 'contentInPart', a cut of j is read.
async fn a_member_moved_out_of_the_forks_folder_keeps_its_content (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let moved : String = member_moved_under ("N", "F", "N", config, tantivy) . await ?;
  let f_line : &str = line_of (&moved, "F");
  let retitled : String = moved . replacen (
    f_line, &format! ( "{} F, moved", &f_line [.. f_line . rfind (") ") . unwrap () + 1] ), 1 );
  let saved : String = save_cleanly (&retitled, "N", config, tantivy) . await ?;
  assert_eq! ( node (config, "F") ? . title, "F, moved", "its text was drawn, and is read" );
  assert_eq! ( contains_of (config, "F") ?, ids (&["k", "j"]), "F keeps its content" );
  assert_eq! ( contains_of (config, "N") ?, ids (&["F"]) );
  assert_eq! ( forks_of (config, "N") ?, Vec::<ID>::new () );
  let (f_line, f_content) : (&str, Vec<&str>) = headline_and_content_at_depth (&saved, "F", 2);
  assert! ( ! f_line . contains ("contentInPart") && shows (&f_content, &["k", "j"]),
            "F is drawn whole in N's content:\n{}", saved );
  save_cleanly ( &saved . replacen (&format! ("{}\n", f_content [1]), "", 1),
                 "N", config, tantivy ) . await ?;
  assert_eq! ( contains_of (config, "F") ?, ids (&["k"]),
               "a cut of drawn content is read" );
  Ok (( )) }

/// F3, foreign, an editable member as such of N3's forks folder, moved
/// into N3's content, keeps its content, and so is not forked.
async fn a_foreign_member_moved_out_forks_nothing (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let moved : String = member_moved_under ("N3", "F3", "N3", config, tantivy) . await ?;
  let response : SaveResponse = save (&moved, "N3", config, tantivy) . await ?;
  assert! ( response . errors . is_empty () && response . fork_confirmation . is_none (),
            "the save forks nothing: {:?}\n{}", response . errors, response . saved_view );
  assert_eq! ( contains_of (config, "F3") ?, ids (&["k3", "j3"]) );
  assert_eq! ( contains_of (config, "N3") ?, ids (&["F3"]) );
  Ok (( )) }

/// G, which forks N4, stands in N4's forks folder as a forker as
/// such, claiming nothing, showing g2 (N4 contains g1). Moved under z,
/// it keeps g1 and g2; claiming nothing, it joins no list there.
async fn a_forker_moved_out_keeps_its_content (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let moved : String = member_moved_under ("N4", "G", "z", config, tantivy) . await ?;
  save_cleanly (&moved, "N4", config, tantivy) . await ?;
  assert_eq! ( contains_of (config, "G") ?, ids (&["g1", "g2"]) );
  assert_eq! ( forks_of (config, "G") ?, ids (&["N4"]) );
  assert_eq! ( contains_of (config, "z") ?, Vec::<ID>::new () );
  Ok (( )) }

/// A headline typed under F, moved out of the forks folder in the same
/// save, joins no list, since the save reads nothing under F: it warns
/// of the orphan graphnode it creates. F keeps its content.
async fn a_headline_typed_under_a_moved_member_joins_no_list (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let moved : String = member_moved_under ("N", "F", "N", config, tantivy) . await ?;
  let typed : String = with_line_after_subtree_of (&moved, "F", "*** typed under F");
  let response : SaveResponse = save (&typed, "N", config, tantivy) . await ?;
  assert! ( response . errors . is_empty (), "{:?}", response . errors );
  assert! ( response . warnings . iter () . any ( |warning|
              warning . contains ("Created an orphan graphnode")
              && warning . contains ("typed under F") ),
            "{:?}", response . warnings );
  assert_eq! ( contains_of (config, "F") ?, ids (&["k", "j"]) );
  Ok (( )) }
