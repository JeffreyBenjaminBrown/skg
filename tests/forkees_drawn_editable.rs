// cargo nextest run --test grouped_forks -E 'test(forkees_drawn_editable::)'
//
// Forkees as such drawn editable ('TODO/every-mirror/2_plan.org', slice
// 2; 'docs/glossary.org', "forkee as such"). N, owned, contains n1,
// forks E, owned, and F, foreign, and hides H, which E contains, and
// G, which F contains. E contains e1, H, e2; F contains f1, G. FN,
// foreign, forks E. Each sub-test edits a view and saves it:
// - a fresh view of N draws E and F editable, each showing its
//   content less what N hides, and its folder of what N hides there;
// - saved unchanged, it writes nothing;
// - cutting e2 from E makes N hide e2;
// - E's title, edited there, is saved; F's is refused;
// - a headline typed under E joins E's content, after the sibling it
//   follows; one typed under F is refused;
// - a node moved from N's content to under E joins E's content, as a
//   typed one does, and leaves N; one moved under F is refused;
// - with a node budget of 1, E and F are drawn write-protected, bare;
// - a buffer drawing E write-protected (as an older server did)
//   rerenders as it is, and an editable-view request on N extends E;
// - a cut from E in a view of FN, foreign, is refused.

use std::error::Error;
use std::collections::BTreeMap;

use crate::view_and_save_helpers::{
  children_of, depth, ids, line_of, moved_after, node, refusal_of, save_cleanly,
  skg_files, under, view_of, with_line_after_subtree_of, without_subtree_of};
use skg::test_utils::run_with_shared_test_stores;
use skg::types::misc::{ID, SkgConfig, TantivyIndex, members_of};
use skg::types::nodes::complete::Graphnode;

const FIXTURES : &str = "tests/forkees_drawn_editable/fixtures";

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-forkees-drawn-editable",
    |s| Box::pin ( async move {
      s . reset ("a_fresh_view_draws_each_forkee_editable", FIXTURES) ?;
      a_fresh_view_draws_each_forkee_editable (&s . config, &mut s . tantivy) . await ?;
      s . reset ("an_unchanged_save_writes_nothing", FIXTURES) ?;
      an_unchanged_save_writes_nothing (&s . config, &mut s . tantivy) . await ?;
      s . reset ("cutting_from_an_owned_forkee_hides", FIXTURES) ?;
      cutting_from_an_owned_forkee_hides (&s . config, &mut s . tantivy) . await ?;
      s . reset ("an_owned_forkees_title_is_saved", FIXTURES) ?;
      an_owned_forkees_title_is_saved (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_foreign_forkees_title_is_refused", FIXTURES) ?;
      a_foreign_forkees_title_is_refused (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_child_typed_under_an_owned_forkee_joins_its_content", FIXTURES) ?;
      a_child_typed_under_an_owned_forkee_joins_its_content (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_child_typed_under_a_foreign_forkee_is_refused", FIXTURES) ?;
      a_child_typed_under_a_foreign_forkee_is_refused (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_node_moved_under_an_owned_forkee_joins_its_content", FIXTURES) ?;
      a_node_moved_under_an_owned_forkee_joins_its_content (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_node_moved_under_a_foreign_forkee_is_refused", FIXTURES) ?;
      a_node_moved_under_a_foreign_forkee_is_refused (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_spent_budget_leaves_forkees_write_protected", FIXTURES) ?;
      a_spent_budget_leaves_forkees_write_protected (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_write_protected_forkee_stays_until_its_forker_is_extended", FIXTURES) ?;
      a_write_protected_forkee_stays_until_its_forker_is_extended (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_cut_under_a_foreign_forker_is_refused", FIXTURES) ?;
      a_cut_under_a_foreign_forker_is_refused (&s . config, &mut s . tantivy) . await ?;
      Ok (( )) } )) }

//
// The sub-tests
//

/// E, owned, and F, foreign, each stand in N's forks folder editable,
/// claiming, showing their content less what N hides (e1 and e2; f1),
/// with the folder of what N hides in it first (H; G), folded.
async fn a_fresh_view_draws_each_forkee_editable (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  for (forkee, shown, hidden) in [ ("E", vec! ["e1", "e2"], "H"),
                                   ("F", vec! ["f1"], "G") ] {
    let line : &str = line_of (&view, forkee);
    assert! ( line . starts_with ("*** ")
              && ! line . contains ("writeProtected")
              && ! line . contains ("noClaim"),
              "{} is drawn editable, claiming, in N's forks folder:\n{}", forkee, view );
    let children : Vec<&str> = children_of (&view, forkee);
    assert! ( children . first () . is_some_and (
                |line| line . contains ("hiddenInForkPartnerFolder") ),
              "{}'s folder of what N hides in it comes first:\n{}", forkee, view );
    let content : Vec<&str> = children . iter () . copied ()
      . filter ( |line| line . contains ("(skg node ") ) . collect ();
    assert_eq! ( content . len (), shown . len (), "{}:\n{}", forkee, view );
    for (line, gnid) in content . iter () . zip (shown) {
      assert! ( line . contains (&format! ("(gnid {})", gnid))
                && ! line . contains ("writeProtected"),
                "{} shows {}, editable:\n{}", forkee, gnid, view ); }
    assert! ( under (&view, forkee) . iter () . any ( |line|
                line . contains (&format! ("(gnid {})", hidden))
                && line . contains ("folded") ),
              "{} holds {} in its folder of what N hides:\n{}", forkee, hidden, view ); }
  Ok (( )) }

/// The fresh view, saved unchanged, writes no file.
async fn an_unchanged_save_writes_nothing (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  let before : BTreeMap<String, String> = skg_files (config) ?;
  let saved : String = save_cleanly (&view, "N", config, tantivy) . await ?;
  assert_eq! ( skg_files (config) ?, before, "an unchanged save writes nothing" );
  assert_eq! ( saved, view, "and draws the view again as it was" );
  Ok (( )) }

/// Cutting e2 from E, N's forkee as such, hides e2 in N.
async fn cutting_from_an_owned_forkee_hides (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  save_cleanly (&without_subtree_of (&view, "e2"), "N", config, tantivy) . await ?;
  assert_eq! ( members_of ( node (config, "N") ? . hides . or_default () ),
               ids (&["H", "G", "e2"]) );
  assert_eq! ( members_of (& node (config, "E") ? . contains),
               ids (&["e1", "H", "e2"]), "E's own list is as it was" );
  Ok (( )) }

/// E's title, edited in N's forks folder, is saved, and nothing else
/// of E changes.
async fn an_owned_forkees_title_is_saved (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  let e_line : &str = line_of (&view, "E");
  let edited : String = view . replacen (
    e_line, &e_line . replace (")) E", ")) E, retitled"), 1 );
  assert_ne! ( edited, view );
  let saved : String = save_cleanly (&edited, "N", config, tantivy) . await ?;
  let e : Graphnode = node (config, "E") ?;
  assert_eq! ( e . title, "E, retitled" );
  assert_eq! ( members_of (&e . contains), ids (&["e1", "H", "e2"]) );
  assert! ( line_of (&saved, "E") . ends_with (" E, retitled"), "{}", saved );
  Ok (( )) }

/// F's title, edited in N's forks folder, is refused: F is foreign.
async fn a_foreign_forkees_title_is_refused (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  let f_line : &str = line_of (&view, "F");
  let edited : String = view . replacen (
    f_line, &f_line . replace (")) F", ")) F, retitled"), 1 );
  let before : BTreeMap<String, String> = skg_files (config) ?;
  let refusal : String = refusal_of (&edited, "N", config, tantivy) . await ?;
  assert! ( refusal . contains ("Cannot edit title/body")
            && refusal . contains ("foreign"),
            "{}", refusal );
  assert_eq! ( skg_files (config) ?, before, "nothing is written" );
  Ok (( )) }

/// A headline typed under E, after e1, joins E's content right after
/// e1 (before H, which N hides), and the saved view shows it there.
async fn a_child_typed_under_an_owned_forkee_joins_its_content (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  let stars : String = "*" . repeat ( depth (line_of (&view, "e1")) );
  let typed : String = with_line_after_subtree_of (
    &view, "e1", &format! ("{} typed under E", stars) );
  let saved : String = save_cleanly (&typed, "N", config, tantivy) . await ?;
  let e : Graphnode = node (config, "E") ?;
  let contains : Vec<ID> = members_of (&e . contains);
  assert_eq! ( contains . len (), 4, "{:?}", contains );
  assert_eq! ( ( &contains [0], &contains [2], &contains [3] ),
               ( &ID::from ("e1"), &ID::from ("H"), &ID::from ("e2") ),
               "the typed child follows e1: {:?}", contains );
  let typed_pid : &ID = &contains [1];
  assert_eq! ( node (config, &typed_pid . 0) ? . title, "typed under E" );
  let shown : Vec<&str> = children_of (&saved, "E") . into_iter ()
    . filter ( |line| line . contains ("(skg node ") ) . collect ();
  assert! ( shown . len () == 3
            && shown [1] . contains (&format! ("(gnid {})", typed_pid . 0)),
            "the saved view shows it under E, after e1:\n{}", saved );
  assert_eq! ( members_of (& node (config, "N") ? . contains), ids (&["n1"]),
               "N's own content is as it was" );
  Ok (( )) }

/// A headline typed under F is refused, saying to make F one's own
/// first; nothing is written.
async fn a_child_typed_under_a_foreign_forkee_is_refused (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  let stars : String = "*" . repeat ( depth (line_of (&view, "f1")) );
  let typed : String = with_line_after_subtree_of (
    &view, "f1", &format! ("{} typed under F", stars) );
  let before : BTreeMap<String, String> = skg_files (config) ?;
  let refusal : String = refusal_of (&typed, "N", config, tantivy) . await ?;
  assert! ( refusal . contains ("Cannot add a child under F")
            && refusal . contains ("make the node your own first"),
            "{}", refusal );
  assert_eq! ( skg_files (config) ?, before, "nothing is written" );
  Ok (( )) }

/// n1, cut from N's content and pasted under E after e1, joins E's
/// content right after e1, as a headline typed there does
/// ('docs/forks.org', "Adding a child under a member as such"); N no
/// longer contains it, and hides nothing new (no fork partner
/// contained it). The saved view shows it under E.
async fn a_node_moved_under_an_owned_forkee_joins_its_content (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  let moved : String = moved_after (&view, "n1", "e1");
  let saved : String = save_cleanly (&moved, "N", config, tantivy) . await ?;
  assert_eq! ( members_of (& node (config, "E") ? . contains),
               ids (&["e1", "n1", "H", "e2"]),
               "n1 joins E's content after e1" );
  let n : Graphnode = node (config, "N") ?;
  assert! ( members_of (&n . contains) . is_empty (), "n1 left N: {:?}", n . contains );
  assert_eq! ( members_of (n . hides . or_default ()), ids (&["H", "G"]),
               "N hides nothing new" );
  let shown : Vec<&str> = children_of (&saved, "E") . into_iter ()
    . filter ( |line| line . contains ("(skg node ") ) . collect ();
  assert! ( shown . len () == 3 && shown [1] . contains ("(gnid n1)"),
            "the saved view shows n1 under E, after e1:\n{}", saved );
  Ok (( )) }

/// n1, moved under F, foreign, is refused as a headline typed there
/// is; nothing is written.
async fn a_node_moved_under_a_foreign_forkee_is_refused (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  let moved : String = moved_after (&view, "n1", "f1");
  let before : BTreeMap<String, String> = skg_files (config) ?;
  let refusal : String = refusal_of (&moved, "N", config, tantivy) . await ?;
  assert! ( refusal . contains ("Cannot add a child under F")
            && refusal . contains ("make the node your own first"),
            "{}", refusal );
  assert_eq! ( skg_files (config) ?, before, "nothing is written" );
  Ok (( )) }

/// With a node budget of 1, N spends it: E and F are drawn
/// write-protected, showing nothing.
async fn a_spent_budget_leaves_forkees_write_protected (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let mut config : SkgConfig = config . clone ();
  config . initial_node_limit = 1;
  let view : String = view_of (&config, tantivy, "N") ?;
  for forkee in ["E", "F"] {
    assert! ( line_of (&view, forkee) . contains ("writeProtected")
              && under (&view, forkee) . is_empty (),
              "{} is write-protected and bare:\n{}", forkee, view ); }
  Ok (( )) }

/// A buffer that draws E write-protected and bare, as a server drew it
/// before forkees as such were drawn editable, rerenders as it is: a
/// plain rerender extends nothing standing. An editable-view request
/// on N (C-c s e) extends E, through N's forks folder.
async fn a_write_protected_forkee_stays_until_its_forker_is_extended (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  let e_line : &str = line_of (&view, "E");
  let old : String = without_subtree_of (&view, "E")
    . replacen (
      &format! ( "{}\n", line_of (&view, "F") ),
      &format! ( "{}\n{}\n",
                 e_line . replace ( "(gnid E) (skgRepos (nodeRepo main))",
                                    "(gnid E) (skgRepos (nodeRepo main)) writeProtected" ),
                 line_of (&view, "F") ),
      1 );
  let before : BTreeMap<String, String> = skg_files (config) ?;
  let rerendered : String = save_cleanly (&old, "N", config, tantivy) . await ?;
  assert_eq! ( skg_files (config) ?, before, "the old buffer writes nothing" );
  assert! ( line_of (&rerendered, "E") . contains ("writeProtected")
            && under (&rerendered, "E") . is_empty (),
            "E stays write-protected and bare:\n{}", rerendered );
  let requested : String = rerendered . replacen (
    "(skg node (gnid N)", "(skg node (viewRequests editableView) (gnid N)", 1 );
  assert_ne! ( requested, rerendered );
  let extended : String = save_cleanly (&requested, "N", config, tantivy) . await ?;
  assert! ( ! line_of (&extended, "E") . contains ("writeProtected")
            && under (&extended, "E") . iter () . any ( |line| line . contains ("(gnid e1)") ),
            "the request on N extends E:\n{}", extended );
  Ok (( )) }

/// FN, foreign, forks E. In a view of FN, cutting e1 from E would make
/// FN hide e1, which only FN's owner can record: the save is refused,
/// naming FN.
async fn a_cut_under_a_foreign_forker_is_refused (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "FN") ?;
  assert! ( ! line_of (&view, "E") . contains ("writeProtected"),
            "E is drawn editable under FN, foreign:\n{}", view );
  let before : BTreeMap<String, String> = skg_files (config) ?;
  let refusal : String =
    refusal_of (&without_subtree_of (&view, "e1"), "FN", config, tantivy) . await ?;
  assert! ( refusal . contains ("FN") && refusal . contains ("is foreign"),
            "{}", refusal );
  assert_eq! ( skg_files (config) ?, before, "nothing is written" );
  Ok (( )) }
