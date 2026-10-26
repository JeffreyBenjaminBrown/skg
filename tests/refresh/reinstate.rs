// Rung 5: reinstate. Every goal item without a child is drawn, within
// the extent: all of a folder's goal and of an editable occurrence's
// content (each occurrence whose content is drawn charging the budget
// once; when it is spent, newly drawn occurrences are
// write-protected), none of a write-protected occurrence's content or
// of a chain. A repeat is write-protected. A folder's reinstated
// member carries its containerward chain.

use super::*;
use indoc::formatdoc;

/// p contains a and b; a contains a1; b contains b1.
fn two_levels () -> InRustGraph {
  graph (&[
    Graphnode { contains : ids (&["a", "b"]), .. node ("p") },
    Graphnode { contains : ids (&["a1"]), .. node ("a") },
    Graphnode { contains : ids (&["b1"]), .. node ("b") },
    node ("a1"), node ("b1"), ]) }

#[test]
fn an_editable_occurrences_content_is_drawn_editable () {
  let run : Run = run ( &format! ("* {} p\n", occ ("p", "")), &two_levels () );
  assert_eq! ( run . outline (), "\
node p
  node a
    node a1
  node b
    node b1" );
  assert_eq! ( run . count (CorrectionKind::Reinstated), 4 ); }

#[test]
fn a_write_protected_occurrences_content_is_not_drawn () {
  let input : String = formatdoc! ("
    * {} p
    ** {} a
    ", occ ("p", ""), occ ("a", "writeProtected") );
  assert_eq! ( run (&input, &two_levels ()) . outline (), "\
node p
  node a wp
  node b
    node b1" ); }

#[test]
fn once_the_budget_is_spent_what_is_drawn_is_write_protected () {
  let run : Run = run_in ( &format! ("* {} p\n", occ ("p", "")), &two_levels (),
                           None, CompletionMode::ContentView, 2 );
  assert_eq! ( run . outline (), "\
node p
  node a
    node a1 wp
  node b wp",
    "p and a spend the budget; b, drawn editable while it lasted, is \
     write-protected at its visit and draws nothing" ); }

#[test]
fn a_repeat_is_write_protected () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["a", "c"]), .. node ("p") },
    Graphnode { contains : ids (&["a"]), .. node ("c") },
    node ("a"), ]);
  assert_eq! ( run ( &format! ("* {} p\n", occ ("p", "")), &graph ) . outline (), "\
node p
  node a
  node c
    node a wp" ); }

#[test]
fn a_member_its_parent_lists_and_no_node_answers_to_is_drawn_as_an_unknown () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["x"]), .. node ("p") } ]);
  assert_eq! ( run ( &format! ("* {} p\n", occ ("p", "")), &graph ) . outline (), "\
node p
  unknown x" ); }

#[test]
fn a_folders_reinstated_member_carries_its_chain () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["q"]), .. node ("top") },
    Graphnode { contains : ids (&["p"]), .. node ("q") },
    node ("p"), ]);
  let input : String = formatdoc! ("
    * {} p
    ** (skg containerFolder)
    ", occ ("p", "") );
  let run : Run = run (&input, &graph);
  assert_eq! ( run . outline (), "\
node p
  containerFolder
    node q wp noClaim
      node top wp noClaim" ); }

#[test]
fn an_empty_write_protected_folder_goes_and_an_empty_editable_one_stays () {
  let graph : InRustGraph = graph (&[ node ("p") ]);
  let input : String = formatdoc! ("
    * {} p
    ** (skg containerFolder)
    ** (skg overriddenFolder)
    ", occ ("p", "") );
  assert_eq! ( run (&input, &graph) . outline (), "\
node p
  overriddenFolder" ); }

#[test]
fn a_subscribee_as_such_shows_what_its_subscriber_neither_hides_nor_contains () {
  let graph : InRustGraph = graph (&[
    Graphnode { subscribesTo  : msv_ids (&["e"]),
                hidesFromSubs : msv_ids (&["h"]),
                contains      : ids (&["i"]), .. node ("s") },
    Graphnode { contains : ids (&["h", "i", "k"]), .. node ("e") },
    node ("h"), node ("i"), node ("k"), ]);
  let input : String = formatdoc! ("
    * {} s
    ** (skg subscribeeFolder)
    *** {} e
    ", occ ("s", ""), occ ("e", "") );
  assert_eq! ( run (&input, &graph) . outline (), "\
node s
  subscribeeFolder
    node e
      node k
  node i" ); }

#[test]
fn a_folder_omits_what_the_restriction_hides () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["p"]), .. node ("q") },
    Graphnode { contains : ids (&["p"]),
                home_skgrepo : SkgrepoName::from ("private"), .. node ("secret") },
    node ("p"), ]);
  let input : String = formatdoc! ("
    * {} p
    ** (skg containerFolder)
    ", occ ("p", "") );
  let restriction : SkgrepoRestriction = main_only ();
  let run : Run = run_in ( &input, &graph, Some (&restriction),
                           CompletionMode::ContentView, 100 );
  assert_eq! ( run . outline (), "\
node p
  containerFolder
    node q wp noClaim" );
  assert! ( ! run . text . contains ("secret"), "{}", run . text ); }

#[test]
fn the_hidden_in_subscribee_folder_holds_what_the_subscriber_hides_of_the_subscribees_content () {
  let graph : InRustGraph = graph (&[
    Graphnode { subscribesTo  : msv_ids (&["e"]),
                hidesFromSubs : msv_ids (&["h", "o"]), .. node ("s") },
    Graphnode { contains : ids (&["h", "k"]), .. node ("e") },
    node ("h"), node ("k"), node ("o"), ]);
  let input : String = formatdoc! ("
    * {} s
    ** (skg subscribeeFolder)
    *** {} e
    **** (skg hiddenInSubscribeeFolder)
    ", occ ("s", ""), occ ("e", "") );
  assert_eq! ( run (&input, &graph) . outline (), "\
node s
  subscribeeFolder
    node e
      hiddenInSubscribeeFolder
        node h wp noClaim
          node e wp noClaim
      node k",
    "h, a folder member, carries its chain" ); }
