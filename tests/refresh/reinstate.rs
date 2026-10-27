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
  let run : Run = run_asserting_fixpoint (
    &format! ("* {} p\n", occurrence_metadata ("p", "")), &two_levels () );
  assert_eq! ( run . outline (), "\
node p
  node a
    node a1
  node b
    node b1" );
  assert_eq! ( run . count (BufferCorrectionKind::Reinstated), 2,
               "p's list, already drawn, lacked a and b; what a and b \
                hold is drawn as they are first extended, not corrected" ); }

#[test]
fn a_write_protected_occurrences_content_is_not_drawn () {
  let input : String = formatdoc! ("
    * {} p
    ** {} a
    ", occurrence_metadata ("p", ""), occurrence_metadata ("a", "writeProtected") );
  assert_eq! ( run_asserting_fixpoint (&input, &two_levels ()) . outline (), "\
node p
  node a wp
  node b
    node b1" ); }

#[test]
fn once_the_budget_is_spent_what_is_drawn_is_write_protected () {
  let run : Run = run_asserting_fixpoint_in (
    &format! ("* {} p\n", occurrence_metadata ("p", "")), &two_levels (),
    None, RefreshMode::ContentView, 2 );
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
  assert_eq! ( run_asserting_fixpoint (
    &format! ("* {} p\n", occurrence_metadata ("p", "")), &graph ) . outline (), "\
node p
  node a
  node c
    node a wp" ); }

#[test]
fn a_member_its_parent_lists_and_no_node_answers_to_is_drawn_as_an_unknown () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["x"]), .. node ("p") } ]);
  assert_eq! ( run_asserting_fixpoint (
    &format! ("* {} p\n", occurrence_metadata ("p", "")), &graph ) . outline (), "\
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
    ", occurrence_metadata ("p", "") );
  let run : Run = run_asserting_fixpoint (&input, &graph);
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
    ", occurrence_metadata ("p", "") );
  assert_eq! ( run_asserting_fixpoint (&input, &graph) . outline (), "\
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
    ", occurrence_metadata ("s", ""), occurrence_metadata ("e", "") );
  assert_eq! ( run_asserting_fixpoint (&input, &graph) . outline (), "\
node s
  subscribeeFolder
    node e
      hiddenInSubscribeeFolder
        node h wp noClaim folded
          node e wp noClaim
      node k
  node i",
    "an editable subscribee as such also shows the folder of what its \
     subscriber hides in it" ); }

#[test]
fn a_folder_omits_the_members_whose_records_are_restricted () {
  // 'secret' and 'hush' are private. secret records its containment
  // of p in 'main', which the restriction shows, so it stands as a
  // Restricted vognode; hush records it in 'private', so nothing
  // stands for it.
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["p"]), .. node ("q") },
    Graphnode { contains : ids (&["p"]),
                home_skgrepo : SkgrepoName::from ("private"), .. node ("secret") },
    Graphnode { contains : rel_partners_at_relRepo (
                  &SkgrepoName::from ("private"), vec! [ ID::from ("p") ] ),
                home_skgrepo : SkgrepoName::from ("private"), .. node ("hush") },
    node ("p"), ]);
  let input : String = formatdoc! ("
    * {} p
    ** (skg containerFolder)
    ", occurrence_metadata ("p", "") );
  let restriction : SkgrepoRestriction = main_only ();
  let run : Run = run_asserting_fixpoint_in ( &input, &graph, Some (&restriction),
                           RefreshMode::ContentView, 100 );
  assert_eq! ( run . outline (), "\
node p
  containerFolder
    node q wp noClaim
    restricted secret noClaim" );
  assert! ( ! run . text . contains ("hush"), "{}", run . text ); }

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
    ", occurrence_metadata ("s", ""), occurrence_metadata ("e", "") );
  assert_eq! ( run_asserting_fixpoint (&input, &graph) . outline (), "\
node s
  subscribeeFolder
    node e
      hiddenInSubscribeeFolder
        node h wp noClaim
          node e wp noClaim
      node k",
    "h, a folder member, carries its chain" ); }

#[test]
fn a_buffer_of_many_kinds_is_a_fixpoint () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains      : ids (&["c", "n", "x", "s"]),
                aliases       : MSV::Specified ( rel_partners_at_relRepo (
                  &SkgrepoName::from ("main"), vec! [ "alias" . to_string () ] )),
                .. node ("p") },
    Graphnode { contains : ids (&["c1", "c2"]), .. node ("c") },
    node ("c1"), node ("c2"), node ("n"),
    Graphnode { overrides : msv_ids (&["n"]), .. node ("o") },
    Graphnode { subscribesTo  : msv_ids (&["e"]),
                hidesFromSubs : msv_ids (&["h", "away"]), .. node ("s") },
    Graphnode { contains : ids (&["h", "k", "c"]), .. node ("e") },
    node ("h"), node ("k"), node ("away"),
    Graphnode { contains : ids (&["p"]), .. node ("q") },
    Graphnode { contains : ids (&["q"]), .. node ("top") }, ]);
  let input : String = formatdoc! ("
    * {} p
    ** (skg aliasFolder)
    *** (skg alias) gone
    ** {} c
    *** {} note
    ** {} s
    *** (skg subscribeeFolder)
    * {} c1
    ", occurrence_metadata ("p", "(viewRequests (folder containers) flags (folder overrides))"),
       occurrence_metadata ("c", "writeProtected"),
       occurrence_metadata ("q", "writeProtected"),
       occurrence_metadata ("s", "(viewRequests (folder subscribesTo))"),
       occurrence_metadata ("c1", "(viewRequests writeProtectedSubtreeView)") );
  let run : Run = run_asserting_fixpoint_in ( &input, &graph, None, RefreshMode::ContentView, 3 );
  assert_eq! ( run . outline (), "\
node p
  containerFolder
    node q wp noClaim
      node top wp noClaim
  overriddenFolder
  flagsFolder
  aliasFolder
    alias gone noClaim ⊥
    alias alias
  node c wp
    node q wp noClaim ⊥
  node o (overridesHere n)
  unknown x
  node s
    subscribeeFolder
      node e wp
      hiddenOutsideOfSubscribeeFolder
        node away wp folded
node c1 wp" ); }
