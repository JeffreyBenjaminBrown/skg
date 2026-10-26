// Rung 8: requests. A view request on a child is fulfilled and
// dropped: 'editableView' makes it the node's editable occurrence and
// its content is drawn; 'writeProtectedView' and
// 'writeProtectedSubtreeView' write-protect it, alone or with its
// content subtree; 'flags' and '(folder REL)' add the folders, filled
// from their goals; 'fork' is the save's and stays. ('tombstone', on a
// ghost, is the diff view's: 'diff_view.rs'.) An edit request is
// carried through unchanged, unless the child's new kind has no slot
// for it: then it is dropped, and reported.

use super::*;
use crate::types::nodes::complete::Flag;
use indoc::formatdoc;

/// p contains c and d; c contains c1; q contains p, and top contains q.
fn family () -> InRustGraph {
  graph (&[
    Graphnode { contains : ids (&["c", "d"]), .. node ("p") },
    Graphnode { contains : ids (&["c1"]), .. node ("c") },
    node ("c1"), node ("d"),
    Graphnode { contains : ids (&["p"]), .. node ("q") },
    Graphnode { contains : ids (&["q"]), .. node ("top") }, ]) }

#[test]
fn editableView_moves_the_editable_occurrence_and_draws_its_content () {
  let input : String = formatdoc! ("
    * {} c
    * {} p
    ** {} c
    ** {} d
    ", occ ("c", ""), occ ("p", ""),
       occ ("c", "writeProtected (viewRequests editableView)"), occ ("d", "") );
  let run : Run = run (&input, &family ());
  assert_eq! ( run . outline (), "\
node c wp
node p
  node c
    node c1
  node d" ); }

/// A request on an occurrence whose content is shown write-protected
/// extends it: each child that claims membership becomes editable, as
/// a drawn occurrence would, and draws its own content.
#[test]
fn editableView_extends_content_shown_write_protected () {
  let input : String = formatdoc! ("
    * {} p
    ** {} c
    ** {} d
    ", occ ("p", "(viewRequests editableView)"),
       occ ("c", "writeProtected"), occ ("d", "writeProtected") );
  assert_eq! ( run (&input, &family ()) . outline (), "\
node p
  node c
    node c1
  node d" ); }

/// Within the budget: here it allows p's extension and c's, so d,
/// reached after it is spent, stays write-protected.
#[test]
fn editableView_extends_content_shown_write_protected_within_the_budget () {
  let input : String = formatdoc! ("
    * {} p
    ** {} c
    ** {} d
    ", occ ("p", "(viewRequests editableView)"),
       occ ("c", "writeProtected"), occ ("d", "writeProtected") );
  assert_eq! ( run_in ( &input, &family (), None, CompletionMode::ContentView, 2 )
               . outline (), "\
node p
  node c
    node c1 wp
  node d wp" ); }

#[test]
fn writeProtectedView_write_protects_the_occurrence_alone () {
  let input : String = formatdoc! ("
    * {} p
    ** {} c
    *** {} c1
    ** {} d
    ", occ ("p", ""), occ ("c", "(viewRequests writeProtectedView)"),
       occ ("c1", ""), occ ("d", "") );
  assert_eq! ( run (&input, &family ()) . outline (), "\
node p
  node c wp
    node c1 noClaim
  node d" ); }

#[test]
fn writeProtectedSubtreeView_write_protects_the_content_subtree () {
  let input : String = formatdoc! ("
    * {} p
    ** {} c
    *** {} c1
    ** {} d
    ", occ ("p", "(viewRequests writeProtectedSubtreeView)"),
       occ ("c", ""), occ ("c1", ""), occ ("d", "") );
  assert_eq! ( run (&input, &family ()) . outline (), "\
node p wp
  node c wp noClaim
    node c1 wp noClaim
  node d wp noClaim" ); }

#[test]
fn a_containers_request_adds_the_folder_with_each_members_chain () {
  let input : String = format! ( "* {} p\n",
    occ ("p", "(viewRequests (folder containers))") );
  let run : Run = run (&input, &family ());
  assert_eq! ( run . outline (), "\
node p
  containerFolder
    node q wp noClaim
      node top wp noClaim
  node c
    node c1
  node d" );
  assert! ( ! run . text . contains ("viewRequests"), "{}", run . text ); }

#[test]
fn a_subscriptions_request_adds_the_subscribee_folder_and_drops_an_empty_subscriber_folder () {
  let graph : InRustGraph = graph (&[
    Graphnode { subscribesTo  : msv_ids (&["e"]),
                hidesFromSubs : msv_ids (&["o"]), .. node ("s") },
    node ("e"), node ("o"), ]);
  let input : String = format! ( "* {} s\n",
    occ ("s", "(viewRequests (folder subscribesTo))") );
  assert_eq! ( run (&input, &graph) . outline (), "\
node s
  subscribeeFolder
    node e wp
    hiddenOutsideOfSubscribeeFolder
      node o wp folded" ); }

#[test]
fn a_flags_request_adds_the_flags_folder_even_when_no_flag_is_true () {
  let graph : InRustGraph = graph (&[
    Graphnode { flags : vec! [ Flag::NoSearchMatching ], .. node ("p") },
    node ("r"), ]);
  let input : String = formatdoc! ("
    * {} p
    * {} r
    ", occ ("p", "(viewRequests flags)"), occ ("r", "(viewRequests flags)") );
  assert_eq! ( run (&input, &graph) . outline (), "\
node p
  flagsFolder
    flag noSearchMatching
node r
  flagsFolder" ); }

#[test]
fn an_aliases_request_adds_the_alias_folder () {
  let graph : InRustGraph = graph (&[
    Graphnode { aliases : MSV::Specified ( rel_partners_at_relRepo (
                  &SkgrepoName::from ("main"), vec! [ "b" . to_string (), "a" . to_string () ] )),
                .. node ("p") } ]);
  let input : String = format! ( "* {} p\n",
    occ ("p", "(viewRequests (folder aliases))") );
  assert_eq! ( run (&input, &graph) . outline (), "\
node p
  aliasFolder
    alias b
    alias a" ); }

#[test]
fn a_fork_request_stays_for_the_save () {
  let graph : InRustGraph = graph (&[ node ("p") ]);
  let input : String = format! ( "* {} p\n", occ ("p", "(viewRequests fork)") );
  assert! ( run (&input, &graph) . text . contains ("(viewRequests fork)") ); }

#[test]
fn an_edit_request_is_carried_through () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["d"]), .. node ("p") }, node ("d") ]);
  let input : String = formatdoc! ("
    * {} p
    ** {} d
    ", occ ("p", ""), occ ("d", "(editRequest delete)") );
  let run : Run = run (&input, &graph);
  assert! ( run . text . contains ("(editRequest delete)"), "{}", run . text );
  assert! ( run . buffer_corrections . is_empty (), "{:?}", run . buffer_corrections ); }

#[test]
fn an_edit_request_the_new_kind_has_no_slot_for_is_dropped_and_reported () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["x"]), .. node ("p") } ]);
  let input : String = formatdoc! ("
    * {} p
    ** {} x
    ", occ ("p", ""), occ ("x", "(editRequest delete)") );
  let run : Run = run (&input, &graph);
  assert_eq! ( run . outline (), "\
node p
  unknown x" );
  assert! ( ! run . text . contains ("editRequest"), "{}", run . text );
  assert_eq! ( run . buffer_correction_kinds (),
               vec! [ BufferCorrectionKind::EditRequestDropped,
                      BufferCorrectionKind::Rekinded (VognodeKind::Unknown) ] ); }

#[test]
fn a_search_stub_of_write_protected_hits_draws_each_hits_containers () {
  let input : String = formatdoc! ("
    * {} p
    * {} c
    ", occ ("p", "writeProtected (viewRequests (folder containers))"),
       occ ("c", "writeProtected (viewRequests (folder containers))") );
  assert_eq! ( run (&input, &family ()) . outline (), "\
node p wp
  containerFolder
    node q wp noClaim
      node top wp noClaim
node c wp
  containerFolder
    node p wp noClaim
      node q wp noClaim
        node top wp noClaim" ); }

#[test]
fn a_subscriptions_request_completes_a_subscribee_folder_already_there () {
  let graph : InRustGraph = graph (&[
    Graphnode { subscribesTo  : msv_ids (&["e"]),
                hidesFromSubs : msv_ids (&["o"]), .. node ("s") },
    node ("e"), node ("o"), ]);
  let input : String = formatdoc! ("
    * {} s
    ** (skg subscribeeFolder)
    *** {} e
    ", occ ("s", "(viewRequests (folder subscribesTo))"), occ ("e", "writeProtected") );
  assert_eq! ( run (&input, &graph) . outline (), "\
node s
  subscribeeFolder
    node e wp
    hiddenOutsideOfSubscribeeFolder
      node o wp folded" ); }
