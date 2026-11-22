// cargo nextest run --test grouped_saves -E 'test(merge::merge_compares_ids_by_pid)'
//
// A nodeMerge compares the members of its lists by pid: a list may
// name a note by an extra ID ('acquirer_after_nodeMerge').
//
// Fixture: k has the extra ID m, f has g, p has q, r has s.
// - a contains k and forks f; b contains m and forks g.
// - c contains p; d hides q.
// - h hides s; i forks G, which contains r, so i shows r.

use skg::nodeMerge::nodeMergeInstructionTriple::nodeMerge_instructions_from_pairs;
use skg::test_utils::{run_with_shared_test_stores, graph_handle_from_config};
use skg::types::misc::{ID, members_of};
use skg::types::save::NodeMerge;

use std::error::Error;

const FIXTURES : &str = "tests/merge/merge_compares_ids_by_pid/fixtures";

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-merge-compares-ids-by-pid",
    |s| Box::pin ( async move {
      s . reset ("a_nodeMerge_compares_ids_by_pid", FIXTURES) ?;
      let merges : Vec<NodeMerge> =
        nodeMerge_instructions_from_pairs (
          & [ ( ID::from ("a"), ID::from ("b") ),
              ( ID::from ("c"), ID::from ("d") ),
              ( ID::from ("h"), ID::from ("i") ) ],
          &graph_handle_from_config (&s . config)? . load_full (),
          &s . config ) ?;
      assert_eq! ( merges . len (), 3 );
      a_note_both_members_name_differently_is_recorded_once (&merges [0]);
      the_acquirer_does_not_hide_what_it_contains_by_another_id (&merges [1]);
      the_acquirer_does_not_hide_what_the_acquiree_showed_by_another_id (&merges [2]);
      Ok (( )) } )) }

/// a contains k and b contains m, one note: the merged note contains
/// it once (besides the acquiree's text preserver), as a's list names
/// it. So with the forks of f and g.
fn a_note_both_members_name_differently_is_recorded_once (
  merge : &NodeMerge,
) {
  let merged = &merge . updated_acquirer . 0;
  let preserver : ID = merge . acquiree_text_preserver . 0 . pid . clone ();
  assert_eq! ( members_of (&merged . contains), vec! [ preserver, ID::from ("k") ] );
  assert_eq! ( members_of ( merged . forks . or_default () ), vec! [ ID::from ("f") ] ); }

/// c contains p, which d hides as q: the merged note, containing it,
/// does not hide it.
fn the_acquirer_does_not_hide_what_it_contains_by_another_id (
  merge : &NodeMerge,
) {
  assert_eq! ( members_of ( merge . updated_acquirer . 0 . hides . or_default () ),
               Vec::<ID>::new () ); }

/// i showed r through G before the nodeMerge, and h hides it as s:
/// one member's hide does not silence the other's view, so the merged
/// note does not hide it.
fn the_acquirer_does_not_hide_what_the_acquiree_showed_by_another_id (
  merge : &NodeMerge,
) {
  assert_eq! ( members_of ( merge . updated_acquirer . 0 . hides . or_default () ),
               Vec::<ID>::new () ); }
