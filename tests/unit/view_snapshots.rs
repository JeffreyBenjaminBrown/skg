// Tests for 'server/view_snapshots.rs'. 'enable' is not called here:
// its folder is process-wide, so enabling it would make every other
// test in this binary write snapshots.

use super::*;

#[test]
fn file_stem_keeps_a_content_views_uuid () {
  assert_eq! (
    file_stem ( &ViewforestId::ContentView (
      "cabeb6e4-7c00-4ed3-a000-03959834efd9" . to_string () )),
    "cabeb6e4-7c00-4ed3-a000-03959834efd9" ); }

#[test]
fn file_stem_makes_a_search_query_safe () {
  let stem : String =
    file_stem ( &ViewforestId::SearchView ( "health AND a/b" . to_string () ));
  assert! ( stem . starts_with ("search_health_AND_a_b_"), "{}", stem );
  assert! ( stem . chars () . all ( |c| c . is_ascii_alphanumeric ()
                                        || c == '-' || c == '_' ),
            "{}", stem ); }

#[test]
fn file_stem_is_at_most_120_characters () {
  assert! (
    file_stem ( &ViewforestId::SearchView ( "x" . repeat (500) )) . len ()
    <= 120 ); }

#[test]
fn file_stem_keeps_apart_queries_that_read_alike () {
  let stem = |query : &str| -> String {
    file_stem ( &ViewforestId::SearchView ( query . to_string () )) };
  assert_ne! ( stem ("a b"), stem ("a_b") );
  assert_ne! ( stem ("año"), stem ("aéo") );
  // on a case-insensitive file system
  assert_ne! ( stem ("Health") . to_lowercase (),
               stem ("health") . to_lowercase () );
  assert_ne! ( stem ( &format! ( "{}1", "x" . repeat (200) )),
               stem ( &format! ( "{}2", "x" . repeat (200) )) ); }
