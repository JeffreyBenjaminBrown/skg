// The PCSVF's sets on the 'sets' fixture: a rootlike cycle (cy1 and
// cy2 contain each other; cy2 is retitled), a multiply-contained
// node (mc, in ma and mb; retitled), a deleted subtree (r no longer
// contains del, which is deleted with its content delc), and a
// in-changed-only change (ln now contains lo, which lp contains too).

use crate::git_fixture::graph_straddle;
use crate::pcsvf::common::{ids, with_fixture, with_fixture_and_foreign};

use skg::diff_view::change_leaves::{ChangeLeaves, LeafKey, RelationshipAspect};
use skg::graph_straddle::types::GraphStraddle;
use skg::types::misc::ID;
use skg::diff_view::pcsvf::{PcsvfSets, pcsvf_sets_of_buffer};
use skg::types::views_state::DiffViewBufferKind;

use std::collections::BTreeSet;
use std::error::Error;

fn sets_fixture_sets (
) -> Result<PcsvfSets, Box<dyn Error>> {
  let mut sets : Option<PcsvfSets> = None;
  with_fixture ("sets", &["main"], |f| {
    let straddle : GraphStraddle = graph_straddle (f) ?;
    sets = Some ( pcsvf_sets_of_buffer (
      &straddle, DiffViewBufferKind::AllChanges ) . clone () );
    Ok (( )) }) ?;
  Ok ( sets . unwrap () ) }

#[test]
fn c_holds_the_out_changed_and_the_in_changed_halves (
) -> Result<(), Box<dyn Error>> {
  let sets : PcsvfSets = sets_fixture_sets () ?;
  assert_eq! ( sets . c,
               ids (&[ "cy2", "del", "delc", "ln", "lo", "mc", "r" ]) );
  Ok (( )) }

#[test]
fn both_members_of_a_rootlike_cycle_are_origins_and_one_is_kept (
) -> Result<(), Box<dyn Error>> {
  let sets : PcsvfSets = sets_fixture_sets () ?;
  assert! ( sets . r . is_superset (&ids (&[ "cy1", "cy2" ])) );
  assert! ( sets . u . is_superset (&ids (&[ "cy1", "cy2" ])) );
  // cy1 reaches cy2, still kept, so cy1 goes; cy2 then reaches only
  // the discarded cy1.
  assert! ( sets . d . contains (&"cy2" . into ()) );
  assert! ( ! sets . d . contains (&"cy1" . into ()) );
  Ok (( )) }

#[test]
fn a_changed_multiply_contained_node_is_its_own_root (
) -> Result<(), Box<dyn Error>> {
  let sets : PcsvfSets = sets_fixture_sets () ?;
  assert! ( sets . r . contains (&"mc" . into ()) );
  assert! ( sets . d . contains (&"mc" . into ()) );
  for container in [ "ma", "mb" ] {
    assert! ( ! sets . u . contains (&container . into ()),
              "the walk stops at mc, so {} is not unfolded", container ); }
  Ok (( )) }

#[test]
fn a_deleted_subtree_is_reached_through_its_former_containers (
) -> Result<(), Box<dyn Error>> {
  let sets : PcsvfSets = sets_fixture_sets () ?;
  assert! ( sets . u . is_superset (&ids (&[ "r", "del", "delc" ])) );
  assert! ( sets . d . contains (&"r" . into ()) );
  assert! ( ! sets . u . contains (&"keep" . into ()) );
  Ok (( )) }

#[test]
fn an_in_changed_only_change_is_drawn_under_its_new_container (
) -> Result<(), Box<dyn Error>> {
  let sets : PcsvfSets = sets_fixture_sets () ?;
  // lo has two containers in the union, so it is an origin; ln, a
  // kept origin, reaches it within U.
  assert! ( sets . r . is_superset (&ids (&[ "ln", "lo" ])) );
  assert! ( ! sets . d . contains (&"lo" . into ()) );
  assert! ( ! sets . u . contains (&"lp" . into ()) );
  assert_eq! ( sets . d, ids (&[ "cy2", "ln", "mc", "r" ]) );
  Ok (( )) }

/// On every fixture of 'tests/pcsvf/': C is the recorders of the change
/// leaves, and the members of the relationship leaves whose
/// relationship exists in one graph only (each a node of some graph).
/// No fixture changes a mention, the in-half's other source.
#[test]
fn c_is_the_recorders_and_the_members_of_presence_changed_relationship_leaves (
) -> Result<(), Box<dyn Error>> {
  let fixtures : [(&str, &[&str], &[&str]); 10] = [
    ("buffers",            &["public", "private"], &[]),
    ("folder_member",      &["main"],              &[]),
    ("foreign",            &["main"],              &["foreign"]),
    ("fresh",              &["main"],              &[]),
    ("historical_content", &["main"],              &[]),
    ("kinds",              &["public", "private"], &[]),
    ("overrides",          &["main"],              &[]),
    ("partner_ids",        &["main"],              &[]),
    ("sets",               &["main"],              &[]),
    ("shared",             &["main"],              &[]), ];
  for (case, skgrepos, foreign) in fixtures {
    with_fixture_and_foreign (case, skgrepos, foreign, |f| {
      let straddle : GraphStraddle = graph_straddle (f) ?;
      let leaves : &ChangeLeaves = straddle . change_leaves ();
      let recorders : BTreeSet<ID> =
        leaves . recorders () . cloned () . collect ();
      let members : BTreeSet<ID> =
        leaves . recorders ()
        . flat_map ( |recorder| leaves . leaves_under (recorder) )
        . filter_map ( |leaf| match &leaf . key {
          LeafKey::Relationship {
            relationship, aspect : RelationshipAspect::Existence } =>
            Some ( relationship . member . clone () ),
          _ => None, } )
        . filter ( |member| straddle . node_presence (member) . is_some () )
        . collect ();
      assert! ( leaves . mentioner_sets_changed () . is_empty (), "{}", case );
      assert! ( ! recorders . is_empty (), "{}", case );
      assert_eq! ( straddle . changed_pids () . out_changed, recorders,
                   "{}", case );
      assert_eq! ( straddle . changed_pids () . in_changed, members,
                   "{}", case );
      Ok (( )) }) ?; }
  Ok (( )) }
