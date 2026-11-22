// cargo nextest run --test grouped_forks -E 'test(view_stats_sharing::)'
//
// The position-relative sharing view-stats
// ('docs/api-and-formats.org', "Stats metadata: graphStats and
// viewStats"; the tokens in 'docs/heralds.org', "Ancestor flags and
// floors"):
// - a forkee-as-such is born of its folder recorder's fork of it,
//   and shows that fork as its relation to generation 2;
// - forking its viewparent: a node shown under a vognode it forks
//   shows a forks-out relation to generation 1.
// One fixture serves both: R forks E and F (each under R's
// forksFolder, born of the fork); R contains P,
// P contains C, and C forks P (so C as content of P carries that
// relation, and C as a view root does not). Each stat round-trips through a
// save (the viewStats parser errors on unknown atoms, so this also
// pins the parse arms).

use skg::reset_then_run_subtest;
use skg::test_utils::save_and_rerender;
use skg::metadata_canon::without_viewnode_ids;
use std::error::Error;

use skg::test_utils::{run_with_shared_test_stores, graph_handle_from_config};
use skg::test_utils::multi_root_view;
use skg::types::config::SkgConfig;
use skg::types::misc::{ID, TantivyIndex};


#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str = "tests/view_stats_sharing/fixtures";
  run_with_shared_test_stores (
    "skg-test-view-stats-sharing",
    |s| Box::pin ( async move {
      reset_then_run_subtest! (s . reset (fixtures),
        sharing_view_stats_appear_and_roundtrip ( &s . config, &mut s . tantivy ) . await);
      reset_then_run_subtest! (s . reset (fixtures),
        forking_the_viewparent_is_position_relative ( &s . config, &mut s . tantivy ) . await);
      Ok (( )) } )) }

fn lines_containing<'a> (
  buf      : &'a str,
  fragment : &str,
) -> Vec<&'a str> {
  buf . lines ()
    . filter ( |l| l . contains (fragment) )
    . collect () }

/// The folder (by its metadata atom) most recently opened above each
/// E line. The fixture shows E only as a folder member, so this
/// attributes each E copy to its folder.
fn e_lines_by_enclosing_folder (
  buf : &str,
) -> Vec<(&'static str, &str)> {
  let mut current_folder : &'static str = "";
  let mut result : Vec<(&'static str, &str)> = Vec::new ();
  for line in buf . lines () {
    if without_viewnode_ids (line) . contains ("forksFolder)") { current_folder = "forksFolder"; }
    else if line . contains ("(gnid E)") {
      result . push ((current_folder, line)); }}
  result }

fn assert_sharing_stats_in_view_of_R (
  buf   : &str,
  label : &str,
) {
  let e_lines : Vec<(&'static str, &str)> =
    e_lines_by_enclosing_folder (buf);
  assert_eq! ( e_lines . len (), 1,
    "{}: E should appear under the forksFolder:\n{}", label, buf );
  // In the semantic wire the parent-relative stat is the relationship
  // to E's grandparent R (generation 2): R forks E. Under a folder,
  // the folder relation is E's birth, and the relation carries
  // (ancestors 2). F, which R forks too, shows the same.
  for (folder, line) in &e_lines {
    match *folder {
      "forksFolder" => {
        assert! ( line . contains ("(birth (forks in 2))"),
          "{}: E-as-forkee is born of the fork:\n{}", label, buf );
        assert! ( line . contains ("(forks (in 1 (ancestors 2)))"),
          "{}: E-as-forkee shows R's fork of it (gen 2):\n{}",
          label, buf ); },
      other => panic! (
        "{}: E shown outside any folder ({:?}):\n{}", label, other, buf ),
    }}
  for f_line in lines_containing (buf, "(gnid F)") {
    assert! ( f_line . contains ("(birth (forks in 2))")
              && f_line . contains ("(forks (in 1 (ancestors 2)))"),
      "{}: F, like E, is born of R's fork of it:\n{}", label, buf ); }
  { // P and C, which forks it, are in one skgrepo, where P stands
    // as itself ('docs/mirrors.org', "Which mirror stands"), so a
    // fresh view of R shows P, not C in its place, with C as P's
    // content.
    assert! ( ! buf . contains ("inPlaceOf"),
      "{}: nothing is shown in another's place:\n{}", label, buf );
    assert_c_forks_its_viewparent_in_view_of_P (buf, label); }}

fn assert_c_forks_its_viewparent_in_view_of_P (
  buf   : &str,
  label : &str,
) {
  let c_lines : Vec<&str> = lines_containing (buf, "(gnid C)");
  // C forks its viewparent (a = P), so it
  // shows a forks-out relation to generation 1.
  assert! ( c_lines . iter ()
            . any ( |l| l . contains ("(forks (out")
                        && l . contains ("(ancestors 1)") ),
    "{}: C shown as content of P (which it forks) shows a forks-out \
     to P (gen 1):\n{}", label, buf ); }

async fn sharing_view_stats_appear_and_roundtrip (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      (
        graph_handle_from_config (config) ? );
      let (de_novo, _pids, _tree)
        : (String, Vec<ID>, _) =
        multi_root_view (
          config, Some (tantivy),
          &[ ID ("R" . to_string ()) ] ) ?;
      assert_sharing_stats_in_view_of_R (&de_novo, "de novo");
      let saved : String = // The save parses the buffer, so this also pins the parse arms.
        save_and_rerender (&de_novo, config, tantivy) . await ?;
      assert_sharing_stats_in_view_of_R (&saved, "after save");
      { // A view rooted at P shows P as itself, a root standing in
        // its own home skgrepo, with C as its content.
        let (view_of_p, _pids, _tree)
          : (String, Vec<ID>, _) =
          multi_root_view (
            config, Some (tantivy),
            &[ ID ("P" . to_string ()) ] ) ?;
        assert_c_forks_its_viewparent_in_view_of_P (&view_of_p, "de novo, view of P");
        let saved_p : String =
          save_and_rerender (&view_of_p, config, tantivy)
          . await ?;
        assert_c_forks_its_viewparent_in_view_of_P (&saved_p, "after save, view of P"); }
      Ok (( )) }

async fn forking_the_viewparent_is_position_relative (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      (
        graph_handle_from_config (config) ? );
      let (view_of_c, _pids, _tree)
        : (String, Vec<ID>, _) =
        multi_root_view (
          config, Some (tantivy),
          &[ ID ("C" . to_string ()) ] ) ?;
      let c_root_line : &str =
        view_of_c . lines ()
        . find ( |l| l . starts_with ("* ") && l . contains ("(gnid C)") )
        . expect ("view of C should have C as its root");
      assert! ( ! ( c_root_line . contains ("(forks (out")
                && c_root_line . contains ("(ancestors 1)") ),
        "C as a view root has no viewparent to fork:\n{}",
        view_of_c );
      Ok (( )) }
