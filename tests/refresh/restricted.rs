// The skgrepo restriction ('docs/data-model_technical.org', "Under a
// skgrepo restriction"). A record is a relationship as one node's file
// states it, in one skgrepo's section. A goal omits a member whose
// record the restriction keeps out, and keeps one whose record is
// visible: an unresolved ID as an unknown, as under no restriction.

use super::*;
use indoc::formatdoc;

/// 'pid' in 'main', listing 'visible' in its 'main' section and
/// 'hidden' in its 'private' one.
fn recorder (
  pid     : &str,
  visible : &[&str],
  hidden  : &[&str],
) -> Graphnode {
  let at = |skgrepo : &str, members : &[&str]| -> Vec<RelPartner<ID>> {
    rel_partners_at_relRepo (
      &SkgrepoName::from (skgrepo),
      members . iter () . map ( |member| ID::from (*member) ) . collect () ) };
  Graphnode { contains : [ at ("main", visible), at ("private", hidden) ] . concat (),
              .. node (pid) } }

fn refreshed_under_main_only (
  input : &str,
  graph : &InRustGraph,
) -> Run {
  let restriction : SkgrepoRestriction = main_only ();
  run_asserting_fixpoint_in (
    input, graph, Some (&restriction), RefreshMode::ContentView, 100 ) }

#[test]
fn an_unknown_whose_record_is_visible_is_drawn_under_a_restriction () {
  let graph : InRustGraph = graph (&[
    recorder ("p", &["c", "missing"], &["hidden-missing"]), node ("c") ]);
  let input : String = formatdoc! ("
    * {} p
    ** (skg unknown (gnid missing))
    ", occurrence_metadata ("p", "") );
  let run : Run = refreshed_under_main_only (&input, &graph);
  assert_eq! ( run . outline (), "\
node p
  node c
  unknown missing",
    "the unknown stays, as under no restriction; the one recorded in \
     'private' is not drawn" );
  assert! ( ! run . text . contains ("hidden-missing"), "{}", run . text ); }

#[test]
fn an_unknown_whose_record_is_visible_is_reinstated_under_a_restriction () {
  let graph : InRustGraph = graph (&[
    recorder ("p", &["missing"], &["hidden-missing"]) ]);
  let input : String = formatdoc! ("
    * {} p
    ", occurrence_metadata ("p", "") );
  assert_eq! ( refreshed_under_main_only (&input, &graph) . outline (), "\
node p
  unknown missing" ); }
