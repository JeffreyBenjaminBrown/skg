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

/// p, in 'main', lists h and c in its 'main' section and hh in its
/// 'private' one; h and hh are private, h contains k.
fn p_h_c () -> InRustGraph {
  graph (&[
    Graphnode { contains : [ ids (&["h", "c"]),
                             rel_partners_at_relRepo (
                               &SkgrepoName::from ("private"),
                               vec! [ ID::from ("hh") ] ) ] . concat (),
                .. node ("p") },
    Graphnode { title        : "h's secret title" . to_string (),
                home_skgrepo : SkgrepoName::from ("private"),
                contains     : ids (&["k"]), .. node ("h") },
    Graphnode { home_skgrepo : SkgrepoName::from ("private"), .. node ("hh") },
    node ("c"), node ("k") ]) }

#[test]
fn a_restricted_member_whose_record_is_visible_is_drawn_as_a_restricted_vognode () {
  let input : String = formatdoc! ("
    * {} p
    ", occurrence_metadata ("p", "") );
  let run : Run = refreshed_under_main_only (&input, &p_h_c ());
  assert_eq! ( run . outline (), "\
node p
  restricted h
  node c",
    "h, recorded in 'main', stands as a Restricted vognode, showing \
     nothing of h; hh, recorded in 'private', is omitted" );
  assert! ( run . text . contains ("restricted (gnid h)"), "{}", run . text );
  for secret in [ "secret title", "(gnid hh)", "(gnid k)" ] {
    assert! ( ! run . text . contains (secret), "{}: {}", secret, run . text ); }}

#[test]
fn an_occurrence_whose_node_the_restriction_newly_keeps_out_becomes_restricted_in_place () {
  let input : String = formatdoc! ("
    * {} p
    ** (skg (vnid zz) folded node (gnid h) (skgRepos (nodeRepo private))) h's secret title
    *** {} k
    ** {} c
    ", occurrence_metadata ("p", ""), occurrence_metadata ("k", ""),
       occurrence_metadata ("c", "") );
  let run : Run = refreshed_under_main_only (&input, &p_h_c ());
  assert_eq! ( run . outline (), "\
node p
  restricted h folded
    node k noClaim
  node c",
    "it keeps its fold state and its children" );
  let h : NodeRef<Viewnode> = run . forest . roots () . next () . unwrap ()
    . children () . next () . unwrap ();
  assert_eq! ( h . value () . viewnode_id, Some ( ViewnodeId ("zz" . into ()) ),
               "it keeps its vnid" );
  assert_eq! ( run . buffer_correction_kinds (),
               vec! [ BufferCorrectionKind::Rekinded (VognodeKind::GnidOnly (GnidOnlyKind::Restricted)) ] );
  assert! ( ! run . text . contains ("secret title"), "{}", run . text ); }

#[test]
fn a_restricted_vognode_becomes_an_occurrence_again_when_the_restriction_widens () {
  let input : String = formatdoc! ("
    * {} p
    ** (skg (vnid zz) folded restricted (gnid h))
    *** {} k
    ** {} c
    ", occurrence_metadata ("p", ""), occurrence_metadata ("k", ""),
       occurrence_metadata ("c", "") );
  let run : Run = run_asserting_fixpoint (&input, &p_h_c ());
  assert_eq! ( run . outline (), "\
node p
  node h folded
    node k
  node c
  node hh" );
  let h : NodeRef<Viewnode> = run . forest . roots () . next () . unwrap ()
    . children () . next () . unwrap ();
  assert_eq! ( h . value () . viewnode_id, Some ( ViewnodeId ("zz" . into ()) ),
               "it keeps its vnid" );
  assert_eq! ( run . buffer_correction_kinds (),
               vec! [ BufferCorrectionKind::Rekinded (VognodeKind::Occurrence),
                      BufferCorrectionKind::Reinstated ],
               "hh, recorded in 'private', is drawn too" );
  assert! ( run . text . contains ("h's secret title"), "{}", run . text ); }

#[test]
fn after_a_restriction_change_the_re_kinds_across_it_are_no_buffer_corrections () {
  let config : SkgConfig = config ();
  let graph : InRustGraph = p_h_c ();
  let restriction : SkgrepoRestriction = main_only ();
  let narrowed : Truth = Truth { graph : &graph, config : &config,
                                 restriction : Some (&restriction) };
  let widened : Truth = Truth { graph : &graph, config : &config, restriction : None };
  let refresh_after_a_switch = |text : &str, truth : &Truth| -> Refreshed {
    let mut forest : ViewForest = parse (text, IdResolution::ThroughGraph (&graph));
    let mut record : ViewnodeIdRecord = ViewnodeIdRecord::of_viewforest (&forest);
    refresh_viewforest (
      &mut forest, &mut record, truth, RefreshMode::ContentView, 100,
      &BufferCorrectionPolicy::after_a_switch (true) )
      . unwrap () };
  let shown : String = formatdoc! ("
    * {} p
    ** {} h
    ** {} c
    ", occurrence_metadata ("p", ""),
       "(skg node (gnid h) (skgRepos (nodeRepo private)))",
       occurrence_metadata ("c", "") );
  let narrowed_run : Refreshed = refresh_after_a_switch (&shown, &narrowed);
  assert! ( narrowed_run . text . contains ("restricted (gnid h)"), "{}", narrowed_run . text );
  assert_eq! ( narrowed_run . buffer_corrections, Vec::new () );
  let widened_run : Refreshed = refresh_after_a_switch (&narrowed_run . text, &widened);
  assert! ( widened_run . text . contains ("h's secret title"), "{}", widened_run . text );
  assert_eq! ( widened_run . buffer_corrections, Vec::new () ); }

#[test]
fn the_children_of_a_restricted_vognode_claim_again_when_it_becomes_an_occurrence () {
  // One forest, kept between refreshes as a view is: narrowed, then
  // widened, never printed and parsed in between.
  let config : SkgConfig = config ();
  let graph : InRustGraph = p_h_c ();
  let restriction : SkgrepoRestriction = main_only ();
  let input : String = formatdoc! ("
    * {} p
    ** (skg node (gnid h) (skgRepos (nodeRepo private))) h's secret title
    *** {} k
    ** {} c
    ", occurrence_metadata ("p", ""), occurrence_metadata ("k", ""),
       occurrence_metadata ("c", "") );
  let mut forest : ViewForest = parse (&input, IdResolution::ThroughGraph (&graph));
  let mut record : ViewnodeIdRecord = ViewnodeIdRecord::of_viewforest (&forest);
  for restriction in [ Some (&restriction), None ] {
    refresh_viewforest (
      &mut forest, &mut record,
      &Truth { graph : &graph, config : &config, restriction },
      RefreshMode::ContentView, 100, &BufferCorrectionPolicy::after_a_switch (true) )
      . unwrap (); }
  assert_eq! ( outline (&forest), "\
node p
  node h
    node k
  node c
  node hh",
    "k claims under h again, so that a save keeps it in h's contents" ); }
