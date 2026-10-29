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
    refresh_and_render (
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

/// r, in 'private', contains a and b, both in 'main'.
fn private_root () -> InRustGraph {
  graph (&[
    Graphnode { title        : "r's secret title" . to_string (),
                home_skgrepo : SkgrepoName::from ("private"),
                contains     : ids (&["a", "b"]), .. node ("r") },
    node ("a"), node ("b") ]) }

#[test]
fn a_root_the_restriction_newly_keeps_out_becomes_restricted_in_place () {
  let input : String = formatdoc! ("
    * (skg (vnid rr) folded node (gnid r) (skgRepos (nodeRepo private))) r's secret title
    ** {} a
    ** {} b
    ", occurrence_metadata ("a", ""), occurrence_metadata ("b", "") );
  let run : Run = refreshed_under_main_only (&input, &private_root ());
  assert_eq! ( run . outline (), "\
restricted r folded
  node a noClaim
  node b noClaim",
    "the view is the user's own request, so its root stands as a \
     Restricted vognode, keeping its fold state and its children, \
     which claim nothing under it" );
  let r : NodeRef<Viewnode> = run . forest . roots () . next () . unwrap ();
  assert_eq! ( r . value () . viewnode_id, Some ( ViewnodeId ("rr" . into ()) ),
               "it keeps its vnid" );
  assert_eq! ( run . buffer_correction_kinds (),
               vec! [ BufferCorrectionKind::Rekinded (VognodeKind::GnidOnly (GnidOnlyKind::Restricted)) ] );
  assert! ( run . text . contains ("restricted (gnid r)"), "{}", run . text );
  assert! ( ! run . text . contains ("secret"), "{}", run . text ); }

#[test]
fn a_restricted_root_becomes_an_occurrence_again_when_the_restriction_widens () {
  let input : String = formatdoc! ("
    * (skg (vnid rr) folded restricted (gnid r))
    ** {} a
    ** {} b
    ", occurrence_metadata ("a", ""), occurrence_metadata ("b", "") );
  let run : Run = run_asserting_fixpoint (&input, &private_root ());
  assert_eq! ( run . outline (), "\
node r folded
  node a
  node b",
    "an editable occurrence, as the root was, its children claiming" );
  let r : NodeRef<Viewnode> = run . forest . roots () . next () . unwrap ();
  assert_eq! ( r . value () . viewnode_id, Some ( ViewnodeId ("rr" . into ()) ),
               "it keeps its vnid" );
  assert_eq! ( run . buffer_correction_kinds (),
               vec! [ BufferCorrectionKind::Rekinded (VognodeKind::Occurrence) ] );
  assert! ( run . text . contains ("r's secret title"), "{}", run . text ); }

#[test]
fn the_children_of_a_restricted_root_claim_again_when_it_becomes_an_occurrence () {
  // One forest, kept between refreshes as a view is: narrowed, then
  // widened, never printed and parsed in between.
  let config : SkgConfig = config ();
  let graph : InRustGraph = private_root ();
  let restriction : SkgrepoRestriction = main_only ();
  let input : String = formatdoc! ("
    * (skg node (gnid r) (skgRepos (nodeRepo private))) r's secret title
    ** {} a
    ** {} b
    ", occurrence_metadata ("a", ""), occurrence_metadata ("b", "") );
  let mut forest : ViewForest = parse (&input, IdResolution::ThroughGraph (&graph));
  let mut record : ViewnodeIdRecord = ViewnodeIdRecord::of_viewforest (&forest);
  let mut texts : Vec<String> = Vec::new ();
  for restriction in [ Some (&restriction), None ] {
    let refreshed : Refreshed = refresh_and_render (
      &mut forest, &mut record,
      &Truth { graph : &graph, config : &config, restriction },
      RefreshMode::ContentView, 100, &BufferCorrectionPolicy::after_a_switch (true) )
      . unwrap ();
    assert_eq! ( refreshed . buffer_corrections, Vec::new (),
                 "a switch's re-kinds across the restriction are no buffer \
                  corrections" );
    texts . push (refreshed . text); }
  assert! ( texts [0] . contains ("restricted (gnid r)")
            && ! texts [0] . contains ("secret"), "{}", texts [0] );
  assert_eq! ( outline (&forest), "\
node r
  node a
  node b",
    "a and b claim under r again, so that a save keeps them in r's contents" ); }

/// r, in 'main', records the alias "r public alias" in 'main' and
/// "r secret alias" in 'private'.
fn r_with_a_private_alias () -> InRustGraph {
  graph (&[
    Graphnode { aliases : MSV::Specified ( vec! [
                  RelPartner::at_relRepo ( SkgrepoName::from ("main"),
                                           "r public alias" . to_string () ),
                  RelPartner::at_relRepo ( SkgrepoName::from ("private"),
                                           "r secret alias" . to_string () ) ] ),
                .. node ("r") } ]) }

#[test]
fn an_alias_whose_record_is_restricted_is_neither_drawn_nor_counted () {
  let input : String = formatdoc! ("
    * {} r
    ", occurrence_metadata ("r", "(viewRequests (folder aliases))") );
  let narrowed : Run = refreshed_under_main_only (&input, &r_with_a_private_alias ());
  assert_eq! ( narrowed . outline (), "\
node r
  aliasFolder
    alias r public alias" );
  assert! ( narrowed . text . contains ("(props (aliases 1))")
            && ! narrowed . text . contains ("secret"), "{}", narrowed . text );
  let unrestricted : Run = run_asserting_fixpoint (&input, &r_with_a_private_alias ());
  assert_eq! ( unrestricted . outline (), "\
node r
  aliasFolder
    alias r public alias
    alias r secret alias" );
  assert! ( unrestricted . text . contains ("(props (aliases 2))"), "{}", unrestricted . text ); }

#[test]
fn a_switch_takes_from_a_view_the_alias_whose_record_it_makes_restricted () {
  let config : SkgConfig = config ();
  let graph : InRustGraph = r_with_a_private_alias ();
  let restriction : SkgrepoRestriction = main_only ();
  let shown : String = formatdoc! ("
    * {} r
    ** (skg aliasFolder)
    *** (skg alias) r public alias
    *** (skg alias (skgRepos (relRepo private))) r secret alias
    ", occurrence_metadata ("r", "") );
  let mut forest : ViewForest = parse (&shown, IdResolution::ThroughGraph (&graph));
  let mut record : ViewnodeIdRecord = ViewnodeIdRecord::of_viewforest (&forest);
  let narrowed : Refreshed = refresh_and_render (
    &mut forest, &mut record,
    &Truth { graph : &graph, config : &config, restriction : Some (&restriction) },
    RefreshMode::ContentView, 100, &BufferCorrectionPolicy::after_a_switch (true) )
    . unwrap ();
  assert_eq! ( outline (&forest), "\
node r
  aliasFolder
    alias r public alias" );
  assert! ( ! narrowed . text . contains ("secret"), "{}", narrowed . text );
  assert_eq! ( narrowed . buffer_corrections, Vec::new (),
               "what the restriction takes is no buffer correction" ); }
