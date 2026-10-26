// The tail: after the walk, every childless tombstone is pruned,
// roots included, so a buffer may end with no roots; a tombstone that
// holds something stays; an editable folder the skgrepo restriction
// emptied is pruned, unless this refresh drew it on request; every
// viewnode gets a vnid.

use super::*;
use indoc::formatdoc;

#[test]
fn a_root_whose_node_vanished_is_pruned_and_the_buffer_may_end_empty () {
  let graph : InRustGraph = graph (&[]);
  let run : Run = run ( &format! ("* {} gone\n", occ ("gone", "")), &graph );
  assert_eq! ( run . outline (), "" );
  assert_eq! ( run . text, "" );
  assert_eq! ( run . correction_kinds (), vec! [ CorrectionKind::Entombstoned ] ); }

#[test]
fn a_tombstone_root_that_holds_something_stays () {
  let graph : InRustGraph = graph (&[ node ("k") ]);
  let input : String = formatdoc! ("
    * {} gone
    ** {} k
    * (skg tombstone)
    ", occ ("gone", ""), occ ("k", "") );
  assert_eq! ( run (&input, &graph) . outline (), "\
tombstone
  node k noClaim" ); }

#[test]
fn every_viewnode_gets_a_vnid () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["c"]), .. node ("p") }, node ("c") ]);
  let run : Run = run ( &format! ("* {} p\n", occ ("p", "(viewRequests (folder aliases))")),
                        &graph );
  assert! ( run . forest . nodes () . all ( |node| node . value () . viewnode_id . is_some () ),
            "{}", run . text );
  assert! ( run . corrections . iter () . all ( |c| c . vnid . is_some () ),
            "{:?}", run . corrections ); }

#[test]
fn a_folder_the_prune_leaves_childless_goes_too () {
  // The containers folder's one member is gone: at the folder's visit
  // it is a tombstone, so the folder is not yet empty; the prune
  // removes the tombstone and then the folder.
  let graph : InRustGraph = graph (&[ node ("p") ]);
  let input : String = formatdoc! ("
    * {} p
    ** (skg containerFolder)
    *** {} gone
    ", occ ("p", ""), occ ("gone", "writeProtected") );
  assert_eq! ( run (&input, &graph) . outline (), "node p" ); }

/// p (in main) subscribes to s, in private, which the restriction to
/// main keeps out.
fn p_subscribing_to_a_private_node () -> InRustGraph {
  graph (&[
    Graphnode { subscribesTo : msv_ids (&["s"]), .. node ("p") },
    Graphnode { home_skgrepo : SkgrepoName::from ("private"), .. node ("s") } ]) }

#[test]
fn an_editable_folder_empty_under_no_restriction_stays () {
  let graph : InRustGraph = graph (&[ node ("p") ]);
  let input : String = formatdoc! ("
    * {} p
    ** (skg subscribeeFolder)
    ", occ ("p", "") );
  let restriction : SkgrepoRestriction = main_only ();
  for restriction in [ None, Some (&restriction) ] {
    let run : Run = run_in ( &input, &graph, restriction,
                             CompletionMode::ContentView, 100 );
    assert_eq! ( run . outline (), "\
node p
  subscribeeFolder",
      "the user can add to it, under {:?}", restriction ); }}

#[test]
fn an_editable_folder_the_restriction_emptied_goes () {
  let input : String = formatdoc! ("
    * {} p
    ** (skg subscribeeFolder)
    *** (skg node (gnid s) (skgRepos (nodeRepo private)) writeProtected) s
    ", occ ("p", "") );
  let restriction : SkgrepoRestriction = main_only ();
  let run : Run = run_in ( &input, &p_subscribing_to_a_private_node (),
                           Some (&restriction), CompletionMode::ContentView, 100 );
  assert_eq! ( run . outline (), "node p",
               "s's tombstone, childless, goes, and so does its folder" ); }

#[test]
fn an_emptied_folder_drawn_on_request_shows_once () {
  let graph : InRustGraph = p_subscribing_to_a_private_node ();
  let config : SkgConfig = config ();
  let restriction : SkgrepoRestriction = main_only ();
  let truth : Truth = Truth { graph : &graph, config : &config,
                              restriction : Some (&restriction) };
  let refresh = |text : &str| -> (ViewForest, String) {
    let mut forest : ViewForest = parse (text, IdResolution::ThroughGraph (&graph));
    let refreshed : Refreshed = refresh_viewforest (
      &mut forest, &mut ViewnodeIdRecord::default (), &truth,
      CompletionMode::ContentView, 100, None,
      &BufferCorrectionPolicy::default () ) . unwrap ();
    ( forest, refreshed . text ) };
  let (requested, text) : (ViewForest, String) = refresh ( &formatdoc! ("
    * {} p
    ", occ ("p", "(viewRequests (folder subscribesTo))") ));
  assert_eq! ( outline (&requested), "\
node p
  subscribeeFolder",
    "the folder the user asked for shows, empty" );
  let (again, _) : (ViewForest, String) = refresh (&text);
  assert_eq! ( outline (&again), "node p",
               "the next refresh prunes it" ); }
