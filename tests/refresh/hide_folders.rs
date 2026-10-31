// The two hide folders under a skgrepo narrowing. A subscriber s
// subscribes to e and hides h. A hide is inside when a subscribee
// contains the hidden graphnode, and outside otherwise; under e shown
// editable as s's subscribee, the folder 'hiddenInSubscribeeFolder'
// shows the inside hides, and beside e, in s's subscribee folder,
// 'hiddenOutsideOfSubscribeeFolder' shows the outside ones. Both
// folders, and e's content, are read by the records the skgrepo
// restriction shows: s's hide of h, and e's containment of h, each
// recorded in 'main' (visible under every skgrepo restriction here)
// or in 'private' (visible only under no skgrepo restriction). Six
// cases: the hide's record visible or restricted, times e unrelated
// to h, containing h by a visible record, or containing it by a
// restricted one.

use super::*;
use indoc::formatdoc;

/// Where the record of a relationship stands.
#[derive(Clone, Copy)]
enum Record { Main, Private }

impl Record {
  fn skgrepo (
    self,
  ) -> &'static str {
    match self { Record::Main => "main", Record::Private => "private", } }
}

fn at (
  record  : Record,
  members : &[&str],
) -> Vec<RelPartner<ID>> {
  rel_partners_at_relRepo (
    &SkgrepoName::from (record . skgrepo ()),
    members . iter () . map ( |member| ID::from (*member) ) . collect () ) }

/// s subscribes to e (recorded in 'main') and hides h by 'hide'; e
/// contains c (in 'main'), and h by 'containment' when there is one.
fn s_e_h (
  hide        : Record,
  containment : Option<Record>,
) -> InRustGraph {
  let e_contains : Vec<RelPartner<ID>> = match containment {
    None => at (Record::Main, &["c"]),
    Some (record) => [ at (Record::Main, &["c"]), at (record, &["h"]) ] . concat (), };
  graph (&[
    Graphnode { subscribesTo  : MSV::Specified ( at (Record::Main, &["e"]) ),
                hidesFromSubs : MSV::Specified ( at (hide, &["h"]) ),
                .. node ("s") },
    Graphnode { contains : e_contains, .. node ("e") },
    node ("c"), node ("h") ]) }

/// Where h stands in a view of s whose subscribee folder the user
/// requested, e editable in it, refreshed under no skgrepo restriction
/// and under 'main' alone: the viewparent of each of h's headlines, as
/// the outline describes it ("hiddenOutsideOfSubscribeeFolder",
/// "hiddenInSubscribeeFolder", or "node e" for e's content).
fn where_h_stands (
  graph : &InRustGraph,
) -> ( Vec<String>, Vec<String> ) {
  let input : String = formatdoc! ("
    * {} s
    ** (skg subscribeeFolder)
    *** {} e
    ", occurrence_metadata ("s", "(viewRequests (folder subscribesTo))"),
       occurrence_metadata ("e", "") );
  let restriction : SkgrepoRestriction = main_only ();
  let under = |restriction : Option<&SkgrepoRestriction>| -> Vec<String> {
    viewparents_of_h ( &run_asserting_fixpoint_in (
      &input, graph, restriction, RefreshMode::ContentView, 100 ) . outline () ) };
  ( under (None), under (Some (&restriction)) ) }

/// The viewparent of each line of 'outline' that stands for h.
fn viewparents_of_h (
  outline : &str,
) -> Vec<String> {
  let lines : Vec<&str> = outline . lines () . collect ();
  let depth = |line : &str| -> usize { line . len () - line . trim_start () . len () };
  lines . iter () . enumerate ()
    . filter ( |(_, line)| line . trim_start () . starts_with ("node h") )
    . map ( |(index, line)| {
      lines [.. index] . iter () . rev ()
        . find ( |above| depth (above) < depth (line) )
        . map ( |above| above . trim () . to_string () )
        . unwrap_or_default () } )
    . collect () }

const OUTSIDE : &str = "hiddenOutsideOfSubscribeeFolder";
const INSIDE  : &str = "hiddenInSubscribeeFolder";
const CONTENT : &str = "node e";

#[test]
fn a_visible_hide_of_what_e_does_not_contain_stands_outside () {
  assert_eq! ( where_h_stands ( &s_e_h (Record::Main, None) ),
               ( vec! [OUTSIDE . to_string ()], vec! [OUTSIDE . to_string ()] )); }

#[test]
fn a_visible_hide_of_what_e_contains_visibly_stands_inside () {
  assert_eq! ( where_h_stands ( &s_e_h (Record::Main, Some (Record::Main)) ),
               ( vec! [INSIDE . to_string ()], vec! [INSIDE . to_string ()] )); }

/// Under the skgrepo narrowing e's content is what its visible
/// records say, which leaves h out: the hide stands outside.
#[test]
fn a_visible_hide_of_what_e_contains_restrictedly_stands_outside_under_a_narrowing () {
  assert_eq! ( where_h_stands ( &s_e_h (Record::Main, Some (Record::Private)) ),
               ( vec! [INSIDE . to_string ()], vec! [OUTSIDE . to_string ()] )); }

/// Under the skgrepo narrowing the restricted hide stands in neither
/// folder, and e does not contain h.
#[test]
fn a_restricted_hide_of_what_e_does_not_contain_stands_nowhere_under_a_narrowing () {
  assert_eq! ( where_h_stands ( &s_e_h (Record::Private, None) ),
               ( vec! [OUTSIDE . to_string ()], Vec::<String>::new () )); }

/// Under the skgrepo narrowing the restricted hide stands in neither
/// folder, and h is e's content, as e's visible record says.
#[test]
fn a_restricted_hide_of_what_e_contains_visibly_is_es_content_under_a_narrowing () {
  assert_eq! ( where_h_stands ( &s_e_h (Record::Private, Some (Record::Main)) ),
               ( vec! [INSIDE . to_string ()], vec! [CONTENT . to_string ()] )); }

#[test]
fn a_restricted_hide_of_what_e_contains_restrictedly_stands_nowhere_under_a_narrowing () {
  assert_eq! ( where_h_stands ( &s_e_h (Record::Private, Some (Record::Private)) ),
               ( vec! [INSIDE . to_string ()], Vec::<String>::new () )); }

/// Lead 2's case B: e itself is private, so its content, recorded in
/// its own file, is restricted, while s's subscription and hide are
/// visible. Under the skgrepo narrowing e is a restricted viewnode,
/// and the hide stands outside, which says nothing of e's content.
#[test]
fn a_visible_hide_of_what_a_restricted_subscribee_contains_stands_outside_under_a_narrowing () {
  let graph : InRustGraph = graph (&[
    Graphnode { subscribesTo  : MSV::Specified ( at (Record::Main, &["e"]) ),
                hidesFromSubs : MSV::Specified ( at (Record::Main, &["h"]) ),
                .. node ("s") },
    Graphnode { home_skgrepo : SkgrepoName::from ("private"),
                contains     : at (Record::Private, &["c", "h"]),
                .. node ("e") },
    node ("c"), node ("h") ]);
  assert_eq! ( where_h_stands (&graph),
               ( vec! [INSIDE . to_string ()], vec! [OUTSIDE . to_string ()] )); }
