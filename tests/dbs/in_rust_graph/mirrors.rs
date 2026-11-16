//! The mirrors of a note, and which of them stands in its place
//! ('server/dbs/in_rust_graph/mirrors.rs').
//!
//! The config declares, in order: 'my-y' and 'my-x', owned (so 'my-y'
//! is the more public), then 'your-x' and 'your-y', foreign, one
//! owner's, whose manifest says 'your-x' is the more public. So the
//! user's skgrepo list is (my-x, my-y, your-x, your-y), as in Jeff's
//! example, and the foreign owner's is (your-y, your-x, my-y, my-x).

use skg::dbs::in_rust_graph::InRustGraph;
use skg::dbs::in_rust_graph::mirrors::{
  is_mirror_of, mirror_shown, mirrors_of};
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::types::misc::{
  ForeignManifestOrder, ID, MSV, RelPartner, SkgConfig, Skgrepo, SkgrepoName,
  SkgrepoOwner, rel_partners_at_relRepo};
use skg::types::nodes::complete::{Graphnode, empty_graphnode};

use std::collections::HashMap;
use std::path::PathBuf;

const DECLARED : [(&str, bool); 4] = [
  ("my-y", true), ("my-x", true), ("your-x", false), ("your-y", false) ];

fn config () -> SkgConfig {
  let mut config : SkgConfig = SkgConfig::dummyFromSkgrepos (
    DECLARED . iter () . map ( |(name, owned)| (
      SkgrepoName::from (*name),
      Skgrepo { name         : SkgrepoName::from (*name),
                abbreviation : None,
                path         : PathBuf::from ( format! ("/tmp/{}", name) ),
                owned        : *owned } ))
    . collect::<HashMap<SkgrepoName, Skgrepo>> () );
  config . skgrepo_order = DECLARED . iter ()
    . map ( |(name, _owned)| SkgrepoName::from (*name) ) . collect ();
  config . set_foreign_manifest_orders ( vec! [ ForeignManifestOrder {
    manifest_skgrepo : SkgrepoName::from ("your-y"),
    listed           : vec! [ SkgrepoName::from ("your-x"),
                              SkgrepoName::from ("your-y") ] } ] );
  config }

fn me () -> SkgrepoOwner { SkgrepoOwner::User }

fn you () -> SkgrepoOwner { SkgrepoOwner::Foreign ( SkgrepoName::from ("your-x") ) }

/// A note 'pid' homed in 'skgrepo', forking each of 'forks', each
/// relationship recorded in its home.
fn note (
  pid     : &str,
  skgrepo : &str,
  forks   : &[&str],
) -> Graphnode {
  let mut node : Graphnode = empty_graphnode ();
  node . pid = ID::from (pid);
  node . title = pid . to_string ();
  node . home_skgrepo = SkgrepoName::from (skgrepo);
  node . overrides =
    if forks . is_empty () { MSV::Unspecified }
    else { MSV::Specified ( rel_partners_at_relRepo (
      &node . home_skgrepo,
      forks . iter () . map ( |gnid| ID::from (*gnid) ) . collect () )) };
  node }

fn unrestricted () -> SkgrepoRestriction {
  SkgrepoRestriction::unrestricted (&config ()) }

/// What stands in the place of 'gnid', from 'owner''s perspective.
fn shown (
  notes       : &[Graphnode],
  restriction : &SkgrepoRestriction,
  owner       : &SkgrepoOwner,
  gnid        : &str,
) -> ID {
  let graph : InRustGraph = InRustGraph::from_graphnodes (notes);
  mirror_shown (&config (), &graph, restriction, owner, &ID::from (gnid)) }

/// Assert that each of 'notes', asked about, shows 'expected', from
/// 'owner''s perspective, unrestricted.
fn every_one_shows (
  notes    : &[Graphnode],
  owner    : &SkgrepoOwner,
  expected : &str,
) {
  for asked in notes {
    assert_eq! (
      shown (notes, &unrestricted (), owner, &asked . pid . 0),
      ID::from (expected),
      "asked of {}, from {:?}'s perspective", asked . pid . 0, owner ); }}

/// Jeff: "If my list of skgrepos is (my X, my Y, your X, your Y) then
/// a node only in my repo Y and your repo X shows as in my Y, one only
/// in your X and your Y shows to me as in your X, etc." Each pair is a
/// note and its fork, whichever of the two is asked about; the
/// foreign owner's list ranks its own most private first.
#[test]
fn jeffs_example_from_both_perspectives () {
  for ( placements, mine, yours ) in [
    ( [ ("n", "your-x", vec! []), ("f", "my-y",   vec! ["n"]) ], "f", "n" ),
    ( [ ("n", "your-y", vec! []), ("f", "your-x", vec! ["n"]) ], "f", "n" ),
    ( [ ("n", "my-y",   vec! []), ("f", "my-x",   vec! ["n"]) ], "f", "n" ),
    ( [ ("n", "your-y", vec! []), ("f", "my-x",   vec! ["n"]) ], "f", "n" ),
  ] {
    let notes : Vec<Graphnode> = placements . iter ()
      . map ( |(pid, skgrepo, forks)| note (pid, skgrepo, forks) ) . collect ();
    every_one_shows (&notes, &me (), mine);
    every_one_shows (&notes, &you (), yours); }}

#[test]
fn a_perspectives_skgrepo_list_is_its_owners_most_private_first_then_declaration_order () {
  let names = |list : Vec<SkgrepoName>| -> Vec<String> {
    list . into_iter () . map ( |name| name . 0 ) . collect () };
  assert_eq! ( names ( config () . perspective_skgrepo_list (&me ()) ),
               vec! [ "my-x", "my-y", "your-x", "your-y" ] );
  assert_eq! ( names ( config () . perspective_skgrepo_list (&you ()) ),
               vec! [ "your-y", "your-x", "my-y", "my-x" ] ); }

/// A third author's mirror stands where the perspective's owner has
/// none: from the user's perspective, a foreign note forking another
/// foreign note shows by the order the config declares them in.
#[test]
fn a_mirror_the_perspectives_owner_does_not_own_may_stand () {
  every_one_shows (
    &[ note ("n", "your-y", &[]), note ("f", "your-x", &["n"]) ],
    &me (), "f" ); }

#[test]
fn a_foreign_two_cycle_shows_the_first_ranked_from_either_note () {
  let notes : Vec<Graphnode> = vec! [
    note ("a", "your-x", &["b"]), note ("b", "your-y", &["a"]) ];
  every_one_shows (&notes, &me (), "a");
  every_one_shows (&notes, &you (), "b"); }

/// Q3, rule 1: one mirror alone in the first-ranked skgrepo stands,
/// though a forker of it is elsewhere.
#[test]
fn rule_one_a_mirror_alone_in_the_first_skgrepo_stands () {
  every_one_shows (
    &[ note ("n", "your-x", &[]), note ("c", "my-y", &["n"]),
       note ("d", "your-y", &["c"]) ],
    &me (), "c" ); }

/// Q3, rule 2: of a chain in one skgrepo, the one no other forks.
#[test]
fn rule_two_the_one_no_other_there_forks_stands () {
  every_one_shows (
    &[ note ("n", "my-x", &[]), note ("c", "my-x", &["n"]),
       note ("d", "my-x", &["c"]) ],
    &me (), "d" ); }

/// Q3, rule 3: two forks of N in N's own skgrepo tie, and N stands;
/// so does a mutual pair in one skgrepo, from either note.
#[test]
fn rule_three_the_original_stands_among_a_tie () {
  let siblings : Vec<Graphnode> = vec! [
    note ("n", "my-x", &[]), note ("r", "my-x", &["n"]), note ("t", "my-x", &["n"]) ];
  assert_eq! ( shown (&siblings, &unrestricted (), &me (), "n"), ID::from ("n") );
  let mutual : Vec<Graphnode> = vec! [
    note ("a", "my-x", &["b"]), note ("b", "my-x", &["a"]) ];
  assert_eq! ( shown (&mutual, &unrestricted (), &me (), "a"), ID::from ("a") );
  assert_eq! ( shown (&mutual, &unrestricted (), &me (), "b"), ID::from ("b") ); }

/// Q3, rule 4: N is not among the tie (a, b and c, all in 'my-x',
/// where b and c fork N and a forks b; a and c are forked by none of
/// the others), so the nearest to N stands, and among equals (b and c)
/// the least pid. 'a', the least pid of all, is farther.
#[test]
fn rule_four_the_nearest_then_the_least_pid_stands () {
  assert_eq! (
    shown ( &[ note ("n", "your-x", &[]), note ("b", "my-x", &["n"]),
               note ("c", "my-x", &["n"]), note ("a", "my-x", &["b"]) ],
            &unrestricted (), &me (), "n" ),
    ID::from ("b") ); }

/// A restricted note in the middle of a path cuts it: 'c', homed in
/// 'my-x', which the restriction keeps out, joins 'n' to 'd', which
/// the foreign owner's list would otherwise rank first.
#[test]
fn a_restricted_middle_note_cuts_the_path () {
  let notes : Vec<Graphnode> = vec! [
    note ("n", "my-y", &[]), note ("c", "my-x", &["n"]), note ("d", "your-y", &["c"]) ];
  let restricted : SkgrepoRestriction =
    SkgrepoRestriction::named (&config (), SkgrepoName::from ("my-y")) . unwrap ();
  assert_eq! ( shown (&notes, &unrestricted (), &you (), "n"), ID::from ("d") );
  assert_eq! ( shown (&notes, &restricted, &you (), "n"), ID::from ("n") );
  let graph : InRustGraph = InRustGraph::from_graphnodes (&notes);
  assert_eq! ( mirrors_of (&graph, &restricted, &ID::from ("n")) . pids (),
               vec! [ ID::from ("n") ] ); }

/// A relationship whose relRepo the restriction keeps out joins no
/// mirrors, though both notes are shown.
#[test]
fn a_restricted_relationship_joins_no_mirrors () {
  let mut fork : Graphnode = note ("f", "my-y", &[]);
  fork . overrides = MSV::Specified (vec! [
    RelPartner::at_relRepo ( SkgrepoName::from ("my-x"), ID::from ("n") ) ]);
  let notes : Vec<Graphnode> = vec! [ note ("n", "your-x", &[]), fork ];
  let restricted : SkgrepoRestriction =
    SkgrepoRestriction::named (&config (), SkgrepoName::from ("my-y")) . unwrap ();
  assert_eq! ( shown (&notes, &unrestricted (), &me (), "n"), ID::from ("f") );
  assert_eq! ( shown (&notes, &restricted, &me (), "n"), ID::from ("n") ); }

#[test]
fn an_extra_id_names_the_same_mirrors_as_the_pid () {
  let mut original : Graphnode = note ("n", "your-x", &[]);
  original . extra_ids = vec! [ ID::from ("n-extra") ];
  let notes : Vec<Graphnode> = vec! [
    original, note ("f", "my-y", &["n-extra"]) ];
  assert_eq! ( shown (&notes, &unrestricted (), &me (), "n-extra"), ID::from ("f") );
  assert_eq! ( shown (&notes, &unrestricted (), &me (), "n"), ID::from ("f") ); }

#[test]
fn a_note_with_no_mirror_and_an_unknown_gnid_show_themselves () {
  let notes : Vec<Graphnode> = vec! [ note ("n", "your-x", &[]) ];
  assert_eq! ( shown (&notes, &unrestricted (), &me (), "n"), ID::from ("n") );
  assert_eq! ( shown (&notes, &unrestricted (), &me (), "never-heard-of-it"),
               ID::from ("never-heard-of-it") ); }

/// Siblings are mirrors of each other, and of their forkee; a note
/// unrelated by forks is not.
#[test]
fn is_mirror_of_follows_forks_either_way () {
  let notes : Vec<Graphnode> = vec! [
    note ("n", "your-x", &[]), note ("r", "my-y", &["n"]),
    note ("t", "your-y", &["n"]), note ("u", "my-x", &[]) ];
  let graph : InRustGraph = InRustGraph::from_graphnodes (&notes);
  let mirror = |a : &str, b : &str| -> bool {
    is_mirror_of (&graph, &unrestricted (), &ID::from (a), &ID::from (b)) };
  assert! ( mirror ("n", "r") && mirror ("r", "n") && mirror ("r", "t") && mirror ("t", "r") );
  assert! ( ! mirror ("n", "u") ); }
