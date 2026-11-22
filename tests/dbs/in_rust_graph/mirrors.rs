//! The mirrors of a note, and which of them stands in its place
//! ('server/dbs/in_rust_graph/mirrors.rs').
//!
//! The config declares, in order: 'my-y' and 'my-x', owned (so 'my-y'
//! is the more public), then 'your-x' and 'your-y', foreign. The
//! skgrepo list of the perspective of S is S, then the others in that
//! order ('docs/mirrors.org', "Which mirror stands").

use skg::dbs::in_rust_graph::InRustGraph;
use skg::dbs::in_rust_graph::mirrors::{
  is_mirror_of, mirror_shown, mirrors_of};
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::types::config::{SkgConfig, Skgrepo};
use skg::types::misc::{ID, MSV, RelPartner, SkgrepoName, rel_partners_at_relRepo};
use skg::types::nodes::complete::{Graphnode, empty_graphnode};

use std::collections::HashMap;
use std::path::PathBuf;

const DECLARED : [(&str, bool); 4] = [
  ("my-y", true), ("my-x", true), ("your-x", false), ("your-y", false) ];

/// The same skgrepos, a foreign one declared first.
const FOREIGN_DECLARED_FIRST : [(&str, bool); 4] = [
  ("your-x", false), ("my-y", true), ("my-x", true), ("your-y", false) ];

fn config () -> SkgConfig {
  config_declaring (&DECLARED) }

fn config_declaring (
  declared : &[(&str, bool)],
) -> SkgConfig {
  let mut config : SkgConfig = SkgConfig::dummyFromSkgrepos (
    declared . iter () . map ( |(name, owned)| (
      SkgrepoName::from (*name),
      Skgrepo { name         : SkgrepoName::from (*name),
                abbreviation : None,
                path         : PathBuf::from ( format! ("/tmp/{}", name) ),
                owned        : *owned } ))
    . collect::<HashMap<SkgrepoName, Skgrepo>> () );
  config . skgrepo_order = declared . iter ()
    . map ( |(name, _owned)| SkgrepoName::from (*name) ) . collect ();
  config }

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
  node . forks =
    if forks . is_empty () { MSV::Unspecified }
    else { MSV::Specified ( rel_partners_at_relRepo (
      &node . home_skgrepo,
      forks . iter () . map ( |gnid| ID::from (*gnid) ) . collect () )) };
  node }

fn unrestricted () -> SkgrepoRestriction {
  SkgrepoRestriction::unrestricted (&config ()) }

/// What stands in the place of 'gnid', from the perspective of
/// 'skgrepo', under 'config'.
fn shown_under (
  config      : &SkgConfig,
  notes       : &[Graphnode],
  restriction : &SkgrepoRestriction,
  skgrepo     : &str,
  gnid        : &str,
) -> ID {
  let graph : InRustGraph = InRustGraph::from_graphnodes (notes);
  mirror_shown ( config, &graph, restriction,
                 &SkgrepoName::from (skgrepo), &ID::from (gnid) ) }

/// What stands in the place of 'gnid', from the perspective of
/// 'skgrepo'.
fn shown (
  notes       : &[Graphnode],
  restriction : &SkgrepoRestriction,
  skgrepo     : &str,
  gnid        : &str,
) -> ID {
  shown_under (&config (), notes, restriction, skgrepo, gnid) }

/// Assert that each of 'notes', asked about, shows 'expected', from
/// the perspective of 'skgrepo', unrestricted.
fn every_one_shows (
  notes    : &[Graphnode],
  skgrepo  : &str,
  expected : &str,
) {
  for asked in notes {
    assert_eq! (
      shown (notes, &unrestricted (), skgrepo, &asked . pid . 0),
      ID::from (expected),
      "asked of {}, from the perspective of {}", asked . pid . 0, skgrepo ); }}

#[test]
fn a_perspectives_skgrepo_list_is_its_skgrepo_then_declaration_order () {
  let names = |skgrepo : &str| -> Vec<String> {
    config () . perspective_skgrepo_list ( &SkgrepoName::from (skgrepo) )
      . into_iter () . map ( |name| name . 0 ) . collect () };
  assert_eq! ( names ("my-y"),   vec! [ "my-y", "my-x", "your-x", "your-y" ] );
  assert_eq! ( names ("my-x"),   vec! [ "my-x", "my-y", "your-x", "your-y" ] );
  assert_eq! ( names ("your-x"), vec! [ "your-x", "my-y", "my-x", "your-y" ] );
  assert_eq! ( names ("your-y"), vec! [ "your-y", "my-y", "my-x", "your-x" ] ); }

/// Four mirrors of n, one in each of four skgrepos: n in your-x,
/// forked by f in my-y, g in my-x and h in your-y. Each skgrepo's
/// perspective shows its own mirror, whichever of the four is asked
/// about.
#[test]
fn each_skgrepos_perspective_shows_its_own_mirror () {
  let notes : Vec<Graphnode> = vec! [
    note ("n", "your-x", &[]), note ("f", "my-y", &["n"]),
    note ("g", "my-x", &["n"]), note ("h", "your-y", &["n"]) ];
  for (skgrepo, expected) in [
    ("my-y", "f"), ("my-x", "g"), ("your-x", "n"), ("your-y", "h") ] {
    every_one_shows (&notes, skgrepo, expected); }}

/// A public and a private fork of one note: each stands when its
/// skgrepo is chosen; with neither chosen (the perspective of the
/// original's skgrepo aside, which shows the original) the public one
/// stands, the config declaring it first.
#[test]
fn a_private_and_a_public_fork_each_chosen_in_turn () {
  let notes : Vec<Graphnode> = vec! [
    note ("n", "your-x", &[]), note ("public", "my-y", &["n"]),
    note ("private", "my-x", &["n"]) ];
  every_one_shows (&notes, "my-y", "public");
  every_one_shows (&notes, "my-x", "private");
  every_one_shows (&notes, "your-x", "n");
  every_one_shows (&notes, "your-y", "public"); }

/// A mirror in a skgrepo the perspective's is not stands where the
/// perspective's holds none: the first skgrepo declared that holds
/// one decides.
#[test]
fn a_mirror_outside_the_perspectives_skgrepo_may_stand () {
  every_one_shows (
    &[ note ("n", "your-y", &[]), note ("f", "your-x", &["n"]) ],
    "my-x", "f" ); }

/// Where a foreign skgrepo is declared first, its original outranks
/// the user's own fork wherever the perspective's skgrepo holds no
/// mirror; the config's own order stands.
#[test]
fn a_foreign_skgrepo_declared_first_outranks_the_users_fork () {
  let config : SkgConfig = config_declaring (&FOREIGN_DECLARED_FIRST);
  let notes : Vec<Graphnode> = vec! [
    note ("n", "your-x", &[]), note ("f", "my-y", &["n"]) ];
  for (skgrepo, expected) in [
    ("my-x", "n"), ("your-y", "n"), ("your-x", "n"), ("my-y", "f") ] {
    for asked in ["n", "f"] {
      assert_eq! ( shown_under (&config, &notes, &unrestricted (), skgrepo, asked),
                   ID::from (expected),
                   "asked of {}, from the perspective of {}", asked, skgrepo ); }} }

#[test]
fn a_foreign_two_cycle_shows_the_first_ranked_from_either_note () {
  let notes : Vec<Graphnode> = vec! [
    note ("a", "your-x", &["b"]), note ("b", "your-y", &["a"]) ];
  every_one_shows (&notes, "your-x", "a");
  every_one_shows (&notes, "your-y", "b");
  every_one_shows (&notes, "my-x", "a"); }

/// One mirror alone in the first-ranked skgrepo stands, though a
/// forker of it is elsewhere.
#[test]
fn a_mirror_alone_in_the_first_skgrepo_stands () {
  every_one_shows (
    &[ note ("n", "your-x", &[]), note ("c", "my-y", &["n"]),
       note ("d", "your-y", &["c"]) ],
    "my-y", "c" ); }

/// Rule 1 of 'docs/mirrors.org', "Which mirror stands": N itself
/// stands among a tie in its own skgrepo: two forks of N there; an
/// owned chain D forks C forks N kept there; a mutual pair there,
/// from either note.
#[test]
fn the_note_itself_stands_among_a_tie () {
  let siblings : Vec<Graphnode> = vec! [
    note ("n", "my-x", &[]), note ("r", "my-x", &["n"]), note ("t", "my-x", &["n"]) ];
  assert_eq! ( shown (&siblings, &unrestricted (), "my-x", "n"), ID::from ("n") );
  let chain : Vec<Graphnode> = vec! [
    note ("n", "my-x", &[]), note ("c", "my-x", &["n"]), note ("d", "my-x", &["c"]) ];
  for asked in ["n", "c", "d"] {
    assert_eq! ( shown (&chain, &unrestricted (), "my-x", asked), ID::from (asked) ); }
  let mutual : Vec<Graphnode> = vec! [
    note ("a", "my-x", &["b"]), note ("b", "my-x", &["a"]) ];
  assert_eq! ( shown (&mutual, &unrestricted (), "my-x", "a"), ID::from ("a") );
  assert_eq! ( shown (&mutual, &unrestricted (), "my-x", "b"), ID::from ("b") ); }

/// Rule 2 of 'docs/mirrors.org', "Which mirror stands": N is not
/// among the tie, a chain D forks C forks N kept in one skgrepo other
/// than N's, so the least pid there, C, stands in N's place. Asked of
/// C or D, the note itself is among the tie, so stands.
#[test]
fn a_chain_kept_outside_the_notes_skgrepo_shows_its_least_pid () {
  let notes : Vec<Graphnode> = vec! [
    note ("n", "your-x", &[]), note ("c", "my-x", &["n"]),
    note ("d", "my-x", &["c"]) ];
  for (asked, expected) in [ ("n", "c"), ("c", "c"), ("d", "d") ] {
    assert_eq! ( shown (&notes, &unrestricted (), "my-x", asked), ID::from (expected),
                 "asked of {}", asked ); }}

/// Rule 2 of 'docs/mirrors.org', "Which mirror stands": N is not
/// among the tie (a, b and c, all in 'my-x', where b and c fork N
/// and a forks b), so the least pid, a, stands, though b and c are
/// nearer N.
#[test]
fn the_least_pid_stands_however_far_from_the_note () {
  assert_eq! (
    shown ( &[ note ("n", "your-x", &[]), note ("b", "my-x", &["n"]),
               note ("c", "my-x", &["n"]), note ("a", "my-x", &["b"]) ],
            &unrestricted (), "my-x", "n" ),
    ID::from ("a") ); }

/// A restricted note in the middle of a path cuts it: 'c', homed in
/// 'my-x', which the restriction keeps out, joins 'n' to 'd', which
/// the perspective of 'your-y' would otherwise rank first.
#[test]
fn a_restricted_middle_note_cuts_the_path () {
  let notes : Vec<Graphnode> = vec! [
    note ("n", "my-y", &[]), note ("c", "my-x", &["n"]), note ("d", "your-y", &["c"]) ];
  let restricted : SkgrepoRestriction =
    SkgrepoRestriction::named (&config (), SkgrepoName::from ("my-y")) . unwrap ();
  assert_eq! ( shown (&notes, &unrestricted (), "your-y", "n"), ID::from ("d") );
  assert_eq! ( shown (&notes, &restricted, "your-y", "n"), ID::from ("n") );
  let graph : InRustGraph = InRustGraph::from_graphnodes (&notes);
  assert_eq! ( mirrors_of (&graph, &restricted, &ID::from ("n"), None) . pids (),
               vec! [ ID::from ("n") ] ); }

/// A relationship whose relRepo the restriction keeps out joins no
/// mirrors, though both notes are shown.
#[test]
fn a_restricted_relationship_joins_no_mirrors () {
  let mut fork : Graphnode = note ("f", "my-y", &[]);
  fork . forks = MSV::Specified (vec! [
    RelPartner::at_relRepo ( SkgrepoName::from ("my-x"), ID::from ("n") ) ]);
  let notes : Vec<Graphnode> = vec! [ note ("n", "your-x", &[]), fork ];
  let restricted : SkgrepoRestriction =
    SkgrepoRestriction::named (&config (), SkgrepoName::from ("my-y")) . unwrap ();
  assert_eq! ( shown (&notes, &unrestricted (), "my-y", "n"), ID::from ("f") );
  assert_eq! ( shown (&notes, &restricted, "my-y", "n"), ID::from ("n") ); }

#[test]
fn an_extra_id_names_the_same_mirrors_as_the_pid () {
  let mut original : Graphnode = note ("n", "your-x", &[]);
  original . extra_ids = vec! [ ID::from ("n-extra") ];
  let notes : Vec<Graphnode> = vec! [
    original, note ("f", "my-y", &["n-extra"]) ];
  assert_eq! ( shown (&notes, &unrestricted (), "my-y", "n-extra"), ID::from ("f") );
  assert_eq! ( shown (&notes, &unrestricted (), "my-y", "n"), ID::from ("f") ); }

#[test]
fn a_note_with_no_mirror_and_an_unknown_gnid_show_themselves () {
  let notes : Vec<Graphnode> = vec! [ note ("n", "your-x", &[]) ];
  assert_eq! ( shown (&notes, &unrestricted (), "my-x", "n"), ID::from ("n") );
  assert_eq! ( shown (&notes, &unrestricted (), "my-x", "never-heard-of-it"),
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
