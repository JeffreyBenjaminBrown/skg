use super::*;
use crate::from_text::buffer_to_viewnodes::uninterpreted
  ::org_to_uninterpreted_viewforest;
use crate::types::maybe_placed_viewnode::maybePlaced_to_placed_viewforest;
use crate::types::misc::{MSV, Skgrepo, SkgrepoName, rel_partners_at_relRepo};
use crate::types::nodes::complete::{Flag, Graphnode, empty_graphnode};
use crate::types::tree::forest::ViewForest;
use indoc::indoc;
use std::collections::HashMap;
use std::path::PathBuf;

fn forest (
  text : &str,
) -> ViewForest {
  let (forest, errors, _warnings) =
    org_to_uninterpreted_viewforest (text) . unwrap ();
  assert! (errors . is_empty (), "unexpected parse errors: {:?}", errors);
  maybePlaced_to_placed_viewforest (forest) . unwrap () }

/// p contains c and is contained by q, which top contains; p has the
/// alias "another name" and the flag noSearchMatching. s subscribes
/// to e, which contains h1 and h2, and s hides h1 and o.
fn graph () -> InRustGraph {
  let main : SkgrepoName = SkgrepoName::from ("main");
  let node = |pid : &str| -> Graphnode { Graphnode {
    pid          : ID::from (pid),
    title        : pid . to_string (),
    home_skgrepo : main . clone (),
    .. empty_graphnode () }};
  let ids = |pids : &[&str]| -> Vec<ID> {
    pids . iter () . map ( |pid| ID::from (*pid) ) . collect () };
  InRustGraph::from_graphnodes (&[
    Graphnode {
      contains : rel_partners_at_relRepo (&main, ids (&["c"])),
      aliases  : MSV::Specified (rel_partners_at_relRepo (
        &main, vec! ["another name" . to_string ()])),
      flags    : vec! [Flag::NoSearchMatching],
      .. node ("p") },
    node ("c"),
    node ("typed"),
    Graphnode {
      contains : rel_partners_at_relRepo (&main, ids (&["p"])),
      .. node ("q") },
    Graphnode {
      contains : rel_partners_at_relRepo (&main, ids (&["q"])),
      .. node ("top") },
    Graphnode {
      subscribesTo  : MSV::Specified (rel_partners_at_relRepo (&main, ids (&["e"]))),
      hidesFromSubs : MSV::Specified (rel_partners_at_relRepo (&main, ids (&["h1", "o"]))),
      .. node ("s") },
    Graphnode {
      contains : rel_partners_at_relRepo (&main, ids (&["h1", "h2"])),
      .. node ("e") },
    node ("h1"),
    node ("h2"),
    node ("o"), ]) }

fn config () -> SkgConfig {
  SkgConfig::dummyFromSkgrepos ( HashMap::from ([ (
    SkgrepoName::from ("main"),
    Skgrepo { name         : SkgrepoName::from ("main"),
              abbreviation : None,
              path         : PathBuf::from ("main"),
              owned        : true, } ) ]) ) }

/// The title of each occurrence and property member of 'text', in
/// document order, with its warrant.
fn warrants (
  text : &str,
) -> Vec<(String, Warrant)> {
  let forest : ViewForest = forest (text);
  let (graph, config) : (InRustGraph, SkgConfig) = (graph (), config ());
  forest . nodes ()
    . filter ( |node| matches! ( &node . value () . kind,
                 ViewnodeKind::Vognode (Vognode::Unrestricted (_))
                 | ViewnodeKind::PropertyMember (_) ))
    . map ( |node| ( node . value () . title () . to_string (),
                     warrant_of_viewnode (node, &graph, &config, None) ))
    . collect () }

fn expect (
  pairs : &[(&str, Warrant)],
) -> Vec<(String, Warrant)> {
  pairs . iter ()
    . map ( |(title, warrant)| (title . to_string (), *warrant) )
    . collect () }

#[test]
fn a_root_is_warranted () {
  assert_eq! ( warrants ("* (skg (node (id c) (repo main))) c\n"),
               expect (&[("c", Warrant::ByGraph)]) ); }

#[test]
fn content_is_warranted_when_its_parent_contains_it () {
  assert_eq! (
    warrants (indoc! {"
      * (skg (node (id p) (repo main))) p
      ** (skg (node (id c) (repo main))) c
      ** (skg (node (id typed) (repo main) noClaim)) typed
      ** (skg (node (id top) (repo main) writeProtected)) top
    "}),
    expect (&[ ("p",     Warrant::ByGraph),
               ("c",     Warrant::ByGraph),
               ("typed", Warrant::No),
               ("top",   Warrant::No) ]) ); }

#[test]
fn a_folder_member_is_warranted_when_the_folders_relation_holds () {
  assert_eq! (
    warrants (indoc! {"
      * (skg (node (id p) (repo main))) p
      ** (skg containerFolder)
      *** (skg (node (id q) (repo main) writeProtected)) q
      *** (skg (node (id c) (repo main) writeProtected)) c
    "}),
    expect (&[ ("p", Warrant::ByGraph),
               ("q", Warrant::ByGraph),
               ("c", Warrant::No) ]) ); }

#[test]
fn a_chain_node_is_warranted_when_it_contains_the_node_above () {
  assert_eq! (
    warrants (indoc! {"
      * (skg (node (id p) (repo main))) p
      ** (skg containerFolder)
      *** (skg (node (id q) (repo main) writeProtected)) q
      **** (skg (node (id top) (repo main) noClaim writeProtected)) top
      **** (skg (node (id c) (repo main) noClaim writeProtected)) c
    "}),
    expect (&[ ("p",   Warrant::ByGraph),
               ("q",   Warrant::ByGraph),
               ("top", Warrant::ByGraph),
               ("c",   Warrant::No) ]) ); }

#[test]
fn a_compound_folders_member_is_warranted_by_the_hide_and_the_containment () {
  assert_eq! (
    warrants (indoc! {"
      * (skg (node (id s) (repo main))) s
      ** (skg subscribeeFolder)
      *** (skg (node (id e) (repo main) writeProtected)) e
      **** (skg hiddenInSubscribeeFolder)
      ***** (skg (node (id h1) (repo main) writeProtected)) h1
      ***** (skg (node (id h2) (repo main) writeProtected)) h2
      *** (skg hiddenOutsideOfSubscribeeFolder)
      **** (skg (node (id o) (repo main) writeProtected)) o
      **** (skg (node (id h1) (repo main) writeProtected)) h1
    "}),
    expect (&[ ("s",  Warrant::ByGraph),
               ("e",  Warrant::ByGraph),
               ("h1", Warrant::ByGraph),
               ("h2", Warrant::No),
               ("o",  Warrant::ByGraph),
               ("h1", Warrant::No) ]) ); }

#[test]
fn a_property_member_is_warranted_when_the_node_has_the_property () {
  assert_eq! (
    warrants (indoc! {"
      * (skg (node (id p) (repo main))) p
      ** (skg aliasFolder)
      *** (skg alias) another name
      *** (skg alias) no name of p
      ** (skg idFolder)
      *** (skg id) p
      *** (skg id) stranger
      ** (skg flagsFolder)
      *** (skg (flag noSearchMatching))
    "}),
    expect (&[ ("p",            Warrant::ByGraph),
               ("another name", Warrant::ByGraph),
               ("no name of p", Warrant::No),
               ("p",            Warrant::ByGraph),
               ("stranger",     Warrant::No),
               ("",             Warrant::ByGraph) ]) ); }

#[test]
fn under_a_tombstone_everything_is_warranted () {
  assert_eq! (
    warrants (indoc! {"
      * (skg tombstone)
      ** (skg (node (id typed) (repo main))) typed
    "}),
    expect (&[ ("typed", Warrant::ByGraph) ]) ); }
