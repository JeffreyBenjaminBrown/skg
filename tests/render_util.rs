// cargo nextest run --test grouped_unit -E 'test(render_util::)'

use skg::assert_metadata_eq;
use skg::org_to_text::viewnode_to_text;
use skg::herald_tokens::RelationshipHeralds;
use skg::serve::parse_metadata_sexp::{ViewnodeMetadata, parse_metadata_to_viewnodemd};
use skg::types::misc::{ID, SkgConfig, Skgrepo, SkgrepoName};
use skg::types::viewnode::{ Viewnode, ViewnodeKind, Vognode, UnrestrictedVognode, Editability, ViewnodeStats, default_unrestrictedVognode };
use skg::types::viewnode::{Folder, Warrant, viewnode_of};
use std::collections::HashMap;
use std::path::PathBuf;

#[test]
fn test_viewnode_to_text_no_metadata () {
  let node : Viewnode = viewnode_of (ViewnodeKind::Vognode (Vognode::Unrestricted (
      default_unrestrictedVognode ( ID::from ("test"),
                         SkgrepoName::from ("main"),
                         "Test Title" . to_string() ))));
  let result : String =
    viewnode_to_text ( 1, &node, &SkgConfig::dummyFromSkgrepos (HashMap::new ()) )
    . expect ("UnrestrictedVognode rendering never fails");
  assert_metadata_eq! ( result, "* (skg node (gnid test) (skgRepos (nodeRepo main))) Test Title\n" ); }

#[test]
fn test_viewnode_to_text_with_body () {
  let t : UnrestrictedVognode = UnrestrictedVognode {
    editability : Editability::Editable {
      body         : Some ( "Test body content" . to_string() ),
      edit_request : None },
    .. default_unrestrictedVognode ( ID::from ("test"),
                          SkgrepoName::from ("main"),
                          "Test Title" . to_string() ) };
  let node : Viewnode = viewnode_of (ViewnodeKind::Vognode (Vognode::Unrestricted (t)));
  let result : String =
    viewnode_to_text ( 2, &node, &SkgConfig::dummyFromSkgrepos (HashMap::new ()) )
    . expect ("UnrestrictedVognode rendering never fails");
  assert_metadata_eq! ( result, "** (skg node (gnid test) (skgRepos (nodeRepo main))) Test Title\nTest body content\n" ); }

#[test]
fn test_viewnode_to_text_with_metadata () {
  let mut node = viewnode_of (ViewnodeKind::Folder (
      Folder::Alias));
  node . foldAndFocus . folded = true;
  let result : String =
    viewnode_to_text ( 1, &node, &SkgConfig::dummyFromSkgrepos (HashMap::new ()) )
    . expect ("AliasFolder rendering never fails");
  assert_metadata_eq! ( result, "* (skg folded aliasFolder)\n" ); }

#[test]
fn test_viewnode_to_text_with_skgid_metadata () {
  let t : UnrestrictedVognode = UnrestrictedVognode {
    editability : Editability::WriteProtected,
    .. default_unrestrictedVognode ( ID::from ("test123"),
                          SkgrepoName::from ("main"),
                          "Test Title" . to_string() ) };
  let node : Viewnode = viewnode_of (ViewnodeKind::Vognode (Vognode::Unrestricted (t)));
  let result : String =
    viewnode_to_text ( 3, &node, &SkgConfig::dummyFromSkgrepos (HashMap::new ()) )
    . expect ("UnrestrictedVognode rendering never fails");
  assert_metadata_eq! ( result, "*** (skg node (gnid test123) (skgRepos (nodeRepo main)) writeProtected) Test Title\n" ); }

#[test]
fn skgrepo_name_with_whitespace_is_one_round_trippable_atom () {
  let skgrepo         : SkgrepoName = SkgrepoName::from ("Mr Cheese");
  let mut unrestricted_node : UnrestrictedVognode =
    default_unrestrictedVognode (
      ID::from ("cheese-node"), skgrepo . clone (),
      "Cooking" . to_string () );
  unrestricted_node . viewStats . repoBoundary = true;
  let node : Viewnode =
    viewnode_of ( ViewnodeKind::Vognode (
      Vognode::Unrestricted (unrestricted_node) ));
  let config = SkgConfig::dummyFromSkgrepos (HashMap::from ([
    ( skgrepo . clone (), Skgrepo {
        name         : skgrepo,
        abbreviation : None,
        path         : PathBuf::from ("cheese"),
        owned        : false } ) ]));
  let rendered : String =
    viewnode_to_text (1, &node, &config)
    . expect ("UnrestrictedVognode rendering never fails");
  assert_eq! (
    rendered,
    "* (skg node (gnid cheese-node) (skgRepos (nodeRepo \"Mr Cheese\")) (view repoBoundary)) Cooking\n" );
  let metadata = parse_metadata_to_viewnodemd (
    rendered
      . split_once (" Cooking")
      . expect ("rendered headline has title") . 0
      . strip_prefix ("* ")
      . expect ("rendered headline has bullet") )
    . expect ("quoted repo should parse");
  assert_eq! (
    metadata . home_skgrepo, Some (SkgrepoName::from ("Mr Cheese")) );
}

#[test]
fn test_metadata_ordering () {
  let t : UnrestrictedVognode = UnrestrictedVognode {
    viewStats : ViewnodeStats {
      cycle             : true,
      .. ViewnodeStats::default() },
    .. default_unrestrictedVognode ( ID::from ("xyz"),
                          SkgrepoName::from ("main"),
                          "Test" . to_string() ) };
  let node : Viewnode = viewnode_of (ViewnodeKind::Vognode (Vognode::Unrestricted (t)));
  let result : String =
    viewnode_to_text ( 1, &node, &SkgConfig::dummyFromSkgrepos (HashMap::new ()) )
    . expect ("UnrestrictedVognode rendering never fails");
  assert_metadata_eq! ( result, "* (skg node (gnid xyz) (skgRepos (nodeRepo main)) (view cycle)) Test\n" ); }

#[test]
fn an_unwarranted_occurrence_says_noWarrant_and_the_parser_drops_it () {
  let t : UnrestrictedVognode = UnrestrictedVognode {
    viewStats : ViewnodeStats {
      isWarranted : Warrant::No,
      cycle       : true,
      .. ViewnodeStats::default() },
    .. default_unrestrictedVognode ( ID::from ("xyz"),
                          SkgrepoName::from ("main"),
                          "Test" . to_string() ) };
  let node : Viewnode = viewnode_of (ViewnodeKind::Vognode (Vognode::Unrestricted (t)));
  let result : String =
    viewnode_to_text ( 1, &node, &SkgConfig::dummyFromSkgrepos (HashMap::new ()) )
    . expect ("UnrestrictedVognode rendering never fails");
  assert_metadata_eq! ( result, "* (skg node (gnid xyz) (skgRepos (nodeRepo main)) (view noWarrant cycle)) Test\n" );
  let metadata : ViewnodeMetadata =
    parse_metadata_to_viewnodemd (
      "(skg node (gnid xyz) (skgRepos (nodeRepo main)) (view noWarrant cycle))" )
    . expect ("noWarrant parses");
  assert_eq! ( metadata . viewStats . isWarranted, Warrant::ByGraph ); }

#[test]
fn test_rel_heralds_emitted () {
  // The semantic (rels ...), (props ...) and (birth ...) strings are
  // emitted verbatim inside (view ...); a node with none emits no view.
  let mk = | heralds : RelationshipHeralds | -> String {
    let t : UnrestrictedVognode = UnrestrictedVognode {
      viewStats : ViewnodeStats {
        rel_heralds : heralds,
        .. ViewnodeStats::default () },
      .. default_unrestrictedVognode ( ID::from ("n"),
                            SkgrepoName::from ("main"),
                            "N" . to_string () ) };
    let node = viewnode_of (ViewnodeKind::Vognode (Vognode::Unrestricted (t)));
    viewnode_to_text (
      1, &node, &SkgConfig::dummyFromSkgrepos (HashMap::new ()) )
      . unwrap () };
  let with_rels : String = mk ( RelationshipHeralds {
    rels  : Some ("(rels (contains (in 2 (ancestors 1)) (out 1)))" . to_string ()),
    props : Some ("(props (aliases 2))" . to_string ()),
    birth : Some ("(birth (contains in 1))" . to_string ()) } );
  assert_eq! ( with_rels,
    "* (skg node (gnid n) (skgRepos (nodeRepo main)) (view (rels (contains (in 2 (ancestors 1)) (out 1))) (props (aliases 2)) (birth (contains in 1)))) N\n",
    "rels not emitted verbatim: {}", with_rels );
  let neither : String = mk ( RelationshipHeralds::default () );
  assert! ( ! neither . contains ("(view ") );
  assert_metadata_eq! ( neither, "* (skg node (gnid n) (skgRepos (nodeRepo main))) N\n" ); }
