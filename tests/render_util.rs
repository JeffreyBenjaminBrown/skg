// cargo nextest run --test grouped_unit -E 'test(render_util::)'

use skg::assert_metadata_eq;
use skg::org_to_text::viewnode_to_text;
use skg::serve::parse_metadata_sexp::parse_metadata_to_viewnodemd;
use skg::types::misc::{ID, SkgConfig, SkgRepo, SkgRepoName};
use skg::types::viewnode::{ Viewnode, ViewnodeKind, Vognode, UnrestrictedVognode, Editability, ViewnodeStats, default_unrestrictedVognode };
use skg::types::viewnode::PropertyFolder;
use std::collections::HashMap;
use std::path::PathBuf;

#[test]
fn test_viewnode_to_text_no_metadata () {
  let node : Viewnode = Viewnode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind    : ViewnodeKind::Vognode (Vognode::Unrestricted (
      default_unrestrictedVognode ( ID::from ("test"),
                         SkgRepoName::from ("main"),
                         "Test Title" . to_string() ))) };
  let result : String =
    viewnode_to_text ( 1, &node, &SkgConfig::dummyFromSkgRepos (HashMap::new ()) )
    . expect ("UnrestrictedVognode rendering never fails");
  assert_metadata_eq! ( result, "* (skg (node (id test) (repo main))) Test Title\n" ); }

#[test]
fn test_viewnode_to_text_with_body () {
  let t : UnrestrictedVognode = UnrestrictedVognode {
    editability : Editability::Editable {
      body         : Some ( "Test body content" . to_string() ),
      edit_request : None },
    .. default_unrestrictedVognode ( ID::from ("test"),
                          SkgRepoName::from ("main"),
                          "Test Title" . to_string() ) };
  let node : Viewnode = Viewnode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind    : ViewnodeKind::Vognode (Vognode::Unrestricted (t)), };
  let result : String =
    viewnode_to_text ( 2, &node, &SkgConfig::dummyFromSkgRepos (HashMap::new ()) )
    . expect ("UnrestrictedVognode rendering never fails");
  assert_metadata_eq! ( result, "** (skg (node (id test) (repo main))) Test Title\nTest body content\n" ); }

#[test]
fn test_viewnode_to_text_with_metadata () {
  let mut node = Viewnode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind        : ViewnodeKind::PropertyFolder (
      PropertyFolder::Alias) };
  node . folded = true;
  let result : String =
    viewnode_to_text ( 1, &node, &SkgConfig::dummyFromSkgRepos (HashMap::new ()) )
    . expect ("AliasFolder rendering never fails");
  assert_metadata_eq! ( result, "* (skg folded aliasFolder)\n" ); }

#[test]
fn test_viewnode_to_text_with_skgid_metadata () {
  let t : UnrestrictedVognode = UnrestrictedVognode {
    editability : Editability::WriteProtected,
    .. default_unrestrictedVognode ( ID::from ("test123"),
                          SkgRepoName::from ("main"),
                          "Test Title" . to_string() ) };
  let node : Viewnode = Viewnode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind    : ViewnodeKind::Vognode (Vognode::Unrestricted (t)), };
  let result : String =
    viewnode_to_text ( 3, &node, &SkgConfig::dummyFromSkgRepos (HashMap::new ()) )
    . expect ("UnrestrictedVognode rendering never fails");
  assert_metadata_eq! ( result, "*** (skg (node (id test123) (repo main) writeProtected)) Test Title\n" ); }

#[test]
fn skgrepo_name_with_whitespace_is_one_round_trippable_atom () {
  let skgrepo         : SkgRepoName = SkgRepoName::from ("Mr Cheese");
  let mut unrestricted_node : UnrestrictedVognode =
    default_unrestrictedVognode (
      ID::from ("cheese-node"), skgrepo . clone (),
      "Cooking" . to_string () );
  unrestricted_node . viewStats . homeSkgRepoAtBoundary = true;
  let node : Viewnode = Viewnode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind : ViewnodeKind::Vognode (Vognode::Unrestricted (unrestricted_node)) };
  let config = SkgConfig::dummyFromSkgRepos (HashMap::from ([
    ( skgrepo . clone (), SkgRepo {
        name         : skgrepo,
        abbreviation : None,
        path         : PathBuf::from ("cheese"),
        owned        : false } ) ]));
  let rendered : String =
    viewnode_to_text (1, &node, &config)
    . expect ("UnrestrictedVognode rendering never fails");
  assert_eq! (
    rendered,
    "* (skg (node (id cheese-node) (repo \"Mr Cheese\") (viewStats (homeRepoHerald \"⌂:Mr Cheese\")))) Cooking\n" );
  let metadata = parse_metadata_to_viewnodemd (
    rendered
      . split_once (" Cooking")
      . expect ("rendered headline has title") . 0
      . strip_prefix ("* ")
      . expect ("rendered headline has bullet") )
    . expect ("quoted repo should parse");
  assert_eq! (
    metadata . home_skgrepo, Some (SkgRepoName::from ("Mr Cheese")) );
}

#[test]
fn test_metadata_ordering () {
  let t : UnrestrictedVognode = UnrestrictedVognode {
    viewStats : ViewnodeStats {
      cycle             : true,
      .. ViewnodeStats::default() },
    .. default_unrestrictedVognode ( ID::from ("xyz"),
                          SkgRepoName::from ("main"),
                          "Test" . to_string() ) };
  let node : Viewnode = Viewnode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind    : ViewnodeKind::Vognode (Vognode::Unrestricted (t)), };
  let result : String =
    viewnode_to_text ( 1, &node, &SkgConfig::dummyFromSkgRepos (HashMap::new ()) )
    . expect ("UnrestrictedVognode rendering never fails");
  assert_metadata_eq! ( result, "* (skg (node (id xyz) (repo main) (viewStats cycle))) Test\n" ); }

#[test]
fn test_rel_heralds_emitted () {
  // The semantic (rels ...) string round-trips verbatim; a node with
  // none emits no rels atom.
  let mk = | rels : Option<&str> | -> String {
    let t : UnrestrictedVognode = UnrestrictedVognode {
      viewStats : ViewnodeStats {
        rel_heralds : rels . map ( |s| s . to_string () ),
        .. ViewnodeStats::default () },
      .. default_unrestrictedVognode ( ID::from ("n"),
                            SkgRepoName::from ("main"),
                            "N" . to_string () ) };
    let node = Viewnode {
      focused : false, folded : false, body_folded : false,
      kind : ViewnodeKind::Vognode (Vognode::Unrestricted (t)) };
    viewnode_to_text (
      1, &node, &SkgConfig::dummyFromSkgRepos (HashMap::new ()) )
      . unwrap () };
  let with_rels : String =
    mk ( Some ("(rels (contains (in 2 (ancestors 1)) (out 1)) (birth (contains in 1)))") );
  assert! ( with_rels . contains (
    "(rels (contains (in 2 (ancestors 1)) (out 1)) (birth (contains in 1)))" ),
            "rels not emitted verbatim: {}", with_rels );
  let neither : String = mk ( None );
  assert! ( ! neither . contains ("(rels ") );
  assert_metadata_eq! ( neither, "* (skg (node (id n) (repo main))) N\n" ); }
