// cargo nextest run --test grouped_unit -E 'test(render_util::)'

use skg::assert_metadata_eq;
use skg::org_to_text::viewnode_to_text;
use skg::serve::parse_metadata_sexp::parse_metadata_to_viewnodemd;
use skg::types::misc::{ID, SkgConfig, SkgfileRepo, RepoName};
use skg::types::viewnode::{ Viewnode, ViewnodeKind, Vognode, ActiveNode, Editability, ViewnodeStats, default_activeNode };
use skg::types::viewnode::PropertyFolder;
use std::collections::HashMap;
use std::path::PathBuf;

#[test]
fn test_viewnode_to_text_no_metadata () {
  let node : Viewnode = Viewnode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind    : ViewnodeKind::Vognode (Vognode::Active (
      default_activeNode ( ID::from ("test"),
                         RepoName::from ("main"),
                         "Test Title" . to_string() ))) };
  let result : String =
    viewnode_to_text ( 1, &node, &SkgConfig::dummyFromRepos (HashMap::new ()) )
    . expect ("ActiveNode rendering never fails");
  assert_metadata_eq! ( result, "* (skg (node (id test) (repo main))) Test Title\n" ); }

#[test]
fn test_viewnode_to_text_with_body () {
  let t : ActiveNode = ActiveNode {
    editability : Editability::Definitive {
      body         : Some ( "Test body content" . to_string() ),
      edit_request : None },
    .. default_activeNode ( ID::from ("test"),
                          RepoName::from ("main"),
                          "Test Title" . to_string() ) };
  let node : Viewnode = Viewnode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind    : ViewnodeKind::Vognode (Vognode::Active (t)), };
  let result : String =
    viewnode_to_text ( 2, &node, &SkgConfig::dummyFromRepos (HashMap::new ()) )
    . expect ("ActiveNode rendering never fails");
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
    viewnode_to_text ( 1, &node, &SkgConfig::dummyFromRepos (HashMap::new ()) )
    . expect ("AliasFolder rendering never fails");
  assert_metadata_eq! ( result, "* (skg folded aliasFolder)\n" ); }

#[test]
fn test_viewnode_to_text_with_id_metadata () {
  let t : ActiveNode = ActiveNode {
    editability : Editability::WriteProtected,
    .. default_activeNode ( ID::from ("test123"),
                          RepoName::from ("main"),
                          "Test Title" . to_string() ) };
  let node : Viewnode = Viewnode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind    : ViewnodeKind::Vognode (Vognode::Active (t)), };
  let result : String =
    viewnode_to_text ( 3, &node, &SkgConfig::dummyFromRepos (HashMap::new ()) )
    . expect ("ActiveNode rendering never fails");
  assert_metadata_eq! ( result, "*** (skg (node (id test123) (repo main) writeProtected)) Test Title\n" ); }

#[test]
fn repo_name_with_whitespace_is_one_round_trippable_atom () {
  let repo : RepoName = RepoName::from ("Mr Cheese");
  let mut active_node : ActiveNode =
    default_activeNode (
      ID::from ("cheese-node"), repo . clone (),
      "Cooking" . to_string () );
  active_node . viewStats . homeRepoAtBoundary = true;
  let node : Viewnode = Viewnode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind : ViewnodeKind::Vognode (Vognode::Active (active_node)) };
  let config = SkgConfig::dummyFromRepos (HashMap::from ([
    ( repo . clone (), SkgfileRepo {
        name         : repo,
        abbreviation : None,
        path         : PathBuf::from ("cheese"),
        user_owns_it : false } ) ]));
  let rendered : String =
    viewnode_to_text (1, &node, &config)
    . expect ("ActiveNode rendering never fails");
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
    metadata . home_repo, Some (RepoName::from ("Mr Cheese")) );
}

#[test]
fn test_metadata_ordering () {
  let t : ActiveNode = ActiveNode {
    viewStats : ViewnodeStats {
      cycle             : true,
      .. ViewnodeStats::default() },
    .. default_activeNode ( ID::from ("xyz"),
                          RepoName::from ("main"),
                          "Test" . to_string() ) };
  let node : Viewnode = Viewnode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind    : ViewnodeKind::Vognode (Vognode::Active (t)), };
  let result : String =
    viewnode_to_text ( 1, &node, &SkgConfig::dummyFromRepos (HashMap::new ()) )
    . expect ("ActiveNode rendering never fails");
  assert_metadata_eq! ( result, "* (skg (node (id xyz) (repo main) (viewStats cycle))) Test\n" ); }

#[test]
fn test_rel_heralds_emitted () {
  // The semantic (rels ...) string round-trips verbatim; a node with
  // none emits no rels atom.
  let mk = | rels : Option<&str> | -> String {
    let t : ActiveNode = ActiveNode {
      viewStats : ViewnodeStats {
        rel_heralds : rels . map ( |s| s . to_string () ),
        .. ViewnodeStats::default () },
      .. default_activeNode ( ID::from ("n"),
                            RepoName::from ("main"),
                            "N" . to_string () ) };
    let node = Viewnode {
      focused : false, folded : false, body_folded : false,
      kind : ViewnodeKind::Vognode (Vognode::Active (t)) };
    viewnode_to_text (
      1, &node, &SkgConfig::dummyFromRepos (HashMap::new ()) )
      . unwrap () };
  let with_rels : String =
    mk ( Some ("(rels (contains (in 2 (ancestors 1)) (out 1)) (birth contains))") );
  assert! ( with_rels . contains (
    "(rels (contains (in 2 (ancestors 1)) (out 1)) (birth contains))" ),
            "rels not emitted verbatim: {}", with_rels );
  let neither : String = mk ( None );
  assert! ( ! neither . contains ("(rels ") );
  assert_metadata_eq! ( neither, "* (skg (node (id n) (repo main))) N\n" ); }
