// cargo nextest run --test grouped_unit -E 'test(render_util::)'

use skg::assert_metadata_eq;
use skg::org_to_text::viewnode_to_text;
use skg::types::misc::{ID, SkgConfig, SourceName};
use skg::types::viewnode::{ ViewNode, ViewNodeKind, Vognode, ActiveNode, IndefOrDef, ViewNodeStats, default_activeNode };
use skg::types::viewnode::QualCol;
use std::collections::HashMap;

#[test]
fn test_viewnode_to_text_no_metadata () {
  let node : ViewNode = ViewNode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind    : ViewNodeKind::Vognode (Vognode::Active (
      default_activeNode ( ID::from ("test"),
                         SourceName::from ("main"),
                         "Test Title" . to_string() ))) };
  let result : String =
    viewnode_to_text ( 1, &node, &SkgConfig::dummyFromSources (HashMap::new ()) )
    . expect ("ActiveNode rendering never fails");
  assert_metadata_eq! ( result, "* (skg (node (id test) (source main))) Test Title\n" ); }

#[test]
fn test_viewnode_to_text_with_body () {
  let t : ActiveNode = ActiveNode {
    indef_or_def : IndefOrDef::Definitive {
      body         : Some ( "Test body content" . to_string() ),
      edit_request : None },
    .. default_activeNode ( ID::from ("test"),
                          SourceName::from ("main"),
                          "Test Title" . to_string() ) };
  let node : ViewNode = ViewNode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind    : ViewNodeKind::Vognode (Vognode::Active (t)), };
  let result : String =
    viewnode_to_text ( 2, &node, &SkgConfig::dummyFromSources (HashMap::new ()) )
    . expect ("ActiveNode rendering never fails");
  assert_metadata_eq! ( result, "** (skg (node (id test) (source main))) Test Title\nTest body content\n" ); }

#[test]
fn test_viewnode_to_text_with_metadata () {
  let mut node = ViewNode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind        : ViewNodeKind::QualCol (
      QualCol::Alias) };
  node . folded = true;
  let result : String =
    viewnode_to_text ( 1, &node, &SkgConfig::dummyFromSources (HashMap::new ()) )
    . expect ("AliasCol rendering never fails");
  assert_metadata_eq! ( result, "* (skg folded aliasCol)\n" ); }

#[test]
fn test_viewnode_to_text_with_id_metadata () {
  let t : ActiveNode = ActiveNode {
    indef_or_def : IndefOrDef::Indefinitive,
    .. default_activeNode ( ID::from ("test123"),
                          SourceName::from ("main"),
                          "Test Title" . to_string() ) };
  let node : ViewNode = ViewNode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind    : ViewNodeKind::Vognode (Vognode::Active (t)), };
  let result : String =
    viewnode_to_text ( 3, &node, &SkgConfig::dummyFromSources (HashMap::new ()) )
    . expect ("ActiveNode rendering never fails");
  assert_metadata_eq! ( result, "*** (skg (node (id test123) (source main) indef)) Test Title\n" ); }

#[test]
fn test_metadata_ordering () {
  let t : ActiveNode = ActiveNode {
    viewStats : ViewNodeStats {
      cycle             : true,
      .. ViewNodeStats::default() },
    .. default_activeNode ( ID::from ("xyz"),
                          SourceName::from ("main"),
                          "Test" . to_string() ) };
  let node : ViewNode = ViewNode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind    : ViewNodeKind::Vognode (Vognode::Active (t)), };
  let result : String =
    viewnode_to_text ( 1, &node, &SkgConfig::dummyFromSources (HashMap::new ()) )
    . expect ("ActiveNode rendering never fails");
  assert_metadata_eq! ( result, "* (skg (node (id xyz) (source main) (viewStats cycle))) Test\n" ); }

#[test]
fn test_rel_heralds_emitted () {
  // The semantic (rels ...) string round-trips verbatim; a node with
  // none emits no rels atom.
  let mk = | rels : Option<&str> | -> String {
    let t : ActiveNode = ActiveNode {
      viewStats : ViewNodeStats {
        rel_heralds : rels . map ( |s| s . to_string () ),
        .. ViewNodeStats::default () },
      .. default_activeNode ( ID::from ("n"),
                            SourceName::from ("main"),
                            "N" . to_string () ) };
    let node = ViewNode {
      focused : false, folded : false, body_folded : false,
      kind : ViewNodeKind::Vognode (Vognode::Active (t)) };
    viewnode_to_text (
      1, &node, &SkgConfig::dummyFromSources (HashMap::new ()) )
      . unwrap () };
  let with_rels : String =
    mk ( Some ("(rels (contains (in 2 (ancestors 1)) (out 1)) (birth contains))") );
  assert! ( with_rels . contains (
    "(rels (contains (in 2 (ancestors 1)) (out 1)) (birth contains))" ),
            "rels not emitted verbatim: {}", with_rels );
  let neither : String = mk ( None );
  assert! ( ! neither . contains ("(rels ") );
  assert_metadata_eq! ( neither, "* (skg (node (id n) (source main))) N\n" ); }
