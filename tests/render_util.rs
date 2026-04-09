// cargo test --test render_util

use skg::org_to_text::viewnode_to_text;
use skg::types::misc::{ID, SkgConfig, SourceName};
use skg::types::viewnode::{ ViewNode, ViewNodeKind, Scaffold, TrueNode, IndefOrDef, GraphNodeStats, NodeContainRels, Birth, ViewNodeStats, viewnode_from_scaffold, default_truenode };
use std::collections::HashMap;

#[test]
fn test_viewnode_to_text_no_metadata () {
  let node : ViewNode = ViewNode {
    focused : false,
    folded  : false,
    kind    : ViewNodeKind::True (
      default_truenode ( ID::from ("test"),
                         SourceName::from ("main"),
                         "Test Title" . to_string() )) };
  let result : String =
    viewnode_to_text ( 1, &node, &SkgConfig::dummyFromSources (HashMap::new ()) )
    . expect ("TrueNode rendering never fails");
  assert_eq! ( result, "* (skg (node (id test) (source main))) Test Title\n" ); }

#[test]
fn test_viewnode_to_text_with_body () {
  let t : TrueNode = TrueNode {
    indef_or_def : IndefOrDef::Definitive {
      body         : Some ( "Test body content" . to_string() ),
      edit_request : None },
    .. default_truenode ( ID::from ("test"),
                          SourceName::from ("main"),
                          "Test Title" . to_string() ) };
  let node : ViewNode = ViewNode {
    focused : false,
    folded  : false,
    kind    : ViewNodeKind::True (t), };
  let result : String =
    viewnode_to_text ( 2, &node, &SkgConfig::dummyFromSources (HashMap::new ()) )
    . expect ("TrueNode rendering never fails");
  assert_eq! ( result, "** (skg (node (id test) (source main))) Test Title\nTest body content\n" ); }

#[test]
fn test_viewnode_to_text_with_metadata () {
  let mut node = viewnode_from_scaffold (Scaffold::AliasCol);
  node . folded = true;
  let result : String =
    viewnode_to_text ( 1, &node, &SkgConfig::dummyFromSources (HashMap::new ()) )
    . expect ("AliasCol rendering never fails");
  assert_eq! ( result, "* (skg folded aliasCol) its aliases\n" ); }

#[test]
fn test_viewnode_to_text_with_id_metadata () {
  let t : TrueNode = TrueNode {
    indef_or_def : IndefOrDef::Indefinitive,
    .. default_truenode ( ID::from ("test123"),
                          SourceName::from ("main"),
                          "Test Title" . to_string() ) };
  let node : ViewNode = ViewNode {
    focused : false,
    folded  : false,
    kind    : ViewNodeKind::True (t), };
  let result : String =
    viewnode_to_text ( 3, &node, &SkgConfig::dummyFromSources (HashMap::new ()) )
    . expect ("TrueNode rendering never fails");
  assert_eq! ( result, "*** (skg (node (id test123) (source main) indefinitive)) Test Title\n" ); }

#[test]
fn test_metadata_ordering () {
  let t : TrueNode = TrueNode {
    viewStats : ViewNodeStats {
      cycle             : true,
      parentIsContainer : false,
      .. ViewNodeStats::default() },
    .. default_truenode ( ID::from ("xyz"),
                          SourceName::from ("main"),
                          "Test" . to_string() ) };
  let node : ViewNode = ViewNode {
    focused : false,
    folded  : false,
    kind    : ViewNodeKind::True (t), };
  let result : String =
    viewnode_to_text ( 1, &node, &SkgConfig::dummyFromSources (HashMap::new ()) )
    . expect ("TrueNode rendering never fails");
  assert_eq! ( result, "* (skg (node (id xyz) (source main) (viewStats cycle))) Test\n" ); }

#[test]
fn test_non_content_birth_forces_container_herald () {
  // A node with containers=1, contents=0 (the defaults).
  // With birth=ContentOf this suppresses the containsHerald.
  // With birth=Independent (or any non-ContentOf) it's forced.
  let make_node = | birth : Birth | -> ViewNode {
    let t : TrueNode = TrueNode {
      birth,
      graphStats : GraphNodeStats {
        containRels : Some ( NodeContainRels {
          containers : 1, contents : 0 } ),
        .. GraphNodeStats::default () },
      .. default_truenode ( ID::from ("n"),
                            SourceName::from ("main"),
                            "N" . to_string () ) };
    ViewNode { focused : false, folded : false,
               kind : ViewNodeKind::True (t) } };
  let cfg : SkgConfig =
    SkgConfig::dummyFromSources ( HashMap::new () );
  let content : String =
    viewnode_to_text ( 1, &make_node (Birth::ContentOf), &cfg )
    . expect ("renders");
  let independent : String =
    viewnode_to_text ( 1, &make_node (Birth::Independent), &cfg )
    . expect ("renders");
  let container_of : String =
    viewnode_to_text ( 1, &make_node (Birth::ContainerOf), &cfg )
    . expect ("renders");
  let links_to : String =
    viewnode_to_text ( 1, &make_node (Birth::LinksTo), &cfg )
    . expect ("renders");
  // ContentOf: herald suppressed (default containers/contents).
  assert! ( ! content . contains ("containsHerald"),
            "ContentOf should suppress default containsHerald" );
  // All non-ContentOf: herald forced, showing "1{".
  assert! ( independent . contains ("(containsHerald 1{)"),
            "Independent should force containsHerald: {}", independent );
  assert! ( container_of . contains ("(containsHerald 1{)"),
            "ContainerOf should force containsHerald: {}", container_of );
  assert! ( links_to . contains ("(containsHerald 1{)"),
            "LinksTo should force containsHerald: {}", links_to ); }
