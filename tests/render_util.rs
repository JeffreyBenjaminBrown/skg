// cargo test --test render_util

use skg::org_to_text::viewnode_to_text;
use skg::types::misc::{ID, SkgConfig, SourceName};
use skg::types::viewnode::{ ViewNode, ViewNodeKind, Scaffold, TrueNode, IndefOrDef, GraphNodeStats, NodeContainRels, ParentIs, ViewNodeStats, viewnode_from_scaffold, default_truenode };
use std::collections::HashMap;

#[test]
fn test_viewnode_to_text_no_metadata () {
  let node : ViewNode = ViewNode {
    focused     : false,
    folded      : false,
    body_folded : false,
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
    focused     : false,
    folded      : false,
    body_folded : false,
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
    focused     : false,
    folded      : false,
    body_folded : false,
    kind    : ViewNodeKind::True (t), };
  let result : String =
    viewnode_to_text ( 3, &node, &SkgConfig::dummyFromSources (HashMap::new ()) )
    . expect ("TrueNode rendering never fails");
  assert_eq! ( result, "*** (skg (node (id test123) (source main) indef)) Test Title\n" ); }

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
    focused     : false,
    folded      : false,
    body_folded : false,
    kind    : ViewNodeKind::True (t), };
  let result : String =
    viewnode_to_text ( 1, &node, &SkgConfig::dummyFromSources (HashMap::new ()) )
    . expect ("TrueNode rendering never fails");
  assert_eq! ( result, "* (skg (node (id xyz) (source main) (viewStats cycle))) Test\n" ); }

#[test]
fn test_birth_affects_container_count_emission () {
  // A node with containers=1, contents=0.
  // The server emits `(containers N)` only when the parentIs-specific
  // suppression rule doesn't apply:
  //   - Container:   hide when containers == 1.
  //   - Independent: hide when containers == 0.
  //   - Content / LinkTarget: always show.
  // The client builds the compound herald from the raw atoms; the
  // `(containsHerald ...)` atom is not emitted server-side.
  let make_node = | parentIs : ParentIs | -> ViewNode {
    let t : TrueNode = TrueNode {
      parentIs,
      graphStats : GraphNodeStats {
        containRels : Some ( NodeContainRels {
          containers : 1, contents : 0 } ),
        .. GraphNodeStats::default () },
      .. default_truenode ( ID::from ("n"),
                            SourceName::from ("main"),
                            "N" . to_string () ) };
    ViewNode { focused : false, folded : false, body_folded : false,
               kind : ViewNodeKind::True (t) } };
  let cfg : SkgConfig =
    SkgConfig::dummyFromSources ( HashMap::new () );
  let content      : String = viewnode_to_text (
    1, &make_node (ParentIs::Container),   &cfg ) . unwrap ();
  let independent  : String = viewnode_to_text (
    1, &make_node (ParentIs::Independent), &cfg ) . unwrap ();
  let container_of : String = viewnode_to_text (
    1, &make_node (ParentIs::Content), &cfg ) . unwrap ();
  let links_to     : String = viewnode_to_text (
    1, &make_node (ParentIs::LinkTarget),     &cfg ) . unwrap ();
  // Container with containers=1: hide.
  assert! ( ! content . contains ("(containers 1)"),
            "Container should suppress containers=1: {}", content );
  // Independent with containers=1: 1 != 0 so show.
  assert! ( independent . contains ("(containers 1)"),
            "Independent should show containers>=1: {}", independent );
  // Content / LinkTarget: always show.
  assert! ( container_of . contains ("(containers 1)"),
            "Content should always show containers: {}", container_of );
  assert! ( links_to . contains ("(containers 1)"),
            "LinkTarget should always show containers: {}", links_to );
  // No `containsHerald` atom on any of them.
  for (name, s) in [("content", &content), ("independent", &independent),
                    ("container_of", &container_of), ("links_to", &links_to)] {
    assert! ( ! s . contains ("containsHerald"),
              "{}: server should not emit containsHerald; got {}", name, s ); } }
