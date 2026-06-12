// cargo nextest run --test grouped_unit -E 'test(render_util::)'

use skg::org_to_text::viewnode_to_text;
use skg::types::misc::{ID, SkgConfig, SourceName};
use skg::types::viewnode::{ ViewNode, ViewNodeKind, Vognode, TrueNode, IndefOrDef, GraphNodeStats, NodeContainRels, Birth, ParentIs, ViewNodeStats, default_truenode };
use skg::types::viewnode::QualCol;
use std::collections::HashMap;

#[test]
fn test_viewnode_to_text_no_metadata () {
  let node : ViewNode = ViewNode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind    : ViewNodeKind::Vognode (Vognode::Active (
      default_truenode ( ID::from ("test"),
                         SourceName::from ("main"),
                         "Test Title" . to_string() ))) };
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
    kind    : ViewNodeKind::Vognode (Vognode::Active (t)), };
  let result : String =
    viewnode_to_text ( 2, &node, &SkgConfig::dummyFromSources (HashMap::new ()) )
    . expect ("TrueNode rendering never fails");
  assert_eq! ( result, "** (skg (node (id test) (source main))) Test Title\nTest body content\n" ); }

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
  assert_eq! ( result, "* (skg folded aliasCol)\n" ); }

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
    kind    : ViewNodeKind::Vognode (Vognode::Active (t)), };
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
    kind    : ViewNodeKind::Vognode (Vognode::Active (t)), };
  let result : String =
    viewnode_to_text ( 1, &node, &SkgConfig::dummyFromSources (HashMap::new ()) )
    . expect ("TrueNode rendering never fails");
  assert_eq! ( result, "* (skg (node (id xyz) (source main) (viewStats cycle))) Test\n" ); }

#[test]
fn test_birth_affects_container_count_emission () {
  // A node with containers=1, contents=0.
  // Birth::Unremarkable itself is omitted from metadata. Non-default
  // birth values are emitted, and they also override the parentIs-specific
  // suppression rule for `(graphStats (containers N))`:
  //   - Affected: hide when containers == 1.
  //   - Independent/Absent: hide when containers == 0.
  //   - ContainsParent/LinksToParent birth: always show.
  // The client builds the compound herald from the raw atoms; the
  // `(containsHerald ...)` atom is not emitted server-side.
  let make_node = | parentIs : ParentIs, birth : Birth | -> ViewNode {
    let t : TrueNode = TrueNode {
      parentIs,
      birth,
      graphStats : GraphNodeStats {
        containRels : Some ( NodeContainRels {
          containers : 1, contents : 0 } ),
        .. GraphNodeStats::default () },
      .. default_truenode ( ID::from ("n"),
                            SourceName::from ("main"),
                            "N" . to_string () ) };
    ViewNode { focused : false, folded : false, body_folded : false,
               kind : ViewNodeKind::Vognode (Vognode::Active (t)) } };
  let cfg : SkgConfig =
    SkgConfig::dummyFromSources ( HashMap::new () );
  let content      : String = viewnode_to_text (
    1, &make_node (ParentIs::Affected, Birth::Unremarkable), &cfg ) . unwrap ();
  let independent  : String = viewnode_to_text (
    1, &make_node (ParentIs::Independent, Birth::Unremarkable), &cfg ) . unwrap ();
  let container_of : String = viewnode_to_text (
    1, &make_node (ParentIs::Independent, Birth::ContainsParent), &cfg ) . unwrap ();
  let links_to     : String = viewnode_to_text (
    1, &make_node (ParentIs::Independent, Birth::LinksToParent), &cfg ) . unwrap ();
  // Affected with containers=1: hide.
  assert! ( ! content . contains ("(containers 1)"),
            "ParentIs=Affected should suppress containers=1: {}", content );
  // Independent with containers=1: 1 != 0 so show.
  assert! ( independent . contains ("(containers 1)"),
            "ParentIs=Independent should show containers>=1: {}", independent );
  // Non-default birth provenance: always show.
  assert! ( container_of . contains ("(containers 1)"),
            "ContainsParent should always show containers: {}", container_of );
  assert! ( links_to . contains ("(containers 1)"),
            "LinksToParent should always show containers: {}", links_to );
  // No `containsHerald` atom on any of them.
  for (name, s) in [("content", &content), ("independent", &independent),
                    ("container_of", &container_of), ("links_to", &links_to)] {
    assert! ( ! s . contains ("containsHerald"),
              "{}: server should not emit containsHerald; got {}", name, s ); } }
