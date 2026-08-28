//! Property suite for the telescope fold/unfold pair. The
//! load-bearing laws (5_plan.org, section-format-and-fold):
//! - fold(unfold(x)) == x for every leveled list ("round-trip");
//! - unfold(fold(sections)) is idempotent from the first application
//!   (unfold output is canonical);
//! - every member's level survives both directions;
//! - dangling/duplicate-anchor junk folds totally and
//!   deterministically;
//! - the silent-leak guard: no member ever changes level.

use super::fold::{FoldedNode, fold_sections};
use super::types::{FoldWarning, ListItem, SectionSlices};
use super::unfold::{UnfoldInput, unfold_node};
use crate::types::misc::{
  ID, MemberAtSource, SkgConfig, SkgfileSource, SourceName,
};

use proptest::prelude::*;
use std::collections::HashMap;
use std::path::PathBuf;

/// The test privacy order: S0 most public .. S3 most private.
fn level_universe () -> Vec<SourceName> {
  (0..4) . map ( |i| SourceName ( format! ("S{}", i) ))
    . collect () }

fn telescope_config () -> SkgConfig {
  let sources : HashMap<SourceName, SkgfileSource> =
    level_universe () . into_iter ()
    . map ( |source| (
      source . clone (),
      SkgfileSource {
        name         : source . clone (),
        abbreviation : None,
        path         : PathBuf::from (format! ("owned/{}", source)),
        user_owns_it : true,
      } ))
    . collect ();
  let mut config : SkgConfig = SkgConfig::dummyFromSources (sources);
  config . source_order = level_universe ();
  config }

fn unfold_sections (
  input : &UnfoldInput,
) -> Vec<(SourceName, SectionSlices)> {
  unfold_node (input, &telescope_config ()) . unwrap ()
    . into_sections () . into_iter ()
    . map ( |(source, node_fs)|
      (source, node_fs . into_section_slices ()) )
    . collect () }

fn identity_resolve (
  id : &ID,
) -> ID {
  id . clone () }

/// An arbitrary leveled list with UNIQUE members: up to N members,
/// each at a random level of the universe. Uniqueness matters
/// because the fold dedups (with warnings), which round-trip inputs
/// must not trigger.
fn arb_leveled_list (
  max_len : usize,
) -> impl Strategy<Value = Vec<MemberAtSource<ID>>> {
  proptest::collection::vec ( 0usize..4, 0..max_len )
    . prop_map ( |levels| {
      let universe : Vec<SourceName> = level_universe ();
      levels . into_iter () . enumerate ()
        . map ( |(i, l)| MemberAtSource::at_source (
          universe [l] . clone (),
          ID ( format! ("id{}", i) )))
        . collect () } ) }

/// Wrap ordered leveled lists (and nothing else) into an
/// UnfoldInput-shaped FoldedNode for the round-trip tests.
fn folded_from_lists (
  home     : &SourceName,
  contains : Vec<MemberAtSource<ID>>,
  subs     : Vec<MemberAtSource<ID>>,
  hides    : Vec<MemberAtSource<ID>>,
) -> FoldedNode {
  FoldedNode {
    title                        : Some ("t" . to_string ()),
    body                         : None,
    home                         : Some ( home . clone () ),
    aliases                      : None,
    contains,
    subscribes_to                :
      if subs . is_empty () { None } else { Some (subs) },
    hides_from_its_subscriptions :
      if hides . is_empty () { None } else { Some (hides) },
    overrides_view_of            : None, }}

fn unfold_then_fold (
  folded : &FoldedNode,
) -> (FoldedNode, Vec<FoldWarning>) {
  let home : SourceName =
    folded . home . clone () . expect ("home set");
  let sections : Vec<(SourceName, SectionSlices)> =
    unfold_sections (
      & UnfoldInput {
        pid      : &ID::new ("p"),
        extra_ids : &[],
        misc      : &[],
        title    : folded . title . as_deref (),
        body     : folded . body . as_deref (),
        home     : &home,
        aliases  : folded . aliases . as_deref ()
                   . unwrap_or (&[]),
        contains : &folded . contains,
        subscribes_to :
          folded . subscribes_to . as_deref () . unwrap_or (&[]),
        hides_from_its_subscriptions :
          folded . hides_from_its_subscriptions . as_deref ()
          . unwrap_or (&[]),
        overrides_view_of :
          folded . overrides_view_of . as_deref ()
          . unwrap_or (&[]), } );
  fold_sections ( &sections, &identity_resolve ) }

proptest! {
  #![proptest_config (ProptestConfig::with_cases (512))]

  #[test]
  fn round_trip_ordered_and_unordered (
    contains in arb_leveled_list (12),
    subs_raw in arb_leveled_list (6),
    hides_raw in arb_leveled_list (6),
  ) {
    // distinct id spaces so the three lists cannot collide
    let subs : Vec<MemberAtSource<ID>> =
      subs_raw . into_iter ()
      . map ( |m| MemberAtSource::at_source (
        m . source, ID ( format! ("s-{}", m . member . 0 ))))
      . collect ();
    let hides : Vec<MemberAtSource<ID>> =
      hides_raw . into_iter ()
      . map ( |m| MemberAtSource::at_source (
        m . source, ID ( format! ("h-{}", m . member . 0 ))))
      . collect ();
    let home : SourceName = SourceName::from ("S0");
    let folded : FoldedNode =
      folded_from_lists (&home, contains, subs, hides);
    let (refolded, warnings) = unfold_then_fold (&folded);
    // The one asymmetry: fold cannot learn a home the unfold did not
    // write scalars into; everything else must round-trip exactly.
    prop_assert_eq! ( &refolded . contains, &folded . contains );
    prop_assert_eq! ( &refolded . subscribes_to,
                      &folded . subscribes_to );
    { // Unordered relations have no order to preserve: sections
      // cannot express cross-level interleavings without anchors,
      // which unordered relations deliberately lack, so the fold's
      // output order is CANONICAL (level-major). The law is
      // set-equality with levels intact.
      let sort = |v : Option<&Vec<MemberAtSource<ID>>>|
      -> Vec<MemberAtSource<ID>> {
        let mut v : Vec<MemberAtSource<ID>> =
          v . cloned () . unwrap_or_default ();
        v . sort_by ( |a, b| a . member . cmp ( &b . member ));
        v };
      prop_assert_eq! (
        sort ( refolded . hides_from_its_subscriptions . as_ref () ),
        sort ( folded . hides_from_its_subscriptions . as_ref () )); }
    prop_assert_eq! ( refolded . title . as_deref (), Some ("t") );
    prop_assert_eq! ( refolded . home, Some (home) );
    prop_assert! ( warnings . is_empty (),
                   "round-trip inputs must not warn: {:?}", warnings );
  }

  #[test]
  fn unfold_is_canonical (
    contains in arb_leveled_list (12),
  ) {
    // unfold . fold . unfold == unfold  (sections are a normal form)
    let home : SourceName = SourceName::from ("S0");
    let folded : FoldedNode = folded_from_lists (
      &home, contains, Vec::new (), Vec::new ());
    let (refolded, _) = unfold_then_fold (&folded);
    let sections_once : Vec<(SourceName, SectionSlices)> =
      unfold_sections (
        & UnfoldInput {
          pid : &ID::new ("p"), extra_ids : &[], misc : &[],
          title : folded . title . as_deref (),
          body : None, home : &home,
          aliases : &[], contains : &folded . contains,
          subscribes_to : &[],
          hides_from_its_subscriptions : &[],
          overrides_view_of : &[], } );
    let sections_twice : Vec<(SourceName, SectionSlices)> =
      unfold_sections (
        & UnfoldInput {
          pid : &ID::new ("p"), extra_ids : &[], misc : &[],
          title : refolded . title . as_deref (),
          body : None, home : &home,
          aliases : &[], contains : &refolded . contains,
          subscribes_to : &[],
          hides_from_its_subscriptions : &[],
          overrides_view_of : &[], } );
    prop_assert_eq! (sections_once, sections_twice);
  }

  #[test]
  fn no_member_ever_changes_level ( // the silent-leak guard
    contains in arb_leveled_list (12),
  ) {
    let home : SourceName = SourceName::from ("S0");
    let folded : FoldedNode = folded_from_lists (
      &home, contains . clone (), Vec::new (), Vec::new ());
    let (refolded, _) = unfold_then_fold (&folded);
    for m in &contains {
      let found : Option<&MemberAtSource<ID>> =
        refolded . contains . iter ()
        . find ( |n| n . member == m . member );
      prop_assert_eq! (
        found . map ( |n| &n . source ), Some ( &m . source ),
        "member {:?} changed level", m . member );
    }
  }
}

#[test]
fn dangling_anchor_attaches_after_preceding_run_with_warning (
) {
  // S1's section: prepend [p], run (a,[x]), then a run whose anchor
  // is unknown -- its members must follow the PRECEDING run, warned.
  let sections : Vec<(SourceName, SectionSlices)> = vec! [
    ( SourceName::from ("S0"),
      SectionSlices {
        title : Some ("t" . to_string ()),
        contains : Some ( vec! [
          ListItem::Member ( ID::new ("a") ),
          ListItem::Member ( ID::new ("b") ) ] ),
        .. SectionSlices::default () } ),
    ( SourceName::from ("S1"),
      SectionSlices {
        contains : Some ( vec! [
          ListItem::Member ( ID::new ("p") ),
          ListItem::Anchor { anchor : ID::new ("a") },
          ListItem::Member ( ID::new ("x") ),
          ListItem::Anchor { anchor : ID::new ("GONE") },
          ListItem::Member ( ID::new ("y") ) ] ),
        .. SectionSlices::default () } ) ];
  let (folded, warnings) =
    fold_sections ( &sections, &identity_resolve );
  let got : Vec<&str> =
    folded . contains . iter ()
    . map ( |m| m . member . 0 . as_str () )
    . collect ();
  assert_eq! ( got, vec! ["p", "a", "x", "y", "b"],
               "y follows its preceding run (a,[x])" );
  assert! ( warnings . iter () . any ( |w| matches! (
    w, FoldWarning::DanglingAnchor { anchor } if anchor . 0 == "GONE" )),
    "dangling anchor warned: {:?}", warnings );
}

#[test]
fn dangling_first_run_joins_the_prepend (
) {
  let sections : Vec<(SourceName, SectionSlices)> = vec! [
    ( SourceName::from ("S0"),
      SectionSlices {
        title : Some ("t" . to_string ()),
        contains : Some ( vec! [
          ListItem::Member ( ID::new ("a") ) ] ),
        .. SectionSlices::default () } ),
    ( SourceName::from ("S1"),
      SectionSlices {
        contains : Some ( vec! [
          ListItem::Anchor { anchor : ID::new ("GONE") },
          ListItem::Member ( ID::new ("y") ) ] ),
        .. SectionSlices::default () } ) ];
  let (folded, warnings) =
    fold_sections ( &sections, &identity_resolve );
  let got : Vec<&str> =
    folded . contains . iter ()
    . map ( |m| m . member . 0 . as_str () )
    . collect ();
  assert_eq! ( got, vec! ["y", "a"],
               "the orphaned first run joins the prepend" );
  assert_eq! ( warnings . len (), 1, "{:?}", warnings );
}

#[test]
fn duplicate_anchors_concatenate_in_file_order (
) {
  let sections : Vec<(SourceName, SectionSlices)> = vec! [
    ( SourceName::from ("S0"),
      SectionSlices {
        title : Some ("t" . to_string ()),
        contains : Some ( vec! [
          ListItem::Member ( ID::new ("a") ) ] ),
        .. SectionSlices::default () } ),
    ( SourceName::from ("S1"),
      SectionSlices {
        contains : Some ( vec! [
          ListItem::Anchor { anchor : ID::new ("a") },
          ListItem::Member ( ID::new ("x") ),
          ListItem::Anchor { anchor : ID::new ("a") },
          ListItem::Member ( ID::new ("z") ) ] ),
        .. SectionSlices::default () } ) ];
  let (folded, _) =
    fold_sections ( &sections, &identity_resolve );
  let got : Vec<&str> =
    folded . contains . iter ()
    . map ( |m| m . member . 0 . as_str () )
    . collect ();
  assert_eq! ( got, vec! ["a", "x", "z"] );
}

#[test]
fn anchors_resolve_through_the_resolver ( // extra-id safety
) {
  let resolve = |id : &ID| -> ID {
    // "a-alias" is an extra id of "a"
    if id . 0 == "a-alias" { ID::new ("a") } else { id . clone () }};
  let sections : Vec<(SourceName, SectionSlices)> = vec! [
    ( SourceName::from ("S0"),
      SectionSlices {
        title : Some ("t" . to_string ()),
        contains : Some ( vec! [
          ListItem::Member ( ID::new ("a") ) ] ),
        .. SectionSlices::default () } ),
    ( SourceName::from ("S1"),
      SectionSlices {
        contains : Some ( vec! [
          ListItem::Anchor { anchor : ID::new ("a-alias") },
          ListItem::Member ( ID::new ("y") ) ] ),
        .. SectionSlices::default () } ) ];
  let (folded, warnings) =
    fold_sections ( &sections, &resolve );
  let got : Vec<&str> =
    folded . contains . iter ()
    . map ( |m| m . member . 0 . as_str () )
    . collect ();
  assert_eq! ( got, vec! ["a", "y"] );
  assert! ( warnings . is_empty (), "{:?}", warnings );
}

#[test]
fn the_home_is_the_most_public_section_titled_or_not (
) { // Jeff's rule: a node's text always lives in its most public
    // section. So the home is the most public SECTION, not the most
    // public section bearing a title; a titleless one above the
    // title is a violation to report, not a shape to search past.
  let titleless_public : (SourceName, SectionSlices) =
    ( SourceName::from ("public"),
      SectionSlices { contains : Some ( vec! [
        ListItem::Member ( ID::new ("C") ) ] ),
        .. SectionSlices::default () } );
  let titled_private : (SourceName, SectionSlices) =
    ( SourceName::from ("private"),
      SectionSlices { title : Some ( "N" . to_string () ),
                      body  : Some ( "secret" . to_string () ),
                      .. SectionSlices::default () } );
  let (folded, warnings) : (FoldedNode, Vec<FoldWarning>) =
    fold_sections (
      & [ titleless_public, titled_private ],
      & identity_resolve );
  assert_eq! ( folded . home,
               Some ( SourceName::from ("public") ),
               "the home is the most public section" );
  assert_eq! ( folded . title, Some ( "N" . to_string () ),
               "the title still folds in, from wherever it sits" );
  assert! ( warnings . contains ( & FoldWarning::TitleBelowHome {
              home     : SourceName::from ("public"),
              title_at : SourceName::from ("private"), } ),
            "the shape is reported: {:?}", warnings );
}

#[test]
fn a_titled_most_public_section_raises_no_title_warning (
) {
  let titled_public : (SourceName, SectionSlices) =
    ( SourceName::from ("public"),
      SectionSlices { title : Some ( "N" . to_string () ),
                      .. SectionSlices::default () } );
  let titleless_private : (SourceName, SectionSlices) =
    ( SourceName::from ("private"),
      SectionSlices { contains : Some ( vec! [
        ListItem::Member ( ID::new ("C") ) ] ),
        .. SectionSlices::default () } );
  let (folded, warnings) : (FoldedNode, Vec<FoldWarning>) =
    fold_sections (
      & [ titled_public, titleless_private ],
      & identity_resolve );
  assert_eq! ( folded . home,
               Some ( SourceName::from ("public") ) );
  assert! ( ! warnings . iter () . any ( |w| matches! (
              w, FoldWarning::TitleBelowHome { .. }
                 | FoldWarning::NonHomeTitle { .. }
                 | FoldWarning::MissingTitle )),
            "the ordinary telescope shape warns about nothing: {:?}",
            warnings );
}
