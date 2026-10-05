//! Flag suite for the telescope composition/decompose pair. The
//! load-bearing laws (5_plan.org, section-format-and-fold):
//! - compose(decompose(x)) == x for every list of relation partners ("round-trip");
//! - decompose(compose(sections)) is idempotent from the first application
//!   (decompose output is canonical);
//! - every member's skgrepo survives both directions;
//! - dangling/duplicate-anchor junk composes totally and
//!   deterministically;
//! - the silent-leak guard: no member ever changes skgrepo.

use super::compose::{ComposedNode, compose_sections, graphnode_from_composition};
use super::types::{CompositionWarning, ListItem, SectionSlices, Telescope};
use super::decompose::{DecompositionInput, decompose_node};
use crate::types::misc::{
  ID, RelPartner, SkgConfig, Skgrepo, SkgrepoName,
};
use crate::types::nodes::complete::Flag;

use proptest::prelude::*;
use std::collections::HashMap;
use std::path::PathBuf;

/// The test privacy order: S0 most public .. S3 most private.
fn skgrepo_universe () -> Vec<SkgrepoName> {
  (0..4) . map ( |i| SkgrepoName ( format! ("S{}", i) ))
    . collect () }

fn telescope_config () -> SkgConfig {
  let skgrepos : HashMap<SkgrepoName, Skgrepo> =
    skgrepo_universe () . into_iter ()
    . map ( |skgrepo| (
      skgrepo . clone (),
      Skgrepo {
        name         : skgrepo . clone (),
        abbreviation : None,
        path         : PathBuf::from (format! ("owned/{}", skgrepo)),
        owned        : true,
      } ))
    . collect ();
  let mut config : SkgConfig = SkgConfig::dummyFromSkgrepos (skgrepos);
  config . skgrepo_order = skgrepo_universe ();
  config }

fn decompose_sections (
  input : &DecompositionInput,
) -> Vec<(SkgrepoName, SectionSlices)> {
  decompose_node (input, &telescope_config ()) . unwrap ()
    . into_sections () . into_iter ()
    . map ( |(skgrepo, node_fs)|
      (skgrepo, node_fs . into_section_slices ()) )
    . collect () }

fn identity_resolve (
  skgid : &ID,
) -> ID {
  skgid . clone () }

/// An arbitrary list of relation partners with UNIQUE members: up to N
/// members, each at a random skgrepo in the universe. Uniqueness matters
/// because the composition dedups (with warnings), which round-trip inputs
/// must not trigger.
fn arb_rel_partners (
  max_len : usize,
) -> impl Strategy<Value = Vec<RelPartner<ID>>> {
  proptest::collection::vec ( 0usize..4, 0..max_len )
    . prop_map ( |skgrepos| {
      let universe : Vec<SkgrepoName> = skgrepo_universe ();
      skgrepos . into_iter () . enumerate ()
        . map ( |(i, l)| RelPartner::at_relRepo (
          universe [l] . clone (),
          ID ( format! ("id{}", i) )))
        . collect () } ) }

/// Wrap ordered lists of relation partners (and nothing else) into an
/// DecompositionInput-shaped ComposedNode for the round-trip tests.
fn composed_from_lists (
  home     : &SkgrepoName,
  contains : Vec<RelPartner<ID>>,
  subs     : Vec<RelPartner<ID>>,
  hides    : Vec<RelPartner<ID>>,
) -> ComposedNode {
  ComposedNode {
    title                        : Some ("t" . to_string ()),
    title_skgrepo                : Some (home . clone ()),
    body                         : None,
    body_skgrepo                 : None,
    home                         : Some ( home . clone () ),
    aliases                      : None,
    contains,
    subscribesTo                 :
      if subs . is_empty () { None } else { Some (subs) },
    hidesFromSubs                :
      if hides . is_empty () { None } else { Some (hides) },
    overrides                    : None, }}

fn decompose_then_compose (
  composed : &ComposedNode,
) -> (ComposedNode, Vec<CompositionWarning>) {
  let home : SkgrepoName =
    composed . home . clone () . expect ("home set");
  let sections : Vec<(SkgrepoName, SectionSlices)> =
    decompose_sections (
      & DecompositionInput {
        pid      : &ID::new ("p"),
        extra_ids : &[],
        flags    : &[],
        title    : composed . title . as_deref (),
        body     : composed . body . as_deref (),
        home     : &home,
        aliases  : composed . aliases . as_deref ()
                   . unwrap_or (&[]),
        contains : &composed . contains,
        subscribesTo :
          composed . subscribesTo . as_deref () . unwrap_or (&[]),
        hidesFromSubs :
          composed . hidesFromSubs . as_deref ()
          . unwrap_or (&[]),
        overrides :
          composed . overrides . as_deref ()
          . unwrap_or (&[]), } );
  compose_sections ( &sections, &identity_resolve ) }

#[test]
fn flags_write_at_home_and_compose_defensively_from_all_sections () {
  let home = SkgrepoName::from ("S0");
  let private = SkgrepoName::from ("S2");
  let misc = vec![
    Flag::Had_ID_Before_Import,
    Flag::NoSearchMatching];
  let contains = vec![RelPartner::at_relRepo (
    private . clone (), ID::from ("child"))];
  let mut sections = decompose_node (&DecompositionInput {
    pid: &ID::from ("p"), extra_ids: &[], flags: &misc,
    title: Some ("title"), body: None, home: &home,
    aliases: &[], contains: &contains, subscribesTo: &[],
    hidesFromSubs: &[], overrides: &[],
  }, &telescope_config ()) . unwrap () . into_sections ();
  assert_eq! (sections . first () . unwrap () . 1 . flags, misc);
  assert! (sections . iter () . skip (1)
    . all (|(_, section)| section . flags . is_empty ()));

  let private_section = sections . iter_mut ()
    . find (|(skgrepo, _)| skgrepo == &private) . unwrap ();
  private_section . 1 . flags = vec![
    Flag::NoSearchMatching,
    Flag::Was_Overloaded];
  let telescope = Telescope::try_new (
    ID::from ("p"), sections, &telescope_config ()) . unwrap ();
  assert_eq! (telescope . flags (), vec![
    Flag::Had_ID_Before_Import,
    Flag::NoSearchMatching,
    Flag::Was_Overloaded]);
}

proptest! {
  #![proptest_config (ProptestConfig::with_cases (512))]

  #[test]
  fn round_trip_ordered_and_unordered (
    contains in arb_rel_partners (12),
    subs_raw in arb_rel_partners (6),
    hides_raw in arb_rel_partners (6),
  ) {
    // distinct id spaces so the three lists cannot collide
    let subs : Vec<RelPartner<ID>> =
      subs_raw . into_iter ()
      . map ( |m| RelPartner::at_relRepo (
        m . relRepo, ID ( format! ("s-{}", m . member . 0 ))))
      . collect ();
    let hides : Vec<RelPartner<ID>> =
      hides_raw . into_iter ()
      . map ( |m| RelPartner::at_relRepo (
        m . relRepo, ID ( format! ("h-{}", m . member . 0 ))))
      . collect ();
    let home     : SkgrepoName = SkgrepoName::from ("S0");
    let composed : ComposedNode =
      composed_from_lists (&home, contains, subs, hides);
    let (recomposed, warnings) = decompose_then_compose (&composed);
    // The one asymmetry: compose cannot learn a home the decomposition did not
    // write title/body text into; everything else must round-trip exactly.
    prop_assert_eq! ( &recomposed . contains, &composed . contains );
    prop_assert_eq! ( &recomposed . subscribesTo,
                      &composed . subscribesTo );
    { // Unordered relations have no order to preserve: sections
      // cannot express cross-repo interleavings without anchors,
      // which unordered relations deliberately lack, so the composition's
      // output order is CANONICAL (repo-major). The law is
      // set-equality with skgrepos intact.
      let sort = |v : Option<&Vec<RelPartner<ID>>>|
      -> Vec<RelPartner<ID>> {
        let mut v : Vec<RelPartner<ID>> =
          v . cloned () . unwrap_or_default ();
        v . sort_by ( |a, b| a . member . cmp ( &b . member ));
        v };
      prop_assert_eq! (
        sort ( recomposed . hidesFromSubs . as_ref () ),
        sort ( composed . hidesFromSubs . as_ref () )); }
    prop_assert_eq! ( recomposed . title . as_deref (), Some ("t") );
    prop_assert_eq! ( recomposed . home, Some (home) );
    prop_assert! ( warnings . is_empty (),
                   "round-trip inputs must not warn: {:?}", warnings );
  }

  #[test]
  fn decompose_is_canonical (
    contains in arb_rel_partners (12),
  ) {
    // decompose . compose . decompose == decompose  (sections are a normal form)
    let home     : SkgrepoName = SkgrepoName::from ("S0");
    let composed : ComposedNode = composed_from_lists (
      &home, contains, Vec::new (), Vec::new ());
    let (recomposed, _) = decompose_then_compose (&composed);
    let sections_once : Vec<(SkgrepoName, SectionSlices)> =
      decompose_sections (
        & DecompositionInput {
          pid : &ID::new ("p"), extra_ids : &[], flags : &[],
          title : composed . title . as_deref (),
          body : None, home : &home,
          aliases : &[], contains : &composed . contains,
          subscribesTo : &[],
          hidesFromSubs : &[],
          overrides : &[], } );
    let sections_twice : Vec<(SkgrepoName, SectionSlices)> =
      decompose_sections (
        & DecompositionInput {
          pid : &ID::new ("p"), extra_ids : &[], flags : &[],
          title : recomposed . title . as_deref (),
          body : None, home : &home,
          aliases : &[], contains : &recomposed . contains,
          subscribesTo : &[],
          hidesFromSubs : &[],
          overrides : &[], } );
    prop_assert_eq! (sections_once, sections_twice);
  }

  #[test]
  fn no_member_ever_changes_skgrepo ( // the silent-leak guard
    contains in arb_rel_partners (12),
  ) {
    let home     : SkgrepoName = SkgrepoName::from ("S0");
    let composed : ComposedNode = composed_from_lists (
      &home, contains . clone (), Vec::new (), Vec::new ());
    let (recomposed, _) = decompose_then_compose (&composed);
    for m in &contains {
      let found : Option<&RelPartner<ID>> =
        recomposed . contains . iter ()
        . find ( |n| n . member == m . member );
      prop_assert_eq! (
        found . map ( |n| &n . relRepo ), Some ( &m . relRepo ),
        "member {:?} changed repo", m . member );
    }
  }
}

#[test]
fn dangling_anchor_attaches_after_preceding_run_with_warning (
) {
  // S1's section: prepend [p], run (a,[x]), then a run whose anchor
  // is unknown -- its members must follow the PRECEDING run, warned.
  let sections : Vec<(SkgrepoName, SectionSlices)> = vec! [
    ( SkgrepoName::from ("S0"),
      SectionSlices {
        title : Some ("t" . to_string ()),
        contains : Some ( vec! [
          ListItem::Member ( ID::new ("a") ),
          ListItem::Member ( ID::new ("b") ) ] ),
        .. SectionSlices::default () } ),
    ( SkgrepoName::from ("S1"),
      SectionSlices {
        contains : Some ( vec! [
          ListItem::Member ( ID::new ("p") ),
          ListItem::Anchor { anchor : ID::new ("a") },
          ListItem::Member ( ID::new ("x") ),
          ListItem::Anchor { anchor : ID::new ("GONE") },
          ListItem::Member ( ID::new ("y") ) ] ),
        .. SectionSlices::default () } ) ];
  let (composed, warnings) =
    compose_sections ( &sections, &identity_resolve );
  let got : Vec<&str> =
    composed . contains . iter ()
    . map ( |m| m . member . 0 . as_str () )
    . collect ();
  assert_eq! ( got, vec! ["p", "a", "x", "y", "b"],
               "y follows its preceding run (a,[x])" );
  assert! ( warnings . iter () . any ( |w| matches! (
    w, CompositionWarning::DanglingAnchor { anchor } if anchor . 0 == "GONE" )),
    "dangling anchor warned: {:?}", warnings );
}

#[test]
fn dangling_first_run_joins_the_prepend (
) {
  let sections : Vec<(SkgrepoName, SectionSlices)> = vec! [
    ( SkgrepoName::from ("S0"),
      SectionSlices {
        title : Some ("t" . to_string ()),
        contains : Some ( vec! [
          ListItem::Member ( ID::new ("a") ) ] ),
        .. SectionSlices::default () } ),
    ( SkgrepoName::from ("S1"),
      SectionSlices {
        contains : Some ( vec! [
          ListItem::Anchor { anchor : ID::new ("GONE") },
          ListItem::Member ( ID::new ("y") ) ] ),
        .. SectionSlices::default () } ) ];
  let (composed, warnings) =
    compose_sections ( &sections, &identity_resolve );
  let got : Vec<&str> =
    composed . contains . iter ()
    . map ( |m| m . member . 0 . as_str () )
    . collect ();
  assert_eq! ( got, vec! ["y", "a"],
               "the orphaned first run joins the prepend" );
  assert_eq! ( warnings . len (), 1, "{:?}", warnings );
}

#[test]
fn duplicate_anchors_concatenate_in_file_order (
) {
  let sections : Vec<(SkgrepoName, SectionSlices)> = vec! [
    ( SkgrepoName::from ("S0"),
      SectionSlices {
        title : Some ("t" . to_string ()),
        contains : Some ( vec! [
          ListItem::Member ( ID::new ("a") ) ] ),
        .. SectionSlices::default () } ),
    ( SkgrepoName::from ("S1"),
      SectionSlices {
        contains : Some ( vec! [
          ListItem::Anchor { anchor : ID::new ("a") },
          ListItem::Member ( ID::new ("x") ),
          ListItem::Anchor { anchor : ID::new ("a") },
          ListItem::Member ( ID::new ("z") ) ] ),
        .. SectionSlices::default () } ) ];
  let (composed, _) =
    compose_sections ( &sections, &identity_resolve );
  let got : Vec<&str> =
    composed . contains . iter ()
    . map ( |m| m . member . 0 . as_str () )
    . collect ();
  assert_eq! ( got, vec! ["a", "x", "z"] );
}

#[test]
fn anchors_resolve_through_the_resolver ( // extra-id safety
) {
  let resolve = |skgid : &ID| -> ID {
    // "a-alias" is an extra id of "a"
    if skgid . 0 == "a-alias" { ID::new ("a") } else { skgid . clone () }};
  let sections : Vec<(SkgrepoName, SectionSlices)> = vec! [
    ( SkgrepoName::from ("S0"),
      SectionSlices {
        title : Some ("t" . to_string ()),
        contains : Some ( vec! [
          ListItem::Member ( ID::new ("a") ) ] ),
        .. SectionSlices::default () } ),
    ( SkgrepoName::from ("S1"),
      SectionSlices {
        contains : Some ( vec! [
          ListItem::Anchor { anchor : ID::new ("a-alias") },
          ListItem::Member ( ID::new ("y") ) ] ),
        .. SectionSlices::default () } ) ];
  let (composed, warnings) =
    compose_sections ( &sections, &resolve );
  let got : Vec<&str> =
    composed . contains . iter ()
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
  let titleless_public : (SkgrepoName, SectionSlices) =
    ( SkgrepoName::from ("public"),
      SectionSlices { contains : Some ( vec! [
        ListItem::Member ( ID::new ("C") ) ] ),
        .. SectionSlices::default () } );
  let titled_private : (SkgrepoName, SectionSlices) =
    ( SkgrepoName::from ("private"),
      SectionSlices { title : Some ( "N" . to_string () ),
                      body  : Some ( "secret" . to_string () ),
                      .. SectionSlices::default () } );
  let (composed, warnings) : (ComposedNode, Vec<CompositionWarning>) =
    compose_sections (
      & [ titleless_public, titled_private ],
      & identity_resolve );
  assert_eq! ( composed . home,
               Some ( SkgrepoName::from ("public") ),
               "the home is the most public section" );
  assert_eq! ( composed . title, Some ( "N" . to_string () ),
               "the title still folds in, from wherever it sits" );
  assert! ( warnings . contains ( & CompositionWarning::TitleBelowHome {
              home     : SkgrepoName::from ("public"),
              title_at : SkgrepoName::from ("private"), } ),
            "the shape is reported: {:?}", warnings );
}

#[test]
fn a_titled_most_public_section_raises_no_title_warning (
) {
  let titled_public : (SkgrepoName, SectionSlices) =
    ( SkgrepoName::from ("public"),
      SectionSlices { title : Some ( "N" . to_string () ),
                      .. SectionSlices::default () } );
  let titleless_private : (SkgrepoName, SectionSlices) =
    ( SkgrepoName::from ("private"),
      SectionSlices { contains : Some ( vec! [
        ListItem::Member ( ID::new ("C") ) ] ),
        .. SectionSlices::default () } );
  let (composed, warnings) : (ComposedNode, Vec<CompositionWarning>) =
    compose_sections (
      & [ titled_public, titleless_private ],
      & identity_resolve );
  assert_eq! ( composed . home,
               Some ( SkgrepoName::from ("public") ) );
  assert! ( ! warnings . iter () . any ( |w| matches! (
              w, CompositionWarning::TitleBelowHome { .. }
                 | CompositionWarning::NonHomeTitle { .. }
                 | CompositionWarning::MissingTitle )),
            "the ordinary telescope shape warns about nothing: {:?}",
            warnings );
}

fn text_node (
  sections : Vec<(SkgrepoName, SectionSlices)>,
) -> (crate::types::nodes::complete::Graphnode, Vec<CompositionWarning>) {
  let (composed, warnings) = compose_sections (&sections, &identity_resolve);
  let node = graphnode_from_composition (
    ID::new ("text-node"), Vec::new (), Vec::new (), composed )
    .expect ("test cases carry a title");
  (node, warnings) }

#[test]
fn title_and_body_select_independently_and_mark_overPrivateTextness (
) {
  let public = SkgrepoName::from ("public");
  let private = SkgrepoName::from ("private");

  let (clean, _) = text_node (vec! [
    ( public . clone (), SectionSlices {
        title : Some ("home title" . to_string ()),
        body  : Some ("home body" . to_string ()),
        .. SectionSlices::default () } ) ]);
  assert_eq! (clean . title, "home title");
  assert_eq! (clean . body . as_deref (), Some ("home body"));
  assert! (!clean . overPrivateText_telescope);

  let (lower_title, warnings) = text_node (vec! [
    ( public . clone (), SectionSlices {
        body : Some ("home body" . to_string ()),
        .. SectionSlices::default () } ),
    ( private . clone (), SectionSlices {
        title : Some ("lower title" . to_string ()),
        .. SectionSlices::default () } ) ]);
  assert_eq! (lower_title . title, "lower title");
  assert_eq! (lower_title . body . as_deref (), Some ("home body"));
  assert! (lower_title . overPrivateText_telescope);
  assert! (warnings . iter () . any ( |warning| matches! (
    warning, CompositionWarning::TitleBelowHome { title_at, .. }
      if title_at == &private )));

  let (lower_body, warnings) = text_node (vec! [
    ( public . clone (), SectionSlices {
        title : Some ("home title" . to_string ()),
        .. SectionSlices::default () } ),
    ( private . clone (), SectionSlices {
        body : Some ("lower body" . to_string ()),
        .. SectionSlices::default () } ) ]);
  assert_eq! (lower_body . title, "home title");
  assert_eq! (lower_body . body . as_deref (), Some ("lower body"));
  assert! (lower_body . overPrivateText_telescope);
  assert! (warnings . iter () . any ( |warning| matches! (
    warning, CompositionWarning::BodyBelowHome { body_at, .. }
      if body_at == &private )));

  let (both_lower, _) = text_node (vec! [
    ( public, SectionSlices::default () ),
    ( private, SectionSlices {
        title : Some ("lower title" . to_string ()),
        body  : Some ("lower body" . to_string ()),
        .. SectionSlices::default () } ) ]);
  assert_eq! (both_lower . title, "lower title");
  assert_eq! (both_lower . body . as_deref (), Some ("lower body"));
  assert! (both_lower . overPrivateText_telescope);
}

#[test]
fn later_text_reports_the_repo_that_actually_won (
) {
  let public = SkgrepoName::from ("public");
  let private = SkgrepoName::from ("private");
  let (_node, warnings) = text_node (vec! [
    ( public . clone (), SectionSlices {
        title : Some ("winner" . to_string ()),
        body  : Some ("winner body" . to_string ()),
        .. SectionSlices::default () } ),
    ( private . clone (), SectionSlices {
        title : Some ("later" . to_string ()),
        body  : Some ("later body" . to_string ()),
        .. SectionSlices::default () } ) ]);
  assert! (warnings . contains (&CompositionWarning::NonHomeTitle {
    skgrepo : private . clone (), selected_at : public . clone () }));
  assert! (warnings . contains (&CompositionWarning::NonHomeBody {
    skgrepo : private, selected_at : public }));
}

#[test]
fn pre_october_2026_keys_still_load () {
  use crate::types::nodes::fs::GraphnodeOnDisk;
  let old : GraphnodeOnDisk = serde_yaml::from_str (
    "pid: p\ntitle: t\nsubscribes_to:\n- s\nhides_from_its_subscriptions:\n- h\noverrides_view_of:\n- o\nmisc:\n- NoSearchMatching\n"
  ) . unwrap ();
  assert_eq! (old . hidesFromSubs, vec! [ID::from ("h")]);
  assert_eq! (old . overrides,     vec! [ID::from ("o")]);
  assert_eq! (old . flags,         vec! [Flag::NoSearchMatching]);
  assert_eq! (old . subscribesTo . len (), 1);
  let yaml : String = old . to_yaml () . unwrap ();
  for new_key in ["subscribesTo:", "hidesFromSubs:", "overrides:", "flags:"] {
    assert! (yaml . contains (new_key), "missing {}:\n{}", new_key, yaml); }
  for old_key in ["subscribes_to", "hides_from_its", "overrides_view_of", "misc"] {
    assert! (! yaml . contains (old_key), "still writes {}:\n{}", old_key, yaml); } }
