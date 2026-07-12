//! The UNFOLD: a node's effective leveled lists -> per-level
//! sections. Placements are DERIVED, never stored in memory: a
//! level-s run is a maximal streak of level-s members, anchored to
//! the nearest preceding STRICTLY more public member (or joining the
//! prepend if none precedes it). Derivation is canonical and stable:
//! if the more-public members did not move, the derived anchors do
//! not change, so an unchanged section serializes byte-identically
//! (the no-cosmetic-rewrites rule).
//!
//! fold(unfold(x)) == x for every leveled list (pinned by the
//! property suite in tests/unit/telescope.rs).

use crate::telescope::types::{ListItem, SectionSlices};
use crate::types::misc::{ID, PrivaciedMember, SourceName};

use std::collections::HashMap;

/// Split effective leveled lists (plus scalars) into per-level
/// 'SectionSlices', most public first. 'position' gives each level's
/// place in the privacy order (from 'SkgConfig::source_position');
/// levels the config does not know sort last, among themselves by
/// name, so junk data still unfolds deterministically.
pub struct UnfoldInput<'a> {
  pub title                        : Option<&'a str>,
  pub body                         : Option<&'a str>,
  pub home                         : &'a SourceName,
  pub aliases                      : &'a [PrivaciedMember<String>],
  pub contains                     : &'a [PrivaciedMember<ID>],
  pub subscribes_to                : &'a [PrivaciedMember<ID>],
  pub hides_from_its_subscriptions : &'a [PrivaciedMember<ID>],
  pub overrides_view_of            : &'a [PrivaciedMember<ID>],
}

pub fn unfold_node (
  input    : &UnfoldInput,
  position : &dyn Fn (&SourceName) -> Option<usize>,
) -> Vec<(SourceName, SectionSlices)> {
  let mut sections : HashMap<SourceName, SectionSlices> =
    HashMap::new ();
  let mut level_names : Vec<SourceName> = Vec::new ();
  { let mut note = |level : &SourceName| {
      if ! sections . contains_key (level) {
        level_names . push ( level . clone () );
        sections . insert (
          level . clone (), SectionSlices::default () ); }};
    note ( input . home );
    for m in input . contains          { note ( &m . level ); }
    for m in input . subscribes_to     { note ( &m . level ); }
    for m in input . hides_from_its_subscriptions
                                       { note ( &m . level ); }
    for m in input . overrides_view_of { note ( &m . level ); }
    for m in input . aliases           { note ( &m . level ); }}
  { // scalars live in the home section
    let home : &mut SectionSlices =
      sections . get_mut ( input . home )
      . expect ("home section was just noted");
    home . title = input . title . map ( str::to_string );
    home . body  = input . body  . map ( str::to_string ); }
  let rank = |level : &SourceName| -> (usize, SourceName) {
    ( position (level) . unwrap_or ( usize::MAX ),
      level . clone () ) };
  for (level, section) in sections . iter_mut () {
    let is_more_public = |a : &SourceName, b : &SourceName| -> bool {
      rank (a) < rank (b) };
    section . contains = unfold_ordered (
      input . contains, level, &is_more_public );
    section . subscribes_to = unfold_ordered (
      input . subscribes_to, level, &is_more_public );
    section . hides_from_its_subscriptions = unfold_unordered (
      input . hides_from_its_subscriptions, level );
    section . overrides_view_of = unfold_unordered (
      input . overrides_view_of, level );
    section . aliases = {
      let mine : Vec<String> =
        input . aliases . iter ()
        . filter ( |m| &m . level == level )
        . map ( |m| m . member . clone () )
        . collect ();
      if mine . is_empty () { None } else { Some (mine) }}; }
  { // Drop empty sections (a level with nothing left ceases to be),
    // except the home, which persists while the node exists.
    level_names . retain ( |l| {
      l == input . home
      || sections . get (l)
         . map ( |s| s . title . is_some ()
                 || s . body . is_some ()
                 || s . aliases . is_some ()
                 || s . contains . is_some ()
                 || s . subscribes_to . is_some ()
                 || s . hides_from_its_subscriptions . is_some ()
                 || s . overrides_view_of . is_some () )
         . unwrap_or (false) } ); }
  level_names . sort_by_key ( |l| rank (l) );
  level_names . into_iter ()
    . map ( |l| { let s : SectionSlices =
                    sections . remove (&l)
                    . expect ("section exists");
                  (l, s) } )
    . collect () }

/// One ordered relation's slice for LEVEL: maximal streaks of
/// level members, each anchored to the nearest preceding strictly
/// more public member; a streak with none joins the prepend. The
/// most public mentioning level yields an anchor-free base by
/// construction (nothing precedes its members more publicly ONLY
/// when it is first -- middle levels can and do anchor). Returns
/// None when the level has no members of this relation.
fn unfold_ordered (
  effective      : &[PrivaciedMember<ID>],
  level          : &SourceName,
  is_more_public : &dyn Fn (&SourceName, &SourceName) -> bool,
) -> Option<Vec<ListItem>> {
  if ! effective . iter () . any ( |m| &m . level == level ) {
    return None; }
  let is_base : bool = { // the most public mentioning level?
    let mut most_public : Option<&SourceName> = None;
    for m in effective {
      match most_public {
        None => { most_public = Some ( &m . level ); }
        Some (mp) => {
          if is_more_public ( &m . level, mp ) {
            most_public = Some ( &m . level ); }} }}
    most_public == Some (level) };
  let mut items : Vec<ListItem> = Vec::new ();
  let mut last_anchor_emitted : Option<ID> = None;
  let mut last_more_public : Option<ID> = None;
  for m in effective {
    if &m . level == level {
      match &last_more_public {
        None => {} // prepend: emit the member with no anchor first
        Some (a) => {
          if ! is_base
          && last_anchor_emitted . as_ref () != Some (a) {
            items . push ( ListItem::Anchor { anchor : a . clone () });
            last_anchor_emitted = Some ( a . clone () ); }} }
      items . push ( ListItem::Member ( m . member . clone () ));
    } else if is_more_public ( &m . level, level ) {
      last_more_public = Some ( m . member . clone () ); }}
  Some (items) }

/// One unordered relation's slice for LEVEL: just its members, in
/// effective order. None when empty.
fn unfold_unordered (
  effective : &[PrivaciedMember<ID>],
  level     : &SourceName,
) -> Option<Vec<ID>> {
  let mine : Vec<ID> =
    effective . iter ()
    . filter ( |m| &m . level == level )
    . map ( |m| m . member . clone () )
    . collect ();
  if mine . is_empty () { None } else { Some (mine) }}
