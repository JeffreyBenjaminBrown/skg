//! The UNFOLD: a node's effective lists of members at sources ->
//! per-source sections. Placements are DERIVED, never stored in
//! memory: a source-s run is a maximal streak of source-s members, anchored to
//! the nearest preceding STRICTLY more public member (or joining the
//! prepend if none precedes it). Derivation is canonical and stable:
//! if the more-public members did not move, the derived anchors do
//! not change, so an unchanged section serializes byte-identically
//! (the no-cosmetic-rewrites rule).
//!
//! fold(unfold(x)) == x for every list of members at sources (pinned by the
//! property suite in tests/unit/telescope.rs).

use crate::telescope::types::{
  ListItem, SectionSlices, Telescope, TelescopeConstructionError,
};
use crate::types::misc::{
  ID, MemberAtSource, SkgConfig, SourceName,
};
use crate::types::nodes::complete::FileProperty;
use crate::types::nodes::fs::{NodeFS, nodefs_from_section};

use std::collections::HashMap;
use std::fmt;

/// Everything required to unfold one complete in-memory node.
pub struct UnfoldInput<'a> {
  pub pid                          : &'a ID,
  pub extra_ids                    : &'a [ID],
  pub misc                         : &'a [FileProperty],
  pub title                        : Option<&'a str>,
  pub body                         : Option<&'a str>,
  pub home                         : &'a SourceName,
  pub aliases                      : &'a [MemberAtSource<String>],
  pub contains                     : &'a [MemberAtSource<ID>],
  pub subscribes_to                : &'a [MemberAtSource<ID>],
  pub hides_from_its_subscriptions : &'a [MemberAtSource<ID>],
  pub overrides_view_of            : &'a [MemberAtSource<ID>],
}

/// A complete on-disk telescope prepared by the unfold boundary.
/// Construction proves that it is nonempty, starts at HOME, and
/// contains same-pid sections at configured unique sources in
/// privacy order. Ownership is deliberately not part of this type;
/// the filesystem writer checks it before mutation.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct UnfoldedTelescope {
  pid      : ID,
  home     : SourceName,
  sections : Vec<(SourceName, NodeFS)>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum UnfoldedTelescopeConstructionError {
  InvalidTelescope (TelescopeConstructionError),
  HomeMismatch {
    expected : SourceName,
    actual   : SourceName,
  },
}

impl fmt::Display for UnfoldedTelescopeConstructionError {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>,
  ) -> fmt::Result {
    match self {
      UnfoldedTelescopeConstructionError::InvalidTelescope (error) =>
        write! (f, "{}", error),
      UnfoldedTelescopeConstructionError::HomeMismatch {
        expected, actual } =>
        write! ( f,
          "Unfolded telescope expected home '{}', but its first section is '{}'.",
          expected, actual ), }} }

impl std::error::Error for UnfoldedTelescopeConstructionError {
}

impl UnfoldedTelescope {
  pub fn try_new (
    pid      : ID,
    home     : SourceName,
    sections : Vec<(SourceName, NodeFS)>,
    config   : &SkgConfig,
  ) -> Result<UnfoldedTelescope, UnfoldedTelescopeConstructionError> {
    let telescope : Telescope = Telescope::try_new (
      pid . clone (), sections . clone (), config )
      . map_err (
        UnfoldedTelescopeConstructionError::InvalidTelescope ) ?;
    if telescope . home () != &home {
      return Err (
        UnfoldedTelescopeConstructionError::HomeMismatch {
          expected : home,
          actual   : telescope . home () . clone (), } ); }
    Ok ( UnfoldedTelescope { pid, home, sections } ) }

  pub fn pid (&self) -> &ID { &self . pid }

  pub fn home (&self) -> &SourceName { &self . home }

  pub fn sections (&self) -> &[(SourceName, NodeFS)] {
    &self . sections }

  pub fn into_sections (self) -> Vec<(SourceName, NodeFS)> {
    self . sections }
}

pub fn unfold_node (
  input  : &UnfoldInput,
  config : &SkgConfig,
) -> Result<UnfoldedTelescope, UnfoldedTelescopeConstructionError> {
  let mut sections : HashMap<SourceName, SectionSlices> =
    HashMap::new ();
  let mut source_names : Vec<SourceName> = Vec::new ();
  { let mut note = |source : &SourceName| {
      if ! sections . contains_key (source) {
        source_names . push ( source . clone () );
        sections . insert (
          source . clone (), SectionSlices::default () ); }};
    note ( input . home );
    for m in input . contains          { note ( &m . source ); }
    for m in input . subscribes_to     { note ( &m . source ); }
    for m in input . hides_from_its_subscriptions
                                       { note ( &m . source ); }
    for m in input . overrides_view_of { note ( &m . source ); }
    for m in input . aliases           { note ( &m . source ); }}
  { // title/body text live in the home section
    let home : &mut SectionSlices =
      sections . get_mut ( input . home )
      . expect ("home section was just noted");
    home . title = input . title . map ( str::to_string );
    home . body  = input . body  . map ( str::to_string ); }
  let rank = |source : &SourceName| -> usize {
    config . source_position (source) . unwrap_or (usize::MAX) };
  for (source, section) in sections . iter_mut () {
    let is_more_public = |a : &SourceName, b : &SourceName| -> bool {
      rank (a) < rank (b) };
    section . contains = unfold_ordered (
      input . contains, source, &is_more_public );
    section . subscribes_to = unfold_ordered (
      input . subscribes_to, source, &is_more_public );
    section . hides_from_its_subscriptions = unfold_unordered (
      input . hides_from_its_subscriptions, source );
    section . overrides_view_of = unfold_unordered (
      input . overrides_view_of, source );
    section . aliases = {
      let mine : Vec<String> =
        input . aliases . iter ()
        . filter ( |m| &m . source == source )
        . map ( |m| m . member . clone () )
        . collect ();
      if mine . is_empty () { None } else { Some (mine) }}; }
  { // Drop empty sections (a source with nothing left ceases to be),
    // except the home, which persists while the node exists.
    source_names . retain ( |l| {
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
  source_names . sort_by_key ( |source| rank (source) );
  let complete_sections : Vec<(SourceName, NodeFS)> =
    source_names . into_iter ()
    . map ( |source| {
      let slices : SectionSlices = sections . remove (&source)
        . expect ("section exists");
      let is_home : bool = source == * input . home;
      let node_fs : NodeFS = nodefs_from_section (
        input . pid, input . extra_ids, input . misc,
        is_home, slices );
      (source, node_fs) } )
    . collect ();
  UnfoldedTelescope::try_new (
    input . pid . clone (), input . home . clone (),
    complete_sections, config ) }

/// One ordered relation's slice for SOURCE: maximal streaks of
/// members at that source, each anchored to the nearest preceding strictly
/// more public member; a streak with none joins the prepend. The
/// most public source mentioning the relation yields an anchor-free base by
/// construction (nothing precedes its members more publicly ONLY
/// when it is first -- middle sources can and do anchor). Returns
/// None when the source has no members of this relation.
fn unfold_ordered (
  effective      : &[MemberAtSource<ID>],
  source         : &SourceName,
  is_more_public : &dyn Fn (&SourceName, &SourceName) -> bool,
) -> Option<Vec<ListItem>> {
  if ! effective . iter () . any ( |m| &m . source == source ) {
    return None; }
  let is_base : bool = { // the most public source mentioning the relation?
    let mut most_public : Option<&SourceName> = None;
    for m in effective {
      match most_public {
        None => { most_public = Some ( &m . source ); }
        Some (mp) => {
          if is_more_public ( &m . source, mp ) {
            most_public = Some ( &m . source ); }} }}
    most_public == Some (source) };
  let mut items : Vec<ListItem> = Vec::new ();
  let mut last_anchor_emitted : Option<ID> = None;
  let mut last_more_public : Option<ID> = None;
  for m in effective {
    if &m . source == source {
      match &last_more_public {
        None => {} // prepend: emit the member with no anchor first
        Some (a) => {
          if ! is_base
          && last_anchor_emitted . as_ref () != Some (a) {
            items . push ( ListItem::Anchor { anchor : a . clone () });
            last_anchor_emitted = Some ( a . clone () ); }} }
      items . push ( ListItem::Member ( m . member . clone () ));
    } else if is_more_public ( &m . source, source ) {
      last_more_public = Some ( m . member . clone () ); }}
  Some (items) }

/// One unordered relation's slice for SOURCE: just its members, in
/// effective order. None when empty.
fn unfold_unordered (
  effective : &[MemberAtSource<ID>],
  source    : &SourceName,
) -> Option<Vec<ID>> {
  let mine : Vec<ID> =
    effective . iter ()
    . filter ( |m| &m . source == source )
    . map ( |m| m . member . clone () )
    . collect ();
  if mine . is_empty () { None } else { Some (mine) }}
