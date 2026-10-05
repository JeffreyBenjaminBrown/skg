//! The UNFOLD: a node's effective lists of relation partners ->
//! per-repo sections. Placements are DERIVED, never stored in
//! memory: a repo-s run is a maximal streak of repo-s members, anchored to
//! the nearest preceding STRICTLY more public member (or joining the
//! prepend if none precedes it). Derivation is canonical and stable:
//! if the more-public members did not move, the derived anchors do
//! not change, so an unchanged section serializes byte-identically
//! (the no-cosmetic-rewrites rule).
//!
//! compose(decompose(x)) == x for every list of relation partners (pinned by the
//! property suite in tests/unit/telescope.rs).

use crate::telescope::types::{
  ListItem, SectionSlices, Telescope, TelescopeConstructionError,
};
use crate::types::misc::{
  ID, RelPartner, SkgConfig, SkgRepoName,
};
use crate::types::nodes::complete::Flag;
use crate::types::nodes::fs::{GraphnodeOnDisk, graphnode_on_disk_from_section};

use std::collections::HashMap;
use std::fmt;

/// Everything required to decompose one complete in-memory node.
pub struct DecompositionInput<'a> {
  pub pid                          : &'a ID,
  pub extra_ids                    : &'a [ID],
  pub flags                        : &'a [Flag],
  pub title                        : Option<&'a str>,
  pub body                         : Option<&'a str>,
  pub home                         : &'a SkgRepoName,
  pub aliases                      : &'a [RelPartner<String>],
  pub contains                     : &'a [RelPartner<ID>],
  pub subscribes_to                : &'a [RelPartner<ID>],
  pub hides_from_its_subscriptions : &'a [RelPartner<ID>],
  pub overrides_view_of            : &'a [RelPartner<ID>],
}

/// A complete on-disk telescope prepared by the decomposition boundary.
/// Construction proves that it is nonempty, starts at HOME, and
/// contains same-pid sections at configured unique skgrepos in
/// privacy order. Ownership is deliberately not part of this type;
/// the filesystem writer checks it before mutation.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DecomposedTelescope {
  pid      : ID,
  home     : SkgRepoName,
  sections : Vec<(SkgRepoName, GraphnodeOnDisk)>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DecomposedTelescopeConstructionError {
  InvalidTelescope (TelescopeConstructionError),
  HomeMismatch {
    expected : SkgRepoName,
    actual   : SkgRepoName,
  },
}

impl fmt::Display for DecomposedTelescopeConstructionError {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>,
  ) -> fmt::Result {
    match self {
      DecomposedTelescopeConstructionError::InvalidTelescope (error) =>
        write! (f, "{}", error),
      DecomposedTelescopeConstructionError::HomeMismatch {
        expected, actual } =>
        write! ( f,
          "Unfolded telescope expected home '{}', but its first section is '{}'.",
          expected, actual ), }} }

impl std::error::Error for DecomposedTelescopeConstructionError {
}

impl DecomposedTelescope {
  pub fn try_new (
    pid      : ID,
    home     : SkgRepoName,
    sections : Vec<(SkgRepoName, GraphnodeOnDisk)>,
    config   : &SkgConfig,
  ) -> Result<DecomposedTelescope, DecomposedTelescopeConstructionError> {
    let telescope : Telescope = Telescope::try_new (
      pid . clone (), sections . clone (), config )
      . map_err (
        DecomposedTelescopeConstructionError::InvalidTelescope ) ?;
    if telescope . home () != &home {
      return Err (
        DecomposedTelescopeConstructionError::HomeMismatch {
          expected : home,
          actual   : telescope . home () . clone (), } ); }
    Ok ( DecomposedTelescope { pid, home, sections } ) }

  pub fn pid (&self) -> &ID { &self . pid }

  pub fn home (&self) -> &SkgRepoName { &self . home }

  pub fn sections (&self) -> &[(SkgRepoName, GraphnodeOnDisk)] {
    &self . sections }

  pub fn into_sections (self) -> Vec<(SkgRepoName, GraphnodeOnDisk)> {
    self . sections }
}

pub fn decompose_node (
  input  : &DecompositionInput,
  config : &SkgConfig,
) -> Result<DecomposedTelescope, DecomposedTelescopeConstructionError> {
  let mut sections : HashMap<SkgRepoName, SectionSlices> =
    HashMap::new ();
  let mut skgrepo_names : Vec<SkgRepoName> = Vec::new ();
  { let mut note = |skgrepo : &SkgRepoName| {
      if ! sections . contains_key (skgrepo) {
        skgrepo_names . push ( skgrepo . clone () );
        sections . insert (
          skgrepo . clone (), SectionSlices::default () ); }};
    note ( input . home );
    for m in input . contains          { note ( &m . relRepo ); }
    for m in input . subscribes_to     { note ( &m . relRepo ); }
    for m in input . hides_from_its_subscriptions
                                       { note ( &m . relRepo ); }
    for m in input . overrides_view_of { note ( &m . relRepo ); }
    for m in input . aliases           { note ( &m . relRepo ); }}
  { // title/body text live in the home section
    let home : &mut SectionSlices =
      sections . get_mut ( input . home )
      . expect ("home section was just noted");
    home . title = input . title . map ( str::to_string );
    home . body  = input . body  . map ( str::to_string ); }
  let rank = |skgrepo : &SkgRepoName| -> usize {
    config . skgrepo_position (skgrepo) . unwrap_or (usize::MAX) };
  for (skgrepo, section) in sections . iter_mut () {
    let is_more_public = |a : &SkgRepoName, b : &SkgRepoName| -> bool {
      rank (a) < rank (b) };
    section . contains = decompose_ordered (
      input . contains, skgrepo, &is_more_public );
    section . subscribes_to = decompose_ordered (
      input . subscribes_to, skgrepo, &is_more_public );
    section . hides_from_its_subscriptions = decompose_unordered (
      input . hides_from_its_subscriptions, skgrepo );
    section . overrides_view_of = decompose_unordered (
      input . overrides_view_of, skgrepo );
    section . aliases = {
      let mine : Vec<String> =
        input . aliases . iter ()
        . filter ( |m| &m . relRepo == skgrepo )
        . map ( |m| m . member . clone () )
        . collect ();
      if mine . is_empty () { None } else { Some (mine) }}; }
  { // Drop empty sections (a skgrepo with nothing left ceases to be),
    // except the home, which persists while the node exists.
    skgrepo_names . retain ( |l| {
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
  skgrepo_names . sort_by_key ( |skgrepo| rank (skgrepo) );
  let complete_sections : Vec<(SkgRepoName, GraphnodeOnDisk)> =
    skgrepo_names . into_iter ()
    . map ( |skgrepo| {
      let slices : SectionSlices = sections . remove (&skgrepo)
        . expect ("section exists");
      let is_home : bool = skgrepo == * input . home;
      let node_fs : GraphnodeOnDisk = graphnode_on_disk_from_section (
        input . pid, input . extra_ids, input . flags,
        is_home, slices );
      (skgrepo, node_fs) } )
    . collect ();
  DecomposedTelescope::try_new (
    input . pid . clone (), input . home . clone (),
    complete_sections, config ) }

/// One ordered relation's slice for REPO: maximal streaks of
/// partners with that relRepo, each anchored to the nearest preceding strictly
/// more public member; a streak with none joins the prepend. The
/// most public skgrepo mentioning the relation yields an anchor-free base by
/// construction (nothing precedes its members more publicly ONLY
/// when it is first -- middle skgrepos can and do anchor). Returns
/// None when the skgrepo has no members of this relation.
fn decompose_ordered (
  effective      : &[RelPartner<ID>],
  skgrepo        : &SkgRepoName,
  is_more_public : &dyn Fn (&SkgRepoName, &SkgRepoName) -> bool,
) -> Option<Vec<ListItem>> {
  if ! effective . iter () . any ( |m| &m . relRepo == skgrepo ) {
    return None; }
  let is_base : bool = { // the most public skgrepo mentioning the relation?
    let mut most_public : Option<&SkgRepoName> = None;
    for m in effective {
      match most_public {
        None => { most_public = Some ( &m . relRepo ); }
        Some (mp) => {
          if is_more_public ( &m . relRepo, mp ) {
            most_public = Some ( &m . relRepo ); }} }}
    most_public == Some (skgrepo) };
  let mut items : Vec<ListItem> = Vec::new ();
  let mut last_anchor_emitted : Option<ID> = None;
  let mut last_more_public : Option<ID> = None;
  for m in effective {
    if &m . relRepo == skgrepo {
      match &last_more_public {
        None => {} // prepend: emit the member with no anchor first
        Some (a) => {
          if ! is_base
          && last_anchor_emitted . as_ref () != Some (a) {
            items . push ( ListItem::Anchor { anchor : a . clone () });
            last_anchor_emitted = Some ( a . clone () ); }} }
      items . push ( ListItem::Member ( m . member . clone () ));
    } else if is_more_public ( &m . relRepo, skgrepo ) {
      last_more_public = Some ( m . member . clone () ); }}
  Some (items) }

/// One unordered relation's slice for REPO: just its members, in
/// effective order. None when empty.
fn decompose_unordered (
  effective : &[RelPartner<ID>],
  skgrepo   : &SkgRepoName,
) -> Option<Vec<ID>> {
  let mine : Vec<ID> =
    effective . iter ()
    . filter ( |m| &m . relRepo == skgrepo )
    . map ( |m| m . member . clone () )
    . collect ();
  if mine . is_empty () { None } else { Some (mine) }}
