//! Core values of the privacy telescope (in comments: "telescope" =
//! privacy telescope and "section" = telescope section; recording
//! positions are named by sources, ordered by privacy.
//!
//! One node = one ID = one telescope: a set of same-ID .skg files,
//! at most one per source ("sections"). Every relationship instance
//! is recorded in exactly one section, whose source is the edge's
//! recording source. On disk each ordered relation is ONE flat sequence of
//! items -- members and anchors -- whose role (base list vs
//! placement) follows from WHICH section holds it, not from its
//! shape: the most public section mentioning a relation holds its
//! anchor-free base; a more private section's items before the first
//! anchor are its prepend, and each anchor starts a run inserted
//! after that member of the strictly-more-public fold. See
//! TODO/user-owned_autofork_chain/5_plan.org, work item
//! section-format-and-fold.

use serde::{Serialize, Deserialize};

use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::nodes::complete::FileProperty;
use crate::types::nodes::fs::NodeFS;

use std::collections::{HashMap, HashSet};
use std::fmt;

/// ONE NODE, as it sits on disk: its sections, in privacy order,
/// most public first.
///
/// The point of the type is that "several files, one node" is a
/// VALUE rather than a condition to be discovered. Before it,
/// grouping the same-pid files ended in an anonymous
/// 'Vec<(SourceName, NodeFS)>', and any function tempted to answer a
/// one-file question about a many-file node could do so without
/// anything forcing it to say which section it meant --- which is
/// how 'source_from_disk' went on answering the pre-telescope
/// question long after telescopes arrived (see
/// TODO/dup-ids-maybe-bad/1_discussion.org). New code meets a
/// 'Telescope' and has to choose.
///
/// Construction is checked against the config, so a value is always
/// nonempty, contains only same-pid sections at configured unique
/// sources, and is ordered most public first. The HOME is therefore
/// unambiguously the first section.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Telescope {
  pid      : ID,
  sections : Vec<(SourceName, NodeFS)>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TelescopeConstructionError {
  Empty {
    pid : ID,
  },
  MixedPid {
    expected : ID,
    actual   : ID,
    source   : SourceName,
  },
  UnknownSource {
    source : SourceName,
  },
  DuplicateSource {
    source : SourceName,
  },
  OutOfOrder {
    previous : SourceName,
    next     : SourceName,
  },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IgnoredForeignPidCollision {
  pub ignored_sources : Vec<SourceName>,
}

/// If a pid has any owned section, its owned sections are the
/// telescope and every non-owned same-pid section is ignored. A pid
/// with no owned section remains an ordinary foreign telescope.
/// Input order is preserved.
pub fn retain_owned_sections_when_pid_collides (
  sections : Vec<(SourceName, NodeFS)>,
  config   : &SkgConfig,
) -> ( Vec<(SourceName, NodeFS)>,
       Option<IgnoredForeignPidCollision> ) {
  let has_owned : bool = sections . iter ()
    . any ( |(source, _)| config . user_owns_source (source) );
  if ! has_owned {
    return (sections, None); }
  let mut retained : Vec<(SourceName, NodeFS)> = Vec::new ();
  let mut ignored_sources : Vec<SourceName> = Vec::new ();
  for (source, node_fs) in sections {
    if config . user_owns_source (&source) {
      retained . push (( source, node_fs )); }
    else {
      ignored_sources . push (source); }}
  let warning : Option<IgnoredForeignPidCollision> =
    if ignored_sources . is_empty () { None }
    else { Some ( IgnoredForeignPidCollision { ignored_sources } ) };
  (retained, warning) }

impl fmt::Display for TelescopeConstructionError {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>,
  ) -> fmt::Result {
    match self {
      TelescopeConstructionError::Empty { pid } =>
        write! ( f, "Telescope '{}' has no sections.", pid ),
      TelescopeConstructionError::MixedPid {
        expected, actual, source } =>
        write! ( f,
          "Telescope '{}' contains a section from source '{}' whose embedded pid is '{}'.",
          expected, source, actual ),
      TelescopeConstructionError::UnknownSource { source } =>
        write! ( f,
          "Telescope contains a section from unconfigured source '{}'.",
          source ),
      TelescopeConstructionError::DuplicateSource { source } =>
        write! ( f,
          "Telescope contains more than one section from source '{}'.",
          source ),
      TelescopeConstructionError::OutOfOrder { previous, next } =>
        write! ( f,
          "Telescope sections are out of privacy order: '{}' precedes '{}'.",
          previous, next ), }} }

impl std::error::Error for TelescopeConstructionError {
}

impl Telescope {
  pub fn try_new (
    pid      : ID,
    sections : Vec<(SourceName, NodeFS)>,
    config   : &SkgConfig,
  ) -> Result<Telescope, TelescopeConstructionError> {
    if sections . is_empty () {
      return Err ( TelescopeConstructionError::Empty { pid } ); }
    let positions : HashMap<SourceName, usize> =
      config . ordered_sources () . into_iter () . enumerate ()
      . map ( |(position, source)| (source, position) )
      . collect ();
    let mut seen_sources : HashSet<SourceName> = HashSet::new ();
    let mut previous : Option<(usize, SourceName)> = None;
    for (source, node_fs) in &sections {
      if node_fs . pid != pid {
        return Err ( TelescopeConstructionError::MixedPid {
          expected : pid,
          actual   : node_fs . pid . clone (),
          source   : source . clone (), } ); }
      let position : usize = * positions . get (source)
        . ok_or_else ( || TelescopeConstructionError::UnknownSource {
          source : source . clone (), } ) ?;
      if ! seen_sources . insert ( source . clone () ) {
        return Err ( TelescopeConstructionError::DuplicateSource {
          source : source . clone (), } ); }
      if let Some ((previous_position, previous_source)) = &previous {
        if *previous_position >= position {
          return Err ( TelescopeConstructionError::OutOfOrder {
            previous : previous_source . clone (),
            next     : source . clone (), } ); }}
      previous = Some (( position, source . clone () )); }
    Ok ( Telescope { pid, sections } ) }

  pub fn pid (
    &self,
  ) -> &ID {
    &self . pid }

  /// The most public section's source.
  pub fn home (
    &self,
  ) -> &SourceName {
    & self . sections . first ()
      . expect ("Telescope construction guarantees a section") . 0 }

  pub fn sections (
    &self,
  ) -> &[(SourceName, NodeFS)] {
    &self . sections }

  /// Every extra id any section claims, first occurrence first.
  /// Unioned across sections rather than read from the home alone:
  /// extra ids are home-section data by convention, and a stray one
  /// elsewhere should still resolve rather than silently dangle.
  pub fn extra_ids (
    &self,
  ) -> Vec<ID> {
    let mut extra_ids : Vec<ID> = Vec::new ();
    for (_, node_fs) in &self . sections {
      for e in &node_fs . extra_ids {
        if ! extra_ids . contains (e) {
          extra_ids . push ( e . clone () ); }} }
    extra_ids }

  /// Every unrecognized file property any section carries, first
  /// occurrence first. Unioned defensively, like 'extra_ids'.
  pub fn misc (
    &self,
  ) -> Vec<FileProperty> {
    let mut misc : Vec<FileProperty> = Vec::new ();
    for (_, node_fs) in &self . sections {
      for m in &node_fs . misc {
        if ! misc . contains (m) {
          misc . push ( m . clone () ); }} }
    misc }

  /// The sections in the form the fold consumes, order preserved.
  pub fn into_slices (
    self,
  ) -> Vec<(SourceName, SectionSlices)> {
    self . sections . into_iter ()
      . map ( |(source, node_fs)|
              (source, node_fs . into_section_slices ()) )
      . collect () }
}

/// One entry of an ordered relation's stored sequence.
///
/// SERIALIZATION (the diff-readable requirement from 4_discussion's
/// stage-moves thread): the sequence is one YAML list, one entry per
/// line -- a member is a bare ID line ("- ID"), an anchor a one-key
/// map line ("- anchor: ID"). Members therefore render identically
/// in every section, so a membership moving between sections diffs
/// as a clean one-line delete/add pair, and anchors look visibly
/// different. Serde: untagged, so a plain string parses as Member
/// and the map as Anchor.
#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize)]
#[serde(untagged)]
pub enum ListItem {
  Member (ID),
  /// Names a member of the strictly-more-public fold; the items
  /// after it (until the next anchor) insert immediately after that
  /// member. Illegal in an unordered relation and in the most public
  /// section mentioning the relation (where it degrades per the
  /// dangling-anchor fallback, with a warning).
  Anchor {
    anchor : ID,
  },
}

impl<'de> Deserialize<'de> for ListItem {
  /// Manual rather than derive(untagged): untagged deserialization
  /// buffers into a self-describing form in which a plain YAML
  /// scalar like `11` is an INTEGER, so `Member(ID)` (a String
  /// newtype) would reject numeric-looking IDs that the old direct
  /// Vec<ID> path accepted. Any scalar is a member; a one-key
  /// {anchor: ...} map is an anchor.
  fn deserialize<D> (
    deserializer : D,
  ) -> Result<ListItem, D::Error>
  where D : serde::Deserializer<'de> {
    #[derive(Deserialize)]
    #[serde(untagged)]
    enum Scalar {
      S (String),
      I (i64),
      F (f64),
      B (bool),
    }
    impl Scalar {
      fn into_id (self) -> ID {
        match self {
          Scalar::S (s) => ID (s),
          Scalar::I (i) => ID ( i . to_string () ),
          Scalar::F (f) => ID ( f . to_string () ),
          Scalar::B (b) => ID ( b . to_string () ), }}}
    #[derive(Deserialize)]
    #[serde(untagged)]
    enum Raw {
      Anchor { anchor : Scalar },
      Member (Scalar),
    }
    Ok ( match Raw::deserialize (deserializer) ? {
      Raw::Anchor { anchor } =>
        ListItem::Anchor { anchor : anchor . into_id () },
      Raw::Member (s) =>
        ListItem::Member ( s . into_id () ), } ) }}

/// What one section contributes to its node, in section-local form.
/// This is the PARSED shape of a section file's list fields; the
/// serde wiring of NodeFS to this shape lands with the rest of the
/// section format. Ordered relations carry items (anchors legal);
/// unordered relations and aliases carry plain members.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct SectionSlices {
  pub title                        : Option<String>,
  pub body                         : Option<String>,
  pub aliases                      : Option<Vec<String>>,
  pub contains                     : Option<Vec<ListItem>>,
  pub subscribes_to                : Option<Vec<ListItem>>,
  pub hides_from_its_subscriptions : Option<Vec<ID>>,
  pub overrides_view_of            : Option<Vec<ID>>,
}

/// Nonfatal fold trouble. The fold is TOTAL: junk degrades to one of
/// these, never to an error or a panic, because a dangling anchor
/// can arise from two perfectly correct saves on different machines
/// (see the plan's "Dangling anchors" section).
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum FoldWarning {
  /// An anchor named no member of the strictly-more-public fold.
  /// Its run attached after the preceding run (or the prepend).
  DanglingAnchor { anchor : ID },
  /// An anchor appeared in the most public section that mentions
  /// the relation -- there is no more-public fold to anchor into.
  /// Handled exactly like a dangling anchor.
  AnchorInBase { anchor : ID },
  /// The same member appeared in two sources; the more public
  /// occurrence won.
  DuplicateMember { member : ID },
  /// The home -- the most public section -- carries no title, so
  /// the text sits at 'title_at', where a reader restricted to the
  /// home source cannot see it. Distinct from 'NonHomeTitle' (a
  /// stray SECOND title) and 'MissingTitle' (no title anywhere).
  TitleBelowHome {
    home     : crate::types::misc::SourceName,
    title_at : crate::types::misc::SourceName,
  },
  /// The selected body is below the home. Title and body select
  /// independently, so this may occur with a title at home.
  BodyBelowHome {
    home    : crate::types::misc::SourceName,
    body_at : crate::types::misc::SourceName,
  },
  /// A section below the one holding the title carried a title too;
  /// the more public one won.
  NonHomeTitle {
    source      : crate::types::misc::SourceName,
    selected_at : crate::types::misc::SourceName,
  },
  /// A later section carried a body; the more-public selected body won.
  NonHomeBody {
    source      : crate::types::misc::SourceName,
    selected_at : crate::types::misc::SourceName,
  },
  /// No section carried a title.
  MissingTitle,
}

impl std::fmt::Display for FoldWarning {
  fn fmt (
    &self,
    f : &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    match self {
      FoldWarning::DanglingAnchor { anchor } =>
        write! ( f,
          "dangling anchor '{}': it named no member of any more public section, so its run attached after the preceding run (or the prepend)",
          anchor ),
      FoldWarning::AnchorInBase { anchor } =>
        write! ( f,
          "anchor '{}' appeared in the most public section mentioning its relation, where there is no more public fold to anchor into; handled like a dangling anchor",
          anchor ),
      FoldWarning::DuplicateMember { member } =>
        write! ( f,
          "member '{}' appeared in two sources; the more public occurrence won",
          member ),
      FoldWarning::TitleBelowHome { home, title_at } =>
        write! ( f,
          "title below the home: the home '{}' carries no title, so this node's text sits at '{}', invisible to anyone reading at '{}'. A node's text belongs in its most public section. A save of this node is refused until the files are repaired by hand: either move the title up to '{}', or delete the '{}' section if it holds nothing else.",
          home, title_at, home, home, home ),
      FoldWarning::BodyBelowHome { home, body_at } =>
        write! ( f,
          "body below the home: the home is '{}', but the selected body sits at '{}'; restricted readers at '{}' cannot see it",
          home, body_at, home ),
      FoldWarning::NonHomeTitle { source, selected_at } =>
        write! ( f,
          "section '{}' carried a later title; the more public title selected from '{}' won",
          source, selected_at ),
      FoldWarning::NonHomeBody { source, selected_at } =>
        write! ( f,
          "section '{}' carried a later body; the more public body selected from '{}' won",
          source, selected_at ),
      FoldWarning::MissingTitle =>
        write! ( f, "no section carried a title" ), }}}

#[cfg(test)]
mod telescope_construction_tests {
  use super::{Telescope, TelescopeConstructionError};
  use crate::types::misc::{
    ID, SkgConfig, SkgfileSource, SourceName};
  use crate::types::nodes::fs::NodeFS;

  use std::collections::HashMap;
  use std::path::PathBuf;

  fn source (
    name : &str,
  ) -> SourceName {
    SourceName::from (name) }

  fn node_fs (
    pid : &str,
  ) -> NodeFS {
    NodeFS {
      title                        : Some ("title" . to_string ()),
      aliases                      : Vec::new (),
      pid                          : ID::from (pid),
      extra_ids                    : Vec::new (),
      body                         : None,
      contains                     : Vec::new (),
      subscribes_to                : Vec::new (),
      hides_from_its_subscriptions : Vec::new (),
      overrides_view_of            : Vec::new (),
      misc                         : Vec::new (), } }

  fn config () -> SkgConfig {
    let ordered : Vec<SourceName> =
      ["public", "trusted", "private"] . into_iter ()
      . map (source) . collect ();
    let sources : HashMap<SourceName, SkgfileSource> =
      ordered . iter () . cloned ()
      . map ( |name| {
        ( name . clone (),
          SkgfileSource {
            path         : PathBuf::from ( &name . 0 ),
            name,
            abbreviation : None,
            user_owns_it : true, } ) } )
      . collect ();
    let mut config : SkgConfig =
      SkgConfig::dummyFromSources (sources);
    config . source_order = ordered;
    config }

  #[test]
  fn checked_constructor_accepts_only_the_documented_shape () {
    let config : SkgConfig = config ();
    let valid : Telescope = Telescope::try_new (
      ID::from ("N"),
      vec! [ (source ("public"), node_fs ("N")),
             (source ("private"), node_fs ("N")) ],
      &config ) . unwrap ();
    assert_eq! ( valid . pid (), &ID::from ("N") );
    assert_eq! ( valid . home (), &source ("public") );
    assert_eq! ( valid . sections () . len (), 2 );

    assert! ( matches! (
      Telescope::try_new ( ID::from ("N"), Vec::new (), &config ),
      Err (TelescopeConstructionError::Empty { .. }) ));
    assert! ( matches! (
      Telescope::try_new (
        ID::from ("N"),
        vec! [ (source ("private"), node_fs ("N")),
               (source ("public"), node_fs ("N")) ],
        &config ),
      Err (TelescopeConstructionError::OutOfOrder { .. }) ));
    assert! ( matches! (
      Telescope::try_new (
        ID::from ("N"),
        vec! [ (source ("public"), node_fs ("N")),
               (source ("public"), node_fs ("N")) ],
        &config ),
      Err (TelescopeConstructionError::DuplicateSource { .. }) ));
    assert! ( matches! (
      Telescope::try_new (
        ID::from ("N"),
        vec! [ (source ("unknown"), node_fs ("N")) ],
        &config ),
      Err (TelescopeConstructionError::UnknownSource { .. }) ));
    assert! ( matches! (
      Telescope::try_new (
        ID::from ("N"),
        vec! [ (source ("public"), node_fs ("other")) ],
        &config ),
      Err (TelescopeConstructionError::MixedPid { .. }) ));
  }
}
