//! Core values of the privacy telescope (in comments: "telescope" =
//! privacy telescope and "section" = telescope section; recording
//! positions are named by skgrepos, ordered by privacy.
//!
//! One node = one ID = one telescope: a set of same-ID .skg files,
//! at most one per skgrepo ("sections"). Every relationship instance
//! is recorded in exactly one section, whose skgrepo is the relationship's
//! relRepo. On disk each ordered relation is ONE flat sequence of
//! items -- members and anchors -- whose role (base list vs
//! placement) follows from WHICH section holds it, not from its
//! shape: the most public section mentioning a relation holds its
//! anchor-free base; a more private section's items before the first
//! anchor are its prepend, and each anchor starts a run inserted
//! after that member of the strictly-more-public compose. See
//! TODO/DONE/privacy-telescope/5_plan.org, work item
//! section-format-and-fold.

use serde::{Serialize, Deserialize};

use crate::types::misc::{ID, SkgConfig, SkgRepoName};
use crate::types::nodes::complete::Flag;
use crate::types::nodes::fs::GraphnodeOnDisk;

use std::collections::{HashMap, HashSet};
use std::fmt;

/// ONE NODE, as it sits on disk: its sections, in privacy order,
/// most public first.
///
/// The point of the type is that "several files, one node" is a
/// VALUE rather than a condition to be discovered. Before it,
/// grouping the same-pid files ended in an anonymous
/// 'Vec<(RepoName, GraphnodeOnDisk)>', and any function tempted to answer a
/// one-file question about a many-file node could do so without
/// anything forcing it to say which section it meant --- which is
/// how 'repo_from_disk' went on answering the pre-telescope
/// question long after telescopes arrived (see
/// TODO/dup-ids-maybe-bad/1_discussion.org). New code meets a
/// 'Telescope' and has to choose.
///
/// Construction is checked against the config, so a value is always
/// nonempty, contains only same-pid sections at configured unique
/// skgrepos, and is ordered most public first. The HOME is therefore
/// unambiguously the first section.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Telescope {
  pid      : ID,
  sections : Vec<(SkgRepoName, GraphnodeOnDisk)>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TelescopeConstructionError {
  Empty {
    pid : ID,
  },
  MixedPid {
    expected : ID,
    actual   : ID,
    skgrepo  : SkgRepoName,
  },
  UnknownSkgRepo {
    skgrepo : SkgRepoName,
  },
  DuplicateSkgRepo {
    skgrepo : SkgRepoName,
  },
  OutOfOrder {
    previous : SkgRepoName,
    next     : SkgRepoName,
  },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IgnoredForeignPidFolderlision {
  pub ignored_skgrepos : Vec<SkgRepoName>,
}

/// If a pid has any owned section, its owned sections are the
/// telescope and every non-owned same-pid section is ignored. A pid
/// with no owned section remains an ordinary foreign telescope.
/// Input order is preserved.
pub fn retain_owned_sections_when_pid_folderlides (
  sections : Vec<(SkgRepoName, GraphnodeOnDisk)>,
  config   : &SkgConfig,
) -> ( Vec<(SkgRepoName, GraphnodeOnDisk)>,
       Option<IgnoredForeignPidFolderlision> ) {
  let has_owned : bool = sections . iter ()
    . any ( |(skgrepo, _)| config . skgrepo_is_owned (skgrepo) );
  if ! has_owned {
    return (sections, None); }
  let mut retained : Vec<(SkgRepoName, GraphnodeOnDisk)> = Vec::new ();
  let mut ignored_skgrepos : Vec<SkgRepoName> = Vec::new ();
  for (skgrepo, node_fs) in sections {
    if config . skgrepo_is_owned (&skgrepo) {
      retained . push (( skgrepo, node_fs )); }
    else {
      ignored_skgrepos . push (skgrepo); }}
  let warning : Option<IgnoredForeignPidFolderlision> =
    if ignored_skgrepos . is_empty () { None }
    else { Some ( IgnoredForeignPidFolderlision { ignored_skgrepos } ) };
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
        expected, actual, skgrepo } =>
        write! ( f,
          "Telescope '{}' contains a section from repo '{}' whose embedded pid is '{}'.",
          expected, skgrepo, actual ),
      TelescopeConstructionError::UnknownSkgRepo { skgrepo } =>
        write! ( f,
          "Telescope contains a section from unconfigured repo '{}'.",
          skgrepo ),
      TelescopeConstructionError::DuplicateSkgRepo { skgrepo } =>
        write! ( f,
          "Telescope contains more than one section from repo '{}'.",
          skgrepo ),
      TelescopeConstructionError::OutOfOrder { previous, next } =>
        write! ( f,
          "Telescope sections are out of privacy order: '{}' precedes '{}'.",
          previous, next ), }} }

impl std::error::Error for TelescopeConstructionError {
}

impl Telescope {
  pub fn try_new (
    pid      : ID,
    sections : Vec<(SkgRepoName, GraphnodeOnDisk)>,
    config   : &SkgConfig,
  ) -> Result<Telescope, TelescopeConstructionError> {
    if sections . is_empty () {
      return Err ( TelescopeConstructionError::Empty { pid } ); }
    let positions : HashMap<SkgRepoName, usize> =
      config . ordered_skgrepos () . into_iter () . enumerate ()
      . map ( |(position, skgrepo)| (skgrepo, position) )
      . collect ();
    let mut seen_skgrepos : HashSet<SkgRepoName> = HashSet::new ();
    let mut previous : Option<(usize, SkgRepoName)> = None;
    for (skgrepo, node_fs) in &sections {
      if node_fs . pid != pid {
        return Err ( TelescopeConstructionError::MixedPid {
          expected : pid,
          actual   : node_fs . pid . clone (),
          skgrepo  : skgrepo . clone (), } ); }
      let position : usize = * positions . get (skgrepo)
        . ok_or_else ( || TelescopeConstructionError::UnknownSkgRepo {
          skgrepo : skgrepo . clone (), } ) ?;
      if ! seen_skgrepos . insert ( skgrepo . clone () ) {
        return Err ( TelescopeConstructionError::DuplicateSkgRepo {
          skgrepo : skgrepo . clone (), } ); }
      if let Some ((previous_position, previous_skgrepo)) = &previous {
        if *previous_position >= position {
          return Err ( TelescopeConstructionError::OutOfOrder {
            previous : previous_skgrepo . clone (),
            next     : skgrepo . clone (), } ); }}
      previous = Some (( position, skgrepo . clone () )); }
    Ok ( Telescope { pid, sections } ) }

  pub fn pid (
    &self,
  ) -> &ID {
    &self . pid }

  /// The most public section's skgrepo.
  pub fn home (
    &self,
  ) -> &SkgRepoName {
    & self . sections . first ()
      . expect ("Telescope construction guarantees a section") . 0 }

  pub fn sections (
    &self,
  ) -> &[(SkgRepoName, GraphnodeOnDisk)] {
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

  /// Every flag any section carries, first
  /// occurrence first. Unioned defensively, like 'extra_ids'.
  pub fn flags (
    &self,
  ) -> Vec<Flag> {
    let mut flags : Vec<Flag> = Vec::new ();
    for (_, node_fs) in &self . sections {
      for m in &node_fs . flags {
        if ! flags . contains (m) {
          flags . push ( m . clone () ); }} }
    flags }

  /// The sections in the form the composition consumes, order preserved.
  pub fn into_slices (
    self,
  ) -> Vec<(SkgRepoName, SectionSlices)> {
    self . sections . into_iter ()
      . map ( |(skgrepo, node_fs)|
              (skgrepo, node_fs . into_section_slices ()) )
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
  /// Names a member of the strictly-more-public compose; the items
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
      fn into_skgid (self) -> ID {
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
        ListItem::Anchor { anchor : anchor . into_skgid () },
      Raw::Member (s) =>
        ListItem::Member ( s . into_skgid () ), } ) }}

/// What one section contributes to its node, in section-local form.
/// This is the PARSED shape of a section file's list fields; the
/// serde wiring of GraphnodeOnDisk to this shape lands with the rest of the
/// section format. Ordered relations carry items (anchors legal);
/// unordered relations and aliases carry plain members.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct SectionSlices {
  pub title                        : Option<String>,
  pub body                         : Option<String>,
  pub aliases                      : Option<Vec<String>>,
  pub contains                     : Option<Vec<ListItem>>,
  pub subscribesTo                 : Option<Vec<ListItem>>,
  pub hidesFromSubs                : Option<Vec<ID>>,
  pub overrides                    : Option<Vec<ID>>,
}

/// Nonfatal compose trouble. The compose is TOTAL: junk degrades to one of
/// these, never to an error or a panic, because a dangling anchor
/// can arise from two perfectly correct saves on different machines
/// (see the plan's "Dangling anchors" section).
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CompositionWarning {
  /// An anchor named no member of the strictly-more-public compose.
  /// Its run attached after the preceding run (or the prepend).
  DanglingAnchor { anchor : ID },
  /// An anchor appeared in the most public section that mentions
  /// the relation -- there is no more-public compose to anchor into.
  /// Handled exactly like a dangling anchor.
  AnchorInBase { anchor : ID },
  /// The same member appeared in two skgrepos; the more public
  /// occurrence won.
  DuplicateMember { member : ID },
  /// The home -- the most public section -- carries no title, so
  /// the text sits at 'title_at', where a reader restricted to the
  /// home skgrepo cannot see it. Distinct from 'NonHomeTitle' (a
  /// stray SECOND title) and 'MissingTitle' (no title anywhere).
  TitleBelowHome {
    home     : crate::types::misc::SkgRepoName,
    title_at : crate::types::misc::SkgRepoName,
  },
  /// The selected body is below the home. Title and body select
  /// independently, so this may occur with a title at home.
  BodyBelowHome {
    home    : crate::types::misc::SkgRepoName,
    body_at : crate::types::misc::SkgRepoName,
  },
  /// A section below the one holding the title carried a title too;
  /// the more public one won.
  NonHomeTitle {
    skgrepo      : crate::types::misc::SkgRepoName,
    selected_at : crate::types::misc::SkgRepoName,
  },
  /// A later section carried a body; the more-public selected body won.
  NonHomeBody {
    skgrepo      : crate::types::misc::SkgRepoName,
    selected_at : crate::types::misc::SkgRepoName,
  },
  /// No section carried a title.
  MissingTitle,
}

impl std::fmt::Display for CompositionWarning {
  fn fmt (
    &self,
    f : &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    match self {
      CompositionWarning::DanglingAnchor { anchor } =>
        write! ( f,
          "dangling anchor '{}': it named no member of any more public section, so its run attached after the preceding run (or the prepend)",
          anchor ),
      CompositionWarning::AnchorInBase { anchor } =>
        write! ( f,
          "anchor '{}' appeared in the most public section mentioning its relation, where there is no more public fold to anchor into; handled like a dangling anchor",
          anchor ),
      CompositionWarning::DuplicateMember { member } =>
        write! ( f,
          "member '{}' appeared in two repos; the more public occurrence won",
          member ),
      CompositionWarning::TitleBelowHome { home, title_at } =>
        write! ( f,
          "title below the home: the home '{}' carries no title, so this node's text sits at '{}', invisible to anyone reading at '{}'. A node's text belongs in its most public section. A save of this node is refused until the files are repaired by hand: either move the title up to '{}', or delete the '{}' section if it holds nothing else.",
          home, title_at, home, home, home ),
      CompositionWarning::BodyBelowHome { home, body_at } =>
        write! ( f,
          "body below the home: the home is '{}', but the selected body sits at '{}'; restricted readers at '{}' cannot see it",
          home, body_at, home ),
      CompositionWarning::NonHomeTitle { skgrepo, selected_at } =>
        write! ( f,
          "section '{}' carried a later title; the more public title selected from '{}' won",
          skgrepo, selected_at ),
      CompositionWarning::NonHomeBody { skgrepo, selected_at } =>
        write! ( f,
          "section '{}' carried a later body; the more public body selected from '{}' won",
          skgrepo, selected_at ),
      CompositionWarning::MissingTitle =>
        write! ( f, "no section carried a title" ), }}}

#[cfg(test)]
mod telescope_construction_tests {
  use super::{Telescope, TelescopeConstructionError};
  use crate::types::misc::{
    ID, SkgConfig, SkgRepo, SkgRepoName};
  use crate::types::nodes::fs::GraphnodeOnDisk;

  use std::collections::HashMap;
  use std::path::PathBuf;

  fn skgrepo (
    name : &str,
  ) -> SkgRepoName {
    SkgRepoName::from (name) }

  fn node_fs (
    pid : &str,
  ) -> GraphnodeOnDisk {
    GraphnodeOnDisk {
      title                        : Some ("title" . to_string ()),
      aliases                      : Vec::new (),
      pid                          : ID::from (pid),
      extra_ids                    : Vec::new (),
      body                         : None,
      contains                     : Vec::new (),
      subscribesTo                 : Vec::new (),
      hidesFromSubs                : Vec::new (),
      overrides                    : Vec::new (),
      flags                        : Vec::new (), } }

  fn config () -> SkgConfig {
    let ordered : Vec<SkgRepoName> =
      ["public", "trusted", "private"] . into_iter ()
      . map (skgrepo) . collect ();
    let skgrepos : HashMap<SkgRepoName, SkgRepo> =
      ordered . iter () . cloned ()
      . map ( |name| {
        ( name . clone (),
          SkgRepo {
            path         : PathBuf::from ( &name . 0 ),
            name,
            abbreviation : None,
            owned        : true, } ) } )
      . collect ();
    let mut config : SkgConfig =
      SkgConfig::dummyFromSkgRepos (skgrepos);
    config . skgrepo_order = ordered;
    config }

  #[test]
  fn checked_constructor_accepts_only_the_documented_shape () {
    let config : SkgConfig = config ();
    let valid : Telescope = Telescope::try_new (
      ID::from ("N"),
      vec! [ (skgrepo ("public"), node_fs ("N")),
             (skgrepo ("private"), node_fs ("N")) ],
      &config ) . unwrap ();
    assert_eq! ( valid . pid (), &ID::from ("N") );
    assert_eq! ( valid . home (), &skgrepo ("public") );
    assert_eq! ( valid . sections () . len (), 2 );

    assert! ( matches! (
      Telescope::try_new ( ID::from ("N"), Vec::new (), &config ),
      Err (TelescopeConstructionError::Empty { .. }) ));
    assert! ( matches! (
      Telescope::try_new (
        ID::from ("N"),
        vec! [ (skgrepo ("private"), node_fs ("N")),
               (skgrepo ("public"), node_fs ("N")) ],
        &config ),
      Err (TelescopeConstructionError::OutOfOrder { .. }) ));
    assert! ( matches! (
      Telescope::try_new (
        ID::from ("N"),
        vec! [ (skgrepo ("public"), node_fs ("N")),
               (skgrepo ("public"), node_fs ("N")) ],
        &config ),
      Err (TelescopeConstructionError::DuplicateSkgRepo { .. }) ));
    assert! ( matches! (
      Telescope::try_new (
        ID::from ("N"),
        vec! [ (skgrepo ("unknown"), node_fs ("N")) ],
        &config ),
      Err (TelescopeConstructionError::UnknownSkgRepo { .. }) ));
    assert! ( matches! (
      Telescope::try_new (
        ID::from ("N"),
        vec! [ (skgrepo ("public"), node_fs ("other")) ],
        &config ),
      Err (TelescopeConstructionError::MixedPid { .. }) ));
  }
}
