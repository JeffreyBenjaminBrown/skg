//! Core values of the privacy telescope (in comments: "telescope" =
//! privacy telescope, "section" = telescope section, "level" =
//! privacy level; the short names are for code).
//!
//! One node = one ID = one telescope: a set of same-ID .skg files,
//! at most one per source ("sections"). Every relationship instance
//! is recorded in exactly one section, whose source is the edge's
//! LEVEL. On disk each ordered relation is ONE flat sequence of
//! items -- members and anchors -- whose role (base list vs
//! placement) follows from WHICH section holds it, not from its
//! shape: the most public section mentioning a relation holds its
//! anchor-free base; a more private section's items before the first
//! anchor are its prepend, and each anchor starts a run inserted
//! after that member of the strictly-more-public fold. See
//! TODO/user-owned_autofork_chain/5_plan.org, work item
//! section-format-and-fold.

use serde::{Serialize, Deserialize};

use crate::types::misc::ID;

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
  /// The same member appeared at two levels; the more public
  /// occurrence won.
  DuplicateMember { member : ID },
  /// The home -- the most public section -- carries no title, so
  /// the text sits at 'title_at', where a reader restricted to the
  /// home's level cannot see it. Distinct from 'NonHomeTitle' (a
  /// stray SECOND title) and 'MissingTitle' (no title anywhere).
  TitleBelowHome {
    home     : crate::types::misc::SourceName,
    title_at : crate::types::misc::SourceName,
  },
  /// A section below the one holding the title carried a title too;
  /// the more public one won.
  NonHomeTitle { level : crate::types::misc::SourceName },
  /// A non-home section carried a body; the home's won.
  NonHomeBody { level : crate::types::misc::SourceName },
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
          "member '{}' appeared at two levels; the more public occurrence won",
          member ),
      FoldWarning::TitleBelowHome { home, title_at } =>
        write! ( f,
          "title below the home: the home '{}' carries no title, so this node's text sits at '{}', invisible to anyone reading at '{}'. A node's text belongs in its most public section. A save of this node is refused until the files are repaired by hand: either move the title up to '{}', or delete the '{}' section if it holds nothing else.",
          home, title_at, home, home, home ),
      FoldWarning::NonHomeTitle { level } =>
        write! ( f,
          "section '{}' carried a second title; the more public one won",
          level ),
      FoldWarning::NonHomeBody { level } =>
        write! ( f,
          "section '{}' carried a body at a level that does not hold the title",
          level ),
      FoldWarning::MissingTitle =>
        write! ( f, "no section carried a title" ), }}}
