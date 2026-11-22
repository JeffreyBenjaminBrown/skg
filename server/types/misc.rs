//! The small types every part of the server names: 'ID' (a node's
//! UUID), 'SkgrepoName', 'MSV' (a list a save may leave unspecified),
//! 'RelPartner' (a relationship's member with its relRepo, and the
//! helpers that read lists of them), 'RelationshipMemberKey',
//! 'SkgrepoChange' and 'TantivyIndex'.

use serde::{Serialize, Deserialize, Serializer, Deserializer};
use std::fmt;
use std::ops::Deref;
use std::sync::Arc;
use tantivy::Index;
use tantivy::schema::Field;

//
// Type Definitions
//

/// MSV = 'Maybe-Specified Vector'.
/// When the user saves a buffer, this type distinguishes
/// "not specified" (so use whatever is already on disk) from
/// "should be this value" (even if empty).
///
/// ELABORATION:
/// When a user saves an Occurrence, its claiming Occurrence children
/// always define its contents, so MSV does not apply there.
/// But other fields -- e.g. aliases --
/// the user is likely not to mention (in particular,
/// not asking for any changes). This type distinguishes the case
/// where the user did not mention the field from the case
/// where the user wants it empty.
///
/// The folder rule these values encode at save extraction
/// ('server/from_text/local_fieldintent_collection/traverse.rs'
/// enforces it; 'supplement_unspecified_fields_from_disk' consumes
/// it): a buffer with no folder for a field (no aliasFolder or
/// forksFolder under the defining node) emits no intent for that
/// field, which lowers to 'Unspecified' -- no opinion, so the value
/// is filled from disk. A present-but-empty folder emits an empty list,
/// which lowers to 'Specified(vec![])' -- the user explicitly wants
/// the field empty.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum MSV<T> {
  Unspecified,
  Specified (Vec<T>),
}

/// Each node has a random ID. Skg does not check for collisions.
/// So far the IDs are Version 4 UUIDs.
/// .
/// Collisions are astronomically unlikely but not impossible.
/// If UUIDs were perfectly uniformly distributed,
/// and there are fewer than 3.3e15 v4 UUIDs --
/// equivalent to 9 billion people with 415,000 nodes each --
/// then the collision probability is less than 1 in 1e6.
/// (In reality 6 bits of a v4 UUIDs are fixed:
/// 4 for the version, and 2 for the variant indicator.
/// So there's a little less headroom, but still enough.)
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub struct ID ( pub String );

/// Comparison identity for a stored structured-relationship member.
/// Resolvable IDs compare by their canonical PID (so primary and extra IDs
/// remain one node); an unresolved raw ID compares byte-for-byte.  This key is
/// never serialization data: callers keep the original `RelPartner.member`
/// when a relationship is retained or rewritten.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum RelationshipMemberKey {
  ResolvedPid (ID),
  UnresolvedRawId (ID),
}

impl RelationshipMemberKey {
  /// The ID either variant carries.
  pub fn id (
    &self,
  ) -> &ID {
    match self {
      RelationshipMemberKey::ResolvedPid (id)
        | RelationshipMemberKey::UnresolvedRawId (id) => id, } }

  pub fn into_id (
    self,
  ) -> ID {
    match self {
      RelationshipMemberKey::ResolvedPid (id)
        | RelationshipMemberKey::UnresolvedRawId (id) => id, } }

  pub fn is_unresolved (
    &self,
  ) -> bool {
    matches! ( self, RelationshipMemberKey::UnresolvedRawId (_) ) }
}

/// One entry in a node's relationship list: the member and the
/// relationship instance's relRepo (the skgrepo whose telescope
/// section records the relationship). The relRepo is about the
/// relationship, not the member node (a public node can be the member of a
/// private membership). Its default comes from the applicable relRepo rule;
/// 'skg-set-relRepo' may move its privacy anywhere at least as
/// private as that default;
/// renormalization never lowers its privacy (the sticky rule). See
/// 'docs/telescopes.org', "Where new relationships land".
///
/// Every value records the relRepo whose section contains it.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct RelPartner<T> {
  pub relRepo   : SkgrepoName,
  pub member    : T,
}

impl<T> RelPartner<T> {
  pub fn at_relRepo (
    relRepo   : SkgrepoName,
    member    : T,
  ) -> RelPartner<T> {
    RelPartner { relRepo, member }}}

/// Tag every member of a list with one relRepo.
pub fn rel_partners_at_relRepo<T> (
  relRepo   : &SkgrepoName,
  members   : Vec<T>,
) -> Vec<RelPartner<T>> {
  members . into_iter ()
    . map ( |m| RelPartner::at_relRepo (
      relRepo . clone (), m ) )
    . collect () }

/// The values in a list of relation partners, skgrepos dropped.
pub fn members_of<T : Clone> (
  list : &[RelPartner<T>],
) -> Vec<T> {
  list . iter ()
    . map ( |m| m . member . clone () )
    . collect () }

/// 'rel_partners_at_relRepo' lifted over MSV.
pub fn rel_partners_at_relRepo_msv<T> (
  relRepo   : &SkgrepoName,
  msv       : MSV<T>,
) -> MSV<RelPartner<T>> {
  match msv {
    MSV::Unspecified     => MSV::Unspecified,
    MSV::Specified (v)   =>
      MSV::Specified ( rel_partners_at_relRepo (relRepo, v) ), }}

/// 'members_of' lifted over MSV.
pub fn members_msv<T : Clone> (
  msv : &MSV<RelPartner<T>>,
) -> MSV<T> {
  match msv {
    MSV::Unspecified     => MSV::Unspecified,
    MSV::Specified (v)   =>
      MSV::Specified ( members_of (v) ), }}

/// Each skgrepo has a unique name, defined in the SkgConfig,
/// used in Viewnode metadata to track provenance.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct SkgrepoName ( pub String );

impl SkgrepoName {
  /// Reserved sentinel skgrepo for an occurrence whose skgrepo
  /// nothing supplies: the parse gives it to a headline that names
  /// none where 'complete_occurrence_skgrepo' finds none either (a new
  /// headline at top level, say), and validation refuses it
  /// ('viewnode_from_metadata', 'find_buffer_errors_for_saving').
  pub const NOT_FOUND_STR : &'static str = "NOT_FOUND";
  pub fn not_found () -> Self {
    SkgrepoName ( Self::NOT_FOUND_STR . to_string () ) } }

/// A change of skgrepo, from 'old' to 'new': of a node's home, or of
/// a relationship's relRepo.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SkgrepoChange {
  pub old : SkgrepoName,
  pub new : SkgrepoName,
}

#[derive (Clone)]
pub struct TantivyIndex {
  // Associates titles and aliases to paths.
  pub index                     : Arc<Index>,
  /// Long-lived 'IndexReader' shared across all callers. Default
  /// 'ReloadPolicy::OnCommitWithDelay' refreshes automatically after
  /// each tantivy-commit, so this stays current without manual
  /// invalidation. Re-creating an 'IndexReader' on every lookup
  /// (which 'index.reader()' does) is the dominant cost of
  /// per-query Tantivy ID lookups, so callers should prefer this
  /// field over '.index.reader()'.
  pub reader                    : tantivy::IndexReader,
  pub id_field                  : Field,
  pub title_or_alias_field      : Field,
  pub raw_title_field           : Field, // Un-reduced title for is_title=true docs: preserves link syntax (e.g. "[[id:X][label]]") that 'title_or_alias_field' strips to bare labels. Populated only on primary-title docs; empty for alias docs. Used by the 'titles by ids' endpoint so clients can tell link-titles from plain titles.
  pub overPrivateText_telescope_field      : Field,
  pub no_search_matching_field  : Field,
  pub skgrepo_field             : Field,
  pub prominence_source_field : Field,
  pub is_title_field            : Field,
  pub had_id_field              : Field,
  pub body_field                : Field,
}

//
// Implementations
//

impl<T> MSV<T> {
  pub fn is_unspecified (&self) -> bool {
    matches! (self, MSV::Unspecified) }
  /// Returns the inner slice, or empty if Unspecified.
  pub fn or_default (&self) -> &[T] {
    match self {
      MSV::Unspecified  => &[],
      MSV::Specified (v) => v } }
  /// Consumes self, returning the inner Vec or empty.
  pub fn into_vec (self) -> Vec<T> {
    match self {
      MSV::Unspecified  => vec![],
      MSV::Specified (v) => v } }
  /// Ensures the value is Specified, defaulting to empty,
  /// and returns a mutable reference to the inner Vec.
  pub fn ensure_specified (&mut self) -> &mut Vec<T> {
    if matches! (self, MSV::Unspecified) {
      *self = MSV::Specified (Vec::new ()); }
    match self {
      MSV::Specified (v) => v,
      MSV::Unspecified => unreachable! () } }
  /// For serde skip_serializing_if.
  /// Skips both Unspecified and Specified([]).
  pub fn skip_serializing (&self) -> bool {
    match self {
      MSV::Unspecified  => true,
      MSV::Specified (v) => v . is_empty () } } }

impl<T> Default for MSV<T> {
  fn default () -> Self {
    MSV::Unspecified } }

impl<T: Serialize> Serialize for MSV<T> {
  fn serialize<S: Serializer> (
    &self, serializer : S
  ) -> Result<S::Ok, S::Error> {
    match self {
      MSV::Unspecified  =>
        serializer . serialize_none (),
      MSV::Specified (v) =>
        v . serialize (serializer) } } }

impl<'de, T: Deserialize<'de>> Deserialize<'de> for MSV<T> {
  fn deserialize<D: Deserializer<'de>> (
    deserializer : D
  ) -> Result<Self, D::Error> {
    Vec::<T>::deserialize (deserializer)
      . map (MSV::Specified) } }

impl ID {
  pub fn new <S : Into<String>> (s: S) -> Self {
    ID ( s . into () ) }}

impl Deref for ID {
  // lets ID be used like a String in (more?) cases
  type Target = String;
  fn deref (&self) -> &Self::Target {
    &self . 0 }}

impl AsRef<str> for ID {
  fn as_ref (&self) -> &str {
    &self . 0 }}

impl Deref for SkgrepoName {
  // lets RepoName be used like a String
  type Target = String;
  fn deref (&self) -> &Self::Target {
    &self . 0 }}

impl fmt::Display for ID {
  fn fmt ( &self,
            f: &mut fmt::Formatter<'_> )
         -> fmt::Result {
    write! ( f, "{}", self . 0 ) }}

impl fmt::Display for SkgrepoName {
  fn fmt ( &self,
            f: &mut fmt::Formatter<'_> )
         -> fmt::Result {
    write! ( f, "{}", self . 0 ) }}

impl From<String> for ID {
  fn from ( s : String ) -> Self {
    ID (s) }}

impl From<&String> for ID {
  fn from ( s : &String ) -> Self {
    ID ( s . clone () ) }}

impl From<String> for SkgrepoName {
  fn from ( s : String ) -> Self {
    SkgrepoName (s) }}

impl From<&String> for SkgrepoName {
  fn from ( s : &String ) -> Self {
    SkgrepoName ( s . clone () ) }}

impl From <&str> for ID {
  fn from(s: &str) -> Self {
    ID ( s . to_string () ) }}

impl From <&str> for SkgrepoName {
  fn from(s: &str) -> Self {
    SkgrepoName ( s . to_string () ) }}
