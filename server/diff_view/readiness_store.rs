//! The commit-readiness store: which change-forest leaves the user
//! has marked commit-ready ('docs/diff-view.org',
//! "Marks, fingerprints and the store"). Each mark keeps the fingerprint of the change it
//! approved; a mark whose fingerprint no longer matches the current
//! change counts as absent. The store lives server-side, in
//! '<data_root>/readiness-marks.json', rewritten after every change.

use crate::diff_view::change_leaves::{ChangeLeaves, LeafKey};

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

pub const READINESS_MARKS_FILE : &str = "readiness-marks.json";

/// See the module comment.
#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize, Deserialize)]
pub struct ReadinessStore {
  #[serde(with = "marks_as_list")]
  pub marks : HashMap<LeafKey, u64>, // the fingerprint each mark approved
}

impl ReadinessStore {
  pub fn is_ready (
    &self,
    key         : &LeafKey,
    fingerprint : u64,
  ) -> bool {
    self . marks . get (key) == Some (&fingerprint) }

  pub fn mark (
    &mut self,
    key         : LeafKey,
    fingerprint : u64,
  ) {
    self . marks . insert (key, fingerprint); }

  pub fn unmark (
    &mut self,
    key : &LeafKey,
  ) {
    self . marks . remove (key); }

  /// Drop every mark whose leaf is not among 'leaves', as when the
  /// changes it approved were git-committed.
  pub fn drop_marks_of_vanished_leaves (
    &mut self,
    leaves : &ChangeLeaves,
  ) {
    self . marks . retain ( |key, _| leaves . leaf (key) . is_some () ); }

  /// The store saved under 'data_root'. A missing or unreadable file
  /// means an empty store.
  pub fn load (
    data_root : &Path,
  ) -> ReadinessStore {
    fs::read_to_string ( readiness_marks_path (data_root) )
      . ok ()
      . and_then ( |text| serde_json::from_str (&text) . ok () )
      . unwrap_or_default () }

  pub fn save (
    &self,
    data_root : &Path,
  ) -> io::Result<()> {
    let text : String =
      serde_json::to_string_pretty (self)
      . map_err ( |e| io::Error::new (io::ErrorKind::InvalidData, e) ) ?;
    fs::write ( readiness_marks_path (data_root), text ) }
}

pub fn readiness_marks_path (
  data_root : &Path,
) -> PathBuf {
  data_root . join (READINESS_MARKS_FILE) }

/// JSON object keys must be strings, so the marks are stored as a
/// list of '{ "leaf": ..., "fingerprint": ... }' entries.
mod marks_as_list {
  use crate::diff_view::change_leaves::LeafKey;

  use serde::{Deserialize, Deserializer, Serialize, Serializer};
  use std::collections::HashMap;

  #[derive(Serialize, Deserialize)]
  struct Mark {
    leaf        : LeafKey,
    fingerprint : u64,
  }

  pub fn serialize<S : Serializer> (
    marks      : &HashMap<LeafKey, u64>,
    serializer : S,
  ) -> Result<S::Ok, S::Error> {
    let mut list : Vec<Mark> =
      marks . iter ()
      . map ( |(leaf, fingerprint)| Mark {
        leaf : leaf . clone (), fingerprint : *fingerprint } )
      . collect ();
    list . sort_by_key ( |mark|
      serde_json::to_string (&mark . leaf) . unwrap_or_default () );
    list . serialize (serializer) }

  pub fn deserialize<'de, D : Deserializer<'de>> (
    deserializer : D,
  ) -> Result<HashMap<LeafKey, u64>, D::Error> {
    let list : Vec<Mark> = Vec::deserialize (deserializer) ?;
    Ok ( list . into_iter ()
         . map ( |mark| (mark . leaf, mark . fingerprint) )
         . collect () ) }
}

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../../tests/unit/readiness_store.rs"]
mod tests;
