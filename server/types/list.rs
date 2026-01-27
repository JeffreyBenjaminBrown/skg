/// Generic list diff types and utilities.

use similar::{ChangeTag, TextDiff};
use std::collections::{HashMap, HashSet};

//
// Types
//

/// Result of comparing two lists.
#[derive(Debug, Clone, Default)]
pub struct ListDiff<T> {
  /// Items that were added (not in HEAD)
  pub added: Vec<T>,
  /// Items that were removed (in HEAD but not in current)
  pub removed: Vec<T>,
}

/// An entry in an interleaved diff list.
#[derive(Debug, Clone)]
pub enum ListDiffEntry<T> {
  /// Item exists in both old and new at same relative position
  Unchanged(T),
  /// Item was in old but not in new (or moved away)
  RemovedHere(T),
  /// Item is in new but not in old (or moved here)
  NewHere(T),
}

//
// Functions
//

/// Compute a diff between two lists.
/// Returns information about additions and removals.
pub fn diff_lists<T> (
  old : &[T],
  new : &[T],
) -> ListDiff<T>
where
  T: Clone + Eq + std::hash::Hash,
{
  let old_set : HashSet<&T> = // Build sets for quick lookup
    old . iter() . collect();
  let new_set : HashSet<&T> =
    new . iter() . collect();
  let added : Vec<T> = // Items added (in new but not in old)
    new . iter()
      . filter ( |item| ! old_set . contains ( item ) )
      . cloned()
      . collect();
  let removed : Vec<T> = // Items removed (in old but not in new)
    old . iter()
      . filter ( |item| ! new_set . contains ( item ) )
      . cloned()
      . collect();
  ListDiff {
    added,
    removed }}

/// Compute an interleaved diff showing removed-here and new-here positions.
/// Uses LCS-based algorithm via the `similar` crate.
pub fn compute_interleaved_diff<T> (
  old : &[T],
  new : &[T],
) -> Vec<ListDiffEntry<T>>
where
  T: Clone + Eq + std::hash::Hash,
{ // Use the similar crate for efficient LCS-based diff
  // We need to convert to strings for similar, then map back
  // Build strings that represent the sequences for diff
  // Each unique item gets a unique character representation
  let mut item_to_char : HashMap<&T, char> =
    HashMap::new();
  let mut char_counter : u32 =
    0x100; // Start from extended ASCII
  for item in old . iter() . chain ( new . iter() ) {
    if ! item_to_char . contains_key ( item ) {
      item_to_char . insert ( item, char::from_u32 ( char_counter ) . unwrap_or ( '?' ) );
      char_counter += 1; }}
  let old_str : String =
    old . iter()
      . map ( |item| item_to_char . get ( item ) . copied() . unwrap_or ( '?' ) )
      . collect();
  let new_str : String =
    new . iter()
      . map ( |item| item_to_char . get ( item ) . copied() . unwrap_or ( '?' ) )
      . collect();
  let diff : TextDiff<str> = // Use similar crate for the actual diff
    TextDiff::from_chars ( &old_str, &new_str );
  let mut result : Vec<ListDiffEntry<T>> =
    Vec::new();
  let mut old_idx : usize = 0;
  let mut new_idx : usize = 0;
  for change in diff . iter_all_changes() {
    match change . tag() {
      ChangeTag::Equal => {
        if new_idx < new . len() {
          result . push ( ListDiffEntry::Unchanged ( new[new_idx] . clone() )); }
        old_idx += 1;
        new_idx += 1; },
      ChangeTag::Delete => {
        if old_idx < old . len() {
          result . push ( ListDiffEntry::RemovedHere ( old[old_idx] . clone() )); }
        old_idx += 1; },
      ChangeTag::Insert => {
        if new_idx < new . len() {
          result . push ( ListDiffEntry::NewHere ( new[new_idx] . clone() )); }
        new_idx += 1; }}}
  result }
