/// Generic list diff types and utilities.

use similar::{ChangeTag, TextDiff};
use std::collections::{HashMap, HashSet};

//
// Types
//

/// Result of comparing two lists.
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Default)]
pub struct Diff_as_TwoLists_Lossy<T> {
  /// In new but not old. In git context, items added since HEAD.
  pub added: Vec<T>,
  /// In old but not new. In git context, items removed since HEAD.
  pub removed: Vec<T>,
}

/// An entry in an interleaved diff list.
#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
pub enum Diff_as_OneList_Item<T> {
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
) -> Diff_as_TwoLists_Lossy<T>
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
  Diff_as_TwoLists_Lossy {
    added,
    removed }}

/// Compute an interleaved diff showing removed-here and new-here positions.
/// Uses LCS-based algorithm via the `similar` crate.
pub fn compute_interleaved_diff<T> (
  old : &[T],
  new : &[T],
) -> Vec<Diff_as_OneList_Item<T>>
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
  let mut result : Vec<Diff_as_OneList_Item<T>> =
    Vec::new();
  let mut old_idx : usize = 0;
  let mut new_idx : usize = 0;
  for change in diff . iter_all_changes() {
    match change . tag() {
      ChangeTag::Equal => {
        if new_idx < new . len() {
          result . push ( Diff_as_OneList_Item::Unchanged (
            new[new_idx] . clone() )); }
        old_idx += 1;
        new_idx += 1; },
      ChangeTag::Delete => {
        if old_idx < old . len() {
          result . push ( Diff_as_OneList_Item::RemovedHere (
            old[old_idx] . clone() )); }
        old_idx += 1; },
      ChangeTag::Insert => {
        if new_idx < new . len() {
          result . push ( Diff_as_OneList_Item::NewHere (
            new[new_idx] . clone() )); }
        new_idx += 1; }}}
  result }
