/// Generic list diff types and utilities.

use similar::{ChangeTag, TextDiff};
use std::collections::HashMap;

//
// Types
//

/// An entry in a list representing
/// the diff of a 'new' list against an 'old' list.
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, PartialEq)]
pub enum Diff_Item<T> {
  /// It exists in both old and new in same relative position.
  Unchanged(T),
  /// It was in old but is not in new (at least in this position).
  Removed(T),
  /// It is in new but was not in old (at least in this position).
  New(T),
}

//
// Functions
//

/// Compute an interleaved diff showing removed-here and new-here positions.
/// Uses LCS-based algorithm via the `similar` crate.
pub fn compute_interleaved_diff<T> (
  old : &[T],
  new : &[T],
) -> Vec<Diff_Item<T>>
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
  let mut result : Vec<Diff_Item<T>> =
    Vec::new();
  let mut old_idx : usize = 0;
  let mut new_idx : usize = 0;
  for change in diff . iter_all_changes() {
    match change . tag() {
      ChangeTag::Equal => {
        if new_idx < new . len() {
          result . push ( Diff_Item::Unchanged (
            new[new_idx] . clone() )); }
        old_idx += 1;
        new_idx += 1; },
      ChangeTag::Delete => {
        if old_idx < old . len() {
          result . push ( Diff_Item::Removed (
            old[old_idx] . clone() )); }
        old_idx += 1; },
      ChangeTag::Insert => {
        if new_idx < new . len() {
          result . push ( Diff_Item::New (
            new[new_idx] . clone() )); }
        new_idx += 1; }}}
  result }
