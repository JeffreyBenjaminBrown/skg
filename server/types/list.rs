/// Generic list diff types and utilities.

use similar::{Algorithm, ChangeTag, DiffOp, capture_diff_slices};
use std::collections::HashSet;

//
// Types
//

/// An entry in a list representing
/// the diff of a 'new' list against an 'old' list.
#[derive(Debug, Clone, PartialEq)]
pub enum Diff_Item<T> {
  /// It exists in both old and new in same relative position.
  Unchanged (T),
  /// It was in old but is not in new (at least in this position).
  Removed (T),
  /// It is in new but was not in old (at least in this position).
  New (T),
}

//
// Functions
//

/// Compute an interleaved diff showing removed-here and new-here positions.
/// Uses the LCS-based slice diff from the `similar` crate, comparing the
/// items themselves. Distinct items are never conflated, regardless of how
/// many the lists hold.
pub fn compute_interleaved_diff<T> (
  old : &[T],
  new : &[T],
) -> Vec<Diff_Item<T>>
where
  T: Clone + Eq + std::hash::Hash + Ord,
{
  let ops : Vec<DiffOp> =
    capture_diff_slices ( Algorithm::Myers, old, new );
  let mut result : Vec<Diff_Item<T>> =
    Vec::new();
  for op in &ops {
    for change in op . iter_changes ( old, new ) {
      match change . tag() {
        ChangeTag::Equal =>
          result . push ( Diff_Item::Unchanged ( change . value() )),
        ChangeTag::Delete =>
          result . push ( Diff_Item::Removed ( change . value() )),
        ChangeTag::Insert =>
          result . push ( Diff_Item::New ( change . value() )), }}}
  result }

/// From a diff, extract:
/// - itemlist: all items (Unchanged, New, and Removed)
/// - removedset: only the Removed items
pub fn itemlist_and_removedset_from_diff<T> (
  diff : &[Diff_Item<T>],
) -> (Vec<T>, HashSet<T>)
where T: Clone + Eq + std::hash::Hash {
  let mut itemlist : Vec<T> = Vec::new();
  let mut removedset : HashSet<T> = HashSet::new();
  for d in diff {
    match d {
      Diff_Item::Unchanged (x)
      | Diff_Item::New (x) =>
        { itemlist . push( x . clone() ); },
      Diff_Item::Removed (x) =>
        { itemlist . push( x . clone() );
          removedset . insert( x . clone() ); }} }
  (itemlist, removedset) }

/// Removes duplicates from a vector.
/// Preserves the order of first occurrence.
pub fn dedup_vector<T> (
  vec : Vec<T>
) -> Vec<T>
where T: Clone + Eq + std::hash::Hash {
  let mut seen : HashSet<T> = HashSet::new();
  let mut result : Vec<T> = Vec::new();
  for item in vec {
    if !seen . contains (&item) {
      seen . insert(item . clone());
      result . push (item); }}
  result }
