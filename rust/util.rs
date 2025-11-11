use crate::types::misc::{ID, SkgConfig};
use std::collections::HashSet;
use std::hash::Hash;
use std::path::PathBuf;

pub fn option_vec_is_empty_or_none<T> (
  option_vec: &Option<Vec<T>>
) -> bool {
  match option_vec {
    None => true,
    Some(vec) => vec.is_empty(), }}

pub fn path_from_pid (
  config : &SkgConfig,
  pid    : ID,
) -> String {
  // TODO Phase 5: Update signature to accept source parameter
  // For now, use "main" source to get Phase 1 compiling
  let main_source =
    config . sources . get ( "main" )
    . expect ( "Config must have a 'main' source" );
  let f : PathBuf = main_source . path . clone() ;
  let s: String = pid.0;
  f . join (s)
    . with_extension ("skg")
    . to_string_lossy ()
    . into_owned ()
}

/// Append elements in 'other' absent from 'extending' to 'extending'.
pub fn extend_vec_with_novel_from_other_vec<T>(
  extending : &mut Vec<T>,
  other     : Option<&Vec<T>>,
) where T: Clone + Eq + Hash {
  if let Some(other_vec) = other {
    let extending_set: HashSet<T> = (
      extending . iter() . cloned() . collect() );
    let novel_items: Vec<T> =
      other_vec . iter()
      . filter( |item|
                 !extending_set.contains(item) )
      . cloned() . collect();
    extending . extend( novel_items ); }}

/// Removes duplicates from a vector.
/// Preserves the order of first occurrence.
pub fn dedup_vector<T> (
  vec : Vec<T>
) -> Vec<T>
where T: Clone + Eq + Hash {
  let mut seen : HashSet<T> = HashSet::new();
  let mut result : Vec<T> = Vec::new();
  for item in vec {
    if !seen.contains(&item) {
      seen.insert(item.clone());
      result.push(item); }}
  result }

/// Removes from 'subtracting_from' anything in 'subtracting'.
/// Preserves the order of elements in 'subtracting_from'.
pub fn setlike_vector_subtraction<T> (
  subtracting_from : Vec<T>,
  subtracting      : &[T],
) -> Vec<T>
where T: Clone + Eq + Hash {
  let subtracting_set : HashSet<T> =
    subtracting . iter() . cloned() . collect();
  subtracting_from . into_iter()
    . filter( |item| !subtracting_set.contains(item) )
    . collect() }
