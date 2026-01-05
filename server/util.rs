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

pub fn path_from_pid_and_source (
  config : &SkgConfig,
  source : &str,
  pid    : ID,
) -> String {
  let source_config =
    config . sources . get ( source )
    . expect ( &format!("Source '{}' not found in config",
                        source) );
  let f : PathBuf = source_config . path . clone() ;
  let s: String = pid.0;
  f . join (s)
    . with_extension ("skg")
    . to_string_lossy ()
    . into_owned ()
}

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
