use crate::types::misc::{ID, SkgConfig, SkgfileSource, SourceName};
use std::collections::HashSet;
use std::hash::Hash;
use std::path::PathBuf;

/// PITFALL: Looks dead, but actually used
/// by serde's `skip_serializing_if` attribute.
pub fn option_vec_is_empty_or_none<T> (
  option_vec: &Option<Vec<T>>
) -> bool {
  match option_vec {
    None => true,
    Some(vec) => vec.is_empty(), }}

pub fn path_from_pid_and_source (
  config : &SkgConfig,
  source : &SourceName,
  pid    : ID,
) -> String {
  let source_config : &SkgfileSource =
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
