use crate::types::{ID, SkgConfig};
use std::collections::HashSet;
use std::hash::Hash;
use std::path::PathBuf;

pub fn path_from_pid (
  config : &SkgConfig,
  pid    : ID,
) -> String {
  let f : PathBuf = config . skg_folder . clone() ;
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
