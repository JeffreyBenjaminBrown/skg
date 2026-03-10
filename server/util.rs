use crate::types::misc::{ID, SkgConfig, SkgfileSource, SourceName};
use std::collections::HashSet;
use std::hash::Hash;
use std::path::PathBuf;

pub fn path_from_pid_and_source (
  config : &SkgConfig,
  source : &SourceName,
  pid    : ID,
) -> Result < String, String > {
  let source_config : &SkgfileSource =
    config . sources . get (source)
    . ok_or_else ( || format! ("Source '{}' not found in config",
                               source) ) ?;
  let f : PathBuf = source_config . path . clone() ;
  let s: String = pid . 0;
  Ok ( f . join (s)
       . with_extension ("skg")
       . to_string_lossy ()
       . into_owned () )
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
    . filter( |item| !subtracting_set . contains (item) )
    . collect() }
