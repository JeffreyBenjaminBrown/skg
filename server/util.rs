use crate::types::misc::{ID, SkgConfig, SkgRepo, SkgRepoName};
use std::collections::HashSet;
use std::hash::Hash;
use std::path::PathBuf;

pub fn path_from_pid_and_skgrepo (
  config  : &SkgConfig,
  skgrepo : &SkgRepoName,
  pid     : ID,
) -> Result < String, String > {
  let skgrepo_config : &SkgRepo =
    config . skgrepos . get (skgrepo)
    . ok_or_else ( || format! ("Repo '{}' not found in config",
                               skgrepo) ) ?;
  let f : PathBuf = skgrepo_config . path . clone() ;
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
