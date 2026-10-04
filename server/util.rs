use crate::types::misc::{ID, SkgConfig, SkgfileRepo, RepoName};
use std::collections::HashSet;
use std::hash::Hash;
use std::path::PathBuf;

pub fn path_from_pid_and_repo (
  config : &SkgConfig,
  repo : &RepoName,
  pid    : ID,
) -> Result < String, String > {
  let repo_config : &SkgfileRepo =
    config . repos . get (repo)
    . ok_or_else ( || format! ("Source '{}' not found in config",
                               repo) ) ?;
  let f : PathBuf = repo_config . path . clone() ;
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
