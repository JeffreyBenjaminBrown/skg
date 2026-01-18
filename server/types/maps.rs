use std::collections::HashMap;
use std::error::Error;
use std::future::Future;
use std::hash::Hash;

/// Generic function: Ensure the map has a value for the given key.
/// If already present in map, does nothing.
/// Otherwise calls fetch_fn to get the value and adds to map.
pub async fn add_v_to_map_if_absent<K, V, F, Fut> (
  key      : &K,
  map      : &mut HashMap<K, V>,
  fetch_fn : F,
) -> Result < (), Box<dyn Error> >
where
  K   : Eq + Hash + Clone,
  F   : FnOnce(&K) -> Fut,
  Fut : Future<Output = Result<V, Box<dyn Error>>>,
{ if ! map . contains_key ( key )
  { let value : V = fetch_fn (key) . await ?;
    map . insert ( key . clone(),
                   value ); }
  Ok (( )) }
