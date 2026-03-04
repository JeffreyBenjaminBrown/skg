/// Generic many-to-many map.
///
/// The bimap crate (BiHashMap) enforces strict 1:1 — inserting
/// (id2, same_uri) would evict (id1, same_uri). We need multiple
/// left values mapping to the same right value (e.g. a view has
/// multiple root IDs) and potentially multiple right values for
/// a single left value (e.g. an ID roots multiple views). No
/// well-maintained crate on crates.io provides many-to-many, so
/// we roll this small wrapper over two HashMaps.

use std::collections::{HashMap, HashSet};
use std::hash::Hash;

pub struct ManyToMany<L, R> {
  left_to_right : HashMap<L, HashSet<R>>,
  right_to_left : HashMap<R, HashSet<L>>,
}

impl<L, R> ManyToMany<L, R>
where L : Clone + Eq + Hash,
      R : Clone + Eq + Hash,
{
  pub fn new () -> Self {
    ManyToMany {
      left_to_right : HashMap::new (),
      right_to_left : HashMap::new () }}

  pub fn insert (
    &mut self,
    l : L,
    r : R,
  ) { self . left_to_right
      . entry ( l . clone () )
      . or_default ()
      . insert ( r . clone () );
    self . right_to_left
      . entry (r)
      . or_default ()
      . insert (l); }

  pub fn remove_pair (
    &mut self,
    l : &L,
    r : &R,
  ) { if let Some (rs) = self . left_to_right . get_mut (l)
      { rs . remove (r);
        if rs . is_empty ()
          { self . left_to_right . remove (l); }}
    if let Some (ls) = self . right_to_left . get_mut (r)
      { ls . remove (l);
        if ls . is_empty ()
          { self . right_to_left . remove (r); }}}

  /// Remove a right value and all its associated left values.
  pub fn remove_right (
    &mut self,
    r : &R,
  ) { if let Some (ls)
        = self . right_to_left . remove (r)
      { for l in ls {
          if let Some (rs) = self . left_to_right . get_mut (&l)
            { rs . remove (r);
              if rs . is_empty ()
                { self . left_to_right . remove (&l); }}}}}

  /// Look up all right values for a given left value.
  pub fn get_right (
    &self,
    l : &L,
  ) -> Option<&HashSet<R>> {
    self . left_to_right . get (l) }

  /// Look up all left values for a given right value.
  pub fn get_left (
    &self,
    r : &R,
  ) -> Option<&HashSet<L>> {
    self . right_to_left . get (r) }
}
