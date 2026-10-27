/// Pure merge functions letting a save under a restricted
/// skgrepo-set preserve relationship-list members the user could not
/// see (the API doc's "Restricted-skgrepo nodes").
///
/// VOCABULARY (shared by both functions):
/// - A disk member is visible iff its record is: its relRepo is
///   unrestricted ('relationship_member_is_visible'). A visible member
///   has a headline the user could delete: an occurrence, an unknown
///   (an unresolvable raw member) or a Restricted vognode (a member
///   whose node the restriction keeps out).
/// - A disk member is positioned iff it appears in the buffer list.
///   Only visible members ever do: save extraction never puts a
///   member whose record is restricted (an invisible one) into a
///   container's list (see 'content_members' /
///   'partnerFolder_members' in local_fieldintent_collection/
///   traverse.rs), so every invisible disk member is non-positioned
///   and the weave alone decides its presence and position.
/// - A visible disk member absent from the buffer was deleted by
///   the user; an invisible one absent from the buffer was omitted
///   by rendering and must survive.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::misc::{ID, RelPartner, SkgConfig, SkgrepoName};
use crate::types::disk_home::home_from_disk;

use std::collections::{HashMap, HashSet};
use std::hash::Hash;

/// Whether saving may treat this member as visible: its skgrepo
/// resolves (in-Rust graph first, then disk) and is in the unrestricted
/// set.
pub fn member_is_visible (
  graph  : &InRustGraph,
  gnid  : &ID,
  config : &SkgConfig,
  restriction : &SkgrepoRestriction,
) -> bool {
  let skgrepo : Option<SkgrepoName> = {
    let from_graph : Option<SkgrepoName> =
      graph . pid_and_skgrepo (gnid)
      . map ( |(_pid, src)| src );
    from_graph . or_else ( || home_from_disk (gnid, config) ) };
  match skgrepo {
    Some (src) => restriction . contains_skgrepo (&src),
    None       => false, }}

/// Whether a record (a relationship as its recorder's file states it)
/// is visible under the restriction: its relRepo is unrestricted.
/// Whatever its member is, the buffer could show it: as an
/// occurrence, as an unknown when nothing resolves it, or as a
/// Restricted vognode when the restriction keeps out its node's home.
pub fn relationship_member_is_visible (
  member      : &RelPartner<ID>,
  restriction : &SkgrepoRestriction,
) -> bool {
  restriction . contains_skgrepo (&member . relRepo) }

/// The anchored weave, for order-meaningful lists ('contains',
/// 'subscribesTo').  Invisible disk members cling to the visible
/// sibling they followed: each maximal run of invisible,
/// non-positioned disk members attaches to the nearest preceding
/// positioned disk member (or to START), and is emitted right after
/// it, wherever the buffer put it.  Positioned members appear in
/// buffer order; deleted (visible, absent) members disappear; new
/// buffer members carry no run.  With everything visible this is
/// the identity on the buffer list.
pub fn weave<T, V> (
  disk_list   : &[T],
  is_visible  : V,
  buffer_list : &[T],
) -> Vec<T>
where T : Eq + Hash + Clone,
      V : Fn (&T) -> bool {
  let positioned : HashSet<&T> =
    buffer_list . iter () . collect ();
  let runs : HashMap<Option<T>, Vec<T>> = {
    // Each woven member is keyed by its anchor: the most recent
    // positioned disk member before it (None = START).  Tracking
    // only positioned members as anchors implements the
    // reattachment rule for free: a run whose visible predecessor
    // was deleted falls through to the previous surviving one.
    let mut runs : HashMap<Option<T>, Vec<T>> = HashMap::new ();
    let mut last_positioned : Option<T> = None;
    for member in disk_list {
      if positioned . contains (member) {
        last_positioned = Some (member . clone ());
      } else if ! is_visible (member) {
        runs . entry (last_positioned . clone ())
          . or_default ()
          . push (member . clone ()); }
      // else: visible and absent from the buffer -- deleted.
    }
    runs };
  let mut result : Vec<T> =
    runs . get (&None) . cloned () . unwrap_or_default ();
  { let mut seen : HashSet<T> = HashSet::new ();
    for member in buffer_list {
      if ! seen . insert (member . clone ()) { continue; }
      result . push (member . clone ());
      if let Some (run) = runs . get (&Some (member . clone ())) {
        result . extend (run . iter () . cloned ()); }}}
  result }

/// The set-difference merge, for order-meaningless lists
/// ('overrides').  The disk list keeps its order; visible
/// members the user deleted are subtracted; new buffer members are
/// appended; nothing else changes -- so reordering is a no-op, the
/// diff stays quiet, and invisible members survive.
pub fn set_difference_merge<T, V> (
  disk_list   : &[T],
  is_visible  : V,
  buffer_list : &[T],
) -> Vec<T>
where T : Eq + Hash + Clone,
      V : Fn (&T) -> bool {
  let in_buffer : HashSet<&T> =
    buffer_list . iter () . collect ();
  let kept : Vec<T> =
    disk_list . iter ()
    . filter ( |m| in_buffer . contains (m) || ! is_visible (m) )
    . cloned ()
    . collect ();
  let mut result : Vec<T> = kept;
  { let on_disk : HashSet<&T> =
      disk_list . iter () . collect ();
    let mut seen : HashSet<T> = HashSet::new ();
    for member in buffer_list {
      if on_disk . contains (member) { continue; }
      if seen . insert (member . clone ()) {
        result . push (member . clone ()); }}}
  result }

#[cfg(test)]
#[path = "../../tests/unit/weave.rs"]
mod tests;
