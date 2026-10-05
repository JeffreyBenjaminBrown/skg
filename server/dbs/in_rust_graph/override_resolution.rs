//! The override resolver: given an ID, which node should be DRAWN
//! in its place? Follows owned 'overrides' relationships from
//! overridden to overrider, transitively, with a seen-set cycle
//! guard. Foreign override relationships never participate in substitution;
//! they are display-only facts (search enrichment, folders, heralds,
//! and paths).
//!
//! Two gates, applied per relationship:
//! - OWNERSHIP is set-independent: a relationship is followed only if its
//!   overrider's skgrepo is owned, regardless of the
//!   active skgrepo-set.
//! - VISIBILITY: when an 'ActiveRepoSet' is supplied, a relationship is
//!   followed only if both its relRepo and its overrider's home skgrepo
//!   are active. An inactive relationship cannot affect visible topology,
//!   and an inactive overrider cannot be drawn; either one stops the walk
//!   at the last visible node. Callers that ask "what marker would the
//!   server have written, ever?" (the tamper check) pass None, i.e.
//!   visibility-ungated.
//!
//! A path of any length is normal: an owned override chain
//! (D overrides C overrides N, all owned) resolves to the end of the
//! chain, and any node on the path is a node the server could
//! legitimately draw (see 'carrier_on_owned_chain'). Only a
//! cycle is anomalous — forbidden upstream by the invariant validators
//! (see [[./override_invariants.rs]]) at save and init/rebuild, and
//! handled here as a backstop: the resolver still terminates and
//! substitutes nothing on a cycle. A multi-overrider hop (monogamy
//! violation) is likewise forbidden upstream; the resolver refuses to
//! choose a branch.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::skgrepo_sets::ActiveSkgRepoSet;
use crate::types::misc::{ID, SkgConfig};

use std::collections::HashSet;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OverrideResolution {
  /// The PID to draw. Equal to the (resolved) input when no
  /// owned, visible overrider exists.
  pub effective      : ID,
  /// The chain of overriders traversed, in order. Empty = no
  /// substitution. A path of any length is normal: an owned
  /// override chain (D overrides C overrides N) resolves to the end
  /// of the chain, and middle carriers are honest (see
  /// 'carrier_on_owned_chain').
  pub path           : Vec<ID>,
  /// True iff the walk met an already-seen PID. In that case no
  /// substitution is performed ('effective' = the input, 'path'
  /// empty), since a cyclic override chain names no sensible
  /// destination. A cycle is forbidden upstream (the invariant
  /// validators reject it at save and init/rebuild); the resolver
  /// handles it as a backstop.
  pub cycle_detected : bool,
  /// The nodes of the detected cycle (the trail from the first repeat
  /// back to it), in walk order; empty when no cycle. The invariant
  /// validators report it as 'OwnedOverrideCycle'.
  pub cycle          : Vec<ID>,
}

pub fn resolve_override (
  config : &SkgConfig,
  graph  : &InRustGraph,
  active : Option<&ActiveSkgRepoSet>,
  skgid  : &ID,
) -> OverrideResolution {
  let input_pid : ID = // extra-ID safety: resolve before walking
    graph . pid_of (skgid)
    . unwrap_or_else ( || skgid . clone () );
  let mut seen : HashSet<ID> =
    HashSet::from ( [ input_pid . clone () ] );
  let mut current : ID = input_pid . clone ();
  let mut path : Vec<ID> = Vec::new ();
  loop {
    let candidates : Vec<ID> =
      followable_overriders_of (config, graph, active, &current);
    if candidates . len () != 1 {
      // 0: nothing (visible, owned) overrides 'current'.
      // >1: monogamy-violating data; refuse to choose a branch.
      // Either way 'current' is the destination.
      return OverrideResolution {
        effective      : current,
        path           : path,
        cycle_detected : false,
        cycle          : Vec::new (), }; }
    let next : ID = candidates [0] . clone ();
    if ! seen . insert ( next . clone () ) {
      let cycle : Vec<ID> = { // the trail from the first repeat back to it
        // The walk in visit order is input_pid followed by 'path';
        // 'next' repeats one of these. The cycle is the suffix from
        // that first occurrence through 'current' (which closes the
        // loop back to 'next').
        let mut walk : Vec<ID> = Vec::with_capacity ( path . len () + 1 );
        walk . push ( input_pid . clone () );
        walk . extend ( path . iter () . cloned () );
        match walk . iter () . position ( |x| x == &next ) {
          Some (i) => walk . split_off (i),
          None     => Vec::new (), }}; // unreachable: 'next' was seen
      return OverrideResolution {
        // A cyclic chain names no sensible destination, so no
        // substitution at all: effective reverts to the input.
        effective      : input_pid,
        path           : Vec::new (),
        cycle_detected : true,
        cycle, }; }
    path . push ( next . clone () );
    current = next; }}

/// Whether 'carrier' is a node ON 'original''s owned override
/// chain — i.e. a node the server could legitimately draw, marked
/// '(overridesHere original)', wherever 'original' would appear as
/// content. The tamper check at save uses this: with chains the drawn
/// node can be any link of the chain (a MIDDLE carrier, when a later
/// link's skgrepo is hidden), not only the end, so it must accept any
/// honest carrier and reject only an off-chain (faked/stale) marker.
/// VISIBILITY-UNGATED ('active' = None) so a marker that was honest
/// when rendered does not start failing after a skgrepo-set switch;
/// 'path' is the full owned chain (ownership still gates).
pub fn carrier_on_owned_chain (
  config   : &SkgConfig,
  graph    : &InRustGraph,
  original : &ID,   // the marker's N
  carrier  : &ID,   // the drawn node's own id
) -> bool {
  resolve_override (config, graph, None, original)
    . path . contains (carrier) }

/// The overriders of 'pid' that substitution may follow: the relationship's
/// relRepo is active, and the overrider is both owned and at an
/// active home skgrepo. Relationship visibility comes from the same
/// directional gated accessor used by folders, paths, and counts.
/// Ownership is modeled on 'owned_overriders_of' in
/// [[./override_invariants.rs]], which serves validation and so applies no
/// visibility filter.
fn followable_overriders_of (
  config : &SkgConfig,
  graph  : &InRustGraph,
  active : Option<&ActiveSkgRepoSet>,
  pid    : &ID,
) -> Vec<ID> {
  let mut result : Vec<ID> = Vec::new ();
  for overrider in graph . other_member_pids_gated (
    pid, RelationRole::OVERRIDDEN, active ) {
    if let Some (overrider_node) = graph . nodes . get (&overrider) {
      let owned : bool =
        config . skgrepos . get (&overrider_node . home_skgrepo)
        . map ( |sc| sc . owned )
        . unwrap_or (false);
      let home_visible : bool =
        active
        . map ( |a| a . is_all ()
                || a . contains_skgrepo (&overrider_node . home_skgrepo) )
        . unwrap_or (true);
      if owned && home_visible {
        result . push (overrider); }} }
  result }
