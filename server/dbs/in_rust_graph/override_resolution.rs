//! The override resolver: given an ID, which node should be DRAWN
//! in its place? Follows user-owned 'overrides_view_of' edges from
//! overridden to overrider, transitively, with a seen-set cycle
//! guard. Foreign override edges never participate in substitution;
//! they are display-only facts (cols, heralds, the override-choice
//! buffer).
//!
//! Two gates, applied per edge:
//! - OWNERSHIP is set-independent: an edge is followed only if its
//!   overrider's source has 'user_owns_it = true', regardless of the
//!   active source-set.
//! - VISIBILITY: when an 'ActiveSourceSet' is supplied, an edge is
//!   followed only if its overrider's source is active. An inactive
//!   overrider cannot be drawn, so it must not substitute; the walk
//!   stops and the last visible node is the effective one. Callers
//!   that ask "what marker would the server have written, ever?"
//!   (the tamper check) pass None, i.e. visibility-ungated.
//!
//! A path of any length is normal: a user-owned override chain
//! (D overrides C overrides N, all owned) resolves to the end of the
//! chain, and any node on the path is a node the server could
//! legitimately draw (see 'carrier_on_user_owned_chain'). Only a
//! cycle is anomalous — forbidden upstream by the invariant validators
//! (see [[./override_invariants.rs]]) at save and init/rebuild, and
//! handled here as a backstop: the resolver still terminates and
//! substitutes nothing on a cycle. A multi-overrider hop (monogamy
//! violation) is likewise forbidden upstream; the resolver refuses to
//! choose a branch.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::source_sets::ActiveSourceSet;
use crate::types::misc::{ID, SkgConfig};

use std::collections::HashSet;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OverrideResolution {
  /// The PID to draw. Equal to the (resolved) input when no
  /// user-owned, visible overrider exists.
  pub effective      : ID,
  /// The chain of overriders traversed, in order. Empty = no
  /// substitution. A path of any length is normal: a user-owned
  /// override chain (D overrides C overrides N) resolves to the end
  /// of the chain, and middle carriers are honest (see
  /// 'carrier_on_user_owned_chain').
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
  /// validators report it as 'UserOwnedOverrideCycle'.
  pub cycle          : Vec<ID>,
}

pub fn resolve_override (
  config : &SkgConfig,
  graph  : &InRustGraph,
  active : Option<&ActiveSourceSet>,
  id     : &ID,
) -> OverrideResolution {
  let input_pid : ID = // extra-ID safety: resolve before walking
    graph . pid_of (id)
    . unwrap_or_else ( || id . clone () );
  let mut seen : HashSet<ID> =
    HashSet::from ( [ input_pid . clone () ] );
  let mut current : ID = input_pid . clone ();
  let mut path : Vec<ID> = Vec::new ();
  loop {
    let candidates : Vec<ID> =
      followable_overriders_of (config, graph, active, &current);
    if candidates . len () != 1 {
      // 0: nothing (visible, user-owned) overrides 'current'.
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

/// Whether 'carrier' is a node ON 'original''s user-owned override
/// chain — i.e. a node the server could legitimately draw, marked
/// '(overridesHere original)', wherever 'original' would appear as
/// content. The tamper check at save uses this: with chains the drawn
/// node can be any link of the chain (a MIDDLE carrier, when a later
/// link's source is hidden), not only the end, so it must accept any
/// honest carrier and reject only an off-chain (faked/stale) marker.
/// VISIBILITY-UNGATED ('active' = None) so a marker that was honest
/// when rendered does not start failing after a source-set switch;
/// 'path' is the full user-owned chain (ownership still gates).
pub fn carrier_on_user_owned_chain (
  config   : &SkgConfig,
  graph    : &InRustGraph,
  original : &ID,   // the marker's N
  carrier  : &ID,   // the drawn node's own id
) -> bool {
  resolve_override (config, graph, None, original)
    . path . contains (carrier) }

/// The overriders of 'pid' that substitution may follow: user-owned
/// (per the config; an unknown source counts as not followable) and,
/// when 'active' is supplied, from an active source. Modeled on
/// 'user_owned_overriders_of' in [[./override_invariants.rs]], which
/// serves validation and so applies no visibility filter.
fn followable_overriders_of (
  config : &SkgConfig,
  graph  : &InRustGraph,
  active : Option<&ActiveSourceSet>,
  pid    : &ID,
) -> Vec<ID> {
  let mut result : Vec<ID> = Vec::new ();
  if let Some (overriders) = graph . overriders_of . get (pid) {
    for overrider in overriders {
      if let Some (overrider_node) = graph . nodes . get (overrider) {
        let user_owned : bool =
          config . sources . get (&overrider_node . source)
          . map ( |sc| sc . user_owns_it )
          . unwrap_or (false);
        let visible : bool =
          active
          . map ( |a| a . is_all ()
                  || a . contains_source (&overrider_node . source) )
          . unwrap_or (true);
        if user_owned && visible {
          result . push ( overrider . clone () ); }}}}
  result }
