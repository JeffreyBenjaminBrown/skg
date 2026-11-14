//! The override resolver: given an ID, which node should be shown
//! in its place? Follows the 'overrides' relationships of one owner,
//! the follower ('SkgrepoOwner'), from overridden to overrider,
//! transitively, with a seen-set cycle guard. Override relationships
//! whose overrider is not the follower's do not participate; they are
//! display-only facts (search enrichment, folders, heralds, and paths).
//!
//! A top-level tree of a view follows the overrides of its root's
//! author ('override_follower_of_tree_containing'): a view rooted at a
//! foreign node shows what that node's author would see, throughout the
//! tree, even under a third author's nodes; a view rooted at an owned
//! node follows the user's overrides. A view switched to follow the
//! user's ('OverridesFollowed::User') follows the user's in every tree.
//! Validation of the user's own data
//! ([[./override_invariants.rs]]) always follows the user's.
//!
//! Two gates, applied per relationship:
//! - ownership is set-independent: a relationship is followed only if its
//!   overrider's home skgrepo belongs to the follower
//!   ('SkgConfig::skgrepo_owner'), regardless of the skgrepo restriction.
//! - visibility: a relationship is followed only if the skgrepo
//!   restriction shows both its relRepo and its overrider's home
//!   skgrepo. A restricted relationship cannot affect visible topology,
//!   and a restricted overrider cannot be shown; either one stops the walk
//!   at the last visible node. Callers that ask "what fact would the
//!   server have written, ever?" (the tamper check) pass the
//!   unrestricted skgrepo restriction ('SkgrepoRestriction::unrestricted').
//!
//! A path of any length is normal: an owned override chain
//! (D overrides C overrides N, all owned) resolves to the end of the
//! chain, and any node on the path is a node the server could
//! legitimately show (see 'carrier_on_override_chain_followed_by'). Only a
//! cycle is anomalous — forbidden upstream for owned data by the invariant
//! validators (see [[./override_invariants.rs]]) at save and init/rebuild,
//! and handled here as a backstop: the resolver still terminates and
//! substitutes nothing on a cycle. A multi-overrider hop (a monogamy
//! violation) is likewise forbidden upstream for owned data; the resolver
//! refuses to choose a branch.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::misc::{ID, SkgConfig, SkgrepoName, SkgrepoOwner};
use crate::types::viewnode::{Viewnode, ViewnodeKind, Vognode};
use crate::types::views_state::OverridesFollowed;

use ego_tree::NodeRef;
use std::collections::HashSet;
use std::iter::once;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OverrideResolution {
  /// The PID to show. Equal to the (resolved) input when no
  /// visible overrider of the follower's exists.
  pub effective      : ID,
  /// The chain of overriders traversed, in order. Empty = no
  /// substitution. A path of any length is normal: an override
  /// chain (D overrides C overrides N) resolves to the end
  /// of the chain, and middle carriers are honest (see
  /// 'carrier_on_override_chain_followed_by').
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

/// Walk from 'gnid' along the override relationships whose overriders
/// are 'follower''s, under 'restriction'.
pub fn resolve_override (
  config   : &SkgConfig,
  graph    : &InRustGraph,
  restriction : &SkgrepoRestriction,
  follower : &SkgrepoOwner, // whose overrides the walk follows
  gnid     : &ID,
) -> OverrideResolution {
  let input_pid : ID = // extra-ID safety: resolve before walking
    graph . pid_else_raw_id (gnid);
  let mut seen : HashSet<ID> =
    HashSet::from ( [ input_pid . clone () ] );
  let mut current : ID = input_pid . clone ();
  let mut path : Vec<ID> = Vec::new ();
  loop {
    let candidates : Vec<ID> =
      followable_overriders_of (config, graph, restriction, follower, &current);
    if candidates . len () != 1 {
      // 0: nothing (visible, the follower's) overrides 'current'.
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

/// The owner whose overrides the top-level tree containing 'position'
/// follows, in a view that follows 'overrides_followed'. The user's,
/// in a view that follows the user's. Otherwise the root author's:
/// the owner ('SkgConfig::skgrepo_owner') of the home skgrepo, as
/// 'graph' knows it, of that tree's root, the child of the buffer
/// root; the user when that root is not an occurrence of a node
/// 'graph' knows (a new node, a ghost, a tombstone), or when
/// 'position' is the buffer root itself.
pub fn override_follower_of_tree_containing (
  config             : &SkgConfig,
  graph              : &InRustGraph,
  overrides_followed : OverridesFollowed,
  position           : NodeRef<Viewnode>,
) -> SkgrepoOwner {
  if overrides_followed == OverridesFollowed::User {
    return SkgrepoOwner::User; }
  let tree_root : Option<NodeRef<Viewnode>> =
    once (position) . chain ( position . ancestors () )
    . find ( |node| node . parent () . is_some_and ( |parent|
      matches! ( parent . value () . kind, ViewnodeKind::BufferRoot )));
  let root_home : Option<SkgrepoName> =
    tree_root . and_then ( |root| match &root . value () . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =>
        graph . pid_and_skgrepo (&occurrence . gnid)
        . map ( |(_pid, home)| home ),
      _ => None, } );
  match root_home {
    Some (home) => config . skgrepo_owner (&home),
    None        => SkgrepoOwner::User, }}

/// Whether 'carrier' is a node on 'original''s chain of 'follower''s
/// overriders — i.e. a node the server could legitimately show, marked
/// '(overridesHere original)', wherever 'original' would appear as
/// content in a tree that follows 'follower''s overrides. The tamper
/// check at save uses this: with chains the shown node can be any
/// link of the chain (a middle carrier, when a later
/// link's skgrepo is hidden), not only the end, so it must accept any
/// honest carrier and reject only an off-chain (faked/stale) fact.
/// Visibility-ungated ('unrestricted', the config's unrestricted
/// skgrepo restriction, which the caller builds once) so a fact that
/// was honest when rendered does not start failing after a switch of
/// the skgrepo restriction; 'path' is the follower's full chain
/// (ownership still gates).
pub fn carrier_on_override_chain_followed_by (
  config       : &SkgConfig,
  graph        : &InRustGraph,
  unrestricted : &SkgrepoRestriction,
  follower     : &SkgrepoOwner,
  original     : &ID,   // the fact's N
  carrier      : &ID,   // the shown node's own id
) -> bool {
  resolve_override (config, graph, unrestricted, follower, original)
    . path . contains (carrier) }

/// The overriders of 'pid' that substitution may follow: the relationship's
/// relRepo is unrestricted, and the overrider is both 'follower''s
/// ('SkgConfig::skgrepo_owner' of its home skgrepo) and at an
/// unrestricted home skgrepo. Relationship visibility comes from the same
/// directional gated accessor used by folders, paths, and counts.
/// For 'SkgrepoOwner::User', ownership is that of 'owned_overriders_of' in
/// [[./override_invariants.rs]], which serves validation and so applies no
/// visibility filter.
fn followable_overriders_of (
  config   : &SkgConfig,
  graph    : &InRustGraph,
  restriction : &SkgrepoRestriction,
  follower : &SkgrepoOwner,
  pid      : &ID,
) -> Vec<ID> {
  let mut result : Vec<ID> = Vec::new ();
  for overrider in graph . other_member_pids_gated (
    pid, RelationRole::OVERRIDDEN, restriction ) {
    if let Some (overrider_node) = graph . nodes . get (&overrider) {
      let is_the_followers : bool =
        &config . skgrepo_owner (&overrider_node . home_skgrepo) == follower;
      let home_visible : bool =
        ! SkgrepoRestriction::node_is_restricted (
          restriction, &overrider_node . home_skgrepo );
      if is_the_followers && home_visible {
        result . push (overrider); }} }
  result }
