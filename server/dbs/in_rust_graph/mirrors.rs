//! Mirrors, and which of a note's mirrors stands in its place
//! ('docs/mirrors.org', "Mirror substitution").
//!
//! A *fork* of a note N is a note that records a relationship of
//! 'MIRROR_RELATION' to N: the forker forks the forkee. The *mirrors*
//! of N are N and every note joined to N by a path of such
//! relationships, followed in either direction, every relationship and
//! every note on the path visible under the skgrepo restriction (the
//! relationship's relRepo, and each note's home skgrepo). Being a
//! mirror is symmetric and transitive, so a cycle of forks is no
//! special case.
//!
//! Where substitution acts, a perspective's owner O decides which of
//! N's mirrors stands in N's place: the first by O's skgrepo list
//! ('SkgConfig::perspective_skgrepo_list'), by home skgrepo. Several
//! mirrors in that first skgrepo tie, and the tie is broken thus:
//! 1. one mirror alone there stands;
//! 2. else, if exactly one of them is forked by none of the others, it
//!    stands (an owned chain D forks C forks N, all in one skgrepo,
//!    shows D);
//! 3. else N itself, if it is one of them;
//! 4. else the one nearest N along forks, in either direction, and
//!    among equals the least pid.
//! The owner need not own the mirror shown.
//!
//! A top-level tree of a view is drawn from the perspective of its
//! root's author ('perspective_owner_of_tree_containing'): a view
//! rooted at a foreign node shows what that node's author would see,
//! throughout the tree, even under a third author's nodes; a view
//! rooted at an owned node, what the user would. A view switched to
//! one owner's perspective ('Perspective::Owner': the user's, or
//! a foreign owner's) is drawn from that owner's in every tree.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::herald_tokens::Side;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::misc::{ID, SkgConfig, SkgrepoName, SkgrepoOwner};
use crate::types::viewnode::{Viewnode, ViewnodeKind, Vognode};
use crate::types::views_state::Perspective;

use ego_tree::NodeRef;
use std::collections::{HashSet, VecDeque};
use std::iter::once;

/// The relation whose relationships join a note to its mirrors: a
/// note that records one of it to N forks N.
pub const MIRROR_RELATION : NodeRelation = NodeRelation::Forks;

/// One mirror of a note N.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Mirror {
  pub pid      : ID,
  pub home     : SkgrepoName,
  /// The fewest fork relationships between it and N, in either
  /// direction. N's own is 0.
  pub distance : usize,
}

/// The mirrors of one note N, and the fork relationships among them.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Mirrors {
  /// N first, then the others nearest first. Empty when the graph
  /// lacks N.
  pub members : Vec<Mirror>,
  /// Each visible relationship between two members, as
  /// '(forker, forkee)'.
  pub forks   : HashSet<(ID, ID)>,
}

impl Mirrors {
  pub fn contains (
    &self,
    pid : &ID,
  ) -> bool {
    self . members . iter () . any ( |mirror| &mirror . pid == pid ) }

  pub fn pids (
    &self,
  ) -> Vec<ID> {
    self . members . iter () . map ( |mirror| mirror . pid . clone () ) . collect () }
}

/// The mirror of the note 'gnid' names that stands in its place,
/// under 'restriction', from the perspective of 'owner'. The note's
/// pid when nothing else does (and 'gnid' itself when the graph lacks
/// it).
pub fn mirror_shown (
  config      : &SkgConfig,
  graph       : &InRustGraph,
  restriction : &SkgrepoRestriction,
  owner       : &SkgrepoOwner,
  gnid        : &ID,
) -> ID {
  let original : ID = graph . pid_else_raw_id (gnid);
  mirror_shown_among (
    config, owner, &original,
    &mirrors_of (graph, restriction, &original) ) }

/// Which of 'mirrors', the mirrors of 'original', stands in its place
/// from the perspective of 'owner': the first by the owner's skgrepo
/// list, ties broken as the module's comment says.
pub fn mirror_shown_among (
  config   : &SkgConfig,
  owner    : &SkgrepoOwner,
  original : &ID,
  mirrors  : &Mirrors,
) -> ID {
  match first_ranked_mirrors (config, owner, mirrors) {
    None => original . clone (),
    Some (FirstRanked::One (pid)) => pid,
    Some (FirstRanked::Tied (tied)) =>
      if tied . iter () . any ( |mirror| &mirror . pid == original ) {
        original . clone ()
      } else {
        tied . into_iter ()
          . min_by ( |a, b| ( a . distance, &a . pid ) . cmp (&( b . distance, &b . pid )) )
          . map ( |mirror| mirror . pid )
          . unwrap_or_else ( || original . clone () ) }, }}

/// What the first two tie-breaking rules make of the mirrors in the
/// first skgrepo of a perspective's skgrepo list that is home to any.
pub enum FirstRanked {
  /// One mirror stands: alone there, or the only one there that none
  /// of the others forks.
  One (ID),
  /// These tie, for the caller to choose among.
  Tied (Vec<Mirror>),
}

/// The members of 'mirrors' that 'owner''s skgrepo list ranks first,
/// with the first two tie-breaking rules applied. None when no member
/// is homed in a skgrepo the config names.
pub fn first_ranked_mirrors (
  config  : &SkgConfig,
  owner   : &SkgrepoOwner,
  mirrors : &Mirrors,
) -> Option<FirstRanked> {
  let first_home : SkgrepoName =
    config . perspective_skgrepo_list (owner) . into_iter ()
    . find ( |skgrepo| mirrors . members . iter ()
             . any ( |mirror| &mirror . home == skgrepo ) ) ?;
  let tied : Vec<Mirror> = mirrors . members . iter ()
    . filter ( |mirror| mirror . home == first_home )
    . cloned () . collect ();
  if tied . len () == 1 {
    return Some ( FirstRanked::One ( tied [0] . pid . clone () )); }
  let forked_by_none_of_the_others : Vec<&Mirror> = tied . iter ()
    . filter ( |mirror| ! tied . iter () . any ( |other|
      other . pid != mirror . pid
      && mirrors . forks . contains (&( other . pid . clone (), mirror . pid . clone () )) ))
    . collect ();
  if forked_by_none_of_the_others . len () == 1 {
    return Some ( FirstRanked::One (
      forked_by_none_of_the_others [0] . pid . clone () )); }
  Some ( FirstRanked::Tied (tied) ) }

/// Whether 'candidate' is a mirror of 'original' under 'restriction'.
/// Either may be a pid or an extra ID.
pub fn is_mirror_of (
  graph       : &InRustGraph,
  restriction : &SkgrepoRestriction,
  original    : &ID,
  candidate   : &ID,
) -> bool {
  mirrors_of (graph, restriction, original)
    . contains ( &graph . pid_else_raw_id (candidate) ) }

/// The mirrors of the note 'gnid' names, under 'restriction'
/// (breadth-first, so each member's distance is its least).
pub fn mirrors_of (
  graph       : &InRustGraph,
  restriction : &SkgrepoRestriction,
  gnid        : &ID,
) -> Mirrors {
  let start : ID = graph . pid_else_raw_id (gnid);
  let Some (start_home) : Option<SkgrepoName> =
    graph . nodes . get (&start) . map ( |node| node . home_skgrepo . clone () )
    else { return Mirrors::default (); };
  let mut mirrors : Mirrors = Mirrors {
    members : vec! [ Mirror { pid : start . clone (), home : start_home, distance : 0 } ],
    forks   : HashSet::new (), };
  let mut seen : HashSet<ID> = HashSet::from ([ start . clone () ]);
  let mut frontier : VecDeque<(ID, usize)> = VecDeque::from ([ ( start, 0 ) ]);
  while let Some ((pid, distance)) = frontier . pop_front () {
    for (partner, fork) in fork_partners_shown (graph, restriction, &pid) {
      mirrors . forks . insert (fork);
      if ! seen . insert ( partner . clone () ) { continue; }
      let home : SkgrepoName = graph . nodes . get (&partner)
        . map ( |node| node . home_skgrepo . clone () )
        . expect ("a partner shown is a node of the graph");
      mirrors . members . push ( Mirror {
        pid : partner . clone (), home, distance : distance + 1 } );
      frontier . push_back (( partner, distance + 1 )); }}
  mirrors }

/// The notes one fork relationship from 'pid', either way, that the
/// skgrepo restriction shows, each with that relationship as
/// '(forker, forkee)': the relationship's relRepo and the partner's
/// home skgrepo are both visible.
fn fork_partners_shown (
  graph       : &InRustGraph,
  restriction : &SkgrepoRestriction,
  pid         : &ID,
) -> Vec<(ID, (ID, ID))> {
  let forkees : Vec<(ID, (ID, ID))> = graph . outbound_pids_for_relation_gated (
    pid, MIRROR_RELATION, restriction ) . into_iter ()
    . map ( |forkee| ( forkee . clone (), ( pid . clone (), forkee ) ))
    . collect ();
  let forkers : Vec<(ID, (ID, ID))> = graph . inbound_pids_for_relation_gated (
    pid, MIRROR_RELATION, restriction ) . into_iter ()
    . map ( |forker| ( forker . clone (), ( forker, pid . clone () ) ))
    . collect ();
  forkees . into_iter () . chain (forkers)
    . filter ( |(partner, _fork)| graph . node_is_shown (partner, restriction) )
    . collect () }

/// The mirrors of each of 'seeds', whatever any skgrepo restriction
/// shows: every note joined to a seed by fork relationships, followed
/// either way, the seeds included.
pub fn mirror_pids_ignoring_restriction (
  graph : &InRustGraph,
  seeds : &HashSet<ID>,
) -> HashSet<ID> {
  let mut seen : HashSet<ID> = seeds . clone ();
  let mut pending : Vec<ID> = seeds . iter () . cloned () . collect ();
  while let Some (pid) = pending . pop () {
    for partner in graph . outbound_pids_for_relation (&pid, MIRROR_RELATION) . into_iter ()
      . chain ( graph . inbound_pids_for_relation (&pid, MIRROR_RELATION) )
    { if seen . insert ( partner . clone () ) { pending . push (partner); }} }
  seen }

/// Which side of 'original' the mirror 'shown' stands on, as the
/// birth fact of a mirror shown in its place says: 'Side::Out' when
/// 'shown' reaches 'original' by forker steps only (it forks a note
/// that ... forks 'original'), 'Side::In' when by forkee steps only,
/// None otherwise (a sibling, say). Where both hold (mutual forks),
/// 'Side::Out'.
pub fn side_of_mirror_shown (
  graph       : &InRustGraph,
  restriction : &SkgrepoRestriction,
  shown       : &ID,
  original    : &ID,
) -> Option<Side> {
  let shown : ID = graph . pid_else_raw_id (shown);
  let original : ID = graph . pid_else_raw_id (original);
  if reaches_by_forkee_steps (graph, restriction, &shown, &original) {
    Some (Side::Out)
  } else if reaches_by_forkee_steps (graph, restriction, &original, &shown) {
    Some (Side::In)
  } else { None }}

/// Whether 'to' is reached from 'from' by following visible fork
/// relationships from forker to forkee only.
fn reaches_by_forkee_steps (
  graph       : &InRustGraph,
  restriction : &SkgrepoRestriction,
  from        : &ID,
  to          : &ID,
) -> bool {
  let mut seen : HashSet<ID> = HashSet::from ([ from . clone () ]);
  let mut pending : Vec<ID> = vec! [ from . clone () ];
  while let Some (pid) = pending . pop () {
    for forkee in graph . outbound_pids_for_relation_gated (
      &pid, MIRROR_RELATION, restriction ) {
      if ! graph . node_is_shown (&forkee, restriction) { continue; }
      if &forkee == to { return true; }
      if seen . insert ( forkee . clone () ) { pending . push (forkee); }} }
  false }

/// The owner from whose perspective the top-level tree containing
/// 'position' is drawn, in a view drawn from 'perspective'.
/// The owner's, in a view switched to one owner's perspective.
/// Otherwise the root author's: the owner ('SkgConfig::skgrepo_owner')
/// of the home skgrepo, as 'graph' knows it, of the node that tree's
/// root, the child of the buffer root, stands for (its '(inPlaceOf
/// N)', else itself: a root another perspective replaced still has its
/// original's author); the user when that root is not an occurrence of
/// a node 'graph' knows (a new node, a ghost, a tombstone), or when
/// 'position' is the buffer root itself.
pub fn perspective_owner_of_tree_containing (
  config             : &SkgConfig,
  graph              : &InRustGraph,
  perspective        : &Perspective,
  position           : NodeRef<Viewnode>,
) -> SkgrepoOwner {
  if let Perspective::Owner (owner) = perspective {
    return owner . clone (); }
  let tree_root : Option<NodeRef<Viewnode>> =
    once (position) . chain ( position . ancestors () )
    . find ( |node| node . parent () . is_some_and ( |parent|
      matches! ( parent . value () . kind, ViewnodeKind::BufferRoot )));
  let root_home : Option<SkgrepoName> =
    tree_root . and_then ( |root| match &root . value () . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =>
        graph . pid_and_skgrepo (
          occurrence . in_place_of . as_ref () . unwrap_or (&occurrence . gnid) )
        . map ( |(_pid, home)| home ),
      _ => None, } );
  match root_home {
    Some (home) => config . skgrepo_owner (&home),
    None        => SkgrepoOwner::User, }}
