use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::{ID, SkgrepoName};
use crate::types::nodes::complete::Flag;

use std::collections::BTreeSet;
use std::sync::{Arc, OnceLock};

/// See 'server/graph_pair/mod.rs'.
pub struct GraphPair {
  pub before        : Arc<InRustGraph>,
  pub after         : Arc<InRustGraph>,
  /// The owned skgrepos' HEAD commits that 'before' was read from
  /// ('owned_head_identity'). When it differs from the current one,
  /// the pair is stale.
  pub head_identity : String,
  /// Computed on first use; see 'GraphPair::changed_pids'.
  pub(super) changed_pids : OnceLock<ChangedPids>,
}

/// Which graphs of the pair hold a node or a relationship.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Presence {
  Both,
  OnlyBefore,
  OnlyAfter,
}

/// The changes to a node present in both graphs that its own files
/// record, other than its relationships (see 'GraphPair::partners').
/// Gained items are in the after-graph's order, lost ones in the
/// before-graph's.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct NodeChanges {
  pub title_changed    : bool,
  pub body_changed     : bool,
  pub aliases_gained   : Vec<String>,
  pub aliases_lost     : Vec<String>,
  pub extra_ids_gained : Vec<ID>,
  pub extra_ids_lost   : Vec<ID>,
  pub flags_gained     : Vec<Flag>,
  pub flags_lost       : Vec<Flag>,
  pub home             : Option<(SkgrepoName, SkgrepoName)>, // (old, new)
}

/// One partner of a node in some role, over the union of the two
/// graphs. A relRepo is None in a graph without the relationship.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Partner {
  pub id             : ID,
  pub presence       : Presence,
  pub relRepo_before : Option<SkgrepoName>,
  pub relRepo_after  : Option<SkgrepoName>,
}

/// The two halves of C, the set of changed graphnodes.
/// Rightward: nodes whose own files record a change, and every new
/// or deleted node. Leftward: nodes whose set of containers,
/// mentioners, subscribers, overriders or hiders changed.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct ChangedPids {
  pub rightward : BTreeSet<ID>,
  pub leftward  : BTreeSet<ID>,
}

impl GraphPair {
  pub fn new (
    before        : Arc<InRustGraph>,
    after         : Arc<InRustGraph>,
    head_identity : String,
  ) -> GraphPair {
    GraphPair { before, after, head_identity,
                changed_pids : OnceLock::new () } }
}

impl Presence {
  /// None when in neither graph.
  pub fn from_membership (
    in_before : bool,
    in_after  : bool,
  ) -> Option<Presence> {
    match (in_before, in_after) {
      (true,  true)  => Some (Presence::Both),
      (true,  false) => Some (Presence::OnlyBefore),
      (false, true)  => Some (Presence::OnlyAfter),
      (false, false) => None, } }
}

impl NodeChanges {
  pub fn text_changed (
    &self,
  ) -> bool {
    self . title_changed || self . body_changed }

  pub fn is_empty (
    &self,
  ) -> bool {
    *self == NodeChanges::default () }
}

impl Partner {
  /// The relationship exists in both graphs, in different skgrepos.
  pub fn relRepo_moved (
    &self,
  ) -> bool {
    self . presence == Presence::Both
      && self . relRepo_before != self . relRepo_after }

  pub fn changed (
    &self,
  ) -> bool {
    self . presence != Presence::Both || self . relRepo_moved () }
}

impl ChangedPids {
  /// C itself.
  pub fn all (
    &self,
  ) -> BTreeSet<ID> {
    self . rightward . union (&self . leftward) . cloned () . collect () }
}
