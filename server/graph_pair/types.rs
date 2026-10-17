use crate::change_forest::ChangeForest;
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
  /// How many '.skg' files of the foreign skgrepos changed since
  /// HEAD ('foreign_changed_skg_file_count'). Foreign skgrepos are
  /// read at their worktrees in both halves, so the pair shows none
  /// of those changes.
  pub foreign_changed_files : usize,
  /// Computed on first use; see 'GraphPair::changed_pids'.
  pub(super) changed_pids : OnceLock<ChangedPids>,
  /// Computed on first use; see 'GraphPair::change_forest'.
  pub(super) change_forest : OnceLock<ChangeForest>,
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
    before                : Arc<InRustGraph>,
    after                 : Arc<InRustGraph>,
    head_identity         : String,
    foreign_changed_files : usize,
  ) -> GraphPair {
    GraphPair { before, after, head_identity, foreign_changed_files,
                changed_pids  : OnceLock::new (),
                change_forest : OnceLock::new () } }

  /// The change-forest of this pair. Computed once per pair.
  pub fn change_forest (
    &self,
  ) -> &ChangeForest {
    self . change_forest . get_or_init (
      || ChangeForest::from_graph_pair (self) ) }

  /// The nodes whose file changed but whose change-forest has no
  /// leaf: only the order of some list changed.
  pub fn order_only_pids (
    &self,
  ) -> &BTreeSet<ID> {
    self . change_forest () . order_only_pids () }
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
