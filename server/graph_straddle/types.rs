//! The types of the graph straddle ('server/graph_straddle/mod.rs'):
//! the straddle itself, with the sets it computes once per straddle
//! (C, the change-forest leaves) and the diff view's cache of each
//! buffer's pruned change view sets ('PrunedChangeViewCache'), which it
//! keeps but does not read; the existence change of a node or
//! relationship across its two graphs; a node's own changes ('NodeChanges'); a partner over
//! the union of the two graphs ('Partner'); and the two halves of C
//! ('ChangedPids').

use crate::diff_view::change_leaves::ChangeLeaves;
use crate::diff_view::pruned_change_view::PrunedChangeViewCache;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::{ID, SkgrepoChange, SkgrepoName};
use crate::types::nodes::complete::Flag;
use crate::types::viewnode::Existence;

use std::collections::BTreeSet;
use std::sync::{Arc, OnceLock};

/// A straddle stands with one foot in the committed graph and one in
/// the live graph. See 'server/graph_straddle/mod.rs'.
pub struct GraphStraddle {
  pub before        : Arc<InRustGraph>,
  pub after         : Arc<InRustGraph>,
  /// The owned skgrepos' HEAD commits that 'before' was read from
  /// ('owned_head_identity'). When it differs from the current one,
  /// the straddle is stale.
  pub head_identity : String,
  /// How many '.skg' files of the foreign skgrepos changed since
  /// HEAD ('foreign_changed_skg_file_count'). Foreign skgrepos are
  /// read at their worktrees in both halves, so the straddle shows none
  /// of those changes.
  pub foreign_changed_files : usize,
  /// Computed on first use; see 'GraphStraddle::changed_pids'.
  pub(super) changed_pids : OnceLock<ChangedPids>,
  /// Computed on first use; see 'GraphStraddle::change_leaves'.
  pub(super) change_leaves : OnceLock<ChangeLeaves>,
  /// The diff view's per-buffer sets; see 'prunedChangeView_sets_of_buffer'.
  /// Kept here so that they persist with the straddle across requests.
  pub(crate) prunedChangeView_cache : PrunedChangeViewCache,
}

/// The changes to a node present in both graphs that its own files
/// record, other than its relationships (see 'GraphStraddle::partners').
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
  pub home             : Option<SkgrepoChange>,
}

/// One partner of a node in some role, over the union of the two
/// graphs. A relRepo is None in a graph without the relationship.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Partner {
  pub id             : ID,
  /// The relationship's existence change; None when both graphs hold it.
  pub existence      : Option<Existence>,
  pub relRepo_before : Option<SkgrepoName>,
  pub relRepo_after  : Option<SkgrepoName>,
}

/// The two halves of C, the set of changed graphnodes.
/// 'out_changed': nodes whose own files record a change, and every new
/// or deleted node. 'in_changed': nodes whose set of containers,
/// mentioners, subscribers, overriders or hiders changed.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct ChangedPids {
  pub out_changed : BTreeSet<ID>,
  pub in_changed  : BTreeSet<ID>,
}

impl GraphStraddle {
  pub fn new (
    before                : Arc<InRustGraph>,
    after                 : Arc<InRustGraph>,
    head_identity         : String,
    foreign_changed_files : usize,
  ) -> GraphStraddle {
    GraphStraddle { before, after, head_identity, foreign_changed_files,
                changed_pids  : OnceLock::new (),
                change_leaves : OnceLock::new (),
                prunedChangeView_cache   : PrunedChangeViewCache::default () } }

  /// The change-forest of this straddle. Computed once per straddle.
  pub fn change_leaves (
    &self,
  ) -> &ChangeLeaves {
    self . change_leaves . get_or_init (
      || ChangeLeaves::from_graph_straddle (self) ) }

  /// The nodes whose file changed but whose change-forest has no
  /// leaf: only the order of some list changed.
  pub fn order_only_pids (
    &self,
  ) -> &BTreeSet<ID> {
    self . change_leaves () . order_only_pids () }

  /// The home skgrepo of the node 'gnid' names: the worktree's, or
  /// HEAD's when the worktree lacks the node. What the diff view asks
  /// of the restriction about a node ('node_is_restricted').
  pub fn home_in_worktree_or_head (
    &self,
    gnid : &ID,
  ) -> Option<SkgrepoName> {
    self . after . pid_and_skgrepo (gnid)
      . or_else ( || self . before . pid_and_skgrepo (gnid) )
      . map ( |(_pid, home)| home ) }
}

/// The existence change of an item of the union of the straddle's
/// two graphs, as a '(diff ...)' form shows it: added when only the
/// after-graph holds it, removed when only the before-graph does,
/// None when both do (or neither).
pub fn existence_across_straddle (
  in_before : bool,
  in_after  : bool,
) -> Option<Existence> {
  match (in_before, in_after) {
    (true,  false) => Some (Existence::Removed),
    (false, true)  => Some (Existence::Added),
    _              => None, } }

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
    self . existence . is_none ()
      && self . relRepo_before != self . relRepo_after }

  pub fn changed (
    &self,
  ) -> bool {
    self . existence . is_some () || self . relRepo_moved () }
}

impl ChangedPids {
  /// C itself.
  pub fn all (
    &self,
  ) -> BTreeSet<ID> {
    self . out_changed . union (&self . in_changed) . cloned () . collect () }
}
