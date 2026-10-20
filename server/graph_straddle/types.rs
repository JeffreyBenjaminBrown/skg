//! The types of the graph straddle ('server/graph_straddle/mod.rs'):
//! the straddle itself, with the sets it computes once per straddle
//! (C, the change-forest leaves) and the diff view's cache of each
//! buffer's PCSVF sets ('PcsvfCache'), which it keeps but does not
//! read; the
//! presence of a node or relationship in its two graphs; a node's own
//! changes ('NodeChanges'); a partner over the union of the two graphs
//! ('Partner'); and the two halves of C ('ChangedPids').

use crate::diff_view::change_leaves::ChangeLeaves;
use crate::diff_view::pcsvf::PcsvfCache;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::git::Sign;
use crate::types::misc::{ID, SkgrepoChange, SkgrepoName};
use crate::types::nodes::complete::Flag;

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
  /// The diff view's per-buffer sets; see 'pcsvf_sets_of_buffer'.
  /// Kept here so that they persist with the straddle across requests.
  pub(crate) pcsvf_cache : PcsvfCache,
}

/// Which graphs of the straddle hold a node or a relationship.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Presence {
  Both,
  OnlyBefore,
  OnlyAfter,
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
  pub presence       : Presence,
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
                pcsvf_cache   : PcsvfCache::default () } }

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

  /// The sign of the '(diff ...)' atom for this presence: + for only
  /// after, - for only before, none for both.
  pub fn diff_sign (
    self,
  ) -> Option<Sign> {
    match self {
      Presence::Both       => None,
      Presence::OnlyBefore => Some (Sign::Minus),
      Presence::OnlyAfter  => Some (Sign::Plus), } }
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
    self . out_changed . union (&self . in_changed) . cloned () . collect () }
}
