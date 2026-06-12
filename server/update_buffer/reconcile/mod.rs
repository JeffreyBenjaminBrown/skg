// The per-kind reconcilers view completion (complete_nodes_in_level_order in
// complete.rs) dispatches to. There is no preorder/postorder split: each is run
// at its node's own BFS visit.

pub mod aliascol;
pub mod content;
pub mod hiddeninsubscribee_col;
pub mod hiddenoutsideof_subscribeecol;
pub mod id_col;
pub mod partner_col;
pub mod subscribee_col;
pub mod view_requests;

use crate::source_sets::ActiveSourceSet;
use crate::types::misc::{ID, SourceName};

/// TODO/full-schema/9-2_source-set-safety.org: rendering omits EVERY
/// inactive member from goal lists (no placeholders are created).  A
/// member whose source cannot be resolved is omitted too: it might be
/// private, and rendering must not leak; saving preserves it
/// regardless (the weave / set-difference merge treat unresolvable as
/// invisible).  A retained inactive placeholder (one already drawn,
/// kept because it hosts active descendants after a source-set
/// reduction) does NOT come back through the goal list: each
/// reconciler treats it as an irrelevant child, preserved as-is.
pub fn omit_inactive_members (
  goal     : Vec<ID>,
  active   : Option<&ActiveSourceSet>,
  resolve  : impl Fn (&ID) -> Option<SourceName>,
) -> Vec<ID> {
  match active . filter ( |a| ! a . is_all () ) {
    None => goal,
    Some (a) =>
      goal . into_iter ()
      . filter ( |id|
          resolve (id)
          . map ( |src| a . contains_source (&src) )
          . unwrap_or (false) )
      . collect (), }}
