// The per-kind reconcilers view completion (complete_nodes_in_level_order in
// complete.rs) dispatches to. There is no preorder/postorder split: each is run
// at its node's own BFS visit.

pub mod aliasfolder;
pub mod flags_folder;
pub mod content;
pub mod hiddeninsubscribee_folder;
pub mod hiddenoutsideof_subscribeefolder;
pub mod id_folder;
pub mod partner_folder;
pub mod subscribee_folder;
pub mod view_requests;

use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::misc::{ID, SkgrepoName};

/// TODO/DONE/full-schema/DONE/9-2_source-set-safety.org: rendering omits EVERY
/// restricted member from goal lists (no placeholders are created).  A
/// member whose skgrepo cannot be resolved is omitted too: it might be
/// private, and rendering must not leak; saving preserves it
/// regardless (the weave / set-difference merge treat unresolvable as
/// invisible).  A retained restricted vognode (one already drawn,
/// kept because it hosts unrestricted descendants after a skgrepo-set
/// reduction) does NOT come back through the goal list: each
/// reconciler treats it as an irrelevant child, preserved as-is.
pub fn omit_restricted_members (
  goal     : Vec<ID>,
  restriction : Option<&SkgrepoRestriction>,
  resolve  : impl Fn (&ID) -> Option<SkgrepoName>,
) -> Vec<ID> {
  match restriction . filter ( |a| ! a . is_all () ) {
    None => goal,
    Some (a) =>
      goal . into_iter ()
      . filter ( |skgid|
          resolve (skgid)
          . map ( |src| a . contains_skgrepo (&src) )
          . unwrap_or (false) )
      . collect (), }}
