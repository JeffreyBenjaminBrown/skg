// The per-kind reconcilers view completion (complete_nodes_in_level_order in
// complete.rs) dispatches to. There is no preorder/postorder split: each is run
// at its node's own BFS visit.

pub mod aliasfolder;
pub mod boolprops_folder;
pub mod content;
pub mod hiddeninsubscribee_folder;
pub mod hiddenoutsideof_subscribeefolder;
pub mod id_folder;
pub mod partner_folder;
pub mod subscribee_folder;
pub mod view_requests;

use crate::repo_sets::ActiveRepoSet;
use crate::types::misc::{ID, RepoName};

/// TODO/full-schema/9-2_repo-set-safety.org: rendering omits EVERY
/// inactive member from goal lists (no placeholders are created).  A
/// member whose repo cannot be resolved is omitted too: it might be
/// private, and rendering must not leak; saving preserves it
/// regardless (the weave / set-difference merge treat unresolvable as
/// invisible).  A retained inactive placeholder (one already drawn,
/// kept because it hosts active descendants after a repo-set
/// reduction) does NOT come back through the goal list: each
/// reconciler treats it as an irrelevant child, preserved as-is.
pub fn omit_inactive_members (
  goal     : Vec<ID>,
  active   : Option<&ActiveRepoSet>,
  resolve  : impl Fn (&ID) -> Option<RepoName>,
) -> Vec<ID> {
  match active . filter ( |a| ! a . is_all () ) {
    None => goal,
    Some (a) =>
      goal . into_iter ()
      . filter ( |id|
          resolve (id)
          . map ( |src| a . contains_repo (&src) )
          . unwrap_or (false) )
      . collect (), }}
