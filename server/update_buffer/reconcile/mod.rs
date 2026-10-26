// The goal computers the refresh ('server/refresh/goal.rs') reads.

pub mod content;
pub mod partner_folder;

use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::misc::{ID, SkgrepoName};

/// Under a skgrepo restriction (the API doc's "Restricted-skgrepo nodes"): rendering omits every
/// restricted member from goal lists (no placeholders are created).  A
/// member whose skgrepo cannot be resolved is omitted too: it might be
/// private, and rendering must not leak; saving preserves it
/// regardless (the weave / set-difference merge treat unresolvable as
/// invisible).  A tombstone standing where a now-hidden node was (one
/// kept because it hosts visible descendants after a skgrepo-set
/// reduction) does not come back through the goal list: each
/// reconciler treats it as an unmanaged child, preserved as-is.
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
