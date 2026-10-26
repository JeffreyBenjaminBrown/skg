// The goal computers the refresh ('server/refresh/goal.rs') reads.

pub mod content;
pub mod partner_folder;

use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::misc::{ID, SkgrepoName};

/// A goal list as the skgrepo restriction lets it be seen: the members
/// it keeps, and whether it omitted any. A folder whose list the
/// restriction emptied (it omitted some and kept none) is pruned
/// ('server/refresh/tail.rs').
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct RestrictedGoalList {
  pub members      : Vec<ID>,
  pub omitted_some : bool,
}

impl RestrictedGoalList {
  /// Whether the restriction emptied the list.
  pub fn emptied (
    &self,
  ) -> bool {
    self . omitted_some && self . members . is_empty () }
}

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
) -> RestrictedGoalList {
  match restriction . filter ( |a| ! a . is_all () ) {
    None => RestrictedGoalList { members : goal, omitted_some : false },
    Some (a) => {
      let listed : usize = goal . len ();
      let members : Vec<ID> =
        goal . into_iter ()
        . filter ( |gnid|
            resolve (gnid)
            . map ( |src| a . contains_skgrepo (&src) )
            . unwrap_or (false) )
        . collect ();
      RestrictedGoalList { omitted_some : members . len () < listed,
                           members } }, }}
