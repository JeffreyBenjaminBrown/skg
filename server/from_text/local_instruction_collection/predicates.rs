/// This file defines the membership predicates of save-instruction
/// extraction.
/// .
/// These small functions encode hard-won policy about which buffer
/// positions count as members of the collection their parent
/// represents.
/// .
/// None of them dedups: duplicate content members are a validation
/// error ('nonignored_children_have_distinct_ids'), and duplicate
/// defining-col members are silently deduplicated at emission.

use crate::types::git::Sign;
use crate::types::viewnode::{EditRequest, InactiveNode, ParentIs, TrueNode};

/// This returns true iff the given Active vognode counts as a
/// member of the writeable relation collection (a SubscribeeCol or
/// OverriddenCol) that is its parent. To count, it must be Affected,
/// not a would-be diff phantom, and not marked for deletion.
pub fn member_counts_for_relation_collection (
  t : &TrueNode,
) -> bool {
  t . parentIs == ParentIs::Affected
    && !t . should_be_diffPhantom ()
    && !matches!( t . edit_request (),
                  Some (&EditRequest::Delete)) }

/// This returns true iff the given Active vognode counts as content
/// of its parent. The condition coincides with
/// 'member_counts_for_relation_collection' -- the same three
/// membership axes govern both -- but the two policies are
/// conceptually distinct, so each keeps its own name.
/// (The caller must also know the child is in content position;
/// that is context, not a fact about the node.)
pub fn active_child_counts_as_content (
  t : &TrueNode,
) -> bool {
  member_counts_for_relation_collection (t) }

/// This returns true iff the given Active vognode, a child of a
/// definitive subscribee-as-such, counts as visible content of the
/// subscribee. That visible content is the signal from which the
/// subscriber's hides/unhides are inferred.
/// PITFALL: Unlike the other two Active-vognode predicates, this one
/// has no diff-phantom condition: an Active node whose diff axes
/// have gone negative still counts as visible here.
pub fn active_child_counts_as_visible_content (
  t : &TrueNode,
) -> bool {
  t . parentIs == ParentIs::Affected
    && !matches!( t . edit_request (),
                  Some (&EditRequest::Delete)) }

/// This returns true iff the given Inactive vognode should be
/// treated as a phantom -- that is, excluded from its parent's
/// content -- because some membership axis is negative.
#[allow(non_snake_case)]
pub fn inactiveNode_is_phantom (
  inactive : &InactiveNode,
) -> bool {
  inactive . membership . staged == Some (Sign::Minus)
  || inactive . membership . unstaged == Some (Sign::Minus) }
