/// This file defines the membership predicates of save-instruction
/// extraction.
/// .
/// These small functions encode hard-won policy about which buffer
/// positions count as members of the folder their parent
/// represents.
/// .
/// None of them dedups: duplicate content members are a validation
/// error ('nonignored_children_have_distinct_ids'), and duplicate
/// defining-folder members are silently deduplicated at emission.

use crate::types::viewnode::{NodeEditRequest, AffectsParent, UnrestrictedVognode};

/// This returns true iff the given Unrestricted vognode counts as a
/// member of the writable PartnerFolder (a SubscribeeFolder or
/// OverriddenFolder) that is its parent. To count, it must be a member,
/// not a would-be diff phantom, and not marked for deletion.
pub fn member_counts_for_partnerFolder (
  t : &UnrestrictedVognode,
) -> bool {
  t . affectsParent == AffectsParent::True
    && !t . should_be_diffPhantom ()
    && !matches!( t . edit_request (),
                  Some (&NodeEditRequest::Delete)) }

/// This returns true iff the given Unrestricted vognode counts as content
/// of its parent. The condition coincides with
/// 'member_counts_for_partnerFolder' -- the same three
/// relationship axes govern both -- but the two policies are
/// conceptually distinct, so each keeps its own name.
/// (The caller must also know the child is in content position;
/// that is context, not a fact about the node.)
pub fn unrestricted_child_counts_as_content (
  t : &UnrestrictedVognode,
) -> bool {
  member_counts_for_partnerFolder (t) }

/// This returns true iff the given Unrestricted vognode, a child of a
/// editable subscribee-as-such, counts as visible content of the
/// subscribee. That visible content is the signal from which the
/// subscriber's hides/unhides are inferred.
/// PITFALL: Unlike the other two Unrestricted-vognode predicates, this one
/// has no diff-phantom condition: an Unrestricted node whose diff axes
/// have gone negative still counts as visible here.
pub fn unrestricted_child_counts_as_visible_content (
  t : &UnrestrictedVognode,
) -> bool {
  t . affectsParent == AffectsParent::True
    && !matches!( t . edit_request (),
                  Some (&NodeEditRequest::Delete)) }
