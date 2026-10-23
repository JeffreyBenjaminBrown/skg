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

use crate::types::viewnode::{NodeEditRequest, Membership, UnrestrictedVognode};

/// This returns true iff the given Unrestricted vognode counts as a
/// member of the writable PartnerFolder (a SubscribeeFolder or
/// OverriddenFolder) that is its parent. To count, it must be a member
/// and not marked for deletion.
pub fn member_counts_for_partnerFolder (
  t : &UnrestrictedVognode,
) -> bool {
  t . membership == Membership::Member
    && !matches!( t . edit_request (),
                  Some (&NodeEditRequest::Delete)) }

/// This returns true iff the given Unrestricted vognode counts as content
/// of its parent. The condition coincides with
/// 'member_counts_for_partnerFolder', but the two policies are
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
pub fn unrestricted_child_counts_as_visible_content (
  t : &UnrestrictedVognode,
) -> bool {
  t . membership == Membership::Member
    && !matches!( t . edit_request (),
                  Some (&NodeEditRequest::Delete)) }
