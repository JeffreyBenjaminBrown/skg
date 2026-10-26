/// This file defines the membership predicates of save-instruction
/// extraction.
/// .
/// These small functions encode hard-won policy about which buffer
/// positions count as members of the folder their parent
/// represents.
/// .
/// None of them dedups: duplicate content members are a validation
/// error ('claiming_children_have_distinct_gnids'), and duplicate
/// defining-folder members are silently deduplicated at emission.

use crate::types::viewnode::{NodeEditRequest, Occurrence};

/// This returns true iff the given occurrence counts as a
/// member of the writable partner folder (a subscribeeFolder or
/// overriddenFolder) that is its parent. To count, it must be a member
/// and not marked for deletion.
pub fn member_counts_for_partnerFolder (
  t : &Occurrence,
) -> bool {
  t . claimsMembership
    && !matches!( t . edit_request (),
                  Some (&NodeEditRequest::Delete)) }

/// This returns true iff the given occurrence counts as content
/// of its parent. The condition coincides with
/// 'member_counts_for_partnerFolder', but the two policies are
/// conceptually distinct, so each keeps its own name.
/// (The caller must also know the child is in content position;
/// that is context, not a fact about the node.)
pub fn occurrence_child_counts_as_content (
  t : &Occurrence,
) -> bool {
  member_counts_for_partnerFolder (t) }

/// This returns true iff the given occurrence, a child of a
/// editable subscribee-as-such, counts as visible content of the
/// subscribee. That visible content is the signal from which the
/// subscriber's hides/unhides are inferred.
pub fn occurrence_child_counts_as_visible_content (
  t : &Occurrence,
) -> bool {
  t . claimsMembership
    && !matches!( t . edit_request (),
                  Some (&NodeEditRequest::Delete)) }
