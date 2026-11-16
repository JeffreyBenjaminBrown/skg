//! This file defines the membership predicates of save-instruction
//! extraction.
//! .
//! These small functions encode hard-won policy about which buffer
//! positions count as members of the folder their parent
//! represents.
//! .
//! None of them dedups: duplicate content members are a validation
//! error ('claiming_children_have_distinct_gnids'), and duplicate
//! defining-folder members are silently deduplicated at emission.

use crate::types::viewnode::{NodeEditRequest, Occurrence};

/// This returns true iff the given occurrence counts as a
/// member of the writable partner folder (a forksFolder) that is
/// its parent. To count, it must be a member
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

/// This returns true iff the given occurrence, marked for deletion,
/// is an overrider carrying '(overridesHere N)', which, under a parent
/// the user does not own, still stands in that parent's list for N
/// ('content_members' in 'traverse.rs' asks only there). Deleting it
/// deletes the overrider, which the list never named; the list keeps N,
/// so a fork deleted where it stands in place of another's original
/// leaves that original's parent as its author wrote it, unforked, and
/// N is drawn again there. Under a parent the user owns, the deletion
/// still takes N out of the list too, as it did before 2026-11-15
/// (and so, in a subscriber, hides N). Removing the headline removes N
/// from any list, as removing any child does ('docs/forks.org',
/// "Taking back a fork").
pub fn deleted_overrider_keeps_its_original (
  t : &Occurrence,
) -> bool {
  t . claimsMembership
    && t . overridesHere . is_some ()
    && matches!( t . edit_request (),
                 Some (&NodeEditRequest::Delete)) }

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
