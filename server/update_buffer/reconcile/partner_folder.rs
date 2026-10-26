use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::types::misc::{ID, RelPartner, RelationshipMemberKey, SkgConfig, SkgrepoName};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::disk_home::home_from_disk;
use crate::update_buffer::reconcile::omit_restricted_members;
use crate::types::viewnode::Folder;

/// The members a folder of this kind under the viewparent should hold,
/// from 'graph': its partners in the folder's relation, each extra ID
/// resolved to its node's pid (an unresolved ID kept as it is), less
/// the members the skgrepo restriction hides. In graph order. None if
/// the kind has no relation member role. The refresh's goal of such a
/// folder in a content view ('server/refresh/goal.rs').
pub fn partnerFolder_members_in_graph (
  graph               : &InRustGraph,
  config              : &SkgConfig,
  viewparent_pid      : &ID,
  kind                : &Folder,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Option<Vec<ID>> {
  let member_role : RelationRole = kind . relation_member_role () ?;
  let recorder_role : RelationRole =
    member_role . opposite_role ();
  let skgrepo_resolver = |gnid : &ID| -> Option<SkgrepoName> {
    graph . pid_and_skgrepo (gnid)
      . map ( |(_pid, src)| src )
      . or_else ( || home_from_disk (gnid, config) ) };
  let members : Vec<ID> =
    if recorder_role . is_first_role () { // the folder shows a list in the recorder's file
      raw_outbound_members (graph, viewparent_pid, member_role, skgrepo_restriction)
        . into_iter ()
        . map ( |member| member . member . into_id () )
        . collect ()
    } else { graph . other_member_pids_gated (
      viewparent_pid, recorder_role, skgrepo_restriction ) };
  Some ( // Under a skgrepo restriction (the API doc's "Restricted-skgrepo nodes"): these folders omit
         // hidden members. A tombstone child already in the folder is
         // unmanaged, so an ordinary rerender never removes it.
    omit_restricted_members (
      members, skgrepo_restriction, skgrepo_resolver )) }

/// The viewparent's partners in the member role's relation, as its file
/// lists them, each tagged by whether the graph resolves it, if the
/// folder shows a list in the viewparent's file (the viewparent is the
/// recorder); else none.
fn raw_outbound_members (
  graph               : &InRustGraph,
  viewparent_pid      : &ID,
  member_role         : RelationRole,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Vec<RelPartner<RelationshipMemberKey>> {
  if member_role . opposite_role () . is_first_role () {
    // Preserve an unresolved ID through this outbound surface.  The old
    // canonical-PID accessor is still right for inverse/write-protected folders, but
    // would erase an Unknown from the writable overriddenFolder.
    graph . outbound_member_keys_for_relation_gated (
      viewparent_pid, member_role . relation, skgrepo_restriction )
  } else { Vec::new () }}
