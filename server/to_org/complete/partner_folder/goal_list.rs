/// Goal-list computers for the compound partner folders' rerender
/// completers: each returns the ordered list of children the folder
/// should contain.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::node_lookup::graphnode_graphFirst_by_pid_and_skgrepo;
use crate::types::misc::{ID, RelationshipMemberKey, SkgConfig, SkgrepoName, members_of};
use crate::types::disk_home::home_from_disk;

use std::collections::HashSet;

fn relationship_member_key (
  graph : &InRustGraph,
  gnid    : &ID,
) -> RelationshipMemberKey {
  graph . relationship_member_key (gnid)
}

/// Goal list for a hiddenInSubscribeeFolder: the intersection of the
/// subscriber's hides-list and the subscribee's contains-list, in the
/// order of the hides list.
pub fn goal_list_for_hiddenInSubscribee_folder (
  graph                : &InRustGraph,
  subscribee_contains : &[ID],
  subscriber_hides    : &[ID],
) -> Vec<ID> {
  let contains_set : HashSet<RelationshipMemberKey> =
    subscribee_contains . iter ()
    . map (|gnid| relationship_member_key (graph, gnid)) . collect ();
  subscriber_hides . iter ()
    . filter ( |gnid| contains_set . contains (&relationship_member_key (graph, gnid)) )
    . cloned () . collect () }

/// Goal list for a hiddenOutsideOfSubscribeeFolder: the subscriber's
/// hides-list minus everything contained by any subscribee.
pub fn goal_list_for_hiddenOutsideOfSubscribee_folder (
  graph                : &InRustGraph,
  wt_subscriber_hides  : &[ID],
  wt_subscribees       : &[ID],
  config               : &SkgConfig,
) -> Vec<ID> {
  let wt_subscribee_content_of = | pid : &ID | -> Vec<ID> {
    match graph_skgrepo (graph, pid, config) {
      Some (src) =>
        graphnode_graphFirst_by_pid_and_skgrepo ( graph, config, pid, &src )
          . ok ()
          . map ( |skg| members_of (& skg . contains) )
          . unwrap_or_default (),
      None => Vec::new () } };
  let wt_all_subscribee_content : HashSet<RelationshipMemberKey> =
    wt_subscribees . iter ()
      . flat_map ( |pid| wt_subscribee_content_of (pid) )
      . map (|gnid| relationship_member_key (graph, &gnid))
      . collect ();
  wt_subscriber_hides . iter ()
    . filter ( |gnid| ! wt_all_subscribee_content
              . contains (&relationship_member_key (graph, gnid)) )
    . cloned () . collect () }

/// Resolve a node's skgrepo: try the in-Rust graph snapshot first,
/// fall back to scanning skgrepo directories for the matching '.skg'
/// file. The disk fallback handles nodes absent from the current
/// graph snapshot.
fn graph_skgrepo (
  graph  : &InRustGraph,
  pid    : &ID,
  config : &SkgConfig,
) -> Option<SkgrepoName> {
  if let Some (s) =
    graph . pid_and_skgrepo (pid) . map ( |(_, s)| s )
  { return Some (s); }
  home_from_disk (pid, config) }
