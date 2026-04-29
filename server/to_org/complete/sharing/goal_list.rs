/// Goal-list computers for sharing-scaffold rerender completers.
///
/// Each function returns `(Vec<ID>, HashSet<ID>)`: the ordered goal
/// list of children the scaffold should contain, and the set of IDs
/// that should appear as phantoms (present at HEAD but absent in the
/// worktree). Outside diff view, the second element is empty.

use crate::git_ops::read_repo::nodecomplete_from_git_head;
use crate::types::git::{SourceDiff, NodeChanges, net_diff_from_per_stage, per_stage_node_changes_for_truenode};
use crate::types::list::{compute_interleaved_diff, itemlist_and_removedset_from_diff, Diff_Item};
use crate::types::memory::nodecomplete_from_memory_or_disk;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::nodes::complete::NodeComplete;

use std::collections::{HashMap, HashSet};

/// Goal list for a SubscribeeCol: the parent's worktree subscribees,
/// optionally diffed against HEAD subscribees so that removed
/// subscriptions appear as phantoms.
pub fn goal_list_for_subscribee_col (
  subscriber_pid       : &ID,
  subscriber_source    : &SourceName,
  source_diffs         : &Option<HashMap<SourceName, SourceDiff>>,
  worktree_subscribees : &[ID],
  config               : &SkgConfig,
) -> (Vec<ID>, HashSet<ID>) {
  let head_subscribees : Option<Vec<ID>> =
    source_diffs . as_ref ()
      . and_then ( |diffs| diffs . get (subscriber_source) )
      . filter ( |sd| sd . is_git_repo )
      . and_then (
         |_| { let skg : NodeComplete =
                 nodecomplete_from_git_head (
                   subscriber_pid, subscriber_source, config )
                 . ok () ?;
               if skg . subscribes_to . is_unspecified () { None }
               else { Some (
                 skg . subscribes_to . into_vec () ) }} );
  match head_subscribees {
    None =>
      (worktree_subscribees . to_vec (), HashSet::new ()),
    Some (head) => {
      let diff : Vec<Diff_Item<ID>> =
        compute_interleaved_diff ( &head, worktree_subscribees );
      itemlist_and_removedset_from_diff (&diff) } } }

/// Goal list for a HiddenInSubscribeeCol: the intersection of the
/// subscriber's hides-list and the subscribee's contains-list,
/// optionally diffed against the HEAD versions of both.
///
/// Uses per-stage contains_diff (via `net_diff_from_per_stage`)
/// rather than the merged `node_changes_for_truenode` view, so that
/// staged-only contains-list changes still produce phantoms.
pub fn goal_list_for_hiddeninsubscribee_col (
  subscribee_pid      : &ID,
  subscribee_source   : &SourceName,
  subscriber_pid      : &ID,
  subscriber_source   : &SourceName,
  subscribee_contains : &[ID],
  subscriber_hides    : &[ID],
  source_diffs        : &Option<HashMap<SourceName, SourceDiff>>,
  config              : &SkgConfig,
) -> (Vec<ID>, HashSet<ID>) {
  let worktree_content : Vec<ID> =
    { // Intersection of subscriber_hides and subscribee_contains,
      // preserving order from subscriber_hides.
      let subscribee_contains_set : HashSet<ID> =
        subscribee_contains . iter () . cloned () . collect ();
      subscriber_hides . iter ()
        . filter ( |id| subscribee_contains_set . contains (id) )
        . cloned () . collect () };
  if source_diffs . is_none () {
    return (worktree_content, HashSet::new ()); }
  let (staged_nc, unstaged_nc)
    : (Option<&NodeChanges>, Option<&NodeChanges>) =
    per_stage_node_changes_for_truenode (
      source_diffs, subscribee_pid, subscribee_source );
  let head_subscribee_contains : Vec<ID> =
    if staged_nc . is_none () && unstaged_nc . is_none () {
      subscribee_contains . to_vec ()
    } else {
      let net : Vec<Diff_Item<ID>> = net_diff_from_per_stage (
        staged_nc   . map ( |c| c . contains_diff . as_slice () ),
        unstaged_nc . map ( |c| c . contains_diff . as_slice () ));
      net . iter () . filter_map (
          |d| match d { // Items in HEAD: Unchanged or Removed.
            Diff_Item::Unchanged (id) |
              Diff_Item::Removed (id) => Some ( id . clone () ),
            Diff_Item::New (_) => None } )
        . collect () };
  let head_subscriber_hides : Vec<ID> =
    nodecomplete_from_git_head (
        subscriber_pid, subscriber_source, config )
      . ok ()
      . map ( |skg| skg . hides_from_its_subscriptions . into_vec () )
      . unwrap_or_default ();
  let head_content : Vec<ID> =
    { let head_subscribee_contains_set : HashSet<ID> =
        head_subscribee_contains . iter () . cloned () . collect ();
      head_subscriber_hides . iter () . filter (
          |id| head_subscribee_contains_set . contains (id)
        ) . cloned () . collect () };
  let diff : Vec<Diff_Item<ID>> =
    compute_interleaved_diff ( &head_content, &worktree_content );
  itemlist_and_removedset_from_diff (&diff) }

/// Goal list for a HiddenOutsideOfSubscribeeCol: the subscriber's
/// hides-list minus everything contained by any subscribee,
/// optionally diffed against the HEAD versions.
pub fn goal_list_for_hiddenoutsideof_subscribeecol (
  subscriber_pid       : &ID,
  subscriber_source    : &SourceName,
  wt_subscriber_hides  : &[ID],
  wt_subscribees       : &[ID],
  source_diffs         : &Option<HashMap<SourceName, SourceDiff>>,
  config               : &SkgConfig,
) -> (Vec<ID>, HashSet<ID>) {
  let wt_all_subscribee_content : HashSet<ID> =
    wt_subscribees . iter ()
      . flat_map ( |pid| {
        match snapshot_global_source (pid, config) {
          Some (src) =>
            nodecomplete_from_memory_or_disk ( config, pid, &src )
              . ok ()
              . map ( |skg| skg . contains )
              . unwrap_or_default (),
          None => Vec::new () } } )
      . collect ();
  let wt_content : Vec<ID> =
    wt_subscriber_hides . iter ()
      . filter ( |id| !wt_all_subscribee_content . contains (id) )
      . cloned () . collect ();
  if source_diffs . is_none () {
    return (wt_content, HashSet::new ()); }
  let (head_subscriber_hides, head_subscribees)
    : (Vec<ID>, Vec<ID>)
    = nodecomplete_from_git_head (
          subscriber_pid, subscriber_source, config )
        . ok ()
        . map ( |skg| (skg . hides_from_its_subscriptions
                          . into_vec (),
                       skg . subscribes_to
                          . into_vec () ))
        . unwrap_or_default ();
  let head_all_subscribee_content : HashSet<ID> =
    head_subscribee_contains_from_memory_and_diffs (
      &head_subscribees, source_diffs, config );
  let head_content : Vec<ID> =
    head_subscriber_hides . iter ()
      . filter ( |id| !head_all_subscribee_content . contains (id) )
      . cloned () . collect ();
  let diff : Vec<Diff_Item<ID>> =
    compute_interleaved_diff ( &head_content, &wt_content );
  itemlist_and_removedset_from_diff (&diff) }

/// From HEAD, compute the union of some subscribees' content.
/// For each subscribee pid:
/// - Look up its source from memory. If missing, skip.
/// - Read per-stage contains_diff via
///   `per_stage_node_changes_for_truenode` and compose the net
///   HEAD→worktree view. Items that were in HEAD are Unchanged +
///   Removed in the net diff. If neither stage has changes for
///   this file, HEAD = worktree, so read contains from memory/disk.
fn head_subscribee_contains_from_memory_and_diffs (
  head_subscribees : &[ID],
  source_diffs     : &Option<HashMap<SourceName, SourceDiff>>,
  config           : &SkgConfig,
) -> HashSet<ID> {
  let mut result : HashSet<ID> = HashSet::new ();
  for pid in head_subscribees {
    let subscribee_source : SourceName =
      match snapshot_global_source (pid, config) {
        Some (s) => s,
        None => continue };
    let (staged_nc, unstaged_nc)
      : (Option<&NodeChanges>, Option<&NodeChanges>) =
      per_stage_node_changes_for_truenode (
        source_diffs, pid, &subscribee_source );
    if staged_nc . is_none () && unstaged_nc . is_none () {
      if let Ok (skg) = nodecomplete_from_memory_or_disk (
        config, pid, &subscribee_source )
      { for id in skg . contains . into_iter () {
          result . insert (id); } }
    } else {
      let net : Vec<Diff_Item<ID>> = net_diff_from_per_stage (
        staged_nc   . map ( |c| c . contains_diff . as_slice () ),
        unstaged_nc . map ( |c| c . contains_diff . as_slice () ));
      for d in &net {
        match d {
          Diff_Item::Unchanged (id) |
            Diff_Item::Removed (id) =>
              { result . insert ( id . clone () ); },
          Diff_Item::New (_) => {} } } } }
  result }

/// Resolve a node's source: try the in-Rust memory snapshot first,
/// fall back to scanning source directories for the matching '.skg'
/// file. Tests that bypass
/// `init_global_handle_for_first_time_or_panic` rely on the disk
/// fallback.
fn snapshot_global_source (
  pid    : &ID,
  config : &SkgConfig,
) -> Option<SourceName> {
  if let Some (s) =
    crate::dbs::memory::snapshot_global ()
      . as_deref ()
      . and_then ( |g| g . pid_and_source (pid) . map ( |(_, s)| s ) )
  { return Some (s); }
  crate::types::phantom::source_from_disk (pid, config) }
