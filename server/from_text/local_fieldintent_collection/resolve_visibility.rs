//! This file defines visibility resolution, which consumes the
//! 'SubscribeeVisibility' signals that collection put in the map.
//! It is async because it consults disk.
//! .
//! The policy of the subscribee-visibility step, for subscriber R
//! viewing subscribee E, is:
//! .
//! - If N is graph-content of subscribee E,
//!   and E has a viewchild with ID N and claimsMembership,
//!   then N is intended to be visible through this subscription,
//!   so it is removed from the hides of subscriber R.
//! - If N is graph-content of E, E has no viewchild with ID N and
//!   claimsMembership, and N is not graph-content of R after the
//!   save, then N is hidden from R.
//! - If N is a claimsMembership viewchild of E but is not
//!   graph-content of E, then it is not a hiderel edit.
//!   This does not touch it. The refresh of the save's rerender
//!   makes it vanish.
//! .
//! The skgrepo restriction limits what can be edited as well as what
//! can be seen ('server/skgrepo_sets.rs'), so both E's content and R's
//! hides are read by the records the
//! skgrepo restriction shows, as the view drew them: a hide whose
//! record is restricted is neither removed nor moved, and no hide is
//! inferred of a member that only a restricted record of E's
//! explains.
//! .
//! A forks folder adds one more signal ('ForksFolderMembers'): a note
//! that forks the subscriber, that the subscriber does not fork after
//! the save, and that its forks folder no longer shows, was deleted
//! there, and is hidden from it ('resolve_forks_folder_removals').
//! .
//! Resolution cannot run during collection, even in principle: it
//! needs disk (the subscribee's contains, the subscriber's hides)
//! and the subscriber's own post-save contains -- a finished entry
//! elsewhere in the same map.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::mirrors::{Mirrors, mirrors_of};
use crate::dbs::node_lookup::opt_graphnode_by_gnid;
use crate::from_text::local_fieldintent_collection::lower::LoweredNodeIntents;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::from_text::local_fieldintent_collection::types::{
  ForksFolderMembers, HiddenOutsideEdit, SubscribeeVisibility };
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::errors::BufferValidationError;
use crate::types::misc::{ID, MSV, RelPartner, SkgConfig, members_of};
use crate::types::nodes::complete::Graphnode;
use crate::types::save::PostSkgsaveCommitNoticeCandidate;

use std::collections::{HashMap, HashSet};
use std::error::Error;

/// The 'visibility' pairs are (subscriber, signal), as
/// 'lower_collected_fieldIntents' extracted them from the map.
pub fn resolve_visibility (
  mut lowered : LoweredNodeIntents,
  visibility  : &[(ID, SubscribeeVisibility)],
  hidden_outside : &[(ID, HiddenOutsideEdit)],
  forks_folder_members : &[(ID, ForksFolderMembers)],
  graph       : &InRustGraph,
  config      : &SkgConfig,
  skgrepo_restriction : &SkgrepoRestriction,
) -> Result<(LoweredNodeIntents, Vec<PostSkgsaveCommitNoticeCandidate>), Box<dyn Error>> {
  validate_no_overlapping_subscribee_hiderel_conflicts (
    visibility, graph, config, skgrepo_restriction ) ?;
  infer_hides_from_contains_removals (
    // Before the signal loop below, so that an explicit
    // subscribee-as-such gesture about the same child wins.
    &mut lowered, visibility, graph, config,
    skgrepo_restriction ) ?;
  for (subscriber, signal) in visibility {
    let Some (subscribee_from_disk) =
      opt_graphnode_by_gnid (
        graph, config, &signal . subscribee ) ?
    else { continue; };
    let Some (subscriber_from_disk) =
      opt_graphnode_by_gnid (
        graph, config, subscriber ) ?
    else { continue; };
    if ! config . skgrepo_is_owned (&subscriber_from_disk . home_skgrepo) {
      continue; }
    let subscriber_contains : HashSet<ID> =
      lowered . subscriber_contains_after_save (
        &subscriber_from_disk );
    let visible_content : HashSet<ID> =
      signal . visible . iter() . cloned() . collect();
    let subscribee_contains : Vec<ID> = // what the buffer could show of the subscribee's content
      members_with_visible_records (
        &subscribee_from_disk . contains, skgrepo_restriction );
    let subscriber_hides : Vec<ID> = // a restricted hide the buffer could not show, so no save removes it
      members_with_visible_records (
        subscriber_from_disk . hidesFromSubs . or_default (),
        skgrepo_restriction );
    let inferred_hides : Vec<ID> =
      subscribee_contains . iter()
      . filter ( |gnid| ! visible_content . contains (*gnid) )
      . filter ( |gnid| ! subscriber_contains . contains (*gnid) )
      . cloned()
      . collect();
    let inferred_unhides : Vec<ID> =
      subscribee_contains . iter()
      . filter ( |gnid| visible_content . contains (*gnid) )
      . filter ( |gnid| subscriber_hides . contains (*gnid) )
      . cloned() . collect();
    if inferred_hides . is_empty() && inferred_unhides . is_empty()
      { continue; }
    lowered . apply_hiderel_delta_to_subscriber (
      subscriber_from_disk,
      &inferred_hides,
      &inferred_unhides ); }
  resolve_forks_folder_removals (
    &mut lowered, forks_folder_members, graph, config,
    skgrepo_restriction ) ?;
  let post_skgsave_commit_notice_candidates = apply_hiddenoutside_edits (
    &mut lowered, hidden_outside, graph, config,
    skgrepo_restriction ) ?;
  Ok ((lowered, post_skgsave_commit_notice_candidates)) }

/// A note N's forks folder shows a forker of N claiming nothing (a
/// forker-only member, which N does not fork) and each mutual
/// claiming; N hides neither. Deleting either from the folder hides it
/// in N, so that it leaves the folder (where a forker N hides is not
/// drawn) for the folder of what N hides outside its fork partners,
/// whence removing it unhides it. So each note that forks N by a
/// record the skgrepo restriction shows, that N does not fork after the
/// save (a deleted mutual's fork the same save ends), that the folder
/// no longer shows, and that N does not hide already, joins N's hides.
/// A forker the restriction keeps out was never drawn, so it is never
/// hidden here. Only an owned N is written. This runs before the edits
/// of the hidden-outside folder, which may unhide it again.
fn resolve_forks_folder_removals (
  lowered             : &mut LoweredNodeIntents,
  signals             : &[(ID, ForksFolderMembers)],
  graph               : &InRustGraph,
  config              : &SkgConfig,
  skgrepo_restriction : &SkgrepoRestriction,
) -> Result<(), Box<dyn Error>> {
  for (note, signal) in signals {
    let Some (note_from_disk) : Option<Graphnode> =
      opt_graphnode_by_gnid (graph, config, note) ?
      else { continue; };
    if ! config . skgrepo_is_owned (&note_from_disk . home_skgrepo) {
      continue; }
    let key : &dyn Fn (&ID) -> ID =
      &|gnid : &ID| -> ID { graph . pid_else_raw_id (gnid) };
    let shown : HashSet<ID> = signal . members . iter () . map (key) . collect ();
    let forked_after_save : HashSet<ID> =
      lowered . subscriber_subscribes_after_save (&note_from_disk) . iter ()
      . map (key) . collect ();
    let hidden : HashSet<ID> =
      lowered . subscriber_hides_after_resolution (&note_from_disk) . iter ()
      . map (key) . collect ();
    let deleted_forkers : Vec<ID> =
      graph . inbound_pids_for_relation_gated (
        &note_from_disk . pid, NodeRelation::Forks, skgrepo_restriction )
      . into_iter ()
      . filter ( |forker| ! shown . contains (forker)
                          && ! forked_after_save . contains (forker)
                          && ! hidden . contains (forker) )
      . collect ();
    if ! deleted_forkers . is_empty () {
      lowered . apply_hiderel_delta_to_subscriber (
        note_from_disk, &deleted_forkers, &[] ); }}
  Ok (( )) }

/// The members of 'list' whose records the skgrepo restriction shows
/// ('SkgrepoRestriction::record_is_visible'), as written.
fn members_with_visible_records (
  list                : &[RelPartner<ID>],
  skgrepo_restriction : &SkgrepoRestriction,
) -> Vec<ID> {
  list . iter ()
    . filter ( |member| SkgrepoRestriction::record_is_visible (
      skgrepo_restriction, &member . relRepo ))
    . map ( |member| member . member . clone () )
    . collect () }

/// Applies the submitted visible-outside subset after all ordinary hide
/// inference.  Only the old *visible outside* hides are replaceable: hides
/// whose records are restricted, and members inside a subscribee, remain owned
/// by the graph and survive an edit of this compound partner folder. A hide is
/// visible, and so replaceable, when the skgrepo restriction holds its relRepo
/// ('SkgrepoRestriction::record_is_visible'): the folder as drawn showed its
/// member, as an occurrence or a gnid-only vognode, so the submitted
/// folder is the whole set of hides with a visible record outside the
/// subscribees, and a folder saved under a skgrepo restriction never unhides a
/// member it could not show. What counts as inside is what the folder's goal
/// subtracts ('goal_list_for_hiddenOutsideOfSubscribee_folder'): the content
/// of each subscribee by the records the skgrepo restriction shows.
fn apply_hiddenoutside_edits (
  lowered                : &mut LoweredNodeIntents,
  edits                  : &[(ID, HiddenOutsideEdit)],
  graph                  : &InRustGraph,
  config                 : &SkgConfig,
  skgrepo_restriction    : &SkgrepoRestriction,
) -> Result<Vec<PostSkgsaveCommitNoticeCandidate>, Box<dyn Error>> {
  let mut seen : HashSet<ID> = HashSet::new ();
  let mut candidates : Vec<PostSkgsaveCommitNoticeCandidate> = Vec::new ();
  for (subscriber, edit) in edits {
    if ! seen . insert (subscriber . clone ()) {
      return Err (Box::new (BufferValidationError::Other (
        format! ("More than one HiddenOutsideOfForkPartners edit was submitted for subscriber {}", subscriber) ))); }
    let Some (subscriber_from_disk) =
      opt_graphnode_by_gnid (graph, config, subscriber) ?
    else { continue; };
    if ! config . skgrepo_is_owned (&subscriber_from_disk . home_skgrepo) {
      continue; }

    let key : &dyn Fn (&ID) -> ID =
      &|gnid : &ID| -> ID { graph . pid_else_raw_id (gnid) };
    let is_visible : &dyn Fn (&RelPartner<ID>) -> bool =
      &|member : &RelPartner<ID>| -> bool {
      SkgrepoRestriction::record_is_visible (
        skgrepo_restriction, &member . relRepo ) };
    let subscribee_gnids : Vec<ID> =
      lowered . subscriber_subscribes_after_save (&subscriber_from_disk);
    let mut inside : HashSet<ID> = HashSet::new ();
    for subscribee_gnid in subscribee_gnids {
      let Some (subscribee) =
        opt_graphnode_by_gnid (graph, config, &subscribee_gnid) ?
      else { continue; };
      for member in subscribee . contains . iter ()
        . filter (|member| is_visible (member)) {
        inside . insert (key (&member . member)); }}

    // The replacement domain is intentionally built from disk, rather than
    // from newly inferred hides: preceding user actions in this save retain
    // their ordinary inference semantics.
    let replaceable_outside : HashSet<ID> =
      subscriber_from_disk . hidesFromSubs . or_default ()
      . iter ()
      . filter (|member| is_visible (member))
      .map (|member| key (&member . member))
      .filter (|member_key| ! inside . contains (member_key))
      .collect ();
    let submitted : HashSet<ID> =
      edit . members . iter () . map (|gnid| key (gnid)) . collect ();
    let disk_hide_keys : HashSet<ID> =
      subscriber_from_disk . hidesFromSubs . or_default ()
      . iter () . map (|member| key (&member . member)) . collect ();
    for member in &edit . members {
      if ! disk_hide_keys . contains (&key (member)) {
        candidates . push (
          PostSkgsaveCommitNoticeCandidate::HiddenOutsideAdded {
            subscriber : subscriber . clone (), member : member . clone () }); }}
    let current_hides : Vec<ID> =
      lowered . subscriber_hides_after_resolution (&subscriber_from_disk);
    let inferred_unhides : Vec<ID> = current_hides . iter ()
      . filter (|gnid| {
        let member_key : ID = key (gnid);
        replaceable_outside . contains (&member_key)
          && ! submitted . contains (&member_key) })
      . cloned () . collect ();
    let current_keys : HashSet<ID> =
      current_hides . iter () . map (|gnid| key (gnid)) . collect ();
    let inferred_hides : Vec<ID> = edit . members . iter ()
      . filter (|gnid| ! current_keys . contains (&key (gnid)))
      . cloned () . collect ();
    if ! inferred_hides . is_empty () || ! inferred_unhides . is_empty () {
      lowered . apply_hiderel_delta_to_subscriber (
        subscriber_from_disk, &inferred_hides, &inferred_unhides); }}
  Ok (candidates) }

/// Deleting (or moving away) a child of an owned subscriber F, where
/// that child is disk-content of a node F subscribes to, hides the
/// child from F's subscriptions -- otherwise the child would
/// reappear under F as unintegrated subscribed content the user just
/// dismissed (docs/sharing-model.org: branches deleted from a clone
/// become hides). Symmetrically, re-adding such a child to F's
/// contains drops a stale hide of it by F. A removed child counts as
/// the member it stands for ('member_it_stands_for'): removing a
/// mirror of a subscribee's member hides that member, unless the
/// same save leaves F containing the member. Placing a mirror
/// drops no hide, as containing it does not integrate the member
/// ('unintegrated_content_gnids' in 'server/refresh/goal.rs' compares
/// IDs): an unhidden member would come back as unintegrated. Details:
/// - Members whose records the skgrepo restriction keeps out were
///   omitted from the buffer, not removed, so they never count as
///   removals ('SkgrepoRestriction::record_is_visible'). A member
///   with a visible record had a headline (a restricted viewnode,
///   where the skgrepo restriction keeps out its node), so its absence
///   is a removal.
/// - A child the same save explicitly shows through one of F's
///   subscriptions (a subscribee-as-such signal) is not hidden here:
///   the explicit subscription-view gesture wins. This runs before
///   the signal loop, so signal deltas also land last.
/// - A node with no disk entry removes nothing (a fork clone's
///   creation-time hides are computed in 'build_fork_clone').
/// - Only what the skgrepo restriction shows explains or blocks a
///   hide: the subscriber's subscriptions, each subscribee's content
///   and the subscriber's hides are read by their visible records. A
///   member only a restricted record explains is not hidden, and
///   re-adding a member drops no restricted hide of it.
fn infer_hides_from_contains_removals (
  lowered                : &mut LoweredNodeIntents,
  visibility             : &[(ID, SubscribeeVisibility)],
  graph                  : &InRustGraph,
  config                 : &SkgConfig,
  skgrepo_restriction    : &SkgrepoRestriction,
) -> Result<(), Box<dyn Error>> {
  for (subscriber_pid, skgrepo, new_contains, subscribes_msv)
    in lowered . nodeSaveIntents_with_specified_contains () {
    if ! config . skgrepo_is_owned (&skgrepo) { continue; }
    let Some (subscriber_from_disk) =
      opt_graphnode_by_gnid (
        graph, config, &subscriber_pid ) ?
    else { continue; };
    let subscriber_contains : Vec<ID> =
      members_of (&subscriber_from_disk . contains);
    let visible_contains : HashSet<&ID> =
      subscriber_from_disk . contains . iter ()
      . filter ( |member| SkgrepoRestriction::record_is_visible (
          skgrepo_restriction, &member . relRepo ))
      . map ( |member| &member . member )
      . collect ();
    let removed : Vec<ID> = {
      let new_contains_set : HashSet<&ID> =
        new_contains . iter () . collect ();
      let signal_visible : HashSet<&ID> = // children the same save explicitly shows through a subscription of this subscriber
        visibility . iter ()
        . filter ( |(pid, _)| *pid == subscriber_pid )
        . flat_map ( |(_, signal)| signal . visible . iter () )
        . collect ();
      subscriber_contains . iter ()
        . filter ( |gnid| ! new_contains_set . contains (gnid) )
        . filter ( |gnid| ! signal_visible . contains (gnid) )
        . filter ( |gnid| visible_contains . contains (gnid) )
        . cloned () . collect () };
    let inferred_unhides : Vec<ID> = {
      let disk_contains : HashSet<&ID> =
        subscriber_contains . iter () . collect ();
      let disk_hides : Vec<ID> = // a restricted hide stays, as the buffer could not show it
        members_with_visible_records (
          subscriber_from_disk . hidesFromSubs . or_default (),
          skgrepo_restriction );
      new_contains . iter ()
        . filter ( |gnid| ! disk_contains . contains (gnid) )
        . filter ( |gnid| disk_hides . contains (gnid) )
        . cloned () . collect () };
    let inferred_hides : Vec<ID> =
      if removed . is_empty () { Vec::new () }
      else {
        let subscribee_content : HashSet<ID> = {
          let subscribes : Vec<ID> = match &subscribes_msv {
            // The post-save subscriptions: unsubscribing a node in
            // the same save also stops hides being inferred from it.
            MSV::Specified (subs) => subs . clone (),
            MSV::Unspecified =>
              members_with_visible_records (
                subscriber_from_disk . forks . or_default (),
                skgrepo_restriction ), };
          let mut content : HashSet<ID> = HashSet::new ();
          for subscribee in &subscribes {
            if let Some (subscribee_from_disk) =
              opt_graphnode_by_gnid (
                graph, config, subscribee ) ?
            { content . extend (
                members_with_visible_records (
                  & subscribee_from_disk . contains,
                  skgrepo_restriction )); }}
          content };
        let new_contains_set : HashSet<&ID> = new_contains . iter () . collect ();
        removed . iter ()
          . filter_map ( |gnid| member_it_stands_for (
            graph, skgrepo_restriction, gnid, &subscribee_content ))
          . filter ( |member| ! new_contains_set . contains (member) )
          . collect () };
    if inferred_hides . is_empty () && inferred_unhides . is_empty ()
      { continue; }
    lowered . apply_hiderel_delta_to_subscriber (
      subscriber_from_disk, &inferred_hides, &inferred_unhides ); }
  Ok (( )) }

/// The member of 'members' that 'gnid', a child removed from a
/// subscriber, stands for: 'gnid' itself when it is one; otherwise
/// the member that is a mirror of it under the skgrepo restriction
/// ('server/dbs/in_rust_graph/mirrors.rs'), the nearest along forks
/// first, and among equals the least. Every contains list, a
/// subscribee's included, records the original; a mirror is shown in
/// its place only where a perspective ranks it first. So when the
/// subscribee's author forks a member of its content (to answer under
/// a note of the subscriber's, say), the content still names the
/// original, which the subscriber's own view draws raw and the
/// author's view draws as the fork. Either way the subscriber met the
/// same member, so dismissing the fork dismisses it. Any mirror
/// counts, not one perspective's: which member a child stands for is
/// a fact about the child, whichever view the user found it in.
pub(crate) fn member_it_stands_for (
  graph       : &InRustGraph,
  restriction : &SkgrepoRestriction,
  gnid        : &ID,
  members     : &HashSet<ID>,
) -> Option<ID> {
  if members . contains (gnid) { return Some (gnid . clone ()); }
  let mirrors : Mirrors = mirrors_of (graph, restriction, gnid);
  members . iter ()
    . filter_map ( |member| mirrors . members . iter ()
      . find ( |mirror| mirror . pid == graph . pid_else_raw_id (member) )
      . map ( |mirror| ( mirror . distance, member ) ))
    . min ()
    . map ( |(_distance, member)| member . clone () ) }

/// Checks one subscriber + content node
/// against all subscribee branches under the subscriber.
/// Rejects the save if one subscribee says "hide it"
/// and another says "show it". A subscribee's content is read by the
/// records the skgrepo restriction shows, as the view drew it.
fn validate_no_overlapping_subscribee_hiderel_conflicts (
  visibility          : &[(ID, SubscribeeVisibility)],
  graph               : &InRustGraph,
  config              : &SkgConfig,
  skgrepo_restriction : &SkgrepoRestriction,
) -> Result<(), Box<dyn Error>> {
  // This needs the visibility signals plus disk contains lists,
  // because the conflict is per subscriber/subscribee-content pair,
  // not just per viewchild shown in the buffer.
  let mut seen : HashMap<(ID, ID), bool> = HashMap::new();
  for (subscriber, signal) in visibility {
    let subscribee_from_disk : Graphnode =
      match opt_graphnode_by_gnid (
        graph, config, &signal . subscribee ) ?
      { Some (subscribee_from_disk) => subscribee_from_disk,
        None                        => continue, };
    let visible_content : HashSet<ID> =
      signal . visible . iter() . cloned() . collect();
    for content_gnid in members_with_visible_records (
      &subscribee_from_disk . contains, skgrepo_restriction ) {
      let content_gnid : ID = content_gnid;
      let key : (ID, ID) =
        (subscriber . clone(), content_gnid . clone());
      let visible : bool = visible_content . contains (&content_gnid);
      match seen . get (&key) {
        Some (previous_visible) => {
          let previous_visible : &bool = previous_visible;
          if *previous_visible != visible {
            return Err (Box::new (BufferValidationError::Other (
              format!( "Conflicting subscribee visibility edits for subscriber {} and content {}",
                        subscriber, content_gnid )) )); }},
        None => {}, }
      seen . insert (key, visible); }}
  Ok (( )) }
