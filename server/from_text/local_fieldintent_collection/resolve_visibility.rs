/// This file defines visibility resolution, which consumes the
/// 'SubscribeeVisibility' signals that collection put in the map.
/// It is async because it consults disk.
/// .
/// The policy, for subscriber R viewing subscribee E, is:
/// .
/// - If N is graph-content of subscribee E,
///   and E has a viewchild with ID N and claimsMembership,
///   then N is intended to be visible through this subscription,
///   so it is removed from the hides of subscriber R.
/// - If N is graph-content of E, E has no viewchild with ID N and
///   claimsMembership, and N is not graph-content of R after the
///   save, then N is hidden from R.
/// - If N is a claimsMembership viewchild of E but is not
///   graph-content of E, then it is not a hiderel edit.
///   This does not touch it. The refresh of the save's rerender
///   will mark it noClaim.
/// .
/// Resolution cannot run during collection, even in principle: it
/// needs disk (the subscribee's contains, the subscriber's hides)
/// and the subscriber's own post-save contains -- a finished entry
/// elsewhere in the same map.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::node_lookup::opt_graphnode_by_gnid;
use crate::from_text::local_fieldintent_collection::lower::LoweredNodeIntents;
use crate::from_text::local_fieldintent_collection::types::{
  HiddenOutsideEdit, SubscribeeVisibility };
use crate::from_text::weave::relationship_member_is_visible;
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
  graph       : &InRustGraph,
  config      : &SkgConfig,
  skgrepo_restriction : Option<&SkgrepoRestriction>, // None means no restriction; callers normalize 'all' to None.
) -> Result<(LoweredNodeIntents, Vec<PostSkgsaveCommitNoticeCandidate>), Box<dyn Error>> {
  validate_no_overlapping_subscribee_hiderel_conflicts (
    visibility, graph, config ) ?;
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
    let subscribee_contains : Vec<ID> =
      members_of (&subscribee_from_disk . contains);
    let subscriber_hides : Vec<ID> =
      members_of (
        subscriber_from_disk . hidesFromSubs
        . or_default() );
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
  let post_skgsave_commit_notice_candidates = apply_hiddenoutside_edits (
    &mut lowered, hidden_outside, graph, config,
    skgrepo_restriction ) ?;
  Ok ((lowered, post_skgsave_commit_notice_candidates)) }

/// Applies the submitted visible-outside subset after all ordinary hide
/// inference.  Only the old *visible outside* hides are replaceable: hides
/// whose records are restricted, and members inside a subscribee, remain owned
/// by the graph and survive an edit of this compound partner folder. A hide is
/// visible, and so replaceable, when the restriction holds its relRepo
/// ('relationship_member_is_visible'): the folder as drawn showed its member,
/// as an occurrence, an unknown or a Restricted vognode, so the submitted
/// folder is the whole set of hides with a visible record outside the
/// subscribees, and a folder saved under a restriction never unhides a
/// member it could not show. What counts as inside is what the folder's goal
/// subtracts ('goal_list_for_hiddenOutsideOfSubscribee_folder'): the whole
/// content of each subscribee, whatever records it.
fn apply_hiddenoutside_edits (
  lowered                : &mut LoweredNodeIntents,
  edits                  : &[(ID, HiddenOutsideEdit)],
  graph                  : &InRustGraph,
  config                 : &SkgConfig,
  skgrepo_restriction    : Option<&SkgrepoRestriction>,
) -> Result<Vec<PostSkgsaveCommitNoticeCandidate>, Box<dyn Error>> {
  let mut seen : HashSet<ID> = HashSet::new ();
  let mut candidates : Vec<PostSkgsaveCommitNoticeCandidate> = Vec::new ();
  for (subscriber, edit) in edits {
    if ! seen . insert (subscriber . clone ()) {
      return Err (Box::new (BufferValidationError::Other (
        format! ("More than one HiddenOutsideOfSubscribee edit was submitted for subscriber {}", subscriber) ))); }
    let Some (subscriber_from_disk) =
      opt_graphnode_by_gnid (graph, config, subscriber) ?
    else { continue; };
    if ! config . skgrepo_is_owned (&subscriber_from_disk . home_skgrepo) {
      continue; }

    let key = |gnid : &ID| -> ID {
      graph . pid_of (gnid) . unwrap_or_else (|| gnid . clone ()) };
    let is_visible = |member : &RelPartner<ID>| -> bool {
      skgrepo_restriction . map_or (
        true, |restriction| relationship_member_is_visible (
          member, restriction )) };
    let subscribee_gnids : Vec<ID> =
      lowered . subscriber_subscribes_after_save (&subscriber_from_disk);
    let mut inside : HashSet<ID> = HashSet::new ();
    for subscribee_gnid in subscribee_gnids {
      let Some (subscribee) =
        opt_graphnode_by_gnid (graph, config, &subscribee_gnid) ?
      else { continue; };
      for member in &subscribee . contains {
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
/// contains drops a stale hide of it by F. Details:
/// - Members whose records the restricted skgrepo-set keeps out were
///   omitted from the buffer, not removed, so they never count as
///   removals ('relationship_member_is_visible'). A member with a
///   visible record had a headline (a Restricted vognode, where the
///   restriction keeps out its node), so its absence is a removal.
/// - A child the same save explicitly shows through one of F's
///   subscriptions (a subscribee-as-such signal) is not hidden here:
///   the explicit subscription-view gesture wins. This runs before
///   the signal loop, so signal deltas also land last.
/// - A node with no disk entry removes nothing (a fork clone's
///   creation-time hides are computed in 'build_fork_clone').
fn infer_hides_from_contains_removals (
  lowered                : &mut LoweredNodeIntents,
  visibility             : &[(ID, SubscribeeVisibility)],
  graph                  : &InRustGraph,
  config                 : &SkgConfig,
  skgrepo_restriction    : Option<&SkgrepoRestriction>,
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
      . filter ( |member| skgrepo_restriction . map_or (
          true, |restriction| relationship_member_is_visible (member, restriction) ))
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
      let disk_hides : Vec<ID> =
        members_of (
          subscriber_from_disk . hidesFromSubs
          . or_default () );
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
              members_of (
                subscriber_from_disk . subscribesTo . or_default () ), };
          let mut content : HashSet<ID> = HashSet::new ();
          for subscribee in &subscribes {
            if let Some (subscribee_from_disk) =
              opt_graphnode_by_gnid (
                graph, config, subscribee ) ?
            { content . extend (
                members_of (& subscribee_from_disk . contains) ); }}
          content };
        removed . into_iter ()
          . filter ( |gnid| subscribee_content . contains (gnid) )
          . collect () };
    if inferred_hides . is_empty () && inferred_unhides . is_empty ()
      { continue; }
    lowered . apply_hiderel_delta_to_subscriber (
      subscriber_from_disk, &inferred_hides, &inferred_unhides ); }
  Ok (( )) }

/// Checks one subscriber + content node
/// against all subscribee branches under the subscriber.
/// Rejects the save if one subscribee says "hide it"
/// and another says "show it".
fn validate_no_overlapping_subscribee_hiderel_conflicts (
  visibility : &[(ID, SubscribeeVisibility)],
  graph      : &InRustGraph,
  config     : &SkgConfig,
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
    for content_gnid in members_of (&subscribee_from_disk . contains) {
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
