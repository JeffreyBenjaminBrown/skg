/// This file defines visibility resolution, which consumes the
/// 'SubscribeeVisibility' signals that collection put in the map.
/// It is async because it consults disk.
/// .
/// The policy, for subscriber R viewing subscribee E, is:
/// .
/// - If N is graph-content of subscribee E,
///   and E has a view-child with ID N and ParentIs=Affected,
///   then N is intended to be visible through this subscription,
///   so it is removed from the hides of subscriber R.
/// - If N is graph-content of E, E has no view-child with ID N and
///   ParentIs=Affected, and N is not graph-content of R after the
///   save, then N is hidden from R.
/// - If N is a ParentIs=Affected view-child of E but is not
///   graph-content of E, then it is not a hiderel edit.
///   This does not touch it. The completion/rerender pipeline
///   will change it to Independent.
/// .
/// Resolution cannot run during collection, even in principle: it
/// needs disk (the subscribee's contains, the subscriber's hides)
/// and the subscriber's own post-save contains -- a finished entry
/// elsewhere in the same map.

use crate::dbs::node_lookup::optNodeComplete_rustFIrst_by_id;
use crate::from_text::local_instruction_collection::lower::LoweredIntents;
use crate::from_text::local_instruction_collection::types::SubscribeeVisibility;
use crate::from_text::weave::member_is_visible;
use crate::source_sets::ActiveSourceSet;
use crate::types::errors::BufferValidationError;
use crate::types::misc::{ID, MSV, SkgConfig, members_of};
use crate::types::nodes::complete::NodeComplete;

use std::collections::{HashMap, HashSet};
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// The 'visibility' pairs are (subscriber, signal), as
/// 'lower_collected_intents' extracted them from the map.
pub async fn resolve_visibility (
  mut lowered : LoweredIntents,
  visibility  : &[(ID, SubscribeeVisibility)],
  config      : &SkgConfig,
  driver      : &TypeDBDriver,
  restricted_source_set : Option<&ActiveSourceSet>, // None means no restriction; callers normalize 'all' to None.
) -> Result<LoweredIntents, Box<dyn Error>> {
  validate_no_overlapping_subscribee_hiderel_conflicts (
    visibility, config, driver ) . await ?;
  infer_hides_from_contains_removals (
    // Before the signal loop below, so that an explicit
    // subscribee-as-such gesture about the same child wins.
    &mut lowered, visibility, config, driver,
    restricted_source_set ) . await ?;
  for (subscriber, signal) in visibility {
    let Some (subscribee_from_disk) =
      optNodeComplete_rustFIrst_by_id (
        config, driver, &signal . subscribee ) . await ?
    else { continue; };
    let Some (subscriber_from_disk) =
      optNodeComplete_rustFIrst_by_id (
        config, driver, subscriber ) . await ?
    else { continue; };
    if ! config . user_owns_source (&subscriber_from_disk . source) {
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
        subscriber_from_disk . hides_from_its_subscriptions
        . or_default() );
    let inferred_hides : Vec<ID> =
      subscribee_contains . iter()
      . filter ( |id| ! visible_content . contains (*id) )
      . filter ( |id| ! subscriber_contains . contains (*id) )
      . cloned()
      . collect();
    let inferred_unhides : Vec<ID> =
      subscribee_contains . iter()
      . filter ( |id| visible_content . contains (*id) )
      . filter ( |id| subscriber_hides . contains (*id) )
      . cloned() . collect();
    if inferred_hides . is_empty() && inferred_unhides . is_empty()
      { continue; }
    lowered . apply_hiderel_delta_to_subscriber (
      subscriber_from_disk,
      &inferred_hides,
      &inferred_unhides ); }
  Ok (lowered) }

/// Deleting (or moving away) a child of an owned subscriber F, where
/// that child is disk-content of a node F subscribes to, HIDES the
/// child from F's subscriptions -- otherwise the child would
/// reappear under F as unintegrated subscribed content the user just
/// dismissed (docs/sharing-model.md: branches deleted from a clone
/// become hides). Symmetrically, re-adding such a child to F's
/// contains drops a stale hide of it by F. Details:
/// - Members invisible under the restricted source-set were OMITTED
///   from the buffer, not removed, so they never count as removals.
/// - A child the same save explicitly shows through one of F's
///   subscriptions (a subscribee-as-such signal) is not hidden here:
///   the explicit subscription-view gesture wins. This runs before
///   the signal loop, so signal deltas also land last.
/// - A node with no disk entry removes nothing (a fork clone's
///   creation-time hides are computed in 'build_fork_clone').
async fn infer_hides_from_contains_removals (
  lowered    : &mut LoweredIntents,
  visibility : &[(ID, SubscribeeVisibility)],
  config     : &SkgConfig,
  driver     : &TypeDBDriver,
  restricted_source_set : Option<&ActiveSourceSet>,
) -> Result<(), Box<dyn Error>> {
  for (subscriber_pid, source, new_contains, subscribes_msv)
    in lowered . save_intents_with_specified_contains () {
    if ! config . user_owns_source (&source) { continue; }
    let Some (subscriber_from_disk) =
      optNodeComplete_rustFIrst_by_id (
        config, driver, &subscriber_pid ) . await ?
    else { continue; };
    let subscriber_contains : Vec<ID> =
      members_of (&subscriber_from_disk . contains);
    let removed : Vec<ID> = {
      let new_contains_set : HashSet<&ID> =
        new_contains . iter () . collect ();
      let signal_visible : HashSet<&ID> = // children the same save explicitly shows through a subscription of this subscriber
        visibility . iter ()
        . filter ( |(pid, _)| *pid == subscriber_pid )
        . flat_map ( |(_, signal)| signal . visible . iter () )
        . collect ();
      subscriber_contains . iter ()
        . filter ( |id| ! new_contains_set . contains (id) )
        . filter ( |id| ! signal_visible . contains (id) )
        . filter ( |id| restricted_source_set . map_or (
            true, |active| member_is_visible (id, config, active) ))
        . cloned () . collect () };
    let inferred_unhides : Vec<ID> = {
      let disk_contains : HashSet<&ID> =
        subscriber_contains . iter () . collect ();
      let disk_hides : Vec<ID> =
        members_of (
          subscriber_from_disk . hides_from_its_subscriptions
          . or_default () );
      new_contains . iter ()
        . filter ( |id| ! disk_contains . contains (id) )
        . filter ( |id| disk_hides . contains (id) )
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
                subscriber_from_disk . subscribes_to . or_default () ), };
          let mut content : HashSet<ID> = HashSet::new ();
          for subscribee in &subscribes {
            if let Some (subscribee_from_disk) =
              optNodeComplete_rustFIrst_by_id (
                config, driver, subscribee ) . await ?
            { content . extend (
                members_of (& subscribee_from_disk . contains) ); }}
          content };
        removed . into_iter ()
          . filter ( |id| subscribee_content . contains (id) )
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
async fn validate_no_overlapping_subscribee_hiderel_conflicts (
  visibility : &[(ID, SubscribeeVisibility)],
  config     : &SkgConfig,
  driver     : &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  // This needs the visibility signals plus disk contains lists,
  // because the conflict is per subscriber/subscribee-content pair,
  // not just per visible child shown in the buffer.
  let mut seen : HashMap<(ID, ID), bool> = HashMap::new();
  for (subscriber, signal) in visibility {
    let subscribee_from_disk : NodeComplete =
      match optNodeComplete_rustFIrst_by_id (
        config, driver, &signal . subscribee ) . await ?
      { Some (subscribee_from_disk) => subscribee_from_disk,
        None                        => continue, };
    let visible_content : HashSet<ID> =
      signal . visible . iter() . cloned() . collect();
    for content_id in members_of (&subscribee_from_disk . contains) {
      let content_id : ID = content_id;
      let key : (ID, ID) =
        (subscriber . clone(), content_id . clone());
      let visible : bool = visible_content . contains (&content_id);
      match seen . get (&key) {
        Some (previous_visible) => {
          let previous_visible : &bool = previous_visible;
          if *previous_visible != visible {
            return Err (Box::new (BufferValidationError::Other (
              format!( "Conflicting subscribee visibility edits for subscriber {} and content {}",
                        subscriber, content_id )) )); }},
        None => {}, }
      seen . insert (key, visible); }}
  Ok (( )) }
