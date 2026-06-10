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
use crate::types::errors::BufferValidationError;
use crate::types::misc::{ID, SkgConfig};
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
) -> Result<LoweredIntents, Box<dyn Error>> {
  validate_no_overlapping_subscribee_hiderel_conflicts (
    visibility, config, driver ) . await ?;
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
    let inferred_hides : Vec<ID> =
      subscribee_from_disk . contains . iter()
      . filter ( |id| ! visible_content . contains (*id) )
      . filter ( |id| ! subscriber_contains . contains (*id) )
      . cloned()
      . collect();
    let inferred_unhides : Vec<ID> =
      subscribee_from_disk . contains . iter()
      . filter ( |id| visible_content . contains (*id) )
      . filter ( |id| subscriber_from_disk . hides_from_its_subscriptions
                      . or_default() . contains (*id) )
      . cloned() . collect();
    if inferred_hides . is_empty() && inferred_unhides . is_empty()
      { continue; }
    lowered . apply_hiderel_delta_to_subscriber (
      subscriber_from_disk,
      &inferred_hides,
      &inferred_unhides ); }
  Ok (lowered) }

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
    for content_id in subscribee_from_disk . contains {
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
