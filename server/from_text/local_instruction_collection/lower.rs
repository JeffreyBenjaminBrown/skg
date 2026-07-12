/// This file defines lowering, which converts 'CollectedIntents'
/// (the traversal's output) into ordered 'NodeIntent's -- the shape
/// that the downstream stages (visibility resolution, disk
/// supplementation, and the noop filter) consume. Lowering is pure
/// and synchronous, and reads no disk.
/// .
/// Entries lower as follows:
/// - A delete entry lowers to 'NodeIntent::Delete'.
/// - An entry with a title/body (that is, one owned by a
///   save-eligible definitive instance) lowers to
///   'NodeIntent::Save', with its empty slots lowering to
///   'MSV::Unspecified'.
/// - An entry holding only unresolved signals (visibility intents
///   and text claims) lowers to no NodeIntent; the signals are
///   returned beside the intents, for the downstream stages that
///   consume them. ('node_merge' slots are likewise not lowered
///   here: nodeMerge extraction reads them via 'nodeMerge_pairs'
///   before lowering consumes the map.)

use crate::from_text::local_instruction_collection::types::{
  CollectedIntents, IntentsForOneId, SubscribeeVisibility };
use crate::types::misc::{
  ID, MSV, PrivaciedMember, SourceName, members_msv, members_of,
  privacied_all, privacied_msv };
use crate::types::nodes::complete::{FileProperty, NodeComplete};
use crate::types::save::{DefineNode, SaveNode, DeleteNode};

use std::collections::{HashMap, HashSet};

/// What the user appears to intend for this node.
/// Might eventually become a DefineNode.
/// Uses MSV values in the Save variant (whereas DefineNode uses
/// SaveNode, which uses NodeComplete, which specifies all values).
pub enum NodeIntent {
  Save   (NodeSaveIntent),
  Delete (DeleteNode), // DefineNode uses the same DeleteNode type
}

pub struct NodeSaveIntent {
  pub pid               : ID,
  pub source            : SourceName,
  pub title             : String,
  pub body              : Option<String>,
  // contains / subscribes_to / overrides_view_of pair each member
  // with an Option<SourceName>: Some when the buffer's headline
  // carried an explicit '(relSource NAME)' atom (see
  // 'ViewNodeStats::rel_source', 'NodeIntent_Local'); None means
  // "derive" (sticky-else-default). 'explicit_levels' extracts the
  // Some entries into a side-channel BEFORE 'into_nodecomplete'
  // discards them, for 'apply_sticky_levels' to validate against
  // each edge's floor.
  pub contains          : MSV<(ID, Option<SourceName>)>,
  pub extra_ids         : Vec<ID>,
  pub aliases           : MSV<String>,
  pub subscribes_to     : MSV<(ID, Option<SourceName>)>,
  pub hides_from_its_subscriptions : MSV<ID>,
  pub overrides_view_of : MSV<(ID, Option<SourceName>)>,
  pub misc              : Vec<FileProperty>,
}

/// Levels the buffer explicitly requested via '(relSource NAME)'
/// (server/heralds.rs; 'ViewNodeStats::rel_source'), keyed by member
/// ID, one map per relation that carries per-member levels (hides is
/// absent: it is inferred, and the col that shows it is read-only --
/// the privatize gesture refuses there). Threaded separately from
/// NodeComplete because NodeComplete's 'PrivaciedMember::level' is a
/// plain SourceName with no "was this explicit" flag, and gets
/// unconditionally resolved by 'apply_sticky_levels' -- this is the
/// side-channel that tells that pass which members carry a real,
/// user-requested level to validate against the sticky/default
/// floor, rather than deriving normally (render-and-gating,
/// TODO/user-owned_autofork_chain/5_plan.org).
#[derive(Clone, Debug, Default)]
pub struct ExplicitLevels {
  pub contains          : HashMap<ID, SourceName>,
  pub subscribes_to     : HashMap<ID, SourceName>,
  pub overrides_view_of : HashMap<ID, SourceName>,
}

/// Strip the per-member explicit-level payload down to plain IDs, by
/// reference (read-only consumers, e.g.
/// 'save_intents_with_specified_contains').
fn ids_only_msv_ref (
  msv : &MSV<(ID, Option<SourceName>)>,
) -> MSV<ID> {
  match msv {
    MSV::Unspecified   => MSV::Unspecified,
    MSV::Specified (v) => MSV::Specified (
      v . iter () . map ( |(id, _)| id . clone () ) . collect () ), }}

/// As 'ids_only_msv_ref', consuming.
fn ids_only_msv (
  msv : MSV<(ID, Option<SourceName>)>,
) -> MSV<ID> {
  match msv {
    MSV::Unspecified   => MSV::Unspecified,
    MSV::Specified (v) => MSV::Specified (
      v . into_iter () . map ( |(id, _)| id ) . collect () ), }}

/// As above, for the non-MSV 'contains' slice.
fn ids_only (
  list : &[(ID, Option<SourceName>)],
) -> Vec<ID> {
  list . iter () . map ( |(id, _)| id . clone () ) . collect () }

impl NodeIntent {
  pub fn pid (
    &self,
  ) -> &ID {
    match self {
      NodeIntent::Save (intent) => &intent . pid,
      NodeIntent::Delete (intent) => &intent . id, }}

  pub fn apply_hiderel_delta (
    &mut self,
    base_hides       : &MSV<ID>,
    inferred_hides   : &[ID],
    inferred_unhides : &[ID],
  ) {
    match self {
      NodeIntent::Save (intent)
        => intent . apply_hiderel_delta (
             base_hides, inferred_hides, inferred_unhides),
      NodeIntent::Delete (_) => {}, }}

  pub fn graph_save_from_nodecomplete (
    node : NodeComplete,
  ) -> NodeIntent {
    // No explicit levels: this seeds an intent straight from disk
    // (a definitive rebuild for hide-delta application), not from a
    // buffer headline that could carry a '(relSource ...)' atom.
    // Preserves the MSV Unspecified/Specified distinction, unlike a
    // plain 'or_default()' round-trip.
    fn no_explicit_msv (
      msv : &MSV<PrivaciedMember<ID>>,
    ) -> MSV<(ID, Option<SourceName>)> {
      match msv {
        MSV::Unspecified   => MSV::Unspecified,
        MSV::Specified (v) => MSV::Specified (
          v . iter () . map ( |m| (m . member . clone (), None) ) . collect () ), }}
    NodeIntent::Save (NodeSaveIntent {
      pid                          : node . pid,
      source                       : node . source,
      title                        : node . title,
      body                         : node . body,
      contains                     : MSV::Specified (
        node . contains . iter ()
        . map ( |m| (m . member . clone (), None) ) . collect () ),
      extra_ids                    : node . extra_ids,
      aliases                      : members_msv (&node . aliases),
      subscribes_to                : no_explicit_msv (&node . subscribes_to),
      hides_from_its_subscriptions :
        members_msv (&node . hides_from_its_subscriptions),
      overrides_view_of            : no_explicit_msv (&node . overrides_view_of),
      misc                         : node . misc,
    }) }

  pub fn save_intent (
    self,
  ) -> Result<NodeSaveIntent, String> {
    match self {
      NodeIntent::Save (intent) =>
        Ok (intent),
      NodeIntent::Delete (_) =>
        Err ("Delete intent does not contain a SaveNode" . to_string()),
    }}

  pub fn into_define_node (
    self,
  ) -> Result<DefineNode, String> {
    match self {
      NodeIntent::Delete (intent)
        => Ok (DefineNode::Delete (intent)),
      NodeIntent::Save (intent)
        => Ok (DefineNode::Save (SaveNode (
          intent . into_nodecomplete() ))) }}
}

impl NodeSaveIntent {
  pub fn fill_unspecified_contains (
    &mut self,
    contains : &[ID],
  ) {
    if self . contains . is_unspecified() {
      // Disk-derived filler: no per-member explicit level (that only
      // ever comes from a buffer headline's own '(relSource ...)').
      self . contains =
        MSV::Specified ( contains . iter () . cloned ()
                          . map ( |id| (id, None) ) . collect () ); }}

  /// The levels the buffer explicitly requested (its headlines'
  /// '(relSource NAME)' atoms), read out BEFORE 'into_nodecomplete'
  /// discards the Option<SourceName> payload. See 'ExplicitLevels'.
  pub fn explicit_levels (
    &self,
  ) -> ExplicitLevels {
    fn collect (
      list : &[(ID, Option<SourceName>)],
    ) -> HashMap<ID, SourceName> {
      list . iter ()
        . filter_map ( |(id, lvl)| lvl . clone ()
                       . map ( |l| (id . clone (), l) ) )
        . collect () }
    ExplicitLevels {
      contains          : collect (self . contains . or_default ()),
      subscribes_to     : collect (self . subscribes_to . or_default ()),
      overrides_view_of : collect (self . overrides_view_of . or_default ()),
    }}

  pub fn into_nodecomplete (
    self,
  ) -> NodeComplete {
    let source : SourceName = self . source . clone();
    NodeComplete {
      title                        : self . title,
      aliases                      :
        privacied_msv (&source, self . aliases),
      source                       : self . source,
      pid                          : self . pid,
      extra_ids                    : self . extra_ids,
      body                         :
        crate::types::nodes::complete::normalize_body ( self . body ),
      contains                     :
        privacied_all (
          &source, ids_only (self . contains . or_default ()) ),
      subscribes_to                :
        privacied_msv (&source, ids_only_msv (self . subscribes_to)),
      hides_from_its_subscriptions :
        privacied_msv (&source, self . hides_from_its_subscriptions),
      overrides_view_of            :
        privacied_msv (&source, ids_only_msv (self . overrides_view_of)),
      misc                         : self . misc,
    }}

  fn apply_hiderel_delta (
    &mut self,
    base_hides       : &MSV<ID>,
    inferred_hides   : &[ID],
    inferred_unhides : &[ID],
  ) {
    let mut hides : Vec<ID> =
      if self . hides_from_its_subscriptions . is_unspecified() {
        base_hides . or_default() . to_vec()
      } else {
        self . hides_from_its_subscriptions . or_default() . to_vec()
      };
    hides . retain ( |id| ! inferred_unhides . contains (id) );
    for id in inferred_hides {
      if ! hides . contains (id) {
        hides . push (id . clone()); }}
    self . hides_from_its_subscriptions =
      MSV::Specified (hides); }}

/// This is an ordered map of one NodeIntent per PID; lowering
/// produces it, and visibility resolution mutates it. Its 'order'
/// field holds the PIDs in first save-or-delete-emission order.
pub struct LoweredIntents {
  order  : Vec<ID>,
  by_pid : HashMap<ID, NodeIntent>,
}

pub struct LoweringOutput {
  pub intents    : LoweredIntents,
  pub visibility : Vec<(ID, SubscribeeVisibility)>, // Each pair is (subscriber, signal); the list is in subscriber first-emission order.
}

/// This returns the (acquirer, acquiree) pair of every nodeMerge
/// request in the map, in acquirer first-emission order. Callers
/// must read this before lowering consumes the map.
#[allow(non_snake_case)]
pub fn nodeMerge_pairs (
  collected : &CollectedIntents,
) -> Vec<(ID, ID)> {
  let mut pairs : Vec<(ID, ID)> = Vec::new();
  for pid in &collected . order {
    if let Some (acquiree) =
      collected . by_pid . get (pid)
      . and_then ( |entry| entry . node_merge . as_ref() )
    { pairs . push (( pid . clone(), acquiree . clone() )); }}
  pairs }

pub fn lower_collected_intents (
  collected : CollectedIntents,
) -> Result<LoweringOutput, String> {
  let CollectedIntents { order, lowerable_order, mut by_pid }
    = collected;
  let visibility : Vec<(ID, SubscribeeVisibility)> = {
    // The signals are extracted across ALL entries, in
    // first-emission order -- including from the signals-only
    // entries, which lower to no NodeIntent.
    let mut visibility : Vec<(ID, SubscribeeVisibility)> =
      Vec::new();
    for pid in &order {
      let entry : &IntentsForOneId =
        by_pid . get (pid)
        . ok_or ( "lower_collected_intents: order names a PID missing from the map" . to_string() ) ?;
      for signal in &entry . visibility {
        visibility . push (( pid . clone(), signal . clone() )); }}
    visibility };
  let mut intents : LoweredIntents =
    LoweredIntents {
      order  : Vec::with_capacity (lowerable_order . len()),
      by_pid : HashMap::with_capacity (lowerable_order . len()) };
  for pid in lowerable_order {
    let entry : IntentsForOneId =
      by_pid . remove (&pid)
      . ok_or ( "lower_collected_intents: lowerable_order names a PID missing from the map" . to_string() ) ?;
    let intent : NodeIntent =
      lower_one_entry (&pid, entry) ?;
    intents . order . push (pid . clone());
    intents . by_pid . insert (pid, intent); }
  for (pid, leftover) in &by_pid {
    // Whatever remains holds only signals. Field intents can only
    // come from cols under a save-eligible owner, and a
    // save-eligible owner emits a title/body, putting its entry in
    // 'lowerable_order'; so anything else here is a collection bug.
    if leftover . contains . is_some()
      || leftover . aliases       . is_some()
      || leftover . subscribes_to . is_some()
      || leftover . overrides     . is_some()
      || leftover . node_merge    . is_some()
    { return Err ( format!(
        "lower_collected_intents: entry for {} has field intents but no title/body",
        pid )); }}
  Ok (LoweringOutput { intents, visibility }) }

/// This ASSUMES the entry is lowerable: it holds a delete or a
/// title/body, having been named by 'lowerable_order'.
fn lower_one_entry (
  pid   : &ID,
  entry : IntentsForOneId,
) -> Result<NodeIntent, String> {
  if entry . delete {
    return Ok (NodeIntent::Delete (DeleteNode {
      id     : pid . clone(),
      source : entry . source . ok_or_else ( || format!(
        "lower_collected_intents: delete entry for {} lacks a source",
        pid )) ?, } )); }
  match entry . title_and_body {
    None =>
      Err ( format!(
        "lower_collected_intents: lowerable entry for {} has neither delete nor title/body",
        pid )),
    Some (( title, body )) => {
      Ok (NodeIntent::Save (NodeSaveIntent {
        pid    : pid . clone(),
        source : entry . source . ok_or_else ( || format!(
          "lower_collected_intents: save entry for {} lacks a source",
          pid )) ?,
        title,
        body,
        contains                     :
          msv_from_slot (entry . contains),
        extra_ids                    : vec![],
        aliases                      :
          msv_from_slot (entry . aliases),
        subscribes_to                :
          msv_from_slot (entry . subscribes_to),
        hides_from_its_subscriptions : MSV::Unspecified,
        overrides_view_of            :
          msv_from_slot (entry . overrides),
        misc                         : Vec::new(),
      })) }} }

fn msv_from_slot<T> (
  slot : Option<Vec<T>>,
) -> MSV<T> {
  match slot {
    None      => MSV::Unspecified,
    Some (vs) => MSV::Specified (vs), }}

impl LoweredIntents {
  pub fn into_ordered_intents (
    self,
  ) -> Vec<NodeIntent> {
    let LoweredIntents { order, mut by_pid } = self;
    order . into_iter()
      . filter_map ( |pid| by_pid . remove (&pid) )
      . collect() }

  /// One (pid, source, contains, subscribes_to) tuple per Save
  /// intent whose contains is Specified -- the candidates for
  /// inferring hides from contains removals (see
  /// 'infer_hides_from_contains_removals'). Cloned out so the caller
  /// can mutate self (via 'apply_hiderel_delta_to_subscriber') while
  /// iterating.
  pub fn save_intents_with_specified_contains (
    &self,
  ) -> Vec<(ID, SourceName, Vec<ID>, MSV<ID>)> {
    self . order . iter ()
      . filter_map ( |pid| match self . by_pid . get (pid) {
          Some (NodeIntent::Save (intent)) =>
            match & intent . contains {
              MSV::Specified (contains) => Some ((
                pid . clone (),
                intent . source . clone (),
                ids_only (contains),
                ids_only_msv_ref (&intent . subscribes_to) )),
              _ => None },
          _ => None })
      . collect () }

  /// This returns the subscriber's contains as it will stand after
  /// this save: from its Save intent if it has one, and otherwise
  /// from disk.
  pub fn subscriber_contains_after_save (
    &self,
    subscriber_from_disk : &NodeComplete,
  ) -> HashSet<ID> {
    match self . by_pid . get (&subscriber_from_disk . pid) {
      Some (NodeIntent::Save (intent)) =>
        intent . contains . or_default () . iter ()
        . map ( |(id, _)| id . clone () ) . collect (),
      _ =>
        members_of (&subscriber_from_disk . contains)
        . into_iter() . collect(),
    }}

  /// This applies inferred hides/unhides to the subscriber's
  /// intent, creating a Save intent from its disk state if it has
  /// none. A Delete intent is left untouched, because deleting wins.
  pub fn apply_hiderel_delta_to_subscriber (
    &mut self,
    subscriber       : NodeComplete,
    inferred_hides   : &[ID],
    inferred_unhides : &[ID],
  ) {
    let base_hides : MSV<ID> =
      members_msv (&subscriber . hides_from_its_subscriptions);
    if let Some (intent) =
      self . by_pid . get_mut (&subscriber . pid)
    { intent . apply_hiderel_delta (
        &base_hides,
        inferred_hides,
        inferred_unhides);
      return; }
    let mut intent : NodeIntent =
      NodeIntent::graph_save_from_nodecomplete (
        subscriber . clone());
    intent . apply_hiderel_delta (
      &base_hides,
      inferred_hides,
      inferred_unhides);
    let pid : ID =
      subscriber . pid;
    self . order . push (pid . clone());
    self . by_pid . insert (pid, intent); }}
