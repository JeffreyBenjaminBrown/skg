/// This file defines lowering, which converts 'CollectedFieldIntents'
/// (the traversal's output) into ordered 'NodeIntent's -- the shape
/// that the downstream stages (visibility resolution, disk
/// supplementation, and the noop filter) consume. Lowering is pure
/// and synchronous, and reads no disk.
/// .
/// Entries lower as follows:
/// - A delete entry lowers to 'NodeIntent::Delete'.
/// - An entry with a title/body (that is, one owned by a
///   save-eligible editable occurrence) lowers to
///   'NodeIntent::Save', with its empty slots lowering to
///   'MSV::Unspecified'.
/// - An entry holding only unresolved signals (visibility fieldIntents
///   and text claims) lowers to no NodeIntent; the signals are
///   returned beside the nodeIntents, for the downstream stages that
///   consume them. ('node_merge' slots are likewise not lowered
///   here: nodeMerge extraction reads them via 'nodeMerge_pairs'
///   before lowering consumes the map.)

use crate::from_text::local_fieldintent_collection::types::{
  CollectedFieldIntents, HiddenOutsideEdit, FieldIntentsForOneId, SubscribeeVisibility };
use crate::types::misc::{
  ID, MSV, RelPartner, SkgRepoName, members_msv, members_of,
  rel_partners_at_relRepo, rel_partners_at_relRepo_msv };
use crate::types::nodes::complete::{Flag, Graphnode};
use crate::types::save::{NodeInstruction, SaveNode, DeleteNode};

use std::collections::{HashMap, HashSet};

/// What the user appears to intend for this node.
/// Might eventually become a NodeInstruction.
/// Uses MSV values in the Save variant (whereas NodeInstruction uses
/// SaveNode, which uses Graphnode, which specifies all values).
pub enum NodeIntent {
  Save   (NodeSaveIntent),
  Delete (DeleteNode), // NodeInstruction uses the same DeleteNode type
}

pub struct NodeSaveIntent {
  pub pid               : ID,
  pub home_skgrepo      : SkgRepoName,
  pub title             : String,
  pub body              : Option<String>,
  // contains / subscribesTo / overrides pair each member
  // with an Option<RepoName>: Some when the buffer's headline
  // carried an '(editRequest (relRepo NAME))' request (see
  // 'ActiveVognode_Generic::relRepo_request', 'FieldIntent'); None means
  // "derive" (sticky-else-default). 'requested_relRepos' extracts the
  // Some entries into a side-channel BEFORE 'into_graphnode'
  // discards them, for 'apply_sticky_relRepos' to validate against
  // each relationship's floor.
  pub contains          : MSV<(ID, Option<SkgRepoName>)>,
  pub extra_ids         : Vec<ID>,
  pub aliases           : MSV<(String, Option<SkgRepoName>)>,
  pub subscribesTo      : MSV<(ID, Option<SkgRepoName>)>,
  pub hidesFromSubs     : MSV<ID>,
  pub overrides         : MSV<(ID, Option<SkgRepoName>)>,
  pub flags              : Vec<Flag>,
  pub flag_request  : Option<(Flag, bool)>,
}

/// Repos the buffer explicitly requested via '(editRequest
/// (relRepo NAME))', keyed by member
/// ID, one map per relation that carries per-member relRepos (hides is
/// absent: it is inferred, and the folder that shows it is write-protected --
/// the set-relRepo gesture refuses there). Threaded
/// separately from
/// Graphnode because Graphnode's 'RelPartner::skgrepo' is a
/// plain RepoName with no "was this explicit" flag, and gets
/// unconditionally resolved by 'apply_sticky_relRepos' -- this is the
/// side-channel that tells that pass which members carry a real,
/// user-requested skgrepo to validate against the default
/// floor, rather than deriving normally (render-and-gating,
/// TODO/DONE/privacy-telescope/5_plan.org).
#[derive(Clone, Debug, Default)]
pub struct RequestedRelRepos {
  pub contains          : HashMap<ID, SkgRepoName>,
  pub aliases           : HashMap<String, SkgRepoName>,
  pub subscribesTo      : HashMap<ID, SkgRepoName>,
  pub overrides         : HashMap<ID, SkgRepoName>,
}

/// Strip the per-member explicit-repo payload down to plain IDs, by
/// reference (read-only consumers, e.g.
/// 'nodeSaveIntents_with_specified_contains').
fn skgids_only_msv_ref (
  msv : &MSV<(ID, Option<SkgRepoName>)>,
) -> MSV<ID> {
  match msv {
    MSV::Unspecified   => MSV::Unspecified,
    MSV::Specified (v) => MSV::Specified (
      v . iter () . map ( |(skgid, _)| skgid . clone () ) . collect () ), }}

/// As 'ids_only_msv_ref', consuming.
fn skgids_only_msv (
  msv : MSV<(ID, Option<SkgRepoName>)>,
) -> MSV<ID> {
  match msv {
    MSV::Unspecified   => MSV::Unspecified,
    MSV::Specified (v) => MSV::Specified (
      v . into_iter () . map ( |(skgid, _)| skgid ) . collect () ), }}

/// As above, for the non-MSV 'contains' slice.
fn skgids_only (
  list : &[(ID, Option<SkgRepoName>)],
) -> Vec<ID> {
  list . iter () . map ( |(skgid, _)| skgid . clone () ) . collect () }

impl NodeIntent {
  pub fn pid (
    &self,
  ) -> &ID {
    match self {
      NodeIntent::Save (intent) => &intent . pid,
      NodeIntent::Delete (intent) => &intent . skgid, }}

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

  pub fn graph_save_from_graphnode (
    node : Graphnode,
  ) -> NodeIntent {
    // No explicit skgrepos: this seeds a nodeIntent straight from disk
    // (an editable rebuild for hide-delta application), not from a
    // buffer headline that could carry a '(relRepo ...)' atom.
    // Preserves the MSV Unspecified/Specified distinction, unlike a
    // plain 'or_default()' round-trip.
    fn no_explicit_msv (
      msv : &MSV<RelPartner<ID>>,
    ) -> MSV<(ID, Option<SkgRepoName>)> {
      match msv {
        MSV::Unspecified   => MSV::Unspecified,
        MSV::Specified (v) => MSV::Specified (
          v . iter () . map ( |m| (m . member . clone (), None) ) . collect () ), }}
    NodeIntent::Save (NodeSaveIntent {
      pid                          : node . pid,
      home_skgrepo                 : node . home_skgrepo,
      title                        : node . title,
      body                         : node . body,
      contains                     : MSV::Specified (
        node . contains . iter ()
        . map ( |m| (m . member . clone (), None) ) . collect () ),
      extra_ids                    : node . extra_ids,
      aliases                      : match node . aliases {
        MSV::Unspecified => MSV::Unspecified,
        MSV::Specified (aliases) => MSV::Specified (
          aliases . into_iter ()
          . map ( |alias| (alias . member, None) )
          . collect () ), },
      subscribesTo                 : no_explicit_msv (&node . subscribesTo),
      hidesFromSubs                :
        members_msv (&node . hidesFromSubs),
      overrides                    : no_explicit_msv (&node . overrides),
      flags                         : node . flags,
      flag_request             : None,
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

  pub fn into_node_instruction (
    self,
  ) -> Result<NodeInstruction, String> {
    match self {
      NodeIntent::Delete (intent)
        => Ok (NodeInstruction::Delete (intent)),
      NodeIntent::Save (intent)
        => Ok (NodeInstruction::Save (SaveNode (
          intent . into_graphnode() ))) }}
}

impl NodeSaveIntent {
  pub fn fill_unspecified_contains (
    &mut self,
    contains : &[ID],
  ) {
    if self . contains . is_unspecified() {
      // Disk-derived filler: no per-member explicit skgrepo (that only
      // ever comes from a buffer headline's own '(relRepo ...)').
      self . contains =
        MSV::Specified ( contains . iter () . cloned ()
                          . map ( |skgid| (skgid, None) ) . collect () ); }}

  /// The skgrepos the buffer explicitly requested (its headlines'
  /// '(relRepo NAME)' atoms), read out BEFORE 'into_graphnode'
  /// discards the Option<RepoName> payload. See 'RequestedRelRepos'.
  pub fn requested_relRepos (
    &self,
  ) -> RequestedRelRepos {
    fn collect (
      list : &[(ID, Option<SkgRepoName>)],
    ) -> HashMap<ID, SkgRepoName> {
      list . iter ()
        . filter_map ( |(skgid, skgrepo)| skgrepo . clone ()
                       . map ( |s| (skgid . clone (), s) ) )
        . collect () }
    RequestedRelRepos {
      contains          : collect (self . contains . or_default ()),
      aliases           : self . aliases . or_default () . iter ()
        . filter_map ( |(text, skgrepo)| skgrepo . clone ()
          . map ( |skgrepo| (text . clone (), skgrepo) ) )
        . collect (),
      subscribesTo      : collect (self . subscribesTo . or_default ()),
      overrides         : collect (self . overrides . or_default ()),
    }}

  pub fn into_graphnode (
    self,
  ) -> Graphnode {
    let skgrepo  : SkgRepoName = self . home_skgrepo . clone();
    let mut node : Graphnode = Graphnode {
      title                        : self . title,
      overPrivateText_telescope               : false,
      aliases                      :
        rel_partners_at_relRepo_msv (
          &skgrepo,
          match self . aliases {
            MSV::Unspecified => MSV::Unspecified,
            MSV::Specified (aliases) => MSV::Specified (
              aliases . into_iter ()
              . map ( |(text, _)| text ) . collect () ), } ),
      home_skgrepo                 : self . home_skgrepo,
      pid                          : self . pid,
      extra_ids                    : self . extra_ids,
      body                         :
        crate::types::nodes::complete::normalize_body ( self . body ),
      contains                     :
        rel_partners_at_relRepo (
          &skgrepo, skgids_only (self . contains . or_default ()) ),
      subscribesTo                 :
        rel_partners_at_relRepo_msv (&skgrepo, skgids_only_msv (self . subscribesTo)),
      hidesFromSubs                :
        rel_partners_at_relRepo_msv (&skgrepo, self . hidesFromSubs),
      overrides                    :
        rel_partners_at_relRepo_msv (&skgrepo, skgids_only_msv (self . overrides)),
      flags                         : self . flags,
    };
    node . normalize_skgids ();
    node }

  fn apply_hiderel_delta (
    &mut self,
    base_hides       : &MSV<ID>,
    inferred_hides   : &[ID],
    inferred_unhides : &[ID],
  ) {
    let mut hides : Vec<ID> =
      if self . hidesFromSubs . is_unspecified() {
        base_hides . or_default() . to_vec()
      } else {
        self . hidesFromSubs . or_default() . to_vec()
      };
    hides . retain ( |skgid| ! inferred_unhides . contains (skgid) );
    for skgid in inferred_hides {
      if ! hides . contains (skgid) {
        hides . push (skgid . clone()); }}
    self . hidesFromSubs =
      MSV::Specified (hides); }}

/// This is an ordered map of one NodeIntent per PID; lowering
/// produces it, and visibility resolution mutates it. Its 'order'
/// field holds the PIDs in first save-or-delete-emission order.
pub struct LoweredNodeIntents {
  order  : Vec<ID>,
  by_pid : HashMap<ID, NodeIntent>,
}

pub struct LoweringOutput {
  pub intents        : LoweredNodeIntents,
  pub visibility     : Vec<(ID, SubscribeeVisibility)>, // Each pair is (subscriber, signal); the list is in subscriber first-emission order.
  pub hidden_outside : Vec<(ID, HiddenOutsideEdit)>,
}

/// This returns the (acquirer, acquiree) pair of every nodeMerge
/// request in the map, in acquirer first-emission order. Callers
/// must read this before lowering consumes the map.
#[allow(non_snake_case)]
pub fn nodeMerge_pairs (
  collected : &CollectedFieldIntents,
) -> Vec<(ID, ID)> {
  let mut pairs : Vec<(ID, ID)> = Vec::new();
  for pid in &collected . order {
    if let Some (acquiree) =
      collected . by_pid . get (pid)
      . and_then ( |entry| entry . node_merge . as_ref() )
    { pairs . push (( pid . clone(), acquiree . clone() )); }}
  pairs }

pub fn lower_collected_fieldIntents (
  collected : CollectedFieldIntents,
) -> Result<LoweringOutput, String> {
  let CollectedFieldIntents { order, lowerable_order, mut by_pid }
    = collected;
  let visibility : Vec<(ID, SubscribeeVisibility)> = {
    // The signals are extracted across ALL entries, in
    // first-emission order -- including from the signals-only
    // entries, which lower to no NodeIntent.
    let mut visibility : Vec<(ID, SubscribeeVisibility)> =
      Vec::new();
    for pid in &order {
      let entry : &FieldIntentsForOneId =
        by_pid . get (pid)
        . ok_or ( "lower_collected_fieldIntents: order names a PID missing from the map" . to_string() ) ?;
      for signal in &entry . visibility {
        visibility . push (( pid . clone(), signal . clone() )); }}
    visibility };
  let hidden_outside : Vec<(ID, HiddenOutsideEdit)> = {
    let mut edits : Vec<(ID, HiddenOutsideEdit)> = Vec::new ();
    for pid in &order {
      let entry : &FieldIntentsForOneId = by_pid . get (pid)
        . ok_or ( "lower_collected_fieldIntents: order names a PID missing from the map" . to_string () ) ?;
      for edit in &entry . hidden_outside {
        edits . push ((pid . clone (), edit . clone ())); }}
    edits };
  let mut intents : LoweredNodeIntents =
    LoweredNodeIntents {
      order  : Vec::with_capacity (lowerable_order . len()),
      by_pid : HashMap::with_capacity (lowerable_order . len()) };
  for pid in lowerable_order {
    let entry : FieldIntentsForOneId =
      by_pid . remove (&pid)
      . ok_or ( "lower_collected_fieldIntents: lowerable_order names a PID missing from the map" . to_string() ) ?;
    let intent : NodeIntent =
      lower_one_entry (&pid, entry) ?;
    intents . order . push (pid . clone());
    intents . by_pid . insert (pid, intent); }
  for (pid, leftover) in &by_pid {
    // Whatever remains holds only signals. FieldIntents can only
    // come from folders under a save-eligible recorder, and a
    // save-eligible recorder emits a title/body, putting its entry in
    // 'lowerable_order'; so anything else here is a collection bug.
    if leftover . contains . is_some()
      || leftover . aliases       . is_some()
      || leftover . subscribesTo . is_some()
      || leftover . overrides     . is_some()
      || leftover . node_merge    . is_some()
      || leftover . flag      . is_some()
    { return Err ( format!(
        "lower_collected_fieldIntents: entry for {} has field intents but no title/body",
        pid )); }}
  Ok (LoweringOutput { intents, visibility, hidden_outside }) }

/// This ASSUMES the entry is lowerable: it holds a delete or a
/// title/body, having been named by 'lowerable_order'.
fn lower_one_entry (
  pid   : &ID,
  entry : FieldIntentsForOneId,
) -> Result<NodeIntent, String> {
  if entry . delete {
    return Ok (NodeIntent::Delete (DeleteNode {
      skgid           : pid . clone(),
      home_skgrepo : entry . home_skgrepo . ok_or_else ( || format!(
        "lower_collected_fieldIntents: delete entry for {} lacks a repo",
        pid )) ?, } )); }
  match entry . title_and_body {
    None =>
      Err ( format!(
        "lower_collected_fieldIntents: lowerable entry for {} has neither delete nor title/body",
        pid )),
    Some (( title, body )) => {
      Ok (NodeIntent::Save (NodeSaveIntent {
        pid          : pid . clone(),
        home_skgrepo : entry . home_skgrepo . ok_or_else ( || format!(
          "lower_collected_fieldIntents: save entry for {} lacks a repo",
          pid )) ?,
        title,
        body,
        contains                     :
          msv_from_slot (entry . contains),
        extra_ids                    : vec![],
        aliases                      :
          msv_from_slot (entry . aliases),
        subscribesTo                 :
          msv_from_slot (entry . subscribesTo),
        hidesFromSubs                : MSV::Unspecified,
        overrides                    :
          msv_from_slot (entry . overrides),
        flags                    : Vec::new(),
        flag_request             : entry . flag,
      })) }} }

fn msv_from_slot<T> (
  slot : Option<Vec<T>>,
) -> MSV<T> {
  match slot {
    None      => MSV::Unspecified,
    Some (vs) => MSV::Specified (vs), }}

impl LoweredNodeIntents {
  pub fn into_ordered_intents (
    self,
  ) -> Vec<NodeIntent> {
    let LoweredNodeIntents { order, mut by_pid } = self;
    order . into_iter()
      . filter_map ( |pid| by_pid . remove (&pid) )
      . collect() }

  /// One (pid, skgrepo, contains, subscribesTo) tuple per Save
  /// nodeIntent whose contains is Specified -- the candidates for
  /// inferring hides from contains removals (see
  /// 'infer_hides_from_contains_removals'). Cloned out so the caller
  /// can mutate self (via 'apply_hiderel_delta_to_subscriber') while
  /// iterating.
  pub fn nodeSaveIntents_with_specified_contains (
    &self,
  ) -> Vec<(ID, SkgRepoName, Vec<ID>, MSV<ID>)> {
    self . order . iter ()
      . filter_map ( |pid| match self . by_pid . get (pid) {
          Some (NodeIntent::Save (intent)) =>
            match & intent . contains {
              MSV::Specified (contains) => Some ((
                pid . clone (),
                intent . home_skgrepo . clone (),
                skgids_only (contains),
                skgids_only_msv_ref (&intent . subscribesTo) )),
              _ => None },
          _ => None })
      . collect () }

  /// This returns the subscriber's contains as it will stand after
  /// this save: from its Save nodeIntent if it has one, and otherwise
  /// from disk.
  pub fn subscriber_contains_after_save (
    &self,
    subscriber_from_disk : &Graphnode,
  ) -> HashSet<ID> {
    match self . by_pid . get (&subscriber_from_disk . pid) {
      Some (NodeIntent::Save (intent)) =>
        intent . contains . or_default () . iter ()
        . map ( |(skgid, _)| skgid . clone () ) . collect (),
      _ =>
        members_of (&subscriber_from_disk . contains)
        . into_iter() . collect(),
    }}

  /// As 'subscriber_contains_after_save', but for the post-save list of
  /// subscribees.  Filter edits must classify hides against this list, not
  /// against a stale disk subscription that the same buffer removed.
  pub fn subscriber_subscribes_after_save (
    &self,
    subscriber_from_disk : &Graphnode,
  ) -> Vec<ID> {
    match self . by_pid . get (&subscriber_from_disk . pid) {
      Some (NodeIntent::Save (intent))
        if ! intent . subscribesTo . is_unspecified () =>
          skgids_only (intent . subscribesTo . or_default ()),
      _ =>
        members_of (
          subscriber_from_disk . subscribesTo . or_default () ),
    }}

  /// The hide IDs after the preceding inference stages.  Filter edits run
  /// last, so they calculate their delta against this intermediate state.
  pub fn subscriber_hides_after_resolution (
    &self,
    subscriber_from_disk : &Graphnode,
  ) -> Vec<ID> {
    match self . by_pid . get (&subscriber_from_disk . pid) {
      Some (NodeIntent::Save (intent))
        if ! intent . hidesFromSubs . is_unspecified () =>
          intent . hidesFromSubs . or_default () . to_vec (),
      _ =>
        members_of (
          subscriber_from_disk . hidesFromSubs . or_default () ),
    }}

  /// This applies inferred hides/unhides to the subscriber's
  /// nodeIntent, creating a Save nodeIntent from its disk state if it has
  /// none. A Delete nodeIntent is left untouched, because deleting wins.
  pub fn apply_hiderel_delta_to_subscriber (
    &mut self,
    subscriber       : Graphnode,
    inferred_hides   : &[ID],
    inferred_unhides : &[ID],
  ) {
    let base_hides : MSV<ID> =
      members_msv (&subscriber . hidesFromSubs);
    if let Some (intent) =
      self . by_pid . get_mut (&subscriber . pid)
    { intent . apply_hiderel_delta (
        &base_hides,
        inferred_hides,
        inferred_unhides);
      return; }
    let mut intent : NodeIntent =
      NodeIntent::graph_save_from_graphnode (
        subscriber . clone());
    intent . apply_hiderel_delta (
      &base_hides,
      inferred_hides,
      inferred_unhides);
    let pid : ID =
      subscriber . pid;
    self . order . push (pid . clone());
    self . by_pid . insert (pid, intent); }}
