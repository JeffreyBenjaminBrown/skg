/// This file defines the types of local instruction collection
/// (TODO/DONE/local-instruction-collection/3_plan.org), plus the
/// instructionMerge insert function that accumulates emissions.
/// .
/// TERMINOLOGY: 'merge' is always qualified. 'nodeMerge' is the
/// acquirer/acquiree operation on graphnodes; 'instructionMerge' is
/// the combining of emitted fieldIntents into the accumulator map. What
/// collection emits are *fieldIntents* -- some are unresolved signals --
/// and they become NodeInstruction *instructions* only after downstream
/// resolution and disk supplementation.

use crate::types::misc::{ID, SkgRepoName};
use crate::types::nodes::complete::Flag;
use std::collections::HashMap;

/// A LocalContext is what flows down the traversal: each node
/// computes one of these for each of its children, and it carries
/// everything an fieldIntent emission needs to know about its ancestors.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LocalContext {
  TopLevel, // The node is a child of the BufferRoot.
  UnderVognode { // The node's parent is a vognode, a phantom, or a DeadViewnode.
    parent_if_editable : Option<ID>, }, // This is Some iff the parent is save-eligible.
  UnderDefiningFolder ( // The node is inside an AliasFolder, a SubscribeeFolder, or an OverriddenFolder.
    DefiningFolderRecorder ),
  SubscribeeAsSuchPosition { // The node is a direct child of a SubscribeeFolder.
    subscriber               : ID,
    subscriber_is_editable   : bool, },
  HiddenOutsidePosition { // The one derived-but-editable filter under a SubscribeeFolder.
    subscriber       : ID,
    is_saveEligible  : bool, },
  UnderWriteProtectedFolder, // The node is inside one of the six write-protected RoleFolders, an IDFolder, or a Property.
}

/// A DefiningFolderRecorder is what a defining folder knows about its recorder
/// (the folder's parent). The id and editability are carried even
/// when the recorder is not save-eligible, because a SubscribeeFolder's
/// children need them for text claims, which outlive the visibility
/// guard.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DefiningFolderRecorder {
  pub skgid              : ID,
  pub is_editable     : bool, // True iff the recorder is Active and editable.
  pub is_saveEligible : bool, // True iff the recorder is editable, Active, carries no Delete request, and is not in subscribee-as-such position.
}

/// A FieldIntent is what one visit can emit. Each emission
/// pairs one of these with a target ID, and the pair is
/// instructionMerged into the accumulator.
/// The first eight kinds are exclusive (at most one per ID); the
/// last three are combineable (any number per ID). That distinction is
/// the shape of 'FieldIntentsForOneId': each exclusive kind gets an
/// Option slot there, and each combineable kind gets a Vec slot.
/// .
/// 'SetTitleAndBody' and 'Delete' carry the emitting node's skgrepo.
/// They are the self-emissions, and lowering a map entry to a
/// NodeInstruction needs to know which skgrepo's file it concerns.
/// .
/// 'SetContains' / 'SetSubscribesTo' / 'SetOverrides' pair each
/// member with an Option<RepoName>: Some when the position's
/// headline carried an explicit '(editRequest (relRepo NAME))'
/// request (the 'skg-set-relRepo' gesture), None meaning
/// "derive" (the usual
/// sticky-else-default rule). 'server/from_text/supplement_from_disk.rs'
/// validates the explicit skgrepos against each relationship's DEFAULT floor
/// at save time (render-and-gating, 5_plan.org;
/// BUG-and-fix_make-edge-more-public.org).
#[derive(Clone, Debug, PartialEq, Eq)]
#[allow(non_camel_case_types)]
pub enum FieldIntent {
  SetTitleAndBody { skgrepo : SkgRepoName,
                    title  : String,
                    body   : Option<String>, },
  SetContains     (Vec<(ID, Option<SkgRepoName>)>),
  SetAliases      (Vec<(String, Option<SkgRepoName>)>),
  SetSubscribesTo (Vec<(ID, Option<SkgRepoName>)>),
  SetOverrides    (Vec<(ID, Option<SkgRepoName>)>),
  Delete          { skgrepo : SkgRepoName },
  NodeMerge       { acquiree : ID },
  SetFlag     { flag : Flag, value : bool },
  // The remaining kinds are combineable.
  SubscribeeVisibility (SubscribeeVisibility),
  HiddenOutsideEdit  (HiddenOutsideEdit),
  SubscribeeTextClaim  (SubscribeeTextClaim),
}

/// This is an unresolved signal: the buffer presents 'visible' as
/// the visible content of 'subscribee', under some subscriber.
/// Resolution into hides/unhides edits on the subscriber needs disk
/// and the subscriber's own post-save contains, so it happens
/// downstream of collection.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SubscribeeVisibility {
  pub subscribee : ID,
  pub visible    : Vec<ID>, // These are the nodes *not* to hide.
}

/// The explicitly submitted visible-outside subset for one subscriber.
/// Unlike direct relationship sets it carries no skgrepo request: hide
/// skgrepos are derived after subscriptions and visibility are resolved.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HiddenOutsideEdit {
  pub members : Vec<ID>,
}

/// This is a validation-only signal: the buffer claims this title
/// and body for a node shown in subscribee-as-such position.
/// Downstream validation errors unless they match disk, because
/// title/body edits in that position are forbidden.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SubscribeeTextClaim {
  pub title : String,
  pub body  : Option<String>,
}

/// This accumulates the fieldIntents aimed at one ID. It is a slot
/// struct: "at most one of each exclusive fieldIntent kind" is the shape
/// of the type (the Option slots), and combineability is visible as
/// the Vec slots. One cross-slot rule stays procedural, in
/// 'instructionMerge_fieldIntent': 'delete' excludes the other exclusive
/// slots, and 'nodeMerge' excludes 'flag'.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct FieldIntentsForOneId {
  pub home_skgrepo   : Option<SkgRepoName>, // This is filled by the self-emissions (SetTitleAndBody and Delete).
  pub title_and_body : Option<(String, Option<String>)>,
  pub contains       : Option<Vec<(ID, Option<SkgRepoName>)>>,
  pub aliases        : Option<Vec<(String, Option<SkgRepoName>)>>,
  pub subscribes_to  : Option<Vec<(ID, Option<SkgRepoName>)>>,
  pub overrides      : Option<Vec<(ID, Option<SkgRepoName>)>>,
  pub delete         : bool,
  pub node_merge     : Option<ID>, // This holds the acquiree.
  pub flag       : Option<(Flag, bool)>,
  pub visibility     : Vec<SubscribeeVisibility>,  // This slot is combineable.
  pub hidden_outside : Vec<HiddenOutsideEdit>,     // This slot is combineable.
  pub text_claims    : Vec<SubscribeeTextClaim>,   // This slot is combineable, and is consumed only by validation.
}

/// This is the traversal's output.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct CollectedFieldIntents {
  pub order           : Vec<ID>, // This holds the IDs in first-emission order.
  pub lowerable_order : Vec<ID>, // This holds the IDs in first save-or-delete-emission order. It is a subset of 'order': an ID whose entry holds only signals appears in 'order' alone. Lowering uses this, so that an ID first seen as (say) a subscribee-as-such is saved at the position of its editable instance.
  pub by_pid          : HashMap<ID, FieldIntentsForOneId>,
}

impl FieldIntentsForOneId {
  /// True iff some exclusive slot other than 'delete' (and other than
  /// 'repo', which 'delete' itself fills) is occupied.
  fn some_nondelete_exclusive_slot_is_filled (
    &self,
  ) -> bool {
    self . title_and_body . is_some()
      || self . contains      . is_some()
      || self . aliases       . is_some()
      || self . subscribes_to . is_some()
      || self . overrides     . is_some()
      || self . node_merge    . is_some()
      || self . flag      . is_some() }}

impl CollectedFieldIntents {
  pub fn new (
  ) -> CollectedFieldIntents {
    CollectedFieldIntents::default() }

  /// This instructionMerges one emitted fieldIntent into the map.
  /// The rules (from TODO/DONE/local-instruction-collection/3_plan.org) are:
  /// - a combineable fieldIntent always pushes;
  /// - an exclusive fieldIntent into an empty slot fills it;
  /// - an exclusive fieldIntent into an occupied slot with an EQUAL
  ///   payload is a silent no-op;
  /// - an exclusive fieldIntent into an occupied slot with a different
  ///   payload is an error. Upstream validation should preclude this
  ///   (at most one editable instance per ID), so hitting it means
  ///   an upstream regression -- kept as a real error, not a panic;
  /// - 'Delete' alongside any other filled exclusive slot (or vice
  ///   versa) is an error, matching the old "Cannot have both Delete
  ///   and Save for same ID";
  /// - 'nodeMerge' and 'flag' are mutually exclusive, including
  ///   when duplicate occurrences of an ID emit them separately;
  /// - 'visibility' and 'text_claims' coexist with anything,
  ///   including 'delete' (resolution ignores deleted subscribers).
  #[allow(non_snake_case)]
  pub fn instructionMerge_fieldIntent (
    &mut self,
    target : ID,
    intent : FieldIntent,
  ) -> Result<(), String> {
    let entry : &mut FieldIntentsForOneId = {
      if ! self . by_pid . contains_key (&target) {
        self . order . push (target . clone()); }
      self . by_pid . entry (target . clone())
        . or_default() };
    match intent {
      FieldIntent::SubscribeeVisibility (v) => {
        entry . visibility . push (v);
        Ok (( )) },
      FieldIntent::HiddenOutsideEdit (edit) => {
        entry . hidden_outside . push (edit);
        Ok (( )) },
      FieldIntent::SubscribeeTextClaim (c) => {
        entry . text_claims . push (c);
        Ok (( )) },
      FieldIntent::Delete { skgrepo } => {
        if entry . some_nondelete_exclusive_slot_is_filled() {
          return Err ( format!(
            "Cannot have both Delete and Save for same ID: {}",
            target )); }
        fill_exclusive_slot (
          &mut entry . home_skgrepo, skgrepo, "repo", &target) ?;
        if ! entry . delete {
          entry . delete = true;
          self . lowerable_order . push (target); }
        Ok (( )) },
      _ => {
        if entry . delete {
          return Err ( format!(
            "Cannot have both Delete and Save for same ID: {}",
            target )); }
        match intent {
          FieldIntent::SetTitleAndBody { skgrepo, title, body } => {
            fill_exclusive_slot (
              &mut entry . home_skgrepo, skgrepo, "repo", &target) ?;
            let was_empty : bool =
              entry . title_and_body . is_none();
            fill_exclusive_slot (
              &mut entry . title_and_body, (title, body),
              "title/body", &target) ?;
            if was_empty {
              self . lowerable_order . push (target); }
            Ok (( )) },
          FieldIntent::SetContains (members) =>
            fill_exclusive_slot (
              &mut entry . contains, members, "contains", &target),
          FieldIntent::SetAliases (texts) =>
            fill_exclusive_slot (
              &mut entry . aliases, texts, "aliases", &target),
          FieldIntent::SetSubscribesTo (members) =>
            fill_exclusive_slot (
              &mut entry . subscribes_to, members,
              "subscribes_to", &target),
          FieldIntent::SetOverrides (members) =>
            fill_exclusive_slot (
              &mut entry . overrides, members,
              "overrides_view_of", &target),
          FieldIntent::NodeMerge { acquiree } => {
            if entry . flag . is_some() {
              return Err ( format!(
                "Cannot combine nodeMerge and flag requests for ID {}",
                target )); }
            fill_exclusive_slot (
              &mut entry . node_merge, acquiree,
              "nodeMerge acquiree", &target) },
          FieldIntent::SetFlag { flag, value } => {
            if entry . node_merge . is_some() {
              return Err ( format!(
                "Cannot combine nodeMerge and flag requests for ID {}",
                target )); }
            fill_exclusive_slot (
              &mut entry . flag, (flag, value),
              "flag", &target) },
          FieldIntent::Delete { .. }
            | FieldIntent::SubscribeeVisibility (_)
            | FieldIntent::HiddenOutsideEdit (_)
            | FieldIntent::SubscribeeTextClaim (_) =>
            unreachable! ("handled by the outer match"), }}}}
}

/// See 'instructionMerge_fieldIntent' for the rules this enforces.
fn fill_exclusive_slot<T : PartialEq + std::fmt::Debug> (
  slot    : &mut Option<T>,
  payload : T,
  label   : &str,
  target  : &ID,
) -> Result<(), String> {
  match slot {
    None => {
      *slot = Some (payload);
      Ok (( )) },
    Some (occupant) if *occupant == payload =>
      Ok (( )), // An equal payload is a silent no-op.
    Some (occupant) =>
      Err ( format!(
        "Conflicting {} intents for ID {} (should have been precluded by validation): {:?} vs {:?}",
        label, target, occupant, payload )), }}
