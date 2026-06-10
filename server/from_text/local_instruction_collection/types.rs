/// This file defines the types of local instruction collection
/// (TODO/local-instruction-collection/3_plan.org), plus the
/// instructionMerge insert function that accumulates emissions.
/// .
/// TERMINOLOGY: 'merge' is always qualified. 'nodeMerge' is the
/// acquirer/acquiree operation on graph nodes; 'instructionMerge' is
/// the combining of emitted intents into the accumulator map. What
/// collection emits are *intents* -- some are unresolved signals --
/// and they become DefineNode *instructions* only after downstream
/// resolution and disk supplementation.

use crate::types::misc::{ID, SourceName};
use std::collections::HashMap;

/// A LocalContext is what flows down the traversal: each node
/// computes one of these for each of its children, and it carries
/// everything an intent emission needs to know about its ancestors.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LocalContext {
  TopLevel, // The node is a child of the BufferRoot.
  UnderVognode { // The node's parent is a vognode, a phantom, or a DeadScaffold.
    parent_if_writeable : Option<ID>, }, // This is Some iff the parent is save-eligible.
  UnderDefiningCol ( // The node is inside an AliasCol, a SubscribeeCol, or an OverriddenCol.
    DefiningColOwner ),
  SubscribeeAsSuchPosition { // The node is a direct child of a SubscribeeCol.
    subscriber               : ID,
    subscriber_is_definitive : bool, },
  UnderReadOnlyCol, // The node is inside one of the six read-only RoleCols, an IDCol, or a Qual.
}

/// A DefiningColOwner is what a defining col knows about its owner
/// (the col's parent). The id and definitiveness are carried even
/// when the owner is not save-eligible, because a SubscribeeCol's
/// children need them for text claims, which outlive the visibility
/// guard.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DefiningColOwner {
  pub id              : ID,
  pub is_definitive   : bool, // True iff the owner is Active and definitive.
  pub is_saveEligible : bool, // True iff the owner is definitive, Active, carries no Delete request, and is not in subscribee-as-such position.
}

/// A NodeIntent_Local is what one visit can emit. Each emission
/// pairs one of these with a target ID, and the pair is
/// instructionMerged into the accumulator.
/// The first seven kinds are exclusive (at most one per ID); the
/// last two are combineable (any number per ID). That distinction is
/// the shape of 'IntentsForOneId': each exclusive kind gets an
/// Option slot there, and each combineable kind gets a Vec slot.
/// .
/// 'SetTitleAndBody' and 'Delete' carry the emitting node's source.
/// They are the self-emissions, and lowering a map entry to a
/// DefineNode needs to know which source's file it concerns.
#[derive(Clone, Debug, PartialEq, Eq)]
#[allow(non_camel_case_types)]
pub enum NodeIntent_Local {
  SetTitleAndBody { source : SourceName,
                    title  : String,
                    body   : Option<String>, },
  SetContains     (Vec<ID>),
  SetAliases      (Vec<String>),
  SetSubscribesTo (Vec<ID>),
  SetOverrides    (Vec<ID>),
  Delete          { source : SourceName },
  NodeMerge       { acquiree : ID },
  // The remaining kinds are combineable.
  SubscribeeVisibility (SubscribeeVisibility),
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

/// This is a validation-only signal: the buffer claims this title
/// and body for a node shown in subscribee-as-such position.
/// Downstream validation errors unless they match disk, because
/// title/body edits in that position are forbidden.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SubscribeeTextClaim {
  pub title : String,
  pub body  : Option<String>,
}

/// This accumulates the intents aimed at one ID. It is a slot
/// struct: "at most one of each exclusive intent kind" is the shape
/// of the type (the Option slots), and combineability is visible as
/// the Vec slots. One cross-slot rule stays procedural, in
/// 'instructionMerge_intent': 'delete' excludes the other exclusive
/// slots.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct IntentsForOneId {
  pub source         : Option<SourceName>, // This is filled by the self-emissions (SetTitleAndBody and Delete).
  pub title_and_body : Option<(String, Option<String>)>,
  pub contains       : Option<Vec<ID>>,
  pub aliases        : Option<Vec<String>>,
  pub subscribes_to  : Option<Vec<ID>>,
  pub overrides      : Option<Vec<ID>>,
  pub delete         : bool,
  pub node_merge     : Option<ID>, // This holds the acquiree.
  pub visibility     : Vec<SubscribeeVisibility>,  // This slot is combineable.
  pub text_claims    : Vec<SubscribeeTextClaim>,   // This slot is combineable, and is consumed only by validation.
}

/// This is the traversal's output.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct CollectedIntents {
  pub order           : Vec<ID>, // This holds the IDs in first-emission order.
  pub lowerable_order : Vec<ID>, // This holds the IDs in first save-or-delete-emission order. It is a subset of 'order': an ID whose entry holds only signals appears in 'order' alone. Lowering uses this, so that an ID first seen as (say) a subscribee-as-such is saved at the position of its definitive instance.
  pub by_pid : HashMap<ID, IntentsForOneId>,
}

impl IntentsForOneId {
  /// True iff some exclusive slot other than 'delete' (and other than
  /// 'source', which 'delete' itself fills) is occupied.
  fn some_nondelete_exclusive_slot_is_filled (
    &self,
  ) -> bool {
    self . title_and_body . is_some()
      || self . contains      . is_some()
      || self . aliases       . is_some()
      || self . subscribes_to . is_some()
      || self . overrides     . is_some()
      || self . node_merge    . is_some() }}

impl CollectedIntents {
  pub fn new (
  ) -> CollectedIntents {
    CollectedIntents::default() }

  /// This instructionMerges one emitted intent into the map.
  /// The rules (from TODO/local-instruction-collection/3_plan.org) are:
  /// - a combineable intent always pushes;
  /// - an exclusive intent into an empty slot fills it;
  /// - an exclusive intent into an occupied slot with an EQUAL
  ///   payload is a silent no-op;
  /// - an exclusive intent into an occupied slot with a different
  ///   payload is an error. Upstream validation should preclude this
  ///   (at most one definitive instance per ID), so hitting it means
  ///   an upstream regression -- kept as a real error, not a panic;
  /// - 'Delete' alongside any other filled exclusive slot (or vice
  ///   versa) is an error, matching the old "Cannot have both Delete
  ///   and Save for same ID";
  /// - 'visibility' and 'text_claims' coexist with anything,
  ///   including 'delete' (resolution ignores deleted subscribers).
  #[allow(non_snake_case)]
  pub fn instructionMerge_intent (
    &mut self,
    target : ID,
    intent : NodeIntent_Local,
  ) -> Result<(), String> {
    let entry : &mut IntentsForOneId = {
      if ! self . by_pid . contains_key (&target) {
        self . order . push (target . clone()); }
      self . by_pid . entry (target . clone())
        . or_default() };
    match intent {
      NodeIntent_Local::SubscribeeVisibility (v) => {
        entry . visibility . push (v);
        Ok (( )) },
      NodeIntent_Local::SubscribeeTextClaim (c) => {
        entry . text_claims . push (c);
        Ok (( )) },
      NodeIntent_Local::Delete { source } => {
        if entry . some_nondelete_exclusive_slot_is_filled() {
          return Err ( format!(
            "Cannot have both Delete and Save for same ID: {}",
            target )); }
        fill_exclusive_slot (
          &mut entry . source, source, "source", &target) ?;
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
          NodeIntent_Local::SetTitleAndBody { source, title, body } => {
            fill_exclusive_slot (
              &mut entry . source, source, "source", &target) ?;
            let was_empty : bool =
              entry . title_and_body . is_none();
            fill_exclusive_slot (
              &mut entry . title_and_body, (title, body),
              "title/body", &target) ?;
            if was_empty {
              self . lowerable_order . push (target); }
            Ok (( )) },
          NodeIntent_Local::SetContains (members) =>
            fill_exclusive_slot (
              &mut entry . contains, members, "contains", &target),
          NodeIntent_Local::SetAliases (texts) =>
            fill_exclusive_slot (
              &mut entry . aliases, texts, "aliases", &target),
          NodeIntent_Local::SetSubscribesTo (members) =>
            fill_exclusive_slot (
              &mut entry . subscribes_to, members,
              "subscribes_to", &target),
          NodeIntent_Local::SetOverrides (members) =>
            fill_exclusive_slot (
              &mut entry . overrides, members,
              "overrides_view_of", &target),
          NodeIntent_Local::NodeMerge { acquiree } =>
            fill_exclusive_slot (
              &mut entry . node_merge, acquiree,
              "nodeMerge acquiree", &target),
          NodeIntent_Local::Delete { .. }
            | NodeIntent_Local::SubscribeeVisibility (_)
            | NodeIntent_Local::SubscribeeTextClaim (_) =>
            unreachable! ("handled by the outer match"), }}}}
}

/// See 'instructionMerge_intent' for the rules this enforces.
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
