//! This file defines the types of local fieldIntent collection
//! (the glossary's "local fieldIntent collection"), plus the
//! instructionMerge insert function that accumulates emissions.
//! .
//! TERMINOLOGY: 'merge' is always qualified. 'nodeMerge' is the
//! acquirer/acquiree operation on graphnodes; 'instructionMerge' is
//! the combining of emitted fieldIntents into the accumulator map. What
//! collection emits are *fieldIntents* -- some are unresolved signals --
//! and they become NodeInstruction *instructions* only after downstream
//! resolution and disk supplementation.

use crate::types::errors::{HeadlineName, with_org_subheadlines};
use crate::types::misc::{ID, SkgrepoName};
use crate::types::nodes::complete::Flag;
use std::collections::HashMap;

/// A LocalContext is what flows down the traversal: each node
/// computes one of these for each of its children, and it carries
/// everything an fieldIntent emission needs to know about its ancestors.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LocalContext {
  TopLevel, // The node is a child of the BufferRoot.
  UnderVognode { // The node's parent is an occurrence, a ghost, an unknown, or a tombstone.
    parent_if_editable : Option<ID>, }, // This is Some iff the parent is save-eligible.
  UnderDefiningFolder ( // The node is inside an aliasFolder or a forksFolder.
    DefiningFolderRecorder ),
  ForkeeAsSuchPosition { // The node is a claiming direct child of a forksFolder: a forkee as such.
    forker                   : ID,
    forker_is_editable       : bool, },
  ForkerAsSuchPosition { // The node is a direct child of a forksFolder claiming nothing: a forker as such.
    forkee                   : ID,
    forkee_is_editable       : bool, },
  HiddenOutsidePosition { // The one derived-but-editable compound partner folder under a forksFolder.
    forker           : ID,
    is_saveEligible  : bool, },
  UnderWriteProtectedFolder, // The node is inside a write-protected partner folder, an idFolder, or a property member.
}

/// A DefiningFolderRecorder is what a defining folder knows about its recorder
/// (the folder's parent). The id and editability are carried even
/// when the recorder is not save-eligible, because a forksFolder's
/// children need them for text claims, which outlive the visibility
/// guard.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DefiningFolderRecorder {
  pub gnid              : ID,
  pub headline        : HeadlineName, // the recorder's occurrence, which emits the folder's fieldIntents
  pub is_editable     : bool, // True iff the recorder is an editable occurrence.
  pub is_saveEligible : bool, // True iff the recorder is an editable occurrence, carries no Delete request, and is no member of a forks folder.
}

/// A FieldIntent is what one visit can emit. Each emission
/// pairs one of these with a target ID, and the pair is
/// instructionMerged into the accumulator.
/// The first seven kinds are exclusive (at most one per ID); the
/// last five are combineable (any number per ID). That distinction is
/// the shape of 'FieldIntentsForOneId': each exclusive kind gets an
/// Option slot there, and each combineable kind gets a Vec slot.
/// .
/// 'SetTitleAndBody' and 'Delete' carry the emitting node's skgrepo.
/// They are the self-emissions, and lowering a map entry to a
/// NodeInstruction needs to know which skgrepo's file it concerns.
/// .
/// 'SetContains' / 'SetForks' pair each
/// member with an Option<RepoName>: Some when the position's
/// headline carried an explicit '(editRequest (relRepo NAME))'
/// request (the 'skg-set-relRepo' gesture), None meaning
/// "derive" (the usual
/// sticky-else-default rule). 'server/from_text/supplement_from_disk.rs'
/// validates the explicit skgrepos against each relationship's default floor
/// at save time ('docs/telescopes.org', "Setting a relationship's
/// skgrepo").
#[derive(Clone, Debug, PartialEq, Eq)]
#[allow(non_camel_case_types)]
pub enum FieldIntent {
  SetTitleAndBody { skgrepo : SkgrepoName,
                    title  : String,
                    body   : Option<String>, },
  SetContains     (Vec<(ID, Option<SkgrepoName>)>),
  SetAliases      (Vec<(String, Option<SkgrepoName>)>),
  SetForks        (Vec<(ID, Option<SkgrepoName>)>),
  Delete          { skgrepo : SkgrepoName },
  NodeMerge       { acquiree : ID },
  SetFlag     { flag : Flag, value : bool },
  // The remaining kinds are combineable.
  ForkeeVisibility (ForkeeVisibility),
  ForkerVisibility (ForkerVisibility),
  HiddenOutsideEdit  (HiddenOutsideEdit),
  MemberAsSuchTextClaim (MemberAsSuchTextClaim),
  ForksFolderMembers (ForksFolderMembers),
}

/// This is an unresolved signal: the buffer presents 'members' as
/// every member of the forks folder of a save-eligible note, claiming
/// or not. A note that forks it, by a record the skgrepo restriction
/// shows, that is not among them and that it does not fork after the
/// save was deleted from the folder, and joins the note's hides
/// ('resolve_forks_folder_removals'): a forker-only member, or a
/// mutual, whose fork the same save ends.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ForksFolderMembers {
  pub members : Vec<ID>,
}

/// This is an unresolved signal: the buffer presents 'visible' as
/// the visible content of 'forkee', under some forker.
/// Resolution into hides/unhides edits on the forker needs disk
/// and the forker's own post-save contains, so it happens
/// downstream of collection.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ForkeeVisibility {
  pub forkee     : ID,
  pub visible    : Vec<ID>, // These are the nodes *not* to hide.
}

/// This is an unresolved signal: the buffer presents 'visible' as
/// the visible content of 'forker', shown as such in the forks folder
/// of the note it forks. That content is shown in part (less what the
/// note contains or hides), so it is never read as the forker's whole
/// list: a child typed there joins it, a child the note hides shown
/// there is unhidden, and a child cut from it is refused, for now
/// ('resolve_forker_visibility').
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ForkerVisibility {
  pub forker     : ID,
  pub visible    : Vec<ID>, // These are the nodes *not* cut.
}

/// The explicitly submitted visible-outside subset for one forker.
/// Unlike direct relationship sets it carries no skgrepo request: hide
/// skgrepos are derived after forks and visibility are resolved.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HiddenOutsideEdit {
  pub members : Vec<ID>,
}

/// This is a validation-only signal: the buffer claims this title
/// and body for a foreign node shown as a member of a forks folder (a
/// forkee or forker as such). Downstream validation errors unless
/// they match disk, because title/body edits of a foreign node in
/// that position are forbidden.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MemberAsSuchTextClaim {
  pub title    : String,
  pub body     : Option<String>,
  pub headline : HeadlineName, // the occurrence claiming them
}

/// This accumulates the fieldIntents aimed at one ID. It is a slot
/// struct: "at most one of each exclusive fieldIntent kind" is the shape
/// of the type (the Option slots), and combineability is visible as
/// the Vec slots. One cross-slot rule stays procedural, in
/// 'instructionMerge_fieldIntent': 'delete' excludes the other exclusive
/// slots, and 'nodeMerge' excludes 'flag'.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct FieldIntentsForOneId {
  pub home_skgrepo   : Option<SkgrepoName>, // This is filled by the self-emissions (SetTitleAndBody and Delete).
  pub title_and_body : Option<(String, Option<String>)>,
  pub contains       : Option<Vec<(ID, Option<SkgrepoName>)>>,
  pub aliases        : Option<Vec<(String, Option<SkgrepoName>)>>,
  pub forks          : Option<Vec<(ID, Option<SkgrepoName>)>>,
  pub delete         : bool,
  pub node_merge     : Option<ID>, // This holds the acquiree.
  pub flag       : Option<(Flag, bool)>,
  pub visibility     : Vec<ForkeeVisibility>,  // This slot is combineable.
  pub forker_visibility : Vec<ForkerVisibility>, // This slot is combineable.
  pub hidden_outside : Vec<HiddenOutsideEdit>,     // This slot is combineable.
  pub forks_folder_members : Vec<ForksFolderMembers>, // This slot is combineable.
  pub text_claims    : Vec<MemberAsSuchTextClaim>,   // This slot is combineable, and is consumed only by validation.
}

/// This is the traversal's output.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct CollectedFieldIntents {
  pub order           : Vec<ID>, // This holds the IDs in first-emission order.
  pub exclusive_emitters : HashMap<ID, Vec<HeadlineName>>, // For each ID, the occurrences that emitted an exclusive fieldIntent for it, in emission order, for the conflict messages.
  pub lowerable_order : Vec<ID>, // This holds the IDs in first save-or-delete-emission order. It is a subset of 'order': an ID whose entry holds only signals appears in 'order' alone. Lowering uses this, so that an ID first seen as (say) a forkee-as-such is saved at the position of its editable instance.
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
      || self . forks . is_some()
      || self . node_merge    . is_some()
      || self . flag      . is_some() }}

impl CollectedFieldIntents {
  pub fn new (
  ) -> CollectedFieldIntents {
    CollectedFieldIntents::default() }

  /// This instructionMerges one emitted fieldIntent into the map.
  /// The rules are:
  /// - a combineable fieldIntent always pushes;
  /// - an exclusive fieldIntent into an empty slot fills it;
  /// - an exclusive fieldIntent into an occupied slot with an equal
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
  /// - the visibility signals and 'text_claims' coexist with anything,
  ///   including 'delete' (resolution ignores deleted forkers).
  /// An error lists the viewnode IDs of the occurrences that emitted
  /// exclusive fieldIntents for the ID, 'emitter' (the occurrence
  /// emitting this one) among them.
  #[allow(non_snake_case)]
  pub fn instructionMerge_fieldIntent (
    &mut self,
    emitter : &HeadlineName,
    target  : ID,
    intent  : FieldIntent,
  ) -> Result<(), String> {
    if ! matches! ( intent,
                    FieldIntent::ForkeeVisibility (_)
                    | FieldIntent::ForkerVisibility (_)
                    | FieldIntent::HiddenOutsideEdit (_)
                    | FieldIntent::MemberAsSuchTextClaim (_)
                    | FieldIntent::ForksFolderMembers (_) )
    { let emitters : &mut Vec<HeadlineName> =
        self . exclusive_emitters . entry (target . clone ())
        . or_default ();
      if ! emitters . contains (emitter) {
        emitters . push (emitter . clone ()); }}
    let emitters : &[HeadlineName] = // for the error messages
      self . exclusive_emitters . get (&target)
      . map ( Vec::as_slice )
      . unwrap_or ( &[] );
    let entry : &mut FieldIntentsForOneId = {
      if ! self . by_pid . contains_key (&target) {
        self . order . push (target . clone()); }
      self . by_pid . entry (target . clone())
        . or_default() };
    match intent {
      FieldIntent::ForkeeVisibility (v) => {
        entry . visibility . push (v);
        Ok (( )) },
      FieldIntent::ForkerVisibility (v) => {
        entry . forker_visibility . push (v);
        Ok (( )) },
      FieldIntent::HiddenOutsideEdit (edit) => {
        entry . hidden_outside . push (edit);
        Ok (( )) },
      FieldIntent::MemberAsSuchTextClaim (c) => {
        entry . text_claims . push (c);
        Ok (( )) },
      FieldIntent::ForksFolderMembers (members) => {
        entry . forks_folder_members . push (members);
        Ok (( )) },
      FieldIntent::Delete { skgrepo } => {
        if entry . some_nondelete_exclusive_slot_is_filled() {
          return Err ( with_org_subheadlines (
            &format! ( "Cannot have both Delete and Save for same ID: {}", target ),
            emitters )); }
        fill_exclusive_slot (
          &mut entry . home_skgrepo, skgrepo, "repo", &target, emitters) ?;
        if ! entry . delete {
          entry . delete = true;
          self . lowerable_order . push (target); }
        Ok (( )) },
      _ => {
        if entry . delete {
          return Err ( with_org_subheadlines (
            &format! ( "Cannot have both Delete and Save for same ID: {}", target ),
            emitters )); }
        match intent {
          FieldIntent::SetTitleAndBody { skgrepo, title, body } => {
            fill_exclusive_slot (
              &mut entry . home_skgrepo, skgrepo, "repo", &target, emitters) ?;
            let was_empty : bool =
              entry . title_and_body . is_none();
            fill_exclusive_slot (
              &mut entry . title_and_body, (title, body),
              "title/body", &target, emitters) ?;
            if was_empty {
              self . lowerable_order . push (target); }
            Ok (( )) },
          FieldIntent::SetContains (members) =>
            fill_exclusive_slot (
              &mut entry . contains, members, "contains", &target, emitters),
          FieldIntent::SetAliases (texts) =>
            fill_exclusive_slot (
              &mut entry . aliases, texts, "aliases", &target, emitters),
          FieldIntent::SetForks (members) =>
            fill_exclusive_slot (
              &mut entry . forks, members,
              "forks", &target, emitters),
          FieldIntent::NodeMerge { acquiree } => {
            if entry . flag . is_some() {
              return Err ( with_org_subheadlines (
                &format! ( "Cannot combine nodeMerge and flag requests for ID {}", target ),
                emitters )); }
            fill_exclusive_slot (
              &mut entry . node_merge, acquiree,
              "nodeMerge acquiree", &target, emitters) },
          FieldIntent::SetFlag { flag, value } => {
            if entry . node_merge . is_some() {
              return Err ( with_org_subheadlines (
                &format! ( "Cannot combine nodeMerge and flag requests for ID {}", target ),
                emitters )); }
            fill_exclusive_slot (
              &mut entry . flag, (flag, value),
              "flag", &target, emitters) },
          FieldIntent::Delete { .. }
            | FieldIntent::ForkeeVisibility (_)
            | FieldIntent::ForkerVisibility (_)
            | FieldIntent::HiddenOutsideEdit (_)
            | FieldIntent::MemberAsSuchTextClaim (_)
            | FieldIntent::ForksFolderMembers (_) =>
            unreachable! ("handled by the outer match"), }}}}
}

/// See 'instructionMerge_fieldIntent' for the rules this enforces.
fn fill_exclusive_slot<T : PartialEq + std::fmt::Debug> (
  slot     : &mut Option<T>,
  payload  : T,
  label    : &str,
  target   : &ID,
  emitters : &[HeadlineName], // the occurrences emitting for 'target'
) -> Result<(), String> {
  match slot {
    None => {
      *slot = Some (payload);
      Ok (( )) },
    Some (occupant) if *occupant == payload =>
      Ok (( )), // An equal payload is a silent no-op.
    Some (occupant) =>
      Err ( with_org_subheadlines (
        &format! (
          "Conflicting {} intents for ID {} (should have been precluded by validation): {:?} vs {:?}",
          label, target, occupant, payload ),
        emitters )), }}
