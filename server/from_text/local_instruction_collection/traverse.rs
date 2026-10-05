/// This file defines the pure traversal at the heart of local
/// instruction collection (TODO/DONE/local-instruction-collection/3_plan.org).
/// .
/// The traversal is one recursive DFS preorder over the placed
/// viewforest. Recursion is UNCONDITIONAL: every node's children are
/// visited, whatever the node's kind, because the user can attach
/// independent nodes anywhere. Only emission is conditional, on the
/// pair (kind, context). Each visit reads the node and its direct
/// children, nothing deeper, nothing upward; everything an
/// fieldIntent emission needs from above arrives in its
/// 'LocalContext'.
/// .
/// The traversal ASSUMES that 'find_buffer_errors_for_saving' has
/// passed. In particular it assumes that:
/// - every vognode has a PID and a config-valid skgrepo;
/// - each ID has at most one editable instance
///   ('Multiple_Defining_Viewnodes');
/// - same-ID instances have consistent toDelete values and skgrepos;
/// - folder shapes are valid: each folder holds only the child kinds it
///   permits, each folder is unique among its siblings, content members
///   have distinct IDs per
///   'nonignored_children_have_distinct_ids', and write-protected-folder
///   members have distinct IDs per
///   'partnerFolder_children_have_distinct_ids'. The defining folders
///   (AliasFolder, SubscribeeFolder, OverriddenFolder) may contain
///   duplicates; emission dedups them silently.
/// Collection performs no ancestry re-verification. On shapes that
/// validation precludes, it stays total, emitting nothing rather
/// than erroring.
/// .
/// DEFINITION: a vognode is *save-eligible* iff it is Active,
/// editable, lacks a Delete edit request, and is not in
/// subscribee-as-such position. (Its position may be anywhere else --
/// including as a member of a write-protected folder, where it is
/// save-eligible for itself but invisible to the folder's recorder.)
/// .
/// DEFINITION: a vognode is *in subscribee-as-such position* iff it
/// is an Active, affectsParent=true direct child of a SubscribeeFolder.
/// A non-member child of a SubscribeeFolder is not a member of the
/// folder, hence not shown *as* a subscribee: it is an ordinary
/// self-writer parked there.

use crate::from_text::local_instruction_collection::predicates::{
  active_child_counts_as_content,
  active_child_counts_as_visible_content,
  member_counts_for_partnerFolder };
use crate::from_text::local_instruction_collection::types::{
  CollectedFieldIntents, DefiningFolderRecorder, LocalContext, FieldIntent,
  HiddenOutsideEdit, SubscribeeTextClaim, SubscribeeVisibility };
use crate::types::misc::{ID, SkgRepoName};
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{
  NodeEditRequest, AffectsParent, Property, PropertyFolder, PartnerFolder, ActiveVognode, Viewnode,
  ViewnodeKind, Vognode, Phantom };

use ego_tree::NodeRef;
use std::collections::HashSet;

pub fn collect_instructions_locally (
  forest : &ViewForest,
) -> Result<CollectedFieldIntents, String> {
  let mut collected : CollectedFieldIntents =
    CollectedFieldIntents::new();
  for root in forest . roots() {
    visit (root, &LocalContext::TopLevel, &mut collected) ?; }
  Ok (collected) }

fn visit (
  node_ref  : NodeRef<Viewnode>,
  context   : &LocalContext,
  collected : &mut CollectedFieldIntents,
) -> Result<(), String> {
  match &node_ref . value() . kind {
    ViewnodeKind::Vognode (Vognode::Active (t)) =>
      visit_active_vognode (node_ref, t, context, collected),
    ViewnodeKind::Vognode (Vognode::Inactive (_)) =>
      // An Inactive vognode is anonymous and emits nothing; its
      // membership is owned by disk supplementation, not extraction. With no
      // identity it owns no defining folder, so (like a DeadViewnode) any
      // folder found under it stays silent.
      recurse_under_vognode (node_ref, None, None, collected),
    ViewnodeKind::Vognode (Vognode::Phantom (p)) =>
      recurse_under_vognode (
        node_ref,
        Some ( DefiningFolderRecorder {
          // Carrying the phantom's identity (write-protected, not
          // save-eligible) keeps a SubscribeeFolder found under a diff
          // phantom meaningful: its children stay in
          // subscribee-as-such position, so their title edits bounce
          // via text claims instead of silently becoming real edits.
          skgid               : p . skgid() . clone(),
          is_editable    : false,
          is_saveEligible : false } ),
        None, collected),
    ViewnodeKind::DeadViewnode =>
      recurse_under_vognode (node_ref, None, None, collected),
    ViewnodeKind::BufferRoot =>
      // A BufferRoot is unreachable as a child, but the traversal
      // stays total anyway.
      recurse_with_uniform_context (
        node_ref, &LocalContext::TopLevel, collected),
    ViewnodeKind::PropertyFolder (PropertyFolder::Alias) =>
      visit_aliasFolder (node_ref, context, collected),
    // The two arms below are exactly the FolderPolicy::EditableSet
    // PartnerFolders; the catch-all PartnerFolder arm after them covers the
    // WriteProtectedSet and WriteProtectedFilter policies. If a new PartnerFolder is
    // added, 'PartnerFolder::policy' says which group it joins.
    ViewnodeKind::PartnerFolder (PartnerFolder::Subscribee) =>
      visit_subscribee_folder (node_ref, context, collected),
    ViewnodeKind::PartnerFolder (PartnerFolder::HiddenOutsideOfSubscribee) =>
      visit_hiddenOutside_folder (node_ref, context, collected),
    ViewnodeKind::PartnerFolder (PartnerFolder::Overridden) =>
      visit_overridden_folder (node_ref, context, collected),
    ViewnodeKind::PropertyFolder (PropertyFolder::ID)
      | ViewnodeKind::PropertyFolder (PropertyFolder::Flags { .. })
      | ViewnodeKind::Property (_)
      | ViewnodeKind::PartnerFolder (_) =>
      // These are the write-protected folders and the Property leaves. Vognodes
      // found inside them are self-writers; their membership in the
      // folder is never read.
      recurse_with_uniform_context (
        node_ref, &LocalContext::UnderWriteProtectedFolder, collected), }}

fn visit_active_vognode (
  node_ref  : NodeRef<Viewnode>,
  t         : &ActiveVognode,
  context   : &LocalContext,
  collected : &mut CollectedFieldIntents,
) -> Result<(), String> {
  let subscribee_as_such_context : Option<(&ID, bool)> =
    match context {
      LocalContext::SubscribeeAsSuchPosition {
        subscriber, subscriber_is_editable }
        if t . affectsParent == AffectsParent::True =>
        Some (( subscriber, *subscriber_is_editable )),
      _ => None };
  let is_editable : bool =
    ! t . is_writeProtected();
  let has_delete_request : bool =
    matches!( t . edit_request(),
              Some (&NodeEditRequest::Delete));
  let is_saveEligible : bool =
    is_editable
    && ! has_delete_request
    && subscribee_as_such_context . is_none();
  if is_editable {
    // Emission happens only inside this block, because an
    // write-protected vognode emits nothing.
    match subscribee_as_such_context {
      Some (( subscriber, subscriber_is_editable )) => {
        collected . instructionMerge_fieldIntent (
          t . skgid . clone(),
          FieldIntent::SubscribeeTextClaim (
            SubscribeeTextClaim {
              title : t . title . clone(),
              body  : t . body() . cloned() } )) ?;
        if subscriber_is_editable {
          // The at-most-one-writer-per-ID guard: only the
          // SubscribeeFolder under the editable instance of a
          // subscriber may write its hide edits. The same subscriber
          // can recur write-protected elsewhere with its own
          // SubscribeeFolder; without this guard those could emit
          // contradictory hide edits for one ID.
          collected . instructionMerge_fieldIntent (
            subscriber . clone(),
            FieldIntent::SubscribeeVisibility (
              SubscribeeVisibility {
                subscribee : t . skgid . clone(),
                visible    : visible_content_members (node_ref) } )) ?; }
        // Delete and NodeMerge edit requests here are ignored: a
        // subscribee-as-such can affect only what its subscriber
        // hides.
      },
      None => {
        if has_delete_request {
          collected . instructionMerge_fieldIntent (
            t . skgid . clone(),
            FieldIntent::Delete {
              skgrepo : t . home_skgrepo . clone() } ) ?;
        } else {
          collected . instructionMerge_fieldIntent (
            t . skgid . clone(),
            FieldIntent::SetTitleAndBody {
              skgrepo : t . home_skgrepo . clone(),
              title   : t . title . clone(),
              body    : t . body() . cloned() } ) ?;
          collected . instructionMerge_fieldIntent (
            t . skgid . clone(),
            // This is always emitted, even if empty: an editable
            // node's content is always Specified.
            FieldIntent::SetContains (
              content_members (node_ref) )) ?;
          if let Some (NodeEditRequest::NodeMerge (acquiree)) =
            t . edit_request()
          { collected . instructionMerge_fieldIntent (
              t . skgid . clone(),
              FieldIntent::NodeMerge {
                acquiree : acquiree . clone() } ) ?; }
          if let Some (NodeEditRequest::SetFlag { flag, value }) =
            t . edit_request()
          { collected . instructionMerge_fieldIntent (
              t . skgid . clone(),
              FieldIntent::SetFlag {
                flag : *flag, value : *value } ) ?; }}},}}
  recurse_under_vognode (
    node_ref,
    Some ( DefiningFolderRecorder {
      skgid               : t . skgid . clone(),
      is_editable,
      is_saveEligible } ),
    if is_saveEligible { Some (t . skgid . clone()) } else { None },
    collected) }

/// This recurses into a vognode's children. Vognode
/// children get 'UnderVognode'; defining-folder children get
/// 'UnderDefiningFolder', carrying the recorder's identity (when it has
/// one); and write-protected folders and Properties get 'UnderWriteProtectedFolder'.
fn recurse_under_vognode (
  node_ref            : NodeRef<Viewnode>,
  recorder            : Option<DefiningFolderRecorder>,
  parent_if_editable  : Option<ID>,
  collected           : &mut CollectedFieldIntents,
) -> Result<(), String> {
  for child in node_ref . children() {
    let child_context : LocalContext =
      match &child . value() . kind {
        ViewnodeKind::PropertyFolder (PropertyFolder::Alias)
          // The two PartnerFolders here are exactly the
          // FolderPolicy::EditableSet ones; the write-protected policies fall
          // to the UnderWriteProtectedFolder arm below.
          | ViewnodeKind::PartnerFolder (PartnerFolder::Subscribee)
          | ViewnodeKind::PartnerFolder (PartnerFolder::Overridden) =>
          match &recorder {
            Some (o) =>
              LocalContext::UnderDefiningFolder (o . clone()),
            None =>
              // The recorder has no identity (it is a DeadViewnode), so
              // the folder will stay silent.
              LocalContext::UnderVognode {
                parent_if_editable : None } },
        ViewnodeKind::PropertyFolder (
          PropertyFolder::ID | PropertyFolder::Flags { .. })
          | ViewnodeKind::Property (_)
          | ViewnodeKind::PartnerFolder (_) =>
          LocalContext::UnderWriteProtectedFolder,
        _ =>
          LocalContext::UnderVognode {
            parent_if_editable : parent_if_editable . clone() } };
    visit (child, &child_context, collected) ?; }
  Ok (( )) }

fn recurse_with_uniform_context (
  node_ref  : NodeRef<Viewnode>,
  context   : &LocalContext,
  collected : &mut CollectedFieldIntents,
) -> Result<(), String> {
  for child in node_ref . children() {
    visit (child, context, collected) ?; }
  Ok (( )) }

fn visit_aliasFolder (
  node_ref  : NodeRef<Viewnode>,
  context   : &LocalContext,
  collected : &mut CollectedFieldIntents,
) -> Result<(), String> {
  if let LocalContext::UnderDefiningFolder (recorder) = context {
    if recorder . is_saveEligible {
      let aliases : Vec<(String, Option<SkgRepoName>)> = {
        let mut aliases : Vec<(String, Option<SkgRepoName>)> = Vec::new();
        let mut seen : HashSet<String> = HashSet::new ();
        for child in node_ref . children() {
          if let ViewnodeKind::Property (Property::Alias {
            text, relRepo_request, .. })
            = &child . value() . kind
          { if seen . insert (text . clone ()) {
              aliases . push (( text . clone (),
                                relRepo_request . clone () )); }} }
        aliases };
      // The MSV semantics are: an absent folder emits no fieldIntent, which
      // lowers to Unspecified, while a present-but-empty folder emits
      // Specified(vec![]).
      collected . instructionMerge_fieldIntent (
        recorder . skgid . clone(),
        FieldIntent::SetAliases (aliases) ) ?; }}
  recurse_with_uniform_context (
    node_ref, &LocalContext::UnderWriteProtectedFolder, collected) }

fn visit_subscribee_folder (
  node_ref  : NodeRef<Viewnode>,
  context   : &LocalContext,
  collected : &mut CollectedFieldIntents,
) -> Result<(), String> {
  match context {
    LocalContext::UnderDefiningFolder (recorder) => {
      if recorder . is_saveEligible {
        collected . instructionMerge_fieldIntent (
          recorder . skgid . clone(),
          FieldIntent::SetSubscribesTo (
            subscribeeFolder_members (node_ref) )) ?; }
      for child in node_ref . children() {
        let child_context : LocalContext =
          match &child . value() . kind {
            ViewnodeKind::PartnerFolder (PartnerFolder::HiddenOutsideOfSubscribee) =>
              LocalContext::HiddenOutsidePosition {
                subscriber      : recorder . skgid . clone(),
                is_saveEligible : recorder . is_saveEligible },
            _ =>
              // The subscriber's identity is passed even when the recorder is
              // not save-eligible, because text claims outlive the
              // visibility guard.
              LocalContext::SubscribeeAsSuchPosition {
                subscriber               : recorder . skgid . clone(),
                subscriber_is_editable : recorder . is_editable }, };
        visit (child, &child_context, collected) ?; }
      Ok (( )) },
    _ =>
      // The folder has no identifiable recorder. Validation precludes this
      // shape; the traversal stays total and silent.
      recurse_with_uniform_context (
        node_ref,
        &LocalContext::UnderVognode { parent_if_editable : None },
        collected), }}

/// Collect the explicitly submitted visible-outside subset.  This folder is a
/// derived filter rather than a direct relationship set, so its meaning is
/// resolved only after ordinary subscribee visibility inference has run.
fn visit_hiddenOutside_folder (
  node_ref  : NodeRef<Viewnode>,
  context   : &LocalContext,
  collected : &mut CollectedFieldIntents,
) -> Result<(), String> {
  if let LocalContext::HiddenOutsidePosition {
    subscriber, is_saveEligible } = context
  {
    if *is_saveEligible {
      let mut members : Vec<ID> = Vec::new ();
      for child in node_ref . children() {
        match &child . value() . kind {
          ViewnodeKind::Vognode (Vognode::Active (t))
            if member_counts_for_partnerFolder (t) => {
              if t . relRepo_request . is_some () {
                return Err ("HiddenOutsideOfSubscribee membership is editable, but hide relRepos are derived." . to_string ()); }
              members . push (t . skgid . clone ()); },
          ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (unknown))) => {
            if unknown . relRepo_request . is_some () {
              return Err ("HiddenOutsideOfSubscribee membership is editable, but hide relRepos are derived." . to_string ()); }
            members . push (unknown . skgid . clone ()); },
          _ => {}, }}
      collected . instructionMerge_fieldIntent (
        subscriber . clone(),
        FieldIntent::HiddenOutsideEdit (HiddenOutsideEdit { members }) ) ?; }}
  recurse_with_uniform_context (
    node_ref, &LocalContext::UnderWriteProtectedFolder, collected)
}

fn visit_overridden_folder (
  node_ref  : NodeRef<Viewnode>,
  context   : &LocalContext,
  collected : &mut CollectedFieldIntents,
) -> Result<(), String> {
  if let LocalContext::UnderDefiningFolder (recorder) = context {
    if recorder . is_saveEligible {
      collected . instructionMerge_fieldIntent (
        recorder . skgid . clone(),
        FieldIntent::SetOverrides (
          partnerFolder_members (node_ref) )) ?; }}
  recurse_with_uniform_context (
    node_ref,
    // The members are self-writers; their membership was read just
    // above, and they form no one's contains.
    &LocalContext::UnderVognode { parent_if_editable : None },
    collected) }

/// As 'dedup_vector', but dedups members carrying skgrepos by ID ALONE
/// (first occurrence wins) rather than by the full (ID, skgrepo) pair: a
/// duplicate ID with a DIFFERENT skgrepo request must still
/// be silently dropped, matching the existing defining-folder dedup
/// policy ("duplicate defining-folder members are silently deduped").
fn dedup_members_by_skgid (
  members : Vec<(ID, Option<SkgRepoName>)>,
) -> Vec<(ID, Option<SkgRepoName>)> {
  let mut seen   : std::collections::HashSet<ID> = std::collections::HashSet::new();
  let mut result : Vec<(ID, Option<SkgRepoName>)> = Vec::new();
  for (skgid, skgrepo) in members {
    if seen . insert (skgid . clone()) {
      result . push ((skgid, skgrepo)); }}
  result }

/// This returns the members of an OverriddenFolder: its Active
/// children that pass the PartnerFolder membership predicate, silently
/// deduplicated (by ID; see 'dedup_members_by_id'), preserving
/// first-occurrence order. Each member is paired with its headline's
/// explicit '(editRequest (relRepo NAME))' request, if any (see
/// 'FieldIntent').  (Inactive
/// children are NOT members here: the overriddenFolder omits inactive
/// members from display, and the set-difference merge preserves
/// them at save.  TODO/DONE/full-schema/DONE/9-2_source-set-safety.org.)
fn partnerFolder_members (
  node_ref : NodeRef<Viewnode>,
) -> Vec<(ID, Option<SkgRepoName>)> {
  let mut members : Vec<(ID, Option<SkgRepoName>)> = Vec::new();
  for child in node_ref . children() {
    match &child . value() . kind {
      ViewnodeKind::Vognode (Vognode::Active (t))
        if member_counts_for_partnerFolder (t) =>
          members . push ((t . skgid . clone(),
                           t . relRepo_request . clone())),
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (unknown))) =>
          members . push ((unknown . skgid . clone(),
                           unknown . relRepo_request . clone())),
      _ => {}, }}
  dedup_members_by_skgid (members) }

/// This returns the members of a SubscribeeFolder: its Active children
/// that pass the PartnerFolder membership predicate, deduplicated (by
/// ID; see 'dedup_members_by_id'). Like 'content_members' (and for
/// the same reason), inactive children contribute nothing:
/// 'subscribesTo' is order-meaningful, but disk supplementation (its weave)
/// already restores invisible subscribees at their disk position, so
/// a buffer-present inactive vognode must not feed this list.
/// Each member is paired with its headline's explicit
/// '(editRequest (relRepo NAME))' request, if any.
#[allow(non_snake_case)]
fn subscribeeFolder_members (
  node_ref : NodeRef<Viewnode>,
) -> Vec<(ID, Option<SkgRepoName>)> {
  let mut members : Vec<(ID, Option<SkgRepoName>)> = Vec::new();
  for child in node_ref . children() {
    match &child . value() . kind {
      ViewnodeKind::Vognode (Vognode::Active (t))
        if member_counts_for_partnerFolder (t) =>
          members . push ((t . skgid . clone(),
                           t . relRepo_request . clone())),
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (unknown))) =>
          members . push ((unknown . skgid . clone(),
                           unknown . relRepo_request . clone())),
      _ => {}, }}
  dedup_members_by_skgid (members) }

/// This returns the content of an editable vognode: its Active
/// children that pass the contains predicate. It does not dedup,
/// because validation ('nonignored_children_have_distinct_ids')
/// already guarantees distinctness. Each member is paired with its
/// headline's explicit '(editRequest (relRepo NAME))' request, if any (see
/// 'FieldIntent').
///
/// Inactive children contribute NOTHING here: an inactive node emits
/// no save intention for its container. Its membership in the
/// container's contains is owned entirely by disk supplementation
/// ('preserve_invisible_members' -> weave in from_text/weave.rs),
/// which restores invisible members from disk at their disk position.
/// Including a buffer-present inactive child would let a stale or
/// concurrently-edited buffer resurrect a member that was
/// authoritatively removed, and would persist reorderings of a
/// write-protected placeholder. (See TODO/problems.org, "Retained inactive
/// nodes emit positional save intentions for their container".)
fn content_members (
  node_ref : NodeRef<Viewnode>,
) -> Vec<(ID, Option<SkgRepoName>)> {
  let mut contents : Vec<(ID, Option<SkgRepoName>)> = Vec::new();
  for child in node_ref . children() {
    match &child . value() . kind {
      ViewnodeKind::Vognode (Vognode::Active (t)) => {
        if active_child_counts_as_content (t) {
          contents . push ((
            // collected_id, not id: a drawn overrider stands for
            // the original member it was drawn in place of.
            t . collected_skgid (),
            t . relRepo_request . clone() )); }},
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (unknown))) =>
        // An Unknown is save-inert as a node, but its raw ID is load-bearing
        // membership data at a structured relationship position. `None` asks
        // disk supplementation to keep an existing destination skgrepo sticky.
        contents . push (( unknown . skgid . clone(),
                           unknown . relRepo_request . clone() )),
      _ => {}, }}
  contents }

/// This returns the children that the buffer presents as visible
/// content of one subscribee-as-such. The list is not saved as the
/// subscribee's contains; it is the signal from which the
/// subscriber's hides/unhides are inferred, downstream. It is not
/// deduplicated.
fn visible_content_members (
  node_ref : NodeRef<Viewnode>,
) -> Vec<ID> {
  let mut visible : Vec<ID> = Vec::new();
  for child in node_ref . children() {
    match &child . value () . kind {
      ViewnodeKind::Vognode (Vognode::Active (t))
        if active_child_counts_as_visible_content (t) => {
        visible . push (
          // collected_id: a drawn overrider presents the original,
          // so hide/unhide inference must speak of the original.
          t . collected_skgid ()); },
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (unknown))) =>
        visible . push (unknown . skgid . clone ()),
      _ => {}, }}
  visible }
