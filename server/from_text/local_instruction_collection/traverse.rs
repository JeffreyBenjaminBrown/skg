/// This file defines the pure traversal at the heart of local
/// instruction collection (TODO/local-instruction-collection/3_plan.org).
/// .
/// The traversal is one recursive DFS preorder over the placed
/// viewforest. Recursion is UNCONDITIONAL: every node's children are
/// visited, whatever the node's kind, because the user can attach
/// independent nodes anywhere. Only emission is conditional, on the
/// pair (kind, context). Each visit reads the node and its direct
/// children, nothing deeper, nothing upward; everything an
/// intent emission needs from above arrives in its
/// 'LocalContext'.
/// .
/// The traversal ASSUMES that 'find_buffer_errors_for_saving' has
/// passed. In particular it assumes that:
/// - every vognode has a PID and a config-valid source;
/// - each ID has at most one definitive instance
///   ('Multiple_Defining_Viewnodes');
/// - same-ID instances have consistent toDelete values and sources;
/// - folder shapes are valid: each folder holds only the child kinds it
///   permits, each folder is unique among its siblings, content members
///   have distinct IDs per
///   'nonignored_children_have_distinct_ids', and read-only-folder
///   members have distinct IDs per
///   'partnerFolder_children_have_distinct_ids'. The defining folders
///   (AliasFolder, SubscribeeFolder, OverriddenFolder) may contain
///   duplicates; emission dedups them silently.
/// Collection performs no ancestry re-verification. On shapes that
/// validation precludes, it stays total, emitting nothing rather
/// than erroring.
/// .
/// DEFINITION: a vognode is *save-eligible* iff it is Active,
/// definitive, lacks a Delete edit request, and is not in
/// subscribee-as-such position. (Its position may be anywhere else --
/// including as a member of a read-only folder, where it is
/// save-eligible for itself but invisible to the folder's owner.)
/// .
/// DEFINITION: a vognode is *in subscribee-as-such position* iff it
/// is an Active, affectsParent=Affected direct child of a SubscribeeFolder.
/// A non-Affected child of a SubscribeeFolder is not a member of the
/// folder, hence not shown *as* a subscribee: it is an ordinary
/// self-writer parked there.

use crate::from_text::local_instruction_collection::predicates::{
  active_child_counts_as_content,
  active_child_counts_as_visible_content,
  member_counts_for_partnerFolder };
use crate::from_text::local_instruction_collection::types::{
  CollectedIntents, DefiningFolderOwner, LocalContext, NodeIntent_Local,
  HiddenOutsideEdit, SubscribeeTextClaim, SubscribeeVisibility };
use crate::types::misc::{ID, SourceName};
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{
  NodeEditRequest, AffectsParent, Qual, QualFolder, PartnerFolder, ActiveNode, ViewNode,
  ViewNodeKind, Vognode, Phantom };

use ego_tree::NodeRef;
use std::collections::HashSet;

pub fn collect_instructions_locally (
  forest : &ViewForest,
) -> Result<CollectedIntents, String> {
  let mut collected : CollectedIntents =
    CollectedIntents::new();
  for root in forest . roots() {
    visit (root, &LocalContext::TopLevel, &mut collected) ?; }
  Ok (collected) }

fn visit (
  node_ref : NodeRef<ViewNode>,
  context  : &LocalContext,
  collected : &mut CollectedIntents,
) -> Result<(), String> {
  match &node_ref . value() . kind {
    ViewNodeKind::Vognode (Vognode::Active (t)) =>
      visit_active_vognode (node_ref, t, context, collected),
    ViewNodeKind::Vognode (Vognode::Inactive (_)) =>
      // An Inactive vognode is anonymous and emits nothing; its
      // membership is owned by the disk weave, not extraction. With no
      // identity it owns no defining folder, so (like a DeadScaffold) any
      // folder found under it stays silent.
      recurse_under_gnode (node_ref, None, None, collected),
    ViewNodeKind::Phantom (p) =>
      recurse_under_gnode (
        node_ref,
        Some ( DefiningFolderOwner {
          // Carrying the phantom's identity (write-protected, not
          // save-eligible) keeps a SubscribeeFolder found under a diff
          // phantom meaningful: its children stay in
          // subscribee-as-such position, so their title edits bounce
          // via text claims instead of silently becoming real edits.
          id               : p . id() . clone(),
          is_definitive    : false,
          is_saveEligible : false } ),
        None, collected),
    ViewNodeKind::DeadScaffold =>
      recurse_under_gnode (node_ref, None, None, collected),
    ViewNodeKind::BufferRoot =>
      // A BufferRoot is unreachable as a child, but the traversal
      // stays total anyway.
      recurse_with_uniform_context (
        node_ref, &LocalContext::TopLevel, collected),
    ViewNodeKind::QualFolder (QualFolder::Alias) =>
      visit_aliasFolder (node_ref, context, collected),
    // The two arms below are exactly the FolderPolicy::WritableSet
    // PartnerFolders; the catch-all PartnerFolder arm after them covers the
    // ReadOnlySet and ReadOnlyFilter policies. If a new PartnerFolder is
    // added, 'PartnerFolder::policy' says which group it joins.
    ViewNodeKind::PartnerFolder (PartnerFolder::Subscribee) =>
      visit_subscribee_folder (node_ref, context, collected),
    ViewNodeKind::PartnerFolder (PartnerFolder::HiddenOutsideOfSubscribee) =>
      visit_hiddenOutside_folder (node_ref, context, collected),
    ViewNodeKind::PartnerFolder (PartnerFolder::Overridden) =>
      visit_overridden_folder (node_ref, context, collected),
    ViewNodeKind::QualFolder (QualFolder::ID)
      | ViewNodeKind::Qual (_)
      | ViewNodeKind::PartnerFolder (_) =>
      // These are the read-only folders and the Qual leaves. Vognodes
      // found inside them are self-writers; their membership in the
      // folder is never read.
      recurse_with_uniform_context (
        node_ref, &LocalContext::UnderReadOnlyFolder, collected), }}

fn visit_active_vognode (
  node_ref  : NodeRef<ViewNode>,
  t         : &ActiveNode,
  context   : &LocalContext,
  collected : &mut CollectedIntents,
) -> Result<(), String> {
  let subscribee_as_such_context : Option<(&ID, bool)> =
    match context {
      LocalContext::SubscribeeAsSuchPosition {
        subscriber, subscriber_is_definitive }
        if t . affectsParent == AffectsParent::True =>
        Some (( subscriber, *subscriber_is_definitive )),
      _ => None };
  let is_definitive : bool =
    ! t . is_writeProtected();
  let has_delete_request : bool =
    matches!( t . edit_request(),
              Some (&NodeEditRequest::Delete));
  let is_saveEligible : bool =
    is_definitive
    && ! has_delete_request
    && subscribee_as_such_context . is_none();
  if is_definitive {
    // Emission happens only inside this block, because an
    // write-protected vognode emits nothing.
    match subscribee_as_such_context {
      Some (( subscriber, subscriber_is_definitive )) => {
        collected . instructionMerge_intent (
          t . id . clone(),
          NodeIntent_Local::SubscribeeTextClaim (
            SubscribeeTextClaim {
              title : t . title . clone(),
              body  : t . body() . cloned() } )) ?;
        if subscriber_is_definitive {
          // The at-most-one-writer-per-ID guard: only the
          // SubscribeeFolder under the definitive instance of a
          // subscriber may write its hide edits. The same subscriber
          // can recur write-protected elsewhere with its own
          // SubscribeeFolder; without this guard those could emit
          // contradictory hide edits for one ID.
          collected . instructionMerge_intent (
            subscriber . clone(),
            NodeIntent_Local::SubscribeeVisibility (
              SubscribeeVisibility {
                subscribee : t . id . clone(),
                visible    : visible_content_members (node_ref) } )) ?; }
        // Delete and NodeMerge edit requests here are ignored: a
        // subscribee-as-such can affect only what its subscriber
        // hides.
      },
      None => {
        if has_delete_request {
          collected . instructionMerge_intent (
            t . id . clone(),
            NodeIntent_Local::Delete {
              source : t . source . clone() } ) ?;
        } else {
          collected . instructionMerge_intent (
            t . id . clone(),
            NodeIntent_Local::SetTitleAndBody {
              source : t . source . clone(),
              title  : t . title . clone(),
              body   : t . body() . cloned() } ) ?;
          collected . instructionMerge_intent (
            t . id . clone(),
            // This is always emitted, even if empty: a definitive
            // node's content is always Specified.
            NodeIntent_Local::SetContains (
              content_members (node_ref) )) ?;
          if let Some (NodeEditRequest::NodeMerge (acquiree)) =
            t . edit_request()
          { collected . instructionMerge_intent (
              t . id . clone(),
              NodeIntent_Local::NodeMerge {
                acquiree : acquiree . clone() } ) ?; }}},}}
  recurse_under_gnode (
    node_ref,
    Some ( DefiningFolderOwner {
      id               : t . id . clone(),
      is_definitive,
      is_saveEligible } ),
    if is_saveEligible { Some (t . id . clone()) } else { None },
    collected) }

/// This recurses into a gnode-ish node's children. Vognode-ish
/// children get 'UnderVognode'; defining-folder children get
/// 'UnderDefiningFolder', carrying the owner's identity (when it has
/// one); and read-only folders and Quals get 'UnderReadOnlyFolder'.
fn recurse_under_gnode (
  node_ref            : NodeRef<ViewNode>,
  owner               : Option<DefiningFolderOwner>,
  parent_if_writeable : Option<ID>,
  collected           : &mut CollectedIntents,
) -> Result<(), String> {
  for child in node_ref . children() {
    let child_context : LocalContext =
      match &child . value() . kind {
        ViewNodeKind::QualFolder (QualFolder::Alias)
          // The two PartnerFolders here are exactly the
          // FolderPolicy::WritableSet ones; the read-only policies fall
          // to the UnderReadOnlyFolder arm below.
          | ViewNodeKind::PartnerFolder (PartnerFolder::Subscribee)
          | ViewNodeKind::PartnerFolder (PartnerFolder::Overridden) =>
          match &owner {
            Some (o) =>
              LocalContext::UnderDefiningFolder (o . clone()),
            None =>
              // The owner has no identity (it is a DeadScaffold), so
              // the folder will stay silent.
              LocalContext::UnderVognode {
                parent_if_writeable : None } },
        ViewNodeKind::QualFolder (QualFolder::ID)
          | ViewNodeKind::Qual (_)
          | ViewNodeKind::PartnerFolder (_) =>
          LocalContext::UnderReadOnlyFolder,
        _ =>
          LocalContext::UnderVognode {
            parent_if_writeable : parent_if_writeable . clone() } };
    visit (child, &child_context, collected) ?; }
  Ok (( )) }

fn recurse_with_uniform_context (
  node_ref  : NodeRef<ViewNode>,
  context   : &LocalContext,
  collected : &mut CollectedIntents,
) -> Result<(), String> {
  for child in node_ref . children() {
    visit (child, context, collected) ?; }
  Ok (( )) }

fn visit_aliasFolder (
  node_ref  : NodeRef<ViewNode>,
  context   : &LocalContext,
  collected : &mut CollectedIntents,
) -> Result<(), String> {
  if let LocalContext::UnderDefiningFolder (owner) = context {
    if owner . is_saveEligible {
      let aliases : Vec<(String, Option<SourceName>)> = {
        let mut aliases : Vec<(String, Option<SourceName>)> = Vec::new();
        let mut seen : HashSet<String> = HashSet::new ();
        for child in node_ref . children() {
          if let ViewNodeKind::Qual (Qual::Alias {
            text, relSource_request, .. })
            = &child . value() . kind
          { if seen . insert (text . clone ()) {
              aliases . push (( text . clone (),
                                relSource_request . clone () )); }} }
        aliases };
      // The MSV semantics are: an absent folder emits no intent, which
      // lowers to Unspecified, while a present-but-empty folder emits
      // Specified(vec![]).
      collected . instructionMerge_intent (
        owner . id . clone(),
        NodeIntent_Local::SetAliases (aliases) ) ?; }}
  recurse_with_uniform_context (
    node_ref, &LocalContext::UnderReadOnlyFolder, collected) }

fn visit_subscribee_folder (
  node_ref  : NodeRef<ViewNode>,
  context   : &LocalContext,
  collected : &mut CollectedIntents,
) -> Result<(), String> {
  match context {
    LocalContext::UnderDefiningFolder (owner) => {
      if owner . is_saveEligible {
        collected . instructionMerge_intent (
          owner . id . clone(),
          NodeIntent_Local::SetSubscribesTo (
            subscribeeFolder_members (node_ref) )) ?; }
      for child in node_ref . children() {
        let child_context : LocalContext =
          match &child . value() . kind {
            ViewNodeKind::PartnerFolder (PartnerFolder::HiddenOutsideOfSubscribee) =>
              LocalContext::HiddenOutsidePosition {
                subscriber      : owner . id . clone(),
                is_saveEligible : owner . is_saveEligible },
            _ =>
              // The subscriber's identity is passed even when the owner is
              // not save-eligible, because text claims outlive the
              // visibility guard.
              LocalContext::SubscribeeAsSuchPosition {
                subscriber               : owner . id . clone(),
                subscriber_is_definitive : owner . is_definitive }, };
        visit (child, &child_context, collected) ?; }
      Ok (( )) },
    _ =>
      // The folder has no identifiable owner. Validation precludes this
      // shape; the traversal stays total and silent.
      recurse_with_uniform_context (
        node_ref,
        &LocalContext::UnderVognode { parent_if_writeable : None },
        collected), }}

/// Collect the explicitly submitted visible-outside subset.  This folder is a
/// derived filter rather than a direct relationship set, so its meaning is
/// resolved only after ordinary subscribee visibility inference has run.
fn visit_hiddenOutside_folder (
  node_ref  : NodeRef<ViewNode>,
  context   : &LocalContext,
  collected : &mut CollectedIntents,
) -> Result<(), String> {
  if let LocalContext::HiddenOutsidePosition {
    subscriber, is_saveEligible } = context
  {
    if *is_saveEligible {
      let mut members : Vec<ID> = Vec::new ();
      for child in node_ref . children() {
        match &child . value() . kind {
          ViewNodeKind::Vognode (Vognode::Active (t))
            if member_counts_for_partnerFolder (t) => {
              if t . relSource_request . is_some () {
                return Err ("HiddenOutsideOfSubscribee membership is editable, but hide relSources are derived." . to_string ()); }
              members . push (t . id . clone ()); },
          ViewNodeKind::Phantom (Phantom::Unknown (unknown)) => {
            if unknown . relSource_request . is_some () {
              return Err ("HiddenOutsideOfSubscribee membership is editable, but hide relSources are derived." . to_string ()); }
            members . push (unknown . id . clone ()); },
          _ => {}, }}
      collected . instructionMerge_intent (
        subscriber . clone(),
        NodeIntent_Local::HiddenOutsideEdit (HiddenOutsideEdit { members }) ) ?; }}
  recurse_with_uniform_context (
    node_ref, &LocalContext::UnderReadOnlyFolder, collected)
}

fn visit_overridden_folder (
  node_ref  : NodeRef<ViewNode>,
  context   : &LocalContext,
  collected : &mut CollectedIntents,
) -> Result<(), String> {
  if let LocalContext::UnderDefiningFolder (owner) = context {
    if owner . is_saveEligible {
      collected . instructionMerge_intent (
        owner . id . clone(),
        NodeIntent_Local::SetOverrides (
          partnerFolder_members (node_ref) )) ?; }}
  recurse_with_uniform_context (
    node_ref,
    // The members are self-writers; their membership was read just
    // above, and they form no one's contains.
    &LocalContext::UnderVognode { parent_if_writeable : None },
    collected) }

/// As 'dedup_vector', but dedups members carrying sources by ID ALONE
/// (first occurrence wins) rather than by the full (ID, source) pair: a
/// duplicate ID with a DIFFERENT source request must still
/// be silently dropped, matching the existing defining-folder dedup
/// policy ("duplicate defining-folder members are silently deduped").
fn dedup_members_by_id (
  members : Vec<(ID, Option<SourceName>)>,
) -> Vec<(ID, Option<SourceName>)> {
  let mut seen   : std::collections::HashSet<ID> = std::collections::HashSet::new();
  let mut result : Vec<(ID, Option<SourceName>)> = Vec::new();
  for (id, source) in members {
    if seen . insert (id . clone()) {
      result . push ((id, source)); }}
  result }

/// This returns the members of an OverriddenFolder: its Active
/// children that pass the PartnerFolder membership predicate, silently
/// deduplicated (by ID; see 'dedup_members_by_id'), preserving
/// first-occurrence order. Each member is paired with its headline's
/// explicit '(editRequest (relSource NAME))' request, if any (see
/// 'NodeIntent_Local').  (Inactive
/// children are NOT members here: the overriddenFolder omits inactive
/// members from display, and the set-difference merge preserves
/// them at save.  TODO/full-schema/9-2_source-set-safety.org.)
fn partnerFolder_members (
  node_ref : NodeRef<ViewNode>,
) -> Vec<(ID, Option<SourceName>)> {
  let mut members : Vec<(ID, Option<SourceName>)> = Vec::new();
  for child in node_ref . children() {
    match &child . value() . kind {
      ViewNodeKind::Vognode (Vognode::Active (t))
        if member_counts_for_partnerFolder (t) =>
          members . push ((t . id . clone(),
                           t . relSource_request . clone())),
      ViewNodeKind::Phantom (Phantom::Unknown (unknown)) =>
          members . push ((unknown . id . clone(),
                           unknown . relSource_request . clone())),
      _ => {}, }}
  dedup_members_by_id (members) }

/// This returns the members of a SubscribeeFolder: its Active children
/// that pass the PartnerFolder membership predicate, deduplicated (by
/// ID; see 'dedup_members_by_id'). Like 'content_members' (and for
/// the same reason), inactive children contribute nothing:
/// 'subscribes_to' is order-meaningful, but the disk merge ('weave')
/// already restores invisible subscribees at their disk position, so
/// a buffer-present inactive placeholder must not feed this list.
/// Each member is paired with its headline's explicit
/// '(editRequest (relSource NAME))' request, if any.
#[allow(non_snake_case)]
fn subscribeeFolder_members (
  node_ref : NodeRef<ViewNode>,
) -> Vec<(ID, Option<SourceName>)> {
  let mut members : Vec<(ID, Option<SourceName>)> = Vec::new();
  for child in node_ref . children() {
    match &child . value() . kind {
      ViewNodeKind::Vognode (Vognode::Active (t))
        if member_counts_for_partnerFolder (t) =>
          members . push ((t . id . clone(),
                           t . relSource_request . clone())),
      ViewNodeKind::Phantom (Phantom::Unknown (unknown)) =>
          members . push ((unknown . id . clone(),
                           unknown . relSource_request . clone())),
      _ => {}, }}
  dedup_members_by_id (members) }

/// This returns the content of a definitive vognode: its Active
/// children that pass the contains predicate. It does not dedup,
/// because validation ('nonignored_children_have_distinct_ids')
/// already guarantees distinctness. Each member is paired with its
/// headline's explicit '(editRequest (relSource NAME))' request, if any (see
/// 'NodeIntent_Local').
///
/// Inactive children contribute NOTHING here: an inactive node emits
/// no save intention for its container. Its membership in the
/// container's contains is owned entirely by the disk merge
/// ('preserve_invisible_members' -> weave in from_text/weave.rs),
/// which restores invisible members from disk at their disk position.
/// Including a buffer-present inactive child would let a stale or
/// concurrently-edited buffer resurrect a member that was
/// authoritatively removed, and would persist reorderings of a
/// read-only placeholder. (See TODO/problems.org, "Retained inactive
/// nodes emit positional save intentions for their container".)
fn content_members (
  node_ref : NodeRef<ViewNode>,
) -> Vec<(ID, Option<SourceName>)> {
  let mut contents : Vec<(ID, Option<SourceName>)> = Vec::new();
  for child in node_ref . children() {
    match &child . value() . kind {
      ViewNodeKind::Vognode (Vognode::Active (t)) => {
        if active_child_counts_as_content (t) {
          contents . push ((
            // collected_id, not id: a drawn overrider stands for
            // the original member it was drawn in place of.
            t . collected_id (),
            t . relSource_request . clone() )); }},
      ViewNodeKind::Phantom (Phantom::Unknown (unknown)) =>
        // An Unknown is inert as a node, but its raw ID is load-bearing
        // membership data at a structured relationship position. `None` asks
        // disk supplementation to keep an existing destination source sticky.
        contents . push (( unknown . id . clone(),
                           unknown . relSource_request . clone() )),
      _ => {}, }}
  contents }

/// This returns the children that the buffer presents as visible
/// content of one subscribee-as-such. The list is not saved as the
/// subscribee's contains; it is the signal from which the
/// subscriber's hides/unhides are inferred, downstream. It is not
/// deduplicated.
fn visible_content_members (
  node_ref : NodeRef<ViewNode>,
) -> Vec<ID> {
  let mut visible : Vec<ID> = Vec::new();
  for child in node_ref . children() {
    match &child . value () . kind {
      ViewNodeKind::Vognode (Vognode::Active (t))
        if active_child_counts_as_visible_content (t) => {
        visible . push (
          // collected_id: a drawn overrider presents the original,
          // so hide/unhide inference must speak of the original.
          t . collected_id ()); },
      ViewNodeKind::Phantom (Phantom::Unknown (unknown)) =>
        visible . push (unknown . id . clone ()),
      _ => {}, }}
  visible }
