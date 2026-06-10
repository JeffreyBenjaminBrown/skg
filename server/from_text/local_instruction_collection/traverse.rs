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
/// - col shapes are valid: each col holds only the child kinds it
///   permits, each col is unique among its siblings, content members
///   have distinct IDs per
///   'nonignored_children_have_distinct_ids', and read-only-col
///   members have distinct IDs per
///   'partnerCol_children_have_distinct_ids'. The defining cols
///   (AliasCol, SubscribeeCol, OverriddenCol) may contain
///   duplicates; emission dedups them silently.
/// Collection performs no ancestry re-verification. On shapes that
/// validation precludes, it stays total, emitting nothing rather
/// than erroring.
/// .
/// DEFINITION: a vognode is *save-eligible* iff it is Active,
/// definitive, lacks a Delete edit request, and is not in
/// subscribee-as-such position. (Its position may be anywhere else --
/// including as a member of a read-only col, where it is
/// save-eligible for itself but invisible to the col's owner.)
/// .
/// DEFINITION: a vognode is *in subscribee-as-such position* iff it
/// is an Active, parentIs=Affected direct child of a SubscribeeCol.
/// A non-Affected child of a SubscribeeCol is not a member of the
/// col, hence not shown *as* a subscribee: it is an ordinary
/// self-writer parked there.

use crate::from_text::local_instruction_collection::predicates::{
  active_child_counts_as_content,
  active_child_counts_as_visible_content,
  inactiveNode_is_phantom,
  member_counts_for_partnerCol };
use crate::from_text::local_instruction_collection::types::{
  CollectedIntents, DefiningColOwner, LocalContext, NodeIntent_Local,
  SubscribeeTextClaim, SubscribeeVisibility };
use crate::types::misc::ID;
use crate::types::list::dedup_vector;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{
  EditRequest, ParentIs, Qual, QualCol, PartnerCol, TrueNode, ViewNode,
  ViewNodeKind, Vognode };

use ego_tree::NodeRef;

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
    ViewNodeKind::Vognode (Vognode::Inactive (i)) =>
      // An Inactive vognode emits nothing itself; the parent's
      // contains predicate is what counts it.
      recurse_under_gnode (
        node_ref,
        Some ( DefiningColOwner {
          id               : i . id . clone(),
          is_definitive    : false,
          is_saveEligible : false } ),
        None, collected),
    ViewNodeKind::Phantom (p) =>
      recurse_under_gnode (
        node_ref,
        Some ( DefiningColOwner {
          // Carrying the phantom's identity (indefinitive, not
          // save-eligible) keeps a SubscribeeCol found under a diff
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
    ViewNodeKind::QualCol (QualCol::Alias) =>
      visit_aliascol (node_ref, context, collected),
    // The two arms below are exactly the ColPolicy::WritableSet
    // PartnerCols; the catch-all PartnerCol arm after them covers the
    // ReadOnlySet and ReadOnlyFilter policies. If a new PartnerCol is
    // added, 'PartnerCol::policy' says which group it joins.
    ViewNodeKind::PartnerCol (PartnerCol::Subscribee) =>
      visit_subscribeecol (node_ref, context, collected),
    ViewNodeKind::PartnerCol (PartnerCol::Overridden) =>
      visit_overriddencol (node_ref, context, collected),
    ViewNodeKind::QualCol (QualCol::ID)
      | ViewNodeKind::Qual (_)
      | ViewNodeKind::PartnerCol (_) =>
      // These are the read-only cols and the Qual leaves. Vognodes
      // found inside them are self-writers; their membership in the
      // col is never read.
      recurse_with_uniform_context (
        node_ref, &LocalContext::UnderReadOnlyCol, collected), }}

fn visit_active_vognode (
  node_ref  : NodeRef<ViewNode>,
  t         : &TrueNode,
  context   : &LocalContext,
  collected : &mut CollectedIntents,
) -> Result<(), String> {
  let subscribee_as_such_context : Option<(&ID, bool)> =
    match context {
      LocalContext::SubscribeeAsSuchPosition {
        subscriber, subscriber_is_definitive }
        if t . parentIs == ParentIs::Affected =>
        Some (( subscriber, *subscriber_is_definitive )),
      _ => None };
  let is_definitive : bool =
    ! t . is_indefinitive();
  let has_delete_request : bool =
    matches!( t . edit_request(),
              Some (&EditRequest::Delete));
  let is_saveEligible : bool =
    is_definitive
    && ! has_delete_request
    && subscribee_as_such_context . is_none();
  if is_definitive {
    // Emission happens only inside this block, because an
    // indefinitive vognode emits nothing.
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
          // SubscribeeCol under the definitive instance of a
          // subscriber may write its hide edits. The same subscriber
          // can recur indefinitively elsewhere with its own
          // SubscribeeCol; without this guard those could emit
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
          if let Some (EditRequest::NodeMerge (acquiree)) =
            t . edit_request()
          { collected . instructionMerge_intent (
              t . id . clone(),
              NodeIntent_Local::NodeMerge {
                acquiree : acquiree . clone() } ) ?; }}},}}
  recurse_under_gnode (
    node_ref,
    Some ( DefiningColOwner {
      id               : t . id . clone(),
      is_definitive,
      is_saveEligible } ),
    if is_saveEligible { Some (t . id . clone()) } else { None },
    collected) }

/// This recurses into a gnode-ish node's children. Vognode-ish
/// children get 'UnderVognode'; defining-col children get
/// 'UnderDefiningCol', carrying the owner's identity (when it has
/// one); and read-only cols and Quals get 'UnderReadOnlyCol'.
fn recurse_under_gnode (
  node_ref            : NodeRef<ViewNode>,
  owner               : Option<DefiningColOwner>,
  parent_if_writeable : Option<ID>,
  collected           : &mut CollectedIntents,
) -> Result<(), String> {
  for child in node_ref . children() {
    let child_context : LocalContext =
      match &child . value() . kind {
        ViewNodeKind::QualCol (QualCol::Alias)
          // The two PartnerCols here are exactly the
          // ColPolicy::WritableSet ones; the read-only policies fall
          // to the UnderReadOnlyCol arm below.
          | ViewNodeKind::PartnerCol (PartnerCol::Subscribee)
          | ViewNodeKind::PartnerCol (PartnerCol::Overridden) =>
          match &owner {
            Some (o) =>
              LocalContext::UnderDefiningCol (o . clone()),
            None =>
              // The owner has no identity (it is a DeadScaffold), so
              // the col will stay silent.
              LocalContext::UnderVognode {
                parent_if_writeable : None } },
        ViewNodeKind::QualCol (QualCol::ID)
          | ViewNodeKind::Qual (_)
          | ViewNodeKind::PartnerCol (_) =>
          LocalContext::UnderReadOnlyCol,
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

fn visit_aliascol (
  node_ref  : NodeRef<ViewNode>,
  context   : &LocalContext,
  collected : &mut CollectedIntents,
) -> Result<(), String> {
  if let LocalContext::UnderDefiningCol (owner) = context {
    if owner . is_saveEligible {
      let texts : Vec<String> = {
        let mut texts : Vec<String> = Vec::new();
        for child in node_ref . children() {
          if let ViewNodeKind::Qual (Qual::Alias { text, .. })
            = &child . value() . kind
          { texts . push (text . clone()); }}
        // Dedup is silent and preserves first-occurrence order:
        // aliases are unordered, and repeating oneself is not an
        // error.
        dedup_vector (texts) };
      // The MSV semantics are: an absent col emits no intent, which
      // lowers to Unspecified, while a present-but-empty col emits
      // Specified(vec![]).
      collected . instructionMerge_intent (
        owner . id . clone(),
        NodeIntent_Local::SetAliases (texts) ) ?; }}
  recurse_with_uniform_context (
    node_ref, &LocalContext::UnderReadOnlyCol, collected) }

fn visit_subscribeecol (
  node_ref  : NodeRef<ViewNode>,
  context   : &LocalContext,
  collected : &mut CollectedIntents,
) -> Result<(), String> {
  match context {
    LocalContext::UnderDefiningCol (owner) => {
      if owner . is_saveEligible {
        collected . instructionMerge_intent (
          owner . id . clone(),
          NodeIntent_Local::SetSubscribesTo (
            subscribeeCol_members (node_ref) )) ?; }
      recurse_with_uniform_context (
        node_ref,
        // The subscriber's identity is passed even when the owner is
        // not save-eligible, because text claims outlive the
        // visibility guard. (Children that are cols themselves
        // ignore this context.)
        &LocalContext::SubscribeeAsSuchPosition {
          subscriber               : owner . id . clone(),
          subscriber_is_definitive : owner . is_definitive },
        collected) },
    _ =>
      // The col has no identifiable owner. Validation precludes this
      // shape; the traversal stays total and silent.
      recurse_with_uniform_context (
        node_ref,
        &LocalContext::UnderVognode { parent_if_writeable : None },
        collected), }}

fn visit_overriddencol (
  node_ref  : NodeRef<ViewNode>,
  context   : &LocalContext,
  collected : &mut CollectedIntents,
) -> Result<(), String> {
  if let LocalContext::UnderDefiningCol (owner) = context {
    if owner . is_saveEligible {
      collected . instructionMerge_intent (
        owner . id . clone(),
        NodeIntent_Local::SetOverrides (
          partnerCol_members (node_ref) )) ?; }}
  recurse_with_uniform_context (
    node_ref,
    // The members are self-writers; their membership was read just
    // above, and they form no one's contains.
    &LocalContext::UnderVognode { parent_if_writeable : None },
    collected) }

/// This returns the members of an OverriddenCol: its Active
/// children that pass the PartnerCol membership predicate, silently
/// deduplicated, preserving first-occurrence order.  (Inactive
/// children are NOT members here: the overriddenCol omits inactive
/// members from display, and the set-difference merge preserves
/// them at save.  TODO/full-schema/9-2_source-set-safety.org.)
fn partnerCol_members (
  node_ref : NodeRef<ViewNode>,
) -> Vec<ID> {
  let mut members : Vec<ID> = Vec::new();
  for child in node_ref . children() {
    if let ViewNodeKind::Vognode (Vognode::Active (t))
      = &child . value() . kind
    { if member_counts_for_partnerCol (t) {
        members . push (t . id . clone()); }}}
  dedup_vector (members) }

/// This returns the members of a SubscribeeCol.  Unlike the
/// OverriddenCol, subscribee order matters, so a buffer-present
/// InactiveNode (a retained placeholder) counts as a positional
/// member, mirroring 'content_members'
/// (TODO/full-schema/9-2_source-set-safety.org).
#[allow(non_snake_case)]
fn subscribeeCol_members (
  node_ref : NodeRef<ViewNode>,
) -> Vec<ID> {
  let mut members : Vec<ID> = Vec::new();
  for child in node_ref . children() {
    match &child . value() . kind {
      ViewNodeKind::Vognode (Vognode::Active (t)) => {
        if member_counts_for_partnerCol (t) {
          members . push (t . id . clone()); }},
      ViewNodeKind::Vognode (Vognode::Inactive (i)) => {
        if ! inactiveNode_is_phantom (i) {
          members . push (i . id . clone()); }},
      _ => {}, }}
  dedup_vector (members) }

/// This returns the content of a definitive vognode: its Active
/// children that pass the contains predicate, plus its non-phantom
/// Inactive children. It does not dedup, because validation
/// ('nonignored_children_have_distinct_ids') already guarantees
/// distinctness.
fn content_members (
  node_ref : NodeRef<ViewNode>,
) -> Vec<ID> {
  let mut contents : Vec<ID> = Vec::new();
  for child in node_ref . children() {
    match &child . value() . kind {
      ViewNodeKind::Vognode (Vognode::Active (t)) => {
        if active_child_counts_as_content (t) {
          contents . push (t . id . clone()); }},
      ViewNodeKind::Vognode (Vognode::Inactive (i)) => {
        if ! inactiveNode_is_phantom (i) {
          contents . push (i . id . clone()); }},
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
    if let ViewNodeKind::Vognode (Vognode::Active (t))
      = &child . value() . kind
    { if active_child_counts_as_visible_content (t) {
        visible . push (t . id . clone()); }}}
  visible }
