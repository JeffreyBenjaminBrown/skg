/// This file defines the pure traversal at the heart of local
/// instruction collection (the glossary's "local fieldIntent collection").
/// .
/// The traversal is one recursive DFS preorder over the placed
/// viewforest. Recursion is unconditional: every node's children are
/// visited, whatever the node's kind, because the user can attach
/// independent nodes anywhere. Only emission is conditional, on the
/// pair (kind, context). Each visit reads the node and its direct
/// children, nothing deeper, nothing upward; everything an
/// fieldIntent emission needs from above arrives in its
/// 'LocalContext'.
/// .
/// The traversal assumes that 'find_buffer_errors_for_saving' has
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
///   (aliasFolder, subscribeeFolder, overriddenFolder) may contain
///   duplicates; emission dedups them silently.
/// Collection performs no ancestry re-verification. On shapes that
/// validation precludes, it stays total, emitting nothing rather
/// than erroring.
/// .
/// DEFINITION: a vognode is *save-eligible* iff it is an occurrence,
/// editable, lacks a Delete edit request, and is not in
/// subscribee-as-such position. (Its position may be anywhere else --
/// including as a member of a write-protected folder, where it is
/// save-eligible for itself but invisible to the folder's recorder.)
/// .
/// DEFINITION: a vognode is *in subscribee-as-such position* iff it
/// is an occurrence member direct child of a subscribeeFolder.
/// A child of a subscribeeFolder claiming nothing is not a member of the
/// folder, hence not shown *as* a subscribee: it is an ordinary
/// self-writer that stands there claiming nothing.

use crate::from_text::local_fieldintent_collection::predicates::{
  occurrence_child_counts_as_content,
  occurrence_child_counts_as_visible_content,
  member_counts_for_partnerFolder };
use crate::from_text::local_fieldintent_collection::types::{
  CollectedFieldIntents, DefiningFolderRecorder, LocalContext, FieldIntent,
  HiddenOutsideEdit, SubscribeeTextClaim, SubscribeeVisibility };
use crate::types::errors::HeadlineName;
use crate::types::misc::{ID, SkgrepoName};
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{RelationshipEditRequest, 
  NodeEditRequest, PropertyMember, Folder, Occurrence, Viewnode,
  ViewnodeKind, Vognode };

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
    ViewnodeKind::Vognode (Vognode::Occurrence (t)) =>
      visit_occurrence (node_ref, t, context, collected),
    ViewnodeKind::Vognode (vognode @ ( Vognode::Ghost (_) | Vognode::Unknown (_) )) =>
      recurse_under_vognode (
        node_ref,
        Some ( DefiningFolderRecorder {
          // Carrying the ghost's or unknown's identity (write-protected,
          // not save-eligible) keeps a subscribeeFolder found under a
          // ghost meaningful: its children stay in
          // subscribee-as-such position, so their title edits bounce
          // via text claims instead of silently becoming real edits.
          gnid               : vognode . gnid() . clone(),
          headline            : node_ref . value () . headline_name (),
          is_editable    : false,
          is_saveEligible : false } ),
        None, collected),
    ViewnodeKind::Tombstone =>
      // A tombstone emits nothing. Where it stands for a restricted
      // node, that node's membership is owned
      // by disk supplementation, not extraction. With no identity it
      // owns no defining folder, so any folder found under it stays
      // silent.
      recurse_under_vognode (node_ref, None, None, collected),
    ViewnodeKind::BufferRoot =>
      // A BufferRoot is unreachable as a child, but the traversal
      // stays total anyway.
      recurse_with_uniform_context (
        node_ref, &LocalContext::TopLevel, collected),
    ViewnodeKind::Folder (Folder::Alias) =>
      visit_aliasFolder (node_ref, context, collected),
    // The three arms below, with the alias folder's above, are exactly
    // the editable folders ('Folder::is_editable'): the
    // ones whose membership the save reads. The catch-all folder arm
    // after them covers the write-protected ones.
    ViewnodeKind::Folder (Folder::Subscribee) =>
      visit_subscribee_folder (node_ref, context, collected),
    ViewnodeKind::Folder (Folder::HiddenOutsideOfSubscribee) =>
      visit_hiddenOutside_folder (node_ref, context, collected),
    ViewnodeKind::Folder (Folder::Overridden) =>
      visit_overridden_folder (node_ref, context, collected),
    ViewnodeKind::Folder (Folder::ID)
      | ViewnodeKind::Folder (Folder::Flags { .. })
      | ViewnodeKind::PropertyMember (_)
      | ViewnodeKind::ChangeViewnode (_)
      | ViewnodeKind::Folder (_) =>
      // These are the write-protected folders and the Property leaves. Vognodes
      // found inside them are self-writers; their membership in the
      // folder is never read.
      recurse_with_uniform_context (
        node_ref, &LocalContext::UnderWriteProtectedFolder, collected), }}

fn visit_occurrence (
  node_ref  : NodeRef<Viewnode>,
  t         : &Occurrence,
  context   : &LocalContext,
  collected : &mut CollectedFieldIntents,
) -> Result<(), String> {
  let subscribee_as_such_context : Option<(&ID, bool)> =
    match context {
      LocalContext::SubscribeeAsSuchPosition {
        subscriber, subscriber_is_editable }
        if t . claimsMembership =>
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
  let headline : HeadlineName = node_ref . value () . headline_name ();
  if is_editable {
    // Emission happens only inside this block, because an
    // write-protected vognode emits nothing.
    match subscribee_as_such_context {
      Some (( subscriber, subscriber_is_editable )) => {
        collected . instructionMerge_fieldIntent (
          &headline,
          t . gnid . clone(),
          FieldIntent::SubscribeeTextClaim (
            SubscribeeTextClaim {
              title    : t . title . clone(),
              body     : t . body() . cloned(),
              headline : headline . clone () } )) ?;
        if subscriber_is_editable {
          // The at-most-one-writer-per-ID guard: only the
          // subscribeeFolder under the editable instance of a
          // subscriber may write its hide edits. The same subscriber
          // can recur write-protected elsewhere with its own
          // subscribeeFolder; without this guard those could emit
          // contradictory hide edits for one ID.
          collected . instructionMerge_fieldIntent (
            &headline,
            subscriber . clone(),
            FieldIntent::SubscribeeVisibility (
              SubscribeeVisibility {
                subscribee : t . gnid . clone(),
                visible    : visible_content_members (node_ref) } )) ?; }
        // Delete and NodeMerge edit requests here are ignored: a
        // subscribee-as-such can affect only what its subscriber
        // hides.
      },
      None => {
        if has_delete_request {
          collected . instructionMerge_fieldIntent (
            &headline,
            t . gnid . clone(),
            FieldIntent::Delete {
              skgrepo : t . home_skgrepo . clone() } ) ?;
        } else {
          collected . instructionMerge_fieldIntent (
            &headline,
            t . gnid . clone(),
            FieldIntent::SetTitleAndBody {
              skgrepo : t . home_skgrepo . clone(),
              title   : t . title . clone(),
              body    : t . body() . cloned() } ) ?;
          collected . instructionMerge_fieldIntent (
            &headline,
            t . gnid . clone(),
            // This is always emitted, even if empty: an editable
            // node's content is always Specified.
            FieldIntent::SetContains (
              content_members (node_ref) )) ?;
          if let Some (NodeEditRequest::NodeMerge (acquiree)) =
            t . edit_request()
          { collected . instructionMerge_fieldIntent (
              &headline,
              t . gnid . clone(),
              FieldIntent::NodeMerge {
                acquiree : acquiree . clone() } ) ?; }
          if let Some (NodeEditRequest::SetFlag { flag, value }) =
            t . edit_request()
          { collected . instructionMerge_fieldIntent (
              &headline,
              t . gnid . clone(),
              FieldIntent::SetFlag {
                flag : *flag, value : *value } ) ?; }}},}}
  recurse_under_vognode (
    node_ref,
    Some ( DefiningFolderRecorder {
      gnid               : t . gnid . clone(),
      headline,
      is_editable,
      is_saveEligible } ),
    if is_saveEligible { Some (t . gnid . clone()) } else { None },
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
        ViewnodeKind::Folder (Folder::Alias)
          // The two partner folders here are the relation-role ones
          // whose goal is ordered; the write-protected folders fall to
          // the UnderWriteProtectedFolder arm below.
          | ViewnodeKind::Folder (Folder::Subscribee)
          | ViewnodeKind::Folder (Folder::Overridden) =>
          match &recorder {
            Some (o) =>
              LocalContext::UnderDefiningFolder (o . clone()),
            None =>
              // The recorder has no identity (it is a tombstone), so
              // the folder will stay silent.
              LocalContext::UnderVognode {
                parent_if_editable : None } },
        ViewnodeKind::Folder (
          Folder::ID | Folder::Flags { .. })
          | ViewnodeKind::PropertyMember (_)
          | ViewnodeKind::ChangeViewnode (_)
          | ViewnodeKind::Folder (_) =>
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
      let aliases : Vec<(String, Option<SkgrepoName>)> = {
        let mut aliases : Vec<(String, Option<SkgrepoName>)> = Vec::new();
        let mut seen : HashSet<String> = HashSet::new ();
        for child in node_ref . children() {
          if let ViewnodeKind::PropertyMember (PropertyMember::Alias {
            text, relationshipEditRequest, claimsMembership, .. })
            = &child . value() . kind
          { // An alias that claims no membership, one the worktree
            // lost, is not read.
            if *claimsMembership
               && seen . insert (text . clone ()) {
              aliases . push (( text . clone (),
                                RelationshipEditRequest::relRepo_of (
                                  relationshipEditRequest ) . cloned () )); }} }
        aliases };
      // The MSV semantics are: an absent folder emits no fieldIntent, which
      // lowers to Unspecified, while a present-but-empty folder emits
      // Specified(vec![]).
      collected . instructionMerge_fieldIntent (
        &recorder . headline,
        recorder . gnid . clone(),
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
          &recorder . headline,
          recorder . gnid . clone(),
          FieldIntent::SetSubscribesTo (
            subscribeeFolder_members (node_ref) )) ?; }
      for child in node_ref . children() {
        let child_context : LocalContext =
          match &child . value() . kind {
            ViewnodeKind::Folder (Folder::HiddenOutsideOfSubscribee) =>
              LocalContext::HiddenOutsidePosition {
                subscriber      : recorder . gnid . clone(),
                is_saveEligible : recorder . is_saveEligible },
            _ =>
              // The subscriber's identity is passed even when the recorder is
              // not save-eligible, because text claims outlive the
              // visibility guard.
              LocalContext::SubscribeeAsSuchPosition {
                subscriber               : recorder . gnid . clone(),
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
/// compound partner folder rather than a direct relationship set, so its meaning is
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
          ViewnodeKind::Vognode (Vognode::Occurrence (t))
            if member_counts_for_partnerFolder (t) => {
              if RelationshipEditRequest::relRepo_of (&t . relationshipEditRequest) . is_some () {
                return Err ("HiddenOutsideOfSubscribee membership is editable, but hide relRepos are derived." . to_string ()); }
              members . push (t . gnid . clone ()); },
          ViewnodeKind::Vognode (Vognode::Unknown (unknown))
            if unknown . claimsMembership => {
            if RelationshipEditRequest::relRepo_of (&unknown . relationshipEditRequest) . is_some () {
              return Err ("HiddenOutsideOfSubscribee membership is editable, but hide relRepos are derived." . to_string ()); }
            members . push (unknown . gnid . clone ()); },
          ViewnodeKind::Vognode (Vognode::Ghost (ghost))
            if ghost . claimsMembership =>
            members . push (ghost . gnid . clone ()),
          _ => {}, }}
      collected . instructionMerge_fieldIntent (
        &node_ref . value () . headline_name (),
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
        &recorder . headline,
        recorder . gnid . clone(),
        FieldIntent::SetOverrides (
          partnerFolder_members (node_ref) )) ?; }}
  recurse_with_uniform_context (
    node_ref,
    // The members are self-writers; their membership was read just
    // above, and they form no one's contains.
    &LocalContext::UnderVognode { parent_if_editable : None },
    collected) }

/// As 'dedup_vector', but dedups members carrying skgrepos by ID alone
/// (first occurrence wins) rather than by the full (ID, skgrepo) pair: a
/// duplicate ID with a different skgrepo request must still
/// be silently dropped, matching the existing defining-folder dedup
/// policy ("duplicate defining-folder members are silently deduped").
fn dedup_members_by_gnid (
  members : Vec<(ID, Option<SkgrepoName>)>,
) -> Vec<(ID, Option<SkgrepoName>)> {
  let mut seen   : std::collections::HashSet<ID> = std::collections::HashSet::new();
  let mut result : Vec<(ID, Option<SkgrepoName>)> = Vec::new();
  for (gnid, skgrepo) in members {
    if seen . insert (gnid . clone()) {
      result . push ((gnid, skgrepo)); }}
  result }

/// This returns the members of an overriddenFolder: its occurrence
/// children that pass the partner folder membership predicate, silently
/// deduplicated (by ID; see 'dedup_members_by_id'), preserving
/// first-occurrence order. Each member is paired with its headline's
/// explicit '(editRequest (relRepo NAME))' request, if any (see
/// 'FieldIntent').  (Hidden
/// nodes are not members here: the overriddenFolder omits hidden
/// members from display, and the set-difference merge preserves
/// them at save; see the API doc's "Restricted-skgrepo nodes".)
fn partnerFolder_members (
  node_ref : NodeRef<Viewnode>,
) -> Vec<(ID, Option<SkgrepoName>)> {
  let mut members : Vec<(ID, Option<SkgrepoName>)> = Vec::new();
  for child in node_ref . children() {
    match &child . value() . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (t))
        if member_counts_for_partnerFolder (t) =>
          members . push ((t . gnid . clone(),
                           RelationshipEditRequest::relRepo_of (&t . relationshipEditRequest) . cloned ())),
      ViewnodeKind::Vognode (Vognode::Unknown (unknown))
        if unknown . claimsMembership =>
          members . push ((unknown . gnid . clone(),
                           RelationshipEditRequest::relRepo_of (&unknown . relationshipEditRequest) . cloned ())),
      ViewnodeKind::Vognode (Vognode::Ghost (ghost))
        if ghost . claimsMembership =>
          // As an unknown's: the list still names it.
          members . push (( ghost . gnid . clone (), None )),
      _ => {}, }}
  dedup_members_by_gnid (members) }

/// This returns the members of a subscribeeFolder: its occurrence children
/// that pass the partner folder membership predicate, deduplicated (by
/// ID; see 'dedup_members_by_id'). Like 'content_members' (and for
/// the same reason), a tombstone contributes nothing:
/// 'subscribesTo' is order-meaningful, but disk supplementation (its weave)
/// already restores invisible subscribees at their disk position.
/// Each member is paired with its headline's explicit
/// '(editRequest (relRepo NAME))' request, if any.
#[allow(non_snake_case)]
fn subscribeeFolder_members (
  node_ref : NodeRef<Viewnode>,
) -> Vec<(ID, Option<SkgrepoName>)> {
  let mut members : Vec<(ID, Option<SkgrepoName>)> = Vec::new();
  for child in node_ref . children() {
    match &child . value() . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (t))
        if member_counts_for_partnerFolder (t) =>
          members . push ((t . gnid . clone(),
                           RelationshipEditRequest::relRepo_of (&t . relationshipEditRequest) . cloned ())),
      ViewnodeKind::Vognode (Vognode::Unknown (unknown))
        if unknown . claimsMembership =>
          members . push ((unknown . gnid . clone(),
                           RelationshipEditRequest::relRepo_of (&unknown . relationshipEditRequest) . cloned ())),
      ViewnodeKind::Vognode (Vognode::Ghost (ghost))
        if ghost . claimsMembership =>
          // As an unknown's: the list still names it.
          members . push (( ghost . gnid . clone (), None )),
      _ => {}, }}
  dedup_members_by_gnid (members) }

/// This returns the content of an editable vognode: its occurrence
/// children that pass the contains predicate. It does not dedup,
/// because validation ('nonignored_children_have_distinct_ids')
/// already guarantees distinctness. Each member is paired with its
/// headline's explicit '(editRequest (relRepo NAME))' request, if any (see
/// 'FieldIntent').
///
/// Restricted nodes contribute nothing here: a restricted node has no
/// occurrence in the buffer (at most a tombstone where one stood), so
/// it emits no save intention for its container. Its membership in the
/// container's contains is owned entirely by disk supplementation
/// ('preserve_invisible_members' -> weave in from_text/weave.rs),
/// which restores invisible members from disk at their disk position,
/// so a stale or concurrently-edited buffer cannot resurrect a member
/// that was authoritatively removed, nor move one.
fn content_members (
  node_ref : NodeRef<Viewnode>,
) -> Vec<(ID, Option<SkgrepoName>)> {
  let mut contents : Vec<(ID, Option<SkgrepoName>)> = Vec::new();
  for child in node_ref . children() {
    match &child . value() . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (t)) => {
        if occurrence_child_counts_as_content (t) {
          contents . push ((
            // collected_id, not id: a shown overrider stands for
            // the original member it is shown in place of.
            t . collected_gnid (),
            RelationshipEditRequest::relRepo_of (&t . relationshipEditRequest) . cloned () )); }},
      ViewnodeKind::Vognode (Vognode::Unknown (unknown))
        if unknown . claimsMembership =>
        // An Unknown is save-inert as a node, but its raw ID is load-bearing
        // membership data at a structured relationship position. `None` asks
        // disk supplementation to keep an existing destination skgrepo sticky.
        contents . push (( unknown . gnid . clone(),
                           RelationshipEditRequest::relRepo_of (&unknown . relationshipEditRequest) . cloned () )),
      ViewnodeKind::Vognode (Vognode::Ghost (ghost))
        if ghost . claimsMembership =>
        // A ghost the list still names (a dangling reference to a node
        // HEAD has) keeps its gnid there, as an unknown does.
        contents . push (( ghost . gnid . clone (), None )),
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
      ViewnodeKind::Vognode (Vognode::Occurrence (t))
        if occurrence_child_counts_as_visible_content (t) => {
        visible . push (
          // collected_id: a shown overrider presents the original,
          // so hide/unhide inference must speak of the original.
          t . collected_gnid ()); },
      ViewnodeKind::Vognode (Vognode::Unknown (unknown))
        if unknown . claimsMembership =>
        visible . push (unknown . gnid . clone ()),
      ViewnodeKind::Vognode (Vognode::Ghost (ghost))
        if ghost . claimsMembership =>
        visible . push (ghost . gnid . clone ()),
      _ => {}, }}
  visible }
