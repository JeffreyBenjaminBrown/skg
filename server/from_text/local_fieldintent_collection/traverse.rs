//! This file defines the pure traversal at the heart of local
//! instruction collection (the glossary's "local fieldIntent collection").
//! .
//! The traversal is one recursive DFS preorder over the placed
//! viewforest. Recursion is unconditional: every node's children are
//! visited, whatever the node's kind, because the user can attach
//! independent nodes anywhere. Only emission is conditional, on the
//! pair (kind, context). Each visit reads the node and its direct
//! children, nothing deeper, nothing upward; everything an
//! fieldIntent emission needs from above arrives in its
//! 'LocalContext'.
//! .
//! The traversal assumes that 'find_buffer_errors_for_saving' has
//! passed. In particular it assumes that:
//! - every vognode has a PID and a config-valid skgrepo;
//! - each ID has at most one editable instance
//!   ('Multiple_Defining_Viewnodes');
//! - same-ID instances have consistent toDelete values and skgrepos;
//! - folder shapes are valid: each folder holds only the child kinds it
//!   permits, each folder is unique among its siblings, content members
//!   have distinct IDs per
//!   'claiming_children_have_distinct_gnids', and write-protected-folder
//!   members have distinct IDs per
//!   'partnerFolder_children_have_distinct_gnids'. The defining folders
//!   (aliasFolder, forksFolder) may contain
//!   duplicates; emission dedups them silently.
//! Collection performs no ancestry re-verification. On shapes that
//! validation precludes, it stays total, emitting nothing rather
//! than erroring.
//! .
//! DEFINITION: a vognode is *save-eligible* iff it is an occurrence,
//! editable, lacks a Delete edit request, and is no member as such of
//! a forks folder. (Its position may be anywhere else -- including as
//! a member of a write-protected folder, where it is save-eligible for
//! itself but invisible to the folder's recorder.) An editable member
//! as such is not save-eligible, since it shows its content in part
//! (less what the note whose folder holds it contains or hides, and
//! what an owned note but the member contains): an absent child is
//! integrated (that note or another owned note keeps it) or hidden by
//! that note, never removed from the member. But when the user owns the member, its title and
//! body are saved, and a child typed under it joins its content
//! ('resolve_visibility'). A foreign one's title and body are text
//! claims, which must match the disk.
//! .
//! DEFINITION: a vognode is a *member as such* of a forks folder iff
//! it is an occurrence direct child of the folder. One that claims is
//! *in forkee-as-such position* (a forkee as such): the note forks it.
//! One claiming nothing is *in forker-as-such position* (a forker as
//! such): a forker-only member, which forks the note and records so in
//! its own file, an indirect member, a mirror of the note joined to it
//! only through others, or a detached forkee.

use crate::from_text::local_fieldintent_collection::predicates::{
  occurrence_child_counts_as_content,
  occurrence_child_counts_as_visible_content,
  deleted_mirror_keeps_its_original,
  member_counts_for_partnerFolder };
use crate::from_text::local_fieldintent_collection::types::{
  CollectedFieldIntents, DefiningFolderRecorder, LocalContext, FieldIntent,
  ForksFolderMembers, HiddenOutsideEdit, MemberAsSuch, MemberAsSuchTextClaim,
  MemberAsSuchVisibility };
use crate::types::errors::HeadlineName;
use crate::types::misc::{ID, SkgrepoName};
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{RelationshipEditRequest, 
  NodeEditRequest, PropertyMember, Folder, Occurrence, Viewnode,
  ViewnodeKind, Vognode };

use ego_tree::NodeRef;
use std::collections::HashSet;

/// Whether the user owns a skgrepo ('SkgConfig::skgrepo_is_owned').
/// The traversal asks it of one thing only: whether a parent's
/// content list is another's, where a deleted mirror keeps its
/// original ('deleted_mirror_keeps_its_original').
pub type IsOwned<'a> = &'a dyn Fn (&SkgrepoName) -> bool;

pub fn collect_instructions_locally (
  forest : &ViewForest,
  owned  : IsOwned,
) -> Result<CollectedFieldIntents, String> {
  let mut collected : CollectedFieldIntents =
    CollectedFieldIntents::new();
  for root in forest . roots() {
    visit (root, &LocalContext::TopLevel, &mut collected, owned) ?; }
  Ok (collected) }

fn visit (
  node_ref  : NodeRef<Viewnode>,
  context   : &LocalContext,
  collected : &mut CollectedFieldIntents,
  owned     : IsOwned,
) -> Result<(), String> {
  match &node_ref . value() . kind {
    ViewnodeKind::Vognode (Vognode::Occurrence (t)) =>
      visit_occurrence (node_ref, t, context, collected, owned),
    ViewnodeKind::Vognode (vognode @ ( Vognode::Ghost (_) | Vognode::GnidOnly (_) )) =>
      recurse_under_vognode (
        node_ref,
        Some ( DefiningFolderRecorder {
          // Carrying the identity of the ghost or gnid-only vognode
          // (write-protected, not save-eligible) keeps a
          // forksFolder found under a ghost meaningful: its
          // children stay members as such, so their contents are
          // never read as whole lists, and a foreign one's title
          // edits bounce via text claims.
          gnid               : vognode . gnid() . clone(),
          headline            : node_ref . value () . headline_name (),
          is_editable    : false,
          is_saveEligible : false } ),
        None, collected, owned),
    ViewnodeKind::Tombstone =>
      // A tombstone emits nothing. Where it stands for a restricted
      // node, that node's membership is owned
      // by disk supplementation, not extraction. With no identity it
      // owns no defining folder, so any folder found under it stays
      // silent.
      recurse_under_vognode (node_ref, None, None, collected, owned),
    ViewnodeKind::BufferRoot =>
      // A BufferRoot is unreachable as a child, but the traversal
      // stays total anyway.
      recurse_with_uniform_context (
        node_ref, &LocalContext::TopLevel, collected, owned),
    ViewnodeKind::Folder (Folder::Alias) =>
      visit_aliasFolder (node_ref, context, collected, owned),
    // The two arms below, with the alias folder's above, are exactly
    // the editable folders ('Folder::is_editable'): the
    // ones whose membership the save reads. The catch-all folder arm
    // after them covers the write-protected ones.
    ViewnodeKind::Folder (Folder::Forks) =>
      visit_forks_folder (node_ref, context, collected, owned),
    ViewnodeKind::Folder (Folder::HiddenOutsideOfForkPartners) =>
      visit_hiddenOutside_folder (node_ref, context, collected, owned),
    ViewnodeKind::Folder (Folder::ID)
      | ViewnodeKind::Folder (Folder::Flags { .. })
      | ViewnodeKind::PropertyMember (_)
      | ViewnodeKind::ChangeViewnode (_)
      | ViewnodeKind::Folder (_) =>
      // These are the write-protected folders and the property members. Vognodes
      // found inside them are self-writers; their membership in the
      // folder is never read.
      recurse_with_uniform_context (
        node_ref, &LocalContext::UnderWriteProtectedFolder, collected, owned), }}

fn visit_occurrence (
  node_ref  : NodeRef<Viewnode>,
  t         : &Occurrence,
  context   : &LocalContext,
  collected : &mut CollectedFieldIntents,
  owned     : IsOwned,
) -> Result<(), String> {
  let member_as_such_of : Option<(&ID, bool)> = // the note whose forks folder holds it, and whether that note is editable
    match context {
      LocalContext::MemberAsSuchPosition {
        note, note_is_editable } =>
        Some (( note, *note_is_editable )),
      _ => None };
  let is_editable : bool =
    ! t . is_writeProtected();
  let has_delete_request : bool =
    matches!( t . edit_request(),
              Some (&NodeEditRequest::Delete));
  let is_saveEligible : bool =
    is_editable
    && ! has_delete_request
    && member_as_such_of . is_none();
  let headline : HeadlineName = node_ref . value () . headline_name ();
  if is_editable {
    // Emission happens only inside this block, because an
    // write-protected vognode emits nothing.
    match member_as_such_of {
      Some (( note, note_is_editable )) => {
        if owned (& t . home_skgrepo) {
          // An owned member as such saves its own title and body,
          // and nothing else of itself: what it shows of its
          // content is partial, so its list is not read here.
          collected . instructionMerge_fieldIntent (
            &headline,
            t . gnid . clone(),
            FieldIntent::SetTitleAndBody {
              skgrepo : t . home_skgrepo . clone(),
              title   : t . title . clone(),
              body    : t . body() . cloned() } ) ?;
        } else {
          // A foreign one's title and body must be as on disk: an
          // edit would otherwise be dropped silently.
          collected . instructionMerge_fieldIntent (
            &headline,
            t . gnid . clone(),
            FieldIntent::MemberAsSuchTextClaim (
              MemberAsSuchTextClaim {
                title    : t . title . clone(),
                body     : t . body() . cloned(),
                headline : headline . clone () } )) ?; }
        if note_is_editable {
          // The at-most-one-writer-per-ID guard: only the
          // forksFolder under the editable instance of a
          // note may write its hide edits. The same note
          // can recur write-protected elsewhere with its own
          // forksFolder; without this guard those could emit
          // contradictory hide edits for one ID.
          let visible : Vec<ID> = visible_content_members (node_ref);
          let in_integratedElsewhere_or_hiddenIn_folder : Vec<ID> =
            members_of_integratedElsewhere_and_hiddenIn_folders (node_ref);
          collected . instructionMerge_fieldIntent (
            &headline,
            note . clone(),
            FieldIntent::MemberAsSuchVisibility (
              MemberAsSuchVisibility {
                member : t . gnid . clone(),
                role   : if t . claimsMembership { MemberAsSuch::Forkee }
                         else { MemberAsSuch::Forker },
                visible, in_integratedElsewhere_or_hiddenIn_folder } )) ?; }
        // Delete, NodeMerge and flag edit requests here are
        // ignored: a member as such can affect only its own title
        // and body (when owned), what the note hides, and, by a
        // child typed under it, its own content
        // ('resolve_visibility'). Deleting it from the folder is
        // read from the folder ('ForksFolderMembers').
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
              content_members (
                node_ref, ! owned (& t . home_skgrepo) ) )) ?;
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
    collected, owned) }

/// This recurses into a vognode's children. Vognode
/// children get 'UnderVognode'; defining-folder children get
/// 'UnderDefiningFolder', carrying the recorder's identity (when it has
/// one); and write-protected folders and Properties get 'UnderWriteProtectedFolder'.
fn recurse_under_vognode (
  node_ref            : NodeRef<Viewnode>,
  recorder            : Option<DefiningFolderRecorder>,
  parent_if_editable  : Option<ID>,
  collected           : &mut CollectedFieldIntents,
  owned               : IsOwned,
) -> Result<(), String> {
  for child in node_ref . children() {
    let child_context : LocalContext =
      match &child . value() . kind {
        ViewnodeKind::Folder (Folder::Alias)
          // The partner folder here is the forks folder, the one
          // editable relation folder; the write-protected folders fall
          // to the UnderWriteProtectedFolder arm below.
          | ViewnodeKind::Folder (Folder::Forks) =>
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
    visit (child, &child_context, collected, owned) ?; }
  Ok (( )) }

fn recurse_with_uniform_context (
  node_ref  : NodeRef<Viewnode>,
  context   : &LocalContext,
  collected : &mut CollectedFieldIntents,
  owned     : IsOwned,
) -> Result<(), String> {
  for child in node_ref . children() {
    visit (child, context, collected, owned) ?; }
  Ok (( )) }

fn visit_aliasFolder (
  node_ref  : NodeRef<Viewnode>,
  context   : &LocalContext,
  collected : &mut CollectedFieldIntents,
  owned     : IsOwned,
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
    node_ref, &LocalContext::UnderWriteProtectedFolder, collected, owned) }

/// The forks folder: its claiming members are the recorder's 'forks',
/// and the members it shows at all, claiming or not, the signal from
/// which deleting a forker hides it ('ForksFolderMembers'). A claiming
/// member stands as a forkee as such; one claiming nothing, as a
/// forker as such.
fn visit_forks_folder (
  node_ref  : NodeRef<Viewnode>,
  context   : &LocalContext,
  collected : &mut CollectedFieldIntents,
  owned     : IsOwned,
) -> Result<(), String> {
  match context {
    LocalContext::UnderDefiningFolder (recorder) => {
      if recorder . is_saveEligible {
        collected . instructionMerge_fieldIntent (
          &recorder . headline,
          recorder . gnid . clone(),
          FieldIntent::SetForks (
            partnerFolder_members (node_ref) )) ?;
        collected . instructionMerge_fieldIntent (
          &recorder . headline,
          recorder . gnid . clone(),
          FieldIntent::ForksFolderMembers ( ForksFolderMembers {
            members : forks_folder_members_shown (node_ref) } )) ?; }
      for child in node_ref . children() {
        let child_context : LocalContext =
          match &child . value() . kind {
            ViewnodeKind::Folder (Folder::HiddenOutsideOfForkPartners) =>
              LocalContext::HiddenOutsidePosition {
                hider           : recorder . gnid . clone(),
                is_saveEligible : recorder . is_saveEligible },
            _ =>
              // The note's identity is passed even when the recorder is
              // not save-eligible, because text claims outlive the
              // visibility guard.
              LocalContext::MemberAsSuchPosition {
                note             : recorder . gnid . clone(),
                note_is_editable : recorder . is_editable }, };
        visit (child, &child_context, collected, owned) ?; }
      Ok (( )) },
    _ =>
      // The folder has no identifiable recorder. Validation precludes this
      // shape; the traversal stays total and silent.
      recurse_with_uniform_context (
        node_ref,
        &LocalContext::UnderVognode { parent_if_editable : None },
        collected, owned), }}

/// Collect the explicitly submitted visible-outside subset.  This folder is a
/// compound partner folder rather than a direct relationship set, so its meaning is
/// resolved only after ordinary forkee visibility inference has run.
fn visit_hiddenOutside_folder (
  node_ref  : NodeRef<Viewnode>,
  context   : &LocalContext,
  collected : &mut CollectedFieldIntents,
  owned     : IsOwned,
) -> Result<(), String> {
  if let LocalContext::HiddenOutsidePosition {
    hider, is_saveEligible } = context
  {
    if *is_saveEligible {
      let mut members : Vec<ID> = Vec::new ();
      for child in node_ref . children() {
        let member : Option<(ID, Option<SkgrepoName>)> =
          match &child . value() . kind {
            ViewnodeKind::Vognode (Vognode::Occurrence (t)) =>
              member_counts_for_partnerFolder (t) . then ( || (
                t . gnid . clone (),
                RelationshipEditRequest::relRepo_of (&t . relationshipEditRequest) . cloned () )),
            _ => claiming_gnid_only_or_ghost_member (child . value ()), };
        if let Some ((gnid, relRepo_request)) = member {
          if relRepo_request . is_some () {
            return Err ("HiddenOutsideOfForkPartners membership is editable, but hide relRepos are derived." . to_string ()); }
          members . push (gnid); }}
      collected . instructionMerge_fieldIntent (
        &node_ref . value () . headline_name (),
        hider . clone(),
        FieldIntent::HiddenOutsideEdit (HiddenOutsideEdit { members }) ) ?; }}
  recurse_with_uniform_context (
    node_ref, &LocalContext::UnderWriteProtectedFolder, collected, owned)
}

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

/// Every vognode the forks folder at 'node_ref' shows, claiming or not,
/// but those marked for deletion: the members present in the saved
/// buffer, against which a forker it no longer shows counts as deleted.
fn forks_folder_members_shown (
  node_ref : NodeRef<Viewnode>,
) -> Vec<ID> {
  node_ref . children ()
    . filter_map ( |child| match &child . value () . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (t)) =>
        ( ! matches! ( t . edit_request (), Some (&NodeEditRequest::Delete) ))
        . then ( || t . gnid . clone () ),
      ViewnodeKind::Vognode (vognode) => Some ( vognode . gnid () . clone () ),
      _ => None, } )
    . collect () }

/// This returns the members of a forksFolder: its occurrence
/// children that pass the partner
/// folder membership predicate, and its claiming gnid-only vognodes
/// and ghosts ('claiming_gnid_only_or_ghost_member'),
/// silently deduplicated (by ID; see 'dedup_members_by_gnid'),
/// preserving first-occurrence
/// order. Each member is paired with its headline's explicit
/// '(editRequest (relRepo NAME))' request, if any (see 'FieldIntent').
/// Tombstones contribute nothing. A member whose record the
/// skgrepo restriction keeps out has no headline: the folder omits it from
/// display, and the save preserves it ('forks' is
/// order-meaningful: disk supplementation's weave restores it at its
/// disk position; see the API doc's "Restricted-skgrepo nodes").
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
      _ => members . extend ( claiming_gnid_only_or_ghost_member (child . value ()) ), }}
  dedup_members_by_gnid (members) }

/// A claiming gnid-only vognode or ghost child, as a member of the
/// list its position implies: its gnid, with a gnid-only vognode's
/// relRepo request. A gnid-only vognode is save-inert as a node, but
/// its raw ID is load-bearing membership data at a structured
/// relationship position: an unknown's, of a reference that resolves
/// to nothing, and a restricted viewnode's, of the record it stands
/// for. A ghost the list still names (a dangling reference to a node
/// HEAD has) keeps its gnid there as a gnid-only vognode does, and
/// carries no relRepo request.
/// 'None' as the request asks disk supplementation to keep an
/// existing destination skgrepo sticky. None for any other child.
fn claiming_gnid_only_or_ghost_member (
  viewnode : &Viewnode,
) -> Option<(ID, Option<SkgrepoName>)> {
  match &viewnode . kind {
    ViewnodeKind::Vognode (Vognode::GnidOnly (gnid_only))
      if gnid_only . claimsMembership =>
      Some (( gnid_only . gnid . clone (),
              RelationshipEditRequest::relRepo_of (&gnid_only . relationshipEditRequest) . cloned () )),
    ViewnodeKind::Vognode (Vognode::Ghost (ghost))
      if ghost . claimsMembership =>
      Some (( ghost . gnid . clone (), None )),
    _ => None, } }

/// This returns the content of an editable vognode: its occurrence
/// children that pass the contains predicate, and, where the vognode
/// is another's ('of_another'), the originals that deleted mirrors
/// stand for ('deleted_mirror_keeps_its_original'). It does not dedup,
/// because validation ('claiming_children_have_distinct_gnids')
/// already guarantees distinctness. Each member is paired with its
/// headline's explicit '(editRequest (relRepo NAME))' request, if any (see
/// 'FieldIntent').
///
/// A restricted node a visible record names stands as a Restricted
/// vognode, which keeps its gnid here when it claims. A member whose
/// record is restricted has no headline (at most a tombstone where
/// one stood), so it emits no save intention for its container. Its
/// membership in the container's contains is owned entirely by disk
/// supplementation ('preserve_invisible_members' -> weave in
/// from_text/weave.rs), which restores invisible members from disk at
/// their disk position, so a stale or concurrently-edited buffer
/// cannot resurrect a member that was authoritatively removed, nor
/// move one.
fn content_members (
  node_ref   : NodeRef<Viewnode>,
  of_another : bool,
) -> Vec<(ID, Option<SkgrepoName>)> {
  let mut contents : Vec<(ID, Option<SkgrepoName>)> = Vec::new();
  for child in node_ref . children() {
    match &child . value() . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (t)) => {
        if occurrence_child_counts_as_content (t)
           || ( of_another && deleted_mirror_keeps_its_original (t) ) {
          contents . push ((
            // collected_id, not id: a shown mirror stands for
            // the original member it is shown in place of.
            t . collected_gnid (),
            RelationshipEditRequest::relRepo_of (&t . relationshipEditRequest) . cloned () )); }},
      _ => contents . extend ( claiming_gnid_only_or_ghost_member (child . value ()) ), }}
  contents }

/// This returns the children that the buffer presents as visible
/// content of one member as such of a forks folder. The list is not
/// saved as the member's contains; it is the signal from which the
/// note's hides and unhides, and the children placed under the
/// member (typed there, or moved there), are inferred, downstream. It is not deduplicated.
fn visible_content_members (
  node_ref : NodeRef<Viewnode>,
) -> Vec<ID> {
  let mut visible : Vec<ID> = Vec::new();
  for child in node_ref . children() {
    match &child . value () . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (t))
        if occurrence_child_counts_as_visible_content (t) => {
        visible . push (
          // collected_id: a shown mirror presents the original,
          // so hide/unhide inference must speak of the original.
          t . collected_gnid ()); },
      _ => visible . extend (
        claiming_gnid_only_or_ghost_member (child . value ())
        . map ( |(gnid, _)| gnid )), }}
  visible }

/// The members the buffer shows in the folders of one member as such
/// of a forks folder that set its content aside: what the user keeps
/// elsewhere ('Folder::IntegratedElsewhere') and what the note hides
/// in it ('Folder::HiddenInForkPartner'). A child standing there was
/// not cut from the member, even where the graph changed since the
/// buffer was drawn (another view stopped keeping it, or unhid it):
/// the save reads what the buffer showed ('resolve_visibility').
fn members_of_integratedElsewhere_and_hiddenIn_folders (
  node_ref : NodeRef<Viewnode>,
) -> Vec<ID> {
  node_ref . children ()
    . filter ( |child| matches! (
      &child . value () . kind,
      ViewnodeKind::Folder ( Folder::IntegratedElsewhere
                             | Folder::HiddenInForkPartner )) )
    . flat_map ( |folder| folder . children () )
    . filter_map ( |member| match &member . value () . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (t)) =>
        Some (t . collected_gnid ()),
      ViewnodeKind::Vognode (vognode) =>
        Some (vognode . gnid () . clone ()),
      _ => None, } )
    . collect () }
