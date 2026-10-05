use super::misc::{ID, SkgrepoName};
use super::nodes::complete::Graphnode;
use super::errors::{SaveError, BufferValidationError};


/////////////////
/// Types
/////////////////

/// When a user changes a node's skgrepo,
/// one of these is generated
/// (in addition to the usual NodeInstruction).
#[derive(Debug)]
pub struct SkgrepoMove {
  pub pid         : ID,
  pub old_skgrepo : SkgrepoName,
  pub new_skgrepo : SkgrepoName,
}

/// Defines what to do with a single node: save it or delete it.
/// PITFALL: Don't merge the 'NodeMerge' type into this one.
/// It might seem natural, but there are places where you expect
/// a save or a delete and do not expect a nodeMerge. I tried it anyway.
/// The resulting pattern-matching and error-guarding was ugly.
/// (Maybe especially because a NodeMerge naturally consists of
/// two Saves and a Delete, neither of which it is reasonable
/// to represent with a NodeMerge.)
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NodeInstruction {
  // PITFALL: Save(SaveNode) might smell funny, but consider that
  // some functions and type fields require specifically a SaveNode,
  // not a NodeInstruction.
  Save (SaveNode),
  Delete (DeleteNode),
}

/// A Save nodeInstruction.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SaveNode(pub Graphnode);

/// A Delete nodeInstruction.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct DeleteNode {
  pub skgid: ID,
  pub home_skgrepo: SkgrepoName,
}

/// The whole plan a save applies to the graph, with no view attached: the
/// graph-mutation step consumes only this. (Named 'SavePlan', not e.g.
/// 'SaveInstructions', because the plan is whole -- a bare list of nodeInstructions
/// might not be.) The rerender step consumes the ViewForest -- returned
/// alongside this as the other half of 'buffer_to_validated_saveplan's pair,
/// not stored here -- plus this plan's PIDs, for collateral selection.
/// (TODO/DONE/local-view-update/plan_v2.org §11.)
#[derive(Debug)]
pub struct SavePlan {
  pub node_instructions      : Vec<NodeInstruction>,
  pub nodeMerge_instructions : Vec<NodeMerge>,
  pub skgrepo_moves          : Vec<SkgrepoMove>,
  /// Forks detected this save: editing a foreign node N is read as a
  /// request to clone it. Held SEPARATE from 'node_instructions' because a
  /// save carrying forks is gated on the user's confirmation -- the
  /// clones are skgsave-committed only on approval (see ForkSpec, the save handler's
  /// fork-confirmation stage). Empty for an ordinary save.
  pub fork_specs         : Vec<ForkSpec>,
  /// Facts collected while interpreting a derived editable filter.  They are
  /// deliberately not warnings yet: the save handler turns them into user
  /// messages only after filesystem and graph mutation succeeds.
  pub post_skgsave_commit_notice_candidates : Vec<PostSkgsaveCommitNoticeCandidate>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PostSkgsaveCommitNoticeCandidate {
  HiddenOutsideAdded { subscriber : ID, member : ID },
}

/// One fork: the user made a foreign node N (write-protected, in a skgrepo
/// they do not own) editable and edited it; that edit is read as a
/// request to clone N. 'clone' is the new OWNED node C, built from the
/// edited buffer node -- a fresh pid, an owned skgrepo, the edited
/// title/body/contains, 'subscribesTo = [N]' and
/// 'overrides = [N]', no hides. N itself is left untouched on
/// disk (its foreign SaveNode is dropped). The 'original_*' fields name
/// N, for the monogamy pre-check and the confirmation buffer's display.
#[derive(Debug, Clone)]
pub struct ForkSpec {
  pub clone            : SaveNode,
  pub original_skgid   : ID,
  pub original_title   : String,
  pub original_skgrepo : SkgrepoName,
  /// True iff the clone's skgrepo was SPECIFIED by the user (in the
  /// confirmation buffer, or via explicit skgrepos on N's new children
  /// in the saved metadata), as opposed to inferred or defaulted. A
  /// confirmed skgrepo renders as settled in the confirmation buffer;
  /// an unconfirmed one renders as the PICK-A-REPO placeholder plus
  /// a suggestion, and the client asks before approving.
  pub skgrepo_confirmed : bool,
}

/// When an 'acquiree' merges into an 'acquirer',
/// we need two SaveNodes and a DeleteNode.
#[derive(Debug, Clone)]
pub struct NodeMerge {
  pub acquiree_text_preserver : SaveNode, // new node with acquiree's title and body
  pub updated_acquirer        : SaveNode, // acquirer with acquiree's IDs, contents, and relationships merged in. (This is complex; see 'three_nodeMerged_graphnodes'.)
  pub acquiree_to_delete      : DeleteNode,
}


/////////////////
/// Functions
/////////////////

impl std::fmt::Display for SaveError {
  fn fmt (
    &self,
    f : &mut std::fmt::Formatter<'_>
  ) -> std::fmt::Result {
    match self {
      SaveError::ParseError (msg) =>
        write!(f, "Parse error: {}", msg),
      SaveError::DatabaseError (err) =>
        write!(f, "Database error: {}", err),
      SaveError::IoError (err) =>
        write!(f, "IO error: {}", err),
      SaveError::BufferValidationErrors { errors, .. } => {
        write!(f, "Buffer validation errors: {} error(s) found",
               errors . len()) }} }}

impl std::error::Error for SaveError {
  fn source (
    &self
  ) -> Option<&(dyn std::error::Error + 'static)> {
    match self {
      SaveError::DatabaseError (err) => Some(err . as_ref()),
      SaveError::IoError (err) => Some (err),
      _ => None, }} }

/// Formats a SaveError as an org-mode buffer content for the client.
pub fn format_save_error_as_org (
  error : &SaveError
) -> String {
  match error {
    SaveError::ParseError (msg) => {
      format!("* NOTHING WAS SAVED\n\nParse error found when interpreting buffer text as save instructions.\n\n** Error Details\n{}",
              msg) },
    SaveError::DatabaseError (err) => {
      format!("* NOTHING WAS SAVED\n\nDatabase error found when interpreting buffer text as save instructions.\n\n** Error Details\n{}",
              err) },
    SaveError::IoError (err) => {
      format!("* NOTHING WAS SAVED\n\nI/O error found when interpreting buffer text as save instructions.\n\n** Error Details\n{}",
              err) },
    SaveError::BufferValidationErrors { errors, .. } => {
      let mut content : String =
        String::from ("* NOTHING WAS SAVED\n\nValidation errors found in buffer.\n\n");
      for (i, error) in errors . iter() . enumerate() {
        content . push_str(&format!("** Error {}\n", i + 1));
        content . push_str(&format_buffer_validation_error (error));
        content . push ('\n'); }
      content }} }

fn format_buffer_validation_error (
  error : &BufferValidationError
) -> String {
  match error {
    BufferValidationError::Body_of_NonVognode(title, kind) => {
      format!("{} node has a body (not allowed):\n- Title: {}\n",
              kind, title) },
    BufferValidationError::IDFolder_Edited(recorder, buffer_skgids, real_skgids) => {
      let fmt_skgids = |skgids : &Vec<ID>| -> String {
        skgids . iter() . map(|i| i . 0 . as_str())
          . collect::<Vec<&str>>() . join(", ") };
      if real_skgids . is_empty() {
        format!("Node {} is not in the graph, so it cannot carry an idFolder:\n- ids claimed by the buffer: {}\n- IDs cannot be created through the buffer. To edit a node's ID list, edit its .skg file directly.\n",
                recorder . 0, fmt_skgids(buffer_skgids))
      } else {
        format!("The idFolder under node {} was edited; saving would not honor that, so the save was aborted:\n- ids claimed by the buffer: {}\n- the node's real ids: {}\n- Reordering is fine, but IDs cannot be added, removed or edited through the buffer. To edit a node's ID list, edit its .skg file directly.\n",
                recorder . 0, fmt_skgids(buffer_skgids), fmt_skgids(real_skgids)) }},
    BufferValidationError::OverridesHere_Mismatch(carrier, original, effective) => {
      let fmt_opt = |skgid : &Option<ID>| -> String {
        skgid . as_ref () . map ( |i| i . 0 . clone () )
          . unwrap_or_else ( || "<none>" . to_string () ) };
      format!("Invalid (overridesHere ...) fact; saving it would rewrite a contains list, so the save was aborted:\n- the node carrying the fact: {}\n- the original the fact claims it stands for: {}\n- what the server would draw in place of that original: {}\n- The fact looks hand-edited or stale. Re-render the view (close and reopen, or C-c g RET) and retry.\n",
              fmt_opt (carrier), original . 0, fmt_opt (effective)) },
    BufferValidationError::Multiple_Defining_Viewnodes (skgid) => {
      format!("ID has multiple defining containers:\n- ID: {}\n",
              skgid . 0) },
    BufferValidationError::AmbiguousDeletion (skgid) => {
      format!("ID has ambiguous deletion instructions:\n- ID: {}\n",
              skgid . 0) },
    BufferValidationError::DuplicatedContent (skgid) => {
      format!("Node has multiple Content children with the same ID:\n- ID: {}\n",
              skgid . 0) },
    BufferValidationError::InconsistentSkgrepos(skgid, skgrepos) => {
      let skgrepo_list: Vec<String> =
        skgrepos . iter() . map(|s| s . 0 . clone()) . collect();
      format!( "Multiple viewnodes with ID {} have inconsistent repos:\n- Repos: {:?}\n- All occurrences of the same ID must have the same repo.\n",
              skgid . 0, skgrepo_list) },
    BufferValidationError::ModifiedForeignNode(skgid, skgrepo) => {
      format!("Cannot modify node from foreign repo:\n- ID: {}\n- Repo: {}\n- Foreign repos can only be viewed, not modified.\n",
              skgid . 0, skgrepo) },
    BufferValidationError::CreatedForeignNode(skgid, skgrepo) => {
      format!("Cannot create node in foreign repo:\n- ID: {}\n- Repo: {}\n- Foreign repos can only be viewed, not modified.\n",
              skgid . 0, skgrepo) },
    BufferValidationError::CannotMoveToOrFromForeignSkgrepo(skgid, disk_skgrepo, buffer_skgrepo) => {
      format!("Cannot move node between repos:\n- ID: {}\n- Repo on disk: {}\n- Repo from buffer: {}\n- One or both repos are foreign.\n",
              skgid . 0, disk_skgrepo, buffer_skgrepo) },
    BufferValidationError::CannotMoveAndMergeSimultaneously(skgid) => {
      format!("Cannot move and merge a node simultaneously:\n- ID: {}\n- Please save the move and merge in separate operations.\n",
              skgid . 0) },
    BufferValidationError::SkgrepoNotInConfig(skgid, skgrepo) => {
      format!("Node references a repo that does not exist in config:\n- ID: {}\n- Repo: {}\n- Please check your config file and ensure this repo is defined.\n",
              skgid . 0, skgrepo) },
    BufferValidationError::ForkSkgrepoUnresolved(skgid) => {
      format!("Cannot fork a foreign node -- no owned repo for the clone:\n- Foreign node: {}\n- It has no owned ancestor in the view to inherit a repo from.\n- Set the clone's repo in the fork-confirmation buffer (C-c s s), then approve.\n",
              skgid . 0) },
    BufferValidationError::ForkAlreadyExists(original, existing) => {
      format!("Cannot fork a node you have already forked:\n- Foreign node: {}\n- Your existing clone: {}\n- A node may have at most one owned override. Edit the existing clone instead.\n",
              original . 0, existing . 0) },
    BufferValidationError::ForkSkgrepoRestricted(skgid, skgrepo) => {
      format!("Cannot fork into a restricted repo:\n- Foreign node: {}\n- Clone's resolved repo: {}\n- That repo is not in the skgrepo restriction. Activate it first; an invisible clone is never created silently.\n",
              skgid . 0, skgrepo) },
    BufferValidationError::ForkSkgrepoNotOwned(skgid, skgrepo) => {
      format!("Cannot fork into a repo you do not own:\n- Foreign node: {}\n- Clone's chosen repo: {}\n- Pick an owned repo for the clone (C-c s s in the confirmation buffer).\n",
              skgid . 0, skgrepo) },
    BufferValidationError::ForkRequestOnUnknownNode(skgid) => {
      format!("Cannot fork an unsaved node:\n- Node: {}\n- It is not in the graph. Only a saved node can be forked; save it first, then fork.\n",
              skgid . 0) },
    BufferValidationError::ForkRequestMultiple(skgid) => {
      format!("Multiple fork requests for the same node:\n- Node: {}\n- At most one fork request per node is allowed.\n",
              skgid . 0) },
    BufferValidationError::OverrideInvariantViolation(msg) => {
      format!("{}\n", msg) },
    BufferValidationError::EditableViewRequestOnEditableNode (skgid) => {
      format!("Editable view request on a node that is already editable:\n- ID: {}\n- The node already shows its content; no expansion needed.\n",
              skgid . 0) },
    BufferValidationError::EditableViewRequestOnNodeWithContentChildren (skgid) => {
      format!("Editable view request on a node with content children:\n- ID: {}\n- The expansion would clobber those children.\n- Save without the request first, then delete children and retry.\n",
              skgid . 0) },
    BufferValidationError::MultipleEditableViewRequestsForSameId (skgid) => {
      format!("Multiple editable view requests for the same ID:\n- ID: {}\n- At most one editable view request per ID is allowed.\n",
              skgid . 0) },
    BufferValidationError::EmptyTitle(skgid) => {
      format!("Node has an empty title:\n- ID: {}\n- Every editable node must have a non-empty title.\n",
              skgid . 0) },
    BufferValidationError::LocalStructureViolation(msg, skgid) => {
      format!("Local structure violation:\n- ID: {}\n- {}\n",
              skgid . 0, msg) },
    BufferValidationError::EditRequestOnWriteProtectedOccurrence (skgid) => {
      format!("Edit request on a write-protected (possibly a phantom) node:\n- ID: {}\n- Write-protected nodes cannot carry write instructions.\n- To delete or merge this node, visit a editable view of it first (C-c g RET).\n",
              skgid . 0) },
    BufferValidationError::EditedWriteProtectedOccurrence {
      skgid, title, changes } => {
      format!("Edited write-protected occurrence:\n- ID: {}\n- Title: {}\n- Changes: {}\n- This occurrence is write-protected; no changes were saved.\n- Re-render, then edit a editable occurrence instead.\n",
              skgid . 0, title, changes . join ("; ")) },
    BufferValidationError::FlagsSurfaceEdited {
      recorder_skgid, recorder_title, changes } => {
      format!("Edited server-owned flags surface:\n- Recorder ID: {}\n- Recorder title: {}\n- Changes: {}\n- No changes were saved. Use skg-set-flag-search-matching for noSearchMatching. HadId and WasOverloaded are provenance and have no setter.\n",
              recorder_skgid . 0, recorder_title, changes . join ("; ")) },
    BufferValidationError::FlagEditOnForeignNode (skgid, skgrepo) => {
      format!("Cannot change a flag on a foreign node:\n- ID: {}\n- Repo: {}\n- Flag changes never create an implicit fork. Visit an owned node instead.\n",
              skgid . 0, skgrepo) },
    BufferValidationError::FlagEditOnUnknownNode (skgid) => {
      format!("Cannot change a flag on an unsaved or unknown node:\n- ID: {}\n- Save the node first, then run the flag setter.\n",
              skgid . 0) },
    BufferValidationError::Other (msg) => {
      format!("{}\n", msg) }, }}

impl NodeInstruction {
  pub fn is_delete (&self) -> bool {
    matches!(self, NodeInstruction::Delete (_))
  }

  pub fn is_save (&self) -> bool {
    matches!(self, NodeInstruction::Save (_))
  }

  /// Split a slice of NodeInstructions into (deletes, saves),
  /// cloning each item.
  pub fn partition_save_and_delete (
    node_defs : &[NodeInstruction]
  ) -> ( Vec<DeleteNode>, Vec<SaveNode> ) {
    use itertools::{Itertools, Either};
    node_defs . iter () . cloned () . partition_map (
      |instr| match instr {
        NodeInstruction::Delete (d) => Either::Left (d),
        NodeInstruction::Save (s)   => Either::Right (s) } )
  }
}

impl From<SaveNode> for NodeInstruction {
  fn from(save: SaveNode) -> Self {
    NodeInstruction::Save (save)
  }
}

impl From<DeleteNode> for NodeInstruction {
  fn from(del: DeleteNode) -> Self {
    NodeInstruction::Delete (del)
  }
}

impl NodeMerge {
  pub fn to_vec (
    &self
  ) -> Vec<NodeInstruction> {
    vec![
      self . acquiree_text_preserver . clone() . into(),
      self . updated_acquirer . clone() . into(),
      self . acquiree_to_delete . clone() . into(),
    ] }

  pub fn acquirer_skgid (
    &self
  ) -> &ID {
    &self . updated_acquirer . 0 . pid
  }

  pub fn acquiree_skgid (
    &self
  ) -> &ID {
    &self . acquiree_to_delete . skgid
  }

  /// Extracts the three targets from a NodeMerge:
  /// - acquiree_text_preserver -> &Graphnode
  /// - updated_acquirer -> &Graphnode
  /// - acquiree_to_delete -> (&ID, &RepoName)
  pub fn targets_from_nodeMerge (
    &self
  ) -> (&Graphnode, &Graphnode, (&ID, &SkgrepoName)) {
    ( &self . acquiree_text_preserver . 0,
      &self . updated_acquirer . 0,
      (&self . acquiree_to_delete . skgid, &self . acquiree_to_delete . home_skgrepo) )
  }
}
