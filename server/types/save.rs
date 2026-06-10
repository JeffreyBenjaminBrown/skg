use super::misc::{ID, SourceName};
use super::nodes::complete::NodeComplete;
use super::errors::{SaveError, BufferValidationError};


/////////////////
/// Types
/////////////////

/// When a user changes a node's source,
/// one of these is generated
/// (in addition to the usual DefineNode).
#[derive(Debug)]
pub struct SourceMove {
  pub pid        : ID,
  pub old_source : SourceName,
  pub new_source : SourceName,
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
pub enum DefineNode {
  // PITFALL: Save(SaveNode) might smell funny, but consider that
  // some functions and type fields require specifically a SaveNode,
  // not a DefineNode.
  Save (SaveNode),
  Delete (DeleteNode),
}

/// A Save instruction.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SaveNode(pub NodeComplete);

/// A Delete instruction.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct DeleteNode {
  pub id: ID,
  pub source: SourceName,
}

/// The whole plan a save applies to the graph, with no view attached: the
/// graph-mutation step consumes only this. (Named 'SavePlan', not e.g.
/// 'SaveInstructions', because the plan is whole -- a bare list of instructions
/// might not be.) The rerender step consumes the ViewForest -- returned
/// alongside this as the other half of 'buffer_to_validated_saveplan's pair,
/// not stored here -- plus this plan's PIDs, for collateral selection.
/// (TODO/DONE/local-view-update/plan_v2.org §11.)
#[derive(Debug)]
pub struct SavePlan {
  pub define_nodes       : Vec<DefineNode>,
  pub nodeMerge_instructions : Vec<NodeMerge>,
  pub source_moves       : Vec<SourceMove>,
}

/// When an 'acquiree' merges into an 'acquirer',
/// we need two SaveNodes and a DeleteNode.
#[derive(Debug, Clone)]
pub struct NodeMerge {
  pub acquiree_text_preserver : SaveNode, // new node with acquiree's title and body
  pub updated_acquirer        : SaveNode, // acquirer with acquiree's IDs, contents, and relationships merged in. (This is complex; see 'three_nodeMerged_nodecompletes'.)
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
      SaveError::BufferValidationErrors (errors) => {
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
    SaveError::BufferValidationErrors (errors) => {
      let mut content : String =
        String::from ("* NOTHING WAS SAVED\n\nValidation errors found in buffer.\n\n");
      for (i, error) in errors . iter() . enumerate() {
        content . push_str(&format!("** Error {}\n", i + 1));
        content . push_str(&format_buffer_validation_error (error));
        content . push ('\n'); }
      content . push_str ("** Resolution\n");
      content . push_str ("Please fix these errors and try saving again.\n");
      content }} }

fn format_buffer_validation_error (
  error : &BufferValidationError
) -> String {
  match error {
    BufferValidationError::Body_of_Scaffold(title, kind) => {
      format!("{} node has a body (not allowed):\n- Title: {}\n",
              kind, title) },
    BufferValidationError::IDCol_Edited(owner, buffer_ids, real_ids) => {
      let fmt_ids = |ids : &Vec<ID>| -> String {
        ids . iter() . map(|i| i . 0 . as_str())
          . collect::<Vec<&str>>() . join(", ") };
      if real_ids . is_empty() {
        format!("Node {} is not in the graph, so it cannot carry an idCol:\n- ids claimed by the buffer: {}\n- IDs cannot be created through the buffer. To edit a node's ID list, edit its .skg file directly.\n",
                owner . 0, fmt_ids(buffer_ids))
      } else {
        format!("The idCol under node {} was edited; saving would not honor that, so the save was aborted:\n- ids claimed by the buffer: {}\n- the node's real ids: {}\n- Reordering is fine, but IDs cannot be added, removed or edited through the buffer. To edit a node's ID list, edit its .skg file directly.\n",
                owner . 0, fmt_ids(buffer_ids), fmt_ids(real_ids)) }},
    BufferValidationError::Multiple_Defining_Viewnodes (id) => {
      format!("ID has multiple defining containers:\n- ID: {}\n",
              id . 0) },
    BufferValidationError::AmbiguousDeletion (id) => {
      format!("ID has ambiguous deletion instructions:\n- ID: {}\n",
              id . 0) },
    BufferValidationError::DuplicatedContent (id) => {
      format!("Node has multiple Content children with the same ID:\n- ID: {}\n",
              id . 0) },
    BufferValidationError::InconsistentSources(id, sources) => {
      let source_list: Vec<String> =
        sources . iter() . map(|s| s . 0 . clone()) . collect();
      format!( "Multiple viewnodes with ID {} have inconsistent sources:\n- Sources: {:?}\n- All instances of the same ID must have the same source.\n",
              id . 0, source_list) },
    BufferValidationError::ModifiedForeignNode(id, source) => {
      format!("Cannot modify node from foreign (read-only) source:\n- ID: {}\n- Source: {}\n- Foreign sources can only be viewed, not modified.\n",
              id . 0, source) },
    BufferValidationError::CreatedForeignNode(id, source) => {
      format!("Cannot create node in foreign (read-only) source:\n- ID: {}\n- Source: {}\n- Foreign sources can only be viewed, not modified.\n",
              id . 0, source) },
    BufferValidationError::CannotMoveToOrFromForeignSource(id, disk_source, buffer_source) => {
      format!("Cannot move node between sources:\n- ID: {}\n- Source on disk: {}\n- Source from buffer: {}\n- One or both sources are foreign (read-only).\n",
              id . 0, disk_source, buffer_source) },
    BufferValidationError::CannotMoveAndMergeSimultaneously(id) => {
      format!("Cannot move and merge a node simultaneously:\n- ID: {}\n- Please save the move and merge in separate operations.\n",
              id . 0) },
    BufferValidationError::SourceNotInConfig(id, source) => {
      format!("Node references a source that does not exist in config:\n- ID: {}\n- Source: {}\n- Please check your config file and ensure this source is defined.\n",
              id . 0, source) },
    BufferValidationError::OverrideInvariantViolation(msg) => {
      format!("{}\n", msg) },
    BufferValidationError::DefinitiveRequestOnDefinitiveNode (id) => {
      format!("Definitive view request on a node that is already definitive:\n- ID: {}\n- The node already shows its content; no expansion needed.\n",
              id . 0) },
    BufferValidationError::DefinitiveRequestOnNodeWithContentChildren (id) => {
      format!("Definitive view request on a node with content children:\n- ID: {}\n- The expansion would clobber those children.\n- Save without the request first, then delete children and retry.\n",
              id . 0) },
    BufferValidationError::MultipleDefinitiveRequestsForSameId (id) => {
      format!("Multiple definitive view requests for the same ID:\n- ID: {}\n- At most one definitive view request per ID is allowed.\n",
              id . 0) },
    BufferValidationError::EmptyTitle(id) => {
      format!("Node has an empty title:\n- ID: {}\n- Every definitive node must have a non-empty title.\n",
              id . 0) },
    BufferValidationError::LocalStructureViolation(msg, id) => {
      format!("Local structure violation:\n- ID: {}\n- {}\n",
              id . 0, msg) },
    BufferValidationError::EditRequestOnIndefinitive (id) => {
      format!("Edit request on an indefinitive (possibly a phantom) node:\n- ID: {}\n- Indefinitive nodes cannot carry write instructions.\n- To delete or merge this node, visit a definitive view of it first (C-c g RET).\n",
              id . 0) },
    BufferValidationError::Other (msg) => {
      format!("{}\n", msg) }, }}

impl DefineNode {
  pub fn is_delete (&self) -> bool {
    matches!(self, DefineNode::Delete (_))
  }

  pub fn is_save (&self) -> bool {
    matches!(self, DefineNode::Save (_))
  }

  /// Split a slice of DefineNodes into (deletes, saves),
  /// cloning each item.
  pub fn partition_save_and_delete (
    node_defs : &[DefineNode]
  ) -> ( Vec<DeleteNode>, Vec<SaveNode> ) {
    use itertools::{Itertools, Either};
    node_defs . iter () . cloned () . partition_map (
      |instr| match instr {
        DefineNode::Delete (d) => Either::Left (d),
        DefineNode::Save (s)   => Either::Right (s) } )
  }
}

impl From<SaveNode> for DefineNode {
  fn from(save: SaveNode) -> Self {
    DefineNode::Save (save)
  }
}

impl From<DeleteNode> for DefineNode {
  fn from(del: DeleteNode) -> Self {
    DefineNode::Delete (del)
  }
}

impl NodeMerge {
  pub fn to_vec (
    &self
  ) -> Vec<DefineNode> {
    vec![
      self . acquiree_text_preserver . clone() . into(),
      self . updated_acquirer . clone() . into(),
      self . acquiree_to_delete . clone() . into(),
    ] }

  pub fn acquirer_id (
    &self
  ) -> &ID {
    &self . updated_acquirer . 0 . pid
  }

  pub fn acquiree_id (
    &self
  ) -> &ID {
    &self . acquiree_to_delete . id
  }

  /// Extracts the three targets from a NodeMerge:
  /// - acquiree_text_preserver -> &NodeComplete
  /// - updated_acquirer -> &NodeComplete
  /// - acquiree_to_delete -> (&ID, &SourceName)
  pub fn targets_from_nodeMerge (
    &self
  ) -> (&NodeComplete, &NodeComplete, (&ID, &SourceName)) {
    ( &self . acquiree_text_preserver . 0,
      &self . updated_acquirer . 0,
      (&self . acquiree_to_delete . id, &self . acquiree_to_delete . source) )
  }
}
