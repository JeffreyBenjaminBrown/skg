use super::{ID, SkgNode, SaveError, BufferValidationError};


/////////////////
/// Types
/////////////////

pub type SaveInstruction = (SkgNode, NonMerge_NodeAction);

/// Tells Rust what to do with a node.
/// PITFALL: What about merges, you ask? Any node saved with a merge request might have other edits, too. So, too, might the acquiree referred to by that merge request. Those edits need to be handled. The NonMerge_NodeAction will be used for that purpose. Only after all "normal" edits are executed do we then execute the merge.
#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum NonMerge_NodeAction {
  /// The default case: the org-node's title, body and content define those of the node.
  Save,
  Delete,
}

/// When an 'acquiree' merges into an 'acquirer',
/// we need three SaveInstructions.
#[derive(Debug, Clone)]
pub struct MergeInstructionTriple {
  pub acquiree_text_preserver : SaveInstruction, // new node with acquiree's title and body
  pub updated_acquirer        : SaveInstruction, // acquirer with acquiree's IDs, contents, and relationships merged in. (This is complex; see 'three_merged_skgnodes'.)
  pub acquiree_to_delete      : SaveInstruction,
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
      SaveError::ParseError(msg) =>
        write!(f, "Parse error: {}", msg),
      SaveError::DatabaseError(err) =>
        write!(f, "Database error: {}", err),
      SaveError::IoError(err) =>
        write!(f, "IO error: {}", err),
      SaveError::BufferValidationErrors(errors) => {
        write!(f, "Buffer validation errors: {} error(s) found",
               errors.len()) }} }}

impl std::error::Error for SaveError {
  fn source (
    &self
  ) -> Option<&(dyn std::error::Error + 'static)> {
    match self {
      SaveError::DatabaseError(err) => Some(err.as_ref()),
      SaveError::IoError(err) => Some(err),
      _ => None, }} }

/// Formats a SaveError as an org-mode buffer content for the client.
pub fn format_save_error_as_org (
  error : &SaveError
) -> String {
  match error {
    SaveError::ParseError(msg) => {
      format!("* NOTHING WAS SAVED\n\nParse error found when interpreting buffer text as save instructions.\n\n** Error Details\n{}",
              msg) },
    SaveError::DatabaseError(err) => {
      format!("* NOTHING WAS SAVED\n\nDatabase error found when interpreting buffer text as save instructions.\n\n** Error Details\n{}",
              err) },
    SaveError::IoError(err) => {
      format!("* NOTHING WAS SAVED\n\nI/O error found when interpreting buffer text as save instructions.\n\n** Error Details\n{}",
              err) },
    SaveError::BufferValidationErrors(errors) => {
      let mut content : String =
        String::from("* NOTHING WAS SAVED\n\nValidation errors found in buffer.\n\n");
      for (i, error) in errors.iter().enumerate() {
        content.push_str(&format!("** Error {}\n", i + 1));
        content.push_str(&format_buffer_validation_error(error));
        content.push('\n'); }
      content.push_str("** Resolution\n");
      content.push_str("Please fix these errors and try saving again.\n");
      content }} }

fn format_buffer_validation_error (
  error : &BufferValidationError
) -> String {
  match error {
    BufferValidationError::Body_of_AliasCol(node) => {
      format!("AliasCol node has a body (not allowed):\n- Title: {}\n",
              node.title) },
    BufferValidationError::Child_of_AliasCol_with_ID(node) => {
      format!("Child of AliasCol has an ID (not allowed):\n- Title: {}\n",
              node.title) },
    BufferValidationError::Body_of_Alias(node) => {
      format!("Alias node has a body (not allowed):\n- Title: {}\n",
              node.title) },
    BufferValidationError::Child_of_Alias(node) => {
      format!("Alias node has children (not allowed):\n- Title: {}\n",
              node.title) },
    BufferValidationError::Alias_with_no_AliasCol_Parent(node) => {
      format!("Alias node must have an AliasCol parent:\n- Title: {}\n",
              node.title) },
    BufferValidationError::Multiple_AliasCols_in_Children(node) => {
      format!("Node has multiple AliasCol children (only one allowed):\n- Title: {}\n",
              node.title) },
    BufferValidationError::Multiple_Defining_Orgnodes(id) => {
      format!("ID has multiple defining containers:\n- ID: {}\n",
              id.0) },
    BufferValidationError::AmbiguousDeletion(id) => {
      format!("ID has ambiguous deletion instructions:\n- ID: {}\n",
              id.0) },
    BufferValidationError::DuplicatedContent(id) => {
      format!("Node has multiple Content children with the same ID:\n- ID: {}\n",
              id.0) },
    BufferValidationError::InconsistentSources(id, sources) => {
      let source_list: Vec<String> =
        sources.iter().map(|s| s.0.clone()).collect();
      format!( "Multiple orgnodes with ID {} have inconsistent sources:\n- Sources: {:?}\n- All instances of the same ID must have the same source.\n",
              id.0, source_list) },
    BufferValidationError::RootWithoutSource(node) => {
      format!("Root node (top-level in forest) must have a source:\n- Title: {}\n- ID: {:?}\n- Please add (source nickname) to the metadata.\n",
              node.title, node.metadata.id) },
    BufferValidationError::ModifiedForeignNode(id, source) => {
      format!("Cannot modify node from foreign (read-only) source:\n- ID: {}\n- Source: {}\n- Foreign sources can only be viewed, not modified.\n",
              id.0, source) },
    BufferValidationError::DiskSourceBufferSourceConflict(id, disk_source, buffer_source) => {
      format!("Source mismatch for node:\n- ID: {}\n- Source on disk: {}\n- Source from buffer: {}\n- Nodes cannot be moved between sources.\n",
              id.0, disk_source, buffer_source) },
    BufferValidationError::SourceNotInConfig(id, source) => {
      format!("Node references a source that does not exist in config:\n- ID: {}\n- Source: {}\n- Please check your config file and ensure this source is defined.\n",
              id.0, source) },
    BufferValidationError::IndefinitiveWithEditRequest(node) => {
      format!("Indefinitive node cannot have an edit request:\n- Title: {}\n- ID: {:?}\n- Indefinitive nodes represent views and should not be edited or deleted.\n",
              node.title, node.metadata.id) },
    BufferValidationError::Other(msg) => {
      format!("{}\n", msg) }, }}

impl MergeInstructionTriple {
  pub fn to_vec (
    &self
  ) -> Vec<SaveInstruction> {
    vec![
      self.acquiree_text_preserver.clone(),
      self.updated_acquirer.clone(),
      self.acquiree_to_delete.clone(),
    ] }

  pub fn acquirer_id (
    &self
  ) -> &ID {
    &self.updated_acquirer.0.ids[0] }

  pub fn acquiree_id (
    &self
  ) -> &ID {
    &self.acquiree_to_delete.0.ids[0] }

  pub fn preserver_id (
    &self
  ) -> &ID {
    &self.acquiree_text_preserver.0.ids[0] }
}
