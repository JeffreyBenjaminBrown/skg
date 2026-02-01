use super::misc::ID;
use super::skgnode::SkgNode;
use super::errors::{SaveError, BufferValidationError};


/////////////////
/// Types
/////////////////

/// Defines what to do with a single node: save it or delete it.
/// PITFALL: Don't merge the 'Merge' type into this one.
/// It might seem natural, but there are places where you expect
/// a save or a delete and do not expect a merge. I tried it anyway.
/// The resulting pattern-matching and error-guarding was ugly.
/// (Maybe especially because a Merge naturally consists of
/// two Saves and a Delete, neither of which it is reasonable
/// to represent with a Merge.)
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum DefineOneNode {
  Save(SkgNode),
  Delete(SkgNode),
}

/// When an 'acquiree' merges into an 'acquirer',
/// we need three DefineOneNodes.
#[derive(Debug, Clone)]
pub struct Merge {
  pub acquiree_text_preserver : DefineOneNode, // new node with acquiree's title and body
  pub updated_acquirer        : DefineOneNode, // acquirer with acquiree's IDs, contents, and relationships merged in. (This is complex; see 'three_merged_skgnodes'.)
  pub acquiree_to_delete      : DefineOneNode,
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
    BufferValidationError::Body_of_Scaffold(title, kind) => {
      format!("{} node has a body (not allowed):\n- Title: {}\n",
              kind, title) },
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
    BufferValidationError::ModifiedForeignNode(id, source) => {
      format!("Cannot modify node from foreign (read-only) source:\n- ID: {}\n- Source: {}\n- Foreign sources can only be viewed, not modified.\n",
              id.0, source) },
    BufferValidationError::DiskSourceBufferSourceConflict(id, disk_source, buffer_source) => {
      format!("Source mismatch for node:\n- ID: {}\n- Source on disk: {}\n- Source from buffer: {}\n- Nodes cannot be moved between sources.\n",
              id.0, disk_source, buffer_source) },
    BufferValidationError::SourceNotInConfig(id, source) => {
      format!("Node references a source that does not exist in config:\n- ID: {}\n- Source: {}\n- Please check your config file and ensure this source is defined.\n",
              id.0, source) },
    BufferValidationError::DefinitiveRequestOnDefinitiveNode(id) => {
      format!("Definitive view request on a node that is already definitive:\n- ID: {}\n- The node already shows its content; no expansion needed.\n",
              id.0) },
    BufferValidationError::DefinitiveRequestOnNodeWithChildren(id) => {
      format!("Definitive view request on a node with children:\n- ID: {}\n- The expansion would clobber those children.\n- Save without the request first, then delete children and retry.\n",
              id.0) },
    BufferValidationError::MultipleDefinitiveRequestsForSameId(id) => {
      format!("Multiple definitive view requests for the same ID:\n- ID: {}\n- At most one definitive view request per ID is allowed.\n",
              id.0) },
    BufferValidationError::LocalStructureViolation(msg, id) => {
      format!("Local structure violation:\n- ID: {}\n- {}\n",
              id.0, msg) },
    BufferValidationError::Other(msg) => {
      format!("{}\n", msg) }, }}

impl DefineOneNode {
  pub fn node(&self) -> &SkgNode {
    match self {
      DefineOneNode::Save(n) => n,
      DefineOneNode::Delete(n) => n,
    } }

  pub fn into_node(self) -> SkgNode {
    match self {
      DefineOneNode::Save(n) => n,
      DefineOneNode::Delete(n) => n,
    } }

  pub fn is_delete(&self) -> bool {
    matches!(self, DefineOneNode::Delete(_))
  }

  pub fn is_save(&self) -> bool {
    matches!(self, DefineOneNode::Save(_))
  }
}

impl Merge {
  pub fn to_vec (
    &self
  ) -> Vec<DefineOneNode> {
    vec![
      self.acquiree_text_preserver.clone(),
      self.updated_acquirer.clone(),
      self.acquiree_to_delete.clone(),
    ] }

  pub fn acquirer_id (
    &self
  ) -> Result<&ID, String> {
    self.updated_acquirer.node().primary_id() }

  pub fn acquiree_id (
    &self
  ) -> Result<&ID, String> {
    self.acquiree_to_delete.node().primary_id() }
}
