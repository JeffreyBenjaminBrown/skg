use crate::types::misc::{ID, SkgConfig, SourceNickname};
use crate::types::errors::BufferValidationError;
use crate::types::save::{SaveInstruction, NonMerge_NodeAction, MergeInstructionTriple};
use crate::dbs::filesystem::one_node::optskgnode_from_id;
use typedb_driver::TypeDBDriver;

/// Validates that foreign (read-only) nodes are not being modified.
/// Filters out foreign nodes without modifications (no need to write).
///
/// ERRORS: if an instruction
/// - Would modify or delete a foreign node
/// - Would create a node in a foreign source
pub async fn validate_and_filter_foreign_instructions(
  instructions: Vec<SaveInstruction>,
  config: &SkgConfig,
  driver: &TypeDBDriver,
) -> Result<Vec<SaveInstruction>,
            Vec<BufferValidationError>> {
  let mut errors: Vec<BufferValidationError> = Vec::new();
  let mut filtered: Vec<SaveInstruction> = Vec::new();
  for (node, action) in instructions {
    if ! { let is_foreign: bool =
             config . sources . get(&node.source)
             . map(|s| !s.user_owns_it)
             . unwrap_or(false);
           is_foreign }
    { // nothing to worry about; move on
      filtered.push((node, action));
      continue; }
    let primary_id : &ID = match node.primary_id() {
      Ok(id) => id,
      Err(e) => {
        errors.push(BufferValidationError::Other(e));
        continue; } };
    match action { // maybe worry about it
      NonMerge_NodeAction::Delete => {
        // Cannot delete foreign nodes
        errors.push(BufferValidationError::ModifiedForeignNode(
          primary_id . clone(),
          SourceNickname::from ( node.source.clone() )) ); }
      NonMerge_NodeAction::Save => {
        // Check if node has been modified.
        // TODO : Later, rather than bork, an attempt to save a foreign node should create a local 'lens' onto it: a node that overrides it, subscribes to it, and begins with whatever contents the user saved.
        match optskgnode_from_id(
          config, driver, primary_id
        ).await {
          Ok(Some(disk_node)) => {
            /* Compare definitive fields (title, body, contains) and non-definitive fields (aliases).
For *definitive* fields (title, body, contains):
Some([]) and None are equivalent, so normalize them for comparison.
.
But for *non-definitive* fields -- aliases currently,
and eventually also overrides_view_of, subscribes_to,
and hides_from_its_subscriptions -- None means "no opinion"
(because the user did not mention it in the buffer),
and therefore does not represent an edit.
.
TODO: When overrides_view_of, subscribes_to, and hides_from_its_subscriptionsare implemented, apply the same "no opinion" logic */
            let title_matches: bool = node.title == disk_node.title;
            let body_matches: bool = node.body == disk_node.body;
            let contains_matches: bool =
              flatten_opt_vec(&node.contains) ==
              flatten_opt_vec(&disk_node.contains);
            let aliases_matches: bool =
              node.aliases.is_none() ||
              ( flatten_opt_vec(&node.aliases) ==
                flatten_opt_vec(&disk_node.aliases) );
            if !(title_matches &&
                 body_matches &&
                 contains_matches &&
                 aliases_matches) {
              errors.push(BufferValidationError::ModifiedForeignNode(
                primary_id . clone(),
                SourceNickname::from(node.source.clone() )) ); }
            // If unchanged, filter out (no need to write)
          }
          Ok(None) => {
            // 'Foreign' node not found on disk.
            errors.push(BufferValidationError::ModifiedForeignNode(
              primary_id . clone(),
              SourceNickname::from(node.source.clone() )) ); }
          Err(e) => { // Other error reading from disk
            return Err(vec![BufferValidationError::Other(
              format!("Error reading foreign node {}: {}",
                      primary_id . as_str(), e)) ] ); }} }} }
  if errors.is_empty() { Ok(filtered)
  } else { Err(errors) }}

/// Validates that merge instructions involve no foreign nodes.
/// A merge modifies the acquirer and deletes the acquiree,
/// so both must be from sources the user owns.
pub(super) fn validate_merges_involve_only_owned_nodes(
  merge_instructions: &[MergeInstructionTriple],
  config: &SkgConfig,
) -> Result<(), Vec<BufferValidationError>> {
  let mut errors: Vec<BufferValidationError> =
    Vec::new();
  for triple in merge_instructions {
    { // Check if acquirer is from foreign source
      let acquirer_source: &String =
        &triple.updated_acquirer.0.source;
      if { let acquirer_is_foreign: bool =
             config . sources . get(acquirer_source)
             . map(|s| !s.user_owns_it)
             . unwrap_or(false);
           acquirer_is_foreign }
      { match triple.acquirer_id()
        { Ok(id) => errors.push(
            BufferValidationError::ModifiedForeignNode(
                id . clone(),
                SourceNickname::from(acquirer_source.clone() )) ),
          Err(e) => errors.push(
            BufferValidationError::Other(e)), }; }}
    { // Check if acquiree is from foreign source
      let acquiree_source: &String =
        &triple.acquiree_to_delete.0.source;
      if { let acquiree_is_foreign: bool =
             config.sources.get(acquiree_source)
             . map(|s| !s.user_owns_it)
             . unwrap_or (false);
           acquiree_is_foreign }
      { match triple.acquiree_id() {
          Ok(id) => errors.push(BufferValidationError::ModifiedForeignNode(
            id . clone(),
            SourceNickname::from(acquiree_source.clone() )) ),
          Err(e) => errors.push(BufferValidationError::Other(e)),
        }; }} }
  if errors.is_empty() { Ok(( ))
  } else { Err(errors) }}

/// Lets us treat Some([]) and None as equivalent.
fn flatten_opt_vec<T: Clone>(
  v: &Option<Vec<T>>
) -> Option<Vec<T>> {
  match v {
    Some(vec) if vec.is_empty() => None,
    other => other.clone(), }}
