use super::ID;
use std::collections::HashSet;
use std::fmt;
use std::str::FromStr;

#[derive(Debug, Clone, PartialEq)]
pub struct OrgNode {
  pub metadata: OrgnodeMetadata,
  pub title: String, // does not re-state the metadata
  pub body: Option<String>,
}

/// Each org headline corresponds to a node.
/// This is the metadata necessary to interpret the headline.
#[derive(Debug, Clone, PartialEq)]
pub struct OrgnodeMetadata {
  pub id: Option<ID>,
  pub source: Option<String>,
  pub viewData: OrgnodeViewData,
  pub code: OrgnodeCode,
}

/* View-related metadata: fields that dictate only how the node is shown. */
#[derive(Debug, Clone, PartialEq)]
pub struct OrgnodeViewData {
  // PITFALL: One could reasonably describe 'focused' and 'folded' as code rather than data. They tell Emacs what to do. Once Emacs has done that, it deletes them from the metadata. The other fields in this type are only acted on to the extent that Emacs displays them.

  pub cycle: bool, // True if a node is in its own org-precedessors.
  pub focused: bool, // Where the cursor is. True for only one node.
  pub folded: bool, // folded in the Emacs org-mode sense
  pub relationships: OrgnodeRelationships,
}

/* These data only influence how the node is shown.
Editing them and then saving the buffer leaves the graph unchanged,
and the edits would be immediately lost,
as this data is regenerated each time the view is rebuilt. */
#[derive(Debug, Clone, PartialEq)]
pub struct OrgnodeRelationships {
  pub parentIsContainer: bool,
  pub parentIsContent: bool,
  pub numContainers: Option<usize>,
  pub numContents: Option<usize>,
  pub numLinksIn: Option<usize>,
}

/// These data determine how the node is treated when saved.
#[derive(Debug, Clone, PartialEq)]
pub struct OrgnodeCode {
  pub relToParent: RelToParent,
  pub indefinitive: bool, // Describes a node's relationship to those of its children that are content. A definitive node defines the title, body and initial contents, if present. So changing which nodes are its children can change its contents. Indefinitive nodes, by contrast, do not permit the user to modify the node they represent. If it has children, they are kept in the view when the user saves, but saving does not create a 'contains' relationship from the indefinitive parent to the child.
  // (On word choice: I record the negative 'indefinitive', rather than the positive default 'definitive', to save characters in the buffer, because the default is omitted from metadata strings, and is much more common.)
  pub editRequest: Option<EditRequest>,
  pub viewRequests: HashSet<ViewRequest>,
}

/// 'RelToParent' describes how a node relates to its parent.
/// It influences both how the user should read it,
/// and how Rust should treat the data when it is saved.
/// PITFALL: It does not describe every potential relationship
/// between the node and its parent.
#[derive(Debug, Clone, PartialEq)]
pub enum RelToParent {
  Content, // The default relationship: parents 'contain' most children.
  AliasCol, // The node collects aliases for its parent.
  Alias, // The node is an alias for its grandparent.
  ParentIgnores, // This node is not used to update its parent. (That does *not* mean it is ignored when the buffer is saved. It and its recursive org-content are processed normally. It only means it has no impact on its parent.)
}

/// Requests for editing operations on a node.
/// Only one edit request is allowed per node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EditRequest {
  Merge(ID), // The node with this request is the acquirer. The node with the ID that this request specifies is the acquiree.
  Delete, // request to delete this node
}

/// Requests for additional views related to a node.
/// Multiple view requests can be active simultaneously.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ViewRequest {
  Aliases,
  Containerward,
  Sourceward,
  Definitive,
}


//
// Implementations
//

impl fmt::Display for RelToParent {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>
  ) -> fmt::Result {
    let s : &str =
      match self {
        RelToParent::Content => "content",
        RelToParent::AliasCol => "aliasCol",
        RelToParent::Alias => "alias",
        RelToParent::ParentIgnores => "parentIgnores",
      };
    write! ( f, "{}", s ) } }

impl FromStr for RelToParent {
  type Err = String;

  fn from_str (
    s : &str
  ) -> Result<Self, Self::Err> {
    match s {
      "content"       => Ok ( RelToParent::Content ),
      "aliasCol"      => Ok ( RelToParent::AliasCol ),
      "alias"         => Ok ( RelToParent::Alias ),
      "parentIgnores" => Ok ( RelToParent::ParentIgnores ),
      _ => Err ( format! ( "Unknown RelToParent value: {}", s )),
    }} }

impl fmt::Display for EditRequest {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>
  ) -> fmt::Result {
    match self {
      EditRequest::Merge(id) => write!(f, "(merge {})", id.0),
      EditRequest::Delete    => write!(f, "toDelete"),
    } } }

impl FromStr for EditRequest {
  type Err = String;

  fn from_str (
    s : &str
  ) -> Result<Self, Self::Err> {
    match s {
      "toDelete" => Ok ( EditRequest::Delete ),
      _ => {
        // Try to parse as "merge <id>"
        if let Some(id_str) = s.strip_prefix("merge ") {
          Ok ( EditRequest::Merge ( ID::from(id_str) ) )
        } else {
          Err ( format! ( "Unknown EditRequest value: {}", s ))
        }} }} }

impl fmt::Display for ViewRequest {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>
  ) -> fmt::Result {
    match self {
      ViewRequest::Aliases       => write!(f, "aliases"),
      ViewRequest::Containerward => write!(f, "containerwardView"),
      ViewRequest::Sourceward    => write!(f, "sourcewardView"),
      ViewRequest::Definitive    => write!(f, "definitiveView"),
    } } }

impl FromStr for ViewRequest {
  type Err = String;

  fn from_str (
    s : &str
  ) -> Result<Self, Self::Err> {
    match s {
      "aliases"           => Ok ( ViewRequest::Aliases ),
      "containerwardView" => Ok ( ViewRequest::Containerward ),
      "sourcewardView"    => Ok ( ViewRequest::Sourceward ),
      "definitiveView"    => Ok ( ViewRequest::Definitive ),
      _ => Err ( format! ( "Unknown ViewRequest value: {}", s )),
    }} }

impl Default for OrgnodeRelationships {
  fn default () -> Self {
    OrgnodeRelationships {
      parentIsContainer : true,
      parentIsContent   : false,
      numContainers : Some ( 1 ),
      numContents   : Some ( 0 ),
      numLinksIn    : Some ( 0 ),
    }} }

impl Default for OrgnodeViewData {
  fn default () -> Self {
    OrgnodeViewData {
      cycle : false,
      focused : false,
      folded : false,
      relationships : OrgnodeRelationships::default (),
    }} }

impl Default for OrgnodeCode {
  fn default () -> Self {
    OrgnodeCode {
      relToParent : RelToParent::Content,
      indefinitive : false,
      editRequest : None,
      viewRequests : HashSet::new (),
    }} }

pub fn default_metadata () -> OrgnodeMetadata {
  OrgnodeMetadata {
    id : None,
    source : None,
    viewData : OrgnodeViewData::default (),
    code : OrgnodeCode::default (),
  }}

/// Renders OrgnodeMetadata as a metadata string suitable for org-mode display.
/// This is the inverse of parse_metadata_to_orgnodemd.
/// Returns string like "(id abc123) (view ...) (code ...)" etc.
pub fn orgnodemd_to_string (
  metadata : &OrgnodeMetadata
) -> String {
  let mut parts : Vec<String> =
    Vec::new ();
  if let Some ( ref id ) = metadata.id {
    parts.push ( format! ( "(id {})", id.0 )); }
  if let Some ( ref source ) = metadata.source {
    parts.push ( format! ( "(source {})", source )); }

  // Build view s-expr
  let mut view_parts : Vec<String> = Vec::new ();
  if metadata.viewData.cycle {
    view_parts.push ( "cycle".to_string () ); }
  if metadata.viewData.focused {
    view_parts.push ( "focused".to_string () ); }
  if metadata.viewData.folded {
    view_parts.push ( "folded".to_string () ); }

  // Build rels s-expr (only if has non-default values)
  let mut rel_parts : Vec<String> = Vec::new ();
  // Only emit if not default (default is true)
  if ! metadata.viewData.relationships.parentIsContainer {
    rel_parts.push ( "notInParent".to_string () ); }
  // Only emit if not default (default is false)
  if metadata.viewData.relationships.parentIsContent {
    rel_parts.push ( "containsParent".to_string () ); }
  // Only emit if not default (default is Some(1))
  if metadata.viewData.relationships.numContainers != Some ( 1 ) {
    if let Some ( count )
      = metadata.viewData.relationships.numContainers {
        rel_parts.push ( format! ( "(containers {})", count )); }}
  // Only emit if not default (default is Some(0))
  if metadata.viewData.relationships.numContents != Some ( 0 ) {
    if let Some ( count )
      = metadata.viewData.relationships.numContents {
        rel_parts.push ( format! ( "(contents {})", count )); }}
  // Only emit if not default (default is Some(0))
  if metadata.viewData.relationships.numLinksIn != Some ( 0 ) {
    if let Some ( count )
      = metadata.viewData.relationships.numLinksIn {
        rel_parts.push ( format! ( "(linksIn {})", count )); }}

  if ! rel_parts . is_empty () {
    view_parts.push ( format! ( "(rels {})",
                                  rel_parts . join ( " " ))); }

  if ! view_parts . is_empty () {
    parts.push ( format! ( "(view {})",
                             view_parts . join ( " " ))); }

  // Build code s-expr
  let mut code_parts : Vec<String> = Vec::new ();
  if metadata.code.relToParent != RelToParent::Content {
    code_parts.push ( format! (
      "(relToParent {})", metadata.code.relToParent )); }
  if metadata.code.indefinitive {
    code_parts.push ( "indefinitive".to_string () ); }

  // Handle editRequest (toDelete or merge)
  if let Some(ref edit_req) = metadata.code.editRequest {
    code_parts.push ( edit_req . to_string () ); }

  // Build viewRequests s-expr (inside code)
  if ! metadata.code.viewRequests . is_empty () {
    let mut request_strings : Vec<String> =
      metadata.code.viewRequests . iter ()
      . map ( | req | req . to_string () )
      . collect ();
    request_strings . sort (); // Ensure consistent ordering
    code_parts.push ( format! ( "(viewRequests {})",
                                  request_strings . join ( " " ))); }

  if ! code_parts . is_empty () {
    parts.push ( format! ( "(code {})",
                             code_parts . join ( " " ))); }
  parts.join ( " " ) }
