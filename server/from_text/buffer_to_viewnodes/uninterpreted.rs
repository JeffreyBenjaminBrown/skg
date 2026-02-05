/// DANGER: This was vibe-coded without much oversight.
/// I *believe* the tests constrain its behavior sufficiently.

use crate::types::sexp::find_sexp_end;
use crate::types::errors::BufferValidationError;
use crate::serve::parse_metadata_sexp::{ parse_metadata_to_viewnodemd, default_metadata, viewnode_from_metadata, ViewnodeMetadata };
use crate::types::unchecked_viewnode::{UncheckedViewNode, unchecked_forest_root_viewnode};

use ego_tree::{Tree, NodeId, NodeMut};
use regex::Regex;
use std::sync::LazyLock;

/// Type alias for headline information: (level, metadata, title)
pub type HeadlineInfo = (usize, Option<ViewnodeMetadata>, String);

/// Result type for headline parsing with specific error messages
type HeadlineResult = Result<HeadlineInfo, String>;

/// Represents a parsed org node with its headline and body lines
#[derive(Debug, Clone)]
struct ViewNodeLineCol {
  headline: HeadlineInfo,
  body: Vec<String>,
}

/// Parse org text into a "forest": a tree with a BufferRoot.
/// Each level-1 headline becomes a child of the BufferRoot.
/// The result takes the org-buffer at face value,
/// without changing the tree (e.g. without absorbing aliases
/// into the relevant ancestor) and without supplementing information
/// (e.g. missing IDs).
///
/// RETURNS (tree, parsing_errors).
///
/// SHARES RESPONSIBILITY for error detection
/// with 'find_buffer_errors_for_saving', which runs later.
/// That function detects the majority of possible errors,
/// but it can't detect them all because it uses a tree of UncheckedViewNodes,
/// which permit fewer kinds of invalid state than the raw text.
/// (For instance, Alias and AliasCol cannot have bodies,
/// but they can in the raw text, and that's an error.)
pub fn org_to_uninterpreted_nodes(
  input: &str
) -> Result < ( Tree<UncheckedViewNode>, Vec<BufferValidationError> ),
              String > {
  let mut forest: Tree<UncheckedViewNode> = Tree::new(unchecked_forest_root_viewnode());
  let mut parsing_errors: Vec<BufferValidationError> = Vec::new();
  // treeid_stack[0] is the BufferRoot, treeid_stack[1] is the current tree root, etc.
  let mut treeid_stack: Vec<NodeId> = vec![ {
    let forest_root_treeid: NodeId = forest.root().id();
    forest_root_treeid } ];
  for view_node_line_col in & {
    let view_node_line_cols: Vec<ViewNodeLineCol> =
      divide_into_viewNodeLineCols(input)?;
    view_node_line_cols } {
    let (level, viewnode, error_opt)
      : (usize, UncheckedViewNode, Option<BufferValidationError>)
      = linecol_to_viewnode(view_node_line_col)?;
    if let Some ( error ) = error_opt {
      parsing_errors . push ( error ); }
    // Adjust treeid_stack to proper level (BufferRoot is level 0, tree roots are level 1)
    while treeid_stack.len() > level {
      treeid_stack.pop(); }
    // Check for orphaned headlines (e.g., ** without preceding *)
    // treeid_stack.len() should equal level (because BufferRoot is at level 0)
    if treeid_stack.len() < level {
      return Err(format!(
        "Node \"{}\" at level {} jumps too far between levels (no valid parent at level {}).",
        viewnode.title(), level, level - 1)); }
    treeid_stack.push( {
      let parent_treeid: NodeId = *treeid_stack.last().unwrap();
      let new_treeid: NodeId = {
        let mut parent_mut : NodeMut<UncheckedViewNode> =
          forest.get_mut(parent_treeid).unwrap();
        parent_mut.append(viewnode).id() };
      new_treeid } ); }
  Ok ( ( forest, parsing_errors ) ) }

/// Parse input text into org node line collections.
/// Each org node consists of a headline and its following body lines.
/// Returns an error if any headline has invalid metadata.
fn divide_into_viewNodeLineCols (
  input: &str
) -> Result<Vec<ViewNodeLineCol>, String> {
  let lines: Vec<&str> = input.lines().collect();
  let mut result: Vec<ViewNodeLineCol> = Vec::new();
  let mut i: usize = 0;
  while i < lines.len() {
    match headline_to_triple( lines[i] ) {
      Ok(headline_info) => {
        // Found a headline. Now collect its body lines.
        let mut body_lines: Vec<String> = Vec::new();
        i += 1; // Move past the headline
        while i < lines.len() {
          match headline_to_triple( lines[i] ) {
            Ok(_) => break, // Found another headline
            Err(e) if e == "__NOT_A_HEADLINE__" => {
              // Not a headline, it's a body line
              body_lines.push( lines[i] . to_string() );
              i += 1;
            },
            Err(e) => return Err(e), // Invalid metadata
          }
        }
        result.push ( ViewNodeLineCol {
          headline: headline_info,
          body: body_lines, } );
      },
      Err(e) if e == "__NOT_A_HEADLINE__" => {
        // Skip non-headline lines at the beginning of the document
        i += 1;
      },
      Err(e) => return Err(e), // Invalid metadata
    }
  }
  Ok (result) }

/// Create an UncheckedViewNode from an ViewNodeLineCol.
/// This helper extracts the node creation logic from the main parsing function.
/// Returns (level, UncheckedViewNode, Option<BufferValidationError>).
fn linecol_to_viewnode(
  view_node_line_col: &ViewNodeLineCol
) -> Result < ( usize, UncheckedViewNode, Option<BufferValidationError> ),
              String > {
  let (level, metadata_option, title): HeadlineInfo =
    view_node_line_col.headline.clone();
  let body_lines: &[String] = &view_node_line_col.body;
  let body_text: Option<String> =
    if body_lines.is_empty() { None
    } else { Some(body_lines.join("\n")) };
  let metadata : ViewnodeMetadata =
    if let Some(parsed_metadata) = metadata_option {
      parsed_metadata
    } else { // No metadata, so use defaults.
      default_metadata () };
  let ( viewnode, error_opt )
    : ( UncheckedViewNode, Option<BufferValidationError> )
    = viewnode_from_metadata ( &metadata, title, body_text );
  Ok ( ( level, viewnode, error_opt ) ) }

/// Check if a line is a valid headline and extract level, metadata, and title.
/// Returns Ok(HeadlineInfo) if it's a valid headline with valid metadata.
/// Returns Ok with None metadata if it's a valid headline without metadata.
/// Returns Err with specific error message if metadata is invalid.
/// Returns Ok for non-headlines (they are valid non-headline lines).
pub fn headline_to_triple (
  headline: &str
) -> HeadlineResult {
  // Simple regex to capture just asterisks and remainder
  static HEADLINE_REGEX
    : LazyLock<Regex>
    = LazyLock::new(|| {
      Regex::new(r"^\s*(\*+)\s+(.+)").unwrap()
    });
  if let Some(captures) = HEADLINE_REGEX.captures(headline) {
    let asterisks: &str = captures.get(1).unwrap().as_str();
    let level: usize = asterisks.len();
    let remainder: &str = captures.get(2).unwrap().as_str().trim();

    // Check if remainder starts with metadata
    if remainder.starts_with("(skg") {
      // Find the end of the s-expression
      if let Some(sexp_end) = find_sexp_end(remainder) {
        // Extract the s-expression substring
        let sexp_str: &str = &remainder[..sexp_end];

        // Verify it's valid by attempting to parse it
        if let Err(e) = sexp::parse(sexp_str) {
          return Err(format!("Invalid s-expression syntax: {}", e));
        }

        // Parse the metadata from the s-expression
        let metadata: Option<ViewnodeMetadata> =
          match parse_metadata_to_viewnodemd(sexp_str) {
            Ok(parsed_metadata) => Some(parsed_metadata),
            Err(e) => return Err(e), // Invalid metadata with specific error
          };

        // The title is everything after the s-expression
        let title: String =
          remainder[sexp_end..].trim().to_string();

        return Ok((level, metadata, title));
      } else {
        return Err("Unclosed metadata parentheses".to_string());
      }
    }

    // No metadata, just title
    let title: String = remainder.to_string();
    Ok((level, None, title))
  } else {
    // Not a headline - this is OK (it's a valid non-headline line)
    // We use a special error to signal "not a headline"
    // This is a bit of a hack, but maintains backward compatibility
    Err("__NOT_A_HEADLINE__".to_string())
  }}
