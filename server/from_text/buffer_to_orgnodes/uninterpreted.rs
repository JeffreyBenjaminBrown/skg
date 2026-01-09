/// DANGER: This was vibe-coded without much oversight.
/// I *believe* the tests constrain its behavior sufficiently.

use crate::types::sexp::find_sexp_end;
use crate::serve::parse_metadata_sexp::parse_metadata_to_orgnodemd;
use crate::types::orgnode::{default_metadata, OrgnodeMetadata};
use crate::types::orgnode_new::{from_parsed, forest_root_orgnode, OrgNode};

use ego_tree::{Tree, NodeId};
use regex::Regex;
use std::sync::LazyLock;

/// Type alias for headline information: (level, metadata, title)
pub type HeadlineInfo = (usize, Option<OrgnodeMetadata>, String);

/// Result type for headline parsing with specific error messages
type HeadlineResult = Result<HeadlineInfo, String>;

/// Represents a parsed org node with its headline and body lines
#[derive(Debug, Clone)]
struct OrgNodeLineCol {
  headline: HeadlineInfo,
  body: Vec<String>,
}

/// Parse org text into a "forest": a tree with a ForestRoot.
/// Each level-1 headline becomes a child of the ForestRoot.
pub fn org_to_uninterpreted_nodes(
  input: &str
) -> Result<Tree<OrgNode>, String> {
  let mut forest: Tree<OrgNode> = Tree::new(forest_root_orgnode());
  // treeid_stack[0] is the ForestRoot, treeid_stack[1] is the current tree root, etc.
  let mut treeid_stack: Vec<NodeId> = vec![ {
    let forest_root_treeid: NodeId = forest.root().id();
    forest_root_treeid } ];
  for org_node_line_col in & {
    let org_node_line_cols: Vec<OrgNodeLineCol> =
      divide_into_orgNodeLineCols(input)?;
    org_node_line_cols } {
    let (level, orgnode): (usize, OrgNode) =
      linecol_to_neworgnode(org_node_line_col)?;
    // Adjust treeid_stack to proper level (ForestRoot is level 0, tree roots are level 1)
    while treeid_stack.len() > level {
      treeid_stack.pop(); }
    // Check for orphaned headlines (e.g., ** without preceding *)
    // treeid_stack.len() should equal level (because ForestRoot is at level 0)
    if treeid_stack.len() < level {
      return Err(format!(
        "Node \"{}\" at level {} jumps too far between levels (no valid parent at level {}).",
        orgnode.title(), level, level - 1)); }
    treeid_stack.push( {
      let parent_treeid: NodeId = *treeid_stack.last().unwrap();
      let new_treeid: NodeId = {
        let mut parent_mut = forest.get_mut(parent_treeid).unwrap();
        parent_mut.append(orgnode).id() };
      new_treeid } ); }
  Ok(forest) }

/// Parse input text into org node line collections.
/// Each org node consists of a headline and its following body lines.
/// Returns an error if any headline has invalid metadata.
fn divide_into_orgNodeLineCols (
  input: &str
) -> Result<Vec<OrgNodeLineCol>, String> {
  let lines: Vec<&str> = input.lines().collect();
  let mut result: Vec<OrgNodeLineCol> = Vec::new();
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
        result.push ( OrgNodeLineCol {
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

/// Create a OrgNode from an OrgNodeLineCol.
/// This helper extracts the node creation logic from the main parsing function.
/// Returns an error if the metadata cannot be parsed.
fn linecol_to_neworgnode(
  org_node_line_col: &OrgNodeLineCol
) -> Result<(usize, OrgNode), String> {
  let (level, metadata_option, title): HeadlineInfo =
    org_node_line_col.headline.clone();
  let body_lines: &[String] = &org_node_line_col.body;
  let body_text: Option<String> =
    if body_lines.is_empty() { None
    } else { Some(body_lines.join("\n")) };
  let metadata : OrgnodeMetadata =
    if let Some(parsed_metadata) = metadata_option {
      parsed_metadata
    } else { // No metadata, so use defaults.
      default_metadata () };
  Ok (( level, from_parsed ( &metadata, title, body_text ) )) }

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
        let metadata: Option<OrgnodeMetadata> =
          match parse_metadata_to_orgnodemd(sexp_str) {
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
