use crate::types::{OrgNode, OrgnodeMetadata, RelToOrgParent, ID};

use ego_tree::Tree;
use regex::Regex;

/// Type alias for headline information: (level, metadata, title)
pub type HeadlineInfo = (usize, Option<OrgnodeMetadata>, String);

/// Represents a parsed org node with its headline and body lines
#[derive(Debug, Clone)]
struct OrgNodeLineCol {
  headline: HeadlineInfo,
  body: Vec<String>,
}

pub fn org_to_uninterpreted_nodes(
  input: &str)
  -> Result<Vec<Tree<OrgNode>>, String> {
  let org_node_line_cols: Vec<OrgNodeLineCol> =
    divide_into_orgNodeLineCols(input)?;
  let mut result_trees: Vec<Tree<OrgNode>> = Vec::new();
  let mut current_tree: Option<Tree<OrgNode>> = None;
  let mut node_stack: Vec<ego_tree::NodeId> = Vec::new();
  for org_node_line_col in &org_node_line_cols {
    let (level, node): (usize, OrgNode) =
      mk_orgnode(org_node_line_col)?;
    if level == 1 { // Start a new tree.
      if let Some(tree) = current_tree.take() {
        result_trees.push(tree); }
      current_tree = Some(Tree::new(node));
      node_stack = vec![
        current_tree . as_mut() . unwrap() . root_mut() . id() ];
    } else { // Insert in the current tree.
      if let Some(ref mut tree) = current_tree {
        // Adjust node_stack to proper level
        while node_stack.len() >= level {
          node_stack.pop(); }
        let parent_id: ego_tree::NodeId =
          *node_stack.last().unwrap();
        let mut parent_node: ego_tree::NodeMut<OrgNode> =
          tree.get_mut(parent_id).unwrap();
        let new_node_id: ego_tree::NodeId =
          parent_node.append(node).id();
        node_stack.push(new_node_id);
      } else {
        // No current tree means this nested node is orphaned
        return Err(format!("Invalid org structure: headline at level {} appears without a preceding level 1 headline. The org text jumps too far between levels.", level));
      }} }
  if let Some(tree) = current_tree {
    // Add the last tree if it exists
    result_trees.push(tree); }
  Ok (result_trees) }

/// Parse input text into org node line collections.
/// Each org node consists of a headline and its following body lines.
/// Returns an error if any headline has invalid metadata.
fn divide_into_orgNodeLineCols (
  input: &str
) -> Result<Vec<OrgNodeLineCol>, String> {
  // TODO ? This is quite inefficient.
  // Both headline_to_triple and validate_headline_metadata
  // parse the same string in nearly the same way,
  // for all but the first node, both are run twice.

  let lines: Vec<&str> = input.lines().collect();
  let mut result: Vec<OrgNodeLineCol> = Vec::new();
  let mut i: usize = 0;
  while i < lines.len() {
    validate_headline_metadata( lines[i] )?;
    if let Some(headline_info) = headline_to_triple( lines[i] ) {
      // Found a headline. Now collect its body lines.
      let mut body_lines: Vec<String> = Vec::new();
      i += 1; // Move past the headline
      while i < lines.len() {
        validate_headline_metadata( lines[i] )?;
        if headline_to_triple( lines[i] ) . is_some() {
          break; // Found another headline.
        } else {
          body_lines.push(
            lines[i] . to_string());
          i += 1; }}
      result.push ( OrgNodeLineCol {
        headline: headline_info,
        body: body_lines, } );
    } else {
      // Skip non-headline lines at the beginning of the document
      i += 1; }}
  Ok (result) }

/// Create an OrgNode from an OrgNodeLineCol.
/// This helper extracts the node creation logic from the main parsing function.
/// Returns an error if the metadata cannot be parsed.
fn mk_orgnode(
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
      create_default_headline_metadata () };
  Ok (( level,
        OrgNode {
          metadata,
          title,
          body: body_text, }
  )) }

/// Validate a headline for metadata errors without parsing it.
/// Returns an error if the line looks like a headline but has invalid metadata.
/// Returns Ok(()) if the line is not a headline or has valid metadata.
fn validate_headline_metadata(
  headline: &str
) -> Result<(), String> {
  static HEADLINE_REGEX
    : std::sync::LazyLock<Regex>
    = std::sync::LazyLock::new(|| {
      Regex::new(r"^\s*(\*+)\s+(?:<skg<([^>]*)>>\s*)?(.+)").unwrap()
    });
  if let Some(captures) = HEADLINE_REGEX.captures(headline) {
    if let Some(meta_match) = captures.get(2) {
      // This line has metadata, validate it
      parse_metadata_to_headline_md(
        meta_match.as_str())?; }}
  Ok (( )) }

/// Check if a line is a valid headline and extract level, metadata, and title.
/// Returns Some(HeadlineInfo) if it's a valid headline, None if it's not a headline.
/// A valid headline starts with asterisks, followed by at least one whitespace,
/// followed by at least one non-whitespace character.
/// Invalid metadata will cause this function to return None.
pub fn headline_to_triple (
  headline: &str
) -> Option<HeadlineInfo> {
  // Regex with capture groups:
  // ^\s*(\*+)\s+ - capture asterisks, ditch following whitespace
  // (?:<skg<([^>]*)>>\s*)? - optionally capture metadata content, ditch following whitespace
  // (.+) - capture the rest of the line as title
  static HEADLINE_REGEX
    : std::sync::LazyLock<Regex>
    = std::sync::LazyLock::new(|| {
      Regex::new(r"^\s*(\*+)\s+(?:<skg<([^>]*)>>\s*)?(.+)").unwrap()
    });
  if let Some(captures) = HEADLINE_REGEX.captures(headline) {
    let asterisks: &str = captures.get(1).unwrap().as_str();
    let level: usize = asterisks.len();
    let metadata: Option<OrgnodeMetadata> =
      if let Some(meta_match) = captures.get(2) {
        match parse_metadata_to_headline_md(meta_match.as_str()) {
          Ok(parsed_metadata) => Some(parsed_metadata),
          Err(_) => // Invalid metadata, treat as invalid headline
            return None, }
      } else { None };
    let title: String =
      captures . get(3) . unwrap() . as_str() . to_string();
    Some((level, metadata, title))
  } else { None }}

/// Parse metadata string directly into OrgnodeMetadata.
/// Parses comma-separated metadata items like "id:foo,folded,relToOrgParent:container".
/// Returns an error for unknown metadata keys or values.
fn parse_metadata_to_headline_md(
  metadata_str: &str
) -> Result<OrgnodeMetadata, String> {
  let mut result : OrgnodeMetadata = create_default_headline_metadata();
  for part in metadata_str.split(',') {
    let trimmed: &str = part.trim();
    if trimmed.is_empty() { continue; }
    if let Some((key_str, value_str)) = trimmed.split_once(':') {
      // Handle key:value pairs
      let key: &str = key_str.trim();
      let value: &str = value_str.trim();
      match key {
        "id" => { result.id = Some(ID::from(value)); },
        "relToOrgParent" => {
          result.relToOrgParent = match value {
            "alias"        => RelToOrgParent::Alias,
            "aliasCol"     => RelToOrgParent::AliasCol,
            "container"    => RelToOrgParent::Container,
            "content"      => RelToOrgParent::Content,
            "none"         => RelToOrgParent::None,
            "searchResult" => RelToOrgParent::SearchResult,
            _ => return Err(
              format!("Unknown relToOrgParent value: {}", value)),
          }; },
        _ => {
          return Err(format!("Unknown metadata key: {}", key)); }}
    } else {
      // Handle bare values (boolean flags)
      match trimmed {
        "repeated"         => result.repeat = true,
        "folded"           => result.folded = true,
        "focused"          => result.focused = true,
        "cycle"            => result.cycle = true,
        "mightContainMore" => result.mightContainMore = true,
        "toDelete"         => result.toDelete = true,
        _ => {
          return Err(format!("Unknown metadata value: {}", trimmed));
        }} }}
  Ok(result) }

/// Create default OrgnodeMetadata with all default values.
/// Used when a headline has no metadata.
fn create_default_headline_metadata() -> OrgnodeMetadata {
  OrgnodeMetadata {
    id: None,
    relToOrgParent: RelToOrgParent::Content,
    cycle: false,
    focused: false,
    folded: false,
    mightContainMore: false,
    repeat: false,
    toDelete: false, }}
