use crate::types::{OrgNode2, OrgNodeMd2, RelToOrgParent2, ID};

use ego_tree::Tree;
use regex::Regex;

/// Type alias for headline information: (level, metadata, title)
type HeadlineInfo = (usize, Option<String>, String);

/// Represents a parsed org node with its headline and body lines
#[derive(Debug, Clone)]
struct OrgNodeLineCol {
  headline: HeadlineInfo,
  body: Vec<String>,
}

pub fn parse_skg_org_to_uninterpreted_nodes2(
  input: &str)
  -> Result<Vec<Tree<OrgNode2>>, String> {
  let org_node_line_cols: Vec<OrgNodeLineCol> =
    divide_into_orgNodeLineCols(input);
  let mut result_trees: Vec<Tree<OrgNode2>> = Vec::new();
  let mut current_tree: Option<Tree<OrgNode2>> = None;
  let mut node_stack: Vec<ego_tree::NodeId> = Vec::new();
  for org_node_line_col in &org_node_line_cols {
    let (level, node): (usize, OrgNode2) =
      mk_orgnode2(org_node_line_col)?;
    if level == 1 { // Start a new tree.
      if let Some(tree) = current_tree.take() {
        result_trees.push(tree); }
      current_tree = Some(Tree::new(node));
      node_stack = vec![current_tree.as_mut().unwrap().root_mut().id()];
    } else { // Insert in the current tree.
      if let Some(ref mut tree) = current_tree {
        // Adjust node_stack to proper level
        while node_stack.len() >= level {
          node_stack.pop(); }
        let parent_id: ego_tree::NodeId =
          *node_stack.last().unwrap();
        let mut parent_node: ego_tree::NodeMut<OrgNode2> =
          tree.get_mut(parent_id).unwrap();
        let new_node_id: ego_tree::NodeId =
          parent_node.append(node).id();
        node_stack.push(new_node_id);
      } else {
        // No current tree means this nested node is orphaned
        return Err(format!("Invalid org structure: headline at level {} appears without a preceding level 1 headline. The org text jumps too far between levels.", level));
      }
    }
  }

  // Add the last tree if it exists
  if let Some(tree) = current_tree {
    result_trees.push(tree);
  }

  Ok(result_trees)
}

/// Parse input text into org node line collections.
/// Each org node consists of a headline and its following body lines.
/// This makes the parsing logic much cleaner by pre-processing the structure.
fn divide_into_orgNodeLineCols(
  input: &str
) -> Vec<OrgNodeLineCol> {
  let lines: Vec<&str> = input.lines().collect();
  let mut result: Vec<OrgNodeLineCol> = Vec::new();
  let mut i: usize = 0;

  while i < lines.len() {
    if let Some(headline_info) = headline_to_triple(lines[i]) {
      // Found a headline, now collect its body lines
      let mut body_lines: Vec<String> = Vec::new();
      i += 1; // Move past the headline

      // Collect body lines until we hit another headline or end of input
      while i < lines.len() {
        if headline_to_triple(lines[i]).is_some() {
          // Hit another headline, stop collecting body lines
          break;
        } else {
          body_lines.push(lines[i].to_string());
          i += 1;
        }
      }

      // Remove trailing empty lines from body
      while body_lines.last() == Some(&String::new()) {
        body_lines.pop();
      }

      result.push(OrgNodeLineCol {
        headline: headline_info,
        body: body_lines,
      });
    } else {
      // Skip non-headline lines at the beginning of the document
      i += 1;
    }
  }

  result
}

/// Create an OrgNode2 from an OrgNodeLineCol.
/// This helper extracts the node creation logic from the main parsing function.
/// Returns an error if the metadata cannot be parsed.
fn mk_orgnode2(
  org_node_line_col: &OrgNodeLineCol
) -> Result<(usize, OrgNode2), String> {
  let (level, metadata_string, title): HeadlineInfo = org_node_line_col.headline.clone();
  let body_lines: &[String] = &org_node_line_col.body;
  let body_text: Option<String> =
    if body_lines.is_empty() { None
    } else { Some(body_lines.join("\n")) };

  let metadata : OrgNodeMd2 =
    if let Some(meta_str) = metadata_string {
      parse_metadata_to_orgNodeMd2(&meta_str)?
    } else { // No metadata, use defaults
      create_default_orgNodeMd2() };

  // Create the final OrgNode2
  let node: OrgNode2 = OrgNode2 {
    metadata,
    title,
    body: body_text,
  };

  Ok((level, node))
}

/// Check if a line is a valid headline and extract level, metadata, and title.
/// Returns Some(HeadlineInfo) if it's a valid headline, None if it's not a headline.
/// A valid headline starts with asterisks, followed by at least one whitespace,
/// followed by at least one non-whitespace character.
fn headline_to_triple(
  headline: &str
) -> Option<HeadlineInfo> {
  // Regex with capture groups:
  // ^\s*(\*+)\s+ - capture asterisks, ditch following whitespace
  // (?:<skg<([^>]*)>>\s*)? - optionally capture metadata content, ditch following whitespace
  // (.+) - capture the rest of the line as title
  static HEADLINE_REGEX: std::sync::LazyLock<Regex> = std::sync::LazyLock::new(|| {
    Regex::new(r"^\s*(\*+)\s+(?:<skg<([^>]*)>>\s*)?(.+)").unwrap()
  });

  if let Some(captures) = HEADLINE_REGEX.captures(headline) {
    let asterisks: &str = captures.get(1).unwrap().as_str();
    let level: usize = asterisks.len();

    let metadata: Option<String> = captures.get(2).map(|m| m.as_str().to_string());

    let title: String = captures.get(3).unwrap().as_str().to_string();

    Some((level, metadata, title))
  } else {
    None
  }
}

/// Parse metadata string directly into OrgNodeMd2.
/// Parses comma-separated metadata items like "id:foo,folded,relToOrgParent:container".
/// Returns an error for unknown metadata keys or values.
fn parse_metadata_to_orgNodeMd2(
  metadata_str: &str
) -> Result<OrgNodeMd2, String> {
  let mut result : OrgNodeMd2 = create_default_orgNodeMd2();
  for part in metadata_str.split(',') {
    let trimmed: &str = part.trim();
    if trimmed.is_empty() {
      continue;
    }

    // Handle key:value pairs
    if let Some((key_str, value_str)) = trimmed.split_once(':') {
      let key: &str = key_str.trim();
      let value: &str = value_str.trim();

      match key {
        "id" => {
          result.id = Some(ID::from(value));
        },
        "relToOrgParent" => {
          result.relToOrgParent = match value {
            "content" => RelToOrgParent2::Content,
            "container" => RelToOrgParent2::Container,
            "aliasCol" => RelToOrgParent2::AliasCol,
            "alias" => RelToOrgParent2::Alias,
            "none" => RelToOrgParent2::None,
            _ => return Err(format!("Unknown relToOrgParent value: {}", value)),
          };
        },
        _ => {
          return Err(format!("Unknown metadata key: {}", key));
        }
      }
    } else {
      // Handle bare values (boolean flags)
      match trimmed {
        "repeated" => result.repeat = true,
        "folded" => result.folded = true,
        "focused" => result.focused = true,
        "cycle" => result.cycle = true,
        "mightContainMore" => result.mightContainMore = true,
        _ => {
          return Err(format!("Unknown metadata value: {}", trimmed));
        }
      }
    }
  }

  Ok(result)
}

/// Create default OrgNodeMd2 with all default values.
/// Used when a headline has no metadata.
fn create_default_orgNodeMd2() -> OrgNodeMd2 {
  OrgNodeMd2 {
    id: None,
    relToOrgParent: RelToOrgParent2::Content,
    cycle: false,
    focused: false,
    folded: false,
    mightContainMore: false,
    repeat: false,
    to_delete: false,
  }
}
