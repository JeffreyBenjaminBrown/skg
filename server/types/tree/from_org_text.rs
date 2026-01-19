/// Simple org-mode format parser for general trees (not Skg-specific).
///
/// This parser is for testing tree utilities with ego_tree.
/// It reads simple org-mode headlines and builds a Tree<String>.
///
/// Format:
/// ```text
/// * Root
/// ** Child 1
/// *** Grandchild
/// ** Child 2
/// ```
///
/// Each line becomes a node, with depth determined by the number of asterisks.
/// The title (text after asterisks and spaces) becomes the node's value.

use ego_tree::{Tree, NodeId};
use std::error::Error;
use std::str::Chars;

/// Parse org-mode text into a Tree<String>.
/// Returns a tree where the root is the first line (typically "* Root").
/// Errors if the format is invalid (e.g., skipping levels, no root).
pub fn tree_from_org_text(
  text: &str
) -> Result<Tree<String>, Box<dyn Error>> {
  let lines: Vec<&str> = text.lines()
    .map(|l| l.trim())
    .filter(|l| !l.is_empty())
    .collect();

  if lines.is_empty() {
    return Err("Empty input".into());
  }

  // Parse first line as root
  let (root_level, root_title) : (usize, String) =
    parse_line(lines[0])?;
  if root_level != 1 {
    return Err("First line must be level 1 (single *)".into( )); }

  let mut tree : Tree<String> = Tree::new(root_title);
  let mut stack: Vec<(usize, NodeId)> =
    vec![(1, tree.root().id())];

  // Parse remaining lines
  for line in &lines[1..] {
    let (level, title) : (usize, String) = parse_line(line)?;

    // Pop stack until we find the parent level
    while !stack.is_empty() && stack.last().unwrap().0 >= level {
      stack.pop();
    }

    if stack.is_empty() {
      return Err(format!("Invalid nesting at line: {}", line).into());
    }

    let parent_id : NodeId = stack.last().unwrap().1;
    let new_id : NodeId =
      tree.get_mut(parent_id).unwrap().append(title).id();
    stack.push((level, new_id));
  }

  Ok(tree)
}

/// Parse a single org-mode line.
/// Returns (level, title) where level is the number of asterisks.
fn parse_line(
  line: &str
) -> Result<(usize, String), Box<dyn Error>> {
  let mut level : usize = 0;
  let mut chars : Chars = line.chars();

  // Count leading asterisks
  while let Some(c) = chars.next() {
    if c == '*' {
      level += 1;
    } else if c == ' ' {
      break;
    } else {
      return Err(format!("Invalid org format: {}", line).into());
    }
  }

  if level == 0 {
    return Err(format!("No asterisks found: {}", line).into());
  }

  let title : String = chars.as_str().trim().to_string();
  if title.is_empty() {
    return Err(format!("Empty title: {}", line).into());
  }

  Ok((level, title))
}
