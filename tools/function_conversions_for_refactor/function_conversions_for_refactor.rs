/// PURPOSE: Helps for determining which of
/// a lot of v1/v2 function pairs still has both defined.
/// It populates level-2 headlines in functions.org
/// with their v1 counterparts as level-3 headings.
///
/// HOW:
/// For each function named {name}_[v2/in_orgtree]
/// without children under "TODO still two versions",
/// search for a definition of {name}, and if found add it as a child.

use std::error::Error;
use std::fs;
use std::path::Path;
use walkdir::WalkDir;

#[derive(Debug, Clone)]
struct OrgNode {
  level: usize,
  title: String,
  children: Vec<OrgNode>,
}

fn parse_org_file(content: &str) -> Result<OrgNode, Box<dyn Error>> {
  let lines: Vec<&str> = content.lines().collect();
  let mut root = OrgNode {
    level: 0,
    title: "ROOT".to_string(),
    children: Vec::new(),
  };

  let mut stack: Vec<&mut OrgNode> = vec![];
  // We can't easily use a mutable reference stack in safe Rust,
  // so we'll rebuild the tree from a flat list instead

  #[derive(Debug, Clone)]
  struct FlatNode {
    level: usize,
    title: String,
    parent_idx: Option<usize>,
  }

  let mut flat_nodes: Vec<FlatNode> = vec![FlatNode {
    level: 0,
    title: "ROOT".to_string(),
    parent_idx: None,
  }];

  let mut current_path: Vec<usize> = vec![0]; // Stack of indices

  for line in lines {
    if !line.starts_with('*') {
      continue;
    }

    let level = line.chars().take_while(|&c| c == '*').count();
    let title = line[level..].trim().to_string();

    // Pop stack until we find the right parent
    while current_path.len() > 1 {
      let current_idx = *current_path.last().unwrap();
      if flat_nodes[current_idx].level < level {
        break;
      }
      current_path.pop();
    }

    let parent_idx = *current_path.last().unwrap();
    let new_idx = flat_nodes.len();

    flat_nodes.push(FlatNode {
      level,
      title,
      parent_idx: Some(parent_idx),
    });

    current_path.push(new_idx);
  }

  // Rebuild tree from flat structure
  let mut nodes: Vec<OrgNode> = flat_nodes.iter().map(|fn_| OrgNode {
    level: fn_.level,
    title: fn_.title.clone(),
    children: Vec::new(),
  }).collect();

  // Build children relationships (bottom-up to avoid borrow issues)
  for i in (1..nodes.len()).rev() {
    let parent_idx = flat_nodes[i].parent_idx.unwrap();
    let child = nodes[i].clone();
    nodes[parent_idx].children.push(child);
  }

  // Reverse to get original order
  for node in &mut nodes {
    node.children.reverse();
  }

  Ok(nodes[0].clone())
}

fn find_node_mut<'a>(node: &'a mut OrgNode, title: &str) -> Option<&'a mut OrgNode> {
  if node.title == title {
    return Some(node);
  }
  for child in &mut node.children {
    if let Some(found) = find_node_mut(child, title) {
      return Some(found);
    }
  }
  None
}

fn search_for_function(func_name: &str, server_dir: &Path) -> Option<String> {
  for entry in WalkDir::new(server_dir)
    .follow_links(true)
    .into_iter()
    .filter_map(|e| e.ok())
  {
    let path = entry.path();
    if path.extension().and_then(|s| s.to_str()) == Some("rs")
      && !path.to_str().unwrap().contains("/target/")
      && !path.to_str().unwrap().contains("/abandoned/")
    {
      if let Ok(content) = fs::read_to_string(path) {
        for line in content.lines() {
          // Match "fn <name>" or "async fn <name>"
          if line.contains(&format!("fn {} ", func_name))
            || line.contains(&format!("fn {}(", func_name))
            || line.contains(&format!("fn {}<", func_name))
          {
            return Some(func_name.to_string());
          }
        }
      }
    }
  }
  None
}

fn get_v1_name(v2_name: &str) -> Option<String> {
  if let Some(base) = v2_name.strip_suffix("_v2") {
    Some(base.to_string())
  } else if let Some(base) = v2_name.strip_suffix("_in_orgtree") {
    Some(base.to_string())
  } else {
    None
  }
}

fn write_org_tree(node: &OrgNode, output: &mut String, level: usize) {
  if level > 0 {
    let stars = "*".repeat(level);
    output.push_str(&format!("{} {}\n", stars, node.title));
  }
  for child in &node.children {
    write_org_tree(child, output, level + 1);
  }
}

fn main() -> Result<(), Box<dyn Error>> {
  let project_root = std::env::current_dir()?;
  let functions_org_path = project_root.join("refactor-ongoing/functions.org");
  let server_dir = project_root.join("server");
  let output_dir = project_root.join("tools/function_conversions_for_refactor");

  // Ensure output directory exists
  fs::create_dir_all(&output_dir)?;

  let content = fs::read_to_string(&functions_org_path)?;
  let mut tree = parse_org_file(&content)?;

  // Find "TODO still two versions" node
  let todo_node = tree.children.iter_mut()
    .find(|child| child.title.contains("TODO still two versions"))
    .ok_or("Could not find 'TODO still two versions' node")?;

  println!("Found 'TODO still two versions' with {} children", todo_node.children.len());

  let mut added_count = 0;

  // For each v2/in_orgtree function without children
  for child in &mut todo_node.children {
    let trimmed_title = child.title.trim_start_matches("DONE").trim();

    if child.children.is_empty() {
      if let Some(v1_name) = get_v1_name(trimmed_title) {
        // Search for v1 function
        if let Some(_) = search_for_function(&v1_name, &server_dir) {
          println!("  Adding {} as child of {}", v1_name, trimmed_title);
          child.children.push(OrgNode {
            level: child.level + 1,
            title: v1_name,
            children: Vec::new(),
          });
          added_count += 1;
        }
      }
    }
  }

  println!("\nAdded {} v1 function references", added_count);

  // Write to output directory
  let mut output = String::new();
  write_org_tree(&tree, &mut output, 0);

  let output_file = output_dir.join("functions_updated.org");
  fs::write(&output_file, output)?;
  println!("Updated output written to {}", output_file.display());

  Ok(())
}
