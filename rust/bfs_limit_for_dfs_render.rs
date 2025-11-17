/// Experimentation with BFS-limited DFS rendering.
///
/// This module implements a tree structure with Content and Leaf nodes,
/// and eventually will support rendering with a BFS limit on Content nodes.

use ego_tree::Tree;
use sexp::{Sexp, Atom};
use std::error::Error;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeType {
  Content,
  Leaf,
}

/// Parse an s-expression string into a tree.
///
/// Format:
/// - `(Content)` or `(Leaf)` represents a single node
/// - `(Content Leaf Leaf)` represents a Content node with two Leaf children
/// - `(Content (Content Leaf))` represents a Content with a Content child that has a Leaf child
///
/// Examples:
/// ```
/// use skg::bfs_limit_for_dfs_render::parse_sexp_to_tree;
/// let tree = parse_sexp_to_tree("(Content)").unwrap();
/// assert_eq!(tree.root().value(), &skg::bfs_limit_for_dfs_render::NodeType::Content);
/// ```
pub fn parse_sexp_to_tree(input: &str) -> Result<Tree<NodeType>, Box<dyn Error>> {
  let sexp = sexp::parse(input)?;
  parse_sexp_node(&sexp)
}

/// Parse a single s-expression into a tree.
fn parse_sexp_node(sexp: &Sexp) -> Result<Tree<NodeType>, Box<dyn Error>> {
  match sexp {
    Sexp::Atom(_) => {
      Err("Expected a list, got an atom".into())
    }
    Sexp::List(items) => {
      if items.is_empty() {
        return Err("Empty list is not a valid node".into());
      }

      // First element is the node type
      let node_type = parse_node_type(&items[0])?;

      // Validate: Leaf cannot have children
      if node_type == NodeType::Leaf && items.len() > 1 {
        return Err("Leaf nodes cannot have children".into());
      }

      // Create the tree with the root node
      let mut tree = Tree::new(node_type.clone());
      let root_id = tree.root().id();

      // Add children (if any)
      for child_sexp in &items[1..] {
        add_child_to_parent(&mut tree, root_id, child_sexp)?;
      }

      Ok(tree)
    }
  }
}

/// Add a child (parsed from sexp) to a parent node.
fn add_child_to_parent(
  tree: &mut Tree<NodeType>,
  parent_id: ego_tree::NodeId,
  child_sexp: &Sexp
) -> Result<(), Box<dyn Error>> {
  match child_sexp {
    Sexp::Atom(_) => {
      // Bare atom - create a childless node
      let node_type = parse_node_type(child_sexp)?;
      tree.get_mut(parent_id).unwrap().append(node_type);
      Ok(())
    }
    Sexp::List(items) => {
      if items.is_empty() {
        return Err("Empty list is not a valid node".into());
      }

      // Parse the child node type
      let node_type = parse_node_type(&items[0])?;

      // Validate: Leaf cannot have children
      if node_type == NodeType::Leaf && items.len() > 1 {
        return Err("Leaf nodes cannot have children".into());
      }

      // Append the child node
      let child_id = {
        let mut parent = tree.get_mut(parent_id).unwrap();
        parent.append(node_type).id()
      };

      // Recursively add grandchildren
      for grandchild_sexp in &items[1..] {
        add_child_to_parent(tree, child_id, grandchild_sexp)?;
      }

      Ok(())
    }
  }
}

/// Parse a node type from an s-expression atom.
fn parse_node_type(sexp: &Sexp) -> Result<NodeType, Box<dyn Error>> {
  match sexp {
    Sexp::Atom(Atom::S(s)) => {
      match s.as_str() {
        "Content" => Ok(NodeType::Content),
        "Leaf" => Ok(NodeType::Leaf),
        _ => Err(format!("Unknown node type: {}", s).into()),
      }
    }
    _ => Err("Node type must be a symbol (Content or Leaf)".into()),
  }
}
