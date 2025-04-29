// cargo test --lib sexp_to_vector::tests  2>&1 | tee output.log

use sexp::{Sexp::{self, Atom, List},
           Atom::S,
           parse};
use std::collections::HashMap;
use std::vec::Vec;

use crate::types::ID;

#[derive(Debug, Clone)]
struct OrgBranch {
  id       : Option<ID>,
  heading  : String,
  body     : Option<String>,
  focused  : bool,
  branches : Vec<OrgBranch>, }

fn parse_sexp_to_branches(
  sexp_str: &str)
  -> Result<Vec<OrgBranch>, String> {
  let sexp = parse(sexp_str)
    .map_err( |e| format!(
      "Failed to parse s-expression: {}", e))?;
  if let List(items) = sexp {
    if items.len() >= 1 {
      if let Atom(S(atom_str)) = &items[0] {
        if atom_str == "content" {
          let content_items = items.into_iter()
            .skip(1) // skip the 'content' atom
            .collect::<Vec<_>>();
          return parse_branches(content_items); } } }
    return Err(
      // PITFALL: Don't wrap this in an `else` branch,
      // because that way it would not catch failures
      // of the nested conditions inside the first `if`.
      "Root element is not a 'content' list"
        .to_string()); }
  else { return Err(
    "Could not parse input as an (s-expression) list."
      .to_string()) } }

fn parse_branches(
  items: Vec<Sexp>)
  -> Result<Vec<OrgBranch>, String> {
  let mut branches = Vec::new();
  for item in items {
    if let Ok(branch) = parse_branch(item) {
      branches.push(branch); }
    else {
      return Err("Failed to parse branch".to_string()); } }
  Ok(branches) }

fn parse_branch(
  sexp: Sexp)
  -> Result<OrgBranch, String> {
  if let List(items) = sexp {
    let mut props = HashMap::new();
    for item in items {
      if let List(pair) = item {
        if pair.len() == 3 &&
          matches!(&pair[1], Atom(S(s)) if s == ".") {
            // Case 1: Three elements, the middle one a dot.
            if let Atom(S(key)) = &pair[0] {
              props.insert(key.clone(),
                           pair[2].clone()); } }
        else if pair.len() == 2 {
          // Case 2: Two elements (key value).
          // Might be needed by the `content` field.
          // TODO: If only one of these branches fires,
          // determine which, and delete the other.
          if let Atom(S(key)) = &pair[0] {
            props.insert(key.clone(), pair[1].clone()); } }
        else { return Err(format!(
          "Malformed property pair: {:?}", pair)); } } }

    let heading = match props.get("heading") {
      Some(Atom(S(h))) => h.clone(),
      _ => return Err(
        "Missing or invalid heading".to_string() ), };

    let id = match props.get("id") {
      Some(Atom(S(id_str)))
        => Some(ID::new(id_str)),
      _ => None, };

    let body = match props.get("body") {
      Some(Atom(S(b))) => Some(b.clone() ),
      _ => None, };

    let focused = // value not needed
      props.contains_key("focused");

    // Extract and parse content (child branches)
    let branches = match props.get("content") {
      Some(List(content_items)) => {
        let content_vec = content_items.clone();
        parse_branches(content_vec)? },
      _ => Vec::new(), }; // No children

    Ok(OrgBranch {
      id,
      heading,
      body,
      focused,
      branches, } ) }
  else {
    Err("Branch must be a list".to_string()) } }

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_parse_simple_sexp() {
    let input = r#"
(content ( (id . "5")
           (heading . "this node is an island")
           (focused . t)))"#;
    let result = parse_sexp_to_branches(input);
    assert!(result.is_ok(),
            "Parse failed, got {:?}", result);
    let branches = result.unwrap();
    assert_eq!(branches.len(), 1,
               "Expected exactly one branch");
    let branch = &branches[0];
    assert_eq!(branch.id.as_ref().map(|id| id.as_str()),
               Some("5"));
    assert_eq!(branch.heading,
               "this node is an island");
    assert_eq!(branch.body,
               None);
    assert!(branch.focused);
    assert!(branch.branches.is_empty()); } }

#[test]
fn test_parse_recursive_sexp() {
  let input = r#"
(content
 ( (id . "1")
   (heading . "a top-level title")
   (body . "This one string could span pages,
but in fact only spans two lines.")
   (content
    ( (id . "2")
      (heading . "a second-level title")
      (focused . t)
      (body . "More text here.")
      (content
       ( (id . "4")
         (heading . "a third-level title, with no body"))))
    ( (id . "3")
      (heading . "another second-level heading")
      (body . "This one string could span pages,
and in fact
spans three lines.")))))"#;

  let result = parse_sexp_to_branches(input);
  assert!(result.is_ok(),
          "Parse failed, got {:?}", result);

  let branches = result.unwrap();
  assert_eq!(branches.len(), 1,
             "Expected exactly one top-level branch");

  // Check the top-level branch (id: "1")
  let top_branch = &branches[0];
  assert_eq!(top_branch.id.as_ref().map(|id| id.as_str()),
             Some("1"));
  assert_eq!(top_branch.heading, "a top-level title");
  assert_eq!(top_branch.body,
             Some("This one string could span pages,
but in fact only spans two lines.".to_string()));
  assert!(!top_branch.focused);
  assert_eq!(top_branch.branches.len(), 2,
             "Top branch should have 2 children");

  // Check first child (id: "2")
  let child1 = &top_branch.branches[0];
  assert_eq!(child1.id.as_ref().map(|id| id.as_str()),
             Some("2"));
  assert_eq!(child1.heading,
             "a second-level title");
  assert_eq!(child1.body,
             Some("More text here.".to_string()));
  assert!(child1.focused);
  assert_eq!(child1.branches.len(), 1,
             "First child should have 1 grandchild");

  // Check grandchild (id: "4")
  let grandchild = &child1.branches[0];
  assert_eq!(grandchild.id.as_ref().map(|id| id.as_str()),
             Some("4"));
  assert_eq!(grandchild.heading,
             "a third-level title, with no body");
  assert_eq!(grandchild.body, None);
  assert!(!grandchild.focused);
  assert!(grandchild.branches.is_empty());

  // Check second child (id: "3")
  let child2 = &top_branch.branches[1];
  assert_eq!(child2.id.as_ref().map(|id| id.as_str()),
             Some("3"));
  assert_eq!(child2.heading,
             "another second-level heading");
  assert_eq!(child2.body,
             Some("This one string could span pages,
and in fact
spans three lines.".to_string()));
  assert!(!child2.focused);
  assert!(child2.branches.is_empty()); }
