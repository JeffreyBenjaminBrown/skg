// PURPOSE
// The filename says it all. This code turns an Emacs-style s-exp string representation of a node into an `OrgNode` type.

// HANDY
// cargo test --lib save::sexp_to_orgnodes::tests 2>&1 | tee temp/output.log

use sexp:: { Sexp:: { self, Atom, List },
             Atom::S, // String. Atom also offers I(nt) and F(loat).
             parse };
use std::collections::HashMap;
use std::vec::Vec;

use crate::types::{ID, OrgNode};

pub fn node_sexp_to_orgnode (
  sexp: Sexp )
  -> Result <OrgNode, String> {

  if let List (items) = sexp { // an Sexp is either a List or an Atom
    let mut props = HashMap::new ();
    for item in items {
      // Defines `props`. Later references to it are reads.
      if let List(_) = item {
        let (key, value) =
          pair_sexp_to_string_pair (&item) ?;
        props.insert (key, value ); }}
    let heading : String =
      match props.get ("heading") {
        Some ( Atom ( S (h) )) => h.clone (),
        _ => return Err(
          "Missing or invalid heading".to_string () ), };
    let id : Option<ID> =
      match props.get("id") {
        Some ( Atom ( S (id_str) ))
          => Some ( ID::new (id_str) ),
        _ => None, };
    let body : Option<String> =
      match props.get("body") {
        Some ( Atom ( S (b) )) =>
          Some ( b.clone () ),
        _ => None, };
    let folded : bool = // value not needed
      props.contains_key ("folded");
    let focused : bool = // value not needed
      props.contains_key ("focused");
    let repeated : bool = // value not needed
      props.contains_key ("repeated");
    let branches : Vec<OrgNode> =
      match props.get ("content") {
        Some ( List (content_items) ) => {
          let content_vec = content_items.clone ();
          content_sexps_to_orgnodes ( content_vec )? },
        _ => Vec::new (), }; // no children
    Ok ( OrgNode { id       : id,
                   heading  : heading,
                   body     : body,
                   folded   : folded,
                   focused  : focused,
                   repeated : repeated,
                   branches : branches, } ) }
  else { Err (
    "Branch must be a list".to_string () ) }}

pub fn content_sexps_to_orgnodes (
  items: Vec<Sexp>)
  -> Result<Vec<OrgNode>, String> {

  let mut branches = Vec::new();
  for (index, item) in items.into_iter().enumerate() {
    match node_sexp_to_orgnode(item) {
      Ok(branch) => branches.push(branch),
      Err(err) => return Err(format!(
        "Failed to parse branch at index {}: {}",
        index, err)) } }
  Ok(branches) }

pub fn content_sexp_to_orgnodes (
  sexp_str : &str ) // One element of the list associated with a `content` key in an orgnode sexp.
  -> Result < Vec<OrgNode>, String > {

  let sexp : Sexp = parse (sexp_str)
    .map_err( |e| format!(
      "Failed to parse s-expression: {}", e))?;
  if let List (items) = sexp {
    if items.len() >= 1 {
      if let Atom(S(atom_str)) = &items[0] {
        if atom_str == "content" {
          let content_items = items.into_iter()
            .skip(1) // skip the 'content' atom
            .collect::<Vec<_>>();
          return content_sexps_to_orgnodes(
            content_items); } } }
    return Err(
      // PITFALL: Don't wrap this in an `else` branch,
      // because that way it would not catch failures
      // of the nested conditions inside the first `if`.
      "Root element is not a 'content' list"
        .to_string()); }
  else { return Err(
    "Could not parse input as an (s-expression) list."
      .to_string()) } }

fn pair_sexp_to_string_pair (
  item: &Sexp )
  -> Result < ( String, // key
                Sexp ), // value
              String > {

  if let List (pair) = item {
    // TODO/AWAITS: If only one of the branches below fires,
    // determine which, and delete the other.
    // (Awaits until I'm getting real data from Emacs.)

    if ( pair.len () == 3 &&
         matches! ( &pair[1],
                     Atom ( S(s) ) if s == "." )) {
      // Case 1: Three elements,
      // the middle one a dot: (key . value)
      if let Atom ( S ( key )) = &pair [0] {
        return Ok (( key.clone(),
                     pair[2].clone () )); }
    } else if pair.len() == 2 {
      // Case 2: Two elements (key value)
      // Might be needed by the `content` field.
      if let Atom ( S ( key )) = &pair [0] {
        return Ok (( key.clone(),
                     pair[1].clone() )); } }
    return Err(format!(
      "Malformed property pair: {:?}", item));
  } else { return Err(
    "Property pair must be a list".to_string()); } }

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_parse_simple_sexp() {
    let input = r#"
(content ( (id . "5")
           (heading . "this node is an island")
           (focused . t)))"#;
    let result = content_sexp_to_orgnodes(input);
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
    assert!( ! branch.repeated);
    assert!(branch.branches.is_empty()); }

  #[test]
  fn test_parse_recursive_sexp() {
    let input = r#"
(content
 ( (id . "1")
   (heading . "a top-level title")
   (body . "This one string could span pages,
but in fact only spans two lines.")
   (content
    ( ( (id . "2")
        (heading . "a second-level title")
        (focused . t)
        (body . "More text here.")
        (content
         ( ( (id . "4")
             (heading . "a third-level title, with no body")))))
      ( (id . "3")
        (heading . "another second-level heading")
        (body . "This one string could span pages,
and in fact
spans three lines."))))))"#;

    let result = content_sexp_to_orgnodes(input);
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
    assert!( ! top_branch.focused);
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
    assert!( ! grandchild.focused);
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
    assert!( ! child2.focused);
    assert!(child2.branches.is_empty()); }

  #[test]
  fn test_pair_sexp_to_string_pair() {
    fn s(value: &str) -> Sexp {
      Atom(S(value.to_string())) }
    fn l(items: Vec<Sexp>) -> Sexp {
      List(items) }

    // (key value) - should pass
    let pair1 = l(vec![s("key"), s("value")]);
    let result1 = pair_sexp_to_string_pair(&pair1);
    assert!(result1.is_ok());
    let (key1, value1) = result1.unwrap();
    assert_eq!(key1, "key");
    assert_eq!(value1, s("value"));

    // (key (list)) - should pass
    let pair2 = l(vec![ s("key"),
                        l(vec![s("item1"),
                               s("item2") ]) ]);
    let result2 = pair_sexp_to_string_pair(&pair2);
    assert!(result2.is_ok());
    let (key2, value2) = result2.unwrap();
    assert_eq!(key2, "key");
    assert_eq!(value2, l(vec![s("item1"),
                              s("item2")]));

    // (key . value) - should pass
    let pair3 = l(vec![s("key"),
                       s("."),
                       s("value")]);
    let result3 = pair_sexp_to_string_pair(&pair3);
    assert!(result3.is_ok());
    let (key3, value3) = result3.unwrap();
    assert_eq!(key3, "key");
    assert_eq!(value3, s("value"));

    // (key . (list)) - should pass
    let pair4 = l(vec![ s("key"),
                        s("."),
                        l(vec![s("item1"),
                               s("item2") ]) ]);
    let result4 = pair_sexp_to_string_pair(&pair4);
    assert!(result4.is_ok());
    let (key4, value4) = result4.unwrap();
    assert_eq!(key4, "key");
    assert_eq!(value4, l(vec![s("item1"), s("item2")]));

    // (key value value) - should fail
    let pair5 = l(vec![s("key"),
                       s("value1"),
                       s("value2")]);
    let result5 = pair_sexp_to_string_pair(&pair5);
    assert!(result5.is_err()); } }
