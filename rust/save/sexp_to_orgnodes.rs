// PURPOSE
// The filename says it all. This code turns an Emacs-style s-exp string representation of a node into an `OrgNode` type.

// HANDY
// cargo test --lib save::sexp_to_orgnodes::tests 2>&1 | tee temp/output.log

use sexp:: {
  Sexp:: { self, Atom, List },
  Atom::S }; // String. Atom also offers I(nt) and F(loat).
use std::collections::HashMap;
use std::vec::Vec;

use crate::types::{ID, OrgNode};

// PITFALL: Mutual recursion between these functions:
//   node_sexp_to_orgnode
//   content_sexps_to_orgnodes

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
  items : Vec<Sexp> ) // the value associated with a 'content' key
  -> Result< Vec<OrgNode>, String > {

  let mut branches : Vec<OrgNode> =
    Vec::new ();
  for (index, item) in items . into_iter() . enumerate() {
    match node_sexp_to_orgnode (item) {
      Ok (branch) => branches.push (branch),
      Err (err) => return Err ( format! (
        "Failed to parse branch at index {}: {}",
        index, err )) }}
  Ok (branches) }

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
