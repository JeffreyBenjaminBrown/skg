// cargo test save::orgfile_to_orgnodes

/* TODO ? Handle text with super-indented initial headings.
(There is an example below.)
.
I'm not sure this would be worth permitting such data.
I'll have to deal with it when ingesting data from, say,
org-roam, where it is valid.
But within skg there's no reason to super-indent.
.
Example of super-indented headings:
  * parent
  *** super-indented child
  ** normal child
or even
  * parent
  ***** super-duper-indented child
  ***** equally super-duper-indented child
  **** super-indented child
  *** normal child for our purposes, because no child of parent has fewer stars
  * parent's brother
*/

// PITFALL: Super-indented initial headings result in themselves and all of their siblings, and their descendents, to be entirely dropped. See "TODO: ... super-indented", above.

// Glossary
// ws      = whitespace
// heading = heading in the Emacs org-mode sense
// body    = body in the Emacs org-mode sense

use crate::types::{ID, OrgNode};

#[allow(unused_imports)]
use indoc::indoc; // For a macro. The unused import checker ignores macro usage; hence the preceding `allow` directive.
use std::collections::{HashMap, HashSet};

/* This function parses a "skg org" document
into a forest of `OrgNode`s.
.
- Lines before the first heading are ignored.
- A heading is `*+ <space>? <content>` where `<content>` is not all whitespace.
- If a heading begins with a metadata block `<<...>>`, it is parsed into (map, set).
  - `id` comes from the map's `"id"` key if present.
  - Each of `repeated`, `folded` and `focused` is true if a bare value of the same name appears in the metadata.
- For a `repeated` node, the body and the *entire subtree* are skipped.
- Body is the consecutive non-heading lines immediately after a heading.
- Children are consecutive headings with level exactly `parent_level + 1`. */
pub fn parse_skg_org_to_nodes(input: &str) -> Vec<OrgNode> {
  let mut cursor = LineCursor::new(input);
  skip_until_first_heading ( &mut cursor );
  let start_level: usize =
    peek_heading_level ( &cursor )
    . unwrap_or (1); // default to 1 if absent
  return parse_nodes_at_level ( &mut cursor, start_level ); }


/* ---------- Cursor ----------*/

#[derive(Debug)]
struct LineCursor<'a> { /// Line iterator with lookahead.
  lines: Vec<&'a str>, // Source text, without trailing newlines.
  idx: usize, // An index into `lines`.
}

impl<'a> LineCursor<'a> {
  fn new ( document: &'a str ) -> Self {
    let lines: Vec<&str> = document . lines() . collect();
    let idx: usize = 0;
    Self { lines, idx }}

  /// Non-consuming look at current line.
  /// Return `None` at EOF.
  fn peek (&self) -> Option<&str> {
    let line_opt: Option<&str> =
      self.lines.get ( self.idx ). copied();
    line_opt }

  /// Consume and return the current line.
  /// Return `None` at EOF.
  fn bump ( &mut self ) -> Option <&'a str> {
    let line: Option<&'a str> =
      self.lines.get ( self.idx ). copied();
    if line.is_some() { // Advance only if next line exists.
      self.idx += 1; }
    line }
}

/* ---------- Headings ----------*/

/// Strips the bullet and leading whitespace,
/// leaving the rest as a single string.
fn parse_metadata_plus_heading ( line: &str
) -> Option<( usize,   // level
              &str)> { // metadata + heading

  // Scan asterisks to count level.
  let mut i: usize = 0; // index into the byte slice
  let bytes: &[u8] = line.as_bytes(); // raw bytes for cheap `*` checks
  while i < bytes.len() && bytes[i] == b'*' { i += 1; }
  if i == 0 { return None; } // no stars => not a heading

  // Allow optional spaces/tabs between stars and content.
  let rest0: &str = &line[i..]; // slice after leading stars
  let rest: &str = rest0.trim_start_matches(
    |c: char|
    c == ' ' || c == '\t' );

  if rest.trim().is_empty() {
    // Not a true heading, just asterisks and whitespace.
    return None; }
  Some ((i, rest)) }

/// Look ahead and return the level of the next heading, if any.
fn peek_heading_level (
  cur : &LineCursor
) -> Option <usize> {
  let level_opt: Option<usize> =
    cur . peek() . and_then (
      |l: &str|
      parse_metadata_plus_heading (l) // TODO: Inefficient. `parse_metadata_plus_heading` ought to be run only once per heading.
        . map ( |(lvl, _)| lvl ));
  level_opt }


/* ---------- Forest/Tree ----------*/

/// Parse a *sequence* of sibling nodes at `level` until
///   (a) a heading with lower level (caller boundary), or
///   (b) EOF.
/// Non-heading lines are ignored.
fn parse_nodes_at_level (
  cur   : &mut LineCursor,
  level : usize
) -> Vec<OrgNode> {

  let mut nodes_acc: Vec<OrgNode> = Vec::new();
  while let Some(line) = cur.peek () {
    match parse_metadata_plus_heading (line) {
      Some((lvl, _)) if lvl == level => {
        // Expected sibling: parse one full node (header + body + subtree)
        let node: OrgNode = parse_one_node_at_level(cur, level);
        nodes_acc.push(node); },
      Some((lvl, _)) if lvl < level => break,
      _ => { // ignore anything else
        let _ignored: Option<&str> = cur.bump(); }}}
  nodes_acc }

/// Parse exactly one node at `level` at the cursor.
/// ASSUMES the current line is a valid heading of exactly this level.
fn parse_one_node_at_level (
  cur   : &mut LineCursor,
  level : usize
) -> OrgNode {

  let heading_line: &str =
    cur . bump() . expect (
      "caller guarantees a heading is present" );
  let (id_opt, is_repeated, is_folded, is_focused, title)
    : ( Option<ID>, bool, bool, bool, String )
    = parse_separating_metadata_and_title ( heading_line );
  if is_repeated { // PITFALL: Ignore its text and children, but don't ignore it entirely. It must be recorded as its parent's child.
    skip_subtree ( cur, level );
    return OrgNode { id       : id_opt,
                     heading  : title,
                     body     : None,
                     folded   : is_folded,
                     focused  : is_focused,
                     repeated : true,
                     branches : Vec::new(), }; }
  OrgNode { id       : id_opt,
            heading  : title,
            body     : collect_body_lines(cur),
            folded   : is_folded,
            focused  : is_focused,
            repeated : false,
            branches : parse_children(cur, level),
  }}

/// Children are headings with level exactly `parent_level + 1`,
/// contiguous in the document.
/// Non-heading lines between siblings are ignored.
fn parse_children (
  cur          : &mut LineCursor,
  parent_level : usize
) -> Vec<OrgNode> {

  let child_level : usize =
    parent_level + 1;
  let mut children_acc : Vec<OrgNode> =
    Vec::new();
  while let Some(next) = cur.peek() {
    match parse_metadata_plus_heading (next) {
      Some ((lvl, _))
        if lvl == child_level => {
          let child: OrgNode =
            parse_one_node_at_level ( cur, child_level );
          children_acc.push ( child ); }
      Some((lvl, _))
        if lvl <= parent_level
        => break,
      _ => {
        let _ignored: Option<&str> = cur.bump();
      }} }
  children_acc }

/// Skip to next heading of level ≤ `parent_level`.
/// Used for `repeated` nodes to discard their body and subtree.
fn skip_subtree ( cur: &mut LineCursor,
                  parent_level: usize ) {
  while let Some (line) = cur.peek () {
    match parse_metadata_plus_heading (line) {
      Some((lvl, _))
        if lvl <= parent_level
        => break,
      _ => { let _ignored: Option<&str> = cur.bump();
      }} }}


/* ---------- Heading parsing ----------*/

/// Parse the *heading line* into `(id, repeated, folded, focused, title)`.
///
/// Steps:
/// 1) Confirm the line is a heading and isolate the post-marker content.
/// 2) If a leading `<<...>>` block exists, parse as metadata (order-agnostic).
/// 3) Remaining text (after `>>`) is the trimmed title.
/// 4) If no metadata block, the whole remainder is the trimmed title.
fn parse_separating_metadata_and_title (
  line: &str
) -> (Option<ID>, bool, bool, bool, String) {

  let parsed: Option<(usize, &str)> =
    parse_metadata_plus_heading (line);
  let (_level, after): (usize, &str) =
    parsed . expect ("Caller verified heading.");
  let after_ws: &str = after.trim_start();

  if let Some(meta_start) = after_ws.strip_prefix("<<") {
    if let Some(end) = meta_start.find(">>") {
      let inner: &str = &meta_start[..end]; // betweeen "<<" and ">>"
      let (kv, bare): (HashMap<String, String>, HashSet<String>) =
        parse_metadata_block (inner);
      let id_opt: Option<ID> = kv.get("id").map(|s| s.into());
      let repeated : bool = bare.contains("repeated");
      let folded   : bool = bare.contains("folded");
      let focused  : bool = bare.contains("focused");
      let title_rest: &str = &meta_start[end + 2..]; // skip ">>"
      let title: String = title_rest.trim().to_string();
      return (id_opt, repeated, folded, focused, title);
    }
    // If "<<" with no matching ">>", fall through to default case, treating it as a regular title.
  }
  { // Default case: no (well-formed) metadata block
    let title: String = after.trim().to_string();
    (None, false, false, false, title) }}


/* ---------- Metadata parser ----------*/

/// Parse the content inside a `<< ... >>` metadata block.
/// Each token is either a key-value pair or a bare value.
/// Tokens are separated by commas.
/// Keys and values are separated by the first colon.
/// Whitespace is stripped.
fn parse_metadata_block (
  inner: &str
) -> (HashMap<String, String>, HashSet<String>) {

  let mut map: HashMap<String, String> =
    HashMap::new();
  let mut set: HashSet<String> =
    HashSet::new();
  for raw in inner.split(',') {
    let tok: &str = raw.trim();
    if tok.is_empty() { continue }
    if let Some(colon) = tok.find(':') {
      // Split key from value on the first colon
      let split          : (&str, &str) = tok.split_at(colon);
      let key_raw        : &str = split.0;
      let val_with_colon : &str = split.1;
      let key            : &str = key_raw.trim();
      let value          : &str = // drop leading ':'
        val_with_colon[1..].trim();
      if !key.is_empty() && !value.is_empty() {
        let k: String = key.to_string();
        let v: String = value.to_string();
        map.insert(k, v); }
    } else { let v: String = tok.to_string();
             set.insert(v);
    }}
  (map, set) }


/* ---------- Bodies & Preamble ----------*/

fn collect_body_lines (
  cur: &mut LineCursor
) -> Option<String> {
  let mut acc: Vec<&str> = Vec::new();
  while let Some (line) = cur.peek () {
    if parse_metadata_plus_heading (line) . is_some () {
      break; } // body ends, next heading starts
    let taken: &str = cur . bump() . unwrap();
    acc.push (taken); }
  if acc.is_empty() { None
  } else {
    let joined: String = // preserve original line breaks
      acc.join("\n");
    Some(joined) }}

fn skip_until_first_heading (
  cur: &mut LineCursor
) {

  while let Some(line) = cur.peek() {
    if parse_metadata_plus_heading(line).is_some() {
      break; }
    let _ignored: Option<&str> = cur.bump();
  }}


/* ----------Tests (optional) ----------*/

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn parses_sample() {
    let sample: &str = indoc! { r#"
  * <<id:1>> 1
  ** <<id:2>> 2, the first child of node 1, which has a body.
  The body of 2.
  *** <<id:3>> 3, the child of node 2, with no body.
  ** <<id:4>> 4, the second child of node 1, with no body.
  ** <<id:1,repeated>> 1
  The body of a repeated node is just a warning that it was repeated. We can ignore it.
  *** <<id:5>> Since this is a child of a repeated node, it is ignored!
  ** A heading with no id, the fourth child of 1.
"# };

    let forest: Vec<OrgNode> = parse_skg_org_to_nodes(sample);
    assert_eq!(forest.len(), 1);

    let n1: &OrgNode = &forest[0];
    assert_eq!(n1.id.as_deref(), Some(&"1".to_string()));
    assert_eq!(n1.heading, "1");
    assert_eq!(n1.branches.len(), 4);

    let n2: &OrgNode = &n1.branches[0];
    assert_eq!(n2.id.as_deref(), Some(&"2".to_string()));
    assert!(n2.body.as_ref().unwrap().contains("The body of 2."));
    assert_eq!(n2.branches.len(), 1);
    assert_eq!(n2.branches[0].id.as_deref(), Some(&"3".to_string()));

    let n4: &OrgNode = &n1.branches[1];
    assert_eq!(n4.id.as_deref(), Some(&"4".to_string()));
    assert!(n4.body.is_none());
    assert!(n4.branches.is_empty());

    let repeated: &OrgNode = &n1.branches[2];
    assert_eq!(repeated.id.as_deref(), Some(&"1".to_string()));
    assert!(repeated.repeated);
    assert!(repeated.body.is_none());
    assert!(repeated.branches.is_empty());

    let n_no_id: &OrgNode = &n1.branches[3];
    assert_eq!(n_no_id.id, None);
    assert_eq!(n_no_id.heading,
               "A heading with no id, the fourth child of 1.");
  }

  #[test]
  fn metadata_parser_examples() {
    let (m1, s1): (HashMap<String, String>, HashSet<String>) = parse_metadata_block("id:1, repeated");
    assert_eq!(m1.get("id").map(String::as_str), Some("1"));
    assert!(s1.contains("repeated"));

    let (m2, s2): (HashMap<String, String>, HashSet<String>) = parse_metadata_block("repeated:true, id:abc123");
    assert_eq!(m2.get("id").map(String::as_str), Some("abc123"));
    assert!(m2.contains_key("repeated"));
    assert!(s2.is_empty());

    let (m3, s3): (HashMap<String, String>, HashSet<String>) = parse_metadata_block("foo:bar, baz , qux: zip ");
    assert_eq!(m3.get("foo").map(String::as_str), Some("bar"));
    assert_eq!(m3.get("qux").map(String::as_str), Some("zip"));
    assert!(s3.contains("baz"));
  }

  #[test]
  fn parses_folded_and_focused() {
    let sample: &str = indoc! { r#"
  * <<id:1, folded>> Node 1 - folded
  ** <<id:2, focused>> Node 2 - focused
  *** <<id:3, folded, focused>> Node 3 - both folded and focused
  ** <<id:4>> Node 4 - neither
"# };

    let forest: Vec<OrgNode> = parse_skg_org_to_nodes(sample);
    assert_eq!(forest.len(), 1);

    let n1: &OrgNode = &forest[0];
    assert_eq!(n1.id.as_deref(), Some(&"1".to_string()));
    assert!(n1.folded);
    assert!(!n1.focused);

    let n2: &OrgNode = &n1.branches[0];
    assert_eq!(n2.id.as_deref(), Some(&"2".to_string()));
    assert!(!n2.folded);
    assert!(n2.focused);

    let n3: &OrgNode = &n2.branches[0];
    assert_eq!(n3.id.as_deref(), Some(&"3".to_string()));
    assert!(n3.folded);
    assert!(n3.focused);

    let n4: &OrgNode = &n1.branches[1];
    assert_eq!(n4.id.as_deref(), Some(&"4".to_string()));
    assert!(!n4.folded);
    assert!(!n4.focused);
  }
}
