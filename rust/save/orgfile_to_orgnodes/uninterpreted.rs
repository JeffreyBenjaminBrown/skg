// Parse org text to 'OrgNodeUninterpreted's.

use crate::types::OrgNodeUninterpreted;
use super::cursor::LineCursor;
use super::cursor::collect_body_lines;
use super::cursor::parse_metadata_plus_headline;
use super::cursor::peek_headline_level;
use super::cursor::skip_until_first_headline;

pub fn parse_skg_org_to_uninterpreted_nodes (
  input : &str
) -> Vec<OrgNodeUninterpreted> {
  // TODO: Test.

  let mut cursor = LineCursor::new (input);
  skip_until_first_headline ( &mut cursor );
  let start_level: usize =
    peek_headline_level ( &cursor )
    . unwrap_or (1); // default to 1 if absent. TODO: Maybe this should throw an error.
  return parse_uninterpreted_nodes_at_level (
    &mut cursor, start_level ); }

/// Parse a *sequence* of sibling uninterpreted nodes at `level`
fn parse_uninterpreted_nodes_at_level (
  cur   : &mut LineCursor,
  level : usize
) -> Vec<OrgNodeUninterpreted> {
  // TODO: This is currently not robust to initial super-indented headlines. In my knowledge graph, see `parsing initial super-indented org-children in org-roam data`.

  let mut nodes_acc : Vec<OrgNodeUninterpreted> =
    Vec::new();
  while let Some(line) = cur.peek () {
    match parse_metadata_plus_headline (line) {
      Some((lvl, _)) if lvl == level => {
        // Expected sibling: parse one full node (header + body + subtree)
        let node: OrgNodeUninterpreted =
          parse_one_uninterpreted_node_at_level
          (cur, level);
        nodes_acc.push (node); },
      Some((lvl, _)) if lvl < level => break,
      _ => { // ignore anything else
        let _ignored: Option<&str> = cur.bump(); }} }
  nodes_acc }

/// Parse exactly one uninterpreted node at `level` at the cursor.
/// ASSUMES the current line is a valid headline of exactly this level.
fn parse_one_uninterpreted_node_at_level (
  cur   : &mut LineCursor,
  level : usize
) -> OrgNodeUninterpreted {

  let input: &str =
    cur . bump() . expect (
      "caller guarantees a headline is present" );
  let (_level, headline_content): (usize, &str) =
    parse_metadata_plus_headline (input)
    . expect("caller guarantees a headline is present");
  OrgNodeUninterpreted {
    headline : headline_content.to_string (),
    body     : collect_body_lines (cur),
    branches : parse_uninterpreted_children ( cur, level ),
  }}

/// Parse children as uninterpreted nodes
fn parse_uninterpreted_children (
  cur          : &mut LineCursor,
  parent_level : usize
) -> Vec<OrgNodeUninterpreted> {

  let child_level : usize =
    parent_level + 1;
  let mut children_acc : Vec<OrgNodeUninterpreted> =
    Vec::new();
  while let Some(next) = cur.peek() {
    match parse_metadata_plus_headline (next) {
      Some ((lvl, _))
        if lvl == child_level => {
          let child: OrgNodeUninterpreted =
            parse_one_uninterpreted_node_at_level ( cur, child_level );
          children_acc.push ( child ); }
      Some((lvl, _))
        if lvl <= parent_level
        => break,
      _ => {
        let _ignored: Option<&str> = cur.bump();
      }} }
  children_acc }
