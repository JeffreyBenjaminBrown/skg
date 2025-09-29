// Parse org text to 'OrgNode's.

use crate::types::{OrgNode, MetadataItem, OrgNodeMetadata};
use super::cursor::LineCursor;
use super::cursor::collect_body_lines;
use super::cursor::parse_metadata_plus_headline;
use super::cursor::peek_headline_level;
use super::cursor::skip_until_first_headline;
use super::cursor::count_headline_level;
use super::interpreted::parse_separating_metadata_and_title;

use sexp::{Atom, Sexp};

pub fn org_to_uninterpreted_nodes (
  input : &str
) -> Vec<OrgNode> {
  let mut cursor = LineCursor::new (input);
  skip_until_first_headline ( &mut cursor );
  let start_level: usize =
    peek_headline_level ( &cursor )
    . unwrap_or (
      // Default to 1 if absent.
      // TODO ? Maybe throw an error instead.
      1 );
  return parse_uninterpreted_nodes_at_level (
    &mut cursor, start_level ); }

/// Parse a *sequence* of sibling uninterpreted nodes at `level`
fn parse_uninterpreted_nodes_at_level (
  cur   : &mut LineCursor,
  level : usize
) -> Vec<OrgNode> {
  // TODO: This is currently not robust to initial super-indented headlines. In my knowledge graph, see `parsing initial super-indented org-children in org-roam data`.

  let mut nodes_acc : Vec<OrgNode> =
    Vec::new();
  while let Some(line) = cur.peek () {
    match count_headline_level (line) {
      Some(lvl) if lvl == level => {
        // Expected sibling: parse one full node (header + body + subtree)
        let node: OrgNode =
          parse_one_uninterpreted_node_at_level
          (cur, level);
        nodes_acc.push (node); },
      Some(lvl) if lvl < level => break,
      _ => { // ignore anything else
        let _ignored: Option<&str> = cur.bump(); }} }
  nodes_acc }

/// Parse exactly one uninterpreted node at `level` at the cursor.
/// ASSUMES the current line is a valid headline of exactly this level.
fn parse_one_uninterpreted_node_at_level (
  cur   : &mut LineCursor,
  level : usize
) -> OrgNode {

  let input: &str =
    cur . bump() . expect (
      "caller guarantees a headline is present" );
  let (_level, title): (usize, &str) =
    parse_metadata_plus_headline (input)
    . expect("caller guarantees a headline is present");
  OrgNode {
    title    : title.to_string (),
    body     : collect_body_lines (cur),
    branches : parse_uninterpreted_children ( cur, level ),
  }}

/// Parse children as uninterpreted nodes
fn parse_uninterpreted_children (
  cur          : &mut LineCursor,
  parent_level : usize
) -> Vec<OrgNode> {

  let child_level : usize =
    parent_level + 1;
  let mut children_acc : Vec<OrgNode> =
    Vec::new();
  while let Some(next) = cur.peek() {
    match count_headline_level (next) {
      Some (lvl)
        if lvl == child_level => {
          let child: OrgNode =
            parse_one_uninterpreted_node_at_level (
              cur, child_level );
          children_acc.push ( child ); }
      Some(lvl)
        if lvl <= parent_level
        => break,
      _ => {
        let _ignored: Option<&str> = cur.bump();
      }} }
  children_acc }

/// Parse headline from any S-expression containing a headline field
/// Expected format: (.. (headline . "HEADLINE") ..)
/// Returns (metadata_items, level, title_text)
pub fn parse_headline_from_sexp (
  request : &str
) -> Result<( Vec<MetadataItem>,
              usize,   // level in org buffer
              String), // title
            String> {
  let sexp : Sexp =
    sexp::parse ( request )
    . map_err ( |e| format! (
      "Failed to parse S-expression: {}", e ) ) ?;
  let headline_text : String =
    extract_headline_from_sexp ( &sexp ) ?;
  let level : usize =
    count_headline_level ( &headline_text )
    . ok_or ("Ccould not count asterisks in (supposed) headline.") ?;

  // Compute line_after_bullet
  let mut i : usize = 0;
  let bytes : &[u8] = headline_text.as_bytes();
  while i < bytes.len() && bytes[i] == b'*' {
    i += 1; } // skip asterisks
  while i < bytes.len() && (bytes[i] == b' ' || bytes[i] == b'\t') {
    i += 1; } // skip whitespace
  let line_after_bullet : &str = &headline_text[i..];

  let (metadata, title) : (OrgNodeMetadata, String) =
    parse_separating_metadata_and_title ( line_after_bullet );
  Ok (( metadata.metadata, level, title )) }

/// Extract the headline value from a parsed S-expression
/// Expected format: (.. (headline . "HEADLINE") ..)
fn extract_headline_from_sexp (
  sexp : &Sexp
) -> Result<String, String> {
  match sexp {
    Sexp::List ( items ) => {
      for item in items {
        if let Sexp::List ( pair ) = item {
          if pair.len() == 3 {
            if let ( Sexp::Atom ( Atom::S ( key ) ),
                     Sexp::Atom ( Atom::S ( dot ) ),
                     Sexp::Atom ( Atom::S ( value ) ) ) =
              ( &pair[0], &pair[1], &pair[2] )
            { if key == "headline" && dot == "." {
              return Ok ( value.clone() ); }} }} }
      Err ( "No headline field found in S-expression"
             . to_string() ) },
    _ => Err ( "Expected list as top-level S-expression"
                . to_string() ) }}
