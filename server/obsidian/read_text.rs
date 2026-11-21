//! Read a title or body of the vault dialect back into skg's text:
//! the inverse of the exporter's 'render_text'. The exporter's guard
//! runs it on everything it writes, and writes what it cannot give
//! back exactly another way; the importer runs it on the vault.

use crate::obsidian::dialect::*;
use crate::types::misc::ID;

use std::ops::Range;

/// The skg text that 'rendered' stands for: balanced code fences
/// copied, wikilinks turned back into '[[id:ID][label]]', escapes
/// undone. 'root_of_stem' gives the ID of the root of the document
/// whose file has that name (without '.md'). None if a wikilink does
/// not say which ID it means.
pub fn unrender_text (
  rendered     : &str,
  root_of_stem : &dyn Fn (&str) -> Option<ID>,
) -> Option<String> {
  let fences : Vec<Range<usize>> = balanced_fence_ranges (rendered);
  let mut out : String = String::with_capacity (rendered . len ());
  let mut read_to : usize = 0;
  for fence in fences {
    out . push_str ( &unrender_outside_fences (&rendered [read_to .. fence . start], root_of_stem) ? );
    out . push_str (&rendered [fence . clone ()]);
    read_to = fence . end; }
  out . push_str ( &unrender_outside_fences (&rendered [read_to ..], root_of_stem) ? );
  Some (out) }

fn unrender_outside_fences (
  rendered     : &str,
  root_of_stem : &dyn Fn (&str) -> Option<ID>,
) -> Option<String> {
  let mut out : String = String::with_capacity (rendered . len ());
  let mut text_from : usize = 0; // start of text not yet unescaped
  let mut i : usize = 0; // a byte offset, always at a char boundary
  while let Some (c) = rendered [i ..] . chars () . next () {
    if c == '\\' {
      // An escape pair is skipped whole, so that an escaped backslash
      // cannot pass for the start of another escape.
      let next : Option<char> = rendered [i + 1 ..] . chars () . next ();
      i += 1 + next . map_or (0, char::len_utf8);
      continue; }
    if rendered [i ..] . starts_with ("[[") {
      let (link, length) : (String, usize) =
        read_wikilink (&rendered [i ..], root_of_stem) ?;
      out . push_str ( &unescape_text (&rendered [text_from .. i]) );
      out . push_str (&link);
      i += length;
      text_from = i;
      continue; }
    i += c . len_utf8 (); }
  out . push_str ( &unescape_text (&rendered [text_from ..]) );
  Some (out) }

/// The Org link that the wikilink starting 'text' stands for, and how
/// many bytes of 'text' it (with its '%%skg-id%%' comment) takes.
fn read_wikilink (
  text         : &str,
  root_of_stem : &dyn Fn (&str) -> Option<ID>,
) -> Option<(String, usize)> {
  let close : usize = text . find ("]]") ?;
  let inner : &str = &text [2 .. close];
  let (target, label) : (&str, &str) = inner . split_once ('|') ?;
  let mut length : usize = close + 2;
  let raw : ID = match text [length ..] . strip_prefix (LINK_ID_COMMENT_OPEN) {
    Some (rest) => {
      let end : usize = rest . find (COMMENT_CLOSE) ?;
      length += LINK_ID_COMMENT_OPEN . len () + end + COMMENT_CLOSE . len ();
      ID::from (&rest [.. end]) },
    None => id_of_link_target (target, root_of_stem) ?, };
  Some (( format! ("[[id:{}][{}]]", raw . as_str (), label), length )) }

/// The note a wikilink target names: 'File#^BLOCKID' or 'File'.
pub fn id_of_link_target (
  target       : &str,
  root_of_stem : &dyn Fn (&str) -> Option<ID>,
) -> Option<ID> {
  match target . split_once ("#^") {
    Some ((_, block_id)) => pid_of_block_id (block_id),
    None                 => root_of_stem (target), }}
