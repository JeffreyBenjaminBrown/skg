//! The redraw delta: which headlines of a view's text a client must
//! rewrite to turn the text it shows into a new text the server
//! sends ('docs/api-and-formats.org', "The redraw delta").
//!
//! The comparison reads both texts in their stripped form, without
//! the fold and focus atoms, which the client keeps as org folds and
//! point rather than as text. The stripped form is one rule that the
//! two clients implement too; all three test against
//! 'tests/shared/fold-and-focus-atom-strip-cases.txt'.

use crate::types::viewnode::ViewnodeId;

use sexp::{Atom, Sexp};
use std::iter::once;

/// A view's text split at its headline lines ('^\*+ ', org's rule):
/// the text before the first one, and one record per headline line.
struct SplitViewText<'t> {
  preamble : &'t str,
  records  : Vec<HeadlineRecord<'t>>,
}

/// A headline line and the body lines after it, up to the next
/// headline line or the end of the text.
struct HeadlineRecord<'t> {
  headline_line       : &'t str, // without its newline
  after_headline_line : &'t str, // the headline line's newline, if any, then the body lines
}

const FOLD_AND_FOCUS_ATOMS : [&str; 3] =
  [ "focused", "folded", "bodyFolded" ];

/// The delta from 'reference_text' to 'new_text': the viewnode IDs
/// of the headline records whose stripped headline lines differ, in
/// 'new_text''s order. None means "whole": the texts differ in more
/// than headline lines (their preambles, their record counts, their
/// viewnode ID sequences or some record's body lines differ), or
/// some headline line of either has no viewnode ID.
pub fn changed_headlines (
  reference_text : &str,
  new_text       : &str,
) -> Option<Vec<ViewnodeId>> {
  let reference : SplitViewText = split_view_text (reference_text);
  let new       : SplitViewText = split_view_text (new_text);
  if reference . preamble != new . preamble
     || reference . records . len () != new . records . len ()
  { return None; }
  let mut changed : Vec<ViewnodeId> = Vec::new ();
  for (reference_record, new_record)
    in reference . records . iter () . zip (new . records . iter ())
  { let reference_viewnode_id : ViewnodeId =
      viewnode_id_of_headline_line (reference_record . headline_line) ?;
    let new_viewnode_id : ViewnodeId =
      viewnode_id_of_headline_line (new_record . headline_line) ?;
    if reference_viewnode_id != new_viewnode_id
       || reference_record . after_headline_line
          != new_record . after_headline_line
    { return None; }
    if stripped_line (reference_record . headline_line)
       != stripped_line (new_record . headline_line)
    { changed . push (new_viewnode_id); }}
  Some (changed) }

/// The envelope entry '(changed-headlines (V1 V2 ...))', each
/// viewnode ID a bare symbol; '(changed-headlines ())' when none.
pub fn changed_headlines_entry (
  viewnode_ids : &[ViewnodeId],
) -> Sexp {
  Sexp::List ( vec! [
    Sexp::Atom ( Atom::S ( "changed-headlines" . to_string () )),
    Sexp::List ( viewnode_ids . iter ()
                 . map ( |viewnode_id|
                         Sexp::Atom ( Atom::S ( viewnode_id . 0 . clone () )) )
                 . collect () ) ] ) }

fn split_view_text<'t> (
  text : &'t str,
) -> SplitViewText<'t> {
  let headline_line_starts : Vec<usize> =
    line_starts (text)
    . filter ( |start| is_headline_line (&text [*start ..]) )
    . collect ();
  let preamble_end : usize =
    headline_line_starts . first () . copied () . unwrap_or (text . len ());
  let records : Vec<HeadlineRecord> =
    headline_line_starts . iter () . enumerate ()
    . map ( |(i, start)| {
      let end : usize = headline_line_starts . get (i + 1)
        . copied () . unwrap_or (text . len ());
      headline_record (&text [*start .. end]) } )
    . collect ();
  SplitViewText { preamble : &text [.. preamble_end],
                  records } }

/// The byte offset of each line's start in 'text'.
fn line_starts (
  text : &str,
) -> impl Iterator<Item = usize> + '_ {
  once (0)
    . chain ( text . match_indices ('\n')
              . map ( |(newline, _)| newline + 1 ) )
    . filter ( move |start| *start < text . len () ) }

fn headline_record (
  record_text : &str, // a headline line, then the body lines after it
) -> HeadlineRecord<'_> {
  let headline_line_end : usize =
    record_text . find ('\n') . unwrap_or (record_text . len ());
  HeadlineRecord {
    headline_line       : &record_text [.. headline_line_end],
    after_headline_line : &record_text [headline_line_end ..], } }

/// Whether 'line' (which may run past its newline) is a headline line:
/// one or more stars, then a space.
fn is_headline_line (
  line : &str,
) -> bool {
  let after_stars : &str = line . trim_start_matches ('*');
  after_stars . len () < line . len ()
    && after_stars . starts_with (' ') }

/// The X of a headline line beginning '^\*+ (skg (vnid X)', the form
/// the renderer writes ('viewnode_to_string_where_it_stands').
fn viewnode_id_of_headline_line (
  headline_line : &str,
) -> Option<ViewnodeId> {
  let after_vnid : &str =
    headline_line . trim_start_matches ('*')
    . strip_prefix (" (skg (vnid ") ?;
  let viewnode_id_end : usize = after_vnid . find (')') ?;
  ViewnodeId::parse (&after_vnid [.. viewnode_id_end]) . ok () }

/// The stripped form of 'line': a headline line without the fold and
/// focus atoms at depth 1 of its '(skg ...)' form, each removed with
/// the one space before it. Depth counts the parentheses outside
/// string literals. Any other line is its own stripped form.
fn stripped_line (
  line : &str,
) -> String {
  match skg_form_start (line) {
    None => line . to_string (),
    Some (form_start) => without_fold_and_focus_atoms (line, form_start), } }

/// Where the '(skg ...)' form of a headline line starts: right after
/// its stars and a space, as '(skg' then a space or ')'.
fn skg_form_start (
  line : &str,
) -> Option<usize> {
  if ! is_headline_line (line) { return None; }
  let form_start : usize =
    line . len () - line . trim_start_matches ('*') . len () + 1;
  let after_skg : &str =
    line [form_start ..] . strip_prefix ("(skg") ?;
  ( after_skg . starts_with (' ') || after_skg . starts_with (')') )
    . then_some (form_start) }

/// 'line' without each fold and focus atom, and the space before it,
/// at depth 1 of the form starting at 'form_start'. The text after
/// the form is kept as it is.
fn without_fold_and_focus_atoms (
  line       : &str,
  form_start : usize,
) -> String {
  let bytes : &[u8] = line . as_bytes ();
  let mut stripped : String = String::with_capacity (line . len ());
  let mut copied_up_to : usize = 0;
  let mut depth : usize = 0;
  let mut i : usize = form_start;
  while i < bytes . len () {
    match bytes [i] {
      b'"' => { i = end_of_string_literal (bytes, i);
                continue; }
      b'(' => depth += 1,
      b')' => { depth -= 1;
                if depth == 0 { break; }}
      b' ' if depth == 1 => {
        let atom_end : usize = end_of_bare_atom (bytes, i + 1);
        if FOLD_AND_FOCUS_ATOMS . contains (&&line [i + 1 .. atom_end]) {
          stripped . push_str (&line [copied_up_to .. i]);
          copied_up_to = atom_end;
          i = atom_end;
          continue; }}
      _ => {}, }
    i += 1; }
  stripped . push_str (&line [copied_up_to ..]);
  stripped }

/// The index just past the string literal whose opening quote is at
/// 'quote', or the end of 'bytes' if it is not closed. A backslash
/// escapes the byte after it.
fn end_of_string_literal (
  bytes : &[u8],
  quote : usize,
) -> usize {
  let mut i : usize = quote + 1;
  while i < bytes . len () {
    match bytes [i] {
      b'\\' => i += 2,
      b'"'  => return i + 1,
      _     => i += 1, }}
  bytes . len () }

/// The index just past the bare atom starting at 'start': the run of
/// bytes that are not whitespace, parentheses or quotes.
fn end_of_bare_atom (
  bytes : &[u8],
  start : usize,
) -> usize {
  bytes [start ..] . iter ()
    . position ( |byte| byte . is_ascii_whitespace ()
                        || matches! (byte, b'(' | b')' | b'"') )
    . map_or (bytes . len (), |length| start + length) }

#[cfg(test)]
#[path = "../tests/unit/view_text_delta.rs"]
mod tests;
