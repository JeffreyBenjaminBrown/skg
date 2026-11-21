//! The Markdown dialect of a vault that 'skg export-obsidian' writes:
//! the pieces both the exporter and its inverse need to agree on.
//! 'TODO/export-obsidian/plan.org' (part 1, "The mapping") describes
//! the format; 'progress.org' beside it records where this departs.
//!
//! Block IDs. A note's bullet ends in ' ^BLOCKID'. Obsidian allows
//! only letters, digits and dashes there, and ignores case, so a PID
//! of lowercase letters, digits and dashes is its own block ID; any
//! other PID is written as 'skgid-' and the hex of its bytes.
//!
//! Escapes. Titles and bodies are Org (sometimes Markdown) text, shown
//! by a Markdown renderer. 'escape_pieces' puts a backslash before each
//! character that would change the file's structure or hide text
//! (a wikilink, a comment, a block ID, a heading, a tag, math, a fence
//! or an HTML block that swallows what follows, a list marker opening a
//! title); 'unescape_text' undoes it. A backslash is doubled exactly
//! when the character after it is one 'unescape_text' would read as
//! escaped, so the pair is a bijection on text.

use crate::types::misc::ID;

use std::ops::Range;

/// The version of these conventions, a property of the README.
pub const VAULT_FORMAT : &str = "skg-obsidian 2";
/// What every version of 'VAULT_FORMAT' begins with.
pub const VAULT_FORMAT_FAMILY : &str = "skg-obsidian ";
pub const README_STEM : &str = "README";
/// The README's properties describe the vault. Each list has one entry
/// per skgrepo folder, in the same order; an unknown remote hash or
/// commit is the empty text.
pub const KEY_FORMAT        : &str = "skg-format";
pub const KEY_FOLDERS       : &str = "skg-folders";
pub const KEY_SKGREPOS      : &str = "skg-skgrepos";
pub const KEY_REMOTE_HASHES : &str = "skg-remote-hashes";
pub const KEY_COMMITS       : &str = "skg-commits";
/// The README heading that a link to a note outside the vault points
/// to, when the vault lacks the "Some links might be broken" note.
pub const OUTSIDE_VAULT_HEADING : &str = "Links to notes outside this vault";

/// A '%%skg {JSON}%%' comment carries a note's metadata.
pub const META_COMMENT_OPEN  : &str = "%%skg ";
pub const COMMENT_CLOSE      : &str = "%%";
/// The title text of a section without a title (one whose note's home
/// is in another skgrepo).
pub const SECTION_TITLE_MARK : &str = "%%skg section%%";
/// Follows a link whose target does not say the linked ID exactly.
pub const LINK_ID_COMMENT_OPEN : &str = "%%skg-id ";
/// A body that the escapes cannot carry exactly is written in a fence
/// with one of these info strings: raw lines, or one JSON string.
pub const VERBATIM_FENCE_INFO      : &str = "skg-verbatim";
pub const VERBATIM_JSON_FENCE_INFO : &str = "skg-verbatim-json";
/// A 'forks' list entry that is an anchor, as a list property's text.
pub const ANCHOR_ENTRY_PREFIX : &str = "skg-anchor ";
const ENCODED_BLOCK_ID_PREFIX : &str = "skgid-";

// Property keys (a document's root) and '%%skg%%' keys (a bullet).
pub const KEY_ID                  : &str = "skg-id";
pub const KEY_TITLE               : &str = "skg-title";
pub const KEY_ALIASES             : &str = "aliases";
pub const KEY_EXTRA_IDS           : &str = "skg-extra-ids";
pub const KEY_FORKS               : &str = "skg-forks";
pub const KEY_HIDES               : &str = "skg-hides";
pub const KEY_FLAGS               : &str = "skg-flags";
pub const KEY_ATTRIBUTION_ORIGINAL    : &str = "skg-attribution-original";
pub const KEY_ATTRIBUTION_SKGREPO     : &str = "skg-attribution-skgrepo";
pub const KEY_ATTRIBUTION_REMOTE_HASH : &str = "skg-attribution-remote-hash";
pub const KEY_ATTRIBUTION_COMMIT      : &str = "skg-attribution-commit";
pub const KEY_ATTRIBUTION_UNCOMMITTED : &str = "skg-attribution-uncommitted";
/// On a bullet: the ID its container lists it by, when not its PID.
pub const KEY_REF                 : &str = "skg-ref";
/// On a bullet holding only a comment: an anchor in 'contains'.
pub const KEY_ANCHOR              : &str = "skg-anchor";
/// On a bullet whose text is 'ABSENT_MEMBER_TEXT': a 'contains' member
/// that is not in this skgrepo.
pub const KEY_ABSENT              : &str = "skg-absent";
pub const ABSENT_MEMBER_TEXT      : &str = "(a note outside this vault)";

//
// Block IDs
//

pub fn block_id_of_pid (
  pid : &ID,
) -> String {
  let text : &str = pid . as_str ();
  let is_own_block_id : bool =
    ! text . is_empty ()
    && ! text . starts_with (ENCODED_BLOCK_ID_PREFIX)
    && text . chars () . all ( |c| c . is_ascii_lowercase ()
                                     || c . is_ascii_digit ()
                                     || c == '-' );
  if is_own_block_id { text . to_string () }
  else {
    let hex : String = text . bytes ()
      . map ( |byte| format! ("{:02x}", byte) )
      . collect ();
    format! ("{}{}", ENCODED_BLOCK_ID_PREFIX, hex) }}

/// The inverse of 'block_id_of_pid'; None if 'block_id' is not one it
/// writes.
pub fn pid_of_block_id (
  block_id : &str,
) -> Option<ID> {
  if ! is_written_block_id (block_id) { return None; }
  match block_id . strip_prefix (ENCODED_BLOCK_ID_PREFIX) {
    None => Some (ID::from (block_id)),
    Some (hex) => {
      if hex . len () % 2 != 0 { return None; }
      let bytes : Option<Vec<u8>> = (0 .. hex . len ()) . step_by (2)
        . map ( |i| u8::from_str_radix (&hex [i .. i + 2], 16) . ok () )
        . collect ();
      String::from_utf8 (bytes ?) . ok () . map (ID::from) }, }}

/// Whether 'name' can be a skgrepo folder of a vault: one path
/// component, not hidden.
pub fn is_vault_folder_name (
  name : &str,
) -> bool {
  ! name . is_empty () && ! name . starts_with ('.')
  && ! name . contains ('/') && ! name . contains ('\\') }

/// The characters 'block_id_of_pid' writes: a subset of Obsidian's.
pub fn is_written_block_id (
  text : &str,
) -> bool {
  ! text . is_empty ()
  && text . chars () . all ( |c| c . is_ascii_lowercase ()
                                  || c . is_ascii_digit ()
                                  || c == '-' ) }

/// If 'line' ends in ' ^BLOCKID' (a block ID this dialect writes),
/// the text before that suffix and the block ID.
pub fn split_trailing_block_id (
  line : &str,
) -> Option<(&str, &str)> {
  let caret : usize = line . rfind (" ^") ?;
  let block_id : &str = &line [caret + 2 ..];
  if ! is_written_block_id (block_id) { return None; }
  Some (( &line [.. caret], block_id )) }

//
// Escapes
//

/// What a stretch of rendered text is made of. Only 'Text' is escaped;
/// the others are written as they are.
pub enum Piece<'a> {
  Text     (&'a str),
  Verbatim (&'a str), // a balanced code fence, copied
  Produced (String),  // a wikilink (and its comment) the exporter made
}

/// Characters that a backslash before them escapes.
const ESCAPABLE : &[char] = &[
  '\\', '[', '%', '^', '$', '#', '!', '`', '~', '<',
  '-', '+', '*', '>', '.', ')' ];

fn is_escapable (
  c : char,
) -> bool {
  ESCAPABLE . contains (&c) }

/// Concatenate 'pieces', escaping their 'Text'. 'is_title' adds the
/// escapes a list item's first line needs: a leading list marker or
/// blockquote marker.
pub fn escape_pieces (
  pieces   : &[Piece],
  is_title : bool,
) -> String {
  let logical : Vec<(char, bool)> = // (char, is it Text)
    pieces . iter ()
    . flat_map ( |piece| -> Vec<(char, bool)> { match piece {
      Piece::Text (t)     => t . chars () . map ( |c| (c, true) ) . collect (),
      Piece::Verbatim (t) => t . chars () . map ( |c| (c, false) ) . collect (),
      Piece::Produced (t) => t . chars () . map ( |c| (c, false) ) . collect (), }} )
    . collect ();
  let chars : Vec<char> = logical . iter () . map ( |(c, _)| *c ) . collect ();
  let mut out : String = String::with_capacity (chars . len () + 16);
  for (i, (c, is_text)) in logical . iter () . enumerate () {
    if *is_text && needs_escape (&chars, &logical, i, is_title) {
      out . push ('\\'); }
    out . push (*c); }
  out }

fn needs_escape (
  chars    : &[char],
  logical  : &[(char, bool)],
  i        : usize,
  is_title : bool,
) -> bool {
  let c : char = chars [i];
  let prev : Option<char> = if i > 0 { Some (chars [i - 1]) } else { None };
  let next : Option<char> = chars . get (i + 1) . copied ();
  let at_line_start : bool = { // only blanks since the line began
    let mut j : usize = i;
    loop {
      if j == 0 { break true; }
      match chars [j - 1] {
        '\n' => break true,
        ' ' | '\t' => j -= 1,
        _ => break false, }}};
  match c {
    '\\' => next . is_some_and (is_escapable),
    '['  => prev == Some ('[') || next == Some ('['),
    '%'  => prev == Some ('%') || next == Some ('%'),
    '^' | '$' => true,
    '#'  => prev . is_none_or ( |p| p . is_whitespace () ),
    '!'  => next == Some ('[')
            && logical . get (i + 1) . is_some_and ( |(_, is_text)| ! is_text ),
    '`' | '~' => at_line_start && run_length (chars, i) >= 3,
    '<'  => at_line_start && next . is_some_and (
              |n| n . is_ascii_alphabetic () || n == '/' || n == '!' || n == '?' ),
    '-' | '+' | '*' => is_title && at_line_start
                       && next . is_none_or ( |n| n == ' ' || n == '\t' ),
    '>'  => is_title && at_line_start,
    '.' | ')' => is_title && title_starts_with_ordered_marker_ending_at (chars, i),
    _ => false, }}

fn run_length (
  chars : &[char],
  i     : usize,
) -> usize {
  chars [i ..] . iter () . take_while ( |c| **c == chars [i] ) . count () }

/// Whether 'chars' (a title) begins, after blanks, with digits that
/// end at 'i', the '.' or ')' of an ordered-list marker.
fn title_starts_with_ordered_marker_ending_at (
  chars : &[char],
  i     : usize,
) -> bool {
  let start : usize = chars . iter () . take_while ( |c| **c == ' ' || **c == '\t' ) . count ();
  i > start
  && chars [start .. i] . iter () . all ( |c| c . is_ascii_digit () )
  && chars . get (i + 1) . is_none_or ( |n| *n == ' ' || *n == '\t' ) }

/// The inverse of 'escape_pieces' on one stretch of 'Text'.
pub fn unescape_text (
  text : &str,
) -> String {
  let mut out : String = String::with_capacity (text . len ());
  let mut chars = text . chars () . peekable ();
  while let Some (c) = chars . next () {
    if c == '\\' {
      if let Some (next) = chars . peek () . copied () {
        if is_escapable (next) {
          out . push (next);
          chars . next ();
          continue; }}}
    out . push (c); }
  out }

//
// Code fences
//

/// A line that opens a code fence: its character and length.
pub fn fence_opener (
  line : &str,
) -> Option<(char, usize)> {
  let indent : usize = line . chars () . take_while ( |c| *c == ' ' ) . count ();
  if indent > 3 { return None; }
  let rest : &str = &line [indent ..];
  let fence_char : char = rest . chars () . next () ?;
  if fence_char != '`' && fence_char != '~' { return None; }
  let length : usize = rest . chars () . take_while ( |c| *c == fence_char ) . count ();
  if length < 3 { return None; }
  let info : &str = &rest [length ..]; // fence chars are one byte each
  if fence_char == '`' && info . contains ('`') { return None; }
  Some (( fence_char, length )) }

pub fn closes_fence (
  line   : &str,
  opener : (char, usize),
) -> bool {
  let indent : usize = line . chars () . take_while ( |c| *c == ' ' ) . count ();
  if indent > 3 { return false; }
  let rest : &str = &line [indent ..];
  let length : usize = rest . chars () . take_while ( |c| *c == opener . 0 ) . count ();
  length >= opener . 1
  && rest [length ..] . chars () . all ( |c| c == ' ' || c == '\t' ) }

/// The byte ranges of 'text' (whole lines, each ending after its '\n'
/// or at the end of 'text') that are balanced code fences: an opener
/// through its closer. An opener with no closer is not one.
pub fn balanced_fence_ranges (
  text : &str,
) -> Vec<Range<usize>> {
  let mut line_ranges : Vec<Range<usize>> = Vec::new ();
  { let mut start : usize = 0;
    for line in text . split_inclusive ('\n') {
      line_ranges . push (start .. start + line . len ());
      start += line . len (); }}
  let line_text = |r : &Range<usize>| -> &str {
    text [r . clone ()] . trim_end_matches ('\n') };
  let mut out : Vec<Range<usize>> = Vec::new ();
  let mut i : usize = 0;
  while i < line_ranges . len () {
    let Some (opener) = fence_opener (line_text (&line_ranges [i]))
      else { i += 1; continue; };
    let closer : Option<usize> = ( i + 1 .. line_ranges . len () )
      . find ( |j| closes_fence (line_text (&line_ranges [*j]), opener) );
    match closer {
      Some (j) => {
        out . push (line_ranges [i] . start .. line_ranges [j] . end);
        i = j + 1; },
      None => i += 1, }}
  out }

/// A fence of backticks longer than any run of them in 'text'.
pub fn fence_for (
  text : &str,
) -> String {
  let mut longest : usize = 0;
  let mut run : usize = 0;
  for c in text . chars () {
    if c == '`' { run += 1; longest = longest . max (run); }
    else { run = 0; }}
  "`" . repeat ( (longest + 1) . max (3) ) }

//
// Metadata comments
//

/// '%%skg JSON%%', with each '%' inside the JSON written as '%' so
/// that the comment cannot end early.
pub fn meta_comment (
  object : &serde_json::Map<String, serde_json::Value>,
) -> String {
  let json : String = serde_json::Value::Object (object . clone ()) . to_string ();
  format! ("{}{}{}", META_COMMENT_OPEN, json . replace ('%', "\\u0025"), COMMENT_CLOSE) }

/// The JSON object of a line that is exactly a 'meta_comment'.
pub fn parse_meta_comment (
  text : &str,
) -> Option<serde_json::Map<String, serde_json::Value>> {
  let json : &str = text . strip_prefix (META_COMMENT_OPEN) ?
    . strip_suffix (COMMENT_CLOSE) ?;
  match serde_json::from_str (json) . ok () ? {
    serde_json::Value::Object (object) => Some (object),
    _ => None, }}

#[cfg(test)]
#[path = "../../tests/unit/obsidian_dialect.rs"]
mod tests;
