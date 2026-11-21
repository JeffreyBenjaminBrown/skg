//! Read a title or body of the vault dialect back into skg's text:
//! the inverse of the exporter's 'render_text'. The exporter's guard
//! runs it on everything it writes, and writes what it cannot give
//! back exactly another way; the importer runs it on the vault.
//! 'dialect.rs' lists the forms it reads.

use crate::obsidian::dialect::*;
use crate::types::misc::ID;

use std::ops::Range;

/// The skg text that 'rendered' stands for: fences of backticks
/// copied, fences of tildes turned back into the Org blocks they were,
/// wikilinks into '[[id:ID][label]]', the other Markdown made from Org
/// back into Org, escapes undone. 'root_of_stem' gives the ID of the
/// root of the document whose file has that name (without '.md').
/// None if the text is not one the exporter writes (a wikilink that
/// does not say which ID it means, an unclosed code span).
pub fn unrender_text (
  rendered     : &str,
  root_of_stem : &dyn Fn (&str) -> Option<ID>,
) -> Option<String> {
  let fences : Vec<Range<usize>> = balanced_fence_ranges (rendered);
  let mut out : String = String::with_capacity (rendered . len ());
  let mut read_to : usize = 0;
  for fence in fences {
    out . push_str ( &unrender_outside_fences (&rendered [read_to .. fence . start], root_of_stem) ? );
    out . push_str ( &unrender_fence (&rendered [fence . clone ()]) ? );
    read_to = fence . end; }
  out . push_str ( &unrender_outside_fences (&rendered [read_to ..], root_of_stem) ? );
  Some (out) }

/// A fence, from its opening line through its closing one (and that
/// line's newline, if it has one). One of backticks is copied; one of
/// tildes is an Org block, or fixed-width lines.
fn unrender_fence (
  fence : &str,
) -> Option<String> {
  let (lines_text, newline) : (&str, &str) = match fence . strip_suffix ('\n') {
    Some (text) => (text, "\n"),
    None        => (fence, ""), };
  let lines : Vec<&str> = lines_text . split ('\n') . collect ();
  let opener : &str = lines [0];
  let indent : &str = &opener [.. opener . len () - opener . trim_start_matches (' ') . len ()];
  let rest : &str = &opener [indent . len () ..];
  if rest . starts_with ('`') { return Some (fence . to_string ()); }
  let info : &str = rest . trim_start_matches ('~');
  let content : &[&str] = &lines [1 .. lines . len () - 1];
  let mut out : Vec<String> = Vec::new ();
  let block : OrgBlock = org_block_of_fence_info (info);
  match org_block_delimiters (&block) {
    None => { // fixed-width lines, written only at the left margin
      if ! indent . is_empty () { return None; }
      out . extend ( content . iter () . map ( |line| fixed_width_line (line) ) ); },
    Some ((begin, end)) => {
      out . push ( format! ("{}{}", indent, begin) );
      out . extend ( content . iter () . map ( |line| line . to_string () ) );
      out . push ( format! ("{}{}", indent, end) ); }, }
  Some ( format! ("{}{}", out . join ("\n"), newline) ) }

fn unrender_outside_fences (
  rendered     : &str,
  root_of_stem : &dyn Fn (&str) -> Option<ID>,
) -> Option<String> {
  let mut out : String = String::with_capacity (rendered . len ());
  let mut text_from : usize = 0; // start of text not yet unescaped
  let mut i : usize = 0; // a byte offset, always at a char boundary
  let flush = |out : &mut String, from : usize, to : usize| {
    out . push_str ( &unescape_text (&rendered [from .. to]) ); };
  while let Some (c) = rendered [i ..] . chars () . next () {
    let rest : &str = &rendered [i ..];
    // What the exporter made from Org, and its length in 'rendered'.
    let made : Option<(String, usize)> = match c {
      '\\' => {
        // An escape pair is skipped whole, so that an escaped backslash
        // cannot pass for the start of another escape.
        let next : Option<char> = rendered [i + 1 ..] . chars () . next ();
        i += 1 + next . map_or (0, char::len_utf8);
        continue; },
      '[' if rest . starts_with ("[[") => Some (read_wikilink (rest, root_of_stem) ?),
      '[' => read_markdown_link (rest),
      '<' => read_autolink (rest),
      '`' => match run_of (rest, '`') {
        1 => { // a code span of the text: copied
          let length : usize = 1 + closing_run (&rest [1 ..], 1) ? + 1;
          flush (&mut out, text_from, i);
          out . push_str (&rest [.. length]);
          i += length;
          text_from = i;
          continue; },
        2 => {
          let inner_length : usize = closing_run (&rest [2 ..], 2) ?;
          let inner : &str = &rest [2 .. 2 + inner_length];
          let org : String =
            match inner . strip_prefix (' ') . and_then ( |s| s . strip_suffix (' ') ) {
              Some (code) if ! code . is_empty () => format! ("~{}~", code),
              _ => format! ("={}=", inner), };
          Some ((org, 2 + inner_length + 2)) },
        _ => return None, },
      '*' => {
        let next : Option<char> = rest [1 ..] . chars () . next ();
        if is_at_line_start (rendered, i) && next . is_none_or ( |n| n == ' ' || n == '\t' || n == '\n' ) {
          None } // a list marker
        else if next == Some ('*') { Some (( "*" . to_string (), 2 )) }
        else { Some (( "/" . to_string (), 1 )) }},
      '|' if i == 0 || rendered . as_bytes () [i - 1] == b'\n' => {
        let line : &str = rest . split ('\n') . next () . unwrap_or ("");
        if is_table_rule_written (line) { Some (( org_table_rule_of (line), line . len () )) }
        else { None }},
      _ => None, };
    match made {
      Some ((org, length)) => {
        flush (&mut out, text_from, i);
        out . push_str (&org);
        i += length;
        text_from = i; },
      None => i += c . len_utf8 (), }}
  flush (&mut out, text_from, rendered . len ());
  Some (out) }

/// Whether only blanks lie between the start of the line and 'i'.
fn is_at_line_start (
  text : &str,
  i    : usize,
) -> bool {
  text [.. i] . chars () . rev () . take_while ( |c| *c != '\n' )
    . all ( |c| c == ' ' || c == '\t' ) }

/// How many times 'c' begins 'text'.
fn run_of (
  text : &str,
  c    : char,
) -> usize {
  text . chars () . take_while ( |x| *x == c ) . count () }

/// Where, in 'text', the first run of exactly 'length' backticks on
/// its line begins.
fn closing_run (
  text   : &str,
  length : usize,
) -> Option<usize> {
  let bytes : &[u8] = text . as_bytes ();
  let mut j : usize = 0;
  while j < bytes . len () && bytes [j] != b'\n' {
    if bytes [j] == b'`' {
      let run : usize = run_of (&text [j ..], '`');
      if run == length { return Some (j); }
      j += run; }
    else { j += 1; }}
  None }

/// '[label](URL)' or '[label](<URL>)', written for '[[URL][label]]'.
fn read_markdown_link (
  text : &str,
) -> Option<(String, usize)> {
  let close : usize = text . find (']') ?;
  let label : &str = &text [1 .. close];
  if label . contains ('\n') || label . contains ('[') { return None; }
  let after : &str = text [close + 1 ..] . strip_prefix ('(') ?;
  let (url, length) : (&str, usize) = match after . strip_prefix ('<') {
    Some (inner) => {
      let end : usize = inner . find ('>') ?;
      if ! inner [end + 1 ..] . starts_with (')') { return None; }
      (&inner [.. end], 1 + end + 2) },
    None => {
      let end : usize = after . find (')') ?;
      (&after [.. end], end + 1) }, };
  if url . is_empty () || url . contains ( |c : char| c . is_whitespace () || c == '<' ) {
    return None; }
  Some (( format! ("[[{}][{}]]", url, label), close + 2 + length )) }

/// '<URL>', written for '[[URL]]'.
fn read_autolink (
  text : &str,
) -> Option<(String, usize)> {
  let rest : Vec<char> = text [1 ..] . chars () . take (8) . collect ();
  if ! starts_a_web_address (&rest) { return None; }
  let end : usize = text . find ('>') ?;
  let url : &str = &text [1 .. end];
  if url . contains ( |c : char| c . is_whitespace () || c == '<' ) { return None; }
  Some (( format! ("[[{}]]", url), end + 1 )) }

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
