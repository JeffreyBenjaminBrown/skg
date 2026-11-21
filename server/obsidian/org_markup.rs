//! Find, in a title or body, the Org markup that the export writes as
//! Markdown, and what it writes for it ('dialect.rs' lists the forms;
//! 'read_text.rs' reads them back). What it finds but cannot write
//! exactly it leaves as text and names, so that the export can count
//! why ('MarkupTally'). Each function returns spans of the text that
//! are not escaped: 'Span::Copied' as they are, 'Span::Written' as
//! Markdown. None of them overlaps a span already 'taken'.

use crate::obsidian::dialect::*;
use crate::types::links::org_literal_ranges::{VERBATIM_POST, VERBATIM_PRE, org_literal_ranges};

use regex::Regex;
use std::collections::{BTreeMap, BTreeSet};
use std::ops::Range;
use std::sync::LazyLock;

pub enum Span {
  Copied,
  Written (String),
}

pub type Spans = Vec<(Range<usize>, Span)>;

/// What one text's markup came to: the kinds converted, with counts,
/// and why any markup was left as text.
#[derive(Default)]
pub struct MarkupTally {
  pub converted : BTreeMap<&'static str, usize>,
  pub declined  : BTreeSet<&'static str>,
}

impl MarkupTally {
  fn convert (&mut self, kind : &'static str) {
    * self . converted . entry (kind) . or_insert (0) += 1; }
  fn decline (&mut self, reason : &'static str) {
    self . declined . insert (reason); }
}

// The kinds converted, and the reasons for leaving markup as text.
pub const KIND_SRC_BLOCK     : &str = "source block";
pub const KIND_EXAMPLE_BLOCK : &str = "example block";
pub const KIND_FIXED_WIDTH   : &str = "fixed-width lines";
pub const KIND_TABLE_RULE    : &str = "table";
pub const KIND_VERBATIM      : &str = "verbatim or code";
pub const KIND_WEB_LINK      : &str = "web link";
pub const KIND_BOLD          : &str = "bold";
pub const KIND_ITALIC        : &str = "italic";
pub const WHY_BLOCK_UNCLOSED   : &str = "a #+begin_ block that never closes";
pub const WHY_BLOCK_KIND       : &str = "a block other than src or example (quote, verse ...)";
pub const WHY_BLOCK_INDENTED   : &str = "a block indented more than 3 spaces";
pub const WHY_BLOCK_CASE       : &str = "block keywords in mixed case";
pub const WHY_BLOCK_END        : &str = "a block whose end line is not its begin line's partner";
pub const WHY_BLOCK_ARGUMENTS  : &str = "block arguments a fence's info string cannot carry";
pub const WHY_FIXED_INDENTED   : &str = "indented fixed-width lines";
pub const WHY_FIXED_EMPTY      : &str = "a fixed-width line that is ': ' alone";
pub const WHY_TABLE_NO_RULE    : &str = "a table with no rule under its first row";
pub const WHY_TABLE_RULES      : &str = "a table with a rule other than one under its first row";
pub const WHY_TABLE_SHAPE      : &str = "a table rule that does not fit its first row";
pub const WHY_VERBATIM_TICK    : &str = "verbatim text holding a backtick";
pub const WHY_VERBATIM_COMMENT : &str = "verbatim text holding '%%'";
pub const WHY_LINK_NOT_WEB     : &str = "an Org link to a file, heading or other non-web target";
pub const WHY_LINK_CHARACTERS  : &str = "a web link whose label or address Markdown cannot carry";
pub const WHY_GUARD            : &str = "the converted text did not read back exactly";

/// '[[TARGET]]' or '[[TARGET][LABEL]]', on one line.
static ORG_LINK : LazyLock<Regex> = LazyLock::new ( ||
  Regex::new (r"\[\[([^\[\]\n]+)\](?:\[([^\[\]\n]*)\])?\]") . unwrap () );

fn overlaps (
  a : &Range<usize>,
  b : &Range<usize>,
) -> bool {
  a . start < b . end && b . start < a . end }

fn is_free (
  range : &Range<usize>,
  taken : &[Range<usize>],
) -> bool {
  ! taken . iter () . any ( |t| overlaps (t, range) ) }

/// Each line of 'text' as a byte range, without its newline.
fn line_ranges (
  text : &str,
) -> Vec<Range<usize>> {
  let mut out : Vec<Range<usize>> = Vec::new ();
  let mut start : usize = 0;
  for line in text . split ('\n') {
    out . push (start .. start + line . len ());
    start += line . len () + 1; }
  out }

//
// Blocks
//

/// The fences of backticks the text holds (copied), and, if 'convert',
/// its Org blocks, runs of fixed-width lines, and tables' rules. Read
/// line by line from the top, as the inverse reads fences.
pub fn block_spans (
  text    : &str,
  convert : bool,
  tally   : &mut MarkupTally,
) -> Spans {
  let lines : Vec<Range<usize>> = line_ranges (text);
  let line = |k : usize| -> &str { &text [lines [k] . clone ()] };
  let mut spans : Spans = Vec::new ();
  let mut k : usize = 0;
  while k < lines . len () {
    if let Some (opener) = fence_opener (line (k)) . filter ( |o| o . 0 == '`' ) {
      if let Some (j) = (k + 1 .. lines . len ()) . find ( |j| closes_fence (line (*j), opener) ) {
        spans . push ((lines [k] . start .. lines [j] . end, Span::Copied));
        k = j + 1;
        continue; }}
    if ! convert { k += 1; continue; }
    if let Some (begin) = OrgBlockBegin::parse (line (k)) {
      let end_prefix : String = format! ("#+end_{}", begin . kind_lowercase);
      let Some (j) = (k + 1 .. lines . len ())
        . find ( |j| line (*j) . trim_start () . to_ascii_lowercase () . starts_with (&end_prefix) )
        else { tally . decline (WHY_BLOCK_UNCLOSED); break; };
      let content : Vec<&str> = (k + 1 .. j) . map (line) . collect ();
      match begin . written (line (j), &content) {
        Ok ((written, kind)) => {
          tally . convert (kind);
          spans . push ((lines [k] . start .. lines [j] . end, Span::Written (written))); },
        Err (why) => tally . decline (why), }
      k = j + 1;
      continue; }
    if fixed_width_indent (line (k)) . is_some () {
      let j : usize = (k .. lines . len ())
        . take_while ( |j| fixed_width_indent (line (*j)) . is_some () ) . last () . unwrap ();
      let run : Vec<&str> = (k ..= j) . map (line) . collect ();
      match fixed_width_written (&run) {
        Ok (written) => {
          tally . convert (KIND_FIXED_WIDTH);
          spans . push ((lines [k] . start .. lines [j] . end, Span::Written (written))); },
        Err (why) => tally . decline (why), }
      k = j + 1;
      continue; }
    if line (k) . starts_with ('|') {
      let j : usize = (k .. lines . len ())
        . take_while ( |j| line (*j) . starts_with ('|') ) . last () . unwrap ();
      if j > k {
        match table_rule_span (&lines, k, j, text) {
          Ok (Some (span)) => { tally . convert (KIND_TABLE_RULE); spans . push (span); },
          Ok (None) => {}, // a Markdown table already
          Err (why) => tally . decline (why), }}
      k = j + 1;
      continue; }
    k += 1; }
  spans }

/// The line that opens an Org block: '#+begin_KIND ARGUMENTS'.
struct OrgBlockBegin {
  indent          : usize,
  keyword         : String, // as written: 'begin_src', 'BEGIN_EXAMPLE' ...
  kind_lowercase  : String,
  arguments       : Option<String>, // what follows the keyword and a space
}

impl OrgBlockBegin {
  fn parse (
    line : &str,
  ) -> Option<OrgBlockBegin> {
    let indent : usize = line . len () - line . trim_start_matches (' ') . len ();
    let rest : &str = line [indent ..] . strip_prefix ("#+") ?;
    let (keyword, arguments) : (&str, Option<&str>) = match rest . split_once (' ') {
      Some ((keyword, arguments)) => (keyword, Some (arguments)),
      None => (rest, None), };
    let kind : &str = keyword . get (.. 6)
      . filter ( |b| b . eq_ignore_ascii_case ("begin_") )
      . map ( |_| &keyword [6 ..] ) ?;
    Some (OrgBlockBegin {
      indent, keyword : keyword . to_string (), kind_lowercase : kind . to_ascii_lowercase (),
      arguments : arguments . map (String::from) }) }

  /// The fence written for this block, ending at 'end_line', and its
  /// kind; or why it cannot be written exactly.
  fn written (
    &self,
    end_line : &str,
    content  : &[&str],
  ) -> Result<(String, &'static str), &'static str> {
    if self . kind_lowercase != "src" && self . kind_lowercase != "example" {
      return Err (WHY_BLOCK_KIND); }
    if self . indent > 3 { return Err (WHY_BLOCK_INDENTED); }
    let uppercase : bool = self . keyword == self . keyword . to_ascii_uppercase ();
    if ! uppercase && self . keyword != self . keyword . to_ascii_lowercase () {
      return Err (WHY_BLOCK_CASE); }
    let (block, kind) : (OrgBlock, &'static str) = match self . kind_lowercase . as_str () {
      "src" => (
        OrgBlock::Src { arguments : self . arguments . clone () . unwrap_or_default (), uppercase },
        KIND_SRC_BLOCK ),
      _ => {
        if self . arguments . is_some () { return Err (WHY_BLOCK_ARGUMENTS); }
        (OrgBlock::Example { uppercase }, KIND_EXAMPLE_BLOCK) }, };
    if self . arguments . as_deref () == Some ("") { return Err (WHY_BLOCK_ARGUMENTS); }
    let (begin, end) : (String, String) =
      org_block_delimiters (&block) . expect ("a src or example block has delimiters");
    let indent : String = " " . repeat (self . indent);
    if end_line != format! ("{}{}", indent, end)
       || format! ("#+{}{}", self . keyword,
                   self . arguments . as_ref () . map_or (String::new (), |a| format! (" {}", a)) )
          != begin {
      return Err (WHY_BLOCK_END); }
    let info : String = fence_info_of_org_block (&block) . ok_or (WHY_BLOCK_ARGUMENTS) ?;
    let fence : String = tilde_fence_for (content);
    let mut lines : Vec<String> = vec! [ format! ("{}{}{}", indent, fence, info) ];
    lines . extend ( content . iter () . map ( |l| l . to_string () ) );
    lines . push ( format! ("{}{}", indent, fence) );
    Ok (( lines . join ("\n"), kind )) }
}

/// Its indentation, if 'line' is an Org fixed-width line.
fn fixed_width_indent (
  line : &str,
) -> Option<usize> {
  let trimmed : &str = line . trim_start_matches (' ');
  ( trimmed == ":" || trimmed . starts_with (": ") ) . then ( || line . len () - trimmed . len () ) }

fn fixed_width_written (
  run : &[&str],
) -> Result<String, &'static str> {
  if run . iter () . any ( |l| fixed_width_indent (l) != Some (0) ) { return Err (WHY_FIXED_INDENTED); }
  let content : Vec<&str> = run . iter ()
    . map ( |l| fixed_width_content (l) . ok_or (WHY_FIXED_EMPTY) )
    . collect::<Result<Vec<&str>, &'static str>> () ?;
  let fence : String = tilde_fence_for (&content);
  let info : String = fence_info_of_org_block (&OrgBlock::FixedWidth) . unwrap ();
  let mut lines : Vec<String> = vec! [ format! ("{}{}", fence, info) ];
  lines . extend ( content . iter () . map ( |l| l . to_string () ) );
  lines . push (fence);
  Ok (lines . join ("\n")) }

/// The rule under the first row of the table on lines 'first' through
/// 'last' (two or more lines, each beginning '|'), written as Markdown
/// needs it; None if the table is Markdown's already (its rule
/// separates cells with '|', not Org's '+').
fn table_rule_span (
  lines : &[Range<usize>],
  first : usize,
  last  : usize,
  text  : &str,
) -> Result<Option<(Range<usize>, Span)>, &'static str> {
  let line = |k : usize| -> &str { &text [lines [k] . clone ()] };
  let is_rule = |k : usize| -> bool { line (k) . starts_with ("|-") || line (k) . starts_with ("|:") };
  if ! is_rule (first + 1) { return Err (WHY_TABLE_NO_RULE); }
  let header : &str = line (first) . trim_end ();
  if ! header . ends_with ('|') || header . len () < 2 { return Err (WHY_TABLE_SHAPE); }
  let cells : usize = header . matches ('|') . count () - 1;
  if cells > 1 && ! line (first + 1) . contains ('+') { return Ok (None); }
  if (first ..= last) . any ( |k| k != first + 1 && is_rule (k) ) { return Err (WHY_TABLE_RULES); }
  let written : String = table_rule_written_for (line (first + 1), cells) . ok_or (WHY_TABLE_SHAPE) ?;
  Ok (Some (( lines [first + 1] . clone (), Span::Written (written) ))) }

//
// Inline
//

/// '=verbatim=' and '~code~' as code spans, and Org links to web pages
/// as Markdown links, outside what is 'taken'.
pub fn inline_org_spans (
  text  : &str,
  taken : &[Range<usize>],
  tally : &mut MarkupTally,
) -> Spans {
  let literal : Vec<Range<usize>> = org_literal_ranges (text);
  let mut spans : Spans = Vec::new ();
  for range in &literal {
    let span_text : &str = &text [range . clone ()];
    let is_inline : bool = ( span_text . starts_with ('=') || span_text . starts_with ('~') )
      && ! span_text . contains ('\n') && span_text . len () >= 3;
    if ! is_inline || ! is_free (range, taken) { continue; }
    let inner : &str = &span_text [1 .. span_text . len () - 1];
    if inner . contains ('`') { tally . decline (WHY_VERBATIM_TICK); continue; }
    if inner . contains ("%%") { tally . decline (WHY_VERBATIM_COMMENT); continue; }
    tally . convert (KIND_VERBATIM);
    spans . push (( range . clone (), Span::Written (
      if span_text . starts_with ('=') { format! ("``{}``", inner) }
      else { format! ("`` {} ``", inner) } ) )); }
  for captures in ORG_LINK . captures_iter (text) {
    let whole : Range<usize> = captures . get (0) . unwrap () . range ();
    let target : &str = &captures [1];
    if target . starts_with ("id:") || ! is_free (&whole, taken)
       || ! is_free (&whole, &literal) { continue; }
    let label : Option<&str> = captures . get (2) . map ( |m| m . as_str () );
    match web_link_written (target, label) {
      Ok (written) => {
        tally . convert (KIND_WEB_LINK);
        spans . push ((whole, Span::Written (written))); },
      Err (why) => tally . decline (why), }}
  spans }

fn web_link_written (
  url   : &str,
  label : Option<&str>,
) -> Result<String, &'static str> {
  let url_chars : Vec<char> = url . chars () . collect ();
  if ! starts_a_web_address (&url_chars) { return Err (WHY_LINK_NOT_WEB); }
  let url_fits : bool = ! url . contains ( |c : char|
      c . is_whitespace () || "<>\\`$" . contains (c) )
    && ! url . contains ("%%");
  if ! url_fits { return Err (WHY_LINK_CHARACTERS); }
  let Some (label) = label else { return Ok ( format! ("<{}>", url) ); };
  let label_fits : bool = ! label . is_empty ()
    && ! label . contains ( |c : char| "[]`<>\\$%^|\n" . contains (c) );
  if ! label_fits { return Err (WHY_LINK_CHARACTERS); }
  Ok ( if url . contains ( |c : char| c == '(' || c == ')' ) { format! ("[{}](<{}>)", label, url) }
       else { format! ("[{}]({})", label, url) } ) }

/// The code spans of the text itself, '`...`' on one line, copied, so
/// that what is inside shows without escapes.
pub fn code_spans (
  text  : &str,
  taken : &[Range<usize>],
) -> Spans {
  let mut spans : Spans = Vec::new ();
  let bytes : &[u8] = text . as_bytes ();
  for line in line_ranges (text) {
    let mut p : usize = line . start;
    while p < line . end {
      if bytes [p] != b'`' { p += 1; continue; }
      let run_ends : usize = (p .. line . end) . find ( |j| bytes [*j] != b'`' ) . unwrap_or (line . end);
      if run_ends - p != 1 { p = run_ends; continue; }
      let Some (q) = (p + 1 .. line . end) . find ( |j| bytes [*j] == b'`' ) else { break; };
      let closes_alone : bool = bytes . get (q + 1) != Some (&b'`');
      let range : Range<usize> = p .. q + 1;
      if closes_alone && q > p + 1 && ! text [range . clone ()] . contains ("%%")
         && is_free (&range, taken) {
        spans . push ((range, Span::Copied));
        p = q + 1; }
      else { p += 1; }}}
  spans }

/// '*bold*' and '/italic/', on one line, by Org's rules for where a
/// marker may open and close; each marker becomes its own span.
pub fn emphasis_spans (
  text  : &str,
  taken : &[Range<usize>],
  tally : &mut MarkupTally,
) -> Spans {
  let literal : Vec<Range<usize>> = org_literal_ranges (text);
  let mut blocked : Vec<Range<usize>> = taken . to_vec ();
  blocked . extend (literal);
  let mut pairs : Vec<Range<usize>> = Vec::new (); // each from its opener through its closer
  let mut spans : Spans = Vec::new ();
  let chars : Vec<(usize, char)> = text . char_indices () . collect ();
  for (marker, kind, written) in [ ('*', KIND_BOLD, "**"), ('/', KIND_ITALIC, "*") ] {
    let mut i : usize = 0;
    while i < chars . len () {
      let (at, c) : (usize, char) = chars [i];
      let opens : bool = c == marker
        && ( i == 0 || { let b : char = chars [i - 1] . 1;
                         b . is_whitespace () || VERBATIM_PRE . contains (b) } )
        && chars . get (i + 1) . is_some_and ( |(_, n)| ! n . is_whitespace () );
      if ! opens { i += 1; continue; }
      let close : Option<usize> = ( i + 2 .. chars . len () )
        . take_while ( |j| chars [*j] . 1 != '\n' )
        . find ( |j| chars [*j] . 1 == marker
                     && ! chars [*j - 1] . 1 . is_whitespace ()
                     && chars . get (*j + 1) . is_none_or ( |(_, a)|
                          a . is_whitespace () || VERBATIM_POST . contains (*a) ) );
      let Some (j) = close else { i += 1; continue; };
      let pair : Range<usize> = at .. chars [j] . 0 + 1;
      let inside : Range<usize> = at + 1 .. chars [j] . 0;
      let is_inside = |t : &Range<usize>| -> bool {
        inside . start <= t . start && t . end <= inside . end };
      let fits : bool =
        blocked . iter () . all ( |t| ! overlaps (t, &pair) || is_inside (t) )
        && pairs . iter () . all ( |t| ! overlaps (t, &pair) || is_inside (t)
                                       || ( t . start < pair . start && pair . end < t . end ) );
      if ! fits { i += 1; continue; }
      tally . convert (kind);
      spans . push ((at .. at + 1, Span::Written (written . to_string ())));
      spans . push ((chars [j] . 0 .. chars [j] . 0 + 1, Span::Written (written . to_string ())));
      pairs . push (pair);
      i = j + 1; }}
  spans }
