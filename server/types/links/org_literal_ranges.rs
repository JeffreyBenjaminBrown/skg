//! Byte ranges of Org text that Org shows literally, so that
//! link-like text inside them is an example, not a link.
//! Both clients reimplement this; tests/shared/literal-link-cases.txt
//! holds the cases all three must agree on.

use std::fmt;
use std::ops::Range;

/// Characters Org accepts just before an opening '=' or '~'.
const VERBATIM_PRE : &str = "-('\"{";
/// Characters Org accepts just after a closing '=' or '~'.
const VERBATIM_POST : &str = "-.,:!?;'\")}\\[";

/// The literal ranges of 'text':
/// - '#+begin_X' ... '#+end_X' blocks (any X), delimiters included,
/// - Markdown fences (three or more backticks), delimiters included,
/// - fixed-width lines (': ...', or a lone ':'),
/// - inline '=verbatim=' and '~code~' spans within one line.
/// An unclosed block or fence runs to the end of 'text'.
pub fn org_literal_ranges (
  text : &str,
) -> Vec<Range<usize>> {
  org_literal_ranges_and_unclosed_block (text) . 0 }

/// As 'org_literal_ranges', plus where an unclosed block or fence
/// starts, if there is one.
pub fn org_literal_ranges_and_unclosed_block (
  text : &str,
) -> (Vec<Range<usize>>, Option<usize>) {
  let mut ranges : Vec<Range<usize>> = Vec::new ();
  let mut open_block : Option<(usize, String)> = None; // (start, closing line)
  let mut offset : usize = 0;
  for raw_line in text . split_inclusive ('\n') {
    let start : usize = offset;
    offset += raw_line . len ();
    let line : &str = raw_line . trim_end_matches ('\n') . trim_end_matches ('\r');
    let trimmed : &str = line . trim_start ();
    let lowered : String = trimmed . to_ascii_lowercase ();
    if let Some ((block_start, closing)) = &open_block {
      if lowered . starts_with (closing . as_str ()) {
        ranges . push (*block_start .. start + line . len ());
        open_block = None; }
      continue; }
    if let Some (kind) = lowered . strip_prefix ("#+begin_") {
      let kind : &str = kind . split_whitespace () . next () . unwrap_or ("");
      open_block = Some ((start, format! ("#+end_{}", kind)));
      continue; }
    if trimmed . starts_with ("```") {
      open_block = Some ((start, "```" . to_string ()));
      continue; }
    if trimmed == ":" || trimmed . starts_with (": ") {
      ranges . push (start .. start + line . len ());
      continue; }
    ranges . extend (
      inline_verbatim_ranges (line) . into_iter ()
      . map (|range| start + range . start .. start + range . end)); }
  let unclosed_block_start : Option<usize> =
    open_block . map ( |(block_start, _)| block_start );
  if let Some (block_start) = unclosed_block_start {
    ranges . push (block_start .. text . len ()); }
  (ranges, unclosed_block_start) }

/// Why both importers refuse a file with a 'HeadlineInsideBlock'.
pub const HEADLINES_INSIDE_BLOCKS_EXPLANATION : &str =
  "Nothing was imported. In these Org files, a line that looks like a heading \
is inside a #+begin_... block or a ``` fence. Org would read it as a heading \
and end the block there, so Skg cannot tell which was meant. Either close the \
block before the heading, or, if the line belongs inside the block, escape it \
with a leading comma (',* ...'), as Org does.";

/// A line Org would read as a headline (asterisks, then a space), lying
/// inside a block or fence of a whole Org file.
pub struct HeadlineInsideBlock {
  pub headline          : Range<usize>, // the line, without its newline
  pub headline_text     : String,
  pub block_line        : usize, // 1-based
  pub block_is_unclosed : bool,
}

impl fmt::Display for HeadlineInsideBlock {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>,
  ) -> fmt::Result {
    write! (f,
      "{:?} looks like a heading but is inside the block or fence that begins on line {}{}",
      self . headline_text, self . block_line,
      if self . block_is_unclosed { ", which is never closed" } else { "" } ) }}

/// Headline-like lines of 'text' inside its blocks and fences.
pub fn headlines_inside_blocks (
  text : &str,
) -> Vec<HeadlineInsideBlock> {
  let (literal, unclosed_block_start) : (Vec<Range<usize>>, Option<usize>) =
    org_literal_ranges_and_unclosed_block (text);
  let mut found : Vec<HeadlineInsideBlock> = Vec::new ();
  let mut offset : usize = 0;
  for raw_line in text . split_inclusive ('\n') {
    let start : usize = offset;
    offset += raw_line . len ();
    let line : &str = raw_line . trim_end_matches ('\n') . trim_end_matches ('\r');
    let stars : usize = line . bytes () . take_while (|b| *b == b'*') . count ();
    if stars == 0 || ! line [stars ..] . starts_with (' ') { continue; }
    if let Some (block) = literal . iter ()
      . find (|range| range . start < start && start < range . end) {
      found . push (HeadlineInsideBlock {
        headline          : start .. start + line . len (),
        headline_text     : line . to_string (),
        block_line        : 1 + text [.. block . start] . matches ('\n') . count (),
        block_is_unclosed : unclosed_block_start == Some (block . start), }); }}
  found }

/// '=verbatim=' and '~code~' spans in one line, following Org's
/// border rules: the opening marker follows the line start, whitespace
/// or a VERBATIM_PRE character, and precedes a non-space; the closing
/// marker follows a non-space and precedes the line end, whitespace or
/// a VERBATIM_POST character.
fn inline_verbatim_ranges (
  line : &str,
) -> Vec<Range<usize>> {
  let chars : Vec<(usize, char)> = line . char_indices () . collect ();
  let mut ranges : Vec<Range<usize>> = Vec::new ();
  let mut index : usize = 0;
  while index < chars . len () {
    let (open_at, marker) : (usize, char) = chars [index];
    let opens : bool =
      ( marker == '=' || marker == '~' )
      && ( index == 0 || {
           let before : char = chars [index - 1] . 1;
           before . is_whitespace () || VERBATIM_PRE . contains (before) } )
      && chars . get (index + 1)
         . is_some_and (|(_, next)| ! next . is_whitespace ());
    if ! opens { index += 1; continue; }
    let closing : Option<usize> = ( index + 2 .. chars . len () )
      . find (|&candidate| {
        let (_, ch) : (usize, char) = chars [candidate];
        ch == marker
        && ! chars [candidate - 1] . 1 . is_whitespace ()
        && chars . get (candidate + 1) . is_none_or (|(_, after)|
             after . is_whitespace () || VERBATIM_POST . contains (*after)) });
    match closing {
      Some (close_index) => {
        let close_end : usize = chars [close_index] . 0 + marker . len_utf8 ();
        ranges . push (open_at .. close_end);
        index = close_index + 1; },
      None => index += 1, }}
  ranges }

#[cfg(test)]
mod tests {
  use super::*;

  fn literal_texts <'a> (
    text : &'a str,
  ) -> Vec<&'a str> {
    org_literal_ranges (text) . into_iter ()
      . map (|range| &text [range]) . collect () }

  #[test]
  fn inline_verbatim_and_code_follow_org_border_rules () {
    assert_eq! (
      literal_texts ("a =[[id:X][LABEL]]= link, (~code~) and x=y=z"),
      vec! ["=[[id:X][LABEL]]=", "~code~"] );
    assert_eq! (literal_texts ("= not verbatim ="), Vec::<&str>::new ());
    assert_eq! (literal_texts ("=N='s title"), vec! ["=N="]);
  }

  #[test]
  fn blocks_fences_and_fixed_width_lines_are_literal () {
    let text : &str =
      "before\n#+BEGIN_SRC org\n[[id:a][b]]\n#+end_src\n: [[id:c][d]]\n```\n[[id:e][f]]\n```\nafter\n";
    assert_eq! (
      literal_texts (text),
      vec! ["#+BEGIN_SRC org\n[[id:a][b]]\n#+end_src",
            ": [[id:c][d]]",
            "```\n[[id:e][f]]\n```"] );
  }

  #[test]
  fn unclosed_block_runs_to_end () {
    assert_eq! (literal_texts ("x\n#+begin_example\n[[id:a][b]]"),
                vec! ["#+begin_example\n[[id:a][b]]"] );
  }
}
