//! Byte ranges of Org text that Org shows literally, so that
//! link-like text inside them is an example, not a link.

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
  if let Some ((block_start, _)) = open_block {
    ranges . push (block_start .. text . len ()); }
  ranges }

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
