//! Reversible escape for body lines that look like editable view headlines.

pub(crate) fn encode_body_for_view (
  body : &str,
) -> String {
  body . split_inclusive ('\n') . map (|line| {
    if looks_like_headline_after_commas (line) {
      format! (",{}", line)
    } else { line . to_string () }
  }) . collect ()
}

pub(crate) fn decode_body_line_from_view (
  line : &str,
) -> String {
  if line . starts_with (',') && looks_like_headline_after_commas (line) {
    line [1..] . to_string ()
  } else { line . to_string () }
}

fn looks_like_headline_after_commas (
  line : &str,
) -> bool {
  let rest : &str = line . trim_start_matches (',');
  let stars : usize = rest . bytes () . take_while (|byte| *byte == b'*') . count ();
  if stars == 0 { return false; }
  let after_stars : &str = &rest [stars..];
  after_stars . starts_with (char::is_whitespace) &&
    ! after_stars . trim () . is_empty ()
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn escape_is_reversible_for_literal_and_already_comma_escaped_headings () {
    for body in ["* a\n", "** code heading\n", ",* literal comma\n",
      ",,* two commas\n", "ordinary\n"] {
      let encoded : String = encode_body_for_view (body);
      let decoded : String = encoded . split_inclusive ('\n')
        .map (decode_body_line_from_view) . collect ();
      assert_eq! (decoded, body); }
  }
}
