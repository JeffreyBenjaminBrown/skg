// Line cursor and headline parsing utilities.

// TODO: This still isn't as efficient as I would like. `count_headline_level` and `parse_metadata_plus_headline` are called multiple times for the same line.

#[derive(Debug)]
pub struct LineCursor<'a> { /// Line iterator with lookahead.
  lines: Vec<&'a str>, // Source text, without trailing newlines.
  idx: usize, // An index into `lines`.
}

impl<'a> LineCursor<'a> {
  pub fn new ( document: &'a str ) -> Self {
    let lines: Vec<&str> = document . lines() . collect();
    let idx: usize = 0;
    Self { lines, idx }}

  /// Non-consuming look at current line.
  /// Return `None` at EOF.
  pub fn peek (&self) -> Option<&str> {
    let line_opt: Option<&str> =
      self.lines.get ( self.idx ). copied();
    line_opt }

  /// Consume and return the current line.
  /// Return `None` at EOF.
  pub fn bump ( &mut self ) -> Option <&'a str> {
    let line: Option<&'a str> =
      self.lines.get ( self.idx ). copied();
    if line.is_some() { // Advance only if next line exists.
      self.idx += 1; }
    line }
}

/// Strips the bullet and leading whitespace,
/// leaving the rest as a single string.
pub fn parse_metadata_plus_headline (
  line: &str
) -> Option<( usize,    // level
              &str )> { // metadata + headline
  let level: usize =
    count_headline_level (line)?;
  let rest0: &str = // drop leading stars
    &line[level..];
  let rest: &str = rest0.trim_start_matches (
    // drop leading space
    |c: char|
    c == ' ' || c == '\t' );
  Some ((level, rest)) }

/// Counts leading asterisks and verifies headline format.
/// Returns the headline level (number of asterisks),
/// or None if it's not a headline.
pub fn count_headline_level (
  line: &str
) -> Option<usize> {

  let mut i: usize = 0;
  let bytes: &[u8] = line.as_bytes();
  while i < bytes.len() && bytes[i] == b'*' {
    i += 1; } // count asterisks
  if i == 0 { return None; } // no stars => not a headline
  if i >= bytes.len() || (bytes[i] != b' ' && bytes[i] != b'\t') {
    return None; } // At least one whitespace must follow asterisks.
  let rest0: &str = &line[i..];
  let rest: &str = // Skip any remaining leading spaces/tabs
    rest0.trim_start_matches(
      |c: char| c == ' ' || c == '\t' );
  if rest.trim().is_empty() {
    // Not a true headline, just asterisks and whitespace.
    return None; }
  Some (i) }

/// Look ahead and return the level of the next headline, if any.
pub fn peek_headline_level (
  cur : &LineCursor
) -> Option <usize> {
  cur . peek() . and_then ( count_headline_level )
}

/// Collects the body lines that follow some (already consumed)
/// org headline. Advances the cursor to the next org headline.
pub fn collect_body_lines (
  cur: &mut LineCursor
) -> Option<String> {
  let mut acc: Vec<&str> = Vec::new();
  while let Some (line) = cur.peek () {
    if count_headline_level (line) . is_some () {
      break; } // body ends, next headline starts
    let taken: &str = cur . bump() . unwrap();
    acc.push (taken); }
  if acc.is_empty() { None
  } else {
    let joined: String = // preserve original line breaks
      acc.join("\n");
    Some(joined) }}

/// Any text preceding the buffer's first headline is ignored.
pub fn skip_until_first_headline (
  cur: &mut LineCursor
) {

  while let Some(line) = cur.peek() {
    if count_headline_level(line).is_some() {
      break; }
    let _ignored: Option<&str> = cur.bump();
  }}
