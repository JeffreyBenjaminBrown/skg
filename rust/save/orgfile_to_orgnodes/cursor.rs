// Line cursor and heading parsing utilities.

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
pub fn parse_metadata_plus_heading (
  line: &str
) -> Option<( usize,    // level
              &str )> { // metadata + heading

  // Scan asterisks to count level.
  let mut i: usize = 0; // index into the byte slice
  let bytes: &[u8] = line.as_bytes(); // raw bytes for cheap `*` checks
  while i < bytes.len() && bytes[i] == b'*' { i += 1; }
  if i == 0 { return None; } // no stars => not a heading

  // Allow optional spaces/tabs between stars and content.
  let rest0: &str = &line[i..]; // slice after leading stars
  let rest: &str = rest0.trim_start_matches(
    |c: char|
    c == ' ' || c == '\t' );

  if rest.trim().is_empty() {
    // Not a true heading, just asterisks and whitespace.
    return None; }
  Some ((i, rest)) }

/// Look ahead and return the level of the next heading, if any.
pub fn peek_heading_level (
  cur : &LineCursor
) -> Option <usize> {
  let level_opt: Option<usize> =
    cur . peek() . and_then (
      |l: &str|
      parse_metadata_plus_heading (l) // TODO: Inefficient. `parse_metadata_plus_heading` ought to be run only once per heading.
        . map ( |(lvl, _)| lvl ));
  level_opt }

/// Collects the body lines that follow some (already consumed)
/// org headline. Advances the cursor to the next org headline.
pub fn collect_body_lines (
  cur: &mut LineCursor
) -> Option<String> {
  let mut acc: Vec<&str> = Vec::new();
  while let Some (line) = cur.peek () {
    if parse_metadata_plus_heading (line) . is_some () {
      break; } // body ends, next heading starts
    let taken: &str = cur . bump() . unwrap();
    acc.push (taken); }
  if acc.is_empty() { None
  } else {
    let joined: String = // preserve original line breaks
      acc.join("\n");
    Some(joined) }}

/// Any text preceding the buffer's first headline is ignored.
pub fn skip_until_first_heading (
  cur: &mut LineCursor
) {

  while let Some(line) = cur.peek() {
    if parse_metadata_plus_heading(line).is_some() {
      break; }
    let _ignored: Option<&str> = cur.bump();
  }}
