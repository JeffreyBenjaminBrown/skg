// PURPOSE: Pre-processors that make literal text findable through
// Tantivy's QueryParser, or that preserve operator syntax while
// escaping only intra-word operator characters.

/// Pre-process `query` so titles and aliases containing Tantivy
/// operator characters are findable by typing the literal text.
///
/// Strategy: split on whitespace. Any word that contains an operator
/// char — or is one of the boolean keywords AND / OR / NOT — is
/// wrapped as a Tantivy phrase ("..."), which treats its contents as
/// literal content (tokenized, but not operator-interpreted). Words
/// with no specials pass through unchanged, preserving the OR-default
/// across terms that
/// [[../../../docs/COMMANDS.org][docs/COMMANDS.org]] advertises and
/// existing tests (e.g. `test_aliases`) rely on.
///
/// Why per-word phrasing rather than whole-query:
/// Wrapping the whole query as a phrase would collapse OR-default
/// into phrase-only (AND + adjacency). Per-word wrapping keeps OR
/// between words while making each word's contents literal.
///
/// Why phrase-wrap rather than backslash-escape each char:
/// Tantivy 0.19's QueryParser does not accept `\[` / `\]` etc.
/// outside phrases — it errors with SyntaxError. Phrase-wrapping is
/// the most reliable way to pass operator chars through the parser.
pub fn escape_tantivy_literal (
  query : &str,
) -> String {
  query
    . split_whitespace ()
    . map ( |word| {
      let needs_wrap : bool =
        word == "AND" || word == "OR" || word == "NOT" ||
        word . chars () . any ( is_tantivy_operator_char );
      if needs_wrap {
        wrap_as_phrase ( word )
      } else {
        word . to_string () }} )
    . collect::<Vec<String>> ()
    . join ( " " ) }

fn wrap_as_phrase (
  word : &str,
) -> String {
  let mut inner : String =
    String::with_capacity ( word . len () + 4 );
  for ch in word . chars () {
    if ch == '\\' || ch == '"' {
      inner . push ( '\\' ); }
    inner . push ( ch ); }
  format! ( "\"{}\"", inner ) }

/// Pre-process `query` for operators=true mode: preserve Tantivy's
/// operator syntax, but backslash-escape any operator character
/// sitting strictly inside a word (alphanumeric or underscore neighbors
/// on both sides within the same whitespace-delimited token).
///
/// Effect: =AND= / =OR= / =NOT= / =+foo= / =-bar= / =(..)= / phrase
/// quotes keep their operator meaning. =foo:bar= becomes =foo\:bar=
/// so the literal text is findable. =C++= stays =C++= because the
/// trailing =+= is not bounded by a word char on the right — it'll
/// be handled by QueryParser's tokenization rather than by escape.
///
/// LIMITATION: the heuristic can't tell a known field name from a
/// user word. =title_or_alias:dog= gets its =:= escaped too, so
/// field-qualified queries need manual =\:= or the user switches
/// back to operators=false.
pub fn escape_tantivy_intra_word (
  query : &str,
) -> String {
  let chars : Vec<char> = query . chars () . collect ();
  let mut out : String =
    String::with_capacity ( query . len () + 8 );
  for (i, &ch) in chars . iter () . enumerate () {
    if is_tantivy_operator_char (ch)
       && has_word_neighbor (&chars, i, false)
       && has_word_neighbor (&chars, i, true) {
      out . push ( '\\' ); }
    out . push (ch); }
  out }

/// True iff, scanning outward from index `i` within the current
/// whitespace-delimited token, the first non-operator non-whitespace
/// character is alphanumeric or underscore.
fn has_word_neighbor (
  chars   : &[char],
  i       : usize,
  forward : bool,
) -> bool {
  let range : Box<dyn Iterator<Item = usize>> =
    if forward { Box::new ( (i + 1) .. chars . len () ) }
    else       { Box::new ( (0 .. i) . rev () ) };
  for j in range {
    let c : char = chars [j];
    if c . is_whitespace () { return false; }
    if is_tantivy_operator_char (c) { continue; }
    return c . is_alphanumeric () || c == '_'; }
  false }

fn is_tantivy_operator_char (
  ch : char,
) -> bool {
  matches! ( ch,
    '+' | '-' | '&' | '|' | '!' | '(' | ')' | '{' | '}' |
    '[' | ']' | '^' | '"' | '~' | '*' | '?' | ':' | '\\' ) }
