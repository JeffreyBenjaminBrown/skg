use crate::types::misc::{ID, SourceName};
use crate::types::skgnode::SkgNode;

use std::path::Path;

//
// Intermediate data structures
//

struct OrgSection {
  level         : usize,          // 0 = file-level, 1 = *, 2 = **, ...
  headline_line : usize,          // line index of the headline (0 for file-level)
  headline      : String,         // text after "* ", or #+title: value
  id            : Option<String>,
  roam_aliases  : Option<Vec<String>>,
  body_start    : usize,          // first line of body (after headline + props)
}

struct SectionTree {
  section    : OrgSection,
  extent_end : usize,             // exclusive end of this section's extent
  children   : Vec<SectionTree>,
}

//
// Public API
//

pub fn parse_org_file (
  path : &Path,
) -> Vec<SkgNode> {
  let contents : String = match std::fs::read_to_string (path) {
    Ok (s) => s,
    Err (_) => return vec![], };
  if contents . is_empty() { return vec![]; }
  let lines : Vec<String> =
    contents . lines() . map (|l| l . to_string() ) . collect();
  let sections : Vec<OrgSection> =
    extract_sections (&lines, path);
  if sections . is_empty() { return vec![]; }
  // The file-level section must have an :ID:.
  if sections[0] . id . is_none() { return vec![]; }
  let forest : Vec<SectionTree> =
    build_section_forest (sections, lines . len());
  let mut nodes : Vec<SkgNode> = Vec::new();
  for st in &forest {
    collect_skgnodes (st, &lines, &mut nodes); }
  nodes }

//
// Section extraction
//

fn extract_sections (
  lines : &[String],
  path  : &Path,
) -> Vec<OrgSection> {
  let mut sections : Vec<OrgSection> = Vec::new();
  // File-level section (level 0)
  let (file_id, file_aliases, props_end)
    : (Option<String>, Option<Vec<String>>, usize)
    = parse_properties_block (lines, 0);
  let (file_title, title_line_end)
    : (String, usize)
    = find_title (lines, props_end, path);
  sections . push ( OrgSection {
    level         : 0,
    headline_line : 0,
    headline      : file_title,
    id            : file_id,
    roam_aliases  : file_aliases,
    body_start    : title_line_end, });
  // Headline sections
  for (i, line) in lines . iter() . enumerate() {
    if ! is_headline (line) { continue; }
    let level : usize = headline_level (line);
    let headline_text : String = headline_title (line);
    let (hl_id, hl_aliases, hl_props_end)
      : (Option<String>, Option<Vec<String>>, usize)
      = parse_properties_block (lines, i + 1);
    let body_start : usize =
      if hl_props_end > i + 1 { hl_props_end }
      else { i + 1 };
    sections . push ( OrgSection {
      level,
      headline_line : i,
      headline      : headline_text,
      id            : hl_id,
      roam_aliases  : hl_aliases,
      body_start, }); }
  sections }

//
// Section tree building
//

fn build_section_forest (
  sections  : Vec<OrgSection>,
  num_lines : usize,
) -> Vec<SectionTree> {
  // Build the tree using a stack. Each stack entry is a partially
  // built SectionTree whose extent_end is not yet known.
  // When a section at same-or-higher level arrives, we pop and
  // finalize the extent_end of everything deeper.
  let mut forest : Vec<SectionTree> = Vec::new();
  let mut stack : Vec<SectionTree> = Vec::new();
  for (idx, section) in sections . into_iter() . enumerate() {
    let level : usize = section . level;
    let _ : usize = idx; // suppress unused warning
    // Pop everything at same or deeper level
    loop {
      let should_pop : bool =
        match stack . last() {
          Some (top) => top . section . level >= level,
          None       => false, };
      if ! should_pop { break; }
      let mut popped : SectionTree = stack . pop() . unwrap();
      // The popped node's extent ends at this section's headline_line
      popped . extent_end = section . headline_line;
      if let Some (parent) = stack . last_mut() {
        parent . children . push (popped); }
      else {
        forest . push (popped); }}
    stack . push ( SectionTree {
      section,
      extent_end : num_lines, // default; will be updated when popped
      children   : Vec::new(), }); }
  // Drain remaining stack — their extent goes to EOF
  while let Some (popped) = stack . pop() {
    // extent_end is already num_lines (set at creation)
    if let Some (parent) = stack . last_mut() {
      parent . children . push (popped); }
    else {
      forest . push (popped); }}
  forest }

//
// SkgNode collection
//

fn collect_skgnodes (
  tree  : &SectionTree,
  lines : &[String],
  out   : &mut Vec<SkgNode>,
) {
  if tree . section . id . is_some() {
    let node : SkgNode = skgnode_from_section_tree (tree, lines);
    out . push (node); }
  // Recurse into children regardless —
  // a non-:ID: section might have :ID: descendants.
  for child in &tree . children {
    collect_skgnodes (child, lines, out); }}

fn skgnode_from_section_tree (
  tree  : &SectionTree,
  lines : &[String],
) -> SkgNode {
  let id_str : &str =
    tree . section . id . as_ref() . unwrap();
  // Collect :ID:-bearing children (transitively through non-:ID: intermediaries)
  let mut contained_ids : Vec<ID> = Vec::new();
  for child in &tree . children {
    collect_contained_ids (child, &mut contained_ids); }
  // Collect the line ranges of :ID:-bearing descendants to exclude from body.
  let mut excluded_ranges : Vec<(usize, usize)> = Vec::new();
  for child in &tree . children {
    collect_id_descendant_ranges (child, &mut excluded_ranges); }
  // Collect body: lines in [body_start..extent_end) not in excluded ranges.
  let body : Option<String> =
    collect_body (lines, tree . section . body_start,
                  tree . extent_end, &excluded_ranges);
  let aliases : Option<Vec<String>> =
    tree . section . roam_aliases . clone();
  SkgNode {
    title    : tree . section . headline . clone(),
    aliases,
    source   : SourceName::default(),
    ids      : vec![ ID::new (id_str) ],
    body,
    contains :
      if contained_ids . is_empty() { None }
      else { Some (contained_ids) },
    subscribes_to                : None,
    hides_from_its_subscriptions : None,
    overrides_view_of            : None, }}

/// Collect IDs of :ID:-bearing descendants, transitively through
/// non-:ID: intermediaries. Only the nearest :ID: descendants are
/// collected — deeper :ID: sections are contained by their closest
/// :ID: ancestor.
fn collect_contained_ids (
  tree : &SectionTree,
  out  : &mut Vec<ID>,
) {
  if tree . section . id . is_some() {
    let id_str : &str =
      tree . section . id . as_ref() . unwrap();
    out . push ( ID::new (id_str) ); }
  else {
    // No :ID: — look through to its children
    for child in &tree . children {
      collect_contained_ids (child, out); }}}

/// Collect the [headline_line, extent_end) ranges of all
/// :ID:-bearing descendant sections (at any depth).
fn collect_id_descendant_ranges (
  tree : &SectionTree,
  out  : &mut Vec<(usize, usize)>,
) {
  if tree . section . id . is_some() {
    // The entire extent of this :ID: section is excluded
    // from the parent's body.
    out . push ( (tree . section . headline_line,
                  tree . extent_end) ); }
  else {
    // Non-:ID: section — recurse into children
    // (the non-:ID: section's own lines stay in the parent's body)
    for child in &tree . children {
      collect_id_descendant_ranges (child, out); }}}

/// Collect body text from lines[start..end), excluding any lines
/// whose index falls in one of the excluded ranges.
fn collect_body (
  lines           : &[String],
  start           : usize,
  end             : usize,
  excluded_ranges : &[( usize, usize )],
) -> Option<String> {
  let mut body_lines : Vec<&str> = Vec::new();
  for i in start .. end . min (lines . len()) {
    let excluded : bool =
      excluded_ranges . iter() . any (
        |(ex_start, ex_end)| i >= *ex_start && i < *ex_end );
    if ! excluded {
      body_lines . push (&lines[i]); }}
  // Trim trailing empty lines
  while body_lines . last() . map_or (false, |l| l . trim() . is_empty() ) {
    body_lines . pop(); }
  if body_lines . is_empty() { None }
  else { Some (body_lines . join ("\n") ) }}

//
// Properties parsing
//

fn parse_properties_block (
  lines : &[String],
  start : usize,
) -> (Option<String>, Option<Vec<String>>, usize) {
  // Look for :PROPERTIES: at or after start (allowing leading whitespace)
  let mut i : usize = start;
  if i >= lines . len() { return (None, None, start); }
  if ! lines[i] . trim() . eq_ignore_ascii_case (":PROPERTIES:") {
    return (None, None, start); }
  i += 1; // skip :PROPERTIES: line
  let mut id : Option<String> = None;
  let mut aliases : Option<Vec<String>> = None;
  while i < lines . len() {
    let trimmed : &str = lines[i] . trim();
    if trimmed . eq_ignore_ascii_case (":END:") {
      return (id, aliases, i + 1); }
    if let Some (value) = extract_property (trimmed, ":ID:") {
      id = Some (value . trim() . to_string()); }
    if let Some (value) = extract_property (trimmed, ":ROAM_ALIASES:") {
      aliases = Some (parse_roam_aliases (value . trim() )); }
    i += 1; }
  (id, aliases, i) }

fn extract_property <'a> (
  line   : &'a str,
  prefix : &str,
) -> Option<&'a str> {
  let line_trimmed : &str = line . trim();
  if line_trimmed . len() < prefix . len() { return None; }
  if line_trimmed[ .. prefix . len() ]
      . eq_ignore_ascii_case (prefix) {
    Some ( &line_trimmed[ prefix . len() .. ] ) }
  else { None }}

/// Parse :ROAM_ALIASES: value.
/// Format: space-separated tokens; multi-word aliases are double-quoted.
/// Example: "machine learning" energy "dark matter"
/// Produces: ["machine learning", "energy", "dark matter"]
pub fn parse_roam_aliases (
  value : &str,
) -> Vec<String> {
  let mut aliases : Vec<String> = Vec::new();
  let mut chars : std::iter::Peekable<std::str::Chars> =
    value . chars() . peekable();
  loop {
    // Skip whitespace
    while chars . peek() == Some (&' ')
      || chars . peek() == Some (&'\t') {
      chars . next(); }
    if chars . peek() . is_none() { break; }
    if chars . peek() == Some (&'"') {
      // Quoted alias
      chars . next(); // consume opening quote
      let mut alias : String = String::new();
      loop {
        match chars . next() {
          Some ('"') => break,
          Some (c)   => alias . push (c),
          None       => break, }}
      if ! alias . is_empty() {
        aliases . push (alias); }}
    else {
      // Unquoted alias — runs to next whitespace
      let mut alias : String = String::new();
      loop {
        match chars . peek() {
          Some (&' ') | Some (&'\t') | None => break,
          _ => alias . push (chars . next() . unwrap() ), }}
      if ! alias . is_empty() {
        aliases . push (alias); }}}
  aliases }

//
// Headline parsing
//

fn is_headline (
  line : &str,
) -> bool {
  // A headline starts with one or more '*' followed by a space.
  // Do not trim leading whitespace — org headlines must start at column 0.
  if ! line . starts_with ('*') { return false; }
  let stars : usize =
    line . chars() . take_while (|c| *c == '*') . count();
  line . len() > stars && line . as_bytes()[stars] == b' ' }

fn headline_level (
  line : &str,
) -> usize {
  line . chars()
    . take_while (|c| *c == '*')
    . count() }

fn headline_title (
  line : &str,
) -> String {
  let stars : usize =
    line . chars() . take_while (|c| *c == '*') . count();
  line[stars ..] . trim_start() . to_string() }

/// Find #+title: line (case-insensitive) in the file-level section.
/// Returns (title, line after title).
/// If not found, uses filename stem as title.
fn find_title (
  lines     : &[String],
  props_end : usize,
  path      : &Path,
) -> (String, usize) {
  for i in props_end .. lines . len() {
    let trimmed : &str = lines[i] . trim();
    if trimmed . to_lowercase() . starts_with ("#+title:") {
      let title : String =
        trimmed["#+title:" . len() ..] . trim() . to_string();
      return (title, i + 1); }
    // Stop looking if we hit a headline
    if is_headline (&lines[i]) { break; }}
  // No title found — use filename stem
  let stem : String = path
    . file_stem()
    . map (|s| s . to_string_lossy() . to_string() )
    . unwrap_or_else (|| "untitled" . to_string() );
  (stem, props_end) }

//
// Tests
//

#[cfg(test)]
mod tests {
  use super::*;
  use std::io::Write;

  fn write_temp_org (
    content : &str,
    name    : &str,
  ) -> tempfile::NamedTempFile {
    let mut f : tempfile::NamedTempFile =
      tempfile::Builder::new()
      . prefix (name)
      . suffix (".org")
      . tempfile()
      . unwrap();
    f . write_all (content . as_bytes() ) . unwrap();
    f }

  #[test]
  fn test_minimal_file () {
    let content : &str = "\
:PROPERTIES:
:ID:       abc-123
:END:
#+title: My Node
";
    let f : tempfile::NamedTempFile =
      write_temp_org (content, "minimal");
    let nodes : Vec<SkgNode> =
      parse_org_file (f . path());
    assert_eq! (nodes . len(), 1);
    assert_eq! (nodes[0] . title, "My Node");
    assert_eq! (nodes[0] . ids, vec![ ID::new ("abc-123") ]);
    assert! (nodes[0] . body . is_none());
    assert! (nodes[0] . contains . is_none()); }

  #[test]
  fn test_file_with_body () {
    let content : &str = "\
:PROPERTIES:
:ID:       abc-123
:END:
#+title: My Node
Some body text here.
More body text.
";
    let f : tempfile::NamedTempFile =
      write_temp_org (content, "with-body");
    let nodes : Vec<SkgNode> =
      parse_org_file (f . path());
    assert_eq! (nodes . len(), 1);
    assert_eq! (nodes[0] . body . as_deref(),
                Some ("Some body text here.\nMore body text.") ); }

  #[test]
  fn test_nested_id_headlines () {
    let content : &str = "\
:PROPERTIES:
:ID:       file-id
:END:
#+title: Parent
* child one
  :PROPERTIES:
  :ID:       child-1
  :END:
  Child one body.
* child two
  :PROPERTIES:
  :ID:       child-2
  :END:
  Child two body.
";
    let f : tempfile::NamedTempFile =
      write_temp_org (content, "nested");
    let nodes : Vec<SkgNode> =
      parse_org_file (f . path());
    assert_eq! (nodes . len(), 3);
    // File-level node
    assert_eq! (nodes[0] . title, "Parent");
    assert_eq! (nodes[0] . ids, vec![ ID::new ("file-id") ]);
    let contained : &Vec<ID> =
      nodes[0] . contains . as_ref() . unwrap();
    assert_eq! (contained . len(), 2);
    assert_eq! (contained[0], ID::new ("child-1"));
    assert_eq! (contained[1], ID::new ("child-2"));
    assert! (nodes[0] . body . is_none());
    // Child nodes
    assert_eq! (nodes[1] . title, "child one");
    assert_eq! (nodes[1] . body . as_deref(),
                Some ("  Child one body.") );
    assert_eq! (nodes[2] . title, "child two");
    assert_eq! (nodes[2] . body . as_deref(),
                Some ("  Child two body.") ); }

  #[test]
  fn test_non_id_headline_folded_into_body () {
    let content : &str = "\
:PROPERTIES:
:ID:       file-id
:END:
#+title: Parent
* just a regular headline
  Some text under it.
** sub-headline
   More text.
";
    let f : tempfile::NamedTempFile =
      write_temp_org (content, "non-id-hl");
    let nodes : Vec<SkgNode> =
      parse_org_file (f . path());
    assert_eq! (nodes . len(), 1);
    assert_eq! (nodes[0] . title, "Parent");
    // The non-:ID: headline and its contents become body
    let body : &str = nodes[0] . body . as_ref() . unwrap();
    assert! (body . contains ("just a regular headline"));
    assert! (body . contains ("Some text under it."));
    assert! (body . contains ("sub-headline"));
    assert! (body . contains ("More text.")); }

  #[test]
  fn test_id_headline_under_non_id_headline () {
    // :ID: child under a non-:ID: intermediary is still
    // contained by the nearest :ID: ancestor.
    let content : &str = "\
:PROPERTIES:
:ID:       file-id
:END:
#+title: Root
* intermediary (no ID)
** actual child
   :PROPERTIES:
   :ID:       deep-child
   :END:
   Deep child body.
";
    let f : tempfile::NamedTempFile =
      write_temp_org (content, "transitive");
    let nodes : Vec<SkgNode> =
      parse_org_file (f . path());
    assert_eq! (nodes . len(), 2);
    // Root should contain deep-child transitively
    let contained : &Vec<ID> =
      nodes[0] . contains . as_ref() . unwrap();
    assert_eq! (contained, &vec![ ID::new ("deep-child") ]);
    // The intermediary headline is in root's body
    let body : &str = nodes[0] . body . as_ref() . unwrap();
    assert! (body . contains ("intermediary (no ID)"));
    // But the deep-child section is NOT in root's body
    assert! (! body . contains ("Deep child body.")); }

  #[test]
  fn test_roam_aliases () {
    let content : &str = "\
:PROPERTIES:
:ID:       abc-123
:ROAM_ALIASES: \"machine learning\" energy \"dark matter\"
:END:
#+title: Physics
";
    let f : tempfile::NamedTempFile =
      write_temp_org (content, "aliases");
    let nodes : Vec<SkgNode> =
      parse_org_file (f . path());
    assert_eq! (nodes . len(), 1);
    let aliases : &Vec<String> =
      nodes[0] . aliases . as_ref() . unwrap();
    assert_eq! (aliases, &vec![
      "machine learning", "energy", "dark matter" ]); }

  #[test]
  fn test_empty_file () {
    let f : tempfile::NamedTempFile =
      write_temp_org ("", "empty");
    let nodes : Vec<SkgNode> =
      parse_org_file (f . path());
    assert! (nodes . is_empty()); }

  #[test]
  fn test_file_without_id () {
    let content : &str = "\
#+title: No ID here
Some text.
";
    let f : tempfile::NamedTempFile =
      write_temp_org (content, "no-id");
    let nodes : Vec<SkgNode> =
      parse_org_file (f . path());
    assert! (nodes . is_empty()); }

  #[test]
  fn test_missing_title_uses_filename () {
    let content : &str = "\
:PROPERTIES:
:ID:       abc-123
:END:
";
    let f : tempfile::NamedTempFile =
      write_temp_org (content, "my_note");
    let nodes : Vec<SkgNode> =
      parse_org_file (f . path());
    assert_eq! (nodes . len(), 1);
    // Title should be derived from filename
    // (tempfile adds random chars, so just check it's not empty)
    assert! (! nodes[0] . title . is_empty()); }

  #[test]
  fn test_parse_roam_aliases_mixed () {
    let result : Vec<String> =
      parse_roam_aliases ("\"machine learning\" energy \"dark matter\"");
    assert_eq! (result, vec![
      "machine learning", "energy", "dark matter" ]); }

  #[test]
  fn test_parse_roam_aliases_unquoted_only () {
    let result : Vec<String> =
      parse_roam_aliases ("energy power force");
    assert_eq! (result, vec![ "energy", "power", "force" ]); }

  #[test]
  fn test_parse_roam_aliases_empty () {
    let result : Vec<String> = parse_roam_aliases ("");
    assert! (result . is_empty()); }

  #[test]
  fn test_is_headline () {
    assert! (is_headline ("* foo"));
    assert! (is_headline ("** bar"));
    assert! (is_headline ("*** baz quux"));
    assert! (! is_headline ("not a headline"));
    assert! (! is_headline ("*bold*"));
    assert! (! is_headline ("")); }

  #[test]
  fn test_headline_level () {
    assert_eq! (headline_level ("* foo"),   1);
    assert_eq! (headline_level ("** bar"),  2);
    assert_eq! (headline_level ("*** baz"), 3); }

  #[test]
  fn test_headline_title () {
    assert_eq! (headline_title ("* hello world"), "hello world");
    assert_eq! (headline_title ("** sub"), "sub"); }

  #[test]
  fn test_power_org_structure () {
    // Simulates the structure of power.org:
    // file-level :ID:, with some :ID: headlines and some without.
    let content : &str = "\
:PROPERTIES:
:ID:       b9775088-1bd9-490f-a062-c6cfd189b65d
:ROAM_ALIASES: energy power force work strength
:END:
#+title: energy
* see also [[id:80cfe814][constraint]]
* the feeling of forcing it
  :PROPERTIES:
  :ID:       1cd8051b-95ee-4f73-a05e-624200b52c90
  :END:
** It might be entirely avoidable.
** It might be a useful last resort sometimes.
* etymology : sociology, physics
  Power implies choice; energy, only possibility.
";
    let f : tempfile::NamedTempFile =
      write_temp_org (content, "power");
    let nodes : Vec<SkgNode> =
      parse_org_file (f . path());
    // Should produce 2 nodes: file-level and "the feeling of forcing it"
    assert_eq! (nodes . len(), 2);
    // File-level node
    assert_eq! (nodes[0] . title, "energy");
    assert_eq! (nodes[0] . aliases . as_ref() . unwrap(),
                &vec!["energy", "power", "force", "work", "strength"]);
    let contained : &Vec<ID> =
      nodes[0] . contains . as_ref() . unwrap();
    assert_eq! (contained . len(), 1);
    assert_eq! (contained[0],
                ID::new ("1cd8051b-95ee-4f73-a05e-624200b52c90"));
    // File body should include non-:ID: headlines but not :ID: sections
    let body : &str = nodes[0] . body . as_ref() . unwrap();
    assert! (body . contains ("see also"));
    assert! (body . contains ("etymology"));
    assert! (! body . contains ("entirely avoidable"));
    // :ID: child node
    assert_eq! (nodes[1] . title, "the feeling of forcing it");
    let child_body : &str = nodes[1] . body . as_ref() . unwrap();
    assert! (child_body . contains ("entirely avoidable"));
    assert! (child_body . contains ("useful last resort")); }
}
