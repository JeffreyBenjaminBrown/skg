use crate::types::misc::{ID, SourceName};
use crate::types::skgnode::{FileProperty, SkgNode};

use std::path::Path;
use uuid::Uuid;

//
// Intermediate data structures
//

struct OrgSection {
  level         : usize,          // 0 = file-level, 1 = *, 2 = **, ...
  headline_line : usize,          // line index of the headline (0 for file-level)
  headline      : String,         // text after "* ", or #+title: value
  id            : Option<String>,
  had_id        : bool,           // true if section already had an :ID: property
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
  let mut forest : Vec<SectionTree> =
    build_section_forest (sections, lines . len());
  for st in &mut forest {
    assign_missing_ids (st); }
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
  let file_had_id : bool = file_id . is_some ();
  sections . push ( OrgSection {
    level         : 0,
    headline_line : 0,
    headline      : file_title,
    id            : file_id,
    had_id        : file_had_id,
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
    let hl_had_id : bool = hl_id . is_some ();
    let body_start : usize =
      if hl_props_end > i + 1 { hl_props_end }
      else { i + 1 };
    sections . push ( OrgSection {
      level,
      headline_line : i,
      headline      : headline_text,
      id            : hl_id,
      had_id        : hl_had_id,
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

fn assign_missing_ids (
  tree : &mut SectionTree,
) {
  if tree . section . id . is_none() {
    tree . section . id = Some (Uuid::new_v4() . to_string()); }
  for child in &mut tree . children {
    assign_missing_ids (child); }}

fn collect_skgnodes (
  tree  : &SectionTree,
  lines : &[String],
  out   : &mut Vec<SkgNode>,
) {
  let node : SkgNode = skgnode_from_section_tree (tree, lines);
  out . push (node);
  for child in &tree . children {
    collect_skgnodes (child, lines, out); }}

fn skgnode_from_section_tree (
  tree  : &SectionTree,
  lines : &[String],
) -> SkgNode {
  // All sections have IDs after assign_missing_ids pre-pass.
  let id_str : &str =
    tree . section . id . as_ref() . unwrap();
  let contained_ids : Vec<ID> =
    tree . children . iter() . map (|child| {
      let cid : &str =
        child . section . id . as_ref() . unwrap();
      ID::new (cid)
    }) . collect();
  // Body: lines from body_start to first child's headline (or extent_end).
  let body_end : usize =
    tree . children . first()
      . map (|c| c . section . headline_line)
      . unwrap_or (tree . extent_end);
  let body : Option<String> =
    collect_body (lines, tree . section . body_start, body_end);
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
    overrides_view_of            : None,
    misc :
      if tree . section . had_id {
        vec![FileProperty::Had_ID_Before_Import] }
      else { Vec::new () }, }}

fn collect_body (
  lines : &[String],
  start : usize,
  end   : usize,
) -> Option<String> {
  let mut body_lines : Vec<&str> = Vec::new();
  for i in start .. end . min (lines . len()) {
    body_lines . push (&lines[i]); }
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
  fn test_non_id_headline_becomes_node () {
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
    // 3 nodes: file + headline + sub-headline
    assert_eq! (nodes . len(), 3);
    assert_eq! (nodes[0] . title, "Parent");
    assert_eq! (nodes[0] . ids, vec![ ID::new ("file-id") ]);
    let contained : &Vec<ID> =
      nodes[0] . contains . as_ref() . unwrap();
    assert_eq! (contained . len(), 1);
    assert! (nodes[0] . body . is_none());
    // Headline node (generated ID)
    assert_eq! (nodes[1] . title, "just a regular headline");
    assert_eq! (nodes[1] . body . as_deref(),
                Some ("  Some text under it.") );
    let hl_contained : &Vec<ID> =
      nodes[1] . contains . as_ref() . unwrap();
    assert_eq! (hl_contained . len(), 1);
    // Sub-headline node
    assert_eq! (nodes[2] . title, "sub-headline");
    assert_eq! (nodes[2] . body . as_deref(),
                Some ("   More text.") ); }

  #[test]
  fn test_id_headline_under_non_id_headline () {
    // Every headline is a node. file→intermediary→deep-child.
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
    // 3 nodes: file, intermediary, deep-child
    assert_eq! (nodes . len(), 3);
    // Root contains intermediary (generated ID)
    let root_contained : &Vec<ID> =
      nodes[0] . contains . as_ref() . unwrap();
    assert_eq! (root_contained . len(), 1);
    assert! (nodes[0] . body . is_none());
    // Intermediary contains deep-child
    assert_eq! (nodes[1] . title, "intermediary (no ID)");
    let mid_contained : &Vec<ID> =
      nodes[1] . contains . as_ref() . unwrap();
    assert_eq! (mid_contained . len(), 1);
    assert_eq! (mid_contained[0], ID::new ("deep-child"));
    // Deep child
    assert_eq! (nodes[2] . title, "actual child");
    assert_eq! (nodes[2] . ids, vec![ ID::new ("deep-child") ]);
    assert_eq! (nodes[2] . body . as_deref(),
                Some ("   Deep child body.") ); }

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
    // Every headline becomes a node: file + 5 headlines = 6 nodes.
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
    assert_eq! (nodes . len(), 6);
    // File-level node
    assert_eq! (nodes[0] . title, "energy");
    assert_eq! (nodes[0] . aliases . as_ref() . unwrap(),
                &vec!["energy", "power", "force", "work", "strength"]);
    let file_contained : &Vec<ID> =
      nodes[0] . contains . as_ref() . unwrap();
    assert_eq! (file_contained . len(), 3);
    // The :ID: child's contained ID is known
    assert_eq! (file_contained[1],
                ID::new ("1cd8051b-95ee-4f73-a05e-624200b52c90"));
    assert! (nodes[0] . body . is_none());
    // "see also" headline
    assert_eq! (nodes[1] . title,
                "see also [[id:80cfe814][constraint]]");
    assert! (nodes[1] . body . is_none());
    assert! (nodes[1] . contains . is_none());
    // "the feeling of forcing it" — has :ID: and 2 sub-headlines
    assert_eq! (nodes[2] . title, "the feeling of forcing it");
    assert_eq! (nodes[2] . ids,
                vec![ ID::new ("1cd8051b-95ee-4f73-a05e-624200b52c90") ]);
    let forcing_contained : &Vec<ID> =
      nodes[2] . contains . as_ref() . unwrap();
    assert_eq! (forcing_contained . len(), 2);
    assert! (nodes[2] . body . is_none());
    // Sub-headlines of "the feeling of forcing it"
    assert_eq! (nodes[3] . title, "It might be entirely avoidable.");
    assert_eq! (nodes[4] . title,
                "It might be a useful last resort sometimes.");
    // "etymology" headline
    assert_eq! (nodes[5] . title, "etymology : sociology, physics");
    assert_eq! (nodes[5] . body . as_deref(),
                Some ("  Power implies choice; energy, only possibility.") );
    assert! (nodes[5] . contains . is_none()); }

  #[test]
  fn test_music_and_consciousness () {
    let content : &str = "\
:PROPERTIES:
:ID:       01104862
:END:
#+title: music & consciousness
= things about consciousness that music highlights
* [[id:39029f2f][Effort and observation are somewhat disjunctive.]]
* [[id:681da8ea][Music illuminates (the?) infinite nature of want.]]
";
    let f : tempfile::NamedTempFile =
      write_temp_org (content, "music");
    let nodes : Vec<SkgNode> =
      parse_org_file (f . path());
    // 3 nodes: file + 2 headlines
    assert_eq! (nodes . len(), 3);
    // File node
    assert_eq! (nodes[0] . title, "music & consciousness");
    assert_eq! (nodes[0] . ids, vec![ ID::new ("01104862") ]);
    assert_eq! (nodes[0] . body . as_deref(),
                Some ("= things about consciousness that music highlights") );
    let contained : &Vec<ID> =
      nodes[0] . contains . as_ref() . unwrap();
    assert_eq! (contained . len(), 2);
    // Headline nodes — titles preserve the [[id:...][...]] links
    assert_eq! (nodes[1] . title,
                "[[id:39029f2f][Effort and observation are somewhat disjunctive.]]");
    assert! (nodes[1] . body . is_none());
    assert! (nodes[1] . contains . is_none());
    assert_eq! (nodes[2] . title,
                "[[id:681da8ea][Music illuminates (the?) infinite nature of want.]]");
    assert! (nodes[2] . body . is_none());
    assert! (nodes[2] . contains . is_none()); }

  #[test]
  fn test_had_id_before_import () {
    // Nodes with :ID: get Had_ID_Before_Import in misc.
    // Nodes without :ID: (assigned UUIDs) get empty misc.
    let content : &str = "\
:PROPERTIES:
:ID:       file-id
:END:
#+title: Parent
* child with ID
  :PROPERTIES:
  :ID:       child-id
  :END:
  Body.
* child without ID
  Just text.
";
    let f : tempfile::NamedTempFile =
      write_temp_org (content, "had-id");
    let nodes : Vec<SkgNode> =
      parse_org_file (f . path());
    assert_eq! (nodes . len(), 3);
    // File-level node had :ID: → Had_ID_Before_Import.
    assert_eq! (nodes[0] . misc,
                vec![FileProperty::Had_ID_Before_Import]);
    // Child with :ID: → Had_ID_Before_Import.
    assert_eq! (nodes[1] . misc,
                vec![FileProperty::Had_ID_Before_Import]);
    // Child without :ID: → empty misc.
    assert! (nodes[2] . misc . is_empty()); }
}
