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

pub fn is_headline (
  line : &str,
) -> bool {
  // A headline starts with one or more '*' followed by a space.
  // Do not trim leading whitespace — org headlines must start at column 0.
  if ! line . starts_with ('*') { return false; }
  let stars : usize =
    line . chars() . take_while (|c| *c == '*') . count();
  line . len() > stars && line . as_bytes()[stars] == b' ' }

pub fn headline_level (
  line : &str,
) -> usize {
  line . chars()
    . take_while (|c| *c == '*')
    . count() }

pub fn headline_title (
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

