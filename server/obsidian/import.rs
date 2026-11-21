//! Read a vault that 'skg export-obsidian' wrote back into .skg files:
//! the inverse of 'export.rs', so that importing an export reproduces
//! the skgrepo's files byte for byte, and exporting that import
//! reproduces the vault ('TODO/export-obsidian/plan.org', part 2).
//!
//! It is a pure function of the vault: it reads IDs from block IDs,
//! properties and comments, mints none, and needs no server. It reads
//! only the dialect the exporter writes, and stops at the first line
//! it cannot place, naming it.

use crate::obsidian::dialect::*;
use crate::obsidian::read_text::{id_of_link_target, unrender_text};
use crate::telescope::types::ListItem;
use crate::types::misc::{ID, SkgrepoName};
use crate::types::nodes::attribution::Attribution;
use crate::types::nodes::complete::Flag;
use crate::types::nodes::fs::GraphnodeOnDisk;

use serde_json::{Map, Value};
use std::collections::{BTreeMap, HashMap};
use std::error::Error;
use std::fs;
use std::path::{Path, PathBuf};

/// The sections of each skgrepo folder of a vault, by folder name.
pub type ImportedVault = BTreeMap<String, Vec<GraphnodeOnDisk>>;

/// Where each file's root note is: its name without '.md', to its ID.
type RootsByStem = HashMap<String, ID>;

/// One line-level item under a note, before it becomes a list entry.
enum ParsedEntry {
  Note   (Box<ParsedNote>),
  Embed  { listed_as : ID },
  Anchor (ID),
  Absent (ID),
}

struct ParsedNote {
  pid       : ID,
  title     : Option<String>,
  body      : Option<String>,
  meta      : Map<String, Value>,
  children  : Vec<ParsedEntry>,
}

//
// Entry points
//

/// Read every skgrepo folder of the vault at 'vault_dir': each folder
/// at its top that holds Markdown files and whose name does not begin
/// with a dot.
pub fn read_vault (
  vault_dir : &Path,
) -> Result<ImportedVault, Box<dyn Error>> {
  let mut files : BTreeMap<String, String> = BTreeMap::new (); // "FOLDER/STEM.md" -> text
  for folder in fs::read_dir (vault_dir) ? {
    let folder : PathBuf = folder ? . path ();
    let name : String = folder . file_name () . unwrap () . to_string_lossy () . to_string ();
    if ! folder . is_dir () || name . starts_with ('.') { continue; }
    for entry in fs::read_dir (&folder) ? {
      let path : PathBuf = entry ? . path ();
      let Some (file_name) = path . file_name () . and_then ( |n| n . to_str () ) else { continue; };
      if ! file_name . ends_with (".md") || ! path . is_file () { continue; }
      files . insert ( format! ("{}/{}", name, file_name), fs::read_to_string (&path) ? ); }}
  read_vault_files (&files) }

/// Like 'read_vault', of a vault's files as texts by relative path
/// (what 'render_vault' returns). Files at the top are not read.
pub fn read_vault_files (
  files : &BTreeMap<String, String>,
) -> Result<ImportedVault, Box<dyn Error>> {
  let mut folders : BTreeMap<&str, Vec<(&str, &str)>> = BTreeMap::new (); // folder -> [(stem, text)]
  for (relative, text) in files {
    let Some ((folder, file_name)) = relative . split_once ('/') else { continue; };
    let Some (stem) = file_name . strip_suffix (".md") else { continue; };
    if folder . starts_with ('.') || stem . contains ('/') { continue; }
    folders . entry (folder) . or_default () . push ((stem, text . as_str ())); }
  let mut roots_by_stem : RootsByStem = HashMap::new ();
  for (folder, folder_files) in &folders {
    for (stem, text) in folder_files {
      let properties : Map<String, Value> = frontmatter_of (text)
        . map_err ( |e| format! ("{}/{}.md: {}", folder, stem, e) ) ? . 0;
      let pid : ID = properties . get (KEY_ID) . and_then ( |v| v . as_str () )
        . map (ID::from)
        . ok_or_else ( || format! ("{}/{}.md has no {} property", folder, stem, KEY_ID) ) ?;
      roots_by_stem . insert (stem . to_string (), pid); }}
  let mut vault : ImportedVault = BTreeMap::new ();
  for (folder, folder_files) in folders {
    let mut sections : Vec<GraphnodeOnDisk> = Vec::new ();
    for (stem, text) in folder_files {
      sections . extend (
        sections_of_file (text, &roots_by_stem)
        . map_err ( |e| format! ("{}/{}.md: {}", folder, stem, e) ) ? ); }
    sections . sort_by ( |a, b| a . pid . cmp (&b . pid) );
    if let Some (pair) = sections . windows (2) . find ( |pair| pair [0] . pid == pair [1] . pid ) {
      return Err ( format! ("{}: the note {} is written twice", folder, pair [0] . pid) . into () ); }
    vault . insert (folder . to_string (), sections); }
  Ok (vault) }

/// Write each folder's sections to 'out_dir/FOLDER/PID.skg', as skg
/// writes them. Each such folder must be absent or empty.
pub fn write_imported_vault (
  vault   : &ImportedVault,
  out_dir : &Path,
) -> Result<usize, Box<dyn Error>> {
  let mut written : usize = 0;
  for (folder, sections) in vault {
    let dir : PathBuf = out_dir . join (folder);
    if dir . exists () && fs::read_dir (&dir) ? . next () . is_some () {
      return Err ( format! ("{} is not empty; refusing to write there", dir . display ()) . into () ); }
    fs::create_dir_all (&dir) ?;
    for section in sections {
      fs::write ( dir . join ( format! ("{}.skg", section . pid . as_str ()) ),
                  section . to_yaml () ? ) ?;
      written += 1; }}
  Ok (written) }

//
// A file
//

/// The properties, and the lines after the front matter.
fn frontmatter_of (
  text : &str,
) -> Result<(Map<String, Value>, Vec<&str>), String> {
  let lines : Vec<&str> = text . strip_suffix ('\n') . unwrap_or (text) . split ('\n') . collect ();
  if lines . first () != Some (&"---") {
    return Err ("it does not begin with front matter" . to_string ()); }
  let close : usize = lines . iter () . skip (1) . position ( |l| *l == "---" )
    . ok_or ("its front matter never closes") ? + 1;
  let yaml : String = lines [1 .. close] . join ("\n");
  let value : yaml_serde::Value = yaml_serde::from_str (&yaml)
    . map_err ( |e| format! ("its front matter is not YAML: {}", e) ) ?;
  let json : Value = serde_json::to_value (&value) . map_err ( |e| e . to_string () ) ?;
  let Value::Object (properties) = json
    else { return Err ("its front matter is not a mapping" . to_string ()); };
  Ok (( properties, lines [close + 1 ..] . to_vec () )) }

fn sections_of_file (
  text          : &str,
  roots_by_stem : &RootsByStem,
) -> Result<Vec<GraphnodeOnDisk>, String> {
  let (properties, lines) : (Map<String, Value>, Vec<&str>) = frontmatter_of (text) ?;
  let heading : &str = lines . first () . and_then ( |l| l . strip_prefix ("# ") )
    . ok_or ("the line after the front matter is not its '# ' heading") ?;
  let pid : ID = ID::from ( properties . get (KEY_ID) . and_then ( |v| v . as_str () ) . unwrap_or ("") );
  let mut i : usize = 1;
  let (body, children) : (Option<String>, Vec<ParsedEntry>) =
    read_body_and_children (&lines, &mut i, 0, roots_by_stem) ?;
  if i < lines . len () {
    return Err ( format! ("line {} is not part of the outline: {:?}", i + 1, lines [i]) ); }
  let root : ParsedNote = ParsedNote {
    pid, title : title_of (heading, roots_by_stem) ?, body,
    meta : properties, children };
  let mut sections : Vec<GraphnodeOnDisk> = Vec::new ();
  collect_sections (root, roots_by_stem, &mut sections) ?;
  Ok (sections) }

/// The body and children of a note whose content starts at column
/// 'column', from line '*i' on; '*i' ends after them.
fn read_body_and_children (
  lines         : &[&str],
  i             : &mut usize,
  column        : usize,
  roots_by_stem : &RootsByStem,
) -> Result<(Option<String>, Vec<ParsedEntry>), String> {
  let body_start : usize = *i;
  let mut fence : Option<(char, usize)> = None;
  while *i < lines . len () {
    let line : &str = lines [*i];
    if line . trim () . is_empty () { *i += 1; continue; }
    let indent : usize = indent_of (line);
    if indent < column { break; }
    let relative : &str = &line [column ..];
    if let Some (opener) = fence {
      if closes_fence (relative, opener) { fence = None; }
      *i += 1;
      continue; }
    if indent == column && is_entry_line (relative) { break; }
    fence = fence_opener (relative);
    *i += 1; }
  if fence . is_some () {
    return Err ( format! ("a code fence opened before line {} never closes", *i + 1) ); }
  let body : Option<String> = read_body (&lines [body_start .. *i], column, roots_by_stem) ?;
  let mut children : Vec<ParsedEntry> = Vec::new ();
  while *i < lines . len () {
    let line : &str = lines [*i];
    if line . trim () . is_empty () { *i += 1; continue; }
    let indent : usize = indent_of (line);
    if indent < column { break; }
    if indent > column || ! is_entry_line (&line [column ..]) {
      return Err ( format! ("line {} is not part of the outline: {:?}", *i + 1, line) ); }
    children . push (read_entry (lines, i, column, roots_by_stem) ?); }
  Ok ((body, children)) }

fn indent_of (
  line : &str,
) -> usize {
  line . bytes () . take_while ( |b| *b == b' ' ) . count () }

/// Whether 'relative' (a line from its bullet's column) is a bullet the
/// exporter writes, rather than a line of a body. Body text cannot
/// look like one: its '^', '%%' and '[[' before an embed are escaped.
fn is_entry_line (
  relative : &str,
) -> bool {
  let Some (rest) = relative . strip_prefix ("- ") else { return false; };
  rest . starts_with ("![[")
  || split_trailing_block_id (rest) . is_some ()
  || ( rest . ends_with (COMMENT_CLOSE)
       && ( rest . starts_with (META_COMMENT_OPEN) || rest . contains ( &format! (" {}", META_COMMENT_OPEN) ) )) }

/// One bullet at 'column', and what it holds.
fn read_entry (
  lines         : &[&str],
  i             : &mut usize,
  column        : usize,
  roots_by_stem : &RootsByStem,
) -> Result<ParsedEntry, String> {
  let line_number : usize = *i + 1;
  let rest : &str = &lines [*i] [column + 2 ..];
  *i += 1;
  let in_line = |e : String| -> String { format! ("line {}: {}", line_number, e) };
  if let Some (embed) = rest . strip_prefix ("![[") {
    let (target, after) : (&str, &str) = embed . split_once ("]]")
      . ok_or_else ( || in_line ("an embed that never closes" . to_string ()) ) ?;
    let meta : Map<String, Value> =
      if after . is_empty () { Map::new () }
      else { after . strip_prefix (' ') . and_then (parse_meta_comment)
             . ok_or_else ( || in_line ("text after an embed" . to_string ()) ) ? };
    let pid : ID = id_of_link_target (target, &|stem| roots_by_stem . get (stem) . cloned ())
      . ok_or_else ( || in_line ( format! ("the embed {:?} names no note", target) )) ?;
    return Ok ( ParsedEntry::Embed { listed_as : text_of (&meta, KEY_REF) . map (ID::from) . unwrap_or (pid) } ); }
  if let Some ((title_text, block_id)) = split_trailing_block_id (rest) {
    let pid : ID = pid_of_block_id (block_id)
      . ok_or_else ( || in_line ( format! ("^{} is no block ID this dialect writes", block_id) )) ?;
    let meta : Map<String, Value> = read_meta_line (lines, i, column + 2) ?;
    let (body, children) : (Option<String>, Vec<ParsedEntry>) =
      read_body_and_children (lines, i, column + 2, roots_by_stem) ?;
    return Ok ( ParsedEntry::Note ( Box::new ( ParsedNote {
      pid, title : title_of (title_text, roots_by_stem) . map_err (in_line) ?,
      body, meta, children } ))); }
  let comment_start : usize = rest . rfind (META_COMMENT_OPEN)
    . ok_or_else ( || in_line ("a bullet with neither a block ID nor an embed" . to_string ()) ) ?;
  let meta : Map<String, Value> = parse_meta_comment (&rest [comment_start ..])
    . ok_or_else ( || in_line ("a comment that is not '%%skg {...}%%'" . to_string ()) ) ?;
  if let Some (anchor) = text_of (&meta, KEY_ANCHOR) {
    return Ok (ParsedEntry::Anchor (ID::from (anchor))); }
  if let Some (absent) = text_of (&meta, KEY_ABSENT) {
    return Ok (ParsedEntry::Absent (ID::from (absent))); }
  Err ( in_line ("a comment bullet that is neither an anchor nor an absent note" . to_string ()) ) }

/// A bullet's '%%skg {...}%%' line, which follows its title after a
/// blank line; empty if it has none.
fn read_meta_line (
  lines  : &[&str],
  i      : &mut usize,
  column : usize,
) -> Result<Map<String, Value>, String> {
  let mut j : usize = *i;
  while j < lines . len () && lines [j] . is_empty () { j += 1; }
  let Some (line) = lines . get (j) else { return Ok (Map::new ()); };
  if indent_of (line) != column || ! line [column ..] . starts_with (META_COMMENT_OPEN) {
    return Ok (Map::new ()); }
  let meta : Map<String, Value> = parse_meta_comment (&line [column ..])
    . ok_or_else ( || format! ("line {}: a comment that is not '%%skg {{...}}%%'", j + 1) ) ?;
  *i = j + 1;
  Ok (meta) }

/// A title as written: the mark of a titleless section, or text.
fn title_of (
  text          : &str,
  roots_by_stem : &RootsByStem,
) -> Result<Option<String>, String> {
  if text == SECTION_TITLE_MARK { return Ok (None); }
  unrender (text, roots_by_stem) . map (Some) }

fn unrender (
  text          : &str,
  roots_by_stem : &RootsByStem,
) -> Result<String, String> {
  unrender_text (text, &|stem| roots_by_stem . get (stem) . cloned ())
    . ok_or_else ( || format! ("a wikilink in {:?} names no note", text) ) }

/// A body from its lines: the lines between its note's title (or
/// comment) and its children, each indented to 'column'.
fn read_body (
  lines         : &[&str],
  column        : usize,
  roots_by_stem : &RootsByStem,
) -> Result<Option<String>, String> {
  let first : Option<usize> = lines . iter () . position ( |l| ! l . trim () . is_empty () );
  let last  : Option<usize> = lines . iter () . rposition ( |l| ! l . trim () . is_empty () );
  let (Some (first), Some (last)) = (first, last) else { return Ok (None); };
  let unindented : Vec<&str> = lines [first ..= last] . iter ()
    . map ( |l| if l . len () >= column { &l [column ..] } else { "" } )
    . collect ();
  let opening : &str = unindented [0];
  let closing : &str = unindented [unindented . len () - 1];
  let fence : String = opening . chars () . take_while ( |c| *c == '`' ) . collect ();
  let is_fenced_as = |info : &str| -> bool {
    fence . len () >= 3 && opening == format! ("{}{}", fence, info)
    && closing == fence && unindented . len () >= 2 };
  if is_fenced_as (VERBATIM_JSON_FENCE_INFO) {
    let json : String = unindented [1 .. unindented . len () - 1] . join ("\n");
    return serde_json::from_str::<String> (&json) . map (Some)
      . map_err ( |e| format! ("a {} fence that is no JSON string: {}", VERBATIM_JSON_FENCE_INFO, e) ); }
  if is_fenced_as (VERBATIM_FENCE_INFO) {
    return Ok (Some ( unindented [1 .. unindented . len () - 1] . join ("\n") )); }
  unrender (& unindented . join ("\n"), roots_by_stem) . map (Some) }

//
// Sections
//

/// The note 'parsed' and every note written inside it, as sections.
fn collect_sections (
  parsed        : ParsedNote,
  roots_by_stem : &RootsByStem,
  out           : &mut Vec<GraphnodeOnDisk>,
) -> Result<(), String> {
  let ParsedNote { pid, title, body, meta, children } = parsed;
  let mut contains : Vec<ListItem> = Vec::new ();
  for child in children {
    contains . push ( match child {
      ParsedEntry::Embed { listed_as } => ListItem::Member (listed_as),
      ParsedEntry::Anchor (anchor)     => ListItem::Anchor { anchor },
      ParsedEntry::Absent (raw)        => ListItem::Member (raw),
      ParsedEntry::Note (note)         => {
        let listed_as : ID = text_of (&note . meta, KEY_REF) . map (ID::from)
          . unwrap_or_else ( || note . pid . clone () );
        collect_sections (*note, roots_by_stem, out) ?;
        ListItem::Member (listed_as) }, } ); }
  let title : Option<String> = match text_of (&meta, KEY_TITLE) {
    Some (exact) => Some (exact . to_string ()),
    None         => title, };
  let in_note = |e : String| -> String { format! ("note {}: {}", pid . as_str (), e) };
  out . push ( GraphnodeOnDisk {
    title,
    aliases     : texts_of (&meta, KEY_ALIASES) . map_err (in_note) ?,
    pid         : pid . clone (),
    extra_ids   : texts_of (&meta, KEY_EXTRA_IDS) . map_err (in_note) ?
      . into_iter () . map (ID::from) . collect (),
    body,
    contains,
    forks       : texts_of (&meta, KEY_FORKS) . map_err (in_note) ? . iter ()
      . map ( |entry| match entry . strip_prefix (ANCHOR_ENTRY_PREFIX) {
        Some (anchor) => Ok (ListItem::Anchor { anchor : ID::from (anchor) }),
        None          => id_of_entry (entry, roots_by_stem) . map (ListItem::Member), } )
      . collect::<Result<Vec<ListItem>, String>> () . map_err (in_note) ?,
    hides       : texts_of (&meta, KEY_HIDES) . map_err (in_note) ? . iter ()
      . map ( |entry| id_of_entry (entry, roots_by_stem) )
      . collect::<Result<Vec<ID>, String>> () . map_err (in_note) ?,
    flags       : texts_of (&meta, KEY_FLAGS) . map_err (in_note) ? . into_iter ()
      . map ( |name| serde_json::from_value::<Flag> (Value::String (name . clone ()))
              . map_err ( |_| format! ("no flag is named {:?}", name) ) )
      . collect::<Result<Vec<Flag>, String>> () . map_err (in_note) ?,
    attribution : attribution_of (&meta) . map_err (in_note) ?, } );
  Ok (( )) }

/// A 'skg-forks' or 'skg-hides' entry: a link to a note of the vault,
/// or an ID.
fn id_of_entry (
  entry         : &str,
  roots_by_stem : &RootsByStem,
) -> Result<ID, String> {
  match entry . strip_prefix ("[[") . and_then ( |e| e . strip_suffix ("]]") ) {
    None => Ok (ID::from (entry)),
    Some (target) => id_of_link_target (target, &|stem| roots_by_stem . get (stem) . cloned ())
      . ok_or_else ( || format! ("the link {:?} names no note", entry) ), }}

fn text_of<'a> (
  meta : &'a Map<String, Value>,
  key  : &str,
) -> Option<&'a str> {
  meta . get (key) . and_then ( |v| v . as_str () ) }

fn texts_of (
  meta : &Map<String, Value>,
  key  : &str,
) -> Result<Vec<String>, String> {
  match meta . get (key) {
    None | Some (Value::Null) => Ok (Vec::new ()),
    Some (Value::Array (items)) => items . iter ()
      . map ( |item| item . as_str () . map (String::from)
              . ok_or_else ( || format! ("{} holds something other than text", key) ) )
      . collect (),
    Some (_) => Err ( format! ("{} is not a list", key) ), }}

fn attribution_of (
  meta : &Map<String, Value>,
) -> Result<Option<Attribution>, String> {
  let original : Option<&str> = text_of (meta, KEY_ATTRIBUTION_ORIGINAL);
  let skgrepo  : Option<&str> = text_of (meta, KEY_ATTRIBUTION_SKGREPO);
  let (Some (original), Some (skgrepo)) = (original, skgrepo) else {
    let any : bool = [ KEY_ATTRIBUTION_ORIGINAL, KEY_ATTRIBUTION_SKGREPO, KEY_ATTRIBUTION_REMOTE_HASH,
                       KEY_ATTRIBUTION_COMMIT, KEY_ATTRIBUTION_UNCOMMITTED ]
      . iter () . any ( |key| meta . contains_key (*key) );
    return if any { Err ( format! ("an attribution needs both {} and {}",
                                   KEY_ATTRIBUTION_ORIGINAL, KEY_ATTRIBUTION_SKGREPO) ) }
           else { Ok (None) }; };
  Ok (Some ( Attribution {
    original    : ID::from (original),
    skgrepo     : SkgrepoName::from (skgrepo),
    remote_hash : text_of (meta, KEY_ATTRIBUTION_REMOTE_HASH) . map (String::from),
    commit      : text_of (meta, KEY_ATTRIBUTION_COMMIT) . map (String::from),
    uncommitted : meta . get (KEY_ATTRIBUTION_UNCOMMITTED) . and_then ( |v| v . as_bool () )
      . unwrap_or (false), } )) }

#[cfg(test)]
#[path = "../../tests/unit/obsidian_import.rs"]
mod tests;
