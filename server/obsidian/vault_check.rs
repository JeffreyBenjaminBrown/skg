//! Check a vault against the rules Obsidian documents (or that its
//! users report), without Obsidian: 'skg check-obsidian-vault' runs it
//! on a real vault, and the tests run it on every vault they export.
//! It reads the Markdown its own way, not through the exporter's code,
//! so that it can catch what the exporter gets wrong.
//!
//! It checks that
//! - every wikilink and embed, in text or in a property, names exactly
//!   one file, and the block ID or heading it names is in that file;
//! - block IDs are unique in their file, ignoring case, and none is
//!   spelled with characters Obsidian does not allow;
//! - front matter parses as YAML, as a mapping whose values are text,
//!   numbers, checkboxes or lists of those, each property name with
//!   one type throughout the vault;
//! - no code fence or comment is left open;
//! - names avoid what Windows, macOS, Linux and Obsidian's links
//!   refuse, paths stay well under Windows' 260 characters, and no
//!   two files differ only in case (nor share a name, which makes a
//!   bare link ambiguous);
//! - each note the manifest lists is in its file: as the root, or as
//!   a block ID.

use crate::obsidian::dialect::{MANIFEST_FILENAME, block_id_of_pid};
use crate::types::misc::ID;

use regex::Regex;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::error::Error;
use std::fs;
use std::path::Path;
use std::sync::LazyLock;
use walkdir::WalkDir;

/// Characters a name may not hold: Windows' rules, and those that
/// break Obsidian's links.
const FORBIDDEN_IN_NAMES : &str = "*\"\\/<>:|?#^[]";
const WINDOWS_RESERVED_NAMES : &[&str] = &[
  "CON", "PRN", "AUX", "NUL",
  "COM1", "COM2", "COM3", "COM4", "COM5", "COM6", "COM7", "COM8", "COM9",
  "LPT1", "LPT2", "LPT3", "LPT4", "LPT5", "LPT6", "LPT7", "LPT8", "LPT9" ];
/// A relative path longer than this leaves under 60 characters of
/// Windows' 260 for the vault's own location.
const MAX_RELATIVE_PATH_CHARS : usize = 200;
const MAX_NAME_BYTES : usize = 255;

static BLOCK_ID_AT_END : LazyLock<Regex> =
  LazyLock::new ( || Regex::new (r"(?:^|\s)\^([A-Za-z0-9-]+)$") . unwrap () );
static CARET_WORD_AT_END : LazyLock<Regex> =
  LazyLock::new ( || Regex::new (r"(?:^|\s)\^(\S+)$") . unwrap () );
static HEADING : LazyLock<Regex> =
  LazyLock::new ( || Regex::new (r"^(#{1,6})(?:[ \t]+(.*?))?[ \t]*$") . unwrap () );

#[derive(Debug, Default)]
pub struct VaultCheckReport {
  pub files     : usize,
  pub bytes     : u64,
  pub links     : usize,
  pub embeds    : usize,
  pub block_ids : usize,
  pub manifest_notes : usize,
  pub problems  : Vec<String>,
}

impl VaultCheckReport {
  pub fn summary (
    &self,
  ) -> String {
    let mut s : String = format! (
      "{} Markdown files ({:.1} MB), {} links, {} embeds, {} block IDs; \
       {} notes of the manifest found.\n{} problem(s).\n",
      self . files, self . bytes as f64 / 1_000_000.0, self . links,
      self . embeds, self . block_ids, self . manifest_notes,
      self . problems . len () );
    for problem in self . problems . iter () . take (50) {
      s . push_str ( & format! ("  {}\n", problem) ); }
    if self . problems . len () > 50 {
      s . push_str ( & format! ("  ... and {} more\n", self . problems . len () - 50) ); }
    s }
}

/// What one Markdown file offers to links, and what it asks of others.
#[derive(Default)]
struct FileFacts {
  block_ids : HashSet<String>, // lowercased
  headings  : HashSet<String>, // lowercased
  skg_id    : Option<String>,
  links     : Vec<(usize, bool, String)>, // (line, is an embed, inside of [[...]])
}

pub fn check_vault (
  vault : &Path,
) -> Result<VaultCheckReport, Box<dyn Error>> {
  let mut report : VaultCheckReport = VaultCheckReport::default ();
  let mut facts : BTreeMap<String, FileFacts> = BTreeMap::new (); // by relative path
  let mut property_types : HashMap<String, (String, String)> = HashMap::new (); // name -> (type, first file)
  for entry in WalkDir::new (vault) . min_depth (1) . into_iter ()
    . filter_entry ( |e| ! e . file_name () . to_string_lossy () . starts_with ('.') ) {
    let entry = entry ?;
    let relative : String = entry . path () . strip_prefix (vault) ?
      . to_string_lossy () . replace ('\\', "/");
    check_name (&relative, &mut report . problems);
    if ! entry . file_type () . is_file () || ! relative . ends_with (".md") { continue; }
    let text : String = fs::read_to_string (entry . path ()) ?;
    report . files += 1;
    report . bytes += text . len () as u64;
    let file_facts : FileFacts =
      read_file (&relative, &text, &mut property_types, &mut report . problems);
    report . block_ids += file_facts . block_ids . len ();
    facts . insert (relative, file_facts); }
  check_collisions (&facts, &mut report . problems);
  check_links (&facts, &mut report);
  check_manifest (vault, &facts, &mut report) ?;
  Ok (report) }

//
// Names
//

fn check_name (
  relative : &str,
  problems : &mut Vec<String>,
) {
  if relative . chars () . count () > MAX_RELATIVE_PATH_CHARS {
    problems . push ( format! ("{}: path longer than {} characters", relative, MAX_RELATIVE_PATH_CHARS )); }
  let name : &str = relative . rsplit ('/') . next () . unwrap_or (relative);
  if name . len () > MAX_NAME_BYTES {
    problems . push ( format! ("{}: name longer than {} bytes", relative, MAX_NAME_BYTES )); }
  if let Some (c) = name . chars () . find ( |c| FORBIDDEN_IN_NAMES . contains (*c) || c . is_control () ) {
    problems . push ( format! ("{}: name holds {:?}", relative, c )); }
  if name . ends_with ('.') || name . ends_with (' ') || name . starts_with (' ') {
    problems . push ( format! ("{}: name begins or ends with a space, or ends with a dot", relative )); }
  let stem : &str = name . split ('.') . next () . unwrap_or (name);
  if WINDOWS_RESERVED_NAMES . contains (& stem . to_ascii_uppercase () . as_str ()) {
    problems . push ( format! ("{}: a name Windows reserves", relative )); }}

fn check_collisions (
  facts    : &BTreeMap<String, FileFacts>,
  problems : &mut Vec<String>,
) {
  let mut by_lowercase_path : HashMap<String, &str> = HashMap::new ();
  let mut by_lowercase_stem : HashMap<String, &str> = HashMap::new ();
  for relative in facts . keys () {
    if let Some (other) = by_lowercase_path . insert (relative . to_lowercase (), relative) {
      problems . push ( format! ("{} and {} differ only in case", other, relative )); }
    if let Some (other) = by_lowercase_stem . insert (stem_of (relative) . to_lowercase (), relative) {
      problems . push ( format! ("{} and {} share a name, so a bare link to it is ambiguous", other, relative )); }}}

fn stem_of (
  relative : &str,
) -> &str {
  let name : &str = relative . rsplit ('/') . next () . unwrap_or (relative);
  name . strip_suffix (".md") . unwrap_or (name) }

//
// Reading a file
//

fn read_file (
  relative       : &str,
  text           : &str,
  property_types : &mut HashMap<String, (String, String)>,
  problems       : &mut Vec<String>,
) -> FileFacts {
  let mut facts : FileFacts = FileFacts::default ();
  let lines : Vec<&str> = text . lines () . collect ();
  let body_start : usize =
    if lines . first () == Some (&"---") {
      match lines . iter () . skip (1) . position ( |line| *line == "---" ) {
        Some (offset) => {
          let yaml : String = lines [1 .. offset + 1] . join ("\n");
          read_frontmatter (relative, &yaml, &mut facts, property_types, problems);
          offset + 2 },
        None => {
          problems . push ( format! ("{}: front matter never closes", relative ));
          lines . len () }, }}
    else { 0 };
  let mut fence : Option<(char, usize)> = None;
  let mut in_comment : bool = false;
  for (index, line) in lines . iter () . enumerate () . skip (body_start) {
    let line_number : usize = index + 1;
    let trimmed : &str = line . trim_start ();
    if let Some (open) = fence {
      if closes (trimmed, open) { fence = None; }
      continue; }
    if ! in_comment {
      if let Some (open) = opens_fence (trimmed) {
        fence = Some (open);
        continue; }
      if let Some (captures) = HEADING . captures (line) {
        let heading : &str = captures . get (2) . map_or ("", |m| m . as_str ());
        let heading : &str = heading . trim_end_matches ('#') . trim_end ();
        facts . headings . insert (heading . to_lowercase ()); }}
    let visible : String = visible_text (line, &mut in_comment);
    for (is_embed, inside) in wikilinks_in (&visible) {
      facts . links . push ((line_number, is_embed, inside)); }
    let visible_end : &str = visible . trim_end ();
    if let Some (captures) = BLOCK_ID_AT_END . captures (visible_end) {
      let block_id : String = captures [1] . to_lowercase ();
      if ! facts . block_ids . insert (block_id . clone ()) {
        problems . push ( format! ("{}:{}: block ID ^{} repeats", relative, line_number, block_id )); }}
    else if let Some (captures) = CARET_WORD_AT_END . captures (visible_end) {
      problems . push ( format! (
        "{}:{}: ^{} looks like a block ID but has characters Obsidian does not allow",
        relative, line_number, &captures [1] )); }}
  if fence . is_some () {
    problems . push ( format! ("{}: a code fence never closes", relative )); }
  if in_comment {
    problems . push ( format! ("{}: a %% comment never closes", relative )); }
  facts }

fn opens_fence (
  trimmed : &str,
) -> Option<(char, usize)> {
  let c : char = trimmed . chars () . next () ?;
  if c != '`' && c != '~' { return None; }
  let length : usize = trimmed . chars () . take_while ( |x| *x == c ) . count ();
  if length < 3 { return None; }
  if c == '`' && trimmed [length ..] . contains ('`') { return None; }
  Some ((c, length)) }

fn closes (
  trimmed : &str,
  open    : (char, usize),
) -> bool {
  let length : usize = trimmed . chars () . take_while ( |x| *x == open . 0 ) . count ();
  length >= open . 1 && trimmed [length ..] . trim () . is_empty () }

/// 'line' without its comments ('%%...%%', which may span lines),
/// inline code, and backslash escapes (each replaced by a space, so
/// that an escaped character is never read as syntax).
fn visible_text (
  line       : &str,
  in_comment : &mut bool,
) -> String {
  let chars : Vec<char> = line . chars () . collect ();
  let mut out : String = String::new ();
  let mut i : usize = 0;
  while i < chars . len () {
    if chars [i] == '%' && chars . get (i + 1) == Some (&'%') {
      *in_comment = ! *in_comment;
      i += 2;
      continue; }
    if *in_comment { i += 1; continue; }
    if chars [i] == '\\' && chars . get (i + 1) . is_some_and ( |c| c . is_ascii_punctuation () ) {
      out . push_str ("  ");
      i += 2;
      continue; }
    if chars [i] == '`' {
      let run : usize = chars [i ..] . iter () . take_while ( |c| **c == '`' ) . count ();
      let closing : Option<usize> = ( i + run .. chars . len () )
        . find ( |j| chars [*j ..] . iter () . take_while ( |c| **c == '`' ) . count () == run
                     && ( *j == 0 || chars [*j - 1] != '`' ) );
      if let Some (j) = closing {
        out . push (' ');
        i = j + run;
        continue; }}
    out . push (chars [i]);
    i += 1; }
  out }

/// Each '[[...]]' in 'visible', with whether it is an embed.
fn wikilinks_in (
  visible : &str,
) -> Vec<(bool, String)> {
  let mut out : Vec<(bool, String)> = Vec::new ();
  let mut rest : &str = visible;
  let mut offset : usize = 0;
  while let Some (open) = rest . find ("[[") {
    let after : &str = &rest [open + 2 ..];
    let Some (close) = after . find ("]]") else { break; };
    let is_embed : bool = open > 0 && rest . as_bytes () [open - 1] == b'!';
    out . push ((is_embed, after [.. close] . to_string ()));
    let consumed : usize = open + 2 + close + 2;
    offset += consumed;
    rest = &visible [offset ..]; }
  out }

fn read_frontmatter (
  relative       : &str,
  yaml           : &str,
  facts          : &mut FileFacts,
  property_types : &mut HashMap<String, (String, String)>,
  problems       : &mut Vec<String>,
) {
  let value : yaml_serde::Value = match yaml_serde::from_str (yaml) {
    Ok (value) => value,
    Err (e) => {
      problems . push ( format! ("{}: front matter is not YAML: {}", relative, e ));
      return; }, };
  let Some (mapping) = value . as_mapping () else {
    problems . push ( format! ("{}: front matter is not a mapping", relative ));
    return; };
  for (key, value) in mapping {
    let Some (name) = key . as_str () else {
      problems . push ( format! ("{}: a property name is not text", relative ));
      continue; };
    let kind : &str = match value {
      yaml_serde::Value::String (_) => "text",
      yaml_serde::Value::Number (_) => "number",
      yaml_serde::Value::Bool (_)   => "checkbox",
      yaml_serde::Value::Null       => continue,
      yaml_serde::Value::Sequence (items) => {
        if items . iter () . any ( |item| ! matches! (item,
             yaml_serde::Value::String (_) | yaml_serde::Value::Number (_)
             | yaml_serde::Value::Bool (_)) ) {
          problems . push ( format! ("{}: property {} is a list of something other than text", relative, name ));
          continue; }
        "list" },
      _ => {
        problems . push ( format! ("{}: property {} is a nested map, which Obsidian cannot show", relative, name ));
        continue; }, };
    if name == "aliases" && kind != "list" && kind != "text" {
      problems . push ( format! ("{}: aliases must be a list", relative )); }
    match property_types . get (name) {
      Some ((first_kind, first_file)) if first_kind != kind =>
        problems . push ( format! (
          "{}: property {} is {} here but {} in {}", relative, name, kind, first_kind, first_file )),
      Some (_) => {},
      None => { property_types . insert (
        name . to_string (), (kind . to_string (), relative . to_string ())); }, }
    let texts : Vec<&str> = match value {
      yaml_serde::Value::String (s) => vec! [ s . as_str () ],
      yaml_serde::Value::Sequence (items) => items . iter () . filter_map ( |i| i . as_str () ) . collect (),
      _ => Vec::new (), };
    if name == "skg-id" { facts . skg_id = texts . first () . map ( |s| s . to_string () ); }
    for text in texts {
      for (is_embed, inside) in wikilinks_in (text) {
        facts . links . push ((0, is_embed, inside)); }}}}

//
// Links
//

fn check_links (
  facts  : &BTreeMap<String, FileFacts>,
  report : &mut VaultCheckReport,
) {
  let mut by_stem : HashMap<String, Vec<&str>> = HashMap::new ();
  let mut by_path : HashMap<String, &str> = HashMap::new ();
  for relative in facts . keys () {
    by_stem . entry (stem_of (relative) . to_lowercase ()) . or_default () . push (relative);
    by_path . insert (relative . strip_suffix (".md") . unwrap_or (relative) . to_lowercase (), relative); }
  let mut problems : Vec<String> = Vec::new ();
  for (relative, file_facts) in facts {
    for (line, is_embed, inside) in &file_facts . links {
      if *is_embed { report . embeds += 1; } else { report . links += 1; }
      let target : &str = inside . split ('|') . next () . unwrap_or ("");
      let (file_part, subpath) : (&str, Option<&str>) = match target . split_once ('#') {
        Some ((file, sub)) => (file, Some (sub)),
        None => (target, None), };
      let target_file : Option<&str> =
        if file_part . is_empty () { Some (relative . as_str ()) }
        else if file_part . contains ('/') {
          by_path . get (& file_part . to_lowercase ()) . copied () }
        else { match by_stem . get (& file_part . to_lowercase ()) {
          Some (files) if files . len () == 1 => Some (files [0]),
          _ => None, }};
      let Some (target_file) = target_file else {
        problems . push ( format! ("{}:{}: [[{}]] names no single file", relative, line, inside ));
        continue; };
      let target_facts : &FileFacts = &facts [target_file];
      match subpath {
        None => {},
        Some (sub) => match sub . strip_prefix ('^') {
          Some (block_id) =>
            if ! target_facts . block_ids . contains (& block_id . to_lowercase ()) {
              problems . push ( format! ("{}:{}: [[{}]]: no block ^{} in {}", relative, line, inside, block_id, target_file )); },
          None =>
            if ! target_facts . headings . contains (& sub . to_lowercase ()) {
              problems . push ( format! ("{}:{}: [[{}]]: no heading {:?} in {}", relative, line, inside, sub, target_file )); }, }, }}}
  report . problems . extend (problems); }

//
// The manifest
//

fn check_manifest (
  vault  : &Path,
  facts  : &BTreeMap<String, FileFacts>,
  report : &mut VaultCheckReport,
) -> Result<(), Box<dyn Error>> {
  let path = vault . join (MANIFEST_FILENAME);
  if ! path . exists () { return Ok (( )); }
  let manifest : toml::Table = fs::read_to_string (&path) ? . parse () ?;
  let Some (files) = manifest . get ("files") . and_then ( |f| f . as_table () ) else {
    report . problems . push ( format! ("{}: no [files] table", MANIFEST_FILENAME ));
    return Ok (( )); };
  for (id, file) in files {
    let Some (file) = file . as_str () else { continue; };
    let Some (file_facts) = facts . get (file) else {
      report . problems . push ( format! ("{}: {} is in {}, which does not exist", MANIFEST_FILENAME, id, file ));
      continue; };
    let is_root : bool = file_facts . skg_id . as_deref () == Some (id . as_str ());
    let has_block : bool = file_facts . block_ids . contains (
      & block_id_of_pid (&ID::from (id . as_str ())) . to_lowercase () );
    if is_root || has_block { report . manifest_notes += 1; }
    else {
      report . problems . push ( format! ("{}: {} is said to be in {}, but is not", MANIFEST_FILENAME, id, file )); }}
  Ok (( )) }
