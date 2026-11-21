//! Export one skgrepo as an Obsidian vault.
//!
//! 'TODO/export-obsidian/plan.org' (part 1) gives the mapping, and
//! 'progress.org' beside it where this departs from it. In brief: a
//! *document* is a note that nothing in the skgrepo contains, or that
//! forks something, with the tree it contains. Each becomes one
//! Markdown file in the vault's folder for the skgrepo: the root's
//! title as the H1 and its metadata as properties; every other note a
//! bullet ending in its block ID, its body indented under it, its
//! children nested. A note reached a second time (several containers,
//! or a cycle) is written once, where it is first reached, and
//! embedded everywhere else.
//!
//! Only the files of the skgrepo asked for are read, so no other
//! skgrepo's text can reach the vault. A link to a note outside it
//! keeps its label and ID (both already in this skgrepo's files) and
//! points to an explanation.
//!
//! 'render_vault' is pure: sections in, file texts out. 'write_vault'
//! puts them on disk.

use crate::attribution::remote_hash_of_skgrepo_path;
use crate::dbs::filesystem::multiple_nodes::read_skg_sections_from_folder;
use crate::export_org::BROKEN_LINK_SINK_ID;
use crate::git_ops::read_gitrepo::open_gitrepo;
use crate::obsidian::dialect::*;
use crate::obsidian::import::{VaultReadme, frontmatter_of, read_readme};
use crate::obsidian::org_markup::{self, MarkupTally, Span, WHY_GUARD};
use crate::obsidian::read_text::unrender_text;
use crate::types::links::{links_with_ranges_from_text, replace_each_link_with_its_label};
use crate::types::misc::{ID, SkgConfig, SkgrepoName};
use crate::types::nodes::attribution::Attribution;
use crate::types::nodes::complete::{Flag, normalize_body};
use crate::types::nodes::fs::GraphnodeOnDisk;
use crate::telescope::types::ListItem;

use serde_json::{Map, Value};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::error::Error;
use std::fs;
use std::ops::Range;
use std::path::{Path, PathBuf};

/// Filenames are cut near this many characters, before the ID suffix.
const FILENAME_TITLE_CHARS : usize = 80;
/// And to at most this many bytes, so that a name stays under the
/// 255-byte limit of Linux and macOS.
const FILENAME_TITLE_BYTES : usize = 200;

//
// Public types
//

/// What the vault's README records of the skgrepo's gitrepo.
#[derive(Clone, Debug, Default)]
pub struct SkgrepoGitFacts {
  pub remote_hash : Option<String>,
  pub commit      : Option<String>,
}

/// A vault as texts: relative path ('/'-separated) to content.
pub struct RenderedVault {
  pub files  : BTreeMap<String, String>,
  pub report : ObsidianExportReport,
}

/// Of the titles (or bodies) that hold Org markup the export writes as
/// Markdown: how many had all of it converted, some, or none.
#[derive(Debug, Default)]
pub struct MarkupCounts {
  pub wholly  : usize,
  pub partly  : usize,
  pub none    : usize,
  pub converted : BTreeMap<&'static str, usize>, // constructs, by kind
  pub reasons   : BTreeMap<&'static str, usize>, // texts, by why markup was left as text
}

impl MarkupCounts {
  fn add (
    &mut self,
    tally : &MarkupTally,
  ) {
    let converted : bool = ! tally . converted . is_empty ();
    match (converted, tally . declined . is_empty ()) {
      (false, true)  => return,
      (true, true)   => self . wholly += 1,
      (true, false)  => self . partly += 1,
      (false, false) => self . none += 1, }
    for (kind, n) in &tally . converted {
      * self . converted . entry (kind) . or_insert (0) += n; }
    for reason in &tally . declined {
      * self . reasons . entry (reason) . or_insert (0) += 1; }}

  fn summary (
    &self,
    what  : &str,
    total : usize,
  ) -> String {
    let with_markup : usize = self . wholly + self . partly + self . none;
    if with_markup == 0 { return String::new (); }
    let mut s : String = format! (
      "{} with Org markup: {} of {}; converted wholly {}, partly {}, not at all {}.\n",
      what, with_markup, total, self . wholly, self . partly, self . none );
    if ! self . converted . is_empty () {
      s . push_str ( & format! ("  Converted: {}.\n", self . converted . iter ()
        . map ( |(kind, n)| format! ("{} {}", n, kind) ) . collect::<Vec<String>> () . join (", ") )); }
    let mut reasons : Vec<(&&str, &usize)> = self . reasons . iter () . collect ();
    reasons . sort_by ( |a, b| b . 1 . cmp (a . 1) . then (a . 0 . cmp (b . 0)) );
    for (reason, n) in reasons {
      s . push_str ( & format! ("  Left as Org text in {} {}: {}.\n", n, what . to_lowercase (), reason )); }
    s }
}

#[derive(Debug, Default)]
pub struct ObsidianExportReport {
  pub skgrepo           : String,
  pub notes             : usize,
  pub documents         : usize,
  pub embeds            : usize,
  pub links             : usize,
  pub links_outside     : usize, // to a note not in the vault
  pub links_left_as_text : usize, // whose label a wikilink cannot carry
  pub title_fallbacks   : usize, // titles kept in 'skg-title'
  pub body_fallbacks    : usize, // bodies in a 'skg-verbatim' fence
  pub bodies            : usize,
  pub bodies_markup     : MarkupCounts,
  pub titles_markup     : MarkupCounts,
  pub absent_members    : usize, // 'contains' entries not in this skgrepo
  pub files_removed     : Vec<String>,
  pub warnings          : Vec<String>,
}

impl ObsidianExportReport {
  pub fn summary (
    &self,
  ) -> String {
    let mut s : String = format! (
      "Exported {} notes of skgrepo {} as {} documents.\n\
       {} embeds, {} links ({} to notes outside the vault, {} left as text).\n\
       Fallbacks: {} titles in skg-title, {} bodies in a skg-verbatim fence.\n",
      self . notes, self . skgrepo, self . documents,
      self . embeds, self . links, self . links_outside, self . links_left_as_text,
      self . title_fallbacks, self . body_fallbacks );
    s . push_str ( &self . bodies_markup . summary ("Bodies", self . bodies) );
    s . push_str ( &self . titles_markup . summary ("Titles", self . notes) );
    if self . absent_members > 0 {
      s . push_str ( & format! (
        "{} contains entries name notes outside this skgrepo.\n",
        self . absent_members )); }
    if ! self . files_removed . is_empty () {
      s . push_str ( & format! (
        "Removed {} file(s) of the previous export.\n",
        self . files_removed . len () )); }
    for warning in &self . warnings {
      s . push_str ( & format! ("Warning: {}\n", warning )); }
    s }
}

//
// Internal types
//

struct VaultIndex<'a> {
  by_pid       : BTreeMap<ID, &'a GraphnodeOnDisk>,
  pid_of_extra : HashMap<ID, ID>,
}

impl<'a> VaultIndex<'a> {
  fn resolve (
    &self,
    raw : &ID,
  ) -> Option<ID> {
    if self . by_pid . contains_key (raw) { return Some (raw . clone ()); }
    self . pid_of_extra . get (raw) . cloned () }
}

/// One line-level item of a document, in order.
enum Entry {
  Note   { pid : ID, depth : usize, listed_as : ID }, // written here
  Embed  { pid : ID, depth : usize, listed_as : ID }, // written elsewhere
  Anchor { depth : usize, anchor : ID },
  Absent { depth : usize, listed_as : ID },
}

impl Entry {
  fn depth (&self) -> usize {
    match self {
      Entry::Note { depth, .. } | Entry::Embed { depth, .. }
      | Entry::Anchor { depth, .. } | Entry::Absent { depth, .. } => *depth, }}
}

struct Document {
  root    : ID,
  stem    : String, // filename without '.md'
  entries : Vec<Entry>,
}

/// Where a note is written: its document, and whether it is the root.
struct Home {
  document : usize,
  is_root  : bool,
}

/// What rendering needs to address any note.
struct Addresses<'a> {
  index        : &'a VaultIndex<'a>,
  homes        : HashMap<ID, Home>,
  documents    : &'a [Document],
  root_of_stem : HashMap<&'a str, &'a ID>,
}

//
// Entry points
//

/// Export the owned skgrepo 'skgrepo' of 'config' into 'vault_dir'.
/// It reads that skgrepo's folder and no other, so no other
/// skgrepo's text can reach the vault.
pub fn export_skgrepo_to_vault (
  config    : &SkgConfig,
  skgrepo   : &SkgrepoName,
  vault_dir : &Path,
) -> Result<RenderedVault, Box<dyn Error>> {
  let skgrepo_path : &Path = match config . skgrepos . get (skgrepo) {
    None => return Err ( format! ("no skgrepo {} in the config", skgrepo) . into () ),
    Some (s) if ! s . owned => return Err ( format! (
      "{} is foreign; export-obsidian exports only owned skgrepos", skgrepo) . into () ),
    Some (s) => &s . path, };
  let sections : Vec<GraphnodeOnDisk> =
    read_skg_sections_from_folder (skgrepo, config) ?
    . into_iter () . map ( |(_, section)| section ) . collect ();
  let git_facts : SkgrepoGitFacts = SkgrepoGitFacts {
    remote_hash : remote_hash_of_skgrepo_path (skgrepo_path),
    commit      : open_gitrepo (skgrepo_path)
      . and_then ( |gitrepo| gitrepo . head () . ok ()
                   . and_then ( |head| head . peel_to_commit () . ok () )
                   . map ( |commit| commit . id () . to_string () )), };
  let mut vault : RenderedVault =
    render_vault (skgrepo, &sections, &git_facts) ?;
  write_vault (&mut vault, vault_dir) ?;
  Ok (vault) }

/// Render the vault of one skgrepo's sections.
pub fn render_vault (
  skgrepo   : &SkgrepoName,
  sections  : &[GraphnodeOnDisk],
  git_facts : &SkgrepoGitFacts,
) -> Result<RenderedVault, Box<dyn Error>> {
  let index : VaultIndex = index_sections (sections) ?;
  let (mut documents, homes) : (Vec<Document>, HashMap<ID, Home>) =
    collect_documents (&index);
  assign_filenames (&mut documents, &index);
  let folder : String = skgrepo . as_str () . to_string ();
  let root_of_stem : HashMap<&str, &ID> = documents . iter ()
    . map ( |document| (document . stem . as_str (), &document . root) )
    . collect ();
  let addresses : Addresses = Addresses {
    index : &index, homes, documents : &documents, root_of_stem };
  let mut report : ObsidianExportReport = ObsidianExportReport {
    skgrepo   : folder . clone (),
    notes     : index . by_pid . len (),
    documents : documents . len (),
    .. ObsidianExportReport::default () };
  let mut files : BTreeMap<String, String> = BTreeMap::new ();
  for document in &documents {
    files . insert (
      format! ("{}/{}.md", folder, document . stem),
      render_document (document, &addresses, &mut report) ); }
  files . insert (
    format! ("{}.md", README_STEM),
    render_readme (&folder, git_facts) );
  Ok (RenderedVault { files, report }) }

/// Write 'vault' into 'vault_dir'. The folder must be absent, empty,
/// or a vault an earlier export wrote (its README says so). Files that
/// earlier export wrote and this one does not are removed; nothing
/// else is ('is_file_an_export_wrote').
pub fn write_vault (
  vault     : &mut RenderedVault,
  vault_dir : &Path,
) -> Result<(), Box<dyn Error>> {
  let readme_path : PathBuf = vault_dir . join ( format! ("{}.md", README_STEM) );
  let mut previous_folders : Vec<String> = Vec::new ();
  if vault_dir . exists () && fs::read_dir (vault_dir) ? . next () . is_some () {
    let readme : Option<VaultReadme> = fs::read_to_string (&readme_path) . ok ()
      . and_then ( |text| read_readme (&text) . ok () )
      . filter ( |readme| readme . format . starts_with (VAULT_FORMAT_FAMILY) );
    let Some (readme) = readme else {
      return Err ( format! (
        "{} is neither empty nor a vault skg exported (its {}.md has no {} property); \
         refusing to write there",
        vault_dir . display (), README_STEM, KEY_FORMAT ) . into () ); };
    previous_folders = readme . folders; }
  for folder in previous_folders {
    let dir : PathBuf = vault_dir . join (&folder);
    if ! dir . is_dir () { continue; }
    let mut stale : Vec<(String, PathBuf)> = Vec::new ();
    for entry in fs::read_dir (&dir) ? {
      let path : PathBuf = entry ? . path ();
      let Some (file_name) = path . file_name () . and_then ( |n| n . to_str () ) else { continue; };
      let relative : String = format! ("{}/{}", folder, file_name);
      if ! vault . files . contains_key (&relative) && is_file_an_export_wrote (&path) {
        stale . push ((relative, path)); }}
    stale . sort ();
    for (relative, path) in stale {
      fs::remove_file (&path) ?;
      vault . report . files_removed . push (relative); }}
  for (relative, content) in &vault . files {
    let path : PathBuf = vault_dir . join (relative);
    if let Some (parent) = path . parent () {
      fs::create_dir_all (parent) ?; }
    fs::write (&path, content) ?; }
  Ok (( )) }

/// Whether the file at 'path' (in a skgrepo folder of a vault) is one
/// an export wrote: a Markdown file whose 'skg-id' property is a
/// document's ID, under the name the exporter gives it, which ends in
/// a suffix of that ID. A file the user wrote, or a copy of an
/// exported file ("... 1.md", as Obsidian names a copy), is not.
fn is_file_an_export_wrote (
  path : &Path,
) -> bool {
  let Some (stem) = path . file_name () . and_then ( |n| n . to_str () )
    . and_then ( |n| n . strip_suffix (".md") ) else { return false; };
  if ! path . is_file () { return false; }
  let Ok (text) = fs::read_to_string (path) else { return false; };
  let Ok ((properties, _)) = frontmatter_of (&text) else { return false; };
  let Some (pid) = properties . get (KEY_ID) . and_then ( |v| v . as_str () ) else { return false; };
  id_suffixes (&ID::from (pid)) . iter ()
    . any ( |suffix| stem . ends_with ( &format! (" ({})", suffix) )) }

//
// Documents and homes
//

fn index_sections (
  sections : &[GraphnodeOnDisk],
) -> Result<VaultIndex<'_>, Box<dyn Error>> {
  let mut by_pid : BTreeMap<ID, &GraphnodeOnDisk> = BTreeMap::new ();
  for section in sections {
    if by_pid . insert (section . pid . clone (), section) . is_some () {
      return Err ( format! ("two sections have the PID {}", section . pid) . into () ); }}
  let mut pid_of_extra : HashMap<ID, ID> = HashMap::new ();
  for section in sections {
    for extra in &section . extra_ids {
      if ! by_pid . contains_key (extra) {
        pid_of_extra . insert (extra . clone (), section . pid . clone ()); }}}
  Ok (VaultIndex { by_pid, pid_of_extra }) }

/// The documents, in the order that decides where a note with several
/// containers is written: roots by PID, then (for cycles no root
/// reaches) the smallest PID not yet written.
fn collect_documents (
  index : &VaultIndex,
) -> (Vec<Document>, HashMap<ID, Home>) {
  let contained : HashSet<ID> = index . by_pid . values ()
    . flat_map ( |section| section . contains . iter () )
    . filter_map ( |item| match item {
      ListItem::Member (raw) => index . resolve (raw),
      ListItem::Anchor { .. } => None, } )
    . collect ();
  let mut roots : HashSet<ID> = index . by_pid . iter ()
    . filter ( |(pid, section)| ! contained . contains (*pid)
                                || ! section . forks . is_empty () )
    . map ( |(pid, _)| pid . clone () )
    . collect ();
  let mut documents : Vec<Document> = Vec::new ();
  let mut homes : HashMap<ID, Home> = HashMap::new ();
  let initial_roots : Vec<ID> = index . by_pid . keys ()
    . filter ( |pid| roots . contains (*pid) ) . cloned () . collect ();
  for root in initial_roots {
    let entries : Vec<Entry> =
      document_entries (&root, documents . len (), index, &roots, &mut homes);
    documents . push (Document { root, stem : String::new (), entries }); }
  let all_pids : Vec<ID> = index . by_pid . keys () . cloned () . collect ();
  for pid in all_pids {
    if homes . contains_key (&pid) { continue; }
    roots . insert (pid . clone ());
    let entries : Vec<Entry> =
      document_entries (&pid, documents . len (), index, &roots, &mut homes);
    documents . push (Document { root : pid, stem : String::new (), entries }); }
  (documents, homes) }

/// Preorder walk of one document. Iterative, so a deep outline cannot
/// overflow the stack. A note already written (here or in an earlier
/// document), and any other document's root, becomes an embed.
fn document_entries (
  root     : &ID,
  document : usize,
  index    : &VaultIndex,
  roots    : &HashSet<ID>,
  homes    : &mut HashMap<ID, Home>,
) -> Vec<Entry> {
  let mut entries : Vec<Entry> = Vec::new ();
  let mut stack : Vec<Entry> = vec! [
    Entry::Note { pid : root . clone (), depth : 0, listed_as : root . clone () } ];
  while let Some (entry) = stack . pop () {
    let Entry::Note { pid, depth, listed_as } = entry
      else { entries . push (entry); continue; };
    if homes . contains_key (&pid) {
      entries . push (Entry::Embed { pid, depth, listed_as });
      continue; }
    homes . insert (pid . clone (), Home { document, is_root : depth == 0 });
    let children : Vec<Entry> = index . by_pid [&pid] . contains . iter ()
      . map ( |item| match item {
        ListItem::Anchor { anchor } =>
          Entry::Anchor { depth : depth + 1, anchor : anchor . clone () },
        ListItem::Member (raw) => match index . resolve (raw) {
          None => Entry::Absent { depth : depth + 1, listed_as : raw . clone () },
          Some (child) if roots . contains (&child) =>
            Entry::Embed { pid : child, depth : depth + 1, listed_as : raw . clone () },
          Some (child) =>
            Entry::Note { pid : child, depth : depth + 1, listed_as : raw . clone () }, }, } )
      . collect ();
    entries . push (Entry::Note { pid, depth, listed_as });
    stack . extend (children . into_iter () . rev ()); }
  entries }

//
// Filenames
//

fn assign_filenames (
  documents : &mut [Document],
  index     : &VaultIndex,
) {
  let mut taken : HashSet<String> = HashSet::new (); // lowercased
  for document in documents . iter_mut () {
    let title : &str = index . by_pid [&document . root] . title . as_deref ()
      . unwrap_or ("section");
    let base : String = filename_base (title);
    let stem : String = id_suffixes (&document . root) . into_iter ()
      . map ( |suffix| format! ("{} ({})", base, suffix) )
      . find ( |stem| ! taken . contains (&stem . to_lowercase ()) )
      . expect ("the last suffix is the whole ID in hex, unique");
    taken . insert (stem . to_lowercase ());
    document . stem = stem; }}

/// A title made safe as a filename: links reduced to their labels,
/// characters that Windows, macOS or Obsidian's links refuse replaced,
/// cut at a word near 'FILENAME_TITLE_CHARS'.
pub fn filename_base (
  title : &str,
) -> String {
  let plain : String = replace_each_link_with_its_label (title);
  let replaced : String = plain . chars ()
    . map ( |c| if c . is_control () || "*\"\\/<>:|?#^[]%`" . contains (c) { ' ' }
                else { c } )
    . collect ();
  let collapsed : String = replaced . split_whitespace () . collect::<Vec<&str>> () . join (" ");
  let trimmed : &str = collapsed . trim_start_matches ( |c| c == '.' || c == ' ' );
  let cut : String = cut_at_word (trimmed, FILENAME_TITLE_CHARS, FILENAME_TITLE_BYTES);
  let cut : &str = cut . trim_end_matches ( |c| c == '.' || c == ' ' );
  if cut . is_empty () { "untitled" . to_string () }
  else { cut . to_string () }}

fn cut_at_word (
  text      : &str,
  max_chars : usize,
  max_bytes : usize,
) -> String {
  let mut end : usize = 0; // a byte offset
  for (count, (offset, c)) in text . char_indices () . enumerate () {
    if count >= max_chars || offset + c . len_utf8 () > max_bytes {
      let head : &str = &text [.. end];
      return match head . rfind (' ') {
        Some (space) if space >= end / 2 => head [.. space] . to_string (),
        _ => head . to_string (), }; }
    end = offset + c . len_utf8 (); }
  text . to_string () }

/// Suffixes to try, shortest first: the ID's first 8, 12, 16 ... letters
/// and digits, lowercased, then the whole ID in hex.
fn id_suffixes (
  pid : &ID,
) -> Vec<String> {
  let alnum : String = pid . as_str () . chars ()
    . filter ( |c| c . is_ascii_alphanumeric () )
    . map ( |c| c . to_ascii_lowercase () )
    . collect ();
  let mut out : Vec<String> = Vec::new ();
  let mut length : usize = 8;
  while length < alnum . len () {
    out . push (alnum [.. length] . to_string ());
    length += 4; }
  if ! alnum . is_empty () { out . push (alnum . clone ()); }
  out . push ( pid . as_str () . bytes () . map ( |b| format! ("{:02x}", b) ) . collect () );
  out }

//
// Rendering a document
//

fn render_document (
  document  : &Document,
  addresses : &Addresses,
  report    : &mut ObsidianExportReport,
) -> String {
  let mut out : String = String::new ();
  for (i, entry) in document . entries . iter () . enumerate () {
    let has_children : bool = document . entries . get (i + 1)
      . is_some_and ( |next| next . depth () == entry . depth () + 1 );
    match entry {
      Entry::Note { pid, depth : 0, .. } =>
        render_root (pid, has_children, addresses, report, &mut out),
      Entry::Note { pid, depth, listed_as } =>
        render_bullet (pid, *depth, listed_as, has_children, addresses, report, &mut out),
      Entry::Embed { pid, depth, listed_as } => {
        report . embeds += 1;
        let mut meta : Map<String, Value> = Map::new ();
        if listed_as != pid {
          meta . insert (KEY_REF . to_string (), Value::from (listed_as . as_str ())); }
        out . push_str ( & format! ("{}- !{}{}\n",
          indent_of_bullet (*depth), wikilink_target (pid, addresses),
          if meta . is_empty () { String::new () }
          else { format! (" {}", meta_comment (&meta)) } )); },
      Entry::Anchor { depth, anchor } => {
        let meta : Map<String, Value> = Map::from_iter ([
          (KEY_ANCHOR . to_string (), Value::from (anchor . as_str ())) ]);
        out . push_str ( & format! ("{}- {}\n", indent_of_bullet (*depth), meta_comment (&meta) )); },
      Entry::Absent { depth, listed_as } => {
        report . absent_members += 1;
        let meta : Map<String, Value> = Map::from_iter ([
          (KEY_ABSENT . to_string (), Value::from (listed_as . as_str ())) ]);
        out . push_str ( & format! ("{}- [[{}|{}]] {}\n",
          indent_of_bullet (*depth), outside_target (addresses),
          ABSENT_MEMBER_TEXT, meta_comment (&meta) )); }, }}
  out }

fn indent_of_bullet (
  depth : usize,
) -> String {
  " " . repeat (2 * (depth - 1)) }

fn render_root (
  pid          : &ID,
  has_children : bool,
  addresses    : &Addresses,
  report       : &mut ObsidianExportReport,
  out          : &mut String,
) {
  let section : &GraphnodeOnDisk = addresses . index . by_pid [pid];
  let (title_text, exact_title) : (String, Option<String>) =
    render_title (section, addresses, report);
  let mut properties : Vec<(String, PropertyValue)> =
    vec! [ (KEY_ID . to_string (), PropertyValue::Text (pid . as_str () . to_string ())) ];
  if let Some (title) = exact_title {
    properties . push ((KEY_TITLE . to_string (), PropertyValue::Text (title))); }
  properties . extend (note_properties (section, addresses, true));
  out . push_str ( &render_frontmatter (&properties) );
  out . push_str ( & format! ("# {}\n", title_text) );
  if let Some (lines) = render_body (section, addresses, report) {
    out . push ('\n');
    push_indented (&lines, 0, out); }
  if has_children { out . push ('\n'); }}

fn render_bullet (
  pid          : &ID,
  depth        : usize,
  listed_as    : &ID,
  has_children : bool,
  addresses    : &Addresses,
  report       : &mut ObsidianExportReport,
  out          : &mut String,
) {
  let section : &GraphnodeOnDisk = addresses . index . by_pid [pid];
  let (title_text, exact_title) : (String, Option<String>) =
    render_title (section, addresses, report);
  let mut meta : Map<String, Value> = Map::new ();
  if let Some (title) = exact_title {
    meta . insert (KEY_TITLE . to_string (), Value::from (title)); }
  for (key, value) in note_properties (section, addresses, false) {
    meta . insert (key, value . to_json ()); }
  if listed_as != pid {
    meta . insert (KEY_REF . to_string (), Value::from (listed_as . as_str ())); }
  let content_column : usize = 2 * depth;
  out . push_str ( & format! ("{}- {} ^{}\n",
    indent_of_bullet (depth), title_text, block_id_of_pid (pid) ));
  let mut wrote_more : bool = false;
  if ! meta . is_empty () {
    out . push ('\n');
    push_indented (&[meta_comment (&meta)], content_column, out);
    wrote_more = true; }
  if let Some (lines) = render_body (section, addresses, report) {
    out . push ('\n');
    push_indented (&lines, content_column, out);
    wrote_more = true; }
  if wrote_more && has_children { out . push ('\n'); }}

/// Each line indented by 'column' spaces, except that an empty line
/// stays empty.
fn push_indented (
  lines  : &[String],
  column : usize,
  out    : &mut String,
) {
  let indent : String = " " . repeat (column);
  for line in lines {
    if ! line . is_empty () { out . push_str (&indent); }
    out . push_str (line);
    out . push ('\n'); }}

/// The title as written (H1 or bullet text), and, when that text
/// cannot give it back exactly, the exact title for 'skg-title'.
fn render_title (
  section   : &GraphnodeOnDisk,
  addresses : &Addresses,
  report    : &mut ObsidianExportReport,
) -> (String, Option<String>) {
  let Some (title) = section . title . as_deref ()
    else { return (SECTION_TITLE_MARK . to_string (), None); };
  let is_plain : bool =
    title . trim () == title && ! title . is_empty ()
    && ! title . chars () . any ( |c| c . is_control () && c != '\t' );
  if is_plain {
    if let Some ((rendered, counts, tally)) = render_text_guarded (title, true, addresses) {
      counts . add_to (report);
      report . titles_markup . add (&tally);
      return (rendered, None); }}
  report . title_fallbacks += 1;
  let shown : String = title . chars ()
    . map ( |c| if c . is_control () { ' ' } else { c } )
    . collect::<String> () . trim () . to_string ();
  let shown : String =
    if shown . is_empty () { "(blank title)" . to_string () }
    else { render_text (&shown, true, false, addresses) . 0 };
  (shown, Some (title . to_string ())) }

/// The body's lines, unindented; None if it has none.
fn render_body (
  section   : &GraphnodeOnDisk,
  addresses : &Addresses,
  report    : &mut ObsidianExportReport,
) -> Option<Vec<String>> {
  let body : &str = section . body . as_deref () ?;
  report . bodies += 1;
  let is_normal : bool =
    normalize_body (Some (body . to_string ())) . as_deref () == Some (body)
    && ! body . chars () . any ( |c| c . is_control () && c != '\n' && c != '\t' );
  if is_normal {
    if let Some ((rendered, counts, tally)) = render_text_guarded (body, false, addresses) {
      counts . add_to (report);
      report . bodies_markup . add (&tally);
      return Some ( rendered . split ('\n') . map (String::from) . collect () ); }}
  report . body_fallbacks += 1;
  let has_awkward_chars : bool = body . chars ()
    . any ( |c| c . is_control () && c != '\n' && c != '\t' );
  Some (
    if has_awkward_chars {
      let json : String = Value::from (body) . to_string ();
      let fence : String = fence_for (&json);
      vec! [ format! ("{}{}", fence, VERBATIM_JSON_FENCE_INFO), json, fence ] }
    else {
      let fence : String = fence_for (body);
      let mut lines : Vec<String> = vec! [ format! ("{}{}", fence, VERBATIM_FENCE_INFO) ];
      lines . extend ( body . split ('\n') . map (String::from) );
      lines . push (fence);
      lines } ) }

//
// Text: links and escapes
//

/// How the links of one text were written.
#[derive(Default)]
struct LinkCounts {
  links              : usize,
  links_outside      : usize,
  links_left_as_text : usize,
}

impl LinkCounts {
  fn add_to (
    &self,
    report : &mut ObsidianExportReport,
  ) {
    report . links              += self . links;
    report . links_outside      += self . links_outside;
    report . links_left_as_text += self . links_left_as_text; }
}

/// 'text' rendered with its Org markup converted, if that reads back
/// exactly, else without; None if neither does.
fn render_text_guarded (
  text      : &str,
  is_title  : bool,
  addresses : &Addresses,
) -> Option<(String, LinkCounts, MarkupTally)> {
  let reads_back = |rendered : &str| -> bool {
    unrender_for_check (rendered, addresses) . as_deref () == Some (text) };
  let (rendered, counts, tally) = render_text (text, is_title, true, addresses);
  if reads_back (&rendered) { return Some ((rendered, counts, tally)); }
  let (rendered, counts, mut plain_tally) = render_text (text, is_title, false, addresses);
  if ! reads_back (&rendered) { return None; }
  plain_tally . declined = tally . declined . into_iter ()
    . chain ( (! tally . converted . is_empty ()) . then_some (WHY_GUARD) )
    . collect::<BTreeSet<&'static str>> ();
  Some ((rendered, counts, plain_tally)) }

/// 'text' (a title, or a body) as Markdown: each link the graph counts
/// becomes a wikilink, the text's own code fences and code spans are
/// copied, Org markup becomes Markdown if 'convert'
/// ('org_markup.rs'), and everything else is escaped.
fn render_text (
  text      : &str,
  is_title  : bool,
  convert   : bool,
  addresses : &Addresses,
) -> (String, LinkCounts, MarkupTally) {
  let mut counts : LinkCounts = LinkCounts::default ();
  let mut tally : MarkupTally = MarkupTally::default ();
  let mut spans : Vec<(Range<usize>, Span)> =
    if is_title { Vec::new () }
    else { org_markup::block_spans (text, convert, &mut tally) };
  let ranges_of = |spans : &[(Range<usize>, Span)]| -> Vec<Range<usize>> {
    spans . iter () . map ( |(range, _)| range . clone () ) . collect () };
  let blocks : Vec<Range<usize>> = ranges_of (&spans);
  for (range, link) in links_with_ranges_from_text (text) {
    let in_a_block : bool = blocks . iter ()
      . any ( |block| block . start < range . end && range . start < block . end );
    if in_a_block { continue; } // code, not a link
    match wikilink_for (&link . gnid, &link . label, addresses) {
      Some ((wikilink, is_outside)) => {
        counts . links += 1;
        if is_outside { counts . links_outside += 1; }
        spans . push ((range, Span::Written (wikilink))); },
      None => counts . links_left_as_text += 1, }}
  if convert {
    let taken : Vec<Range<usize>> = ranges_of (&spans);
    spans . extend (org_markup::inline_org_spans (text, &taken, &mut tally)); }
  { let taken : Vec<Range<usize>> = ranges_of (&spans);
    spans . extend (org_markup::code_spans (text, &taken)); }
  if convert {
    let taken : Vec<Range<usize>> = ranges_of (&spans);
    spans . extend (org_markup::emphasis_spans (text, &taken, &mut tally)); }
  spans . sort_by_key ( |(range, _)| range . start );
  let mut pieces : Vec<Piece> = Vec::new ();
  let mut copied_to : usize = 0;
  for (range, span) in spans {
    if copied_to < range . start {
      pieces . push (Piece::Text (&text [copied_to .. range . start])); }
    pieces . push ( match span {
      Span::Written (written) => Piece::Produced (written),
      Span::Copied            => Piece::Verbatim (&text [range . clone ()]), } );
    copied_to = range . end; }
  if copied_to < text . len () {
    pieces . push (Piece::Text (&text [copied_to ..])); }
  (escape_pieces (&pieces, is_title), counts, tally) }

/// The wikilink for '[[id:RAW][LABEL]]', and whether its note is
/// outside the vault; None when the label or ID cannot ride in one.
fn wikilink_for (
  raw       : &ID,
  label     : &str,
  addresses : &Addresses,
) -> Option<(String, bool)> {
  let label_fits : bool =
    ! label . contains ("]]") && ! label . contains ("[[") && ! label . contains ('|')
    && ! label . contains ("%%") && ! label . ends_with (']') && ! label . contains ('\n')
    && ! label . contains ('`'); // a code span would take the link apart
  let raw_fits : bool = // so that its comment ends where it should
    ! raw . as_str () . contains ("%%") && ! raw . as_str () . ends_with ('%')
    && ! raw . as_str () . contains ('\n') && ! raw . as_str () . is_empty ();
  if ! label_fits || ! raw_fits { return None; }
  let resolved : Option<ID> = addresses . index . resolve (raw);
  let (target, says_raw_exactly, is_outside) : (String, bool, bool) = match &resolved {
    Some (pid) => (link_target (pid, addresses), pid == raw, false),
    None => (outside_target (addresses), false, true), };
  let comment : String =
    if says_raw_exactly { String::new () }
    else { format! ("{}{}{}", LINK_ID_COMMENT_OPEN, raw . as_str (), COMMENT_CLOSE) };
  Some (( format! ("[[{}|{}]]{}", target, label, comment), is_outside )) }

/// Where a link to a note outside the vault points: the "Some links
/// might be broken" note if the vault has it, else the README's
/// explanation.
fn outside_target (
  addresses : &Addresses,
) -> String {
  let sink : ID = ID::from (BROKEN_LINK_SINK_ID);
  if addresses . homes . contains_key (&sink) { link_target (&sink, addresses) }
  else { format! ("{}#{}", README_STEM, OUTSIDE_VAULT_HEADING) }}

/// 'File' for a document's root, 'File#^BLOCKID' for any other note.
fn link_target (
  pid       : &ID,
  addresses : &Addresses,
) -> String {
  let home : &Home = &addresses . homes [pid];
  let stem : &str = &addresses . documents [home . document] . stem;
  if home . is_root { stem . to_string () }
  else { format! ("{}#^{}", stem, block_id_of_pid (pid)) }}

fn wikilink_target (
  pid       : &ID,
  addresses : &Addresses,
) -> String {
  format! ("[[{}]]", link_target (pid, addresses)) }

/// The text 'rendered' stands for, read back as the importer reads it;
/// None if it does not parse. A guard: what it cannot give back
/// exactly is written another way.
fn unrender_for_check (
  rendered  : &str,
  addresses : &Addresses,
) -> Option<String> {
  unrender_text (rendered, &|stem : &str| {
    addresses . root_of_stem . get (stem) . map ( |pid| (*pid) . clone () ) } ) }

//
// Properties and metadata
//

enum PropertyValue {
  Text (String),
  List (Vec<String>),
  Checkbox (bool),
}

impl PropertyValue {
  fn to_json (
    &self,
  ) -> Value {
    match self {
      PropertyValue::Text (t)     => Value::from (t . as_str ()),
      PropertyValue::List (items) => Value::from (items . clone ()),
      PropertyValue::Checkbox (b) => Value::from (*b), }}
}

/// The metadata of 'section' other than its ID and title, as
/// properties in their order. Forks and hides of notes in the vault
/// are links when 'as_links' (in properties, where Obsidian shows
/// them as backlinks); in a comment they stay IDs.
fn note_properties (
  section   : &GraphnodeOnDisk,
  addresses : &Addresses,
  as_links  : bool,
) -> Vec<(String, PropertyValue)> {
  let mut out : Vec<(String, PropertyValue)> = Vec::new ();
  let id_entry = |raw : &ID| -> String {
    match addresses . index . resolve (raw) {
      Some (pid) if as_links && pid == *raw => wikilink_target (&pid, addresses),
      _ => raw . as_str () . to_string (), }};
  if ! section . aliases . is_empty () {
    out . push ((KEY_ALIASES . to_string (), PropertyValue::List (section . aliases . clone ()))); }
  if ! section . extra_ids . is_empty () {
    out . push ((KEY_EXTRA_IDS . to_string (), PropertyValue::List (
      section . extra_ids . iter () . map ( |id| id . as_str () . to_string () ) . collect () ))); }
  if ! section . forks . is_empty () {
    out . push ((KEY_FORKS . to_string (), PropertyValue::List (
      section . forks . iter () . map ( |item| match item {
        ListItem::Member (raw) => id_entry (raw),
        ListItem::Anchor { anchor } => format! ("{}{}", ANCHOR_ENTRY_PREFIX, anchor . as_str ()), } )
      . collect () ))); }
  if ! section . hides . is_empty () {
    out . push ((KEY_HIDES . to_string (), PropertyValue::List (
      section . hides . iter () . map (id_entry) . collect () ))); }
  if ! section . flags . is_empty () {
    out . push ((KEY_FLAGS . to_string (), PropertyValue::List (
      section . flags . iter () . map (flag_name) . collect () ))); }
  if let Some (attribution) = &section . attribution {
    out . extend (attribution_properties (attribution)); }
  out }

pub fn flag_name (
  flag : &Flag,
) -> String {
  match serde_json::to_value (flag) {
    Ok (Value::String (name)) => name,
    _ => unreachable! ("a Flag serializes as its name"), }}

fn attribution_properties (
  attribution : &Attribution,
) -> Vec<(String, PropertyValue)> {
  let mut out : Vec<(String, PropertyValue)> = vec! [
    (KEY_ATTRIBUTION_ORIGINAL . to_string (),
     PropertyValue::Text (attribution . original . as_str () . to_string ())),
    (KEY_ATTRIBUTION_SKGREPO . to_string (),
     PropertyValue::Text (attribution . skgrepo . as_str () . to_string ())) ];
  if let Some (hash) = &attribution . remote_hash {
    out . push ((KEY_ATTRIBUTION_REMOTE_HASH . to_string (), PropertyValue::Text (hash . clone ()))); }
  if let Some (commit) = &attribution . commit {
    out . push ((KEY_ATTRIBUTION_COMMIT . to_string (), PropertyValue::Text (commit . clone ()))); }
  if attribution . uncommitted {
    out . push ((KEY_ATTRIBUTION_UNCOMMITTED . to_string (), PropertyValue::Checkbox (true))); }
  out }

/// YAML front matter. Each string is a JSON string, which YAML reads
/// as a double-quoted scalar, so no value can be misread.
fn render_frontmatter (
  properties : &[(String, PropertyValue)],
) -> String {
  let mut out : String = String::from ("---\n");
  for (key, value) in properties {
    match value {
      PropertyValue::Text (text) =>
        out . push_str ( & format! ("{}: {}\n", key, Value::from (text . as_str ())) ),
      PropertyValue::Checkbox (b) =>
        out . push_str ( & format! ("{}: {}\n", key, b) ),
      PropertyValue::List (items) => {
        out . push_str ( & format! ("{}:\n", key) );
        for item in items {
          out . push_str ( & format! ("  - {}\n", Value::from (item . as_str ())) ); }}, }}
  out . push_str ("---\n");
  out }

//
// Manifest and README
//

/// The README: its properties describe the vault (the format, and
/// for each skgrepo folder its skgrepo, remote hash and commit, which
/// attribute a responder's forks); its text explains the format.
fn render_readme (
  folder    : &str,
  git_facts : &SkgrepoGitFacts,
) -> String {
  let one = |text : &str| -> PropertyValue { PropertyValue::List (vec! [ text . to_string () ]) };
  let properties : Vec<(String, PropertyValue)> = vec! [
    (KEY_FORMAT . to_string (), PropertyValue::Text (VAULT_FORMAT . to_string ())),
    (KEY_FOLDERS . to_string (), one (folder)),
    (KEY_SKGREPOS . to_string (), one (folder)),
    (KEY_REMOTE_HASHES . to_string (), one (git_facts . remote_hash . as_deref () . unwrap_or (""))),
    (KEY_COMMITS . to_string (), one (git_facts . commit . as_deref () . unwrap_or (""))) ];
  format! (
"{frontmatter}# About this vault
This vault was exported from skg, a knowledge-graph editor whose notes live in '.skg' files. Each folder holds one *skgrepo*, a set of notes at one privacy level; this export holds the skgrepo `{folder}`. The properties above say which version of the format this is, and for each folder which skgrepo it holds and the Git commit it was exported from.

Each file is a *document*: a note that no other note in the skgrepo contains (or one that forks another note), with everything it contains. Its title is the heading at the top, and its ID and other metadata are the properties above it.

Every other note is a bullet. The bullet ends with the note's ID as a block ID (`^...`), its body is indented beneath it, and the notes it contains are nested beneath that. A note contained in several places is written once, where it is first reached, and shown elsewhere as an embed (`![[...]]`), which stays current.

Links are written as `[[File#^ID|label]]`, so they survive renamed files. Metadata Obsidian has no place for (extra IDs, flags, attributions, hidden notes) rides in comments, `%%skg {{...}}%%`, which Reading view hides.

The notes are written in Org, and their markup is shown as Markdown where that can be undone exactly: Org's bold and italic, verbatim and code, links to web pages, source and example blocks (as code blocks fenced by tildes, whose first line says what they were), fixed-width lines, and the rule under a table's first row. Other Org markup shows as the note has it. A backslash before a character keeps it from being read as Markdown; the character itself is what the note says.

Please do not edit these files in place: the next export overwrites them, and removes those it no longer writes. Files of your own, and copies of exported files, are left alone.

## {heading}
A link that points here leads to a note that is not in this vault, usually because it is private. The link keeps its label, and the comment after it (`%%skg-id ...%%`) keeps the note's ID.
",
    frontmatter = render_frontmatter (&properties),
    folder = folder,
    heading = OUTSIDE_VAULT_HEADING ) }

#[cfg(test)]
#[path = "../../tests/unit/obsidian_export.rs"]
mod tests;
