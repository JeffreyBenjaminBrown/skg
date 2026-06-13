//! Export .skg content to plain .org files.
//!
//! See TODO/export-to-org/ (plan.org, discussion.org). In brief:
//! every "export root" -- a node with a child whose title links to
//! the instruction node EXPORT_MARKER_ID and whose body yields a
//! `target_filepath` -- is written to
//! `<output_base>/<target_filepath>.org` as a recursive content
//! view, limited to a chosen source-set, stripped of skg metadata,
//! with `[[id:..][label]]` links rewritten to relative org links.
//!
//! The core (`export_to_org`) takes nodes + an ActiveSourceSet + an
//! output base, so it needs neither TypeDB nor Tantivy and is
//! unit-testable. The server handler and the `export-org`
//! subcommand both call it.

use crate::source_sets::ActiveSourceSet;
use crate::types::misc::ID;
use crate::types::nodes::complete::NodeComplete;
use crate::types::textlinks::replace_each_link_with_its_label;

use regex::Regex;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fs;
use std::path::Path;
use std::sync::LazyLock;

/// A child whose title links to this node marks its parent as an
/// export root. (The node itself is documentation; it is never a
/// marker, since its title does not link to itself.)
pub const EXPORT_MARKER_ID : &str =
  "3d9aa9be-d95a-48bc-b362-33f9e7ebdf6f";

/// Broken links (whose target is not exported under the chosen
/// source-set) point at the export of this node.
pub const BROKEN_LINK_SINK_ID : &str =
  "9ff04e25-01e8-4634-8aa5-f5849bc1eb81";

static TEXTLINK_PATTERN : LazyLock<Regex> =
  LazyLock::new ( || Regex::new (
    r"\[\[id:(.*?)\]\[(.*?)\]\]" ) . unwrap () );

//
// Public types
//

pub struct ExportReport {
  pub files_written : Vec<String>, // relative paths, e.g. "docs/setup.org"
  pub roots_found   : usize,
  pub broken_links  : usize,
  pub warnings      : Vec<String>,
}

impl ExportReport {
  /// A human-readable summary (the `content` field of the wire
  /// response, and what the CLI prints). Warnings are reported
  /// separately, via `self.warnings`.
  pub fn summary (
    &self,
  ) -> String {
    let mut s : String = String::new ();
    s . push_str ( & format! (
      "Exported {} file(s) from {} export root(s).\n",
      self . files_written . len (),
      self . roots_found ) );
    for f in &self . files_written {
      s . push_str ( & format! ("  org-exports/{}\n", f) ); }
    if self . broken_links > 0 {
      s . push_str ( & format! (
        "{} broken link(s) (pointed at the broken-link note, or \
         shown as plain text if that note is not exported).\n",
        self . broken_links ) ); }
    if ! self . warnings . is_empty () {
      s . push_str ( & format! (
        "{} warning(s):\n", self . warnings . len () ) );
      for w in &self . warnings {
        s . push_str ( & format! ("  - {}\n", w) ); }}
    s }
}

//
// Internal types
//

struct ExportRoot {
  root_pid : ID,
  target   : String, // relative, no extension, e.g. "docs/setup"
}

enum HomeKind {
  Root    { title  : String }, // links to the file (top), or *title same-file
  Content { anchor : String }, // links to *anchor (deep)
}

/// Where a node id lives in the export: which file, and how to
/// address it within / across files.
struct Home {
  target : String,
  kind   : HomeKind,
}

enum EvKind {
  Normal,   // an ordinary content node: headline + body, recurse
  LinkLeaf, // a nested export root, or a repeat already rendered in
            // this file (a diamond's later branch, or a cycle):
            // render as a link to its home, do not recurse
}

struct Ev {
  pid   : ID,
  depth : usize,
  kind  : EvKind,
}

//
// Entry point
//

/// Render and write every export root reachable in `nodes` under
/// `active`, into `output_base`. Pure of TypeDB/Tantivy; does
/// filesystem writes only under `output_base`.
pub fn export_to_org (
  active      : &ActiveSourceSet,
  nodes       : &[NodeComplete],
  output_base : &Path,
) -> Result<ExportReport, Box<dyn Error>> {
  let mut warnings : Vec<String> = Vec::new ();
  let by_pid : HashMap<ID, &NodeComplete> =
    nodes . iter ()
    . map ( |n| (n . pid . clone (), n) )
    . collect ();
  let alias_to_pid : HashMap<ID, ID> = {
    let mut m : HashMap<ID, ID> = HashMap::new ();
    for n in nodes {
      for extra in &n . extra_ids {
        m . insert ( extra . clone (), n . pid . clone () ); }}
    m };

  let (roots_by_pid, marker_pids) : (HashMap<ID, ExportRoot>,
                                     HashSet<ID>) =
    discover_roots (
      nodes, &alias_to_pid, active, &mut warnings );

  // Process roots in a deterministic order (by target), so that the
  // canonical home of a multiply-contained node is stable.
  let mut roots : Vec<&ExportRoot> =
    roots_by_pid . values () . collect ();
  roots . sort_by ( |a, b| a . target . cmp (&b . target) );

  // One event stream per root, reused for both home-building and
  // rendering so anchors are guaranteed consistent.
  let root_events : Vec<(&ExportRoot, Vec<Ev>)> =
    roots . iter ()
    . map ( |r| ( *r,
                  collect_events (
                    &r . root_pid, &by_pid, &alias_to_pid,
                    &roots_by_pid, &marker_pids, active ) ) )
    . collect ();

  let homes : HashMap<ID, Home> =
    build_homes (&roots, &root_events, &by_pid, &mut warnings);

  let mut report : ExportReport = ExportReport {
    files_written : Vec::new (),
    roots_found   : roots . len (),
    broken_links  : 0,
    warnings,        // moved; further warnings push via report.warnings
  };
  for (root, events) in &root_events {
    let content : String =
      render_root (
        root, events, &homes, &by_pid, &alias_to_pid,
        &mut report . broken_links, &mut report . warnings );
    let rel : String = format! ("{}.org", root . target);
    let path : std::path::PathBuf = output_base . join (&rel);
    // Best-effort and non-destructive (Q6): a single bad target or
    // I/O error becomes a warning, not an abort, so one misconfigured
    // root cannot take down every other export.
    match write_export_file (&path, &content) {
      Ok (())  => report . files_written . push (rel),
      Err (e) => report . warnings . push ( format! (
        "could not write {} (root {}): {}", rel, root . root_pid, e )), }}
  report . files_written . sort ();
  Ok (report) }

fn write_export_file (
  path    : &Path,
  content : &str,
) -> Result<(), Box<dyn Error>> {
  if let Some (parent) = path . parent () {
    fs::create_dir_all (parent) ?; }
  fs::write (path, content) ?;
  Ok (()) }

//
// Discovery
//

/// Returns (root_pid -> ExportRoot, set of marker child pids).
///
/// A MARKER is a node whose title links to EXPORT_MARKER_ID AND
/// whose body yields a `target_filepath` -- the precise definition
/// from the spec. A node that merely links to the instruction node
/// in its title for documentation (no target_filepath) is NOT a
/// marker: it renders as ordinary content. Marker pids are excluded
/// from rendered content; a node whose title links to the
/// instruction node but whose target_filepath is malformed is also
/// excluded (it is a broken marker, not content) and warned about,
/// but does not mark a parent for export.
///
/// A node is an export ROOT iff one of its `contains` children (in
/// order) is a marker; that child's target_filepath is the file.
fn discover_roots (
  nodes        : &[NodeComplete],
  alias_to_pid : &HashMap<ID, ID>,
  active       : &ActiveSourceSet,
  warnings     : &mut Vec<String>,
) -> (HashMap<ID, ExportRoot>, HashSet<ID>) {
  let marker_id : ID = ID::from (EXPORT_MARKER_ID);
  // Sorted, so warnings and "first wins" are deterministic.
  let mut sorted : Vec<&NodeComplete> = nodes . iter () . collect ();
  sorted . sort_by ( |a, b| a . pid . cmp (&b . pid) );

  let mut marker_pids   : HashSet<ID> = HashSet::new ();
  let mut marker_target : HashMap<ID, String> = HashMap::new ();
  for n in &sorted {
    if ! title_links_to (n, &marker_id, alias_to_pid) { continue; }
    match parse_target_filepath (n . body . as_deref ()) {
      Ok (Some (t)) => {
        marker_pids . insert (n . pid . clone ());
        marker_target . insert (n . pid . clone (), t); },
      Ok (None) => {}, // links to the instruction node, but is content
      Err (e) => {
        marker_pids . insert (n . pid . clone ());
        warnings . push ( format! (
          "node {} links to the export-instruction node but its \
           target_filepath is invalid: {}; it will not mark a \
           parent for export",
          n . pid, e ) ); }, }}

  let mut roots : HashMap<ID, ExportRoot> = HashMap::new ();
  let mut target_owner : HashMap<String, ID> = HashMap::new ();
  for parent in &sorted {
    let marker_children : Vec<String> =
      parent . contains . iter ()
      . filter_map ( |cid|
        marker_target . get (
          &resolve_pid (cid, alias_to_pid)) . cloned () )
      . collect ();
    let target : String = match marker_children . first () {
      Some (t) => t . clone (),
      None     => continue, };
    if marker_children . len () > 1 {
      warnings . push ( format! (
        "node {} has more than one export-marker child; using the \
         first ({:?})",
        parent . pid, target ) ); }
    if ! node_active (parent, active) {
      warnings . push ( format! (
        "export root {} is in source {} which is inactive under \
         source-set {}; skipping",
        parent . pid, parent . source, active . name ) );
      continue; }
    if let Some (owner) = target_owner . get (&target) {
      warnings . push ( format! (
        "target_filepath {:?} is claimed by both {} and {}; \
         keeping {}",
        target, owner, parent . pid, owner ) );
      continue; }
    target_owner . insert ( target . clone (), parent . pid . clone () );
    roots . insert (
      parent . pid . clone (),
      ExportRoot {
        root_pid : parent . pid . clone (),
        target, } ); }
  (roots, marker_pids) }

/// True if any textlink in `node`'s title points (after alias
/// resolution) at `id`. Alias resolution matches the rewriting and
/// traversal paths, so an extra-id link to the instruction node is
/// still recognized.
fn title_links_to (
  node         : &NodeComplete,
  id           : &ID,
  alias_to_pid : &HashMap<ID, ID>,
) -> bool {
  TEXTLINK_PATTERN . captures_iter (&node . title)
    . any ( |c| resolve_pid (&ID::from (&c[1]), alias_to_pid) == *id ) }

/// Tolerant parse of a marker body. Accepts `target_filepath = X`
/// where X is bare or quoted. A bare value with whitespace is
/// refused (Q3). Absolute paths and `.`/`..` components are refused
/// (so a target cannot escape the export directory). Returns the
/// relative path without extension, or None if no target_filepath
/// line is present.
fn parse_target_filepath (
  body : Option<&str>,
) -> Result<Option<String>, String> {
  let body : &str = match body {
    Some (b) => b,
    None     => return Ok (None), };
  for line in body . lines () {
    let line : &str = line . trim ();
    if line . is_empty () || line . starts_with ('#') { continue; }
    let (key, rhs) : (&str, &str) = match line . split_once ('=') {
      Some (kv) => kv,
      None      => continue, };
    if key . trim () != "target_filepath" { continue; }
    let raw : &str = rhs . trim ();
    let (value, quoted) : (String, bool) =
      if raw . len () >= 2
         && ( ( raw . starts_with ('"')  && raw . ends_with ('"') )
              || ( raw . starts_with ('\'') && raw . ends_with ('\'') ) ) {
        ( raw [1 .. raw . len () - 1] . to_string (), true )
      } else {
        ( raw . to_string (), false ) };
    if ! quoted && value . chars () . any (|c| c . is_whitespace ()) {
      return Err ( format! (
        "target_filepath value {:?} contains whitespace but is not \
         quoted; write target_filepath = \"with space\" if intended",
        value ) ); }
    let value : &str =
      value . strip_prefix ("./") . unwrap_or (&value);
    if value . is_empty () {
      return Err ( "target_filepath is empty" . to_string () ); }
    for comp in value . split ('/') {
      if comp . is_empty () || comp == "." || comp == ".." {
        return Err ( format! (
          "target_filepath {:?} must be a relative path with no \
           empty, '.', or '..' components",
          value ) ); }}
    return Ok ( Some ( value . to_string () ) ); }
  Ok (None) }

//
// Event collection (one DFS used for homes and rendering)
//

/// Preorder DFS over the content tree of one root, producing the
/// event stream rendered (and used to build homes). Iterative (an
/// explicit stack), so an arbitrarily deep outline cannot overflow
/// the call stack. A monotonic per-file `rendered` set means each
/// node is expanded at most ONCE per file: a node reached again
/// (whether as a diamond's later branch or as a cycle back to an
/// ancestor) becomes a LinkLeaf to its home rather than being
/// re-expanded -- so a wide DAG cannot blow up and content is not
/// duplicated within a file.
fn collect_events (
  root_pid     : &ID,
  by_pid       : &HashMap<ID, &NodeComplete>,
  alias_to_pid : &HashMap<ID, ID>,
  roots_by_pid : &HashMap<ID, ExportRoot>,
  marker_pids  : &HashSet<ID>,
  active       : &ActiveSourceSet,
) -> Vec<Ev> {
  let mut out : Vec<Ev> = Vec::new ();
  let mut rendered : HashSet<ID> = HashSet::new ();
  // (pid, depth, is_root)
  let mut stack : Vec<(ID, usize, bool)> =
    vec! [ (root_pid . clone (), 1, true) ];
  while let Some ((pid, depth, is_root)) = stack . pop () {
    let node : &NodeComplete = match by_pid . get (&pid) {
      Some (n) => n,
      None     => continue, };
    if ( ! is_root && roots_by_pid . contains_key (&pid) )
       || rendered . contains (&pid) {
      out . push ( Ev { pid, depth, kind : EvKind::LinkLeaf } );
      continue; }
    rendered . insert ( pid . clone () );
    out . push ( Ev { pid, depth, kind : EvKind::Normal } );
    // Collect valid children in order, then push reversed so they
    // pop (and thus render) in forward order.
    let mut kids : Vec<ID> = Vec::new ();
    for child_id in &node . contains {
      let cpid : ID = resolve_pid (child_id, alias_to_pid);
      if marker_pids . contains (&cpid) { continue; } // markers never render
      let child : &NodeComplete = match by_pid . get (&cpid) {
        Some (c) => c,
        None     => continue, }; // dangling contains entry: silently skip
      if ! node_active (child, active) { continue; } // omit inactive (recursive)
      kids . push (cpid); }
    for cpid in kids . into_iter () . rev () {
      stack . push ( (cpid, depth + 1, false) ); }}
  out }

//
// Homes
//

fn build_homes (
  roots       : &[&ExportRoot],
  root_events : &[(&ExportRoot, Vec<Ev>)],
  by_pid      : &HashMap<ID, &NodeComplete>,
  warnings    : &mut Vec<String>,
) -> HashMap<ID, Home> {
  let mut homes : HashMap<ID, Home> = HashMap::new ();
  // A node that is itself a root always homes to its own file.
  for r in roots {
    let title : String = match by_pid . get (&r . root_pid) {
      Some (n) => anchor_text (n),
      None     => String::new (), };
    homes . insert (
      r . root_pid . clone (),
      Home { target : r . target . clone (),
             kind   : HomeKind::Root { title } } ); }
  // Then non-root content nodes; first appearance (in target order)
  // wins. Pre-seed each file's anchor set with its root heading.
  let mut anchors_per_file : HashMap<String, HashSet<String>> =
    HashMap::new ();
  for r in roots {
    if let Some (n) = by_pid . get (&r . root_pid) {
      anchors_per_file . entry (r . target . clone ())
        . or_default () . insert ( anchor_text (n) ); }}
  for (root, events) in root_events {
    for ev in events {
      if ! matches! (ev . kind, EvKind::Normal) { continue; }
      if homes . contains_key (&ev . pid) { continue; }
      let node : &NodeComplete = match by_pid . get (&ev . pid) {
        Some (n) => n,
        None     => continue, };
      let anchor : String = anchor_text (node);
      let file_anchors : &mut HashSet<String> =
        anchors_per_file . entry (root . target . clone ())
        . or_default ();
      if ! file_anchors . insert (anchor . clone ()) {
        warnings . push ( format! (
          "ambiguous heading {:?} in {}.org: deep links to it \
           resolve to the first occurrence",
          anchor, root . target ) ); }
      homes . insert (
        ev . pid . clone (),
        Home { target : root . target . clone (),
               kind   : HomeKind::Content { anchor } } ); }}
  homes }

//
// Rendering
//

fn render_root (
  root         : &ExportRoot,
  events       : &[Ev],
  homes        : &HashMap<ID, Home>,
  by_pid       : &HashMap<ID, &NodeComplete>,
  alias_to_pid : &HashMap<ID, ID>,
  broken       : &mut usize,
  warnings     : &mut Vec<String>,
) -> String {
  let target : &str = &root . target;
  let mut out : String = String::new ();
  for ev in events {
    let node : &NodeComplete = match by_pid . get (&ev . pid) {
      Some (n) => n,
      None     => continue, };
    let stars : String = "*" . repeat (ev . depth);
    // A heading is always the title's plain-label form (links
    // collapsed to their labels). This keeps the headline text
    // identical to the anchor used for deep links to it, and keeps
    // fragile link markup out of headlines; links live in bodies.
    let label : String = anchor_text (node);
    match ev . kind {
      EvKind::LinkLeaf => {
        let link : String = match homes . get (&ev . pid) {
          Some (home) => org_link_for_home (home, target, &label),
          None        => label, }; // unreachable: link-leaf targets are homed
        out . push_str ( & format! ("{} {}\n", stars, link) ); },
      EvKind::Normal => {
        out . push_str ( & format! ("{} {}\n", stars, label) );
        if let Some (body) = node . body . as_deref () {
          if ! body . trim () . is_empty () {
            let body : String =
              rewrite_links ( body, target, homes,
                              alias_to_pid, broken, warnings );
            warn_body_structure (&node . pid, &body, warnings);
            out . push_str ( body . trim_end_matches ('\n') );
            out . push ('\n'); }} }, }}
  out }

/// Rewrite every `[[id:UID][label]]` in `text` to a relative org
/// link, resolving UID to its export home (or the broken-link sink).
fn rewrite_links (
  text           : &str,
  current_target : &str,
  homes          : &HashMap<ID, Home>,
  alias_to_pid   : &HashMap<ID, ID>,
  broken         : &mut usize,
  warnings       : &mut Vec<String>,
) -> String {
  let sink_pid : ID = ID::from (BROKEN_LINK_SINK_ID);
  TEXTLINK_PATTERN . replace_all (text, |caps : &regex::Captures| {
    let uid : ID = ID::from (&caps[1]);
    let label : &str = &caps[2];
    let pid : ID =
      alias_to_pid . get (&uid) . cloned () . unwrap_or (uid);
    match homes . get (&pid) {
      Some (home) => org_link_for_home (home, current_target, label),
      None => {
        *broken += 1;
        match homes . get (&sink_pid) {
          Some (sink) =>
            org_link_for_home (sink, current_target, label),
          None => {
            warnings . push ( format! (
              "broken link to {} ({:?}): not exported under this \
               source-set, and the broken-link note is not either; \
               emitting the label as plain text",
              pid, label ) );
            label . to_string () }, }}, }
  } ) . into_owned () }

fn org_link_for_home (
  home           : &Home,
  current_target : &str,
  label          : &str,
) -> String {
  let same : bool = home . target == current_target;
  match &home . kind {
    HomeKind::Root { title } =>
      if same { format! ("[[*{}][{}]]", title, label) }
      else    { format! ("[[{}][{}]]",
                         relpath (current_target, &home . target),
                         label) },
    HomeKind::Content { anchor } =>
      if same { format! ("[[*{}][{}]]", anchor, label) }
      else    { format! ("[[{}::*{}][{}]]",
                         relpath (current_target, &home . target),
                         anchor, label) }, }}

/// Relative path from the file at `from_target` to the file at
/// `to_target` (both extension-less, '/'-separated, under the same
/// export base). Result carries the `.org` extension and a leading
/// `./` or `../`, matching the repo's link style.
fn relpath (
  from_target : &str,
  to_target   : &str,
) -> String {
  let from_dirs : Vec<&str> = {
    let mut v : Vec<&str> = from_target . split ('/') . collect ();
    v . pop (); // drop the filename component
    v };
  let to_comps : Vec<&str> = to_target . split ('/') . collect ();
  let to_dirs : &[&str] = &to_comps [.. to_comps . len () - 1];
  let common : usize = from_dirs . iter () . zip (to_dirs . iter ())
    . take_while ( |(a, b)| a == b )
    . count ();
  let ups : usize = from_dirs . len () - common;
  let mut parts : Vec<String> = Vec::new ();
  for _ in 0 .. ups { parts . push ( ".." . to_string () ); }
  for c in &to_comps [common ..] { parts . push ( c . to_string () ); }
  // Append .org to the final (filename) component.
  if let Some (last) = parts . last_mut () {
    last . push_str (".org"); }
  let joined : String = parts . join ("/");
  if ups == 0 { format! ("./{}", joined) } else { joined } }

fn warn_body_structure (
  pid      : &ID,
  body     : &str,
  warnings : &mut Vec<String>,
) {
  for line in body . lines () {
    let is_headline : bool =
      line . starts_with ('*')
      && line . trim_start_matches ('*') . starts_with (' ');
    if is_headline || line . starts_with ("#+") {
      warnings . push ( format! (
        "node {}: body line {:?} begins like org structure and may \
         render as a spurious headline/keyword",
        pid, line ) );
      return; }}} // one warning per node is enough

//
// Small shared helpers
//

fn resolve_pid (
  id           : &ID,
  alias_to_pid : &HashMap<ID, ID>,
) -> ID {
  alias_to_pid . get (id) . cloned () . unwrap_or_else (|| id . clone ()) }

fn node_active (
  node   : &NodeComplete,
  active : &ActiveSourceSet,
) -> bool {
  active . is_all () || active . contains_source (&node . source) }

fn anchor_text (
  node : &NodeComplete,
) -> String {
  replace_each_link_with_its_label (&node . title)
    . trim () . to_string () }

#[cfg(test)]
#[path = "../tests/unit/export_org.rs"]
mod tests;
