/// Minimal spanning forest of changed definitions.
///
/// Usage:
///   cargo run -p change-forest -- <base> [<origin>]
///
/// <base>   : branch name or commit hash to diff against.
/// <origin> : what to compare (defaults to the worktree).
///            Can be a branch name or commit hash.
///
/// The tool:
///   1. Runs `git diff <base> [<origin>]` to find changed lines.
///   2. Parses all Rust files in server/ to collect definitions and call edges.
///   3. Maps changed lines to definitions.
///   4. Computes the minimal spanning forest:
///      the smallest set of call-graph nodes and edges that
///      connects every changed definition to every other
///      changed definition it can reach.
///   5. Outputs an org-mode file (minimal-change-forest.org).
///
/// Each org headline is "file  definition_name".
/// Changed definitions are unmarked; intermediate (bridge) nodes
/// are tagged :bridge:.

use std::collections::{HashMap, HashSet, VecDeque};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use syn::spanned::Spanned;
use syn::visit::Visit;
use syn::{File as SynFile, ImplItem, ItemEnum, ItemFn, ItemImpl, ItemStruct, ItemTrait};
use walkdir::WalkDir;

//
// Data structures
//

#[derive(Clone, Debug)]
struct Definition {
  name       : String,
  file       : String,
  start_line : usize,
  end_line   : usize,
  kind       : DefKind,
}

#[derive(Clone, Debug, PartialEq)]
enum DefKind {
  Function,
  Type,
}

//
// Parsing: collect definitions and calls
//

struct DefinitionCollector {
  file_path : String,
  #[allow(dead_code)]
  source    : String,
  defs      : Vec<Definition>,
  context   : Vec<String>,
}

impl DefinitionCollector {
  fn new (
    file_path : String,
    source    : String,
  ) -> Self {
    Self { file_path, source, defs: Vec::new(), context: Vec::new() } }

  fn qualified_name (
    &self,
    raw_name : &str,
  ) -> String {
    if let Some (parent) = self . context . last () {
      format! ("{}:{}", parent, raw_name)
    } else {
      raw_name . to_string () } }

  fn record_fn (
    &mut self,
    qualified_name : String,
    start_line     : usize,
    end_line       : usize,
  ) {
    self . defs . push ( Definition {
      name       : qualified_name . clone (),
      file       : self . file_path . clone (),
      start_line,
      end_line,
      kind       : DefKind::Function, } );
    self . context . push (qualified_name); }

  fn get_type_name (
    ty : &syn::Type,
  ) -> Option<String> {
    match ty {
      syn::Type::Path (tp) =>
        tp . path . segments . last ()
        . map ( |s| s . ident . to_string () ),
      syn::Type::Reference (r) =>
        Self::get_type_name (&r . elem),
      _ => None, } }
}

impl<'ast> Visit<'ast> for DefinitionCollector {
  fn visit_item_fn (
    &mut self,
    node : &'ast ItemFn,
  ) {
    let raw : String = node . sig . ident . to_string ();
    let qname : String = self . qualified_name (&raw);
    let start : usize = node . span () . start () . line;
    let end   : usize = node . span () . end ()   . line;
    self . record_fn (qname, start, end);
    syn::visit::visit_item_fn (self, node);
    self . context . pop (); }

  fn visit_item_impl (
    &mut self,
    node : &'ast ItemImpl,
  ) {
    let type_name : Option<String> =
      Self::get_type_name (&node . self_ty);
    for item in &node . items {
      if let ImplItem::Fn (method) = item {
        let method_name : String =
          method . sig . ident . to_string ();
        let qname : String = match &type_name {
          Some (ty) => format! ("{}::{}", ty, method_name),
          None      => method_name, };
        let start : usize = method . span () . start () . line;
        let end   : usize = method . span () . end ()   . line;
        self . record_fn (qname, start, end);
        syn::visit::visit_impl_item_fn (self, method);
        self . context . pop (); } } }

  fn visit_item_struct (
    &mut self,
    node : &'ast ItemStruct,
  ) {
    let name  : String = node . ident . to_string ();
    let start : usize  = node . span () . start () . line;
    let end   : usize  = node . span () . end ()   . line;
    self . defs . push ( Definition {
      name : name . clone (),
      file : self . file_path . clone (),
      start_line : start,
      end_line   : end,
      kind       : DefKind::Type, } );
    syn::visit::visit_item_struct (self, node); }

  fn visit_item_enum (
    &mut self,
    node : &'ast ItemEnum,
  ) {
    let name  : String = node . ident . to_string ();
    let start : usize  = node . span () . start () . line;
    let end   : usize  = node . span () . end ()   . line;
    self . defs . push ( Definition {
      name : name . clone (),
      file : self . file_path . clone (),
      start_line : start,
      end_line   : end,
      kind       : DefKind::Type, } );
    syn::visit::visit_item_enum (self, node); }

  fn visit_item_trait (
    &mut self,
    node : &'ast ItemTrait,
  ) {
    let name  : String = node . ident . to_string ();
    let start : usize  = node . span () . start () . line;
    let end   : usize  = node . span () . end ()   . line;
    self . defs . push ( Definition {
      name : name . clone (),
      file : self . file_path . clone (),
      start_line : start,
      end_line   : end,
      kind       : DefKind::Type, } );
    syn::visit::visit_item_trait (self, node); }
}

struct CallCollector {
  calls : Vec<String>,
}

impl CallCollector {
  fn new () -> Self { Self { calls: Vec::new () } }
}

impl<'ast> Visit<'ast> for CallCollector {
  fn visit_expr_call (
    &mut self,
    node : &'ast syn::ExprCall,
  ) {
    if let syn::Expr::Path (ep) = &*node . func {
      if let Some (last) = ep . path . segments . last () {
        let name : String = last . ident . to_string ();
        if !is_std_call (&name) && !is_boring (&name) {
          self . calls . push (name); } } }
    syn::visit::visit_expr_call (self, node); }

  fn visit_expr_method_call (
    &mut self,
    node : &'ast syn::ExprMethodCall,
  ) {
    let name : String = node . method . to_string ();
    if !is_std_method (&name) {
      self . calls . push (name); }
    syn::visit::visit_expr_method_call (self, node); }

  fn visit_macro (
    &mut self,
    node : &'ast syn::Macro,
  ) {
    use syn::punctuated::Punctuated;
    use syn::Token;
    if let Ok (args) = node . parse_body_with (
      Punctuated::<syn::Expr, Token![,]>::parse_terminated )
    { for expr in args . iter () {
        self . visit_expr (expr); } }
    syn::visit::visit_macro (self, node); }
}

//
// Standard library filters (from call-graph)
//

fn is_boring (name : &str) -> bool {
  matches! ( name,
    "new" | "default" | "from" | "from_str" ) }

fn is_std_call (name : &str) -> bool {
  matches! ( name,
    "println" | "print" | "eprintln" | "eprint" | "format" | "panic"
    | "vec" | "assert" | "assert_eq" | "assert_ne" | "debug_assert"
    | "Ok" | "Err" | "Some" | "None"
    | "Box" | "Arc" | "Rc" | "Vec" | "String" | "HashMap" | "HashSet"
    | "write" | "writeln" ) }

fn is_std_method (name : &str) -> bool {
  matches! ( name,
    "clone" | "to_string" | "to_owned" | "into" | "from" | "as_ref" | "as_mut"
    | "unwrap" | "expect" | "unwrap_or" | "unwrap_or_else" | "unwrap_or_default"
    | "ok" | "err" | "is_ok" | "is_err" | "is_some" | "is_none"
    | "map" | "and_then" | "or_else" | "filter" | "filter_map" | "flat_map"
    | "collect" | "iter" | "into_iter" | "iter_mut"
    | "push" | "pop" | "insert" | "remove" | "get" | "get_mut"
    | "len" | "is_empty" | "clear" | "first" | "last" | "contains"
    | "join" | "split" | "trim" | "starts_with" | "ends_with"
    | "parse" | "to_lowercase" | "to_uppercase"
    | "read" | "write" | "flush"
    | "lock" | "await"
    | "fold" | "reduce" | "find" | "any" | "all" | "count"
    | "take" | "skip" | "enumerate" | "zip" | "chain"
    | "cloned" | "copied"
    | "sort" | "sort_by" | "sort_by_key" | "reverse"
    | "extend" | "append" | "default"
    | "deref" | "deref_mut"
    | "eq" | "ne" | "cmp" | "partial_cmp"
    | "fmt" | "display" | "new"
    | "with_capacity" | "capacity" | "reserve"
    | "entry" | "or_insert" | "or_insert_with"
    | "keys" | "values" | "items"
    | "as_str" | "as_bytes" | "as_slice"
    | "borrow" | "borrow_mut" | "replace"
    | "next" | "peek"
    | "ok_or" | "ok_or_else"
    | "and" | "or"
    | "max" | "min" | "max_by" | "min_by"
    | "position" | "rposition"
    | "store" | "load" ) }

//
// Parsing helpers
//

fn dedup<T: Clone + Eq + std::hash::Hash> (
  items : Vec<T>,
) -> Vec<T> {
  let mut seen : HashSet<T> = HashSet::new ();
  items . into_iter ()
    . filter ( |item| seen . insert (item . clone ()) )
    . collect () }

fn extract_calls_from_fn (
  source     : &str,
  start_line : usize,
  end_line   : usize,
) -> Vec<String> {
  let lines : Vec<&str> = source . lines () . collect ();
  let fn_source : String =
    lines . get ((start_line . saturating_sub (1)) .. end_line)
    . map ( |slice| slice . join ("\n") )
    . unwrap_or_default ();
  let wrapped : String =
    format! ("fn __dummy__() {{ {} }}", fn_source);
  let mut collector : CallCollector = CallCollector::new ();
  if let Ok (file) = syn::parse_file (&wrapped) {
    collector . visit_file (&file); }
  dedup (collector . calls) }

fn parse_rust_file (
  path      : &Path,
  base_path : &Path,
) -> Result < (Vec<Definition>, String), String > {
  let source : String =
    fs::read_to_string (path)
    . map_err ( |e| format! ("Failed to read {}: {}", path . display (), e) ) ?;
  let syntax : SynFile =
    syn::parse_file (&source)
    . map_err ( |e| format! ("Failed to parse {}: {}", path . display (), e) ) ?;
  let relative_path : String =
    path . strip_prefix (base_path)
    . unwrap_or (path)
    . to_string_lossy () . to_string ();
  let mut collector : DefinitionCollector =
    DefinitionCollector::new (relative_path, source . clone ());
  collector . visit_file (&syntax);
  Ok (( collector . defs, source )) }

//
// Git diff parsing
//

/// A range of lines that changed in a file (1-based, inclusive).
#[derive(Debug)]
struct ChangedRange {
  start : usize,
  count : usize,
}

/// Parse `git diff --unified=0` output to find changed line ranges per file.
/// Returns map from relative file path to list of changed ranges
/// (on the "new" / right-hand side of the diff).
fn parse_diff_for_changed_lines (
  diff_output : &str,
) -> HashMap < String, Vec<ChangedRange> > {
  let mut result : HashMap<String, Vec<ChangedRange>> = HashMap::new ();
  let mut current_file : Option<String> = None;
  for line in diff_output . lines () {
    if line . starts_with ("+++ b/") {
      current_file = Some ( line [6..] . to_string () );
    } else if line . starts_with ("@@ ") {
      // Parse @@ -old +new @@
      // The +new part is like +start,count or +start (count=1)
      if let Some (ref file) = current_file {
        if let Some (plus_part) = line . split_whitespace ()
          . find ( |s| s . starts_with ('+') )
        { let nums : &str = &plus_part [1..];
          let (start, count) : (usize, usize) =
            if let Some (comma) = nums . find (',') {
              let s : usize = nums [..comma] . parse () . unwrap_or (0);
              let c : usize = nums [comma+1..] . parse () . unwrap_or (0);
              (s, c)
            } else {
              let s : usize = nums . parse () . unwrap_or (0);
              (s, 1)
            };
          if count > 0 {
            result . entry (file . clone ())
              . or_default ()
              . push ( ChangedRange { start, count } ); } } } } }
  result }

//
// Map changed lines to definitions
//

fn definition_overlaps_changes (
  def     : &Definition,
  changes : &[ChangedRange],
) -> bool {
  for ch in changes {
    let ch_end : usize = ch . start + ch . count - 1;
    if def . start_line <= ch_end && ch . start <= def . end_line {
      return true; } }
  false }

fn find_changed_definitions (
  all_defs    : &[Definition],
  changed_map : &HashMap<String, Vec<ChangedRange>>,
) -> HashSet<String> {
  let mut changed : HashSet<String> = HashSet::new ();
  for def in all_defs {
    if let Some (ranges) = changed_map . get (&def . file) {
      if definition_overlaps_changes (def, ranges) {
        changed . insert (def . name . clone ()); } } }
  changed }

//
// Call graph
//

struct CallGraph {
  /// name → (file, DefKind)
  defs      : HashMap < String, (String, DefKind) >,
  /// caller → [callees] (deduplicated, in source order)
  callees   : HashMap < String, Vec<String> >,
}

impl CallGraph {
  fn build (
    all_defs    : &[Definition],
    file_sources : &HashMap<String, String>,
  ) -> Self {
    let mut defs : HashMap<String, (String, DefKind)> = HashMap::new ();
    let mut name_to_files : HashMap<String, HashSet<String>> = HashMap::new ();
    let mut short_to_full : HashMap<String, Vec<String>> = HashMap::new ();
    let all_names : HashSet<String> =
      all_defs . iter () . map ( |d| d . name . clone () ) . collect ();
    for def in all_defs {
      defs . insert (
        def . name . clone (),
        (def . file . clone (), def . kind . clone ()) );
      name_to_files
        . entry (def . name . clone ())
        . or_default ()
        . insert (def . file . clone ());
      short_to_full
        . entry (def . name . clone ())
        . or_default ()
        . push (def . name . clone ());
      if let Some (pos) = def . name . find ("::") {
        let short : &str = &def . name [pos + 2..];
        short_to_full
          . entry (short . to_string ())
          . or_default ()
          . push (def . name . clone ()); } }
    // Resolve calls.
    let resolve = |caller: &str, call_name: &str| -> Vec<String> {
      let mut prefix : String = caller . to_string ();
      loop {
        let candidate : String = format! ("{}:{}", prefix, call_name);
        if all_names . contains (&candidate) {
          return vec![candidate]; }
        if let Some (pos) = prefix . rfind (':') {
          prefix = prefix [..pos] . to_string ();
        } else { break; } }
      short_to_full . get (call_name) . cloned () . unwrap_or_default () };
    let mut callees_map : HashMap<String, Vec<String>> = HashMap::new ();
    for def in all_defs {
      if def . kind != DefKind::Function { continue; }
      if let Some (source) = file_sources . get (&def . file) {
        let calls : Vec<String> =
          extract_calls_from_fn (source, def . start_line, def . end_line);
        let mut resolved : Vec<String> = Vec::new ();
        for call_name in calls {
          for full_name in resolve (&def . name, &call_name) {
            if defs . contains_key (&full_name) {
              resolved . push (full_name); } } }
        if !resolved . is_empty () {
          callees_map . insert (
            def . name . clone (),
            dedup (resolved) ); } } }
    CallGraph { defs, callees: callees_map } }
}

//
// Minimal spanning forest
//

/// BFS forward from `start` through call edges.
/// Returns the predecessor map (node → predecessor on shortest path from start).
/// Stops expanding past other changed nodes (they are recorded but not expanded).
fn bfs_forward (
  start   : &str,
  graph   : &CallGraph,
  changed : &HashSet<String>,
) -> HashMap<String, String> {
  let mut pred : HashMap<String, String> = HashMap::new ();
  let mut queue : VecDeque<String> = VecDeque::new ();
  queue . push_back (start . to_string ());
  while let Some (current) = queue . pop_front () {
    // Don't expand past a different changed node (it's a destination).
    if current != start && changed . contains (&current) {
      continue; }
    if let Some (children) = graph . callees . get (&current) {
      for child in children {
        if !pred . contains_key (child) && child != start {
          pred . insert (child . clone (), current . clone ());
          queue . push_back (child . clone ()); } } } }
  pred }

/// Reconstruct the path from `start` to `end` using the predecessor map.
fn reconstruct_path (
  start : &str,
  end   : &str,
  pred  : &HashMap<String, String>,
) -> Option < Vec<String> > {
  if start == end { return Some (vec![start . to_string ()]); }
  let mut path : Vec<String> = vec![end . to_string ()];
  let mut cur : &str = end;
  while cur != start {
    match pred . get (cur) {
      Some (p) => {
        path . push (p . clone ());
        cur = p; },
      None => return None, } }
  path . reverse ();
  Some (path) }

/// Compute the minimal spanning forest.
///
/// For each changed definition, BFS forward to find paths to other changed defs.
/// Union all shortest paths. Also include standalone changed defs
/// (those not connected to any other changed def).
///
/// Returns: (set of nodes in forest, set of edges caller→callee in forest).
fn compute_forest (
  changed : &HashSet<String>,
  graph   : &CallGraph,
) -> (HashSet<String>, HashSet<(String, String)>) {
  let mut forest_nodes : HashSet<String> = HashSet::new ();
  let mut forest_edges : HashSet<(String, String)> = HashSet::new ();
  // Only BFS from function definitions (types have no call edges).
  let fn_changed : HashSet<&String> =
    changed . iter ()
    . filter ( |n| graph . defs . get (*n)
      . map ( |d| d . 1 == DefKind::Function ) . unwrap_or (false) )
    . collect ();
  for src in &fn_changed {
    let pred : HashMap<String, String> =
      bfs_forward (src, graph, changed);
    for dst in &fn_changed {
      if *src == *dst { continue; }
      if !pred . contains_key (*dst) { continue; }
      if let Some (path) = reconstruct_path (src, dst, &pred) {
        for node in &path {
          forest_nodes . insert (node . clone ()); }
        for pair in path . windows (2) {
          forest_edges . insert (
            (pair[0] . clone (), pair[1] . clone ()) ); } } } }
  // Add standalone changed defs (types, or functions not connected to others).
  for c in changed {
    if graph . defs . contains_key (c) {
      forest_nodes . insert (c . clone ()); } }
  (forest_nodes, forest_edges) }

//
// Org-mode output
//

/// Build a forest (set of trees) from the edges and render as org-mode.
fn render_forest_as_org (
  forest_nodes : &HashSet<String>,
  forest_edges : &HashSet<(String, String)>,
  changed      : &HashSet<String>,
  graph        : &CallGraph,
) -> String {
  // Build adjacency list for the forest subgraph.
  let mut children : HashMap<String, Vec<String>> = HashMap::new ();
  let mut has_parent : HashSet<String> = HashSet::new ();
  for (caller, callee) in forest_edges {
    children
      . entry (caller . clone ())
      . or_default ()
      . push (callee . clone ());
    has_parent . insert (callee . clone ()); }
  // Sort children for deterministic output.
  for v in children . values_mut () {
    v . sort (); }
  // Roots: forest nodes with no parent in the forest.
  let mut roots : Vec<String> =
    forest_nodes . iter ()
    . filter ( |n| !has_parent . contains (*n) )
    . cloned ()
    . collect ();
  roots . sort ();
  let mut out : String = String::new ();
  let mut visited : HashSet<String> = HashSet::new ();
  for root in &roots {
    write_org_node (
      &mut out, root, 1, &children, changed,
      graph, &mut visited); }
  out }

fn write_org_node (
  out      : &mut String,
  name     : &str,
  depth    : usize,
  children : &HashMap<String, Vec<String>>,
  changed  : &HashSet<String>,
  graph    : &CallGraph,
  visited  : &mut HashSet<String>,
) {
  if visited . contains (name) {
    // Avoid infinite loops from cycles in the forest edges.
    return; }
  visited . insert (name . to_string ());
  let stars : String = "*" . repeat (depth);
  let file : &str =
    graph . defs . get (name)
    . map ( |d| d . 0 . as_str () )
    . unwrap_or ("?");
  let prefix : &str =
    if changed . contains (name) { "CHANGED " }
    else { "" };
  let tag : &str =
    if changed . contains (name) { "" }
    else { "  :bridge:" };
  out . push_str (
    &format! ("{} {}{}  {}{}\n", stars, prefix, file, name, tag) );
  if let Some (kids) = children . get (name) {
    for kid in kids {
      write_org_node (
        out, kid, depth + 1, children, changed,
        graph, visited); } } }

//
// Main
//

fn main () {
  let args : Vec<String> = env::args () . collect ();
  if args . len () < 2 {
    eprintln! ("Usage: change-forest <base> [<origin>]");
    eprintln! ("");
    eprintln! ("  <base>   : branch or commit to diff against (e.g. 'main')");
    eprintln! ("  <origin> : what to compare (default: worktree)");
    std::process::exit (1); }
  let base : &str = &args [1];
  let origin : Option<&str> =
    args . get (2) . map ( |s| s . as_str () );

  let manifest_dir : PathBuf =
    PathBuf::from (env! ("CARGO_MANIFEST_DIR"));
  let project_root : &Path =
    manifest_dir . parent () . unwrap ()
    . parent () . unwrap ()
    . parent () . unwrap ();
  let server_dir : PathBuf = project_root . join ("server");

  // Phase 1: Git diff.
  let mut diff_cmd : Command = Command::new ("git");
  diff_cmd . arg ("diff")
    . arg ("--unified=0")
    . arg (base);
  if let Some (o) = origin {
    diff_cmd . arg (o); }
  diff_cmd . arg ("--") . arg ("server/");
  diff_cmd . current_dir (project_root);
  let diff_output_raw : std::process::Output =
    diff_cmd . output ()
    . expect ("Failed to run git diff");
  if !diff_output_raw . status . success () {
    let stderr : String =
      String::from_utf8_lossy (&diff_output_raw . stderr) . to_string ();
    eprintln! ("git diff failed: {}", stderr);
    std::process::exit (1); }
  let diff_text : String =
    String::from_utf8_lossy (&diff_output_raw . stdout) . to_string ();
  let changed_lines : HashMap<String, Vec<ChangedRange>> =
    parse_diff_for_changed_lines (&diff_text);
  eprintln! ("Changed files: {}", changed_lines . len ());

  // Phase 2: Parse all Rust files in server/.
  let mut all_defs : Vec<Definition> = Vec::new ();
  let mut file_sources : HashMap<String, String> = HashMap::new ();
  for entry in WalkDir::new (&server_dir)
    . into_iter ()
    . filter_map ( |e| e . ok () )
    . filter ( |e| e . path () . extension ()
      . is_some_and ( |ext| ext == "rs" ) )
  { let path : &Path = entry . path ();
    match parse_rust_file (path, project_root) {
      Ok ((defs, source)) => {
        let rel : String = path . strip_prefix (project_root)
          . unwrap_or (path) . to_string_lossy () . to_string ();
        file_sources . insert (rel, source);
        all_defs . extend (defs); }
      Err (e) => { eprintln! ("Warning: {}", e); } } }
  eprintln! ("Definitions found: {}", all_defs . len ());

  // Phase 3: Find changed definitions.
  let changed : HashSet<String> =
    find_changed_definitions (&all_defs, &changed_lines);
  eprintln! ("Changed definitions: {}", changed . len ());
  if changed . is_empty () {
    eprintln! ("No definitions changed in the diff.");
    std::process::exit (0); }
  let mut sorted_changed : Vec<&String> =
    changed . iter () . collect ();
  sorted_changed . sort ();
  for c in &sorted_changed {
    eprintln! ("  {}", c); }

  // Phase 4: Build call graph.
  let graph : CallGraph =
    CallGraph::build (&all_defs, &file_sources);
  eprintln! ("Call graph: {} defs, {} caller entries",
    graph . defs . len (),
    graph . callees . len ());

  // Phase 5: Compute minimal spanning forest.
  let (forest_nodes, forest_edges) : (HashSet<String>, HashSet<(String, String)>) =
    compute_forest (&changed, &graph);
  let n_bridge : usize =
    forest_nodes . iter ()
    . filter ( |n| !changed . contains (*n) )
    . count ();
  eprintln! ("Forest: {} nodes ({} changed, {} bridge), {} edges",
    forest_nodes . len (),
    forest_nodes . len () - n_bridge,
    n_bridge,
    forest_edges . len ());

  // Phase 6: Render and write.
  let org : String =
    render_forest_as_org (
      &forest_nodes, &forest_edges, &changed, &graph);
  let output_path : PathBuf =
    project_root . join ("minimal-change-forest.org");
  fs::write (&output_path, &org)
    . expect ("Failed to write minimal-change-forest.org");
  eprintln! ("Wrote {}", output_path . display ());
}
