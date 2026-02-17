/// Generate a call tree for a function:
///   cargo run -p call-graph [-- function_name]
///
/// Regenerate all existing call trees:
///   cargo run -p call-graph --bin call-graph-regen
///
/// PURPOSE: Generate a 'call tree' document. That's an .org file, saved to
/// tools/introspect/call-graph/output/function-name.org, for some function name. Each headline
/// is a function name. The root headline is the same as the file's basename. Each
/// headline calls each of its child headlines. A function that appears in its own
/// ancestry is marked "REC" (recursive) and not expanded further, to avoid infinite
/// loops. Functions that appear multiple times in the tree (via different call paths)
/// are expanded each time.
/// Only functions that are part of the library are listed; functions from other
/// libraries ('std::collections', etc.) are not listed.
///
/// The program creates a simple .csv database, saved to tools/introspect/call-graph/db/,
/// with two files: 'functions.csv' and 'calls.csv'. 'functions' has one row per
/// function, containing the name of the function, the full filepath (minus project
/// root) it appears in, and how many LOC it has. The other has one row for each
/// function call, with four columns: caller-file, caller (function name), callee-file,
/// and callee (function name).
///
/// If a function F calls a function G more than once, only one row is recorded in
/// calls.csv to relate the pair (F,G). (But if G in turn calls F, a second row will
/// need to be included to encode the reverse relationship.)
///
/// For implementations, the name used is 'TypeName::method_name'. The line count
/// includes everything, even the header comment, except blank lines. Some especially
/// boring implementations are ignored -- namely "new", "default", "from", and "from_str".
///
/// The program only analyzes server/. It looks inside macro invocations
/// (like println! or format!) to find function calls within their arguments.
///
/// By default it starts from 'main'. But if the user supplies an alternative function
/// name, it looks for a function of that name, and if there's only one, starts from
/// there instead. If it finds more than one function definition with that name, it
/// just reports that fact, letting the user rename one of them.

use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use syn::spanned::Spanned;
use syn::visit::Visit;
use syn::{File, ImplItem, ItemFn, ItemImpl};
use walkdir::WalkDir;

//
// Data structures
//

#[derive(Clone, Debug)]
struct FunctionDef {
  name: String,
  file: String,
  loc: usize,
  start_line: usize,
  end_line: usize,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct CallRelation {
  caller_file: String,
  caller: String,
  callee_file: String,
  callee: String,
  call_order: usize,  // order in which callee is first called within caller
}

//
// Parsing visitors
//

struct FunctionCollector {
  file_path: String,
  source: String,
  functions: Vec<FunctionDef>,
  function_context: Vec<String>,  // Stack of qualified parent function names
}

impl FunctionCollector {
  fn new(file_path: String, source: String) -> Self {
    Self {
      file_path,
      source,
      functions: Vec::new(),
      function_context: Vec::new(),
    }
  }

  /// Build qualified name for a function, prefixing with parent context if any.
  /// Inner functions get names like "parent:child" or "Type::method:inner".
  fn qualified_name(&self, raw_name: &str) -> String {
    if let Some(parent) = self.function_context.last() {
      format!("{}:{}", parent, raw_name)
    } else {
      raw_name.to_string()
    }
  }

  fn count_non_blank_lines(&self, start: usize, end: usize) -> usize {
    self.source
      .lines()
      .skip(start.saturating_sub(1))
      .take(end.saturating_sub(start.saturating_sub(1)))
      .filter(|line| !line.trim().is_empty())
      .count()
  }

  /// Record a function definition from its span info.
  fn record_function(&mut self, qualified_name: String, start_line: usize, end_line: usize) {
    let loc = self.count_non_blank_lines(start_line, end_line);
    self.functions.push(FunctionDef {
      name: qualified_name.clone(),
      file: self.file_path.clone(),
      loc,
      start_line,
      end_line,
    });
    self.function_context.push(qualified_name);
  }

  fn get_type_name(ty: &syn::Type) -> Option<String> {
    match ty {
      syn::Type::Path(tp) => tp.path.segments.last().map(|s| s.ident.to_string()),
      syn::Type::Reference(r) => Self::get_type_name(&r.elem),
      _ => None,
    }
  }
}

impl<'ast> Visit<'ast> for FunctionCollector {
  fn visit_item_fn(&mut self, node: &'ast ItemFn) {
    let raw_name = node.sig.ident.to_string();
    let qualified_name = self.qualified_name(&raw_name);
    let start_line = node.span().start().line;
    let end_line = node.span().end().line;

    self.record_function(qualified_name, start_line, end_line);
    syn::visit::visit_item_fn(self, node);
    self.function_context.pop();
  }

  fn visit_item_impl(&mut self, node: &'ast ItemImpl) {
    let type_name = Self::get_type_name(&node.self_ty);

    for item in &node.items {
      if let ImplItem::Fn(method) = item {
        let method_name = method.sig.ident.to_string();
        let qualified_name = match &type_name {
          Some(ty) => format!("{}::{}", ty, method_name),
          None => method_name,
        };
        let start_line = method.span().start().line;
        let end_line = method.span().end().line;

        self.record_function(qualified_name, start_line, end_line);
        syn::visit::visit_impl_item_fn(self, method);
        self.function_context.pop();
      }
    }
    // Don't call default visitor - we handled items manually
  }
}

struct CallCollector {
  calls: Vec<(String, usize)>,  // (name, line_number)
}

impl CallCollector {
  fn new() -> Self {
    Self { calls: Vec::new() }
  }
}

impl<'ast> Visit<'ast> for CallCollector {
  fn visit_expr_call(&mut self, node: &'ast syn::ExprCall) {
    if let syn::Expr::Path(ep) = &*node.func {
      let segments: Vec<_> = ep.path.segments.iter().map(|s| s.ident.to_string()).collect();
      if let Some(last) = segments.last() {
        if !(is_std_call(last) ||
             is_boring_skg_call(last))
        {
          let line = node.span().start().line;
          self.calls.push((last.clone(), line));
        }
      }
    }
    syn::visit::visit_expr_call(self, node);
  }

  fn visit_expr_method_call(&mut self, node: &'ast syn::ExprMethodCall) {
    let method_name = node.method.to_string();
    if !is_std_method(&method_name) {
      let line = node.span().start().line;
      self.calls.push((method_name, line));
    }
    syn::visit::visit_expr_method_call(self, node);
  }

  /// Handle macro invocations like println!(...), format!(...), etc.
  /// by attempting to parse their token streams for function calls.
  fn visit_macro(&mut self, node: &'ast syn::Macro) {
    // Try to parse the macro's tokens as comma-separated expressions.
    // This handles cases like: println!("...", { some_fn_call() })
    use syn::punctuated::Punctuated;
    use syn::Token;
    if let Ok(args) = node.parse_body_with(
      Punctuated::<syn::Expr, Token![,]>::parse_terminated
    ) {
      for expr in args.iter() {
        self.visit_expr(expr);
      }
    }
    syn::visit::visit_macro(self, node);
  }
}

//
// Standard library filters
//

fn is_boring_skg_call(name: &str) -> bool {
  // An implementation defined in Skg, but too simple
  // to warrant inclusion in a call tree .org file.
  matches!(
    name,
    | "new" | "default" | "from" | "from_str"
  )
}


fn is_std_call(name: &str) -> bool {
  matches!(
    name,
    "println" | "print" | "eprintln" | "eprint" | "format" | "panic"
      | "vec" | "assert" | "assert_eq" | "assert_ne" | "debug_assert"
      | "Ok" | "Err" | "Some" | "None"
      | "Box" | "Arc" | "Rc" | "Vec" | "String" | "HashMap" | "HashSet"
      | "write" | "writeln"
  )
}

fn is_std_method(name: &str) -> bool {
  matches!(
    name,
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
      | "extend" | "append"
      | "default"
      | "deref" | "deref_mut"
      | "eq" | "ne" | "cmp" | "partial_cmp"
      | "fmt" | "display"
      | "new"
      | "with_capacity" | "capacity" | "reserve"
      | "entry" | "or_insert" | "or_insert_with"
      | "keys" | "values" | "items"
      | "as_str" | "as_bytes" | "as_slice"
      | "borrow" | "borrow_mut"
      | "replace"
      | "next" | "peek"
      | "ok_or" | "ok_or_else"
      | "and" | "or"
      | "max" | "min" | "max_by" | "min_by"
      | "position" | "rposition"
  )
}

//
// Parsing helpers
//

/// Deduplicate a vector, preserving order of first occurrences.
fn dedup_preserving_order<T: Clone + Eq + std::hash::Hash>(items: Vec<T>) -> Vec<T> {
  let mut seen = HashSet::new();
  items
    .into_iter()
    .filter(|item| seen.insert(item.clone()))
    .collect()
}

/// Returns calls in the order they appear in source (deduplicated by first occurrence).
fn extract_calls_from_fn(source: &str, start_line: usize, end_line: usize) -> Vec<String> {
  let lines: Vec<&str> = source.lines().collect();
  let fn_source: String = lines
    .get((start_line.saturating_sub(1))..end_line)
    .map(|slice| slice.join("\n"))
    .unwrap_or_default();

  let wrapped = format!("fn __dummy__() {{ {} }}", fn_source);
  let mut collector = CallCollector::new();

  if let Ok(file) = syn::parse_file(&wrapped) {
    collector.visit_file(&file);
  }

  // Sort by line number, then deduplicate preserving first occurrence
  let mut calls = collector.calls;
  calls.sort_by_key(|(_, line)| *line);

  dedup_preserving_order(calls.into_iter().map(|(name, _)| name).collect())
}

fn parse_file(path: &Path, base_path: &Path) -> Result<(Vec<FunctionDef>, String), String> {
  let source = fs::read_to_string(path)
    .map_err(|e| format!("Failed to read {}: {}", path.display(), e))?;

  let syntax: File = syn::parse_file(&source)
    .map_err(|e| format!("Failed to parse {}: {}", path.display(), e))?;

  let relative_path = path
    .strip_prefix(base_path)
    .unwrap_or(path)
    .to_string_lossy()
    .to_string();

  let mut collector = FunctionCollector::new(relative_path, source.clone());
  collector.visit_file(&syntax);

  Ok((collector.functions, source))
}

//
// CSV writing
//

fn write_csvs(
  functions: &[FunctionDef],
  calls: &HashSet<CallRelation>,
  output_dir: &Path,
) {
  // Write functions.csv
  let functions_csv = output_dir.join("functions.csv");
  let mut csv_content = String::from("name,file,loc\n");
  for func in functions {
    csv_content.push_str(&format!(
      "\"{}\",\"{}\",{}\n",
      func.name, func.file, func.loc
    ));
  }
  fs::write(&functions_csv, csv_content).expect("Failed to write functions.csv");
  println!("Wrote {}", functions_csv.display());

  // Write calls.csv (sorted for reproducibility)
  let calls_csv = output_dir.join("calls.csv");
  let mut csv_content = String::from("caller_file,caller,callee_file,callee,call_order\n");
  let mut sorted_calls: Vec<_> = calls.iter().collect();
  sorted_calls.sort_by(|a, b| {
    (&a.caller_file, &a.caller, a.call_order)
      .cmp(&(&b.caller_file, &b.caller, b.call_order))
  });
  for rel in sorted_calls {
    csv_content.push_str(&format!(
      "\"{}\",\"{}\",\"{}\",\"{}\",{}\n",
      rel.caller_file, rel.caller, rel.callee_file, rel.callee, rel.call_order
    ));
  }
  fs::write(&calls_csv, csv_content).expect("Failed to write calls.csv");
  println!("Wrote {}", calls_csv.display());
}

//
// CSV reading
//

fn read_functions_csv(path: &Path) -> Vec<(String, String, usize)> {
  let content = fs::read_to_string(path).expect("Failed to read functions.csv");
  let mut result = Vec::new();

  for line in content.lines().skip(1) {
    // Parse CSV: "name","file",loc
    let parts: Vec<&str> = line.split(',').collect();
    if parts.len() >= 3 {
      let name = parts[0].trim_matches('"').to_string();
      let file = parts[1].trim_matches('"').to_string();
      let loc: usize = parts[2].parse().unwrap_or(0);
      result.push((name, file, loc));
    }
  }

  result
}

fn read_calls_csv(path: &Path) -> Vec<(String, String, String, String, usize)> {
  let content = fs::read_to_string(path).expect("Failed to read calls.csv");
  let mut result = Vec::new();

  for line in content.lines().skip(1) {
    // Parse CSV: "caller_file","caller","callee_file","callee",call_order
    let parts: Vec<&str> = line.split(',').collect();
    if parts.len() >= 5 {
      let caller_file = parts[0].trim_matches('"').to_string();
      let caller = parts[1].trim_matches('"').to_string();
      let callee_file = parts[2].trim_matches('"').to_string();
      let callee = parts[3].trim_matches('"').to_string();
      let call_order: usize = parts[4].parse().unwrap_or(0);
      result.push((caller_file, caller, callee_file, callee, call_order));
    }
  }

  result
}

//
// Org tree generation
//

struct CallGraph {
  functions: HashMap<String, (String, usize)>,  // name -> (file, loc)
  calls: HashMap<String, Vec<String>>,          // caller -> callees
  callers: HashMap<String, Vec<String>>,        // callee -> callers
}

impl CallGraph {
  fn from_csvs(functions_path: &Path, calls_path: &Path) -> Self {
    let mut graph = Self {
      functions: HashMap::new(),
      calls: HashMap::new(),
      callers: HashMap::new(),
    };

    // Load functions
    for (name, file, loc) in read_functions_csv(functions_path) {
      graph.functions.insert(name, (file, loc));
    }

    // Load calls with order information
    let mut calls_with_order: HashMap<String, Vec<(String, usize)>> = HashMap::new();
    for (_, caller, _, callee, call_order) in read_calls_csv(calls_path) {
      calls_with_order
        .entry(caller.clone())
        .or_default()
        .push((callee.clone(), call_order));
      graph.callers
        .entry(callee)
        .or_default()
        .push(caller);
    }

    // Sort by call_order, then deduplicate preserving order
    for (caller, mut callees_with_order) in calls_with_order {
      callees_with_order.sort_by_key(|(_, order)| *order);
      let callees: Vec<String> = callees_with_order.into_iter().map(|(c, _)| c).collect();
      graph.calls.insert(caller, dedup_preserving_order(callees));
    }

    // Deduplicate callers (order doesn't matter for callers)
    for callers in graph.callers.values_mut() {
      *callers = dedup_preserving_order(std::mem::take(callers));
    }

    graph
  }

  fn generate_org_tree(&self, root: &str) -> String {
    let mut output = String::new();

    self.write_node(&mut output, root, 1, &mut vec![root.to_string()]);

    output
  }

  fn write_node(
    &self,
    output: &mut String,
    func_name: &str,
    depth: usize,
    ancestors: &mut Vec<String>,
  ) {
    let stars = "*".repeat(depth);

    // Check if this is a recursive call (the function is in its own ancestry)
    let is_recursive = ancestors[..ancestors.len().saturating_sub(1)]
      .contains(&func_name.to_string());

    if is_recursive {
      output.push_str(&format!("{} REC {}\n", stars, func_name));
      return;
    }

    // Check function exists
    if !self.functions.contains_key(func_name) {
      return;
    }

    // Write headline
    if depth == 1 {
      let callers_str = self.callers
        .get(func_name)
        .map(|c| c.join("|"))
        .unwrap_or_default();
      if callers_str.is_empty() {
        output.push_str(&format!("{} {}\n", stars, func_name));
      } else {
        output.push_str(&format!("{} {} <- {}\n", stars, func_name, callers_str));
      }
    } else {
      output.push_str(&format!("{} {}\n", stars, func_name));
    }

    // Get callees and recurse (recursive calls last)
    if let Some(callees) = self.calls.get(func_name) {
      // Partition: non-recursive first, recursive last
      let (recursive, non_recursive): (Vec<_>, Vec<_>) = callees
        .iter()
        .partition(|c| ancestors.contains(c));

      for callee in non_recursive.iter().chain(recursive.iter()) {
        ancestors.push((*callee).clone());
        self.write_node(output, callee, depth + 1, ancestors);
        ancestors.pop();
      }
    }
  }

  fn find_functions_by_name(&self, name: &str) -> Vec<(&String, &String)> {
    self.functions
      .iter()
      .filter(|(n, _)| *n == name)
      .map(|(n, (f, _))| (n, f))
      .collect()
  }
}

//
// Main
//

fn main() {
  let args: Vec<String> = env::args().collect();
  let target_function = args.get(1).map(|s| s.as_str()).unwrap_or("main");

  // Find project root via CARGO_MANIFEST_DIR
  let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  let project_root = manifest_dir.parent().unwrap().parent().unwrap().parent().unwrap();

  let server_dir = project_root.join("server");
  let db_dir = manifest_dir.join("db");
  let output_dir = manifest_dir.join("output");

  // Create output directories
  fs::create_dir_all(&db_dir).expect("Failed to create db/ directory");
  fs::create_dir_all(&output_dir).expect("Failed to create output/ directory");

  //
  // PHASE 1: Parse codebase and write CSVs
  //

  let mut all_functions: Vec<FunctionDef> = Vec::new();
  let mut file_sources: HashMap<String, String> = HashMap::new();

  println!("Scanning Rust files in server/ ...");

  for entry in WalkDir::new(&server_dir)
    .into_iter()
    .filter_map(|e| e.ok())
    .filter(|e| e.path().extension().is_some_and(|ext| ext == "rs"))
  {
    let path = entry.path();
    match parse_file(path, project_root) {
      Ok((functions, source)) => {
        let rel_path = path
          .strip_prefix(project_root)
          .unwrap_or(path)
          .to_string_lossy()
          .to_string();
        file_sources.insert(rel_path, source);
        all_functions.extend(functions);
      }
      Err(e) => {
        eprintln!("Warning: {}", e);
      }
    }
  }

  println!("Found {} functions/methods", all_functions.len());

  // Build name -> files mapping
  let mut name_to_files: HashMap<String, HashSet<String>> = HashMap::new();
  for func in &all_functions {
    name_to_files
      .entry(func.name.clone())
      .or_default()
      .insert(func.file.clone());
  }

  // Build short name -> full names mapping
  let mut short_to_full: HashMap<String, Vec<String>> = HashMap::new();
  for func in &all_functions {
    short_to_full
      .entry(func.name.clone())
      .or_default()
      .push(func.name.clone());

    if let Some(pos) = func.name.find("::") {
      let short = &func.name[pos + 2..];
      short_to_full
        .entry(short.to_string())
        .or_default()
        .push(func.name.clone());
    }
  }

  // Build set of all known function names for scoped lookup
  let all_function_names: HashSet<String> = all_functions
    .iter()
    .map(|f| f.name.clone())
    .collect();

  // Resolve a call with scope awareness: check caller's ancestry first.
  // For caller "A:B:C" calling "x", checks: A:B:C:x, A:B:x, A:x, then global.
  let resolve_call = |caller: &str, call_name: &str| -> Vec<String> {
    // Try scoped lookup: walk up caller's ancestry
    let mut prefix = caller.to_string();
    loop {
      let candidate = format!("{}:{}", prefix, call_name);
      if all_function_names.contains(&candidate) {
        return vec![candidate];
      }
      // Move up one level (strip after last ':')
      if let Some(pos) = prefix.rfind(':') {
        prefix = prefix[..pos].to_string();
      } else {
        break;
      }
    }
    // Fall back to global resolution
    short_to_full.get(call_name).cloned().unwrap_or_default()
  };

  // Extract call relationships
  let mut call_relations: HashSet<CallRelation> = HashSet::new();

  for func in &all_functions {
    if let Some(source) = file_sources.get(&func.file) {
      let calls = extract_calls_from_fn(source, func.start_line, func.end_line);

      for (call_order, call_name) in calls.into_iter().enumerate() {
        let resolved_names = resolve_call(&func.name, &call_name);
        for full_name in resolved_names {
          if let Some(callee_files) = name_to_files.get(&full_name) {
            for callee_file in callee_files {
              call_relations.insert(CallRelation {
                caller_file: func.file.clone(),
                caller: func.name.clone(),
                callee_file: callee_file.clone(),
                callee: full_name.clone(),
                call_order,
              });
            }
          }
        }
      }
    }
  }

  println!("Found {} call relationships", call_relations.len());

  // Write CSVs
  write_csvs(&all_functions, &call_relations, &db_dir);

  //
  // PHASE 2: Read CSVs and generate org tree
  //

  let graph = CallGraph::from_csvs(
    &db_dir.join("functions.csv"),
    &db_dir.join("calls.csv"),
  );

  // Check if target function exists
  let matches = graph.find_functions_by_name(target_function);

  if matches.is_empty() {
    eprintln!("Error: No function named '{}' found.", target_function);
    eprintln!("Sample available functions:");
    for (name, _) in graph.functions.iter().take(20) {
      eprintln!("  - {}", name);
    }
    std::process::exit(1);
  }

  if matches.len() > 1 {
    eprintln!(
      "Error: Found {} functions named '{}':",
      matches.len(),
      target_function
    );
    for (name, file) in &matches {
      eprintln!("  - {} (in {})", name, file);
    }
    eprintln!("Please rename one of them to disambiguate.");
    std::process::exit(1);
  }

  println!("Generating call tree from '{}'...", target_function);

  let org_content = graph.generate_org_tree(target_function);

  let output_file = output_dir.join(format!("{}.org", target_function));
  fs::write(&output_file, &org_content).expect("Failed to write org file");
  println!(
    "Wrote {} ({} lines)",
    output_file.display(),
    org_content.lines().count()
  );
}
