#!/usr/bin/env rust-script
//! Detect `let` declarations without type signatures in Rust code.
//!
//! Usage: rust-script find_untyped_lets.rs [path]

use std::env;
use std::fs;
use std::path::PathBuf;

fn main() {
  let args: Vec<String> = env::args().collect();
  let path = if args.len() > 1 {
    PathBuf::from(&args[1])
  } else {
    PathBuf::from(".")
  };

  if path.is_file() {
    check_file(&path);
  } else {
    walk_directory(&path);
  }
}

fn walk_directory(dir: &PathBuf) {
  if let Ok(entries) = fs::read_dir(dir) {
    for entry in entries.flatten() {
      let path = entry.path();
      if path.is_dir() {
        let name = path.file_name().unwrap().to_string_lossy();
        // Skip target, hidden dirs, etc.
        if !name.starts_with('.') && name != "target" {
          walk_directory(&path);
        }
      } else if path.extension().map(|e| e == "rs").unwrap_or(false) {
        check_file(&path);
      }
    }
  }
}

fn check_file(path: &PathBuf) {
  let content = match fs::read_to_string(path) {
    Ok(c) => c,
    Err(_) => return,
  };

  let lines: Vec<&str> = content.lines().collect();

  for (line_idx, line) in lines.iter().enumerate() {
    // Look for 'let' keyword (not if/while/for let)
    if !line.contains("let") {
      continue;
    }

    // Quick rejection of if let, while let, etc.
    if line.contains("if let")
      || line.contains("while let")
      || line.contains("for let") {
      continue;
    }

    // Find all 'let' positions
    let mut search_start = 0;
    while let Some(let_pos) = line[search_start..].find("let") {
      let absolute_pos = search_start + let_pos;

      // Check if it's actually the keyword 'let' (not part of another word)
      let is_keyword = {
        let before_ok = absolute_pos == 0
          || !line.chars().nth(absolute_pos - 1).unwrap().is_alphanumeric();
        let after_ok = absolute_pos + 3 >= line.len()
          || !line.chars().nth(absolute_pos + 3).unwrap().is_alphanumeric();
        before_ok && after_ok
      };

      if is_keyword {
        // Collect the declaration across multiple lines
        let decl = collect_declaration(&lines, line_idx, absolute_pos);

        // Check if it has a type annotation
        if !has_type_annotation(&decl) && !is_excluded_pattern(&decl) {
          println!("{}:{}: Missing type annotation: {}",
            path.display(),
            line_idx + 1,
            decl.trim().chars().take(80).collect::<String>());
        }
      }

      search_start = absolute_pos + 3;
    }
  }
}

/// Collect a let declaration that might span multiple lines.
/// Returns the declaration from 'let' until '=' or ';'
fn collect_declaration(lines: &[&str], start_line: usize, let_pos: usize) -> String {
  let mut result = String::new();

  // Start from the 'let' position
  result.push_str(&lines[start_line][let_pos..]);

  // Check if we already have '=' on the same line
  if result.contains('=') {
    // Extract up to and including '='
    if let Some(eq_pos) = result.find('=') {
      result.truncate(eq_pos + 1);
    }
    return result;
  }

  // Continue to next lines if needed
  for line_offset in 1..10 {  // Limit search to avoid runaway
    if start_line + line_offset >= lines.len() {
      break;
    }

    let next_line = lines[start_line + line_offset];
    result.push(' ');
    result.push_str(next_line);

    if next_line.contains('=') {
      // Extract up to and including '='
      if let Some(eq_pos) = result.rfind('=') {
        result.truncate(eq_pos + 1);
      }
      break;
    }

    if next_line.contains(';') {
      // Declaration without '=' (unlikely but handle it)
      if let Some(semi_pos) = result.rfind(';') {
        result.truncate(semi_pos + 1);
      }
      break;
    }
  }

  result
}

/// Check if a declaration has a type annotation.
/// Looks for ':' that is not part of '::'
fn has_type_annotation(decl: &str) -> bool {
  // Remove :: to avoid false positives
  let cleaned = decl.replace("::", "  ");

  // Look for standalone ':'
  // It should appear before '='
  if let Some(eq_pos) = cleaned.find('=') {
    let before_eq = &cleaned[..eq_pos];
    before_eq.contains(':')
  } else {
    // No '=' found, check the whole thing
    cleaned.contains(':')
  }
}

/// Check if this is a pattern we should exclude.
fn is_excluded_pattern(decl: &str) -> bool {
  // Exclude 'let _ = ...' (wildcard pattern)
  if decl.contains("let _") && !decl.contains("let __") {
    return true;
  }

  // Note: 'let else' patterns CAN have type annotations, so we don't exclude them

  false
}
