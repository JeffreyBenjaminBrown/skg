// PURPOSE: Automatically reduce visibility of Rust items where possible.
//
// STRATEGY: Instead of compiling after each change (slow), we:
//   1. Collect all `pub` and `pub(super)` items
//   2. Batch-reduce them all to private
//   3. Compile once and parse errors to see which items failed
//   4. Restore failures to pub(super) and try again
//   5. Restore remaining failures to pub
//
// This minimizes compile cycles from O(n) to O(1) in the best case.

use std::collections::HashMap;
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::Command;
use regex::Regex;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Item {
    file: PathBuf,
    line: usize,        // 1-indexed
    col: usize,         // 1-indexed, start of visibility keyword
    original_vis: String,  // "pub" or "pub(super)"
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Visibility {
    Private,
    PubSuper,
    Pub,
}

impl Visibility {
    fn as_str(&self) -> &'static str {
        match self {
            Visibility::Private => "",
            Visibility::PubSuper => "pub(super) ",
            Visibility::Pub => "pub ",
        }
    }
}

// Global to store target directory for cargo check
static mut TARGET_DIR: Option<String> = None;

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    let target_dir = if args.len() > 1 { args[1].clone() } else { ".".to_string() };

    // Store for later use by run_cargo_check
    unsafe { TARGET_DIR = Some(target_dir.clone()); }

    println!("Scope Reducer - Automatically reducing Rust visibility");
    println!("Target directory: {}", target_dir);
    println!();

    // Find all Rust files
    let rust_files = find_rust_files(Path::new(&target_dir))?;
    println!("Found {} Rust files", rust_files.len());

    // Collect all pub and pub(super) items
    let items = collect_items(&rust_files)?;
    println!("Found {} pub/pub(super) items", items.len());

    if items.is_empty() {
        println!("Nothing to reduce!");
        return Ok(());
    }

    // Separate by original visibility
    let pub_items: Vec<&Item> = items.iter()
        .filter(|i| i.original_vis == "pub")
        .collect();
    let pub_super_items: Vec<&Item> = items.iter()
        .filter(|i| i.original_vis == "pub(super)")
        .collect();

    println!("  - {} pub items", pub_items.len());
    println!("  - {} pub(super) items", pub_super_items.len());
    println!();

    let mut total_changes = 0;

    // Phase 1: Try making all pub items private
    if !pub_items.is_empty() {
        println!("=== Phase 1: Trying to make {} pub items private ===", pub_items.len());
        let pub_items_owned: Vec<Item> = pub_items.iter().map(|i| (*i).clone()).collect();
        let (succeeded, failed) = try_reduce_batch(&pub_items_owned, Visibility::Private)?;

        total_changes += succeeded.len();
        println!("  {} succeeded as private", succeeded.len());

        // Phase 2: Try making failures pub(super)
        if !failed.is_empty() {
            println!("=== Phase 2: Trying to make {} failed items pub(super) ===", failed.len());
            let (succeeded2, failed2) = try_reduce_batch(&failed, Visibility::PubSuper)?;

            total_changes += succeeded2.len();
            println!("  {} succeeded as pub(super)", succeeded2.len());

            // Restore remaining to pub
            if !failed2.is_empty() {
                println!("  {} must remain pub", failed2.len());
                restore_items(&failed2, Visibility::Pub)?;
            }
        }
    }

    // Phase 3: Try making pub(super) items private
    if !pub_super_items.is_empty() {
        println!("=== Phase 3: Trying to make {} pub(super) items private ===", pub_super_items.len());
        let pub_super_owned: Vec<Item> = pub_super_items.iter().map(|i| (*i).clone()).collect();
        let (succeeded, failed) = try_reduce_batch(&pub_super_owned, Visibility::Private)?;

        total_changes += succeeded.len();
        println!("  {} succeeded as private", succeeded.len());

        // Restore failures
        if !failed.is_empty() {
            println!("  {} must remain pub(super)", failed.len());
            restore_items(&failed, Visibility::PubSuper)?;
        }
    }

    println!();
    println!("=== Summary ===");
    println!("Total changes: {}", total_changes);

    Ok(())
}

fn find_rust_files(dir: &Path) -> io::Result<Vec<PathBuf>> {
    let mut files = Vec::new();
    find_rust_files_recursive(dir, &mut files)?;
    Ok(files)
}

fn find_rust_files_recursive(dir: &Path, files: &mut Vec<PathBuf>) -> io::Result<()> {
    if !dir.is_dir() {
        return Ok(());
    }

    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();

        if path.is_dir() {
            // Skip target directory and hidden directories
            let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
            if name != "target" && !name.starts_with('.') {
                find_rust_files_recursive(&path, files)?;
            }
        } else if path.extension().and_then(|e| e.to_str()) == Some("rs") {
            files.push(path);
        }
    }
    Ok(())
}

fn collect_items(files: &[PathBuf]) -> io::Result<Vec<Item>> {
    let mut items = Vec::new();

    // Pattern to match pub or pub(super) at start of item declarations
    // We look for: pub/pub(super) followed by fn/struct/enum/type/const/static/trait
    // NOTE: We SKIP `mod` and `use` - changing mod visibility causes cascade failures
    let pub_pattern = Regex::new(
        r"(?m)^(\s*)(pub(?:\(super\))?)\s+(fn|struct|enum|type|const|static|trait)\s+([a-zA-Z_][a-zA-Z0-9_]*)"
    ).unwrap();

    // Also match pub in impl blocks (methods)
    let impl_pub_pattern = Regex::new(
        r"(?m)^(\s+)(pub(?:\(super\))?)\s+(fn)\s+([a-zA-Z_][a-zA-Z0-9_]*)"
    ).unwrap();

    for file in files {
        let content = fs::read_to_string(file)?;

        for cap in pub_pattern.captures_iter(&content) {
            let full_match = cap.get(0).unwrap();
            let indent = cap.get(1).unwrap().as_str();
            let vis = cap.get(2).unwrap().as_str();

            // Calculate line number
            let line = content[..full_match.start()].matches('\n').count() + 1;
            let line_start = content[..full_match.start()].rfind('\n').map(|p| p + 1).unwrap_or(0);
            let col = full_match.start() - line_start + indent.len() + 1;

            items.push(Item {
                file: file.clone(),
                line,
                col,
                original_vis: vis.to_string(),
            });
        }

        // Collect impl block methods separately (they have more indentation)
        for cap in impl_pub_pattern.captures_iter(&content) {
            let full_match = cap.get(0).unwrap();
            let indent = cap.get(1).unwrap().as_str();
            let vis = cap.get(2).unwrap().as_str();

            // Skip if already captured by the first pattern
            let line = content[..full_match.start()].matches('\n').count() + 1;
            if items.iter().any(|i| i.file == *file && i.line == line) {
                continue;
            }

            let line_start = content[..full_match.start()].rfind('\n').map(|p| p + 1).unwrap_or(0);
            let col = full_match.start() - line_start + indent.len() + 1;

            items.push(Item {
                file: file.clone(),
                line,
                col,
                original_vis: vis.to_string(),
            });
        }
    }

    Ok(items)
}

fn try_reduce_batch(items: &[Item], target_vis: Visibility) -> io::Result<(Vec<Item>, Vec<Item>)> {
    if items.is_empty() {
        return Ok((Vec::new(), Vec::new()));
    }

    // Process file-by-file to reduce cascading failures
    let mut by_file: HashMap<PathBuf, Vec<&Item>> = HashMap::new();
    for item in items {
        by_file.entry(item.file.clone()).or_default().push(item);
    }

    let mut all_succeeded: Vec<Item> = Vec::new();
    let mut all_failed: Vec<Item> = Vec::new();

    let total_files = by_file.len();
    for (file_idx, (file, file_items)) in by_file.iter().enumerate() {
        print!("    File {}/{}: {} ({} items)... ",
               file_idx + 1, total_files,
               file.file_name().unwrap_or_default().to_string_lossy(),
               file_items.len());
        std::io::stdout().flush().ok();

        let original_content = fs::read_to_string(file)?;
        let new_content = apply_visibility_changes(&original_content, file_items, target_vis);
        fs::write(file, &new_content)?;

        match run_cargo_check() {
            Ok(()) => {
                // All items in this file succeeded
                println!("{} succeeded", file_items.len());
                for item in file_items {
                    all_succeeded.push((*item).clone());
                }
            }
            Err(_error_output) => {
                // Some or all failed - try individually
                fs::write(file, &original_content)?;
                let items_owned: Vec<Item> = file_items.iter().map(|i| (*i).clone()).collect();
                let (succeeded, failed) = try_reduce_individually_in_file(
                    file, &original_content, &items_owned, target_vis)?;
                println!("{} succeeded, {} failed", succeeded.len(), failed.len());
                all_succeeded.extend(succeeded);
                all_failed.extend(failed);
            }
        }
    }

    Ok((all_succeeded, all_failed))
}

fn try_reduce_individually_in_file(
    file: &Path,
    original_content: &str,
    items: &[Item],
    target_vis: Visibility
) -> io::Result<(Vec<Item>, Vec<Item>)> {
    let mut succeeded = Vec::new();
    let mut failed = Vec::new();
    let mut current_content = original_content.to_string();
    let mut current_succeeded: Vec<&Item> = Vec::new();

    for item in items {
        // Apply this item's change on top of previous successes
        let mut items_to_apply: Vec<&Item> = current_succeeded.clone();
        items_to_apply.push(item);
        let new_content = apply_visibility_changes(original_content, &items_to_apply, target_vis);
        fs::write(file, &new_content)?;

        if run_cargo_check().is_ok() {
            current_succeeded.push(item);
            current_content = new_content;
            succeeded.push(item.clone());
        } else {
            // Restore to just the previous successes
            if current_succeeded.is_empty() {
                fs::write(file, original_content)?;
            } else {
                fs::write(file, &current_content)?;
            }
            failed.push(item.clone());
        }
    }

    Ok((succeeded, failed))
}

fn restore_items(items: &[Item], vis: Visibility) -> io::Result<()> {
    let mut by_file: HashMap<PathBuf, Vec<&Item>> = HashMap::new();
    for item in items {
        by_file.entry(item.file.clone()).or_default().push(item);
    }

    for (file, file_items) in by_file {
        let content = fs::read_to_string(&file)?;
        let new_content = apply_visibility_changes(&content, &file_items, vis);
        fs::write(&file, &new_content)?;
    }

    Ok(())
}

fn apply_visibility_changes(content: &str, items: &[&Item], target_vis: Visibility) -> String {
    let mut lines: Vec<String> = content.lines().map(|l| l.to_string()).collect();

    // Sort items by line in reverse order so we can modify without shifting indices
    let mut sorted_items: Vec<&&Item> = items.iter().collect();
    sorted_items.sort_by(|a, b| b.line.cmp(&a.line));

    for item in sorted_items {
        let line_idx = item.line - 1;
        if line_idx >= lines.len() {
            continue;
        }

        let line = &lines[line_idx];

        // Replace the visibility keyword
        let new_line = if item.original_vis == "pub(super)" {
            line.replacen("pub(super) ", target_vis.as_str(), 1)
        } else {
            // Be careful to replace "pub " not "pub(" for pub(super) items
            replace_pub_keyword(line, target_vis.as_str())
        };

        lines[line_idx] = new_line;
    }

    lines.join("\n") + if content.ends_with('\n') { "\n" } else { "" }
}

fn replace_pub_keyword(line: &str, replacement: &str) -> String {
    // Replace "pub " but not "pub(" (which would be pub(crate) or pub(super))
    // Since regex crate doesn't support look-ahead, we do it manually
    if let Some(pos) = line.find("pub ") {
        // Check it's not "pub(" by looking at what follows
        let after_pub = &line[pos + 4..];
        if !after_pub.trim_start().starts_with('(') {
            let before = &line[..pos];
            let after = &line[pos + 4..];
            return format!("{}{}{}", before, replacement, after);
        }
    }
    // Also handle "pub\t" and "pub\n" cases
    if let Some(pos) = line.find("pub\t") {
        let before = &line[..pos];
        let after = &line[pos + 4..];
        return format!("{}{}{}", before, replacement, after);
    }
    line.to_string()
}

fn run_cargo_check() -> Result<(), String> {
    let target_dir = unsafe { TARGET_DIR.as_ref().unwrap() };

    // Use --all-targets to also compile tests in tests/ directory.
    // This ensures we don't reduce visibility of items needed by tests.
    let output = Command::new("cargo")
        .args(["check", "--all-targets", "--message-format=short"])
        .current_dir(target_dir)
        .output()
        .expect("Failed to run cargo check");

    if output.status.success() {
        Ok(())
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        Err(format!("{}\n{}", stderr, stdout))
    }
}

