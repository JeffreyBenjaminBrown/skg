// Regenerate all existing call trees:
//   cargo run -p call-graph --bin call-graph-regen
//
// Regenerates all .org files in tools/introspect/call-graph/output/
// by deriving function names from filenames and re-running call-graph on each.

use std::fs;
use std::path::Path;
use std::process::Command;

fn main() {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let project_root = Path::new(manifest_dir).parent().unwrap().parent().unwrap().parent().unwrap();
    let output_dir = Path::new(manifest_dir).join("output");

    if !output_dir.exists() {
        println!("No output directory found at {}", output_dir.display());
        return;
    }

    let entries: Vec<_> = fs::read_dir(&output_dir)
        .expect("Failed to read output directory")
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().map(|ext| ext == "org").unwrap_or(false))
        .collect();

    if entries.is_empty() {
        println!("No .org files found in {}", output_dir.display());
        return;
    }

    println!("Regenerating {} file(s)...\n", entries.len());

    for entry in entries {
        let filename = entry.file_name();
        let filename_str = filename.to_string_lossy();

        // Strip .org extension to get function name
        let func_name = filename_str.trim_end_matches(".org");

        // Run call-graph with this function name
        let output = Command::new("cargo")
            .args(["run", "-p", "call-graph", "--bin", "call-graph", "-q", "--", func_name])
            .current_dir(project_root)
            .output()
            .expect("Failed to run call-graph");

        if output.status.success() {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Extract the "Wrote ... .org" line (not the .csv lines)
            if let Some(line) = stdout.lines().find(|l| l.contains(".org")) {
                println!("OK:   {}", line.trim());
            } else {
                println!("OK:   {}", filename_str);
            }
        } else {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // Check if it's a "function not found" error
            if stderr.contains("No function named") {
                println!("\x1b[31mSKIP:\x1b[0m {} (function '{}' not found)", filename_str, func_name);
            } else if stderr.contains("Found") && stderr.contains("functions named") {
                println!("\x1b[31mSKIP:\x1b[0m {} (multiple functions named '{}')", filename_str, func_name);
            } else {
                println!("\x1b[31mFAIL:\x1b[0m {} -> {}", filename_str, stderr.lines().next().unwrap_or("unknown error"));
            }
        }
    }

    println!("\nDone.");
}
