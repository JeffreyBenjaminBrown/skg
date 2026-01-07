// Run from anywhere:
//   cargo run -p code-map --bin code-map-regen
//
// Regenerates all .org files in tools/introspect/code-map/output/
// by deriving paths from filenames and re-running code-map on each.

use std::fs;
use std::path::Path;
use std::process::Command;

fn main() {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let project_root = Path::new(manifest_dir).parent().unwrap().parent().unwrap().parent().unwrap();
    let output_dir = project_root.join("tools/introspect/code-map/output");

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

        // Strip .org extension
        let name = filename_str.trim_end_matches(".org");

        // Reverse the transformation: _ -> /, - -> _
        let path: String = name.chars()
            .map(|c| match c {
                '_' => '/',
                '-' => '_',
                c => c,
            })
            .collect();

        let target_path = project_root.join(&path);

        if !target_path.exists() {
            println!("\x1b[31mSKIP:\x1b[0m {} -> {} (path not found)", filename_str, path);
            continue;
        }

        // Run code-map with this path
        let output = Command::new("cargo")
            .args(["run", "-p", "code-map", "--bin", "code-map", "-q", "--", &path])
            .current_dir(project_root)
            .output()
            .expect("Failed to run code-map");

        if output.status.success() {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Extract line count from output like "Generated ... (486 lines)"
            if let Some(line) = stdout.lines().last() {
                println!("OK:   {} -> {}", filename_str, line.trim());
            } else {
                println!("OK:   {}", filename_str);
            }
        } else {
            let stderr = String::from_utf8_lossy(&output.stderr);
            println!("\x1b[31mFAIL:\x1b[0m {} -> {}", filename_str, stderr.trim());
        }
    }

    println!("\nDone.");
}
