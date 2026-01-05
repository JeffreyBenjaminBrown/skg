// cargo run --bin skg_filenames_match_pids [optional_directory]

use std::path::Path;
use std::fs;

/// Utility to check that all .skg filenames match their primary IDs
/// Usage: cargo run --bin check_skg_filenames [directory]
fn main() {
  let args: Vec<String> = std::env::args().collect();

  // Default directories to check if none specified
  let default_directories = vec![
    "tests",
    "data/skg",
  ];

  let directories = if args.len() > 1 {
    vec![args[1].as_str()]
  } else {
    default_directories
  };

  let mut mismatches = Vec::new();
  let mut total_files = 0;

  for dir in directories {
    if Path::new(dir).exists() {
      println!("Checking directory: {}", dir);

      // Get all actual .skg files in the directory (recursively)
      let actual_files = get_skg_files_in_dir(dir);
      println!("  Found {} .skg files", actual_files.len());

      // Process each .skg file individually
      for file_path in &actual_files {
        match extract_primary_id(file_path) {
          Ok(primary_id) => {
            total_files += 1;

            // Expected filename based on primary ID (in any subdirectory of dir)
            let expected_filename = format!("{}.skg", primary_id);

            // Check if the current file path ends with the expected filename
            let current_filename = Path::new(file_path)
              .file_name()
              .unwrap()
              .to_string_lossy();

            if current_filename == expected_filename {
              println!("  ✅ {}", expected_filename);
            } else {
              println!("  ❌ MISMATCH: Node with primary ID '{}' expected filename '{}' but found in '{}'",
                       primary_id, expected_filename, current_filename);
              mismatches.push((primary_id.clone(), expected_filename.clone()));
            }
          }
          Err(e) => {
            println!("  ❌ Error reading file '{}': {}", file_path, e);
          }
        }
      }
      println!();
    } else {
      println!("Directory '{}' does not exist, skipping\n", dir);
    }
  }

  println!("=== SUMMARY ===");
  println!("Total .skg files processed: {}", total_files);
  println!("Mismatched filename/ID pairs: {}", mismatches.len());

  if mismatches.is_empty() {
    println!("✅ All .skg filenames match their primary IDs!");
  } else {
    println!("❌ Found mismatches:");
    for (id, expected_filename) in mismatches {
      println!("  - Primary ID '{}' expected filename '{}'",
               id, expected_filename);
    }
  }
}

/// Recursively find all .skg files in a directory
fn get_skg_files_in_dir(dir: &str) -> Vec<String> {
  let mut skg_files = Vec::new();

  if let Ok(entries) = fs::read_dir(dir) {
    for entry in entries {
      if let Ok(entry) = entry {
        let path = entry.path();
        if path.is_file()
          && path.extension().map_or(false, |ext| ext == "skg")
        {
          skg_files.push(path.to_string_lossy().to_string());
        } else if path.is_dir() {
          let subdir_files = get_skg_files_in_dir(
            &path.to_string_lossy());
          skg_files.extend(subdir_files);
        }
      }
    }
  }

  skg_files
}

/// Extract the primary ID from a .skg file by parsing its YAML content.
/// Returns the first ID in the ids list.
fn extract_primary_id(skgfile_path: &str) -> Result<String, String> {
  let contents = fs::read_to_string(skgfile_path)
    .map_err(|e| e.to_string())?;
  let yaml: serde_yaml::Value = serde_yaml::from_str(&contents)
    .map_err(|e| e.to_string())?;
  let ids = yaml.get("ids")
    .ok_or_else(|| "No 'ids' field found".to_string())?;
  let ids_seq = ids.as_sequence()
    .ok_or_else(|| "'ids' is not a list".to_string())?;
  let first_id = ids_seq.first()
    .ok_or_else(|| "'ids' list is empty".to_string())?;
  let id_str = first_id.as_str()
    .ok_or_else(|| "First ID is not a string".to_string())?;
  Ok(id_str.to_string())
}
