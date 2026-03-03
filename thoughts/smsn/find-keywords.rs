// Claude, unedited by a human (except for this line).

use std::collections::HashSet;
use std::fs;
use std::io::{self, Write};
use regex::Regex;

fn main() -> io::Result<()> {
    let smsn_dir = "vcs.from-borg.2024-01-10.hp17/universal/";
    let output_file = "discoveries/smsn/keywords.txt";
    let test_mode = false;
    let test_sample_size = 100;

    println!("Scanning .smsn files in: {}", smsn_dir);
    let pattern = Regex::new(r"^@([a-zA-Z\-_]+) ").unwrap();
    let mut keywords: HashSet<String> = HashSet::new();
    let mut files_processed = 0;
    let entries = fs::read_dir(smsn_dir)?;

    for entry in entries {
        let entry = entry?;
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) != Some("smsn") {
            continue;
        }
        if test_mode && files_processed >= test_sample_size {
            break;
        }
        match fs::read_to_string(&path) {
            Ok(content) => {
                for line in content.lines() {
                    if let Some(captures) = pattern.captures(line) {
                        if let Some(keyword) = captures.get(1) {
                            keywords.insert(format!("@{}", keyword.as_str()));
                        }
                    }
                }
                files_processed += 1;
                if files_processed % 10 == 0 {
                    print!("\rProcessed {} files...", files_processed);
                    io::stdout().flush().unwrap();
                }
            }
            Err(e) => {
                eprintln!("\nError reading {:?}: {}", path, e);
            }
        }
    }

    println!("\n\nFound {} unique keywords", keywords.len());
    let mut keywords_vec: Vec<String> = keywords.into_iter().collect();
    keywords_vec.sort_by(|a, b| {
        let len_cmp = a.len().cmp(&b.len());
        if len_cmp == std::cmp::Ordering::Equal {
            a.cmp(b)
        } else {
            len_cmp
        }
    });

    let mut output = fs::File::create(output_file)?;
    for keyword in &keywords_vec {
        writeln!(output, "{}", keyword)?;
    }

    println!("Wrote {} keywords to {}", keywords_vec.len(), output_file);
    println!("\nFirst 10 keywords (shortest):");
    for keyword in keywords_vec.iter().take(10) {
        println!("  {}", keyword);
    }
    println!("\nLast 10 keywords (longest):");
    for keyword in keywords_vec.iter().rev().take(10).rev() {
        println!("  {}", keyword);
    }

    Ok(())
}
