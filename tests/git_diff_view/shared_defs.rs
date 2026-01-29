/// Shared definitions for git diff view tests.

/// The expected git diff view output.
/// This is what multi_root_view should produce with diff_mode_enabled=true.
pub const GIT_DIFF_VIEW: &str = "\
* (skg (node (id 1) (source main))) 1
** (skg (node (id 11) (source main))) 11
*** (skg (node (id gets-removed) (source main) indefinitive (diff removed))) gets-removed
*** (skg (node (id moves) (diff new-here))) moves
** (skg (node (id 12) (source main))) 12
*** (skg (node (id moves) (source main) indefinitive (diff removed-here))) moves
* (skg (node (id new) (source main))) new
";

pub use git2::Repository;
pub use std::collections::HashMap;
pub use std::error::Error;
pub use std::fs;
pub use std::path::{Path, PathBuf};
pub use std::sync::Arc;
pub use tempfile::TempDir;

pub use futures::executor::block_on;
pub use typedb_driver::{TypeDBDriver, Credentials, DriverOptions, Database};

pub use skg::dbs::init::{overwrite_new_empty_db, define_schema, create_empty_tantivy_index};
pub use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
pub use skg::dbs::typedb::nodes::create_all_nodes;
pub use skg::dbs::typedb::relationships::create_all_relationships;
pub use skg::to_org::render::content_view::multi_root_view;
pub use skg::serve::handlers::save_buffer::update_from_and_rerender_buffer;
pub use skg::types::misc::{ID, SkgConfig, SkgfileSource, TantivyIndex};
pub use skg::types::skgnode::SkgNode;

//
// Git helpers
//

pub fn copy_dir_all(src: &Path, dst: &Path) -> Result<(), Box<dyn Error>> {
  if !dst.exists() { fs::create_dir_all(dst)?; }
  for entry in fs::read_dir(src)? {
    let entry = entry?;
    let src_path = entry.path();
    let dst_path = dst.join(entry.file_name());
    if entry.file_type()?.is_dir() {
      copy_dir_all(&src_path, &dst_path)?;
    } else {
      fs::copy(&src_path, &dst_path)?;
    }
  }
  Ok(())
}

pub fn configure_git_user(repo: &Repository) {
  let mut config = repo.config().unwrap();
  config.set_str("user.email", "test@test.com").unwrap();
  config.set_str("user.name", "Test").unwrap();
}

pub fn commit_all(repo: &Repository, message: &str) {
  let mut index = repo.index().unwrap();
  index.add_all(["*.skg"], git2::IndexAddOption::DEFAULT, None).unwrap();
  index.write().unwrap();
  let tree_id = index.write_tree().unwrap();
  let tree = repo.find_tree(tree_id).unwrap();
  let sig = repo.signature().unwrap();

  match repo.head() {
    Ok(head) => {
      let parent = head.peel_to_commit().unwrap();
      repo.commit(Some("HEAD"), &sig, &sig, message, &tree, &[&parent]).unwrap();
    },
    Err(_) => {
      repo.commit(Some("HEAD"), &sig, &sig, message, &tree, &[]).unwrap();
    }
  }
}

//
// Database helpers
//

pub async fn setup_test_dbs(
  db_name: &str,
  source_path: &str,
  tantivy_folder: &str,
) -> Result<(SkgConfig, TypeDBDriver, TantivyIndex), Box<dyn Error>> {
  let config = {
    let mut sources = HashMap::new();
    sources.insert("main".to_string(), SkgfileSource {
      nickname: "main".to_string(),
      path: PathBuf::from(source_path),
      user_owns_it: true,
    });
    SkgConfig::fromSourcesAndDbName(sources, db_name, tantivy_folder)
  };

  let driver = TypeDBDriver::new(
    "127.0.0.1:1729",
    Credentials::new("admin", "password"),
    DriverOptions::new(false, None)?
  ).await?;

  let nodes: Vec<SkgNode> = {
    let mut sources = HashMap::new();
    sources.insert("main".to_string(), SkgfileSource {
      nickname: "main".to_string(),
      path: PathBuf::from(source_path),
      user_owns_it: true,
    });
    read_all_skg_files_from_sources(&SkgConfig::dummyFromSources(sources))?
  };

  overwrite_new_empty_db(db_name, &driver).await?;
  define_schema(db_name, &driver).await?;
  create_all_nodes(db_name, &driver, &nodes).await?;
  create_all_relationships(db_name, &driver, &nodes).await?;

  let tantivy_index = create_empty_tantivy_index(&config.tantivy_folder)?;
  Ok((config, driver, tantivy_index))
}

pub async fn cleanup_test_dbs(
  db_name: &str,
  driver: &TypeDBDriver,
  tantivy_folder: Option<&Path>,
) -> Result<(), Box<dyn Error>> {
  let databases = driver.databases();
  if databases.contains(db_name).await? {
    let database: Arc<Database> = databases.get(db_name).await?;
    database.delete().await?;
  }
  if let Some(path) = tantivy_folder {
    if path.exists() { fs::remove_dir_all(path)?; }
  }
  Ok(())
}

//
// Disk verification helpers
//

pub fn read_skgnode(repo_path: &Path, id: &str) -> Result<SkgNode, Box<dyn Error>> {
  let path = repo_path.join(format!("{}.skg", id));
  let content = fs::read_to_string(&path)?;
  let node: SkgNode = serde_yaml::from_str(&content)?;
  Ok(node)
}

//
// Buffer comparison helpers
//

/// Assert that `actual` buffer contains all the structure from `expected`.
/// Each expected line must have a matching actual line with:
/// - Same level (number of *)
/// - Same title (end of line)
/// - All (skg ...) metadata fragments present
pub fn assert_buffer_contains(actual: &str, expected: &str) {
  let actual_lines: Vec<&str> = actual.lines().collect();

  for expected_line in expected.lines() {
    if expected_line.trim().is_empty() { continue; }

    let found = actual_lines.iter().any(|actual_line| {
      line_matches(actual_line, expected_line)
    });

    assert!(found,
      "Expected line not found in actual output.\n\
       Expected: {}\n\
       Actual output:\n{}",
      expected_line, actual);
  }
}

fn line_matches(actual: &str, expected: &str) -> bool {
  let actual_level = count_stars(actual);
  let expected_level = count_stars(expected);
  if actual_level != expected_level { return false; }

  let actual_title = extract_title(actual);
  let expected_title = extract_title(expected);
  if actual_title != expected_title { return false; }

  for fragment in extract_metadata_fragments(expected) {
    if !actual.contains(&fragment) { return false; }
  }

  true
}

fn count_stars(line: &str) -> usize {
  line.trim_start().chars().take_while(|c| *c == '*').count()
}

fn extract_title(line: &str) -> &str {
  if let Some(pos) = line.rfind(')') {
    line[pos + 1..].trim()
  } else {
    line.trim_start().trim_start_matches('*').trim()
  }
}

//
// Buffer editing helpers
//

/// Remove lines containing the given substring.
pub fn without_lines_containing(buffer: &str, substring: &str) -> String {
  buffer.lines()
    .filter(|line| !line.contains(substring))
    .collect::<Vec<_>>()
    .join("\n") + "\n"
}

/// Insert a line after the line containing the given substring.
pub fn insert_after(buffer: &str, after_substring: &str, new_line: &str) -> String {
  let mut result = Vec::new();
  for line in buffer.lines() {
    result.push(line.to_string());
    if line.contains(after_substring) {
      result.push(new_line.to_string());
    }
  }
  result.join("\n") + "\n"
}

/// Extract leaf metadata fragments like "(id foo)", "(diff new)", etc.
fn extract_metadata_fragments(line: &str) -> Vec<String> {
  let mut fragments = Vec::new();
  let mut i = 0;
  let chars: Vec<char> = line.chars().collect();

  while i < chars.len() {
    if chars[i] == '(' {
      let start = i;
      let mut depth = 1;
      i += 1;
      let mut has_nested = false;

      while i < chars.len() && depth > 0 {
        if chars[i] == '(' {
          depth += 1;
          has_nested = true;
        } else if chars[i] == ')' {
          depth -= 1;
        }
        i += 1;
      }

      if !has_nested {
        let fragment: String = chars[start..i].iter().collect();
        fragments.push(fragment);
      } else {
        let inner: String = chars[start..i].iter().collect();
        fragments.extend(extract_metadata_fragments(&inner[1..inner.len()-1]));
      }
    } else {
      i += 1;
    }
  }

  fragments
}

//
// Test setup: create git repo with head->worktree transition
//

pub fn setup_git_repo_with_fixtures(
  repo_path: &Path
) -> Result<Repository, Box<dyn Error>> {
  copy_dir_all(Path::new("tests/git_diff_view/fixtures/head"), repo_path)?;
  let repo = Repository::init(repo_path)?;
  configure_git_user(&repo);
  commit_all(&repo, "Initial commit");

  for entry in fs::read_dir(repo_path)? {
    let path = entry?.path();
    if path.extension().map_or(false, |ext| ext == "skg") {
      fs::remove_file(&path)?;
    }
  }
  copy_dir_all(Path::new("tests/git_diff_view/fixtures/worktree"), repo_path)?;

  Ok(repo)
}
