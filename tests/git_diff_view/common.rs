/// Shared utilities for all git diff view tests.

pub use git2::Repository;
pub use std::collections::HashMap;
pub use std::error::Error;
pub use std::fs;
pub use std::net::TcpStream;
pub use std::path::{Path, PathBuf};
pub use tempfile::TempDir;

pub use futures::executor::block_on;

pub use skg::dbs::init::create_empty_tantivy_index;
pub use skg::to_org::render::content_view::multi_root_view;
pub use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
pub use skg::test_utils::graph_handle_from_config;
pub use skg::types::misc::{ID, SkgConfig, SkgRepo, TantivyIndex, SkgRepoName};
pub use skg::dbs::in_rust_graph::InRustGraphHandle;
pub use skg::types::nodes::fs::GraphnodeOnDisk;
pub use skg::types::nodes::complete::Graphnode;
pub use skg::serve::ViewsState;
pub use skg::types::views_state::OpenViews;

//
// Git helpers
//

pub fn copy_dir_all ( src: &Path,
                      dst: &Path
                    ) -> Result<(), Box<dyn Error>> {
  if !dst . exists() { fs::create_dir_all (dst)?; }
  for entry in fs::read_dir (src)? {
    let entry = entry?;
    let src_path = entry . path();
    let dst_path = dst . join(entry . file_name());
    if entry . file_type()?. is_dir() {
      copy_dir_all(&src_path, &dst_path)?;
    } else {
      fs::copy(&src_path, &dst_path)?; }}
  Ok (( )) }

pub fn configure_git_user(gitrepo: &Repository) {
  let mut config = gitrepo . config() . unwrap();
  config . set_str("user.email", "test@test.com") . unwrap();
  config . set_str("user.name", "Test") . unwrap();
}

pub fn commit_all(gitrepo: &Repository, message: &str) {
  let mut index = gitrepo . index() . unwrap();
  index . add_all(["*.skg"], git2::IndexAddOption::DEFAULT, None) . unwrap();
  index . write() . unwrap();
  let tree_id = index . write_tree() . unwrap();
  let tree = gitrepo . find_tree (tree_id) . unwrap();
  let sig = gitrepo . signature() . unwrap();

  match gitrepo . head() {
    Ok (head) => {
      let parent = head . peel_to_commit() . unwrap();
      gitrepo . commit(Some ("HEAD"), &sig, &sig, message, &tree, &[&parent]) . unwrap();
    },
    Err (_) => {
      gitrepo . commit(Some ("HEAD"), &sig, &sig, message, &tree, &[]) . unwrap();
    }
  }
}

//
// Database helpers
//

pub async fn setup_test_stores(
  _test_name: &str,
  skgrepo_path: &str,
  tantivy_folder: &str,
) -> Result<(SkgConfig, TantivyIndex), Box<dyn Error>> {
  let mut skgrepos : HashMap<SkgRepoName, SkgRepo> = HashMap::new();
  skgrepos . insert (SkgRepoName::from ("main"), SkgRepo {
    name: SkgRepoName::from ("main"),
    abbreviation: None,
    path: PathBuf::from (skgrepo_path),
    owned: true, });
  let config = SkgConfig::fromSkgReposAndTantivyFolder (
    skgrepos, tantivy_folder );
  let tantivy_index =
    create_empty_tantivy_index (&config . tantivy_folder) ?;
  Ok ((config, tantivy_index)) }

pub async fn cleanup_test_stores(
  _test_name: &str,

  tantivy_folder: Option<&Path>,
) -> Result<(), Box<dyn Error>> {
  if let Some (path) = tantivy_folder {
    // A save's Tantivy update commits on a background worker
    // (coding-advice/common-gotchas.md); removing the folder while a
    // commit is in flight flakes with DirectoryNotEmpty.
    skg::dbs::tantivy::background_writer::wait_for_tantivy_writes_idle ();
    if path . exists() { fs::remove_dir_all (path)?; }
  }
  Ok(())
}

//
// Disk verification helpers
//

pub fn read_graphnode(gitrepo_path: &Path, skgid: &str) -> Result<Graphnode, Box<dyn Error>> {
  // Read YAML as GraphnodeOnDisk, then attach skgrepo.
  // Tests in this module use skgrepo "main".
  let path = gitrepo_path . join(format!("{}.skg", skgid));
  let content = fs::read_to_string (&path)?;
  let node_fs: GraphnodeOnDisk = serde_yaml::from_str (&content)?;
  Ok ( node_fs . into_complete_as_single_section ( SkgRepoName::from ("main") ))
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
  let actual_lines: Vec<&str> = actual . lines() . collect();

  for expected_line in expected . lines() {
    if expected_line . trim() . is_empty() { continue; }

    let found = actual_lines . iter() . any(|actual_line| {
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
  let actual_level = count_stars (actual);
  let expected_level = count_stars (expected);
  if actual_level != expected_level { return false; }

  let actual_title = extract_title (actual);
  let expected_title = extract_title (expected);
  if actual_title != expected_title { return false; }

  for fragment in extract_metadata_fragments (expected) {
    if !actual . contains (&fragment) { return false; }
  }

  true
}

fn count_stars(line: &str) -> usize {
  line . trim_start() . chars() . take_while(|c| *c == '*') . count()
}

fn extract_title(line: &str) -> &str {
  if let Some (pos) = line . rfind (')') {
    line[pos + 1..] . trim()
  } else {
    line . trim_start() . trim_start_matches ('*') . trim()
  }
}

/// Extract leaf metadata fragments like "(id foo)", "(diff new)", etc.
fn extract_metadata_fragments(line: &str) -> Vec<String> {
  let mut fragments = Vec::new();
  let mut i = 0;
  let chars: Vec<char> = line . chars() . collect();

  while i < chars . len() {
    if chars[i] == '(' {
      let start = i;
      let mut depth = 1;
      i += 1;
      let mut has_nested = false;

      while i < chars . len() && depth > 0 {
        if chars[i] == '(' {
          depth += 1;
          has_nested = true;
        } else if chars[i] == ')' {
          depth -= 1;
        }
        i += 1;
      }

      if !has_nested {
        let fragment: String = chars[start..i] . iter() . collect();
        fragments . push (fragment);
      } else {
        let inner: String = chars[start..i] . iter() . collect();
        fragments . extend(extract_metadata_fragments(&inner[1..inner . len()-1]));
      }
    } else {
      i += 1;
    }
  }

  fragments
}

//
// Buffer editing helpers
//

/// Remove lines containing the given substring.
pub fn without_lines_containing(buffer: &str, substring: &str) -> String {
  buffer . lines()
    . filter(|line| !line . contains (substring))
    . collect::<Vec<_>>()
    . join ("\n") + "\n"
}

/// Create a connected TCP stream pair for testing.
/// Returns (write_end, read_end). The write_end is passed
/// to 'update_from_and_rerender_buffer'; the read_end can
/// be used to read streamed collateral-view messages.
pub fn mk_test_tcp_stream_pair ()
  -> (TcpStream, TcpStream)
{ let listener : std::net::TcpListener =
    std::net::TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let addr : std::net::SocketAddr =
    listener . local_addr () . unwrap ();
  let write_end : TcpStream =
    TcpStream::connect (addr) . unwrap ();
  let (read_end, _) =
    listener . accept () . unwrap ();
  (write_end, read_end) }

/// Insert a line after the line containing the given substring.
pub fn insert_after(buffer: &str, after_substring: &str, new_line: &str) -> String {
  let mut result = Vec::new();
  for line in buffer . lines() {
    result . push(line . to_string());
    if line . contains (after_substring) {
      result . push(new_line . to_string());
    }
  }
  result . join ("\n") + "\n"
}

//
// Test setup helper
//

/// Create a gitrepo with head->worktree transition from fixture directories.
/// The worktree changes land unstaged (index == HEAD).
pub fn setup_gitrepo_with_fixtures(
  gitrepo_path: &Path,
  head_fixtures: &str,
  worktree_fixtures: &str,
) -> Result<Repository, Box<dyn Error>> {
  copy_dir_all(Path::new (head_fixtures), gitrepo_path)?;
  let gitrepo = Repository::init (gitrepo_path)?;
  configure_git_user (&gitrepo);
  commit_all(&gitrepo, "Initial commit");

  for entry in fs::read_dir (gitrepo_path)? {
    let path = entry?. path();
    if path . extension() . map_or(false, |ext| ext == "skg") {
      fs::remove_file (&path)?;
    }
  }
  copy_dir_all(Path::new (worktree_fixtures), gitrepo_path)?;

  Ok (gitrepo)
}

/// Like 'setup_gitrepo_with_fixtures' but then 'git add .' after
/// switching the worktree, so the transition lands staged (index == worktree,
/// both differ from HEAD) rather than unstaged.
pub fn setup_gitrepo_with_fixtures_staged(
  gitrepo_path: &Path,
  head_fixtures: &str,
  worktree_fixtures: &str,
) -> Result<Repository, Box<dyn Error>> {
  let gitrepo = setup_gitrepo_with_fixtures(
    gitrepo_path, head_fixtures, worktree_fixtures )?;
  stage_everything(&gitrepo)?;
  Ok (gitrepo)
}

/// Stage every .skg change currently in the worktree (including
/// deletions) so that the full diff lives on the staged side.
pub fn stage_everything(
  gitrepo: &Repository,
) -> Result<(), Box<dyn Error>> {
  let mut index = gitrepo . index ()?;
  index . add_all(
    ["*.skg"],
    git2::IndexAddOption::DEFAULT,
    None )?;
  // add_all doesn't register deletions for files removed from the worktree.
  index . update_all(["*.skg"], None)?;
  index . write ()?;
  Ok (( ))
}
