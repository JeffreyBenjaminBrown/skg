// Git and stream helpers for the tests over git fixtures (the graph
// straddle and the diff view) and for other tests that need a gitrepo
// or a stream pair. Declared once per test target that uses it, in
// 'tests/grouped_repos.rs', 'tests/grouped_unit.rs',
// 'tests/grouped_views.rs' and 'tests/grouped_overrides.rs'.

use git2::Repository;
use std::error::Error;
use std::fs;
use std::net::TcpStream;
use std::path::Path;

use skg::test_utils::copy_dir_all;
use skg::types::misc::SkgrepoName;
use skg::types::nodes::complete::Graphnode;
use skg::types::nodes::fs::GraphnodeOnDisk;

//
// Git helpers
//

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
// Disk verification helpers
//

pub fn read_graphnode(gitrepo_path: &Path, skgid: &str) -> Result<Graphnode, Box<dyn Error>> {
  // Read YAML as GraphnodeOnDisk, then attach skgrepo.
  // Tests in this module use skgrepo "main".
  let path = gitrepo_path . join(format!("{}.skg", skgid));
  let content = fs::read_to_string (&path)?;
  let node_fs: GraphnodeOnDisk = yaml_serde::from_str (&content)?;
  Ok ( node_fs . into_complete_as_single_section ( SkgrepoName::from ("main") ))
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
