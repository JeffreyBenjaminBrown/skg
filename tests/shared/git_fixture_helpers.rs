/// Shared utilities for tests over git fixtures: the graph straddle and the total diff view.

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
pub use skg::types::misc::{ID, SkgConfig, Skgrepo, TantivyIndex, SkgrepoName};
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
  let mut skgrepos : HashMap<SkgrepoName, Skgrepo> = HashMap::new();
  skgrepos . insert (SkgrepoName::from ("main"), Skgrepo {
    name: SkgrepoName::from ("main"),
    abbreviation: None,
    path: PathBuf::from (skgrepo_path),
    owned: true, });
  let config = SkgConfig::fromSkgreposAndTantivyFolder (
    skgrepos, tantivy_folder );
  let tantivy_index =
    create_empty_tantivy_index (&config . tantivy_folder) ?;
  Ok ((config, tantivy_index)) }

pub async fn cleanup_test_stores(
  _test_name: &str,

  tantivy_folder: Option<&Path>,
) -> Result<(), Box<dyn Error>> {
  if let Some (path) = tantivy_folder {
    // A save's Tantivy update is tantivy-committed on a background worker
    // (coding-advice/common-gotchas.md); removing the folder while a
    // tantivy-commit is in flight flakes with DirectoryNotEmpty.
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
