// cargo nextest run --test grouped_unit -E 'test(multi_repo_loading::)'

use std::collections::HashMap;
use std::fs;
use std::io::{Result as IoResult, Error as IoError, ErrorKind as IoErrorKind};
use std::path::PathBuf;
use tempfile::{tempdir, TempDir};

use skg::dbs::filesystem::multiple_nodes::error_unless_each_id_names_one_node;
use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_repos;
use skg::dbs::filesystem::one_node::write_graphnode_to_repo;
use skg::test_utils::set_repo_retagging_relRepos;
use skg::types::misc::{SkgfileRepo, SkgConfig, ID, RepoName};
use skg::types::nodes::complete::{Graphnode, empty_node_complete};

/// Helper to create a minimal SkgConfig for tests.
/// `data_root` should be each test's tempdir so that any
/// `initialization-error_*.org` reports land inside the tempdir
/// rather than polluting the project root.
fn test_config(
  repos   : HashMap<RepoName, SkgfileRepo>,
  data_root : PathBuf,
) -> SkgConfig {
  let mut cfg : SkgConfig = SkgConfig::dummyFromRepos (repos);
  cfg . data_root = data_root;
  cfg }

#[test]
fn test_load_from_single_repo() {
  let temp_dir : TempDir = tempdir() . unwrap();
  let repo_path : PathBuf = temp_dir . path() . join ("main");
  fs::create_dir_all (&repo_path) . unwrap();

  let config : SkgConfig = { // Needed by write_graphnode_to_repo
    let mut repos : HashMap<RepoName, SkgfileRepo> =
      HashMap::new();
    repos . insert(
      RepoName::from ("main"),
      SkgfileRepo {
        name: RepoName::from ("main"),
        abbreviation: None,
        path: repo_path . clone(),
        user_owns_it: true, } );
    test_config (repos, temp_dir . path () . to_path_buf ()) };

  // Create a test node
  let mut node : Graphnode = empty_node_complete();
  node . pid = ID::new ("test1");
  node . title = "Test Node 1" . to_string();
  set_repo_retagging_relRepos ( &mut node, &RepoName::from ("main") );
  write_graphnode_to_repo(&node, &config) . unwrap();

  let result : IoResult<Vec<Graphnode>> =
    read_all_skg_files_from_repos (&config);
  assert!(result . is_ok(),
          "Should successfully load from single repo");

  let nodes : Vec<Graphnode> = result . unwrap();
  assert_eq!(nodes . len(), 1, "Should have loaded 1 node");
  assert_eq!(&*nodes[0] . home_repo, "main", "Repo should be 'main'");
  assert_eq!(nodes[0] . title, "Test Node 1");
}

#[test]
fn test_load_from_multiple_repos() {
  let temp_dir : TempDir = tempdir() . unwrap();

  // Create two repo directories
  let main_path : PathBuf = temp_dir . path() . join ("main");
  let shared_path : PathBuf = temp_dir . path() . join ("shared");
  fs::create_dir_all (&main_path) . unwrap();
  fs::create_dir_all (&shared_path) . unwrap();

  let config : SkgConfig = {
    let mut repos : HashMap<RepoName, SkgfileRepo> = HashMap::new();
    repos . insert(
      RepoName::from ("main"),
      SkgfileRepo {
        name: RepoName::from ("main"),
        abbreviation: None,
        path: main_path,
        user_owns_it: true, } );
    repos . insert(
      RepoName::from ("shared"),
      SkgfileRepo {
        name: RepoName::from ("shared"),
        abbreviation: None,
        path: shared_path,
        user_owns_it: false, } );
    test_config (repos, temp_dir . path () . to_path_buf ()) };

  // Create nodes in main repo
  let mut node1 : Graphnode = empty_node_complete();
  node1 . pid = ID::new ("main1");
  node1 . title = "Main Node 1" . to_string();
  set_repo_retagging_relRepos ( &mut node1, &RepoName::from ("main") );
  write_graphnode_to_repo(&node1, &config) . unwrap();

  let mut node2 : Graphnode = empty_node_complete();
  node2 . pid = ID::new ("main2");
  node2 . title = "Main Node 2" . to_string();
  set_repo_retagging_relRepos ( &mut node2, &RepoName::from ("main") );
  write_graphnode_to_repo(&node2, &config) . unwrap();

  // Create a node in the shared repo. Written RAW: 'shared' is
  // foreign, and the node writer is the SAVE path, which refuses a
  // foreign home rather than drop the title silently. Planting a
  // foreign fixture is a filesystem act, not a save.
  fs::write (
    config . repos . get (&RepoName::from ("shared"))
      . unwrap () . path . join ("shared1.skg"),
    "pid: shared1\ntitle: Shared Node 1\n" ) . unwrap();

  let result : IoResult<Vec<Graphnode>> =
    read_all_skg_files_from_repos (&config);
  assert!(result . is_ok(), "Should successfully load from multiple repos");

  let nodes : Vec<Graphnode> = result . unwrap();
  assert_eq!(nodes . len(), 3, "Should have loaded 3 nodes total");

  // Verify repos are set correctly
  let main_nodes: Vec<&Graphnode> = nodes . iter()
    . filter(|n| &*n . home_repo == "main")
    . collect();
  let shared_nodes: Vec<&Graphnode> = nodes . iter()
    . filter(|n| &*n . home_repo == "shared")
    . collect();

  assert_eq!(main_nodes . len(), 2, "Should have 2 nodes from main");
  assert_eq!(shared_nodes . len(), 1, "Should have 1 node from shared");
}

#[test]
fn test_telescope_is_not_a_conflict_but_two_pids_are() {
  let temp_dir : TempDir = tempdir() . unwrap();

  // Create two repo directories
  let main_path : PathBuf = temp_dir . path() . join ("main");
  let shared_path : PathBuf = temp_dir . path() . join ("shared");
  fs::create_dir_all (&main_path) . unwrap();
  fs::create_dir_all (&shared_path) . unwrap();

  let config : SkgConfig = {
    let mut repos : HashMap<RepoName, SkgfileRepo> =
      HashMap::new();
    repos . insert(
      RepoName::from ("main"),
      SkgfileRepo {
        name: RepoName::from ("main"),
        abbreviation: None,
        path: main_path,
        user_owns_it: true, } );
    repos . insert(
      RepoName::from ("shared"),
      SkgfileRepo {
        name: RepoName::from ("shared"),
        abbreviation: None,
        path: shared_path,
        user_owns_it: false, } );
    test_config (repos, temp_dir . path () . to_path_buf ()) };

  // Same pid in both repos: no longer a duplicate -- they are the
  // SECTIONS of one privacy telescope, folded into one node whose
  // home is the more public section (alphabetical fallback order for
  // this dummy config: "main" precedes "shared"). The stray second
  // title is a fold warning, not an error. The section files are
  // written RAW: a whole-node write would (correctly) sweep the
  // pid's sections at other repos, so two sequential
  // write_graphnode_to_repo calls cannot build a telescope.
  fs::write (
    config . repos . get (&RepoName::from ("main"))
      . unwrap () . path . join ("duplicate_id.skg"),
    "pid: duplicate_id\ntitle: Node in Main\n" ) . unwrap ();
  fs::write (
    config . repos . get (&RepoName::from ("shared"))
      . unwrap () . path . join ("duplicate_id.skg"),
    "pid: duplicate_id\ntitle: Node in Shared\n" ) . unwrap ();

  let nodes : Vec<Graphnode> =
    read_all_skg_files_from_repos (&config) . unwrap();
  assert_eq!( nodes . len(), 1,
    "same-pid files across repos fold into one telescope" );
  assert_eq!( nodes[0] . title, "Node in Main",
    "the home (most public titled section) wins the title" );
  assert_eq!( nodes[0] . home_repo, RepoName::from ("main") );
  error_unless_each_id_names_one_node (
    &nodes, &config . data_root)
    . expect ("a telescope is not an id conflict");

  { // What REMAINS a conflict: one id claimed by two distinct pids
    // (here via extra_ids).
    let mut node_a : Graphnode = empty_node_complete();
    node_a . pid = ID::new ("pid-a");
    node_a . title = "A" . to_string();
    node_a . extra_ids = vec! [ ID::new ("contested") ];
    set_repo_retagging_relRepos ( &mut node_a, &RepoName::from ("main") );
    let mut node_b : Graphnode = empty_node_complete();
    node_b . pid = ID::new ("pid-b");
    node_b . title = "B" . to_string();
    node_b . extra_ids = vec! [ ID::new ("contested") ];
    set_repo_retagging_relRepos ( &mut node_b, &RepoName::from ("shared") );
    let result : IoResult<()> =
      error_unless_each_id_names_one_node (
        & [ node_a, node_b ], &config . data_root);
    assert!(result . is_err(), "Should fail: two pids claim one id");
    let err : IoError = result . unwrap_err();
    assert_eq!(err . kind(), IoErrorKind::InvalidData);
    let err_msg = err . to_string();
    assert!(err_msg . contains ("claimed by more than one node"),
            "Error should say what the violation is: {}", err_msg);
    assert!(err_msg . contains ("contested"), "Error should include the ID"); }
}

#[test]
fn test_one_id_claimed_by_a_pid_and_anothers_extra_id() {
  let temp_dir : TempDir = tempdir() . unwrap();

  // Create two repo directories
  let main_path : PathBuf = temp_dir . path() . join ("main");
  let shared_path : PathBuf = temp_dir . path() . join ("shared");
  fs::create_dir_all (&main_path) . unwrap();
  fs::create_dir_all (&shared_path) . unwrap();

  let config : SkgConfig = {
    let mut repos : HashMap<RepoName, SkgfileRepo> =
      HashMap::new();
    repos . insert(
      RepoName::from ("main"),
      SkgfileRepo {
        name: RepoName::from ("main"),
        abbreviation: None,
        path: main_path,
        user_owns_it: true, } );
    repos . insert(
      RepoName::from ("shared"),
      SkgfileRepo {
        name: RepoName::from ("shared"),
        abbreviation: None,
        path: shared_path,
        user_owns_it: false, } );
    test_config (repos, temp_dir . path () . to_path_buf ()) };

  // Create node in main with multiple IDs
  let mut node1 : Graphnode = empty_node_complete();
  node1 . pid = ID::new ("id1");
  node1 . extra_ids = vec![ID::new ("id2")];
  node1 . title = "Node with Multiple IDs" . to_string();
  set_repo_retagging_relRepos ( &mut node1, &RepoName::from ("main") );
  write_graphnode_to_repo(&node1, &config) . unwrap();

  // Create node in shared that has one overlapping ID. Written RAW:
  // 'shared' is foreign, and the node writer refuses a foreign home.
  fs::write (
    config . repos . get (&RepoName::from ("shared"))
      . unwrap () . path . join ("id2.skg"),
    "pid: id2\ntitle: Another Node\nextra_ids:\n- id3\n" ) . unwrap();

  let nodes : Vec<Graphnode> =
    read_all_skg_files_from_repos (&config) . unwrap();
  let result : IoResult<()> =
    error_unless_each_id_names_one_node (
      &nodes, &config . data_root);
  assert!(result . is_err(), "Should fail due to overlapping ID");

  let err : IoError = result . unwrap_err();
  assert_eq!(err . kind(), IoErrorKind::InvalidData);
  assert!(err . to_string() . contains ("id2"), "Error should mention the overlapping ID");
}

#[test]
fn test_load_from_empty_repos() {
  let temp_dir : TempDir = tempdir() . unwrap();
  let repo_path : PathBuf = temp_dir . path() . join ("empty_repo");
  fs::create_dir_all (&repo_path) . unwrap();

  let result : IoResult<Vec<Graphnode>> = {
    let mut repos : HashMap<RepoName, SkgfileRepo> =
      HashMap::new();
    repos . insert(
      RepoName::from ("empty"),
      SkgfileRepo {
        name: RepoName::from ("empty"),
        abbreviation: None,
        path: repo_path,
        user_owns_it: true, } );
    read_all_skg_files_from_repos(
      &test_config (repos,
                    temp_dir . path () . to_path_buf () )) };
  assert!(result . is_ok(),
          "Should successfully handle empty repo");

  let nodes : Vec<Graphnode> = result . unwrap();
  assert_eq!(nodes . len(), 0,
             "Should have loaded 0 nodes from empty repo");
}

#[test]
fn test_repo_field_set_correctly() {
  let temp_dir : TempDir = tempdir() . unwrap();

  let repo_a : PathBuf = temp_dir . path() . join ("repo_a");
  let repo_b : PathBuf = temp_dir . path() . join ("repo_b");
  fs::create_dir_all (&repo_a) . unwrap();
  fs::create_dir_all (&repo_b) . unwrap();

  let config : SkgConfig = {
    let mut repos : HashMap<RepoName, SkgfileRepo> =
      HashMap::new();
    repos . insert(
      RepoName::from ("repo_a"),
      SkgfileRepo {
        name: RepoName::from ("repo_a"),
        abbreviation: None,
        path: repo_a,
        user_owns_it: true, } );
    repos . insert(
      RepoName::from ("repo_b"),
      SkgfileRepo {
        name: RepoName::from ("repo_b"),
        abbreviation: None,
        path: repo_b,
        user_owns_it: true, } );
    test_config (repos, temp_dir . path () . to_path_buf ()) };

  // Create nodes
  let mut node_a : Graphnode = empty_node_complete();
  node_a . pid = ID::new ("node_a");
  node_a . title = "Node A" . to_string();
  set_repo_retagging_relRepos ( &mut node_a, &RepoName::from ("repo_a") );
  write_graphnode_to_repo(&node_a, &config) . unwrap();

  let mut node_b : Graphnode = empty_node_complete();
  node_b . pid = ID::new ("node_b");
  node_b . title = "Node B" . to_string();
  set_repo_retagging_relRepos ( &mut node_b, &RepoName::from ("repo_b") );
  write_graphnode_to_repo(&node_b, &config) . unwrap();

  let result : IoResult<Vec<Graphnode>> =
    read_all_skg_files_from_repos (&config);
  assert!(result . is_ok());

  let nodes : Vec<Graphnode> = result . unwrap();
  assert_eq!(nodes . len(), 2);

  // Find each node and verify repo
  let node_a_result : Option<&Graphnode> =
    nodes . iter() . find(|n| n . pid . as_str() == "node_a");
  let node_b_result : Option<&Graphnode> =
    nodes . iter() . find(|n| n . pid . as_str() == "node_b");

  assert!(node_a_result . is_some());
  assert!(node_b_result . is_some());

  assert_eq!(&*node_a_result . unwrap() . home_repo, "repo_a");
  assert_eq!(&*node_b_result . unwrap() . home_repo, "repo_b");
}

#[test]
fn test_many_id_conflicts_create_org_file() {
  // Test that >10 duplicates triggers org file creation
  let temp_dir : TempDir = tempdir() . unwrap();

  let repo_a : PathBuf = temp_dir . path() . join ("repo_a");
  let repo_b : PathBuf = temp_dir . path() . join ("repo_b");
  fs::create_dir_all (&repo_a) . unwrap();
  fs::create_dir_all (&repo_b) . unwrap();

  let mut repos : HashMap<RepoName, SkgfileRepo> =
    HashMap::new();
  repos . insert(
    RepoName::from ("repo_a"),
    SkgfileRepo {
      name: RepoName::from ("repo_a"),
        abbreviation: None,
      path: repo_a,
      user_owns_it: true, } );
  repos . insert(
    RepoName::from ("repo_b"),
    SkgfileRepo {
      name: RepoName::from ("repo_b"),
        abbreviation: None,
      path: repo_b,
      user_owns_it: true,
    }
  );
  let config : SkgConfig = test_config (repos, temp_dir . path () . to_path_buf ());

  // Create 15 GENUINE id conflicts: same-pid files across repos
  // are telescope sections now, so a conflict means one id claimed
  // by two DISTINCT pids -- here via extra_ids. In-memory nodes
  // suffice; the check takes the folded node list.
  let mut nodes : Vec<Graphnode> = Vec::new ();
  for i in 1..=15 {
    let id : String = format!("dup_id_{}", i);
    let mut node_a : Graphnode = empty_node_complete();
    let mut node_b : Graphnode = empty_node_complete();
    node_a . pid = ID::new (&format!("pid_a_{}", i));
    node_b . pid = ID::new (&format!("pid_b_{}", i));
    node_a . extra_ids = vec![ID::new (&id)];
    node_b . extra_ids = vec![ID::new (&id)];
    node_a . title = format!("Node A {}", i);
    node_b . title = format!("Node B {}", i);
    set_repo_retagging_relRepos ( &mut node_a, &RepoName::from ("repo_a") );
    set_repo_retagging_relRepos ( &mut node_b, &RepoName::from ("repo_b") );
    nodes . push (node_a);
    nodes . push (node_b); }

  let result : IoResult<()> =
    error_unless_each_id_names_one_node (
      &nodes, &config . data_root);
  assert!(result . is_err(), "Should fail: 15 ids claimed by two nodes each");

  let err : IoError = result . unwrap_err();
  assert_eq!(err . kind(), IoErrorKind::InvalidData);
  let err_msg = err . to_string();
  assert!(err_msg . contains ("15")
          && err_msg . contains ("claimed by more than one node"),
          "Error should count the conflicts and name them: {}", err_msg);

  // Check that org file was created in the test's tempdir.
  let org_file_path : PathBuf =
    temp_dir . path () . join (
      "initialization-error_ids-claimed-by-two-nodes.org");
  assert!(org_file_path . exists(),
          "Org file should be created for >10 conflicts");

  // Generate expected content programmatically
  let mut expected : String = String::new();
  expected . push_str ("#+title: IDs claimed by more than one node\n");
  expected . push_str ("#+date: <generated at initialization>\n\n");
  expected . push_str ("15 id(s) claimed by more than one node. Same-id files ACROSS REPOS are not this: those are the sections of one privacy telescope (docs/telescopes.org). Each id below is claimed, as a primary or extra id, by the distinct nodes listed under it.\n\n");

  // IDs are sorted alphabetically (lexicographic), not numerically
  // So: dup_id_1, dup_id_10, dup_id_11, ..., dup_id_2, ...
  let mut ids : Vec<String> =
    (1..=15) . map(|i| format!("dup_id_{}", i)) . collect();
  ids . sort();

  for id in ids {
    let n : &str = id . rsplit ('_') . next () . unwrap ();
    expected . push_str(&format!("* {}\n", id));
    expected . push_str(&format!("** pid_a_{} (repo_a)\n", n));
    expected . push_str(&format!("** pid_b_{} (repo_b)\n", n));
  }

  // Read and verify full org file content
  let org_content : String = fs::read_to_string (&org_file_path) . unwrap();
  assert_eq!(org_content, expected,
             "Org file content should match expected format exactly");
  // No explicit cleanup: temp_dir's Drop handles it.
}

#[test]
fn test_unreadable_files_creates_org_file() {
  // Test that unreadable files trigger org file creation
  let temp_dir : TempDir = tempdir() . unwrap();

  let repo_good : PathBuf = temp_dir . path() . join ("repo_good");
  let repo_bad : PathBuf = temp_dir . path() . join ("repo_bad");
  fs::create_dir_all (&repo_good) . unwrap();
  // Don't create repo_bad directory - it should cause an error

  // Create config with only the good repo for writing
  let mut write_repos : HashMap<RepoName, SkgfileRepo> =
    HashMap::new();
  write_repos . insert(
    RepoName::from ("repo_good"),
    SkgfileRepo {
      name: RepoName::from ("repo_good"),
        abbreviation: None,
      path: repo_good . clone(),
      user_owns_it: true, } );
  let write_config : SkgConfig =
    test_config (write_repos,
                 temp_dir . path () . to_path_buf ());

  // Create a valid node in the good repo
  let mut node : Graphnode = empty_node_complete();
  node . pid = ID::new ("test1");
  node . title = "Test Node" . to_string();
  set_repo_retagging_relRepos ( &mut node, &RepoName::from ("repo_good") );
  write_graphnode_to_repo(&node, &write_config) . unwrap();

  // Create config with both repos for reading (including the bad one)
  let mut repos : HashMap<RepoName, SkgfileRepo> =
    HashMap::new();
  repos . insert(
    RepoName::from ("repo_good"),
    SkgfileRepo {
      name: RepoName::from ("repo_good"),
        abbreviation: None,
      path: repo_good,
      user_owns_it: true, } );
  repos . insert(
    RepoName::from ("repo_bad"),
    SkgfileRepo {
      name: RepoName::from ("repo_bad"),
        abbreviation: None,
      path: repo_bad . clone(),
      user_owns_it: true, } );

  let result : IoResult<Vec<Graphnode>> =
    read_all_skg_files_from_repos(
      &test_config (repos,
                    temp_dir . path () . to_path_buf () ));
  assert!(result . is_err(), "Should fail due to unreadable repo");

  let err : IoError = result . unwrap_err();
  assert_eq!(err . kind(), IoErrorKind::InvalidData);
  let err_msg = err . to_string();
  assert!(err_msg . contains ("unreadable"),
          "Error should mention unreadable files: {}", err_msg);

  // Check that org file was created in the test's tempdir.
  let org_file_path : PathBuf =
    temp_dir . path ()
      . join ("initialization-error_unreadable-skg-files.org");
  assert!(org_file_path . exists(),
          "Org file should be created for unreadable files");

  // Read org file content
  let org_content : String =
    fs::read_to_string (&org_file_path) . unwrap();

  // Verify header and count
  assert!(org_content . starts_with ("#+title: Unreadable SKG Files\n"));
  assert!(org_content . contains ("#+date: <generated at initialization>\n\n"));
  assert!(org_content . contains ("Found 1 unreadable file(s).\n\n"));

  // Verify structure: should have path as level 1, repo as level 2, error as level 3
  let bad_path_str : String = repo_bad . display() . to_string();
  assert!(org_content . contains(&format!("* {}\n", bad_path_str)),
          "Should list the bad path at level 1");
  assert!(org_content . contains ("** repo_bad\n"),
          "Should list repo_bad at level 2");
  assert!(org_content . contains ("*** Error: "),
          "Should have error message at level 3");

  // Error message is OS-dependent, but should mention the path issue
  assert!(org_content . contains ("No such file or directory") ||
          org_content . contains ("cannot find the path") ||
          org_content . contains ("system cannot find"),
          "Error should mention file/directory not found");

  // No explicit cleanup: temp_dir's Drop handles it.
}

/// The malformed scalar and foreign-home shapes
/// 'write_graphnode_telescope' refuses. Neither
/// arises from a skg save (every relRepo is clamped to at least the
/// owner's home); both arrive from hand-edited files or a pull.
/// Writing either would publish the node's text or lose it.
#[test]
fn a_write_refuses_a_foreign_home_and_a_title_hoist() {
  let temp_dir : TempDir = tempdir() . unwrap();
  let public_path  : PathBuf = temp_dir . path() . join ("public");
  let foreign_path : PathBuf = temp_dir . path() . join ("foreign");
  fs::create_dir_all (&public_path)  . unwrap();
  fs::create_dir_all (&foreign_path) . unwrap();
  let config : SkgConfig = {
    let mut repos : HashMap<RepoName, SkgfileRepo> =
      HashMap::new();
    for (name, path, owned) in
      [ ("public",  public_path  . clone(), true  ),
        ("foreign", foreign_path . clone(), false ) ] {
      repos . insert ( RepoName::from (name), SkgfileRepo {
        name         : RepoName::from (name),
        abbreviation : None,
        path,
        user_owns_it : owned, } ); }
    let mut config : SkgConfig =
      test_config (repos, temp_dir . path () . to_path_buf ());
    config . repo_order = // most public first
      vec! [ RepoName::from ("foreign"),
             RepoName::from ("public") ];
    config };
  { // FOREIGN HOME: refused, rather than silently dropping the title.
    let mut node : Graphnode = empty_node_complete();
    node . pid   = ID::new ("F");
    node . title = "foreign-homed" . to_string();
    set_repo_retagging_relRepos (
      &mut node, &RepoName::from ("foreign") );
    let err : IoError =
      write_graphnode_to_repo (&node, &config)
      . expect_err ("a foreign home is not writable");
    assert!( err . to_string() . contains ("do not own"),
             "the refusal says why: {}", err ); }
  { // TITLE HOIST: the home exists and is titleless, so the text
    // lives more privately and this write would publish it.
    fs::write ( public_path . join ("H.skg"),
                "pid: H\ncontains:\n- C\n" ) . unwrap();
    let mut node : Graphnode = empty_node_complete();
    node . pid   = ID::new ("H");
    node . title = "private text" . to_string();
    set_repo_retagging_relRepos (
      &mut node, &RepoName::from ("public") );
    let err : IoError =
      write_graphnode_to_repo (&node, &config)
      . expect_err ("hoisting a title into a titleless home is refused");
    assert!( err . to_string() . contains ("would publish it"),
             "the refusal says why: {}", err );
    assert_eq!( fs::read_to_string (
                  public_path . join ("H.skg") ) . unwrap(),
                "pid: H\ncontains:\n- C\n",
                "and nothing was written" ); }
  { // The ordinary shape still writes: an owned, titled home.
    let mut node : Graphnode = empty_node_complete();
    node . pid   = ID::new ("N");
    node . title = "ordinary" . to_string();
    set_repo_retagging_relRepos (
      &mut node, &RepoName::from ("public") );
    write_graphnode_to_repo (&node, &config)
      . expect ("an owned titled home writes"); }
}

#[test]
fn ordinary_writers_refuse_body_only_hoists_and_preflight_the_batch() {
  use skg::dbs::filesystem::multiple_nodes::write_all_nodes_to_fs;
  let temp_dir : TempDir = tempdir() . unwrap();
  let public_path  : PathBuf = temp_dir . path() . join ("public");
  let private_path : PathBuf = temp_dir . path() . join ("private");
  let foreign_path : PathBuf = temp_dir . path() . join ("foreign");
  for path in [&public_path, &private_path, &foreign_path] {
    fs::create_dir_all (path) . unwrap (); }
  let config : SkgConfig = {
    let mut repos : HashMap<RepoName, SkgfileRepo> = HashMap::new ();
    for (name, path, owned) in [
      ("public",  public_path  . clone (), true),
      ("private", private_path . clone (), true),
      ("foreign", foreign_path . clone (), false),
    ] {
      repos . insert ( RepoName::from (name), SkgfileRepo {
        name         : RepoName::from (name),
        abbreviation : None,
        path,
        user_owns_it : owned,
      } ); }
    let mut config : SkgConfig =
      test_config (repos, temp_dir . path () . to_path_buf ());
    config . repo_order = ["public", "private", "foreign"]
      . into_iter () . map (RepoName::from) . collect ();
    config };

  fs::write (
    public_path . join ("B.skg"),
    "title: visible title\npid: B\n" ) . unwrap ();
  fs::write (
    private_path . join ("B.skg"),
    "pid: B\nbody: hidden body\n" ) . unwrap ();
  let mut body_hoist : Graphnode = empty_node_complete ();
  body_hoist . pid = ID::new ("B");
  body_hoist . title = "visible title" . to_string ();
  body_hoist . body = Some ("hidden body" . to_string ());
  set_repo_retagging_relRepos (
    &mut body_hoist, &RepoName::from ("public") );
  let err : IoError = write_graphnode_to_repo (&body_hoist, &config)
    . expect_err ("a body below home requires interactive Hoist approval");
  assert! (err . to_string () . contains ("would publish it"));
  assert_eq! (
    fs::read_to_string (private_path . join ("B.skg")) . unwrap (),
    "pid: B\nbody: hidden body\n" );

  let mut valid : Graphnode = empty_node_complete ();
  valid . pid = ID::new ("V");
  valid . title = "valid" . to_string ();
  set_repo_retagging_relRepos (
    &mut valid, &RepoName::from ("public") );
  let mut invalid : Graphnode = empty_node_complete ();
  invalid . pid = ID::new ("X");
  invalid . title = "invalid" . to_string ();
  set_repo_retagging_relRepos (
    &mut invalid, &RepoName::from ("public") );
  invalid . contains . push (
    skg::types::misc::RelPartner::at_relRepo (
      RepoName::from ("foreign"), ID::new ("child") ));
  write_all_nodes_to_fs (vec! [valid, invalid], config)
    . expect_err ("a later foreign output rejects the entire batch");
  assert! (! public_path . join ("V.skg") . exists (),
           "the valid earlier node was not written before batch failure");
}
