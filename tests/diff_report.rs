use skg::diff_report::diff_report_as_org;
use skg::diff_report::types::DiffSelection;
use skg::serve::handlers::diff_report::handle_diff_report_request;
use skg::test_utils::read_lp_message;
use skg::types::misc::{SkgConfig, SkgRepo, SkgRepoName};

use git2::Repository;
use std::collections::HashMap;
use std::error::Error;
use std::fs;
use std::io::BufReader;
use std::net::{TcpListener, TcpStream};
use std::path::PathBuf;
use tempfile::TempDir;

#[path = "diff_report/diff.rs"]
mod diff;
#[path = "diff_report/render.rs"]
mod render;
#[path = "diff_report/snapshot.rs"]
mod git_snapshot;

#[test]
fn diff_report_includes_inbound_and_link_changes (
) -> Result<(), Box<dyn Error>> {
  let fixture : DiffFixture =
    DiffFixture::new () ?;
  fixture . write_node (
    "a",
    "Alpha",
    "",
    &[] ) ?;
  fixture . write_node (
    "b",
    "Beta",
    "",
    &[] ) ?;
  fixture . commit_all ("initial") ?;
  fixture . write_node (
    "a",
    "Alpha [[id:b][Beta]]",
    "body changed",
    &["b"] ) ?;
  let report : String =
    diff_report_as_org (
      &fixture . config,
      DiffSelection {
        include_staged: true,
        include_unstaged: true }) ?;
  assert! (
    report . contains ("**** contained"),
    "container node should report contained-role changes:\n{}",
    report );
  assert! (
    report . contains ("**** containers (with gains)"),
    "child node should report inbound container gains:\n{}",
    report );
  assert! (
    report . contains ("**** mentioner"),
    "mentioner role should be reported:\n{}",
    report );
  assert! (
    report . contains ("**** mentioned"),
    "mentioned role should be reported:\n{}",
    report );
  assert! (
    report . contains ("**** title"),
    "title text diff should be reported:\n{}",
    report );
  Ok (( )) }

#[test]
fn diff_report_shows_override_changes_on_raw_nodes (
) -> Result<(), Box<dyn Error>> {
  // No substitution in diff surfaces
  // (TODO/DONE/full-schema/DONE/12-2_diff-mode-policy_discussion.org): the diff
  // report renders raw nodes -- each under its own title -- and
  // reports an overrides change in both roles.
  let fixture : DiffFixture =
    DiffFixture::new () ?;
  fixture . write_node ("n", "Original", "", &[]) ?;
  fixture . write_node ("r", "Overrider", "", &[]) ?;
  fixture . commit_all ("initial") ?;
  fs::write (
    fixture . skgrepo . join ("r.skg"),
    "title: Overrider\npid: r\noverrides:\n- n\n" ) ?;
  let report : String =
    diff_report_as_org (
      &fixture . config,
      DiffSelection {
        include_staged: true,
        include_unstaged: true }) ?;
  assert! (
    report . contains ("overrider"),
    "the gaining node reports its outbound override change:\n{}",
    report );
  assert! (
    report . contains ("overridden"),
    "the overridden node reports the inbound change:\n{}",
    report );
  assert! (
    report . contains ("Overrider") && report . contains ("Original"),
    "each node appears under its OWN title -- no substitution:\n{}",
    report );
  Ok (( )) }

#[test]
fn diff_report_distinguishes_head_index_and_worktree (
) -> Result<(), Box<dyn Error>> {
  let fixture : DiffFixture =
    DiffFixture::new () ?;
  fixture . write_node ("a", "head", "", &[]) ?;
  fixture . commit_all ("initial") ?;
  fixture . write_node ("a", "index", "", &[]) ?;
  fixture . stage_all () ?;
  fixture . write_node ("a", "worktree", "", &[]) ?;
  let staged_report : String =
    diff_report_as_org (
      &fixture . config,
      DiffSelection {
        include_staged: true,
        include_unstaged: false }) ?;
  assert! (
    staged_report . contains ("+index"),
    "staged-only report should compare HEAD to index:\n{}",
    staged_report );
  assert! (
    ! staged_report . contains ("+worktree"),
    "staged-only report should not include worktree-only title:\n{}",
    staged_report );
  let unstaged_report : String =
    diff_report_as_org (
      &fixture . config,
      DiffSelection {
        include_staged: false,
        include_unstaged: true }) ?;
  assert! (
    unstaged_report . contains ("+worktree"),
    "unstaged-only report should compare index to worktree:\n{}",
    unstaged_report );
  assert! (
    unstaged_report . contains ("-index"),
    "unstaged-only report should use index as baseline:\n{}",
    unstaged_report );
  Ok (( )) }

#[test]
fn diff_report_handler_sends_length_prefixed_response (
) -> Result<(), Box<dyn Error>> {
  let fixture : DiffFixture =
    DiffFixture::new () ?;
  fixture . write_node ("a", "head", "", &[]) ?;
  fixture . commit_all ("initial") ?;
  fixture . write_node ("a", "worktree", "", &[]) ?;
  let listener : TcpListener =
    TcpListener::bind ("127.0.0.1:0") ?;
  let addr =
    listener . local_addr () ?;
  let client : TcpStream =
    TcpStream::connect (addr) ?;
  let (mut server, _peer) =
    listener . accept () ?;
  handle_diff_report_request (
    &mut server,
    "((request . \"diff report\") \
      (include-staged . \"true\") \
      (include-unstaged . \"true\"))",
    &fixture . config );
  drop (server);
  let mut reader : BufReader<TcpStream> =
    BufReader::new (client);
  let response : String =
    read_lp_message (&mut reader) ?;
  assert! (
    response . contains ("diff-report"),
    "response should be tagged as diff-report:\n{}",
    response );
  assert! (
    response . contains ("* affected nodes"),
    "response content should contain the org report:\n{}",
    response );
  Ok (( )) }

#[test]
fn diff_report_shows_cross_repo_inbound_relationships (
) -> Result<(), Box<dyn Error>> {
  let multi : MultiSkgRepoFixture =
    MultiSkgRepoFixture::new () ?;
  multi . left . write_node ("a", "Alpha", "", &[]) ?;
  multi . right . write_node ("b", "Beta", "", &[]) ?;
  multi . left . commit_all ("left initial") ?;
  multi . right . commit_all ("right initial") ?;
  multi . left . write_node ("a", "Alpha", "", &["b"]) ?;
  let report : String =
    diff_report_as_org (
      &multi . config,
      DiffSelection {
        include_staged: true,
        include_unstaged: true }) ?;
  assert! (
    report . contains ("**** contained"),
    "left-repo container should report outbound contained change:\n{}",
    report );
  assert! (
    report . contains ("**** containers (with gains)"),
    "right-repo child should report inbound container gain:\n{}",
    report );
  assert! (
    report . contains ("Alpha"),
    "report should include left-repo container title:\n{}",
    report );
  assert! (
    report . contains ("Beta"),
    "report should include right-repo child title:\n{}",
    report );
  Ok (( )) }

#[test]
fn diff_report_shows_repo_move_across_repos (
) -> Result<(), Box<dyn Error>> {
  let multi : MultiSkgRepoFixture =
    MultiSkgRepoFixture::new () ?;
  multi . left . write_node ("a", "Moved", "", &[]) ?;
  multi . right . write_node ("keep", "Keep", "", &[]) ?;
  multi . left . commit_all ("left initial") ?;
  multi . right . commit_all ("right initial") ?;
  fs::remove_file (multi . left . skgrepo . join ("a.skg")) ?;
  multi . right . write_node ("a", "Moved", "", &[]) ?;
  let report : String =
    diff_report_as_org (
      &multi . config,
      DiffSelection {
        include_staged: true,
        include_unstaged: true }) ?;
  assert! (
    report . contains ("**** repo"),
    "repo move should include repo section:\n{}",
    report );
  assert! (
    report . contains ("***** was: left"),
    "repo move should show old repo:\n{}",
    report );
  assert! (
    report . contains ("***** is: right"),
    "repo move should show new repo:\n{}",
    report );
  Ok (( )) }

#[test]
fn diff_report_shows_vanished_nodes (
) -> Result<(), Box<dyn Error>> {
  // TODO/more.org: a node the worktree still references, though its
  // file exists in no skgrepo, is investigated in git history: the
  // report names the commit it vanished at and what it was connected
  // to when last present. A reference that NEVER existed is reported
  // as such.
  let fixture : DiffFixture =
    DiffFixture::new () ?;
  fixture . write_node (
    "p", "Parent", "", &["v", "ghost"] ) ?;
  fixture . write_node (
    "v", "Vanishing", "links [[id:w][kept]]", &["w"] ) ?;
  fixture . write_node (
    "w", "Kept", "", &[] ) ?;
  fixture . commit_all ("initial") ?;
  { // Delete v's file (p still refers to it) and commit. The commit
    // helper's add_all does not stage deletions, so stage explicitly.
    fs::remove_file ( fixture . skgrepo . join ("v.skg") ) ?;
    let mut index : git2::Index = fixture . gitrepo . index () ?;
    index . update_all (["*"].iter (), None) ?;
    index . write () ?;
    fixture . commit_all ("delete v") ?; }
  fixture . write_node (
    // An unstaged worktree change, so the diff has changed paths.
    "w", "Kept, retitled", "", &[] ) ?;
  let report : String =
    diff_report_as_org (
      &fixture . config,
      DiffSelection {
        include_staged: true,
        include_unstaged: true }) ?;
  assert! ( report . contains ("* vanished nodes"),
    "the vanished-nodes section must render:\n{}", report );
  assert! ( report . contains ("** v\n"),
    "v must be reported as vanished:\n{}", report );
  assert! ( report . contains ("title when last present: Vanishing"),
    "v's old title must be shown:\n{}", report );
  assert! ( report . contains ("vanished at commit")
            && report . contains ("delete v"),
    "the vanishing commit must be named:\n{}", report );
  assert! ( report . contains ("***** p (via contains)"),
    "p's old reference to v must be shown:\n{}", report );
  assert! ( report . contains ("***** contains\n****** w"),
    "v's own old contains must be shown:\n{}", report );
  assert! ( report . contains ("** ghost\n*** never present"),
    "a never-existing reference must say so:\n{}", report );
  Ok (( )) }

#[test]
fn diff_report_refuses_non_git_repos (
) -> Result<(), Box<dyn Error>> {
  let tmp : TempDir =
    tempfile::tempdir () ?;
  let skgrepo_dir : PathBuf =
    tmp . path () . join ("main");
  fs::create_dir (&skgrepo_dir) ?;
  let skgrepo_name : SkgRepoName =
    SkgRepoName::from ("main");
  let config : SkgConfig =
    SkgConfig::dummyFromSkgRepos (HashMap::from ([
      (skgrepo_name . clone (),
       SkgRepo {
         name: skgrepo_name,
         abbreviation: None,
         path: skgrepo_dir,
         owned: true }) ]));
  let error : String =
    diff_report_as_org (
      &config,
      DiffSelection {
        include_staged: true,
        include_unstaged: true })
    . unwrap_err ();
  assert! (
    error . contains ("not in a git repository"),
    "non-git repo should be refused: {}",
    error );
  Ok (( )) }

struct DiffFixture {
  _tmp    : TempDir,
  gitrepo    : Repository,
  skgrepo  : PathBuf,
  config  : SkgConfig,
}

struct SkgrepoWithGitrepo {
  gitrepo   : Repository,
  skgrepo : PathBuf,
}

impl SkgrepoWithGitrepo {
  fn new (
    root : &PathBuf,
    name : &str,
  ) -> Result<Self, Box<dyn Error>> {
    let skgrepo : PathBuf =
      root . join (name);
    fs::create_dir (&skgrepo) ?;
    let gitrepo : Repository =
      Repository::init (&skgrepo) ?;
    configure_git_user (&gitrepo) ?;
    Ok ( SkgrepoWithGitrepo { gitrepo, skgrepo } )
  }

  fn write_node (
    &self,
    pid      : &str,
    title    : &str,
    body     : &str,
    contains : &[&str],
  ) -> Result<(), Box<dyn Error>> {
    write_node_file (&self . skgrepo, pid, title, body, contains)
  }

  fn commit_all (
    &self,
    message : &str,
  ) -> Result<(), Box<dyn Error>> {
    commit_gitrepo_all (&self . gitrepo, message)
  }
}

struct MultiSkgRepoFixture {
  _tmp   : TempDir,
  left   : SkgrepoWithGitrepo,
  right  : SkgrepoWithGitrepo,
  config : SkgConfig,
}

impl MultiSkgRepoFixture {
  fn new (
  ) -> Result<Self, Box<dyn Error>> {
    let tmp : TempDir =
      tempfile::tempdir () ?;
    let left : SkgrepoWithGitrepo =
      SkgrepoWithGitrepo::new (&tmp . path () . to_path_buf (), "left") ?;
    let right : SkgrepoWithGitrepo =
      SkgrepoWithGitrepo::new (&tmp . path () . to_path_buf (), "right") ?;
    let left_name : SkgRepoName =
      SkgRepoName::from ("left");
    let right_name : SkgRepoName =
      SkgRepoName::from ("right");
    let config : SkgConfig =
      SkgConfig::dummyFromSkgRepos (HashMap::from ([
        (left_name . clone (),
         SkgRepo {
           name: left_name,
           abbreviation: None,
           path: left . skgrepo . clone (),
           owned: true }),
        (right_name . clone (),
         SkgRepo {
           name: right_name,
           abbreviation: None,
           path: right . skgrepo . clone (),
           owned: true }) ]));
    Ok ( MultiSkgRepoFixture {
      _tmp: tmp,
      left,
      right,
      config } )
  }
}

impl DiffFixture {
  fn new (
  ) -> Result<Self, Box<dyn Error>> {
    let tmp : TempDir =
      tempfile::tempdir () ?;
    let gitrepo : Repository =
      Repository::init (tmp . path ()) ?;
    configure_git_user (&gitrepo) ?;
    let skgrepo : PathBuf =
      tmp . path () . join ("main");
    fs::create_dir (&skgrepo) ?;
    let skgrepo_name : SkgRepoName =
      SkgRepoName::from ("main");
    let config : SkgConfig =
      SkgConfig::dummyFromSkgRepos (HashMap::from ([
        (skgrepo_name . clone (),
         SkgRepo {
           name: skgrepo_name,
           abbreviation: None,
           path: skgrepo . clone (),
           owned: true }) ]));
    Ok ( DiffFixture {
      _tmp: tmp,
      gitrepo,
      skgrepo,
      config } )
  }

  fn write_node (
    &self,
    pid      : &str,
    title    : &str,
    body     : &str,
    contains : &[&str],
  ) -> Result<(), Box<dyn Error>> {
    write_node_file (&self . skgrepo, pid, title, body, contains)
  }

  fn stage_all (
    &self,
  ) -> Result<(), Box<dyn Error>> {
    stage_gitrepo_all (&self . gitrepo) }

  fn commit_all (
    &self,
    message : &str,
  ) -> Result<(), Box<dyn Error>> {
    commit_gitrepo_all (&self . gitrepo, message) }
}

fn write_node_file (
  skgrepo   : &PathBuf,
  pid      : &str,
  title    : &str,
  body     : &str,
  contains : &[&str],
) -> Result<(), Box<dyn Error>> {
  let contains_yaml : String =
    if contains . is_empty () {
      String::new ()
    } else {
      format! (
        "contains:\n{}\n",
        contains . iter ()
          . map ( |skgid| format! ("- {}", skgid) )
          . collect::<Vec<String>> ()
          . join ("\n") ) };
  let body_yaml : String =
    if body . is_empty () {
      String::new ()
    } else {
      format! ("body: {}\n", body) };
  fs::write (
    skgrepo . join (format! ("{}.skg", pid)),
    format! (
      "title: {}\npid: {}\n{}{}",
      title, pid, body_yaml, contains_yaml )) ?;
  Ok (( )) }

fn stage_gitrepo_all (
  gitrepo : &Repository,
) -> Result<(), Box<dyn Error>> {
  let mut index : git2::Index =
    gitrepo . index () ?;
  index . add_all (["*"].iter (), git2::IndexAddOption::DEFAULT, None) ?;
  index . write () ?;
  Ok (( )) }

fn commit_gitrepo_all (
  gitrepo    : &Repository,
  message : &str,
) -> Result<(), Box<dyn Error>> {
  stage_gitrepo_all (gitrepo) ?;
  let mut index : git2::Index =
    gitrepo . index () ?;
  let tree_id : git2::Oid =
    index . write_tree () ?;
  let tree : git2::Tree =
    gitrepo . find_tree (tree_id) ?;
  let sig : git2::Signature =
    gitrepo . signature () ?;
  let parents : Vec<git2::Commit> =
    match gitrepo . head () {
      Ok (head) => vec! [head . peel_to_commit () ?],
      Err (_)   => Vec::new () };
  let parent_refs : Vec<&git2::Commit> =
    parents . iter () . collect ();
  gitrepo . commit (
    Some ("HEAD"),
    &sig, &sig,
    message,
    &tree,
    &parent_refs) ?;
  Ok (( )) }

fn configure_git_user (
  gitrepo : &Repository,
) -> Result<(), Box<dyn Error>> {
  let mut config : git2::Config =
    gitrepo . config () ?;
  config . set_str ("user.email", "test@test.com") ?;
  config . set_str ("user.name", "Test") ?;
  Ok (( )) }
