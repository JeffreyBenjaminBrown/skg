use skg::diff_analysis::diff_analysis_report;
use skg::diff_analysis::types::DiffSelection;
use skg::serve::handlers::diff_analysis::handle_diff_analysis_request;
use skg::test_utils::read_lp_message;
use skg::types::misc::{SkgConfig, SkgfileSource, SourceName};

use git2::Repository;
use std::collections::HashMap;
use std::error::Error;
use std::fs;
use std::io::BufReader;
use std::net::{TcpListener, TcpStream};
use std::path::PathBuf;
use tempfile::TempDir;

#[path = "diff_analysis/diff.rs"]
mod diff;
#[path = "diff_analysis/render.rs"]
mod render;
#[path = "diff_analysis/snapshot.rs"]
mod snapshot;

#[test]
fn diff_analysis_report_includes_inbound_and_textlink_changes (
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
    diff_analysis_report (
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
    report . contains ("**** source"),
    "textlink source role should be reported:\n{}",
    report );
  assert! (
    report . contains ("**** dest"),
    "textlink dest role should be reported:\n{}",
    report );
  assert! (
    report . contains ("**** title"),
    "title text diff should be reported:\n{}",
    report );
  Ok (( )) }

#[test]
fn diff_analysis_reports_override_changes_on_raw_nodes (
) -> Result<(), Box<dyn Error>> {
  // No substitution in diff surfaces
  // (TODO/full-schema/12-2_diff-mode-policy_discussion.org): diff
  // analysis renders raw nodes -- each under its own title -- and
  // reports an overrides_view_of change in both roles.
  let fixture : DiffFixture =
    DiffFixture::new () ?;
  fixture . write_node ("n", "Original", "", &[]) ?;
  fixture . write_node ("r", "Overrider", "", &[]) ?;
  fixture . commit_all ("initial") ?;
  fs::write (
    fixture . source . join ("r.skg"),
    "title: Overrider\npid: r\noverrides_view_of:\n- n\n" ) ?;
  let report : String =
    diff_analysis_report (
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
fn diff_analysis_distinguishes_head_index_and_worktree (
) -> Result<(), Box<dyn Error>> {
  let fixture : DiffFixture =
    DiffFixture::new () ?;
  fixture . write_node ("a", "head", "", &[]) ?;
  fixture . commit_all ("initial") ?;
  fixture . write_node ("a", "index", "", &[]) ?;
  fixture . stage_all () ?;
  fixture . write_node ("a", "worktree", "", &[]) ?;
  let staged_report : String =
    diff_analysis_report (
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
    diff_analysis_report (
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
fn diff_analysis_handler_sends_length_prefixed_response (
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
  handle_diff_analysis_request (
    &mut server,
    "((request . \"diff analysis\") \
      (include-staged . \"true\") \
      (include-unstaged . \"true\"))",
    &fixture . config );
  drop (server);
  let mut reader : BufReader<TcpStream> =
    BufReader::new (client);
  let response : String =
    read_lp_message (&mut reader) ?;
  assert! (
    response . contains ("diff-analysis"),
    "response should be tagged as diff-analysis:\n{}",
    response );
  assert! (
    response . contains ("* affected nodes"),
    "response content should contain the org report:\n{}",
    response );
  Ok (( )) }

#[test]
fn diff_analysis_reports_cross_source_inbound_relationships (
) -> Result<(), Box<dyn Error>> {
  let multi : MultiSourceFixture =
    MultiSourceFixture::new () ?;
  multi . left . write_node ("a", "Alpha", "", &[]) ?;
  multi . right . write_node ("b", "Beta", "", &[]) ?;
  multi . left . commit_all ("left initial") ?;
  multi . right . commit_all ("right initial") ?;
  multi . left . write_node ("a", "Alpha", "", &["b"]) ?;
  let report : String =
    diff_analysis_report (
      &multi . config,
      DiffSelection {
        include_staged: true,
        include_unstaged: true }) ?;
  assert! (
    report . contains ("**** contained"),
    "left-source container should report outbound contained change:\n{}",
    report );
  assert! (
    report . contains ("**** containers (with gains)"),
    "right-source child should report inbound container gain:\n{}",
    report );
  assert! (
    report . contains ("Alpha"),
    "report should include left-source container title:\n{}",
    report );
  assert! (
    report . contains ("Beta"),
    "report should include right-source child title:\n{}",
    report );
  Ok (( )) }

#[test]
fn diff_analysis_reports_source_move_across_repos (
) -> Result<(), Box<dyn Error>> {
  let multi : MultiSourceFixture =
    MultiSourceFixture::new () ?;
  multi . left . write_node ("a", "Moved", "", &[]) ?;
  multi . right . write_node ("keep", "Keep", "", &[]) ?;
  multi . left . commit_all ("left initial") ?;
  multi . right . commit_all ("right initial") ?;
  fs::remove_file (multi . left . source . join ("a.skg")) ?;
  multi . right . write_node ("a", "Moved", "", &[]) ?;
  let report : String =
    diff_analysis_report (
      &multi . config,
      DiffSelection {
        include_staged: true,
        include_unstaged: true }) ?;
  assert! (
    report . contains ("**** source"),
    "source move should include source section:\n{}",
    report );
  assert! (
    report . contains ("***** was: left"),
    "source move should show old source:\n{}",
    report );
  assert! (
    report . contains ("***** is: right"),
    "source move should show new source:\n{}",
    report );
  Ok (( )) }

#[test]
fn diff_analysis_reports_vanished_nodes (
) -> Result<(), Box<dyn Error>> {
  // TODO/more.org: a node the worktree still references, though its
  // file exists in no source, is investigated in git history: the
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
    fs::remove_file ( fixture . source . join ("v.skg") ) ?;
    let mut index : git2::Index = fixture . repo . index () ?;
    index . update_all (["*"].iter (), None) ?;
    index . write () ?;
    fixture . commit_all ("delete v") ?; }
  fixture . write_node (
    // An unstaged worktree change, so the diff has changed paths.
    "w", "Kept, retitled", "", &[] ) ?;
  let report : String =
    diff_analysis_report (
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
fn diff_analysis_refuses_non_git_sources (
) -> Result<(), Box<dyn Error>> {
  let tmp : TempDir =
    tempfile::tempdir () ?;
  let source_dir : PathBuf =
    tmp . path () . join ("main");
  fs::create_dir (&source_dir) ?;
  let source_name : SourceName =
    SourceName::from ("main");
  let config : SkgConfig =
    SkgConfig::dummyFromSources (HashMap::from ([
      (source_name . clone (),
       SkgfileSource {
         name: source_name,
         abbreviation: None,
         path: source_dir,
         user_owns_it: true }) ]));
  let error : String =
    diff_analysis_report (
      &config,
      DiffSelection {
        include_staged: true,
        include_unstaged: true })
    . unwrap_err ();
  assert! (
    error . contains ("not in a git repository"),
    "non-git source should be refused: {}",
    error );
  Ok (( )) }

struct DiffFixture {
  _tmp    : TempDir,
  repo    : Repository,
  source  : PathBuf,
  config  : SkgConfig,
}

struct SourceRepo {
  repo   : Repository,
  source : PathBuf,
}

impl SourceRepo {
  fn new (
    root : &PathBuf,
    name : &str,
  ) -> Result<Self, Box<dyn Error>> {
    let source : PathBuf =
      root . join (name);
    fs::create_dir (&source) ?;
    let repo : Repository =
      Repository::init (&source) ?;
    configure_git_user (&repo) ?;
    Ok ( SourceRepo { repo, source } )
  }

  fn write_node (
    &self,
    pid      : &str,
    title    : &str,
    body     : &str,
    contains : &[&str],
  ) -> Result<(), Box<dyn Error>> {
    write_node_file (&self . source, pid, title, body, contains)
  }

  fn commit_all (
    &self,
    message : &str,
  ) -> Result<(), Box<dyn Error>> {
    commit_repo_all (&self . repo, message)
  }
}

struct MultiSourceFixture {
  _tmp   : TempDir,
  left   : SourceRepo,
  right  : SourceRepo,
  config : SkgConfig,
}

impl MultiSourceFixture {
  fn new (
  ) -> Result<Self, Box<dyn Error>> {
    let tmp : TempDir =
      tempfile::tempdir () ?;
    let left : SourceRepo =
      SourceRepo::new (&tmp . path () . to_path_buf (), "left") ?;
    let right : SourceRepo =
      SourceRepo::new (&tmp . path () . to_path_buf (), "right") ?;
    let left_name : SourceName =
      SourceName::from ("left");
    let right_name : SourceName =
      SourceName::from ("right");
    let config : SkgConfig =
      SkgConfig::dummyFromSources (HashMap::from ([
        (left_name . clone (),
         SkgfileSource {
           name: left_name,
           abbreviation: None,
           path: left . source . clone (),
           user_owns_it: true }),
        (right_name . clone (),
         SkgfileSource {
           name: right_name,
           abbreviation: None,
           path: right . source . clone (),
           user_owns_it: true }) ]));
    Ok ( MultiSourceFixture {
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
    let repo : Repository =
      Repository::init (tmp . path ()) ?;
    configure_git_user (&repo) ?;
    let source : PathBuf =
      tmp . path () . join ("main");
    fs::create_dir (&source) ?;
    let source_name : SourceName =
      SourceName::from ("main");
    let config : SkgConfig =
      SkgConfig::dummyFromSources (HashMap::from ([
        (source_name . clone (),
         SkgfileSource {
           name: source_name,
           abbreviation: None,
           path: source . clone (),
           user_owns_it: true }) ]));
    Ok ( DiffFixture {
      _tmp: tmp,
      repo,
      source,
      config } )
  }

  fn write_node (
    &self,
    pid      : &str,
    title    : &str,
    body     : &str,
    contains : &[&str],
  ) -> Result<(), Box<dyn Error>> {
    write_node_file (&self . source, pid, title, body, contains)
  }

  fn stage_all (
    &self,
  ) -> Result<(), Box<dyn Error>> {
    stage_repo_all (&self . repo) }

  fn commit_all (
    &self,
    message : &str,
  ) -> Result<(), Box<dyn Error>> {
    commit_repo_all (&self . repo, message) }
}

fn write_node_file (
  source   : &PathBuf,
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
          . map ( |id| format! ("- {}", id) )
          . collect::<Vec<String>> ()
          . join ("\n") ) };
  let body_yaml : String =
    if body . is_empty () {
      String::new ()
    } else {
      format! ("body: {}\n", body) };
  fs::write (
    source . join (format! ("{}.skg", pid)),
    format! (
      "title: {}\npid: {}\n{}{}",
      title, pid, body_yaml, contains_yaml )) ?;
  Ok (( )) }

fn stage_repo_all (
  repo : &Repository,
) -> Result<(), Box<dyn Error>> {
  let mut index : git2::Index =
    repo . index () ?;
  index . add_all (["*"].iter (), git2::IndexAddOption::DEFAULT, None) ?;
  index . write () ?;
  Ok (( )) }

fn commit_repo_all (
  repo    : &Repository,
  message : &str,
) -> Result<(), Box<dyn Error>> {
  stage_repo_all (repo) ?;
  let mut index : git2::Index =
    repo . index () ?;
  let tree_id : git2::Oid =
    index . write_tree () ?;
  let tree : git2::Tree =
    repo . find_tree (tree_id) ?;
  let sig : git2::Signature =
    repo . signature () ?;
  let parents : Vec<git2::Commit> =
    match repo . head () {
      Ok (head) => vec! [head . peel_to_commit () ?],
      Err (_)   => Vec::new () };
  let parent_refs : Vec<&git2::Commit> =
    parents . iter () . collect ();
  repo . commit (
    Some ("HEAD"),
    &sig, &sig,
    message,
    &tree,
    &parent_refs) ?;
  Ok (( )) }

fn configure_git_user (
  repo : &Repository,
) -> Result<(), Box<dyn Error>> {
  let mut config : git2::Config =
    repo . config () ?;
  config . set_str ("user.email", "test@test.com") ?;
  config . set_str ("user.name", "Test") ?;
  Ok (( )) }
