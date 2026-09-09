use skg::serve::handlers::titles_by_ids::{
  add_deleted_node_titles_by_ids,
  handle_titles_by_ids_request_with_source_set};
use skg::source_sets::ActiveSourceSet;
use skg::dbs::in_rust_graph::InRustGraph;
use skg::test_utils::read_lp_message;
use skg::types::git::SourceDiff;
use skg::types::misc::{ID, MSV, SkgConfig, SkgfileSource, SourceName, SourceSetName, members_at_source_msv};
use skg::types::nodes::complete::{empty_node_complete, NodeComplete};

use std::collections::{BTreeSet, HashMap};
use std::error::Error;
use std::fs;
use std::net::{TcpListener, TcpStream};
use std::path::Path;
use tempfile::TempDir;

#[test]
fn titles_by_ids_handler_sends_parseable_titles (
) -> Result<(), Box<dyn Error>> {
  let mut node : NodeComplete =
    empty_node_complete ();
  node . pid =
    ID::new ("11111111-1111-4111-8111-111111111111");
  node . title =
    "?" . to_string ();
  node . source =
    SourceName::from ("main");
  node . aliases =
    members_at_source_msv ( & node . source, MSV::Specified (vec!["Alias One" . to_string ()]) );
  let mut spaced_title_node : NodeComplete =
    empty_node_complete ();
  spaced_title_node . pid =
    ID::new ("44444444-4444-4444-8444-444444444444");
  spaced_title_node . title =
    "The Real Title, with spaces" . to_string ();
  spaced_title_node . source =
    SourceName::from ("main");
  let graph : InRustGraph = InRustGraph::from_nodecompletes (
    &[node, spaced_title_node]);
  let config : SkgConfig = SkgConfig::dummyFromSources (HashMap::new ());
  let active : ActiveSourceSet = ActiveSourceSet::named (
    &config, SourceSetName::from ("all"))?;
  let listener : TcpListener =
    TcpListener::bind ("127.0.0.1:0")?;
  let addr =
    listener . local_addr ()?;
  let client : TcpStream =
    TcpStream::connect (addr)?;
  let (mut server, _peer) =
    listener . accept ()?;
  let request : &str =
    "((request . \"titles by ids\") \
      (ids \"11111111-1111-4111-8111-111111111111\" \
           \"44444444-4444-4444-8444-444444444444\"))";
  handle_titles_by_ids_request_with_source_set (
    &mut server, request, &config, &active, &graph);
  drop (server);
  let mut reader =
    std::io::BufReader::new (client);
  let response : String =
    read_lp_message (&mut reader)?;
  let parsed : sexp::Sexp =
    sexp::parse (&response)?;
  let response_text : String =
    parsed . to_string ();
  assert! (
    response_text . contains ("titles-by-ids"),
    "response should be tagged as titles-by-ids: {}",
    response_text );
  assert! (
    response . contains ("\"?\""),
    "response should quote titles that are not valid Emacs symbols: {}",
    response );
  assert! (
    response_text . contains ("?"),
    "response should preserve the title: {}",
    response_text );
  assert! (
    response_text . contains ("The Real Title, with spaces"),
    "response should preserve titles with spaces: {}",
    response_text );
  Ok (( )) }

#[test]
fn restricted_title_lookup_challenges_without_releasing_text (
) -> Result<(), Box<dyn Error>> {
  let source : SourceName = SourceName::from ("main");
  let mut node : NodeComplete = empty_node_complete ();
  node . pid = ID::new ("ugly-title-id");
  node . source = source . clone ();
  node . title = "UNIQUE TITLE SECRET" . to_string ();
  node . ugly_telescope = true;
  let graph : InRustGraph =
    InRustGraph::from_nodecompletes (&[node . clone ()]);
  let config : SkgConfig = SkgConfig::dummyFromSources (HashMap::from ([
    (source . clone (), SkgfileSource {
      name         : source . clone (),
      abbreviation : None,
      path         : Path::new ("/tmp") . to_path_buf (),
      user_owns_it : true,
    })
  ]));
  let active = ActiveSourceSet {
    name    : SourceSetName::from ("public"),
    sources : BTreeSet::from ([source]),
  };
  let respond = |request : &str| -> Result<String, Box<dyn Error>> {
    let listener : TcpListener = TcpListener::bind ("127.0.0.1:0")?;
    let client : TcpStream = TcpStream::connect (listener . local_addr ()?)?;
    let (mut server, _) = listener . accept ()?;
    handle_titles_by_ids_request_with_source_set (
      &mut server, request, &config, &active, &graph );
    drop (server);
    Ok (read_lp_message (&mut std::io::BufReader::new (client))?)
  };
  let challenged : String = respond (
    "((request . \"titles by ids\") (ids \"ugly-title-id\"))" )?;
  assert! ( challenged . contains ("ugly-telescope-confirmation") );
  assert! ( ! challenged . contains ("UNIQUE TITLE SECRET") );
  let approved : String = respond (
    "((request . \"titles by ids\") (ids \"ugly-title-id\") \
      (allow-ugly-telescopes \"ugly-title-id\"))" )?;
  assert! ( approved . contains ("UNIQUE TITLE SECRET") );
  assert! ( approved . contains ("selected below") );
  Ok (( ))
}

#[test]
fn deleted_titles_supplement_tantivy_title_map (
) -> Result<(), Box<dyn Error>> {
  let pid : ID =
    ID::new ("22222222-2222-4222-8222-222222222222");
  let extra_id : ID =
    ID::new ("33333333-3333-4333-8333-333333333333");
  let mut deleted_node : NodeComplete =
    empty_node_complete ();
  deleted_node . pid =
    pid . clone ();
  deleted_node . extra_ids =
    vec! [extra_id . clone ()];
  deleted_node . title =
    "Deleted Title" . to_string ();
  let mut source_diff : SourceDiff =
    SourceDiff::new_not_git_repo ();
  source_diff . deleted_nodes . insert (
    pid . clone (), deleted_node );
  let source_diffs : HashMap<SourceName, SourceDiff> =
    HashMap::from ([
      (SourceName::from ("main"), source_diff) ]);
  let mut title_map : HashMap<ID, String> =
    HashMap::new ();
  add_deleted_node_titles_by_ids (
    &mut title_map,
    &[pid . clone (), extra_id . clone ()],
    &source_diffs );
  assert_eq! (
    title_map . get (&pid),
    Some (&"Deleted Title" . to_string ()));
  assert_eq! (
    title_map . get (&extra_id),
    Some (&"Deleted Title" . to_string ()));
  Ok (( )) }

#[test]
fn titles_by_ids_finds_deleted_git_file_title_without_diff_mode (
) -> Result<(), Box<dyn Error>> {
  let id : ID =
    ID::new ("a782564f-029a-45d7-b96e-1986d8759924");
  let source_name : SourceName =
    SourceName::from ("main");
  let temp_dir : TempDir =
    TempDir::new ()?;
  let source_dir : &Path =
    temp_dir . path ();
  let repo : git2::Repository =
    git2::Repository::init (source_dir)?;
  configure_git_user (&repo)?;
  fs::write (
    source_dir . join (format! ("{}.skg", id . 0)),
    format! ("title: Deleted From Git\npid: {}\n", id . 0))?;
  commit_all (&repo, "initial commit")?;
  fs::remove_file (
    source_dir . join (format! ("{}.skg", id . 0)))?;
  let config : SkgConfig =
    SkgConfig::dummyFromSources (HashMap::from ([
      (source_name . clone (),
       SkgfileSource {
         name          : source_name,
         abbreviation  : None,
         path          : source_dir . to_path_buf (),
         user_owns_it  : true }) ]));
  let listener : TcpListener =
    TcpListener::bind ("127.0.0.1:0")?;
  let addr =
    listener . local_addr ()?;
  let client : TcpStream =
    TcpStream::connect (addr)?;
  let (mut server, _peer) =
    listener . accept ()?;
  let request : String =
    format! (
      "((request . \"titles by ids\") (git-evidence . \"true\") (ids \"{}\"))",
      id . 0);
  let active : ActiveSourceSet = ActiveSourceSet::named (
    &config, SourceSetName::from ("all"))?;
  handle_titles_by_ids_request_with_source_set (
    &mut server, &request, &config, &active, &InRustGraph::new ());
  drop (server);
  let mut reader =
    std::io::BufReader::new (client);
  let response : String =
    read_lp_message (&mut reader)?;
  assert! (
    response . contains ("Deleted From Git"),
    "deleted file title should be returned: {}",
    response );
  Ok (( )) }

#[test]
fn titles_by_ids_finds_untracked_git_file_title_without_diff_mode (
) -> Result<(), Box<dyn Error>> {
  let id : ID =
    ID::new ("abf5bac1-de1c-4026-868a-60a51a5a3176");
  let source_name : SourceName =
    SourceName::from ("main");
  let temp_dir : TempDir =
    TempDir::new ()?;
  let source_dir : &Path =
    temp_dir . path ();
  let repo : git2::Repository =
    git2::Repository::init (source_dir)?;
  configure_git_user (&repo)?;
  fs::write (source_dir . join ("README.md"), "initial\n")?;
  commit_all (&repo, "initial commit")?;
  fs::write (
    source_dir . join (format! ("{}.skg", id . 0)),
    format! ("title: Untracked Title\npid: {}\n", id . 0))?;
  let config : SkgConfig =
    SkgConfig::dummyFromSources (HashMap::from ([
      (source_name . clone (),
       SkgfileSource {
         name          : source_name,
         abbreviation  : None,
         path          : source_dir . to_path_buf (),
         user_owns_it  : true }) ]));
  let listener : TcpListener =
    TcpListener::bind ("127.0.0.1:0")?;
  let addr =
    listener . local_addr ()?;
  let client : TcpStream =
    TcpStream::connect (addr)?;
  let (mut server, _peer) =
    listener . accept ()?;
  let request : String =
    format! (
      "((request . \"titles by ids\") (git-evidence . \"true\") (ids \"{}\"))",
      id . 0);
  let active : ActiveSourceSet = ActiveSourceSet::named (
    &config, SourceSetName::from ("all"))?;
  handle_titles_by_ids_request_with_source_set (
    &mut server, &request, &config, &active, &InRustGraph::new ());
  drop (server);
  let mut reader =
    std::io::BufReader::new (client);
  let response : String =
    read_lp_message (&mut reader)?;
  assert! (
    response . contains ("Untracked Title"),
    "untracked file title should be returned: {}",
    response );
  Ok (( )) }

fn configure_git_user (
  repo : &git2::Repository,
) -> Result<(), Box<dyn Error>> {
  let mut config : git2::Config =
    repo . config ()?;
  config . set_str ("user.email", "test@test.com")?;
  config . set_str ("user.name", "Test")?;
  Ok (( )) }

fn commit_all (
  repo    : &git2::Repository,
  message : &str,
) -> Result<(), Box<dyn Error>> {
  let mut index : git2::Index =
    repo . index ()?;
  index . add_all (["*.skg"], git2::IndexAddOption::DEFAULT, None)?;
  index . write ()?;
  let tree_id : git2::Oid =
    index . write_tree ()?;
  let tree : git2::Tree =
    repo . find_tree (tree_id)?;
  let sig : git2::Signature =
    repo . signature ()?;
  let parents : Vec<git2::Commit> =
    match repo . head () {
      Ok (head) => vec! [head . peel_to_commit ()?],
      Err (_)   => Vec::new () };
  let parent_refs : Vec<&git2::Commit> =
    parents . iter () . collect ();
  repo . commit (
    Some ("HEAD"),
    &sig, &sig,
    message,
    &tree,
    &parent_refs)?;
  Ok (( )) }

#[test]
fn ordinary_title_lookup_preserves_selected_absence_and_text (
) -> Result<(), Box<dyn Error>> {
  let directory : TempDir = TempDir::new ()?;
  let source : SourceName = SourceName::from ("main");
  let mut node : NodeComplete = empty_node_complete ();
  node . pid = ID::new ("selected");
  node . source = source . clone ();
  node . title = "Selected title" . into ();
  let graph : InRustGraph = InRustGraph::from_nodecompletes (&[node]);
  fs::write (directory . path () . join ("selected.skg"),
    "pid: selected\ntitle: Newer disk title\n")?;
  fs::write (directory . path () . join ("newcomer.skg"),
    "pid: newcomer\ntitle: Unselected disk secret\n")?;
  let config : SkgConfig = SkgConfig::dummyFromSources (HashMap::from ([
    (source . clone (), SkgfileSource {
      name: source, abbreviation: None,
      path: directory . path () . to_path_buf (), user_owns_it: true,
    })]));
  let active : ActiveSourceSet = ActiveSourceSet::named (
    &config, SourceSetName::from ("all"))?;
  let listener : TcpListener = TcpListener::bind ("127.0.0.1:0")?;
  let client : TcpStream = TcpStream::connect (listener . local_addr ()?)?;
  let (mut server, _) = listener . accept ()?;
  handle_titles_by_ids_request_with_source_set (
    &mut server,
    "((request . \"titles by ids\") (ids \"selected\" \"newcomer\"))",
    &config, &active, &graph);
  drop (server);
  let response : String = read_lp_message (&mut std::io::BufReader::new (client))?;
  assert! (response . contains ("Selected title"));
  assert! (!response . contains ("Newer disk title"));
  assert! (!response . contains ("Unselected disk secret"));
  assert! (!response . contains ("newcomer"));
  Ok (()) }
