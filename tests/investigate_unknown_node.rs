// The "investigate unknown node" request
// ('TODO/total-diff-view/02_plan.org', "Investigating an unknown
// node"). One gitrepo: p contains v, ghost and w; v, which contains w
// and links to it, is deleted in a second commit; ghost never existed.

use skg::dbs::init::empty_in_ram_tantivy_index;
use skg::serve::handlers::investigate_unknown_node::handle_investigate_unknown_node_request;
use skg::skgrepo_sets::{SkgrepoRestriction, SkgrepoSetName};
use skg::test_utils::{graph_handle_from_config, read_lp_message, skg_env_from_parts};
use skg::types::env::{RuntimeGeneration, SkgEnv};
use skg::types::misc::{SkgConfig, Skgrepo, SkgrepoName, TantivyIndex};

use git2::Repository;
use std::collections::HashMap;
use std::error::Error;
use std::fs;
use std::io::BufReader;
use std::net::{TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tempfile::TempDir;

struct Fixture {
  _root   : TempDir,
  config  : SkgConfig,
}

fn write_node (
  skgrepo  : &Path,
  pid      : &str,
  title    : &str,
  body     : &str,
  contains : &[&str],
) -> Result<(), Box<dyn Error>> {
  let contains_yaml : String =
    if contains . is_empty () { String::new () }
    else { format! ( "contains:\n{}\n",
                     contains . iter () . map ( |id| format! ("- {}", id) )
                     . collect::<Vec<String>> () . join ("\n") ) };
  let body_yaml : String =
    if body . is_empty () { String::new () }
    else { format! ("body: {}\n", body) };
  fs::write ( skgrepo . join ( format! ("{}.skg", pid) ),
              format! ( "title: {}\npid: {}\n{}{}",
                        title, pid, body_yaml, contains_yaml )) ?;
  Ok (( )) }

/// Stage every change in the gitrepo, deletions included, and commit it.
fn commit_everything (
  gitrepo : &Repository,
  message : &str,
) -> Result<(), Box<dyn Error>> {
  let mut index : git2::Index = gitrepo . index () ?;
  index . add_all ( ["*"] . iter (), git2::IndexAddOption::DEFAULT, None ) ?;
  index . update_all ( ["*"] . iter (), None ) ?;
  index . write () ?;
  let tree : git2::Tree = gitrepo . find_tree ( index . write_tree () ? ) ?;
  let sig : git2::Signature = gitrepo . signature () ?;
  let parents : Vec<git2::Commit> = match gitrepo . head () {
    Ok (head) => vec! [ head . peel_to_commit () ? ],
    Err (_)   => Vec::new () };
  let parent_refs : Vec<&git2::Commit> = parents . iter () . collect ();
  gitrepo . commit ( Some ("HEAD"), &sig, &sig, message, &tree, &parent_refs ) ?;
  Ok (( )) }

fn fixture (
) -> Result<Fixture, Box<dyn Error>> {
  let root : TempDir = TempDir::new () ?;
  let skgrepo : PathBuf = root . path () . join ("main");
  fs::create_dir (&skgrepo) ?;
  let gitrepo : Repository = Repository::init (&skgrepo) ?;
  { let mut git_config : git2::Config = gitrepo . config () ?;
    git_config . set_str ("user.email", "test@test.com") ?;
    git_config . set_str ("user.name", "Test") ?; }
  write_node (&skgrepo, "p", "Parent", "", &["v", "ghost", "w"]) ?;
  write_node (&skgrepo, "v", "Vanishing", "links [[id:w][kept]]", &["w"]) ?;
  write_node (&skgrepo, "w", "Kept", "", &[]) ?;
  commit_everything (&gitrepo, "initial") ?;
  fs::remove_file ( skgrepo . join ("v.skg") ) ?;
  commit_everything (&gitrepo, "delete v") ?;
  let name : SkgrepoName = SkgrepoName::from ("main");
  let config : SkgConfig = SkgConfig::dummyFromSkgrepos ( HashMap::from ([
    ( name . clone (),
      Skgrepo { name, abbreviation : None, path : skgrepo, owned : true } ) ]));
  Ok ( Fixture { _root : root, config } ) }

/// The investigation of 'id' under the skgrepo restriction named
/// 'restriction': the response's content, and whether it reported
/// errors.
fn investigate (
  f           : &Fixture,
  id          : &str,
  restriction : &str,
) -> Result<(String, bool), Box<dyn Error>> {
  let tantivy : TantivyIndex = empty_in_ram_tantivy_index () ?;
  let env : SkgEnv = skg_env_from_parts (
    &f . config, &tantivy, &graph_handle_from_config (&f . config) ? );
  let runtime : Arc<RuntimeGeneration> = env . runtime_snapshot ();
  let restriction : SkgrepoRestriction = {
    let mut all : SkgrepoRestriction =
      SkgrepoRestriction::named ( &f . config, SkgrepoSetName::from ("all") ) ?;
    all . name = SkgrepoSetName::from (restriction);
    all };
  let listener : TcpListener = TcpListener::bind ("127.0.0.1:0") ?;
  let client : TcpStream = TcpStream::connect ( listener . local_addr () ? ) ?;
  let (mut server, _peer) = listener . accept () ?;
  handle_investigate_unknown_node_request (
    &mut server,
    &format! ( "((request . \"investigate unknown node\") (id . \"{}\"))", id ),
    &runtime, &restriction );
  drop (server);
  let response : String = read_lp_message ( &mut BufReader::new (client) ) ?;
  assert! ( response . starts_with ("((response-type investigate-unknown-node)"),
            "unexpected response: {}", response );
  let content : String =
    match sexp::parse (&response) ? {
      sexp::Sexp::List (items) => items . iter () . find_map ( |item| match item {
        sexp::Sexp::List (pair) if pair . len () == 2 => match (&pair [0], &pair [1]) {
          ( sexp::Sexp::Atom (sexp::Atom::S (k)), sexp::Sexp::Atom (sexp::Atom::S (v)) )
            if k == "content" => Some (v . clone ()),
          _ => None },
        _ => None } ),
      _ => None }
    . ok_or ("the response has no content") ?;
  Ok (( content, ! response . contains ("(errors ())") )) }

/// A node deleted in a later commit is reported with its title then,
/// the commit it vanished at, its own relation lists then, and every
/// node that referred to it then.
#[test]
fn a_deleted_node_is_reported_from_history (
) -> Result<(), Box<dyn Error>> {
  let f : Fixture = fixture () ?;
  let (content, errors) : (String, bool) = investigate (&f, "v", "all") ?;
  assert! ( ! errors, "no error expected:\n{}", content );
  assert! ( content . starts_with ("* v\n"), "the ID heads the answer:\n{}", content );
  assert! ( content . contains ("*** title when last present: Vanishing"),
            "v's old title must be shown:\n{}", content );
  assert! ( content . contains ("*** vanished at commit")
            && content . contains ("delete v"),
            "the vanishing commit must be named:\n{}", content );
  assert! ( content . contains ("**** contains\n***** w"),
            "v's own old contains must be shown:\n{}", content );
  assert! ( content . contains ("**** p (via contains)"),
            "p's old reference to v must be shown:\n{}", content );
  Ok (( )) }

#[test]
fn a_never_committed_id_has_no_sightings (
) -> Result<(), Box<dyn Error>> {
  let f : Fixture = fixture () ?;
  let (content, errors) : (String, bool) = investigate (&f, "ghost", "all") ?;
  assert! ( ! errors, "no error expected:\n{}", content );
  assert_eq! ( content,
               "* ghost\n** never present in the git history of any skgrepo\n" );
  Ok (( )) }

#[test]
fn a_resolvable_id_reports_its_node (
) -> Result<(), Box<dyn Error>> {
  let f : Fixture = fixture () ?;
  let (content, errors) : (String, bool) = investigate (&f, "w", "all") ?;
  assert! ( ! errors, "no error expected:\n{}", content );
  assert_eq! ( content, "w resolves to node Kept (w) in main\n" );
  Ok (( )) }

/// History can hold the text of any skgrepo, so the request is
/// refused under a restriction.
#[test]
fn the_request_is_refused_under_a_skgrepo_restriction (
) -> Result<(), Box<dyn Error>> {
  let f : Fixture = fixture () ?;
  let (content, errors) : (String, bool) = investigate (&f, "v", "some") ?;
  assert! ( errors, "the refusal must be an error:\n{}", content );
  assert! ( ! content . contains ("Vanishing"),
            "a refusal must reveal nothing:\n{}", content );
  Ok (( )) }
