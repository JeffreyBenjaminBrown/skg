// The git fixture: a case's skgrepos, each made a gitrepo whose HEAD
// and worktree differ, with the config, search index, live graph and
// views a test of the graph straddle or the diff view runs against.
// Declared in 'tests/grouped_repos.rs', and in
// 'tests/grouped_overrides.rs' for the fixpoint in 'tests/idempotence.rs'.

use crate::git_fixture_helpers::{mk_test_tcp_stream_pair, setup_gitrepo_with_fixtures};

use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::dbs::init::create_empty_tantivy_index;
use skg::graph_straddle::types::GraphStraddle;
use skg::metadata_canon::{viewnode_id_atom_of, with_viewnode_id_atom, without_viewnode_ids};
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::serve::handlers::skgrepo_sets::handle_skgrepo_set_request;
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::test_utils::{
  cleanup_test_tantivy, copy_dir_all, extract_string_field_from_sexp,
  graph_handle_from_config, read_all_lp_messages, skg_env_from_parts,
  update_from_and_rerender_buffer_test};
use skg::test_utils::{multi_root_view, multi_root_view_with_skgrepo_set};
use skg::types::env::SkgEnv;
use skg::types::misc::{ID, SkgConfig, Skgrepo, SkgrepoName, TantivyIndex};
use skg::types::viewnode::Viewnode;
use skg::types::views_state::{OpenViews, ViewforestId};

use ego_tree::Tree;
use futures::executor::block_on;
use std::collections::{BTreeMap, HashMap};
use std::error::Error;
use std::fs;
use std::io::BufReader;
use std::net::TcpStream;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

pub struct Fixture {
  pub root        : TempDir,
  /// The owned skgrepos, in privacy order. Each is the folder of its
  /// name under 'root'.
  pub skgrepos    : Vec<String>,
  pub config      : SkgConfig,
  pub tantivy     : TantivyIndex,
  pub graph       : InRustGraphHandle,
  pub views_state : ViewsState,
}

/// Where a case keeps its owned skgrepos.
#[derive(Clone, Copy)]
pub enum OwnedSkgrepos<'a> {
  /// One gitrepo per skgrepo, in privacy order: at HEAD
  /// '<case>/<skgrepo>/head', with '<case>/<skgrepo>/worktree' in the
  /// worktree.
  Named (&'a [&'a str]),
  /// One skgrepo, main: at HEAD '<case>/head', with '<case>/worktree'
  /// in the worktree.
  OnlyMain,
}

/// The fixture of the case whose folder is 'case': its owned skgrepos,
/// then its foreign skgrepos 'foreign', in privacy order. A foreign
/// skgrepo '<case>/<skgrepo>' is made a gitrepo as an owned one is
/// when it has a 'head' folder, and is copied as a plain folder
/// otherwise.
pub fn git_fixture (
  case    : &Path,
  owned   : OwnedSkgrepos,
  foreign : &[&str],
) -> Result<Fixture, Box<dyn Error>> {
  let root : TempDir = TempDir::new () ?;
  let owned_skgrepos : Vec<(&str, PathBuf)> = match owned {
    OwnedSkgrepos::Named (names) => names . iter ()
      . map ( |name| ( *name, case . join (name) )) . collect (),
    OwnedSkgrepos::OnlyMain => vec! [ ( "main", case . to_path_buf () ) ], };
  let mut skgrepo_map : HashMap<SkgrepoName, Skgrepo> = HashMap::new ();
  for (name, fixtures, is_owned) in owned_skgrepos . iter ()
    . map ( |(name, fixtures)| ( *name, fixtures . clone (), true ))
    . chain ( foreign . iter ()
              . map ( |name| ( *name, case . join (name), false )) ) {
    let path : PathBuf = root . path () . join (name);
    if is_owned || fixtures . join ("head") . is_dir () {
      setup_gitrepo_with_fixtures (
        &path,
        fixtures . join ("head") . to_str () . unwrap (),
        fixtures . join ("worktree") . to_str () . unwrap () ) ?;
    } else { copy_dir_all (&fixtures, &path) ?; }
    skgrepo_map . insert ( SkgrepoName::from (name), Skgrepo {
      name : SkgrepoName::from (name), abbreviation : None,
      path, owned : is_owned } ); }
  let mut config : SkgConfig = SkgConfig::fromSkgreposAndTantivyFolder (
    skgrepo_map, root . path () . join ("tantivy") . to_str () . unwrap () );
  config . skgrepo_order = owned_skgrepos . iter ()
    . map ( |(name, _)| *name ) . chain ( foreign . iter () . copied () )
    . map (SkgrepoName::from) . collect ();
  config . data_root = root . path () . to_path_buf ();
  let tantivy : TantivyIndex = create_empty_tantivy_index (&config . tantivy_folder) ?;
  let graph : InRustGraphHandle = graph_handle_from_config (&config) ?;
  Ok ( Fixture {
    root,
    skgrepos : owned_skgrepos . iter ()
      . map ( |(name, _)| name . to_string () ) . collect (),
    config, tantivy, graph,
    views_state : ViewsState {
      open_views     : OpenViews::new (),
      graph_straddle : None, } } ) }

/// Run 'body' on the 'git_fixture', then remove its search index.
pub fn with_git_fixture (
  case    : &Path,
  owned   : OwnedSkgrepos,
  foreign : &[&str],
  body    : impl FnOnce (&mut Fixture) -> Result<(), Box<dyn Error>>,
) -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = git_fixture (case, owned, foreign) ?;
  let result : Result<(), Box<dyn Error>> = body (&mut f);
  cleanup_test_tantivy ( Some (&f . config . tantivy_folder) ) ?;
  result }

/// HEAD paired with the live graph.
pub fn graph_straddle (
  f : &Fixture,
) -> Result<GraphStraddle, Box<dyn Error>> {
  Ok ( GraphStraddle::from_head_and_live (&f . config, f . graph . load_full ()) ? ) }

/// Save 'text' as the view 'viewforest_id'.
pub fn save_view (
  f       : &mut Fixture,
  viewforest_id : &ViewforestId,
  text    : &str,
) -> Result<SaveResponse, Box<dyn Error>> {
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  Ok ( block_on ( update_from_and_rerender_buffer_test (
    &mut stream, text, &f . config, &f . tantivy, &f . graph,
    &Ok ( viewforest_id . clone () ), &mut f . views_state )) ? ) }

/// Save 'text' as the view 'viewforest_id'; it must save without errors.
pub fn save_cleanly (
  f       : &mut Fixture,
  viewforest_id : &ViewforestId,
  text    : &str,
) -> Result<SaveResponse, Box<dyn Error>> {
  let response : SaveResponse = save_view (f, viewforest_id, text) ?;
  assert! ( response . errors . is_empty (),
            "the buffer must save without errors, got {:?}\n\nbuffer:\n{}",
            response . errors, text );
  Ok (response) }

/// 'buffer' with its one line starting 'prefix' replaced by
/// 'replace (line)'. Both see the line without its viewnode ID, which
/// the replacement keeps.
pub fn edit_line (
  buffer  : &str,
  prefix  : &str,
  replace : impl Fn (&str) -> String,
) -> String {
  let matching : usize = buffer . lines ()
    . filter ( |line| without_viewnode_ids (line) . starts_with (prefix) ) . count ();
  assert_eq! ( matching, 1,
               "exactly one line should start {:?}:\n{}", prefix, buffer );
  buffer . lines ()
    . map ( |line| {
      let line_without_viewnode_id : String = without_viewnode_ids (line);
      if ! line_without_viewnode_id . starts_with (prefix) {
        return line . to_string (); }
      let replaced : String = replace (&line_without_viewnode_id);
      match viewnode_id_atom_of (line) {
        Some (viewnode_id_atom) => with_viewnode_id_atom (&replaced, viewnode_id_atom),
        None => replaced, } } )
    . collect::<Vec<String>> () . join ("\n") + "\n" }

/// Everything a handler wrote to 'writer', read from its peer
/// 'read_end': 'writer' is dropped first, so the read ends after the
/// last message. The messages are joined by newlines.
pub fn everything_written (
  writer   : TcpStream,
  read_end : TcpStream,
) -> String {
  drop (writer);
  read_all_lp_messages ( &mut BufReader::new (read_end) ) . join ("\n") }

/// Every '.skg' file of the fixture's owned skgrepos, by path, with
/// its contents.
pub fn skg_files (
  f : &Fixture,
) -> Result<BTreeMap<String, String>, Box<dyn Error>> {
  let mut files : BTreeMap<String, String> = BTreeMap::new ();
  for skgrepo in &f . skgrepos {
    for entry in fs::read_dir ( f . root . path () . join (skgrepo) ) ? {
      let path : PathBuf = entry ? . path ();
      if path . extension () . is_some_and ( |ext| ext == "skg" ) {
        files . insert (
          format! ( "{}/{}", skgrepo,
                    path . file_name () . unwrap () . to_string_lossy () ),
          fs::read_to_string (&path) ? ); }}}
  Ok (files) }

//
// Fixtures of two skgrepos, and a content view under a restriction
//

/// Write each skgrepo's files, by name without '.skg', to both its
/// 'head' and its 'worktree' under 'dir', so that they agree.
pub fn write_unchanged_skgrepos (
  dir      : &Path,
  skgrepos : &[(&str, &[(&str, &str)])],
) -> Result<(), Box<dyn Error>> {
  for (skgrepo, files) in skgrepos {
    for side in ["head", "worktree"] {
      let folder : PathBuf = dir . join (skgrepo) . join (side);
      fs::create_dir_all (&folder) ?;
      for (name, text) in *files {
        fs::write ( folder . join (format! ("{}.skg", name)), text ) ?; }}}
  Ok (( )) }

/// Draw a content view of 'root', under 'restriction' when one is
/// given, and register it as 'view'.
pub fn open_content_view (
  f           : &mut Fixture,
  root        : &str,
  view        : &str,
  restriction : Option<&SkgrepoRestriction>,
) -> Result<String, Box<dyn Error>> {
  let roots : [ID; 1] = [ ID::from (root) ];
  let (text, pids, tree) : (String, Vec<ID>, Tree<Viewnode>) =
    match restriction {
      Some (restriction) => multi_root_view_with_skgrepo_set (
        &f . config, Some (&f . tantivy), &roots, restriction ) ?,
      None => multi_root_view (&f . config, Some (&f . tantivy), &roots) ?, };
  f . views_state . open_views . register_view (
    &f . graph . load_full (), ViewforestId::ContentView (view . into ()),
    tree, &pids );
  Ok (text) }

/// Switch to the skgrepo-set 'name'; return 'view' as rerendered,
/// and the "rerender-done" message, which carries the buffer
/// corrections.
pub fn switch_skgrepo_set (
  f           : &mut Fixture,
  restriction : &mut SkgrepoRestriction,
  name        : &str,
  view        : &str,
) -> Result<(String, String), Box<dyn Error>> {
  let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
  let (mut stream, read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  handle_skgrepo_set_request (
    &mut stream,
    &format! ("((request . \"set skgrepo restriction\") (name . \"{}\"))\n", name),
    &env, &mut f . views_state, restriction );
  drop (stream);
  let messages : Vec<String> = read_all_lp_messages ( &mut BufReader::new (read_end) );
  let rerendered : Option<String> = messages . iter ()
    . find ( |message| message . contains ("rerender-view")
                       && message . contains (&format! ("(view-id \"{}\")", view)) )
    . and_then ( |message| extract_string_field_from_sexp (
      &message [ message . find ('(') ? .. ], "content" ));
  let done : String = messages . iter ()
    . find ( |message| message . contains ("rerender-done") )
    . cloned ()
    . unwrap_or_else ( || panic! ( "the switch to {} ends: {:?}", name, messages ));
  Ok (( rerendered . unwrap_or_else (
          || panic! ( "the switch to {} rerenders the view: {:?}", name, messages )),
        done )) }
