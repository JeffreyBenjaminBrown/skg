// View and save helpers for the tests of the forks folder's members
// as such: each test saves a buffer as a view of one root, reads a
// fresh view, and finds headlines by their gnid.
// Declared in 'tests/grouped_forks.rs', 'tests/grouped_saves.rs' and
// 'tests/grouped_views.rs'.

use crate::tcp_stream_pair::mk_test_tcp_stream_pair;
use std::collections::BTreeMap;
use std::error::Error;
use std::net::TcpStream;

use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_skgrepos;
use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::dbs::node_lookup::graphnode_by_gnid;
use skg::metadata_canon::without_viewnode_ids;
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::test_utils::{graph_handle_from_config, single_root_view};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::types::errors::SaveError;
use skg::types::config::SkgConfig;
use skg::types::misc::{ID, TantivyIndex, members_of};
use skg::types::nodes::complete::Graphnode;
use skg::types::save::format_save_error_as_org;
use skg::types::views_state::{OpenViews, ViewforestId};

/// Save 'buf' as a view of 'root'.
pub async fn save (
  buf     : &str,
  root    : &str,
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<SaveResponse, Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = ViewsState {
    open_views     : OpenViews::new (),
    graph_straddle : None, };
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  update_from_and_rerender_buffer (
    &mut stream, buf, config, tantivy, &graph,
    &Ok ( ViewforestId::ContentView ( format! ("view of {}", root) )),
    &mut views_state ) . await }

/// A fresh view of 'root', without viewnode IDs.
pub fn view_of (
  config  : &SkgConfig,
  tantivy : &TantivyIndex,
  root    : &str,
) -> Result<String, Box<dyn Error>> {
  let (view, _, _) = single_root_view (config, Some (tantivy), &ID::from (root)) ?;
  Ok ( without_viewnode_ids (&view) ) }

/// Save 'buf', which must save cleanly; the saved view, without
/// viewnode IDs.
pub async fn save_cleanly (
  buf     : &str,
  root    : &str,
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<String, Box<dyn Error>> {
  let response : SaveResponse = save (buf, root, config, tantivy) . await ?;
  assert! ( response . errors . is_empty (), "save errors: {:?}", response . errors );
  Ok ( without_viewnode_ids (&response . saved_view) ) }

/// Save 'view' unchanged as a view of 'root': no file is written, and
/// the saved view is 'view' again.
pub async fn is_a_fixpoint (
  view    : &str,
  root    : &str,
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let before : BTreeMap<String, String> = skg_files (config) ?;
  let saved : String = save_cleanly (view, root, config, tantivy) . await ?;
  assert_eq! ( skg_files (config) ?, before, "an unchanged save writes nothing" );
  assert_eq! ( saved, view, "and draws the view again as it was" );
  Ok (( )) }

/// What the save of 'buf' says, which must refuse it.
pub async fn refusal_of (
  buf     : &str,
  root    : &str,
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<String, Box<dyn Error>> {
  Ok ( match save (buf, root, config, tantivy) . await {
    Ok (response) => {
      assert! ( ! response . errors . is_empty (),
                "the save must be refused; it saved:\n{}", response . saved_view );
      format! ("{:?}", response . errors) },
    Err (error) => match error . downcast_ref::<SaveError> () {
      Some (save_error) => format_save_error_as_org (save_error),
      None => error . to_string (), }, } ) }

/// The first line showing 'gnid'.
pub fn line_of<'a> (
  buf  : &'a str,
  gnid : &str,
) -> &'a str {
  buf . lines ()
    . find ( |line| line . contains (&format! ("(gnid {})", gnid)) )
    . unwrap_or_else ( || panic! ( "no line shows {} in:\n{}", gnid, buf )) }

pub fn depth (
  line : &str,
) -> usize {
  line . chars () . take_while ( |c| *c == '*' ) . count () }

/// The lines under the first line showing 'gnid'.
pub fn under<'a> (
  buf  : &'a str,
  gnid : &str,
) -> Vec<&'a str> {
  let lines : Vec<&str> = buf . lines () . collect ();
  let at : usize = lines . iter ()
    . position ( |line| line . contains (&format! ("(gnid {})", gnid)) )
    . unwrap_or_else ( || panic! ( "no line shows {} in:\n{}", gnid, buf ));
  lines [at + 1 ..] . iter () . copied ()
    . take_while ( |line| depth (line) == 0 || depth (line) > depth (lines [at]) )
    . collect () }

/// The child headlines (one level below) of the first line showing
/// 'gnid'.
pub fn children_of<'a> (
  buf  : &'a str,
  gnid : &str,
) -> Vec<&'a str> {
  let parent_depth : usize = depth (line_of (buf, gnid));
  under (buf, gnid) . into_iter ()
    . filter ( |line| depth (line) == parent_depth + 1 )
    . collect () }

/// The content (node) children of the first line showing 'gnid'.
pub fn content_of<'a> (
  buf  : &'a str,
  gnid : &str,
) -> Vec<&'a str> {
  children_of (buf, gnid) . into_iter ()
    . filter ( |line| line . contains ("(skg node ") || line . contains ("(skg folded node ") )
    . collect () }

/// 'buf' less the first line showing 'gnid' and what stands under it.
pub fn without_subtree_of (
  buf  : &str,
  gnid : &str,
) -> String {
  let head : &str = line_of (buf, gnid);
  let below : Vec<&str> = under (buf, gnid);
  let mut removed : String = format! ("{}\n", head);
  for line in below { removed . push_str (&format! ("{}\n", line)); }
  buf . replacen (&removed, "", 1) }

/// 'buf' with the line 'typed' right after the first line showing
/// 'gnid' and what stands under it.
pub fn with_line_after_subtree_of (
  buf   : &str,
  gnid  : &str,
  typed : &str,
) -> String {
  let head : &str = line_of (buf, gnid);
  let below : Vec<&str> = under (buf, gnid);
  let mut subtree : String = format! ("{}\n", head);
  for line in below { subtree . push_str (&format! ("{}\n", line)); }
  buf . replacen (&subtree, &format! ("{}{}\n", subtree, typed), 1) }

/// 'view' with the first line showing 'moved' (a leaf) cut, and
/// pasted right after the first line showing 'sibling' and what
/// stands under it, at that line's depth.
pub fn moved_after (
  view    : &str,
  moved   : &str,
  sibling : &str,
) -> String {
  let moved_line : &str = line_of (view, moved);
  assert! ( under (view, moved) . is_empty (), "{} is a leaf:\n{}", moved, view );
  let stars : String = "*" . repeat ( depth (line_of (view, sibling)) );
  with_line_after_subtree_of (
    &without_subtree_of (view, moved), sibling,
    &format! ( "{} {}", stars, moved_line . trim_start_matches ('*') . trim_start () )) }

/// The gnids of the members of the first forks folder in 'buf', in
/// order.
pub fn first_forks_folders_members (
  buf : &str,
) -> Vec<String> {
  let lines : Vec<&str> = buf . lines () . collect ();
  let at : usize = lines . iter ()
    . position ( |line| line . contains ("forksFolder") )
    . unwrap_or_else ( || panic! ( "no forks folder in:\n{}", buf ));
  lines [at + 1 ..] . iter ()
    . take_while ( |line| depth (line) > depth (lines [at]) )
    . filter ( |line| depth (line) == depth (lines [at]) + 1 )
    . filter_map ( |line| line . split ("(gnid ") . nth (1) )
    . map ( |rest| rest . split (')') . next () . unwrap () . to_string () )
    . collect () }

pub fn node (
  config : &SkgConfig,
  pid    : &str,
) -> Result<Graphnode, Box<dyn Error>> {
  graphnode_by_gnid (
    &graph_handle_from_config (config) ? . load_full (), &ID::from (pid) ) }

/// The node 'pid' as the files on disk have it.
pub fn node_from_disk (
  config : &SkgConfig,
  pid    : &str,
) -> Result<Graphnode, Box<dyn Error>> {
  let gnid : ID = ID::from (pid);
  read_all_skg_files_from_skgrepos (config) ?
    . into_iter ()
    . find ( |node| node . pid == gnid )
    . ok_or_else ( || format! ("node not found on disk: {}", pid) . into () ) }

pub fn hides_of (
  config : &SkgConfig,
  pid    : &str,
) -> Result<Vec<ID>, Box<dyn Error>> {
  Ok ( members_of ( node (config, pid) ? . hides . or_default () )) }

pub fn forks_of (
  config : &SkgConfig,
  pid    : &str,
) -> Result<Vec<ID>, Box<dyn Error>> {
  Ok ( members_of ( node (config, pid) ? . forks . or_default () )) }

pub fn contains_of (
  config : &SkgConfig,
  pid    : &str,
) -> Result<Vec<ID>, Box<dyn Error>> {
  Ok ( members_of ( &node (config, pid) ? . contains )) }

pub fn ids (
  pids : &[&str],
) -> Vec<ID> {
  pids . iter () . map ( |pid| ID::from (*pid) ) . collect () }

/// Every .skg file of the fixture, by skgrepo and name.
pub fn skg_files (
  config : &SkgConfig,
) -> Result<BTreeMap<String, String>, Box<dyn Error>> {
  let mut files : BTreeMap<String, String> = BTreeMap::new ();
  for (name, skgrepo) in &config . skgrepos {
    for entry in std::fs::read_dir (&skgrepo . path) ? {
      let path : std::path::PathBuf = entry ? . path ();
      if path . extension () . is_some_and ( |extension| extension == "skg" ) {
        files . insert (
          format! ( "{}/{}", name, path . file_name () . unwrap () . to_string_lossy () ),
          std::fs::read_to_string (&path) ? ); }}}
  Ok (files) }
