// cargo nextest run --test grouped_overrides -E 'test(extend_partner_folder_member::)'
//
// Default folders on ordinary editable nodes and on editable
// partner folder members.  The initial presentation contains only a
// forksFolder, where the node has a fork partner.  The other relation
// folders require explicit requests; after one of their members is
// extended as editable, that member gets the same default forksFolder.
// A forkee as such additionally gets its nonempty
// hiddenInForkPartnerFolder.

use std::error::Error;
use crate::tcp_stream_pair::mk_test_tcp_stream_pair;
use std::net::TcpStream;

use skg::dbs::in_rust_graph::{
  InRustGraphHandle};
use skg::test_utils::{run_with_test_stores, graph_handle_from_config};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::test_utils::multi_root_view;
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::types::views_state::OpenViews;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};
use ego_tree::Tree;
use skg::types::viewnode::Viewnode;
use skg::metadata_canon::without_viewnode_ids;

async fn save (
  buf     : &str,
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
  graph   : &InRustGraphHandle,
) -> Result<SaveResponse, Box<dyn Error>> {
  let mut views_state : ViewsState = ViewsState {
    open_views        : OpenViews::new (),
    graph_straddle    : None, };
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  update_from_and_rerender_buffer (
    &mut stream, buf, config, tantivy, graph,
    &Err ( String::new () ), &mut views_state ) . await }

fn subtree_for_node_in_folder (
  buf : &str,
  gnid : &str,
  enclosing_folder : &str,
) -> String {
  let needle : String = format! ("(gnid {})", gnid);
  let lines : Vec<&str> = buf . lines () . collect ();
  let mut folder_stack : Vec<(usize, &str)> = Vec::new ();
  let mut matches : Vec<usize> = Vec::new ();
  for (i, line) in lines . iter () . enumerate () {
    let depth : usize = line . chars ()
      . take_while ( |c| *c == '*' ) . count ();
    if depth > 0 {
      while folder_stack . last ()
        .map_or (false, |(folder_depth, _)| *folder_depth >= depth)
      { folder_stack . pop (); }}
    if line . contains (&needle)
       && folder_stack . last ()
            .map ( |(_, folder)| *folder == enclosing_folder )
            .unwrap_or (false)
    { matches . push (i); }
    if let Some (folder) = [
      "forksFolder",
      "hiderFolder", "hiddenFolder",
      "hiddenInForkPartnerFolder", "hiddenOutsideOfForkPartnersFolder",
    ] . into_iter () . find ( |folder| line . contains (folder) )
    { folder_stack . push ((depth, folder)); }}
  assert_eq! ( matches . len (), 1,
    "expected one occurrence of {} directly within {} in:\n{}",
    needle, enclosing_folder, buf );
  let start : usize = matches [0];
  let depth : usize = lines [start] . chars ()
    . take_while ( |c| *c == '*' ) . count ();
  lines [start ..] . iter ()
    . take (1)
    .chain ( lines [start + 1 ..] . iter () . take_while ( |line| {
      let child_depth : usize = line . chars ()
        . take_while ( |c| *c == '*' ) . count ();
      child_depth == 0 || child_depth > depth } ) )
    . map ( |line| *line )
    . collect::<Vec<&str>> () . join ("\n")
}

fn add_editable_view_requests_to_folder_members (
  buf : &str,
  members : &[(&str, &str)],
) -> String {
  let mut folder_stack : Vec<(usize, &str)> = Vec::new ();
  let mut output : Vec<String> = Vec::new ();
  for line in buf . lines () {
    let depth : usize = line . chars ()
      . take_while ( |c| *c == '*' ) . count ();
    if depth > 0 {
      while folder_stack . last ()
        .map_or (false, |(folder_depth, _)| *folder_depth >= depth)
      { folder_stack . pop (); }}
    let enclosing : Option<&str> =
      folder_stack . last () . map ( |(_, folder)| *folder );
    if members . iter () . any ( |(gnid, folder)| {
      enclosing == Some (*folder)
      && line . contains (&format! ("(gnid {})", gnid)) } )
    { output . push ( line . replace (
        " writeProtected ",
        " writeProtected (viewRequests editableView) " ) ); }
    else { output . push (line . to_string ()); }
    if let Some (folder) = [
      "forksFolder",
      "hiderFolder", "hiddenFolder",
      "hiddenInForkPartnerFolder", "hiddenOutsideOfForkPartnersFolder",
    ] . into_iter () . find ( |folder| line . contains (folder) )
    { folder_stack . push ((depth, folder)); }}
  output . join ("\n") + "\n"
}

/// A forking root of a fresh view is drawn with its forks folder: N
/// forks Subscribee and Overridden, claiming them, and Overrider and S
/// fork N, claiming nothing; last, the folder of what it hides outside
/// them, whose members are drawn folded: N hides Hidden, which no
/// forkee contains.
#[test]
fn a_subscribing_root_is_drawn_with_its_subscription_folders
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-extend-partner-folder-member-subscribing-root",
    "tests/extend_partner_folder_member/fixtures",
    "/tmp/tantivy-test-extend-partner-folder-member-subscribing-root",
    |config, tantivy| Box::pin ( async move {
      let (n_view, _pids, _tree)
        : (String, Vec<ID>, Tree<Viewnode>) =
        multi_root_view (
          config, Some (tantivy), &[ID::from ("N")] ) ?;
      let outline : Vec<String> = without_viewnode_ids (&n_view) . lines ()
        . filter ( |line| line . starts_with ('*') )
        . map ( |line| line . split (" (skgRepos") . next () . unwrap () . to_string () )
        . collect ();
      assert_eq! ( outline [.. 8], [
        "* (skg node (gnid N)",
        "** (skg forksFolder)",
        "*** (skg node (gnid Subscribee)",
        "*** (skg node (gnid Overridden)",
        "*** (skg node (gnid Overrider)",
        "*** (skg node (gnid S)",
        "*** (skg hiddenOutsideOfForkPartnersFolder)",
        "**** (skg folded node (gnid Hidden)" ],
        "{}", n_view );
      Ok (( )) } )) }

#[test]
fn initial_and_as_such_editable_nodes_get_only_the_default_folders
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-extend-partner-folder-member",
    "tests/extend_partner_folder_member/fixtures",
    "/tmp/tantivy-test-extend-partner-folder-member",
    |config, tantivy| Box::pin ( async move {
      let graph : InRustGraphHandle =
        graph_handle_from_config (config) ?;
      let (n_view, _pids, _tree)
        : (String, Vec<ID>, Tree<Viewnode>) =
        multi_root_view (
          config, Some (tantivy), &[ID::from ("N")] )
 ?;
      assert! ( n_view . contains ("forksFolder"),
        "N's nonempty forksFolder should be shown initially:\n{}",
        n_view );
      for exotic in [ "hiderFolder", "hiddenFolder" ] {
        assert! ( ! n_view . contains (exotic),
          "{} should require an explicit folder request:\n{}",
          exotic, n_view ); }

      let requested_folders : String = n_view . replacen (
        "node ",
        "node (viewRequests (folder forks) \
         (folder hides))", 1 );
      let with_folders : String =
        save (&requested_folders, config, tantivy, &graph)
        . await ? . saved_view;
      for folder in [ "forksFolder",
                      "hiderFolder", "hiddenFolder" ] {
        assert! ( with_folders . contains (folder),
          "the explicit requests should show {}:\n{}", folder, with_folders ); }

      let members : [(&str, &str); 4] = [
        ("Subscribee", "forksFolder"),
        ("S", "forksFolder"),
        ("Hidden", "hiddenFolder"),
        ("Hider", "hiderFolder") ];
      let extended_request : String =
        add_editable_view_requests_to_folder_members (&with_folders, &members);
      let saved : String =
        save (&extended_request, config, tantivy, &graph)
        . await ? . saved_view;
      for (gnid, folder) in members {
        let subtree : String =
          subtree_for_node_in_folder (&saved, gnid, folder);
        assert! ( subtree . contains ("forksFolder"),
          "editable partner folder member {} should get its nonempty \
           forksFolder:\n{}", gnid, saved );
        for exotic in [ "hiderFolder", "hiddenFolder" ] {
          assert! ( ! subtree . contains (exotic),
            "editable partner folder member {} should not get {} by \
             default:\n{}", gnid, exotic, saved ); }}
      let subscribee_subtree : String =
        subtree_for_node_in_folder (
          &saved, "Subscribee", "forksFolder");
      assert! ( subscribee_subtree . contains ("hiddenInForkPartnerFolder"),
        "a editable subscribee-as-such should also show its nonempty \
         hidden-here folder:\n{}", saved );
      let subscriber_subtree : String =
        subtree_for_node_in_folder (&saved, "S", "forksFolder");
      assert! ( subscriber_subtree . contains ("(gnid C)")
                && subscriber_subtree . contains ("content of S"),
        "S's own content should appear on extension:\n{}", saved );
      assert! ( ! subscriber_subtree . contains ("hiddenInForkPartnerFolder")
                && ! subscriber_subtree . contains (
                  "hiddenOutsideOfForkPartnersFolder"),
        "a note's hides apply to its forkees as such, not to a \
         forker-only member of its forksFolder:\n{}", saved );
      Ok (( )) } )) }
