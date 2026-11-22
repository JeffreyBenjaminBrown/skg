// cargo nextest run --test grouped_forks -E 'test(extend_partner_folder_member::)'
//
// Default folders on ordinary editable nodes and on editable
// partner folder members.  The initial presentation contains only a
// forksFolder, where the node has a fork partner.  The other relation
// folders require explicit requests; after one of their members is
// extended as editable, that member gets the same default forksFolder.
// A member of a forks folder (a member as such) gets none: the folder
// it stands in lists every mirror already. A forkee as such gets its
// nonempty hiddenInForkPartnerFolder.

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
use skg::types::config::SkgConfig;
use skg::types::misc::{ID, TantivyIndex};
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
  let depth_of : &dyn Fn (usize) -> usize = &|i : usize| -> usize {
    lines [i] . chars () . take_while ( |c| *c == '*' ) . count () };
  let shallowest : Vec<usize> = { // a member as such may show partners of its own, deeper
    let least : usize = matches . iter () . map ( |i| depth_of (*i) ) . min ()
      . unwrap_or (0);
    matches . into_iter () . filter ( |i| depth_of (*i) == least ) . collect () };
  assert_eq! ( shallowest . len (), 1,
    "expected one shallowest occurrence of {} directly within {} in:\n{}",
    needle, enclosing_folder, buf );
  let start : usize = shallowest [0];
  let depth : usize = depth_of (start);
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
/// forks Forkee and SecondForkee, claiming them, and Forker and S
/// fork N, claiming nothing; then the notes each of those forks, the
/// indirect members, claiming nothing, by pid; last, the folder of
/// what it hides outside them, whose members are drawn folded: N
/// hides Hidden, which no member contains.
#[test]
fn a_forking_root_is_drawn_with_its_forks_folder
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-extend-partner-folder-member-forking-root",
    "tests/extend_partner_folder_member/fixtures",
    "/tmp/tantivy-test-extend-partner-folder-member-forking-root",
    |config, tantivy| Box::pin ( async move {
      let (n_view, _pids, _tree)
        : (String, Vec<ID>, Tree<Viewnode>) =
        multi_root_view (
          config, Some (tantivy), &[ID::from ("N")] ) ?;
      let outline : Vec<String> = without_viewnode_ids (&n_view) . lines ()
        . filter ( |line| line . starts_with ('*')
                          && ! line . starts_with ("****") // a forkee as such shows its own content
                          || line . contains ("(gnid Hidden)") )
        . map ( |line| line . split (" (skgRepos") . next () . unwrap () . to_string () )
        . collect ();
      assert_eq! ( outline [.. 12], [
        "* (skg node (gnid N)",
        "** (skg forksFolder)",
        "*** (skg node (gnid Forkee)",
        "*** (skg node (gnid SecondForkee)",
        "*** (skg node (gnid Forker)",
        "*** (skg node (gnid S)",
        "*** (skg node (gnid ForkeeLeaf)",
        "*** (skg node (gnid ForkerLeaf)",
        "*** (skg node (gnid SLeaf)",
        "*** (skg node (gnid SecondForkeeLeaf)",
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
        ("Forkee", "forksFolder"),
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
        let is_a_member_as_such : bool = folder == "forksFolder";
        assert_eq! ( subtree . contains ("forksFolder"), ! is_a_member_as_such,
          "editable partner folder member {} should get its nonempty \
           forksFolder unless it is a member of a forks folder:\n{}", gnid, saved );
        for exotic in [ "hiderFolder", "hiddenFolder" ] {
          assert! ( ! subtree . contains (exotic),
            "editable partner folder member {} should not get {} by \
             default:\n{}", gnid, exotic, saved ); }}
      let forkee_subtree : String =
        subtree_for_node_in_folder (
          &saved, "Forkee", "forksFolder");
      assert! ( forkee_subtree . contains ("hiddenInForkPartnerFolder"),
        "an editable forkee-as-such should also show its nonempty \
         hidden-here folder:\n{}", saved );
      let forker_subtree : String =
        subtree_for_node_in_folder (&saved, "S", "forksFolder");
      assert! ( forker_subtree . contains ("(gnid C)")
                && forker_subtree . contains ("content of S"),
        "S's own content should appear on extension:\n{}", saved );
      assert! ( ! forker_subtree . contains ("hiddenInForkPartnerFolder")
                && ! forker_subtree . contains (
                  "hiddenOutsideOfForkPartnersFolder"),
        "a note's hides apply to its forkees as such, not to a \
         forker-only member of its forksFolder:\n{}", saved );
      Ok (( )) } )) }
