// cargo nextest run --test grouped_overrides -E 'test(corrections_channel::)'
//
// The save response carries the buffer corrections of every view the save
// refreshed, each tagged with its view ('SaveResponse.buffer_corrections'):
// - a plain save, of a buffer as the server drew it, reports nothing;
// - a save that adds a child to 'two' in the view of 'one' corrects
//   the view of 'five', where 'two' is editable too and its list,
//   already drawn, now lacks the child. That buffer correction is tagged
//   with the view of 'five'; the saved view reports nothing.
//
// - a save that deletes a node reports no buffer correction for it,
//   in any view;
// - a save's nodeMerge reports no buffer correction, in any view.
//
// Fixture: one contains two and three; two contains four; five
// contains two.

use std::error::Error;
use std::net::{TcpListener, TcpStream};

use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::refresh::buffer_corrections::{BufferCorrectionKind, ViewBufferCorrections};
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::test_utils::{run_with_test_stores, graph_handle_from_config};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::test_utils::single_root_view;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};
use skg::types::views_state::{OpenViews, ViewforestId};

#[test]
fn buffer_corrections_reach_the_save_response_tagged_with_their_view
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-corrections-channel",
    "tests/corrections_channel/fixtures",
    "/tmp/tantivy-test-corrections-channel",
    |config, tantivy| Box::pin ( async move {
      let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
      let mut views_state : ViewsState = ViewsState {
        open_views     : OpenViews::new (),
        graph_straddle : None, };
      let view_of_one : ViewforestId = ViewforestId::ContentView ("view of one" . into ());
      let view_of_five : ViewforestId = ViewforestId::ContentView ("view of five" . into ());
      let one : String = open_view (
        "one", &view_of_one, config, tantivy, &graph, &mut views_state ) ?;
      open_view ( "five", &view_of_five, config, tantivy, &graph, &mut views_state ) ?;
      { // a plain save
        let response : SaveResponse = save (
          &one, &view_of_one, config, tantivy, &graph, &mut views_state ) . await ?;
        assert! ( response . errors . is_empty (), "{:?}", response . errors );
        assert! ( response . buffer_corrections . is_empty (),
                  "a plain save corrects nothing: {:?}", response . buffer_corrections ); }
      { // a child added to two, which the view of five also shows
        let with_child : String = one . lines ()
          . flat_map ( |line| {
            let mut lines : Vec<String> = vec! [ line . to_string () ];
            if line . contains ("(gnid two)") {
              lines . push ( "*** new child" . to_string () ); }
            lines } )
          . collect::<Vec<String>> () . join ("\n") + "\n";
        let response : SaveResponse = save (
          &with_child, &view_of_one, config, tantivy, &graph,
          &mut views_state ) . await ?;
        assert! ( response . errors . is_empty (), "{:?}", response . errors );
        assert_eq! ( response . buffer_corrections . len (), 1,
                     "only the view of five is corrected: {:?}", response . buffer_corrections );
        let five : &ViewBufferCorrections = &response . buffer_corrections [0];
        assert_eq! ( five . viewforest_id, view_of_five );
        assert_eq! ( five . buffer_corrections . len (), 1, "{:?}", five . buffer_corrections );
        assert_eq! ( five . buffer_corrections [0] . kind, BufferCorrectionKind::Reinstated );
        assert_eq! ( five . buffer_corrections [0] . title, "new child" );
        assert! ( five . buffer_corrections [0] . vnid . is_some (),
                  "the drawn child has a vnid by the time it is reported" );
        assert! ( response . warnings . is_empty (),
                  "a buffer correction is not a warning: {:?}", response . warnings ); }
      Ok (( )) } )) }

/// A save that deletes a node reports no buffer correction for it:
/// four, deleted in the view of one, leaves no "Entombstoned" in that
/// view or in the view of five, where it stood under two too. The
/// tombstone is the saving user's own doing ('BufferCorrectionPolicy').
#[test]
fn a_saves_own_deletion_is_no_buffer_correction
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-corrections-channel-deletion",
    "tests/corrections_channel/fixtures",
    "/tmp/tantivy-test-corrections-channel-deletion",
    |config, tantivy| Box::pin ( async move {
      let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
      let mut views_state : ViewsState = ViewsState {
        open_views     : OpenViews::new (),
        graph_straddle : None, };
      let view_of_one : ViewforestId = ViewforestId::ContentView ("view of one" . into ());
      let view_of_five : ViewforestId = ViewforestId::ContentView ("view of five" . into ());
      let one : String = open_view (
        "one", &view_of_one, config, tantivy, &graph, &mut views_state ) ?;
      let five : String = open_view (
        "five", &view_of_five, config, tantivy, &graph, &mut views_state ) ?;
      assert! ( one . contains ("(gnid four)") && five . contains ("(gnid four)"),
                "both views show four:\n{}\n{}", one, five );
      let deleting_four : String =
        one . replacen ( "(gnid four)", "(gnid four) (editRequest delete)", 1 );
      let response : SaveResponse = save (
        &deleting_four, &view_of_one, config, tantivy, &graph,
        &mut views_state ) . await ?;
      assert! ( response . errors . is_empty (), "{:?}", response . errors );
      assert! ( graph . load_full () . get (&ID::from ("four")) . is_none (),
                "the save deleted four" );
      let entombstoned : Vec<&ViewBufferCorrections> = response . buffer_corrections . iter ()
        . filter ( |view| view . buffer_corrections . iter ()
                   . any ( |c| c . kind == BufferCorrectionKind::Entombstoned ))
        . collect ();
      assert! ( entombstoned . is_empty (),
                "the save's own deletion is no buffer correction: {:?}",
                response . buffer_corrections );
      Ok (( )) } )) }

/// A save's nodeMerge reports nothing: in the view of one, three
/// acquires two. There two's occurrence becomes three's and merges with
/// it, and what three gained is drawn under it; in the view of five,
/// whose two is now three, and in the view of two, whose root is now
/// three, what three gained is drawn too. All of it is the user's own
/// request ('BufferCorrectionPolicy::AfterSave').
#[test]
fn a_saves_own_nodeMerge_is_no_buffer_correction
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-corrections-channel-nodeMerge",
    "tests/corrections_channel/fixtures",
    "/tmp/tantivy-test-corrections-channel-nodeMerge",
    |config, tantivy| Box::pin ( async move {
      let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
      let mut views_state : ViewsState = ViewsState {
        open_views     : OpenViews::new (),
        graph_straddle : None, };
      let view_of_one : ViewforestId = ViewforestId::ContentView ("view of one" . into ());
      let one : String = open_view (
        "one", &view_of_one, config, tantivy, &graph, &mut views_state ) ?;
      open_view ( "five", &ViewforestId::ContentView ("view of five" . into ()),
                  config, tantivy, &graph, &mut views_state ) ?;
      open_view ( "two", &ViewforestId::ContentView ("view of two" . into ()),
                  config, tantivy, &graph, &mut views_state ) ?;
      let merging : String =
        one . replacen ( "(gnid three)", "(gnid three) (editRequest (merge two))", 1 );
      assert_ne! ( merging, one );
      let response : SaveResponse = save (
        &merging, &view_of_one, config, tantivy, &graph,
        &mut views_state ) . await ?;
      assert! ( response . errors . is_empty (), "{:?}", response . errors );
      assert_eq! ( graph . load_full () . pid_of (&ID::from ("two")),
                   Some ( ID::from ("three") ),
                   "three acquired two" );
      assert! ( response . saved_view . contains ("MERGED: two"),
                "what three gained is drawn:\n{}", response . saved_view );
      assert! ( response . buffer_corrections . is_empty (),
                "the save's own nodeMerge is no buffer correction: {:?}",
                response . buffer_corrections );
      Ok (( )) } )) }

/// Render the view of 'root' and register it as 'viewforest_id'.
fn open_view (
  root          : &str,
  viewforest_id : &ViewforestId,
  config        : &SkgConfig,
  tantivy       : &mut TantivyIndex,
  graph         : &InRustGraphHandle,
  views_state   : &mut ViewsState,
) -> Result<String, Box<dyn Error>> {
  let (text, pids, viewforest) =
    single_root_view ( config, Some (tantivy), &ID::from (root) ) ?;
  views_state . open_views . register_view (
    &graph . load_full (), viewforest_id . clone (), viewforest, &pids );
  Ok (text) }

async fn save (
  buffer        : &str,
  viewforest_id : &ViewforestId,
  config        : &SkgConfig,
  tantivy       : &mut TantivyIndex,
  graph         : &InRustGraphHandle,
  views_state   : &mut ViewsState,
) -> Result<SaveResponse, Box<dyn Error>> {
  let listener : TcpListener = TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let mut stream : TcpStream =
    TcpStream::connect (listener . local_addr () . unwrap ()) . unwrap ();
  update_from_and_rerender_buffer (
    &mut stream, buffer, config, tantivy, graph,
    &Ok (viewforest_id . clone ()), views_state ) . await }
