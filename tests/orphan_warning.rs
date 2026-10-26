// cargo nextest run --test grouped_saves -E 'test(orphan_warning::)'
//
// A headline typed under a parent whose claims the save does not read
// is saved as a node, which nothing contains: an orphan. The save
// warns of each, naming the headline. One typed under an editable
// parent joins its list and draws no warning.
//
// Fixture (shared with corrections_channel): one contains two and
// three; two contains four; five contains two. The view of four shows
// its containers folder, which holds two, write-protected.

use std::error::Error;
use std::net::{TcpListener, TcpStream};

use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::test_utils::{run_with_test_stores, graph_handle_from_config};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::to_org::render::content_view::single_root_view;
use skg::types::misc::ID;
use skg::types::views_state::{OpenViews, ViewforestId};

#[test]
fn a_headline_typed_where_no_list_is_read_is_warned_of_as_an_orphan
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-orphan-warning",
    "tests/corrections_channel/fixtures",
    "/tmp/tantivy-test-orphan-warning",
    |config, tantivy| Box::pin ( async move {
      let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
      let mut views_state : ViewsState = ViewsState {
        open_views     : OpenViews::new (),
        graph_straddle : None, };
      let view_of_four : ViewforestId = ViewforestId::ContentView ("view of four" . into ());
      let (four, pids, viewforest) =
        single_root_view ( config, Some (tantivy), &ID::from ("four") ) ?;
      views_state . open_views . register_view (
        &graph . load_full (), view_of_four . clone (), viewforest, &pids );
      let two_line : &str = four . lines ()
        . find ( |line| line . contains ("(gnid two)") )
        . unwrap_or_else ( || panic! ("the containers folder holds two:\n{}", four) );
      assert! ( two_line . starts_with ("*** ") && two_line . contains ("writeProtected"),
                "two is write-protected in four's containers folder:\n{}", four );
      let typed : String = four . lines ()
        . flat_map ( |line| {
          let mut lines : Vec<String> = vec! [ line . to_string () ];
          if line == two_line {
            lines . push ( "**** typed under two" . to_string () ); }
          lines } )
        . chain ( [ "** typed under four" . to_string () ] )
        . collect::<Vec<String>> () . join ("\n") + "\n";
      let listener : TcpListener = TcpListener::bind ("127.0.0.1:0") . unwrap ();
      let mut stream : TcpStream =
        TcpStream::connect (listener . local_addr () . unwrap ()) . unwrap ();
      let response : SaveResponse = update_from_and_rerender_buffer (
        &mut stream, &typed, config, tantivy, &graph,
        &Ok (view_of_four . clone ()), &mut views_state ) . await ?;
      assert! ( response . errors . is_empty (), "{:?}", response . errors );
      let orphans : Vec<&String> = response . warnings . iter ()
        . filter ( |warning| warning . starts_with ("Created an orphan") )
        . collect ();
      assert_eq! ( orphans . len (), 1, "{:?}", response . warnings );
      assert! ( orphans [0] . contains ("*** typed under two\n"),
                "the warning names the headline: {}", orphans [0] );
      assert! ( response . saved_view . contains (") typed under two\n"),
                "the orphan is saved, and drawn where it was typed:\n{}",
                response . saved_view );
      Ok (( )) } )) }
