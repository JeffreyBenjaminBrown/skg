// cargo nextest run --test grouped_saves -E 'test(orphan_warning::)'
//
// A headline typed under a parent whose claims the save does not read
// is saved as an orphan graphnode, a node that no list contains. The
// save warns of each, naming the headline by its title and new gnid.
// One typed under an editable parent joins its list and draws no
// warning, and a root typed at top level, where no list is, draws
// none either.
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
use skg::test_utils::single_root_view;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};
use skg::types::views_state::{OpenViews, ViewforestId};

const ORPHAN_WARNING : &str =
  "Created an orphan graphnode, a node that no list contains, from a headline whose parent reads no claims:\n";

#[test]
fn a_headline_typed_where_no_list_is_read_is_warned_of_as_an_orphan
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-orphan-warning",
    "tests/corrections_channel/fixtures",
    "/tmp/tantivy-test-orphan-warning",
    |config, tantivy| Box::pin ( async move {
      let four : String = view_of_four_with_two_write_protected (config, tantivy) ?;
      let two_line : String = line_of_two (&four);
      let typed : String = four . lines ()
        . flat_map ( |line| {
          let mut lines : Vec<String> = vec! [ line . to_string () ];
          if line == two_line {
            lines . push ( "**** typed under two" . to_string () ); }
          lines } )
        . chain ( [ "** typed under four" . to_string () ] )
        . collect::<Vec<String>> () . join ("\n") + "\n";
      let response : SaveResponse =
        save_view_of_four (config, tantivy, &four, &typed) . await ?;
      assert! ( response . errors . is_empty (), "{:?}", response . errors );
      let orphans : Vec<&String> = orphan_warnings (&response);
      assert_eq! ( orphans . len (), 1, "{:?}", response . warnings );
      assert! ( orphans [0] . starts_with (
                  &format! ("{}*** typed under two\n- graphnode ID: ", ORPHAN_WARNING) )
                && orphans [0] . lines () . count () == 3,
                "the warning names the headline by its title and gnid alone: {}",
                orphans [0] );
      assert! ( ! orphans [0] . contains ("not yet saved"),
                "the headline is saved: {}", orphans [0] );
      assert! ( response . saved_view . contains (") typed under two\n"),
                "the orphan is saved, and drawn where it was typed:\n{}",
                response . saved_view );
      Ok (( )) } )) }

/// A headline typed under a tombstone takes its skgrepo from the
/// nearest occurrence above, four, and is saved and warned of; so are
/// one typed under a ghost and one typed in four's containers folder,
/// which is write-protected. A root typed at top level, with its
/// skgrepo, is saved and not warned of.
#[test]
fn headlines_under_a_tombstone_a_ghost_and_a_write_protected_folder_are_orphans
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-orphan-warning-kinds",
    "tests/corrections_channel/fixtures",
    "/tmp/tantivy-test-orphan-warning-kinds",
    |config, tantivy| Box::pin ( async move {
      let four : String = view_of_four_with_two_write_protected (config, tantivy) ?;
      let folder_line : String = four . lines ()
        . find ( |line| line . contains ("containerFolder") )
        . unwrap_or_else ( || panic! ("four shows its containers folder:\n{}", four) )
        . to_string ();
      let typed : String = four . lines ()
        . flat_map ( |line| {
          let mut lines : Vec<String> = vec! [ line . to_string () ];
          if line == folder_line {
            lines . push ( "*** typed in the containers folder" . to_string () ); }
          lines } )
        . chain ( [ "** (skg tombstone)",
                    "*** typed under a tombstone",
                    "** (skg ghost (gnid three) (skgRepos (nodeRepo main))) three",
                    "*** typed under a ghost",
                    "* (skg node (skgRepos (nodeRepo main))) a typed root" ]
                  . map ( |line| line . to_string () ))
        . collect::<Vec<String>> () . join ("\n") + "\n";
      let response : SaveResponse =
        save_view_of_four (config, tantivy, &four, &typed) . await ?;
      assert! ( response . errors . is_empty (), "{:?}\n\nbuffer:\n{}",
                response . errors, typed );
      let mut named : Vec<String> = orphan_warnings (&response) . iter ()
        . map ( |warning| warning . lines () . nth (1) . unwrap_or ("") . to_string () )
        . collect ();
      named . sort ();
      assert_eq! ( named,
                   vec! [ "*** typed in the containers folder" . to_string (),
                          "*** typed under a ghost" . to_string (),
                          "*** typed under a tombstone" . to_string () ],
                   "{:?}", response . warnings );
      for title in [ "typed in the containers folder", "typed under a ghost",
                     "typed under a tombstone", "a typed root" ] {
        assert! ( response . saved_view . lines () . any ( |line|
                    line . ends_with (&format! (") {}", title))
                    && line . contains ("(nodeRepo main)") ),
                  "{} is saved in main:\n{}", title, response . saved_view ); }
      Ok (( )) } )) }

//
// Helpers
//

/// The view of four; two stands write-protected in its containers
/// folder.
fn view_of_four_with_two_write_protected (
  config  : &SkgConfig,
  tantivy : &TantivyIndex,
) -> Result<String, Box<dyn Error>> {
  let (four, _pids, _viewforest) =
    single_root_view ( config, Some (tantivy), &ID::from ("four") ) ?;
  let two_line : String = line_of_two (&four);
  assert! ( two_line . starts_with ("*** ") && two_line . contains ("writeProtected"),
            "two is write-protected in four's containers folder:\n{}", four );
  Ok (four) }

fn line_of_two (
  four : &str,
) -> String {
  four . lines ()
    . find ( |line| line . contains ("(gnid two)") )
    . unwrap_or_else ( || panic! ("the containers folder holds two:\n{}", four) )
    . to_string () }

/// Register FOUR, the view of four, and save TYPED as it.
async fn save_view_of_four (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
  four    : &str,
  typed   : &str,
) -> Result<SaveResponse, Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = ViewsState {
    open_views     : OpenViews::new (),
    graph_straddle : None, };
  let view_of_four : ViewforestId = ViewforestId::ContentView ("view of four" . into ());
  let (rendered, pids, viewforest) =
    single_root_view ( config, Some (tantivy), &ID::from ("four") ) ?;
  assert_eq! ( rendered, four );
  views_state . open_views . register_view (
    &graph . load_full (), view_of_four . clone (), viewforest, &pids );
  let listener : TcpListener = TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let mut stream : TcpStream =
    TcpStream::connect (listener . local_addr () . unwrap ()) . unwrap ();
  update_from_and_rerender_buffer (
    &mut stream, typed, config, tantivy, &graph,
    &Ok (view_of_four), &mut views_state ) . await }

fn orphan_warnings (
  response : &SaveResponse,
) -> Vec<&String> {
  response . warnings . iter ()
    . filter ( |warning| warning . starts_with (ORPHAN_WARNING) )
    . collect () }
