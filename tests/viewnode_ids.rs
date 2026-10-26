// cargo nextest run --test grouped_views -E 'test(viewnode_ids::)'
//
// Viewnode IDs ('docs/api-and-formats.org', "The metadata headers"):
// every viewnode of a rendered buffer carries '(vnid X)', the view
// keeps it across saves and rerenders, and the view's
// 'ViewnodeIdRecord' knows the ones the forest holds.
//
// Fixture: p contains a; a contains b and c; c subscribes to d. The
// view of a shows p in a's containers folder, b and c as members, and
// c's subscribee folder holding d.

use crate::git_fixture_helpers::mk_test_tcp_stream_pair;

use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::metadata_canon::{viewnode_id_atom_of, without_viewnode_ids};
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::test_utils::{
  extract_string_field_from_sexp, graph_handle_from_config,
  read_all_lp_messages, run_with_test_stores};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::to_org::render::content_view::multi_root_view;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};
use skg::types::viewnode::{
  Folder, ViewnodeId, Viewnode, viewforest_root_viewnode,
  viewnode_of, ViewnodeKind};
use skg::types::views_state::{OpenViews, ViewforestId, ViewnodeIdRecord};

use ego_tree::{NodeId, Tree};
use std::collections::HashSet;
use std::error::Error;
use std::io::BufReader;
use std::net::TcpStream;

const FIXTURES : &str = "tests/viewnode_ids/fixtures";

#[test]
fn viewnode_ids_in_content_views (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-viewnode-ids",
    FIXTURES,
    "/tmp/tantivy-test-viewnode-ids",
    |config, tantivy| Box::pin ( async move {
      every_headline_of_a_new_view_has_its_own_viewnode_id (config, tantivy) ?;
      a_viewnode_id_survives_a_save_and_a_rerender (config, tantivy) . await ?;
      a_copied_headline_gets_a_fresh_viewnode_id_and_the_original_keeps_its_own (
        config, tantivy ) . await ?;
      a_moved_headline_keeps_its_viewnode_id (config, tantivy) . await ?;
      a_typed_headline_gets_a_viewnode_id (config, tantivy) . await ?;
      // last, since it cuts b from a's content
      an_unknown_viewnode_id_is_dropped (config, tantivy) . await ?;
      Ok (( )) } )) }

//
// Helpers
//

const VIEW_OF_A : &str = "view of a";

/// Open the view of 'root' as a client's request does, registered
/// under 'view_name'. Returns its text.
fn open_view (
  config      : &SkgConfig,
  tantivy     : &TantivyIndex,
  graph       : &InRustGraphHandle,
  views_state : &mut ViewsState,
  view_name   : &str,
  root        : &str,
) -> Result<String, Box<dyn Error>> {
  let (text, pids, tree) : (String, Vec<ID>, Tree<Viewnode>) =
    multi_root_view (config, Some (tantivy), &[ ID::from (root) ]) ?;
  views_state . open_views . register_view (
    &graph . load_full (), ViewforestId::ContentView (view_name . to_string ()),
    tree, &pids );
  Ok (text) }

/// Save 'buffer' as the view 'view_name'. Returns the response and
/// the messages streamed meanwhile (the collateral views).
async fn save_view (
  buffer      : &str,
  config      : &SkgConfig,
  tantivy     : &TantivyIndex,
  graph       : &InRustGraphHandle,
  views_state : &mut ViewsState,
  view_name   : &str,
) -> Result<(SaveResponse, Vec<String>), Box<dyn Error>> {
  let (mut stream, read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response : SaveResponse = update_from_and_rerender_buffer (
    &mut stream, buffer, config, tantivy, graph,
    &Ok ( ViewforestId::ContentView (view_name . to_string ()) ),
    views_state ) . await ?;
  drop (stream);
  let streamed : Vec<String> =
    read_all_lp_messages ( &mut BufReader::new (read_end) );
  assert! ( response . errors . is_empty (),
            "the buffer must save, got {:?}\n\nbuffer:\n{}",
            response . errors, buffer );
  Ok (( response, streamed )) }

fn fresh_views_state () -> ViewsState {
  ViewsState { open_views     : OpenViews::new (),
               graph_straddle : None } }

/// The viewnode ID of the one headline of 'buffer' that, viewnode IDs aside,
/// contains 'needle'.
fn viewnode_id_of_line_containing (
  buffer : &str,
  needle : &str,
) -> String {
  let lines : Vec<&str> = buffer . lines ()
    . filter ( |line| without_viewnode_ids (line) . contains (needle) ) . collect ();
  assert_eq! ( lines . len (), 1,
               "one headline should contain {:?}:\n{}", needle, buffer );
  viewnode_id_atom_of (lines [0])
    . unwrap_or_else ( || panic! ( "{:?} has no viewnode ID:\n{}", lines [0], buffer ))
    . to_string () }

/// The viewnode IDs of the headlines of 'buffer' that, viewnode IDs aside,
/// contain 'needle', in buffer order.
fn viewnode_ids_of_lines_containing (
  buffer : &str,
  needle : &str,
) -> Vec<String> {
  buffer . lines ()
    . filter ( |line| without_viewnode_ids (line) . contains (needle) )
    . map ( |line| viewnode_id_atom_of (line) . expect ("a viewnode ID") . to_string () )
    . collect () }

/// Every viewnode ID atom of 'buffer', in buffer order.
fn viewnode_id_atoms (
  buffer : &str,
) -> Vec<String> {
  buffer . lines () . filter ( |line| line . starts_with ('*') )
    . map ( |line| viewnode_id_atom_of (line)
            . unwrap_or_else ( || panic! ( "{:?} has no viewnode ID:\n{}", line, buffer ))
            . to_string () )
    . collect () }

/// The X of a '(vnid X)' atom.
fn viewnode_id_of_atom (
  viewnode_id_atom : &str,
) -> ViewnodeId {
  ViewnodeId ( viewnode_id_atom . trim_start_matches ("(vnid ") . trim_end_matches (')')
        . to_string () ) }

//
// Tests
//

/// A new view's every headline carries a viewnode ID, no two alike, none
/// spelled like a number, and the view's counter is past them all.
fn every_headline_of_a_new_view_has_its_own_viewnode_id (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = fresh_views_state ();
  let buffer : String = open_view (
    config, tantivy, &graph, &mut views_state, VIEW_OF_A, "a" ) ?;
  let atoms : Vec<String> = viewnode_id_atoms (&buffer);
  assert_eq! ( atoms . iter () . collect::<HashSet<_>> () . len (), atoms . len (),
               "no two headlines share a viewnode ID:\n{}", buffer );
  for atom in &atoms {
    assert! ( viewnode_id_of_atom (atom) . 0 . parse::<f64> () . is_err (),
              "{} reads as a number:\n{}", atom, buffer ); }
  let record : ViewnodeIdRecord = views_state . open_views
    . viewnode_id_record_of ( &ViewforestId::ContentView (VIEW_OF_A . to_string ()) );
  for atom in &atoms {
    assert! ( viewnode_id_of_atom (atom) . counter_value () . unwrap ()
              < record . next_viewnode_id_counter,
              "the counter is past {}", atom ); }
  Ok (( )) }

/// Saved unchanged, the buffer comes back byte for byte, its viewnode IDs
/// with it; and a collateral view rerendered by a save keeps its own.
async fn a_viewnode_id_survives_a_save_and_a_rerender (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = fresh_views_state ();
  let buffer : String = open_view (
    config, tantivy, &graph, &mut views_state, VIEW_OF_A, "a" ) ?;
  let other : String = open_view (
    config, tantivy, &graph, &mut views_state, "another view of a", "a" ) ?;
  let (saved, _) : (SaveResponse, Vec<String>) = save_view (
    &buffer, config, tantivy, &graph, &mut views_state, VIEW_OF_A ) . await ?;
  assert_eq! ( saved . saved_view, buffer, "the viewnode IDs survive a save" );
  let retitled : String = buffer . replacen (")) b\n", ")) b, retitled\n", 1);
  assert_ne! ( retitled, buffer );
  let (_, streamed) : (SaveResponse, Vec<String>) = save_view (
    &retitled, config, tantivy, &graph, &mut views_state, VIEW_OF_A ) . await ?;
  let collateral : String = streamed . iter ()
    . find ( |message| message . contains ("collateral-view")
                       && message . contains ("another view of a") )
    . and_then ( |message| extract_string_field_from_sexp (
      &message [ message . find ('(') . unwrap () .. ], "content" ))
    . unwrap_or_else ( || panic! ( "the other view is rerendered: {:?}", streamed ));
  assert_eq! ( collateral,
               other . replacen (")) b\n", ")) b, retitled\n", 1),
               "the collateral view keeps its viewnode IDs" );
  Ok (( )) }

/// A copy of a headline, made with its metadata, loses the viewnode ID it
/// carried; the original keeps it.
async fn a_copied_headline_gets_a_fresh_viewnode_id_and_the_original_keeps_its_own (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = fresh_views_state ();
  let buffer : String = open_view (
    config, tantivy, &graph, &mut views_state, VIEW_OF_A, "a" ) ?;
  let p_line : &str = buffer . lines ()
    . find ( |line| line . contains ("node (gnid p) ") )
    . expect ("p is shown");
  let p_viewnode_id : String = viewnode_id_atom_of (p_line) . unwrap () . to_string ();
  let copied : String = // the copy a root after the buffer's last
    format! ( "{}* {}\n", buffer, p_line . trim_start_matches ('*') . trim_start () );
  let (saved, _) : (SaveResponse, Vec<String>) = save_view (
    &copied, config, tantivy, &graph, &mut views_state, VIEW_OF_A ) . await ?;
  let p_viewnode_ids : Vec<String> =
    viewnode_ids_of_lines_containing (&saved . saved_view, "node (gnid p) ");
  assert_eq! ( p_viewnode_ids . len (), 2, "{}", saved . saved_view );
  assert_eq! ( p_viewnode_ids [0], p_viewnode_id,
               "the original keeps its viewnode ID:\n{}", saved . saved_view );
  assert! ( ! viewnode_id_atoms (&buffer) . contains (&p_viewnode_ids [1]),
            "the copy's viewnode ID is new:\n{}", saved . saved_view );
  Ok (( )) }

/// A headline moved elsewhere in the buffer keeps its viewnode ID.
async fn a_moved_headline_keeps_its_viewnode_id (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = fresh_views_state ();
  let buffer : String = open_view (
    config, tantivy, &graph, &mut views_state, VIEW_OF_A, "a" ) ?;
  let b_line : &str = buffer . lines ()
    . find ( |line| line . contains ("node (gnid b) ") )
    . expect ("b is shown");
  let b_viewnode_id : String = viewnode_id_atom_of (b_line) . unwrap () . to_string ();
  let moved : String = // b, from first in a's content to last
    buffer . replacen ( &format! ("{}\n", b_line), "", 1 )
    + &format! ("{}\n", b_line);
  let (saved, _) : (SaveResponse, Vec<String>) = save_view (
    &moved, config, tantivy, &graph, &mut views_state, VIEW_OF_A ) . await ?;
  let last_headline : &str = saved . saved_view . lines ()
    . filter ( |line| line . starts_with ('*') ) . last () . unwrap ();
  assert! ( last_headline . contains ("node (gnid b) "),
            "b moved to last:\n{}", saved . saved_view );
  assert_eq! ( viewnode_id_of_line_containing (&saved . saved_view, "node (gnid b) "),
               b_viewnode_id, "b keeps its viewnode ID:\n{}", saved . saved_view );
  Ok (( )) }

/// A headline the user typed, without metadata, gets a viewnode ID the buffer
/// never used.
async fn a_typed_headline_gets_a_viewnode_id (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = fresh_views_state ();
  let buffer : String = open_view (
    config, tantivy, &graph, &mut views_state, VIEW_OF_A, "a" ) ?;
  let typed : String = format! ("{}** a typed child\n", buffer);
  let (saved, _) : (SaveResponse, Vec<String>) = save_view (
    &typed, config, tantivy, &graph, &mut views_state, VIEW_OF_A ) . await ?;
  let typed_viewnode_id : String =
    viewnode_id_of_line_containing (&saved . saved_view, "a typed child");
  assert! ( ! viewnode_id_atoms (&buffer) . contains (&typed_viewnode_id),
            "the typed headline's viewnode ID is new:\n{}", saved . saved_view );
  Ok (( )) }

/// A viewnode ID the view's record does not know is dropped at the
/// parse, so its headline gets a fresh one: one brought back from a
/// headline cut at an earlier save, and one typed by hand past the
/// counter, which leaves the counter where it was.
async fn an_unknown_viewnode_id_is_dropped (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = fresh_views_state ();
  let buffer : String = open_view (
    config, tantivy, &graph, &mut views_state, VIEW_OF_A, "a" ) ?;
  let b_line : &str = buffer . lines ()
    . find ( |line| line . contains ("node (gnid b) ") )
    . expect ("b is shown");
  let b_viewnode_id_atom : String =
    viewnode_id_atom_of (b_line) . unwrap () . to_string ();
  let without_b : String = buffer . replacen ( &format! ("{}\n", b_line), "", 1 );
  let (cut, _) : (SaveResponse, Vec<String>) = save_view (
    &without_b, config, tantivy, &graph, &mut views_state, VIEW_OF_A ) . await ?;
  assert! ( ! cut . saved_view . contains (&b_viewnode_id_atom),
            "b's viewnode ID left with b:\n{}", cut . saved_view );
  let counter_before : u64 = views_state . open_views
    . viewnode_id_record_of ( &ViewforestId::ContentView (VIEW_OF_A . to_string ()) )
    . next_viewnode_id_counter;
  let typed : String = format! (
    "{}** (skg {}) brought back\n** (skg (vnid zz)) typed past the counter\n",
    cut . saved_view, b_viewnode_id_atom );
  let (saved, _) : (SaveResponse, Vec<String>) = save_view (
    &typed, config, tantivy, &graph, &mut views_state, VIEW_OF_A ) . await ?;
  for needle in ["brought back", "typed past the counter"] {
    let atom : String =
      viewnode_id_of_line_containing (&saved . saved_view, needle);
    assert! ( atom != b_viewnode_id_atom && atom != "(vnid zz)",
              "{:?} has a fresh viewnode ID:\n{}", needle, saved . saved_view );
    assert! ( viewnode_id_of_atom (&atom) . counter_value () . unwrap ()
              < counter_before + 4,
              "{:?} got the next fresh viewnode ID:\n{}", needle, saved . saved_view ); }
  let counter_after : u64 = views_state . open_views
    . viewnode_id_record_of ( &ViewforestId::ContentView (VIEW_OF_A . to_string ()) )
    . next_viewnode_id_counter;
  assert! ( counter_after < counter_before + 4,
            "the counter moved only past the two fresh viewnode IDs: {} -> {}",
            counter_before, counter_after );
  Ok (( )) }

/// The record's pass at the refresh's tail gives a viewnode ID to each viewnode
/// without one, in buffer order, never one the counter has passed
/// (so not one a viewnode cut from the buffer had), never one spelled
/// like a number, and knows the viewnode IDs the forest holds and no
/// others.
#[test]
fn the_record_assigns_fresh_viewnode_ids_and_forgets_what_left (
) {
  let mut tree : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = tree . root () . id ();
  let folder : NodeId = tree . get_mut (root) . unwrap () . append (
    viewnode_of (ViewnodeKind::Folder (Folder::Subscribee)) ) . id ();
  for _ in 0 .. 120 {
    tree . get_mut (folder) . unwrap () . append ( viewnode_of (
      ViewnodeKind::Tombstone )); }
  let mut record : ViewnodeIdRecord = ViewnodeIdRecord::default ();
  record . assign_viewnode_ids (&mut tree);
  assert_eq! ( tree . root () . value () . viewnode_id, None, "the buffer root gets none" );
  let viewnode_ids : Vec<ViewnodeId> = tree . root () . descendants () . skip (1)
    . map ( |node| node . value () . viewnode_id . clone () . expect ("a viewnode ID") )
    . collect ();
  assert_eq! ( viewnode_ids [0], ViewnodeId ("0a" . to_string ()),
               "the first viewnode ID not spelled like a number" );
  assert! ( viewnode_ids . windows (2) . all ( |pair| pair [0] . counter_value ()
                                              < pair [1] . counter_value () ),
            "assigned in buffer order: {:?}", viewnode_ids );
  assert! ( viewnode_ids . iter () . all ( |viewnode_id| viewnode_id . 0 . parse::<f64> () . is_err () ),
            "{:?}", viewnode_ids );
  assert_eq! ( record . known_viewnode_ids,
               viewnode_ids . iter () . cloned () . collect::<HashSet<ViewnodeId>> () );
  { // Cut the folder and its last child; append a new viewnode.
    let last : NodeId = tree . get (folder) . unwrap () . last_child () . unwrap () . id ();
    tree . get_mut (last) . unwrap () . detach ();
    let kept : Vec<NodeId> = tree . get (folder) . unwrap () . children ()
      . map ( |child| child . id () ) . collect ();
    for child in kept {
      tree . get_mut (root) . unwrap () . append_id (child); }
    tree . get_mut (folder) . unwrap () . detach ();
    tree . get_mut (root) . unwrap () . append ( viewnode_of (
      ViewnodeKind::Tombstone )); }
  record . assign_viewnode_ids (&mut tree);
  let newest : ViewnodeId = tree . root () . last_child () . unwrap () . value ()
    . viewnode_id . clone () . unwrap ();
  assert! ( newest . counter_value () > viewnode_ids . last () . unwrap () . counter_value (),
            "{:?} reuses no viewnode ID the buffer had", newest );
  assert! ( ! record . known_viewnode_ids . contains (&viewnode_ids [0])
            && ! record . known_viewnode_ids . contains ( viewnode_ids . last () . unwrap () ),
            "the record forgets the folder and its cut child: {:?}",
            record . known_viewnode_ids );
  assert! ( record . known_viewnode_ids . contains (&newest), "{:?}", newest ); }
