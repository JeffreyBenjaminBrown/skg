// cargo nextest run --test override_menu
//
// The override-choice buffer
// (TODO/full-schema/11_override-rendering-and-navigation.org): a new
// single-root view of an overridden node returns a menu -- the node
// as root, each (visible) overrider an indefinitive Independent
// child of what it overrides, all edges including foreign, branches
// stopping at the first repeated ID with the cycle viewstat.
//
// Fixture (fixtures-multi): owned R and foreign F override Z;
// foreign F2 overrides F (a legal foreign chain); foreign F3 alone
// overrides Z2; RO (source "other", excluded by set "main")
// overrides ZO; foreign C1 and C2 override each other (a legal
// foreign cycle); PLAIN is overridden by nothing.

use std::collections::HashSet;
use std::error::Error;
use std::net::{TcpListener, TcpStream};
use std::sync::Arc;

use skg::dbs::in_rust_graph::install_or_swap_global_handle;
use skg::serve::ViewsState;
use skg::serve::handlers::single_root_view::handle_single_root_view_request;
use skg::source_sets::{
  ActiveSourceSet, SourceSetName};
use skg::test_utils::{graph_handle_from_config, read_lp_message,
                      skg_env_from_parts};
use skg::test_utils::run_with_shared_test_db;
use skg::to_org::render::override_menu::override_menu_view;
use skg::types::env::SkgEnv;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};
use skg::types::tree::forest::ViewForest;
use skg::types::viewnode::{ParentIs, mk_indefinitive_viewnode};
use skg::types::misc::SourceName;
use skg::types::views_state::{OpenViews, ViewUri};
use typedb_driver::TypeDBDriver;

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str = "tests/override_menu/fixtures-multi";
  run_with_shared_test_db (
    "skg-test-override-menu",
    |s| Box::pin ( async move {
      s . reset ("menu_shows_all_edges_with_op_heralds", fixtures) . await ?;
      menu_shows_all_edges_with_op_heralds (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("menu_appears_for_foreign_only_overriders", fixtures) . await ?;
      menu_appears_for_foreign_only_overriders (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("menu_stops_cycles_with_the_cycle_viewstat", fixtures) . await ?;
      menu_stops_cycles_with_the_cycle_viewstat (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("inactive_overriders_are_omitted_and_can_empty_the_menu", fixtures) . await ?;
      inactive_overriders_are_omitted_and_can_empty_the_menu (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("menu_still_offered_in_diff_mode", fixtures) . await ?;
      menu_still_offered_in_diff_mode (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("open_menu_survives_diff_mode_toggle", fixtures) . await ?;
      open_menu_survives_diff_mode_toggle (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("handler_precedence_and_menu_dedup", fixtures) . await ?;
      handler_precedence_and_menu_dedup (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

fn connected_tcp_stream_pair (
) -> Result<(TcpStream, TcpStream), Box<dyn Error>> {
  let listener : TcpListener =
    TcpListener::bind ("127.0.0.1:0")?;
  let addr = listener . local_addr ()?;
  let client : TcpStream =
    TcpStream::connect (addr)?;
  let (server, _addr) =
    listener . accept ()?;
  Ok ((server, client)) }

fn org_depth ( line : &str ) -> usize {
  line . chars () . take_while ( |c| *c == '*' ) . count () }

/// (depth, line) for the line containing the given id atom.
fn line_with_id<'a> (
  buf : &'a str,
  id  : &str,
) -> Option<(usize, &'a str)> {
  let needle : String = format! ("(id {})", id);
  buf . lines ()
    . find ( |l| l . contains (&needle) )
    . map ( |l| (org_depth (l), l) ) }

async fn menu_shows_all_edges_with_op_heralds (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let graph = graph_handle_from_config (config) ?;
      install_or_swap_global_handle (
        graph_handle_from_config (config) ? );
      let env : SkgEnv =
        skg_env_from_parts (
          config, Arc::clone (driver), tantivy, &graph );
      let (menu, _pids, _tree) =
        override_menu_view ( &env, &ID::from ("Z"), None )
        . await ?
        . expect ("Z is overridden, so a menu exists");
      { let (z_depth, z_line) =
          line_with_id (&menu, "Z") . expect ("Z is the root");
        assert_eq! (z_depth, 1, "{}", menu);
        assert! ( z_line . contains ("(parentIs absent)"),
                  "{}", menu ); }
      for overrider in ["R", "F"] {
        let (depth, line) =
          line_with_id (&menu, overrider)
          . unwrap_or_else ( || panic! (
              "{} overrides Z (foreign edges included):\n{}",
              overrider, menu ));
        assert_eq! ( depth, 2, "{}", menu );
        assert! ( line . contains ("indef"), "{}", menu );
        assert! ( line . contains ("(parentIs independent)"),
          "a menu child must not read as content (saving the menu \
           must not edit Z's contains):\n{}", menu );
        assert! ( line . contains ("overridesParent"),
          "the Op herald marks each overrider:\n{}", menu ); }
      { let (f2_depth, f2_line) =
          line_with_id (&menu, "F2")
          . expect ("the foreign chain continues: F2 overrides F");
        assert_eq! ( f2_depth, 3, "{}", menu );
        assert! ( f2_line . contains ("overridesParent"),
                  "{}", menu ); }
      Ok (( )) }

async fn menu_appears_for_foreign_only_overriders (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let graph = graph_handle_from_config (config) ?;
      install_or_swap_global_handle (
        graph_handle_from_config (config) ? );
      let env : SkgEnv =
        skg_env_from_parts (
          config, Arc::clone (driver), tantivy, &graph );
      let (menu, _pids, _tree) =
        override_menu_view ( &env, &ID::from ("Z2"), None )
        . await ?
        . expect ("the menu fires for ANY overrider, foreign too");
      assert! ( line_with_id (&menu, "F3") . is_some (),
                "{}", menu );
      Ok (( )) }

async fn menu_stops_cycles_with_the_cycle_viewstat (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let graph = graph_handle_from_config (config) ?;
      install_or_swap_global_handle (
        graph_handle_from_config (config) ? );
      let env : SkgEnv =
        skg_env_from_parts (
          config, Arc::clone (driver), tantivy, &graph );
      let (menu, _pids, _tree) =
        override_menu_view ( &env, &ID::from ("C1"), None )
        . await ?
        . expect ("C1 is overridden (by C2)");
      let c1_lines : Vec<&str> =
        menu . lines ()
        . filter ( |l| l . contains ("(id C1)") )
        . collect ();
      assert_eq! ( c1_lines . len (), 2,
        "C1 appears as root and once as the cycle stop:\n{}", menu );
      assert! ( c1_lines [1] . contains ("cycle"),
        "the repeat carries the cycle viewstat:\n{}", menu );
      assert_eq! ( menu . lines () . count (), 3,
        "the branch stops at the repeat:\n{}", menu );
      Ok (( )) }

async fn inactive_overriders_are_omitted_and_can_empty_the_menu (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let graph = graph_handle_from_config (config) ?;
      install_or_swap_global_handle (
        graph_handle_from_config (config) ? );
      let env : SkgEnv =
        skg_env_from_parts (
          config, Arc::clone (driver), tantivy, &graph );
      let active : ActiveSourceSet =
        ActiveSourceSet::named (
          config, SourceSetName::from ("main")) ?;
      assert! (
        override_menu_view (
          &env, &ID::from ("ZO"), Some (&active) )
        . await ? . is_none (),
        "RO is inactive under set 'main', so no visible overrider \
         exists and the caller falls through to a normal render" );
      let (menu, _pids, _tree) =
        override_menu_view ( &env, &ID::from ("ZO"), None )
        . await ?
        . expect ("under 'all', RO is visible");
      assert! ( line_with_id (&menu, "RO") . is_some (),
                "{}", menu );
      Ok (( )) }

/// The shape signature of a rendered org buffer: (depth, id) per
/// headline, ignoring decoration (e.g. notInGit, diff axes), so
/// shape-stability across a diff-mode toggle is assertable without
/// pinning every decoration detail.
fn shape_signature (
  buf : &str,
) -> Vec<(usize, String)> {
  buf . lines ()
    . map ( |l| {
        let id : String =
          l . find ("(id ")
          . map ( |start| l [start + 4 ..]
                  . chars ()
                  . take_while ( |c| *c != ')' )
                  . collect () )
          . unwrap_or_default ();
        ( org_depth (l), id ) } )
    . collect () }

/// Stage 11 decided the override-choice menu appears in diff mode
/// too; stage 12-2 pins it: visiting an overridden node with
/// diff_mode_enabled still yields the menu, not a content view.
async fn menu_still_offered_in_diff_mode (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let graph = graph_handle_from_config (config) ?;
      install_or_swap_global_handle (
        graph_handle_from_config (config) ? );
      let env : SkgEnv =
        skg_env_from_parts (
          config, Arc::clone (driver), tantivy, &graph );
      let active : ActiveSourceSet =
        ActiveSourceSet::named (
          config, SourceSetName::from ("all")) ?;
      let mut views_state : ViewsState =
        ViewsState {
          diff_mode_enabled : true,
          open_views        : OpenViews::new (), };
      let (mut server, client) =
        connected_tcp_stream_pair () ?;
      std::thread::scope ( |scope| {
        scope . spawn ( || {
          handle_single_root_view_request (
            &mut server,
            "((request . \"single root content view\") \
              (id . \"Z\") (view-uri . \"diff-menu-uuid\"))",
            &env, &mut views_state, &active ); } ); } );
      let response : String = {
        let mut reader : std::io::BufReader<TcpStream> =
          std::io::BufReader::new (client);
        read_lp_message ( &mut reader ) ? };
      assert! ( response . contains ("override-menu:Z"),
        "diff mode still offers the menu:\n{}", response );
      assert! ( response . contains (
          "The requested node is overridden" ),
        "{}", response );
      Ok (( )) }

/// An open menu survives a diff-mode toggle intact: its generated
/// indefinitive lines neither phantom nor duplicate (decoration like
/// notInGit may appear; the shape -- depths and ids -- must not
/// change).
async fn open_menu_survives_diff_mode_toggle (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let graph = graph_handle_from_config (config) ?;
      install_or_swap_global_handle (
        graph_handle_from_config (config) ? );
      let env : SkgEnv =
        skg_env_from_parts (
          config, Arc::clone (driver), tantivy, &graph );
      let active : ActiveSourceSet =
        ActiveSourceSet::named (
          config, SourceSetName::from ("all")) ?;
      let mut views_state : ViewsState =
        ViewsState {
          diff_mode_enabled : false,
          open_views        : OpenViews::new (), };
      let menu_before : String = {
        // Open the menu through the handler, registering it.
        let (mut server, client) =
          connected_tcp_stream_pair () ?;
        std::thread::scope ( |scope| {
          scope . spawn ( || {
            handle_single_root_view_request (
              &mut server,
              "((request . \"single root content view\") \
                (id . \"Z\") (view-uri . \"toggle-menu-uuid\"))",
              &env, &mut views_state, &active ); } ); } );
        let mut reader : std::io::BufReader<TcpStream> =
          std::io::BufReader::new (client);
        read_lp_message ( &mut reader ) ? };
      let toggle = |views_state : &mut ViewsState| -> Vec<String> {
        let (mut server, client) =
          connected_tcp_stream_pair () . unwrap ();
        std::thread::scope ( |scope| {
          scope . spawn ( || {
            skg::serve::handlers::rerender_all_views::handle_git_diff_toggle_and_rerender (
              &mut server, &env, views_state, &active ); } ); } );
        drop (server);
        let mut reader : std::io::BufReader<TcpStream> =
          std::io::BufReader::new (client);
        let mut messages : Vec<String> = Vec::new ();
        while let Ok (m) = skg::test_utils::read_lp_message (&mut reader) {
          messages . push (m); }
        messages };
      let menu_view_of = |messages : &[String]| -> String {
        messages . iter ()
          . find ( |m| m . contains ("rerender-view")
                       && m . contains ("override-menu:Z") )
          . cloned ()
          . unwrap_or_else ( || panic! (
              "the open menu rerenders on toggle: {:?}", messages )) };
      { // Toggle ON: the menu rerenders, shape unchanged.
        let messages : Vec<String> = toggle (&mut views_state);
        assert! ( views_state . diff_mode_enabled );
        let rerendered : String = menu_view_of (&messages);
        for (depth, id) in shape_signature (&menu_before) {
          if id . is_empty () { continue; }
          assert! (
            shape_signature (&rerendered) . contains (
              & (depth, id . clone ()) ),
            "menu line (depth {}, id {}) survives the toggle:\n{}",
            depth, id, rerendered ); }
        assert_eq! (
          rerendered . matches ("(id Z)") . count (),
          menu_before . matches ("(id Z)") . count (),
          "no duplicate or phantom Z:\n{}", rerendered ); }
      { // Toggle OFF: still intact.
        let messages : Vec<String> = toggle (&mut views_state);
        assert! ( ! views_state . diff_mode_enabled );
        let rerendered : String = menu_view_of (&messages);
        for (depth, id) in shape_signature (&menu_before) {
          if id . is_empty () { continue; }
          assert! (
            shape_signature (&rerendered) . contains (
              & (depth, id . clone ()) ),
            "menu line (depth {}, id {}) survives the toggle:\n{}",
            depth, id, rerendered ); }}
      Ok (( )) }

async fn handler_precedence_and_menu_dedup (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let graph = graph_handle_from_config (config) ?;
      install_or_swap_global_handle (
        graph_handle_from_config (config) ? );
      let env : SkgEnv =
        skg_env_from_parts (
          config, Arc::clone (driver), tantivy, &graph );
      let active : ActiveSourceSet =
        ActiveSourceSet::named (
          config, SourceSetName::from ("all")) ?;
      let mut views_state : ViewsState =
        ViewsState {
          diff_mode_enabled : false,
          open_views        : OpenViews::new (), };
      let request_for = |id : &str| -> String {
        format! ( "((request . \"single root content view\") \
                    (id . \"{}\") (view-uri . \"test-uuid-{}\"))",
                  id, id ) };
      let respond = |views_state : &mut ViewsState,
                     request : &str| -> String {
        let (mut server, client) =
          connected_tcp_stream_pair () . unwrap ();
        std::thread::scope ( |scope| {
          // The handler is sync and calls block_on internally; it
          // cannot run inside this test's executor.
          scope . spawn ( || {
            handle_single_root_view_request (
              &mut server, request, &env,
              views_state, &active ); } ); } );
        let mut reader : std::io::BufReader<TcpStream> =
          std::io::BufReader::new (client);
        read_lp_message ( &mut reader ) . unwrap () };
      { // Visiting overridden Z yields the menu, registered under
        // its dedicated URI, with the minibuffer notice.
        let response : String =
          respond (&mut views_state, &request_for ("Z"));
        assert! ( response . contains ("override-menu:Z"),
                  "{}", response );
        assert! ( response . contains (
            "The requested node is overridden" ),
          "{}", response );
        assert! ( views_state . open_views . views . contains_key (
            &ViewUri::OverrideMenu ("Z" . to_string ())),
          "the menu is a registered view" ); }
      { // A second request switches to the open menu.
        let response : String =
          respond (&mut views_state, &request_for ("Z"));
        assert! ( response . contains ("switch-to-view"),
                  "{}", response );
        assert! ( response . contains ("override-menu:Z"),
                  "{}", response ); }
      { // An open RAW content view of Z beats the menu.
        views_state . open_views . unregister_view (
          &ViewUri::OverrideMenu ("Z" . to_string ()));
        let raw_forest : ViewForest = {
          let mut f : ViewForest = ViewForest::new ();
          f . append_root (
            mk_indefinitive_viewnode (
              ID::from ("Z"), SourceName::from ("main"),
              "Z" . to_string (), ParentIs::Absent ));
          f };
        views_state . open_views . register_view (
          ViewUri::ContentView ("raw-z-uuid" . to_string ()),
          raw_forest,
          &[ ID::from ("Z") ] );
        let response : String =
          respond (&mut views_state, &request_for ("Z"));
        assert! ( response . contains ("switch-to-view"),
                  "{}", response );
        assert! ( response . contains ("raw-z-uuid"),
          "the open raw view wins; no menu:\n{}", response ); }
      { // An unoverridden node renders normally, no notice.
        let response : String =
          respond (&mut views_state, &request_for ("PLAIN"));
        assert! ( response . contains ("(id PLAIN)"),
                  "{}", response );
        assert! ( ! response . contains ("to-minibuffer"),
                  "{}", response );
        let _ : &HashSet<ID> = &views_state . open_views . views
          . get ( &ViewUri::ContentView (
              "test-uuid-PLAIN" . to_string ()))
          . expect ("normal views register under the client URI")
          . pids; }
      { // Bypass: the overridden node itself opens, no menu.
        views_state . open_views . unregister_view (
          &ViewUri::ContentView ("raw-z-uuid" . to_string ()));
        let bypass_request : String =
          "((request . \"single root content view\") \
            (id . \"Z\") (view-uri . \"bypass-z-uuid\") \
            (override-choice . \"bypass\"))" . to_string ();
        let response : String =
          respond (&mut views_state, &bypass_request);
        assert! ( response . contains ("(id Z)"),
                  "{}", response );
        assert! ( ! response . contains ("to-minibuffer")
                  && ! response . contains ("override-menu"),
          "bypass opens the raw node, no menu:\n{}", response ); }
      { // An unknown override-choice value errors loudly. (Z2, not
        // Z: the bypass request above registered a raw view of Z,
        // and the switch-to-view check would win before the
        // override-choice value is even parsed.)
        let bad_request : String =
          "((request . \"single root content view\") \
            (id . \"Z2\") (view-uri . \"bad-uuid\") \
            (override-choice . \"sideways\"))" . to_string ();
        let response : String =
          respond (&mut views_state, &bad_request);
        assert! ( response . contains ("Unknown override-choice"),
                  "{}", response ); }
      Ok (( )) }
