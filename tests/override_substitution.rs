// cargo nextest run --test grouped_overrides -E 'test(override_substitution::)'
//
// Override substitution (TODO/full-schema/11_override-rendering-and-navigation.org):
// when completion would CREATE a viewnode for node N as recursive
// content, and a user-owned, visible R overrides N, it draws R
// instead, marked '(overridesHere N)' -- and saving such a view
// round-trips to the ORIGINAL IDs, so a container's contains list is
// never silently rewritten from N to R.
//
// Fixture (tests/override_substitution/fixtures, single source
// "main", user-owned): Q contains P and P2; P contains N and M;
// R overrides N and contains W; S subscribes to E; E contains N.
//
// The multi-source fixture (fixtures-multi) adds the ownership and
// visibility gates: FR (source "foreign", not user-owned) overrides
// N1; R2 (source "other", user-owned) overrides N2; N3 lives in
// "other" while P3 and its overrider R3 live in "main".
//
// PITFALL: marked buffers hit the tamper check, which reads the
// process-global in-Rust graph; each sub-test installs its own
// fixture graph via install_or_swap_global_handle, which keeps the
// shared process's global consistent with the current fixtures.

use indoc::indoc;
use std::error::Error;
use std::net::TcpStream;
use std::sync::Arc;

use skg::from_text::buffer_to_validated_saveplan;
use skg::serve::ViewsState;
use skg::source_sets::{ActiveSourceSet, SourceSetName};
use skg::dbs::in_rust_graph::install_or_swap_global_handle;
use skg::test_utils::{
  run_with_shared_test_db,
  graph_handle_from_config};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::to_org::render::content_view::{
  multi_root_view, multi_root_view_with_source_set};
use skg::types::errors::{BufferValidationError, SaveError};
use skg::types::misc::{ID, SkgConfig, SourceName, TantivyIndex, members_of};
use skg::types::nodes::complete::NodeComplete;
use skg::types::save::{DefineNode, SaveNode};
use skg::types::views_state::OpenViews;

use skg::dbs::in_rust_graph::InRustGraphHandle;
use typedb_driver::TypeDBDriver;

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str = "tests/override_substitution/fixtures";
  run_with_shared_test_db (
    "skg-test-override-substitution",
    |s| Box::pin ( async move {
      s . reset ("de_novo_draws_the_overrider_marked", fixtures) . await ?;
      de_novo_draws_the_overrider_marked (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("save_roundtrips_to_original_and_is_idempotent", fixtures) . await ?;
      save_roundtrips_to_original_and_is_idempotent (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("extraction_honors_the_marker", fixtures) . await ?;
      extraction_honors_the_marker (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("diff_mode_disables_substitution", fixtures) . await ?;
      diff_mode_disables_substitution (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("marked_view_is_shape_stable_across_diff_toggle", fixtures) . await ?;
      marked_view_is_shape_stable_across_diff_toggle (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset_from_config ("ownership_and_visibility_gate_substitution",
                             "tests/override_substitution/fixtures-multi/skgconfig.toml") . await ?;
      ownership_and_visibility_gate_substitution (
        &s . config, &s . driver ) . await ?;
      s . reset_from_config ("chain_half_visible_keeps_the_original",
                             "tests/override_substitution/fixtures-chain/skgconfig.toml") . await ?;
      chain_half_visible_keeps_the_original (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

fn marked_lines<'a> (
  buf      : &'a str,
  original : &str,
) -> Vec<&'a str> {
  let marker : String =
    format! ("(overridesHere {})", original);
  buf . lines ()
    . filter ( |l| l . contains (&marker) )
    . collect () }

fn saved_node_by_id<'a> (
  instructions : &'a [DefineNode],
  id           : &str,
) -> &'a NodeComplete {
  opt_saved_node_by_id (instructions, id)
    . unwrap_or_else ( || panic! ("SaveNode not found: {}", id) ) }

fn opt_saved_node_by_id<'a> (
  instructions : &'a [DefineNode],
  id           : &str,
) -> Option<&'a NodeComplete> {
  for instruction in instructions {
    if let DefineNode::Save (SaveNode (node)) = instruction {
      if node . pid == ID::from (id) {
        return Some (node); }}}
  None }

async fn define_nodes_from (
  buffer : &str,
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<Vec<DefineNode>, SaveError> {
  Ok ( buffer_to_validated_saveplan (
         buffer, config, driver, None ) . await ?
       . 1 . define_nodes ) }

async fn save_and_rerender (
  buf     : &str,
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<String, Box<dyn Error>> {
  let graph : InRustGraphHandle =
    graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false,
    open_views        : OpenViews::new (),
  };
  let listener : std::net::TcpListener =
    std::net::TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let mut stream : TcpStream =
    TcpStream::connect (listener . local_addr () . unwrap ()) . unwrap ();
  let response = update_from_and_rerender_buffer (
    &mut stream,
    buf, driver, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state ) . await ?;
  assert! ( response . errors . is_empty (),
    "save must not error; got: {:?}", response . errors );
  Ok ( response . saved_view ) }

fn read_fixture_file (
  config : &SkgConfig,
  pid    : &str,
) -> String {
  let path : std::path::PathBuf =
    config . sources . values () . next () . unwrap ()
    . path . join ( format! ("{}.skg", pid) );
  std::fs::read_to_string (&path)
    . unwrap_or_else ( |e| panic! ("reading {:?}: {}", path, e) ) }

async fn de_novo_draws_the_overrider_marked (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      install_or_swap_global_handle (
        graph_handle_from_config (config) ? );
      let (view, _pids, _tree) =
        multi_root_view (
          driver, config, Some (tantivy),
          &[ ID::from ("P") ], false ) . await ?;
      { let marked : Vec<&str> = marked_lines (&view, "N");
        assert_eq! ( marked . len (), 1,
          "exactly one drawn substitute for N:\n{}", view );
        assert! ( marked [0] . contains ("(id R)"),
          "the substitute is R:\n{}", view ); }
      assert! ( view . lines () . any (
                  |l| l . contains ("(id W)") ),
        "R's own content W is drawn beneath it:\n{}", view );
      assert! ( view . lines () . any (
                  |l| l . contains ("(id M)")
                      && ! l . contains ("overridesHere") ),
        "M, not overridden, draws raw:\n{}", view );
      Ok (( )) }

async fn save_roundtrips_to_original_and_is_idempotent (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      install_or_swap_global_handle (
        graph_handle_from_config (config) ? );
      let (de_novo, _pids, _tree) =
        multi_root_view (
          driver, config, Some (tantivy),
          &[ ID::from ("P") ], false ) . await ?;
      let saved : String =
        save_and_rerender (&de_novo, config, driver, tantivy)
        . await ?;
      { let p_file : String = read_fixture_file (config, "P");
        assert! ( p_file . contains ("- N"),
          "P still contains N after the save:\n{}", p_file );
        assert! ( ! p_file . contains ("- R"),
          "P must NOT have been rewritten to contain R:\n{}",
          p_file ); }
      { let r_file : String = read_fixture_file (config, "R");
        assert! ( r_file . contains ("- W"),
          "R's own contains is untouched:\n{}", r_file ); }
      assert_eq! ( marked_lines (&saved, "N") . len (), 1,
        "the rerendered saved view still draws marked R:\n{}",
        saved );
      let saved_again : String =
        save_and_rerender (&saved, config, driver, tantivy)
        . await ?;
      assert_eq! ( saved, saved_again,
        "a second save is a noop (idempotence through the \
         collected-ID orderkey)" );
      Ok (( )) }

async fn extraction_honors_the_marker (
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      install_or_swap_global_handle (
        graph_handle_from_config (config) ? );
      { // A marked child collects its original: P's collected
        // contains equals disk ([N, M]), so the noop filter drops
        // P's instruction entirely -- the round-trip in its
        // strongest form.
        let buffer = indoc! {"
          * (skg (node (id P) (source main))) P
          ** (skg (node (id R) (source main) (viewStats (overridesHere N)) indef)) R
          ** (skg (node (id M) (source main) indef)) M
        "};
        assert! (
          opt_saved_node_by_id (
            & define_nodes_from (buffer, config, driver) . await ?,
            "P" ) . is_none (),
          "P's contains is unchanged (N, M), so no instruction \
           touches P" ); }
      { // Deleting the drawn child deletes the original member.
        let buffer = indoc! {"
          * (skg (node (id P) (source main))) P
          ** (skg (node (id M) (source main) indef)) M
        "};
        assert_eq! (
          members_of ( & saved_node_by_id (
            & define_nodes_from (buffer, config, driver) . await ?,
            "P" ) . contains ),
          vec![ ID::from ("M") ] ); }
      { // Reordering the drawn child positions the original.
        let buffer = indoc! {"
          * (skg (node (id P) (source main))) P
          ** (skg (node (id M) (source main) indef)) M
          ** (skg (node (id R) (source main) (viewStats (overridesHere N)) indef)) R
        "};
        assert_eq! (
          members_of ( & saved_node_by_id (
            & define_nodes_from (buffer, config, driver) . await ?,
            "P" ) . contains ),
          vec![ ID::from ("M"), ID::from ("N") ] ); }
      { // Moving the drawn child to another parent moves the original.
        let buffer = indoc! {"
          * (skg (node (id Q) (source main))) Q
          ** (skg (node (id P) (source main))) P
          *** (skg (node (id M) (source main) indef)) M
          ** (skg (node (id P2) (source main))) P2
          *** (skg (node (id R) (source main) (viewStats (overridesHere N)) indef)) R
        "};
        let instructions : Vec<DefineNode> =
          define_nodes_from (buffer, config, driver) . await ?;
        assert_eq! (
          members_of ( & saved_node_by_id (&instructions, "P") . contains ),
          vec![ ID::from ("M") ] );
        assert_eq! (
          members_of ( & saved_node_by_id (&instructions, "P2") . contains ),
          vec![ ID::from ("N") ] ); }
      { // Edits to the drawn R save to R; N is untouched.
        let buffer = indoc! {"
          * (skg (node (id P) (source main))) P
          ** (skg (node (id R) (source main) (viewStats (overridesHere N)))) R-edited
          *** (skg (node (id W) (source main) indef)) W
          ** (skg (node (id M) (source main) indef)) M
        "};
        let instructions : Vec<DefineNode> =
          define_nodes_from (buffer, config, driver) . await ?;
        assert_eq! (
          saved_node_by_id (&instructions, "R") . title,
          "R-edited" );
        assert! (
          opt_saved_node_by_id (&instructions, "N") . is_none (),
          "no instruction touches N" );
        assert! (
          opt_saved_node_by_id (&instructions, "P") . is_none (),
          "P's contains is unchanged, so P noops" ); }
      { // A legacy buffer drawing N raw still saves identically
        // (a noop, like the marked equivalent above).
        let buffer = indoc! {"
          * (skg (node (id P) (source main))) P
          ** (skg (node (id N) (source main) indef)) N
          ** (skg (node (id M) (source main) indef)) M
        "};
        assert! (
          opt_saved_node_by_id (
            & define_nodes_from (buffer, config, driver) . await ?,
            "P" ) . is_none () ); }
      { // Tamper: a marker the server would not have drawn aborts.
        let buffer = indoc! {"
          * (skg (node (id P) (source main))) P
          ** (skg (node (id M) (source main) (viewStats (overridesHere W)) indef)) M
        "};
        match define_nodes_from (buffer, config, driver) . await {
          Err (SaveError::BufferValidationErrors { errors, .. }) => {
            assert! ( errors . iter () . any ( |e| matches! (
              e, BufferValidationError::OverridesHere_Mismatch (..) )),
              "expected OverridesHere_Mismatch, got: {:?}", errors ); },
          other => panic! (
            "tampered marker must abort the save; got: {:?}",
            other . map ( |v| v . len () ) ), }}
      { // Subscribee-as-such: the visible-content signal speaks of
        // the original, so no phantom hide of N is inferred.
        let buffer = indoc! {"
          * (skg (node (id S) (source main))) S
          ** (skg subscribeeCol)
          *** (skg (node (id E) (source main))) E
          **** (skg (node (id R) (source main) (viewStats (overridesHere N)) indef)) R
        "};
        let instructions : Vec<DefineNode> =
          define_nodes_from (buffer, config, driver) . await ?;
        if let Some (s_node) = opt_saved_node_by_id (&instructions, "S") {
          assert! (
            ! s_node . hides_from_its_subscriptions . or_default ()
              . iter () . any ( |m| &m . member == &ID::from ("N") ),
            "S must not hide N: the drawn R stands for N" ); }}
      Ok (( )) }

async fn diff_mode_disables_substitution (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      install_or_swap_global_handle (
        graph_handle_from_config (config) ? );
      let (view, _pids, _tree) =
        multi_root_view (
          driver, config, Some (tantivy),
          &[ ID::from ("P") ], true ) . await ?; // diff mode
      assert! ( marked_lines (&view, "N") . is_empty (),
        "diff surfaces show raw graph facts; no substitution:\n{}",
        view );
      assert! ( view . lines () . any (
                  |l| l . contains ("(id N)") ),
        "N draws raw in diff mode:\n{}", view );
      Ok (( )) }

/// A view already containing a drawn substitute (R marked
/// '(overridesHere N)') is shape-stable across a diff-mode toggle:
/// the rerender keeps the marked child (creation-only substitution;
/// the reconciler matches by collected ID), creates no duplicate raw
/// N and no phantom for N
/// (TODO/full-schema/12-2_diff-mode-policy_discussion.org).
async fn marked_view_is_shape_stable_across_diff_toggle (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      install_or_swap_global_handle (
        graph_handle_from_config (config) ? );
      { // git-init the temp fixture copy, so the toggle's diff is
        // real (and clean: HEAD == worktree).
        let source_path : &std::path::Path =
          & config . sources . values () . next () . unwrap () . path;
        let repo : git2::Repository =
          git2::Repository::init (source_path) ?;
        { let mut git_config : git2::Config = repo . config () ?;
          git_config . set_str ("user.email", "test@test.invalid") ?;
          git_config . set_str ("user.name", "skg tests") ?; }
        let mut index : git2::Index = repo . index () ?;
        index . add_all (
          ["*.skg"] . iter (), git2::IndexAddOption::DEFAULT, None ) ?;
        index . write () ?;
        let tree_id : git2::Oid = index . write_tree () ?;
        let tree : git2::Tree = repo . find_tree (tree_id) ?;
        let sig : git2::Signature = repo . signature () ?;
        repo . commit (
          Some ("HEAD"), &sig, &sig, "baseline", &tree, &[] ) ?; }
      let graph : InRustGraphHandle =
        graph_handle_from_config (config) ?;
      let env : skg::types::env::SkgEnv =
        skg::test_utils::skg_env_from_parts (
          config, Arc::clone (driver), tantivy, &graph );
      let mut views_state : ViewsState =
        ViewsState {
          diff_mode_enabled : false,
          open_views        : OpenViews::new (), };
      let before : String = {
        let (view, pids, tree) =
          multi_root_view (
            driver, config, None,
            &[ ID::from ("P") ], false ) . await ?;
        views_state . open_views . register_view (
          skg::types::views_state::ViewUri::ContentView (
            "toggle-subst-uuid" . to_string ()),
          tree, &pids );
        view };
      assert_eq! ( marked_lines (&before, "N") . len (), 1,
        "precondition: P's view draws marked R for N:\n{}", before );
      let connected_tcp_stream_pair = || -> (TcpStream, TcpStream) {
        let listener : std::net::TcpListener =
          std::net::TcpListener::bind ("127.0.0.1:0") . unwrap ();
        let client : TcpStream =
          TcpStream::connect (listener . local_addr () . unwrap ())
          . unwrap ();
        let (server, _) = listener . accept () . unwrap ();
        (server, client) };
      let toggle = |views_state : &mut ViewsState| -> String {
        let (mut server, client) =
          connected_tcp_stream_pair ();
        std::thread::scope ( |scope| {
          scope . spawn ( || {
            skg::serve::handlers::rerender_all_views::handle_git_diff_toggle_and_rerender (
              &mut server, &env, views_state,
              & ActiveSourceSet::named (
                  config, SourceSetName ("all" . to_string ()))
                . expect ("set all resolves") ); } ); } );
        drop (server);
        let mut reader : std::io::BufReader<TcpStream> =
          std::io::BufReader::new (client);
        let mut rerendered : Option<String> = None;
        while let Ok (m) =
          skg::test_utils::read_lp_message (&mut reader) {
          if m . contains ("rerender-view")
             && m . contains ("toggle-subst-uuid") {
            rerendered = Some (m); }}
        rerendered . expect ("the registered view rerenders") };
      let shape = | buf : &str | -> Vec<(usize, String)> {
        // (depth, id) per headline carrying an id; decoration-blind.
        buf . lines ()
          . filter_map ( |l| {
              let depth : usize =
                l . chars () . take_while ( |c| *c == '*' ) . count ();
              l . find ("(id ")
                . map ( |start| ( depth,
                    l [start + 4 ..]
                      . chars ()
                      . take_while ( |c| *c != ')' )
                      . collect () )) } )
          . collect () };
      let content_of = | msg : &str | -> String {
        let start : usize =
          msg . find ("(content \"")
          . expect ("rerender-view carries content")
          + "(content \"" . len ();
        let end : usize =
          msg . rfind ("\"))")
          . expect ("rerender-view content terminates");
        msg [start .. end] . to_string () };
      let before_shape : Vec<(usize, String)> = shape (&before);
      let assert_stable = | view : &str, when : &str | {
        assert_eq! ( shape (view), before_shape,
          "{}: same nodes at the same depths -- the marked child \
           is kept, no duplicate raw N, no phantom:\n{}",
          when, view );
        assert_eq! ( marked_lines (view, "N") . len (), 1,
          "{}: exactly one marked substitute survives:\n{}",
          when, view );
        assert! ( ! view . contains ("removedM"),
          "{}: no phantom -- P's contains still names N, and the \
           marked R collects N:\n{}", when, view ); };
      { let on : String = toggle (&mut views_state);
        assert! ( views_state . diff_mode_enabled );
        assert_stable ( & content_of (&on), "diff on" ); }
      { let off : String = toggle (&mut views_state);
        assert! ( ! views_state . diff_mode_enabled );
        assert_stable ( & content_of (&off), "diff off" ); }
      Ok (( )) }

async fn ownership_and_visibility_gate_substitution (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
      install_or_swap_global_handle (
        graph_handle_from_config (config) ? );
      { // A foreign overrider never substitutes.
        let (view, _pids, _tree) =
          multi_root_view (
            driver, config, None,
            &[ ID::from ("P1") ], false ) . await ?;
        assert! ( marked_lines (&view, "N1") . is_empty (),
          "FR is foreign; N1 draws raw:\n{}", view );
        assert! ( view . contains ("(id N1)"), "{}", view ); }
      let active : ActiveSourceSet =
        ActiveSourceSet::named (
          config, SourceSetName ("main" . to_string ())) ?;
      { // An inactive owned overrider does not substitute.
        let (view, _pids, _tree) =
          multi_root_view_with_source_set (
            driver, config, None,
            &[ ID::from ("P2") ], false, &active ) . await ?;
        assert! ( marked_lines (&view, "N2") . is_empty (),
          "R2's source is inactive; N2 draws raw:\n{}", view );
        assert! ( view . contains ("(id N2)"), "{}", view ); }
      { // The same overrider substitutes when its source is active.
        let (view, _pids, _tree) =
          multi_root_view (
            driver, config, None,
            &[ ID::from ("P2") ], false ) . await ?;
        let marked : Vec<&str> = marked_lines (&view, "N2");
        assert_eq! ( marked . len (), 1, "{}", view );
        assert! ( marked [0] . contains ("(id R2)"), "{}", view ); }
      { // Omission beats substitution: inactive original, active
        // overrider -> neither is drawn.
        let (view, _pids, _tree) =
          multi_root_view_with_source_set (
            driver, config, None,
            &[ ID::from ("P3") ], false, &active ) . await ?;
        assert! ( ! view . contains ("(id N3)"),
          "the inactive original is omitted:\n{}", view );
        assert! ( ! view . contains ("(id R3)"),
          "its overrider must not be drawn in its place (the \
           marker would name an inactive node):\n{}", view ); }
      Ok (( )) }

/// Save 'buf' under a specific active source-set (the test shims save
/// under 'all'). Asserts no save errors and returns the rerendered
/// view.
async fn save_under_set (
  buf     : &str,
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
  set     : &ActiveSourceSet,
) -> Result<String, Box<dyn Error>> {
  let graph : InRustGraphHandle =
    graph_handle_from_config (config) ?;
  let mut env : skg::types::env::SkgEnv =
    skg::test_utils::skg_env_from_parts (
      config, Arc::clone (driver), tantivy, &graph );
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false,
    open_views        : OpenViews::new (), };
  let listener : std::net::TcpListener =
    std::net::TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let mut stream : TcpStream =
    TcpStream::connect (listener . local_addr () . unwrap ()) . unwrap ();
  let response =
    skg::serve::handlers::save_buffer::update_from_and_rerender_buffer (
      &mut stream, buf, &mut env, false,
      &Err (String::new ()), &mut views_state,
      Some (set), true,
      &std::collections::HashMap::new () ) . await ?;
  assert! ( response . errors . is_empty (),
    "save must not error; got: {:?}", response . errors );
  Ok ( response . saved_view ) }

/// A half-visible user-owned chain: D overrides C overrides N, with the
/// end D in source 'other'. Under 'all' the END D substitutes for N;
/// under 'main' (hiding 'other') the MIDDLE C substitutes. Saving the
/// half-visible view accepts the middle carrier (it is on N's chain)
/// and keeps N in P's contains; an off-chain marker is rejected.
async fn chain_half_visible_keeps_the_original (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      install_or_swap_global_handle (
        graph_handle_from_config (config) ? );
      { // Under 'all', the chain end D is drawn in N's place.
        let (view, _p, _t) =
          multi_root_view (
            driver, config, Some (tantivy),
            &[ ID::from ("P") ], false ) . await ?;
        let marked : Vec<&str> = marked_lines (&view, "N");
        assert_eq! ( marked . len (), 1,
          "one substitute for N under all:\n{}", view );
        assert! ( marked [0] . contains ("(id D)"),
          "the chain end D is drawn under all:\n{}", view ); }
      let main_set : ActiveSourceSet =
        ActiveSourceSet::named (
          config, SourceSetName ("main" . to_string ())) ?;
      let view_main : String = {
        // Under 'main', D's source 'other' is inactive, so the MIDDLE
        // C is drawn instead.
        let (view, _p, _t) =
          multi_root_view_with_source_set (
            driver, config, Some (tantivy),
            &[ ID::from ("P") ], false, &main_set ) . await ?;
        let marked : Vec<&str> = marked_lines (&view, "N");
        assert_eq! ( marked . len (), 1,
          "one substitute for N under main:\n{}", view );
        assert! ( marked [0] . contains ("(id C)"),
          "the chain middle C is drawn under main:\n{}", view );
        view };
      { // Saving the half-visible view accepts the middle carrier and
        // keeps N (not C) in P's contains.
        let saved : String =
          save_under_set (&view_main, config, driver, tantivy, &main_set)
          . await ?;
        assert_eq! ( marked_lines (&saved, "N") . len (), 1,
          "the rerendered saved view still draws a marked substitute \
           for N:\n{}", saved );
        let p_file : String = {
          let main_path : &std::path::Path =
            & config . sources
              . get ( &SourceName::from ("main") ) . unwrap () . path;
          std::fs::read_to_string ( main_path . join ("P.skg") )
            . unwrap () };
        assert! ( p_file . contains ("- N"),
          "P still contains N after the half-visible save:\n{}", p_file );
        assert! ( ! p_file . contains ("- C"),
          "P must NOT be rewritten to contain the carrier C:\n{}",
          p_file ); }
      { // A marker on a node NOT on N's chain is rejected.
        let buffer = indoc! {"
          * (skg (node (id P) (source main))) P
          ** (skg (node (id D) (source other) (viewStats (overridesHere P)) indef)) D
        "};
        match define_nodes_from (buffer, config, driver) . await {
          Err (SaveError::BufferValidationErrors { errors, .. }) =>
            assert! ( errors . iter () . any ( |e| matches! (
              e, BufferValidationError::OverridesHere_Mismatch (..) )),
              "off-chain marker must abort; got: {:?}", errors ),
          other => panic! (
            "off-chain marker must abort the save; got: {:?}",
            other . map ( |v| v . len () ) ), }}
      Ok (( )) }
