// cargo nextest run --test grouped_overrides -E 'test(overridden_as_such::)'
//
// The overridden-as-such exception
// (TODO/full-schema/11_override-rendering-and-navigation.org), on
// vision.org's R/E/G/H example: R subscribes to AND overrides E,
// R hides H, E contains G and H. Extended with X overriding G, so
// the no-cascade decision is observable:
// - expanding E under the subscribeeCol applies R's hides (no H)
//   and the GENERAL substitution rule (G draws as X, marked);
// - expanding E under the overriddenCol applies neither: full
//   contains, raw G and raw H -- the user asked for the original.
// The col members themselves (each E copy) always draw raw: cols
// never substitute.
//
// PITFALL: marked buffers hit the tamper check, which reads the
// process-global in-Rust graph; each sub-test installs its own
// fixture graph via install_or_swap_global_handle.

use std::error::Error;
use std::net::TcpStream;
use std::sync::Arc;

use skg::serve::ViewsState;
use skg::dbs::in_rust_graph::install_or_swap_global_handle;
use skg::test_utils::{run_with_shared_test_db, graph_handle_from_config};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::to_org::render::content_view::multi_root_view;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};
use skg::types::views_state::OpenViews;

use skg::dbs::in_rust_graph::InRustGraphHandle;
use typedb_driver::TypeDBDriver;

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str = "tests/overridden_as_such/fixtures";
  run_with_shared_test_db (
    "skg-test-overridden-as-such",
    |s| Box::pin ( async move {
      s . reset ("subscribee_as_such_expansion_hides_and_substitutes", fixtures) . await ?;
      subscribee_as_such_expansion_hides_and_substitutes (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("overridden_as_such_expansion_is_raw_and_unhidden", fixtures) . await ?;
      overridden_as_such_expansion_is_raw_and_unhidden (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("col_members_never_substitute", fixtures) . await ?;
      col_members_never_substitute (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

/// The lines of 'buf', each tagged with its nearest ENCLOSING col
/// (by metadata atom; "" outside any col). Depth-aware: a col stops
/// enclosing once a line at its own depth or shallower appears, so
/// a sibling following a nested col is attributed to the outer col,
/// not the nested one. Body lines (no stars) inherit the current
/// attribution.
fn lines_by_enclosing_col (
  buf : &str,
) -> Vec<(&'static str, &str)> {
  let org_depth = |line : &str| -> usize {
    line . chars () . take_while ( |c| *c == '*' ) . count () };
  let mut stack : Vec<(usize, &'static str)> = Vec::new ();
  let mut result : Vec<(&'static str, &str)> = Vec::new ();
  for line in buf . lines () {
    let depth : usize = org_depth (line);
    if depth > 0 {
      while stack . last ()
        . map_or (false, |(col_depth, _)| *col_depth >= depth)
      { stack . pop (); }}
    result . push ((
      stack . last () . map ( |(_, c)| *c ) . unwrap_or (""),
      line ));
    if depth > 0 {
      if line . contains ("(skg subscribeeCol)")
        { stack . push ((depth, "subscribeeCol")); }
      else if line . contains ("(skg overriddenCol)")
        { stack . push ((depth, "overriddenCol")); }
      else if line . contains ("(skg hiddenInSubscribeeCol)")
        { stack . push ((depth, "hiddenInSubscribeeCol")); }}}
  result }

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

/// Request a definitive view of the E copy under 'col' (by editing
/// the de novo view's text), save, and return the rerendered view.
async fn expand_e_under (
  col     : &str,
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<String, Box<dyn Error>> {
  let (de_novo, _pids, _tree) =
    multi_root_view (
      driver, config, Some (tantivy),
      &[ ID::from ("R") ], false ) . await ?;
  let edited : String =
    { let mut out : Vec<String> = Vec::new ();
      for (line_col, line) in lines_by_enclosing_col (&de_novo) {
        if line_col == col && line . contains ("(id E)") {
          out . push ( line . replace (
            "indef",
            "indef (viewRequests definitiveView)" )); }
        else { out . push ( line . to_string () ); }}
      out . join ("\n") + "\n" };
  assert_ne! ( edited, de_novo,
    "the {} copy of E was found and given a request", col );
  save_and_rerender (&edited, config, driver, tantivy) . await }

async fn subscribee_as_such_expansion_hides_and_substitutes (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      install_or_swap_global_handle (
        graph_handle_from_config (config) ? );
      let view : String =
        expand_e_under ("subscribeeCol", config, driver, tantivy)
        . await ?;
      let tagged : Vec<(&str, &str)> =
        lines_by_enclosing_col (&view);
      assert! ( tagged . iter () . any ( |(c, l)|
                  *c == "subscribeeCol"
                  && l . contains ("(overridesHere G)")
                  && l . contains ("(id X)") ),
        "the general rule applies below a subscribee-as-such: \
         G draws as X, marked:\n{}", view );
      assert! ( ! tagged . iter () . any ( |(c, l)|
                  *c == "subscribeeCol"
                  && l . contains ("(id H)") ),
        "R's hides apply to the subscribee-as-such expansion; H \
         appears only under the hiddenInSubscribeeCol:\n{}", view );
      assert! ( tagged . iter () . any ( |(c, l)|
                  *c == "hiddenInSubscribeeCol"
                  && l . contains ("(id H)") ),
        "H shows in the hiddenInSubscribeeCol:\n{}", view );
      Ok (( )) }

async fn overridden_as_such_expansion_is_raw_and_unhidden (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      install_or_swap_global_handle (
        graph_handle_from_config (config) ? );
      let view : String =
        expand_e_under ("overriddenCol", config, driver, tantivy)
        . await ?;
      let tagged : Vec<(&str, &str)> =
        lines_by_enclosing_col (&view);
      assert! ( tagged . iter () . any ( |(c, l)|
                  *c == "overriddenCol"
                  && l . contains ("(id G)")
                  && ! l . contains ("overridesHere") ),
        "the bypass: G draws RAW (not as X) below an \
         overridden-as-such:\n{}", view );
      assert! ( tagged . iter () . any ( |(c, l)|
                  *c == "overriddenCol"
                  && l . contains ("(id H)") ),
        "no hides apply: H draws below the overridden-as-such \
         (hides are scoped to subscriptions):\n{}", view );
      assert! ( ! view . contains ("(id X)"),
        "X appears nowhere in this expansion:\n{}", view );
      Ok (( )) }

async fn col_members_never_substitute (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      install_or_swap_global_handle (
        graph_handle_from_config (config) ? );
      let (view, _pids, _tree) =
        multi_root_view (
          driver, config, Some (tantivy),
          &[ ID::from ("R") ], false ) . await ?;
      // R overrides E, yet both col copies of E draw raw: the
      // subscribeeCol shows the graph fact "R subscribes to E" and
      // the overriddenCol shows "R overrides E"; substituting
      // inside either would obscure the fact displayed.
      let e_lines : Vec<&str> =
        view . lines ()
        . filter ( |l| l . contains ("(id E)") )
        . collect ();
      assert_eq! ( e_lines . len (), 2,
        "E appears under both cols:\n{}", view );
      assert! ( e_lines . iter () . all (
                  |l| ! l . contains ("overridesHere") ),
        "neither copy is substituted:\n{}", view );
      Ok (( )) }
