// cargo nextest run --test grouped_overrides -E 'test(overridden_as_such::)'
//
// The overridden-as-such exception
// (TODO/full-schema/11_override-rendering-and-navigation.org), on
// vision.org's R/E/G/H example: R subscribes to AND overrides E,
// R hides H, E contains G and H. Extended with X overriding G, so
// the no-cascade decision is observable:
// - expanding E under the subscribeeFolder applies R's hides (no H)
//   and the GENERAL substitution rule (G draws as X, marked);
// - expanding E under the overriddenFolder applies neither: full
//   contains, raw G and raw H -- the user asked for the original.
// The folder members themselves (each E copy) always draw raw: folders
// never substitute.

use std::error::Error;
use std::net::TcpStream;

use skg::serve::ViewsState;
use skg::test_utils::{run_with_shared_test_stores, graph_handle_from_config};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::to_org::render::content_view::multi_root_view;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};
use skg::types::views_state::OpenViews;

use skg::dbs::in_rust_graph::InRustGraphHandle;

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str = "tests/overridden_as_such/fixtures";
  run_with_shared_test_stores (
    "skg-test-overridden-as-such",
    |s| Box::pin ( async move {
      s . reset ("subscribee_as_such_expansion_hides_and_substitutes", fixtures) ?;
      subscribee_as_such_expansion_hides_and_substitutes (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("overridden_as_such_expansion_is_raw_and_unhidden", fixtures) ?;
      overridden_as_such_expansion_is_raw_and_unhidden (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("folder_members_never_substitute", fixtures) ?;
      folder_members_never_substitute (
        &s . config, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

/// The lines of 'buf', each tagged with its nearest ENCLOSING folder
/// (by metadata atom; "" outside any folder). Depth-aware: a folder stops
/// enclosing once a line at its own depth or shallower appears, so
/// a sibling following a nested folder is attributed to the outer folder,
/// not the nested one. Body lines (no stars) inherit the current
/// attribution.
fn lines_by_enclosing_folder (
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
        . map_or (false, |(folder_depth, _)| *folder_depth >= depth)
      { stack . pop (); }}
    result . push ((
      stack . last () . map ( |(_, c)| *c ) . unwrap_or (""),
      line ));
    if depth > 0 {
      if line . contains ("(skg subscribeeFolder)")
        { stack . push ((depth, "subscribeeFolder")); }
      else if line . contains ("(skg overriddenFolder)")
        { stack . push ((depth, "overriddenFolder")); }
      else if line . contains ("(skg hiddenInSubscribeeFolder)")
        { stack . push ((depth, "hiddenInSubscribeeFolder")); }}}
  result }

async fn save_and_rerender (
  buf     : &str,
  config : &SkgConfig,
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
    buf, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state ) . await ?;
  assert! ( response . errors . is_empty (),
    "save must not error; got: {:?}", response . errors );
  Ok ( response . saved_view ) }

/// Request a definitive view of the E copy under 'folder' (by editing
/// the de novo view's text), save, and return the rerendered view.
async fn expand_e_under (
  folder     : &str,
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<String, Box<dyn Error>> {
  let (de_novo, _pids, _tree) =
    multi_root_view (
      config, Some (tantivy),
      &[ ID::from ("R") ], false ) ?;
  let view_with_folder : String =
    if folder == "overriddenFolder" {
      let request : String = de_novo . replace (
        "(affectsParent na)",
        "(affectsParent na) (viewRequests (folder overrides))" );
      save_and_rerender (&request, config, tantivy) . await ?
    } else { de_novo };
  let edited : String =
    { let mut out : Vec<String> = Vec::new ();
      for (line_col, line) in lines_by_enclosing_folder (&view_with_folder) {
        if line_col == folder && line . contains ("(id E)") {
          out . push ( line . replace (
            "writeProtected",
            "writeProtected (viewRequests definitiveView)" )); }
        else { out . push ( line . to_string () ); }}
      out . join ("\n") + "\n" };
  assert_ne! ( edited, view_with_folder,
    "the {} copy of E was found and given a request", folder );
  save_and_rerender (&edited, config, tantivy) . await }

async fn subscribee_as_such_expansion_hides_and_substitutes (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      (
        graph_handle_from_config (config) ? );
      let view : String =
        expand_e_under ("subscribeeFolder", config, tantivy)
        . await ?;
      let tagged : Vec<(&str, &str)> =
        lines_by_enclosing_folder (&view);
      assert! ( tagged . iter () . any ( |(c, l)|
                  *c == "subscribeeFolder"
                  && l . contains ("(overridesHere G)")
                  && l . contains ("(id X)") ),
        "the general rule applies below a subscribee-as-such: \
         G draws as X, marked:\n{}", view );
      assert! ( ! tagged . iter () . any ( |(c, l)|
                  *c == "subscribeeFolder"
                  && l . contains ("(id H)") ),
        "R's hides apply to the subscribee-as-such expansion; H \
         appears only under the hiddenInSubscribeeFolder:\n{}", view );
      assert! ( tagged . iter () . any ( |(c, l)|
                  *c == "hiddenInSubscribeeFolder"
                  && l . contains ("(id H)") ),
        "H shows in the hiddenInSubscribeeFolder:\n{}", view );
      Ok (( )) }

async fn overridden_as_such_expansion_is_raw_and_unhidden (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      (
        graph_handle_from_config (config) ? );
      let view : String =
        expand_e_under ("overriddenFolder", config, tantivy)
        . await ?;
      let tagged : Vec<(&str, &str)> =
        lines_by_enclosing_folder (&view);
      assert! ( tagged . iter () . any ( |(c, l)|
                  *c == "overriddenFolder"
                  && l . contains ("(id G)")
                  && ! l . contains ("overridesHere") ),
        "the bypass: G draws RAW (not as X) below an \
         overridden-as-such:\n{}", view );
      assert! ( tagged . iter () . any ( |(c, l)|
                  *c == "overriddenFolder"
                  && l . contains ("(id H)") ),
        "no hides apply: H draws below the overridden-as-such \
         (hides are scoped to subscriptions):\n{}", view );
      assert! ( ! view . contains ("(id X)"),
        "X appears nowhere in this expansion:\n{}", view );
      Ok (( )) }

async fn folder_members_never_substitute (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      (
        graph_handle_from_config (config) ? );
      let (de_novo, _pids, _tree) =
        multi_root_view (
          config, Some (tantivy),
          &[ ID::from ("R") ], false ) ?;
      let request : String = de_novo . replace (
        "(affectsParent na)",
        "(affectsParent na) (viewRequests (folder overrides))" );
      let view : String =
        save_and_rerender (&request, config, tantivy) . await ?;
      // R overrides E, yet both folder copies of E draw raw: the
      // subscribeeFolder shows the graph fact "R subscribes to E" and
      // the overriddenFolder shows "R overrides E"; substituting
      // inside either would obscure the fact displayed.
      let e_lines : Vec<&str> =
        view . lines ()
        . filter ( |l| l . contains ("(id E)") )
        . collect ();
      assert_eq! ( e_lines . len (), 2,
        "E appears under both folders:\n{}", view );
      assert! ( e_lines . iter () . all (
                  |l| ! l . contains ("overridesHere") ),
        "neither copy is substituted:\n{}", view );
      Ok (( )) }
