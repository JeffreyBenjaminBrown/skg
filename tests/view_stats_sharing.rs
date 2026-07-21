// cargo nextest run --test grouped_overrides -E 'test(view_stats_sharing::)'
//
// The position-relative sharing view-stats
// (TODO/full-schema/10_heralds-and-stats.org):
// - grandparentOverrides ("gO"): a subscribee-as-such whose col owner
//   also overrides it;
// - grandparentSubscribes ("gS"): an overriddenCol member whose col
//   owner also subscribes to it;
// - overridesParent ("Op"): a node drawn under a gnode it overrides.
// One fixture serves all three: R subscribes to E and F and overrides
// E (so E under R's subscribeeCol carries gO and under R's
// overriddenCol carries gS, while F carries neither); R contains P,
// P contains C, and C overrides P (so C as content of P carries Op,
// and C as a view root does not). Each stat round-trips through a
// save (the viewStats parser errors on unknown atoms, so this also
// pins the parse arms).
//
// PITFALL: these stats read the process-global in-Rust graph
// snapshot; each sub-test installs its own fixture graph via
// install_or_swap_global_handle.

use std::error::Error;
use std::net::TcpStream;
use std::sync::Arc;

use skg::dbs::in_rust_graph::install_or_swap_global_handle;
use skg::test_utils::{run_with_shared_test_db, graph_handle_from_config};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::to_org::render::content_view::multi_root_view;
use skg::serve::ViewsState;
use skg::types::views_state::OpenViews;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};

use skg::dbs::in_rust_graph::InRustGraphHandle;
use typedb_driver::TypeDBDriver;

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str = "tests/view_stats_sharing/fixtures";
  run_with_shared_test_db (
    "skg-test-view-stats-sharing",
    |s| Box::pin ( async move {
      s . reset ("sharing_view_stats_appear_and_roundtrip", fixtures) . await ?;
      sharing_view_stats_appear_and_roundtrip (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("overridesParent_is_position_relative", fixtures) . await ?;
      overridesParent_is_position_relative (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

fn lines_containing<'a> (
  buf      : &'a str,
  fragment : &str,
) -> Vec<&'a str> {
  buf . lines ()
    . filter ( |l| l . contains (fragment) )
    . collect () }

/// The col (by its metadata atom) most recently opened above each
/// E line. The fixture draws E only as a col member, so this
/// attributes each E copy to its col regardless of col order.
fn e_lines_by_enclosing_col (
  buf : &str,
) -> Vec<(&'static str, &str)> {
  let mut current_col : &'static str = "";
  let mut result : Vec<(&'static str, &str)> = Vec::new ();
  for line in buf . lines () {
    if line . contains ("subscribeeCol)") { current_col = "subscribeeCol"; }
    else if line . contains ("overriddenCol)") { current_col = "overriddenCol"; }
    else if line . contains ("(id E)") {
      result . push ((current_col, line)); }}
  result }

fn assert_sharing_stats_in_view_of_R (
  buf   : &str,
  label : &str,
) {
  let e_lines : Vec<(&'static str, &str)> =
    e_lines_by_enclosing_col (buf);
  assert_eq! ( e_lines . len (), 2,
    "{}: E should appear under both the subscribeeCol and the \
     overriddenCol:\n{}", label, buf );
  // In the SEMANTIC wire the parent-relative stats are relationships to
  // E's grandparent R (generation 2): R both subscribes to and overrides
  // E. Under a col, the COL relation is E's reason-for-being (birth); the
  // OTHER (the old gO/gS sharing stat) shows as an ordinary relation.
  // Both carry (ancestors 2). The cols differ by which relation is birth.
  for (col, line) in &e_lines {
    match *col {
      "subscribeeCol" => {
        assert! ( line . contains ("(birth subscribes)"),
          "{}: E-as-subscribee is born of the subscribe:\n{}", label, buf );
        assert! ( line . contains ("(overrides")
                  && line . contains ("(ancestors 2)"),
          "{}: E-as-subscribee also shows gO (R overrides E, gen 2):\n{}",
          label, buf ); },
      "overriddenCol" => {
        assert! ( line . contains ("(birth overrides)"),
          "{}: E-as-overridden is born of the override:\n{}", label, buf );
        assert! ( line . contains ("(subscribes")
                  && line . contains ("(ancestors 2)"),
          "{}: E-as-overridden also shows gS (R subscribes E, gen 2):\n{}",
          label, buf ); },
      other => panic! (
        "{}: E drawn outside any col ({:?}):\n{}", label, other, buf ),
    }}
  for f_line in lines_containing (buf, "(id F)") {
    // F is a subscribee R does NOT override, so it has no overrides
    // relation at all -- only its col subscribe (its reason-for-being).
    assert! ( ! f_line . contains ("(overrides"),
      "{}: F, a subscribee R does not override, shows no override \
       relation:\n{}", label, buf ); }
  { // Since override substitution (plan 11), a fresh view of R draws
    // C in place of P (C overrides P), marked; the Op position (C
    // under a drawn P) is asserted from a view of P instead, in
    // assert_op_in_view_of_P.
    let c_lines : Vec<&str> = lines_containing (buf, "(id C)");
    assert! ( c_lines . iter ()
              . any ( |l| l . contains ("(overridesHere P)") ),
      "{}: C is drawn in place of P, marked:\n{}", label, buf ); }}

fn assert_op_in_view_of_P (
  buf   : &str,
  label : &str,
) {
  let c_lines : Vec<&str> = lines_containing (buf, "(id C)");
  // Op ("overridesParent"): C overrides its visible parent (a = P), so
  // it is born of an overrides-OUT relation to generation 1.
  assert! ( c_lines . iter ()
            . any ( |l| l . contains ("(overrides (out")
                        && l . contains ("(ancestors 1)") ),
    "{}: C drawn as content of P (which it overrides) is born of an \
     overrides-out to P (gen 1):\n{}", label, buf ); }

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

async fn sharing_view_stats_appear_and_roundtrip (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      install_or_swap_global_handle (
        graph_handle_from_config (config) ? );
      let (de_novo, _pids, _tree)
        : (String, Vec<ID>, _) =
        multi_root_view (
          driver, config, Some (tantivy),
          &[ ID ("R" . to_string ()) ], false ) . await ?;
      assert_sharing_stats_in_view_of_R (&de_novo, "de novo");
      let saved : String = // The save parses the buffer, so this also pins the parse arms.
        save_and_rerender (&de_novo, config, driver, tantivy) . await ?;
      assert_sharing_stats_in_view_of_R (&saved, "after save");
      { // Op needs P drawn as a parent, which substitution prevents
        // in a view of R; a view rooted at P draws P raw (roots
        // never substitute) with C as its content.
        let (view_of_p, _pids, _tree)
          : (String, Vec<ID>, _) =
          multi_root_view (
            driver, config, Some (tantivy),
            &[ ID ("P" . to_string ()) ], false ) . await ?;
        assert_op_in_view_of_P (&view_of_p, "de novo, view of P");
        let saved_p : String =
          save_and_rerender (&view_of_p, config, driver, tantivy)
          . await ?;
        assert_op_in_view_of_P (&saved_p, "after save, view of P"); }
      Ok (( )) }

async fn overridesParent_is_position_relative (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      install_or_swap_global_handle (
        graph_handle_from_config (config) ? );
      let (view_of_c, _pids, _tree)
        : (String, Vec<ID>, _) =
        multi_root_view (
          driver, config, Some (tantivy),
          &[ ID ("C" . to_string ()) ], false ) . await ?;
      let c_root_line : &str =
        view_of_c . lines ()
        . find ( |l| l . starts_with ("* ") && l . contains ("(id C)") )
        . expect ("view of C should have C as its root");
      assert! ( ! c_root_line . contains ("overridesParent"),
        "C as a view root has no visible parent to override:\n{}",
        view_of_c );
      Ok (( )) }
