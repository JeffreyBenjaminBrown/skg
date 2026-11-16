// cargo nextest run --test grouped_forks -E 'test(forkee_as_such_extension::)'
//
// Extending a forkee as such, on the R/E/G/H example: R
// forks E, R hides H, E contains G and H.
// Extended with X forking G, so substitution below the
// forkee as such is observable: extending E under the
// forksFolder applies R's hides (no H) and the general
// substitution rule (G is shown as X, marked). The folder member
// itself (E) is shown raw: folders never substitute.

use skg::test_utils::save_and_rerender;
use std::error::Error;

use skg::metadata_canon::without_viewnode_ids;
use skg::test_utils::{run_with_shared_test_stores, graph_handle_from_config};
use skg::test_utils::multi_root_view;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};


#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str = "tests/forkee_as_such_extension/fixtures";
  run_with_shared_test_stores (
    "skg-test-forkee-as-such-extension",
    |s| Box::pin ( async move {
      s . reset ("forkee_as_such_extension_hides_and_substitutes", fixtures) ?;
      forkee_as_such_extension_hides_and_substitutes (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("folder_members_never_substitute", fixtures) ?;
      folder_members_never_substitute (
        &s . config, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

/// The lines of 'buf', each tagged with its nearest enclosing folder
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
      let line_without_viewnode_id : String = without_viewnode_ids (line);
      if line_without_viewnode_id . contains ("(skg forksFolder)")
        { stack . push ((depth, "forksFolder")); }
      else if line_without_viewnode_id . contains ("(skg hiddenInForkPartnerFolder)")
        { stack . push ((depth, "hiddenInForkPartnerFolder")); }}}
  result }

/// Request an editable view of the E copy under 'folder' (by editing
/// the de novo view's text), save, and return the rerendered view.
async fn extend_e_under (
  folder     : &str,
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<String, Box<dyn Error>> {
  let (view_with_folder, _pids, _tree) =
    multi_root_view (
      config, Some (tantivy),
      &[ ID::from ("R") ] ) ?;
  let edited : String =
    { let mut out : Vec<String> = Vec::new ();
      for (line_col, line) in lines_by_enclosing_folder (&view_with_folder) {
        if line_col == folder && line . contains ("(gnid E)") {
          out . push ( line . replace (
            "writeProtected",
            "writeProtected (viewRequests editableView)" )); }
        else { out . push ( line . to_string () ); }}
      out . join ("\n") + "\n" };
  assert_ne! ( edited, view_with_folder,
    "the {} copy of E was found and given a request", folder );
  save_and_rerender (&edited, config, tantivy) . await }

async fn forkee_as_such_extension_hides_and_substitutes (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      (
        graph_handle_from_config (config) ? );
      let view : String =
        extend_e_under ("forksFolder", config, tantivy)
        . await ?;
      let tagged : Vec<(&str, &str)> =
        lines_by_enclosing_folder (&view);
      assert! ( tagged . iter () . any ( |(c, l)|
                  *c == "forksFolder"
                  && l . contains ("(inPlaceOf G)")
                  && l . contains ("(gnid X)") ),
        "the general rule applies below a forkee-as-such: \
         G is shown as X, marked:\n{}", view );
      assert! ( ! tagged . iter () . any ( |(c, l)|
                  *c == "forksFolder"
                  && l . contains ("(gnid H)") ),
        "R's hides apply to the forkee-as-such extension; H \
         appears only under the hiddenInForkPartnerFolder:\n{}", view );
      assert! ( tagged . iter () . any ( |(c, l)|
                  *c == "hiddenInForkPartnerFolder"
                  && l . contains ("(gnid H)") ),
        "H shows in the hiddenInForkPartnerFolder:\n{}", view );
      Ok (( )) }

async fn folder_members_never_substitute (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      (
        graph_handle_from_config (config) ? );
      let (view, _pids, _tree) =
        multi_root_view (
          config, Some (tantivy),
          &[ ID::from ("R") ] ) ?;
      // R forks E, yet the folder copy of E is shown raw: the
      // forksFolder shows the graph fact "R forks E";
      // substituting inside it would obscure the fact displayed.
      let e_lines : Vec<&str> =
        view . lines ()
        . filter ( |l| l . contains ("(gnid E)") )
        . collect ();
      assert_eq! ( e_lines . len (), 1,
        "E appears under the forks folder:\n{}", view );
      assert! ( e_lines . iter () . all (
                  |l| ! l . contains ("inPlaceOf") ),
        "it is not substituted:\n{}", view );
      Ok (( )) }
