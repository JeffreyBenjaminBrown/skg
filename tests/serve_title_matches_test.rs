// cargo test --test serve_title_matches_test

use skg::context::ContextOriginType;
use skg::dbs::tantivy::{search_index, update_context_origin_types};
use skg::org_to_text::viewnode_forest_to_string;
use skg::types::misc::{ID, MSV, TantivyIndex};
use skg::types::skgnode::{SkgNode, empty_skgnode};
use skg::dbs::init::in_fs_wipe_index_then_create_it;
use skg::serve::handlers::title_matches::{
  group_matches_by_id, build_search_forest, SearchScope};

use std::collections::HashMap;
use std::path::Path;
use std::fs;

#[test]
fn test_title_matches_org_format (
) -> Result < (), Box < dyn std::error::Error >> {
  // This test verifies that the title matches response
  // returns org-mode formatted results with grouped IDs.

  /* PITFALL: This is easiest to understand by reading just the definitions of the nodes and the assertions. The logic between those two portions, though necessary for *running* the test, is not particularly helpful for understanding the test. */

  let index_dir : &str =
    "tests/serve_title_matches_test/temp_index";

  // Test logic. (Cleanup follows it.)
  let test_result
    : Result < (), Box < dyn std::error::Error >>
    = ( || {

      // Create test nodes with overlapping titles/aliases
      let mut node1 : SkgNode = // the best match
        empty_skgnode ();
      node1 . ids =
        vec! [ ID::new ("id_1") ];
      node1 . title =
        "the bear eats cheese" . to_string ();
      node1 . aliases =
        MSV::Specified ( vec! [
          "bear cheese" . to_string (),
          "the cheese" . to_string ()
        ] );

      let mut node2 : SkgNode = // matches, but less well
        empty_skgnode ();
      node2 . ids =
        vec! [ ID::new ("id_2") ];
      node2 . title =
        "cheese makes me happy" . to_string ();

      let mut node3 : SkgNode = // will not match
        empty_skgnode ();
      node3 . ids =
        vec! [ ID::new ("id_3") ];
      node3 . title =
        "aliens evade silently" . to_string ();

      let nodes : Vec < SkgNode > =
        vec! [ node1, node2, node3 ];

      let ( tantivy_index, _ ) : ( TantivyIndex, usize ) =
        in_fs_wipe_index_then_create_it (
          &nodes,
          Path::new (index_dir) ) ?;
      let search_terms : &str =
        "the bear eats cheese";
      let ( best_matches, searcher ) =
        search_index ( &tantivy_index, search_terms ) ?;
      let matches_by_id =
        group_matches_by_id (
          best_matches, searcher, &tantivy_index,
          &SearchScope::Everything );
      let (forest, _result_ids) =
        build_search_forest (
          search_terms,
          &matches_by_id );
      let result : String =
        viewnode_forest_to_string ( &forest )
        . expect ("search forest rendering never fails");
      let lines : Vec < &str > =
        result . lines () . collect ();

      // Extract just the links from each line
      fn extract_link ( line: &str
      ) -> Option < (String, String) > {
        if let Some (start) = line . find ("[[id:") {
          let link_part : &str =
            &line [ start.. ];
          if let Some (id_end) = link_part . find ("][") {
            let id : String =
              link_part [ 5..id_end ] . to_string ();
            if let Some (title_end) = link_part . find ("]]") {
              let title : String =
                link_part [ id_end + 2..title_end ] . to_string ();
              return Some ( (id, title) ); }} }
        None }

      // Extract alias lines (level-4 "skg alias" entries)
      fn extract_alias ( line: &str ) -> Option < String > {
        if line . starts_with ("**** (skg alias) ") {
          Some ( line [ "**** (skg alias) " . len () ..  ]
                 . to_string () )
        } else { None } }

      // Find level-2 headlines (start with "** ")
      let mut level2_headlines : Vec < (String, String) > =
        Vec::new ();
      let mut aliases_under_current : Vec < String > =
        Vec::new ();
      let mut all_alias_groups : Vec < Vec < String > > =
        Vec::new ();

      for line in &lines {
        if line . starts_with ("** ") {
          if ! aliases_under_current . is_empty () {
            // Save any accumulated aliases
            all_alias_groups . push (
              aliases_under_current . clone () );
            aliases_under_current . clear (); }
          if let Some (link) = extract_link (line) {
            // Extract this level-2 headline
            level2_headlines . push (link); }
        } else if let Some (alias) = extract_alias (line) {
          // Extract alias
          aliases_under_current . push (alias); } }
      if ! aliases_under_current . is_empty () {
        // Don't forget the last group
        all_alias_groups . push (aliases_under_current); }

      // Verify the structure
      assert_eq! ( level2_headlines . len (), 2,
                   "Should have exactly 2 level-2 headlines" );

      // First level-2 should be "the bear eats cheese" with id_1
      assert_eq! ( level2_headlines [ 0 ] . 0, "id_1",
                   "First level-2 should have id_1" );
      assert_eq! ( level2_headlines [ 0 ] . 1, "the bear eats cheese",
                   "First level-2 should be 'the bear eats cheese'" );

      // Second level-2 should be "cheese makes me happy" with id_2
      assert_eq! ( level2_headlines [ 1 ] . 0, "id_2",
                   "Second level-2 should have id_2" );
      assert_eq! ( level2_headlines [ 1 ] . 1, "cheese makes me happy",
                   "Second level-2 should be 'cheese makes me happy'" );

      // First level-2 should have 2 alias children
      assert_eq! ( all_alias_groups . len (), 1,
                   "Should have exactly 1 group of aliases (second level-2 should have none)" );
      assert_eq! ( all_alias_groups [ 0 ] . len (), 2,
                   "First level-2 should have exactly 2 alias children" );

      // Verify the alias texts under first level-2
      assert! ( all_alias_groups [ 0 ] . contains (
                  &"bear cheese" . to_string () ),
                "Should have 'bear cheese' as an alias" );
      assert! ( all_alias_groups [ 0 ] . contains (
                  &"the cheese" . to_string () ),
                "Should have 'the cheese' as an alias" );

      // --- origins_only filtering ---
      // Stamp id_1 as Root; id_2 has no origin type.
      let mut context_types : HashMap < ID, String > =
        HashMap::new ();
      context_types . insert (
        ID::new ("id_1"),
        ContextOriginType::Root . label () . to_string () );
      update_context_origin_types (
        &tantivy_index, &context_types ) ?;

      let ( best_matches_2, searcher_2 ) =
        search_index ( &tantivy_index, search_terms ) ?;
      let matches_filtered =
        group_matches_by_id (
          best_matches_2, searcher_2, &tantivy_index,
          &SearchScope::Rooty );
      assert_eq! ( matches_filtered . len (), 1,
                   "origins_only should return only the Root node" );
      assert! ( matches_filtered . contains_key ( &ID::new ("id_1") ),
                "origins_only should keep id_1 (Root)" );

      println! ("✓ Org-mode format verified successfully");
      Ok ( () )
    }) ();

  // Clean up or preserve based on test result
  match test_result {
    Ok ( () ) => {
      // Test passed, clean up
      if Path::new (index_dir) . exists () {
        fs::remove_dir_all (index_dir) ?;
      }
      Ok ( () )
    },
    Err (e) => {
      // Test failed, preserve directory
      println! ( "Test failed. Preserving index directory at {} for debugging.", index_dir );
      Err (e)
    }
  }
}
