// cargo test --test serve_title_matches_test

use skg::types::{ID, empty_skgnode, SkgNode, TantivyIndex};
use skg::init::in_fs_wipe_index_then_create_it;
use skg::serve::title_matches::generate_title_matches_response;
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
        vec! [ ID::new ( "id_1" ) ];
      node1 . title =
        "the bear eats cheese" . to_string ();
      node1 . aliases =
        Some ( vec! [
          "bear cheese" . to_string (),
          "the cheese" . to_string ()
        ] );

      let mut node2 : SkgNode = // matches, but less well
        empty_skgnode ();
      node2 . ids =
        vec! [ ID::new ( "id_2" ) ];
      node2 . title =
        "cheese makes me happy" . to_string ();

      let mut node3 : SkgNode = // will not match
        empty_skgnode ();
      node3 . ids =
        vec! [ ID::new ( "id_3" ) ];
      node3 . title =
        "aliens evade silently" . to_string ();

      let nodes : Vec < SkgNode > =
        vec! [ node1, node2, node3 ];

      let ( tantivy_index, _ ) : ( TantivyIndex, usize ) =
        in_fs_wipe_index_then_create_it (
          &nodes,
          Path::new ( index_dir ) ) ?;
      let search_terms : &str =
        "the bear eats cheese";
      let result : String =
        generate_title_matches_response (
          search_terms,
          &tantivy_index );
      let lines : Vec < &str > =
        result . lines () . collect ();

      // Extract just the links from each line
      fn extract_link ( line: &str
      ) -> Option < (String, String) > {
        if let Some ( start ) = line . find ( "[[id:" ) {
          let link_part : &str =
            &line [ start.. ];
          if let Some ( id_end ) = link_part . find ( "][" ) {
            let id : String =
              link_part [ 5..id_end ] . to_string ();
            if let Some ( title_end ) = link_part . find ( "]]" ) {
              let title : String =
                link_part [ id_end + 2..title_end ] . to_string ();
              return Some ( (id, title) ); }} }
        None }

      // Find level-2 headlines (start with "** ")
      let mut level2_headlines : Vec < (String, String) > =
        Vec::new ();
      let mut level3_under_current : Vec < (String, String) > =
        Vec::new ();
      let mut all_level3_groups : Vec < Vec < (String, String) > > =
        Vec::new ();

      for line in lines {
        if line . starts_with ( "** " ) {
          if ! level3_under_current . is_empty () {
            // Save any accumulated level-3 headlines
            all_level3_groups . push (
              level3_under_current . clone () );
            level3_under_current . clear (); }
          if let Some ( link ) = extract_link ( line ) {
            // Extract this level-2 headline
            level2_headlines . push ( link ); }
        } else if line . starts_with ( "*** " ) {
          // Extract level-3 headline
          if let Some ( link ) = extract_link ( line ) {
            level3_under_current . push ( link ); }} }
      if ! level3_under_current . is_empty () {
        // Don't forget the last group
        all_level3_groups . push ( level3_under_current ); }

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

      // First level-2 should have 2 level-3 children
      assert_eq! ( all_level3_groups . len (), 1,
                   "Should have exactly 1 group of level-3 headlines (second level-2 should have no children)" );
      assert_eq! ( all_level3_groups [ 0 ] . len (), 2,
                   "First level-2 should have exactly 2 children" );

      // Verify the level-3 headlines under first level-2
      let level3_titles : Vec < String > =
        all_level3_groups [ 0 ] . iter () . map ( |(_, t)| t . clone () ) . collect ();
      assert! ( level3_titles . contains ( &"bear cheese" . to_string () ),
                "Should have 'bear cheese' as a level-3" );
      assert! ( level3_titles . contains ( &"the cheese" . to_string () ),
                "Should have 'the cheese' as a level-3" );

      // All level-3s should have same ID as their parent
      for (id, _) in &all_level3_groups [ 0 ] {
        assert_eq! ( id, "id_1",
                     "All level-3 headlines should have same ID as parent" );
      }

      println! ( "✓ Org-mode format verified successfully" );
      Ok ( () )
    }) ();

  // Clean up or preserve based on test result
  match test_result {
    Ok ( () ) => {
      // Test passed, clean up
      if Path::new ( index_dir ) . exists () {
        fs::remove_dir_all ( index_dir ) ?;
      }
      Ok ( () )
    },
    Err ( e ) => {
      // Test failed, preserve directory
      println! ( "Test failed. Preserving index directory at {} for debugging.", index_dir );
      Err ( e )
    }
  }
}
