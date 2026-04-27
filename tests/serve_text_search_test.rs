// cargo test --test serve_text_search_test

use skg::context::ContextOriginType;
use skg::dbs::tantivy::context_update::update_context_origin_types;
use skg::dbs::tantivy::search::{SearchOptions, search_index};
use skg::from_text::buffer_to_viewnodes::uninterpreted::headline_to_triple;
use skg::org_to_text::viewforest_to_string;
use skg::types::misc::{ID, MSV, SkgConfig, TantivyIndex};
use skg::types::nodes::complete::{NodeComplete, empty_node_complete};
use skg::types::viewnode::Scaffold;
use skg::dbs::init::in_fs_wipe_index_then_create_it;
use skg::serve::handlers::text_search::{
  group_matches_by_id, build_search_viewforest, SearchScope};

use std::collections::HashMap;
use std::path::Path;
use std::fs;

#[test]
fn test_text_search_org_format (
) -> Result < (), Box < dyn std::error::Error >> {
  // This test verifies that the text-search response
  // returns org-mode formatted results with grouped IDs.

  /* PITFALL: This is easiest to understand by reading just the definitions of the nodes and the assertions. The logic between those two portions, though necessary for *running* the test, is not particularly helpful for understanding the test. */

  let index_dir : &str =
    "tests/serve_text_search_test/temp_index";

  // Test logic. (Cleanup follows it.)
  let test_result
    : Result < (), Box < dyn std::error::Error >>
    = ( || {

      // Create test nodes with overlapping titles/aliases
      let mut node1 : NodeComplete = // the best match
        empty_node_complete ();
      node1 . pid =
        ID::new ("id_1");
      node1 . title =
        "the bear eats cheese" . to_string ();
      node1 . aliases =
        MSV::Specified ( vec! [
          "bear cheese" . to_string (),
          "the cheese" . to_string ()
        ] );

      let mut node2 : NodeComplete = // matches, but less well
        empty_node_complete ();
      node2 . pid =
        ID::new ("id_2");
      node2 . title =
        "cheese makes me happy" . to_string ();

      let mut node3 : NodeComplete = // will not match
        empty_node_complete ();
      node3 . pid =
        ID::new ("id_3");
      node3 . title =
        "aliens evade silently" . to_string ();

      let nodes : Vec < NodeComplete > =
        vec! [ node1, node2, node3 ];

      let ( tantivy_index, _ ) : ( TantivyIndex, usize ) =
        in_fs_wipe_index_then_create_it (
          &nodes,
          Path::new (index_dir) ) ?;
      let search_terms : &str =
        "the bear eats cheese";
      let ( best_matches, searcher ) =
        search_index ( &tantivy_index, search_terms, &SearchOptions::default () ) ?;
      let matches_by_id =
        group_matches_by_id (
          best_matches, searcher, &tantivy_index,
          &SearchScope::Everything );
      let (viewforest, _search_results) =
        build_search_viewforest (
          search_terms,
          &matches_by_id );
      let dummy_config : SkgConfig =
        SkgConfig::dummyFromSources (HashMap::new ());
      let result : String =
        viewforest_to_string ( &viewforest, &dummy_config )
        . expect ("search viewforest rendering never fails");
      let lines : Vec < &str > =
        result . lines () . collect ();

      // Parse each line with headline_to_triple.
      // Collect level-1 TrueNode headlines and alias groups.
      let mut level1_headlines : Vec < (String, String) > =
        Vec::new ();
      let mut aliases_under_current : Vec < String > =
        Vec::new ();
      let mut all_alias_groups : Vec < Vec < String > > =
        Vec::new ();

      for line in &lines {
        if let Ok (( level, metadata, title ))
          = headline_to_triple (line)
        { match level {
            1 => {
              if ! aliases_under_current . is_empty () {
                all_alias_groups . push (
                  aliases_under_current . clone () );
                aliases_under_current . clear (); }
              if let Some (md) = metadata {
                if let Some (id) = md . id {
                  level1_headlines . push (
                    ( id . to_string (), title ) ); }} },
            _ => {
              if let Some (md) = metadata {
                if let Some ( Scaffold::Alias { .. } )
                  = md . scaffold
                  { aliases_under_current . push (title); }} }, }} }
      if ! aliases_under_current . is_empty () {
        all_alias_groups . push (aliases_under_current); }

      // Verify the structure
      assert_eq! ( level1_headlines . len (), 2,
                   "Should have exactly 2 level-1 headlines" );

      // First level-1 should be "the bear eats cheese" with id_1
      assert_eq! ( level1_headlines [ 0 ] . 0, "id_1",
                   "First level-1 should have id_1" );
      assert_eq! ( level1_headlines [ 0 ] . 1, "the bear eats cheese",
                   "First level-1 should be 'the bear eats cheese'" );

      // Second level-1 should be "cheese makes me happy" with id_2
      assert_eq! ( level1_headlines [ 1 ] . 0, "id_2",
                   "Second level-1 should have id_2" );
      assert_eq! ( level1_headlines [ 1 ] . 1, "cheese makes me happy",
                   "Second level-1 should be 'cheese makes me happy'" );

      // First level-1 should have 2 alias children
      assert_eq! ( all_alias_groups . len (), 1,
                   "Should have exactly 1 group of aliases (second level-1 should have none)" );
      assert_eq! ( all_alias_groups [ 0 ] . len (), 2,
                   "First level-1 should have exactly 2 alias children" );

      // Verify the alias texts under first level-1
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
        search_index ( &tantivy_index, search_terms, &SearchOptions::default () ) ?;
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

/// Search results must show the full title text including
/// `[[id:X][label]]` syntax, not the link-stripped version that
/// Tantivy stores in `title_or_alias` for searching purposes.
/// Otherwise a node titled "[[id:X][science]]" (a textlink whose
/// label is "science") is indistinguishable from a node titled
/// just "science" in the user's results listing.
#[test]
fn test_search_results_preserve_textlinks_in_title (
) -> Result < (), Box < dyn std::error::Error >> {
  let index_dir : &str =
    "tests/serve_text_search_test/temp_index_textlinks";
  let test_result : Result < (), Box < dyn std::error::Error >>
    = ( || {
      // Two nodes; both will match a search for "science".
      // node_link's title is a textlink with label "science"; its
      // raw title carries the link syntax.
      let mut node_link : NodeComplete = empty_node_complete ();
      node_link . pid = ID::new ("link_node");
      node_link . title =
        "[[id:other][science]]" . to_string ();
      let mut node_plain : NodeComplete = empty_node_complete ();
      node_plain . pid = ID::new ("plain_node");
      node_plain . title = "science" . to_string ();
      let nodes : Vec < NodeComplete > =
        vec! [ node_link, node_plain ];
      let ( tantivy_index, _ ) : ( TantivyIndex, usize ) =
        in_fs_wipe_index_then_create_it (
          &nodes, Path::new (index_dir) ) ?;
      let ( best_matches, searcher ) =
        search_index ( &tantivy_index, "science",
                       &SearchOptions::default () ) ?;
      let matches_by_id = group_matches_by_id (
        best_matches, searcher, &tantivy_index,
        &SearchScope::Everything );
      let (viewforest, _ids) = build_search_viewforest (
        "science", &matches_by_id );
      let dummy_config : SkgConfig =
        SkgConfig::dummyFromSources (HashMap::new ());
      let result : String =
        viewforest_to_string ( &viewforest, &dummy_config )
        . expect ("search viewforest rendering never fails");
      println! ("Rendered search results:\n{}", result);

      // Pick out the level-1 result for link_node and confirm the
      // displayed title is the un-reduced form.
      let mut found_link_node_title : Option<String> = None;
      for line in result . lines () {
        if let Ok (( 1, Some (md), title )) = headline_to_triple (line) {
          if md . id . as_ref () . map ( |i| i . as_str () )
                                  == Some ("link_node") {
            found_link_node_title = Some (title); }} }
      let title : String = found_link_node_title
        . expect ("link_node should appear among results");
      assert_eq! (
        title, "[[id:other][science]]",
        "link_node's search-result title should preserve the \
         textlink syntax; the bug was rendering it as 'science'." );
      Ok (( ))
    }) ();
  match test_result {
    Ok (()) => {
      if Path::new (index_dir) . exists () {
        fs::remove_dir_all (index_dir) ?; }
      Ok (())
    },
    Err (e) => {
      println! ( "Test failed. Preserving index directory at {} for debugging.", index_dir );
      Err (e) }} }
