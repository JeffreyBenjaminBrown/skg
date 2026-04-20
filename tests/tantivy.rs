// cargo test --test tantivy

use std::path::Path;
use std::collections::HashMap;
use tantivy::schema as schema;
use tantivy::TantivyDocument;
use tantivy::schema::document::Value;
use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use skg::dbs::filesystem::not_nodes::load_config;
use skg::dbs::init::in_fs_wipe_index_then_create_it;
use skg::dbs::tantivy::title_and_source_by_id;
use skg::dbs::tantivy::escape::{escape_tantivy_intra_word, escape_tantivy_literal};
use skg::dbs::tantivy::search::{SearchOptions, search_index};
use skg::dbs::tantivy::write::update_index_with_nodes;
use skg::types::misc::{ID, MSV, SourceName, TantivyIndex};
use skg::types::skgnode::{SkgNode, empty_skgnode};

#[test]
fn test_many_tantivy_things (
) -> Result<(), Box<dyn std::error::Error>> {
  let index_dir : &str =
    "/tmp/tantivy-test-many-things";
  let index_path: &Path =
    Path::new (index_dir);

  let config = load_config ("tests/tantivy/fixtures/skgconfig.toml")?;
  let nodes: Vec<SkgNode> =
    read_all_skg_files_from_sources (&config)?;

  let (tantivy_index, indexed_count): (TantivyIndex, usize) =
    in_fs_wipe_index_then_create_it (
      &nodes,
      index_path )?;
  let id_field: schema::Field =
    tantivy_index . id_field;
  let title_or_alias_field: schema::Field =
    tantivy_index . title_or_alias_field;
  assert!( indexed_count > 0,
           "Expected to index at least one title" );

  let (best_matches, searcher)
    : (Vec<(f32, tantivy::DocAddress)>, tantivy::Searcher)
    = search_index(
      &tantivy_index,
      "test second",
      &SearchOptions::default())?; // the search query
  assert!(!best_matches . is_empty(),
          "Expected to find at least one match");

  print_search_results (
    best_matches . clone(), &searcher, id_field, title_or_alias_field)?;

  if !best_matches . is_empty() {
    let (_top_score, top_doc_address): (f32, tantivy::DocAddress) =
      best_matches[0];
    let top_doc: TantivyDocument =
      searcher . doc (top_doc_address)?;
    let top_id: &str =
      top_doc . get_first (id_field)
      . unwrap() . as_str() . unwrap();
    assert!(
      top_id == "5",
      "Expected primary ID '5' to have highest score, but got: {}",
      top_id ); }

  // Test the new update functionality
  println!("\n--- Testing Update Functionality ---");

  // First search for "This is one big tuna." - should find node 1 as top result
  let (initial_matches, initial_searcher)
    : (Vec<(f32, tantivy::DocAddress)>, tantivy::Searcher)
    = search_index(&tantivy_index, "This is one big tuna.", &SearchOptions::default())?;

  assert!(!initial_matches . is_empty(),
         "Expected to find at least one match for 'This is one big tuna.'");

  let (_initial_score, initial_top_address): (f32, tantivy::DocAddress) =
    initial_matches[0];
  let initial_top_doc: TantivyDocument =
    initial_searcher . doc (initial_top_address)?;
  let initial_top_id: &str =
    initial_top_doc . get_first (id_field)
    . unwrap() . as_str() . unwrap();

  assert_eq!(initial_top_id, "1",
            "Expected node 1 to be top result initially, but got: {}",
            initial_top_id);
  println!("✓ Initial search correctly found node 1 as top result");

  // Create a new SkgNode with ID 6 and title "This is one big tuna."
  let mut new_node : SkgNode =
    empty_skgnode ();
  { new_node . title = "This is one big tuna." . to_string();
    new_node . pid = ID::new ("6"); }

  // Update the index with the new node
  let update_count: usize =
    update_index_with_nodes(&[new_node], &tantivy_index)?;
  assert_eq!(update_count, 1,
            "Expected to update exactly 1 document, but updated: {}",
            update_count);
  println!("✓ Successfully updated index with new node 6");

  // Search again - now node 6 should be first, node 1 should be second
  let (final_matches, final_searcher)
    : (Vec<(f32, tantivy::DocAddress)>, tantivy::Searcher)
    = search_index(&tantivy_index, "This is one big tuna.", &SearchOptions::default())?;

  assert!(final_matches . len() >= 2,
         "Expected at least 2 matches after update, but got: {}",
         final_matches . len());

  // Check first result is node 6
  let (_final_score1, final_address1): (f32, tantivy::DocAddress) =
    final_matches[0];
  let final_doc1: TantivyDocument =
    final_searcher . doc (final_address1)?;
  let final_id1: &str =
    final_doc1 . get_first(tantivy_index . id_field)
    . unwrap() . as_str() . unwrap();

  assert_eq!(final_id1, "6",
            "Expected node 6 to be first result after update, but got: {}",
            final_id1);
  println!("✓ After update, node 6 is correctly the top result");

  // Check second result is node 1
  let (_final_score2, final_address2): (f32, tantivy::DocAddress) =
    final_matches[1];
  let final_doc2: TantivyDocument =
    final_searcher . doc (final_address2)?;
  let final_id2: &str =
    final_doc2 . get_first(tantivy_index . id_field)
    . unwrap() . as_str() . unwrap();

  assert_eq!(final_id2, "1",
            "Expected node 1 to be second result after update, but got: {}",
            final_id2);
  println!("✓ After update, node 1 is correctly the second result");

  println!("All update functionality tests passed!");
  Ok (( )) }

pub fn print_search_results(
  best_matches: Vec<(f32, tantivy::DocAddress)>,
  searcher: &tantivy::Searcher,
  id_field: schema::Field,
  title_or_alias_field: schema::Field
) -> Result<(), Box<dyn std::error::Error>> {
  if best_matches . is_empty() {
    println!("No matches found.");
  } else {
    let mut path_to_results: HashMap <String, Vec<(f32, String)>> =
      HashMap::new();
    for (score, doc_address) in best_matches {
      let retrieved_doc: TantivyDocument =
        searcher . doc (doc_address)?;
      let path_value: &str =
        retrieved_doc . get_first (id_field)
        . unwrap() . as_str() . unwrap();
      let title_value: &str =
        retrieved_doc . get_first (title_or_alias_field)
        . unwrap() . as_str() . unwrap();
      path_to_results
        . entry(path_value . to_string())
        . or_insert_with (Vec::new)
        . push((score, title_value . to_string())); }
    for (path, titles) in path_to_results {
      println!("File: {}", path);
      for (score, title) in titles {
        println!("  - Score: {:.2} | Title: {}",
                 score, title); }
      println!(); } }
  Ok (()) }

#[test]
fn test_aliases() -> Result<(), Box<dyn std::error::Error>> {
  let empty_node : SkgNode = empty_skgnode ();
  let mut apple  = empty_node . clone();
  { apple . pid      = ID::new ("apple");
    apple . title    =               "eat apple" . to_string();
    apple . aliases  = MSV::Specified(vec![    "munch apple" . to_string(),
                                    "chomp apple" . to_string() ]); }
  let mut banana = empty_node . clone();
  { banana . pid     = ID::new ("banana");
    banana . title   =               "eat banana" . to_string();
    banana . aliases = MSV::Specified(vec![    "chomp banana" . to_string(),
                                    "throw banana" . to_string()]); }
  let mut kiwi   = empty_node . clone();
  { kiwi . pid       = ID::new ("kiwi");
    kiwi . title     =               "eat kiwi" . to_string();
    kiwi . aliases   = MSV::Specified(vec![    "munch kiwi" . to_string()]); }
  let nodes = vec![apple, banana, kiwi];

  // Create Tantivy index - use a separate directory to avoid conflicts with test_many_tantivy_things
  let index_dir : &str =
    "/tmp/tantivy-test-aliases";

  let (tantivy_index, _indexed_count): (TantivyIndex, usize) =
    in_fs_wipe_index_then_create_it(
      &nodes,
      Path::new (index_dir) )?;

  println!("Indexed {} documents", _indexed_count);

  // Test 1: Search for "eat apple"
  let (matches1, searcher1) =
    search_index(
      &tantivy_index, "eat apple",
      &SearchOptions::default())?;
  let ids1: Vec<String> =
    matches1 . iter()
    . map(|(_score, doc_address)| {
      let doc = searcher1 . doc::<TantivyDocument> (*doc_address) . unwrap();
      doc . get_first(tantivy_index . id_field) . unwrap()
        . as_str() . unwrap() . to_string() })
    . collect();

  // Should return apple as top hit, and also banana and kiwi somewhere
  assert_eq!(ids1[0], "apple",
             "'eat apple' search should return apple as top hit");
  assert!(ids1 . contains(&"banana" . to_string()),
          "'eat apple' search should include banana");
  assert!(ids1 . contains(&"kiwi" . to_string()),
          "'eat apple' search should include kiwi");
  println!("✓ Test 1 passed: 'eat apple' returned {:?}",
           ids1);

  // Test 2: Search for "chomp apple"
  let (matches2, searcher2) =
    search_index(&tantivy_index, "chomp apple", &SearchOptions::default())?;
  let ids2: Vec<String> = matches2 . iter()
    . map(|(_score, doc_address)| {
      let doc = searcher2 . doc::<TantivyDocument> (*doc_address) . unwrap();
      doc . get_first(tantivy_index . id_field) . unwrap() . as_str() . unwrap() . to_string()
    })
    . collect();

  // Should return apple as top hit, banana somewhere, but not kiwi
  assert_eq!(ids2[0], "apple", "chomp apple search should return apple as top hit");
  assert!(ids2 . contains(&"banana" . to_string()), "chomp apple search should include banana");
  assert!(!ids2 . contains(&"kiwi" . to_string()), "chomp apple search should not include kiwi");
  println!("✓ Test 2 passed: 'chomp apple' returned {:?}", ids2);

  // Test 3: Search for "throw banana"
  let (matches3, searcher3) = search_index(&tantivy_index, "throw banana", &SearchOptions::default())?;
  let ids3: Vec<String> = matches3 . iter()
    . map(|(_score, doc_address)| {
      let doc = searcher3 . doc::<TantivyDocument> (*doc_address) . unwrap();
      doc . get_first(tantivy_index . id_field) . unwrap() . as_str() . unwrap() . to_string()
    })
    . collect();

  assert_eq!(ids3 . len(), 3,
             "'throw banana' search should return only three results (the ones for banana)");
  assert_eq!(ids3[0], "banana",
             "'throw banana' search should return banana (via the alias 'throw banana'");
  assert_eq!(ids3[1], "banana",
             "'throw banana' search should return banana (via the title 'eat banana'");
  assert_eq!(ids3[2], "banana",
             "'throw banana' search should return only banana (via the alias 'chomp banana')");
  println!("✓ Test 3 passed: 'throw banana' returned {:?}", ids3);

  println!("All alias tests passed!");
  Ok (())
}

#[test]
fn test_escape_tantivy_literal (
) {
  assert_eq! ( escape_tantivy_literal ("plain words"),
               "plain words",
               "plain text should pass through unchanged" );
  assert_eq! ( escape_tantivy_literal ("C++ tips"),
               "\"C++\" tips",
               "word with specials wrapped as phrase; plain word passes through" );
  assert_eq! ( escape_tantivy_literal ("cat:dog"),
               "\"cat:dog\"",
               "colon forces phrase-wrap" );
  assert_eq! ( escape_tantivy_literal ("[draft] plan"),
               "\"[draft]\" plan",
               "brackets force phrase-wrap of their word" );
  assert_eq! ( escape_tantivy_literal ("up^2"),
               "\"up^2\"",
               "caret forces phrase-wrap" );
  assert_eq! ( escape_tantivy_literal ("cat AND dog"),
               "cat \"AND\" dog",
               "bare AND should be wrapped as a phrase" );
  assert_eq! ( escape_tantivy_literal ("foo OR bar NOT baz"),
               "foo \"OR\" bar \"NOT\" baz",
               "bare OR and NOT should be wrapped" );
  assert_eq! ( escape_tantivy_literal ("sandwich"), // contains "and" but not "AND"
               "sandwich",
               "lowercase 'and' inside a word is untouched" );
  assert_eq! ( escape_tantivy_literal ("say \"hi\""),
               "say \"\\\"hi\\\"\"",
               "quote inside a word escaped, whole word phrase-wrapped" );
  assert_eq! ( escape_tantivy_literal (""),
               "",
               "empty string stays empty" ); }

#[test]
fn test_escape_tantivy_intra_word (
) {
  // Plain text passes through.
  assert_eq! ( escape_tantivy_intra_word ("plain words"),
               "plain words" );
  // Bare boolean keywords are preserved — they're not operator
  // chars, so the escape heuristic doesn't touch them.
  assert_eq! ( escape_tantivy_intra_word ("dog AND cat"),
               "dog AND cat" );
  // Word-initial operator chars (the require/exclude prefix) stay.
  assert_eq! ( escape_tantivy_intra_word ("+dog -puppy"),
               "+dog -puppy" );
  // Phrase quotes at token boundaries stay.
  assert_eq! ( escape_tantivy_intra_word ("\"fox in socks\""),
               "\"fox in socks\"" );
  // Grouping parens at token boundaries stay.
  assert_eq! ( escape_tantivy_intra_word ("(dog OR cat) AND food"),
               "(dog OR cat) AND food" );
  // Intra-word operator: colon with word chars on both sides.
  assert_eq! ( escape_tantivy_intra_word ("foo:bar"),
               "foo\\:bar" );
  // Intra-word operator with a run of ops on one side — not both
  // sides bounded by word, so not escaped. Trailing '+' meets
  // end-of-token on the right.
  assert_eq! ( escape_tantivy_intra_word ("C++"),
               "C++" );
  // Field-qualified query inadvertently gets its ':' escaped —
  // the heuristic can't distinguish field names from words.
  // Documented limitation.
  assert_eq! ( escape_tantivy_intra_word ("title_or_alias:dog"),
               "title_or_alias\\:dog" );
  // Empty stays empty.
  assert_eq! ( escape_tantivy_intra_word (""),
               "" ); }

#[test]
fn test_search_finds_titles_with_special_chars (
) -> Result<(), Box<dyn std::error::Error>> {
  let empty : SkgNode = empty_skgnode ();
  let mut c_plus : SkgNode = empty . clone ();
  { c_plus . pid   = ID::new ("c_plus");
    c_plus . title = "C++ tips" . to_string (); }
  let mut brackets : SkgNode = empty . clone ();
  { brackets . pid   = ID::new ("brackets");
    brackets . title = "[draft] plan" . to_string (); }
  let mut colons : SkgNode = empty . clone ();
  { colons . pid   = ID::new ("colons");
    colons . title = "cat:dog" . to_string (); }
  let nodes : Vec<SkgNode> =
    vec![c_plus, brackets, colons];
  let index_dir : &str =
    "/tmp/tantivy-test-special-chars";
  let (tantivy_index, _indexed_count) : (TantivyIndex, usize) =
    in_fs_wipe_index_then_create_it (
      &nodes, Path::new (index_dir) ) ?;
  for (query, expected_id) in &[
    ("C++ tips",     "c_plus"),
    ("[draft] plan", "brackets"),
    ("cat:dog",      "colons"),
  ] { let (matches, searcher) :
        (Vec<(f32, tantivy::DocAddress)>, tantivy::Searcher) =
        search_index (&tantivy_index, query, &SearchOptions::default ()) ?;
      assert! ( !matches . is_empty (),
                "literal '{}' should find a result", query );
      let top_doc : TantivyDocument =
        searcher . doc (matches[0] . 1) ?;
      let top_id : &str =
        top_doc
        . get_first (tantivy_index . id_field) . unwrap ()
        . as_str () . unwrap ();
      assert_eq! ( top_id, *expected_id,
                   "'{}' should find the {} node",
                   query, expected_id ); }
  Ok (( )) }

/// Body axis: body text is indexed when body=true, invisible when false.
#[test]
fn test_search_body_axis (
) -> Result<(), Box<dyn std::error::Error>> {
  let empty : SkgNode = empty_skgnode ();
  let mut recipe : SkgNode = empty . clone ();
  { recipe . pid   = ID::new ("recipe");
    recipe . title = "soup recipe" . to_string ();
    recipe . body  = Some ( "add paprika and salt" . to_string () ); }
  let mut decoy : SkgNode = empty . clone ();
  { decoy . pid   = ID::new ("decoy");
    decoy . title = "shopping list" . to_string (); }
  let nodes : Vec<SkgNode> = vec! [recipe, decoy];
  let (ti, _) : (TantivyIndex, usize) =
    in_fs_wipe_index_then_create_it (
      &nodes, Path::new ("/tmp/tantivy-test-body-axis") ) ?;
  { // body=false: body content is NOT findable
    let (matches, _searcher) =
      search_index (&ti, "paprika", &SearchOptions::default ()) ?;
    assert! ( matches . is_empty (),
              "body word should NOT match when body=false" ); }
  { // body=true: body content IS findable
    let opts = SearchOptions { body: true, ..SearchOptions::default () };
    let (matches, searcher) =
      search_index (&ti, "paprika", &opts) ?;
    assert! ( !matches . is_empty (),
              "body word should match when body=true" );
    let top_doc : TantivyDocument =
      searcher . doc (matches[0] . 1) ?;
    let top_id : &str =
      top_doc . get_first (ti . id_field) . unwrap ()
      . as_str () . unwrap ();
    assert_eq! ( top_id, "recipe",
                 "body search should find the recipe node" ); }
  Ok (( )) }

/// Regex axis: patterns match on tokens, bypassing the QueryParser.
#[test]
fn test_search_regex_axis (
) -> Result<(), Box<dyn std::error::Error>> {
  let empty : SkgNode = empty_skgnode ();
  let mut history    : SkgNode = empty . clone ();
  { history    . pid   = ID::new ("history");
    history    . title = "history" . to_string (); }
  let mut historical : SkgNode = empty . clone ();
  { historical . pid   = ID::new ("historical");
    historical . title = "historical" . to_string (); }
  let mut hello      : SkgNode = empty . clone ();
  { hello      . pid   = ID::new ("hello");
    hello      . title = "hello" . to_string (); }
  let nodes : Vec<SkgNode> = vec! [history, historical, hello];
  let (ti, _) : (TantivyIndex, usize) =
    in_fs_wipe_index_then_create_it (
      &nodes, Path::new ("/tmp/tantivy-test-regex-axis") ) ?;
  { // Prefix via regex: 'histor.*' matches history, historical.
    let opts = SearchOptions { regex: true, ..SearchOptions::default () };
    let (matches, searcher) =
      search_index (&ti, "histor.*", &opts) ?;
    let mut ids : Vec<String> = Vec::new ();
    for (_, addr) in &matches {
      let doc : TantivyDocument = searcher . doc (*addr) ?;
      ids . push (
        doc . get_first (ti . id_field) . unwrap ()
        . as_str () . unwrap () . to_string () ); }
    assert! ( ids . contains (&"history" . to_string ()),
              "regex 'histor.*' should match 'history' (got {:?})", ids );
    assert! ( ids . contains (&"historical" . to_string ()),
              "regex 'histor.*' should match 'historical' (got {:?})", ids );
    assert! ( ! ids . contains (&"hello" . to_string ()),
              "regex 'histor.*' should NOT match 'hello' (got {:?})", ids ); }
  Ok (( )) }

/// Multi-word regex: whitespace splits the pattern; each piece
/// becomes its own RegexQuery, SHOULD-combined. A pattern containing
/// literal whitespace could never match a single token, so splitting
/// is the only way for multi-word regex queries to return anything.
#[test]
fn test_search_regex_multiword (
) -> Result<(), Box<dyn std::error::Error>> {
  let empty : SkgNode = empty_skgnode ();
  let mk = |pid : &str, title : &str| -> SkgNode {
    let mut n : SkgNode = empty . clone ();
    n . pid = ID::new (pid);
    n . title = title . to_string ();
    n };
  let nodes : Vec<SkgNode> = vec! [
    mk ("polsci",   "political science"),
    mk ("polit",    "political"),
    mk ("sci",      "science"),
    mk ("bio",      "biology") ];
  let (ti, _) : (TantivyIndex, usize) =
    in_fs_wipe_index_then_create_it (
      &nodes,
      Path::new ("/tmp/tantivy-test-regex-multiword") ) ?;
  let gather = | pattern : &str | -> Result<Vec<String>, Box<dyn std::error::Error>> {
    let opts : SearchOptions =
      SearchOptions { regex: true, ..SearchOptions::default () };
    let (matches, searcher) = search_index (&ti, pattern, &opts) ?;
    let mut ids : Vec<String> = Vec::new ();
    for (_, addr) in &matches {
      let doc : TantivyDocument = searcher . doc (*addr) ?;
      ids . push (
        doc . get_first (ti . id_field) . unwrap ()
        . as_str () . unwrap () . to_string () ); }
    Ok (ids) };
  { // Literal multi-word: each word OR'd.
    let ids : Vec<String> = gather ("political science") ?;
    assert! ( ids . contains (&"polsci" . to_string ()),
              "should match 'political science' (got {:?})", ids );
    assert! ( ids . contains (&"polit"  . to_string ()),
              "should match 'political' (got {:?})", ids );
    assert! ( ids . contains (&"sci"    . to_string ()),
              "should match 'science' (got {:?})", ids );
    assert! ( ! ids . contains (&"bio" . to_string ()),
              "should NOT match 'biology' (got {:?})", ids ); }
  { // Prefix regex per piece.
    let ids : Vec<String> = gather ("polit.* scien.*") ?;
    assert! ( ids . contains (&"polsci" . to_string ()),
              "prefix regex should match 'political science' (got {:?})", ids );
    assert! ( ids . contains (&"polit"  . to_string ()),
              "prefix regex should match 'political' (got {:?})", ids );
    assert! ( ids . contains (&"sci"    . to_string ()),
              "prefix regex should match 'science' (got {:?})", ids );
    assert! ( ! ids . contains (&"bio" . to_string ()),
              "prefix regex should NOT match 'biology' (got {:?})", ids ); }
  Ok (( )) }

/// Regex + operators: AND / OR / NOT / +foo / -bar combine
/// per-piece RegexQueries at the document level.
#[test]
fn test_search_regex_with_operators (
) -> Result<(), Box<dyn std::error::Error>> {
  let empty : SkgNode = empty_skgnode ();
  let mk = |pid : &str, title : &str| -> SkgNode {
    let mut n : SkgNode = empty . clone ();
    n . pid = ID::new (pid);
    n . title = title . to_string ();
    n };
  let nodes : Vec<SkgNode> = vec! [
    mk ("polsci",   "political science"),
    mk ("polbio",   "political biology"),
    mk ("scibio",   "science biology"),
    mk ("bio",      "biology") ];
  let (ti, _) : (TantivyIndex, usize) =
    in_fs_wipe_index_then_create_it (
      &nodes,
      Path::new ("/tmp/tantivy-test-regex-operators") ) ?;
  let gather = | pattern : &str | -> Result<Vec<String>, Box<dyn std::error::Error>> {
    let opts : SearchOptions =
      SearchOptions { regex: true, operators: true,
                      ..SearchOptions::default () };
    let (matches, searcher) = search_index (&ti, pattern, &opts) ?;
    let mut ids : Vec<String> = Vec::new ();
    for (_, addr) in &matches {
      let doc : TantivyDocument = searcher . doc (*addr) ?;
      ids . push (
        doc . get_first (ti . id_field) . unwrap ()
        . as_str () . unwrap () . to_string () ); }
    Ok (ids) };
  { // AND: docs must contain tokens matching both patterns.
    let ids : Vec<String> = gather ("poli.* AND sci.*") ?;
    assert! ( ids . contains (&"polsci" . to_string ()),
              "AND should match 'political science' (got {:?})", ids );
    assert! ( ! ids . contains (&"polbio" . to_string ()),
              "AND should NOT match 'political biology' (got {:?})", ids );
    assert! ( ! ids . contains (&"scibio" . to_string ()),
              "AND should NOT match 'science biology' (got {:?})", ids );
    assert! ( ! ids . contains (&"bio" . to_string ()),
              "AND should NOT match 'biology' (got {:?})", ids ); }
  { // NOT: second piece becomes MUST_NOT.
    let ids : Vec<String> = gather ("poli.* NOT bio.*") ?;
    assert! ( ids . contains (&"polsci" . to_string ()),
              "NOT should match 'political science' (got {:?})", ids );
    assert! ( ! ids . contains (&"polbio" . to_string ()),
              "NOT should exclude 'political biology' (got {:?})", ids );
    assert! ( ! ids . contains (&"scibio" . to_string ()),
              "NOT should exclude 'science biology' (got {:?})", ids ); }
  { // +/- prefix: same effect without the keyword.
    let ids : Vec<String> = gather ("+poli.* -bio.*") ?;
    assert! ( ids . contains (&"polsci" . to_string ()),
              "+/- should match 'political science' (got {:?})", ids );
    assert! ( ! ids . contains (&"polbio" . to_string ()),
              "+/- should exclude 'political biology' (got {:?})", ids ); }
  { // Explicit OR matches the default SHOULD behavior.
    let ids : Vec<String> = gather ("poli.* OR bio.*") ?;
    assert! ( ids . contains (&"polsci" . to_string ()),
              "OR should match 'political science' (got {:?})", ids );
    assert! ( ids . contains (&"polbio" . to_string ()),
              "OR should match 'political biology' (got {:?})", ids );
    assert! ( ids . contains (&"bio" . to_string ()),
              "OR should match 'biology' (got {:?})", ids ); }
  Ok (( )) }

#[test]
fn test_title_by_id_returns_title_not_alias (
) -> Result<(), Box<dyn std::error::Error>> {
  let empty_node : SkgNode = empty_skgnode ();
  let mut node = empty_node . clone ();
  { node . pid     = ID::new ("node-with-aliases");
    node . title   =               "The Real Title" . to_string ();
    node . aliases = MSV::Specified (vec![   "Alias One" . to_string (),
                                   "Alias Two" . to_string () ]); }
  let nodes : Vec<SkgNode> = vec![node];
  let index_dir : &str =
    "/tmp/tantivy-test-title-by-id";
  let (tantivy_index, indexed_count) : (TantivyIndex, usize) =
    in_fs_wipe_index_then_create_it (
      &nodes,
      Path::new (index_dir) )?;
  assert_eq! (indexed_count, 3,
    "Expected 3 documents (1 title + 2 aliases)");
  let result : Option<(String, SourceName)> =
    title_and_source_by_id (
      &tantivy_index,
      &ID::new ("node-with-aliases") );
  assert_eq! (result . as_ref () . map ( |(t, _)| t . as_str () ),
    Some ("The Real Title"),
    "title_and_source_by_id should return the title, not an alias");
  let missing : Option<(String, SourceName)> =
    title_and_source_by_id (
      &tantivy_index,
      &ID::new ("nonexistent-id") );
  assert_eq! (missing, None,
    "title_and_source_by_id should return None for missing IDs");
  println! ("title_and_source_by_id test passed!");
  Ok (( )) }
