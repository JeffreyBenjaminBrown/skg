// cargo nextest run --test typedb -- typedb::ancestry
//
// Each test has its own fixtures folder under
// tests/typedb/ancestry/fixtures-*/
// with dedicated .skg files and a README.org spec.

use skg::dbs::typedb::ancestry::{
  AncestryTree,
  full_containerward_ancestry};
use skg::test_utils::run_with_shared_test_db;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};

use std::error::Error;
use std::sync::Arc;
use typedb_driver::TypeDBDriver;

//
// Helpers
//

/// Render an AncestryTree tree to org-mode text.
/// Each node becomes a headline at the given depth,
/// using just its ID as the title (no skg metadata).
fn ancestry_node_to_org(
  node  : &AncestryTree,
  depth : usize,
) -> String {
  let stars : String = "*" . repeat (depth);
  let id : &ID = node . id ();
  match node {
    AncestryTree::Root (_) |
    AncestryTree::Repeated (_) |
    AncestryTree::DepthTruncated (_) =>
      format! ("{} {}\n", stars, id),
    AncestryTree::Inner (_, children) => {
      let mut out : String =
        format! ("{} {}\n", stars, id);
      for child in children {
        out . push_str (
          & ancestry_node_to_org (child, depth + 1) ); }
      out }, } }

/// Parse simple org text into a tree structure for comparison.
/// Returns Vec of (title, depth, children) tuples, nested.
/// Input format: lines like "* title", "** title", etc.
#[derive(Debug, Clone, PartialEq, Eq)]
struct OrgTree {
  title    : String,
  children : Vec<OrgTree>,
}

fn parse_org_to_trees(
  org : &str,
) -> Vec<OrgTree> {
  let lines : Vec<(usize, String)> =
    org . lines ()
    . filter ( |l| ! l . trim () . is_empty () )
    . map ( |line| {
      let trimmed : &str = line . trim_start ();
      let star_count : usize =
        trimmed . chars ()
        . take_while ( |c| *c == '*' )
        . count ();
      let title : String =
        trimmed [star_count ..] . trim () . to_string ();
      (star_count, title) } )
    . collect ();
  parse_org_lines (&lines, 0) . 0 }

fn parse_org_lines(
  lines : &[(usize, String)],
  start : usize,
) -> (Vec<OrgTree>, usize) {
  if start >= lines . len () {
    return (vec![], start); }
  let base_depth : usize = lines [start] . 0;
  let mut trees : Vec<OrgTree> = Vec::new ();
  let mut i : usize = start;
  while i < lines . len () {
    let (depth, ref title) = lines [i];
    if depth < base_depth {
      break; }
    if depth > base_depth {
      // Should not happen at top level; skip.
      i += 1;
      continue; }
    // This line is at base_depth; it's a new root.
    i += 1;
    let (children, next_i) : (Vec<OrgTree>, usize) =
      if i < lines . len () && lines [i] . 0 > base_depth {
        parse_org_lines (lines, i)
      } else {
        (vec![], i) };
    trees . push ( OrgTree {
      title    : title . clone (),
      children } );
    i = next_i; }
  (trees, i) }

/// Compare two OrgTree slices, ignoring child order.
/// Each node's title must match, and each node's children
/// (as a multiset of subtrees) must match.
fn trees_match_unordered(
  actual   : &[OrgTree],
  expected : &[OrgTree],
) -> bool {
  if actual . len () != expected . len () {
    return false; }
  // For each expected tree, find a matching actual tree (greedy).
  let mut used : Vec<bool> =
    vec![false; actual . len ()];
  for exp in expected {
    let found : bool = actual . iter ()
      . enumerate ()
      . any ( |(i, act)| {
        if used [i] { return false; }
        if act . title == exp . title &&
           trees_match_unordered (
             & act . children,
             & exp . children )
        { used [i] = true;
          true }
        else { false } } );
    if ! found { return false; } }
  true }

/// Assert that the actual AncestryTree tree matches
/// the expected org-mode text, ignoring child order.
fn assert_ancestry_matches_org(
  actual       : &AncestryTree,
  expected_org : &str,
) {
  let actual_org : String =
    ancestry_node_to_org (actual, 1);
  let actual_trees : Vec<OrgTree> =
    parse_org_to_trees (& actual_org);
  let expected_trees : Vec<OrgTree> =
    parse_org_to_trees (expected_org);
  assert! (
    trees_match_unordered (& actual_trees, & expected_trees),
    "Ancestry tree mismatch.\n\
     Expected (order-insensitive):\n{}\n\
     Actual:\n{}",
    expected_org, actual_org ); }

/// Assert that every leaf in the expected org
/// is also a leaf (no children) in the actual tree.
fn assert_leaves_are_leaves(
  actual : &AncestryTree,
  expected_org : &str,
) {
  let expected_trees : Vec<OrgTree> =
    parse_org_to_trees (expected_org);
  let expected_leaves : Vec<String> =
    collect_leaves_from_org_trees (& expected_trees);
  let actual_org : String =
    ancestry_node_to_org (actual, 1);
  let actual_trees : Vec<OrgTree> =
    parse_org_to_trees (& actual_org);
  for leaf_title in & expected_leaves {
    assert! (
      is_leaf_somewhere (& actual_trees, leaf_title),
      "Expected '{}' to be a leaf in actual tree, but it is not.\n\
       Actual:\n{}",
      leaf_title, actual_org ); } }

fn collect_leaves_from_org_trees(
  trees : &[OrgTree],
) -> Vec<String> {
  let mut leaves : Vec<String> = Vec::new ();
  for tree in trees {
    if tree . children . is_empty () {
      leaves . push (tree . title . clone ()); }
    else {
      leaves . extend (
        collect_leaves_from_org_trees (
          & tree . children )); } }
  leaves }

fn is_leaf_somewhere(
  trees : &[OrgTree],
  title : &str,
) -> bool {
  for tree in trees {
    if tree . title == title && tree . children . is_empty () {
      return true; }
    if is_leaf_somewhere (& tree . children, title) {
      return true; } }
  false }

//
// Tests
//

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_db (
    "skg-test-typedb-ancestry",
    |s| Box::pin ( async move {
      s . reset ("test_ancestry_island",
                 "tests/typedb/ancestry/fixtures-island") . await ?;
      test_ancestry_island (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("test_ancestry_fork",
                 "tests/typedb/ancestry/fixtures-fork") . await ?;
      test_ancestry_fork (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("test_ancestry_1cycle",
                 "tests/typedb/ancestry/fixtures-1cycle") . await ?;
      test_ancestry_1cycle (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("test_ancestry_2cycle",
                 "tests/typedb/ancestry/fixtures-2cycle") . await ?;
      test_ancestry_2cycle (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("test_ancestry_3cycle",
                 "tests/typedb/ancestry/fixtures-3cycle") . await ?;
      test_ancestry_3cycle (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("test_ancestry_diamond",
                 "tests/typedb/ancestry/fixtures-diamond") . await ?;
      test_ancestry_diamond (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("test_ancestry_fork_diamond_cycle",
                 "tests/typedb/ancestry/fixtures-fork-diamond-cycle") . await ?;
      test_ancestry_fork_diamond_cycle (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("test_ancestry_depth_limit",
                 "tests/typedb/ancestry/fixtures-depth-limit") . await ?;
      test_ancestry_depth_limit (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

async fn test_ancestry_island(
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  async fn go(
    db_name : &str,
    driver  : &TypeDBDriver,
  ) -> Result<(), Box<dyn Error>> {
    let result : AncestryTree =
      full_containerward_ancestry (
        db_name, driver, &ID::from("a"), 20
      ) . await ?;
    let expected : &str = "\
* a
";
    assert_ancestry_matches_org (& result, expected);
    assert_leaves_are_leaves (& result, expected);
    // Island should be a Root node.
    assert! (
      matches! (result, AncestryTree::Root (_)),
      "Island node should be Root, got {:?}", result );
    Ok(()) }
  go(& config . db_name, driver) . await }

async fn test_ancestry_fork(
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  async fn go(
    db_name : &str,
    driver  : &TypeDBDriver,
  ) -> Result<(), Box<dyn Error>> {
    let result : AncestryTree =
      full_containerward_ancestry (
        db_name, driver, &ID::from("x"), 20
      ) . await ?;
    let expected : &str = "\
* x
** a
** b
*** c
";
    assert_ancestry_matches_org (& result, expected);
    assert_leaves_are_leaves (& result, expected);
    Ok(()) }
  go(& config . db_name, driver) . await }

async fn test_ancestry_1cycle(
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  async fn go(
    db_name : &str,
    driver  : &TypeDBDriver,
  ) -> Result<(), Box<dyn Error>> {
    let result : AncestryTree =
      full_containerward_ancestry (
        db_name, driver, &ID::from("a"), 20
      ) . await ?;
    let expected : &str = "\
* a
** a
";
    assert_ancestry_matches_org (& result, expected);
    assert_leaves_are_leaves (& result, expected);
    Ok(()) }
  go(& config . db_name, driver) . await }

async fn test_ancestry_2cycle(
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  async fn go(
    db_name : &str,
    driver  : &TypeDBDriver,
  ) -> Result<(), Box<dyn Error>> {
    let result : AncestryTree =
      full_containerward_ancestry (
        db_name, driver, &ID::from("a"), 20
      ) . await ?;
    let expected : &str = "\
* a
** b
*** a
";
    assert_ancestry_matches_org (& result, expected);
    assert_leaves_are_leaves (& result, expected);
    Ok(()) }
  go(& config . db_name, driver) . await }

async fn test_ancestry_3cycle(
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  async fn go(
    db_name : &str,
    driver  : &TypeDBDriver,
  ) -> Result<(), Box<dyn Error>> {
    let result : AncestryTree =
      full_containerward_ancestry (
        db_name, driver, &ID::from("a"), 20
      ) . await ?;
    let expected : &str = "\
* a
** c
*** b
**** a
";
    assert_ancestry_matches_org (& result, expected);
    assert_leaves_are_leaves (& result, expected);
    Ok(()) }
  go(& config . db_name, driver) . await }

async fn test_ancestry_diamond(
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  async fn go(
    db_name : &str,
    driver  : &TypeDBDriver,
  ) -> Result<(), Box<dyn Error>> {
    let result : AncestryTree =
      full_containerward_ancestry (
        db_name, driver, &ID::from("x"), 20
      ) . await ?;
    // x is contained by both b and c;
    // both b and c are contained by a.
    // One 'a' is a genuine Root, the other is Repeated.
    let expected : &str = "\
* x
** b
*** a
** c
*** a
";
    assert_ancestry_matches_org (& result, expected);
    assert_leaves_are_leaves (& result, expected);
    Ok(()) }
  go(& config . db_name, driver) . await }

async fn test_ancestry_fork_diamond_cycle(
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  async fn go(
    db_name : &str,
    driver  : &TypeDBDriver,
  ) -> Result<(), Box<dyn Error>> {
    let result : AncestryTree =
      full_containerward_ancestry (
        db_name, driver, &ID::from("origin"), 20
      ) . await ?;
    let expected : &str = "\
* origin
** aa
*** a
**** origin
**** root
** b
*** root
";
    assert_ancestry_matches_org (& result, expected);
    assert_leaves_are_leaves (& result, expected);
    Ok(()) }
  go(& config . db_name, driver) . await }

async fn test_ancestry_depth_limit(
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  async fn go(
    db_name : &str,
    driver  : &TypeDBDriver,
  ) -> Result<(), Box<dyn Error>> {
    // max_depth=3 truncates the chain e→d→c→b→a at depth 3.
    let result : AncestryTree =
      full_containerward_ancestry (
        db_name, driver, &ID::from("e"), 3
      ) . await ?;
    let expected : &str = "\
* e
** d
*** c
";
    assert_ancestry_matches_org (& result, expected);
    assert_leaves_are_leaves (& result, expected);
    Ok(()) }
  go(& config . db_name, driver) . await }
