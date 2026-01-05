// cargo test --test hidden_from_subscriptions -- --nocapture
//
// Tests for HiddenOutsideOfSubscribeeCol, HiddenInSubscribeeCol, and HiddenFromSubscribees.
// These test that:
// 1. Initial view shows subscribees as indefinitive with HiddenOutsideOfSubscribeeCol
// 2. After saving with definitive view requests, HiddenInSubscribeeCol is shown

use indoc::indoc;
use regex::Regex;
use skg::init::{overwrite_new_empty_db, define_schema, create_empty_tantivy_index};
use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use skg::dbs::typedb::nodes::create_all_nodes;
use skg::dbs::typedb::relationships::create_all_relationships;
use skg::serve::handlers::save_buffer::update_from_and_rerender_buffer;
use skg::to_org::render::content_view::single_root_view;
use skg::types::misc::{SkgConfig, SkgfileSource, ID, TantivyIndex};
use skg::types::skgnode::SkgNode;
use futures::executor::block_on;
use std::collections::HashMap;
use std::error::Error;
use std::path::PathBuf;
use typedb_driver::{TypeDBDriver, Credentials, DriverOptions};

async fn setup_test(
  db_name: &str,
  fixtures_path: &str,
) -> Result<(SkgConfig,
             TypeDBDriver,
             TantivyIndex),
            Box<dyn Error>> {
  let mut sources: HashMap<String, SkgfileSource> =
    HashMap::new();
  sources.insert(
    "main".to_string(),
    SkgfileSource {
      nickname: "main".to_string(),
      path: PathBuf::from(fixtures_path),
      user_owns_it: true, }, );
  let config: SkgConfig = SkgConfig {
    db_name: db_name.to_string(),
    tantivy_folder: PathBuf::from(format!("/tmp/tantivy-{}",
                                          db_name)),
    sources,
    port: 1730,
    delete_on_quit: false,
    initial_node_limit: 1000, };
  let driver: TypeDBDriver = TypeDBDriver::new(
    "127.0.0.1:1729",
    Credentials::new("admin", "password"),
    DriverOptions::new(false, None)?,
  ).await?;
  let nodes: Vec<SkgNode> =
    read_all_skg_files_from_sources(
      &config.sources)?;
  overwrite_new_empty_db(db_name, &driver).await?;
  define_schema(db_name, &driver).await?;
  create_all_nodes(db_name, &driver, &nodes).await?;
  create_all_relationships(db_name, &driver, &nodes).await?;
  let tantivy_index: TantivyIndex =
    create_empty_tantivy_index(&config.tantivy_folder)?;
  Ok((config, driver, tantivy_index)) }

async fn cleanup_test(
  db_name: &str,
  driver: &TypeDBDriver,
  tantivy_folder: &std::path::Path,
) -> Result<(), Box<dyn Error>> {
  if driver.databases().contains(db_name).await? {
    driver.databases().get(db_name).await?.delete().await?; }
  if tantivy_folder.exists() {
    std::fs::remove_dir_all(tantivy_folder)?; }
  Ok (( )) }

/// Add (viewRequests definitiveView) to all subscribee nodes in org text.
/// Modifies the code section of each subscribee to request a definitive view.
///
/// Input:  `(code (interp subscribee) indefinitive)`
/// Output: `(code (interp subscribee) indefinitive (viewRequests definitiveView))`
fn add_definitive_view_request_to_subscribees (
  org_text : &str,
) -> String {
  // Match subscribee code sections and add viewRequests before the closing paren
  // Pattern: (code (interp subscribee) indefinitive)
  // We need to add (viewRequests definitiveView) inside the code section
  let re = Regex::new(
    r"\(code \(interp subscribee\) indefinitive\)"
  ).unwrap();
  re.replace_all(
    org_text,
    "(code (interp subscribee) indefinitive (viewRequests definitiveView))"
  ).to_string() }

/// Every kind of Col:
/// - R subscribes to E1, E2
/// - R hides hidden-in-E1, hidden-in-E2, hidden-for-no-reason
/// - E1 contains hidden-in-E1 (hidden) and E11 (not hidden)
/// - E2 contains E21 (not hidden) and hidden-in-E2 (hidden)
/// - hidden-for-no-reason is not in any subscribee's content
///
/// Tests two views:
/// - Initial view from R: subscribees are indefinitive (bare leaves)
/// - View from R with definitive views expanded at each subscribee
///
/// Also tests ordering rule: HiddenInSubscribeeCol precedes content regardless of .skg order.
/// E2's .skg has [E21, hidden-in-E2] but view shows HiddenInSubscribeeCol before E21.
#[test]
fn test_every_kind_of_col(
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let db_name = "skg-test-hidden-every-kind-of-col";
    let (config, driver, tantivy) = setup_test(
      db_name,
      "tests/hidden_from_subscriptions/fixtures-every-kind-of-col"
    ).await?;

    let initial_view: String = single_root_view(
      &driver,
      &config,
      &ID("R".to_string()), // Initial view from R ("subscribeR")
    ).await?;
    println!("Initial view from R:\n{}", initial_view);

    let expected_initial = indoc! {
      "* (skg (id R) (source main) (view (rels (containers 0) (contents 1)))) R
       ** (skg (code (interp subscribeeCol))) it subscribes to these
       *** (skg (code (interp hiddenOutsideOfSubscribeeCol))) hidden from all subscriptions
       **** (skg (id hidden-for-no-reason) (source main) (view (rels (containers 0))) (code (interp hiddenFromSubscribees) indefinitive)) hidden-for-no-reason
       *** (skg (id E1) (view (rels (containers 0) (contents 2))) (code (interp subscribee) indefinitive)) E1
       *** (skg (id E2) (view (rels (containers 0) (contents 2))) (code (interp subscribee) indefinitive)) E2
       ** (skg (id R1) (source main)) R1
       "};
    assert_eq!(initial_view, expected_initial,
      "Initial view from R: indefinitive subscribees are bare leaves; only HiddenOutsideOfSubscribeeCol shown");

    let expanded = { // Request definitive views, then save
      let modified_view : String =
        add_definitive_view_request_to_subscribees ( &initial_view );
      println!("Modified view (with definitive requests):\n{}", modified_view);
      let response = update_from_and_rerender_buffer (
        &modified_view, &driver, &config, &tantivy ) . await ?;
      response.buffer_content };
    println!("View from R after save with definitive view requests:\n{}", expanded);

    let expected_expanded = indoc! {
      "* (skg (id R) (source main) (view (rels (containers 0) (contents 1)))) R
       ** (skg (code (interp subscribeeCol))) it subscribes to these
       *** (skg (code (interp hiddenOutsideOfSubscribeeCol))) hidden from all subscriptions
       **** (skg (id hidden-for-no-reason) (source main) (view (rels (containers 0))) (code (interp hiddenFromSubscribees) indefinitive)) hidden-for-no-reason
       *** (skg (id E1) (source main) (view (rels (containers 0) (contents 2))) (code (interp subscribee))) E1
       **** (skg (code (interp hiddenInSubscribeeCol))) hidden from this subscription
       ***** (skg (id hidden-in-E1) (source main) (code (interp hiddenFromSubscribees) indefinitive)) hidden-in-E1
       **** (skg (id E11) (source main)) E11
       *** (skg (id E2) (source main) (view (rels (containers 0) (contents 2))) (code (interp subscribee))) E2
       **** (skg (code (interp hiddenInSubscribeeCol))) hidden from this subscription
       ***** (skg (id hidden-in-E2) (source main) (code (interp hiddenFromSubscribees) indefinitive)) hidden-in-E2
       **** (skg (id E21) (source main)) E21
       ** (skg (id R1) (source main)) R1
       "};
    assert_eq!(expanded, expected_expanded,
      "View with expanded subscribees: HiddenInSubscribeeCol shown before content");

    cleanup_test(
      db_name,
      &driver,
      &config.tantivy_folder,
    ).await?;
    Ok (( )) } ) }

/// Hidden within but none hidden without:
/// - R subscribes to E1
/// - R hides H
/// - E1 contains [E11, H, E12] where H is hidden and E11, E12 are not
///
/// Tests two views:
/// - Initial view from R: E1 is indefinitive (bare leaf), H doesn't appear
/// - View from R with definitive views expanded at each subscribee: H appears in HiddenInSubscribeeCol before E11 and E12
///
/// No HiddenOutsideOfSubscribeeCol in either state (H is in E1's content).
#[test]
fn test_hidden_within_but_none_without(
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let db_name = "skg-test-hidden-within-but-none-without";
    let (config, driver, tantivy) = setup_test(
      db_name, "tests/hidden_from_subscriptions/fixtures-hidden-within-but-none-without" ). await?;
    let initial_view: String = single_root_view(
      &driver,
      &config,
      &ID("R".to_string()), // the root
    ).await?;
    println!("Initial view from R:\n{}", initial_view);

    let expected_initial = indoc! {
      "* (skg (id R) (source main) (view (rels (containers 0) (contents 1)))) R
       ** (skg (code (interp subscribeeCol))) it subscribes to these
       *** (skg (id E1) (view (rels (containers 0) (contents 3))) (code (interp subscribee) indefinitive)) E1
       ** (skg (id R1) (source main)) R1
       "};
    assert_eq!(initial_view, expected_initial,
      "Initial view from R: indefinitive subscribee is bare leaf; H doesn't appear");

    let expanded = { // request definitive views, then save
      let modified_view : String =
        add_definitive_view_request_to_subscribees ( &initial_view );
      println!("Modified view (with definitive requests):\n{}", modified_view);
      let response = update_from_and_rerender_buffer (
        &modified_view, &driver, &config, &tantivy ) . await ?;
      response.buffer_content };
    println!("View from R after save with definitive view requests:\n{}", expanded);

    // HiddenInSubscribeeCol precedes content regardless of .skg order.
    // E1.skg has [E11, H, E12] but view shows HiddenInSubscribeeCol (with H) before E11 and E12.
    let expected_expanded = indoc! {
      "* (skg (id R) (source main) (view (rels (containers 0) (contents 1)))) R
       ** (skg (code (interp subscribeeCol))) it subscribes to these
       *** (skg (id E1) (source main) (view (rels (containers 0) (contents 3))) (code (interp subscribee))) E1
       **** (skg (code (interp hiddenInSubscribeeCol))) hidden from this subscription
       ***** (skg (id H) (source main) (code (interp hiddenFromSubscribees) indefinitive)) H
       **** (skg (id E11) (source main)) E11
       **** (skg (id E12) (source main)) E12
       ** (skg (id R1) (source main)) R1
       "};
    assert_eq!(expanded, expected_expanded,
      "View with expanded subscribees: HiddenInSubscribeeCol with H before E11 and E12");

    cleanup_test(
      db_name,
      &driver,
      &config.tantivy_folder,
    ).await?;
    Ok (( )) } ) }

/// Hidden without but none hidden within:
/// - R subscribes to E1, E2
/// - R hides H
/// - H is NOT in any subscribee's content
/// - E1 contains [E11, E12], E12 contains [E121]
/// - E2 has no content
///
/// Tests two views:
/// - Initial view from R: E1, E2 are indefinitive (bare leaves), H in HiddenOutsideOfSubscribeeCol
/// - View from R with definitive views expanded at each subscribee: E1 shows E11, E12 (E12 indefinitive); E2 expanded but empty; H still in HiddenOutsideOfSubscribeeCol
///
/// No HiddenInSubscribeeCol in either state (H is not in any subscribee's content).
#[test]
fn test_hidden_without_but_none_within(
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let db_name = "skg-test-hidden-without-but-none-within";
    let (config, driver, tantivy) =
      setup_test(db_name,
                 "tests/hidden_from_subscriptions/fixtures-hidden-without-but-none-within").await?;
    let initial_view: String = single_root_view(
      &driver,
      &config,
      &ID("R".to_string()), // origin of the view
    ).await?;
    println!("Initial view from R:\n{}", initial_view);
    let expected_initial = indoc! {
      "* (skg (id R) (source main) (view (rels (containers 0) (contents 1)))) R
       ** (skg (code (interp subscribeeCol))) it subscribes to these
       *** (skg (code (interp hiddenOutsideOfSubscribeeCol))) hidden from all subscriptions
       **** (skg (id H) (source main) (view (rels (containers 0))) (code (interp hiddenFromSubscribees) indefinitive)) H
       *** (skg (id E1) (view (rels (containers 0) (contents 2))) (code (interp subscribee) indefinitive)) E1
       *** (skg (id E2) (view (rels (containers 0))) (code (interp subscribee) indefinitive)) E2
       ** (skg (id R1) (source main)) R1
       "};
    assert_eq!(initial_view, expected_initial,
      "Initial view from R: H in HiddenOutsideOfSubscribeeCol; E1 and E2 are indefinitive bare leaves");
    let with_subscribees_expanded = {
      let modified_view : String =
        add_definitive_view_request_to_subscribees ( &initial_view );
      println!("Modified view (with definitive requests):\n{}", modified_view);
      let response = update_from_and_rerender_buffer (
        &modified_view, &driver, &config, &tantivy ) . await ?;
      response.buffer_content };
    println!("View from R after save with definitive view requests:\n{}",
             with_subscribees_expanded);
    let expected_expanded = indoc! {
      "* (skg (id R) (source main) (view (rels (containers 0) (contents 1)))) R
       ** (skg (code (interp subscribeeCol))) it subscribes to these
       *** (skg (code (interp hiddenOutsideOfSubscribeeCol))) hidden from all subscriptions
       **** (skg (id H) (source main) (view (rels (containers 0))) (code (interp hiddenFromSubscribees) indefinitive)) H
       *** (skg (id E1) (source main) (view (rels (containers 0) (contents 2))) (code (interp subscribee))) E1
       **** (skg (id E11) (source main)) E11
       **** (skg (id E12) (source main) (view (rels (contents 1)))) E12
       ***** (skg (id E121) (source main)) E121
       *** (skg (id E2) (source main) (view (rels (containers 0))) (code (interp subscribee))) E2
       ** (skg (id R1) (source main)) R1
       "};
    assert_eq!(with_subscribees_expanded, expected_expanded,
      "View with expanded subscribees: H still in HiddenOutsideOfSubscribeeCol; E1 expanded with E11, E12; E2 expanded but empty");

    cleanup_test(
      db_name,
      &driver,
      &config.tantivy_folder,
    ).await?;
    Ok (( )) } ) }

/// Overlapping hidden within:
/// - R subscribes to E1, E2
/// - R hides H
/// - Both E1 and E2 contain H (and nothing else)
///
/// Tests two views:
/// - Initial view from R: E1, E2 are indefinitive (bare leaves), H doesn't appear
/// - View from R with definitive views expanded at each subscribee: H appears in HiddenInSubscribeeCol under BOTH E1 and E2
///
/// No HiddenOutsideOfSubscribeeCol in either state (H is in subscribees' content).
#[test]
fn test_overlapping_hidden_within(
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let db_name = "skg-test-overlapping-hidden-within";
    let (config, driver, tantivy) =
      setup_test(db_name,
                 "tests/hidden_from_subscriptions/fixtures-overlapping-hidden-within").await?;
    let initial_view: String = single_root_view(
      &driver,
      &config,
      &ID("R".to_string()), // root of the view
    ).await?;
    println!("Initial view from R:\n{}", initial_view);
    let expected_initial = indoc! {
      "* (skg (id R) (source main) (view (rels (containers 0) (contents 1)))) R
       ** (skg (code (interp subscribeeCol))) it subscribes to these
       *** (skg (id E1) (view (rels (containers 0) (contents 1))) (code (interp subscribee) indefinitive)) E1
       *** (skg (id E2) (view (rels (containers 0) (contents 1))) (code (interp subscribee) indefinitive)) E2
       ** (skg (id R1) (source main)) R1
       "};
    assert_eq!(initial_view, expected_initial,
      "Initial view from R: indefinitive subscribees are bare leaves; H doesn't appear");
    let expanded = {
      let modified_view : String =
        add_definitive_view_request_to_subscribees ( &initial_view );
      println!("Modified view (with definitive requests):\n{}",
               modified_view);
      let response = update_from_and_rerender_buffer (
        &modified_view, &driver, &config, &tantivy ) . await ?;
      response.buffer_content };
    println!("View from R after save with definitive view requests:\n{}", expanded);
    let expected_expanded = indoc! {
      "* (skg (id R) (source main) (view (rels (containers 0) (contents 1)))) R
       ** (skg (code (interp subscribeeCol))) it subscribes to these
       *** (skg (id E1) (source main) (view (rels (containers 0) (contents 1))) (code (interp subscribee))) E1
       **** (skg (code (interp hiddenInSubscribeeCol))) hidden from this subscription
       ***** (skg (id H) (source main) (view (rels (containers 2))) (code (interp hiddenFromSubscribees) indefinitive)) H
       *** (skg (id E2) (source main) (view (rels (containers 0) (contents 1))) (code (interp subscribee))) E2
       **** (skg (code (interp hiddenInSubscribeeCol))) hidden from this subscription
       ***** (skg (id H) (source main) (view (rels (containers 2))) (code (interp hiddenFromSubscribees) indefinitive)) H
       ** (skg (id R1) (source main)) R1
       "};
    assert_eq!(expanded, expected_expanded,
      "View with expanded subscribees: H appears in HiddenInSubscribeeCol under both E1 and E2");
    cleanup_test(
      db_name,
      &driver,
      &config.tantivy_folder,
    ).await?;
    Ok (( )) } ) }
