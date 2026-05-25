// cargo test --test source_sets
//
// These are feature-first tests for TODO/source-sets/plan.org. They
// intentionally name the source-set API before the implementation
// exists, and should fail until that feature is wired in.

use indoc::indoc;
use ego_tree::Tree;

use skg::dbs::filesystem::not_nodes::load_config;
use skg::source_sets::{
  ActiveSourceSet,
  SourceSetName,
  filter_path_to_active_sources_for_test,
  filter_branches_to_active_sources_for_test,
  save_inactive_placeholder_buffer_for_test,
  run_with_source_set_test_db};
use skg::to_org::render::content_view::multi_root_view_with_source_set;
use skg::types::misc::{ID, SourceName};
use skg::types::viewnode::ViewNode;

use std::collections::BTreeSet;
use std::error::Error;

#[test]
fn config_loads_default_source_set_and_named_source_sets (
) -> Result<(), Box<dyn Error>> {
  let config =
    load_config ("tests/source_sets/fixtures/skgconfig.toml")?;
  assert_eq! (
    config . default_source_set_name (),
    &SourceSetName::from ("public"));
  assert_eq! (
    config . source_set_sources (&SourceSetName::from ("public"))?,
    BTreeSet::from ([SourceName::from ("public")]));
  assert_eq! (
    config . source_set_sources (&SourceSetName::from ("all"))?,
    BTreeSet::from ([
      SourceName::from ("private"),
      SourceName::from ("public")]));
  Ok (( )) }

#[test]
fn config_rejects_reserved_all_source_and_source_set_names (
) {
  let source_all =
    load_config ("tests/source_sets/fixtures-invalid/source-all/skgconfig.toml");
  assert! (
    source_all . is_err (),
    "configured source named all must be rejected" );
  let source_set_all =
    load_config ("tests/source_sets/fixtures-invalid/source-set-all/skgconfig.toml");
  assert! (
    source_set_all . is_err (),
    "user-defined source-set named all must be rejected" );
}

#[test]
fn content_view_renders_inactive_contained_nodes_as_placeholders (
) -> Result<(), Box<dyn Error>> {
  run_with_source_set_test_db (
    "skg-test-source-sets-content-placeholder",
    "tests/source_sets/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-source-sets-content-placeholder",
    |config, driver, _tantivy| Box::pin ( async move {
      let active : ActiveSourceSet =
        ActiveSourceSet::named (
          &config,
          SourceSetName::from ("public"))?;
      let (actual, _pids, _viewforest) : (String, Vec<ID>, Tree<ViewNode>) =
        multi_root_view_with_source_set (
          driver, config, None,
          &[ID::from ("root")],
          false,
          &active ) . await ?;
      assert! (actual . contains (
        "(skg (inactiveNode (id private-a) (source private))) node from inactive source"),
        "inactive contained node should render as a placeholder: {}",
        actual );
      assert! (actual . contains ("active-a"));
      assert! (actual . contains ("active-b"));
      assert! (
        ! actual . contains ("private title must not leak"),
        "inactive placeholder must not reveal title: {}",
        actual );
      Ok (( )) } )) }

#[test]
fn diff_view_marks_inactive_placeholders_newhere_and_removedhere_without_content_leak (
) -> Result<(), Box<dyn Error>> {
  run_with_source_set_test_db (
    "skg-test-source-sets-diff-placeholder",
    "tests/source_sets/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-source-sets-diff-placeholder",
    |config, driver, _tantivy| Box::pin ( async move {
      let active : ActiveSourceSet =
        ActiveSourceSet::named (
          &config,
          SourceSetName::from ("public"))?;
      let (actual, _pids, _viewforest) : (String, Vec<ID>, Tree<ViewNode>) =
        multi_root_view_with_source_set (
          driver, config, None,
          &[ID::from ("diff-root")],
          true,
          &active ) . await ?;
      assert! (
        actual . contains (
          "(skg (inactiveNode (id private-new) (source private) (unstaged newM))) node from inactive source"),
        "inactive new-here placeholder should carry only relationship-position diff metadata: {}",
        actual );
      assert! (
        actual . contains (
          "(skg (inactiveNode (id private-removed) (source private) (unstaged removedM))) node from inactive source"),
        "inactive removed-here placeholder should carry only relationship-position diff metadata: {}",
        actual );
      for forbidden in [
        "private new title must not leak",
        "private removed title must not leak",
        "private body must not leak",
        "source private) private",
      ] {
        assert! (
          ! actual . contains (forbidden),
          "inactive diff placeholder leaked '{}': {}",
          forbidden,
          actual ); }
      Ok (( )) } )) }

#[test]
fn search_filters_inactive_sources_before_ranking_and_truncation (
) -> Result<(), Box<dyn Error>> {
  run_with_source_set_test_db (
    "skg-test-source-sets-search-filtering",
    "tests/source_sets/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-source-sets-search-filtering",
    |config, _driver, tantivy| Box::pin ( async move {
      let active : ActiveSourceSet =
        ActiveSourceSet::named (
          &config,
          SourceSetName::from ("public"))?;
      let ids : Vec<ID> =
        skg::serve::handlers::text_search::search_ids_for_source_set_for_test (
          &tantivy,
          &config,
          &active,
          "shared ranking term",
          2 )?;
      assert_eq! (
        ids,
        vec![ID::from ("active-search-hit")],
        "inactive high-scoring hits must be filtered before ranking \
         and display truncation" );
      Ok (( )) } )) }

#[test]
fn saving_inactive_placeholder_moves_and_deletes_update_contains (
) -> Result<(), Box<dyn Error>> {
  let buffer = indoc! {"
    * (skg (node (id root) (source public))) root
    ** (skg (node (id active-b) (source public))) active-b
    ** (skg (inactiveNode (id private-a) (source private))) node from inactive source
  "};
  let saved =
    save_inactive_placeholder_buffer_for_test (
      "tests/source_sets/fixtures/skgconfig.toml",
      buffer)?;
  assert_eq! (
    saved . contains_for (&ID::from ("root"))?,
    vec![ID::from ("active-b"), ID::from ("private-a")],
    "moving an inactive placeholder should reorder the active \
     container's contains list" );
  let buffer = indoc! {"
    * (skg (node (id root) (source public))) root
    ** (skg (node (id active-b) (source public))) active-b
  "};
  let saved =
    save_inactive_placeholder_buffer_for_test (
      "tests/source_sets/fixtures/skgconfig.toml",
      buffer)?;
  assert_eq! (
    saved . contains_for (&ID::from ("root"))?,
    vec![ID::from ("active-b")],
    "deleting an inactive placeholder should remove that inactive \
     ID from contains" );
  Ok (( )) }

#[test]
fn saving_edits_to_inactive_placeholder_content_are_rejected (
) {
  let buffer = indoc! {"
    * (skg (node (id root) (source public))) root
    ** (skg (inactiveNode (id private-a) (source private))) edited title
    This body edit should be rejected.
  "};
  let result =
    save_inactive_placeholder_buffer_for_test (
      "tests/source_sets/fixtures/skgconfig.toml",
      buffer);
  assert! (
    result . is_err (),
    "editing inactive placeholder title/body should be rejected" );
}

#[test]
fn backward_path_truncates_before_first_inactive_node (
) -> Result<(), Box<dyn Error>> {
  let config =
    load_config ("tests/source_sets/fixtures/skgconfig.toml")?;
  let active : ActiveSourceSet =
    ActiveSourceSet::named (
      &config,
      SourceSetName::from ("public"))?;
  let path : Vec<ID> =
    vec![
      ID::from ("active-container"),
      ID::from ("private-container"),
      ID::from ("active-root-after-private") ];
  assert_eq! (
    filter_path_to_active_sources_for_test (&config, &active, path)?,
    vec![ID::from ("active-container")],
    "mid-path filtering should keep exactly the active prefix \
     and stop before the first inactive node" );
  Ok (( )) }

#[test]
fn backward_path_filters_forks_per_branch_and_omits_empty_forks (
) -> Result<(), Box<dyn Error>> {
  let config =
    load_config ("tests/source_sets/fixtures/skgconfig.toml")?;
  let active : ActiveSourceSet =
    ActiveSourceSet::named (
      &config,
      SourceSetName::from ("public"))?;
  let mixed_branches : BTreeSet<ID> =
    BTreeSet::from ([
      ID::from ("active-fork-branch"),
      ID::from ("private-fork-branch")]);
  assert_eq! (
    filter_branches_to_active_sources_for_test (
      &config, &active, mixed_branches)?,
    BTreeSet::from ([ID::from ("active-fork-branch")]),
    "partially inactive forks should render only active branches" );
  let inactive_branches : BTreeSet<ID> =
    BTreeSet::from ([
      ID::from ("private-fork-branch"),
      ID::from ("private-other-branch")]);
  assert! (
    filter_branches_to_active_sources_for_test (
      &config, &active, inactive_branches)?
    . is_empty (),
    "fully inactive forks should not render an empty fork scaffold" );
  Ok (( )) }

#[test]
fn titles_by_ids_omits_inactive_source_titles (
) -> Result<(), Box<dyn Error>> {
  let config =
    load_config ("tests/source_sets/fixtures/skgconfig.toml")?;
  let active : ActiveSourceSet =
    ActiveSourceSet::named (
      &config,
      SourceSetName::from ("public"))?;
  let titles =
    skg::serve::handlers::titles_by_ids::titles_by_ids_for_source_set_for_test (
      &config,
      &active,
      &[ ID::from ("active-a"),
         ID::from ("private-a") ])?;
  assert_eq! (
    titles . get (&ID::from ("active-a")),
    Some (&"active-a" . to_string ()));
  assert! (
    ! titles . contains_key (&ID::from ("private-a")),
    "inactive-source title lookup must omit private-a" );
  Ok (( )) }
