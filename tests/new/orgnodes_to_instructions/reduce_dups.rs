// I spec'd these tests by hand.
// They are much briefer than those by Claude
// (at tests/new/orgnodes_to_instructions/reduce_dups/by_claude.rs),
// but they catch most of the tricky logic.

use indoc::indoc;
use skg::new::{org_to_uninterpreted_nodes2, interpret_forest, find_inconsistent_instructions};
use skg::new::orgnodes_to_instructions::reduce_dups::reduce_instructions;
use skg::typedb::init::populate_test_db_from_fixtures;
use skg::types::{ID, SkgConfig};
use typedb_driver::{TypeDBDriver, Credentials, DriverOptions};
use std::path::PathBuf;
use std::error::Error;

struct TestContext {
  config: SkgConfig,
  driver: Option<TypeDBDriver>,
}

impl TestContext {
  async fn new() -> Self {
    match Self::setup_with_fixtures().await {
      Ok((config, driver)) => Self { config, driver: Some(driver) },
      Err(_) => {
        let config = SkgConfig {
          db_name: "skg-test-reduce-dups".to_string(),
          skg_folder: PathBuf::from("tests/new/orgnodes_to_instructions/reduce_dups/fixtures"),
          tantivy_folder: PathBuf::from("/tmp/test/tantivy"),
          port: 1730,
        };
        Self { config, driver: None }
      }
    }
  }

  async fn setup_with_fixtures() -> Result<(SkgConfig, TypeDBDriver), Box<dyn Error>> {
    let config = SkgConfig {
      db_name: "skg-test-reduce-dups".to_string(),
      skg_folder: PathBuf::from("tests/new/orgnodes_to_instructions/reduce_dups/fixtures"),
      tantivy_folder: PathBuf::from("/tmp/test/tantivy"),
      port: 1730,
    };

    let driver = TypeDBDriver::new(
      "127.0.0.1:1729",
      Credentials::new("admin", "password"),
      DriverOptions::new(false, None)?
    ).await?;

    let fixtures_folder = config.skg_folder.to_str()
      .ok_or("Invalid UTF-8 in fixtures path")?;

    populate_test_db_from_fixtures(
      fixtures_folder,
      &config.db_name,
      &driver
    ).await?;

    Ok((config, driver))
  }

  async fn run_pipeline(&self, input: &str) -> Result<Vec<(skg::types::SkgNode, skg::types::NodeSaveAction)>, Box<dyn std::error::Error>> {
    let trees = org_to_uninterpreted_nodes2(input)?;
    let instructions = interpret_forest(trees)?;

    if let Some(driver) = &self.driver {
      reduce_instructions(&self.config, driver, instructions).await
    } else {
      Err("TypeDB not available".into())
    }
  }
}

fn test_inconsistent_delete() {
  let input = indoc! {"
        * <skg<id:1>> 1
        * <skg<id:1,toDelete>> 2
    "};

  let trees = org_to_uninterpreted_nodes2(input).unwrap();
  let (inconsistent_deletes, _) = find_inconsistent_instructions(&trees);
  assert!(!inconsistent_deletes.is_empty(),
          "Should detect inconsistent toDelete");

  let instructions = interpret_forest(trees).unwrap();
  assert_eq!(instructions.len(), 2);
  assert_eq!(instructions[0].0.ids[0], ID::from("1"));
  assert_eq!(instructions[1].0.ids[0], ID::from("1"));
  assert_ne!(instructions[0].1.toDelete, instructions[1].1.toDelete);
}

async fn test_deletions_excluded(ctx: &TestContext) {
  let input = indoc! {"
        * <skg<id:1>> 1
        ** <skg<id:2,toDelete>> 2
        ** <skg<id:3>> 3
    "};

  if let Ok(reduced) = ctx.run_pipeline(input).await {
    assert_eq!(reduced.len(), 3); // There are 3 instructions.
    let id1_instruction = reduced.iter()
      .find(|(node, _)| node.ids.contains(&ID::from("1")))
      .expect("Should have instruction for id:1");
    let id2_instruction = reduced.iter()
      .find(|(node, _)| node.ids.contains(&ID::from("2")))
      .expect("Should have instruction for id:2");
    assert_eq!(id1_instruction.1.toDelete, false);
    assert_eq!(id2_instruction.1.toDelete, true);
    assert_eq!( // id 1 should contain 3 and not 2 (which is being deleted)
      id1_instruction.0.contains, vec![ID::from("3")]);
  }
}

async fn test_defining_node_defines(ctx: &TestContext) {
  let input = indoc! {"
        * <skg<id:1,mightContainMore>> 1 adder
        Ignored body.
        ** <skg<id:2>> 2
        * <skg<id:1>> 1 definer
        ** <skg<id:3>> 3
    "};

  if let Ok(reduced) = ctx.run_pipeline(input).await {
    assert_eq!(reduced.len(), 3); // 3 unique ids (id 1 is dup'd)
    let id1_instruction = reduced.iter()
      .find(|(node, _)| node.ids.contains(&ID::from("1")))
      .unwrap();
    assert_eq!(id1_instruction.0.title, "1 definer");
    // Defining instruction should define body completely, even if None
    assert_eq!(id1_instruction.0.body, None);
    assert_eq!(id1_instruction.0.contains, vec![ID::from("3"), ID::from("2")]);
  }
}

async fn test_adding_without_definer(ctx: &TestContext) {
  let input = indoc! {"
        * <skg<id:1,mightContainMore>> 1 adder
        ** <skg<id:2>> 2
        ** <skg<id:4>> 4
        ** <skg<id:4,mightContainMore>> 4 again
    "};

  if let Ok(reduced) = ctx.run_pipeline(input).await {
    let id1_instruction = reduced.iter()
      .find(|(node, _)| node.ids.contains(&ID::from("1")))
      .expect("Should have instruction for id:1");

    assert_eq!(
      // even a mightContainMore will clobber the title on disk
      id1_instruction.0.title,
      "1 adder");
    assert_eq!(
      // Body comes from disk since no instruction provides one.
      id1_instruction.0.body,
      Some("body from disk".to_string()));
    assert_eq!(
      // Since there is no defining node, contents are read from disk,
      // and then the mightContainMore node with id 1
      // appends its contents, with deduplication.
      id1_instruction.0.contains,
      vec![ ID::from("2"),
            ID::from("3"),
            ID::from("4"), ]); }}

#[tokio::test]
async fn test_reduce_instructions_pipeline() {
  let ctx = TestContext::new().await;

  test_inconsistent_delete();
  test_deletions_excluded(&ctx).await;
  test_defining_node_defines(&ctx).await;
  test_adding_without_definer(&ctx).await;

  if ctx.driver.is_none() {
    println!("TypeDB not available, skipped driver-dependent tests");
  }
}
