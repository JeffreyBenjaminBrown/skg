use skg::dbs::filesystem::not_nodes::load_config_with_overrides;
use skg::to_org::render::content_view::single_root_view;
use skg::types::misc::{ID, SkgConfig};
use std::error::Error;

#[test]
fn one_subscribee_has_contextual_counts_under_two_subscribers (
) -> Result<(), Box<dyn Error>> {
  let config : SkgConfig = load_config_with_overrides (
    "tests/subscribee_heralds/fixtures/skgconfig.toml",
    Some ("skg-test-subscribee-heralds"), &[] ) ?;
  for (subscriber, numerator) in [("A1", 1), ("A2", 2)] {
    let (view, _, _) : (String, Vec<ID>, _) = single_root_view (
      &config, None, &ID::from (subscriber), false ) ?;
    let b_line : &str = view . lines ()
      . find (|line| line . contains ("(id B)"))
      . expect ("shared subscribee in view");
    assert! (b_line . contains (&format! (
      "(contains (out 3 (unintegrated {}))", numerator)),
      "{} must report its own U over B's graph-level D: {}",
      subscriber, view);
  }
  Ok (( )) }
