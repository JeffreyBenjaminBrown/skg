use skg::test_utils::load_config_with_overrides;
use skg::test_utils::single_root_view_text;
use skg::types::config::SkgConfig;
use skg::types::misc::ID;
use std::error::Error;

#[test]
fn one_forkee_has_contextual_counts_under_two_forkers (
) -> Result<(), Box<dyn Error>> {
  let config : SkgConfig = load_config_with_overrides (
    "tests/forkee_heralds/fixtures/skgconfig.toml",
    Some ("skg-test-forkee-heralds"), &[] ) ?;
  for (forker, numerator) in [("A1", 1), ("A2", 2)] {
    let view : String = single_root_view_text (
      &config, None, &ID::from (forker) ) ?;
    let b_line : &str = view . lines ()
      . find (|line| line . contains ("(gnid B)"))
      . expect ("shared forkee in view");
    assert! (b_line . contains (&format! (
      "(contains (out 3 (unintegrated {}))", numerator)),
      "{} must report its own U over B's graph-level D: {}",
      forker, view);
  }
  Ok (( )) }
