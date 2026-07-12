// cargo nextest run --test grouped_saves -E 'test(merge::merge_hides_intersection)'
//
// TODO/more.org, "Take something like the intersection of hides when
// merging": whatever EITHER member showed as unintegrated subscribed
// content pre-merge -- contained in one of its subscribees, unhidden
// by and not contained in that member -- the merged node must keep
// showing, so such ids are dropped from the combined hides. Hides
// nobody's view contradicted survive the union.
//
// Fixture: a hides [x, z]; b subscribes to e and hides [y];
// e contains [x, y]. So pre-merge b SHOWS x (e contains it, b
// neither hides nor contains it); nobody shows y (b hides it, a
// cannot see it); nobody shows z (no subscribee contains it).

use skg::nodeMerge::nodeMergeInstructionTriple::nodeMerge_instructions_from_pairs;
use skg::test_utils::run_with_shared_test_db;
use skg::types::misc::{ID, PrivaciedMember};
use skg::types::save::NodeMerge;

use std::error::Error;

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_db (
    "skg-test-merge-hides-intersection",
    |s| Box::pin ( async move {
      s . reset ("merge_drops_hides_the_other_member_contradicted",
                 "tests/merge/merge_hides_intersection/fixtures") . await ?;
      let merges : Vec<NodeMerge> =
        nodeMerge_instructions_from_pairs (
          & [ ( ID::from ("a"), ID::from ("b") ) ],
          &s . config, &s . driver ) . await ?;
      assert_eq! ( merges . len (), 1 );
      let hides : &[PrivaciedMember<ID>] =
        merges[0] . updated_acquirer . 0
        . hides_from_its_subscriptions . or_default ();
      assert! ( ! hides . iter () . any (|m| m . member == ID::from ("x")),
        "b showed x through e pre-merge, so the merge must not hide it: {:?}",
        hides );
      assert! ( hides . iter () . any (|m| m . member == ID::from ("y")),
        "nobody showed y (b hid it; a could not see it), so its hide survives: {:?}",
        hides );
      assert! ( hides . iter () . any (|m| m . member == ID::from ("z")),
        "nobody showed z (no subscribee contains it), so its hide survives: {:?}",
        hides );
      Ok (( )) } )) }
