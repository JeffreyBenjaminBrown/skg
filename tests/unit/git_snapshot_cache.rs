// The HEAD snapshot cache keeps the snapshot of one key: reading a
// snapshot for a new key (a moved HEAD) drops the one before it.

use super::*;

#[test]
fn a_snapshot_read_for_a_new_head_replaces_the_one_before () {
  let mut cache : GitSnapshotCache = GitSnapshotCache::default ();
  assert! ( cache . get ("OwnedHead|old") . is_none () );
  cache . keep_only ( "OwnedHead|old" . to_string (),
                      GraphSnapshot::default () );
  assert! ( cache . get ("OwnedHead|old") . is_some () );
  cache . keep_only ( "OwnedHead|new" . to_string (),
                      GraphSnapshot::default () );
  assert! ( cache . get ("OwnedHead|old") . is_none (),
            "the old HEAD's snapshot is dropped" );
  assert! ( cache . get ("OwnedHead|new") . is_some () );
  cache . clear ();
  assert! ( cache . get ("OwnedHead|new") . is_none () ); }
