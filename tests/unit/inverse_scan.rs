use super::*;
use crate::types::git::NodeChanges;
use crate::types::nodes::fs::NodeFS;

fn id (s : &str) -> ID { ID ( s . to_string () ) }
fn src (s : &str) -> SourceName { SourceName ( s . to_string () ) }

fn nodecomplete (
  pid       : &str,
  overrides : Vec<&str>,
) -> NodeComplete {
  let node_fs : NodeFS =
    serde_yaml::from_str ( & format! (
      "pid: '{}'\ntitle: \"{}\"\n{}",
      pid, pid,
      if overrides . is_empty () { String::new () }
      else { format! (
        "overrides_view_of:\n{}",
        overrides . iter ()
          . map ( |o| format! ("  - \"{}\"\n", o) )
          . collect::<String> () ) } )) . unwrap ();
  node_fs . into_complete_as_single_section ( src ("main") ) }

fn modified_entry (
  overrides_diff : Vec<Diff_Item<ID>>,
) -> NodeCompleteDiff {
  NodeCompleteDiff {
    status : GitDiffStatus::Modified,
    node_changes : Some ( NodeChanges {
      overrides_view_of_diff : overrides_diff,
      .. NodeChanges::default () } ),
    before_node : None,
    after_node : None } }

fn deleted_entry (
  before : NodeComplete,
) -> NodeCompleteDiff {
  NodeCompleteDiff {
    status : GitDiffStatus::Deleted,
    node_changes : None,
    before_node : Some (before),
    after_node : None } }

fn added_entry (
  after : NodeComplete,
) -> NodeCompleteDiff {
  NodeCompleteDiff {
    status : GitDiffStatus::Added,
    node_changes : None,
    before_node : None,
    after_node : Some (after) } }

fn empty_source_diff () -> SourceDiff {
  SourceDiff {
    is_git_repo   : true,
    staged        : HashMap::new (),
    unstaged      : HashMap::new (),
    added_nodes   : HashMap::new (),
    deleted_nodes : HashMap::new () } }

#[test]
fn signs_come_from_modified_deleted_and_added_files_per_stage () {
  let owner : ID = id ("N");
  let mut sd : SourceDiff = empty_source_diff ();
  // edge-r's Modified file removed its edge to N, STAGED.
  sd . staged . insert (
    PathBuf::from ("edge-r.skg"),
    modified_entry ( vec! [
      Diff_Item::Removed ( owner . clone () ) ] ));
  // del-r's file was Deleted UNSTAGED; its before_node named N.
  sd . unstaged . insert (
    PathBuf::from ("del-r.skg"),
    deleted_entry ( nodecomplete ("del-r", vec! ["N"]) ));
  // newfile-r's file was Added UNSTAGED; its after_node names N.
  sd . unstaged . insert (
    PathBuf::from ("newfile-r.skg"),
    added_entry ( nodecomplete ("newfile-r", vec! ["N"]) ));
  // bystander: a Deleted file that never named N.
  sd . unstaged . insert (
    PathBuf::from ("bystander.skg"),
    deleted_entry ( nodecomplete ("bystander", vec! []) ));
  let diffs : Option<HashMap<SourceName, SourceDiff>> =
    Some ( HashMap::from ([ ( src ("main"), sd ) ]) );
  let scan : HashMap<ID, MembershipAxes> =
    inverse_scan_for_inbound_col (
      &owner, NodeRelation::OverridesViewOf, &diffs );
  assert_eq! ( scan . len (), 3, "{:?}", scan );
  assert_eq! ( scan [ &id ("edge-r") ],
    MembershipAxes { staged : Some (Sign::Minus), unstaged : None } );
  assert_eq! ( scan [ &id ("del-r") ],
    MembershipAxes { staged : None, unstaged : Some (Sign::Minus) } );
  assert_eq! ( scan [ &id ("newfile-r") ],
    MembershipAxes { staged : None, unstaged : Some (Sign::Plus) } );
  assert! ( ! scan [ &id ("del-r") ] . net_is_present () );
  assert! ( scan [ &id ("newfile-r") ] . net_is_present () );
}

#[test]
fn owner_absent_from_every_diff_yields_nothing () {
  let mut sd : SourceDiff = empty_source_diff ();
  sd . unstaged . insert (
    PathBuf::from ("other.skg"),
    modified_entry ( vec! [
      Diff_Item::Removed ( id ("SOMEONE-ELSE") ) ] ));
  let diffs : Option<HashMap<SourceName, SourceDiff>> =
    Some ( HashMap::from ([ ( src ("main"), sd ) ]) );
  let scan : HashMap<ID, MembershipAxes> =
    inverse_scan_for_inbound_col (
      &id ("N"), NodeRelation::OverridesViewOf, &diffs );
  assert! ( scan . is_empty (), "{:?}", scan );
}

#[test]
fn each_relation_is_read_separately () {
  // One Modified member file removed its SUBSCRIPTION to N while its
  // override of N is untouched: the overriderCol's scan must see
  // nothing, the subscriberCol's scan the Minus.
  let owner : ID = id ("N");
  let mut sd : SourceDiff = empty_source_diff ();
  sd . unstaged . insert (
    PathBuf::from ("m.skg"),
    NodeCompleteDiff {
      status : GitDiffStatus::Modified,
      node_changes : Some ( NodeChanges {
        subscribes_to_diff : vec! [
          Diff_Item::Removed ( owner . clone () ) ],
        .. NodeChanges::default () } ),
      before_node : None,
      after_node : None } );
  let diffs : Option<HashMap<SourceName, SourceDiff>> =
    Some ( HashMap::from ([ ( src ("main"), sd ) ]) );
  assert! ( inverse_scan_for_inbound_col (
      &owner, NodeRelation::OverridesViewOf, &diffs )
    . is_empty () );
  assert_eq! ( inverse_scan_for_inbound_col (
      &owner, NodeRelation::Subscribes, &diffs ) [ &id ("m") ],
    MembershipAxes { staged : None, unstaged : Some (Sign::Minus) } );
}

#[test]
fn cross_source_move_yields_no_membership_change () {
  // mover's file leaves source A and lands in source B within the
  // same stage, asserting the same edge to N before and after: the
  // Minus and Plus must cancel, leaving no membership sign at all.
  let owner : ID = id ("N");
  let mut sd_a : SourceDiff = empty_source_diff ();
  sd_a . unstaged . insert (
    PathBuf::from ("mover.skg"),
    deleted_entry ( nodecomplete ("mover", vec! ["N"]) ));
  let mut sd_b : SourceDiff = empty_source_diff ();
  sd_b . unstaged . insert (
    PathBuf::from ("mover.skg"),
    added_entry ( nodecomplete ("mover", vec! ["N"]) ));
  let diffs : Option<HashMap<SourceName, SourceDiff>> =
    Some ( HashMap::from ([
      ( src ("a"), sd_a ),
      ( src ("b"), sd_b ) ]) );
  let scan : HashMap<ID, MembershipAxes> =
    inverse_scan_for_inbound_col (
      &owner, NodeRelation::OverridesViewOf, &diffs );
  assert! ( scan . is_empty (),
    "a move must not fabricate a membership change: {:?}", scan );
}
