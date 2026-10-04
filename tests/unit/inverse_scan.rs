use super::*;
use crate::repo_sets::RepoSetName;
use crate::types::git::NodeChanges;
use crate::types::misc::MSV;
use crate::types::nodes::complete::empty_node_complete;
use crate::types::nodes::fs::GraphnodeOnDisk;
use std::collections::BTreeSet;

fn id (s : &str) -> ID { ID ( s . to_string () ) }
fn src (s : &str) -> RepoName { RepoName ( s . to_string () ) }

fn graphnode (
  pid       : &str,
  overrides : Vec<&str>,
) -> Graphnode {
  let node_fs : GraphnodeOnDisk =
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
) -> GraphnodeDiff {
  GraphnodeDiff {
    status : GitDiffStatus::Modified,
    node_changes : Some ( NodeChanges {
      overrides_view_of_diff : overrides_diff,
      .. NodeChanges::default () } ),
    before_node : None,
    after_node : None } }

fn deleted_entry (
  before : Graphnode,
) -> GraphnodeDiff {
  GraphnodeDiff {
    status : GitDiffStatus::Deleted,
    node_changes : None,
    before_node : Some (before),
    after_node : None } }

fn added_entry (
  after : Graphnode,
) -> GraphnodeDiff {
  GraphnodeDiff {
    status : GitDiffStatus::Added,
    node_changes : None,
    before_node : None,
    after_node : Some (after) } }

fn empty_repo_diff () -> RepoDiff {
  RepoDiff {
    is_gitrepo   : true,
    staged        : HashMap::new (),
    unstaged      : HashMap::new (),
    added_nodes   : HashMap::new (),
    deleted_nodes : HashMap::new () } }

#[test]
fn signs_come_from_modified_deleted_and_added_files_per_stage () {
  let owner : ID = id ("N");
  let mut sd : RepoDiff = empty_repo_diff ();
  // edge-r's Modified file removed its edge to N, STAGED.
  sd . staged . insert (
    PathBuf::from ("edge-r.skg"),
    modified_entry ( vec! [
      Diff_Item::Removed ( owner . clone () ) ] ));
  // del-r's file was Deleted UNSTAGED; its before_node named N.
  sd . unstaged . insert (
    PathBuf::from ("del-r.skg"),
    deleted_entry ( graphnode ("del-r", vec! ["N"]) ));
  // newfile-r's file was Added UNSTAGED; its after_node names N.
  sd . unstaged . insert (
    PathBuf::from ("newfile-r.skg"),
    added_entry ( graphnode ("newfile-r", vec! ["N"]) ));
  // bystander: a Deleted file that never named N.
  sd . unstaged . insert (
    PathBuf::from ("bystander.skg"),
    deleted_entry ( graphnode ("bystander", vec! []) ));
  let diffs : Option<HashMap<RepoName, RepoDiff>> =
    Some ( HashMap::from ([ ( src ("main"), sd ) ]) );
  let scan : HashMap<ID, RelationshipAxes> =
    inverse_scan_for_inbound_folder (
      &owner, NodeRelation::OverridesViewOf, &diffs, None );
  assert_eq! ( scan . len (), 3, "{:?}", scan );
  assert_eq! ( scan [ &id ("edge-r") ],
    RelationshipAxes { staged : Some (Sign::Minus), unstaged : None } );
  assert_eq! ( scan [ &id ("del-r") ],
    RelationshipAxes { staged : None, unstaged : Some (Sign::Minus) } );
  assert_eq! ( scan [ &id ("newfile-r") ],
    RelationshipAxes { staged : None, unstaged : Some (Sign::Plus) } );
  assert! ( ! scan [ &id ("del-r") ] . net_is_present () );
  assert! ( scan [ &id ("newfile-r") ] . net_is_present () );
}

#[test]
fn owner_absent_from_every_diff_yields_nothing () {
  let mut sd : RepoDiff = empty_repo_diff ();
  sd . unstaged . insert (
    PathBuf::from ("other.skg"),
    modified_entry ( vec! [
      Diff_Item::Removed ( id ("SOMEONE-ELSE") ) ] ));
  let diffs : Option<HashMap<RepoName, RepoDiff>> =
    Some ( HashMap::from ([ ( src ("main"), sd ) ]) );
  let scan : HashMap<ID, RelationshipAxes> =
    inverse_scan_for_inbound_folder (
      &id ("N"), NodeRelation::OverridesViewOf, &diffs, None );
  assert! ( scan . is_empty (), "{:?}", scan );
}

#[test]
fn each_relation_is_read_separately () {
  // One Modified member file removed its SUBSCRIPTION to N while its
  // override of N is untouched: the overriderFolder's scan must see
  // nothing, the subscriberFolder's scan the Minus.
  let owner : ID = id ("N");
  let mut sd : RepoDiff = empty_repo_diff ();
  sd . unstaged . insert (
    PathBuf::from ("m.skg"),
    GraphnodeDiff {
      status : GitDiffStatus::Modified,
      node_changes : Some ( NodeChanges {
        subscribes_to_diff : vec! [
          Diff_Item::Removed ( owner . clone () ) ],
        .. NodeChanges::default () } ),
      before_node : None,
      after_node : None } );
  let diffs : Option<HashMap<RepoName, RepoDiff>> =
    Some ( HashMap::from ([ ( src ("main"), sd ) ]) );
  assert! ( inverse_scan_for_inbound_folder (
      &owner, NodeRelation::OverridesViewOf, &diffs, None )
    . is_empty () );
  assert_eq! ( inverse_scan_for_inbound_folder (
      &owner, NodeRelation::SubscribesTo, &diffs, None ) [ &id ("m") ],
    RelationshipAxes { staged : None, unstaged : Some (Sign::Minus) } );
}

#[test]
fn cross_repo_move_yields_no_membership_change () {
  // mover's file leaves Skg repo A and lands in Skg repo B within the
  // same stage, asserting the same edge to N before and after: the
  // Minus and Plus must cancel, leaving no membership sign at all.
  let owner : ID = id ("N");
  let mut sd_a : RepoDiff = empty_repo_diff ();
  sd_a . unstaged . insert (
    PathBuf::from ("mover.skg"),
    deleted_entry ( graphnode ("mover", vec! ["N"]) ));
  let mut sd_b : RepoDiff = empty_repo_diff ();
  sd_b . unstaged . insert (
    PathBuf::from ("mover.skg"),
    added_entry ( graphnode ("mover", vec! ["N"]) ));
  let diffs : Option<HashMap<RepoName, RepoDiff>> =
    Some ( HashMap::from ([
      ( src ("a"), sd_a ),
      ( src ("b"), sd_b ) ]) );
  let scan : HashMap<ID, RelationshipAxes> =
    inverse_scan_for_inbound_folder (
      &owner, NodeRelation::OverridesViewOf, &diffs, None );
  assert! ( scan . is_empty (),
    "a move must not fabricate a membership change: {:?}", scan );
}

#[test]
fn relRepo_gates_deleted_stage_signs () {
  // del-r's file was Deleted; its before_node's override of N was
  // recorded in the PRIVATE Skg repo (a RelPartner whose Skg repo
  // differs from del-r's own -- public -- home). A public-only
  // active set must not see the resulting phantom sign; ungated
  // (None) still does.
  let owner : ID = id ("N");
  let mut before : Graphnode = empty_node_complete ();
  before . pid = id ("del-r");
  before . title = "del-r" . to_string ();
  before . home_repo = src ("public");
  before . overrides_view_of = MSV::Specified ( vec! [
    RelPartner::at_relRepo ( src ("private"), owner . clone () ) ] );
  let mut sd : RepoDiff = empty_repo_diff ();
  sd . unstaged . insert (
    PathBuf::from ("del-r.skg"),
    deleted_entry ( before ) );
  let diffs : Option<HashMap<RepoName, RepoDiff>> =
    Some ( HashMap::from ([ ( src ("public"), sd ) ]) );
  let public_only : ActiveRepoSet = ActiveRepoSet {
    name    : RepoSetName::from ("public"),
    repos : BTreeSet::from ([ src ("public") ]) };
  let gated : HashMap<ID, RelationshipAxes> =
    inverse_scan_for_inbound_folder (
      &owner, NodeRelation::OverridesViewOf, &diffs,
      Some (&public_only) );
  assert! ( gated . is_empty (),
    "a Deleted-stage sign recorded at an inactive repo must not \
     surface: {:?}", gated );
  let ungated : HashMap<ID, RelationshipAxes> =
    inverse_scan_for_inbound_folder (
      &owner, NodeRelation::OverridesViewOf, &diffs, None );
  assert! ( ! ungated . is_empty (),
    "ungated (None) scan should still see the Deleted-stage sign: {:?}",
    ungated );
}
