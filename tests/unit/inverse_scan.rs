use super::*;
use crate::skgrepo_sets::SkgRepoSetName;
use crate::types::git::NodeChanges;
use crate::types::misc::MSV;
use crate::types::nodes::complete::empty_graphnode;
use crate::types::nodes::fs::GraphnodeOnDisk;
use std::collections::BTreeSet;

fn skgid (s : &str) -> ID { ID ( s . to_string () ) }
fn src (s : &str) -> SkgRepoName { SkgRepoName ( s . to_string () ) }

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
        "overrides:\n{}",
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
      overrides_diff : overrides_diff,
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

fn empty_skgrepo_diff () -> SkgRepoDiff {
  SkgRepoDiff {
    is_gitrepo   : true,
    staged        : HashMap::new (),
    unstaged      : HashMap::new (),
    added_nodes   : HashMap::new (),
    deleted_nodes : HashMap::new () } }

#[test]
fn signs_come_from_modified_deleted_and_added_files_per_stage () {
  let recorder : ID = skgid ("N");
  let mut sd   : SkgRepoDiff = empty_skgrepo_diff ();
  // edge-r's Modified file removed its relationship to N, STAGED.
  sd . staged . insert (
    PathBuf::from ("edge-r.skg"),
    modified_entry ( vec! [
      Diff_Item::Removed ( recorder . clone () ) ] ));
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
  let diffs : Option<HashMap<SkgRepoName, SkgRepoDiff>> =
    Some ( HashMap::from ([ ( src ("main"), sd ) ]) );
  let scan : HashMap<ID, RelationshipAxes> =
    inverse_scan_for_inbound_folder (
      &recorder, NodeRelation::Overrides, &diffs, None );
  assert_eq! ( scan . len (), 3, "{:?}", scan );
  assert_eq! ( scan [ &skgid ("edge-r") ],
    RelationshipAxes { staged : Some (Sign::Minus), unstaged : None } );
  assert_eq! ( scan [ &skgid ("del-r") ],
    RelationshipAxes { staged : None, unstaged : Some (Sign::Minus) } );
  assert_eq! ( scan [ &skgid ("newfile-r") ],
    RelationshipAxes { staged : None, unstaged : Some (Sign::Plus) } );
  assert! ( ! scan [ &skgid ("del-r") ] . net_is_present () );
  assert! ( scan [ &skgid ("newfile-r") ] . net_is_present () );
}

#[test]
fn recorder_absent_from_every_diff_yields_nothing () {
  let mut sd : SkgRepoDiff = empty_skgrepo_diff ();
  sd . unstaged . insert (
    PathBuf::from ("other.skg"),
    modified_entry ( vec! [
      Diff_Item::Removed ( skgid ("SOMEONE-ELSE") ) ] ));
  let diffs : Option<HashMap<SkgRepoName, SkgRepoDiff>> =
    Some ( HashMap::from ([ ( src ("main"), sd ) ]) );
  let scan : HashMap<ID, RelationshipAxes> =
    inverse_scan_for_inbound_folder (
      &skgid ("N"), NodeRelation::Overrides, &diffs, None );
  assert! ( scan . is_empty (), "{:?}", scan );
}

#[test]
fn each_relation_is_read_separately () {
  // One Modified member file removed its SUBSCRIPTION to N while its
  // override of N is untouched: the overriderFolder's scan must see
  // nothing, the subscriberFolder's scan the Minus.
  let recorder : ID = skgid ("N");
  let mut sd   : SkgRepoDiff = empty_skgrepo_diff ();
  sd . unstaged . insert (
    PathBuf::from ("m.skg"),
    GraphnodeDiff {
      status : GitDiffStatus::Modified,
      node_changes : Some ( NodeChanges {
        subscribesTo_diff : vec! [
          Diff_Item::Removed ( recorder . clone () ) ],
        .. NodeChanges::default () } ),
      before_node : None,
      after_node : None } );
  let diffs : Option<HashMap<SkgRepoName, SkgRepoDiff>> =
    Some ( HashMap::from ([ ( src ("main"), sd ) ]) );
  assert! ( inverse_scan_for_inbound_folder (
      &recorder, NodeRelation::Overrides, &diffs, None )
    . is_empty () );
  assert_eq! ( inverse_scan_for_inbound_folder (
      &recorder, NodeRelation::SubscribesTo, &diffs, None ) [ &skgid ("m") ],
    RelationshipAxes { staged : None, unstaged : Some (Sign::Minus) } );
}

#[test]
fn cross_skgrepo_move_yields_no_membership_change () {
  // mover's file leaves skgrepo A and lands in skgrepo B within the
  // same stage, asserting the same relationship to N before and after: the
  // Minus and Plus must cancel, leaving no membership sign at all.
  let recorder : ID = skgid ("N");
  let mut sd_a : SkgRepoDiff = empty_skgrepo_diff ();
  sd_a . unstaged . insert (
    PathBuf::from ("mover.skg"),
    deleted_entry ( graphnode ("mover", vec! ["N"]) ));
  let mut sd_b : SkgRepoDiff = empty_skgrepo_diff ();
  sd_b . unstaged . insert (
    PathBuf::from ("mover.skg"),
    added_entry ( graphnode ("mover", vec! ["N"]) ));
  let diffs : Option<HashMap<SkgRepoName, SkgRepoDiff>> =
    Some ( HashMap::from ([
      ( src ("a"), sd_a ),
      ( src ("b"), sd_b ) ]) );
  let scan : HashMap<ID, RelationshipAxes> =
    inverse_scan_for_inbound_folder (
      &recorder, NodeRelation::Overrides, &diffs, None );
  assert! ( scan . is_empty (),
    "a move must not fabricate a membership change: {:?}", scan );
}

#[test]
fn relRepo_gates_deleted_stage_signs () {
  // del-r's file was Deleted; its before_node's override of N was
  // recorded in the PRIVATE skgrepo (a RelPartner whose skgrepo
  // differs from del-r's own -- public -- home). A public-only
  // active set must not see the resulting phantom sign; ungated
  // (None) still does.
  let recorder      : ID = skgid ("N");
  let mut before : Graphnode = empty_graphnode ();
  before . pid = skgid ("del-r");
  before . title = "del-r" . to_string ();
  before . home_skgrepo = src ("public");
  before . overrides = MSV::Specified ( vec! [
    RelPartner::at_relRepo ( src ("private"), recorder . clone () ) ] );
  let mut sd : SkgRepoDiff = empty_skgrepo_diff ();
  sd . unstaged . insert (
    PathBuf::from ("del-r.skg"),
    deleted_entry ( before ) );
  let diffs : Option<HashMap<SkgRepoName, SkgRepoDiff>> =
    Some ( HashMap::from ([ ( src ("public"), sd ) ]) );
  let public_only : ActiveSkgRepoSet = ActiveSkgRepoSet {
    name    : SkgRepoSetName::from ("public"),
    skgrepos : BTreeSet::from ([ src ("public") ]) };
  let gated : HashMap<ID, RelationshipAxes> =
    inverse_scan_for_inbound_folder (
      &recorder, NodeRelation::Overrides, &diffs,
      Some (&public_only) );
  assert! ( gated . is_empty (),
    "a Deleted-stage sign recorded at an inactive repo must not \
     surface: {:?}", gated );
  let ungated : HashMap<ID, RelationshipAxes> =
    inverse_scan_for_inbound_folder (
      &recorder, NodeRelation::Overrides, &diffs, None );
  assert! ( ! ungated . is_empty (),
    "ungated (None) scan should still see the Deleted-stage sign: {:?}",
    ungated );
}
