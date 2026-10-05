use skg::diff_report::diff::diff_git_snapshots;
use skg::diff_report::types::{
  DiffReport, GraphSnapshot, NodeBucket, NodeDiffReport, RelationshipDiff,
  GitSnapshotPair, ValueSetDiff};
use skg::types::misc::{ID, MSV, SkgRepoName, rel_partners_at_relRepo};
use skg::types::nodes::complete::{Graphnode, empty_graphnode};

use std::collections::{BTreeSet, HashMap};

fn skgid (
  s : &str,
) -> ID {
  ID::new (s)
}

fn skgrepo (
  s : &str,
) -> SkgRepoName {
  SkgRepoName::from (s)
}

fn node (
  pid      : &str,
  title    : &str,
  contains : &[&str],
) -> Graphnode {
  let mut node : Graphnode =
    empty_graphnode ();
  node . pid = skgid (pid);
  node . title = title . to_string ();
  node . home_skgrepo = skgrepo ("main");
  node . contains =
    rel_partners_at_relRepo (
      &node . home_skgrepo,
      contains . iter () . map ( |x| skgid (x) ) . collect () );
  node
}

fn git_snapshot (
  nodes : Vec<Graphnode>,
) -> GraphSnapshot {
  let mut git_snapshot : GraphSnapshot =
    GraphSnapshot::default ();
  for node in nodes {
    for skgid in node . all_skgids () {
      git_snapshot . id_claims . entry (skgid . clone ())
        . or_insert_with (std::collections::BTreeMap::new)
        . entry (node . pid . clone ())
        . or_insert_with (BTreeSet::new)
        . insert (node . home_skgrepo . clone ()); }
    git_snapshot . nodes . insert (node . pid . clone (), node); }
  git_snapshot
}

fn report_for (
  before : Vec<Graphnode>,
  after  : Vec<Graphnode>,
) -> DiffReport {
  diff_git_snapshots (&GitSnapshotPair {
    before: git_snapshot (before),
    after: git_snapshot (after) })
}

fn reports_by_pid (
  report : &DiffReport,
) -> HashMap<ID, &NodeDiffReport> {
  report . buckets . iter ()
    . flat_map ( |bucket| bucket . nodes . iter () )
    . map ( |node_report| (node_report . pid . clone (), node_report) )
    . collect ()
}

fn bucket_names (
  report : &DiffReport,
) -> Vec<&'static str> {
  report . buckets . iter ()
    . map ( |bucket| bucket . name )
    . collect ()
}

#[test]
fn gained_container_affects_child () {
  let before : Vec<Graphnode> =
    vec! [ node ("a", "A", &[]),
           node ("b", "B", &[]) ];
  let after : Vec<Graphnode> =
    vec! [ node ("a", "A", &["b"]),
           node ("b", "B", &[]) ];
  let report : DiffReport =
    report_for (before, after);
  let reports : HashMap<ID, &NodeDiffReport> =
    reports_by_pid (&report);
  let b_report : &NodeDiffReport =
    reports . get (&skgid ("b")) . unwrap ();
  let container_diff : &RelationshipDiff =
    b_report . relationship_diffs . iter ()
      . find ( |diff| diff . role == "container" )
      . unwrap ();
  assert_eq! (container_diff . gained, vec! [skgid ("a")]);
  assert! (container_diff . lost . is_empty ());
  assert! (container_diff . unchanged . is_empty ());
}

#[test]
fn lost_container_reports_current_existing_containers () {
  let before : Vec<Graphnode> =
    vec! [ node ("old", "Old", &["child"]),
           node ("stay", "Stay", &["child"]),
           node ("child", "Child", &[]) ];
  let after : Vec<Graphnode> =
    vec! [ node ("old", "Old", &[]),
           node ("stay", "Stay", &["child"]),
           node ("new", "New", &["child"]),
           node ("child", "Child", &[]) ];
  let report : DiffReport =
    report_for (before, after);
  let reports : HashMap<ID, &NodeDiffReport> =
    reports_by_pid (&report);
  let child_report : &NodeDiffReport =
    reports . get (&skgid ("child")) . unwrap ();
  let container_diff : &RelationshipDiff =
    child_report . relationship_diffs . iter ()
      . find ( |diff| diff . role == "container" )
      . unwrap ();
  assert_eq! (container_diff . lost, vec! [skgid ("old")]);
  assert_eq! (container_diff . gained, vec! [skgid ("new")]);
  assert_eq! (container_diff . unchanged, vec! [skgid ("stay")]);
}

#[test]
fn backward_subscribee_reports_current_existing_related_nodes () {
  let mut old : Graphnode =
    node ("old", "Old", &[]);
  old . subscribes_to = MSV::Specified (rel_partners_at_relRepo (&old . home_skgrepo, vec! [skgid ("target")]));
  let mut stay : Graphnode =
    node ("stay", "Stay", &[]);
  stay . subscribes_to = MSV::Specified (rel_partners_at_relRepo (&stay . home_skgrepo, vec! [skgid ("target")]));
  let mut stay_after : Graphnode =
    node ("stay", "Stay", &[]);
  stay_after . subscribes_to = MSV::Specified (rel_partners_at_relRepo (&stay_after . home_skgrepo, vec! [skgid ("target")]));
  let mut new : Graphnode =
    node ("new", "New", &[]);
  new . subscribes_to = MSV::Specified (rel_partners_at_relRepo (&new . home_skgrepo, vec! [skgid ("target")]));
  let report : DiffReport =
    report_for (
      vec! [ old, stay, node ("target", "Target", &[]) ],
      vec! [ stay_after, new, node ("target", "Target", &[]) ] );
  let reports : HashMap<ID, &NodeDiffReport> =
    reports_by_pid (&report);
  let target_report : &NodeDiffReport =
    reports . get (&skgid ("target")) . unwrap ();
  let subscribee_diff : &RelationshipDiff =
    target_report . relationship_diffs . iter ()
      . find ( |diff| diff . role == "subscribee" )
      . unwrap ();
  assert_eq! (subscribee_diff . lost, vec! [skgid ("old")]);
  assert_eq! (subscribee_diff . gained, vec! [skgid ("new")]);
  assert_eq! (subscribee_diff . unchanged, vec! [skgid ("stay")]);
}

#[test]
fn contained_order_change_gets_list_diff () {
  let before : Vec<Graphnode> =
    vec! [ node ("a", "A", &["b", "c"]),
           node ("b", "B", &[]),
           node ("c", "C", &[]) ];
  let after : Vec<Graphnode> =
    vec! [ node ("a", "A", &["c", "b"]),
           node ("b", "B", &[]),
           node ("c", "C", &[]) ];
  let report : DiffReport =
    report_for (before, after);
  let reports : HashMap<ID, &NodeDiffReport> =
    reports_by_pid (&report);
  assert! (
    reports . get (&skgid ("a")) . unwrap ()
      . contained_list_diff . is_some () );
}

#[test]
fn links_are_reported_in_both_directions () {
  let before : Vec<Graphnode> =
    vec! [ node ("a", "A", &[]),
           node ("b", "B", &[]) ];
  let mut a_after : Graphnode =
    node ("a", "A [[id:b][B]]", &[]);
  a_after . body = Some ("body".to_string ());
  let after : Vec<Graphnode> =
    vec! [ a_after, node ("b", "B", &[]) ];
  let report : DiffReport =
    report_for (before, after);
  let reports : HashMap<ID, &NodeDiffReport> =
    reports_by_pid (&report);
  let a_skgrepo : &RelationshipDiff =
    reports . get (&skgid ("a")) . unwrap ()
      . relationship_diffs . iter ()
      . find ( |diff| diff . role == "mentioner" )
      . unwrap ();
  let b_dest : &RelationshipDiff =
    reports . get (&skgid ("b")) . unwrap ()
      . relationship_diffs . iter ()
      . find ( |diff| diff . role == "mentioned" )
      . unwrap ();
  assert_eq! (a_skgrepo . gained, vec! [skgid ("b")]);
  assert_eq! (b_dest . gained, vec! [skgid ("a")]);
}

#[test]
fn duplicate_skgids_across_skgrepos_are_omitted_from_node_buckets () {
  let before : Vec<Graphnode> =
    vec! [];
  let mut left : Graphnode =
    node ("a", "A left", &[]);
  left . home_skgrepo = skgrepo ("left");
  let mut right : Graphnode =
    node ("b", "B right", &[]);
  right . home_skgrepo = skgrepo ("right");
  right . extra_ids = vec! [skgid ("a")];
  let report : DiffReport =
    report_for (before, vec! [left, right]);
  assert_eq! (report . duplicate_ids . len (), 1);
  assert! (
    report . buckets . iter ()
      . all ( |bucket| bucket . nodes . is_empty () ));
}

#[test]
fn telescope_shape_is_not_a_duplicate () {
  // One pid claimed from two REPOS is the normal telescope shape
  // (sections at two privacy levels), not a duplicate-ID violation.
  let mut telescope : Graphnode =
    node ("a", "A", &[]);
  telescope . home_skgrepo = skgrepo ("public");
  let mut git_snapshot_after : GraphSnapshot =
    git_snapshot (vec! [telescope]);
  git_snapshot_after . id_claims . get_mut (&skgid ("a")) . unwrap ()
    . get_mut (&skgid ("a")) . unwrap ()
    . insert (skgrepo ("private"));
  let report : DiffReport =
    diff_git_snapshots (&GitSnapshotPair {
      before: git_snapshot (vec! []),
      after: git_snapshot_after });
  assert! (report . duplicate_ids . is_empty (),
           "multi-repo single-pid claims flagged as duplicates");
  assert! (
    report . buckets . iter ()
      . any ( |bucket| ! bucket . nodes . is_empty () ),
    "the telescope should appear in the node buckets" );
}

#[test]
fn bucket_order_frontloads_problematic_categories () {
  let report : DiffReport =
    report_for (Vec::new (), Vec::new ());
  assert_eq! (
    bucket_names (&report),
    vec! [
      "modified, newly orphaned",
      "new roots",
      "deleted roots",
      "deleted nodes, not roots",
      "deleted nodes, probably via merger",
      "modified, moved across repos",
      "modified, other",
      "new nodes, not roots" ] );
}

#[test]
fn deleted_pid_preserved_as_extra_id_is_probably_merged () {
  let before : Vec<Graphnode> =
    vec! [ node ("old", "Old", &[]) ];
  let mut merged : Graphnode =
    node ("merged", "Merged", &[]);
  merged . extra_ids = vec! [skgid ("old")];
  let report : DiffReport =
    report_for (before, vec! [merged]);
  let merge_bucket : &NodeBucket =
    report . buckets . iter ()
      . find ( |bucket| bucket . name ==
        "deleted nodes, probably via merger" )
      . unwrap ();
  assert! (
    merge_bucket . nodes . iter ()
      . any ( |node_report| node_report . pid == skgid ("old") ));
  let deleted_roots : &NodeBucket =
    report . buckets . iter ()
      . find ( |bucket| bucket . name == "deleted roots" )
      . unwrap ();
  assert! (
    deleted_roots . nodes . iter ()
      . all ( |node_report| node_report . pid != skgid ("old") ));
}

#[test]
fn aliases_use_set_diff () {
  let mut before_node : Graphnode =
    node ("a", "A", &[]);
  before_node . aliases =
    MSV::Specified (rel_partners_at_relRepo (&before_node . home_skgrepo, vec! ["old".to_string ()]));
  let mut after_node : Graphnode =
    node ("a", "A", &[]);
  after_node . aliases =
    MSV::Specified (rel_partners_at_relRepo (&after_node . home_skgrepo, vec! ["new".to_string ()]));
  let report : DiffReport =
    report_for (vec! [before_node], vec! [after_node]);
  let reports : HashMap<ID, &NodeDiffReport> =
    reports_by_pid (&report);
  let alias_diff : &ValueSetDiff =
    reports . get (&skgid ("a")) . unwrap ()
      . value_set_diffs . iter ()
      . find ( |diff| diff . name == "aliases" )
      . unwrap ();
  assert_eq! (alias_diff . lost, vec! ["old".to_string ()]);
  assert_eq! (alias_diff . gained, vec! ["new".to_string ()]);
}

#[test]
fn repo_move_is_reported () {
  let mut before_node : Graphnode =
    node ("a", "A", &[]);
  before_node . home_skgrepo = skgrepo ("left");
  let mut after_node : Graphnode =
    node ("a", "A", &[]);
  after_node . home_skgrepo = skgrepo ("right");
  let report : DiffReport =
    report_for (vec! [before_node], vec! [after_node]);
  let reports : HashMap<ID, &NodeDiffReport> =
    reports_by_pid (&report);
  assert_eq! (
    reports . get (&skgid ("a")) . unwrap () . skgrepo_change,
    Some ((skgrepo ("left"), skgrepo ("right"))) );
}

#[test]
fn skgrepo_move_uses_its_own_bucket () {
  let mut before_node : Graphnode =
    node ("a", "A", &[]);
  before_node . home_skgrepo = skgrepo ("left");
  let mut after_node : Graphnode =
    node ("a", "A", &[]);
  after_node . home_skgrepo = skgrepo ("right");
  let report : DiffReport =
    report_for (vec! [before_node], vec! [after_node]);
  let move_bucket : &NodeBucket =
    report . buckets . iter ()
      . find ( |bucket| bucket . name ==
        "modified, moved across repos" )
      . unwrap ();
  assert! (
    move_bucket . nodes . iter ()
      . any ( |node_report| node_report . pid == skgid ("a") ));
  let modified_other : &NodeBucket =
    report . buckets . iter ()
      . find ( |bucket| bucket . name == "modified, other" )
      . unwrap ();
  assert! (
    modified_other . nodes . iter ()
      . all ( |node_report| node_report . pid != skgid ("a") ));
}

#[test]
fn root_classification_detects_newly_orphaned_nodes () {
  let before : Vec<Graphnode> =
    vec! [ node ("parent", "Parent", &["child"]),
           node ("child", "Child", &[]) ];
  let after : Vec<Graphnode> =
    vec! [ node ("parent", "Parent", &[]),
           node ("child", "Child", &[]) ];
  let report : DiffReport =
    report_for (before, after);
  let newly_orphaned : &NodeBucket =
    report . buckets . iter ()
      . find ( |bucket| bucket . name == "modified, newly orphaned" )
      . unwrap ();
  assert! (
    newly_orphaned . nodes . iter ()
      . any ( |node_report| node_report . pid == skgid ("child") ));
}

#[test]
fn pure_contained_reorder_has_list_diff_without_set_diff () {
  let before : Vec<Graphnode> =
    vec! [ node ("a", "A", &["b", "c"]),
           node ("b", "B", &[]),
           node ("c", "C", &[]) ];
  let after : Vec<Graphnode> =
    vec! [ node ("a", "A", &["c", "b"]),
           node ("b", "B", &[]),
           node ("c", "C", &[]) ];
  let report : DiffReport =
    report_for (before, after);
  let reports : HashMap<ID, &NodeDiffReport> =
    reports_by_pid (&report);
  let a_report : &NodeDiffReport =
    reports . get (&skgid ("a")) . unwrap ();
  let contained_set_diff : Option<&RelationshipDiff> =
    a_report . relationship_diffs . iter ()
      . find ( |diff| diff . role == "contained" );
  assert! (contained_set_diff . is_none ());
  assert! (a_report . contained_list_diff . is_some ());
}
