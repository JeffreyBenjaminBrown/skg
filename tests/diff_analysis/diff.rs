use skg::diff_analysis::diff::diff_snapshots;
use skg::diff_analysis::types::{
  DiffReport, GraphSnapshot, NodeBucket, NodeDiffReport, RelationshipDiff,
  SnapshotPair, ValueSetDiff};
use skg::types::misc::{ID, MSV, SourceName};
use skg::types::nodes::complete::{NodeComplete, empty_node_complete};

use std::collections::{BTreeSet, HashMap};

fn id (
  s : &str,
) -> ID {
  ID::new (s)
}

fn source (
  s : &str,
) -> SourceName {
  SourceName::from (s)
}

fn node (
  pid      : &str,
  title    : &str,
  contains : &[&str],
) -> NodeComplete {
  let mut node : NodeComplete =
    empty_node_complete ();
  node . pid = id (pid);
  node . title = title . to_string ();
  node . source = source ("main");
  node . contains =
    contains . iter () . map ( |x| id (x) ) . collect ();
  node
}

fn snapshot (
  nodes : Vec<NodeComplete>,
) -> GraphSnapshot {
  let mut snapshot : GraphSnapshot =
    GraphSnapshot::default ();
  for node in nodes {
    for id in node . all_ids () {
      snapshot . id_sources . entry (id . clone ())
        . or_insert_with (BTreeSet::new)
        . insert (node . source . clone ()); }
    snapshot . nodes . insert (node . pid . clone (), node); }
  snapshot
}

fn report_for (
  before : Vec<NodeComplete>,
  after  : Vec<NodeComplete>,
) -> DiffReport {
  diff_snapshots (&SnapshotPair {
    before: snapshot (before),
    after: snapshot (after) })
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
  let before : Vec<NodeComplete> =
    vec! [ node ("a", "A", &[]),
           node ("b", "B", &[]) ];
  let after : Vec<NodeComplete> =
    vec! [ node ("a", "A", &["b"]),
           node ("b", "B", &[]) ];
  let report : DiffReport =
    report_for (before, after);
  let reports : HashMap<ID, &NodeDiffReport> =
    reports_by_pid (&report);
  let b_report : &NodeDiffReport =
    reports . get (&id ("b")) . unwrap ();
  let container_diff : &RelationshipDiff =
    b_report . relationship_diffs . iter ()
      . find ( |diff| diff . role == "container" )
      . unwrap ();
  assert_eq! (container_diff . gained, vec! [id ("a")]);
  assert! (container_diff . lost . is_empty ());
  assert! (container_diff . unchanged . is_empty ());
}

#[test]
fn lost_container_reports_current_existing_containers () {
  let before : Vec<NodeComplete> =
    vec! [ node ("old", "Old", &["child"]),
           node ("stay", "Stay", &["child"]),
           node ("child", "Child", &[]) ];
  let after : Vec<NodeComplete> =
    vec! [ node ("old", "Old", &[]),
           node ("stay", "Stay", &["child"]),
           node ("new", "New", &["child"]),
           node ("child", "Child", &[]) ];
  let report : DiffReport =
    report_for (before, after);
  let reports : HashMap<ID, &NodeDiffReport> =
    reports_by_pid (&report);
  let child_report : &NodeDiffReport =
    reports . get (&id ("child")) . unwrap ();
  let container_diff : &RelationshipDiff =
    child_report . relationship_diffs . iter ()
      . find ( |diff| diff . role == "container" )
      . unwrap ();
  assert_eq! (container_diff . lost, vec! [id ("old")]);
  assert_eq! (container_diff . gained, vec! [id ("new")]);
  assert_eq! (container_diff . unchanged, vec! [id ("stay")]);
}

#[test]
fn contained_order_change_gets_list_diff () {
  let before : Vec<NodeComplete> =
    vec! [ node ("a", "A", &["b", "c"]),
           node ("b", "B", &[]),
           node ("c", "C", &[]) ];
  let after : Vec<NodeComplete> =
    vec! [ node ("a", "A", &["c", "b"]),
           node ("b", "B", &[]),
           node ("c", "C", &[]) ];
  let report : DiffReport =
    report_for (before, after);
  let reports : HashMap<ID, &NodeDiffReport> =
    reports_by_pid (&report);
  assert! (
    reports . get (&id ("a")) . unwrap ()
      . contained_list_diff . is_some () );
}

#[test]
fn textlinks_are_reported_in_both_directions () {
  let before : Vec<NodeComplete> =
    vec! [ node ("a", "A", &[]),
           node ("b", "B", &[]) ];
  let mut a_after : NodeComplete =
    node ("a", "A [[id:b][B]]", &[]);
  a_after . body = Some ("body".to_string ());
  let after : Vec<NodeComplete> =
    vec! [ a_after, node ("b", "B", &[]) ];
  let report : DiffReport =
    report_for (before, after);
  let reports : HashMap<ID, &NodeDiffReport> =
    reports_by_pid (&report);
  let a_source : &RelationshipDiff =
    reports . get (&id ("a")) . unwrap ()
      . relationship_diffs . iter ()
      . find ( |diff| diff . role == "source" )
      . unwrap ();
  let b_dest : &RelationshipDiff =
    reports . get (&id ("b")) . unwrap ()
      . relationship_diffs . iter ()
      . find ( |diff| diff . role == "dest" )
      . unwrap ();
  assert_eq! (a_source . gained, vec! [id ("b")]);
  assert_eq! (b_dest . gained, vec! [id ("a")]);
}

#[test]
fn duplicate_ids_across_sources_are_omitted_from_node_buckets () {
  let before : Vec<NodeComplete> =
    vec! [];
  let mut left : NodeComplete =
    node ("a", "A left", &[]);
  left . source = source ("left");
  let mut right : NodeComplete =
    node ("b", "B right", &[]);
  right . source = source ("right");
  right . extra_ids = vec! [id ("a")];
  let report : DiffReport =
    report_for (before, vec! [left, right]);
  assert_eq! (report . duplicate_ids . len (), 1);
  assert! (
    report . buckets . iter ()
      . all ( |bucket| bucket . nodes . is_empty () ));
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
      "modified, moved across sources",
      "modified, other",
      "new nodes, not roots" ] );
}

#[test]
fn deleted_pid_preserved_as_extra_id_is_probably_merged () {
  let before : Vec<NodeComplete> =
    vec! [ node ("old", "Old", &[]) ];
  let mut merged : NodeComplete =
    node ("merged", "Merged", &[]);
  merged . extra_ids = vec! [id ("old")];
  let report : DiffReport =
    report_for (before, vec! [merged]);
  let merge_bucket : &NodeBucket =
    report . buckets . iter ()
      . find ( |bucket| bucket . name ==
        "deleted nodes, probably via merger" )
      . unwrap ();
  assert! (
    merge_bucket . nodes . iter ()
      . any ( |node_report| node_report . pid == id ("old") ));
  let deleted_roots : &NodeBucket =
    report . buckets . iter ()
      . find ( |bucket| bucket . name == "deleted roots" )
      . unwrap ();
  assert! (
    deleted_roots . nodes . iter ()
      . all ( |node_report| node_report . pid != id ("old") ));
}

#[test]
fn aliases_use_set_diff () {
  let mut before_node : NodeComplete =
    node ("a", "A", &[]);
  before_node . aliases =
    MSV::Specified (vec! ["old".to_string ()]);
  let mut after_node : NodeComplete =
    node ("a", "A", &[]);
  after_node . aliases =
    MSV::Specified (vec! ["new".to_string ()]);
  let report : DiffReport =
    report_for (vec! [before_node], vec! [after_node]);
  let reports : HashMap<ID, &NodeDiffReport> =
    reports_by_pid (&report);
  let alias_diff : &ValueSetDiff =
    reports . get (&id ("a")) . unwrap ()
      . value_set_diffs . iter ()
      . find ( |diff| diff . name == "aliases" )
      . unwrap ();
  assert_eq! (alias_diff . lost, vec! ["old".to_string ()]);
  assert_eq! (alias_diff . gained, vec! ["new".to_string ()]);
}

#[test]
fn source_move_is_reported () {
  let mut before_node : NodeComplete =
    node ("a", "A", &[]);
  before_node . source = source ("left");
  let mut after_node : NodeComplete =
    node ("a", "A", &[]);
  after_node . source = source ("right");
  let report : DiffReport =
    report_for (vec! [before_node], vec! [after_node]);
  let reports : HashMap<ID, &NodeDiffReport> =
    reports_by_pid (&report);
  assert_eq! (
    reports . get (&id ("a")) . unwrap () . source_change,
    Some ((source ("left"), source ("right"))) );
}

#[test]
fn source_move_uses_its_own_bucket () {
  let mut before_node : NodeComplete =
    node ("a", "A", &[]);
  before_node . source = source ("left");
  let mut after_node : NodeComplete =
    node ("a", "A", &[]);
  after_node . source = source ("right");
  let report : DiffReport =
    report_for (vec! [before_node], vec! [after_node]);
  let move_bucket : &NodeBucket =
    report . buckets . iter ()
      . find ( |bucket| bucket . name ==
        "modified, moved across sources" )
      . unwrap ();
  assert! (
    move_bucket . nodes . iter ()
      . any ( |node_report| node_report . pid == id ("a") ));
  let modified_other : &NodeBucket =
    report . buckets . iter ()
      . find ( |bucket| bucket . name == "modified, other" )
      . unwrap ();
  assert! (
    modified_other . nodes . iter ()
      . all ( |node_report| node_report . pid != id ("a") ));
}

#[test]
fn root_classification_detects_newly_orphaned_nodes () {
  let before : Vec<NodeComplete> =
    vec! [ node ("parent", "Parent", &["child"]),
           node ("child", "Child", &[]) ];
  let after : Vec<NodeComplete> =
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
      . any ( |node_report| node_report . pid == id ("child") ));
}

#[test]
fn pure_contained_reorder_has_list_diff_without_set_diff () {
  let before : Vec<NodeComplete> =
    vec! [ node ("a", "A", &["b", "c"]),
           node ("b", "B", &[]),
           node ("c", "C", &[]) ];
  let after : Vec<NodeComplete> =
    vec! [ node ("a", "A", &["c", "b"]),
           node ("b", "B", &[]),
           node ("c", "C", &[]) ];
  let report : DiffReport =
    report_for (before, after);
  let reports : HashMap<ID, &NodeDiffReport> =
    reports_by_pid (&report);
  let a_report : &NodeDiffReport =
    reports . get (&id ("a")) . unwrap ();
  let contained_set_diff : Option<&RelationshipDiff> =
    a_report . relationship_diffs . iter ()
      . find ( |diff| diff . role == "contained" );
  assert! (contained_set_diff . is_none ());
  assert! (a_report . contained_list_diff . is_some ());
}
