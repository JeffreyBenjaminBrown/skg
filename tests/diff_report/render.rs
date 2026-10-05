use skg::diff_report::render::render_report;
use skg::diff_report::types::{
  DiffReport, ListDiffItem, NodeBucket, NodeDiffReport, RelationshipDiff,
  RepoForReport};
use skg::types::misc::{ID, SkgRepoName};

use std::collections::HashMap;

fn skgid (
  s : &str,
) -> ID {
  ID::from (s)
}

fn node_report (
  pid   : &str,
  title : &str,
) -> NodeDiffReport {
  NodeDiffReport {
    pid: skgid (pid),
    home_skgrepo: RepoForReport::After (SkgRepoName::from ("main")),
    title: title . to_string (),
    title_diff: None,
    body_diff: None,
    skgrepo_change: None,
    value_set_diffs: Vec::new (),
    relationship_diffs: Vec::new (),
    content_list_diff: None }
}

#[test]
fn abbreviations_use_enough_skgid_prefix_to_disambiguate () {
  let report : DiffReport =
    DiffReport {
      vanished: Vec::new (),
      duplicate_ids: Vec::new (),
      titles: HashMap::new (),
      buckets: vec! [
        NodeBucket {
          name: "modified, other",
          nodes: vec! [
            node_report ("abcdef-1", "same title"),
            node_report ("abcdef-2", "same title") ] } ] };
  let rendered : String =
    render_report (&report);
  assert! (
    rendered . contains ("abcdef-1..same title"),
    "first abbreviation should include enough ID: {}",
    rendered );
  assert! (
    rendered . contains ("abcdef-2..same title"),
    "second abbreviation should include enough ID: {}",
      rendered );
}

#[test]
fn unchanged_contained_diff_lines_align_with_changed_lines () {
  let mut node : NodeDiffReport =
    node_report ("parent", "Parent");
  node . content_list_diff =
    Some (vec! [
      ListDiffItem::Unchanged (skgid ("keep")),
      ListDiffItem::Removed (skgid ("gone")),
      ListDiffItem::Added (skgid ("new")) ]);
  let report : DiffReport =
    DiffReport {
      vanished: Vec::new (),
      duplicate_ids: Vec::new (),
      titles: HashMap::from ([
        (skgid ("keep"), "Keep".to_string ()),
        (skgid ("gone"), "Gone".to_string ()),
        (skgid ("new"), "New".to_string ()) ]),
      buckets: vec! [
        NodeBucket {
          name: "modified, other",
          nodes: vec! [node] } ] };
  let rendered : String =
    render_report (&report);
  assert! (
    rendered . contains (
      "\n**** contained diff\n  k..Keep\n -g..Gone\n +n..New\n" ),
    "contained diff viewnodes should align: {}",
      rendered );
}

#[test]
fn container_relationship_renders_existing_containers_not_gained () {
  let mut node : NodeDiffReport =
    node_report ("child", "Child");
  node . relationship_diffs =
    vec! [
      RelationshipDiff {
        role: "container",
        lost: vec! [skgid ("old")],
        gained: vec! [skgid ("new")],
        unchanged: vec! [skgid ("stay")] } ];
  let report : DiffReport =
    DiffReport {
      vanished: Vec::new (),
      duplicate_ids: Vec::new (),
      titles: HashMap::from ([
        (skgid ("old"), "Old".to_string ()),
        (skgid ("new"), "New".to_string ()),
        (skgid ("stay"), "Stay".to_string ()) ]),
      buckets: vec! [
        NodeBucket {
          name: "modified, other",
          nodes: vec! [node] } ] };
  let rendered : String =
    render_report (&report);
  assert! (
    rendered . contains ("**** containers (with gains and losses)\n"),
    "container relationship should describe gains and losses: {}",
    rendered );
  assert! (
    ! rendered . contains ("***** gained\n"),
    "container relationship should not use gained heading: {}",
    rendered );
  assert! (
    rendered . contains ("****** -o..Old\n"),
    "lost container should be marked with '-': {}",
    rendered );
  assert! (
    rendered . contains ("****** +n..New\n"),
    "gained container should be marked with '+': {}",
    rendered );
  assert! (
    rendered . contains ("******  s..Stay\n"),
    "unchanged container should be marked with a space: {}",
      rendered );
}

#[test]
fn unchanged_container_relationship_heading_is_explicit () {
  let mut node : NodeDiffReport =
    node_report ("child", "Child");
  node . relationship_diffs =
    vec! [
      RelationshipDiff {
        role: "container",
        lost: Vec::new (),
        gained: Vec::new (),
        unchanged: vec! [skgid ("stay")] } ];
  let report : DiffReport =
    DiffReport {
      vanished: Vec::new (),
      duplicate_ids: Vec::new (),
      titles: HashMap::from ([
        (skgid ("stay"), "Stay".to_string ()) ]),
      buckets: vec! [
        NodeBucket {
          name: "modified, other",
          nodes: vec! [node] } ] };
  let rendered : String =
    render_report (&report);
  assert! (
    rendered . contains ("**** containers (unchanged)\n"),
    "unchanged-only container heading should be explicit: {}",
    rendered );
}

#[test]
fn non_container_backward_relationships_render_as_signed_sets () {
  let mut node : NodeDiffReport =
    node_report ("target", "Target");
  node . relationship_diffs =
    vec! [
      RelationshipDiff {
        role: "mentioned",
        lost: vec! [skgid ("old")],
        gained: vec! [skgid ("new")],
        unchanged: vec! [skgid ("stay")] } ];
  let report : DiffReport =
    DiffReport {
      vanished: Vec::new (),
      duplicate_ids: Vec::new (),
      titles: HashMap::from ([
        (skgid ("old"), "Old".to_string ()),
        (skgid ("new"), "New".to_string ()),
        (skgid ("stay"), "Stay".to_string ()) ]),
      buckets: vec! [
        NodeBucket {
          name: "modified, other",
          nodes: vec! [node] } ] };
  let rendered : String =
    render_report (&report);
  assert! (
    rendered . contains ("**** mentioned (with gains and losses)\n"),
    "backward relationship should use signed-set heading: {}",
    rendered );
  assert! (
    rendered . contains ("****** -o..Old\n"),
    "lost backward relation should be marked with '-': {}",
    rendered );
  assert! (
    rendered . contains ("****** +n..New\n"),
    "gained backward relation should be marked with '+': {}",
    rendered );
  assert! (
    rendered . contains ("******  s..Stay\n"),
    "unchanged backward relation should be marked with a space: {}",
    rendered );
}
