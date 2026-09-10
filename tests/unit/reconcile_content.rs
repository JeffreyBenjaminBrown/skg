use crate::types::git::{GitDiffStatus, NodeChanges, NodeCompleteDiff, SourceDiff, per_stage_node_changes_for_activeNode};

use super::*;
use std::path::PathBuf;

fn source_name (s: &str) -> SourceName { SourceName ( s . to_string () ) }
fn id          (s: &str) -> ID          { ID ( s . to_string () ) }

fn make_diff_entry (text_changed: bool) -> NodeCompleteDiff {
  NodeCompleteDiff {
    status: GitDiffStatus::Modified,
    node_changes: Some ( NodeChanges {
      text_changed,
      .. NodeChanges::default () } ),
    before_node: None,
    after_node: None, } }

fn sd_with (
  pid     : &ID,
  staged  : Option<bool>,
  unstag  : Option<bool>,
) -> SourceDiff {
  let file : PathBuf = PathBuf::from ( format! ( "{}.skg", pid . 0 ) );
  let mut s : HashMap<PathBuf, NodeCompleteDiff> = HashMap::new ();
  let mut u : HashMap<PathBuf, NodeCompleteDiff> = HashMap::new ();
  if let Some (t) = staged { s . insert (file . clone (), make_diff_entry (t)); }
  if let Some (t) = unstag { u . insert (file,           make_diff_entry (t)); }
  SourceDiff {
    is_git_repo: true,
    staged: s, unstaged: u,
    added_nodes: HashMap::new (),
    deleted_nodes: HashMap::new (), } }

fn diffs_with (src: &SourceName, sd: SourceDiff)
  -> Option<HashMap<SourceName, SourceDiff>> {
  let mut m : HashMap<SourceName, SourceDiff> = HashMap::new ();
  m . insert (src . clone (), sd);
  Some (m) }

fn text_changed_both (
  diffs : &Option<HashMap<SourceName, SourceDiff>>,
  pid   : &ID,
  src   : &SourceName,
) -> (bool, bool) {
  let (s, u) = per_stage_node_changes_for_activeNode (diffs, pid, src);
  ( s . map ( |n| n . text_changed ) . unwrap_or (false),
    u . map ( |n| n . text_changed ) . unwrap_or (false) )
}

#[test]
fn text_change_only_staged () {
  let src = source_name ("public");
  let pid = id ("n");
  let diffs = diffs_with (&src, sd_with (&pid, Some (true), None));
  assert_eq! ( text_changed_both (&diffs, &pid, &src), (true, false) );
}

#[test]
fn text_change_only_unstaged () {
  let src = source_name ("public");
  let pid = id ("n");
  let diffs = diffs_with (&src, sd_with (&pid, None, Some (true)));
  assert_eq! ( text_changed_both (&diffs, &pid, &src), (false, true) );
}

#[test]
fn text_change_both_stages () {
  let src = source_name ("public");
  let pid = id ("n");
  let diffs = diffs_with (&src, sd_with (&pid, Some (true), Some (true)));
  assert_eq! ( text_changed_both (&diffs, &pid, &src), (true, true) );
}

#[test]
fn no_diff_at_all () {
  let src = source_name ("public");
  let pid = id ("n");
  assert_eq! ( text_changed_both (&None, &pid, &src), (false, false) );
}

// §6.5: an Unknown content child whose id is no longer in the parent's
// contains converts to DeadScaffold; one still in contains is retained.
fn parent_with_unknown_child (child : &ID) -> (Tree<ViewNode>, NodeId, NodeId) {
  use crate::types::viewnode::{mk_definitive_viewnode, mk_unknown_viewnode};
  let mut tree : Tree<ViewNode> =
    Tree::new ( mk_definitive_viewnode (
      id ("p"), source_name ("main"), "p" . to_string (), None ) );
  let parent : NodeId = tree . root () . id ();
  let child_nid : NodeId =
    tree . root_mut () . append ( mk_unknown_viewnode (child . clone ()) ) . id ();
  (tree, parent, child_nid) }

fn is_dead (tree : &Tree<ViewNode>, nid : NodeId) -> bool {
  matches! ( tree . get (nid) . unwrap () . value () . kind,
             ViewNodeKind::DeadScaffold ) }

fn is_unknown (tree : &Tree<ViewNode>, nid : NodeId) -> bool {
  matches! ( & tree . get (nid) . unwrap () . value () . kind,
             ViewNodeKind::Phantom (Phantom::Unknown (_)) ) }

#[test]
fn nonmember_unknown_child_becomes_dead () {
  let (mut tree, parent, child) = parent_with_unknown_child (& id ("ghost"));
  convert_nonmember_unknown_children_to_dead (
    &mut tree, parent, &[ id ("kept") ] ) . unwrap ();
  assert! ( is_dead (&tree, child),
            "an Unknown no longer in contains should become DeadScaffold" );
}

#[test]
fn member_unknown_child_is_retained () {
  let (mut tree, parent, child) = parent_with_unknown_child (& id ("ghost"));
  convert_nonmember_unknown_children_to_dead (
    &mut tree, parent, &[ id ("ghost") ] ) . unwrap ();
  assert! ( is_unknown (&tree, child),
            "an Unknown still in contains should be retained" );
}

#[test]
fn same_session_surviving_content_membership_becomes_unknown () {
  use crate::types::viewnode::mk_definitive_viewnode;
  let ghost : ID = id ("ghost");
  let mut tree : Tree<ViewNode> = Tree::new (mk_definitive_viewnode (
    id ("parent"), source_name ("main"), "parent" . to_string (), None ));
  let parent : NodeId = tree . root () . id ();
  let mut active : ViewNode = mk_definitive_viewnode (
    ghost . clone (), source_name ("main"), "last seen" . to_string (),
    Some ("last seen body" . to_string ()) );
  active . focused = true;
  active . folded = true;
  let child : NodeId = tree . root_mut () . append (active) . id ();
  let mut relationship_sources : HashMap<ID, SourceName> = HashMap::new ();
  relationship_sources . insert (ghost . clone (), source_name ("private"));
  let graph_snap : std::sync::Arc<InRustGraph> =
    std::sync::Arc::new (InRustGraph::new ());

  normalize_relationship_backed_content_unknowns (
    &mut tree, parent, &[ghost . clone ()], &relationship_sources,
    &source_name ("main"), &graph_snap, &HashMap::new () ) . unwrap ();

  let rendered = tree . get (child) . unwrap () . value ();
  assert! (rendered . focused && rendered . folded,
    "same-session normalization preserves view wrapper state");
  match &rendered . kind {
    ViewNodeKind::Phantom (Phantom::Unknown (unknown)) => {
      assert_eq! (unknown . id, ghost);
      assert_eq! (unknown . rel_source,
                  Some (source_name ("private")));
      assert_eq! (unknown . rel_source_request, None); },
    other => panic! ("surviving relationship must render Unknown, got {other:?}"), }
}

#[test]
fn same_session_extra_id_membership_becomes_unknown_with_raw_id () {
  use crate::types::viewnode::mk_definitive_viewnode;
  let primary : ID = id ("deleted-primary");
  let raw_extra : ID = id ("surviving-extra-id");
  let mut tree : Tree<ViewNode> = Tree::new (mk_definitive_viewnode (
    id ("parent"), source_name ("main"), "parent" . to_string (), None ));
  let parent : NodeId = tree . root () . id ();
  let mut active : ViewNode = mk_definitive_viewnode (
    primary . clone (), source_name ("main"), "last seen" . to_string (), None );
  active . focused = true;
  let child : NodeId = tree . root_mut () . append (active) . id ();
  let mut relationship_sources : HashMap<ID, SourceName> = HashMap::new ();
  relationship_sources . insert (raw_extra . clone (), source_name ("foreign"));
  let graph_snap : std::sync::Arc<InRustGraph> =
    std::sync::Arc::new (InRustGraph::new ());
  let mut deleted_extra_ids : HashMap<ID, HashSet<ID>> = HashMap::new ();
  deleted_extra_ids . insert (
    primary, [raw_extra . clone ()] . into_iter () . collect ());

  normalize_relationship_backed_content_unknowns (
    &mut tree, parent, &[raw_extra . clone ()], &relationship_sources,
    &source_name ("main"), &graph_snap, &deleted_extra_ids ) . unwrap ();

  let rendered = tree . get (child) . unwrap () . value ();
  assert! (rendered . focused,
    "extra-id normalization preserves the active child wrapper state");
  match &rendered . kind {
    ViewNodeKind::Phantom (Phantom::Unknown (unknown)) => {
      assert_eq! (unknown . id, raw_extra,
        "the retained on-disk spelling, not the deleted primary, is rendered");
      assert_eq! (unknown . rel_source, Some (source_name ("foreign"))); },
    other => panic! ("surviving extra-id relationship must be Unknown, got {other:?}"), }
}

// review-2 §2.1 regression: a content goal id present only as a
// parentIs=Independent child must still get ChildData pre-fetched.
// complete_content_children counts only parentIs=Affected Normal children as
// "already present", so an Independent same-id child is sent to the create
// closure; if build_child_creation_data skipped pre-fetching it (because it
// collected the skip-set from ALL Normal children, Independent included), the
// closure's child_data.get(id).expect(..) panics. The skip-set must match the
// present-set: an Independent same-id child must NOT be skipped.
#[test]
fn independent_same_id_child_is_prefetched () {
  use crate::types::viewnode::{ mk_definitive_viewnode, ParentIs };
  let goal : ID = id ("regression_independent_child");
  let mut tree : Tree<ViewNode> =
    Tree::new ( mk_definitive_viewnode (
      id ("p"), source_name ("main"), "p" . to_string (), None ));
  let parent : NodeId = tree . root () . id ();
  let mut child : ViewNode =
    mk_definitive_viewnode (
      goal . clone (), source_name ("main"), "c" . to_string (), None );
  if let ViewNodeKind::Vognode (Vognode::Active (t)) = &mut child . kind
    { t . parentIs = ParentIs::Independent; }
  tree . root_mut () . append (child);
  let config : SkgConfig =
    SkgConfig::dummyFromSources ( HashMap::new () );
  let no_deletes : HashMap<ID, SourceName> = HashMap::new ();
  let no_relationship_sources : HashMap<ID, SourceName> = HashMap::new ();
  let graph_snap : std::sync::Arc<InRustGraph> =
    std::sync::Arc::new ( InRustGraph::new () );
  let data : HashMap<ID, ChildData> =
    build_child_creation_data (
      &tree, parent, &[ goal . clone () ], &no_relationship_sources,
      &config, &graph_snap,
      &no_deletes, None, false )
      . unwrap ();
  assert! ( data . contains_key (&goal),
            "an Independent same-id child's goal id must be pre-fetched, not \
             skipped -- else complete_content_children panics on it" );
}
