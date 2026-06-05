use crate::types::git::{GitDiffStatus, NodeChanges, NodeCompleteDiff, SourceDiff, per_stage_node_changes_for_truenode};

use super::*;
use std::path::PathBuf;

fn source_name (s: &str) -> SourceName { SourceName ( s . to_string () ) }
fn id          (s: &str) -> ID          { ID ( s . to_string () ) }

fn make_diff_entry (text_changed: bool) -> NodeCompleteDiff {
  NodeCompleteDiff {
    status: GitDiffStatus::Modified,
    node_changes: Some ( NodeChanges {
      text_changed,
      aliases_diff:       Vec::new (),
      ids_diff:           Vec::new (),
      contains_diff:      Vec::new (),
      subscribes_to_diff: Vec::new (),
      hides_diff:         Vec::new (), } ),
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
  let (s, u) = per_stage_node_changes_for_truenode (diffs, pid, src);
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
             ViewNodeKind::Vognode (Vognode::Unknown (_)) ) }

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
