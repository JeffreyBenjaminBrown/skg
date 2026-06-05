use super::*;
use super::super::git::{GitDiffStatus, NodeChanges};

fn source_name (s: &str) -> SourceName { SourceName ( s . to_string () ) }
fn id        (s: &str) -> ID         { ID ( s . to_string () ) }

fn make_parent_diff (
  contains_diff : Vec<Diff_Item<ID>>,
) -> NodeCompleteDiff {
  NodeCompleteDiff {
    status: GitDiffStatus::Modified,
    node_changes: Some ( NodeChanges {
      text_changed:       false,
      aliases_diff:       Vec::new (),
      ids_diff:           Vec::new (),
      contains_diff,
      subscribes_to_diff: Vec::new (),
      hides_diff:         Vec::new (), } ),
    before_node: None,
    after_node: None, } }

fn source_diff_with_parent_contains (
  parent_pid : &ID,
  staged_ops   : Vec<Diff_Item<ID>>,
  unstaged_ops : Vec<Diff_Item<ID>>,
) -> SourceDiff {
  let parent_file : PathBuf =
    PathBuf::from ( format! ( "{}.skg", parent_pid . 0 ) );
  let mut staged   : HashMap<PathBuf, NodeCompleteDiff> = HashMap::new ();
  let mut unstaged : HashMap<PathBuf, NodeCompleteDiff> = HashMap::new ();
  if !staged_ops . is_empty () {
    staged . insert ( parent_file . clone (),
                      make_parent_diff (staged_ops) ); }
  if !unstaged_ops . is_empty () {
    unstaged . insert ( parent_file,
                        make_parent_diff (unstaged_ops) ); }
  SourceDiff {
    is_git_repo: true,
    staged, unstaged,
    added_nodes: HashMap::new (),
    deleted_nodes: HashMap::new (), } }

#[test]
fn staged_removal_is_attributed_to_staged_side () {
  let parent : ID = id ("parent");
  let child  : ID = id ("child");
  let src    : SourceName = source_name ("public");
  let mut diffs : HashMap<SourceName, SourceDiff> = HashMap::new ();
  diffs . insert ( src . clone (),
                   source_diff_with_parent_contains (
                     &parent,
                     vec! [ Diff_Item::Removed (child . clone ()) ],
                     vec! [] ) );
  let (ex, mem) = phantom_axes (
    &child, &src, &parent, &src, Some (&diffs) );
  assert_eq! ( mem, MembershipAxes {
    staged: Some (Sign::Minus), unstaged: None } );
  assert_eq! ( ex, ExistenceAxes::default () );
}

#[test]
fn unstaged_removal_is_attributed_to_unstaged_side () {
  let parent : ID = id ("parent");
  let child  : ID = id ("child");
  let src    : SourceName = source_name ("public");
  let mut diffs : HashMap<SourceName, SourceDiff> = HashMap::new ();
  diffs . insert ( src . clone (),
                   source_diff_with_parent_contains (
                     &parent,
                     vec! [],
                     vec! [ Diff_Item::Removed (child . clone ()) ] ) );
  let (_, mem) = phantom_axes (
    &child, &src, &parent, &src, Some (&diffs) );
  assert_eq! ( mem, MembershipAxes {
    staged: None, unstaged: Some (Sign::Minus) } );
}

#[test]
fn staged_add_then_unstaged_remove () {
  let parent : ID = id ("parent");
  let child  : ID = id ("child");
  let src    : SourceName = source_name ("public");
  let mut diffs : HashMap<SourceName, SourceDiff> = HashMap::new ();
  diffs . insert ( src . clone (),
                   source_diff_with_parent_contains (
                     &parent,
                     vec! [ Diff_Item::New     (child . clone ()) ],
                     vec! [ Diff_Item::Removed (child . clone ()) ] ) );
  let (_, mem) = phantom_axes (
    &child, &src, &parent, &src, Some (&diffs) );
  assert_eq! ( mem, MembershipAxes {
    staged: Some (Sign::Plus), unstaged: Some (Sign::Minus) } );
}

#[test]
fn no_parent_contains_diff_falls_back_to_unstaged_minus () {
  // Fallback preserves legacy behavior for callers (e.g. subscription
  // phantoms) where there's no contains_diff entry for this child.
  let parent : ID = id ("parent");
  let child  : ID = id ("child");
  let src    : SourceName = source_name ("public");
  let diffs  : HashMap<SourceName, SourceDiff> = HashMap::new ();
  let (_, mem) = phantom_axes (
    &child, &src, &parent, &src, Some (&diffs) );
  assert_eq! ( mem, MembershipAxes {
    staged: None, unstaged: Some (Sign::Minus) } );
}
