use super::*;
use crate::types::git::GitDiffStatus;

fn id (s : &str) -> ID { ID ( s . to_string () ) }
fn ids (ss : &[&str]) -> Vec<ID> {
  ss . iter () . map ( |s| id (s) ) . collect () }
fn src (s : &str) -> SourceName { SourceName ( s . to_string () ) }

fn modified_hides_entry (
  hides_diff : Vec<Diff_Item<ID>>,
) -> NodeCompleteDiff {
  NodeCompleteDiff {
    status : GitDiffStatus::Modified,
    node_changes : Some ( NodeChanges {
      hides_diff,
      .. NodeChanges::default () } ),
    before_node : None,
    after_node : None } }

fn diffs_with_one_entry (
  source   : &SourceName,
  staged   : Option<(PathBuf, NodeCompleteDiff)>,
  unstaged : Option<(PathBuf, NodeCompleteDiff)>,
) -> Option<HashMap<SourceName, SourceDiff>> {
  Some ( HashMap::from ([ ( source . clone (), SourceDiff {
    is_git_repo   : true,
    staged        : staged   . into_iter () . collect (),
    unstaged      : unstaged . into_iter () . collect (),
    added_nodes   : HashMap::new (),
    deleted_nodes : HashMap::new (), } ) ]) ) }

#[test]
fn snapshots_degenerate_when_the_file_is_unchanged () {
  let worktree : Vec<ID> = ids (&["a", "b"]);
  let [head, index, wt] =
    three_snapshots_of_relation_list (
      &id ("S"), &src ("main"),
      NodeRelation::HidesFromItsSubscriptions,
      &worktree,
      & diffs_with_one_entry (&src ("main"), None, None) );
  assert_eq! (head,  worktree);
  assert_eq! (index, worktree);
  assert_eq! (wt,    worktree);
}

#[test]
fn unstaged_change_reconstructs_the_index_as_the_before_list () {
  // worktree [a, c]; the unstaged diff says b was removed and c
  // added, so index (= the unstaged before-list) is [a, b], and
  // HEAD = index (no staged entry).
  let file : PathBuf = PathBuf::from ("S.skg");
  let diffs = diffs_with_one_entry (
    &src ("main"),
    None,
    Some (( file, modified_hides_entry ( vec! [
      Diff_Item::Unchanged (id ("a")),
      Diff_Item::Removed   (id ("b")),
      Diff_Item::New       (id ("c")) ] )) ));
  let [head, index, _wt] =
    three_snapshots_of_relation_list (
      &id ("S"), &src ("main"),
      NodeRelation::HidesFromItsSubscriptions,
      & ids (&["a", "c"]), &diffs );
  assert_eq! (index, ids (&["a", "b"]));
  assert_eq! (head,  ids (&["a", "b"]));
}

#[test]
fn staged_change_separates_head_from_index () {
  // The staged diff says b was removed (HEAD [a, b] -> index [a]);
  // no unstaged entry, so index = worktree = [a].
  let file : PathBuf = PathBuf::from ("S.skg");
  let diffs = diffs_with_one_entry (
    &src ("main"),
    Some (( file, modified_hides_entry ( vec! [
      Diff_Item::Unchanged (id ("a")),
      Diff_Item::Removed   (id ("b")) ] )) ),
    None );
  let [head, index, wt] =
    three_snapshots_of_relation_list (
      &id ("S"), &src ("main"),
      NodeRelation::HidesFromItsSubscriptions,
      & ids (&["a"]), &diffs );
  assert_eq! (head,  ids (&["a", "b"]));
  assert_eq! (index, ids (&["a"]));
  assert_eq! (wt,    ids (&["a"]));
}

#[test]
fn hiddenin_signs_come_from_either_input_list_with_exact_stages () {
  // Subscriber S hides [h1, h2] throughout; subscribee B's contains
  // gained h2 STAGED.  h2's hidden-in membership is therefore newly
  // derived-in with a STAGED Plus -- driven by the contains input,
  // with no hides change at all.
  let b_file : PathBuf = PathBuf::from ("B.skg");
  let diffs = diffs_with_one_entry (
    &src ("main"),
    Some (( b_file, NodeCompleteDiff {
      status : GitDiffStatus::Modified,
      node_changes : Some ( NodeChanges {
        contains_diff : vec! [
          Diff_Item::Unchanged (id ("h1")),
          Diff_Item::New       (id ("h2")) ],
        .. NodeChanges::default () } ),
      before_node : None,
      after_node : None } )),
    None );
  let (goal, removed, axes) =
    goal_list_for_hiddeninsubscribee_col (
      &id ("B"), &src ("main"),
      &id ("S"), &src ("main"),
      & ids (&["h1", "h2"]), // B's worktree contains
      & ids (&["h1", "h2"]), // S's worktree hides
      &diffs );
  assert_eq! (goal, ids (&["h1", "h2"]));
  assert! (removed . is_empty ());
  assert_eq! ( axes [ &id ("h2") ],
    MembershipAxes { staged : Some (Sign::Plus), unstaged : None } );
  assert! ( ! axes . contains_key (&id ("h1")),
    "an unchanged member contributes no signs" );
}

#[test]
fn hiddenin_removed_member_gets_exact_stage_label () {
  // S's hides dropped h1 UNSTAGED while B's contains kept it: h1
  // leaves the derived membership with an unstaged Minus, and joins
  // the goal list as a removed member.
  let s_file : PathBuf = PathBuf::from ("S.skg");
  let diffs = diffs_with_one_entry (
    &src ("main"),
    None,
    Some (( s_file, modified_hides_entry ( vec! [
      Diff_Item::Removed (id ("h1")) ] )) ));
  let (goal, removed, axes) =
    goal_list_for_hiddeninsubscribee_col (
      &id ("B"), &src ("main"),
      &id ("S"), &src ("main"),
      & ids (&["h1"]), // B's worktree contains
      & ids (&[]),     // S's worktree hides
      &diffs );
  assert_eq! (goal, ids (&["h1"]));
  assert! (removed . contains (&id ("h1")));
  assert_eq! ( axes [ &id ("h1") ],
    MembershipAxes { staged : None, unstaged : Some (Sign::Minus) } );
}

#[test]
fn no_diffs_means_no_signs_and_the_worktree_goal () {
  let (goal, removed, axes) =
    goal_list_for_hiddeninsubscribee_col (
      &id ("B"), &src ("main"),
      &id ("S"), &src ("main"),
      & ids (&["h1", "v"]),
      & ids (&["h1"]),
      &None );
  assert_eq! (goal, ids (&["h1"]));
  assert! (removed . is_empty ());
  assert! (axes . is_empty ());
}
