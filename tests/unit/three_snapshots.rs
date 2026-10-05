use super::*;
use crate::types::git::GitDiffStatus;

fn skgid (s : &str) -> ID { ID ( s . to_string () ) }
fn skgids (ss : &[&str]) -> Vec<ID> {
  ss . iter () . map ( |s| skgid (s) ) . collect () }
fn src (s : &str) -> SkgRepoName { SkgRepoName ( s . to_string () ) }

fn modified_hides_entry (
  hides_diff : Vec<Diff_Item<ID>>,
) -> GraphnodeDiff {
  GraphnodeDiff {
    status : GitDiffStatus::Modified,
    node_changes : Some ( NodeChanges {
      hides_diff,
      .. NodeChanges::default () } ),
    before_node : None,
    after_node : None } }

fn diffs_with_one_entry (
  skgrepo  : &SkgRepoName,
  staged   : Option<(PathBuf, GraphnodeDiff)>,
  unstaged : Option<(PathBuf, GraphnodeDiff)>,
) -> Option<HashMap<SkgRepoName, SkgRepoDiff>> {
  Some ( HashMap::from ([ ( skgrepo . clone (), SkgRepoDiff {
    is_gitrepo   : true,
    staged        : staged   . into_iter () . collect (),
    unstaged      : unstaged . into_iter () . collect (),
    added_nodes   : HashMap::new (),
    deleted_nodes : HashMap::new (), } ) ]) ) }

#[test]
fn snapshots_degenerate_when_the_file_is_unchanged () {
  let worktree : Vec<ID> = skgids (&["a", "b"]);
  let [head, index, wt] =
    three_git_snapshots_of_relation_list (
      &skgid ("S"), &src ("main"),
      NodeRelation::HidesFromSubs,
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
      Diff_Item::Unchanged (skgid ("a")),
      Diff_Item::Removed   (skgid ("b")),
      Diff_Item::New       (skgid ("c")) ] )) ));
  let [head, index, _wt] =
    three_git_snapshots_of_relation_list (
      &skgid ("S"), &src ("main"),
      NodeRelation::HidesFromSubs,
      & skgids (&["a", "c"]), &diffs );
  assert_eq! (index, skgids (&["a", "b"]));
  assert_eq! (head,  skgids (&["a", "b"]));
}

#[test]
fn staged_change_separates_head_from_index () {
  // The staged diff says b was removed (HEAD [a, b] -> index [a]);
  // no unstaged entry, so index = worktree = [a].
  let file : PathBuf = PathBuf::from ("S.skg");
  let diffs = diffs_with_one_entry (
    &src ("main"),
    Some (( file, modified_hides_entry ( vec! [
      Diff_Item::Unchanged (skgid ("a")),
      Diff_Item::Removed   (skgid ("b")) ] )) ),
    None );
  let [head, index, wt] =
    three_git_snapshots_of_relation_list (
      &skgid ("S"), &src ("main"),
      NodeRelation::HidesFromSubs,
      & skgids (&["a"]), &diffs );
  assert_eq! (head,  skgids (&["a", "b"]));
  assert_eq! (index, skgids (&["a"]));
  assert_eq! (wt,    skgids (&["a"]));
}

#[test]
fn hiddenin_signs_come_from_either_input_list_with_exact_stages () {
  let graph = InRustGraph::new ();
  // Subscriber S hides [h1, h2] throughout; subscribee B's contains
  // gained h2 STAGED.  h2's hidden-in membership is therefore newly
  // derived-in with a STAGED Plus -- driven by the contains input,
  // with no hides change at all.
  let b_file : PathBuf = PathBuf::from ("B.skg");
  let diffs = diffs_with_one_entry (
    &src ("main"),
    Some (( b_file, GraphnodeDiff {
      status : GitDiffStatus::Modified,
      node_changes : Some ( NodeChanges {
        contains_diff : vec! [
          Diff_Item::Unchanged (skgid ("h1")),
          Diff_Item::New       (skgid ("h2")) ],
        .. NodeChanges::default () } ),
      before_node : None,
      after_node : None } )),
    None );
  let (goal, removed, axes) =
    goal_list_for_hiddenInSubscribee_folder (
      &graph,
      &skgid ("B"), &src ("main"),
      &skgid ("S"), &src ("main"),
      & skgids (&["h1", "h2"]), // B's worktree contains
      & skgids (&["h1", "h2"]), // S's worktree hides
      &diffs );
  assert_eq! (goal, skgids (&["h1", "h2"]));
  assert! (removed . is_empty ());
  assert_eq! ( axes [ &skgid ("h2") ],
    RelationshipAxes { staged : Some (Sign::Plus), unstaged : None } );
  assert! ( ! axes . contains_key (&skgid ("h1")),
    "an unchanged member contributes no signs" );
}

#[test]
fn hiddenin_removed_member_gets_exact_stage_label () {
  let graph = InRustGraph::new ();
  // S's hides dropped h1 UNSTAGED while B's contains kept it: h1
  // leaves the derived membership with an unstaged Minus, and joins
  // the goal list as a removed member.
  let s_file : PathBuf = PathBuf::from ("S.skg");
  let diffs = diffs_with_one_entry (
    &src ("main"),
    None,
    Some (( s_file, modified_hides_entry ( vec! [
      Diff_Item::Removed (skgid ("h1")) ] )) ));
  let (goal, removed, axes) =
    goal_list_for_hiddenInSubscribee_folder (
      &graph,
      &skgid ("B"), &src ("main"),
      &skgid ("S"), &src ("main"),
      & skgids (&["h1"]), // B's worktree contains
      & skgids (&[]),     // S's worktree hides
      &diffs );
  assert_eq! (goal, skgids (&["h1"]));
  assert! (removed . contains (&skgid ("h1")));
  assert_eq! ( axes [ &skgid ("h1") ],
    RelationshipAxes { staged : None, unstaged : Some (Sign::Minus) } );
}

#[test]
fn no_diffs_means_no_signs_and_the_worktree_goal () {
  let graph = InRustGraph::new ();
  let (goal, removed, axes) =
    goal_list_for_hiddenInSubscribee_folder (
      &graph,
      &skgid ("B"), &src ("main"),
      &skgid ("S"), &src ("main"),
      & skgids (&["h1", "v"]),
      & skgids (&["h1"]),
      &None );
  assert_eq! (goal, skgids (&["h1"]));
  assert! (removed . is_empty ());
  assert! (axes . is_empty ());
}
