use super::*;

fn skgid (s: &str) -> ID { ID ( s . to_string () ) }

#[test]
fn removed_anchors_to_the_next_survivor () {
  // HEAD [a, gone, b] -> worktree [a, b]: the phantom 'gone' sits
  // between its surviving siblings, not at the front.
  let net = vec! [
    Diff_Item::Unchanged (skgid ("a")),
    Diff_Item::Removed   (skgid ("gone")),
    Diff_Item::Unchanged (skgid ("b")) ];
  assert_eq! ( phantom_insertion_plan (&net),
               vec! [ ( skgid ("gone"), Some (skgid ("b")) ) ] ); }

#[test]
fn trailing_removed_appends () {
  // HEAD [a, gone] -> worktree [a]: nothing survives after 'gone',
  // so it appends (anchor None).
  let net = vec! [
    Diff_Item::Unchanged (skgid ("a")),
    Diff_Item::Removed   (skgid ("gone")) ];
  assert_eq! ( phantom_insertion_plan (&net),
               vec! [ ( skgid ("gone"), None ) ] ); }

#[test]
fn consecutive_removed_keep_order_before_shared_anchor () {
  // HEAD [x, y, s] -> worktree [s]: x then y, both before s.
  let net = vec! [
    Diff_Item::Removed   (skgid ("x")),
    Diff_Item::Removed   (skgid ("y")),
    Diff_Item::New       (skgid ("s")) ];
  assert_eq! ( phantom_insertion_plan (&net),
               vec! [ ( skgid ("x"), Some (skgid ("s")) ),
                      ( skgid ("y"), Some (skgid ("s")) ) ] ); }

#[test]
fn no_removals_is_empty_plan () {
  let net = vec! [
    Diff_Item::Unchanged (skgid ("a")),
    Diff_Item::New       (skgid ("b")) ];
  assert! ( phantom_insertion_plan (&net) . is_empty () ); }
