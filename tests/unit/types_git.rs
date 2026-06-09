use super::*;

fn removed<T: Clone> (v: T) -> Diff_Item<T> { Diff_Item::Removed (v) }
fn new_   <T: Clone> (v: T) -> Diff_Item<T> { Diff_Item::New      (v) }
fn same   <T: Clone> (v: T) -> Diff_Item<T> { Diff_Item::Unchanged (v) }

// The case that motivated lifting this bug: an item removed on
// the STAGED side only, with no entry on the unstaged side,
// should produce a net Removed -- NOT Unchanged. The earlier
// code mis-classified (staged=Minus, unstaged=None) as
// Unchanged, causing the rerender pipeline to try reading the
// now-missing .skg file.
#[test]
fn staged_only_remove_is_net_removed () {
  let staged   : Vec<Diff_Item<&str>> = vec! [ removed ("x") ];
  let unstaged : Vec<Diff_Item<&str>> = vec! [];
  let net : Vec<Diff_Item<&str>> =
    net_diff_from_per_stage (
      Some (&staged), Some (&unstaged) );
  assert_eq! ( net, vec! [ Diff_Item::Removed ("x") ] ); }

#[test]
fn staged_only_add_is_net_new () {
  let staged   : Vec<Diff_Item<&str>> = vec! [ new_ ("x") ];
  let unstaged : Vec<Diff_Item<&str>> = vec! [];
  assert_eq! (
    net_diff_from_per_stage (
      Some (&staged), Some (&unstaged) ),
    vec! [ Diff_Item::New ("x") ] ); }

#[test]
fn unstaged_only_remove_is_net_removed () {
  let staged   : Vec<Diff_Item<&str>> = vec! [];
  let unstaged : Vec<Diff_Item<&str>> = vec! [ removed ("x") ];
  assert_eq! (
    net_diff_from_per_stage (
      Some (&staged), Some (&unstaged) ),
    vec! [ Diff_Item::Removed ("x") ] ); }

#[test]
fn unstaged_only_add_is_net_new () {
  let staged   : Vec<Diff_Item<&str>> = vec! [];
  let unstaged : Vec<Diff_Item<&str>> = vec! [ new_ ("x") ];
  assert_eq! (
    net_diff_from_per_stage (
      Some (&staged), Some (&unstaged) ),
    vec! [ Diff_Item::New ("x") ] ); }

// Unchanged items in the baseline must survive as net Unchanged
// (not dropped). Callers rely on goal-list construction seeing
// them to preserve order.
#[test]
fn baseline_unchanged_is_net_unchanged () {
  let unstaged : Vec<Diff_Item<&str>> = vec! [ same ("x") ];
  assert_eq! (
    net_diff_from_per_stage (
      None, Some (&unstaged) ),
    vec! [ Diff_Item::Unchanged ("x") ] ); }

// Added staged then removed unstaged: net is no change (drop).
#[test]
fn added_staged_then_removed_unstaged_drops () {
  let staged   : Vec<Diff_Item<&str>> = vec! [ new_    ("x") ];
  let unstaged : Vec<Diff_Item<&str>> = vec! [ removed ("x") ];
  assert_eq! (
    net_diff_from_per_stage (
      Some (&staged), Some (&unstaged) ),
    Vec::<Diff_Item<&str>>::new () ); }

// Removed staged then re-added unstaged: present throughout -> Unchanged.
#[test]
fn removed_staged_then_readded_unstaged_is_unchanged () {
  let staged   : Vec<Diff_Item<&str>> = vec! [ removed ("x") ];
  let unstaged : Vec<Diff_Item<&str>> = vec! [ new_    ("x") ];
  assert_eq! (
    net_diff_from_per_stage (
      Some (&staged), Some (&unstaged) ),
    vec! [ Diff_Item::Unchanged ("x") ] ); }

// When the SAME parent's contains changed in BOTH stages, a removed member must
// keep its HEAD position among surviving siblings, not get tail-appended.
// HEAD=[a,x,b], index=[a,b] (x removed staged), worktree=[a,c,b] (c added
// unstaged): x must land between a and b. (review-2 §2.2)
#[test]
fn two_stage_removed_member_keeps_head_position () {
  let staged   : Vec<Diff_Item<&str>> =
    vec! [ same ("a"), removed ("x"), same ("b") ];
  let unstaged : Vec<Diff_Item<&str>> =
    vec! [ same ("a"), new_ ("c"), same ("b") ];
  let net : Vec<Diff_Item<&str>> =
    net_diff_from_per_stage ( Some (&staged), Some (&unstaged) );
  let pos = |needle : Diff_Item<&str>| -> usize {
    net . iter () . position ( |it| *it == needle )
      . unwrap_or_else ( || panic! (
        "net diff missing {:?}; got {:?}", needle, net ) ) };
  // every item is classified correctly ...
  let a : usize = pos ( Diff_Item::Unchanged ("a") );
  let x : usize = pos ( Diff_Item::Removed    ("x") );
  let c : usize = pos ( Diff_Item::New        ("c") );
  let b : usize = pos ( Diff_Item::Unchanged  ("b") );
  // ... and the removed x sits between a and b, not after b.
  assert! ( a < x && x < b,
            "removed x should keep its HEAD slot (a < x < b); got {:?}", net );
  assert! ( c < b,
            "new c should precede b; got {:?}", net ); }
