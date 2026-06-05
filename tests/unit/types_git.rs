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
