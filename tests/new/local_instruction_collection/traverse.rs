/// These are unit tests for the local-instruction-collection
/// traversal
/// (server/from_text/local_instruction_collection/traverse.rs).
/// They cover the explicit case list in
/// TODO/local-instruction-collection/3_plan.org, "testing".
/// The traversal is pure and synchronous, so these tests need no db.

use ego_tree::Tree;
use indoc::indoc;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::from_text::local_instruction_collection::traverse::collect_instructions_locally;
use skg::from_text::local_instruction_collection::types::{
  CollectedIntents, IntentsForOneId, SubscribeeTextClaim,
  SubscribeeVisibility };
use skg::types::git::Sign;
use skg::types::maybe_placed_viewnode::{
  MpViewnode, maybePlaced_to_placed_tree };
use skg::types::misc::ID;
use skg::types::tree::forest::ViewForest;
use skg::types::viewnode::{ViewNode, ViewNodeKind, Vognode};

fn collected_from_org (
  input : &str,
) -> CollectedIntents {
  collect_instructions_locally (
    &forest_from_org (input) ) . unwrap() }

fn forest_from_org (
  input : &str,
) -> ViewForest {
  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  ViewForest::from_internal_tree (
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap() ) }

fn entry<'a> (
  collected : &'a CollectedIntents,
  id        : &str,
) -> &'a IntentsForOneId {
  collected . by_pid . get (&ID::from (id))
    . unwrap_or_else ( || panic! ("no entry for {}", id) ) }

#[test]
fn ordinary_definitive_emissions () {
  let collected : CollectedIntents =
    collected_from_org ( indoc! {"
      * (skg (node (id root) (source main))) root
      Root body
      ** (skg (node (id child) (source main))) child
      ** (skg (node (id independent) (source main) (parentIs independent))) independent
      ** (skg aliasCol) aliases
      *** (skg alias) nickname
      ** (skg subscribeeCol)
      *** (skg (node (id s) (source main) indef)) s
      ** (skg overriddenCol)
      *** (skg (node (id o) (source main) indef)) o
      * (skg (node (id doomed) (source main) (editRequest delete))) doomed
      * (skg (node (id acquirer) (source main) (editRequest (merge acquiree)))) acquirer
      "} );
  { let root : &IntentsForOneId = entry (&collected, "root");
    assert_eq!( root . title_and_body,
                Some (( "root" . to_string(),
                        Some ("Root body" . to_string()) )) );
    assert_eq!( root . contains, Some (vec![ID::from ("child")]) );
    assert_eq!( root . aliases,
                Some (vec!["nickname" . to_string()]) );
    assert_eq!( root . subscribes_to, Some (vec![ID::from ("s")]) );
    assert_eq!( root . overrides, Some (vec![ID::from ("o")]) );
    assert!( ! root . delete ); }
  { let child : &IntentsForOneId = entry (&collected, "child");
    // A definitive leaf's contains is Specified and empty;
    // unmentioned fields stay unfilled (lowering to Unspecified).
    assert_eq!( child . contains, Some (vec![]) );
    assert_eq!( child . aliases, None ); }
  { let doomed : &IntentsForOneId = entry (&collected, "doomed");
    assert!( doomed . delete );
    assert_eq!( doomed . title_and_body, None ); }
  { let acquirer : &IntentsForOneId = entry (&collected, "acquirer");
    assert_eq!( acquirer . node_merge, Some (ID::from ("acquiree")) ); }
  assert_eq!(
    collected . order,
    vec![ ID::from ("root"), ID::from ("child"),
          ID::from ("independent"),
          ID::from ("doomed"), ID::from ("acquirer") ]); }

#[test]
fn subscribee_as_such_emits_claim_and_visibility () {
  let collected : CollectedIntents =
    collected_from_org ( indoc! {"
      * (skg (node (id subscriber) (source main))) subscriber
      ** (skg subscribeeCol)
      *** (skg (node (id e) (source main))) e
      Subscribee body
      **** (skg (node (id visible) (source main))) visible
      **** (skg (node (id parked) (source main) (parentIs independent))) parked
      **** (skg (node (id leaving) (source main) (editRequest delete))) leaving
      "} );
  { let e : &IntentsForOneId = entry (&collected, "e");
    // The subscribee-as-such emits no Set* intents, only a text
    // claim.
    assert_eq!( e . title_and_body, None );
    assert_eq!( e . contains, None );
    assert_eq!(
      e . text_claims,
      vec![ SubscribeeTextClaim {
        title : "e" . to_string(),
        body  : Some ("Subscribee body" . to_string()) } ]); }
  { let subscriber : &IntentsForOneId = entry (&collected, "subscriber");
    assert_eq!(
      subscriber . visibility,
      vec![ SubscribeeVisibility {
        subscribee : ID::from ("e"),
        visible    : vec![ID::from ("visible")] } ]);
    assert_eq!( subscriber . subscribes_to,
                Some (vec![ID::from ("e")]) ); }
  { // Ordinary definitive children of a subscribee-as-such still
    // emit their own instructions, but form no one's contains.
    let visible : &IntentsForOneId = entry (&collected, "visible");
    assert!( visible . title_and_body . is_some() ); }}

#[test]
fn aliascol_under_subscribee_as_such_emits_nothing () {
  // This is the trap from the discussion: a naive implementation
  // would write the subscribee's aliases.
  let collected : CollectedIntents =
    collected_from_org ( indoc! {"
      * (skg (node (id subscriber) (source main))) subscriber
      ** (skg subscribeeCol)
      *** (skg (node (id e) (source main))) e
      **** (skg aliasCol) aliases
      ***** (skg alias) sneaky alias
      "} );
  assert_eq!( entry (&collected, "e") . aliases, None ); }

#[test]
fn cols_under_toDelete_or_indefinitive_owners_emit_nothing () {
  let collected : CollectedIntents =
    collected_from_org ( indoc! {"
      * (skg (node (id doomed) (source main) (editRequest delete))) doomed
      ** (skg aliasCol) aliases
      *** (skg alias) dead alias
      ** (skg subscribeeCol)
      *** (skg (node (id s) (source main) indef)) s
      * (skg (node (id ghost) (source main) indef)) ghost
      ** (skg aliasCol) aliases
      *** (skg alias) ghost alias
      ** (skg overriddenCol)
      *** (skg (node (id o) (source main) indef)) o
      "} );
  { let doomed : &IntentsForOneId = entry (&collected, "doomed");
    assert!( doomed . delete );
    assert_eq!( doomed . aliases, None );
    assert_eq!( doomed . subscribes_to, None ); }
  assert!( collected . by_pid . get (&ID::from ("ghost")) . is_none(),
           "an indefinitive vognode and its cols emit nothing" ); }

#[test]
fn definitive_member_of_readonly_col_emits_for_itself_only () {
  let collected : CollectedIntents =
    collected_from_org ( indoc! {"
      * (skg (node (id owner) (source main))) owner
      ** (skg subscriberCol)
      *** (skg (node (id intruder) (source main))) intruder
      Intruder body
      **** (skg (node (id intruder-child) (source main))) intruder child
      "} );
  { let intruder : &IntentsForOneId = entry (&collected, "intruder");
    assert_eq!( intruder . title_and_body,
                Some (( "intruder" . to_string(),
                        Some ("Intruder body" . to_string()) )) );
    assert_eq!( intruder . contains,
                Some (vec![ID::from ("intruder-child")]) ); }
  { // The col's owner is unaffected by the col's membership.
    let owner : &IntentsForOneId = entry (&collected, "owner");
    assert_eq!( owner . contains, Some (vec![]) );
    assert_eq!( owner . subscribes_to, None ); }}

#[test]
fn definitive_child_of_inactive_vognode_emits () {
  let collected : CollectedIntents =
    collected_from_org ( indoc! {"
      * (skg (node (id root) (source main))) root
      ** (skg (inactiveNode (id hidden) (source private)))
      *** (skg (node (id stowaway) (source main))) stowaway
      "} );
  { let stowaway : &IntentsForOneId = entry (&collected, "stowaway");
    assert!( stowaway . title_and_body . is_some() ); }
  assert!( collected . by_pid . get (&ID::from ("hidden")) . is_none(),
           "the inactive node itself emits nothing" );
  assert_eq!( entry (&collected, "root") . contains,
              Some (vec![]),
              "the inactive node is not content of its parent; its \
               membership is owned by the disk weave" ); }

#[test]
fn definitive_node_inside_diff_phantom_subtree_emits () {
  let input : &str =
    indoc! {"
      * (skg (node (id root) (source main))) root
      ** (skg (node (id fading) (source main))) fading
      *** (skg (node (id survivor) (source main))) survivor
      "};
  let forest : ViewForest = {
    let mut forest : ViewForest = forest_from_org (input);
    let fading_treeid : ego_tree::NodeId =
      forest . nodes()
      . find ( |n| matches!(
          &n . value() . kind,
          ViewNodeKind::Vognode (Vognode::Active (t))
            if t . id == ID::from ("fading") ))
      . map ( |n| n . id() )
      . expect ("fading node not found");
    { let tree : &mut Tree<ViewNode> =
        forest . as_internal_tree_mut();
      if let ViewNodeKind::Vognode (Vognode::Active (t)) =
        &mut tree . get_mut (fading_treeid) . unwrap() . value() . kind
      { t . membership . unstaged = Some (Sign::Minus); }
      tree . get_mut (fading_treeid) . unwrap()
        . value() . normal_to_phantom (); }
    forest };
  let collected : CollectedIntents =
    collect_instructions_locally (&forest) . unwrap();
  assert!( collected . by_pid . get (&ID::from ("fading")) . is_none(),
           "the phantom itself emits nothing" );
  { let survivor : &IntentsForOneId = entry (&collected, "survivor");
    assert!( survivor . title_and_body . is_some(),
             "a definitive node beneath a phantom emits for itself" ); }
  assert_eq!( entry (&collected, "root") . contains, Some (vec![]),
              "the phantom is not content of its parent" ); }

#[test]
fn indefinitive_subscribee_as_such_emits_nothing () {
  let collected : CollectedIntents =
    collected_from_org ( indoc! {"
      * (skg (node (id subscriber) (source main))) subscriber
      ** (skg subscribeeCol)
      *** (skg (node (id e) (source main) indef)) e
      **** (skg (node (id under) (source main) indef)) under
      "} );
  assert!( collected . by_pid . get (&ID::from ("e")) . is_none() );
  assert_eq!( entry (&collected, "subscriber") . visibility, vec![] ); }

#[test]
fn definitive_subscribee_under_indefinitive_subscriber_claims_without_visibility () {
  let collected : CollectedIntents =
    collected_from_org ( indoc! {"
      * (skg (node (id subscriber) (source main) indef)) subscriber
      ** (skg subscribeeCol)
      *** (skg (node (id e) (source main))) e
      **** (skg (node (id visible) (source main))) visible
      "} );
  assert_eq!(
    entry (&collected, "e") . text_claims,
    vec![ SubscribeeTextClaim {
      title : "e" . to_string(),
      body  : None } ]);
  assert!( collected . by_pid . get (&ID::from ("subscriber")) . is_none(),
           "no visibility intent reaches an indefinitive subscriber" ); }

#[test]
fn present_but_empty_cols_differ_from_absent_cols () {
  let collected : CollectedIntents =
    collected_from_org ( indoc! {"
      * (skg (node (id explicit) (source main))) explicit
      ** (skg aliasCol) aliases
      ** (skg subscribeeCol)
      ** (skg overriddenCol)
      * (skg (node (id silent) (source main))) silent
      "} );
  { let explicit : &IntentsForOneId = entry (&collected, "explicit");
    // A present-but-empty col is an explicitly empty field.
    assert_eq!( explicit . aliases, Some (vec![]) );
    assert_eq!( explicit . subscribes_to, Some (vec![]) );
    assert_eq!( explicit . overrides, Some (vec![]) ); }
  { let silent : &IntentsForOneId = entry (&collected, "silent");
    // An absent col expresses no opinion.
    assert_eq!( silent . aliases, None );
    assert_eq!( silent . subscribes_to, None );
    assert_eq!( silent . overrides, None ); }}

#[test]
fn duplicate_defining_col_members_dedup_preserving_order () {
  let collected : CollectedIntents =
    collected_from_org ( indoc! {"
      * (skg (node (id owner) (source main))) owner
      ** (skg aliasCol) aliases
      *** (skg alias) echo
      *** (skg alias) other
      *** (skg alias) echo
      ** (skg subscribeeCol)
      *** (skg (node (id s1) (source main) indef)) s1
      *** (skg (node (id s2) (source main) indef)) s2
      *** (skg (node (id s1) (source main) indef)) s1
      ** (skg overriddenCol)
      *** (skg (node (id o1) (source main) indef)) o1
      *** (skg (node (id o2) (source main) indef)) o2
      *** (skg (node (id o1) (source main) indef)) o1
      "} );
  let owner : &IntentsForOneId = entry (&collected, "owner");
  assert_eq!( owner . aliases,
              Some (vec!["echo" . to_string(), "other" . to_string()]) );
  assert_eq!( owner . subscribes_to,
              Some (vec![ID::from ("s1"), ID::from ("s2")]) );
  assert_eq!( owner . overrides,
              Some (vec![ID::from ("o1"), ID::from ("o2")]) ); }
