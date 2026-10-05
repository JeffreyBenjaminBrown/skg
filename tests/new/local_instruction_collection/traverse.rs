/// These are unit tests for the local-instruction-collection
/// traversal
/// (server/from_text/local_instruction_collection/traverse.rs).
/// They cover the explicit case list in
/// TODO/DONE/local-instruction-collection/3_plan.org, "testing".
/// The traversal is pure and synchronous, so these tests need no db.

use ego_tree::Tree;
use indoc::indoc;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::from_text::local_instruction_collection::traverse::collect_instructions_locally;
use skg::from_text::local_instruction_collection::types::{
  CollectedFieldIntents, FieldIntentsForOneId, SubscribeeTextClaim,
  SubscribeeVisibility };
use skg::types::git::Sign;
use skg::types::maybe_placed_viewnode::{
  MpViewnode, maybePlaced_to_placed_tree };
use skg::types::misc::ID;
use skg::types::nodes::complete::Flag;
use skg::types::tree::forest::ViewForest;
use skg::types::viewnode::{Viewnode, ViewnodeKind, Vognode};

fn collected_from_org (
  input : &str,
) -> CollectedFieldIntents {
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
  collected : &'a CollectedFieldIntents,
  skgid        : &str,
) -> &'a FieldIntentsForOneId {
  collected . by_pid . get (&ID::from (skgid))
    . unwrap_or_else ( || panic! ("no entry for {}", skgid) ) }

#[test]
fn ordinary_editable_emissions () {
  let collected : CollectedFieldIntents =
    collected_from_org ( indoc! {"
      * (skg (node (id root) (repo main) (editRequest (flag noSearchMatching true)))) root
      Root body
      ** (skg (node (id child) (repo main))) child
      ** (skg (node (id independent) (repo main) (affectsParent false))) independent
      ** (skg aliasFolder) aliases
      *** (skg alias) nickname
      ** (skg subscribeeFolder)
      *** (skg (node (id s) (repo main) writeProtected)) s
      ** (skg overriddenFolder)
      *** (skg (node (id o) (repo main) writeProtected)) o
      * (skg (node (id doomed) (repo main) (editRequest delete))) doomed
      * (skg (node (id acquirer) (repo main) (editRequest (merge acquiree)))) acquirer
      "} );
  { let root : &FieldIntentsForOneId = entry (&collected, "root");
    assert_eq!( root . title_and_body,
                Some (( "root" . to_string(),
                        Some ("Root body" . to_string()) )) );
    assert_eq!( root . contains, Some (vec![(ID::from ("child"), None)]) );
    assert_eq!( root . aliases,
                Some (vec![("nickname" . to_string(), None)]) );
    assert_eq!( root . subscribesTo, Some (vec![(ID::from ("s"), None)]) );
    assert_eq!( root . overrides, Some (vec![(ID::from ("o"), None)]) );
    assert_eq!( root . flag,
                Some ((Flag::NoSearchMatching, true)) );
    assert!( ! root . delete ); }
  { let child : &FieldIntentsForOneId = entry (&collected, "child");
    // An editable leaf's contains is Specified and empty;
    // unmentioned fields stay unfilled (lowering to Unspecified).
    assert_eq!( child . contains, Some (vec![]) );
    assert_eq!( child . aliases, None ); }
  { let doomed : &FieldIntentsForOneId = entry (&collected, "doomed");
    assert!( doomed . delete );
    assert_eq!( doomed . title_and_body, None ); }
  { let acquirer : &FieldIntentsForOneId = entry (&collected, "acquirer");
    assert_eq!( acquirer . node_merge, Some (ID::from ("acquiree")) ); }
  assert_eq!(
    collected . order,
    vec![ ID::from ("root"), ID::from ("child"),
          ID::from ("independent"),
          ID::from ("doomed"), ID::from ("acquirer") ]); }

#[test]
fn subscribee_as_such_emits_claim_and_visibility () {
  let collected : CollectedFieldIntents =
    collected_from_org ( indoc! {"
      * (skg (node (id subscriber) (repo main))) subscriber
      ** (skg subscribeeFolder)
      *** (skg (node (id e) (repo main))) e
      Subscribee body
      **** (skg (node (id visible) (repo main))) visible
      **** (skg (node (id parked) (repo main) (affectsParent false))) parked
      **** (skg (node (id leaving) (repo main) (editRequest delete))) leaving
      "} );
  { let e : &FieldIntentsForOneId = entry (&collected, "e");
    // The subscribee-as-such emits no Set* intents, only a text
    // claim.
    assert_eq!( e . title_and_body, None );
    assert_eq!( e . contains, None );
    assert_eq!(
      e . text_claims,
      vec![ SubscribeeTextClaim {
        title : "e" . to_string(),
        body  : Some ("Subscribee body" . to_string()) } ]); }
  { let subscriber : &FieldIntentsForOneId = entry (&collected, "subscriber");
    assert_eq!(
      subscriber . visibility,
      vec![ SubscribeeVisibility {
        subscribee : ID::from ("e"),
        visible    : vec![ID::from ("visible")] } ]);
    assert_eq!( subscriber . subscribesTo,
                Some (vec![(ID::from ("e"), None)]) ); }
  { // Ordinary editable children of a subscribee-as-such still
    // emit their own instructions, but form no one's contains.
    let visible : &FieldIntentsForOneId = entry (&collected, "visible");
    assert!( visible . title_and_body . is_some() ); }}

#[test]
fn aliasfolder_under_subscribee_as_such_emits_nothing () {
  // This is the trap from the discussion: a naive implementation
  // would write the subscribee's aliases.
  let collected : CollectedFieldIntents =
    collected_from_org ( indoc! {"
      * (skg (node (id subscriber) (repo main))) subscriber
      ** (skg subscribeeFolder)
      *** (skg (node (id e) (repo main))) e
      **** (skg aliasFolder) aliases
      ***** (skg alias) sneaky alias
      "} );
  assert_eq!( entry (&collected, "e") . aliases, None ); }

#[test]
fn folders_under_toDelete_or_writeProtected_recorders_emit_nothing () {
  let collected : CollectedFieldIntents =
    collected_from_org ( indoc! {"
      * (skg (node (id doomed) (repo main) (editRequest delete))) doomed
      ** (skg aliasFolder) aliases
      *** (skg alias) dead alias
      ** (skg subscribeeFolder)
      *** (skg (node (id s) (repo main) writeProtected)) s
      * (skg (node (id ghost) (repo main) writeProtected)) ghost
      ** (skg aliasFolder) aliases
      *** (skg alias) ghost alias
      ** (skg overriddenFolder)
      *** (skg (node (id o) (repo main) writeProtected)) o
      "} );
  { let doomed : &FieldIntentsForOneId = entry (&collected, "doomed");
    assert!( doomed . delete );
    assert_eq!( doomed . aliases, None );
    assert_eq!( doomed . subscribesTo, None ); }
  assert!( collected . by_pid . get (&ID::from ("ghost")) . is_none(),
           "a write-protected vognode and its folders emit nothing" ); }

#[test]
fn editable_member_of_write_protected_folder_emits_for_itself_only () {
  let collected : CollectedFieldIntents =
    collected_from_org ( indoc! {"
      * (skg (node (id recorder) (repo main))) recorder
      ** (skg subscriberFolder)
      *** (skg (node (id intruder) (repo main))) intruder
      Intruder body
      **** (skg (node (id intruder-child) (repo main))) intruder child
      "} );
  { let intruder : &FieldIntentsForOneId = entry (&collected, "intruder");
    assert_eq!( intruder . title_and_body,
                Some (( "intruder" . to_string(),
                        Some ("Intruder body" . to_string()) )) );
    assert_eq!( intruder . contains,
                Some (vec![(ID::from ("intruder-child"), None)]) ); }
  { // The folder's recorder is unaffected by the folder's membership.
    let recorder : &FieldIntentsForOneId = entry (&collected, "recorder");
    assert_eq!( recorder . contains, Some (vec![]) );
    assert_eq!( recorder . subscribesTo, None ); }}

#[test]
fn editable_child_of_inactive_vognode_emits () {
  let collected : CollectedFieldIntents =
    collected_from_org ( indoc! {"
      * (skg (node (id root) (repo main))) root
      ** (skg (inactiveNode (id hidden) (repo private)))
      *** (skg (node (id stowaway) (repo main))) stowaway
      "} );
  { let stowaway : &FieldIntentsForOneId = entry (&collected, "stowaway");
    assert!( stowaway . title_and_body . is_some() ); }
  assert!( collected . by_pid . get (&ID::from ("hidden")) . is_none(),
           "the inactive node itself emits nothing" );
  assert_eq!( entry (&collected, "root") . contains,
              Some (vec![]),
              "the inactive node is not content of its parent; its \
               membership is owned by the disk weave" ); }

#[test]
fn editable_node_inside_diff_phantom_subtree_emits () {
  let input : &str =
    indoc! {"
      * (skg (node (id root) (repo main))) root
      ** (skg (node (id fading) (repo main))) fading
      *** (skg (node (id survivor) (repo main))) survivor
      "};
  let forest : ViewForest = {
    let mut forest : ViewForest = forest_from_org (input);
    let fading_treeid : ego_tree::NodeId =
      forest . nodes()
      . find ( |n| matches!(
          &n . value() . kind,
          ViewnodeKind::Vognode (Vognode::Active (t))
            if t . skgid == ID::from ("fading") ))
      . map ( |n| n . id() )
      . expect ("fading node not found");
    { let tree : &mut Tree<Viewnode> =
        forest . as_internal_tree_mut();
      if let ViewnodeKind::Vognode (Vognode::Active (t)) =
        &mut tree . get_mut (fading_treeid) . unwrap() . value() . kind
      { t . relationship_axes . unstaged = Some (Sign::Minus); }
      tree . get_mut (fading_treeid) . unwrap()
        . value() . normal_to_phantom (); }
    forest };
  let collected : CollectedFieldIntents =
    collect_instructions_locally (&forest) . unwrap();
  assert!( collected . by_pid . get (&ID::from ("fading")) . is_none(),
           "the phantom itself emits nothing" );
  { let survivor : &FieldIntentsForOneId = entry (&collected, "survivor");
    assert!( survivor . title_and_body . is_some(),
             "a editable node beneath a phantom emits for itself" ); }
  assert_eq!( entry (&collected, "root") . contains, Some (vec![]),
              "the phantom is not content of its parent" ); }

#[test]
fn writeProtected_subscribee_as_such_emits_nothing () {
  let collected : CollectedFieldIntents =
    collected_from_org ( indoc! {"
      * (skg (node (id subscriber) (repo main))) subscriber
      ** (skg subscribeeFolder)
      *** (skg (node (id e) (repo main) writeProtected)) e
      **** (skg (node (id under) (repo main) writeProtected)) under
      "} );
  assert!( collected . by_pid . get (&ID::from ("e")) . is_none() );
  assert_eq!( entry (&collected, "subscriber") . visibility, vec![] ); }

#[test]
fn editable_subscribee_under_writeProtected_subscriber_claims_without_visibility () {
  let collected : CollectedFieldIntents =
    collected_from_org ( indoc! {"
      * (skg (node (id subscriber) (repo main) writeProtected)) subscriber
      ** (skg subscribeeFolder)
      *** (skg (node (id e) (repo main))) e
      **** (skg (node (id visible) (repo main))) visible
      "} );
  assert_eq!(
    entry (&collected, "e") . text_claims,
    vec![ SubscribeeTextClaim {
      title : "e" . to_string(),
      body  : None } ]);
  assert!( collected . by_pid . get (&ID::from ("subscriber")) . is_none(),
           "no visibility intent reaches a write-protected subscriber" ); }

#[test]
fn present_but_empty_folders_differ_from_absent_folders () {
  let collected : CollectedFieldIntents =
    collected_from_org ( indoc! {"
      * (skg (node (id explicit) (repo main))) explicit
      ** (skg aliasFolder) aliases
      ** (skg subscribeeFolder)
      ** (skg overriddenFolder)
      * (skg (node (id silent) (repo main))) silent
      "} );
  { let explicit : &FieldIntentsForOneId = entry (&collected, "explicit");
    // A present-but-empty folder is an explicitly empty field.
    assert_eq!( explicit . aliases, Some (vec![]) );
    assert_eq!( explicit . subscribesTo, Some (vec![]) );
    assert_eq!( explicit . overrides, Some (vec![]) ); }
  { let silent : &FieldIntentsForOneId = entry (&collected, "silent");
    // An absent folder expresses no opinion.
    assert_eq!( silent . aliases, None );
    assert_eq!( silent . subscribesTo, None );
    assert_eq!( silent . overrides, None ); }}

#[test]
fn duplicate_defining_folder_members_dedup_preserving_order () {
  let collected : CollectedFieldIntents =
    collected_from_org ( indoc! {"
      * (skg (node (id recorder) (repo main))) recorder
      ** (skg aliasFolder) aliases
      *** (skg alias) echo
      *** (skg alias) other
      *** (skg alias) echo
      ** (skg subscribeeFolder)
      *** (skg (node (id s1) (repo main) writeProtected)) s1
      *** (skg (node (id s2) (repo main) writeProtected)) s2
      *** (skg (node (id s1) (repo main) writeProtected)) s1
      ** (skg overriddenFolder)
      *** (skg (node (id o1) (repo main) writeProtected)) o1
      *** (skg (node (id o2) (repo main) writeProtected)) o2
      *** (skg (node (id o1) (repo main) writeProtected)) o1
      "} );
  let recorder : &FieldIntentsForOneId = entry (&collected, "recorder");
  assert_eq!( recorder . aliases,
              Some (vec![("echo" . to_string(), None),
                          ("other" . to_string(), None)]) );
  assert_eq!( recorder . subscribesTo,
              Some (vec![(ID::from ("s1"), None), (ID::from ("s2"), None)]) );
  assert_eq!( recorder . overrides,
              Some (vec![(ID::from ("o1"), None), (ID::from ("o2"), None)]) ); }
