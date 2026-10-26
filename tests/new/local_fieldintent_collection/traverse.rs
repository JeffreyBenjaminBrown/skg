/// These are unit tests for the local-fieldintent-collection
/// traversal
/// (server/from_text/local_fieldintent_collection/traverse.rs).
/// The traversal is pure and synchronous, so these tests need no db.

use ego_tree::Tree;
use indoc::indoc;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::from_text::local_fieldintent_collection::traverse::collect_instructions_locally;
use skg::types::errors::HeadlineName;
use skg::from_text::local_fieldintent_collection::types::{
  CollectedFieldIntents, FieldIntentsForOneId, SubscribeeTextClaim,
  SubscribeeVisibility };
use skg::types::maybe_placed_viewnode::{
  MpViewnode, maybePlaced_to_placed_tree };
use skg::types::misc::{ID, SkgrepoName};
use skg::types::nodes::complete::Flag;
use skg::types::tree::forest::ViewForest;

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
      * (skg node (gnid root) (skgRepos (nodeRepo main)) (editRequest (flag noSearchMatching true))) root
      Root body
      ** (skg node (gnid child) (skgRepos (nodeRepo main))) child
      ** (skg node (gnid independent) (skgRepos (nodeRepo main)) noClaim) independent
      ** (skg aliasFolder) aliases
      *** (skg alias) nickname
      ** (skg subscribeeFolder)
      *** (skg node (gnid s) (skgRepos (nodeRepo main)) writeProtected) s
      ** (skg overriddenFolder)
      *** (skg node (gnid o) (skgRepos (nodeRepo main)) writeProtected) o
      * (skg node (gnid doomed) (skgRepos (nodeRepo main)) (editRequest delete)) doomed
      * (skg node (gnid acquirer) (skgRepos (nodeRepo main)) (editRequest (merge acquiree))) acquirer
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
fn writeProtected_members_relRepo_requests_reach_their_editable_recorder () {
  // A relRepo request describes the relationship from the view-parent,
  // so it is the editable recorder's instruction even when the member
  // carrying it is write-protected.
  let collected : CollectedFieldIntents =
    collected_from_org ( indoc! {"
      * (skg node (gnid root) (skgRepos (nodeRepo main))) root
      ** (skg node (gnid c) (skgRepos (nodeRepo main)) writeProtected (editRequest (relRepo private))) c
      ** (skg subscribeeFolder)
      *** (skg node (gnid s) (skgRepos (nodeRepo main)) writeProtected (editRequest (relRepo private))) s
      ** (skg overriddenFolder)
      *** (skg node (gnid o) (skgRepos (nodeRepo main)) writeProtected (editRequest (relRepo private))) o
      "} );
  let root : &FieldIntentsForOneId = entry (&collected, "root");
  let private : Option<SkgrepoName> = Some (SkgrepoName::from ("private"));
  assert_eq!( root . contains,
              Some (vec![(ID::from ("c"), private . clone ())]) );
  assert_eq!( root . subscribesTo,
              Some (vec![(ID::from ("s"), private . clone ())]) );
  assert_eq!( root . overrides,
              Some (vec![(ID::from ("o"), private)]) ); }

#[test]
fn subscribee_as_such_emits_claim_and_visibility () {
  let collected : CollectedFieldIntents =
    collected_from_org ( indoc! {"
      * (skg node (gnid subscriber) (skgRepos (nodeRepo main))) subscriber
      ** (skg subscribeeFolder)
      *** (skg node (gnid e) (skgRepos (nodeRepo main))) e
      Subscribee body
      **** (skg node (gnid visible) (skgRepos (nodeRepo main))) visible
      **** (skg node (gnid hidden) (skgRepos (nodeRepo main)) noClaim) hidden
      **** (skg node (gnid leaving) (skgRepos (nodeRepo main)) (editRequest delete)) leaving
      "} );
  { let e : &FieldIntentsForOneId = entry (&collected, "e");
    // The subscribee-as-such emits no Set* intents, only a text
    // claim.
    assert_eq!( e . title_and_body, None );
    assert_eq!( e . contains, None );
    assert_eq!(
      e . text_claims,
      vec![ SubscribeeTextClaim {
        title    : "e" . to_string(),
        body     : Some ("Subscribee body" . to_string()),
        headline : HeadlineName::of ( None, Some (&ID::from ("e")), "e" ) } ]); }
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
  // The trap: a naive implementation
  // would write the subscribee's aliases.
  let collected : CollectedFieldIntents =
    collected_from_org ( indoc! {"
      * (skg node (gnid subscriber) (skgRepos (nodeRepo main))) subscriber
      ** (skg subscribeeFolder)
      *** (skg node (gnid e) (skgRepos (nodeRepo main))) e
      **** (skg aliasFolder) aliases
      ***** (skg alias) sneaky alias
      "} );
  assert_eq!( entry (&collected, "e") . aliases, None ); }

#[test]
fn folders_under_toDelete_or_writeProtected_recorders_emit_nothing () {
  let collected : CollectedFieldIntents =
    collected_from_org ( indoc! {"
      * (skg node (gnid doomed) (skgRepos (nodeRepo main)) (editRequest delete)) doomed
      ** (skg aliasFolder) aliases
      *** (skg alias) dead alias
      ** (skg subscribeeFolder)
      *** (skg node (gnid s) (skgRepos (nodeRepo main)) writeProtected) s
      * (skg node (gnid ghost) (skgRepos (nodeRepo main)) writeProtected) ghost
      ** (skg aliasFolder) aliases
      *** (skg alias) ghost alias
      ** (skg overriddenFolder)
      *** (skg node (gnid o) (skgRepos (nodeRepo main)) writeProtected) o
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
      * (skg node (gnid recorder) (skgRepos (nodeRepo main))) recorder
      ** (skg subscriberFolder)
      *** (skg node (gnid intruder) (skgRepos (nodeRepo main))) intruder
      Intruder body
      **** (skg node (gnid intruder-child) (skgRepos (nodeRepo main))) intruder child
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
fn editable_child_of_tombstone_emits () {
  let collected : CollectedFieldIntents =
    collected_from_org ( indoc! {"
      * (skg node (gnid root) (skgRepos (nodeRepo main))) root
      ** (skg tombstone)
      *** (skg node (gnid stowaway) (skgRepos (nodeRepo main))) stowaway
      "} );
  { let stowaway : &FieldIntentsForOneId = entry (&collected, "stowaway");
    assert!( stowaway . title_and_body . is_some() ); }
  assert_eq!( entry (&collected, "root") . contains,
              Some (vec![]),
              "the tombstone is not content of its parent; where it \
               stands for a hidden node, the disk weave owns that \
               membership" ); }

#[test]
fn editable_node_inside_historical_phantom_subtree_emits () {
  let input : &str =
    indoc! {"
      * (skg node (gnid root) (skgRepos (nodeRepo main))) root
      ** (skg ghost (gnid fading) (skgRepos (nodeRepo main))) fading
      *** (skg node (gnid survivor) (skgRepos (nodeRepo main))) survivor
      "};
  let forest : ViewForest = forest_from_org (input);
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
      * (skg node (gnid subscriber) (skgRepos (nodeRepo main))) subscriber
      ** (skg subscribeeFolder)
      *** (skg node (gnid e) (skgRepos (nodeRepo main)) writeProtected) e
      **** (skg node (gnid under) (skgRepos (nodeRepo main)) writeProtected) under
      "} );
  assert!( collected . by_pid . get (&ID::from ("e")) . is_none() );
  assert_eq!( entry (&collected, "subscriber") . visibility, vec![] ); }

#[test]
fn editable_subscribee_under_writeProtected_subscriber_claims_without_visibility () {
  let collected : CollectedFieldIntents =
    collected_from_org ( indoc! {"
      * (skg node (gnid subscriber) (skgRepos (nodeRepo main)) writeProtected) subscriber
      ** (skg subscribeeFolder)
      *** (skg node (gnid e) (skgRepos (nodeRepo main))) e
      **** (skg node (gnid visible) (skgRepos (nodeRepo main))) visible
      "} );
  assert_eq!(
    entry (&collected, "e") . text_claims,
    vec![ SubscribeeTextClaim {
      title    : "e" . to_string(),
      body     : None,
      headline : HeadlineName::of ( None, Some (&ID::from ("e")), "e" ) } ]);
  assert!( collected . by_pid . get (&ID::from ("subscriber")) . is_none(),
           "no visibility intent reaches a write-protected subscriber" ); }

#[test]
fn present_but_empty_folders_differ_from_absent_folders () {
  let collected : CollectedFieldIntents =
    collected_from_org ( indoc! {"
      * (skg node (gnid explicit) (skgRepos (nodeRepo main))) explicit
      ** (skg aliasFolder) aliases
      ** (skg subscribeeFolder)
      ** (skg overriddenFolder)
      * (skg node (gnid silent) (skgRepos (nodeRepo main))) silent
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
      * (skg node (gnid recorder) (skgRepos (nodeRepo main))) recorder
      ** (skg aliasFolder) aliases
      *** (skg alias) echo
      *** (skg alias) other
      *** (skg alias) echo
      ** (skg subscribeeFolder)
      *** (skg node (gnid s1) (skgRepos (nodeRepo main)) writeProtected) s1
      *** (skg node (gnid s2) (skgRepos (nodeRepo main)) writeProtected) s2
      *** (skg node (gnid s1) (skgRepos (nodeRepo main)) writeProtected) s1
      ** (skg overriddenFolder)
      *** (skg node (gnid o1) (skgRepos (nodeRepo main)) writeProtected) o1
      *** (skg node (gnid o2) (skgRepos (nodeRepo main)) writeProtected) o2
      *** (skg node (gnid o1) (skgRepos (nodeRepo main)) writeProtected) o1
      "} );
  let recorder : &FieldIntentsForOneId = entry (&collected, "recorder");
  assert_eq!( recorder . aliases,
              Some (vec![("echo" . to_string(), None),
                          ("other" . to_string(), None)]) );
  assert_eq!( recorder . subscribesTo,
              Some (vec![(ID::from ("s1"), None), (ID::from ("s2"), None)]) );
  assert_eq!( recorder . overrides,
              Some (vec![(ID::from ("o1"), None), (ID::from ("o2"), None)]) ); }
