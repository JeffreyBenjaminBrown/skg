/// These are unit tests for the local-fieldintent-collection
/// traversal
/// (server/from_text/local_fieldintent_collection/traverse.rs).
/// The traversal is pure and synchronous, so these tests need no db.

use crate::parse_helpers::viewforest_as_written;
use indoc::indoc;
use skg::from_text::local_fieldintent_collection::traverse::collect_instructions_locally;
use skg::types::errors::HeadlineName;
use skg::from_text::local_fieldintent_collection::types::{
  CollectedFieldIntents, FieldIntentsForOneId, MemberAsSuchTextClaim,
  ForkeeVisibility, ForkerVisibility };
use skg::types::misc::{ID, SkgrepoName};
use skg::types::nodes::complete::Flag;
use skg::types::tree::forest::ViewForest;

fn collected_from_org (
  input : &str,
) -> CollectedFieldIntents {
  collect_instructions_locally (
    &viewforest_as_written (input), &|_| true ) . unwrap() }

/// As 'collected_from_org', where the user owns only 'main'.
fn collected_from_org_owning_main (
  input : &str,
) -> CollectedFieldIntents {
  collect_instructions_locally (
    &viewforest_as_written (input),
    &|skgrepo| *skgrepo == SkgrepoName::from ("main") ) . unwrap() }

fn entry<'a> (
  collected : &'a CollectedFieldIntents,
  gnid        : &str,
) -> &'a FieldIntentsForOneId {
  collected . by_pid . get (&ID::from (gnid))
    . unwrap_or_else ( || panic! ("no entry for {}", gnid) ) }

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
      ** (skg forksFolder)
      *** (skg node (gnid s) (skgRepos (nodeRepo main)) writeProtected) s
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
    assert_eq!( root . forks, Some (vec![(ID::from ("s"), None)]) );
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
      ** (skg forksFolder)
      *** (skg node (gnid s) (skgRepos (nodeRepo main)) writeProtected (editRequest (relRepo private))) s
      "} );
  let root : &FieldIntentsForOneId = entry (&collected, "root");
  let private : Option<SkgrepoName> = Some (SkgrepoName::from ("private"));
  assert_eq!( root . contains,
              Some (vec![(ID::from ("c"), private . clone ())]) );
  assert_eq!( root . forks,
              Some (vec![(ID::from ("s"), private)]) ); }

#[test]
fn forkee_as_such_emits_claim_and_visibility () {
  let collected : CollectedFieldIntents =
    collected_from_org ( indoc! {"
      * (skg node (gnid forker) (skgRepos (nodeRepo main))) forker
      ** (skg forksFolder)
      *** (skg node (gnid e) (skgRepos (nodeRepo main))) e
      Forkee body
      **** (skg node (gnid visible) (skgRepos (nodeRepo main))) visible
      **** (skg node (gnid hidden) (skgRepos (nodeRepo main)) noClaim) hidden
      **** (skg node (gnid leaving) (skgRepos (nodeRepo main)) (editRequest delete)) leaving
      "} );
  { let e : &FieldIntentsForOneId = entry (&collected, "e");
    // An owned forkee as such saves its title and body, and no
    // other Set* intent: its contains is not read from the buffer.
    assert_eq!( e . title_and_body,
                Some (( "e" . to_string (),
                        Some ("Forkee body" . to_string ()) )) );
    assert_eq!( e . contains, None );
    assert!( e . text_claims . is_empty () ); }
  { let forker : &FieldIntentsForOneId = entry (&collected, "forker");
    assert_eq!(
      forker . forkee_visibility,
      vec![ ForkeeVisibility {
        forkee     : ID::from ("e"),
        visible    : vec![ID::from ("visible")] } ]);
    assert_eq!( forker . forks,
                Some (vec![(ID::from ("e"), None)]) ); }
  { // Ordinary editable children of a forkee-as-such still
    // emit their own instructions, but form no one's contains.
    let visible : &FieldIntentsForOneId = entry (&collected, "visible");
    assert!( visible . title_and_body . is_some() ); }}

/// A foreign forkee as such emits no Set* intents, only a text
/// claim, which validation checks against the disk.
#[test]
fn foreign_forkee_as_such_emits_a_text_claim () {
  let collected : CollectedFieldIntents =
    collected_from_org_owning_main ( indoc! {"
      * (skg node (gnid forker) (skgRepos (nodeRepo main))) forker
      ** (skg forksFolder)
      *** (skg node (gnid e) (skgRepos (nodeRepo theirs))) e
      Forkee body
      **** (skg node (gnid visible) (skgRepos (nodeRepo theirs))) visible
      "} );
  let e : &FieldIntentsForOneId = entry (&collected, "e");
  assert_eq!( e . title_and_body, None );
  assert_eq!( e . contains, None );
  assert_eq!(
    e . text_claims,
    vec![ MemberAsSuchTextClaim {
      title    : "e" . to_string(),
      body     : Some ("Forkee body" . to_string()),
      headline : HeadlineName::of ( None, Some (&ID::from ("e")), "e" ) } ]);
  assert_eq!(
    entry (&collected, "forker") . forkee_visibility,
    vec![ ForkeeVisibility {
      forkee     : ID::from ("e"),
      visible    : vec![ID::from ("visible")] } ]); }

#[test]
fn aliasfolder_under_forkee_as_such_emits_nothing () {
  // The trap: a naive implementation
  // would write the forkee's aliases.
  let collected : CollectedFieldIntents =
    collected_from_org ( indoc! {"
      * (skg node (gnid forker) (skgRepos (nodeRepo main))) forker
      ** (skg forksFolder)
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
      ** (skg forksFolder)
      *** (skg node (gnid s) (skgRepos (nodeRepo main)) writeProtected) s
      * (skg node (gnid ghost) (skgRepos (nodeRepo main)) writeProtected) ghost
      ** (skg aliasFolder) aliases
      *** (skg alias) ghost alias
      "} );
  { let doomed : &FieldIntentsForOneId = entry (&collected, "doomed");
    assert!( doomed . delete );
    assert_eq!( doomed . aliases, None );
    assert_eq!( doomed . forks, None ); }
  assert!( collected . by_pid . get (&ID::from ("ghost")) . is_none(),
           "a write-protected vognode and its folders emit nothing" ); }

/// An editable member of the forks folder claiming nothing (a forker
/// as such) shows its content in part, less what the recorder
/// contains or hides: it saves its title and body when owned, never
/// its contains, and the recorder gets the forker-visibility signal of
/// what it shows. It is no fork of the recorder.
#[test]
fn forker_as_such_emits_its_text_and_visibility_but_no_contains () {
  let collected : CollectedFieldIntents =
    collected_from_org ( indoc! {"
      * (skg node (gnid recorder) (skgRepos (nodeRepo main))) recorder
      ** (skg forksFolder)
      *** (skg node (gnid forker) (skgRepos (nodeRepo main)) noClaim) forker
      Forker body
      **** (skg node (gnid forker-child) (skgRepos (nodeRepo main))) forker child
      **** (skg node (gnid placed) (skgRepos (nodeRepo main)) noClaim) placed
      "} );
  { let forker : &FieldIntentsForOneId = entry (&collected, "forker");
    assert_eq!( forker . title_and_body,
                Some (( "forker" . to_string(),
                        Some ("Forker body" . to_string()) )) );
    assert_eq!( forker . contains, None,
                "its partial list is never read as its contains" );
    assert!( forker . text_claims . is_empty () ); }
  { let recorder : &FieldIntentsForOneId = entry (&collected, "recorder");
    assert_eq!( recorder . contains, Some (vec![]) );
    assert_eq!( recorder . forks, Some (vec![]) );
    assert_eq!( recorder . forker_visibility,
                vec![ ForkerVisibility {
                  forker_as_such : ID::from ("forker"),
                  visible : vec![ID::from ("forker-child")] } ]);
    assert!( recorder . forkee_visibility . is_empty () ); }
  { // Its editable children emit their own instructions.
    let child : &FieldIntentsForOneId = entry (&collected, "forker-child");
    assert!( child . title_and_body . is_some() ); }}

/// A foreign forker as such emits only a text claim; under a
/// write-protected recorder, no visibility signal.
#[test]
fn foreign_forker_as_such_emits_a_text_claim () {
  let collected : CollectedFieldIntents =
    collected_from_org_owning_main ( indoc! {"
      * (skg node (gnid recorder) (skgRepos (nodeRepo main)) writeProtected) recorder
      ** (skg forksFolder)
      *** (skg node (gnid forker) (skgRepos (nodeRepo theirs)) noClaim) forker
      **** (skg node (gnid forker-child) (skgRepos (nodeRepo theirs))) forker child
      "} );
  let forker : &FieldIntentsForOneId = entry (&collected, "forker");
  assert_eq!( forker . title_and_body, None );
  assert_eq!( forker . contains, None );
  assert_eq!(
    forker . text_claims,
    vec![ MemberAsSuchTextClaim {
      title    : "forker" . to_string(),
      body     : None,
      headline : HeadlineName::of ( None, Some (&ID::from ("forker")), "forker" ) } ]);
  assert!( collected . by_pid . get (&ID::from ("recorder")) . is_none (),
           "a write-protected recorder receives no signal" ); }

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
               stands for a restricted node, the disk weave owns that \
               membership" ); }

#[test]
fn editable_node_inside_ghost_subtree_emits () {
  let input : &str =
    indoc! {"
      * (skg node (gnid root) (skgRepos (nodeRepo main))) root
      ** (skg ghost (gnid fading) (skgRepos (nodeRepo main)) noClaim) fading
      *** (skg node (gnid underneath) (skgRepos (nodeRepo main))) underneath
      "};
  let forest : ViewForest = viewforest_as_written (input);
  let collected : CollectedFieldIntents =
    collect_instructions_locally (&forest, &|_| true) . unwrap();
  assert!( collected . by_pid . get (&ID::from ("fading")) . is_none(),
           "the ghost itself emits nothing" );
  { let underneath : &FieldIntentsForOneId = entry (&collected, "underneath");
    assert!( underneath . title_and_body . is_some(),
             "an editable node beneath a ghost emits for itself" ); }
  assert_eq!( entry (&collected, "root") . contains, Some (vec![]),
              "the ghost is not content of its parent" ); }

#[test]
fn a_ghost_that_claims_keeps_its_gnid_in_its_parents_list () {
  // A ghost its parent's list still names (a dangling reference to a
  // node HEAD has) claims membership: the save keeps its gnid there,
  // as it keeps an unknown's. One that claims nothing contributes
  // nothing.
  let input : &str =
    indoc! {"
      * (skg node (gnid root) (skgRepos (nodeRepo main))) root
      ** (skg node (gnid before) (skgRepos (nodeRepo main)) writeProtected) before
      ** (skg ghost (gnid named) (skgRepos (nodeRepo main))) named
      ** (skg ghost (gnid dropped) (skgRepos (nodeRepo main)) noClaim) dropped
      ** (skg node (gnid after) (skgRepos (nodeRepo main)) writeProtected) after
      "};
  let forest : ViewForest = viewforest_as_written (input);
  let collected : CollectedFieldIntents =
    collect_instructions_locally (&forest, &|_| true) . unwrap();
  assert!( collected . by_pid . get (&ID::from ("named")) . is_none(),
           "the ghost itself emits nothing" );
  assert_eq!( entry (&collected, "root") . contains,
              Some ( [ "before", "named", "after" ] . iter ()
                     . map ( |gnid| (ID::from (*gnid), None) ) . collect () )); }

#[test]
fn writeProtected_forkee_as_such_emits_nothing () {
  let collected : CollectedFieldIntents =
    collected_from_org ( indoc! {"
      * (skg node (gnid forker) (skgRepos (nodeRepo main))) forker
      ** (skg forksFolder)
      *** (skg node (gnid e) (skgRepos (nodeRepo main)) writeProtected) e
      **** (skg node (gnid under) (skgRepos (nodeRepo main)) writeProtected) under
      "} );
  assert!( collected . by_pid . get (&ID::from ("e")) . is_none() );
  assert_eq!( entry (&collected, "forker") . forkee_visibility, vec![] ); }

#[test]
fn editable_forkee_under_writeProtected_forker_claims_without_visibility () {
  let collected : CollectedFieldIntents =
    collected_from_org_owning_main ( indoc! {"
      * (skg node (gnid forker) (skgRepos (nodeRepo main)) writeProtected) forker
      ** (skg forksFolder)
      *** (skg node (gnid e) (skgRepos (nodeRepo theirs))) e
      **** (skg node (gnid visible) (skgRepos (nodeRepo theirs))) visible
      "} );
  assert_eq!(
    entry (&collected, "e") . text_claims,
    vec![ MemberAsSuchTextClaim {
      title    : "e" . to_string(),
      body     : None,
      headline : HeadlineName::of ( None, Some (&ID::from ("e")), "e" ) } ]);
  assert!( collected . by_pid . get (&ID::from ("forker")) . is_none(),
           "no visibility intent reaches a write-protected forker" ); }

#[test]
fn present_but_empty_folders_differ_from_absent_folders () {
  let collected : CollectedFieldIntents =
    collected_from_org ( indoc! {"
      * (skg node (gnid explicit) (skgRepos (nodeRepo main))) explicit
      ** (skg aliasFolder) aliases
      ** (skg forksFolder)
      * (skg node (gnid silent) (skgRepos (nodeRepo main))) silent
      "} );
  { let explicit : &FieldIntentsForOneId = entry (&collected, "explicit");
    // A present-but-empty folder is an explicitly empty field.
    assert_eq!( explicit . aliases, Some (vec![]) );
    assert_eq!( explicit . forks, Some (vec![]) ); }
  { let silent : &FieldIntentsForOneId = entry (&collected, "silent");
    // An absent folder expresses no opinion.
    assert_eq!( silent . aliases, None );
    assert_eq!( silent . forks, None ); }}

#[test]
fn duplicate_defining_folder_members_dedup_preserving_order () {
  let collected : CollectedFieldIntents =
    collected_from_org ( indoc! {"
      * (skg node (gnid recorder) (skgRepos (nodeRepo main))) recorder
      ** (skg aliasFolder) aliases
      *** (skg alias) echo
      *** (skg alias) other
      *** (skg alias) echo
      ** (skg forksFolder)
      *** (skg node (gnid s1) (skgRepos (nodeRepo main)) writeProtected) s1
      *** (skg node (gnid s2) (skgRepos (nodeRepo main)) writeProtected) s2
      *** (skg node (gnid s1) (skgRepos (nodeRepo main)) writeProtected) s1
      "} );
  let recorder : &FieldIntentsForOneId = entry (&collected, "recorder");
  assert_eq!( recorder . aliases,
              Some (vec![("echo" . to_string(), None),
                          ("other" . to_string(), None)]) );
  assert_eq!( recorder . forks,
              Some (vec![(ID::from ("s1"), None), (ID::from ("s2"), None)]) ); }
