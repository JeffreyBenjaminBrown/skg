/// These tests pin the instructionMerge insert rules of
/// server/from_text/local_fieldintent_collection/types.rs.

use skg::from_text::local_fieldintent_collection::types::{
  CollectedFieldIntents, FieldIntentsForOneId, FieldIntent,
  SubscribeeTextClaim, SubscribeeVisibility };
use skg::types::errors::HeadlineName;
use skg::types::misc::{ID, SkgrepoName};
use skg::types::viewnode::ViewnodeId;
use skg::types::nodes::complete::Flag;

/// The occurrence emitting, for the tests that do not care which.
fn emitter (
) -> HeadlineName {
  HeadlineName::of ( Some (&ViewnodeId ("0a" . to_string ())), None, "" ) }

fn title_intent (
  title : &str,
) -> FieldIntent {
  FieldIntent::SetTitleAndBody {
    skgrepo : SkgrepoName::from ("main"),
    title   : title . to_string(),
    body    : None } }

fn delete_intent (
) -> FieldIntent {
  FieldIntent::Delete {
    skgrepo : SkgrepoName::from ("main") } }

#[test]
fn exclusive_slot_rules () {
  let mut acc : CollectedFieldIntents =
    CollectedFieldIntents::new();
  // An empty slot fills.
  acc . instructionMerge_fieldIntent (
    &emitter (), ID::from ("a"), title_intent ("t") ) . unwrap();
  // Re-inserting an equal payload is a silent no-op.
  acc . instructionMerge_fieldIntent (
    &emitter (), ID::from ("a"), title_intent ("t") ) . unwrap();
  // A differing payload errors.
  assert!( acc . instructionMerge_fieldIntent (
    &emitter (), ID::from ("a"), title_intent ("different") ) . is_err() );
  // Different variants for one ID coexist.
  acc . instructionMerge_fieldIntent (
    &emitter (), ID::from ("a"),
    FieldIntent::SetContains (vec![(ID::from ("c"), None)]) ) . unwrap();
  acc . instructionMerge_fieldIntent (
    &emitter (), ID::from ("a"),
    FieldIntent::SetAliases (vec![("x" . to_string(), None)]) ) . unwrap();
  acc . instructionMerge_fieldIntent (
    &emitter (), ID::from ("a"),
    FieldIntent::NodeMerge { acquiree : ID::from ("b") } ) . unwrap();
  let entry : &FieldIntentsForOneId =
    acc . by_pid . get (&ID::from ("a")) . unwrap();
  assert_eq!( entry . title_and_body,
              Some (("t" . to_string(), None)) );
  assert_eq!( entry . contains, Some (vec![(ID::from ("c"), None)]) );
  assert_eq!( entry . node_merge, Some (ID::from ("b")) ); }

#[test]
fn delete_excludes_other_exclusive_slots () {
  { // Delete after a Set* errors.
    let mut acc : CollectedFieldIntents =
      CollectedFieldIntents::new();
    acc . instructionMerge_fieldIntent (
      &emitter (), ID::from ("a"), title_intent ("t") ) . unwrap();
    assert!( acc . instructionMerge_fieldIntent (
      &emitter (), ID::from ("a"), delete_intent () ) . is_err() ); }
  { // A Set* after Delete errors.
    let mut acc : CollectedFieldIntents =
      CollectedFieldIntents::new();
    acc . instructionMerge_fieldIntent (
      &emitter (), ID::from ("a"), delete_intent () ) . unwrap();
    assert!( acc . instructionMerge_fieldIntent (
      &emitter (), ID::from ("a"), title_intent ("t") ) . is_err() );
    assert!( acc . instructionMerge_fieldIntent (
      &emitter (), ID::from ("a"),
      FieldIntent::NodeMerge { acquiree : ID::from ("b") }
    ) . is_err() ); }
  { // Delete plus Delete collapses silently.
    let mut acc : CollectedFieldIntents =
      CollectedFieldIntents::new();
    acc . instructionMerge_fieldIntent (
      &emitter (), ID::from ("a"), delete_intent () ) . unwrap();
    acc . instructionMerge_fieldIntent (
      &emitter (), ID::from ("a"), delete_intent () ) . unwrap();
    assert!( acc . by_pid . get (&ID::from ("a")) . unwrap()
             . delete ); }}

#[test]
fn flag_and_node_merge_are_mutually_exclusive () {
  let flag : FieldIntent =
    FieldIntent::SetFlag {
      flag : Flag::NoSearchMatching,
      value    : true };
  let node_merge : FieldIntent =
    FieldIntent::NodeMerge {
      acquiree : ID::from ("b") };
  for (first, second) in [
    (flag . clone(), node_merge . clone()),
    (node_merge . clone(), flag . clone()) ] {
    let mut acc : CollectedFieldIntents =
      CollectedFieldIntents::new();
    acc . instructionMerge_fieldIntent (
      &emitter (), ID::from ("a"), first ) . unwrap();
    let error : String = acc . instructionMerge_fieldIntent (
      &emitter (), ID::from ("a"), second ) . unwrap_err();
    assert!( error . contains (
      "Cannot combine nodeMerge and flag requests") ); }}

#[test]
fn combineable_intents_always_combine () {
  let mut acc : CollectedFieldIntents =
    CollectedFieldIntents::new();
  acc . instructionMerge_fieldIntent (
    &emitter (), ID::from ("subscriber"), delete_intent () ) . unwrap();
  // Visibility and text claims coexist with anything, even delete,
  // and several may accumulate per ID.
  for subscribee in ["e1", "e2", "e1"] {
    acc . instructionMerge_fieldIntent (
      &emitter (), ID::from ("subscriber"),
      FieldIntent::SubscribeeVisibility (
        SubscribeeVisibility {
          subscribee : ID::from (subscribee),
          visible    : vec![] } )) . unwrap(); }
  acc . instructionMerge_fieldIntent (
    &emitter (), ID::from ("subscriber"),
    FieldIntent::SubscribeeTextClaim (
      SubscribeeTextClaim {
        title    : "t" . to_string(),
        body     : None,
        headline : emitter () } )) . unwrap();
  let entry : &FieldIntentsForOneId =
    acc . by_pid . get (&ID::from ("subscriber")) . unwrap();
  assert_eq!( entry . visibility . len(), 3 );
  assert_eq!( entry . text_claims . len(), 1 ); }

#[test]
fn order_records_first_emission_per_skgid () {
  let mut acc : CollectedFieldIntents =
    CollectedFieldIntents::new();
  acc . instructionMerge_fieldIntent (
    &emitter (), ID::from ("b"), title_intent ("tb") ) . unwrap();
  acc . instructionMerge_fieldIntent (
    &emitter (), ID::from ("a"), title_intent ("ta") ) . unwrap();
  acc . instructionMerge_fieldIntent (
    &emitter (), ID::from ("b"),
    FieldIntent::SetContains (vec![]) ) . unwrap();
  assert_eq!( acc . order,
              vec![ID::from ("b"), ID::from ("a")] ); }

#[test]
fn a_conflict_names_the_occurrences_emitting_as_subheadlines () {
  let mut acc : CollectedFieldIntents =
    CollectedFieldIntents::new();
  let first : HeadlineName =
    HeadlineName::of ( Some (&ViewnodeId ("0a" . to_string ())), Some (&ID::from ("a")), "t" );
  let second : HeadlineName =
    HeadlineName::of ( Some (&ViewnodeId ("0c" . to_string ())), Some (&ID::from ("a")), "different" );
  acc . instructionMerge_fieldIntent (
    &first, ID::from ("a"), title_intent ("t") ) . unwrap();
  let error : String = acc . instructionMerge_fieldIntent (
    &second, ID::from ("a"), title_intent ("different") ) . unwrap_err();
  assert!( error . starts_with (
             "Conflicting title/body intents for ID a (should have been precluded" ),
           "{}", error );
  assert!( error . ends_with ( concat! (
             "\n*** t\n- graphnode ID: a\n- viewnode ID: 0a. To find it: C-c g v 0a\n",
             "*** different\n- graphnode ID: a\n- viewnode ID: 0c. To find it: C-c g v 0c\n" )),
           "each occurrence emitting is a subheadline: {}", error ); }
