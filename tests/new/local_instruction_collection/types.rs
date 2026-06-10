/// These tests pin the instructionMerge insert rules of
/// server/from_text/local_instruction_collection/types.rs
/// (TODO/local-instruction-collection/3_plan.org).

use skg::from_text::local_instruction_collection::types::{
  CollectedIntents, IntentsForOneId, NodeIntent_Local,
  SubscribeeTextClaim, SubscribeeVisibility };
use skg::types::misc::{ID, SourceName};

fn title_intent (
  title : &str,
) -> NodeIntent_Local {
  NodeIntent_Local::SetTitleAndBody {
    source : SourceName::from ("main"),
    title  : title . to_string(),
    body   : None } }

fn delete_intent (
) -> NodeIntent_Local {
  NodeIntent_Local::Delete {
    source : SourceName::from ("main") } }

#[test]
fn exclusive_slot_rules () {
  let mut acc : CollectedIntents =
    CollectedIntents::new();
  // An empty slot fills.
  acc . instructionMerge_intent (
    ID::from ("a"), title_intent ("t") ) . unwrap();
  // Re-inserting an equal payload is a silent no-op.
  acc . instructionMerge_intent (
    ID::from ("a"), title_intent ("t") ) . unwrap();
  // A differing payload errors.
  assert!( acc . instructionMerge_intent (
    ID::from ("a"), title_intent ("different") ) . is_err() );
  // Different variants for one ID coexist.
  acc . instructionMerge_intent (
    ID::from ("a"),
    NodeIntent_Local::SetContains (vec![ID::from ("c")]) ) . unwrap();
  acc . instructionMerge_intent (
    ID::from ("a"),
    NodeIntent_Local::SetAliases (vec!["x" . to_string()]) ) . unwrap();
  acc . instructionMerge_intent (
    ID::from ("a"),
    NodeIntent_Local::NodeMerge { acquiree : ID::from ("b") } ) . unwrap();
  let entry : &IntentsForOneId =
    acc . by_pid . get (&ID::from ("a")) . unwrap();
  assert_eq!( entry . title_and_body,
              Some (("t" . to_string(), None)) );
  assert_eq!( entry . contains, Some (vec![ID::from ("c")]) );
  assert_eq!( entry . node_merge, Some (ID::from ("b")) ); }

#[test]
fn delete_excludes_other_exclusive_slots () {
  { // Delete after a Set* errors.
    let mut acc : CollectedIntents =
      CollectedIntents::new();
    acc . instructionMerge_intent (
      ID::from ("a"), title_intent ("t") ) . unwrap();
    assert!( acc . instructionMerge_intent (
      ID::from ("a"), delete_intent () ) . is_err() ); }
  { // A Set* after Delete errors.
    let mut acc : CollectedIntents =
      CollectedIntents::new();
    acc . instructionMerge_intent (
      ID::from ("a"), delete_intent () ) . unwrap();
    assert!( acc . instructionMerge_intent (
      ID::from ("a"), title_intent ("t") ) . is_err() );
    assert!( acc . instructionMerge_intent (
      ID::from ("a"),
      NodeIntent_Local::NodeMerge { acquiree : ID::from ("b") }
    ) . is_err() ); }
  { // Delete plus Delete collapses silently.
    let mut acc : CollectedIntents =
      CollectedIntents::new();
    acc . instructionMerge_intent (
      ID::from ("a"), delete_intent () ) . unwrap();
    acc . instructionMerge_intent (
      ID::from ("a"), delete_intent () ) . unwrap();
    assert!( acc . by_pid . get (&ID::from ("a")) . unwrap()
             . delete ); }}

#[test]
fn combineable_intents_always_combine () {
  let mut acc : CollectedIntents =
    CollectedIntents::new();
  acc . instructionMerge_intent (
    ID::from ("subscriber"), delete_intent () ) . unwrap();
  // Visibility and text claims coexist with anything, even delete,
  // and several may accumulate per ID.
  for subscribee in ["e1", "e2", "e1"] {
    acc . instructionMerge_intent (
      ID::from ("subscriber"),
      NodeIntent_Local::SubscribeeVisibility (
        SubscribeeVisibility {
          subscribee : ID::from (subscribee),
          visible    : vec![] } )) . unwrap(); }
  acc . instructionMerge_intent (
    ID::from ("subscriber"),
    NodeIntent_Local::SubscribeeTextClaim (
      SubscribeeTextClaim {
        title : "t" . to_string(),
        body  : None } )) . unwrap();
  let entry : &IntentsForOneId =
    acc . by_pid . get (&ID::from ("subscriber")) . unwrap();
  assert_eq!( entry . visibility . len(), 3 );
  assert_eq!( entry . text_claims . len(), 1 ); }

#[test]
fn order_records_first_emission_per_id () {
  let mut acc : CollectedIntents =
    CollectedIntents::new();
  acc . instructionMerge_intent (
    ID::from ("b"), title_intent ("tb") ) . unwrap();
  acc . instructionMerge_intent (
    ID::from ("a"), title_intent ("ta") ) . unwrap();
  acc . instructionMerge_intent (
    ID::from ("b"),
    NodeIntent_Local::SetContains (vec![]) ) . unwrap();
  assert_eq!( acc . order,
              vec![ID::from ("b"), ID::from ("a")] ); }
