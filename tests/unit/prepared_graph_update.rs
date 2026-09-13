use super::{GraphUpdatePreparationError, PreparedGraphUpdate,
            normalize_and_coalesce_definitions, prepare_graph_update,
            validate_identity_and_derive_changes};
use crate::dbs::in_rust_graph::complete_validation::{
  CompleteGraphError, validate_complete_graph_candidate,
};
use crate::dbs::in_rust_graph::{InRustGraph, InRustGraphHandle, new_handle};
use crate::types::misc::{ID, SkgConfig, SkgfileSource, SourceName};
use crate::types::nodes::complete::{NodeComplete, empty_node_complete};
use crate::types::save::{DefineNode, DeleteNode, SaveNode};

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use proptest::prelude::*;

fn config () -> SkgConfig {
  let source : SourceName = SourceName::from ("main");
  SkgConfig::dummyFromSources (HashMap::from ([
    (source . clone (), SkgfileSource {
      name         : source,
      abbreviation : None,
      path         : PathBuf::from ("unused"),
      user_owns_it : true,
    }),
  ]))
}

fn node (
  pid : &str,
) -> NodeComplete {
  let mut node : NodeComplete = empty_node_complete ();
  node . pid = ID::from (pid);
  node . title = pid . to_string ();
  node
}

#[test]
fn publication_uses_the_exact_prepared_candidate () {
  let base : Arc<InRustGraph> = Arc::new (
    InRustGraph::from_nodecompletes (&[node ("base")]));
  let graph : InRustGraphHandle = new_handle ((*base) . clone ());
  let actual_base : Arc<InRustGraph> = graph . load_full ();
  let prepared : PreparedGraphUpdate = prepare_graph_update (
    &config (), actual_base, vec![DefineNode::Save (SaveNode (node ("new")))])
    . unwrap ();
  let expected : Arc<InRustGraph> = prepared . candidate () . clone ();
  let (_published, _definitions) : (Arc<InRustGraph>, Vec<DefineNode>) =
    prepared . publish (&graph) . unwrap ();
  let visible : Arc<InRustGraph> = graph . load_full ();
  assert! (Arc::ptr_eq (&visible, &expected));
}

#[test]
fn a_prepared_update_refuses_a_different_base () {
  let graph : InRustGraphHandle = new_handle (
    InRustGraph::from_nodecompletes (&[node ("base")]));
  let base : Arc<InRustGraph> = graph . load_full ();
  let prepared : PreparedGraphUpdate = prepare_graph_update (
    &config (), base, vec![DefineNode::Save (SaveNode (node ("new")))])
    . unwrap ();
  let replacement : Arc<InRustGraph> = Arc::new (
    InRustGraph::from_nodecompletes (&[node ("replacement")]));
  graph . store (replacement . clone ());
  assert! (prepared . publish (&graph) . is_err ());
  assert! (Arc::ptr_eq (&graph . load_full (), &replacement));
}

#[test]
fn preparation_owns_normalized_definitions () {
  let graph : InRustGraphHandle = new_handle (
    InRustGraph::from_nodecompletes (&[node ("base")]));
  let mut saved : NodeComplete = node ("new");
  saved . extra_ids = vec![
    ID::from ("E"), ID::from ("new"), ID::from ("E")];
  let prepared : PreparedGraphUpdate = prepare_graph_update (
    &config (), graph . load_full (),
    vec![DefineNode::Save (SaveNode (saved))])
    . unwrap ();
  let DefineNode::Save (SaveNode (saved)) = &prepared . definitions () [0]
    else { panic! ("expected Save"); };
  assert_eq! (saved . extra_ids, vec![ID::from ("E")]);
}

#[test]
fn identity_overlay_rejects_untouched_primary_and_extra_owners () {
  let mut alias_owner : NodeComplete = node ("alias-owner");
  alias_owner . extra_ids = vec![ID::from ("owned-extra")];
  let base : Arc<InRustGraph> = Arc::new (InRustGraph::from_nodecompletes (&[
    node ("primary-owner"), alias_owner,
  ]));
  let mut claimant : NodeComplete = node ("claimant");
  claimant . extra_ids = vec![
    ID::from ("primary-owner"), ID::from ("owned-extra")];
  let error : GraphUpdatePreparationError = prepare_graph_update (
    &config (), base, vec![DefineNode::Save (SaveNode (claimant))])
    . expect_err ("both untouched claims must be protected");
  assert! (error . complete_graph_errors . iter () . any (|error| matches! (
    error,
    CompleteGraphError::PrimaryExtraCollision { id, .. }
      if id == &ID::from ("primary-owner"))));
  assert! (error . complete_graph_errors . iter () . any (|error| matches! (
    error,
    CompleteGraphError::DuplicateExtraId { id, .. }
      if id == &ID::from ("owned-extra"))));
}

#[test]
fn identity_overlay_rejects_two_saves_claiming_one_new_id () {
  let mut first : NodeComplete = node ("first");
  first . extra_ids = vec![ID::from ("shared")];
  let mut second : NodeComplete = node ("second");
  second . extra_ids = vec![ID::from ("shared")];
  let error : GraphUpdatePreparationError = prepare_graph_update (
    &config (), Arc::new (InRustGraph::new ()), vec![
      DefineNode::Save (SaveNode (first)),
      DefineNode::Save (SaveNode (second)),
    ]) . expect_err ("two final owners must conflict");
  assert! (matches! (
    error . complete_graph_errors . first (),
    Some (CompleteGraphError::DuplicateExtraId { id, owners })
      if id == &ID::from ("shared") && owners == &vec![
        ID::from ("first"), ID::from ("second")]
  ));
}

#[test]
fn simultaneous_overlay_accepts_merge_style_primary_transfer () {
  let base : Arc<InRustGraph> = Arc::new (InRustGraph::from_nodecompletes (&[
    node ("N1"), node ("N2"),
  ]));
  let mut acquirer : NodeComplete = node ("N1");
  acquirer . extra_ids = vec![ID::from ("N2")];
  let prepared : PreparedGraphUpdate = prepare_graph_update (
    &config (), base, vec![
      DefineNode::Save (SaveNode (acquirer)),
      DefineNode::Delete (DeleteNode {
        id     : ID::from ("N2"),
        source : SourceName::from ("main"),
      }),
    ]) . unwrap ();
  assert_eq! (
    prepared . candidate () . pid_of (&ID::from ("N2")),
    Some (ID::from ("N1")));
  assert! (prepared . changes . canonicalization_changes . iter () . any (
    |change| change . id == ID::from ("N2")
      && change . old_owner == Some (ID::from ("N2"))
      && change . new_owner == Some (ID::from ("N1"))));
}

#[test]
fn repeated_definitions_use_the_last_graph_state () {
  let mut first : NodeComplete = node ("P");
  first . title = "first" . to_string ();
  let mut last : NodeComplete = node ("P");
  last . title = "last" . to_string ();
  let prepared : PreparedGraphUpdate = prepare_graph_update (
    &config (), Arc::new (InRustGraph::new ()), vec![
      DefineNode::Save (SaveNode (first)),
      DefineNode::Delete (DeleteNode {
        id     : ID::from ("P"),
        source : SourceName::from ("main"),
      }),
      DefineNode::Save (SaveNode (last)),
    ]) . unwrap ();
  assert_eq! (prepared . definitions () . len (), 3);
  assert_eq! (
    prepared . candidate () . get (&ID::from ("P")) . unwrap () . title,
    "last");
}

#[test]
fn repeated_save_then_delete_leaves_no_graph_node () {
  let prepared : PreparedGraphUpdate = prepare_graph_update (
    &config (), Arc::new (InRustGraph::new ()), vec![
      DefineNode::Save (SaveNode (node ("P"))),
      DefineNode::Delete (DeleteNode {
        id     : ID::from ("P"),
        source : SourceName::from ("main"),
      }),
    ]) . unwrap ();
  assert! (prepared . candidate () . get (&ID::from ("P")) . is_none ());
}

#[test]
fn repeated_delete_then_save_leaves_the_saved_graph_node () {
  let base : Arc<InRustGraph> = Arc::new (
    InRustGraph::from_nodecompletes (&[node ("P")]));
  let mut saved : NodeComplete = node ("P");
  saved . title = "final" . to_string ();
  let prepared : PreparedGraphUpdate = prepare_graph_update (
    &config (), base, vec![
      DefineNode::Delete (DeleteNode {
        id     : ID::from ("P"),
        source : SourceName::from ("main"),
      }),
      DefineNode::Save (SaveNode (saved)),
    ]) . unwrap ();
  assert_eq! (
    prepared . candidate () . get (&ID::from ("P")) . unwrap () . title,
    "final");
}

#[test]
fn arbitrary_extra_id_revocation_is_actionably_rejected () {
  let mut old : NodeComplete = node ("P");
  old . extra_ids = vec![ID::from ("E2"), ID::from ("E1")];
  let base : Arc<InRustGraph> = Arc::new (
    InRustGraph::from_nodecompletes (&[old]));
  let mut saved : NodeComplete = node ("P");
  saved . title = "A titled node" . to_string ();
  let error : GraphUpdatePreparationError = prepare_graph_update (
    &config (), base, vec![DefineNode::Save (SaveNode (saved))])
    . expect_err ("managed Save must not revoke aliases");
  assert_eq! (error . extra_id_revocations . len (), 1);
  assert_eq! (
    error . extra_id_revocations [0] . dropped_ids,
    vec![ID::from ("E1"), ID::from ("E2")]);
  let message : String = error . to_string ();
  for expected in ["P", "A titled node", "E1", "E2", "another dataset"] {
    assert! (message . contains (expected), "missing {expected}: {message}"); }
}

#[test]
fn adding_an_extra_id_remains_valid () {
  let base : Arc<InRustGraph> = Arc::new (
    InRustGraph::from_nodecompletes (&[node ("P")]));
  let mut saved : NodeComplete = node ("P");
  saved . extra_ids = vec![ID::from ("E")];
  let prepared : PreparedGraphUpdate = prepare_graph_update (
    &config (), base, vec![DefineNode::Save (SaveNode (saved))])
    . unwrap ();
  assert_eq! (
    prepared . candidate () . pid_of (&ID::from ("E")),
    Some (ID::from ("P")));
}

#[test]
fn identity_lookup_work_does_not_grow_with_the_base_graph () {
  let definitions : Vec<DefineNode> = vec![DefineNode::Save (SaveNode ({
    let mut saved : NodeComplete = node ("new");
    saved . extra_ids = vec![ID::from ("new-extra")];
    saved
  }))];
  let small : PreparedGraphUpdate = prepare_graph_update (
    &config (), Arc::new (InRustGraph::from_nodecompletes (&[node ("old")])),
    definitions . clone ()) . unwrap ();
  let many : Vec<NodeComplete> = (0..1_000)
    . map (|i| node (&format! ("old-{i}")))
    . collect ();
  let large : PreparedGraphUpdate = prepare_graph_update (
    &config (), Arc::new (InRustGraph::from_nodecompletes (&many)), definitions)
    . unwrap ();
  assert_eq! (
    small . changes . identity_base_lookup_bound,
    large . changes . identity_base_lookup_bound);
}

proptest! {
  #![proptest_config (ProptestConfig::with_cases (256))]

  #[test]
  fn identity_overlay_matches_the_full_oracle (
    extra_indexes in proptest::collection::vec (
      proptest::collection::vec (0usize..7, 0..5), 0..3),
  ) {
    let base : InRustGraph = InRustGraph::from_nodecompletes (&[
      node ("P0"), node ("P1"), node ("P2"),
    ]);
    let id_universe : [&str; 7] = [
      "P0", "P1", "P2", "N0", "N1", "X0", "X1"];
    let definitions : Vec<DefineNode> = extra_indexes . into_iter ()
      . enumerate ()
      . map (|(owner_index, indexes)| {
        let mut saved : NodeComplete = node (&format! ("N{owner_index}"));
        saved . extra_ids = indexes . into_iter ()
          . map (|index| ID::from (id_universe [index]))
          . collect ();
        DefineNode::Save (SaveNode (saved))
      })
      . collect ();
    let batch = normalize_and_coalesce_definitions (definitions);
    let (_, local_errors, revocations) =
      validate_identity_and_derive_changes (
        &config (), &base, &batch . final_graph_definitions);
    prop_assert! (revocations . is_empty ());
    let full = validate_complete_graph_candidate (
      &config (), &base, &batch . final_graph_definitions);
    let mut local : Vec<String> = local_errors . iter ()
      . map (|error| format! ("{error:?}"))
      . collect ();
    let mut oracle : Vec<String> = full . errors . iter ()
      . filter (|error| matches! (
        error,
        CompleteGraphError::DuplicatePrimaryId { .. }
        | CompleteGraphError::DuplicateExtraId { .. }
        | CompleteGraphError::PrimaryExtraCollision { .. }
        | CompleteGraphError::UnconfiguredNodeHome { .. }))
      . map (|error| format! ("{error:?}"))
      . collect ();
    local . sort ();
    oracle . sort ();
    prop_assert_eq! (local, oracle);
  }
}
