use super::{GraphChangeSet, GraphUpdatePreparationError, PreparedGraphUpdate,
            normalize_and_coalesce_nodeInstructions, prepare_graph_update,
            validate_identity_and_derive_changes};
use crate::dbs::in_rust_graph::complete_validation::{
  CompleteGraphError, validate_complete_graph_candidate,
};
use crate::dbs::in_rust_graph::{InRustGraph, InRustGraphHandle, new_handle};
use crate::dbs::in_rust_graph::apply_nodeInstructions_to_inRustGraph;
use crate::dbs::in_rust_graph::internal_index_validation::{
  LocalIndexValidation, validate_local_internal_indexes,
};
use crate::types::misc::{
  ID, MSV, SkgConfig, SkgRepo, SkgRepoName, rel_partners_at_relRepo,
};
use crate::types::nodes::complete::{Graphnode, empty_graphnode};
use crate::types::save::{NodeInstruction, DeleteNode, SaveNode};

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use proptest::prelude::*;

fn config () -> SkgConfig {
  let skgrepo : SkgRepoName = SkgRepoName::from ("main");
  SkgConfig::dummyFromSkgRepos (HashMap::from ([
    (skgrepo . clone (), SkgRepo {
      name         : skgrepo,
      abbreviation : None,
      path         : PathBuf::from ("unused"),
      owned        : true,
    }),
  ]))
}

fn node (
  pid : &str,
) -> Graphnode {
  let mut node : Graphnode = empty_graphnode ();
  node . pid = ID::from (pid);
  node . title = pid . to_string ();
  node
}

fn changed_relationship_fixture (
  unrelated_count : usize,
) -> (InRustGraph, InRustGraph, Vec<NodeInstruction>, GraphChangeSet) {
  let mut old_recorder : Graphnode = node ("recorder");
  old_recorder . contains = rel_partners_at_relRepo (
    &SkgRepoName::from ("main"), vec![ID::from ("old")]);
  let mut base_nodes : Vec<Graphnode> = vec![old_recorder];
  base_nodes . extend ((0..unrelated_count)
    . map (|i| node (&format! ("unrelated-{i}"))));
  let base : InRustGraph = InRustGraph::from_graphnodes (&base_nodes);
  let mut final_recorder : Graphnode = node ("recorder");
  final_recorder . contains = rel_partners_at_relRepo (
    &SkgRepoName::from ("main"), vec![ID::from ("new")]);
  let nodeInstructions : Vec<NodeInstruction> =
    vec![NodeInstruction::Save (SaveNode (final_recorder))];
  let (changes, errors, revocations)
    : (GraphChangeSet, Vec<CompleteGraphError>, Vec<super::ExtraIdRevocation>) =
    validate_identity_and_derive_changes (&config (), &base, &nodeInstructions);
  assert! (errors . is_empty ());
  assert! (revocations . is_empty ());
  let mut candidate : InRustGraph = base . clone ();
  apply_nodeInstructions_to_inRustGraph (&mut candidate, &nodeInstructions);
  (base, candidate, nodeInstructions, changes)
}

fn add_membership (
  index    : &mut im::HashMap<ID, im::HashSet<ID>>,
  key      : &str,
  recorder : &str,
) {
  let mut recorders : im::HashSet<ID> = index . get (&ID::from (key))
    . cloned () . unwrap_or_default ();
  recorders . insert (ID::from (recorder));
  index . insert (ID::from (key), recorders);
}

#[test]
fn publication_uses_the_exact_prepared_candidate () {
  let base : Arc<InRustGraph> = Arc::new (
    InRustGraph::from_graphnodes (&[node ("base")]));
  let graph : InRustGraphHandle = new_handle ((*base) . clone ());
  let actual_base : Arc<InRustGraph> = graph . load_full ();
  let prepared : PreparedGraphUpdate = prepare_graph_update (
    &config (), actual_base, vec![NodeInstruction::Save (SaveNode (node ("new")))])
    . unwrap ();
  let expected : Arc<InRustGraph> = prepared . candidate () . clone ();
  let (_published, _nodeInstructions) : (Arc<InRustGraph>, Vec<NodeInstruction>) =
    prepared . swap_in (&graph) . unwrap ();
  let visible : Arc<InRustGraph> = graph . load_full ();
  assert! (Arc::ptr_eq (&visible, &expected));
}

#[test]
fn a_prepared_update_refuses_a_different_base () {
  let graph : InRustGraphHandle = new_handle (
    InRustGraph::from_graphnodes (&[node ("base")]));
  let base : Arc<InRustGraph> = graph . load_full ();
  let prepared : PreparedGraphUpdate = prepare_graph_update (
    &config (), base, vec![NodeInstruction::Save (SaveNode (node ("new")))])
    . unwrap ();
  let replacement : Arc<InRustGraph> = Arc::new (
    InRustGraph::from_graphnodes (&[node ("replacement")]));
  graph . store (replacement . clone ());
  assert! (prepared . swap_in (&graph) . is_err ());
  assert! (Arc::ptr_eq (&graph . load_full (), &replacement));
}

#[test]
fn preparation_owns_normalized_nodeInstructions () {
  let graph : InRustGraphHandle = new_handle (
    InRustGraph::from_graphnodes (&[node ("base")]));
  let mut saved : Graphnode = node ("new");
  saved . extra_ids = vec![
    ID::from ("E"), ID::from ("new"), ID::from ("E")];
  let prepared : PreparedGraphUpdate = prepare_graph_update (
    &config (), graph . load_full (),
    vec![NodeInstruction::Save (SaveNode (saved))])
    . unwrap ();
  let NodeInstruction::Save (SaveNode (saved)) = &prepared . nodeInstructions () [0]
    else { panic! ("expected Save"); };
  assert_eq! (saved . extra_ids, vec![ID::from ("E")]);
}

#[test]
fn identity_overlay_rejects_untouched_primary_and_extra_carriers () {
  let mut alias_carrier : Graphnode = node ("alias-carrier");
  alias_carrier . extra_ids = vec![ID::from ("owned-extra")];
  let base : Arc<InRustGraph> = Arc::new (InRustGraph::from_graphnodes (&[
    node ("primary-carrier"), alias_carrier,
  ]));
  let mut claimant : Graphnode = node ("claimant");
  claimant . extra_ids = vec![
    ID::from ("primary-carrier"), ID::from ("owned-extra")];
  let error : GraphUpdatePreparationError = prepare_graph_update (
    &config (), base, vec![NodeInstruction::Save (SaveNode (claimant))])
    . expect_err ("both untouched claims must be protected");
  assert! (error . complete_graph_errors . iter () . any (|error| matches! (
    error,
    CompleteGraphError::PrimaryExtraCollision { skgid, .. }
      if skgid == &ID::from ("primary-carrier"))));
  assert! (error . complete_graph_errors . iter () . any (|error| matches! (
    error,
    CompleteGraphError::DuplicateExtraId { skgid, .. }
      if skgid == &ID::from ("owned-extra"))));
}

#[test]
fn identity_overlay_rejects_two_saves_claiming_one_new_skgid () {
  let mut first : Graphnode = node ("first");
  first . extra_ids = vec![ID::from ("shared")];
  let mut second : Graphnode = node ("second");
  second . extra_ids = vec![ID::from ("shared")];
  let error : GraphUpdatePreparationError = prepare_graph_update (
    &config (), Arc::new (InRustGraph::new ()), vec![
      NodeInstruction::Save (SaveNode (first)),
      NodeInstruction::Save (SaveNode (second)),
    ]) . expect_err ("two final carriers must conflict");
  assert! (matches! (
    error . complete_graph_errors . first (),
    Some (CompleteGraphError::DuplicateExtraId { skgid, carriers })
      if skgid == &ID::from ("shared") && carriers == &vec![
        ID::from ("first"), ID::from ("second")]
  ));
}

#[test]
fn simultaneous_overlay_accepts_merge_style_primary_transfer () {
  let base : Arc<InRustGraph> = Arc::new (InRustGraph::from_graphnodes (&[
    node ("N1"), node ("N2"),
  ]));
  let mut acquirer : Graphnode = node ("N1");
  acquirer . extra_ids = vec![ID::from ("N2")];
  let prepared : PreparedGraphUpdate = prepare_graph_update (
    &config (), base, vec![
      NodeInstruction::Save (SaveNode (acquirer)),
      NodeInstruction::Delete (DeleteNode {
        skgid           : ID::from ("N2"),
        home_skgrepo : SkgRepoName::from ("main"),
      }),
    ]) . unwrap ();
  assert_eq! (
    prepared . candidate () . pid_of (&ID::from ("N2")),
    Some (ID::from ("N1")));
  assert! (prepared . changes . canonicalization_changes . iter () . any (
    |change| change . skgid == ID::from ("N2")
      && change . old_carrier == Some (ID::from ("N2"))
      && change . new_carrier == Some (ID::from ("N1"))));
}

#[test]
fn repeated_definitions_use_the_last_graph_state () {
  let mut first : Graphnode = node ("P");
  first . title = "first" . to_string ();
  let mut last : Graphnode = node ("P");
  last . title = "last" . to_string ();
  let prepared : PreparedGraphUpdate = prepare_graph_update (
    &config (), Arc::new (InRustGraph::new ()), vec![
      NodeInstruction::Save (SaveNode (first)),
      NodeInstruction::Delete (DeleteNode {
        skgid           : ID::from ("P"),
        home_skgrepo : SkgRepoName::from ("main"),
      }),
      NodeInstruction::Save (SaveNode (last)),
    ]) . unwrap ();
  assert_eq! (prepared . nodeInstructions () . len (), 3);
  assert_eq! (
    prepared . candidate () . get (&ID::from ("P")) . unwrap () . title,
    "last");
}

#[test]
fn repeated_save_then_delete_leaves_no_graph_node () {
  let prepared : PreparedGraphUpdate = prepare_graph_update (
    &config (), Arc::new (InRustGraph::new ()), vec![
      NodeInstruction::Save (SaveNode (node ("P"))),
      NodeInstruction::Delete (DeleteNode {
        skgid           : ID::from ("P"),
        home_skgrepo : SkgRepoName::from ("main"),
      }),
    ]) . unwrap ();
  assert! (prepared . candidate () . get (&ID::from ("P")) . is_none ());
}

#[test]
fn repeated_delete_then_save_leaves_the_saved_graph_node () {
  let base : Arc<InRustGraph> = Arc::new (
    InRustGraph::from_graphnodes (&[node ("P")]));
  let mut saved : Graphnode = node ("P");
  saved . title = "final" . to_string ();
  let prepared : PreparedGraphUpdate = prepare_graph_update (
    &config (), base, vec![
      NodeInstruction::Delete (DeleteNode {
        skgid           : ID::from ("P"),
        home_skgrepo : SkgRepoName::from ("main"),
      }),
      NodeInstruction::Save (SaveNode (saved)),
    ]) . unwrap ();
  assert_eq! (
    prepared . candidate () . get (&ID::from ("P")) . unwrap () . title,
    "final");
}

#[test]
fn arbitrary_extra_id_revocation_is_actionably_rejected () {
  let mut old : Graphnode = node ("P");
  old . extra_ids = vec![ID::from ("E2"), ID::from ("E1")];
  let base : Arc<InRustGraph> = Arc::new (
    InRustGraph::from_graphnodes (&[old]));
  let mut saved : Graphnode = node ("P");
  saved . title = "A titled node" . to_string ();
  let error : GraphUpdatePreparationError = prepare_graph_update (
    &config (), base, vec![NodeInstruction::Save (SaveNode (saved))])
    . expect_err ("managed Save must not revoke aliases");
  assert_eq! (error . extra_id_revocations . len (), 1);
  assert_eq! (
    error . extra_id_revocations [0] . dropped_skgids,
    vec![ID::from ("E1"), ID::from ("E2")]);
  let message : String = error . to_string ();
  for expected in ["P", "A titled node", "E1", "E2", "another dataset"] {
    assert! (message . contains (expected), "missing {expected}: {message}"); }
}

#[test]
fn adding_an_extra_id_remains_valid () {
  let base : Arc<InRustGraph> = Arc::new (
    InRustGraph::from_graphnodes (&[node ("P")]));
  let mut saved : Graphnode = node ("P");
  saved . extra_ids = vec![ID::from ("E")];
  let prepared : PreparedGraphUpdate = prepare_graph_update (
    &config (), base, vec![NodeInstruction::Save (SaveNode (saved))])
    . unwrap ();
  assert_eq! (
    prepared . candidate () . pid_of (&ID::from ("E")),
    Some (ID::from ("P")));
}

#[test]
fn identity_lookup_work_does_not_grow_with_the_base_graph () {
  let nodeInstructions : Vec<NodeInstruction> = vec![NodeInstruction::Save (SaveNode ({
    let mut saved : Graphnode = node ("new");
    saved . extra_ids = vec![ID::from ("new-extra")];
    saved
  }))];
  let small : PreparedGraphUpdate = prepare_graph_update (
    &config (), Arc::new (InRustGraph::from_graphnodes (&[node ("old")])),
    nodeInstructions . clone ()) . unwrap ();
  let many : Vec<Graphnode> = (0..1_000)
    . map (|i| node (&format! ("old-{i}")))
    . collect ();
  let large : PreparedGraphUpdate = prepare_graph_update (
    &config (), Arc::new (InRustGraph::from_graphnodes (&many)), nodeInstructions)
    . unwrap ();
  assert_eq! (
    small . changes . identity_base_lookup_bound,
    large . changes . identity_base_lookup_bound);
}

#[test]
fn local_index_check_catches_an_omitted_removal () {
  let (base, mut candidate, nodeInstructions, changes) = changed_relationship_fixture (0);
  add_membership (&mut candidate . contained_by, "old", "recorder");
  let report : LocalIndexValidation = validate_local_internal_indexes (
    &base, &candidate, &nodeInstructions, &changes);
  assert! (report . errors . iter () . any (|error|
    error . index == "contained_by" && error . key == ID::from ("old")));
}

#[test]
fn local_index_check_catches_an_omitted_insertion () {
  let (base, mut candidate, nodeInstructions, changes) = changed_relationship_fixture (0);
  candidate . contained_by . remove (&ID::from ("new"));
  let report : LocalIndexValidation = validate_local_internal_indexes (
    &base, &candidate, &nodeInstructions, &changes);
  assert! (report . errors . iter () . any (|error|
    error . index == "contained_by" && error . key == ID::from ("new")));
}

#[test]
fn local_index_check_catches_an_omitted_canonical_migration () {
  let mut recorder : Graphnode = node ("recorder");
  recorder . contains = rel_partners_at_relRepo (
    &SkgRepoName::from ("main"), vec![ID::from ("future")]);
  let base : InRustGraph = InRustGraph::from_graphnodes (&[recorder]);
  let mut target : Graphnode = node ("target");
  target . extra_ids = vec![ID::from ("future")];
  let nodeInstructions : Vec<NodeInstruction> =
    vec![NodeInstruction::Save (SaveNode (target))];
  let (changes, errors, revocations)
    : (GraphChangeSet, Vec<CompleteGraphError>, Vec<super::ExtraIdRevocation>) =
    validate_identity_and_derive_changes (&config (), &base, &nodeInstructions);
  assert! (errors . is_empty () && revocations . is_empty ());
  let mut candidate : InRustGraph = base . clone ();
  apply_nodeInstructions_to_inRustGraph (&mut candidate, &nodeInstructions);
  candidate . contained_by . remove (&ID::from ("target"));
  add_membership (&mut candidate . contained_by, "future", "recorder");
  let report : LocalIndexValidation = validate_local_internal_indexes (
    &base, &candidate, &nodeInstructions, &changes);
  assert_eq! (
    report . errors . iter ()
      . filter (|error| error . index == "contained_by") . count (),
    2);
}

#[test]
fn local_index_check_catches_an_empty_key_left_behind () {
  let (base, mut candidate, nodeInstructions, changes) = changed_relationship_fixture (0);
  candidate . contained_by . insert (
    ID::from ("old"), im::HashSet::new ());
  let report : LocalIndexValidation = validate_local_internal_indexes (
    &base, &candidate, &nodeInstructions, &changes);
  assert! (report . errors . iter () . any (|error|
    error . index == "contained_by" && error . key == ID::from ("old")));
}

#[test]
fn local_index_check_work_does_not_grow_with_unrelated_nodes () {
  let (small_base, small_candidate, small_nodeInstructions, small_changes) =
    changed_relationship_fixture (0);
  let (large_base, large_candidate, large_nodeInstructions, large_changes) =
    changed_relationship_fixture (1_000);
  let small : LocalIndexValidation = validate_local_internal_indexes (
    &small_base, &small_candidate, &small_nodeInstructions, &small_changes);
  let large : LocalIndexValidation = validate_local_internal_indexes (
    &large_base, &large_candidate, &large_nodeInstructions, &large_changes);
  assert! (small . errors . is_empty () && large . errors . is_empty ());
  assert_eq! (small . node_checks, large . node_checks);
  assert_eq! (small . identity_checks, large . identity_checks);
  assert_eq! (
    small . relationship_membership_checks,
    large . relationship_membership_checks);
}

#[test]
fn merge_override_collision_names_participants_and_both_repairs () {
  let skgrepo : SkgRepoName = SkgRepoName::from ("main");
  let mut n1 : Graphnode = node ("N1");
  n1 . title = "Acquirer title" . to_string ();
  let mut n2 : Graphnode = node ("N2");
  n2 . title = "Acquiree title" . to_string ();
  let mut r1 : Graphnode = node ("R1");
  r1 . title = "Existing overrider title" . to_string ();
  r1 . overrides = MSV::Specified (rel_partners_at_relRepo (
    &skgrepo, vec![ID::from ("N1")]));
  let mut r2 : Graphnode = node ("R2");
  r2 . title = "Redirected overrider title" . to_string ();
  r2 . overrides = MSV::Specified (rel_partners_at_relRepo (
    &skgrepo, vec![ID::from ("N2")]));
  let base : Arc<InRustGraph> = Arc::new (
    InRustGraph::from_graphnodes (&[n1 . clone (), n2, r1, r2]));
  n1 . extra_ids = vec![ID::from ("N2")];
  let error : GraphUpdatePreparationError = prepare_graph_update (
    &config (), base, vec![
      NodeInstruction::Save (SaveNode (n1)),
      NodeInstruction::Delete (DeleteNode {
        skgid : ID::from ("N2"), home_skgrepo: skgrepo,
      }),
    ]) . expect_err ("merge redirection must violate monogamy");
  assert_eq! (error . merge_override_collisions . len (), 1);
  let message : String = error . to_string ();
  for expected in [
    "N1", "N2", "R1", "R2", "Acquirer title", "Acquiree title",
    "Existing overrider title", "Redirected overrider title",
    "canonicalizing", "R2 -> R1 -> N1", "R1 -> R2 -> N1",
  ] {
    assert! (message . contains (expected), "missing {expected}: {message}"); }
}

proptest! {
  #![proptest_config (ProptestConfig::with_cases (256))]

  #[test]
  fn identity_overlay_matches_the_full_oracle (
    extra_indexes in proptest::collection::vec (
      proptest::collection::vec (0usize..7, 0..5), 0..3),
  ) {
    let base : InRustGraph = InRustGraph::from_graphnodes (&[
      node ("P0"), node ("P1"), node ("P2"),
    ]);
    let id_universe : [&str; 7] = [
      "P0", "P1", "P2", "N0", "N1", "X0", "X1"];
    let nodeInstructions : Vec<NodeInstruction> = extra_indexes . into_iter ()
      . enumerate ()
      . map (|(owner_index, indexes)| {
        let mut saved : Graphnode = node (&format! ("N{owner_index}"));
        saved . extra_ids = indexes . into_iter ()
          . map (|index| ID::from (id_universe [index]))
          . collect ();
        NodeInstruction::Save (SaveNode (saved))
      })
      . collect ();
    let batch = normalize_and_coalesce_nodeInstructions (nodeInstructions);
    let (_, local_errors, revocations) =
      validate_identity_and_derive_changes (
        &config (), &base, &batch . final_graph_nodeInstructions);
    prop_assert! (revocations . is_empty ());
    let full = validate_complete_graph_candidate (
      &config (), &base, &batch . final_graph_nodeInstructions);
    let prepared = prepare_graph_update (
      &config (), Arc::new (base . clone ()),
      batch . filesystem_nodeInstructions . clone ());
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
    prop_assert_eq! (prepared . is_ok (), full . errors . is_empty ());
    if let Ok (prepared) = prepared {
      let actual : &InRustGraph = prepared . candidate ();
      prop_assert_eq! (&actual . nodes, &full . graph . nodes);
      prop_assert_eq! (&actual . extra_id_to_pid, &full . graph . extra_id_to_pid);
      prop_assert_eq! (&actual . contained_by, &full . graph . contained_by);
      prop_assert_eq! (&actual . subscribers_of, &full . graph . subscribers_of);
      prop_assert_eq! (&actual . hiders_of, &full . graph . hiders_of);
      prop_assert_eq! (&actual . overriders_of, &full . graph . overriders_of);
      prop_assert_eq! (&actual . mentioners_of, &full . graph . mentioners_of);
    }
  }
}
