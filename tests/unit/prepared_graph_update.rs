use super::{GraphChangeSet, GraphUpdatePreparationError, PreparedGraphUpdate,
            normalize_and_coalesce_definitions, prepare_graph_update,
            validate_identity_and_derive_changes};
use crate::dbs::in_rust_graph::complete_validation::{
  CompleteGraphError, validate_complete_graph_candidate,
};
use crate::dbs::in_rust_graph::{InRustGraph, InRustGraphHandle, new_handle};
use crate::dbs::in_rust_graph::apply_definenodes_to_inRustGraph;
use crate::dbs::in_rust_graph::internal_index_validation::{
  LocalIndexValidation, validate_local_internal_indexes,
};
use crate::types::misc::{
  ID, MSV, SkgConfig, SkgfileSource, SourceName, members_at_source,
};
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

fn changed_edge_fixture (
  unrelated_count : usize,
) -> (InRustGraph, InRustGraph, Vec<DefineNode>, GraphChangeSet) {
  let mut old_owner : NodeComplete = node ("owner");
  old_owner . contains = members_at_source (
    &SourceName::from ("main"), vec![ID::from ("old")]);
  let mut base_nodes : Vec<NodeComplete> = vec![old_owner];
  base_nodes . extend ((0..unrelated_count)
    . map (|i| node (&format! ("unrelated-{i}"))));
  let base : InRustGraph = InRustGraph::from_nodecompletes (&base_nodes);
  let mut final_owner : NodeComplete = node ("owner");
  final_owner . contains = members_at_source (
    &SourceName::from ("main"), vec![ID::from ("new")]);
  let definitions : Vec<DefineNode> =
    vec![DefineNode::Save (SaveNode (final_owner))];
  let (changes, errors, revocations)
    : (GraphChangeSet, Vec<CompleteGraphError>, Vec<super::ExtraIdRevocation>) =
    validate_identity_and_derive_changes (&config (), &base, &definitions);
  assert! (errors . is_empty ());
  assert! (revocations . is_empty ());
  let mut candidate : InRustGraph = base . clone ();
  apply_definenodes_to_inRustGraph (&mut candidate, &definitions);
  (base, candidate, definitions, changes)
}

fn add_membership (
  index : &mut im::HashMap<ID, im::HashSet<ID>>,
  key   : &str,
  owner : &str,
) {
  let mut owners : im::HashSet<ID> = index . get (&ID::from (key))
    . cloned () . unwrap_or_default ();
  owners . insert (ID::from (owner));
  index . insert (ID::from (key), owners);
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

#[test]
fn local_index_check_catches_an_omitted_removal () {
  let (base, mut candidate, definitions, changes) = changed_edge_fixture (0);
  add_membership (&mut candidate . contained_by, "old", "owner");
  let report : LocalIndexValidation = validate_local_internal_indexes (
    &base, &candidate, &definitions, &changes);
  assert! (report . errors . iter () . any (|error|
    error . index == "contained_by" && error . key == ID::from ("old")));
}

#[test]
fn local_index_check_catches_an_omitted_insertion () {
  let (base, mut candidate, definitions, changes) = changed_edge_fixture (0);
  candidate . contained_by . remove (&ID::from ("new"));
  let report : LocalIndexValidation = validate_local_internal_indexes (
    &base, &candidate, &definitions, &changes);
  assert! (report . errors . iter () . any (|error|
    error . index == "contained_by" && error . key == ID::from ("new")));
}

#[test]
fn local_index_check_catches_an_omitted_canonical_migration () {
  let mut owner : NodeComplete = node ("owner");
  owner . contains = members_at_source (
    &SourceName::from ("main"), vec![ID::from ("future")]);
  let base : InRustGraph = InRustGraph::from_nodecompletes (&[owner]);
  let mut target : NodeComplete = node ("target");
  target . extra_ids = vec![ID::from ("future")];
  let definitions : Vec<DefineNode> =
    vec![DefineNode::Save (SaveNode (target))];
  let (changes, errors, revocations)
    : (GraphChangeSet, Vec<CompleteGraphError>, Vec<super::ExtraIdRevocation>) =
    validate_identity_and_derive_changes (&config (), &base, &definitions);
  assert! (errors . is_empty () && revocations . is_empty ());
  let mut candidate : InRustGraph = base . clone ();
  apply_definenodes_to_inRustGraph (&mut candidate, &definitions);
  candidate . contained_by . remove (&ID::from ("target"));
  add_membership (&mut candidate . contained_by, "future", "owner");
  let report : LocalIndexValidation = validate_local_internal_indexes (
    &base, &candidate, &definitions, &changes);
  assert_eq! (
    report . errors . iter ()
      . filter (|error| error . index == "contained_by") . count (),
    2);
}

#[test]
fn local_index_check_catches_an_empty_key_left_behind () {
  let (base, mut candidate, definitions, changes) = changed_edge_fixture (0);
  candidate . contained_by . insert (
    ID::from ("old"), im::HashSet::new ());
  let report : LocalIndexValidation = validate_local_internal_indexes (
    &base, &candidate, &definitions, &changes);
  assert! (report . errors . iter () . any (|error|
    error . index == "contained_by" && error . key == ID::from ("old")));
}

#[test]
fn local_index_check_work_does_not_grow_with_unrelated_nodes () {
  let (small_base, small_candidate, small_definitions, small_changes) =
    changed_edge_fixture (0);
  let (large_base, large_candidate, large_definitions, large_changes) =
    changed_edge_fixture (1_000);
  let small : LocalIndexValidation = validate_local_internal_indexes (
    &small_base, &small_candidate, &small_definitions, &small_changes);
  let large : LocalIndexValidation = validate_local_internal_indexes (
    &large_base, &large_candidate, &large_definitions, &large_changes);
  assert! (small . errors . is_empty () && large . errors . is_empty ());
  assert_eq! (small . node_checks, large . node_checks);
  assert_eq! (small . identity_checks, large . identity_checks);
  assert_eq! (
    small . relationship_membership_checks,
    large . relationship_membership_checks);
}

#[test]
fn merge_override_collision_names_participants_and_both_repairs () {
  let source : SourceName = SourceName::from ("main");
  let mut n1 : NodeComplete = node ("N1");
  n1 . title = "Acquirer title" . to_string ();
  let mut n2 : NodeComplete = node ("N2");
  n2 . title = "Acquiree title" . to_string ();
  let mut r1 : NodeComplete = node ("R1");
  r1 . title = "Existing overrider title" . to_string ();
  r1 . overrides_view_of = MSV::Specified (members_at_source (
    &source, vec![ID::from ("N1")]));
  let mut r2 : NodeComplete = node ("R2");
  r2 . title = "Redirected overrider title" . to_string ();
  r2 . overrides_view_of = MSV::Specified (members_at_source (
    &source, vec![ID::from ("N2")]));
  let base : Arc<InRustGraph> = Arc::new (
    InRustGraph::from_nodecompletes (&[n1 . clone (), n2, r1, r2]));
  n1 . extra_ids = vec![ID::from ("N2")];
  let error : GraphUpdatePreparationError = prepare_graph_update (
    &config (), base, vec![
      DefineNode::Save (SaveNode (n1)),
      DefineNode::Delete (DeleteNode {
        id : ID::from ("N2"), source,
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
    let prepared = prepare_graph_update (
      &config (), Arc::new (base . clone ()),
      batch . filesystem_definitions . clone ());
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
      prop_assert_eq! (&actual . textlinks_in, &full . graph . textlinks_in);
    }
  }
}
