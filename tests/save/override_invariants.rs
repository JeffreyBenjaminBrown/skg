use skg::dbs::in_rust_graph::{
  InRustGraph,
  InRustGraphHandle,
  new_handle,
};
use skg::save::validate_override_invariants_after_save;
use skg::types::errors::{BufferValidationError, SaveError};
use skg::types::misc::{ID, MSV, SkgConfig, SkgfileSource, SourceName};
use skg::types::nodes::complete::{NodeComplete, empty_node_complete};
use skg::types::save::{DefineNode, SaveNode, DeleteNode, NodeMerge};

use std::collections::HashMap;
use std::error::Error;
use std::path::PathBuf;

fn config () -> SkgConfig {
  SkgConfig::dummyFromSources (HashMap::from ([(
    SourceName::from ("owned"),
    SkgfileSource {
      name: SourceName::from ("owned"),
      abbreviation: None,
      path: PathBuf::from ("/tmp/owned"),
      user_owns_it: true,
    })])) }

fn node (
  pid       : &str,
  overrides : &[&str],
) -> NodeComplete {
  let mut node : NodeComplete =
    empty_node_complete ();
  node . pid = ID::from (pid);
  node . title = pid . to_string ();
  node . source = SourceName::from ("owned");
  node . overrides_view_of =
    if overrides . is_empty () {
      MSV::Unspecified
    } else {
      MSV::Specified (
        overrides . iter ()
        . map ( |id| ID::from (*id) )
        . collect () )
    };
  node }

fn assert_override_validation_error (
  result : Result<(), Box<dyn Error>>,
) {
  let err : Box<dyn Error> =
    result . expect_err ("save should be rejected");
  let save_error : &SaveError =
    err . downcast_ref::<SaveError> ()
    . expect ("error should remain a SaveError");
  assert! (matches!(
    save_error,
    SaveError::BufferValidationErrors { errors, .. }
      if matches!(
        errors . first (),
        Some (BufferValidationError::OverrideInvariantViolation (_)))
  )); }

#[test]
fn save_simulation_rejects_second_user_owned_overrider () {
  let initial : Vec<NodeComplete> = vec![
    node ("target", &[]),
    node ("one", &["target"]),
  ];
  let graph : InRustGraphHandle =
    new_handle (InRustGraph::from_nodecompletes (&initial));
  let result : Result<(), Box<dyn Error>> =
    validate_override_invariants_after_save (
      &[DefineNode::Save (SaveNode (node ("two", &["target"])))],
      &[],
      &config (),
      &graph );
  assert_override_validation_error (result); }

#[test]
fn merge_that_collides_two_overriders_is_rejected () {
  // R1 overrides N1 and R2 overrides N2; all user-owned, and the
  // graph satisfies monogamy (one overrider each). A save merges N2
  // into N1: the acquirer N1 takes N2's id as an extra id, so R2's
  // override of N2 now resolves to N1 -- two user-owned overriders of
  // one node. The real pipeline re-saves the acquiree's neighbors
  // (here R2, whose outbound override pointed at N2) as ordinary save
  // instructions, which is what makes the collision a /touched/ edge;
  // we reproduce that here. This exercises the nodeMerge_definenodes
  // arm of validate_override_invariants_after_save, previously driven
  // only with an empty merge list.
  let initial : Vec<NodeComplete> = vec![
    node ("N1", &[]),
    node ("N2", &[]),
    node ("R1", &["N1"]),
    node ("R2", &["N2"]),
  ];
  let graph : InRustGraphHandle =
    new_handle (InRustGraph::from_nodecompletes (&initial));
  let updated_acquirer : NodeComplete = {
    let mut n1 : NodeComplete = node ("N1", &[]);
    n1 . extra_ids = vec![ID::from ("N2")];
    n1 };
  let merge : NodeMerge = NodeMerge {
    acquiree_text_preserver :
      SaveNode (node ("merged-N2-text", &[])),
    updated_acquirer :
      SaveNode (updated_acquirer),
    acquiree_to_delete :
      DeleteNode {
        id     : ID::from ("N2"),
        source : SourceName::from ("owned"), }, };
  let result : Result<(), Box<dyn Error>> =
    validate_override_invariants_after_save (
      &[DefineNode::Save (SaveNode (node ("R2", &["N2"])))],
      &[merge],
      &config (),
      &graph );
  assert_override_validation_error (result); }

#[test]
fn save_simulation_rejects_user_owned_override_chain () {
  let initial : Vec<NodeComplete> = vec![
    node ("z", &[]),
    node ("y", &["z"]),
  ];
  let graph : InRustGraphHandle =
    new_handle (InRustGraph::from_nodecompletes (&initial));
  let result : Result<(), Box<dyn Error>> =
    validate_override_invariants_after_save (
      &[DefineNode::Save (SaveNode (node ("x", &["y"])))],
      &[],
      &config (),
      &graph );
  assert_override_validation_error (result); }
