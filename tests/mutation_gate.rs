//! Deterministic regression for the graph half of a concurrent save.
//!
//! Two writers that both clone the same ArcSwap snapshot and independently
//! publish a replacement lose whichever write stores first.  The real save
//! pipeline also writes disk and derived indexes, so its shared MutationGate
//! must cover snapshot capture through publication.  These tests force that
//! old interleaving without involving filesystem timing or external services.

use futures::executor::block_on;
use futures::join;
use skg::dbs::in_rust_graph::{
  InRustGraph, InRustGraphHandle, apply_definenodes_to_inRustGraph, new_handle,
};
use skg::dbs::init::empty_in_ram_tantivy_index;
use skg::dbs::tantivy::background_writer::{
  TantivyWriteTask, enqueue_tantivy_write, lock_tantivy_writes,
  wait_for_tantivy_writes_idle,
};
use skg::dbs::tantivy::search::{SearchOptions, search_index};
use skg::types::env::new_mutation_gate;
use skg::types::misc::{ID, MemberAtSource, SourceName};
use skg::types::nodes::complete::{NodeComplete, empty_node_complete};
use skg::types::nodes::rust::NodeRust;
use skg::types::save::{DefineNode, DeleteNode, SaveNode};

use std::collections::HashMap;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use tokio::sync::{Barrier, Notify};

fn save (pid : &str) -> DefineNode {
  DefineNode::Save ( SaveNode (node (pid, "", "main")) ) }

fn node (pid : &str, title : &str, source : &str) -> NodeComplete {
  let mut node = empty_node_complete ();
  node . pid = ID::from (pid);
  node . title = title . to_string ();
  node . source = SourceName::from (source);
  node }

fn complete_from_rust (node : &NodeRust) -> NodeComplete {
  NodeComplete {
    title : node . title . clone (),
    overPrivateText_telescope : node . overPrivateText_telescope,
    aliases : node . aliases . clone (),
    source : node . source . clone (),
    pid : node . pid . clone (),
    extra_ids : node . extra_ids . clone (),
    body : node . body . clone (),
    contains : node . contains . clone (),
    subscribes_to : node . subscribes_to . clone (),
    hides_from_its_subscriptions : node . hides_from_its_subscriptions . clone (),
    overrides_view_of : node . overrides_view_of . clone (),
    misc : node . misc . clone (),
  } }

fn publish_from_snapshot (
  handle : &InRustGraphHandle,
  snapshot : &Arc<InRustGraph>,
  instruction : &DefineNode,
) {
  let mut candidate : InRustGraph = (**snapshot) . clone ();
  apply_definenodes_to_inRustGraph (
    &mut candidate, std::slice::from_ref (instruction) );
  handle . store ( Arc::new (candidate) ); }

#[test]
fn two_simultaneous_snapshot_writers_lose_an_update_without_a_gate () {
  let handle : InRustGraphHandle = new_handle ( InRustGraph::new () );
  let a = save ("a");
  let b = save ("b");
  // Capture both old snapshots before either candidate is published.  The
  // barrier makes this the exact lost-update schedule rather than a race that
  // happens only occasionally under load.
  let a_snapshot = handle . load_full ();
  let b_snapshot = handle . load_full ();
  let barrier = Arc::new ( Barrier::new (2) );
  block_on ( async {
    let a_barrier = barrier . clone ();
    let b_barrier = barrier . clone ();
    let write_a = async {
      a_barrier . wait () . await;
      publish_from_snapshot (&handle, &a_snapshot, &a); };
    let write_b = async {
      b_barrier . wait () . await;
      publish_from_snapshot (&handle, &b_snapshot, &b); };
    join! (write_a, write_b); } );
  let final_graph = handle . load_full ();
  assert_eq! (final_graph . len (), 1,
              "two same-base snapshots overwrite one another without serialization");
  assert! (final_graph . nodes . contains_key (&ID::from ("a")) ||
           final_graph . nodes . contains_key (&ID::from ("b"))); }

#[test]
fn shared_mutation_gate_serializes_snapshot_capture_and_preserves_both_writes () {
  let handle : InRustGraphHandle = new_handle ( InRustGraph::new () );
  let gate = new_mutation_gate ();
  let a = save ("a");
  let b = save ("b");
  let first_captured = Arc::new ( Notify::new () );
  let release_first = Arc::new ( Notify::new () );
  let second_entered = Arc::new ( AtomicBool::new (false) );

  block_on ( async {
    let first_captured_a = first_captured . clone ();
    let release_first_a = release_first . clone ();
    let gate_a = gate . clone ();
    let write_a = async {
      let _guard = gate_a . lock () . await;
      let snapshot = handle . load_full ();
      first_captured_a . notify_one ();
      release_first_a . notified () . await;
      publish_from_snapshot (&handle, &snapshot, &a); };

    let gate_b = gate . clone ();
    let second_entered_b = second_entered . clone ();
    let write_b = async {
      let _guard = gate_b . lock () . await;
      second_entered_b . store (true, Ordering::Release);
      let snapshot = handle . load_full ();
      publish_from_snapshot (&handle, &snapshot, &b); };

    let control = async {
      first_captured . notified () . await;
      assert! ( ! second_entered . load (Ordering::Acquire),
                "the second writer entered before the first released the shared gate" );
      release_first . notify_one (); };

    join! (write_a, write_b, control); } );

  let final_graph = handle . load_full ();
  assert_eq! (final_graph . len (), 2);
  assert! (final_graph . nodes . contains_key (&ID::from ("a")));
  assert! (final_graph . nodes . contains_key (&ID::from ("b"))); }

#[test]
fn save_after_merge_delete_resolves_the_acquiree_to_the_merged_node () {
  let mut owner = node ("owner", "owner", "main");
  owner . contains = vec! [ MemberAtSource::at_source (
    SourceName::from ("main"), ID::from ("acquiree")) ];
  let initial = vec! [
    owner . clone (),
    node ("acquiree", "old", "main"),
    node ("acquirer", "new", "main"),
  ];
  let handle = new_handle (InRustGraph::from_nodecompletes (&initial));
  let gate = new_mutation_gate ();
  let merge_entered = Arc::new (Notify::new ());
  let release_merge = Arc::new (Notify::new ());

  block_on (async {
    let gate_a = gate . clone ();
    let entered_a = merge_entered . clone ();
    let release_a = release_merge . clone ();
    let merge_delete = async {
      let _guard = gate_a . lock () . await;
      let snapshot = handle . load_full ();
      entered_a . notify_one ();
      release_a . notified () . await;
      let mut merged = node ("acquirer", "merged", "main");
      merged . extra_ids = vec! [ID::from ("acquiree")];
      let mut rewritten_owner = owner . clone ();
      rewritten_owner . contains [0] . member = ID::from ("acquirer");
      let instructions = vec! [
        DefineNode::Save (SaveNode (merged)),
        DefineNode::Save (SaveNode (rewritten_owner)),
        DefineNode::Delete (DeleteNode {
          id : ID::from ("acquiree"), source : SourceName::from ("main") }),
      ];
      let mut candidate = (*snapshot) . clone ();
      apply_definenodes_to_inRustGraph (&mut candidate, &instructions);
      handle . store (Arc::new (candidate)); };

    let gate_b = gate . clone ();
    let entered_b = merge_entered . clone ();
    let save_after = async {
      entered_b . notified () . await;
      release_merge . notify_one ();
      let _guard = gate_b . lock () . await;
      let snapshot = handle . load_full ();
      let mut observer = node ("observer", "observer", "main");
      // The request still names the acquiree.  Applying against the
      // post-merge snapshot must canonicalize its inverse entry.
      observer . contains = vec! [ MemberAtSource::at_source (
        SourceName::from ("main"), ID::from ("acquiree")) ];
      publish_from_snapshot (
        &handle, &snapshot, &DefineNode::Save (SaveNode (observer))); };
    join! (merge_delete, save_after); });

  let graph = handle . load_full ();
  assert! (! graph . nodes . contains_key (&ID::from ("acquiree")));
  assert_eq! (graph . pid_of (&ID::from ("acquiree")), Some (ID::from ("acquirer")));
  let inbound = graph . contained_by . get (&ID::from ("acquirer")) . unwrap ();
  assert! (inbound . contains (&ID::from ("owner")));
  assert! (inbound . contains (&ID::from ("observer"))); }

#[test]
fn save_plan_captured_after_source_move_preserves_the_new_source () {
  let initial = node ("moved", "old title", "main");
  let handle = new_handle (InRustGraph::from_nodecompletes (&[initial]));
  let gate = new_mutation_gate ();
  let move_entered = Arc::new (Notify::new ());
  let release_move = Arc::new (Notify::new ());

  block_on (async {
    let gate_a = gate . clone ();
    let entered_a = move_entered . clone ();
    let release_a = release_move . clone ();
    let source_move = async {
      let _guard = gate_a . lock () . await;
      let snapshot = handle . load_full ();
      entered_a . notify_one ();
      release_a . notified () . await;
      let mut moved = node ("moved", "old title", "private");
      moved . extra_ids = vec! [ID::from ("former-id")];
      publish_from_snapshot (
        &handle, &snapshot, &DefineNode::Save (SaveNode (moved))); };

    let gate_b = gate . clone ();
    let entered_b = move_entered . clone ();
    let edit_after_move = async {
      entered_b . notified () . await;
      release_move . notify_one ();
      let _guard = gate_b . lock () . await;
      // This represents save-plan construction: copy the node only after the
      // gate is acquired, then alter the field supplied by the edited buffer.
      let snapshot = handle . load_full ();
      let mut edited : NodeComplete =
        complete_from_rust (snapshot . get (&ID::from ("moved")) . unwrap ());
      edited . title = "edited after move" . to_string ();
      publish_from_snapshot (
        &handle, &snapshot, &DefineNode::Save (SaveNode (edited))); };
    join! (source_move, edit_after_move); });

  let graph = handle . load_full ();
  let moved = graph . get (&ID::from ("moved")) . unwrap ();
  assert_eq! (moved . source, SourceName::from ("private"));
  assert_eq! (moved . title, "edited after move");
  assert_eq! (graph . pid_of (&ID::from ("former-id")), Some (ID::from ("moved"))); }

#[test]
fn rebuild_started_after_a_mutation_publishes_a_snapshot_containing_it () {
  let handle = new_handle (InRustGraph::from_nodecompletes (&[
    node ("seed", "seed", "main") ]));
  let gate = new_mutation_gate ();
  let mutation_entered = Arc::new (Notify::new ());
  let release_mutation = Arc::new (Notify::new ());

  block_on (async {
    let gate_a = gate . clone ();
    let entered_a = mutation_entered . clone ();
    let release_a = release_mutation . clone ();
    let mutation = async {
      let _guard = gate_a . lock () . await;
      let snapshot = handle . load_full ();
      entered_a . notify_one ();
      release_a . notified () . await;
      publish_from_snapshot (&handle, &snapshot, &save ("saved")); };

    let gate_b = gate . clone ();
    let entered_b = mutation_entered . clone ();
    let rebuild = async {
      entered_b . notified () . await;
      release_mutation . notify_one ();
      let _guard = gate_b . lock () . await;
      // A real rebuild reads disk here.  Its candidate must therefore be
      // based on the state published by every earlier gated mutation.
      let disk_nodes : Vec<NodeComplete> = handle . load_full () . nodes
        . values () . map (complete_from_rust) . collect ();
      handle . store (Arc::new (InRustGraph::from_nodecompletes (&disk_nodes))); };
    join! (mutation, rebuild); });

  let graph = handle . load_full ();
  assert! (graph . nodes . contains_key (&ID::from ("seed")));
  assert! (graph . nodes . contains_key (&ID::from ("saved"))); }

#[test]
fn tantivy_worker_applies_same_pid_tasks_in_publication_order () {
  let index = empty_in_ram_tantivy_index () . unwrap ();
  // Holding the writer lock makes both tasks queue before either can commit,
  // so the assertion depends on channel order rather than scheduler timing.
  let writer_lock = lock_tantivy_writes ();
  for title in ["firstversiontoken", "secondversiontoken"] {
    enqueue_tantivy_write (TantivyWriteTask {
      tantivy_index : index . clone (),
      instructions : vec! [DefineNode::Save (SaveNode (
        node ("same-pid", title, "main")))],
      context_types : HashMap::new (),
    }); }
  drop (writer_lock);
  wait_for_tantivy_writes_idle ();

  let opts = SearchOptions::default ();
  let (old_hits, _) = search_index (&index, "firstversiontoken", &opts) . unwrap ();
  let (new_hits, _) = search_index (&index, "secondversiontoken", &opts) . unwrap ();
  assert! (old_hits . is_empty (), "the first publication must be superseded");
  assert_eq! (new_hits . len (), 1, "the final queued publication must be searchable"); }
