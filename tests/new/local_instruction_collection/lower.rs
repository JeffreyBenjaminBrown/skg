/// These are unit tests for lowering
/// (server/from_text/local_instruction_collection/lower.rs), which
/// turns CollectedFieldIntents into ordered NodeIntents plus extracted
/// signals.

use ego_tree::Tree;
use indoc::indoc;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::from_text::local_instruction_collection::lower::{
  lower_collected_fieldIntents, nodeMerge_pairs, LoweringOutput };
use skg::from_text::local_instruction_collection::traverse::collect_instructions_locally;
use skg::from_text::local_instruction_collection::types::CollectedFieldIntents;
use skg::types::maybe_placed_viewnode::{
  MpViewnode, maybePlaced_to_placed_tree };
use skg::types::misc::{ID, MSV, members_of, members_msv};
use skg::types::nodes::complete::Graphnode;
use skg::types::save::{NodeInstruction, SaveNode, DeleteNode};
use skg::types::tree::forest::ViewForest;

fn collected_from_org (
  input : &str,
) -> CollectedFieldIntents {
  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let forest : ViewForest =
    ViewForest::from_internal_tree (
      maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap() );
  collect_instructions_locally (&forest) . unwrap() }

#[test]
fn lowering_produces_ordered_nodeInstructions_and_signals () {
  let collected : CollectedFieldIntents =
    collected_from_org ( indoc! {"
      * (skg (node (id root) (repo main))) root
      Root body
      ** (skg aliasFolder) aliases
      *** (skg alias) nickname
      ** (skg (node (id child) (repo main))) child
      ** (skg subscribeeFolder)
      *** (skg (node (id e) (repo main))) e
      **** (skg (node (id visible) (repo main))) visible
      * (skg (node (id doomed) (repo main) (editRequest delete))) doomed
      "} );
  let LoweringOutput { intents, visibility, .. } =
    lower_collected_fieldIntents (collected) . unwrap();
  { // There is one visibility pair, aimed at the subscriber.
    assert_eq!( visibility . len(), 1 );
    let (subscriber, signal) = &visibility [0];
    assert_eq!( subscriber, &ID::from ("root") );
    assert_eq!( signal . subscribee, ID::from ("e") );
    assert_eq!( signal . visible, vec![ID::from ("visible")] ); }
  let nodeInstructions : Vec<NodeInstruction> =
    intents . into_ordered_intents()
    . into_iter()
    . map ( |intent| intent . into_node_instruction() . unwrap() )
    . collect();
  // 'e' is a subscribee-as-such, holding only signals, so it
  // lowers to nothing.
  assert_eq!( nodeInstructions . len(), 4 );
  { let root : &Graphnode = match &nodeInstructions [0] {
      NodeInstruction::Save (SaveNode (node)) => node,
      _ => panic! ("expected Save for root") };
    assert_eq!( root . pid, ID::from ("root") );
    assert_eq!( root . title, "root" );
    assert_eq!( root . body, Some ("Root body" . to_string()) );
    assert_eq!( members_of (&root . contains), vec![ID::from ("child")] );
    assert_eq!( members_msv (&root . aliases),
                MSV::Specified (vec!["nickname" . to_string()]) );
    assert_eq!( members_msv (&root . subscribesTo),
                MSV::Specified (vec![ID::from ("e")]) );
    // Unmentioned fields lower to Unspecified; hides is never
    // emitted by collection (resolution sets it later).
    assert_eq!( root . hidesFromSubs,
                MSV::Unspecified );
    assert_eq!( root . overrides, MSV::Unspecified ); }
  assert!( matches!( &nodeInstructions [1],
    NodeInstruction::Save (SaveNode (node))
      if node . pid == ID::from ("child") ));
  assert!( matches!( &nodeInstructions [2],
    NodeInstruction::Save (SaveNode (node))
      if node . pid == ID::from ("visible") ));
  assert!( matches!( &nodeInstructions [3],
    NodeInstruction::Delete (DeleteNode { skgid, .. })
      if skgid == &ID::from ("doomed") )); }

#[test]
fn nodeMerge_pairs_come_from_the_map_in_order () {
  let collected : CollectedFieldIntents =
    collected_from_org ( indoc! {"
      * (skg (node (id a) (repo main) (editRequest (merge x)))) a
      * (skg (node (id b) (repo main))) b
      * (skg (node (id c) (repo main) (editRequest (merge y)))) c
      "} );
  assert_eq!(
    nodeMerge_pairs (&collected),
    vec![ (ID::from ("a"), ID::from ("x")),
          (ID::from ("c"), ID::from ("y")) ]); }
