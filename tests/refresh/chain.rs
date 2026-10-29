// The containerward chain ('server/refresh/chain.rs'): each chain node
// is what the ID rule makes of its gnid under its parent's chain goal,
// built as the refresh builds that kind.

use super::*;
use crate::refresh::chain::attach_containerward_chain;
use crate::refresh::goal::{Goal, chain_goal};
use crate::refresh::id_rule::{IdRuleAnswer, id_rule};
use crate::types::tree::forest::stands_in_containerward_chain;
use indoc::formatdoc;

/// m, in 'main', is contained by a. a's containers are top, h and s.
/// h is private, and records its containment of a in 'main' (a visible
/// record of a restricted node); hc contains h. s is in 'main' and
/// records its containment of a in 'private' (a restricted record).
fn m_and_its_chain () -> InRustGraph {
  let in_private = |pids : &[&str]| -> Vec<RelPartner<ID>> {
    rel_partners_at_relRepo (
      &SkgrepoName::from ("private"),
      pids . iter () . map ( |pid| ID::from (*pid) ) . collect () ) };
  graph (&[
    node ("m"),
    Graphnode { contains : ids (&["m"]), .. node ("a") },
    Graphnode { contains : ids (&["a"]), .. node ("top") },
    Graphnode { home_skgrepo : SkgrepoName::from ("private"),
                title        : "h's secret title" . to_string (),
                contains     : ids (&["a"]), .. node ("h") },
    Graphnode { contains : ids (&["h"]), .. node ("hc") },
    Graphnode { contains : in_private (&["a"]), .. node ("s") } ]) }

#[test]
fn a_chain_node_is_what_the_id_rule_answers () {
  let graph : InRustGraph = m_and_its_chain ();
  let restriction : SkgrepoRestriction = main_only ();
  let input : String = formatdoc! ("
    * {} m
    ** (skg containerFolder)
    ", occurrence_metadata ("m", "") );
  let run : Run = run_asserting_fixpoint_in (
    &input, &graph, Some (&restriction), RefreshMode::ContentView, 100 );
  assert_eq! ( run . outline (), "\
node m
  containerFolder
    node a wp noClaim
      restricted h noClaim
      node top wp noClaim",
    "a's chain shows top, and h as a restricted headline, where it \
     stops; s, whose record is restricted, is not drawn" );
  for secret in [ "secret title", "(gnid hc)", "(gnid s)" ] {
    assert! ( ! run . text . contains (secret), "{}: {}", secret, run . text ); }
  let config : SkgConfig = config ();
  let truth : Truth = Truth { graph : &graph, config : &config,
                              restriction : Some (&restriction) };
  let policy : BufferCorrectionPolicy = BufferCorrectionPolicy::ReportAll;
  let walk : Walk = Walk::new (&truth, RefreshMode::ContentView, 0, &policy);
  let mut chain_nodes : usize = 0;
  for node in run . forest . nodes () . filter ( |node| stands_in_containerward_chain (*node) ) {
    chain_nodes += 1;
    let parent_pid : ID = node . parent () . unwrap () . value () . occurrence_gnid ()
      . expect ("a chain node stands under an occurrence") . clone ();
    let goal : Goal = chain_goal (&parent_pid, &walk);
    let ViewnodeKind::Vognode (vognode) = &node . value () . kind
      else { panic! ("a chain node is a vognode") };
    assert! ( ! vognode . claims (), "{:?} claims nothing", vognode );
    match ( id_rule (vognode . gnid (), Some (&goal), false, &walk), vognode ) {
      ( IdRuleAnswer::Occurrence (pid), Vognode::Occurrence (occurrence) ) =>
        assert! ( occurrence . gnid == pid && occurrence . is_writeProtected () ),
      ( IdRuleAnswer::GnidOnly (kind), Vognode::GnidOnly (gnid_only) ) =>
        assert_eq! ( gnid_only . kind, kind ),
      ( answer, vognode ) =>
        panic! ( "the ID rule answers {:?} for {:?}", answer, vognode ), }}
  assert_eq! ( chain_nodes, 2 ); }

/// The chain drawn outside a refresh, under an editable occurrence,
/// is drawn alike: write-protected and claiming nothing, though its
/// parent reads claims.
#[test]
fn a_chain_under_an_editable_occurrence_claims_nothing () {
  let graph : InRustGraph = m_and_its_chain ();
  let restriction : SkgrepoRestriction = main_only ();
  let input : String = formatdoc! ("
    * {} m
    ", occurrence_metadata ("m", "") );
  let mut forest : ViewForest = parse (&input, IdResolution::ThroughGraph (&graph));
  let config : SkgConfig = config ();
  let truth : Truth = Truth { graph : &graph, config : &config,
                              restriction : Some (&restriction) };
  let policy : BufferCorrectionPolicy = BufferCorrectionPolicy::ReportAll;
  let walk : Walk = Walk::new (&truth, RefreshMode::ContentView, 0, &policy);
  let m : NodeId = forest . roots () . next () . unwrap () . id ();
  attach_containerward_chain ( forest . as_internal_tree_mut (), m, &walk ) . unwrap ();
  assert_eq! ( outline (&forest), "\
node m
  node a wp noClaim
    restricted h noClaim
    node top wp noClaim" ); }
