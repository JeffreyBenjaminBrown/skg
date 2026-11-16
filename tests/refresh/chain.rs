// The containerward chain ('server/refresh/chain.rs'): each chain node
// is what the ID rule makes of its gnid under its parent's chain goal,
// built as the refresh builds that kind. Under a member of a forks
// folder a content view draws it in a containers folder, folded, and
// under a repeat nothing; the diff view hangs it directly.

use super::*;
use crate::refresh::chain::draw_containerward_chain;
use crate::refresh::goal::{Goal, chain_goal};
use crate::refresh::id_rule::{IdRuleAnswer, WhereTheHeadlineStands, id_rule};
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
    &input, &graph, &restriction, RefreshMode::ContentView, 100 );
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
                              restriction : &restriction };
  let walk : Walk = Walk::new (&truth, RefreshMode::ContentView, 0, Perspective::RootAuthors);
  let mut chain_nodes : usize = 0;
  for node in run . forest . nodes () . filter ( |node| stands_in_containerward_chain (*node, false) ) {
    chain_nodes += 1;
    let parent_pid : ID = node . parent () . unwrap () . value () . occurrence_gnid ()
      . expect ("a chain node stands under an occurrence") . clone ();
    let goal : Goal = chain_goal (&parent_pid, &walk);
    let ViewnodeKind::Vognode (vognode) = &node . value () . kind
      else { panic! ("a chain node is a vognode") };
    assert! ( ! vognode . claims (), "{:?} claims nothing", vognode );
    match ( id_rule (vognode . gnid (), WhereTheHeadlineStands::Under (&goal), &walk), vognode ) {
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
                              restriction : &restriction };
  let walk : Walk = Walk::new (&truth, RefreshMode::ContentView, 0, Perspective::RootAuthors);
  let m : NodeId = forest . roots () . next () . unwrap () . id ();
  draw_containerward_chain ( forest . as_internal_tree_mut (), m, &walk ) . unwrap ();
  assert_eq! ( outline (&forest), "\
node m
  node a wp noClaim
    restricted h noClaim
    node top wp noClaim" ); }

/// p contains n, e and f; q contains e and f; top contains q. n forks
/// e, and f forks n.
fn n_and_its_fork_partners () -> InRustGraph {
  graph (&[
    Graphnode { contains : ids (&["n", "e", "f"]), .. node ("p") },
    Graphnode { contains : ids (&["e", "f"]), .. node ("q") },
    Graphnode { contains : ids (&["q"]), .. node ("top") },
    Graphnode { forks : msv_ids (&["e"]), .. node ("n") },
    node ("e"),
    Graphnode { forks : msv_ids (&["n"]), .. node ("f") }, ]) }

/// In a content view a member of the forks folder carries its chain
/// in a containers folder, drawn folded, which leaves out the
/// member's view-ancestors: under e, the forkee, and under f, a
/// forker claiming nothing, the folder holds q (folded, with its own
/// chain), not p, which stands above them. Nothing of the chain
/// stands directly under either member. (p is write-protected, so
/// its content does not draw e or f, whose occurrences there would
/// hold their nodes' editability.)
#[test]
fn a_forks_folder_member_carries_its_chain_in_a_folded_containers_folder () {
  let input : String = formatdoc! ("
    * {} p
    ** {} n
    ", occurrence_metadata ("p", "writeProtected"),
       occurrence_metadata ("n", "(viewRequests editableView)") );
  let run : Run = run_asserting_fixpoint (&input, &n_and_its_fork_partners ());
  assert_eq! ( run . outline (), "\
node p wp
  node n noClaim
    forksFolder
      node e wp
        containerFolder
          node q wp noClaim folded
            node top wp noClaim
      node f wp noClaim
        containerFolder
          node q wp noClaim folded
            node top wp noClaim" ); }

/// A repeat (an occurrence of a node another occurrence holds the
/// editability of) in the forks folder shows nothing, not even its
/// containers folder: e, a root of the view too. g, which n also
/// forks, is no repeat, and shows its containers folder.
#[test]
fn a_repeat_in_the_forks_folder_shows_nothing () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["e", "g"]), .. node ("q") },
    Graphnode { forks : msv_ids (&["e", "g"]), .. node ("n") },
    node ("e"), node ("g"), ]);
  let input : String = formatdoc! ("
    * {} n
    * {} e
    ", occurrence_metadata ("n", "(viewRequests editableView)"),
       occurrence_metadata ("e", "") );
  assert_eq! ( run_asserting_fixpoint (&input, &graph) . outline (), "\
node n
  forksFolder
    node e wp
    node g wp
      containerFolder
        node q wp noClaim folded
node e" ); }

/// The diff view hangs the chain directly under a member of the forks
/// folder, as under every partner folder's member; a content view
/// puts it in a containers folder.
#[test]
fn the_diff_view_hangs_a_forks_folder_members_chain_directly () {
  let graph_nodes : Vec<Graphnode> = vec! [
    Graphnode { contains : ids (&["e"]), .. node ("q") },
    Graphnode { forks : msv_ids (&["e"]), .. node ("s") },
    node ("e"), ];
  let straddle : GraphStraddle = straddle (&graph_nodes, &graph_nodes);
  let processed_store : ProcessedStore = ProcessedStore::default ();
  let input : String = formatdoc! ("
    * {} s
    ** (skg forksFolder)
    ", occurrence_metadata ("s", "") );
  assert_eq! ( run_asserting_fixpoint_in (
    &input, &straddle . after, &unrestricted (),
    RefreshMode::DiffView { straddle        : &straddle,
                            buffer_kind     : DiffViewBufferKind::AllChanges,
                            processed_store : &processed_store },
    100 ) . outline (), "\
node s
  forksFolder
    node e wp
      node q wp noClaim" );
  assert_eq! ( run_asserting_fixpoint (&input, &graph (&graph_nodes)) . outline (), "\
node s
  forksFolder
    node e wp
      containerFolder
        node q wp noClaim folded",
    "a content view puts the chain in a containers folder" ); }
