// The diff view: the goal also holds what HEAD alone warrants (a ghost
// for a node HEAD has and the worktree lacks, under the parent that
// contained it at HEAD; a ghost warrant, 'ByHead', on what HEAD alone
// relates to its parent); a ghost, a pending change, stays until it is
// restored or committed, and becomes an occurrence again, in place, when
// its node is back; change viewnodes are recomputed in place.

use super::*;
use crate::types::viewnode::Existence;
use indoc::formatdoc;

fn diff_run_asserting_fixpoint (
  text     : &str,
  straddle : &GraphStraddle,
) -> Run {
  let readiness_store : ReadinessStore = ReadinessStore::default ();
  run_asserting_fixpoint_in ( text, &straddle . after, None,
           CompletionMode::DiffView { straddle,
                                      buffer_kind : DiffViewBufferKind::AllChanges,
                                      readiness_store : &readiness_store },
           100 ) }

#[test]
fn an_occurrence_whose_node_head_has_and_the_worktree_lacks_becomes_its_ghost () {
  let straddle : GraphStraddle = straddle (
    &[ Graphnode { contains : ids (&["c", "d"]), .. node ("p") },
       node ("c"), node ("d") ],
    &[ Graphnode { contains : ids (&["c"]), .. node ("p") },
       node ("c") ] );
  let input : String = formatdoc! ("
    * {} p
    ** {} c
    ** (skg (vnid dd) node (gnid d) (skgRepos (nodeRepo main))) d
    ", occurrence_metadata ("p", ""), occurrence_metadata ("c", "writeProtected") );
  let run : Run = diff_run_asserting_fixpoint (&input, &straddle);
  assert_eq! ( run . outline (), "\
node p
  changesetMarker content
  node c wp
  ghost d noClaim ᗣ",
    "p's list no longer names d, so its ghost claims nothing" );
  let d : NodeRef<Viewnode> = run . forest . roots () . next () . unwrap ()
    . children () . nth (2) . unwrap ();
  assert_eq! ( d . value () . viewnode_id, Some ( ViewnodeId ("dd" . into ()) ));
  assert_eq! ( d . value () . change_facts . change . node, Some (Existence::Removed) );
  assert_eq! ( d . value () . change_facts . change . rel, Some (Existence::Removed) ); }

/// A dangling reference: the worktree deleted d but p still lists it.
/// The occurrence re-kinded to d's ghost keeps its claim, since the
/// list names it, so that a save keeps that list as it is.
#[test]
fn an_occurrence_re_kinded_to_a_ghost_claims_where_the_list_still_names_it () {
  let straddle : GraphStraddle = straddle (
    &[ Graphnode { contains : ids (&["d"]), .. node ("p") }, node ("d") ],
    &[ Graphnode { contains : ids (&["d"]), .. node ("p") } ] );
  let input : String = formatdoc! ("
    * {} p
    ** {} d
    ", occurrence_metadata ("p", ""), occurrence_metadata ("d", "") );
  let run : Run = diff_run_asserting_fixpoint (&input, &straddle);
  assert_eq! ( run . outline (), "\
node p
  ghost d" );
  let d : NodeRef<Viewnode> =
    run . forest . roots () . next () . unwrap () . first_child () . unwrap ();
  let ViewnodeKind::Vognode (Vognode::Ghost (ghost)) = &d . value () . kind
    else { panic! ("d is no ghost: {:?}", d . value () . kind) };
  assert! ( ghost . claimsMembership );
  assert_eq! ( ghost . warrant, Warrant::ByGraph );
  assert! ( ! run . text . contains ("noClaim"), "{}", run . text ); }

#[test]
fn a_relationship_head_alone_has_is_drawn_with_a_ghost_warrant_and_no_claim () {
  let straddle : GraphStraddle = straddle (
    &[ Graphnode { contains : ids (&["m"]), .. node ("p") }, node ("m") ],
    &[ node ("p"), node ("m") ] );
  let run : Run = diff_run_asserting_fixpoint (
    &format! ("* {} p\n", occurrence_metadata ("p", "")), &straddle );
  assert_eq! ( run . outline (), "\
node p
  changesetMarker content
  node m noClaim ᗣ
    containerFolder
      node p wp noClaim ᗣ" );
  assert! ( run . text . contains ("ghostWarrant"), "{}", run . text ); }

#[test]
fn a_ghost_whose_node_is_back_becomes_an_occurrence_in_place () {
  let straddle : GraphStraddle = straddle (
    &[ Graphnode { contains : ids (&["g"]), .. node ("p") }, node ("g") ],
    &[ Graphnode { contains : ids (&["g"]), .. node ("p") }, node ("g") ] );
  let input : String = formatdoc! ("
    * {} p
    ** (skg (vnid gg) ghost (gnid g) (skgRepos (nodeRepo main))) g
    ", occurrence_metadata ("p", "") );
  let run : Run = diff_run_asserting_fixpoint (&input, &straddle);
  assert_eq! ( run . outline (), "\
node p
  node g wp" );
  assert_eq! ( run . buffer_correction_kinds (), vec! [ BufferCorrectionKind::Rekinded (VognodeKind::Occurrence) ] ); }

/// The ghost asked that its node be restored, and the node is back:
/// the occurrence it becomes has no slot for the request.
#[test]
fn a_restoreNode_request_on_a_ghost_whose_node_is_back_is_dropped_and_reported () {
  let straddle : GraphStraddle = straddle (
    &[ Graphnode { contains : ids (&["g"]), .. node ("p") }, node ("g") ],
    &[ Graphnode { contains : ids (&["g"]), .. node ("p") }, node ("g") ] );
  let input : String = formatdoc! ("
    * {} p
    ** (skg (vnid gg) ghost (gnid g) (skgRepos (nodeRepo main)) (editRequest restoreNode)) g
    ", occurrence_metadata ("p", "") );
  let run : Run = diff_run_asserting_fixpoint (&input, &straddle);
  assert_eq! ( run . outline (), "\
node p
  node g wp" );
  assert_eq! ( run . buffer_correction_kinds (),
               vec! [ BufferCorrectionKind::EditRequestDropped,
                      BufferCorrectionKind::Rekinded (VognodeKind::Occurrence) ] ); }

/// d, marked for deletion, was deleted since HEAD by another means:
/// its ghost carries no delete, and the dropped request is reported.
#[test]
fn a_delete_on_an_occurrence_that_becomes_a_ghost_is_dropped_and_reported () {
  let straddle : GraphStraddle = straddle (
    &[ Graphnode { contains : ids (&["d"]), .. node ("p") }, node ("d") ],
    &[ node ("p") ] );
  let input : String = formatdoc! ("
    * {} p
    ** {} d
    ", occurrence_metadata ("p", ""),
       occurrence_metadata ("d", "(editRequest delete)") );
  let run : Run = diff_run_asserting_fixpoint (&input, &straddle);
  assert_eq! ( run . outline (), "\
node p
  changesetMarker content
  ghost d noClaim ᗣ" );
  let d : NodeRef<Viewnode> = run . forest . roots () . next () . unwrap ()
    . children () . nth (1) . unwrap ();
  let ViewnodeKind::Vognode (Vognode::Ghost (ghost)) = &d . value () . kind
    else { panic! ("d is no ghost: {:?}", d . value () . kind) };
  assert! ( ! ghost . restoreNode_request );
  assert_eq! ( run . buffer_correction_kinds (),
               vec! [ BufferCorrectionKind::EditRequestDropped,
                      BufferCorrectionKind::Rekinded (VognodeKind::Ghost) ] ); }

#[test]
fn a_property_head_alone_has_is_a_member_with_a_ghost_warrant () {
  let alias = |texts : &[&str]| -> MSV<RelPartner<String>> {
    MSV::Specified ( rel_partners_at_relRepo (
      &SkgrepoName::from ("main"),
      texts . iter () . map ( |text| text . to_string () ) . collect () )) };
  let straddle : GraphStraddle = straddle (
    &[ Graphnode { aliases : alias (&["a", "b"]), .. node ("p") } ],
    &[ Graphnode { aliases : alias (&["b"]), .. node ("p") } ] );
  let input : String = formatdoc! ("
    * {} p
    ** (skg aliasFolder)
    ", occurrence_metadata ("p", "") );
  let run : Run = diff_run_asserting_fixpoint (&input, &straddle);
  assert_eq! ( run . outline (), "\
node p
  aliasFolder
    alias a noClaim ᗣ
    alias b" ); }

#[test]
fn change_viewnodes_are_kept_while_their_change_exists_and_entombstoned_when_gone () {
  let straddle : GraphStraddle = straddle (
    &[ Graphnode { title : "old" . into (), contains : ids (&["c"]), .. node ("p") },
       node ("c") ],
    &[ Graphnode { contains : ids (&["c"]), .. node ("p") }, node ("c") ] );
  let input : String = formatdoc! ("
    * {} p
    ** (skg (vnid ot) (oldText title)) old
    ** (skg (changesetMarker content))
    ** {} c
    ", occurrence_metadata ("p", ""), occurrence_metadata ("c", "writeProtected") );
  let run : Run = diff_run_asserting_fixpoint (&input, &straddle);
  assert_eq! ( run . outline (), "\
node p
  oldText
  node c wp",
    "the old text stays; the content marker, whose change is gone, goes" );
  let old_text : NodeRef<Viewnode> =
    run . forest . roots () . next () . unwrap () . first_child () . unwrap ();
  assert_eq! ( old_text . value () . viewnode_id, Some ( ViewnodeId ("ot" . into ()) )); }

/// The user wrote a note, n, under the old text of p's title, and
/// saved it; the title change is then committed, so the old text is
/// gone. Its tombstone keeps the note.
#[test]
fn a_gone_change_viewnodes_tombstone_keeps_what_the_user_wrote_under_it () {
  let graph_nodes : Vec<Graphnode> = vec! [ node ("p"), node ("n") ];
  let straddle : GraphStraddle = straddle (&graph_nodes, &graph_nodes);
  let input : String = formatdoc! ("
    * {} p
    ** (skg (vnid ot) (oldText title)) old
    *** {} n
    ", occurrence_metadata ("p", ""), occurrence_metadata ("n", "") );
  let run : Run = diff_run_asserting_fixpoint (&input, &straddle);
  assert_eq! ( run . outline (), "\
node p
  tombstone
    node n noClaim" ); }

#[test]
fn a_missing_change_viewnode_is_drawn () {
  let straddle : GraphStraddle = straddle (
    &[ Graphnode { title : "old" . into (), body : Some ("was\n" . into ()),
                   .. node ("p") } ],
    &[ Graphnode { body : Some ("is\n" . into ()), .. node ("p") } ] );
  let run : Run = diff_run_asserting_fixpoint (
    &format! ("* {} p\n", occurrence_metadata ("p", "")), &straddle );
  assert_eq! ( run . outline (), "\
node p
  oldText
    bodyDiff" ); }

/// What a content view adds unasked, the diff view does not: s's
/// subscribee folder, though an editable-view request extends s, and
/// the folder of what s hides in e, though e is an editable subscribee
/// as such.
#[test]
fn the_diff_view_adds_no_default_folders () {
  let graph_nodes : Vec<Graphnode> = vec! [
    Graphnode { subscribesTo  : msv_ids (&["e"]),
                hidesFromSubs : msv_ids (&["h"]), .. node ("s") },
    Graphnode { contains : ids (&["h"]), .. node ("e") },
    node ("h"),
    Graphnode { subscribesTo : msv_ids (&["e"]), .. node ("u") }, ];
  let straddle : GraphStraddle = straddle (&graph_nodes, &graph_nodes);
  let input : String = formatdoc! ("
    * {} s
    ** (skg subscribeeFolder)
    *** {} e
    * {} u
    ", occurrence_metadata ("s", ""), occurrence_metadata ("e", ""),
       occurrence_metadata ("u", "(viewRequests editableView)") );
  assert_eq! ( diff_run_asserting_fixpoint (&input, &straddle) . outline (), "\
node s
  subscribeeFolder
    node e
node u" );
  assert_eq! ( run_asserting_fixpoint (&input, &graph (&graph_nodes)) . outline (), "\
node s
  subscribeeFolder
    node e
      hiddenInSubscribeeFolder
        node h wp noClaim folded
          node e wp noClaim
node u
  subscribeeFolder
    node e wp",
    "a content view adds both" ); }

#[test]
fn the_diff_view_substitutes_nothing () {
  let graph_nodes : Vec<Graphnode> = vec! [
    Graphnode { contains : ids (&["n"]), .. node ("p") },
    node ("n"),
    Graphnode { overrides : msv_ids (&["n"]), .. node ("o") }, ];
  let straddle : GraphStraddle = straddle (&graph_nodes, &graph_nodes);
  let run : Run = diff_run_asserting_fixpoint (
    &format! ("* {} p\n", occurrence_metadata ("p", "")), &straddle );
  assert_eq! ( run . outline (), "\
node p
  node n wp" ); }

#[test]
fn a_ghost_where_content_stands_holds_its_old_content () {
  let straddle : GraphStraddle = straddle (
    &[ Graphnode { contains : ids (&["d"]), .. node ("p") },
       Graphnode { contains : ids (&["d1"]), .. node ("d") },
       node ("d1") ],
    &[ node ("p"), node ("d1") ] );
  let input : String = formatdoc! ("
    * {} p
    ** {} d
    ", occurrence_metadata ("p", ""), occurrence_metadata ("d", "writeProtected") );
  let run : Run = diff_run_asserting_fixpoint (&input, &straddle);
  assert_eq! ( run . outline (), "\
node p
  changesetMarker content
  ghost d noClaim ᗣ
    node d1 noClaim ᗣ
      containerFolder
        ghost d noClaim ᗣ",
    "d1, which lost its container, is in U, so it is editable, and \
     shows the container it lost" ); }

#[test]
fn a_deleted_node_that_its_parent_listed_by_an_extra_id_becomes_its_ghost () {
  // As 're_kind.rs' has it in a content view; here HEAD knows that x
  // was n's extra ID.
  let straddle : GraphStraddle = straddle (
    &[ Graphnode { contains : ids (&["x"]), .. node ("p") },
       Graphnode { extra_ids : vec! [ ID::from ("x") ], .. node ("n") } ],
    &[ Graphnode { contains : ids (&["x"]), .. node ("p") } ] );
  let input : String = formatdoc! ("
    * {} p
    ** (skg (vnid nn) node (gnid n) (skgRepos (nodeRepo main))) n
    ", occurrence_metadata ("p", "") );
  let run : Run = diff_run_asserting_fixpoint (&input, &straddle);
  assert_eq! ( run . outline (), "\
node p
  changesetMarker content
  ghost n noClaim ᗣ",
    "n's occurrence is its ghost, where it stood, and by the ID rule \
     the worktree's x, which HEAD resolves to n, is that ghost too" ); }

#[test]
fn a_buffer_of_many_changes_is_a_fixpoint () {
  // a acquired b by a nodeMerge; d was deleted; m left p's content;
  // p's title, aliases and flags changed; q's relationship to p moved
  // to another skgrepo; new joined p's content.
  let alias = |texts : &[&str]| -> MSV<RelPartner<String>> {
    MSV::Specified ( rel_partners_at_relRepo (
      &SkgrepoName::from ("main"),
      texts . iter () . map ( |text| text . to_string () ) . collect () )) };
  let private_q : Vec<RelPartner<ID>> = vec! [
    RelPartner::at_relRepo ( SkgrepoName::from ("private"), ID::from ("q") ) ];
  let straddle : GraphStraddle = straddle (
    &[ Graphnode { title    : "old p" . into (),
                   contains : ids (&["b", "d", "m", "q"]),
                   aliases  : alias (&["one"]), .. node ("p") },
       node ("a"), node ("b"),
       Graphnode { contains : ids (&["d1"]), .. node ("d") },
       node ("d1"), node ("m"), node ("q"),
       Graphnode { contains : ids (&["p"]), .. node ("top") } ],
    &[ Graphnode { contains : [ ids (&["b", "new"]), private_q ] . concat (),
                   aliases  : alias (&["two"]),
                   flags    : vec! [ crate::types::nodes::complete::Flag::NoSearchMatching ],
                   .. node ("p") },
       Graphnode { extra_ids : vec! [ ID::from ("b") ], .. node ("a") },
       node ("d1"), node ("m"), node ("q"), node ("new"),
       Graphnode { contains : ids (&["p"]), .. node ("top") } ] );
  let input : String = formatdoc! ("
    * {} p
    ** (skg aliasFolder)
    ** {} b
    ** {} d
    ** {} m
    ** {} q
    ", occurrence_metadata ("p", "(viewRequests (folder containers) flags)"),
       occurrence_metadata ("b", ""), occurrence_metadata ("d", ""),
       occurrence_metadata ("m", ""), occurrence_metadata ("q", "") );
  let run : Run = diff_run_asserting_fixpoint (&input, &straddle);
  assert_eq! ( run . outline (), "\
node p
  oldText
  containerFolder
    node top wp noClaim
  flagsFolder
    flag noSearchMatching
  changesetMarker content
  changesetMarker relRepo
  aliasFolder
    alias one noClaim ᗣ
    alias two
  ghost b noClaim ᗣ
    acquiredByFolder
      node a wp noClaim
  ghost d noClaim ᗣ
    node d1 noClaim ᗣ
      containerFolder
        ghost d noClaim ᗣ
  node m noClaim ᗣ
    containerFolder
      node p wp noClaim ᗣ
        node top wp noClaim
  node a
    acquiredFolder
      ghost b noClaim ᗣ
    idFolder
      id a noClaim
      id b noClaim
    containerFolder
      node p wp noClaim
        node top wp noClaim
  node new
    containerFolder
      node p wp noClaim
        node top wp noClaim
  node q
    relRepoMove" ); }
