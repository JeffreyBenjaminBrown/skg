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
  ghost d" );
  let d : NodeRef<Viewnode> = run . forest . roots () . next () . unwrap ()
    . children () . nth (2) . unwrap ();
  assert_eq! ( d . value () . viewnode_id, Some ( ViewnodeId ("dd" . into ()) ));
  assert_eq! ( d . value () . change_facts . change . node, Some (Existence::Removed) );
  assert_eq! ( d . value () . change_facts . change . rel, Some (Existence::Removed) ); }

#[test]
fn a_relationship_head_alone_has_is_drawn_with_a_ghost_warrant_and_no_claim () {
  let straddle : GraphStraddle = straddle (
    &[ Graphnode { contains : ids (&["m"]), .. node ("p") }, node ("m") ],
    &[ node ("p"), node ("m") ] );
  let run : Run = diff_run_asserting_fixpoint ( &format! ("* {} p\n", occurrence_metadata ("p", "")), &straddle );
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

#[test]
fn a_missing_change_viewnode_is_drawn () {
  let straddle : GraphStraddle = straddle (
    &[ Graphnode { title : "old" . into (), body : Some ("was\n" . into ()),
                   .. node ("p") } ],
    &[ Graphnode { body : Some ("is\n" . into ()), .. node ("p") } ] );
  let run : Run = diff_run_asserting_fixpoint ( &format! ("* {} p\n", occurrence_metadata ("p", "")), &straddle );
  assert_eq! ( run . outline (), "\
node p
  oldText
    bodyDiff" ); }

#[test]
fn the_diff_view_substitutes_nothing () {
  let graph_nodes : Vec<Graphnode> = vec! [
    Graphnode { contains : ids (&["n"]), .. node ("p") },
    node ("n"),
    Graphnode { overrides : msv_ids (&["n"]), .. node ("o") }, ];
  let straddle : GraphStraddle = straddle (&graph_nodes, &graph_nodes);
  let run : Run = diff_run_asserting_fixpoint ( &format! ("* {} p\n", occurrence_metadata ("p", "")), &straddle );
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
  ghost d
    node d1 noClaim ᗣ
      containerFolder
        ghost d",
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
  ghost n",
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
       occurrence_metadata ("b", ""), occurrence_metadata ("d", ""), occurrence_metadata ("m", ""), occurrence_metadata ("q", "") );
  let run : Run = diff_run_asserting_fixpoint (&input, &straddle);
  assert! ( run . outline () . contains ("ghost d"), "{}", run . outline () ); }
