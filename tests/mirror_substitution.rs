// cargo nextest run --test grouped_forks -E 'test(mirror_substitution::)'
//
// Mirror substitution ('docs/mirrors.org', "Mirror substitution"):
// when the refresh would draw a viewnode for node N as recursive
// content, and the perspective's skgrepo list ranks a mirror R of N
// first, it shows R instead, marked '(inPlaceOf N)' -- and saving such a view
// round-trips to the original IDs, so a container's contains list is
// never silently rewritten from N to R.
//
// Fixture (tests/mirror_substitution/fixtures, single skgrepo
// "main", owned): Q contains P and P2; P contains N and M;
// R forks N and contains W; S forks E; E contains N. In one skgrepo
// N always stands as itself.
//
// The apart fixture (fixtures-apart) is the same graph with N homed
// in a second owned skgrepo, "base", so that a view of P, drawn from
// "main", shows R in N's place.
//
// The multi-repo fixture (fixtures-multi) adds the skgrepo and
// visibility gates: FR (skgrepo "foreign", not owned) forks
// N1; R2 (skgrepo "other", owned) forks N2; N3 lives in
// "other" while P3 and its fork R3 live in "main".
//
// The foreign-root fixture (fixtures-foreign-root) gives a view its
// root's home skgrepo's perspective: R (skgrepo "foreign", not owned)
// contains M (skgrepo "origin", foreign) and K (foreign); A, in
// "foreign", and U, in "main", each fork M; P, in "main", contains M,
// and W, in "main", contains U. A third foreign skgrepo, "third",
// forks M with B and K with T. Foreign F2 contains M2, which contains
// N2; U2, in "main", forks M2, and T2, third's, N2. The config
// declares foreign, main, third, origin.
//
// The replaced-root-folders fixture (fixtures-replaced-root-folders)
// gives a foreign root and the user's fork of it different folders:
// foreign L, titled "missive", alias "letter", contains L1, L2 and L3;
// foreign Q contains L, and foreign X links to it. J, in "main",
// titled "answer", alias "reply",
// forks L, contains L1 and hides L2; V, in
// "main", contains J, and Y, in "main", links to it.

use skg::test_utils::{save_added_forks, save_and_rerender};
use indoc::indoc;
use std::error::Error;
use crate::tcp_stream_pair::mk_test_tcp_stream_pair;
use std::net::TcpStream;

use skg::from_text::buffer_to_validated_saveplan;
use skg::assert_metadata_eq;
use skg::serve::ViewsState;
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::test_utils::{
  run_with_shared_test_stores,
  graph_handle_from_config};
use skg::test_utils::{
  multi_root_view, multi_root_view_from_perspective, multi_root_view_with_skgrepo_set};
use skg::types::errors::{BufferValidationError, SaveError};
use skg::types::config::SkgConfig;
use skg::types::misc::{ID, SkgrepoName, TantivyIndex, members_of};
use skg::types::nodes::complete::Graphnode;
use skg::types::save::{NodeInstruction, SaveNode};
use skg::types::views_state::{OpenViews, Perspective, ViewforestId};
use skg::serve::handlers::perspective::handle_set_perspective_request;
use skg::serve::handlers::rerender_views::handle_rerender_views_request;
use skg::serve::handlers::text_search::handle_text_search_request;
use skg::serve::handlers::save_buffer::{
  SaveResponse, update_from_and_rerender_buffer_with_approvals};
use skg::from_text::strandeds::StrandedsReview;
use skg::save::DeleteFrom;
use skg::from_text::buffer_to_validated_saveplan_with_fork_skgrepos_in_graph;
use skg::serve::handlers::save_staleness::WhatTheBaselineShows;
use skg::test_utils::{
  extract_string_field_from_sexp, read_all_lp_messages, skg_env_from_parts};
use skg::types::env::SkgEnv;
use std::collections::{HashMap, HashSet};
use std::io::BufReader;

use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_skgrepos;
use skg::dbs::tantivy::write::update_index_with_nodes;
use skg::types::nodes::tantivy::GraphnodeInTantivy;
use skg::dbs::in_rust_graph::{InRustGraph, InRustGraphHandle};
use skg::types::viewnode::Viewnode;
use ego_tree::Tree;

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str = "tests/mirror_substitution/fixtures";
  run_with_shared_test_stores (
    "skg-test-mirror-substitution",
    |s| Box::pin ( async move {
      let apart : &str = "tests/mirror_substitution/fixtures-apart";
      s . reset ("de_novo_rendering_shows_the_mirror_marked", apart) ?;
      de_novo_rendering_shows_the_mirror_marked (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("save_roundtrips_to_original_and_is_a_fixpoint", apart) ?;
      save_roundtrips_to_original_and_is_a_fixpoint (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("extraction_honors_inPlaceOf", fixtures) ?;
      extraction_honors_inPlaceOf (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_second_owned_fork_saves_and_the_original_stands", fixtures) ?;
      a_second_owned_fork_saves_and_the_original_stands (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("an_owned_mutual_pair_saves_and_the_original_stands", fixtures) ?;
      an_owned_mutual_pair_saves_and_the_original_stands (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset_from_config ("the_perspectives_skgrepo_and_visibility_gate_substitution",
                             "tests/mirror_substitution/fixtures-multi/skgconfig.toml") ?;
      the_perspectives_skgrepo_and_visibility_gate_substitution (
        &s . config ) . await ?;
      s . reset_from_config ("chain_half_visible_keeps_the_original",
                             "tests/mirror_substitution/fixtures-chain/skgconfig.toml") ?;
      chain_half_visible_keeps_the_original (
        &s . config, &mut s . tantivy ) . await ?;
      let foreign_root : &str =
        "tests/mirror_substitution/fixtures-foreign-root";
      s . reset ("a_view_follows_the_perspective_of_its_roots_home", foreign_root) ?;
      a_view_follows_the_perspective_of_its_roots_home (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("under_a_foreign_root_a_save_roundtrips_its_homes_mirror", foreign_root) ?;
      under_a_foreign_root_a_save_roundtrips_its_homes_mirror (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("the_tamper_check_accepts_any_mirror_and_refuses_a_non_mirror", foreign_root) ?;
      the_tamper_check_accepts_any_mirror_and_refuses_a_non_mirror (
        &s . config ) . await ?;
      s . reset ("under_a_foreign_root_a_forked_child_is_shown_as_the_original", foreign_root) ?;
      under_a_foreign_root_a_forked_child_is_shown_as_the_original (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_forked_foreign_root_follows_its_clones_home", foreign_root) ?;
      a_forked_foreign_root_follows_its_clones_home (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_switched_view_follows_a_skgrepos_perspective_until_switched_back", foreign_root) ?;
      a_switched_view_follows_a_skgrepos_perspective_until_switched_back (
        &s . config, &mut s . tantivy ) ?;
      s . reset ("a_switch_of_perspective_re_points_a_substituted_content_child", foreign_root) ?;
      a_switch_of_perspective_re_points_a_substituted_content_child (
        &s . config, &mut s . tantivy ) ?;
      s . reset ("a_re_pointed_content_child_draws_its_forks_folder_afresh",
                 "tests/mirror_substitution/fixtures-re-pointed-child") ?;
      a_re_pointed_content_child_draws_its_forks_folder_afresh (
        &s . config, &mut s . tantivy ) ?;
      s . reset ("the_first_declared_mirror_stands_where_the_perspectives_skgrepo_holds_none",
                 "tests/mirror_substitution/fixtures-third-author") ?;
      the_first_declared_mirror_stands_where_the_perspectives_skgrepo_holds_none (
        &s . config, &mut s . tantivy ) ?;
      s . reset ("a_switch_to_a_skgrepo_is_refused_where_it_would_change_nothing", foreign_root) ?;
      a_switch_to_a_skgrepo_is_refused_where_it_would_change_nothing (
        &s . config, &mut s . tantivy ) ?;
      s . reset ("next_cycles_through_every_perspective_that_shows_the_view_differently", foreign_root) ?;
      next_cycles_through_every_perspective_that_shows_the_view_differently (
        &s . config, &mut s . tantivy ) ?;
      s . reset ("a_perspective_stays_in_the_cycle_where_another_hides_what_it_forks", foreign_root) ?;
      a_perspective_stays_in_the_cycle_where_another_hides_what_it_forks (
        &s . config, &mut s . tantivy ) ?;
      s . reset ("the_root_homes_are_named_in_the_order_of_their_first_roots", foreign_root) ?;
      the_root_homes_are_named_in_the_order_of_their_first_roots (
        &s . config, &mut s . tantivy ) ?;
      s . reset ("next_from_a_perspective_that_left_the_cycle_goes_back_to_the_root_homes", foreign_root) ?;
      next_from_a_perspective_that_left_the_cycle_goes_back_to_the_root_homes (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_perspective_replaces_the_root_and_the_root_homes_restore_it", foreign_root) ?;
      a_perspective_replaces_the_root_and_the_root_homes_restore_it (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_replaced_root_shows_its_own_containers", foreign_root) ?;
      a_replaced_root_shows_its_own_containers (
        &s . config, &mut s . tantivy ) ?;
      let replaced_root_folders : &str =
        "tests/mirror_substitution/fixtures-replaced-root-folders";
      s . reset ("a_replaced_root_shows_its_own_default_folders", replaced_root_folders) ?;
      a_replaced_root_shows_its_own_default_folders (
        &s . config, &mut s . tantivy ) ?;
      s . reset ("a_replaced_root_redraws_the_folders_it_was_asked_for", replaced_root_folders) ?;
      a_replaced_root_redraws_the_folders_it_was_asked_for (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_replaced_search_hit_stays_a_hit", replaced_root_folders) ?;
      a_replaced_search_hit_stays_a_hit (
        &s . config, &mut s . tantivy ) ?;
      s . reset ("a_save_following_a_skgrepos_perspective_shows_a_new_fork_in_place", foreign_root) ?;
      a_save_following_a_skgrepos_perspective_shows_a_new_fork_in_place (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_held_view_follows_whatever_its_save_names", foreign_root) ?;
      a_held_view_follows_whatever_its_save_names (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("the_tamper_check_accepts_a_mirror_whatever_perspective_the_save_names", foreign_root) ?;
      the_tamper_check_accepts_a_mirror_whatever_perspective_the_save_names (
        &s . config ) ?;
      s . reset ("deleting_the_users_fork_in_a_foreign_parent_leaves_the_parent_and_shows_another_mirror", foreign_root) ?;
      deleting_the_users_fork_in_a_foreign_parent_leaves_the_parent_and_shows_another_mirror (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_deleted_mirror_keeps_its_original_but_a_removed_one_does_not", foreign_root) ?;
      a_deleted_mirror_keeps_its_original_but_a_removed_one_does_not (
        &s . config ) ?;
      Ok (( )) } )) }

fn marked_lines<'a> (
  buf      : &'a str,
  original : &str,
) -> Vec<&'a str> {
  let fact : String =
    format! ("(inPlaceOf {})", original);
  buf . lines ()
    . filter ( |l| l . contains (&fact) )
    . collect () }

fn saved_node_by_gnid<'a> (
  instructions : &'a [NodeInstruction],
  gnid           : &str,
) -> &'a Graphnode {
  opt_saved_node_by_gnid (instructions, gnid)
    . unwrap_or_else ( || panic! ("SaveNode not found: {}", gnid) ) }

fn opt_saved_node_by_gnid<'a> (
  instructions : &'a [NodeInstruction],
  gnid           : &str,
) -> Option<&'a Graphnode> {
  for instruction in instructions {
    if let NodeInstruction::Save (SaveNode (node)) = instruction {
      if node . pid == ID::from (gnid) {
        return Some (node); }}}
  None }

async fn node_instructions_from (
  buffer : &str,
  config : &SkgConfig,
) -> Result<Vec<NodeInstruction>, SaveError> {
  Ok ( buffer_to_validated_saveplan (
         buffer, config, &SkgrepoRestriction::unrestricted (config) )  ?
       . 1 . node_instructions ) }

/// The text of 'pid''s .skg file in the skgrepo "main".
fn read_fixture_file (
  config : &SkgConfig,
  pid    : &str,
) -> String {
  read_skgrepo_file (config, "main", pid) }

async fn de_novo_rendering_shows_the_mirror_marked (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      (
        graph_handle_from_config (config) ? );
      let (view, _pids, _tree) =
        multi_root_view (
          config, Some (tantivy),
          &[ ID::from ("P") ] ) ?;
      { let marked : Vec<&str> = marked_lines (&view, "N");
        assert_eq! ( marked . len (), 1,
          "exactly one shown substitute for N:\n{}", view );
        assert! ( marked [0] . contains ("(gnid R)"),
          "the substitute is R:\n{}", view ); }
      assert! ( view . lines () . any (
                  |l| l . contains ("(gnid W)") ),
        "R's own content W is shown beneath it:\n{}", view );
      assert! ( view . lines () . any (
                  |l| l . contains ("(gnid M)")
                      && ! l . contains ("inPlaceOf") ),
        "M, not forked, is shown raw:\n{}", view );
      Ok (( )) }

async fn save_roundtrips_to_original_and_is_a_fixpoint (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      (
        graph_handle_from_config (config) ? );
      let (de_novo, _pids, _tree) =
        multi_root_view (
          config, Some (tantivy),
          &[ ID::from ("P") ] ) ?;
      let saved : String =
        save_and_rerender (&de_novo, config, tantivy)
        . await ?;
      { let p_file : String = read_fixture_file (config, "P");
        assert! ( p_file . contains ("- N"),
          "P still contains N after the save:\n{}", p_file );
        assert! ( ! p_file . contains ("- R"),
          "P must NOT have been rewritten to contain R:\n{}",
          p_file ); }
      { let r_file : String = read_fixture_file (config, "R");
        assert! ( r_file . contains ("- W"),
          "R's own contains is untouched:\n{}", r_file ); }
      assert_eq! ( marked_lines (&saved, "N") . len (), 1,
        "the rerendered saved view still shows marked R:\n{}",
        saved );
      let saved_again : String =
        save_and_rerender (&saved, config, tantivy)
        . await ?;
      assert_metadata_eq! ( saved, saved_again,
        "a second save is a noop (the fixpoint through the \
         collected-ID orderkey)" );
      Ok (( )) }

/// In its own skgrepo a note stands as itself, however many of its
/// forks are there (rule 1 of 'docs/mirrors.org', "Which mirror
/// stands"): a view of P, drawn from "main", shows N, R only in its
/// forks folder. M, in N's skgrepo, comes to fork N beside R (saved
/// through the save's graph update); N's three mirrors tie there, and
/// N still stands as itself, and M, whose mirrors are the same three,
/// as itself. R and M stand in N's forks folder, forkers claiming
/// nothing; R stands in M's too, an indirect member, born of nothing.
async fn a_second_owned_fork_saves_and_the_original_stands (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (before, _pids, _tree) =
    multi_root_view ( config, Some (tantivy), &[ ID::from ("P") ] ) ?;
  assert! ( marked_lines (&before, "N") . is_empty ()
            && before . lines () . filter ( |l| l . contains ("(gnid R)") )
               . all ( |l| l . contains ("(birth (forks out 2))") ),
            "before the save, N stands as itself, R only in its forks folder:\n{}", before );
  save_added_forks (
    &ID::from ("M"), &[ ID::from ("N") ], config, tantivy ) . await ?;
  assert! ( read_fixture_file (config, "M") . contains ("- N"),
            "M forks N on disk:\n{}", read_fixture_file (config, "M") );
  let (after, _pids, _tree) =
    multi_root_view ( config, Some (tantivy), &[ ID::from ("P") ] ) ?;
  assert! ( marked_lines (&after, "N") . is_empty ()
            && marked_lines (&after, "M") . is_empty (),
            "nothing stands in N's or M's place:\n{}", after );
  let r_lines : Vec<&str> = after . lines ()
    . filter ( |line| line . contains ("(gnid R)") ) . collect ();
  assert! ( line_of_gnid (&after, "N") . is_some ()
            && line_of_gnid (&after, "M") . is_some ()
            && r_lines . len () == 2
            && r_lines [0] . contains ("(birth (forks out 2))")
            && ! r_lines [1] . contains ("birth"),
            "N and M are shown as themselves, R only as a forker in N's forks folder and an indirect member in M's:\n{}", after );
  Ok (( )) }

/// N comes to fork R, which forks N: an owned mutual pair, in
/// one skgrepo (saved as above). N stands as itself there (rule 1 of
/// 'docs/mirrors.org', "Which mirror stands"); R
/// is drawn in N's forks folder, a mutual, born of both sides.
async fn an_owned_mutual_pair_saves_and_the_original_stands (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  save_added_forks (
    &ID::from ("N"), &[ ID::from ("R") ], config, tantivy ) . await ?;
  assert! ( read_fixture_file (config, "N") . contains ("- R"),
            "N forks R on disk:\n{}", read_fixture_file (config, "N") );
  let (after, _pids, _tree) =
    multi_root_view ( config, Some (tantivy), &[ ID::from ("P") ] ) ?;
  assert! ( marked_lines (&after, "N") . is_empty ()
            && line_of_gnid (&after, "N") . is_some (),
            "N is shown as itself:\n{}", after );
  assert! ( line_of_gnid (&after, "R")
            . is_some_and ( |l| l . contains ("(birth (forks in 2) (forks out 2))") ),
            "R is shown only as N's mutual, in its forks folder:\n{}", after );
  Ok (( )) }

async fn extraction_honors_inPlaceOf (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      (
        graph_handle_from_config (config) ? );
      { // A marked child collects its original: P's collected
        // contains equals disk ([N, M]), so the noop filter drops
        // P's nodeInstruction entirely -- the round-trip in its
        // strongest form.
        let buffer = indoc! {"
          * (skg node (gnid P) (skgRepos (nodeRepo main))) P
          ** (skg node (gnid R) (skgRepos (nodeRepo main)) writeProtected (inPlaceOf N)) R
          ** (skg node (gnid M) (skgRepos (nodeRepo main)) writeProtected) M
        "};
        assert! (
          opt_saved_node_by_gnid (
            & node_instructions_from (buffer, config) . await ?,
            "P" ) . is_none (),
          "P's contains is unchanged (N, M), so no instruction \
           touches P" ); }
      { // Deleting the shown child deletes the original member.
        let buffer = indoc! {"
          * (skg node (gnid P) (skgRepos (nodeRepo main))) P
          ** (skg node (gnid M) (skgRepos (nodeRepo main)) writeProtected) M
        "};
        assert_eq! (
          members_of ( & saved_node_by_gnid (
            & node_instructions_from (buffer, config) . await ?,
            "P" ) . contains ),
          vec![ ID::from ("M") ] ); }
      { // Reordering the shown child positions the original.
        let buffer = indoc! {"
          * (skg node (gnid P) (skgRepos (nodeRepo main))) P
          ** (skg node (gnid M) (skgRepos (nodeRepo main)) writeProtected) M
          ** (skg node (gnid R) (skgRepos (nodeRepo main)) writeProtected (inPlaceOf N)) R
        "};
        assert_eq! (
          members_of ( & saved_node_by_gnid (
            & node_instructions_from (buffer, config) . await ?,
            "P" ) . contains ),
          vec![ ID::from ("M"), ID::from ("N") ] ); }
      { // Moving the shown child to another parent moves the original.
        let buffer = indoc! {"
          * (skg node (gnid Q) (skgRepos (nodeRepo main))) Q
          ** (skg node (gnid P) (skgRepos (nodeRepo main))) P
          *** (skg node (gnid M) (skgRepos (nodeRepo main)) writeProtected) M
          ** (skg node (gnid P2) (skgRepos (nodeRepo main))) P2
          *** (skg node (gnid R) (skgRepos (nodeRepo main)) writeProtected (inPlaceOf N)) R
        "};
        let instructions : Vec<NodeInstruction> =
          node_instructions_from (buffer, config) . await ?;
        assert_eq! (
          members_of ( & saved_node_by_gnid (&instructions, "P") . contains ),
          vec![ ID::from ("M") ] );
        assert_eq! (
          members_of ( & saved_node_by_gnid (&instructions, "P2") . contains ),
          vec![ ID::from ("N") ] ); }
      { // Edits to the shown R save to R; N is untouched.
        let buffer = indoc! {"
          * (skg node (gnid P) (skgRepos (nodeRepo main))) P
          ** (skg node (gnid R) (skgRepos (nodeRepo main)) (inPlaceOf N)) R-edited
          *** (skg node (gnid W) (skgRepos (nodeRepo main)) writeProtected) W
          ** (skg node (gnid M) (skgRepos (nodeRepo main)) writeProtected) M
        "};
        let instructions : Vec<NodeInstruction> =
          node_instructions_from (buffer, config) . await ?;
        assert_eq! (
          saved_node_by_gnid (&instructions, "R") . title,
          "R-edited" );
        assert! (
          opt_saved_node_by_gnid (&instructions, "N") . is_none (),
          "no instruction touches N" );
        assert! (
          opt_saved_node_by_gnid (&instructions, "P") . is_none (),
          "P's contains is unchanged, so P noops" ); }
      { // A legacy buffer showing N raw still saves identically
        // (a noop, like the marked equivalent above).
        let buffer = indoc! {"
          * (skg node (gnid P) (skgRepos (nodeRepo main))) P
          ** (skg node (gnid N) (skgRepos (nodeRepo main)) writeProtected) N
          ** (skg node (gnid M) (skgRepos (nodeRepo main)) writeProtected) M
        "};
        assert! (
          opt_saved_node_by_gnid (
            & node_instructions_from (buffer, config) . await ?,
            "P" ) . is_none () ); }
      { // Tamper: a fact the server would not have rendered aborts.
        let buffer = indoc! {"
          * (skg node (gnid P) (skgRepos (nodeRepo main))) P
          ** (skg node (gnid M) (skgRepos (nodeRepo main)) writeProtected (inPlaceOf W)) M
        "};
        match node_instructions_from (buffer, config) . await {
          Err (SaveError::BufferValidationErrors { errors, .. }) => {
            assert! ( errors . iter () . any ( |e| matches! (
              e, BufferValidationError::InPlaceOf_Mismatch (..) )),
              "expected InPlaceOf_Mismatch, got: {:?}", errors ); },
          other => panic! (
            "tampered fact must abort the save; got: {:?}",
            other . map ( |v| v . len () ) ), }}
      { // Forkee-as-such: the visible-content signal speaks of
        // the original, so no spurious hide of N is inferred.
        let buffer = indoc! {"
          * (skg node (gnid S) (skgRepos (nodeRepo main))) S
          ** (skg forksFolder)
          *** (skg node (gnid E) (skgRepos (nodeRepo main))) E
          **** (skg node (gnid R) (skgRepos (nodeRepo main)) writeProtected (inPlaceOf N)) R
        "};
        let instructions : Vec<NodeInstruction> =
          node_instructions_from (buffer, config) . await ?;
        if let Some (s_node) = opt_saved_node_by_gnid (&instructions, "S") {
          assert! (
            ! s_node . hides . or_default ()
              . iter () . any ( |m| &m . member == &ID::from ("N") ),
            "S must not hide N: the shown R stands for N" ); }}
      { // Deleting R where it stands for N there deletes R. It hides
        // nothing from S: P, an owned note, keeps N, so a forkee as
        // such never shows N ('docs/forks.org', "What you keep
        // elsewhere"), and a buffer drawn before that rule, as this
        // one is, which shows it, cuts nothing the view drew. Nor is N
        // filed in S.
        let buffer = indoc! {"
          * (skg node (gnid S) (skgRepos (nodeRepo main))) S
          ** (skg forksFolder)
          *** (skg node (gnid E) (skgRepos (nodeRepo main))) E
          **** (skg node (gnid R) (skgRepos (nodeRepo main)) (inPlaceOf N) (editRequest delete)) R
        "};
        let instructions : Vec<NodeInstruction> =
          node_instructions_from (buffer, config) . await ?;
        assert! ( instructions . iter () . any ( |i| matches! (
                    i, NodeInstruction::Delete (d) if d . gnid == ID::from ("R") )),
          "R is deleted" );
        if let Some (s_node) = opt_saved_node_by_gnid (&instructions, "S") {
          assert! (
            ! s_node . hides . or_default ()
              . iter () . any ( |m| &m . member == &ID::from ("N") ),
            "S does not hide N, which P keeps" );
          assert! (
            ! s_node . contains
              . iter () . any ( |m| &m . member == &ID::from ("N") ),
            "S does not file N" ); }}
      Ok (( )) }

/// Which mirror stands is the perspective's skgrepo's choice, and
/// the skgrepo restriction gates it. FR, foreign, forks N1, which P1's
/// home, "main", holds: from the root homes N1 stands as itself; from
/// foreign's perspective FR stands. R2, in "other", forks N2, in
/// "main": from the root homes N2 stands; from other's, R2, unless
/// the skgrepo restriction keeps "other" out.
async fn the_perspectives_skgrepo_and_visibility_gate_substitution (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
      (
        graph_handle_from_config (config) ? );
      let unrestricted : SkgrepoRestriction = SkgrepoRestriction::unrestricted (config);
      { // A foreign fork stands only from its own skgrepo's perspective.
        let (view, _pids, _tree) =
          multi_root_view (
            config, None,
            &[ ID::from ("P1") ] ) ?;
        assert! ( marked_lines (&view, "N1") . is_empty (),
          "from the root homes, N1, in P1's home, is shown raw:\n{}", view );
        assert! ( view . contains ("(gnid N1)"), "{}", view );
        let foreigns : String = multi_root_view_from_perspective (
          config, &[ ID::from ("P1") ], &unrestricted, skgrepo ("foreign") ) ? . 0;
        let marked : Vec<&str> = marked_lines (&foreigns, "N1");
        assert! ( marked . len () == 1 && marked [0] . contains ("(gnid FR)"),
          "from foreign's perspective FR stands in N1's place:\n{}", foreigns ); }
      let restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (
          config, SkgrepoName ("main" . to_string ())) ?;
      { // From the root homes, N2, in P2's home, stands as itself.
        let (view, _pids, _tree) =
          multi_root_view (
            config, None,
            &[ ID::from ("P2") ] ) ?;
        assert! ( marked_lines (&view, "N2") . is_empty ()
                  && view . contains ("(gnid N2)"),
          "from the root homes N2 is shown raw:\n{}", view ); }
      { // From other's perspective R2 substitutes, unrestricted.
        let view : String = multi_root_view_from_perspective (
          config, &[ ID::from ("P2") ], &unrestricted, skgrepo ("other") ) ? . 0;
        let marked : Vec<&str> = marked_lines (&view, "N2");
        assert_eq! ( marked . len (), 1, "{}", view );
        assert! ( marked [0] . contains ("(gnid R2)"), "{}", view ); }
      { // A restricted owned fork does not substitute, even from its
        // own skgrepo's perspective.
        let view : String = multi_root_view_from_perspective (
          config, &[ ID::from ("P2") ], &restriction, skgrepo ("other") ) ? . 0;
        assert! ( marked_lines (&view, "N2") . is_empty (),
          "R2's repo is restricted; N2 is shown raw:\n{}", view );
        assert! ( view . contains ("(gnid N2)"), "{}", view ); }
      { // The skgrepo restriction beats substitution: restricted original,
        // recorded in an unrestricted section, unrestricted fork
        // -> the original stands as a Restricted vognode, and the
        // fork is not shown in its place.
        let (view, _pids, _tree) =
          multi_root_view_with_skgrepo_set (
            config, None,
            &[ ID::from ("P3") ], &restriction ) ?;
        assert! ( view . contains ("restricted (gnid N3)"),
          "the restricted original stands as a Restricted vognode:\n{}", view );
        assert! ( ! view . contains ("(gnid R3)"),
          "its fork must not be shown in its place (the \
           fact would name a restricted node):\n{}", view ); }
      Ok (( )) }

/// Save 'buf' under a specific skgrepo restriction (the test shims save
/// unrestricted). Asserts no save errors and returns the rerendered
/// view.
async fn save_under_set (
  buf     : &str,
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
  set     : &SkgrepoRestriction,
) -> Result<String, Box<dyn Error>> {
  let graph : InRustGraphHandle =
    graph_handle_from_config (config) ?;
  let mut env : skg::types::env::SkgEnv =
    skg::test_utils::skg_env_from_parts (
      config, tantivy, &graph );
  let mut views_state : ViewsState = ViewsState {
    open_views        : OpenViews::new (),
    graph_straddle    : None, };
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response =
    skg::serve::handlers::save_buffer::update_from_and_rerender_buffer (
      &mut stream, buf, &mut env, 
      &Err (String::new ()), &mut views_state,
      set, true,
      &std::collections::HashMap::new () ) . await ?;
  assert! ( response . errors . is_empty (),
    "save must not error; got: {:?}", response . errors );
  Ok ( response . saved_view ) }

/// An owned chain D forks C forks N, D in skgrepo "other", C and N in
/// "main" with P, which contains N. From the root homes ("main") N
/// stands as itself, unrestricted or not: the chain kept in N's
/// skgrepo shows N. From other's perspective the chain end D stands;
/// under "main" (keeping "other" out), N again, tying with C there.
/// A buffer in which the chain's middle C bears N's part, saved under
/// "main", is accepted (C is on N's chain) and keeps N in P's
/// contains; an off-chain fact is rejected.
async fn chain_half_visible_keeps_the_original (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      (
        graph_handle_from_config (config) ? );
      let main_set : SkgrepoRestriction =
        SkgrepoRestriction::named (
          config, SkgrepoName ("main" . to_string ())) ?;
      let unrestricted : SkgrepoRestriction = SkgrepoRestriction::unrestricted (config);
      for set in [ &unrestricted, &main_set ] {
        // From the root homes, N stands as itself.
        let (view, _p, _t) =
          multi_root_view_with_skgrepo_set (
            config, Some (tantivy),
            &[ ID::from ("P") ], set ) ?;
        assert! ( marked_lines (&view, "N") . is_empty ()
                  && view . contains ("(gnid N)"),
          "from the root homes N stands as itself:\n{}", view ); }
      { // From other's perspective, unrestricted, the chain end D.
        let view : String = multi_root_view_from_perspective (
          config, &[ ID::from ("P") ], &unrestricted, skgrepo ("other") ) ? . 0;
        let marked : Vec<&str> = marked_lines (&view, "N");
        assert! ( marked . len () == 1 && marked [0] . contains ("(gnid D)"),
          "from other's perspective the chain end D stands:\n{}", view ); }
      { // Under 'main', other's D is kept out; N and C tie in "main".
        let view : String = multi_root_view_from_perspective (
          config, &[ ID::from ("P") ], &main_set, skgrepo ("other") ) ? . 0;
        assert! ( marked_lines (&view, "N") . is_empty ()
                  && view . contains ("(gnid N)"),
          "under main, from other's perspective, N stands as itself:\n{}", view ); }
      { // Saving a buffer whose middle bearer C stands for N keeps N
        // (not C) in P's contains.
        let buffer = indoc! {"
          * (skg node (gnid P) (skgRepos (nodeRepo main))) P
          ** (skg node (gnid C) (skgRepos (nodeRepo main)) writeProtected (inPlaceOf N)) C
        "};
        let saved : String =
          save_under_set (buffer, config, tantivy, &main_set)
          . await ?;
        assert! ( marked_lines (&saved, "N") . is_empty ()
                  && saved . contains ("(gnid N)"),
          "the rerendered saved view, drawn from the root homes, shows N \
           itself:\n{}", saved );
        let p_file : String = read_skgrepo_file (config, "main", "P");
        assert! ( p_file . contains ("- N"),
          "P still contains N after the half-visible save:\n{}", p_file );
        assert! ( ! p_file . contains ("- C"),
          "P must NOT be rewritten to contain the bearer C:\n{}",
          p_file ); }
      { // A fact on a node not on N's chain is rejected.
        let buffer = indoc! {"
          * (skg node (gnid P) (skgRepos (nodeRepo main))) P
          ** (skg node (gnid D) (skgRepos (nodeRepo other)) writeProtected (inPlaceOf P)) D
        "};
        match node_instructions_from (buffer, config) . await {
          Err (SaveError::BufferValidationErrors { errors, .. }) =>
            assert! ( errors . iter () . any ( |e| matches! (
              e, BufferValidationError::InPlaceOf_Mismatch (..) )),
              "off-chain fact must abort; got: {:?}", errors ),
          other => panic! (
            "off-chain fact must abort the save; got: {:?}",
            other . map ( |v| v . len () ) ), }}
      Ok (( )) }

/// The text of 'pid''s .skg file in the skgrepo 'skgrepo'.
fn read_skgrepo_file (
  config  : &SkgConfig,
  skgrepo : &str,
  pid     : &str,
) -> String {
  let path : std::path::PathBuf =
    config . skgrepos . get ( &SkgrepoName::from (skgrepo) ) . unwrap ()
    . path . join ( format! ("{}.skg", pid) );
  std::fs::read_to_string (&path)
    . unwrap_or_else ( |e| panic! ("reading {:?}: {}", path, e) ) }

/// The one owned node on disk that forks 'original'.
fn owned_fork_on_disk (
  config   : &SkgConfig,
  original : &str,
) -> Result<Graphnode, Box<dyn Error>> {
  let forks : Vec<Graphnode> =
    read_all_skg_files_from_skgrepos (config) ?
    . into_iter ()
    . filter ( |node| config . skgrepo_is_owned (&node . home_skgrepo)
               && node . forks . or_default () . iter ()
               . any ( |m| m . member == ID::from (original) ) )
    . collect ();
  match forks . as_slice () {
    [one] => Ok (one . clone ()),
    _ => Err ( format! ( "expected one owned fork of {} on disk; found {:?}",
                         original,
                         forks . iter () . map ( |n| &n . pid )
                         . collect::<Vec<&ID>> () ) . into () ), }}

/// The view's line for the occurrence whose gnid is 'gnid'.
fn line_of_gnid<'a> (
  view : &'a str,
  gnid : &str,
) -> Option<&'a str> {
  let fact : String = format! ("(gnid {})", gnid);
  view . lines () . find ( |l| l . contains (&fact) ) }

/// As 'line_of_gnid', passing over a line that claims nothing: a
/// forker of a forkee as such, which that forkee, drawn editable, shows
/// in its own forks folder.
fn claiming_line_of_gnid<'a> (
  view : &'a str,
  gnid : &str,
) -> Option<&'a str> {
  let fact : String = format! ("(gnid {})", gnid);
  view . lines () . find ( |l| l . contains (&fact) && ! l . contains ("noClaim") ) }

/// R, in "foreign", contains M, in "origin"; A, in "foreign", and U,
/// in "main", each fork M; P, in "main", contains M. A view of R
/// shows A in M's place, a view of P shows U, and a view of both
/// shows each under its own root: each tree is drawn from its root's
/// home skgrepo.
async fn a_view_follows_the_perspective_of_its_roots_home (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  graph_handle_from_config (config) ?;
  let (view, _pids, _tree) =
    multi_root_view (
      config, Some (tantivy), &[ ID::from ("R"), ID::from ("P") ] ) ?;
  let marked : Vec<&str> = marked_lines (&view, "M");
  assert_eq! ( marked . len (), 2, "{}", view );
  assert! ( marked [0] . starts_with ("** ")
            && marked [0] . contains ("(gnid A)"),
    "under R, A, in R's home, is shown in M's place:\n{}", view );
  assert! ( ! marked [0] . contains ("writeProtected"),
    "and editable, so the substitution is the editable kind:\n{}", view );
  assert! ( marked [1] . starts_with ("** ")
            && marked [1] . contains ("(gnid U)"),
    "under P, U, in P's home, is shown in M's place:\n{}", view );
  Ok (( )) }

/// Saving a view of foreign R, unchanged, where A stands for M, writes
/// nothing to R, passes the inPlaceOf tamper check, and is a
/// fixpoint.
async fn under_a_foreign_root_a_save_roundtrips_its_homes_mirror (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  graph_handle_from_config (config) ?;
  let r_before : String = read_skgrepo_file (config, "foreign", "R");
  let (view, _pids, _tree) =
    multi_root_view ( config, Some (tantivy), &[ ID::from ("R") ] ) ?;
  assert! ( line_of_gnid (&view, "A")
            . is_some_and ( |l| l . contains ("(inPlaceOf M)") ),
    "{}", view );
  let saved : String = save_and_rerender (&view, config, tantivy) . await ?;
  assert_eq! ( read_skgrepo_file (config, "foreign", "R"), r_before,
    "R, still containing M, is untouched by the save" );
  assert! ( owned_fork_on_disk (config, "K") . is_err (),
    "an unchanged save forks nothing" );
  assert_metadata_eq! ( view, saved,
    "the save of an unchanged view of R is a fixpoint" );
  Ok (( )) }

/// The tamper check accepts any mirror of the original as its
/// bearer, whichever the tree's perspective would show: under owned
/// P, whose tree is drawn from "main" (which shows U in M's place),
/// A, a fork of M in "foreign", carrying
/// '(inPlaceOf M)', saves. K, no mirror of M, carrying it, aborts
/// the save, the refusal naming U, the mirror shown.
async fn the_tamper_check_accepts_any_mirror_and_refuses_a_non_mirror (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  graph_handle_from_config (config) ?;
  let borne_by = |bearer : &str| -> String { format! (
    "* (skg node (gnid P) (skgRepos (nodeRepo main))) P\n\
     ** (skg node (gnid {bearer}) (skgRepos (nodeRepo foreign)) (inPlaceOf M)) {bearer}\n" ) };
  assert! ( node_instructions_from (&borne_by ("A"), config) . await . is_ok (),
    "a mirror the tree's perspective would not show may still bear the part" );
  match node_instructions_from (&borne_by ("K"), config) . await {
    Err (SaveError::BufferValidationErrors { errors, .. }) =>
      assert! ( errors . iter () . any ( |e| matches! (
        e, BufferValidationError::InPlaceOf_Mismatch (
             Some (bearer), original, Some (shown), _ )
           if bearer == &ID::from ("K") && original == &ID::from ("M")
              && shown == &ID::from ("U") )),
        "a bearer that is no mirror of M must abort, naming U; got: {:?}",
        errors ),
    other => panic! (
      "a bearer that is no mirror of M must abort the save; got: {:?}",
      other . map ( |v| v . len () ) ), }
  Ok (( )) }

/// Intended: under a foreign root the view is drawn from the root's
/// home skgrepo, so a child of R the user edits there, which forks it
/// into the user's clone, is shown afterward as the original,
/// unedited, and not as the clone; the save's plan warns so. The
/// clone is shown where the view is drawn from the clone's skgrepo
/// (under an owned root, or under a fork of R:
/// 'a_forked_foreign_root_follows_its_clones_home').
async fn under_a_foreign_root_a_forked_child_is_shown_as_the_original (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  graph_handle_from_config (config) ?;
  let (view, _pids, _tree) =
    multi_root_view ( config, Some (tantivy), &[ ID::from ("R") ] ) ?;
  let k_line : &str = line_of_gnid (&view, "K")
    . ok_or_else ( || format! ("K is shown under R:\n{}", view) ) ?;
  assert! ( ! k_line . contains ("writeProtected"), "{}", view );
  let edited : String = view . replace (
    k_line, &k_line . replace (") K", ") K-edited") );
  { // The save plan warns that the view goes on showing the original.
    let graph : InRustGraph = InRustGraph::from_graphnodes (
      &read_all_skg_files_from_skgrepos (config) ? );
    let (_, _, warnings) = buffer_to_validated_saveplan_with_fork_skgrepos_in_graph (
      &edited, &graph, config, &SkgrepoRestriction::unrestricted (config),
      &HashMap::new (), &WhatTheBaselineShows::default (), Perspective::RootHomes, false ) ?;
    assert! ( warnings . iter () . any ( |w|
                w . contains ("Your edit of 'K'")
                && w . contains ("from the perspective of foreign")
                && w . contains ("goes on showing the original") ),
      "{:?}", warnings ); }
  let saved : String = save_and_rerender (&edited, config, tantivy) . await ?;
  let clone : Graphnode = owned_fork_on_disk (config, "K") ?;
  assert_eq! ( clone . title, "K-edited", "the edit forked K" );
  assert_eq! ( clone . home_skgrepo, SkgrepoName::from ("main") );
  assert! ( line_of_gnid (&saved, "K")
            . is_some_and ( |l| l . ends_with (") K") ),
    "under R, K is shown as itself, unedited:\n{}", saved );
  let clone_lines : Vec<&str> = saved . lines ()
    . filter ( |line| line . contains (&format! ("(gnid {})", clone . pid . 0)) )
    . collect ();
  assert! ( clone_lines . iter () . all ( |line| line . contains ("(birth (forks out 2))") ),
    "under R, the user's clone of K is shown only in K's forks folder, as its forker:\n{}", saved );
  assert! ( line_of_gnid (&saved, "A")
            . is_some_and ( |l| l . contains ("(inPlaceOf M)") ),
    "under R, A still stands for M:\n{}", saved );
  { // Editing K there again would fork it again; the save refuses,
    // naming the clone to edit instead.
    let k_line : &str = line_of_gnid (&saved, "K") . unwrap ();
    let edited_again : String = saved . replace (
      k_line, &k_line . replace (") K", ") K-edited-again") );
    match node_instructions_from (&edited_again, config) . await {
      Err (SaveError::BufferValidationErrors { errors, .. }) =>
        assert! ( errors . iter () . any ( |e| matches! (
          e, BufferValidationError::ForkAlreadyExists (original, existing)
             if *original == ID::from ("K") && *existing == clone . pid )),
          "got: {:?}", errors ),
      other => panic! ( "a second fork of K must be refused; got: {:?}",
                        other . map ( |v| v . len () ) ), }}
  Ok (( )) }

/// Editing foreign root R forks it; the saved view is re-rooted at the
/// user's clone of R, which stands for nothing (no '(inPlaceOf R)'),
/// and beneath it the clone's home, "main", decides: U, not A, is
/// shown in M's place. The save's plan warns of nothing, since the
/// clone, a root in its own home, is shown; had the view been
/// switched to "foreign", R would stand in its clone's place, and the
/// plan warns so.
async fn a_forked_foreign_root_follows_its_clones_home (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  graph_handle_from_config (config) ?;
  let (view, _pids, _tree) =
    multi_root_view ( config, Some (tantivy), &[ ID::from ("R") ] ) ?;
  let r_line : &str = view . lines () . next () . unwrap ();
  assert! ( r_line . contains ("(gnid R)")
            && ! r_line . contains ("writeProtected"), "{}", view );
  let edited : String = view . replacen (
    r_line, &r_line . replace (") R", ") R-edited"), 1 );
  { // A fork of a root warns only where the re-rooted view would not
    // show the clone.
    let graph : InRustGraph = InRustGraph::from_graphnodes (
      &read_all_skg_files_from_skgrepos (config) ? );
    let warnings_from = |perspective : Perspective|
      -> Result<Vec<String>, Box<dyn Error>> {
      let (_, _, warnings) = buffer_to_validated_saveplan_with_fork_skgrepos_in_graph (
        &edited, &graph, config, &SkgrepoRestriction::unrestricted (config),
        &HashMap::new (), &WhatTheBaselineShows::default (), perspective, false ) ?;
      Ok ( warnings . into_iter ()
           . filter ( |w| w . contains ("Your edit of 'R'") ) . collect () ) };
    assert! ( warnings_from (Perspective::RootHomes) ? . is_empty () );
    let switched : Vec<String> = warnings_from ( skgrepo ("foreign") ) ?;
    assert! ( switched . len () == 1
              && switched [0] . contains ("from the perspective of foreign")
              && switched [0] . contains ("goes on showing the original"),
      "{:?}", switched ); }
  let saved : String = save_and_rerender (&edited, config, tantivy) . await ?;
  let clone : Graphnode = owned_fork_on_disk (config, "R") ?;
  let root_line : &str = saved . lines () . next () . unwrap ();
  assert! ( root_line . contains (&format! ("(gnid {})", clone . pid . 0))
            && ! root_line . contains ("inPlaceOf"),
    "the root is R's clone:\n{}", saved );
  let marked : Vec<&str> = marked_lines (&saved, "M");
  assert! ( marked . len () == 1 && marked [0] . contains ("(gnid U)"),
    "under the clone of R, the user's U is shown in M's place:\n{}", saved );
  assert! ( saved . lines ()
              . filter ( |line| line . contains ("(gnid A)") )
              . all ( |line| line . contains ("noClaim") ),
    "under the clone of R, A is not shown in M's place, only as a forker \
     of M in M's forks folder, M being drawn editable as U's forkee:\n{}",
    saved );
  Ok (( )) }

//
// Switching a view's perspective
// ('docs/api-and-formats.org', "Set perspective").
//

/// A connection holding one view, of 'root_gnids', as a content-view
/// request registers it, under the ID 'view_id'.
fn connection_holding_a_view_of (
  config     : &SkgConfig,
  tantivy    : &TantivyIndex,
  view_id    : &str,
  root_gnids : &[ID],
) -> Result<(ViewsState, SkgEnv, String), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let env : SkgEnv = skg_env_from_parts (config, tantivy, &graph);
  let (view, pids, tree) : (String, Vec<ID>, Tree<Viewnode>) =
    multi_root_view (config, Some (tantivy), root_gnids) ?;
  let mut views_state : ViewsState =
    ViewsState { open_views : OpenViews::new (), graph_straddle : None };
  views_state . open_views . register_view (
    & env . runtime_snapshot () . graph,
    ViewforestId::from_client_string (view_id . to_string ()),
    tree, &pids, &SkgrepoRestriction::unrestricted (config) );
  Ok (( views_state, env, view )) }

/// Send 'request' to 'handler' as the connection's thread would, and
/// return every message it answered with.
fn messages_answering (
  request     : &str,
  views_state : &mut ViewsState,
  env         : &SkgEnv,
  config      : &SkgConfig,
  handler     : fn (&mut TcpStream, &str, &SkgEnv, &mut ViewsState, &SkgrepoRestriction),
) -> Vec<String> {
  let (mut server_stream, client_stream) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  std::thread::scope ( |scope| {
    // The handlers call block_on, so each runs on its own thread.
    scope . spawn ( || handler (
      &mut server_stream, request, env, views_state,
      &SkgrepoRestriction::unrestricted (config) )); } );
  drop (server_stream);
  read_all_lp_messages ( &mut BufReader::new (client_stream) ) }

fn switch_request (
  view_id   : &str,
  followed  : Perspective,
) -> String {
  switch_request_naming (view_id, &followed . wire_name ()) }

fn switch_request_naming (
  view_id   : &str,
  named     : &str,
) -> String {
  format! (
    "((request . \"set perspective\") (view-id . \"{}\") (perspective . \"{}\"))",
    view_id, named ) }

fn skgrepo (
  name : &str,
) -> Perspective {
  Perspective::Skgrepo ( SkgrepoName::from (name) ) }

/// The one rerendered view's text among 'messages', if one was sent.
fn rerendered_text (
  messages : &[String],
) -> Option<String> {
  messages . iter ()
    . find ( |message| message . contains ("(response-type rerender-view)") )
    . and_then ( |message| extract_string_field_from_sexp (message, "content") ) }

/// A view of foreign R shows A, in R's home, in M's place. Switched
/// to the perspective of "main", it is rerendered with U, in "main",
/// there instead; the answer names the view and the choice; a foreign
/// redraw (the "rerender views" a reload asks for) keeps it. Switched
/// back, it shows A again.
fn a_switched_view_follows_a_skgrepos_perspective_until_switched_back (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (mut views_state, env, view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "R", &[ ID::from ("R") ]) ?;
  assert! ( line_of_gnid (&view, "A") . is_some_and ( |l| l . contains ("(inPlaceOf M)") ),
    "a fresh view of R follows R's home:\n{}", view );
  let messages : Vec<String> = messages_answering (
    &switch_request ("R", skgrepo ("main")),
    &mut views_state, &env, config, handle_set_perspective_request );
  let answer : &String = messages . first () . unwrap ();
  assert! ( answer . contains ("(response-type perspective)")
            && extract_string_field_from_sexp (answer, "view-id") . as_deref () == Some ("R")
            && extract_string_field_from_sexp (answer, "perspective") . as_deref () == Some ("skgrepo main")
            && extract_string_field_from_sexp (answer, "content") . is_none (),
    "the answer names the view and the new choice: {:?}", messages );
  let switched : String = rerendered_text (&messages)
    . ok_or_else ( || format! ("the view is rerendered: {:?}", messages) ) ?;
  assert! ( line_of_gnid (&switched, "U") . is_some_and ( |l| l . contains ("(inPlaceOf M)") )
            && claiming_line_of_gnid (&switched, "A") . is_none (),
    "switched, main's U stands in M's place:\n{}", switched );
  assert_eq! ( views_state . open_views . perspective_of_view (
                 &ViewforestId::from_client_string ("R" . to_string ()) ),
               skgrepo ("main") );
  let redrawn : String = rerendered_text ( &messages_answering (
    "((request . \"rerender views\") (view-ids \"R\"))",
    &mut views_state, &env, config, handle_rerender_views_request ))
    . ok_or ("the redraw rerenders the view") ?;
  assert! ( line_of_gnid (&redrawn, "U") . is_some (),
    "a redraw the user did not ask for keeps the choice:\n{}", redrawn );
  let back : String = rerendered_text ( &messages_answering (
    &switch_request ("R", Perspective::RootHomes),
    &mut views_state, &env, config, handle_set_perspective_request ))
    . ok_or ("switching back rerenders the view") ?;
  assert! ( line_of_gnid (&back, "A") . is_some_and ( |l| l . contains ("(inPlaceOf M)") )
            && claiming_line_of_gnid (&back, "U") . is_none (),
    "switched back, A, in R's home, stands in M's place again:\n{}", back );
  Ok (( )) }

/// Every mirror may bear the part, so a content child already
/// shown in another's place is re-pointed whenever the view's
/// perspective ranks another mirror first: a view of owned P shows U,
/// in P's home, in M's place; switched to third's perspective, B; to
/// foreign's, A; back to the root homes', U again.
fn a_switch_of_perspective_re_points_a_substituted_content_child (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (mut views_state, env, view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "P", &[ ID::from ("P") ]) ?;
  assert! ( line_of_gnid (&view, "U") . is_some_and ( |l| l . contains ("(inPlaceOf M)") ),
    "a fresh view of P shows the user's U in M's place:\n{}", view );
  for (perspective, shown) in [ ( skgrepo ("third"), "B" ),
                                ( skgrepo ("foreign"), "A" ),
                                ( Perspective::RootHomes, "U" ) ] {
    let switched : String = rerendered_text ( &messages_answering (
      &switch_request ("P", perspective . clone ()),
      &mut views_state, &env, config, handle_set_perspective_request ))
      . ok_or_else ( || format! ("the switch to {:?} rerenders the view", perspective) ) ?;
    let marked : Vec<&str> = marked_lines (&switched, "M");
    assert! ( marked . len () == 1 && marked [0] . contains ( &format! ("(gnid {})", shown) ),
      "from {:?}, {} stands in M's place, alone:\n{}", perspective, shown, switched ); }
  Ok (( )) }

/// A content child re-pointed by a switch of perspective draws its
/// folders afresh, for the note it now shows, since those it held
/// describe the note it showed before. P, the user's, contains N,
/// alice's; bob's F forks N, and the user's U forks F. A view of P
/// shows U in N's place, its forks folder holding F, which U forks,
/// then N, an indirect member. From alice's perspective N stands raw,
/// its forks folder holding F, which forks it, then U, an indirect
/// member; from bob's, F stands in N's place, its forks folder
/// holding N, which it forks, and U, which forks it. No folder holds
/// the note it describes or an unwarranted member, and U's forkee F
/// claims (drawn claiming nothing, a client would read it as a pending
/// detachment). Back at the root homes', the view is drawn as the
/// fresh view drew it, and a second round of switches draws the same
/// views again.
fn a_re_pointed_content_child_draws_its_forks_folder_afresh (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (mut views_state, env, view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "P", &[ ID::from ("P") ]) ?;
  assert_eq! ( forks_folder_members_under (&view, "U"), vec! [ "F", "N" ],
    "a fresh view of P shows U in N's place, its forks folder holding F, then N:\n{}", view );
  for _round in 0 .. 2 {
    for (perspective, shown, members) in [
      ( skgrepo ("alice"),       "N", vec! [ "F", "U" ] ),
      ( skgrepo ("bob"),         "F", vec! [ "N", "U" ] ),
      ( skgrepo ("alice"),       "N", vec! [ "F", "U" ] ),
      ( Perspective::RootHomes, "U", vec! [ "F", "N" ] ) ] {
      let switched : String = rerendered_text ( &messages_answering (
        &switch_request ("P", perspective . clone ()),
        &mut views_state, &env, config, handle_set_perspective_request ))
        . ok_or_else ( || format! ("the switch to {:?} rerenders the view", perspective) ) ?;
      assert_eq! ( forks_folder_members_under (&switched, shown), members,
        "from {:?}, {}'s forks folder holds its own mirrors:\n{}",
        perspective, shown, switched );
      assert! ( ! switched . contains ("noWarrant"),
        "from {:?}, no member is unwarranted:\n{}", perspective, switched );
      if perspective == Perspective::RootHomes {
        assert_eq! ( without_vnids ( switched . lines () ),
                     without_vnids ( view . lines () ),
          "back at the root homes', the view is drawn as the fresh view drew it" ); }}}
  assert! ( line_of_gnid (&view, "F") . is_some_and ( |line| ! line . contains ("noClaim") ),
    "U's forkee F claims, so is no pending detachment:\n{}", view );
  Ok (( )) }

/// The gnids of the members of the forks folder right under the
/// occurrence of 'gnid' in 'view', in order.
fn forks_folder_members_under (
  view : &str,
  gnid : &str,
) -> Vec<String> {
  let lines : Vec<&str> = view . lines () . collect ();
  let fact : String = format! ("(gnid {})", gnid);
  let Some (at) : Option<usize> =
    lines . iter () . position ( |line| line . contains (&fact)
                                        && ! line . contains ("forksFolder") )
    else { return Vec::new (); };
  let depth : usize = headline_depth (lines [at]);
  let folder : Option<usize> =
    lines . iter () . enumerate () . skip (at + 1)
    . take_while ( |(_, line)| headline_depth (line) > depth )
    . find ( |(_, line)| headline_depth (line) == depth + 1
                         && line . contains ("forksFolder") )
    . map ( |(index, _)| index );
  let Some (folder) : Option<usize> = folder else { return Vec::new (); };
  lines . iter () . skip (folder + 1)
    . take_while ( |line| headline_depth (line) > depth + 1 )
    . filter ( |line| headline_depth (line) == depth + 2 )
    . filter_map ( |line| {
        let start : usize = line . find ("(gnid ") ? + "(gnid " . len ();
        let end : usize = start + line [start ..] . find (')') ?;
        Some ( line [start .. end] . to_string () ) } )
    . collect () }

/// How many stars begin the headline 'line'.
fn headline_depth (
  line : &str,
) -> usize {
  line . chars () . take_while ( |c| *c == '*' ) . count () }

/// Where the perspective's skgrepo holds no mirror of a note, the
/// first mirror by declaration order stands, whoever owns it: P, in
/// "main", contains N, alice's, which bob's F forks; the config
/// declares bob's skgrepo before alice's, so a view of P shows F in
/// N's place, born of its forking N ('(birth (forks out))'). From
/// alice's perspective N shows raw there; and under Q, which contains
/// F, N stands in F's place, born of being forked by it
/// ('(birth (forks in))').
fn the_first_declared_mirror_stands_where_the_perspectives_skgrepo_holds_none (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (mut views_state, env, view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "P", &[ ID::from ("P") ]) ?;
  let marked : Vec<&str> = marked_lines (&view, "N");
  assert! ( marked . len () == 1 && marked [0] . contains ("(gnid F)")
            && marked [0] . contains ("(birth (forks out))"),
    "main holds no mirror of N, so bob's F, declared first, stands:\n{}", view );
  let alices : String = rerendered_text ( &messages_answering (
    &switch_request ("P", skgrepo ("alice")),
    &mut views_state, &env, config, handle_set_perspective_request ))
    . ok_or ("the switch to alice's perspective rerenders the view") ?;
  assert! ( marked_lines (&alices, "N") . is_empty ()
            && line_of_gnid (&alices, "N") . is_some (),
    "from alice's perspective N shows raw:\n{}", alices );
  let (mut views_state, env, _view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "Q", &[ ID::from ("Q") ]) ?;
  let alices_q : String = rerendered_text ( &messages_answering (
    &switch_request ("Q", skgrepo ("alice")),
    &mut views_state, &env, config, handle_set_perspective_request ))
    . ok_or ("the switch to alice's perspective rerenders the view of Q") ?;
  let in_fs_place : Vec<&str> = marked_lines (&alices_q, "F");
  assert! ( in_fs_place . len () == 1 && in_fs_place [0] . contains ("(gnid N)")
            && in_fs_place [0] . contains ("(birth (forks in))"),
    "from alice's perspective N, which F forks, stands in F's place:\n{}", alices_q );
  Ok (( )) }

/// A view of P, in "main", is drawn from "main" already, so a switch
/// to "main" is refused, with the reason, the unchanged perspective
/// and an empty rerender stream; "next" passes it over, to the
/// skgrepos that would show the view differently, in declaration
/// order: foreign (A in M's place), third (B), origin (M itself). A
/// switch back to the root homes is not refused. A view the
/// connection does not hold, a request naming no perspective, and a
/// request naming an unknown skgrepo or a name no perspective has
/// ("user", "owner of foreign", "root authors") are refused too.
fn a_switch_to_a_skgrepo_is_refused_where_it_would_change_nothing (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (mut views_state, env, _view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "P", &[ ID::from ("P") ]) ?;
  let owned : Vec<String> = messages_answering (
    &switch_request ("P", skgrepo ("main")),
    &mut views_state, &env, config, handle_set_perspective_request );
  assert! ( refusal_of (&owned) . is_some_and ( |reason| reason . contains (
              "The perspective of main would show this buffer as the perspective of each root's home skgrepo does") ),
    "{:?}", owned );
  assert! ( extract_string_field_from_sexp (&owned [0], "perspective")
            . as_deref () == Some ("root homes")
            && extract_string_field_from_sexp (&owned [0], "perspective-name")
            . as_deref () == Some ("the perspective of each root's home skgrepo (main)"),
    "the refusal carries the unchanged perspective: {:?}", owned );
  for (expected_wire, expected_mirror) in [ ("skgrepo foreign", "A"),
                                            ("skgrepo third",   "B"),
                                            ("skgrepo origin",  "M") ] {
    let next : Vec<String> = messages_answering (
      &switch_request_naming ("P", "next"),
      &mut views_state, &env, config, handle_set_perspective_request );
    let in_ms_place = |text : &str| -> bool {
      if expected_mirror == "M" {
        line_of_gnid (text, "M") . is_some_and ( |l| ! l . contains ("inPlaceOf") ) }
      else { line_of_gnid (text, expected_mirror)
             . is_some_and ( |l| l . contains ("(inPlaceOf M)") ) }};
    assert! ( extract_string_field_from_sexp (&next [0], "perspective")
              . as_deref () == Some (expected_wire)
              && rerendered_text (&next) . is_some_and ( |text| in_ms_place (&text) ),
      "next passes over main, which would change nothing, for each other skgrepo in turn: {:?}", next ); }
  let back : Vec<String> = messages_answering (
    &switch_request ("P", Perspective::RootHomes),
    &mut views_state, &env, config, handle_set_perspective_request );
  assert! ( extract_string_field_from_sexp (&back [0], "content") . is_none ()
            && rerendered_text (&back) . is_some (),
    "a switch to the root homes is not refused: {:?}", back );
  let unheld : Vec<String> = messages_answering (
    &switch_request ("R", skgrepo ("main")),
    &mut views_state, &env, config, handle_set_perspective_request );
  assert! ( refusal_of (&unheld) . is_some_and ( |reason| reason . contains ("does not hold") ),
    "{:?}", unheld );
  let unnamed : Vec<String> = messages_answering (
    "((request . \"set perspective\") (view-id . \"P\"))",
    &mut views_state, &env, config, handle_set_perspective_request );
  assert! ( refusal_of (&unnamed) . is_some (), "{:?}", unnamed );
  for unknown in [ "skgrepo nobody", "user", "owner of foreign", "root authors" ] {
    let refused : Vec<String> = messages_answering (
      &switch_request_naming ("P", unknown),
      &mut views_state, &env, config, handle_set_perspective_request );
    assert! ( refusal_of (&refused) . is_some_and ( |reason| reason . contains ("'perspective' must be") ),
      "{:?} names no perspective: {:?}", unknown, refused ); }
  Ok (( )) }

/// The refusal in 'messages', the answer to a switch: its reason, if
/// it is one. Asserts the shape every answer has: the answer first,
/// and, for a refusal, an empty rerender stream.
fn refusal_of (
  messages : &[String],
) -> Option<String> {
  let answer : &String = messages . first () ?;
  assert! ( answer . contains ("(response-type perspective)"), "{:?}", messages );
  let refusal : Option<String> = extract_string_field_from_sexp (answer, "content");
  if refusal . is_some () {
    assert! ( rerendered_text (messages) . is_none ()
              && messages . iter () . any ( |m| m . contains ("(response-type rerender-done)") ),
      "a refusal is followed by an empty rerender stream: {:?}", messages ); }
  refusal }

/// "next" cycles a view of foreign R through every perspective that
/// would show it differently, in a fixed order, the cycle computed
/// afresh at each request: the root homes (A in M's place), then
/// main's (U), third's (B in M's place and T in K's), origin's (M
/// itself), and back. Foreign's, R's own home, would draw what the
/// root homes do, so it is passed over. Each answer names the
/// perspective as a message to the user would.
fn next_cycles_through_every_perspective_that_shows_the_view_differently (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (mut views_state, env, _view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "R", &[ ID::from ("R") ]) ?;
  let mut next = |expected_wire : &str, expected_name : &str, shown_for_m : &str|
    -> Result<String, Box<dyn Error>> {
    let messages : Vec<String> = messages_answering (
      &switch_request_naming ("R", "next"),
      &mut views_state, &env, config, handle_set_perspective_request );
    assert! ( refusal_of (&messages) . is_none (), "{:?}", messages );
    assert_eq! ( extract_string_field_from_sexp (&messages [0], "perspective")
                   . as_deref (), Some (expected_wire), "{:?}", messages );
    assert_eq! ( extract_string_field_from_sexp (&messages [0], "perspective-name")
                   . as_deref (), Some (expected_name), "{:?}", messages );
    let text : String = rerendered_text (&messages) . ok_or ("the switch rerenders") ?;
    assert! ( line_of_gnid (&text, shown_for_m)
              . is_some_and ( |l| if shown_for_m == "M" { ! l . contains ("inPlaceOf") }
                                  else { l . contains ("(inPlaceOf M)") } ),
      "{} stands in M's place:\n{}", shown_for_m, text );
    Ok (text) };
  next ("skgrepo main", "the perspective of main", "U") ?;
  let third : String = next ("skgrepo third", "the perspective of third", "B") ?;
  assert! ( line_of_gnid (&third, "T") . is_some_and ( |l| l . contains ("(inPlaceOf K)") ),
    "third's T stands in K's place:\n{}", third );
  next ("skgrepo origin", "the perspective of origin", "M") ?;
  next ("root homes", "the perspective of each root's home skgrepo (foreign)", "A") ?;
  next ("skgrepo main", "the perspective of main", "U") ?;
  Ok (( )) }

/// The cycle does not depend on the path taken. In a view of F2,
/// switched to "main", main's U2 stands in M2's place and holds
/// nothing; N2, which third forks, is drawn from main's perspective
/// only as content of M2, U2's forkee as such, where it stands raw;
/// third's is the next after main's, as found from the root homes'
/// drawing, and a request naming it from there is not refused.
fn a_perspective_stays_in_the_cycle_where_another_hides_what_it_forks (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (mut views_state, env, _view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "F2", &[ ID::from ("F2") ]) ?;
  let mut switch = |named : &str| -> Vec<String> { messages_answering (
    &switch_request_naming ("F2", named),
    &mut views_state, &env, config, handle_set_perspective_request ) };
  let to_user : Vec<String> = switch ("next");
  assert! ( extract_string_field_from_sexp (&to_user [0], "perspective")
              . as_deref () == Some ("skgrepo main")
            && rerendered_text (&to_user) . is_some_and ( |text|
                 line_of_gnid (&text, "U2") . is_some ()
                 && line_of_gnid (&text, "N2") . is_some_and ( |l| ! l . contains ("inPlaceOf") ) ),
    "{:?}", to_user );
  let named : Vec<String> = switch ("skgrepo third");
  assert! ( refusal_of (&named) . is_none (), "{:?}", named );
  let back_to_user : Vec<String> = switch ("skgrepo main");
  assert! ( refusal_of (&back_to_user) . is_none (), "{:?}", back_to_user );
  let to_third : Vec<String> = switch ("next");
  assert! ( extract_string_field_from_sexp (&to_third [0], "perspective")
              . as_deref () == Some ("skgrepo third")
            && rerendered_text (&to_third) . is_some_and ( |text|
                 line_of_gnid (&text, "T2") . is_some_and ( |l| l . contains ("(inPlaceOf N2)") )),
    "from main's, the next is third's, which shows T2 in N2's place: {:?}", to_third );
  Ok (( )) }

/// A view with several roots names its root homes' perspective by
/// each root's home skgrepo, once, in the order of their first roots.
fn the_root_homes_are_named_in_the_order_of_their_first_roots (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (mut views_state, env, _view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (
      config, tantivy, "RPK", &[ ID::from ("R"), ID::from ("P"), ID::from ("K") ] ) ?;
  messages_answering (
    &switch_request ("RPK", skgrepo ("main")),
    &mut views_state, &env, config, handle_set_perspective_request );
  let back : Vec<String> = messages_answering (
    &switch_request ("RPK", Perspective::RootHomes),
    &mut views_state, &env, config, handle_set_perspective_request );
  assert_eq! ( extract_string_field_from_sexp (&back [0], "perspective-name") . as_deref (),
               Some ("the perspective of each root's home skgrepo (foreign, main)"), "{:?}", back );
  Ok (( )) }

/// A view in which no skgrepo's perspective would show anything
/// differently (one of L, which has no mirror and holds nothing) has
/// a cycle of one, so "next" is refused there. But a
/// view whose perspective has left its cycle, as a save can leave it
/// (a save names its buffer's perspective, and it is kept whatever the
/// cycle is now), goes back to the root homes on "next".
async fn next_from_a_perspective_that_left_the_cycle_goes_back_to_the_root_homes (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (mut views_state, mut env, view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "L", &[ ID::from ("L") ]) ?;
  let lonely : Vec<String> = messages_answering (
    &switch_request_naming ("L", "next"),
    &mut views_state, &env, config, handle_set_perspective_request );
  assert! ( refusal_of (&lonely) . is_some_and ( |reason| reason . contains ("No other perspective") ),
    "{:?}", lonely );
  let view_id : ViewforestId = ViewforestId::from_client_string ("L" . to_string ());
  let (mut stream, _read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  let response : SaveResponse =
    update_from_and_rerender_buffer_with_approvals (
      &mut stream, &view, &view, &mut env, &Ok ( view_id . clone () ),
      &mut views_state, &SkgrepoRestriction::unrestricted (config),
      true, &HashMap::new (), &HashSet::new (),
      &StrandedsReview::NotReviewed, DeleteFrom::NotChosen, &[], None,
      skgrepo ("third") ) . await ?;
  assert! ( response . errors . is_empty (), "{:?}", response . errors );
  assert_eq! ( views_state . open_views . perspective_of_view (&view_id),
               skgrepo ("third") );
  let back : Vec<String> = messages_answering (
    &switch_request_naming ("L", "next"),
    &mut views_state, &env, config, handle_set_perspective_request );
  assert! ( refusal_of (&back) . is_none ()
            && extract_string_field_from_sexp (&back [0], "perspective")
               . as_deref () == Some ("root homes"),
    "{:?}", back );
  Ok (( )) }

/// The root too: a view of M, which A (foreign's), U (main's) and B
/// (third's) each fork, shows M raw from the root homes, as a fresh
/// view does, M standing in its own home, "origin"; and each
/// skgrepo's fork of it from that skgrepo's perspective, marked
/// '(inPlaceOf M)', in declaration order. The root is re-pointed where
/// it stands, so it keeps its viewnode ID both ways, and the root
/// homes show M raw again. A save through the replaced root edits the
/// mirror, not M.
async fn a_perspective_replaces_the_root_and_the_root_homes_restore_it (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (mut views_state, mut env, view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "M", &[ ID::from ("M") ]) ?;
  let root_line = |text : &str| -> String { text . lines () . next () . unwrap () . to_string () };
  assert! ( root_line (&view) . contains ("(vnid 0a) node (gnid M)")
            && ! view . contains ("inPlaceOf"),
    "a fresh view shows its root raw:\n{}", view );
  let mut next = |expected_wire : &str| -> Result<String, Box<dyn Error>> {
    let messages : Vec<String> = messages_answering (
      &switch_request_naming ("M", "next"),
      &mut views_state, &env, config, handle_set_perspective_request );
    assert_eq! ( extract_string_field_from_sexp (&messages [0], "perspective")
                   . as_deref (), Some (expected_wire), "{:?}", messages );
    Ok ( root_line ( &rerendered_text (&messages) . ok_or ("the switch rerenders") ? )) };
  for (wire, shown) in [ ("skgrepo foreign", "A"), ("skgrepo main", "U"), ("skgrepo third", "B") ] {
    let root : String = next (wire) ?;
    assert! ( root . contains ( &format! ("(vnid 0a) node (gnid {})", shown) )
              && root . contains ("(inPlaceOf M)"),
      "from {}, the root is {}, standing for M: {}", wire, shown, root ); }
  let back : String = next ("root homes") ?;
  assert! ( back . contains ("(vnid 0a) node (gnid M)") && ! back . contains ("inPlaceOf"),
    "the root homes show M raw again: {}", back );
  let switched : String = { let messages : Vec<String> = messages_answering (
      &switch_request ("M", skgrepo ("main")),
      &mut views_state, &env, config, handle_set_perspective_request );
    rerendered_text (&messages) . ok_or ("the switch rerenders") ? };
  let edited : String = switched . replacen (") U\n", ") U, retitled\n", 1);
  assert_ne! ( edited, switched, "{}", switched );
  let (mut stream, _read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  let response : SaveResponse =
    update_from_and_rerender_buffer_with_approvals (
      &mut stream, &edited, &switched, &mut env,
      &Ok ( ViewforestId::from_client_string ("M" . to_string ()) ),
      &mut views_state, &SkgrepoRestriction::unrestricted (config),
      true, &HashMap::new (), &HashSet::new (),
      &StrandedsReview::NotReviewed, DeleteFrom::NotChosen, &[], None,
      skgrepo ("main") ) . await ?;
  assert! ( response . errors . is_empty (), "{:?}", response . errors );
  assert! ( read_skgrepo_file (config, "main", "U") . contains ("title: U, retitled"),
    "the save edits U" );
  assert! ( read_skgrepo_file (config, "origin", "M") . contains ("title: M"),
    "and leaves M" );
  assert! ( root_line (&response . saved_view) . contains ("(gnid U)")
            && root_line (&response . saved_view) . contains ("(inPlaceOf M)")
            && response . saved_view . contains ("U, retitled"),
    "the saved view is still shown from main's perspective:\n{}", response . saved_view );
  Ok (( )) }

/// A replaced root's containers folder is the replacement's: a view
/// of M lists P and R, which contain M. From main's perspective the
/// root is U, and its folder lists W, which contains U, as a fresh
/// view of U would, and nothing of M's; from foreign's it is A, which
/// nothing contains, so there is no folder; and back at the root
/// homes, M's folder is drawn as the fresh view drew it.
fn a_replaced_root_shows_its_own_containers (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (mut views_state, env, view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "M", &[ ID::from ("M") ]) ?;
  let (_, _, fresh_view_of_u) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "U", &[ ID::from ("U") ]) ?;
  assert! ( view . contains ("containerFolder")
            && line_of_gnid (&view, "P") . is_some ()
            && line_of_gnid (&view, "R") . is_some (),
    "a fresh view of M lists its containers:\n{}", view );
  let mut switch = |named : &str| -> Result<String, Box<dyn Error>> {
    let messages : Vec<String> = messages_answering (
      &switch_request_naming ("M", named),
      &mut views_state, &env, config, handle_set_perspective_request );
    assert! ( refusal_of (&messages) . is_none (), "{:?}", messages );
    Ok ( rerendered_text (&messages) . ok_or ("the switch rerenders") ? ) };
  let users : String = switch ("skgrepo main") ?;
  let users_container_folder : Vec<&str> = users . lines ()
    . skip_while ( |l| ! l . contains ("containerFolder") ) . skip (1)
    . take_while ( |l| l . starts_with ("***") )
    . collect ();
  assert! ( users_container_folder . len () == 1
            && users_container_folder [0] . contains ("(gnid W)")
            && ! users . contains ("noWarrant"),
    "from main's, U's folder lists W, which contains U, and nothing of M's \
     (M's containers show only under M, in U's forks folder, since U \
     forks M):\n{}", users );
  assert_eq! ( without_vnids ( users . lines () . skip (1) ),
               without_vnids ( fresh_view_of_u . lines () . skip (1) ),
    "U's folder is drawn as a fresh view of U draws it" );
  let foreigns : String = switch ("skgrepo foreign") ?;
  assert! ( line_of_gnid (&foreigns, "A") . is_some ()
            && ! foreigns . lines () . any ( |l| l . starts_with ("** ")
                                                && l . contains ("containerFolder") ),
    "from foreign's, A, which nothing contains, has no containers folder \
     (M, in its forks folder, has its own):\n{}", foreigns );
  let back : String = switch ("root homes") ?;
  assert_eq! ( without_vnids ( back . lines () ), without_vnids ( view . lines () ),
    "back at the root homes, M's folder is drawn again as the fresh view drew it" );
  Ok (( )) }

/// A replaced root's default folders are the replacement's: a view
/// of foreign L shows its forks folder holding J, which forks it,
/// claiming nothing. From main's perspective the root is J, the
/// user's fork of L, and it is drawn as a fresh view of J draws it:
/// its containers folder, and its forks folder holding L, claiming,
/// counting the one child of L that J neither contains nor hides. Back
/// at the root homes, L is drawn again as the fresh view drew it.
fn a_replaced_root_shows_its_own_default_folders (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (mut views_state, env, view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "L", &[ ID::from ("L") ]) ?;
  let (_, _, fresh_view_of_j) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "J", &[ ID::from ("J") ]) ?;
  assert! ( line_of_gnid (&view, "J") . is_some_and (
              |line| line . contains ("noClaim") && line . contains ("(birth (forks out 2))") ),
    "a fresh view of L shows J in its forks folder, claiming nothing:\n{}", view );
  let mut switch = |named : &str| -> Result<String, Box<dyn Error>> {
    let messages : Vec<String> = messages_answering (
      &switch_request_naming ("L", named),
      &mut views_state, &env, config, handle_set_perspective_request );
    assert! ( refusal_of (&messages) . is_none (), "{:?}", messages );
    Ok ( rerendered_text (&messages) . ok_or ("the switch rerenders") ? ) };
  let users : String = switch ("skgrepo main") ?;
  assert! ( users . contains ("forksFolder")
            && line_of_gnid (&users, "L") . is_some_and (
                 |line| line . contains ("(unintegrated 1)") )
            && ! users . contains ("noWarrant"),
    "from main's, J shows its forks folder, L in it with its count:\n{}", users );
  assert_eq! ( without_vnids ( users . lines () . skip (1) ),
               without_vnids ( fresh_view_of_j . lines () . skip (1) ),
    "J's folders are drawn as a fresh view of J draws them" );
  let back : String = switch ("root homes") ?;
  assert_eq! ( without_vnids ( back . lines () ), without_vnids ( view . lines () ),
    "back at the root homes, L is drawn again as the fresh view drew it" );
  Ok (( )) }

/// A folder the user asked for on a root that a perspective replaces
/// is drawn again for the replacement: L's view, given its mentioners
/// and aliases folders, lists X, which links to L, and L's alias
/// "letter". From main's perspective the root is J, and those
/// folders list Y, which links to J, and J's alias "reply", with
/// nothing of L's; back at the root homes, L's again.
async fn a_replaced_root_redraws_the_folders_it_was_asked_for (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (mut views_state, mut env, view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "L", &[ ID::from ("L") ]) ?;
  let requesting : String = view . replacen (
    "(skg (vnid 0a) node (gnid L)",
    "(skg (vnid 0a) node (gnid L) (viewRequests (folder mentioners) (folder aliases))", 1 );
  assert_ne! ( requesting, view, "{}", view );
  let (mut stream, _read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  let response : SaveResponse =
    update_from_and_rerender_buffer_with_approvals (
      &mut stream, &requesting, &view, &mut env,
      &Ok ( ViewforestId::from_client_string ("L" . to_string ()) ),
      &mut views_state, &SkgrepoRestriction::unrestricted (config),
      true, &HashMap::new (), &HashSet::new (),
      &StrandedsReview::NotReviewed, DeleteFrom::NotChosen, &[], None,
      Perspective::RootHomes ) . await ?;
  assert! ( response . errors . is_empty (), "{:?}", response . errors );
  let asked : String = response . saved_view;
  let shows_ls = |text : &str| -> bool {
    line_of_gnid (text, "X") . is_some () && text . contains (") letter") };
  let shows_js = |text : &str| -> bool {
    line_of_gnid (text, "Y") . is_some () && text . contains (") reply") };
  assert! ( asked . contains ("mentionerFolder") && asked . contains ("aliasFolder")
            && shows_ls (&asked) && ! shows_js (&asked),
    "L's view, asked for them, lists X and \"letter\":\n{}", asked );
  let mut switch = |named : &str| -> Result<String, Box<dyn Error>> {
    let messages : Vec<String> = messages_answering (
      &switch_request_naming ("L", named),
      &mut views_state, &env, config, handle_set_perspective_request );
    assert! ( refusal_of (&messages) . is_none (), "{:?}", messages );
    Ok ( rerendered_text (&messages) . ok_or ("the switch rerenders") ? ) };
  let users : String = switch ("skgrepo main") ?;
  assert! ( users . contains ("mentionerFolder") && users . contains ("aliasFolder")
            && shows_js (&users)
            && line_of_gnid (&users, "X") . is_none ()
            && ! users . contains (") letter")
            && ! users . contains ("noWarrant"),
    "from main's, J's folders list Y and \"reply\", and nothing of L's:\n{}", users );
  let back : String = switch ("root homes") ?;
  assert_eq! ( without_vnids ( back . lines () ), without_vnids ( asked . lines () ),
    "back at the root homes, L's folders are drawn again" );
  Ok (( )) }

/// A search hit is a write-protected root that shows only its
/// folders. Switched to main's perspective, the hit for foreign L
/// ("missive") shows J, the user's fork, still write-protected, with
/// what a search hit for J ("answer") shows: its containers folder and
/// its forks folder, folded, and no content, which only an
/// editable root draws. Back at the root homes, the hit shows L's
/// folders again.
fn a_replaced_search_hit_stays_a_hit (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  { // The session's search index starts empty.
    let tantivy_nodes : Vec<GraphnodeInTantivy> =
      read_all_skg_files_from_skgrepos (config) ?
      . iter () . map (GraphnodeInTantivy::from) . collect ();
    update_index_with_nodes (&tantivy_nodes, tantivy) ?; }
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let env : SkgEnv = skg_env_from_parts (config, tantivy, &graph);
  let mut views_state : ViewsState =
    ViewsState { open_views : OpenViews::new (), graph_straddle : None };
  let mut search = |terms : &str| -> String {
    let messages : Vec<String> = messages_answering (
      &format! ( "((request . \"text search\") (terms . \"{}\") (regex . \"false\") (body . \"false\") (operators . \"false\"))",
                 terms ),
      &mut views_state, &env, config, handle_text_search_request );
    extract_string_field_from_sexp (&messages [0], "content") . unwrap_or_default () };
  let fresh_hit_j : String = search ("answer");
  let hit_l : String = search ("missive");
  let mut switch = |named : &str| -> Result<String, Box<dyn Error>> {
    let messages : Vec<String> = messages_answering (
      &switch_request_naming ("search:missive", named),
      &mut views_state, &env, config, handle_set_perspective_request );
    assert! ( refusal_of (&messages) . is_none (), "{:?}", messages );
    Ok ( rerendered_text (&messages) . ok_or ("the switch rerenders") ? ) };
  let users : String = switch ("skgrepo main") ?;
  assert! ( users . lines () . next () . is_some_and (
              |root| root . contains ("(gnid J)") && root . contains ("writeProtected") )
            && line_of_gnid (&users, "L1") . is_none (),
    "from main's, the hit is J, write-protected, without content:\n{}", users );
  assert_eq! ( without_vnids ( users . lines () . skip (1) ),
               without_vnids ( fresh_hit_j . lines () . skip (1) ),
    "J's folders are drawn as a search hit for J draws them" );
  let back : String = switch ("root homes") ?;
  assert_eq! ( without_vnids ( back . lines () ), without_vnids ( hit_l . lines () ),
    "back at the root homes, the hit shows L's folders again" );
  Ok (( )) }

/// The lines, each without its '(vnid ID)'.
fn without_vnids<'a> (
  lines : impl Iterator<Item = &'a str>,
) -> Vec<String> {
  lines . map ( |line| match line . find ("(vnid ") {
      Some (start) => {
        let end : usize = start + line [start ..] . find (')') . unwrap () + 1;
        format! ( "{}{}", &line [.. start], &line [end ..] ) },
      None => line . to_string (), } )
    . collect () }

/// In a view of foreign R drawn from main's perspective, editing
/// R's child K forks it, and the save's rerender shows the clone, with
/// the edit, in K's place, as '(inPlaceOf K)', K standing only in
/// the clone's forks folder (the folder K's occurrence carried, now
/// the clone's); U still stands in M's place. The connection keeps the
/// choice the save named.
async fn a_save_following_a_skgrepos_perspective_shows_a_new_fork_in_place (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (mut views_state, mut env, _view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "R", &[ ID::from ("R") ]) ?;
  let switched : String = rerendered_text ( &messages_answering (
    &switch_request ("R", skgrepo ("main")),
    &mut views_state, &env, config, handle_set_perspective_request ))
    . ok_or ("the switch rerenders the view") ?;
  let k_line : &str = line_of_gnid (&switched, "K")
    . ok_or_else ( || format! ("K is shown under R:\n{}", switched) ) ?;
  let edited : String = switched . replace (
    k_line, &k_line . replace (") K", ") K-edited") );
  let (mut stream, _read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  let response : SaveResponse =
    update_from_and_rerender_buffer_with_approvals (
      &mut stream, &edited, &switched, &mut env,
      &Ok ( ViewforestId::from_client_string ("R" . to_string ()) ),
      &mut views_state, &SkgrepoRestriction::unrestricted (config),
      true, &HashMap::new (), &HashSet::new (),
      &StrandedsReview::NotReviewed, DeleteFrom::NotChosen, &[], None,
      skgrepo ("main") ) . await ?;
  assert! ( response . errors . is_empty (), "{:?}", response . errors );
  let clone : Graphnode = owned_fork_on_disk (config, "K") ?;
  let saved : &str = &response . saved_view;
  assert! ( line_of_gnid (saved, &clone . pid . 0)
            . is_some_and ( |l| l . contains ("(inPlaceOf K)") && l . ends_with (") K-edited") ),
    "the clone, edited, stands in K's place:\n{}", saved );
  assert! ( saved . lines () . filter ( |line| line . contains ("(gnid K)") )
            . all ( |line| line . contains ("(birth (forks in 2))") ),
            "K stands only in the clone's forks folder:\n{}", saved );
  assert! ( line_of_gnid (saved, "U") . is_some_and ( |l| l . contains ("(inPlaceOf M)") ),
    "U still stands in M's place:\n{}", saved );
  assert_eq! ( views_state . open_views . perspective_of_view (
                 &ViewforestId::from_client_string ("R" . to_string ()) ),
               skgrepo ("main") );
  Ok (( )) }

/// Under foreign R, U, main's, and A, foreign's, each carrying
/// '(inPlaceOf M)', may bear it whichever perspective the save
/// names: each is a mirror of M. K, no mirror of M, is refused under
/// either, the refusal naming the mirror the named perspective shows:
/// A from the root homes (R's home is "foreign"), U from main's. A
/// root standing for another node has that node's home: K standing
/// for M at the root names M itself, in its own home "origin", from
/// the root homes, and U from main's.
fn the_tamper_check_accepts_a_mirror_whatever_perspective_the_save_names (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraph = InRustGraph::from_graphnodes (
    &read_all_skg_files_from_skgrepos (config) ? );
  let buffer_showing = |bearer : &str, repo : &str| -> String { format! (
    "* (skg node (gnid R) (skgRepos (nodeRepo foreign))) R\n** (skg node (gnid {}) (skgRepos (nodeRepo {})) (inPlaceOf M)) {}\n",
    bearer, repo, bearer ) };
  let mismatch_naming = |buffer : &str, followed : Perspective| -> Option<ID> {
    match buffer_to_validated_saveplan_with_fork_skgrepos_in_graph (
      buffer, &graph, config, &SkgrepoRestriction::unrestricted (config),
      &HashMap::new (), &WhatTheBaselineShows::default (), followed, false ) {
      Err (SaveError::BufferValidationErrors { errors, .. }) =>
        errors . iter () . find_map ( |e| match e {
          BufferValidationError::InPlaceOf_Mismatch (_, _, shown, _) =>
            Some ( shown . clone () . unwrap_or ( ID::from ("none") )),
          _ => None, } ),
      _ => None, } };
  for followed in [ skgrepo ("main"), Perspective::RootHomes ] {
    assert_eq! ( mismatch_naming (&buffer_showing ("U", "main"), followed . clone ()), None );
    assert_eq! ( mismatch_naming (&buffer_showing ("A", "foreign"), followed . clone ()), None ); }
  assert_eq! ( mismatch_naming (&buffer_showing ("K", "foreign"), skgrepo ("main")),
               Some ( ID::from ("U") ));
  assert_eq! ( mismatch_naming (&buffer_showing ("K", "foreign"), Perspective::RootHomes),
               Some ( ID::from ("A") ));
  { // At the root, K standing for M has M's home, "origin", as its
    // root's home.
    let root_k_standing_for_m : &str =
      "* (skg node (gnid K) (skgRepos (nodeRepo foreign)) (inPlaceOf M)) K\n";
    assert_eq! ( mismatch_naming (root_k_standing_for_m, skgrepo ("main")),
                 Some ( ID::from ("U") ));
    assert_eq! ( mismatch_naming (root_k_standing_for_m, Perspective::RootHomes),
                 Some ( ID::from ("M") )); }
  Ok (( )) }

/// The client is the authority on its buffer's perspective:
/// a view the connection holds as following main's, saved by a
/// client whose buffer still follows the root homes (drawn before
/// the switch, as after a lost answer, or by a client that never
/// switched), is checked and redrawn following the root homes, and
/// the connection takes that choice for it.
async fn a_held_view_follows_whatever_its_save_names (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (mut views_state, mut env, view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "R", &[ ID::from ("R") ]) ?;
  messages_answering (
    &switch_request ("R", skgrepo ("main")),
    &mut views_state, &env, config, handle_set_perspective_request );
  let view_id : ViewforestId = ViewforestId::from_client_string ("R" . to_string ());
  assert_eq! ( views_state . open_views . perspective_of_view (&view_id),
               skgrepo ("main") );
  let (mut stream, _read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  let response : SaveResponse =
    update_from_and_rerender_buffer_with_approvals (
      &mut stream, &view, &view, &mut env, &Ok ( view_id . clone () ),
      &mut views_state, &SkgrepoRestriction::unrestricted (config),
      true, &HashMap::new (), &HashSet::new (),
      &StrandedsReview::NotReviewed, DeleteFrom::NotChosen, &[], None,
      Perspective::RootHomes ) . await ?;
  assert! ( response . errors . is_empty (), "{:?}", response . errors );
  assert! ( line_of_gnid (&response . saved_view, "A")
            . is_some_and ( |l| l . contains ("(inPlaceOf M)") ),
    "the save is redrawn following R's home:\n{}", response . saved_view );
  assert_eq! ( views_state . open_views . perspective_of_view (&view_id),
               Perspective::RootHomes );
  Ok (( )) }

/// The user takes back a fork where it stands in place of another's
/// original: a view of foreign R, switched to main's perspective,
/// shows U in M's place, and 'delete' is written on U. The save
/// deletes U and nothing else: it asks to fork nothing, R keeps M, and
/// the redrawn view shows in U's place the mirror of M that main's
/// perspective now ranks first: A, in "foreign", which main's list
/// ranks above third's and origin's, by declaration order. Deleting a
/// mirror shown in its original's place deletes the mirror, not the
/// original's membership, so it edits no foreign parent.
async fn deleting_the_users_fork_in_a_foreign_parent_leaves_the_parent_and_shows_another_mirror (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (mut views_state, mut env, _view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "R", &[ ID::from ("R") ]) ?;
  let switched : String = rerendered_text ( &messages_answering (
    &switch_request ("R", skgrepo ("main")),
    &mut views_state, &env, config, handle_set_perspective_request ))
    . ok_or ("the switch rerenders the view") ?;
  let u_line : &str = line_of_gnid (&switched, "U")
    . filter ( |l| l . contains ("(inPlaceOf M)") )
    . ok_or_else ( || format! ("U stands in M's place under R:\n{}", switched) ) ?;
  let edited : String = switched . replace (
    u_line, &u_line . replace ("(inPlaceOf M)",
                               "(inPlaceOf M) (editRequest delete)") );
  let r_before : String = read_skgrepo_file (config, "foreign", "R");
  let (mut stream, _read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  let response : SaveResponse =
    update_from_and_rerender_buffer_with_approvals (
      &mut stream, &edited, &switched, &mut env,
      &Ok ( ViewforestId::from_client_string ("R" . to_string ()) ),
      &mut views_state, &SkgrepoRestriction::unrestricted (config),
      false, &HashMap::new (), &HashSet::new (),
      &StrandedsReview::NotReviewed, DeleteFrom::NotChosen, &[], None,
      skgrepo ("main") ) . await ?;
  assert! ( response . errors . is_empty (), "{:?}", response . errors );
  assert! ( response . fork_confirmation . is_none (),
    "deleting one's own fork asks to fork nothing:\n{}", response . saved_view );
  assert! ( read_all_skg_files_from_skgrepos (config) ?
            . iter () . all ( |node| node . pid != ID::from ("U") ),
    "U is deleted" );
  assert_eq! ( read_skgrepo_file (config, "foreign", "R"), r_before,
    "R is as its author wrote it" );
  assert! ( owned_fork_on_disk (config, "R") . is_err (), "R was not forked" );
  let saved : &str = &response . saved_view;
  assert! ( line_of_gnid (saved, "A")
            . is_some_and ( |l| l . starts_with ("** ")
                                && l . contains ("(inPlaceOf M)") ),
    "A, foreign's fork, is shown in U's place:\n{}", saved );
  assert! ( line_of_gnid (saved, "U") . is_none (), "{}", saved );
  Ok (( )) }

/// At the plan: 'delete' on a mirror standing for M leaves M in
/// its parent's list under a foreign parent, which is not forked;
/// removing the mirror's headline instead removes M, which forks
/// R. Under an owned parent, the deletion takes M out of the list too,
/// as it did before.
fn a_deleted_mirror_keeps_its_original_but_a_removed_one_does_not (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraph = InRustGraph::from_graphnodes (
    &read_all_skg_files_from_skgrepos (config) ? );
  let plan = |buffer : &str| -> Result<skg::types::save::SavePlan, SaveError> {
    buffer_to_validated_saveplan_with_fork_skgrepos_in_graph (
      buffer, &graph, config, &SkgrepoRestriction::unrestricted (config),
      &HashMap::new (), &WhatTheBaselineShows::default (),
      skgrepo ("main"), false ) . map ( |(_, plan, _)| plan ) };
  let deletes_u = |instructions : &[NodeInstruction]| -> bool {
    instructions . iter () . any ( |i| matches! (
      i, NodeInstruction::Delete (d) if d . gnid == ID::from ("U") )) };
  { // Under foreign R.
    let deleted : skg::types::save::SavePlan = plan ( indoc! {"
      * (skg node (gnid R) (skgRepos (nodeRepo foreign))) R
      ** (skg node (gnid U) (skgRepos (nodeRepo main)) (inPlaceOf M) (editRequest delete)) U
      ** (skg node (gnid K) (skgRepos (nodeRepo foreign))) K
      "} ) ?;
    assert! ( deletes_u (&deleted . node_instructions) );
    assert! ( deleted . fork_specs . is_empty (),
      "deleting U forks nothing: {:?}", deleted . fork_specs );
    assert! ( opt_saved_node_by_gnid (&deleted . node_instructions, "R") . is_none () );
    let removed : skg::types::save::SavePlan = plan ( indoc! {"
      * (skg node (gnid R) (skgRepos (nodeRepo foreign))) R
      ** (skg node (gnid K) (skgRepos (nodeRepo foreign))) K
      "} ) ?;
    assert! ( removed . fork_specs . iter ()
              . any ( |spec| spec . original_gnid == ID::from ("R") ),
      "removing U from R removes M, which forks R: {:?}", removed . fork_specs ); }
  { // Under owned P, which contains M.
    let deleted : skg::types::save::SavePlan = plan ( indoc! {"
      * (skg node (gnid P) (skgRepos (nodeRepo main))) P
      ** (skg node (gnid U) (skgRepos (nodeRepo main)) (inPlaceOf M) (editRequest delete)) U
      "} ) ?;
    assert! ( deletes_u (&deleted . node_instructions) );
    assert! ( opt_saved_node_by_gnid (&deleted . node_instructions, "P")
              . is_some_and ( |p| p . contains . is_empty () ),
      "P loses M" ); }
  Ok (( )) }
