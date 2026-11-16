// cargo nextest run --test grouped_forks -E 'test(override_substitution::)'
//
// Override substitution ('docs/override.org', "Automatic override
// substitution"):
// when the refresh would draw a viewnode for node N as recursive
// content, and an owned, visible R overrides N, it shows R
// instead, marked '(overridesHere N)' -- and saving such a view
// round-trips to the original IDs, so a container's contains list is
// never silently rewritten from N to R.
//
// Fixture (tests/override_substitution/fixtures, single skgrepo
// "main", owned): Q contains P and P2; P contains N and M;
// R overrides N and contains W; S subscribes to E; E contains N.
//
// The multi-repo fixture (fixtures-multi) adds the ownership and
// visibility gates: FR (skgrepo "foreign", not owned) overrides
// N1; R2 (skgrepo "other", owned) overrides N2; N3 lives in
// "other" while P3 and its overrider R3 live in "main".
//
// The foreign-root fixture (fixtures-foreign-root) gives a view the
// overrides of its root's author to follow: R (skgrepo "foreign", not
// owned) contains M and K; A, also foreign, and U, in "main", each
// override M; P, in "main", contains M, and W, in "main", contains U.
// A second foreign owner,
// "third", overrides M with B and K with T. Foreign F2 contains M2,
// which contains N2; U2, in "main", overrides M2, and T2, third's, N2.
//
// The replaced-root-folders fixture (fixtures-replaced-root-folders)
// gives a foreign root and the user's fork of it different folders:
// foreign L, titled "missive", alias "letter", contains L1, L2 and L3;
// foreign Q contains L, and foreign X links to it. J, in "main",
// titled "answer", alias "reply",
// subscribes to and overrides L, contains L1 and hides L2; V, in
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
  multi_root_view, multi_root_view_with_skgrepo_set};
use skg::types::errors::{BufferValidationError, SaveError};
use skg::types::misc::{ID, SkgConfig, SkgrepoName, SkgrepoOwner, TantivyIndex, members_of};
use skg::types::nodes::complete::Graphnode;
use skg::types::save::{NodeInstruction, SaveNode};
use skg::types::views_state::{OpenViews, OverridesFollowed, ViewforestId};
use skg::serve::handlers::overrides_followed::handle_set_overrides_followed_request;
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
  let fixtures : &str = "tests/override_substitution/fixtures";
  run_with_shared_test_stores (
    "skg-test-override-substitution",
    |s| Box::pin ( async move {
      s . reset ("de_novo_rendering_shows_the_overrider_marked", fixtures) ?;
      de_novo_rendering_shows_the_overrider_marked (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("save_roundtrips_to_original_and_is_a_fixpoint", fixtures) ?;
      save_roundtrips_to_original_and_is_a_fixpoint (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("extraction_honors_overridesHere", fixtures) ?;
      extraction_honors_overridesHere (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_second_owned_overrider_saves_and_q3s_mirror_stands", fixtures) ?;
      a_second_owned_overrider_saves_and_q3s_mirror_stands (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("an_owned_mutual_pair_saves_and_q3s_mirror_stands", fixtures) ?;
      an_owned_mutual_pair_saves_and_q3s_mirror_stands (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset_from_config ("ownership_and_visibility_gate_substitution",
                             "tests/override_substitution/fixtures-multi/skgconfig.toml") ?;
      ownership_and_visibility_gate_substitution (
        &s . config ) . await ?;
      s . reset_from_config ("chain_half_visible_keeps_the_original",
                             "tests/override_substitution/fixtures-chain/skgconfig.toml") ?;
      chain_half_visible_keeps_the_original (
        &s . config, &mut s . tantivy ) . await ?;
      let foreign_root : &str =
        "tests/override_substitution/fixtures-foreign-root";
      s . reset ("a_view_follows_the_overrides_of_its_roots_author", foreign_root) ?;
      a_view_follows_the_overrides_of_its_roots_author (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("under_a_foreign_root_a_save_roundtrips_its_authors_overrider", foreign_root) ?;
      under_a_foreign_root_a_save_roundtrips_its_authors_overrider (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("the_tamper_check_accepts_any_mirror_and_refuses_a_non_mirror", foreign_root) ?;
      the_tamper_check_accepts_any_mirror_and_refuses_a_non_mirror (
        &s . config ) . await ?;
      s . reset ("under_a_foreign_root_a_forked_child_is_shown_as_the_original", foreign_root) ?;
      under_a_foreign_root_a_forked_child_is_shown_as_the_original (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_forked_foreign_root_follows_the_users_overrides", foreign_root) ?;
      a_forked_foreign_root_follows_the_users_overrides (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_switched_view_follows_the_users_overrides_until_switched_back", foreign_root) ?;
      a_switched_view_follows_the_users_overrides_until_switched_back (
        &s . config, &mut s . tantivy ) ?;
      s . reset ("a_switch_of_perspective_re_points_a_substituted_content_child", foreign_root) ?;
      a_switch_of_perspective_re_points_a_substituted_content_child (
        &s . config, &mut s . tantivy ) ?;
      s . reset ("a_third_authors_mirror_stands_where_the_user_has_none",
                 "tests/override_substitution/fixtures-third-author") ?;
      a_third_authors_mirror_stands_where_the_user_has_none (
        &s . config, &mut s . tantivy ) ?;
      s . reset ("a_switch_to_the_users_overrides_is_refused_where_it_would_change_nothing", foreign_root) ?;
      a_switch_to_the_users_overrides_is_refused_where_it_would_change_nothing (
        &s . config, &mut s . tantivy ) ?;
      s . reset ("next_cycles_through_every_perspective_that_shows_the_view_differently", foreign_root) ?;
      next_cycles_through_every_perspective_that_shows_the_view_differently (
        &s . config, &mut s . tantivy ) ?;
      s . reset ("a_perspective_stays_in_the_cycle_where_another_hides_what_it_overrides", foreign_root) ?;
      a_perspective_stays_in_the_cycle_where_another_hides_what_it_overrides (
        &s . config, &mut s . tantivy ) ?;
      s . reset ("the_root_authors_are_named_in_the_order_of_their_first_roots", foreign_root) ?;
      the_root_authors_are_named_in_the_order_of_their_first_roots (
        &s . config, &mut s . tantivy ) ?;
      s . reset ("next_from_a_perspective_that_left_the_cycle_goes_back_to_the_root_authors", foreign_root) ?;
      next_from_a_perspective_that_left_the_cycle_goes_back_to_the_root_authors (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_perspective_replaces_the_root_and_the_root_authors_restore_it", foreign_root) ?;
      a_perspective_replaces_the_root_and_the_root_authors_restore_it (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_replaced_root_shows_its_own_containers", foreign_root) ?;
      a_replaced_root_shows_its_own_containers (
        &s . config, &mut s . tantivy ) ?;
      let replaced_root_folders : &str =
        "tests/override_substitution/fixtures-replaced-root-folders";
      s . reset ("a_replaced_root_shows_its_own_default_folders", replaced_root_folders) ?;
      a_replaced_root_shows_its_own_default_folders (
        &s . config, &mut s . tantivy ) ?;
      s . reset ("a_replaced_root_redraws_the_folders_it_was_asked_for", replaced_root_folders) ?;
      a_replaced_root_redraws_the_folders_it_was_asked_for (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_replaced_search_hit_stays_a_hit", replaced_root_folders) ?;
      a_replaced_search_hit_stays_a_hit (
        &s . config, &mut s . tantivy ) ?;
      s . reset ("a_save_following_the_users_overrides_shows_a_new_fork_in_place", foreign_root) ?;
      a_save_following_the_users_overrides_shows_a_new_fork_in_place (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_held_view_follows_whatever_its_save_names", foreign_root) ?;
      a_held_view_follows_whatever_its_save_names (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("the_tamper_check_accepts_a_mirror_whatever_perspective_the_save_names", foreign_root) ?;
      the_tamper_check_accepts_a_mirror_whatever_perspective_the_save_names (
        &s . config ) ?;
      s . reset ("deleting_the_users_fork_in_a_foreign_parent_leaves_the_parent_and_shows_the_original", foreign_root) ?;
      deleting_the_users_fork_in_a_foreign_parent_leaves_the_parent_and_shows_the_original (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_deleted_overrider_keeps_its_original_but_a_removed_one_does_not", foreign_root) ?;
      a_deleted_overrider_keeps_its_original_but_a_removed_one_does_not (
        &s . config ) ?;
      Ok (( )) } )) }

fn marked_lines<'a> (
  buf      : &'a str,
  original : &str,
) -> Vec<&'a str> {
  let fact : String =
    format! ("(overridesHere {})", original);
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

fn read_fixture_file (
  config : &SkgConfig,
  pid    : &str,
) -> String {
  let path : std::path::PathBuf =
    config . skgrepos . values () . next () . unwrap ()
    . path . join ( format! ("{}.skg", pid) );
  std::fs::read_to_string (&path)
    . unwrap_or_else ( |e| panic! ("reading {:?}: {}", path, e) ) }

async fn de_novo_rendering_shows_the_overrider_marked (
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
                      && ! l . contains ("overridesHere") ),
        "M, not overridden, is shown raw:\n{}", view );
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

/// M, in N's skgrepo, comes to fork N beside R (saved through the
/// save's graph update). N's
/// three mirrors tie in that skgrepo, R and M both forked by none of
/// the others, so N stands as itself (Q3, rule 3), where R stood before
/// the save; and M, whose mirrors are the same three, stands as itself.
/// R and M stand in N's forks folder, forkers claiming nothing.
async fn a_second_owned_overrider_saves_and_q3s_mirror_stands (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (before, _pids, _tree) =
    multi_root_view ( config, Some (tantivy), &[ ID::from ("P") ] ) ?;
  assert! ( marked_lines (&before, "N") . iter ()
            . any ( |l| l . contains ("(gnid R)") ),
            "before the save, R stands in N's place:\n{}", before );
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
            && ! r_lines . is_empty ()
            && r_lines . iter () . all ( |line| line . contains ("(birth (forks out 2))") ),
            "N and M are shown as themselves, R only as a forker in N's forks folder:\n{}", after );
  Ok (( )) }

/// N comes to fork R, which forks N: an owned mutual pair, in
/// one skgrepo (saved as above). Each is forked by the other, so N
/// stands as itself (Q3, rule 3), where R stood before the save; R
/// is drawn in N's forks folder, a mutual, born of both sides.
async fn an_owned_mutual_pair_saves_and_q3s_mirror_stands (
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

async fn extraction_honors_overridesHere (
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
          ** (skg node (gnid R) (skgRepos (nodeRepo main)) writeProtected (overridesHere N)) R
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
          ** (skg node (gnid R) (skgRepos (nodeRepo main)) writeProtected (overridesHere N)) R
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
          *** (skg node (gnid R) (skgRepos (nodeRepo main)) writeProtected (overridesHere N)) R
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
          ** (skg node (gnid R) (skgRepos (nodeRepo main)) (overridesHere N)) R-edited
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
          ** (skg node (gnid M) (skgRepos (nodeRepo main)) writeProtected (overridesHere W)) M
        "};
        match node_instructions_from (buffer, config) . await {
          Err (SaveError::BufferValidationErrors { errors, .. }) => {
            assert! ( errors . iter () . any ( |e| matches! (
              e, BufferValidationError::OverridesHere_Mismatch (..) )),
              "expected OverridesHere_Mismatch, got: {:?}", errors ); },
          other => panic! (
            "tampered fact must abort the save; got: {:?}",
            other . map ( |v| v . len () ) ), }}
      { // Subscribee-as-such: the visible-content signal speaks of
        // the original, so no spurious hide of N is inferred.
        let buffer = indoc! {"
          * (skg node (gnid S) (skgRepos (nodeRepo main))) S
          ** (skg forksFolder)
          *** (skg node (gnid E) (skgRepos (nodeRepo main))) E
          **** (skg node (gnid R) (skgRepos (nodeRepo main)) writeProtected (overridesHere N)) R
        "};
        let instructions : Vec<NodeInstruction> =
          node_instructions_from (buffer, config) . await ?;
        if let Some (s_node) = opt_saved_node_by_gnid (&instructions, "S") {
          assert! (
            ! s_node . hides . or_default ()
              . iter () . any ( |m| &m . member == &ID::from ("N") ),
            "S must not hide N: the shown R stands for N" ); }}
      { // Deleting R where it stands for N there deletes R and hides
        // N from S, as removing R does: a deleted overrider keeps its
        // original only in the content of a parent the user does not
        // own ('content_members'), and a subscribee as such has no
        // content the save writes, only S's hides. Nor is N filed in S.
        let buffer = indoc! {"
          * (skg node (gnid S) (skgRepos (nodeRepo main))) S
          ** (skg forksFolder)
          *** (skg node (gnid E) (skgRepos (nodeRepo main))) E
          **** (skg node (gnid R) (skgRepos (nodeRepo main)) (overridesHere N) (editRequest delete)) R
        "};
        let instructions : Vec<NodeInstruction> =
          node_instructions_from (buffer, config) . await ?;
        assert! ( instructions . iter () . any ( |i| matches! (
                    i, NodeInstruction::Delete (d) if d . gnid == ID::from ("R") )),
          "R is deleted" );
        let s_node : &Graphnode = saved_node_by_gnid (&instructions, "S");
        assert! (
          s_node . hides . or_default ()
            . iter () . any ( |m| &m . member == &ID::from ("N") ),
          "S hides N" );
        assert! (
          ! s_node . contains
            . iter () . any ( |m| &m . member == &ID::from ("N") ),
          "S does not file N" ); }
      Ok (( )) }

async fn ownership_and_visibility_gate_substitution (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
      (
        graph_handle_from_config (config) ? );
      { // A foreign overrider never substitutes.
        let (view, _pids, _tree) =
          multi_root_view (
            config, None,
            &[ ID::from ("P1") ] ) ?;
        assert! ( marked_lines (&view, "N1") . is_empty (),
          "FR is foreign; N1 is shown raw:\n{}", view );
        assert! ( view . contains ("(gnid N1)"), "{}", view ); }
      let restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (
          config, SkgrepoName ("main" . to_string ())) ?;
      { // A restricted owned overrider does not substitute.
        let (view, _pids, _tree) =
          multi_root_view_with_skgrepo_set (
            config, None,
            &[ ID::from ("P2") ], &restriction ) ?;
        assert! ( marked_lines (&view, "N2") . is_empty (),
          "R2's repo is restricted; N2 is shown raw:\n{}", view );
        assert! ( view . contains ("(gnid N2)"), "{}", view ); }
      { // The same overrider substitutes when its skgrepo is unrestricted.
        let (view, _pids, _tree) =
          multi_root_view (
            config, None,
            &[ ID::from ("P2") ] ) ?;
        let marked : Vec<&str> = marked_lines (&view, "N2");
        assert_eq! ( marked . len (), 1, "{}", view );
        assert! ( marked [0] . contains ("(gnid R2)"), "{}", view ); }
      { // The skgrepo restriction beats substitution: restricted original,
        // recorded in an unrestricted section, unrestricted overrider
        // -> the original stands as a Restricted vognode, and the
        // overrider is not shown in its place.
        let (view, _pids, _tree) =
          multi_root_view_with_skgrepo_set (
            config, None,
            &[ ID::from ("P3") ], &restriction ) ?;
        assert! ( view . contains ("restricted (gnid N3)"),
          "the restricted original stands as a Restricted vognode:\n{}", view );
        assert! ( ! view . contains ("(gnid R3)"),
          "its overrider must not be shown in its place (the \
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

/// A half-visible owned chain: D overrides C overrides N, with the
/// end D in skgrepo 'other'. Unrestricted the END D substitutes for N;
/// under 'main' (hiding 'other') the middle C substitutes. Saving the
/// half-visible view accepts the middle carrier (it is on N's chain)
/// and keeps N in P's contains; an off-chain fact is rejected.
async fn chain_half_visible_keeps_the_original (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      (
        graph_handle_from_config (config) ? );
      { // Unrestricted, the chain end D is shown in N's place.
        let (view, _p, _t) =
          multi_root_view (
            config, Some (tantivy),
            &[ ID::from ("P") ] ) ?;
        let marked : Vec<&str> = marked_lines (&view, "N");
        assert_eq! ( marked . len (), 1,
          "one substitute for N unrestricted:\n{}", view );
        assert! ( marked [0] . contains ("(gnid D)"),
          "the chain end D is shown unrestricted:\n{}", view ); }
      let main_set : SkgrepoRestriction =
        SkgrepoRestriction::named (
          config, SkgrepoName ("main" . to_string ())) ?;
      let view_main : String = {
        // Under 'main', D's skgrepo 'other' is restricted, so the middle
        // C is shown instead.
        let (view, _p, _t) =
          multi_root_view_with_skgrepo_set (
            config, Some (tantivy),
            &[ ID::from ("P") ], &main_set ) ?;
        let marked : Vec<&str> = marked_lines (&view, "N");
        assert_eq! ( marked . len (), 1,
          "one substitute for N under main:\n{}", view );
        assert! ( marked [0] . contains ("(gnid C)"),
          "the chain middle C is shown under main:\n{}", view );
        view };
      { // Saving the half-visible view accepts the middle carrier and
        // keeps N (not C) in P's contains.
        let saved : String =
          save_under_set (&view_main, config, tantivy, &main_set)
          . await ?;
        assert_eq! ( marked_lines (&saved, "N") . len (), 1,
          "the rerendered saved view still shows a marked substitute \
           for N:\n{}", saved );
        let p_file : String = {
          let main_path : &std::path::Path =
            & config . skgrepos
              . get ( &SkgrepoName::from ("main") ) . unwrap () . path;
          std::fs::read_to_string ( main_path . join ("P.skg") )
            . unwrap () };
        assert! ( p_file . contains ("- N"),
          "P still contains N after the half-visible save:\n{}", p_file );
        assert! ( ! p_file . contains ("- C"),
          "P must NOT be rewritten to contain the carrier C:\n{}",
          p_file ); }
      { // A fact on a node not on N's chain is rejected.
        let buffer = indoc! {"
          * (skg node (gnid P) (skgRepos (nodeRepo main))) P
          ** (skg node (gnid D) (skgRepos (nodeRepo other)) writeProtected (overridesHere P)) D
        "};
        match node_instructions_from (buffer, config) . await {
          Err (SaveError::BufferValidationErrors { errors, .. }) =>
            assert! ( errors . iter () . any ( |e| matches! (
              e, BufferValidationError::OverridesHere_Mismatch (..) )),
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

/// R, foreign, contains M; A, of R's author, and U, the user's, each
/// override M; P, owned, contains M. A view of R shows A in M's
/// place, a view of P shows U, and a view of both shows each under
/// its own root.
async fn a_view_follows_the_overrides_of_its_roots_author (
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
    "under R, R's author's A is shown in M's place:\n{}", view );
  assert! ( ! marked [0] . contains ("writeProtected"),
    "and editable, so the substitution is the editable kind:\n{}", view );
  assert! ( marked [1] . starts_with ("** ")
            && marked [1] . contains ("(gnid U)"),
    "under P, the user's U is shown in M's place:\n{}", view );
  Ok (( )) }

/// Saving a view of foreign R, unchanged, where A stands for M, writes
/// nothing to R, passes the overridesHere tamper check, and is a
/// fixpoint.
async fn under_a_foreign_root_a_save_roundtrips_its_authors_overrider (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  graph_handle_from_config (config) ?;
  let r_before : String = read_skgrepo_file (config, "foreign", "R");
  let (view, _pids, _tree) =
    multi_root_view ( config, Some (tantivy), &[ ID::from ("R") ] ) ?;
  assert! ( line_of_gnid (&view, "A")
            . is_some_and ( |l| l . contains ("(overridesHere M)") ),
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
/// carrier, whichever the tree's perspective would show: under owned
/// P, whose tree is drawn from the user's perspective (which shows U
/// in M's place), A, a fork of M by M's own author, carrying
/// '(overridesHere M)', saves. K, no mirror of M, carrying it, aborts
/// the save, the refusal naming U, the mirror shown.
async fn the_tamper_check_accepts_any_mirror_and_refuses_a_non_mirror (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  graph_handle_from_config (config) ?;
  let carried_by = |carrier : &str| -> String { format! (
    "* (skg node (gnid P) (skgRepos (nodeRepo main))) P\n\
     ** (skg node (gnid {carrier}) (skgRepos (nodeRepo foreign)) (overridesHere M)) {carrier}\n" ) };
  assert! ( node_instructions_from (&carried_by ("A"), config) . await . is_ok (),
    "a mirror the tree's perspective would not show is still an honest carrier" );
  match node_instructions_from (&carried_by ("K"), config) . await {
    Err (SaveError::BufferValidationErrors { errors, .. }) =>
      assert! ( errors . iter () . any ( |e| matches! (
        e, BufferValidationError::OverridesHere_Mismatch (
             Some (carrier), original, Some (shown), _ )
           if carrier == &ID::from ("K") && original == &ID::from ("M")
              && shown == &ID::from ("U") )),
        "a carrier that is no mirror of M must abort, naming U; got: {:?}",
        errors ),
    other => panic! (
      "a carrier that is no mirror of M must abort the save; got: {:?}",
      other . map ( |v| v . len () ) ), }
  Ok (( )) }

/// Intended: under a foreign root the view shows what the root's
/// author would see, so a child of R the user edits there, which
/// forks it into the user's clone, is shown afterward as the
/// original, unedited, and not as the clone. The clone is shown where
/// the user's own overrides are followed (under an owned root, or
/// under a fork of R: 'a_forked_foreign_root_follows_the_users_overrides').
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
            . is_some_and ( |l| l . contains ("(overridesHere M)") ),
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
/// user's clone of R, which stands for nothing (no '(overridesHere
/// R)': the root authors' would show R raw in its place), and beneath
/// it the user's overrides are followed: U, not A, is shown in M's
/// place.
async fn a_forked_foreign_root_follows_the_users_overrides (
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
  let saved : String = save_and_rerender (&edited, config, tantivy) . await ?;
  let clone : Graphnode = owned_fork_on_disk (config, "R") ?;
  let root_line : &str = saved . lines () . next () . unwrap ();
  assert! ( root_line . contains (&format! ("(gnid {})", clone . pid . 0))
            && ! root_line . contains ("overridesHere"),
    "the root is R's clone:\n{}", saved );
  let marked : Vec<&str> = marked_lines (&saved, "M");
  assert! ( marked . len () == 1 && marked [0] . contains ("(gnid U)"),
    "under the clone of R, the user's U is shown in M's place:\n{}", saved );
  assert! ( line_of_gnid (&saved, "A") . is_none (),
    "under the clone of R, A is not shown:\n{}", saved );
  Ok (( )) }

//
// Switching whose overrides a view follows
// ('docs/api-and-formats.org', "Set overrides followed").
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
  followed  : OverridesFollowed,
) -> String {
  switch_request_naming (view_id, &followed . wire_name ()) }

fn switch_request_naming (
  view_id   : &str,
  named     : &str,
) -> String {
  format! (
    "((request . \"set overrides followed\") (view-id . \"{}\") (overrides-followed . \"{}\"))",
    view_id, named ) }

fn owner_of (
  skgrepo : &str,
) -> OverridesFollowed {
  OverridesFollowed::Owner ( SkgrepoOwner::Foreign ( SkgrepoName::from (skgrepo) )) }

/// The one rerendered view's text among 'messages', if one was sent.
fn rerendered_text (
  messages : &[String],
) -> Option<String> {
  messages . iter ()
    . find ( |message| message . contains ("(response-type rerender-view)") )
    . and_then ( |message| extract_string_field_from_sexp (message, "content") ) }

/// A view of foreign R shows A, R's author's, in M's place. Switched
/// to the user's overrides, it is rerendered with U, the user's, there
/// instead; the answer names the view and the choice; a foreign redraw
/// (the "rerender views" a reload asks for) keeps it. Switched back,
/// it shows A again.
fn a_switched_view_follows_the_users_overrides_until_switched_back (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (mut views_state, env, view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "R", &[ ID::from ("R") ]) ?;
  assert! ( line_of_gnid (&view, "A") . is_some_and ( |l| l . contains ("(overridesHere M)") ),
    "a fresh view of R follows R's author:\n{}", view );
  let messages : Vec<String> = messages_answering (
    &switch_request ("R", OverridesFollowed::user ()),
    &mut views_state, &env, config, handle_set_overrides_followed_request );
  let answer : &String = messages . first () . unwrap ();
  assert! ( answer . contains ("(response-type overrides-followed)")
            && extract_string_field_from_sexp (answer, "view-id") . as_deref () == Some ("R")
            && extract_string_field_from_sexp (answer, "overrides-followed") . as_deref () == Some ("user")
            && extract_string_field_from_sexp (answer, "content") . is_none (),
    "the answer names the view and the new choice: {:?}", messages );
  let switched : String = rerendered_text (&messages)
    . ok_or_else ( || format! ("the view is rerendered: {:?}", messages) ) ?;
  assert! ( line_of_gnid (&switched, "U") . is_some_and ( |l| l . contains ("(overridesHere M)") )
            && line_of_gnid (&switched, "A") . is_none (),
    "switched, the user's U stands in M's place:\n{}", switched );
  assert_eq! ( views_state . open_views . overrides_followed_by_view (
                 &ViewforestId::from_client_string ("R" . to_string ()) ),
               OverridesFollowed::user () );
  let redrawn : String = rerendered_text ( &messages_answering (
    "((request . \"rerender views\") (view-ids \"R\"))",
    &mut views_state, &env, config, handle_rerender_views_request ))
    . ok_or ("the redraw rerenders the view") ?;
  assert! ( line_of_gnid (&redrawn, "U") . is_some (),
    "a redraw the user did not ask for keeps the choice:\n{}", redrawn );
  let back : String = rerendered_text ( &messages_answering (
    &switch_request ("R", OverridesFollowed::RootAuthors),
    &mut views_state, &env, config, handle_set_overrides_followed_request ))
    . ok_or ("switching back rerenders the view") ?;
  assert! ( line_of_gnid (&back, "A") . is_some_and ( |l| l . contains ("(overridesHere M)") )
            && line_of_gnid (&back, "U") . is_none (),
    "switched back, R's author's A stands in M's place again:\n{}", back );
  Ok (( )) }

/// Every mirror is an honest carrier, so a content child already
/// shown in another's place is re-pointed whenever the view's
/// perspective ranks another mirror first: a view of owned P shows U,
/// the user's, in M's place; switched to third's perspective, B; to
/// M's own author's, A; back to the root authors', U again.
fn a_switch_of_perspective_re_points_a_substituted_content_child (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (mut views_state, env, view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "P", &[ ID::from ("P") ]) ?;
  assert! ( line_of_gnid (&view, "U") . is_some_and ( |l| l . contains ("(overridesHere M)") ),
    "a fresh view of P shows the user's U in M's place:\n{}", view );
  for (perspective, shown) in [ ( owner_of ("third"), "B" ),
                                ( owner_of ("foreign"), "A" ),
                                ( OverridesFollowed::RootAuthors, "U" ) ] {
    let switched : String = rerendered_text ( &messages_answering (
      &switch_request ("P", perspective . clone ()),
      &mut views_state, &env, config, handle_set_overrides_followed_request ))
      . ok_or_else ( || format! ("the switch to {:?} rerenders the view", perspective) ) ?;
    let marked : Vec<&str> = marked_lines (&switched, "M");
    assert! ( marked . len () == 1 && marked [0] . contains ( &format! ("(gnid {})", shown) ),
      "from {:?}, {} stands in M's place, alone:\n{}", perspective, shown, switched ); }
  Ok (( )) }

/// Where the perspective's owner has no mirror of a note, the first
/// mirror by declaration order stands, whoever wrote it: P, the
/// user's, contains N, alice's, which bob's F forks; the config
/// declares bob's skgrepo before alice's, so a view of P shows F in
/// N's place, born of its forking N ('(birth (forks out))'). From
/// alice's perspective N shows raw there; and under Q, which contains
/// F, N stands in F's place, born of being forked by it
/// ('(birth (forks in))').
fn a_third_authors_mirror_stands_where_the_user_has_none (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (mut views_state, env, view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "P", &[ ID::from ("P") ]) ?;
  let marked : Vec<&str> = marked_lines (&view, "N");
  assert! ( marked . len () == 1 && marked [0] . contains ("(gnid F)")
            && marked [0] . contains ("(birth (forks out))"),
    "the user has no mirror of N, so bob's F, declared first, stands:\n{}", view );
  let alices : String = rerendered_text ( &messages_answering (
    &switch_request ("P", owner_of ("alice")),
    &mut views_state, &env, config, handle_set_overrides_followed_request ))
    . ok_or ("the switch to alice's perspective rerenders the view") ?;
  assert! ( marked_lines (&alices, "N") . is_empty ()
            && line_of_gnid (&alices, "N") . is_some (),
    "from alice's perspective N shows raw:\n{}", alices );
  let (mut views_state, env, _view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "Q", &[ ID::from ("Q") ]) ?;
  let alices_q : String = rerendered_text ( &messages_answering (
    &switch_request ("Q", owner_of ("alice")),
    &mut views_state, &env, config, handle_set_overrides_followed_request ))
    . ok_or ("the switch to alice's perspective rerenders the view of Q") ?;
  let in_fs_place : Vec<&str> = marked_lines (&alices_q, "F");
  assert! ( in_fs_place . len () == 1 && in_fs_place [0] . contains ("(gnid N)")
            && in_fs_place [0] . contains ("(birth (forks in))"),
    "from alice's perspective N, which F forks, stands in F's place:\n{}", alices_q );
  Ok (( )) }

/// A view whose every root the user owns shows the user's overrides
/// already, so a switch to them is refused, with the reason, the
/// unchanged perspective and an empty rerender stream; "next" skips
/// them, to the owners who would show it differently, foreign then
/// third; a
/// switch back to the root authors' is not refused. A view the
/// connection does not hold, and a request naming no perspective,
/// are refused too.
fn a_switch_to_the_users_overrides_is_refused_where_it_would_change_nothing (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (mut views_state, env, _view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "P", &[ ID::from ("P") ]) ?;
  let owned : Vec<String> = messages_answering (
    &switch_request ("P", OverridesFollowed::user ()),
    &mut views_state, &env, config, handle_set_overrides_followed_request );
  assert! ( refusal_of (&owned) . is_some_and ( |reason| reason . contains ("Your overrides would show this buffer as its root authors' do") ),
    "{:?}", owned );
  assert! ( extract_string_field_from_sexp (&owned [0], "overrides-followed")
            . as_deref () == Some ("root authors")
            && extract_string_field_from_sexp (&owned [0], "perspective-name")
            . as_deref () == Some ("each root author's overrides (yours)"),
    "the refusal carries the unchanged perspective: {:?}", owned );
  for (expected_wire, expected_overrider) in [ ("owner of foreign", "A"),
                                               ("owner of third",   "B") ] {
    let next : Vec<String> = messages_answering (
      &switch_request_naming ("P", "next"),
      &mut views_state, &env, config, handle_set_overrides_followed_request );
    assert! ( extract_string_field_from_sexp (&next [0], "overrides-followed")
              . as_deref () == Some (expected_wire)
              && rerendered_text (&next) . is_some_and ( |text| line_of_gnid (&text, expected_overrider)
                   . is_some_and ( |l| l . contains ("(overridesHere M)") )),
      "next skips the user's, which would change nothing, for each foreign owner's in turn: {:?}", next ); }
  let back : Vec<String> = messages_answering (
    &switch_request ("P", OverridesFollowed::RootAuthors),
    &mut views_state, &env, config, handle_set_overrides_followed_request );
  assert! ( extract_string_field_from_sexp (&back [0], "content") . is_none ()
            && rerendered_text (&back) . is_some (),
    "a switch to the root authors' is not refused: {:?}", back );
  let unheld : Vec<String> = messages_answering (
    &switch_request ("R", OverridesFollowed::user ()),
    &mut views_state, &env, config, handle_set_overrides_followed_request );
  assert! ( refusal_of (&unheld) . is_some_and ( |reason| reason . contains ("does not hold") ),
    "{:?}", unheld );
  let unnamed : Vec<String> = messages_answering (
    "((request . \"set overrides followed\") (view-id . \"P\"))",
    &mut views_state, &env, config, handle_set_overrides_followed_request );
  assert! ( refusal_of (&unnamed) . is_some (), "{:?}", unnamed );
  let unknown_owner : Vec<String> = messages_answering (
    &switch_request_naming ("P", "owner of nobody"),
    &mut views_state, &env, config, handle_set_overrides_followed_request );
  assert! ( refusal_of (&unknown_owner) . is_some (), "{:?}", unknown_owner );
  Ok (( )) }

/// The refusal in 'messages', the answer to a switch: its reason, if
/// it is one. Asserts the shape every answer has: the answer first,
/// and, for a refusal, an empty rerender stream.
fn refusal_of (
  messages : &[String],
) -> Option<String> {
  let answer : &String = messages . first () ?;
  assert! ( answer . contains ("(response-type overrides-followed)"), "{:?}", messages );
  let refusal : Option<String> = extract_string_field_from_sexp (answer, "content");
  if refusal . is_some () {
    assert! ( rerendered_text (messages) . is_none ()
              && messages . iter () . any ( |m| m . contains ("(response-type rerender-done)") ),
      "a refusal is followed by an empty rerender stream: {:?}", messages ); }
  refusal }

/// "next" cycles a view of foreign R through every perspective that
/// would show it differently, in a fixed order, the cycle computed
/// afresh at each request: the root authors' (A in M's place), the
/// user's (U), third's (B in M's place and T in K's), and back. Each
/// answer names the perspective as a message to the user would.
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
      &mut views_state, &env, config, handle_set_overrides_followed_request );
    assert! ( refusal_of (&messages) . is_none (), "{:?}", messages );
    assert_eq! ( extract_string_field_from_sexp (&messages [0], "overrides-followed")
                   . as_deref (), Some (expected_wire), "{:?}", messages );
    assert_eq! ( extract_string_field_from_sexp (&messages [0], "perspective-name")
                   . as_deref (), Some (expected_name), "{:?}", messages );
    let text : String = rerendered_text (&messages) . ok_or ("the switch rerenders") ?;
    assert! ( line_of_gnid (&text, shown_for_m)
              . is_some_and ( |l| l . contains ("(overridesHere M)") ),
      "{} stands in M's place:\n{}", shown_for_m, text );
    Ok (text) };
  next ("user", "your overrides", "U") ?;
  let third : String = next ("owner of third", "third's overrides", "B") ?;
  assert! ( line_of_gnid (&third, "T") . is_some_and ( |l| l . contains ("(overridesHere K)") ),
    "third's T stands in K's place:\n{}", third );
  next ("root authors", "each root author's overrides (foreign's)", "A") ?;
  next ("user", "your overrides", "U") ?;
  Ok (( )) }

/// The cycle does not depend on the path taken. In a view of F2, the
/// user's U2 stands in M2's place and holds nothing, so N2, which
/// third overrides, is not drawn from the user's perspective; third's
/// is still the next after it, as found from the root authors'
/// drawing, and a request naming it from there is not refused.
fn a_perspective_stays_in_the_cycle_where_another_hides_what_it_overrides (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (mut views_state, env, _view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "F2", &[ ID::from ("F2") ]) ?;
  let mut switch = |named : &str| -> Vec<String> { messages_answering (
    &switch_request_naming ("F2", named),
    &mut views_state, &env, config, handle_set_overrides_followed_request ) };
  let to_user : Vec<String> = switch ("next");
  assert! ( extract_string_field_from_sexp (&to_user [0], "overrides-followed")
              . as_deref () == Some ("user")
            && rerendered_text (&to_user) . is_some_and ( |text|
                 line_of_gnid (&text, "U2") . is_some () && line_of_gnid (&text, "N2") . is_none () ),
    "{:?}", to_user );
  let named : Vec<String> = switch ("owner of third");
  assert! ( refusal_of (&named) . is_none (), "{:?}", named );
  let back_to_user : Vec<String> = switch ("user");
  assert! ( refusal_of (&back_to_user) . is_none (), "{:?}", back_to_user );
  let to_third : Vec<String> = switch ("next");
  assert! ( extract_string_field_from_sexp (&to_third [0], "overrides-followed")
              . as_deref () == Some ("owner of third")
            && rerendered_text (&to_third) . is_some_and ( |text|
                 line_of_gnid (&text, "T2") . is_some_and ( |l| l . contains ("(overridesHere N2)") )),
    "from the user's, the next is third's, which shows T2 in N2's place: {:?}", to_third );
  Ok (( )) }

/// A view with several roots names its root authors' perspective by
/// each author, once, in the order of their first roots: "yours" for
/// the user.
fn the_root_authors_are_named_in_the_order_of_their_first_roots (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (mut views_state, env, _view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (
      config, tantivy, "RPK", &[ ID::from ("R"), ID::from ("P"), ID::from ("K") ] ) ?;
  messages_answering (
    &switch_request ("RPK", OverridesFollowed::user ()),
    &mut views_state, &env, config, handle_set_overrides_followed_request );
  let back : Vec<String> = messages_answering (
    &switch_request ("RPK", OverridesFollowed::RootAuthors),
    &mut views_state, &env, config, handle_set_overrides_followed_request );
  assert_eq! ( extract_string_field_from_sexp (&back [0], "perspective-name") . as_deref (),
               Some ("each root author's overrides (foreign's, yours)"), "{:?}", back );
  Ok (( )) }

/// A view in which no one's perspective would show anything
/// differently (one of L, which has no mirror and holds nothing) has
/// a cycle of one, so "next" is refused there. But a
/// view whose perspective has left its cycle, as a save can leave it
/// (a save names its buffer's perspective, and it is kept whatever the
/// cycle is now), goes back to the root authors' on "next".
async fn next_from_a_perspective_that_left_the_cycle_goes_back_to_the_root_authors (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (mut views_state, mut env, view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "L", &[ ID::from ("L") ]) ?;
  let lonely : Vec<String> = messages_answering (
    &switch_request_naming ("L", "next"),
    &mut views_state, &env, config, handle_set_overrides_followed_request );
  assert! ( refusal_of (&lonely) . is_some_and ( |reason| reason . contains ("No one's overrides") ),
    "{:?}", lonely );
  let view_id : ViewforestId = ViewforestId::from_client_string ("L" . to_string ());
  let (mut stream, _read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  let response : SaveResponse =
    update_from_and_rerender_buffer_with_approvals (
      &mut stream, &view, &view, &mut env, &Ok ( view_id . clone () ),
      &mut views_state, &SkgrepoRestriction::unrestricted (config),
      true, &HashMap::new (), &HashSet::new (),
      &StrandedsReview::NotReviewed, DeleteFrom::NotChosen, &[], None,
      owner_of ("third") ) . await ?;
  assert! ( response . errors . is_empty (), "{:?}", response . errors );
  assert_eq! ( views_state . open_views . overrides_followed_by_view (&view_id),
               owner_of ("third") );
  let back : Vec<String> = messages_answering (
    &switch_request_naming ("L", "next"),
    &mut views_state, &env, config, handle_set_overrides_followed_request );
  assert! ( refusal_of (&back) . is_none ()
            && extract_string_field_from_sexp (&back [0], "overrides-followed")
               . as_deref () == Some ("root authors"),
    "{:?}", back );
  Ok (( )) }

/// The root too: a view of M, which A (M's own author's), U (the
/// user's) and B (third's) each override, shows M raw from the root
/// authors' perspective, as a fresh view does, and each owner's
/// override of it from that owner's, marked '(overridesHere M)'. The
/// root is re-pointed where it stands, so it keeps its viewnode ID
/// both ways, and the root authors' perspective shows M raw again. A
/// save through the replaced root edits the overrider, not M.
async fn a_perspective_replaces_the_root_and_the_root_authors_restore_it (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (mut views_state, mut env, view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "M", &[ ID::from ("M") ]) ?;
  let root_line = |text : &str| -> String { text . lines () . next () . unwrap () . to_string () };
  assert! ( root_line (&view) . contains ("(vnid 0a) node (gnid M)")
            && ! view . contains ("overridesHere"),
    "a fresh view shows its root raw:\n{}", view );
  let mut next = |expected_wire : &str| -> Result<String, Box<dyn Error>> {
    let messages : Vec<String> = messages_answering (
      &switch_request_naming ("M", "next"),
      &mut views_state, &env, config, handle_set_overrides_followed_request );
    assert_eq! ( extract_string_field_from_sexp (&messages [0], "overrides-followed")
                   . as_deref (), Some (expected_wire), "{:?}", messages );
    Ok ( root_line ( &rerendered_text (&messages) . ok_or ("the switch rerenders") ? )) };
  for (wire, shown) in [ ("user", "U"), ("owner of foreign", "A"), ("owner of third", "B") ] {
    let root : String = next (wire) ?;
    assert! ( root . contains ( &format! ("(vnid 0a) node (gnid {})", shown) )
              && root . contains ("(overridesHere M)"),
      "from {}, the root is {}, standing for M: {}", wire, shown, root ); }
  let back : String = next ("root authors") ?;
  assert! ( back . contains ("(vnid 0a) node (gnid M)") && ! back . contains ("overridesHere"),
    "the root authors' show M raw again: {}", back );
  let switched : String = { let messages : Vec<String> = messages_answering (
      &switch_request ("M", OverridesFollowed::user ()),
      &mut views_state, &env, config, handle_set_overrides_followed_request );
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
      OverridesFollowed::user () ) . await ?;
  assert! ( response . errors . is_empty (), "{:?}", response . errors );
  assert! ( read_skgrepo_file (config, "main", "U") . contains ("title: U, retitled"),
    "the save edits U" );
  assert! ( read_skgrepo_file (config, "foreign", "M") . contains ("title: M"),
    "and leaves M" );
  assert! ( root_line (&response . saved_view) . contains ("(gnid U)")
            && root_line (&response . saved_view) . contains ("(overridesHere M)")
            && response . saved_view . contains ("U, retitled"),
    "the saved view is still shown from the user's perspective:\n{}", response . saved_view );
  Ok (( )) }

/// A replaced root's containers folder is the replacement's: a view
/// of M lists P and R, which contain M. From the user's perspective
/// the root is U, and its folder lists W, which contains U, as a fresh
/// view of U would, and nothing of M's; from foreign's it is A, which
/// nothing contains, so there is no folder; and back at the root
/// authors', M's folder is drawn as the fresh view drew it.
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
      &mut views_state, &env, config, handle_set_overrides_followed_request );
    assert! ( refusal_of (&messages) . is_none (), "{:?}", messages );
    Ok ( rerendered_text (&messages) . ok_or ("the switch rerenders") ? ) };
  let users : String = switch ("user") ?;
  let users_container_folder : Vec<&str> = users . lines ()
    . skip_while ( |l| ! l . contains ("containerFolder") ) . skip (1)
    . take_while ( |l| l . starts_with ("***") )
    . collect ();
  assert! ( users_container_folder . len () == 1
            && users_container_folder [0] . contains ("(gnid W)")
            && ! users . contains ("noWarrant"),
    "from the user's, U's folder lists W, which contains U, and nothing of M's \
     (M's containers show only under M, in U's forks folder, since U \
     forks M):\n{}", users );
  assert_eq! ( without_vnids ( users . lines () . skip (1) ),
               without_vnids ( fresh_view_of_u . lines () . skip (1) ),
    "U's folder is drawn as a fresh view of U draws it" );
  let foreigns : String = switch ("owner of foreign") ?;
  assert! ( line_of_gnid (&foreigns, "A") . is_some ()
            && ! foreigns . contains ("containerFolder"),
    "from foreign's, A, which nothing contains, has no containers folder:\n{}", foreigns );
  let back : String = switch ("root authors") ?;
  assert_eq! ( without_vnids ( back . lines () ), without_vnids ( view . lines () ),
    "back at the root authors', M's folder is drawn again as the fresh view drew it" );
  Ok (( )) }

/// A replaced root's default folders are the replacement's: a view
/// of foreign L shows its forks folder holding J, which forks it,
/// claiming nothing. From the user's perspective the root is J, the
/// user's fork of L, and it is drawn as a fresh view of J draws it:
/// its containers folder, and its forks folder holding L, claiming,
/// counting the one child of L that J neither contains nor hides. Back
/// at the root authors', L is drawn again as the fresh view drew it.
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
      &mut views_state, &env, config, handle_set_overrides_followed_request );
    assert! ( refusal_of (&messages) . is_none (), "{:?}", messages );
    Ok ( rerendered_text (&messages) . ok_or ("the switch rerenders") ? ) };
  let users : String = switch ("user") ?;
  assert! ( users . contains ("forksFolder")
            && line_of_gnid (&users, "L") . is_some_and (
                 |line| line . contains ("(unintegrated 1)") )
            && ! users . contains ("noWarrant"),
    "from the user's, J shows its forks folder, L in it with its count:\n{}", users );
  assert_eq! ( without_vnids ( users . lines () . skip (1) ),
               without_vnids ( fresh_view_of_j . lines () . skip (1) ),
    "J's folders are drawn as a fresh view of J draws them" );
  let back : String = switch ("root authors") ?;
  assert_eq! ( without_vnids ( back . lines () ), without_vnids ( view . lines () ),
    "back at the root authors', L is drawn again as the fresh view drew it" );
  Ok (( )) }

/// A folder the user asked for on a root that a perspective replaces
/// is drawn again for the replacement: L's view, given its mentioners
/// and aliases folders, lists X, which links to L, and L's alias
/// "letter". From the user's perspective the root is J, and those
/// folders list Y, which links to J, and J's alias "reply", with
/// nothing of L's; back at the root authors', L's again.
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
      OverridesFollowed::RootAuthors ) . await ?;
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
      &mut views_state, &env, config, handle_set_overrides_followed_request );
    assert! ( refusal_of (&messages) . is_none (), "{:?}", messages );
    Ok ( rerendered_text (&messages) . ok_or ("the switch rerenders") ? ) };
  let users : String = switch ("user") ?;
  assert! ( users . contains ("mentionerFolder") && users . contains ("aliasFolder")
            && shows_js (&users)
            && line_of_gnid (&users, "X") . is_none ()
            && ! users . contains (") letter")
            && ! users . contains ("noWarrant"),
    "from the user's, J's folders list Y and \"reply\", and nothing of L's:\n{}", users );
  let back : String = switch ("root authors") ?;
  assert_eq! ( without_vnids ( back . lines () ), without_vnids ( asked . lines () ),
    "back at the root authors', L's folders are drawn again" );
  Ok (( )) }

/// A search hit is a write-protected root that shows only its
/// folders. Switched to the user's perspective, the hit for foreign L
/// ("missive") shows J, the user's fork, still write-protected, with
/// what a search hit for J ("answer") shows: its containers folder and
/// its forks folder, folded, and no content, which only an
/// editable root draws. Back at the root authors', the hit shows L's
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
      &mut views_state, &env, config, handle_set_overrides_followed_request );
    assert! ( refusal_of (&messages) . is_none (), "{:?}", messages );
    Ok ( rerendered_text (&messages) . ok_or ("the switch rerenders") ? ) };
  let users : String = switch ("user") ?;
  assert! ( users . lines () . next () . is_some_and (
              |root| root . contains ("(gnid J)") && root . contains ("writeProtected") )
            && line_of_gnid (&users, "L1") . is_none (),
    "from the user's, the hit is J, write-protected, without content:\n{}", users );
  assert_eq! ( without_vnids ( users . lines () . skip (1) ),
               without_vnids ( fresh_hit_j . lines () . skip (1) ),
    "J's folders are drawn as a search hit for J draws them" );
  let back : String = switch ("root authors") ?;
  assert_eq! ( without_vnids ( back . lines () ), without_vnids ( hit_l . lines () ),
    "back at the root authors', the hit shows L's folders again" );
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

/// In a view of foreign R that follows the user's overrides, editing
/// R's child K forks it, and the save's rerender shows the clone, with
/// the edit, in K's place, as '(overridesHere K)', K standing only in
/// the clone's forks folder (the folder K's occurrence carried, now
/// the clone's); U still stands in M's place. The connection keeps the
/// choice the save named.
async fn a_save_following_the_users_overrides_shows_a_new_fork_in_place (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (mut views_state, mut env, _view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "R", &[ ID::from ("R") ]) ?;
  let switched : String = rerendered_text ( &messages_answering (
    &switch_request ("R", OverridesFollowed::user ()),
    &mut views_state, &env, config, handle_set_overrides_followed_request ))
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
      OverridesFollowed::user () ) . await ?;
  assert! ( response . errors . is_empty (), "{:?}", response . errors );
  let clone : Graphnode = owned_fork_on_disk (config, "K") ?;
  let saved : &str = &response . saved_view;
  assert! ( line_of_gnid (saved, &clone . pid . 0)
            . is_some_and ( |l| l . contains ("(overridesHere K)") && l . ends_with (") K-edited") ),
    "the clone, edited, stands in K's place:\n{}", saved );
  assert! ( saved . lines () . filter ( |line| line . contains ("(gnid K)") )
            . all ( |line| line . contains ("(birth (forks in 2))") ),
            "K stands only in the clone's forks folder:\n{}", saved );
  assert! ( line_of_gnid (saved, "U") . is_some_and ( |l| l . contains ("(overridesHere M)") ),
    "U still stands in M's place:\n{}", saved );
  assert_eq! ( views_state . open_views . overrides_followed_by_view (
                 &ViewforestId::from_client_string ("R" . to_string ()) ),
               OverridesFollowed::user () );
  Ok (( )) }

/// Under foreign R, U, the user's, and A, R's author's, each carrying
/// '(overridesHere M)', are honest whichever perspective the save
/// names: each is a mirror of M. K, no mirror of M, is refused under
/// either, the refusal naming the mirror the named perspective shows.
/// A root standing for another node has that node's author: K standing
/// for M at the root names A, M's author's mirror, under the root
/// authors' perspective, and U under the user's.
fn the_tamper_check_accepts_a_mirror_whatever_perspective_the_save_names (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraph = InRustGraph::from_graphnodes (
    &read_all_skg_files_from_skgrepos (config) ? );
  let buffer_showing = |carrier : &str, repo : &str| -> String { format! (
    "* (skg node (gnid R) (skgRepos (nodeRepo foreign))) R\n** (skg node (gnid {}) (skgRepos (nodeRepo {})) (overridesHere M)) {}\n",
    carrier, repo, carrier ) };
  let mismatch_naming = |buffer : &str, followed : OverridesFollowed| -> Option<ID> {
    match buffer_to_validated_saveplan_with_fork_skgrepos_in_graph (
      buffer, &graph, config, &SkgrepoRestriction::unrestricted (config),
      &HashMap::new (), &WhatTheBaselineShows::default (), followed ) {
      Err (SaveError::BufferValidationErrors { errors, .. }) =>
        errors . iter () . find_map ( |e| match e {
          BufferValidationError::OverridesHere_Mismatch (_, _, shown, _) =>
            Some ( shown . clone () . unwrap_or ( ID::from ("none") )),
          _ => None, } ),
      _ => None, } };
  for followed in [ OverridesFollowed::user (), OverridesFollowed::RootAuthors ] {
    assert_eq! ( mismatch_naming (&buffer_showing ("U", "main"), followed . clone ()), None );
    assert_eq! ( mismatch_naming (&buffer_showing ("A", "foreign"), followed . clone ()), None ); }
  assert_eq! ( mismatch_naming (&buffer_showing ("K", "foreign"), OverridesFollowed::user ()),
               Some ( ID::from ("U") ));
  assert_eq! ( mismatch_naming (&buffer_showing ("K", "foreign"), OverridesFollowed::RootAuthors),
               Some ( ID::from ("A") ));
  { // At the root, K standing for M has M's author, foreign, as its
    // root author.
    let root_k_standing_for_m : &str =
      "* (skg node (gnid K) (skgRepos (nodeRepo foreign)) (overridesHere M)) K\n";
    assert_eq! ( mismatch_naming (root_k_standing_for_m, OverridesFollowed::user ()),
                 Some ( ID::from ("U") ));
    assert_eq! ( mismatch_naming (root_k_standing_for_m, OverridesFollowed::RootAuthors),
                 Some ( ID::from ("A") )); }
  Ok (( )) }

/// The client is the authority on whose overrides its buffer follows:
/// a view the connection holds as following the user's, saved by a
/// client whose buffer still follows the root authors' (drawn before
/// the switch, as after a lost answer, or by a client that never
/// switched), is checked and redrawn following the root authors', and
/// the connection takes that choice for it.
async fn a_held_view_follows_whatever_its_save_names (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (mut views_state, mut env, view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "R", &[ ID::from ("R") ]) ?;
  messages_answering (
    &switch_request ("R", OverridesFollowed::user ()),
    &mut views_state, &env, config, handle_set_overrides_followed_request );
  let view_id : ViewforestId = ViewforestId::from_client_string ("R" . to_string ());
  assert_eq! ( views_state . open_views . overrides_followed_by_view (&view_id),
               OverridesFollowed::user () );
  let (mut stream, _read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  let response : SaveResponse =
    update_from_and_rerender_buffer_with_approvals (
      &mut stream, &view, &view, &mut env, &Ok ( view_id . clone () ),
      &mut views_state, &SkgrepoRestriction::unrestricted (config),
      true, &HashMap::new (), &HashSet::new (),
      &StrandedsReview::NotReviewed, DeleteFrom::NotChosen, &[], None,
      OverridesFollowed::RootAuthors ) . await ?;
  assert! ( response . errors . is_empty (), "{:?}", response . errors );
  assert! ( line_of_gnid (&response . saved_view, "A")
            . is_some_and ( |l| l . contains ("(overridesHere M)") ),
    "the save is redrawn following R's author:\n{}", response . saved_view );
  assert_eq! ( views_state . open_views . overrides_followed_by_view (&view_id),
               OverridesFollowed::RootAuthors );
  Ok (( )) }

/// The user takes back a fork where it stands in place of another's
/// original: a view of foreign R, switched to the user's overrides,
/// shows U in M's place, and 'delete' is written on U. The save
/// deletes U and nothing else: it asks to fork nothing, R keeps M, and
/// the redrawn view shows in U's place the mirror of M that the user's
/// perspective now ranks first: A, M's author's fork of it, in M's
/// skgrepo, which the user's list ranks above third's. (Before 2026-11-15
/// U's deletion dropped M from R's content, an edit of foreign R, so
/// the save asked to fork R.)
async fn deleting_the_users_fork_in_a_foreign_parent_leaves_the_parent_and_shows_the_original (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (mut views_state, mut env, _view) : (ViewsState, SkgEnv, String) =
    connection_holding_a_view_of (config, tantivy, "R", &[ ID::from ("R") ]) ?;
  let switched : String = rerendered_text ( &messages_answering (
    &switch_request ("R", OverridesFollowed::user ()),
    &mut views_state, &env, config, handle_set_overrides_followed_request ))
    . ok_or ("the switch rerenders the view") ?;
  let u_line : &str = line_of_gnid (&switched, "U")
    . filter ( |l| l . contains ("(overridesHere M)") )
    . ok_or_else ( || format! ("U stands in M's place under R:\n{}", switched) ) ?;
  let edited : String = switched . replace (
    u_line, &u_line . replace ("(overridesHere M)",
                               "(overridesHere M) (editRequest delete)") );
  let r_before : String = read_skgrepo_file (config, "foreign", "R");
  let (mut stream, _read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  let response : SaveResponse =
    update_from_and_rerender_buffer_with_approvals (
      &mut stream, &edited, &switched, &mut env,
      &Ok ( ViewforestId::from_client_string ("R" . to_string ()) ),
      &mut views_state, &SkgrepoRestriction::unrestricted (config),
      false, &HashMap::new (), &HashSet::new (),
      &StrandedsReview::NotReviewed, DeleteFrom::NotChosen, &[], None,
      OverridesFollowed::user () ) . await ?;
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
                                && l . contains ("(overridesHere M)") ),
    "A, M's author's fork, is shown in U's place:\n{}", saved );
  assert! ( line_of_gnid (saved, "U") . is_none (), "{}", saved );
  Ok (( )) }

/// At the plan: 'delete' on an overrider standing for M leaves M in
/// its parent's list under a foreign parent, which is not forked;
/// removing the overrider's headline instead removes M, which forks
/// R. Under an owned parent, the deletion takes M out of the list too,
/// as it did before.
fn a_deleted_overrider_keeps_its_original_but_a_removed_one_does_not (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraph = InRustGraph::from_graphnodes (
    &read_all_skg_files_from_skgrepos (config) ? );
  let plan = |buffer : &str| -> Result<skg::types::save::SavePlan, SaveError> {
    buffer_to_validated_saveplan_with_fork_skgrepos_in_graph (
      buffer, &graph, config, &SkgrepoRestriction::unrestricted (config),
      &HashMap::new (), &WhatTheBaselineShows::default (),
      OverridesFollowed::user () ) . map ( |(_, plan, _)| plan ) };
  let deletes_u = |instructions : &[NodeInstruction]| -> bool {
    instructions . iter () . any ( |i| matches! (
      i, NodeInstruction::Delete (d) if d . gnid == ID::from ("U") )) };
  { // Under foreign R.
    let deleted : skg::types::save::SavePlan = plan ( indoc! {"
      * (skg node (gnid R) (skgRepos (nodeRepo foreign))) R
      ** (skg node (gnid U) (skgRepos (nodeRepo main)) (overridesHere M) (editRequest delete)) U
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
      ** (skg node (gnid U) (skgRepos (nodeRepo main)) (overridesHere M) (editRequest delete)) U
      "} ) ?;
    assert! ( deletes_u (&deleted . node_instructions) );
    assert! ( opt_saved_node_by_gnid (&deleted . node_instructions, "P")
              . is_some_and ( |p| p . contains . is_empty () ),
      "P loses M" ); }
  Ok (( )) }
