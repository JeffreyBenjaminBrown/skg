// cargo nextest run --test grouped_views -E 'test(hidden_from_forks::)'
//
// Tests for hiddenOutsideOfForkPartnersFolder, hiddenInForkPartnerFolder, and the hides relation.
// These test that:
// 1. Initial view draws each forkee as such editable, with its
//    hiddenInForkPartnerFolder when nonempty, and the forks folder's
//    hiddenOutsideOfForkPartnersFolder
// 2. Cuts, pastes and hidden-folder edits under them save as hides

use skg::skgrepo_sets::SkgrepoRestriction;
use skg::metadata_canon::without_viewnode_ids;
use crate::tcp_stream_pair::mk_test_tcp_stream_pair;
use indoc::indoc;
use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_skgrepos;
use skg::assert_metadata_eq;
use skg::test_utils::{
  extract_string_field_from_sexp,
  graph_handle_from_config,
  read_all_lp_messages,
  run_with_shared_test_stores,
  save_preview_test,
  update_from_and_rerender_buffer_test as update_from_and_rerender_buffer };
use skg::test_utils::single_root_view;
use skg::types::misc::{SkgConfig, ID, TantivyIndex, members_of};
use skg::types::nodes::complete::Graphnode;

use skg::serve::ViewsState;
use skg::types::views_state::{OpenViews, ViewforestId};
use skg::dbs::in_rust_graph::{
  InRustGraphHandle,
  };

use std::error::Error;
use std::io::BufReader;
use std::net::TcpStream;

async fn save_buffer_for_hidden_from_forks_test (
  buffer      : &str,

  config      : &SkgConfig,
  tantivy     : &mut TantivyIndex,
  graph       : &InRustGraphHandle,
  views_state : &mut ViewsState,
) -> Result<String, Box<dyn Error>> {
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response = update_from_and_rerender_buffer (
    &mut stream,
    buffer, config, tantivy, graph,

    &Err ( String::new () ), views_state
  ) . await ?;
  Ok (response . saved_view) }

async fn save_buffer_and_read_collateral_views (
  buffer      : &str,
  viewforest_id : &ViewforestId,

  config      : &SkgConfig,
  tantivy     : &mut TantivyIndex,
  graph       : &InRustGraphHandle,
  views_state : &mut ViewsState,
) -> Result<(String, Vec<String>), Box<dyn Error>> {
  let (mut stream, read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response =
    update_from_and_rerender_buffer (
      &mut stream,
      buffer, config, tantivy, graph,
      &Ok (viewforest_id . clone ()), views_state
    ) . await ?;
  drop (stream);
  let mut reader : BufReader<TcpStream> =
    BufReader::new (read_end);
  let collateral_views : Vec<String> =
    read_all_lp_messages (&mut reader)
    . into_iter()
    // Skip the save-relax-lock message; only collateral-views
    // carry a content field.
    . filter ( |msg| msg . contains ("collateral-view") )
    . map ( |msg|
      extract_string_field_from_sexp (&msg, "content")
      . unwrap_or_else (||
        panic! ("content field not found in collateral-view sexp: {}",
                msg)) )
    . collect();
  Ok ((response . saved_view, collateral_views)) }

fn remove_e1_subtree (
  buffer : &str,
) -> String {
  buffer
    . lines()
    . filter ( |line| {
      ! line . contains ("(gnid e1)")
      && ! line . contains ("(gnid e11)") })
    . collect::<Vec<_>>()
    . join ("\n") + "\n" }

fn insert_before_r1 (
  buffer : &str,
  inserted : &str,
) -> String {
  buffer . split_inclusive ('\n')
    . map ( |line|
      if without_viewnode_ids (line) . starts_with ("** (skg node (gnid r1)") {
        format! ("{}{}", inserted, line) }
      else { line . to_string () } )
    . collect () }

fn extended_forkee_edit_view (
  initial_view : &str,
  edit_kind    : &str,
) -> String {
  let without_e1 : String = remove_e1_subtree (initial_view);
  match edit_kind {
    "delete" => without_e1,
    "move_to_r" => insert_before_r1 (
      &without_e1,
      "** (skg node (gnid e1) (skgRepos (nodeRepo foreign)) writeProtected) e1\n" ),
    "move_to_a" => format! (
      "{}* (skg node (gnid a) (skgRepos (nodeRepo owned))) a\n** (skg node (gnid e1) (skgRepos (nodeRepo foreign)) writeProtected) e1\n",
      without_e1 ),
    _ => panic! ("unknown edit kind: {}", edit_kind), }}

fn assert_hides_e1_under_the_forkee (
  buffer : &str,
) {
  assert! (
    // 'folded': a new hidden folder begins folded,
    // expressed as a folded atom on each member.
    without_viewnode_ids (buffer) . contains ("**** (skg hiddenInForkPartnerFolder)\n***** (skg folded node (gnid e1) (skgRepos (nodeRepo foreign)) writeProtected"),
    "Expected e1 to be rendered folded under hiddenInForkPartnerFolder:\n{}",
    buffer );
  assert! (
    ! buffer . lines() . any ( |line|
      without_viewnode_ids (line) . starts_with ("**** (skg node (gnid e1)") ),
    "Expected e1 not to remain visible as forkee-as-such content:\n{}",
    buffer ); }

fn assert_does_not_hide_e1 (
  buffer : &str,
) {
  assert! (
    ! buffer . contains ("hiddenInForkPartnerFolder"),
    "Expected no hiddenInForkPartnerFolder for e1:\n{}",
    buffer ); }

fn move_h_from_hiddenin_folder_to_visible_forkee_content (
  buffer : &str,
) -> String {
  let mut in_h_subtree : bool = false; // H's containerward chain moves with it
  buffer
    . lines()
    . filter_map ( |line| {
      in_h_subtree = in_h_subtree && line . starts_with ("******");
      if without_viewnode_ids (line) . contains ("(skg hiddenInForkPartnerFolder)") {
        None
      } else if line . contains ("(gnid H)") {
        in_h_subtree = true;
        Some (line . replacen ("***** ", "**** ", 1)
              // A real client manages fold metadata itself (they are
              // re-derived from visibility at save), so the moved
              // line would not carry the folder's 'folded' mark.
              . replacen ("folded ", "", 1))
      } else if in_h_subtree {
        Some (line . replacen ("*", "", 1))
      } else {
        Some (line . to_string()) }})
    . collect::<Vec<_>>()
    . join ("\n") + "\n" }

fn remove_hiddenin_branch (
  buffer : &str,
) -> String {
  let mut in_h_subtree : bool = false;
  buffer
    . lines()
    . filter ( |line| {
      in_h_subtree = line . contains ("(gnid H)")
        || ( in_h_subtree && line . starts_with ("******") );
      ! without_viewnode_ids (line) . contains ("(skg hiddenInForkPartnerFolder)")
      && ! in_h_subtree } )
    . collect::<Vec<_>>()
    . join ("\n") + "\n" }

fn add_e11_to_hiddenoutside_folder (
  buffer : &str,
) -> String {
  insert_after_line_containing (
    buffer,
    "(skg hiddenOutsideOfForkPartnersFolder)",
    "**** (skg node (gnid E11) (skgRepos (nodeRepo main)) writeProtected) E11" ) }

fn assert_e1_removed_from_visible_forkee_branch (
  buffer : &str,
) {
  assert! (
    ! buffer . lines() . any ( |line|
      without_viewnode_ids (line) . starts_with ("**** (skg node (gnid e1)") ),
    "Expected e1 not to be regenerated as forkee-as-such content:\n{}",
    buffer );
  assert! (
    without_viewnode_ids (buffer) . contains ("**** (skg node (gnid e2)"),
    "Expected unrelated forkee-as-such content e2 to remain visible:\n{}",
    buffer ); }

fn remove_e1_and_e2_subtrees (
  buffer : &str,
) -> String {
  buffer
    . lines()
    . filter ( |line| {
      ! line . contains ("(gnid e1)")
      && ! line . contains ("(gnid e11)")
      && ! line . contains ("(gnid e2)")
      && ! line . contains ("(gnid e12)") })
    . collect::<Vec<_>>()
    . join ("\n") + "\n" }

fn insert_after_line_containing (
  buffer     : &str,
  needle     : &str,
  insertion  : &str,
) -> String {
  let mut result : Vec<String> = Vec::new();
  for line in buffer . lines() {
    result . push (line . to_string());
    if without_viewnode_ids (line) . contains (needle) {
      result . extend (
        insertion . lines() . map ( |l| l . to_string() )); }}
  result . join ("\n") + "\n" }

fn assert_line_order (
  buffer : &str,
  earlier : &str,
  later   : &str,
) {
  let earlier_pos : usize =
    buffer . find (earlier) . unwrap_or_else (||
      panic! ("Expected {:?} in:\n{}", earlier, buffer));
  let later_pos : usize =
    buffer . find (later) . unwrap_or_else (||
      panic! ("Expected {:?} in:\n{}", later, buffer));
  assert!(
    earlier_pos < later_pos,
    "Expected {:?} before {:?} in:\n{}",
    earlier, later, buffer ); }

fn node_from_disk (
  config : &SkgConfig,
  pid    : &str,
) -> Result<Graphnode, Box<dyn Error>> {
  let gnid : ID = ID::from (pid);
  read_all_skg_files_from_skgrepos (config)?
    . into_iter()
    . find ( |node| node . pid == gnid )
    . ok_or_else ( || format! ("node not found on disk: {}", pid) . into() ) }

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-hidden-from-forks",
    |s| Box::pin ( async move {
      s . reset ("test_deleting_foreign_forkee_content_preserves_branch_edit",
        "tests/hidden_from_forks/fixtures-forkee-edit") ?;
      test_deleting_foreign_forkee_content_preserves_branch_edit (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_deleting_foreign_forkee_content_infers_hide",
        "tests/hidden_from_forks/fixtures-forkee-edit") ?;
      test_deleting_foreign_forkee_content_infers_hide (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_removing_forkee_content_from_forker_contains_infers_hide",
        "tests/hidden_from_forks/fixtures-clone-edit") ?;
      test_removing_forkee_content_from_forker_contains_infers_hide (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_readding_forkee_content_to_forker_contains_unhides",
        "tests/hidden_from_forks/fixtures-clone-edit") ?;
      test_readding_forkee_content_to_forker_contains_unhides (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_placing_and_removing_a_fork_of_forkee_content_hides_what_it_forks",
        "tests/hidden_from_forks/fixtures-fork-of-forkee-content") ?;
      test_placing_and_removing_a_fork_of_forkee_content_hides_what_it_forks (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_removing_a_fork_of_forkee_content_hides_what_it_forks",
        "tests/hidden_from_forks/fixtures-fork-of-forkee-content") ?;
      test_removing_a_fork_of_forkee_content_hides_what_it_forks (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_a_preview_and_a_save_say_what_they_hide",
        "tests/hidden_from_forks/fixtures-fork-of-forkee-content") ?;
      test_a_preview_and_a_save_say_what_they_hide (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_a_save_says_why_a_removal_hides_nothing",
        "tests/hidden_from_forks/fixtures-fork-of-forkee-content") ?;
      test_a_save_says_why_a_removal_hides_nothing (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_placing_a_fork_of_a_hidden_node_keeps_its_hide",
        "tests/hidden_from_forks/fixtures-fork-of-forkee-content") ?;
      test_placing_a_fork_of_a_hidden_node_keeps_its_hide (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_replacing_a_fork_with_its_original_hides_nothing",
        "tests/hidden_from_forks/fixtures-fork-of-forkee-content") ?;
      test_replacing_a_fork_with_its_original_hides_nothing (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_collateral_view_reflects_newly_hidden_forkee_content",
        "tests/hidden_from_forks/fixtures-forkee-edit") ?;
      test_collateral_view_reflects_newly_hidden_forkee_content (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_moving_foreign_forkee_content_to_forker_does_not_hide",
        "tests/hidden_from_forks/fixtures-forkee-edit") ?;
      test_moving_foreign_forkee_content_to_forker_does_not_hide (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_moving_foreign_forkee_content_elsewhere_still_hides",
        "tests/hidden_from_forks/fixtures-forkee-edit") ?;
      test_moving_foreign_forkee_content_elsewhere_still_hides (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_a_node_placed_under_a_foreign_forkee_is_refused",
        "tests/hidden_from_forks/fixtures-forkee-edit") ?;
      test_a_node_placed_under_a_foreign_forkee_is_refused (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_a_node_placed_under_an_owned_forkee_joins_it",
        "tests/hidden_from_forks/fixtures-forkee-edit") ?;
      test_a_node_placed_under_an_owned_forkee_joins_it (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset_from_config ("test_forkee_and_compound_partner_folders",
        "tests/hidden_from_forks/fixtures-every-kind-of-folder/skgconfig.toml") ?;
      test_forkee_and_compound_partner_folders (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset_from_config ("test_hidden_within_but_none_without",
        "tests/hidden_from_forks/fixtures-hidden-within-but-none-without/skgconfig.toml") ?;
      test_hidden_within_but_none_without (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_moving_hidden_forkee_content_to_visible_branch_infers_unhide",
        "tests/hidden_from_forks/fixtures-hidden-within-but-none-without") ?;
      test_moving_hidden_forkee_content_to_visible_branch_infers_unhide (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_collateral_view_reflects_newly_unhidden_forkee_content",
        "tests/hidden_from_forks/fixtures-hidden-within-but-none-without") ?;
      test_collateral_view_reflects_newly_unhidden_forkee_content (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_deleting_from_hiddenin_folder_does_not_unhide",
        "tests/hidden_from_forks/fixtures-hidden-within-but-none-without") ?;
      test_deleting_from_hiddenin_folder_does_not_unhide (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset_from_config ("test_hidden_without_but_none_within",
        "tests/hidden_from_forks/fixtures-hidden-without-but-none-within/skgconfig.toml") ?;
      test_hidden_without_but_none_within (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_adding_to_hiddenoutside_folder_hides_and_moves_inside",
        "tests/hidden_from_forks/fixtures-hidden-without-but-none-within") ?;
      test_adding_to_hiddenoutside_folder_hides_and_moves_inside (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset_from_config ("test_overlapping_hidden_within",
        "tests/hidden_from_forks/fixtures-overlapping-hidden-within/skgconfig.toml") ?;
      test_overlapping_hidden_within (
        &s . config, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

async fn test_deleting_foreign_forkee_content_preserves_branch_edit (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let graph : InRustGraphHandle =
      (
        graph_handle_from_config (config) ?);
    let mut views_state : ViewsState = ViewsState {
      open_views        : OpenViews::new (),
      graph_straddle    : None,};

    let (initial_view, _pids, _)
      : (String, Vec<ID>, _)
      = single_root_view(
          &config, None,
          &ID ("r" . to_string()) )?;
    let extended : String =
      save_buffer_for_hidden_from_forks_test (
        &initial_view,
        &config, tantivy, &graph, &mut views_state
      ) . await?;
    let edited : String =
      extended_forkee_edit_view (&extended, "delete");
    let rerendered : String =
      save_buffer_for_hidden_from_forks_test (
        &edited, &config, tantivy, &graph, &mut views_state
      ) . await?;

    assert_e1_removed_from_visible_forkee_branch (&rerendered);
    Ok (( )) }

async fn test_deleting_foreign_forkee_content_infers_hide (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let graph : InRustGraphHandle =
      (
        graph_handle_from_config (config) ?);
    let mut views_state : ViewsState = ViewsState {
      open_views        : OpenViews::new (),
      graph_straddle    : None,};

    let (initial_view, _pids, _)
      : (String, Vec<ID>, _)
      = single_root_view(
          &config, None,
          &ID ("r" . to_string()) )?;
    let extended : String =
      save_buffer_for_hidden_from_forks_test (
        &initial_view,
        &config, tantivy, &graph, &mut views_state
      ) . await?;
    let edited : String =
      extended_forkee_edit_view (&extended, "delete");
    let (mut stream, _read_end) : (TcpStream, TcpStream) =
      mk_test_tcp_stream_pair ();
    let response = update_from_and_rerender_buffer (
      &mut stream, &edited, config, tantivy, &graph,
      &Err (String::new ()), &mut views_state ) . await ?;
    let rerendered : String = response . saved_view;

    assert_hides_e1_under_the_forkee (&rerendered);
    let r_skg : Graphnode =
      node_from_disk (&config, "r")?;
    assert_eq!(
      members_of ( r_skg . hides . or_default() ),
      vec! [ID::from ("e1")],
      "Expected inferred hide to be persisted in r.skg: {:?}",
      r_skg . hides );
    Ok (( )) }

/// The contains-removal rule ('docs/forks.org', "Editing the clone:
/// deletions become hides"): owned f contains [c, x] and forks o,
/// whose contains include c (but not x). Saving f without its children must
/// hide c (it would otherwise reappear under f as unintegrated forkee
/// content), and must not hide x (no forkee contains it -- deleting it
/// is an ordinary contains edit).
async fn test_removing_forkee_content_from_forker_contains_infers_hide (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let graph : InRustGraphHandle =
      (
        graph_handle_from_config (config) ?);
    let mut views_state : ViewsState = ViewsState {
      open_views        : OpenViews::new (),
      graph_straddle    : None,};
    let buffer : &str = indoc! {"
      * (skg node (gnid f) (skgRepos (nodeRepo owned))) f
      "};
    save_buffer_for_hidden_from_forks_test (
      buffer, &config, tantivy, &graph, &mut views_state
    ) . await ?;
    let f_skg : Graphnode =
      node_from_disk (&config, "f") ?;
    assert! ( f_skg . contains . is_empty (),
      "f's contains must be emptied by the save: {:?}",
      f_skg . contains );
    assert_eq! (
      members_of ( f_skg . hides . or_default () ),
      vec! [ ID::from ("c") ],
      "removing c (content of forkee o) must hide it, and removing x (content of no forkee) must not: {:?}",
      f_skg . hides );
    Ok (( )) }

/// The inverse of the contains-removal rule: owned g hides c and
/// forks o (which contains c). Saving g with c as content
/// must drop the now-stale hide -- c is integrated, so g should no
/// longer appear among c's hiders.
async fn test_readding_forkee_content_to_forker_contains_unhides (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let graph : InRustGraphHandle =
      (
        graph_handle_from_config (config) ?);
    let mut views_state : ViewsState = ViewsState {
      open_views        : OpenViews::new (),
      graph_straddle    : None,};
    let buffer : &str = indoc! {"
      * (skg node (gnid g) (skgRepos (nodeRepo owned))) g
      ** (skg node (gnid c) (skgRepos (nodeRepo foreign)) writeProtected) c
      "};
    save_buffer_for_hidden_from_forks_test (
      buffer, &config, tantivy, &graph, &mut views_state
    ) . await ?;
    let g_skg : Graphnode =
      node_from_disk (&config, "g") ?;
    assert_eq! ( members_of (& g_skg . contains), vec! [ ID::from ("c") ],
      "g's contains must now hold c: {:?}", g_skg . contains );
    assert! (
      g_skg . hides . or_default () . is_empty (),
      "re-adding c to g's contains must drop g's stale hide of c: {:?}",
      g_skg . hides );
    Ok (( )) }

/// Foreign j contains owned m and foreign k; foreign r forks m,
/// as one's clone of another's note does, and j's own contains, like
/// any container's, still names m. Owned f forks j and contains
/// nothing. Placing r in f and saving,
/// then removing it and saving, is the gesture that dismisses any
/// other child of j. But owned g, which forks j too, contains r
/// itself: g is an indirect member of f's forks folder, and every
/// member explains a hide alike, so r would show again under g. The
/// removal hides r, the member of g's content it is, rather than m,
/// the member of j's content r stands for (as removing r from g does,
/// where no member contains r itself:
/// 'test_removing_a_fork_of_forkee_content_hides_what_it_forks').
async fn test_placing_and_removing_a_fork_of_forkee_content_hides_what_it_forks (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let graph : InRustGraphHandle =
      graph_handle_from_config (config) ?;
    let mut views_state : ViewsState = ViewsState {
      open_views        : OpenViews::new (),
      graph_straddle    : None,};
    let placed : &str = indoc! {"
      * (skg node (gnid f) (skgRepos (nodeRepo owned))) f
      ** (skg node (gnid r) (skgRepos (nodeRepo foreign)) writeProtected) r
      "};
    save_buffer_for_hidden_from_forks_test (
      placed, &config, tantivy, &graph, &mut views_state
    ) . await ?;
    let f_placed : Graphnode = node_from_disk (&config, "f") ?;
    assert_eq! ( members_of (& f_placed . contains), vec! [ ID::from ("r") ],
      "the first save places r in f: {:?}", f_placed . contains );
    let removed : &str = indoc! {"
      * (skg node (gnid f) (skgRepos (nodeRepo owned))) f
      "};
    save_buffer_for_hidden_from_forks_test (
      removed, &config, tantivy, &graph, &mut views_state
    ) . await ?;
    let f_skg : Graphnode = node_from_disk (&config, "f") ?;
    assert! ( f_skg . contains . is_empty (),
      "the second save removes r from f: {:?}", f_skg . contains );
    assert_eq! (
      members_of ( f_skg . hides . or_default () ),
      vec! [ ID::from ("r") ],
      "removing r, a member of the content of g, an indirect member, must hide r: {:?}",
      f_skg . hides );
    Ok (( )) }

/// Owned g forks j and contains r, which forks m, a
/// member of j's content. Removing r from g hides m.
async fn test_removing_a_fork_of_forkee_content_hides_what_it_forks (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let graph : InRustGraphHandle =
      graph_handle_from_config (config) ?;
    let mut views_state : ViewsState = ViewsState {
      open_views        : OpenViews::new (),
      graph_straddle    : None,};
    let buffer : &str = indoc! {"
      * (skg node (gnid g) (skgRepos (nodeRepo owned))) g
      "};
    save_buffer_for_hidden_from_forks_test (
      buffer, &config, tantivy, &graph, &mut views_state
    ) . await ?;
    let g_skg : Graphnode = node_from_disk (&config, "g") ?;
    assert! ( g_skg . contains . is_empty (),
      "g's contains must be emptied by the save: {:?}", g_skg . contains );
    assert_eq! (
      members_of ( g_skg . hides . or_default () ),
      vec! [ ID::from ("m") ],
      "removing r, which forks m, must hide m: {:?}",
      g_skg . hides );
    Ok (( )) }

async fn save_and_return_hides (
  buffer      : &str,
  config      : &SkgConfig,
  tantivy     : &mut TantivyIndex,
  graph       : &InRustGraphHandle,
  views_state : &mut ViewsState,
) -> Result<Vec<String>, Box<dyn Error>> {
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response = update_from_and_rerender_buffer (
    &mut stream, buffer, config, tantivy, graph,
    &Err ( String::new () ), views_state ) . await ?;
  assert! ( response . errors . is_empty (),
    "the save must succeed: {:?}", response . errors );
  Ok (response . hides) }

/// Owned g forks j and contains r, which forks m, a
/// member of j's content. A preview of the save that removes r says
/// it hides m, and writes nothing; the save itself says the same.
/// Then h, which hides m, takes m in, and the save says it stops
/// hiding m.
async fn test_a_preview_and_a_save_say_what_they_hide (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let graph : InRustGraphHandle =
      graph_handle_from_config (config) ?;
    let mut views_state : ViewsState = ViewsState {
      open_views        : OpenViews::new (),
      graph_straddle    : None,};
    let removed : &str = indoc! {"
      * (skg node (gnid g) (skgRepos (nodeRepo owned))) g
      "};
    let expected : Vec<String> =
      vec! [ "Hides 'm' from 'g', as content of 'j'." . to_string () ];
    let preview = {
      let (mut stream, _read_end) : (TcpStream, TcpStream) =
        mk_test_tcp_stream_pair ();
      save_preview_test (
        &mut stream, removed, config, tantivy, &graph, &mut views_state
      ) . await ? };
    assert! ( preview . preview, "a preview is answered as one" );
    assert_eq! ( preview . hides, expected, "the preview names the hide" );
    assert_eq! (
      members_of (& node_from_disk (&config, "g") ? . contains),
      vec! [ ID::from ("r") ],
      "a preview writes nothing" );
    assert! ( views_state . open_views . views . is_empty (),
      "a preview registers no view" );
    assert_eq! (
      save_and_return_hides (
        removed, config, tantivy, &graph, &mut views_state ) . await ?,
      expected, "the save names the hide it wrote" );
    let placed : &str = indoc! {"
      * (skg node (gnid h) (skgRepos (nodeRepo owned))) h
      ** (skg node (gnid m) (skgRepos (nodeRepo owned))) m
      "};
    assert_eq! (
      save_and_return_hides (
        placed, config, tantivy, &graph, &mut views_state ) . await ?,
      vec! [ "Stops hiding 'm' from 'h', as content of 'j'." . to_string () ],
      "placing a hidden member in its forker unhides it" );
    Ok (( )) }

/// A child leaving a forker that forks something, whose
/// departure hides nothing, earns a line naming the rule that held:
/// y, which j does not contain (nor g or h, which fork j too, the
/// indirect members of f's forks folder) and which forks nothing,
/// placed in f and removed; and r, removed from g in the save that puts m, the
/// member r stands for, in its place.
async fn test_a_save_says_why_a_removal_hides_nothing (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let graph : InRustGraphHandle =
      graph_handle_from_config (config) ?;
    let mut views_state : ViewsState = ViewsState {
      open_views        : OpenViews::new (),
      graph_straddle    : None,};
    let placed : &str = indoc! {"
      * (skg node (gnid f) (skgRepos (nodeRepo owned))) f
      ** (skg node (gnid y) (skgRepos (nodeRepo foreign)) writeProtected) y
      "};
    assert! (
      save_and_return_hides (
        placed, config, tantivy, &graph, &mut views_state ) . await ?
      . is_empty (),
      "placing a child removes nothing" );
    let removed : &str = indoc! {"
      * (skg node (gnid f) (skgRepos (nodeRepo owned))) f
      "};
    assert_eq! (
      save_and_return_hides (
        removed, config, tantivy, &graph, &mut views_state ) . await ?,
      vec! [ "Hides nothing: 'y' left 'f', but it is no content of 'j' or 'g' or 'h', nor a mirror of any." . to_string () ] );
    let replaced : &str = indoc! {"
      * (skg node (gnid g) (skgRepos (nodeRepo owned))) g
      ** (skg node (gnid m) (skgRepos (nodeRepo owned))) m
      "};
    assert_eq! (
      save_and_return_hides (
        replaced, config, tantivy, &graph, &mut views_state ) . await ?,
      vec! [ "Hides nothing: 'r' left 'g', but 'g' still contains 'm', which it stands for." . to_string () ] );
    Ok (( )) }

/// Owned h forks j and hides m. Placing r, which forks
/// m, in h keeps the hide of m: containing r does not integrate m
/// (the unintegrated content compares IDs), so dropping the hide
/// would bring m back as unintegrated.
async fn test_placing_a_fork_of_a_hidden_node_keeps_its_hide (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let graph : InRustGraphHandle =
      graph_handle_from_config (config) ?;
    let mut views_state : ViewsState = ViewsState {
      open_views        : OpenViews::new (),
      graph_straddle    : None,};
    let buffer : &str = indoc! {"
      * (skg node (gnid h) (skgRepos (nodeRepo owned))) h
      ** (skg node (gnid r) (skgRepos (nodeRepo foreign)) writeProtected) r
      "};
    save_buffer_for_hidden_from_forks_test (
      buffer, &config, tantivy, &graph, &mut views_state
    ) . await ?;
    let h_skg : Graphnode = node_from_disk (&config, "h") ?;
    assert_eq! ( members_of (& h_skg . contains), vec! [ ID::from ("r") ],
      "h's contains must now hold r: {:?}", h_skg . contains );
    assert_eq! (
      members_of ( h_skg . hides . or_default () ),
      vec! [ ID::from ("m") ],
      "placing r, which forks m, must keep h's hide of m: {:?}",
      h_skg . hides );
    Ok (( )) }

/// Owned g contains r, which forks m, a member of j's content.
/// A save that replaces r with m in g hides nothing: g still holds
/// the member r stood for.
async fn test_replacing_a_fork_with_its_original_hides_nothing (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let graph : InRustGraphHandle =
      graph_handle_from_config (config) ?;
    let mut views_state : ViewsState = ViewsState {
      open_views        : OpenViews::new (),
      graph_straddle    : None,};
    let buffer : &str = indoc! {"
      * (skg node (gnid g) (skgRepos (nodeRepo owned))) g
      ** (skg node (gnid m) (skgRepos (nodeRepo owned))) m
      "};
    save_buffer_for_hidden_from_forks_test (
      buffer, &config, tantivy, &graph, &mut views_state
    ) . await ?;
    let g_skg : Graphnode = node_from_disk (&config, "g") ?;
    assert_eq! ( members_of (& g_skg . contains), vec! [ ID::from ("m") ],
      "g's contains must now hold m: {:?}", g_skg . contains );
    assert! (
      g_skg . hides . or_default () . is_empty (),
      "g holds m, so removing r must not hide it: {:?}",
      g_skg . hides );
    Ok (( )) }

async fn test_collateral_view_reflects_newly_hidden_forkee_content (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let graph : InRustGraphHandle =
      (
        graph_handle_from_config (config) ?);
    let mut views_state : ViewsState = ViewsState {
      open_views        : OpenViews::new (),
      graph_straddle    : None,};
    let saved_viewforest_id : ViewforestId =
      ViewforestId::ContentView (
        "collateral-hide-saved" . to_string());
    let collateral_viewforest_id : ViewforestId =
      ViewforestId::ContentView (
        "collateral-hide-other" . to_string());

    let (initial_view, pids, viewforest) =
      single_root_view(
        &config, None,
        &ID ("r" . to_string()) )?;
    views_state . open_views . register_view (
      &graph . load_full (), saved_viewforest_id . clone(), viewforest, &pids, &SkgrepoRestriction::unrestricted (config));

    let (extended, collateral_views) =
      save_buffer_and_read_collateral_views (
        &initial_view,
        &saved_viewforest_id, &config, tantivy, &graph,
        &mut views_state ) . await?;
    assert!(
      collateral_views . is_empty(),
      "No collateral view should be registered yet: {:?}",
      collateral_views);

    let extended_viewforest =
      views_state . open_views . viewforest_id_to_view (&saved_viewforest_id)
      . expect ("saved view should be registered")
      . clone();
    let extended_pids : Vec<ID> =
      views_state . open_views . viewforest_id_to_pids (&saved_viewforest_id);
    views_state . open_views . register_view (
      &graph . load_full (), collateral_viewforest_id, extended_viewforest, &extended_pids, &SkgrepoRestriction::unrestricted (config));

    let edited : String =
      extended_forkee_edit_view (&extended, "delete");
    let (_saved_view, collateral_views) =
      save_buffer_and_read_collateral_views (
        &edited, &saved_viewforest_id, &config, tantivy, &graph,
        &mut views_state ) . await?;

    assert_eq!(
      collateral_views . len(), 1,
      "Expected one collateral view:\n{:?}",
      collateral_views);
    // e1 was hidden, so it appears under the hiddenInForkPartnerFolder.
    // Its occurrence in the forkee's content, which claimed
    // membership there and is no longer warranted, vanishes with its
    // subtree (e11): e1 shows once, hidden, write-protected and folded.
    let collateral : &str = &collateral_views[0];
    assert! (
      without_viewnode_ids (collateral) . contains (
        "**** (skg hiddenInForkPartnerFolder)\n***** (skg folded node (gnid e1) (skgRepos (nodeRepo foreign)) writeProtected"),
      "Expected e1 under hiddenInForkPartnerFolder:\n{}", collateral );
    assert_eq! ( collateral . lines ()
                   . filter ( |line| line . contains ("(gnid e1)") ) . count (),
                 1,
      "e1's occurrence in the forkee's content vanishes:\n{}", collateral );
    assert! ( ! collateral . contains ("(gnid e11)"),
      "e11 vanishes with it:\n{}", collateral );

    Ok (( )) }

async fn test_moving_foreign_forkee_content_to_forker_does_not_hide (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let graph : InRustGraphHandle =
      (
        graph_handle_from_config (config) ?);
    let mut views_state : ViewsState = ViewsState {
      open_views        : OpenViews::new (),
      graph_straddle    : None,};

    let (initial_view, _pids, _)
      : (String, Vec<ID>, _)
      = single_root_view(
          &config, None,
          &ID ("r" . to_string()) )?;
    let extended : String =
      save_buffer_for_hidden_from_forks_test (
        &initial_view,
        &config, tantivy, &graph, &mut views_state
      ) . await?;
    let edited : String =
      extended_forkee_edit_view (&extended, "move_to_r");
    let rerendered : String =
      save_buffer_for_hidden_from_forks_test (
        &edited, &config, tantivy, &graph, &mut views_state
      ) . await?;

    assert_does_not_hide_e1 (&rerendered);
    assert! (
      without_viewnode_ids (&rerendered) . contains (
        "** (skg node (gnid e1) (skgRepos (nodeRepo foreign)) writeProtected" ),
      "Expected e1 to remain ordinary content of r:\n{}",
      rerendered );
    // Regression ('docs/forks.org', "Unintegrated content: the
    // forksFolder"): e1, now contained by the
    // forker r, must not also re-appear as unintegrated forkee-as-such
    // content. Before the contains-subtraction fix it showed twice -- once as
    // r's integrated content (above) and once under the forkee-as-such.
    assert_e1_removed_from_visible_forkee_branch (&rerendered);
    Ok (( )) }

async fn test_moving_foreign_forkee_content_elsewhere_still_hides (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let graph : InRustGraphHandle =
      (
        graph_handle_from_config (config) ?);
    let mut views_state : ViewsState = ViewsState {
      open_views        : OpenViews::new (),
      graph_straddle    : None,};

    let (initial_view, _pids, _)
      : (String, Vec<ID>, _)
      = single_root_view(
          &config, None,
          &ID ("r" . to_string()) )?;
    let extended : String =
      save_buffer_for_hidden_from_forks_test (
        &initial_view,
        &config, tantivy, &graph, &mut views_state
      ) . await?;
    let edited : String =
      extended_forkee_edit_view (&extended, "move_to_a");
    let rerendered : String =
      save_buffer_for_hidden_from_forks_test (
        &edited, &config, tantivy, &graph, &mut views_state
      ) . await?;

    assert_hides_e1_under_the_forkee (&rerendered);
    assert! (
      without_viewnode_ids (&rerendered) . contains (
        "** (skg node (gnid e1) (skgRepos (nodeRepo foreign)) writeProtected" ),
      "Expected e1 to remain content of a:\n{}",
      rerendered );
    Ok (( )) }

/// A node the graph has, placed under e, a foreign forkee as such, is
/// refused as a headline typed there is: only e's owner can change
/// e's content ('docs/forks.org', "Adding a child under a member as
/// such"). Nothing is written.
async fn test_a_node_placed_under_a_foreign_forkee_is_refused (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let graph : InRustGraphHandle =
      (
        graph_handle_from_config (config) ?);
    let mut views_state : ViewsState = ViewsState {
      open_views        : OpenViews::new (),
      graph_straddle    : None,};

    let (initial_view, _pids, _)
      : (String, Vec<ID>, _)
      = single_root_view(
          &config, None,
          &ID ("r" . to_string()) )?;
    let extended : String =
      save_buffer_for_hidden_from_forks_test (
        &initial_view,
        &config, tantivy, &graph, &mut views_state
      ) . await?;
    let edited : String =
      insert_after_line_containing (
        &remove_e1_and_e2_subtrees (&extended),
        "(gnid e)",
        "**** (skg node (gnid e2) (skgRepos (nodeRepo foreign)) writeProtected) e2\n\
         **** (skg node (gnid a) (skgRepos (nodeRepo owned)) writeProtected) a\n\
         **** (skg node (gnid e1) (skgRepos (nodeRepo foreign)) writeProtected) e1" );
    let e_before : Graphnode = node_from_disk (&config, "e")?;
    let refusal : String =
      match save_buffer_for_hidden_from_forks_test (
        &edited, &config, tantivy, &graph, &mut views_state ) . await {
        Ok (saved) => panic! ( "placing a under foreign e must be refused; it saved:\n{}", saved ),
        Err (error) => error . to_string (), };
    assert!(
      refusal . contains ("Cannot add a child under e ('forkee-e'), a foreign node shown as a forkee"),
      "{}", refusal );
    let e_skg : Graphnode =
      node_from_disk (&config, "e")?;
    assert_eq! ( e_skg . contains, e_before . contains, "e's content is unchanged" );
    Ok (( )) }

/// A node the graph has, placed under r, an owned forkee as such
/// (here a member the same save makes a forkee: a claims it in its
/// forks folder), joins r's content, after the sibling it follows,
/// as a headline typed there does ('docs/forks.org', "Adding a child
/// under a member as such"); a hides nothing for it.
async fn test_a_node_placed_under_an_owned_forkee_joins_it (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let graph : InRustGraphHandle =
      (
        graph_handle_from_config (config) ?);
    let mut views_state : ViewsState = ViewsState {
      open_views        : OpenViews::new (),
      graph_straddle    : None,};
    let edited : String =
      indoc! {"
              * (skg node (gnid a) (skgRepos (nodeRepo owned))) a
              ** (skg forksFolder)
              *** (skg node (gnid r) (skgRepos (nodeRepo owned))) r
              **** (skg node (gnid r2) (skgRepos (nodeRepo owned)) writeProtected) r2
              **** (skg node (gnid e11) (skgRepos (nodeRepo foreign)) writeProtected) e11
              **** (skg node (gnid r1) (skgRepos (nodeRepo owned)) writeProtected) r1
              "} . to_string();
    let rerendered : String =
      save_buffer_for_hidden_from_forks_test (
        &edited, &config, tantivy, &graph, &mut views_state
      ) . await?;
    assert!(
      rerendered . contains ("(gnid e11)"),
      "e11 stands under r:\n{}",
      rerendered );
    assert!(
      ! rerendered . contains ("hiddenInForkPartnerFolder"),
      "placing e11 hides nothing:\n{}",
      rerendered );
    let r_skg : Graphnode =
      node_from_disk (&config, "r")?;
    assert_eq! (
      r_skg . contains . iter () . map ( |m| m . member . clone () ) . collect::<Vec<ID>> (),
      vec! [ ID::from ("r1"), ID::from ("r2"), ID::from ("e11") ],
      "e11 joins r's content after r2, the sibling it follows" );
    let a_skg : Graphnode =
      node_from_disk (&config, "a")?;
    assert! ( a_skg . hides . or_default () . is_empty (), "{:?}", a_skg . hides );
    Ok (( )) }

/// Every kind of Folder:
/// - R forks E1, E2
/// - R hides hidden-in-E1, hidden-in-E2, hidden-for-no-reason
/// - E1 contains hidden-in-E1 (hidden) and E11 (not hidden)
/// - E2 contains E21 (not hidden) and hidden-in-E2 (hidden)
/// - hidden-for-no-reason is not in any forkee's content
///
/// Tests two views:
/// - Initial view from R: forkees as such drawn editable, neither
///   showing a forks folder of its own (a member as such shows one
///   only when asked)
/// - The view after a save that asks to extend each forkee: the same, since each is editable already
///
/// Also tests ordering rule: hiddenInForkPartnerFolder precedes content regardless of .skg order.
/// E2's .skg has [E21, hidden-in-E2] but view shows hiddenInForkPartnerFolder before E21.
async fn test_forkee_and_compound_partner_folders (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {

    let (initial_view, _pids, _)
      : (String, Vec<ID>, _)
      = single_root_view(
          &config, None,
          &ID("R" . to_string()) )?;
    println!("Initial view from R:\n{}", initial_view);

    let expected_view = indoc! {
      "* (skg node (gnid R) (skgRepos (nodeRepo main)) (view (rels (contains (out 1)) (forks (out 2)) (hides (out 3))))) R
       ** (skg forksFolder)
       *** (skg node (gnid E1) (skgRepos (nodeRepo main)) (view (rels (contains (out 2 (unintegrated 1))) (forks (in 1 (ancestors 2)))) (birth (forks in 2)))) forkee-1
       **** (skg hiddenInForkPartnerFolder)
       ***** (skg folded node (gnid hidden-in-E1) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1 (ancestors 2))) (hides (in 1 (ancestors 4)))) (birth (contains in 2) (hides in 4)))) hidden-in-E1
       ****** (skg node (gnid E1) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (out 2 (ancestors 1))) (forks (in 1))) cycle)) forkee-1
       **** (skg node (gnid E11) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) E11
       *** (skg node (gnid E2) (skgRepos (nodeRepo main)) (view (rels (contains (out 2 (unintegrated 1))) (forks (in 1 (ancestors 2)))) (birth (forks in 2)))) forkee-2
       **** (skg hiddenInForkPartnerFolder)
       ***** (skg folded node (gnid hidden-in-E2) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1 (ancestors 2))) (hides (in 1 (ancestors 4)))) (birth (contains in 2) (hides in 4)))) hidden-in-E2
       ****** (skg node (gnid E2) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (out 2 (ancestors 1))) (forks (in 1))) cycle)) forkee-2
       **** (skg node (gnid E21) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) E21
       *** (skg hiddenOutsideOfForkPartnersFolder)
       **** (skg folded node (gnid hidden-for-no-reason) (skgRepos (nodeRepo main)) writeProtected (view (rels (hides (in 1 (ancestors 3)))) (birth (hides in 3)))) hidden-for-no-reason
       ** (skg node (gnid R1) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) R1
       "};
    assert_metadata_eq!(initial_view, expected_view,
      "Initial view from R: each forkee as such drawn editable, showing what it shows");

    let extended = { // save unchanged: each forkee as such is editable already
      let modified_view : String =
        initial_view . clone ();
      println!("Saved unchanged:\n{}", modified_view);
      let graph : InRustGraphHandle =
        (
          graph_handle_from_config (config) ?);
      let mut views_state : ViewsState = ViewsState {
        open_views            : OpenViews::new (),
        graph_straddle        : None,};
      let (mut stream, _read_end) : (TcpStream, TcpStream) =
        mk_test_tcp_stream_pair ();
      let response = update_from_and_rerender_buffer (
          &mut stream,
          &modified_view, &config, tantivy, &graph,

          &Err ( String::new () ), &mut views_state
        ) . await ?;
      response . saved_view };
    println!("View from R after an unchanged save:\n{}", extended);

    // Under each member of a hiddenInForkPartnerFolder hangs its
    // containerward chain: here the forkee, a cycle in the view. Each
    // editable forkee as such shows its own forks folder, R forking it
    // and claiming nothing there.
    assert_metadata_eq!(extended, expected_view,
      "View with extended forkees: hiddenInForkPartnerFolder shown before content; hiddenOutsideOfForkPartnersFolder at end");

    Ok (( )) }

/// Hidden within but none hidden without:
/// - R forks E1
/// - R hides H
/// - E1 contains [E11, H, E12] where H is hidden and E11, E12 are not
///
/// Tests two views:
/// - Initial view from R: E1 drawn editable, H in hiddenInForkPartnerFolder before E11 and E12
/// - The view after a save that asks to extend each forkee: the same, since each is editable already
///
/// No hiddenOutsideOfForkPartnersFolder in either state (H is in E1's content).
async fn test_hidden_within_but_none_without (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let (initial_view, _pids, _)
      : (String, Vec<ID>, _)
      = single_root_view( &config, None,
                          &ID("R" . to_string()) )?;
    println!("Initial view from R:\n{}", initial_view);

    let expected_view = indoc! {
      "* (skg node (gnid R) (skgRepos (nodeRepo main)) (view (rels (contains (out 1)) (forks (out 1)) (hides (out 1))))) R
       ** (skg forksFolder)
       *** (skg node (gnid E1) (skgRepos (nodeRepo main)) (view (rels (contains (out 3 (unintegrated 2))) (forks (in 1 (ancestors 2)))) (birth (forks in 2)))) forkee-1
       **** (skg hiddenInForkPartnerFolder)
       ***** (skg folded node (gnid H) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1 (ancestors 2))) (hides (in 1 (ancestors 4)))) (birth (contains in 2) (hides in 4)))) H
       ****** (skg node (gnid E1) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (out 3 (ancestors 1))) (forks (in 1))) cycle)) forkee-1
       **** (skg node (gnid E11) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) E11
       **** (skg node (gnid E12) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) E12
       ** (skg node (gnid R1) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) R1
       "};
    assert_metadata_eq!(initial_view, expected_view,
      "Initial view from R: each forkee as such drawn editable, showing what it shows");

    let extended = { // save unchanged: each forkee as such is editable already
      let modified_view : String =
        initial_view . clone ();
      println!("Saved unchanged:\n{}", modified_view);
      let graph : InRustGraphHandle =
        (
          graph_handle_from_config (config) ?);
      let mut views_state : ViewsState = ViewsState {
        open_views            : OpenViews::new (),
        graph_straddle        : None,};
      let (mut stream, _read_end) : (TcpStream, TcpStream) =
        mk_test_tcp_stream_pair ();
      let response = update_from_and_rerender_buffer (
          &mut stream,
          &modified_view, &config, tantivy, &graph,

          &Err ( String::new () ), &mut views_state
        ) . await ?;
      response . saved_view };
    println!("View from R after an unchanged save:\n{}", extended);

    // hiddenInForkPartnerFolder precedes content regardless of .skg order.
    // E1.skg has [E11, H, E12] but view shows hiddenInForkPartnerFolder (with H) before E11 and E12;
    // after it, E1's forks folder, R forking it.
    assert_metadata_eq!(extended, expected_view,
      "View with extended forkees: hiddenInForkPartnerFolder with H before E11 and E12");

    Ok (( )) }

async fn test_moving_hidden_forkee_content_to_visible_branch_infers_unhide (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let graph : InRustGraphHandle =
      (
        graph_handle_from_config (config) ?);
    let mut views_state : ViewsState = ViewsState {
      open_views        : OpenViews::new (),
      graph_straddle    : None,};

    let (initial_view, _pids, _)
      : (String, Vec<ID>, _)
      = single_root_view(
          &config, None,
          &ID ("R" . to_string()) )?;
    let extended : String =
      save_buffer_for_hidden_from_forks_test (
        &initial_view,
        &config, tantivy, &graph, &mut views_state
      ) . await?;
    let edited : String =
      move_h_from_hiddenin_folder_to_visible_forkee_content (
        &extended );
    let rerendered : String =
      save_buffer_for_hidden_from_forks_test (
        &edited, &config, tantivy, &graph, &mut views_state
      ) . await?;

    assert! (
      ! rerendered . contains ("hiddenInForkPartnerFolder"),
      "Expected no hiddenInForkPartnerFolder after unhiding H:\n{}",
      rerendered );
    assert! (
      rerendered . lines() . any ( |line|
        without_viewnode_ids (line) . starts_with ("**** (skg node (gnid H)") ),
      "Expected H to be visible forkee-as-such content:\n{}",
      rerendered );
    let r_skg : Graphnode =
      node_from_disk (&config, "R")?;
    assert!(
      ! r_skg . hides
        . or_default() . iter ()
        . any ( |m| m . member == ID::from ("H") ),
      "Expected inferred unhide to be persisted in R.skg: {:?}",
      r_skg . hides );

    Ok (( )) }

async fn test_collateral_view_reflects_newly_unhidden_forkee_content (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let graph : InRustGraphHandle =
      (
        graph_handle_from_config (config) ?);
    let mut views_state : ViewsState = ViewsState {
      open_views        : OpenViews::new (),
      graph_straddle    : None,};
    let saved_viewforest_id : ViewforestId =
      ViewforestId::ContentView (
        "collateral-unhide-saved" . to_string());
    let collateral_viewforest_id : ViewforestId =
      ViewforestId::ContentView (
        "collateral-unhide-other" . to_string());

    let (initial_view, pids, viewforest) =
      single_root_view(
        &config, None,
        &ID ("R" . to_string()) )?;
    views_state . open_views . register_view (
      &graph . load_full (), saved_viewforest_id . clone(), viewforest, &pids, &SkgrepoRestriction::unrestricted (config));

    let (extended, collateral_views) =
      save_buffer_and_read_collateral_views (
        &initial_view,
        &saved_viewforest_id, &config, tantivy, &graph,
        &mut views_state ) . await?;
    assert!(
      collateral_views . is_empty(),
      "No collateral view should be registered yet: {:?}",
      collateral_views);

    let extended_viewforest =
      views_state . open_views . viewforest_id_to_view (&saved_viewforest_id)
      . expect ("saved view should be registered")
      . clone();
    let extended_pids : Vec<ID> =
      views_state . open_views . viewforest_id_to_pids (&saved_viewforest_id);
    views_state . open_views . register_view (
      &graph . load_full (), collateral_viewforest_id, extended_viewforest, &extended_pids, &SkgrepoRestriction::unrestricted (config));

    let edited : String =
      move_h_from_hiddenin_folder_to_visible_forkee_content (
        &extended );
    let (_saved_view, collateral_views) =
      save_buffer_and_read_collateral_views (
        &edited, &saved_viewforest_id, &config, tantivy, &graph,
        &mut views_state ) . await?;

    assert_eq!(
      collateral_views . len(), 1,
      "Expected one collateral view:\n{:?}",
      collateral_views);
    assert!(
      collateral_views[0] . lines () . any ( |line|
        without_viewnode_ids (line) . starts_with ("***** (skg folded node (gnid H)")
        && line . contains ("noWarrant") ),
      "Expected the collateral view's hiddenInForkPartnerFolder to keep H, unwarranted:\n{}",
      collateral_views[0]);
    assert! (
      collateral_views[0] . lines() . any ( |line|
        without_viewnode_ids (line) . starts_with ("**** (skg node (gnid H)") ),
      "Expected H to be visible forkee-as-such content in collateral view:\n{}",
      collateral_views[0] );

    Ok (( )) }

async fn test_deleting_from_hiddenin_folder_does_not_unhide (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let graph : InRustGraphHandle =
      (
        graph_handle_from_config (config) ?);
    let mut views_state : ViewsState = ViewsState {
      open_views        : OpenViews::new (),
      graph_straddle    : None,};

    let (initial_view, _pids, _)
      : (String, Vec<ID>, _)
      = single_root_view(
          &config, None,
          &ID ("R" . to_string()) )?;
    let extended : String =
      save_buffer_for_hidden_from_forks_test (
        &initial_view,
        &config, tantivy, &graph, &mut views_state
      ) . await?;
    let edited : String =
      remove_hiddenin_branch (&extended);
    let rerendered : String =
      save_buffer_for_hidden_from_forks_test (
        &edited, &config, tantivy, &graph, &mut views_state
      ) . await?;

    assert! (
      without_viewnode_ids (&rerendered) . contains (
        "**** (skg hiddenInForkPartnerFolder)\n***** (skg folded node (gnid H)"),
      "Expected H to be regenerated under hiddenInForkPartnerFolder:\n{}",
      rerendered );
    assert! (
      ! rerendered . lines() . any ( |line|
        without_viewnode_ids (line) . starts_with ("**** (skg node (gnid H)") ),
      "Expected deleting from hiddenInForkPartnerFolder not to unhide H:\n{}",
      rerendered );

    Ok (( )) }

/// Hidden without but none hidden within:
/// - R forks E1, E2
/// - R hides H
/// - H is not in any forkee's content
/// - E1 contains [E11, E12], E12 contains [E121]
/// - E2 has no content
///
/// Tests two views:
/// - Initial view from R: E1, E2 drawn editable, H in hiddenOutsideOfForkPartnersFolder
/// - The view after a save that asks to extend each forkee: the same, since each is editable already
///
/// No hiddenInForkPartnerFolder in either state (H is not in any forkee's content).
async fn test_hidden_without_but_none_within (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let (initial_view, _pids, _)
      : (String, Vec<ID>, _)
      = single_root_view( &config, None,
                          &ID("R" . to_string()) )?;
    println!("Initial view from R:\n{}", initial_view);
    let expected_view = indoc! {
      "* (skg node (gnid R) (skgRepos (nodeRepo main)) (view (rels (contains (out 1)) (forks (out 2)) (hides (out 1))))) R
       ** (skg forksFolder)
       *** (skg node (gnid E1) (skgRepos (nodeRepo main)) (view (rels (contains (out 2 (unintegrated 2))) (forks (in 1 (ancestors 2)))) (birth (forks in 2)))) forkee-1
       **** (skg node (gnid E11) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) E11
       **** (skg node (gnid E12) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)) (out 1))) (birth (contains in 1)))) E12
       ***** (skg node (gnid E121) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) E121
       *** (skg node (gnid E2) (skgRepos (nodeRepo main)) (view (rels (contains (out 0 (unintegrated 0))) (forks (in 1 (ancestors 2)))) (birth (forks in 2)))) forkee-2
       *** (skg hiddenOutsideOfForkPartnersFolder)
       **** (skg folded node (gnid H) (skgRepos (nodeRepo main)) writeProtected (view (rels (hides (in 1 (ancestors 3)))) (birth (hides in 3)))) H
       ** (skg node (gnid R1) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) R1
       "};
    assert_metadata_eq!(initial_view, expected_view,
      "Initial view from R: each forkee as such drawn editable, showing what it shows");
    let with_forkees_extended = {
      let modified_view : String =
        initial_view . clone ();
      println!("Saved unchanged:\n{}", modified_view);
      let graph : InRustGraphHandle =
        (
          graph_handle_from_config (config) ?);
      let mut views_state : ViewsState = ViewsState {
        open_views            : OpenViews::new (),
        graph_straddle        : None,};
      let (mut stream, _read_end) : (TcpStream, TcpStream) =
        mk_test_tcp_stream_pair ();
      let response = update_from_and_rerender_buffer (
        &mut stream,
        &modified_view, &config, tantivy, &graph,

        &Err ( String::new () ), &mut views_state ) . await ?;
      response . saved_view };
    println!("View from R after an unchanged save:\n{}",
             with_forkees_extended);
    assert_metadata_eq!(with_forkees_extended, expected_view,
      "View with extended forkees: H still in hiddenOutsideOfForkPartnersFolder (at end); E1 extended with E11, E12; E2 extended, its content empty; each shows its forks folder, R forking it");

    Ok (( )) }

async fn test_adding_to_hiddenoutside_folder_hides_and_moves_inside (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let graph : InRustGraphHandle =
      (
        graph_handle_from_config (config) ?);
    let mut views_state : ViewsState = ViewsState {
      open_views        : OpenViews::new (),
      graph_straddle    : None,};

    let (initial_view, _pids, _)
      : (String, Vec<ID>, _)
      = single_root_view(
          &config, None,
          &ID ("R" . to_string()) )?;
    let edited : String =
      add_e11_to_hiddenoutside_folder (&initial_view);
    let (mut stream, _read_end) : (TcpStream, TcpStream) =
      mk_test_tcp_stream_pair ();
    let response = update_from_and_rerender_buffer (
      &mut stream, &edited, config, tantivy, &graph,
      &Err (String::new ()), &mut views_state ) . await ?;
    let rerendered : String = response . saved_view;

    assert! (
      rerendered . lines () . any ( |line|
        line . contains ("(gnid H)")),
      "Expected original hidden-outside viewnode H to remain:\n{}",
      rerendered );
    { let e11_lines : Vec<&str> = rerendered . lines ()
        . filter ( |line| line . contains ("(gnid E11)") ) . collect ();
      assert! (
        e11_lines . len () == 1
        && e11_lines [0] . contains ("(hides (in 1 (ancestors 4)))"),
        "E11, now hidden inside forkee E1, which is drawn editable, \
         leaves the folder where it was typed and stands only in E1's \
         hiddenInForkPartnerFolder:\n{}",
        rerendered ); }
    let forker : Graphnode = node_from_disk (config, "R") ?;
    assert! (
      members_of (forker . hides . or_default ())
        . contains (&ID::from ("E11")),
      "Expected adding E11 to hiddenOutsideOfForkPartnersFolder to save a hide." );
    assert! (
      response . warnings . iter () . any ( |warning|
        warning . contains ("Saved hide for")
        && warning . contains ("HiddenInForkPartnerFolder") ),
      "Expected the post-skgsave-commit moved-inside warning: {:?}",
      response . warnings );
    let extended : String =
      save_buffer_for_hidden_from_forks_test (
        &rerendered,
        &config, tantivy, &graph, &mut views_state
      ) . await ?;
    assert! (
      extended . contains ("hiddenInForkPartnerFolder")
      && extended . lines () . any ( |line| line . contains ("(gnid E11)") ),
      "The hidden child should appear in hiddenHere once its forkee is \
       editable:\n{}", extended );
    let without_h : String = extended . lines ()
      . filter (|line| ! line . contains ("(gnid H)"))
      . collect::<Vec<_>> () . join ("\n") + "\n";
    let _ : String = save_buffer_for_hidden_from_forks_test (
      &without_h, &config, tantivy, &graph, &mut views_state
    ) . await ?;
    let forker_after_removal : Graphnode = node_from_disk (config, "R") ?;
    assert! (
      ! members_of (
          forker_after_removal . hides . or_default ())
        . contains (&ID::from ("H")),
      "Removing H from hiddenOutsideOfForkPartnersFolder must remove its hide." );

    Ok (( )) }

/// Overlapping hidden within:
/// - R forks E1, E2
/// - R hides H
/// - Both E1 and E2 contain H (and nothing else)
///
/// Tests two views:
/// - Initial view from R: E1, E2 drawn editable, H in each one's hiddenInForkPartnerFolder
/// - The view after a save that asks to extend each forkee: the same, since each is editable already
///
/// No hiddenOutsideOfForkPartnersFolder in either state (H is in forkees' content).
async fn test_overlapping_hidden_within (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let (initial_view, _pids, _)
      : (String, Vec<ID>, _)
      = single_root_view( &config, None,
                          &ID("R" . to_string()) )?;
    println!("Initial view from R:\n{}", initial_view);
    let expected_view = indoc! {
      "* (skg node (gnid R) (skgRepos (nodeRepo main)) (view (rels (contains (out 1)) (forks (out 2)) (hides (out 1))))) R
       ** (skg forksFolder)
       *** (skg node (gnid E1) (skgRepos (nodeRepo main)) (view (rels (contains (out 1 (unintegrated 0))) (forks (in 1 (ancestors 2)))) (birth (forks in 2)))) forkee-1
       **** (skg hiddenInForkPartnerFolder)
       ***** (skg folded node (gnid H) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 2 (ancestors 2))) (hides (in 1 (ancestors 4)))) (birth (contains in 2) (hides in 4)))) H
       ****** (skg node (gnid E1) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (out 1 (ancestors 1))) (forks (in 1))) cycle)) forkee-1
       ****** (skg node (gnid E2) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (out 1 (ancestors 1))) (forks (in 1))))) forkee-2
       *** (skg node (gnid E2) (skgRepos (nodeRepo main)) (view (rels (contains (out 1 (unintegrated 0))) (forks (in 1 (ancestors 2)))) (birth (forks in 2)))) forkee-2
       **** (skg hiddenInForkPartnerFolder)
       ***** (skg folded node (gnid H) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 2 (ancestors 2))) (hides (in 1 (ancestors 4)))) (birth (contains in 2) (hides in 4)))) H
       ****** (skg node (gnid E1) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (out 1 (ancestors 1))) (forks (in 1))))) forkee-1
       ****** (skg node (gnid E2) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (out 1 (ancestors 1))) (forks (in 1))) cycle)) forkee-2
       ** (skg node (gnid R1) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) R1
       "};
    assert_metadata_eq!(initial_view, expected_view,
      "Initial view from R: each forkee as such drawn editable, showing what it shows");
    let extended = {
      let modified_view : String =
        initial_view . clone ();
      println!("Saved unchanged:\n{}",
               modified_view);
      let graph : InRustGraphHandle =
        (
          graph_handle_from_config (config) ?);
      let mut views_state : ViewsState = ViewsState {
        open_views            : OpenViews::new (),
        graph_straddle        : None,};
      let (mut stream, _read_end) : (TcpStream, TcpStream) =
        mk_test_tcp_stream_pair ();
      let response = update_from_and_rerender_buffer (
        &mut stream,
        &modified_view, &config, tantivy, &graph,

        &Err ( String::new () ), &mut views_state ) . await ?;
      response . saved_view };
    println!("View from R after an unchanged save:\n{}", extended);
    assert_metadata_eq!(extended, expected_view,
      "View with extended forkees: H appears in hiddenInForkPartnerFolder under both E1 and E2, each showing its forks folder");
    Ok (( )) }
