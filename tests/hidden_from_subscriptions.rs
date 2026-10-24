// cargo nextest run --test grouped_views -E 'test(hidden_from_subscriptions::)'
//
// Tests for HiddenOutsideOfSubscribeeFolder, HiddenInSubscribeeFolder, and HiddenFromSubscribees.
// These test that:
// 1. Initial view shows subscribees as write-protected with HiddenOutsideOfSubscribeeFolder
// 2. After saving with editable view requests, HiddenInSubscribeeFolder is shown

use skg::metadata_canon::without_viewnode_ids;
use crate::git_fixture_helpers::mk_test_tcp_stream_pair;
use indoc::indoc;
use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_skgrepos;
use skg::assert_metadata_eq;
use skg::test_utils::{
  extract_string_field_from_sexp,
  graph_handle_from_config,
  read_all_lp_messages,
  run_with_shared_test_stores,
  update_from_and_rerender_buffer_test as update_from_and_rerender_buffer };
use skg::to_org::render::content_view::single_root_view;
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

/// Add (viewRequests editableView) to all subscribee nodes in org text.
/// Modifies the node section of each subscribee to request an editable view.
/// Subscribees are UnrestrictedVognode children of SubscribeeFolders.
///
/// KLUDGE: We identify subscribees by matching on "subscribee-" in the title.
/// That's easier than navigating the org-tree's topoogy.
fn add_editable_view_request_to_subscribees (
  org_text : &str,
) -> String {
  // Process line-by-line, inserting viewRequests after write-protected
  // for lines containing "subscribee-" in the title
  org_text
    . lines()
    . map(|line| {
      if line . contains ("subscribee-") && line . contains ("writeProtected") {
        // Insert viewRequests right after the `write-protected' atom. In the
        // semantic-herald format `writeProtected' may be followed by `)'
        // (no more atoms) or by `(rels ...)' or `(viewStats ...)'.
        if line . contains ("writeProtected)") {
          line . replace (
            "writeProtected)", "writeProtected (viewRequests editableView))")
        } else {
          line . replace (
            "writeProtected ", "writeProtected (viewRequests editableView) ") }
      } else {
        line . to_string()
      }
    })
    . collect::<Vec<_>>()
    . join ("\n") + "\n" }

async fn save_buffer_for_hidden_subscriptions_test (
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
      ! line . contains ("(id e1)")
      && ! line . contains ("(id e11)") })
    . collect::<Vec<_>>()
    . join ("\n") + "\n" }

fn insert_before_r1 (
  buffer : &str,
  inserted : &str,
) -> String {
  buffer . replace (
    "** (skg (node (id r1)",
    &format! ("{}** (skg (node (id r1)", inserted)) }

fn extended_subscribee_edit_view (
  initial_view : &str,
  edit_kind    : &str,
) -> String {
  let without_e1 : String = remove_e1_subtree (initial_view);
  match edit_kind {
    "delete" => without_e1,
    "move_to_r" => insert_before_r1 (
      &without_e1,
      "** (skg (node (id e1) (repo foreign) writeProtected)) e1\n" ),
    "move_to_a" => format! (
      "{}* (skg (node (id a) (repo owned))) a\n** (skg (node (id e1) (repo foreign) writeProtected)) e1\n",
      without_e1 ),
    _ => panic! ("unknown edit kind: {}", edit_kind), }}

fn assert_hides_e1_in_subscribee_folder (
  buffer : &str,
) {
  assert! (
    // 'folded': a new hidden folder begins folded,
    // expressed as a folded atom on each member.
    without_viewnode_ids (buffer) . contains ("**** (skg hiddenInSubscribeeFolder)\n***** (skg folded (node (id e1) (repo foreign) writeProtected"),
    "Expected e1 to be rendered folded under HiddenInSubscribeeFolder:\n{}",
    buffer );
  assert! (
    ! buffer . lines() . any ( |line|
      line . starts_with ("**** (skg (node (id e1)") ),
    "Expected e1 not to remain visible as subscribee-as-such content:\n{}",
    buffer ); }

fn assert_does_not_hide_e1 (
  buffer : &str,
) {
  assert! (
    ! buffer . contains ("hiddenInSubscribeeFolder"),
    "Expected no HiddenInSubscribeeFolder for e1:\n{}",
    buffer ); }

fn move_h_from_hiddenin_folder_to_visible_subscribee_content (
  buffer : &str,
) -> String {
  let mut in_h_subtree : bool = false; // H's containerward chain moves with it
  buffer
    . lines()
    . filter_map ( |line| {
      in_h_subtree = in_h_subtree && line . starts_with ("******");
      if without_viewnode_ids (line) . contains ("(skg hiddenInSubscribeeFolder)") {
        None
      } else if line . contains ("(id H)") {
        in_h_subtree = true;
        Some (line . replacen ("***** ", "**** ", 1)
              // A real client manages fold metadata itself (they are
              // re-derived from visibility at save), so the moved
              // line would not carry the folder's 'folded' mark.
              . replacen ("(skg folded ", "(skg ", 1))
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
      in_h_subtree = line . contains ("(id H)")
        || ( in_h_subtree && line . starts_with ("******") );
      ! without_viewnode_ids (line) . contains ("(skg hiddenInSubscribeeFolder)")
      && ! in_h_subtree } )
    . collect::<Vec<_>>()
    . join ("\n") + "\n" }

fn add_e11_to_hiddenoutside_folder (
  buffer : &str,
) -> String {
  insert_after_line_containing (
    buffer,
    "(skg hiddenOutsideOfSubscribeeFolder)",
    "**** (skg (node (id E11) (repo main) writeProtected)) E11" ) }

fn assert_e1_removed_from_visible_subscribee_branch (
  buffer : &str,
) {
  assert! (
    ! buffer . lines() . any ( |line|
      line . starts_with ("**** (skg (node (id e1)") ),
    "Expected e1 not to be regenerated as subscribee-as-such content:\n{}",
    buffer );
  assert! (
    buffer . contains ("**** (skg (node (id e2)"),
    "Expected unrelated subscribee-as-such content e2 to remain visible:\n{}",
    buffer ); }

fn remove_e1_and_e2_subtrees (
  buffer : &str,
) -> String {
  buffer
    . lines()
    . filter ( |line| {
      ! line . contains ("(id e1)")
      && ! line . contains ("(id e11)")
      && ! line . contains ("(id e2)")
      && ! line . contains ("(id e12)") })
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
  let skgid : ID = ID::from (pid);
  read_all_skg_files_from_skgrepos (config)?
    . into_iter()
    . find ( |node| node . pid == skgid )
    . ok_or_else ( || format! ("node not found on disk: {}", pid) . into() ) }

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-hidden-from-subscriptions",
    |s| Box::pin ( async move {
      s . reset ("test_deleting_foreign_subscribee_content_preserves_branch_edit",
        "tests/hidden_from_subscriptions/fixtures-subscribee-edit") ?;
      test_deleting_foreign_subscribee_content_preserves_branch_edit (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_deleting_foreign_subscribee_content_infers_hide",
        "tests/hidden_from_subscriptions/fixtures-subscribee-edit") ?;
      test_deleting_foreign_subscribee_content_infers_hide (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_removing_subscribee_content_from_subscriber_contains_infers_hide",
        "tests/hidden_from_subscriptions/fixtures-clone-edit") ?;
      test_removing_subscribee_content_from_subscriber_contains_infers_hide (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_readding_subscribee_content_to_subscriber_contains_unhides",
        "tests/hidden_from_subscriptions/fixtures-clone-edit") ?;
      test_readding_subscribee_content_to_subscriber_contains_unhides (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_collateral_view_reflects_newly_hidden_subscribee_content",
        "tests/hidden_from_subscriptions/fixtures-subscribee-edit") ?;
      test_collateral_view_reflects_newly_hidden_subscribee_content (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_moving_foreign_subscribee_content_to_subscriber_does_not_hide",
        "tests/hidden_from_subscriptions/fixtures-subscribee-edit") ?;
      test_moving_foreign_subscribee_content_to_subscriber_does_not_hide (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_moving_foreign_subscribee_content_elsewhere_still_hides",
        "tests/hidden_from_subscriptions/fixtures-subscribee-edit") ?;
      test_moving_foreign_subscribee_content_elsewhere_still_hides (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_extra_view_child_under_foreign_subscribee_is_deleted",
        "tests/hidden_from_subscriptions/fixtures-subscribee-edit") ?;
      test_extra_view_child_under_foreign_subscribee_is_deleted (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_extra_view_child_under_owned_subscribee_is_deleted",
        "tests/hidden_from_subscriptions/fixtures-subscribee-edit") ?;
      test_extra_view_child_under_owned_subscribee_is_deleted (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset_from_config ("test_subscribee_and_filter_folders",
        "tests/hidden_from_subscriptions/fixtures-every-kind-of-folder/skgconfig.toml") ?;
      test_subscribee_and_filter_folders (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset_from_config ("test_hidden_within_but_none_without",
        "tests/hidden_from_subscriptions/fixtures-hidden-within-but-none-without/skgconfig.toml") ?;
      test_hidden_within_but_none_without (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_moving_hidden_subscribee_content_to_visible_branch_infers_unhide",
        "tests/hidden_from_subscriptions/fixtures-hidden-within-but-none-without") ?;
      test_moving_hidden_subscribee_content_to_visible_branch_infers_unhide (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_collateral_view_reflects_newly_unhidden_subscribee_content",
        "tests/hidden_from_subscriptions/fixtures-hidden-within-but-none-without") ?;
      test_collateral_view_reflects_newly_unhidden_subscribee_content (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_deleting_from_hiddenin_folder_does_not_unhide",
        "tests/hidden_from_subscriptions/fixtures-hidden-within-but-none-without") ?;
      test_deleting_from_hiddenin_folder_does_not_unhide (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset_from_config ("test_hidden_without_but_none_within",
        "tests/hidden_from_subscriptions/fixtures-hidden-without-but-none-within/skgconfig.toml") ?;
      test_hidden_without_but_none_within (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_adding_to_hiddenoutside_folder_hides_and_moves_inside",
        "tests/hidden_from_subscriptions/fixtures-hidden-without-but-none-within") ?;
      test_adding_to_hiddenoutside_folder_hides_and_moves_inside (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset_from_config ("test_overlapping_hidden_within",
        "tests/hidden_from_subscriptions/fixtures-overlapping-hidden-within/skgconfig.toml") ?;
      test_overlapping_hidden_within (
        &s . config, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

async fn test_deleting_foreign_subscribee_content_preserves_branch_edit (
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
      save_buffer_for_hidden_subscriptions_test (
        &add_editable_view_request_to_subscribees (&initial_view),
        &config, tantivy, &graph, &mut views_state
      ) . await?;
    let edited : String =
      extended_subscribee_edit_view (&extended, "delete");
    let rerendered : String =
      save_buffer_for_hidden_subscriptions_test (
        &edited, &config, tantivy, &graph, &mut views_state
      ) . await?;

    assert_e1_removed_from_visible_subscribee_branch (&rerendered);
    Ok (( )) }

async fn test_deleting_foreign_subscribee_content_infers_hide (
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
      save_buffer_for_hidden_subscriptions_test (
        &add_editable_view_request_to_subscribees (&initial_view),
        &config, tantivy, &graph, &mut views_state
      ) . await?;
    let edited : String =
      extended_subscribee_edit_view (&extended, "delete");
    let (mut stream, _read_end) : (TcpStream, TcpStream) =
      mk_test_tcp_stream_pair ();
    let response = update_from_and_rerender_buffer (
      &mut stream, &edited, config, tantivy, &graph,
      &Err (String::new ()), &mut views_state ) . await ?;
    let rerendered : String = response . saved_view;

    assert_hides_e1_in_subscribee_folder (&rerendered);
    let r_skg : Graphnode =
      node_from_disk (&config, "r")?;
    assert_eq!(
      members_of ( r_skg . hidesFromSubs . or_default() ),
      vec! [ID::from ("e1")],
      "Expected inferred hide to be persisted in r.skg: {:?}",
      r_skg . hidesFromSubs );
    Ok (( )) }

/// The contains-removal rule ('docs/forks.org', "Editing the clone:
/// deletions become hides"): owned f contains [c, x] and subscribes to o,
/// whose contains include c (but not x). Saving f without its children must
/// hide c (it would otherwise reappear under f as unintegrated subscribed
/// content), and must not hide x (no subscribee contains it -- deleting it
/// is an ordinary contains edit).
async fn test_removing_subscribee_content_from_subscriber_contains_infers_hide (
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
      * (skg (node (id f) (repo owned))) f
      "};
    save_buffer_for_hidden_subscriptions_test (
      buffer, &config, tantivy, &graph, &mut views_state
    ) . await ?;
    let f_skg : Graphnode =
      node_from_disk (&config, "f") ?;
    assert! ( f_skg . contains . is_empty (),
      "f's contains must be emptied by the save: {:?}",
      f_skg . contains );
    assert_eq! (
      members_of ( f_skg . hidesFromSubs . or_default () ),
      vec! [ ID::from ("c") ],
      "removing c (content of subscribee o) must hide it, and removing x (content of no subscribee) must not: {:?}",
      f_skg . hidesFromSubs );
    Ok (( )) }

/// The inverse of the contains-removal rule: owned g hides c and
/// subscribes to o (which contains c). Saving g with c as content
/// must drop the now-stale hide -- c is integrated, so g should no
/// longer appear among c's hiders.
async fn test_readding_subscribee_content_to_subscriber_contains_unhides (
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
      * (skg (node (id g) (repo owned))) g
      ** (skg (node (id c) (repo foreign) writeProtected)) c
      "};
    save_buffer_for_hidden_subscriptions_test (
      buffer, &config, tantivy, &graph, &mut views_state
    ) . await ?;
    let g_skg : Graphnode =
      node_from_disk (&config, "g") ?;
    assert_eq! ( members_of (& g_skg . contains), vec! [ ID::from ("c") ],
      "g's contains must now hold c: {:?}", g_skg . contains );
    assert! (
      g_skg . hidesFromSubs . or_default () . is_empty (),
      "re-adding c to g's contains must drop g's stale hide of c: {:?}",
      g_skg . hidesFromSubs );
    Ok (( )) }

async fn test_collateral_view_reflects_newly_hidden_subscribee_content (
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
      &graph . load_full (), saved_viewforest_id . clone(), viewforest, &pids);

    let (extended, collateral_views) =
      save_buffer_and_read_collateral_views (
        &add_editable_view_request_to_subscribees (&initial_view),
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
      &graph . load_full (), collateral_viewforest_id, extended_viewforest, &extended_pids);

    let edited : String =
      extended_subscribee_edit_view (&extended, "delete");
    let (_saved_view, collateral_views) =
      save_buffer_and_read_collateral_views (
        &edited, &saved_viewforest_id, &config, tantivy, &graph,
        &mut views_state ) . await?;

    assert_eq!(
      collateral_views . len(), 1,
      "Expected one collateral view:\n{:?}",
      collateral_views);
    // e1 was hidden, so it appears under the HiddenInSubscribeeFolder. But e1's
    // in-view subtree (e11) is a user branch, so the glossary's "retain
    // as non-member or tombstone, then prune" retains the visible
    // occurrence as non-member
    // rather than deleting it -- chaos-monkey safety: the user may
    // have deliberately placed content there, and we must not lose it. So e1
    // shows twice: once hidden (write-protected), once as a preserved
    // non-member branch, still editable with e11 its content, whose stats
    // say, as the hidden folder's member's do, that its viewparent (the
    // subscribee) contains it and the subscriber, three generations up,
    // hides it.
    let collateral : &str = &collateral_views[0];
    assert! (
      without_viewnode_ids (collateral) . contains (
        "**** (skg hiddenInSubscribeeFolder)\n***** (skg folded (node (id e1) (repo foreign) writeProtected"),
      "Expected e1 under HiddenInSubscribeeFolder:\n{}", collateral );
    let retained_e1 : &str = collateral . lines ()
      . find ( |line| line . starts_with ("**** (skg (node (id e1)")
                      && line . contains ("nonMember") )
      . unwrap_or_else ( || panic! (
        "Expected hidden branch e1 made a non-member (subtree preserved):\n{}",
        collateral ));
    assert! ( ! retained_e1 . contains ("writeProtected"),
      "the retained e1 keeps its editability:\n{}", collateral );
    assert! ( collateral . lines ()
                . any ( |line| line . starts_with ("***** (skg (node (id e11)")
                               && ! line . contains ("nonMember") ),
      "e11 stays the retained e1's content:\n{}", collateral );
    assert! ( retained_e1 . contains ("(contains (in 1 (ancestors 1))")
              && retained_e1 . contains ("(hidesFromSubs (in 1 (ancestors 3)))"),
      "the retained e1 says who contains and who hides it:\n{}", collateral );

    Ok (( )) }

async fn test_moving_foreign_subscribee_content_to_subscriber_does_not_hide (
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
      save_buffer_for_hidden_subscriptions_test (
        &add_editable_view_request_to_subscribees (&initial_view),
        &config, tantivy, &graph, &mut views_state
      ) . await?;
    let edited : String =
      extended_subscribee_edit_view (&extended, "move_to_r");
    let rerendered : String =
      save_buffer_for_hidden_subscriptions_test (
        &edited, &config, tantivy, &graph, &mut views_state
      ) . await?;

    assert_does_not_hide_e1 (&rerendered);
    assert! (
      rerendered . contains (
        "** (skg (node (id e1) (repo foreign) writeProtected" ),
      "Expected e1 to remain ordinary content of r:\n{}",
      rerendered );
    // Regression ('docs/forks.org', "Unintegrated content: the
    // subscribeeFolder"): e1, now contained by the
    // subscriber r, must not also re-appear as unintegrated subscribee-as-such
    // content. Before the contains-subtraction fix it showed twice -- once as
    // r's integrated content (above) and once under the subscribee-as-such.
    assert_e1_removed_from_visible_subscribee_branch (&rerendered);
    Ok (( )) }

async fn test_moving_foreign_subscribee_content_elsewhere_still_hides (
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
      save_buffer_for_hidden_subscriptions_test (
        &add_editable_view_request_to_subscribees (&initial_view),
        &config, tantivy, &graph, &mut views_state
      ) . await?;
    let edited : String =
      extended_subscribee_edit_view (&extended, "move_to_a");
    let rerendered : String =
      save_buffer_for_hidden_subscriptions_test (
        &edited, &config, tantivy, &graph, &mut views_state
      ) . await?;

    assert_hides_e1_in_subscribee_folder (&rerendered);
    assert! (
      rerendered . contains (
        "** (skg (node (id e1) (repo foreign) writeProtected" ),
      "Expected e1 to remain content of a:\n{}",
      rerendered );
    Ok (( )) }

async fn test_extra_view_child_under_foreign_subscribee_is_deleted (
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
      save_buffer_for_hidden_subscriptions_test (
        &add_editable_view_request_to_subscribees (&initial_view),
        &config, tantivy, &graph, &mut views_state
      ) . await?;
    let edited : String =
      insert_after_line_containing (
        &remove_e1_and_e2_subtrees (&extended),
        "(id e)",
        "**** (skg (node (id e2) (repo foreign) writeProtected)) e2\n\
         **** (skg (node (id a) (repo owned) writeProtected)) a\n\
         **** (skg (node (id e1) (repo foreign) writeProtected)) e1" );
    let rerendered : String =
      save_buffer_for_hidden_subscriptions_test (
        &edited, &config, tantivy, &graph, &mut views_state
      ) . await?;

    // §6.0: the extra viewchild 'a' is a stale leaf (not in e's contains,
    // claiming membership, no children), so it is deleted -- not preserved as
    // a non-member child.
    assert!(
      ! rerendered . lines() . any ( |line| line . contains ("(id a)") ),
      "Extra stale-leaf view-child 'a' should be deleted, not preserved:\n{}",
      rerendered );
    assert_line_order (&rerendered, "(id e1)", "(id e2)");
    assert_does_not_hide_e1 (&rerendered);
    let e_skg : Graphnode =
      node_from_disk (&config, "e")?;
    assert!(
      ! e_skg . contains . iter () . any ( |m| m . member == ID::from ("a") ),
      "Extra view-child should not become graph-content of e: {:?}",
      e_skg . contains );

    Ok (( )) }

async fn test_extra_view_child_under_owned_subscribee_is_deleted (
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
              * (skg (node (id a) (repo owned))) a
              ** (skg subscribeeFolder)
              *** (skg (node (id r) (repo owned))) r
              **** (skg (node (id r2) (repo owned) writeProtected)) r2
              **** (skg (node (id e) (repo foreign) writeProtected)) subscribee-e
              **** (skg (node (id r1) (repo owned) writeProtected)) r1
              "} . to_string();
    let rerendered : String =
      save_buffer_for_hidden_subscriptions_test (
        &edited, &config, tantivy, &graph, &mut views_state
      ) . await?;

    // §6.0: the extra viewchild 'e' is a stale leaf (not in r's contains,
    // claiming membership, no children), so it is deleted -- not preserved as
    // a non-member child.
    assert!(
      ! rerendered . lines() . any ( |line| line . contains ("(id e)") ),
      "Extra stale-leaf view-child 'e' should be deleted, not preserved:\n{}",
      rerendered );
    assert_line_order (&rerendered, "(id r1)", "(id r2)");
    assert!(
      ! rerendered . contains ("hiddenInSubscribeeFolder"),
      "Extra view-child should not infer a hide:\n{}",
      rerendered );
    let r_skg : Graphnode =
      node_from_disk (&config, "r")?;
    assert!(
      ! r_skg . contains . iter () . any ( |m| m . member == ID::from ("e") ),
      "Extra view-child should not become graph-content of r: {:?}",
      r_skg . contains );

    Ok (( )) }

/// Every kind of Folder:
/// - R subscribes to E1, E2
/// - R hides hidden-in-E1, hidden-in-E2, hidden-for-no-reason
/// - E1 contains hidden-in-E1 (hidden) and E11 (not hidden)
/// - E2 contains E21 (not hidden) and hidden-in-E2 (hidden)
/// - hidden-for-no-reason is not in any subscribee's content
///
/// Tests two views:
/// - Initial view from R: subscribees are write-protected (bare leaves)
/// - View from R with editable views extended at each subscribee
///
/// Also tests ordering rule: HiddenInSubscribeeFolder precedes content regardless of .skg order.
/// E2's .skg has [E21, hidden-in-E2] but view shows HiddenInSubscribeeFolder before E21.
async fn test_subscribee_and_filter_folders (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {

    let (initial_view, _pids, _)
      : (String, Vec<ID>, _)
      = single_root_view(
          &config, None,
          &ID("R" . to_string()) )?;
    println!("Initial view from R:\n{}", initial_view);

    let expected_initial = indoc! {
      "* (skg (node (id R) (repo main) (rels (contains (out 1)) (subscribesTo (out 2)) (hidesFromSubs (out 3))))) R
       ** (skg subscribeeFolder)
       *** (skg (node (id E1) (repo main) writeProtected (rels (contains (out 2 (unintegrated 1))) (subscribesTo (in 1 (ancestors 2))) (birth (subscribesTo in 2))))) subscribee-1
       *** (skg (node (id E2) (repo main) writeProtected (rels (contains (out 2 (unintegrated 1))) (subscribesTo (in 1 (ancestors 2))) (birth (subscribesTo in 2))))) subscribee-2
       *** (skg hiddenOutsideOfSubscribeeFolder)
       **** (skg folded (node (id hidden-for-no-reason) (repo main) writeProtected (rels (hidesFromSubs (in 1 (ancestors 3))) (birth (hidesFromSubs in 3))))) hidden-for-no-reason
       ** (skg (node (id R1) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) R1
       "};
    assert_metadata_eq!(initial_view, expected_initial,
      "Initial view from R: write-protected subscribees are bare leaves; only HiddenOutsideOfSubscribeeFolder shown");

    let extended = { // Request editable views, then save
      let modified_view : String =
        add_editable_view_request_to_subscribees (&initial_view);
      println!("Modified view (with editable requests):\n{}", modified_view);
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
    println!("View from R after save with editable view requests:\n{}", extended);

    let expected_extended = indoc! {
      "* (skg (node (id R) (repo main) (rels (contains (out 1)) (subscribesTo (out 2)) (hidesFromSubs (out 3))))) R
       ** (skg subscribeeFolder)
       *** (skg (node (id E1) (repo main) (rels (contains (out 2 (unintegrated 1))) (subscribesTo (in 1 (ancestors 2))) (birth (subscribesTo in 2))))) subscribee-1
       **** (skg hiddenInSubscribeeFolder)
       ***** (skg folded (node (id hidden-in-E1) (repo main) writeProtected (rels (contains (in 1 (ancestors 2))) (hidesFromSubs (in 1 (ancestors 4))) (birth (contains in 2) (hidesFromSubs in 4))))) hidden-in-E1
       ****** (skg (node (id E1) (repo main) nonMember writeProtected (rels (contains (out 2 (ancestors 1))) (subscribesTo (in 1))) (viewStats cycle))) subscribee-1
       **** (skg (node (id E11) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) E11
       *** (skg (node (id E2) (repo main) (rels (contains (out 2 (unintegrated 1))) (subscribesTo (in 1 (ancestors 2))) (birth (subscribesTo in 2))))) subscribee-2
       **** (skg hiddenInSubscribeeFolder)
       ***** (skg folded (node (id hidden-in-E2) (repo main) writeProtected (rels (contains (in 1 (ancestors 2))) (hidesFromSubs (in 1 (ancestors 4))) (birth (contains in 2) (hidesFromSubs in 4))))) hidden-in-E2
       ****** (skg (node (id E2) (repo main) nonMember writeProtected (rels (contains (out 2 (ancestors 1))) (subscribesTo (in 1))) (viewStats cycle))) subscribee-2
       **** (skg (node (id E21) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) E21
       *** (skg hiddenOutsideOfSubscribeeFolder)
       **** (skg folded (node (id hidden-for-no-reason) (repo main) writeProtected (rels (hidesFromSubs (in 1 (ancestors 3))) (birth (hidesFromSubs in 3))))) hidden-for-no-reason
       ** (skg (node (id R1) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) R1
       "};
    // Under each member of a hiddenInSubscribeeFolder hangs its
    // containerward chain: here the subscribee, a cycle in the view.
    assert_metadata_eq!(extended, expected_extended,
      "View with extended subscribees: HiddenInSubscribeeFolder shown before content; HiddenOutsideOfSubscribeeFolder at end");

    Ok (( )) }

/// Hidden within but none hidden without:
/// - R subscribes to E1
/// - R hides H
/// - E1 contains [E11, H, E12] where H is hidden and E11, E12 are not
///
/// Tests two views:
/// - Initial view from R: E1 is write-protected (bare leaf), H doesn't appear
/// - View from R with editable views extended at each subscribee: H appears in HiddenInSubscribeeFolder before E11 and E12
///
/// No HiddenOutsideOfSubscribeeFolder in either state (H is in E1's content).
async fn test_hidden_within_but_none_without (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let (initial_view, _pids, _)
      : (String, Vec<ID>, _)
      = single_root_view( &config, None,
                          &ID("R" . to_string()) )?;
    println!("Initial view from R:\n{}", initial_view);

    let expected_initial = indoc! {
      "* (skg (node (id R) (repo main) (rels (contains (out 1)) (subscribesTo (out 1)) (hidesFromSubs (out 1))))) R
       ** (skg subscribeeFolder)
       *** (skg (node (id E1) (repo main) writeProtected (rels (contains (out 3 (unintegrated 2))) (subscribesTo (in 1 (ancestors 2))) (birth (subscribesTo in 2))))) subscribee-1
       ** (skg (node (id R1) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) R1
       "};
    assert_metadata_eq!(initial_view, expected_initial,
      "Initial view from R: write-protected subscribee is bare leaf; H doesn't appear");

    let extended = { // request editable views, then save
      let modified_view : String =
        add_editable_view_request_to_subscribees (&initial_view);
      println!("Modified view (with editable requests):\n{}", modified_view);
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
    println!("View from R after save with editable view requests:\n{}", extended);

    // HiddenInSubscribeeFolder precedes content regardless of .skg order.
    // E1.skg has [E11, H, E12] but view shows HiddenInSubscribeeFolder (with H) before E11 and E12.
    let expected_extended = indoc! {
      "* (skg (node (id R) (repo main) (rels (contains (out 1)) (subscribesTo (out 1)) (hidesFromSubs (out 1))))) R
       ** (skg subscribeeFolder)
       *** (skg (node (id E1) (repo main) (rels (contains (out 3 (unintegrated 2))) (subscribesTo (in 1 (ancestors 2))) (birth (subscribesTo in 2))))) subscribee-1
       **** (skg hiddenInSubscribeeFolder)
       ***** (skg folded (node (id H) (repo main) writeProtected (rels (contains (in 1 (ancestors 2))) (hidesFromSubs (in 1 (ancestors 4))) (birth (contains in 2) (hidesFromSubs in 4))))) H
       ****** (skg (node (id E1) (repo main) nonMember writeProtected (rels (contains (out 3 (ancestors 1))) (subscribesTo (in 1))) (viewStats cycle))) subscribee-1
       **** (skg (node (id E11) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) E11
       **** (skg (node (id E12) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) E12
       ** (skg (node (id R1) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) R1
       "};
    assert_metadata_eq!(extended, expected_extended,
      "View with extended subscribees: HiddenInSubscribeeFolder with H before E11 and E12");

    Ok (( )) }

async fn test_moving_hidden_subscribee_content_to_visible_branch_infers_unhide (
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
      save_buffer_for_hidden_subscriptions_test (
        &add_editable_view_request_to_subscribees (&initial_view),
        &config, tantivy, &graph, &mut views_state
      ) . await?;
    let edited : String =
      move_h_from_hiddenin_folder_to_visible_subscribee_content (
        &extended );
    let rerendered : String =
      save_buffer_for_hidden_subscriptions_test (
        &edited, &config, tantivy, &graph, &mut views_state
      ) . await?;

    assert! (
      ! rerendered . contains ("hiddenInSubscribeeFolder"),
      "Expected no HiddenInSubscribeeFolder after unhiding H:\n{}",
      rerendered );
    assert! (
      rerendered . lines() . any ( |line|
        line . starts_with ("**** (skg (node (id H)") ),
      "Expected H to be visible subscribee-as-such content:\n{}",
      rerendered );
    let r_skg : Graphnode =
      node_from_disk (&config, "R")?;
    assert!(
      ! r_skg . hidesFromSubs
        . or_default() . iter ()
        . any ( |m| m . member == ID::from ("H") ),
      "Expected inferred unhide to be persisted in R.skg: {:?}",
      r_skg . hidesFromSubs );

    Ok (( )) }

async fn test_collateral_view_reflects_newly_unhidden_subscribee_content (
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
      &graph . load_full (), saved_viewforest_id . clone(), viewforest, &pids);

    let (extended, collateral_views) =
      save_buffer_and_read_collateral_views (
        &add_editable_view_request_to_subscribees (&initial_view),
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
      &graph . load_full (), collateral_viewforest_id, extended_viewforest, &extended_pids);

    let edited : String =
      move_h_from_hiddenin_folder_to_visible_subscribee_content (
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
      ! collateral_views[0] . contains ("hiddenInSubscribeeFolder"),
      "Expected collateral view to remove HiddenInSubscribeeFolder:\n{}",
      collateral_views[0]);
    assert! (
      collateral_views[0] . lines() . any ( |line|
        line . starts_with ("**** (skg (node (id H)") ),
      "Expected H to be visible subscribee-as-such content in collateral view:\n{}",
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
      save_buffer_for_hidden_subscriptions_test (
        &add_editable_view_request_to_subscribees (&initial_view),
        &config, tantivy, &graph, &mut views_state
      ) . await?;
    let edited : String =
      remove_hiddenin_branch (&extended);
    let rerendered : String =
      save_buffer_for_hidden_subscriptions_test (
        &edited, &config, tantivy, &graph, &mut views_state
      ) . await?;

    assert! (
      without_viewnode_ids (&rerendered) . contains (
        "**** (skg hiddenInSubscribeeFolder)\n***** (skg folded (node (id H)"),
      "Expected H to be regenerated under HiddenInSubscribeeFolder:\n{}",
      rerendered );
    assert! (
      ! rerendered . lines() . any ( |line|
        line . starts_with ("**** (skg (node (id H)") ),
      "Expected deleting from HiddenInSubscribeeFolder not to unhide H:\n{}",
      rerendered );

    Ok (( )) }

/// Hidden without but none hidden within:
/// - R subscribes to E1, E2
/// - R hides H
/// - H is not in any subscribee's content
/// - E1 contains [E11, E12], E12 contains [E121]
/// - E2 has no content
///
/// Tests two views:
/// - Initial view from R: E1, E2 are write-protected (bare leaves), H in HiddenOutsideOfSubscribeeFolder
/// - View from R with editable views extended at each subscribee: E1 shows E11, E12 (E12 write-protected); E2 extended but empty; H still in HiddenOutsideOfSubscribeeFolder
///
/// No HiddenInSubscribeeFolder in either state (H is not in any subscribee's content).
async fn test_hidden_without_but_none_within (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let (initial_view, _pids, _)
      : (String, Vec<ID>, _)
      = single_root_view( &config, None,
                          &ID("R" . to_string()) )?;
    println!("Initial view from R:\n{}", initial_view);
    let expected_initial = indoc! {
      "* (skg (node (id R) (repo main) (rels (contains (out 1)) (subscribesTo (out 2)) (hidesFromSubs (out 1))))) R
       ** (skg subscribeeFolder)
       *** (skg (node (id E1) (repo main) writeProtected (rels (contains (out 2 (unintegrated 2))) (subscribesTo (in 1 (ancestors 2))) (birth (subscribesTo in 2))))) subscribee-1
       *** (skg (node (id E2) (repo main) writeProtected (rels (contains (out 0 (unintegrated 0))) (subscribesTo (in 1 (ancestors 2))) (birth (subscribesTo in 2))))) subscribee-2
       *** (skg hiddenOutsideOfSubscribeeFolder)
       **** (skg folded (node (id H) (repo main) writeProtected (rels (hidesFromSubs (in 1 (ancestors 3))) (birth (hidesFromSubs in 3))))) H
       ** (skg (node (id R1) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) R1
       "};
    assert_metadata_eq!(initial_view, expected_initial,
      "Initial view from R: H in HiddenOutsideOfSubscribeeFolder; E1 and E2 are writeProtected bare leaves");
    let with_subscribees_extended = {
      let modified_view : String =
        add_editable_view_request_to_subscribees (&initial_view);
      println!("Modified view (with editable requests):\n{}", modified_view);
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
    println!("View from R after save with editable view requests:\n{}",
             with_subscribees_extended);
    let expected_extended = indoc! {
      "* (skg (node (id R) (repo main) (rels (contains (out 1)) (subscribesTo (out 2)) (hidesFromSubs (out 1))))) R
       ** (skg subscribeeFolder)
       *** (skg (node (id E1) (repo main) (rels (contains (out 2 (unintegrated 2))) (subscribesTo (in 1 (ancestors 2))) (birth (subscribesTo in 2))))) subscribee-1
       **** (skg (node (id E11) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) E11
       **** (skg (node (id E12) (repo main) (rels (contains (in 1 (ancestors 1)) (out 1)) (birth (contains in 1))))) E12
       ***** (skg (node (id E121) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) E121
       *** (skg (node (id E2) (repo main) (rels (contains (out 0 (unintegrated 0))) (subscribesTo (in 1 (ancestors 2))) (birth (subscribesTo in 2))))) subscribee-2
       *** (skg hiddenOutsideOfSubscribeeFolder)
       **** (skg folded (node (id H) (repo main) writeProtected (rels (hidesFromSubs (in 1 (ancestors 3))) (birth (hidesFromSubs in 3))))) H
       ** (skg (node (id R1) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) R1
       "};
    assert_metadata_eq!(with_subscribees_extended, expected_extended,
      "View with extended subscribees: H still in HiddenOutsideOfSubscribeeFolder (at end); E1 extended with E11, E12; E2 extended but empty");

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
        line . contains ("(id H)")),
      "Expected original hidden-outside viewnode H to remain:\n{}",
      rerendered );
    assert! (
      ! rerendered . lines () . any ( |line|
        line . contains ("(id E11)") ),
      "A newly hidden child inside a collapsed subscribee should not also \
       appear through an unrequested hiddenFolder:\n{}",
      rerendered );
    let subscriber : Graphnode = node_from_disk (config, "R") ?;
    assert! (
      members_of (subscriber . hidesFromSubs . or_default ())
        . contains (&ID::from ("E11")),
      "Expected adding E11 to HiddenOutsideOfSubscribeeFolder to save a hide." );
    assert! (
      response . warnings . iter () . any ( |warning|
        warning . contains ("Saved hide for")
        && warning . contains ("HiddenInSubscribeeFolder") ),
      "Expected the post-skgsave-commit moved-inside warning: {:?}",
      response . warnings );
    let extended : String =
      save_buffer_for_hidden_subscriptions_test (
        &add_editable_view_request_to_subscribees (&rerendered),
        &config, tantivy, &graph, &mut views_state
      ) . await ?;
    assert! (
      extended . contains ("hiddenInSubscribeeFolder")
      && extended . lines () . any ( |line| line . contains ("(id E11)") ),
      "The hidden child should appear in hiddenHere once its subscribee is \
       editable:\n{}", extended );
    let without_h : String = extended . lines ()
      . filter (|line| ! line . contains ("(id H)"))
      . collect::<Vec<_>> () . join ("\n") + "\n";
    let _ : String = save_buffer_for_hidden_subscriptions_test (
      &without_h, &config, tantivy, &graph, &mut views_state
    ) . await ?;
    let subscriber_after_removal : Graphnode = node_from_disk (config, "R") ?;
    assert! (
      ! members_of (
          subscriber_after_removal . hidesFromSubs . or_default ())
        . contains (&ID::from ("H")),
      "Removing H from HiddenOutsideOfSubscribeeFolder must remove its hide." );

    Ok (( )) }

/// Overlapping hidden within:
/// - R subscribes to E1, E2
/// - R hides H
/// - Both E1 and E2 contain H (and nothing else)
///
/// Tests two views:
/// - Initial view from R: E1, E2 are write-protected (bare leaves), H doesn't appear
/// - View from R with editable views extended at each subscribee: H appears in HiddenInSubscribeeFolder under both E1 and E2
///
/// No HiddenOutsideOfSubscribeeFolder in either state (H is in subscribees' content).
async fn test_overlapping_hidden_within (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let (initial_view, _pids, _)
      : (String, Vec<ID>, _)
      = single_root_view( &config, None,
                          &ID("R" . to_string()) )?;
    println!("Initial view from R:\n{}", initial_view);
    let expected_initial = indoc! {
      "* (skg (node (id R) (repo main) (rels (contains (out 1)) (subscribesTo (out 2)) (hidesFromSubs (out 1))))) R
       ** (skg subscribeeFolder)
       *** (skg (node (id E1) (repo main) writeProtected (rels (contains (out 1 (unintegrated 0))) (subscribesTo (in 1 (ancestors 2))) (birth (subscribesTo in 2))))) subscribee-1
       *** (skg (node (id E2) (repo main) writeProtected (rels (contains (out 1 (unintegrated 0))) (subscribesTo (in 1 (ancestors 2))) (birth (subscribesTo in 2))))) subscribee-2
       ** (skg (node (id R1) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) R1
       "};
    assert_metadata_eq!(initial_view, expected_initial,
      "Initial view from R: write-protected subscribees are bare leaves; H doesn't appear");
    let extended = {
      let modified_view : String =
        add_editable_view_request_to_subscribees (&initial_view);
      println!("Modified view (with editable requests):\n{}",
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
    println!("View from R after save with editable view requests:\n{}", extended);
    let expected_extended = indoc! {
      "* (skg (node (id R) (repo main) (rels (contains (out 1)) (subscribesTo (out 2)) (hidesFromSubs (out 1))))) R
       ** (skg subscribeeFolder)
       *** (skg (node (id E1) (repo main) (rels (contains (out 1 (unintegrated 0))) (subscribesTo (in 1 (ancestors 2))) (birth (subscribesTo in 2))))) subscribee-1
       **** (skg hiddenInSubscribeeFolder)
       ***** (skg folded (node (id H) (repo main) writeProtected (rels (contains (in 2 (ancestors 2))) (hidesFromSubs (in 1 (ancestors 4))) (birth (contains in 2) (hidesFromSubs in 4))))) H
       ****** (skg (node (id E1) (repo main) nonMember writeProtected (rels (contains (out 1 (ancestors 1))) (subscribesTo (in 1))) (viewStats cycle))) subscribee-1
       ****** (skg (node (id E2) (repo main) nonMember writeProtected (rels (contains (out 1 (ancestors 1))) (subscribesTo (in 1))))) subscribee-2
       *** (skg (node (id E2) (repo main) (rels (contains (out 1 (unintegrated 0))) (subscribesTo (in 1 (ancestors 2))) (birth (subscribesTo in 2))))) subscribee-2
       **** (skg hiddenInSubscribeeFolder)
       ***** (skg folded (node (id H) (repo main) writeProtected (rels (contains (in 2 (ancestors 2))) (hidesFromSubs (in 1 (ancestors 4))) (birth (contains in 2) (hidesFromSubs in 4))))) H
       ****** (skg (node (id E1) (repo main) nonMember writeProtected (rels (contains (out 1 (ancestors 1))) (subscribesTo (in 1))))) subscribee-1
       ****** (skg (node (id E2) (repo main) nonMember writeProtected (rels (contains (out 1 (ancestors 1))) (subscribesTo (in 1))) (viewStats cycle))) subscribee-2
       ** (skg (node (id R1) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) R1
       "};
    assert_metadata_eq!(extended, expected_extended,
      "View with extended subscribees: H appears in HiddenInSubscribeeFolder under both E1 and E2");
    Ok (( )) }
