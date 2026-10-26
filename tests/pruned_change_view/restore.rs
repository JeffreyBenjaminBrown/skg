// The diff view's two restore requests ('docs/diff-view.org',
// "Restoring what a change removed"): '(editRequest restoreRel)' on a
// removed membership, '(editRequest restoreNode)' on a
// ghost.
//
// In the 'removed_partners' fixture b, d and e drop x or c from their
// content and s drops its subscribee s2. Its "all changes" buffer holds
// b with x as an editable removed member, d with c likewise, and under
// c's container folder d as a '~-' container; and s2 with s, in its
// subscriber folder, as a '~-' subscriber.
//
// In the 'restore' fixture g, deleted since HEAD, was contained by q
// (between q1 and q2), subscribed to by t, and contained gc and gd; gd
// is deleted too, and r dropped gc from its content. Its buffer holds
// q with g as a ghost (gc and gd under it), r with gc as
// a removed member (g, historical, in gc's container folder), and t
// with g in its subscribee folder.

use skg::metadata_canon::without_viewnode_ids;
use crate::pruned_change_view::common::{
  assert_lines_in_order, open_all_changes_buffer, save, saved_and_rerendered, with_fixture};
use crate::git_fixture::{edit_line, skg_files};

use skg::serve::handlers::save_buffer::SaveResponse;
use skg::types::errors::SaveError;
use skg::types::save::format_save_error_as_org;
use skg::serve::parse_metadata_sexp::{
  RESTORE_NODE_ON_A_NODE_NOT_DELETED, RESTORE_REL_ON_A_DELETED_NODE,
  RESTORE_REL_WITHOUT_A_REMOVED_MEMBERSHIP};

use std::collections::BTreeMap;
use std::error::Error;

const X_REMOVED_FROM_B : &str =
  "** (skg node (gnid x) (skgRepos (nodeRepo main)) noClaim (view (rels";
const D_REMOVED_AS_CS_CONTAINER : &str =
  "**** (skg node (gnid d) (skgRepos (nodeRepo main)) writeProtected (view ghostWarrant cycle)";
const S_REMOVED_AS_S2S_SUBSCRIBER : &str =
  "*** (skg node (gnid s) (skgRepos (nodeRepo main)) writeProtected";
/// d, a node of the containerward chain under c, a member of x's
/// containers folder.
const D_IN_THE_CHAIN_UNDER_C : &str =
  "***** (skg node (gnid d) (skgRepos (nodeRepo main)) writeProtected (view ghostWarrant)";
const G_UNDER_Q : &str =
  "** (skg ghost (gnid g)";
const GC_UNDER_G : &str =
  "*** (skg node (gnid gc) (skgRepos (nodeRepo main)) writeProtected";
const GC_REMOVED_FROM_R : &str =
  "** (skg node (gnid gc) (skgRepos (nodeRepo main)) noClaim (view ghostWarrant) (diff";

/// g's file at HEAD, as the save writes a node.
const G_AT_HEAD : &str =
  "title: g\npid: g\nbody: |2-\n  what g said\ncontains:\n- gc\n- gd\n";

/// The line, viewnode ID aside, with 'request' added to its node or
/// historical form, before that form's '(diff ...)'.
fn with_request (
  request : &str,
) -> impl Fn (&str) -> String + '_ {
  move |line : &str| line . replacen (
    " (diff ", &format! (" (editRequest {}) (diff ", request), 1 ) }

/// 'buffer' with 'request' on its one line starting 'prefix'.
fn requesting (
  buffer  : &str,
  prefix  : &str,
  request : &str,
) -> String {
  edit_line (buffer, prefix, with_request (request)) }

/// The file of 'node' in the fixture's skgrepo 'main'.
fn file_of (
  files : &BTreeMap<String, String>,
  node  : &str,
) -> String {
  files . get ( &format! ("main/{}.skg", node) ) . cloned ()
    . unwrap_or_else ( || panic! ( "no file of {} among {:?}", node, files . keys () )) }

/// Save 'buffer', which must save without errors; the save, then the
/// fixture's files.
fn saved (
  f      : &mut crate::git_fixture::Fixture,
  buffer : &str,
) -> Result<(SaveResponse, BTreeMap<String, String>), Box<dyn Error>> {
  let response : SaveResponse = save (f, buffer) ?;
  assert! ( response . errors . is_empty (),
            "the save must succeed, got {:?}\n{}", response . errors, buffer );
  Ok (( response, skg_files (f) ? )) }

#[test]
fn restoring_a_removed_content_member () -> Result<(), Box<dyn Error>> {
  with_fixture ("removed_partners", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let (response, files) : (SaveResponse, BTreeMap<String, String>) = saved ( f,
      &requesting (&buffer, X_REMOVED_FROM_B, "restoreRel") ) ?;
    assert_eq! ( file_of (&files, "b"), "title: b\npid: b\ncontains:\n- x\n" );
    assert! ( response . warnings . is_empty (), "{:?}", response . warnings );
    assert! ( ! response . saved_view . contains ("restoreRel"),
              "the request is consumed:\n{}", response . saved_view );
    assert_lines_in_order ( &response . saved_view, &[
      // b no longer differs from HEAD, and x is its member again
      "* (skg node (gnid b) (skgRepos (nodeRepo main)) (view (rels (contains (out 1))))) b",
      "** (skg node (gnid x) (skgRepos (nodeRepo main)) (view (rels (contains (in 3 (ancestors 1)) (out 1))) (birth (contains in 1)))) x" ] );
    saved_and_rerendered (f, &response . saved_view) ?;
    Ok (( )) }) }

/// A '~-' container in a container folder: the relationship is the
/// member's content relationship to the folder's viewparent, recorded
/// by the member.
#[test]
fn restoring_a_removed_container_from_a_folder () -> Result<(), Box<dyn Error>> {
  with_fixture ("removed_partners", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let (response, files) : (SaveResponse, BTreeMap<String, String>) = saved ( f,
      &requesting (&buffer, D_REMOVED_AS_CS_CONTAINER, "restoreRel") ) ?;
    assert_eq! ( file_of (&files, "d"), "title: d\npid: d\ncontains:\n- c\n" );
    assert! ( response . warnings . is_empty (), "{:?}", response . warnings );
    saved_and_rerendered (f, &response . saved_view) ?;
    Ok (( )) }) }

/// A removed subscription, from the subscriber folder of the
/// subscribee: it returns to its index at HEAD in the subscriber's list.
#[test]
fn restoring_a_removed_subscription_from_a_subscriber_folder (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("removed_partners", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let (response, files) : (SaveResponse, BTreeMap<String, String>) = saved ( f,
      &requesting (&buffer, S_REMOVED_AS_S2S_SUBSCRIBER, "restoreRel") ) ?;
    assert_eq! ( file_of (&files, "s"),
                 "title: s\npid: s\nsubscribesTo:\n- s1\n- s2\n- s3\n" );
    assert! ( response . warnings . is_empty (), "{:?}", response . warnings );
    saved_and_rerendered (f, &response . saved_view) ?;
    Ok (( )) }) }

/// The node's file is written back as HEAD had it, its out-link to
/// gd (still deleted) included; its container and its subscriber list
/// it again, q at its index at HEAD. g's list names gd, which HEAD
/// has, so gd is its ghost there, claiming membership: a later save of
/// the buffer keeps gd in g's list, as it would an unknown's.
#[test]
fn restoring_a_deleted_node () -> Result<(), Box<dyn Error>> {
  with_fixture ("restore", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let (response, files) : (SaveResponse, BTreeMap<String, String>) = saved ( f,
      &requesting (&buffer, G_UNDER_Q, "restoreNode") ) ?;
    assert_eq! ( file_of (&files, "g"),
                 G_AT_HEAD );
    assert_eq! ( file_of (&files, "q"),
                 "title: q\npid: q\ncontains:\n- q1\n- g\n- q2\n" );
    assert_eq! ( file_of (&files, "t"), "title: t\npid: t\nsubscribesTo:\n- g\n" );
    assert! ( ! files . contains_key ("main/gd.skg") );
    assert! ( response . warnings . is_empty (), "{:?}", response . warnings );
    let rerendered : &str = &response . saved_view;
    assert! ( ! rerendered . contains ("ghost (gnid g)"),
              "g is no longer deleted:\n{}", rerendered );
    assert! ( ! rerendered . contains ("restoreNode"), "{}", rerendered );
    assert! ( without_viewnode_ids (rerendered) . contains (
                "*** (skg ghost (gnid gd) (skgRepos (nodeRepo main)) (diff (node removed)" ),
              "g's out-link to gd, still deleted, shows as gd's ghost, claiming:\n{}",
              rerendered );
    saved_and_rerendered (f, rerendered) ?;
    assert_eq! ( file_of (&skg_files (f) ?, "g"), G_AT_HEAD,
                 "the saves keep gd in g's list" );
    Ok (( )) }) }

/// Restoring g and, in the same save, gc's membership in g (which g's
/// restored file carries) and in r.
#[test]
fn both_requests_in_one_save () -> Result<(), Box<dyn Error>> {
  with_fixture ("restore", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let buffer : String = requesting (&buffer, G_UNDER_Q, "restoreNode");
    let buffer : String = requesting (&buffer, GC_UNDER_G, "restoreRel");
    let buffer : String = requesting (&buffer, GC_REMOVED_FROM_R, "restoreRel");
    let (response, files) : (SaveResponse, BTreeMap<String, String>) = saved (f, &buffer) ?;
    assert_eq! ( file_of (&files, "r"), "title: r\npid: r\ncontains:\n- gc\n" );
    assert_eq! ( file_of (&files, "g"),
                 G_AT_HEAD );
    assert! ( response . warnings . is_empty (), "{:?}", response . warnings );
    Ok (( )) }) }

/// A relationship whose recorder is still deleted is not restored.
#[test]
fn a_membership_in_a_deleted_node_is_dropped_with_a_warning (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("restore", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let files_before : BTreeMap<String, String> = skg_files (f) ?;
    let (response, files) : (SaveResponse, BTreeMap<String, String>) = saved ( f,
      &requesting (&buffer, GC_UNDER_G, "restoreRel") ) ?;
    assert_eq! ( files, files_before );
    assert_eq! ( response . warnings . len (), 1, "{:?}", response . warnings );
    assert! ( response . warnings [0] . starts_with ("A restoreRel request was dropped")
              && response . warnings [0] . contains ("deleted node at one end")
              && response . warnings [0] . contains (
                "\n*** gc\n- graphnode ID: gc\n- viewnode ID: "),
              "{:?}", response . warnings );
    Ok (( )) }) }

/// Each request on the wrong kind of headline stops the save with the
/// parser's message, naming the headline with its viewnode ID.
#[test]
fn restore_requests_on_the_wrong_kind_stop_the_save () -> Result<(), Box<dyn Error>> {
  with_fixture ("restore", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let files_before : BTreeMap<String, String> = skg_files (f) ?;
    for (prefix, request, message) in [
      ( G_UNDER_Q, "restoreRel", RESTORE_REL_ON_A_DELETED_NODE ),
      ( GC_REMOVED_FROM_R, "restoreNode", RESTORE_NODE_ON_A_NODE_NOT_DELETED ),
      ( "* (skg node (gnid q) ", "restoreRel", RESTORE_REL_WITHOUT_A_REMOVED_MEMBERSHIP ), ] {
      let refusal : String = match save ( f, &requesting (&buffer, prefix, request) ) {
        Err (e) => match e . downcast_ref::<SaveError> () {
          Some (save_error) => format_save_error_as_org (save_error),
          None => e . to_string (), },
        Ok (response) => format! ( "{:?}", response . errors ), };
      assert! ( refusal . contains (message)
                && refusal . contains ("- viewnode ID: ")
                && refusal . contains ("To find it: C-c g v "),
                "{} on {:?} should be refused with {:?}, got {}",
                request, prefix, message, refusal );
      assert_eq! ( skg_files (f) ?, files_before ); }
    Ok (( )) }) }

/// In the 'subscribee_member' fixture, p hangs under sub, the added
/// member of s1's changed subscriber folder, as sub's containerward
/// chain. A restoreRel with '(rel removed)' typed beside it there is
/// refused, as on a chain node under a containers folder's member.
#[test]
fn restoreRel_is_refused_on_a_chain_node_under_a_subscriber_folders_member (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("subscribee_member", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let files_before : BTreeMap<String, String> = skg_files (f) ?;
    let requested : String = edit_line (
      &buffer,
      "***** (skg node (gnid p) (skgRepos (nodeRepo main)) writeProtected",
      |line : &str| line . replacen (
        "writeProtected", "writeProtected (editRequest restoreRel) (diff (rel removed))", 1 ));
    assert_ne! ( requested, buffer );
    let refusal : String = match save (f, &requested) {
      Err (e) => match e . downcast_ref::<SaveError> () {
        Some (save_error) => format_save_error_as_org (save_error),
        None => e . to_string (), },
      Ok (response) => format! ( "{:?}", response . errors ), };
    assert! ( refusal . contains (RESTORE_REL_WITHOUT_A_REMOVED_MEMBERSHIP),
              "a restoreRel on a chain node should be refused, got {}", refusal );
    assert_eq! ( skg_files (f) ?, files_before );
    Ok (( )) }) }

/// A restoreRel stands where it may by position: on a node of the
/// containerward chain (d, under c in x's containers folder) it is
/// refused, with or without a '(rel removed)' typed beside it, while
/// on a containers folder's member carrying '(rel removed)' (d in c's
/// containers folder) it saves.
#[test]
fn restoreRel_is_refused_on_a_chain_node_and_taken_by_a_folder_member (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("removed_partners", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let files_before : BTreeMap<String, String> = skg_files (f) ?;
    for parts in [ "(editRequest restoreRel) (diff (rel removed))",
                   "(editRequest restoreRel)" ] {
      let requested : String = edit_line (
        &buffer, D_IN_THE_CHAIN_UNDER_C,
        |line : &str| line . replacen (
          "(view ghostWarrant))", &format! ("{} (view ghostWarrant))", parts), 1 ));
      assert_ne! ( requested, buffer );
      let refusal : String = match save (f, &requested) {
        Err (e) => match e . downcast_ref::<SaveError> () {
          Some (save_error) => format_save_error_as_org (save_error),
          None => e . to_string (), },
        Ok (response) => format! ( "{:?}", response . errors ), };
      assert! ( refusal . contains (RESTORE_REL_WITHOUT_A_REMOVED_MEMBERSHIP)
                && refusal . contains ("viewnode ID"),
                "a restoreRel with {} on a chain node should be refused, got {}",
                parts, refusal );
      assert_eq! ( skg_files (f) ?, files_before ); }
    let (_response, files) : (SaveResponse, BTreeMap<String, String>) = saved ( f,
      &requesting (&buffer, D_REMOVED_AS_CS_CONTAINER, "restoreRel") ) ?;
    assert_eq! ( file_of (&files, "d"), "title: d\npid: d\ncontains:\n- c\n" );
    Ok (( )) }) }
