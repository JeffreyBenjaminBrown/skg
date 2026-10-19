// A buffer of the total diff view follows its fresh sets after a
// save ('TODO/total-diff-view/4_plan-fixes.org', "The view follows its
// fresh sets after a save"). The 'fresh' fixture (one skgrepo, main):
// r contains a, whose body changed, and w, unchanged; w contains w1
// and w2; w1 contains w1a and subscribes to w2. s, unchanged,
// contains s1. p and q are new, and contain each other. So the
// buffer's roots are q and r, and w is a write-protected leaf.

use crate::pcsvf::common::{
  Fixture, open_all_changes_buffer, rerendered_view_content, save,
  send_stage_and_commit_request, with_fixture};
use crate::pcsvf::common::git_fixture_helpers::mk_test_tcp_stream_pair;

use skg::serve::handlers::save_buffer::SaveResponse;
use skg::test_utils::{skg_env_from_parts, update_from_and_rerender_buffer_test};
use skg::to_org::render::content_view::multi_root_view;
use skg::types::env::SkgEnv;
use skg::types::misc::ID;
use skg::types::views_state::ViewId;
use skg::types::viewnode::Viewnode;

use ego_tree::Tree;
use futures::executor::block_on;
use std::error::Error;
use std::net::TcpStream;

/// Save 'buffer' as the "all changes" buffer; it must save without
/// errors. Returns the response.
fn save_cleanly (
  f      : &mut Fixture,
  buffer : &str,
) -> Result<SaveResponse, Box<dyn Error>> {
  let response : SaveResponse = save (f, buffer) ?;
  assert! ( response . errors . is_empty (),
            "the buffer must save without errors, got {:?}\n\nbuffer:\n{}",
            response . errors, buffer );
  Ok (response) }

/// Retitle 'node' to 'title' by saving a content view of it, as
/// another buffer would.
fn retitle_in_a_content_view (
  f     : &mut Fixture,
  node  : &str,
  title : &str,
) -> Result<(), Box<dyn Error>> {
  let view_id : ViewId =
    ViewId::ContentView ( format! ("content-view-of-{}", node) );
  let (text, pids, tree) : (String, Vec<ID>, Tree<Viewnode>) =
    multi_root_view ( &f . config, Some (&f . tantivy), &[ ID::from (node) ] ) ?;
  f . views_state . open_views . register_view (
    &f . graph . load_full (), view_id . clone (), tree, &pids );
  let root_line : &str = text . lines () . next () . unwrap ();
  let edited : String = text . replacen (
    root_line,
    &format! ( "{} {}", &root_line [ .. root_line . rfind (' ') . unwrap () ],
               title ),
    1 );
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response : SaveResponse = block_on (
    update_from_and_rerender_buffer_test (
      &mut stream, &edited, &f . config, &f . tantivy, &f . graph,
      &Ok (view_id), &mut f . views_state )) ?;
  assert! ( response . errors . is_empty (),
            "the content view must save, got {:?}", response . errors );
  Ok (( )) }

/// The lines of 'buffer' that draw an unrestricted vognode of 'node'.
fn node_lines<'a> (
  buffer : &'a str,
  node   : &str,
) -> Vec<&'a str> {
  let atom : String = format! ("(node (id {}) ", node);
  buffer . lines () . filter ( |line| line . contains (&atom) ) . collect () }

/// Whether some line of 'buffer' draws 'node' editable.
fn drawn_editable (
  buffer : &str,
  node   : &str,
) -> bool {
  node_lines (buffer, node) . iter ()
    . any ( |line| ! line . contains ("writeProtected") ) }

/// The IDs of the buffer's roots, in order.
fn root_ids (
  buffer : &str,
) -> Vec<String> {
  buffer . lines () . filter ( |line| line . starts_with ("* ") )
    . map ( |line| {
      let start : usize = line . find ("(id ") . unwrap () + 4;
      line [start ..] . split (')') . next () . unwrap () . to_string () } )
    . collect () }

/// The subtree of 'buffer' under its one line starting 'prefix', that
/// line included.
fn subtree_starting<'a> (
  buffer : &'a str,
  prefix : &str,
) -> Vec<&'a str> {
  let lines : Vec<&str> = buffer . lines () . collect ();
  let start : usize = lines . iter ()
    . position ( |line| line . starts_with (prefix) )
    . unwrap_or_else ( || panic! ( "no line starts {:?}:\n{}", prefix, buffer ));
  let depth = |line : &str| line . chars () . take_while ( |c| *c == '*' ) . count ();
  let own_depth : usize = depth (lines [start]);
  std::iter::once (lines [start])
    . chain ( lines [ start + 1 .. ] . iter () . copied ()
              . take_while ( |line| ! line . starts_with ('*')
                             || depth (line) > own_depth ))
    . collect () }

/// w changes after the buffer opened, while the buffer draws it only
/// as a write-protected leaf. After the next save it is editable,
/// with its change drawn, and its members, outside U, stay
/// write-protected. Its origin r is drawn already, so no root is
/// added, and no change is unshown.
#[test]
fn a_write_protected_leaf_that_changes_becomes_editable (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("fresh", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    assert! ( ! drawn_editable (&buffer, "w"), "{}", buffer );
    retitle_in_a_content_view (f, "w", "w retitled") ?;
    let response : SaveResponse = save_cleanly (f, &buffer) ?;
    let saved : &str = &response . saved_view;
    assert! ( drawn_editable (saved, "w"), "w must be editable:\n{}", saved );
    assert! ( saved . contains ("*** (skg (oldText title)) w"),
              "w's old title must be drawn:\n{}", saved );
    for member in [ "w1", "w2" ] {
      assert! ( ! node_lines (saved, member) . is_empty ()
                && ! drawn_editable (saved, member),
                "{} must be a write-protected leaf:\n{}", member, saved ); }
    assert_eq! ( root_ids (saved), vec! [ "q", "r" ] );
    assert_eq! ( response . readiness_summary . unwrap () . unshown_changes, 0 );
    Ok (( )) }) }

/// w1a, two levels under the write-protected leaf w, changes. After
/// the next save, w and w1 are editable and w1a's change is drawn
/// under them; w2 stays write-protected.
#[test]
fn a_change_two_levels_under_a_write_protected_leaf_is_drawn (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("fresh", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    assert! ( node_lines (&buffer, "w1a") . is_empty (), "{}", buffer );
    retitle_in_a_content_view (f, "w1a", "w1a retitled") ?;
    let response : SaveResponse = save_cleanly (f, &buffer) ?;
    let saved : &str = &response . saved_view;
    for node in [ "w", "w1", "w1a" ] {
      assert! ( drawn_editable (saved, node),
                "{} must be editable:\n{}", node, saved ); }
    assert! ( ! drawn_editable (saved, "w2"), "{}", saved );
    assert! ( saved . contains ("***** (skg (oldText title)) w1a"),
              "w1a's old title must be drawn under w1:\n{}", saved );
    assert_eq! ( response . readiness_summary . unwrap () . unshown_changes, 0 );
    Ok (( )) }) }

/// s1 changes, under s, an origin the buffer does not draw. The next
/// save appends s as a root, after the standing ones, drawn as a
/// first drawing draws a context root, with s1's change under it; a
/// further save appends nothing more.
#[test]
fn a_change_under_a_new_origin_appends_the_origin_as_a_root (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("fresh", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    retitle_in_a_content_view (f, "s1", "s1 retitled") ?;
    let response : SaveResponse = save_cleanly (f, &buffer) ?;
    let saved : String = response . saved_view;
    assert_eq! ( root_ids (&saved), vec! [ "q", "r", "s" ], "{}", saved );
    let s_subtree : Vec<&str> = subtree_starting (&saved, "* (skg bodyFolded (node (id s) ");
    assert! ( s_subtree . iter () . any ( |line|
                line . starts_with ("** (skg (node (id s1) ")
                && ! line . contains ("writeProtected") )
              && s_subtree . contains (&"*** (skg (oldText title)) s1"),
              "s1's change must be drawn under s:\n{}", saved );
    assert_eq! ( response . readiness_summary . unwrap () . unshown_changes, 0 );
    let again : SaveResponse = save_cleanly (f, &saved) ?;
    assert_eq! ( again . saved_view, saved, "a further save adds nothing" );
    Ok (( )) }) }

/// The new cycle of p and q gains a member: q now contains z, which
/// contains p. The cycle's root q stands, so no second root is
/// added for the cycle.
#[test]
fn a_changed_cycle_membership_appends_no_second_root (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("fresh", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let edited : String = {
      // Put "** z" in the place of q's editable member p, and p's
      // subtree under z.
      let lines : Vec<&str> = buffer . lines () . collect ();
      let at : usize = lines . iter () . position ( |line|
        line . starts_with ("** (skg (node (id p) ")
        && ! line . contains ("writeProtected") ) . unwrap ();
      let end : usize = at + 1 + lines [ at + 1 .. ] . iter ()
        . take_while ( |line| ! line . starts_with ("* ")
                              && ! line . starts_with ("** ") ) . count ();
      lines [.. at] . iter () . map ( |line| line . to_string () )
        . chain ( std::iter::once ( "** z" . to_string () ))
        . chain ( lines [ at .. end ] . iter () . map ( |line|
          if line . starts_with ('*') { format! ("*{}", line) }
          else { line . to_string () } ))
        . chain ( lines [end ..] . iter () . map ( |line| line . to_string () ))
        . collect::<Vec<String>> () . join ("\n") + "\n" };
    let response : SaveResponse = save_cleanly (f, &edited) ?;
    let saved : String = response . saved_view;
    assert! ( saved . lines () . any ( |line|
                line . starts_with ("** (skg (node (id ") && line . ends_with (") z") ),
              "z must be drawn under q:\n{}", saved );
    assert_eq! ( root_ids (&saved), vec! [ "q", "r" ], "{}", saved );
    let again : SaveResponse = save_cleanly (f, &saved) ?;
    assert_eq! ( root_ids (&again . saved_view), vec! [ "q", "r" ] );
    Ok (( )) }) }

/// An editable-view request on w, a write-protected member outside
/// U, cascades through its content as in an ordinary view, until the
/// node budget (2 here) is spent: w and w1 expand; w2 and w1a stay
/// write-protected. No default folder is added (w1 subscribes to w2),
/// and the nodes made editable stay so after the next save.
#[test]
fn an_editable_view_request_cascades_within_the_node_budget (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("fresh", &["main"], |f| {
    f . config . initial_node_limit = 2;
    let buffer : String = open_all_changes_buffer (f) ?;
    let requested : String = buffer . replacen (
      "(node (id w) (repo main) writeProtected",
      "(node (id w) (repo main) writeProtected (viewRequests editableView)", 1 );
    assert_ne! ( requested, buffer );
    let saved : String = save_cleanly (f, &requested) ? . saved_view;
    for node in [ "w", "w1" ] {
      assert! ( drawn_editable (&saved, node),
                "{} must be editable:\n{}", node, saved ); }
    for node in [ "w2", "w1a" ] {
      assert! ( ! node_lines (&saved, node) . is_empty ()
                && ! drawn_editable (&saved, node),
                "{} must be write-protected:\n{}", node, saved ); }
    assert! ( ! saved . contains ("subscribeeFolder"),
              "no default folder is added:\n{}", saved );
    assert! ( ! saved . contains ("viewRequests"), "{}", saved );
    let again : String = save_cleanly (f, &saved) ? . saved_view;
    assert_eq! ( again, saved, "the nodes made editable stay editable" );
    Ok (( )) }) }

/// U (a, p, q and r) exceeds the node budget, here 1; the buffer is
/// drawn in full regardless, when opened and after a save.
#[test]
fn a_buffer_whose_u_exceeds_the_node_budget_is_drawn_in_full (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("fresh", &["main"], |f| {
    f . config . initial_node_limit = 1;
    let buffer : String = open_all_changes_buffer (f) ?;
    let saved : String = save_cleanly (f, &buffer) ? . saved_view;
    for drawn in [ &buffer, &saved ] {
      for node in [ "a", "p", "q", "r" ] {
        assert! ( drawn_editable (drawn, node),
                  "{} must be editable:\n{}", node, drawn ); }
      assert! ( drawn . contains ("*** (skg (oldText body))"),
                "a's change must be drawn:\n{}", drawn ); }
    assert_eq! ( saved, buffer );
    Ok (( )) }) }

/// After a commit, U is empty, but the lingering nodes the buffer drew
/// editable stay editable.
#[test]
fn after_a_commit_the_lingering_nodes_remain_editable (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("fresh", &["main"], |f| {
    let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
    let buffer : String = open_all_changes_buffer (f) ?;
    let response : String = send_stage_and_commit_request ( f, &env,
      "((request . \"stage and commit\") (message . \"all\") (confirmed . t))\n" ) ?;
    assert! ( response . contains ("(commits ((main "), "{}", response );
    let content : String = rerendered_view_content (&response);
    assert_eq! ( root_ids (&content), root_ids (&buffer) );
    for node in [ "a", "p", "q", "r" ] {
      assert! ( drawn_editable (&content, node),
                "{} must stay editable:\n{}", node, content ); }
    assert! ( ! content . contains ("(diff "), "{}", content );
    Ok (( )) }) }
