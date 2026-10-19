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

/// 'buffer' with an editable-view request at the first occurrence of
/// 'metadata', which must draw a write-protected node.
fn requesting_editable_at (
  buffer   : &str,
  metadata : &str,
) -> String {
  let at : usize = buffer . find (metadata)
    . unwrap_or_else ( || panic! ( "no {:?} in:\n{}", metadata, buffer ));
  format! ( "{}{}", &buffer [.. at],
            buffer [at ..] . replacen (
              "writeProtected", "writeProtected (viewRequests editableView)", 1 )) }

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

/// The 'shared' fixture (one skgrepo, main): x and y, whose bodies
/// changed, each contain n, which contains a, whose body changed, and
/// b, unchanged. So x and y are the buffer's roots, and n is drawn
/// editable under x and write-protected under y. An editable-view
/// request on n under y makes that occurrence editable and the one
/// under x write-protected, with its content; a, a node of U under
/// it, is editable again under y, its change drawn there. A further
/// save changes nothing.
#[test]
fn an_editable_view_request_moves_editability_between_occurrences (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("shared", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let n_under_y : &str = "** (skg (node (id n) (repo main) writeProtected ";
    assert! ( subtree_starting (&buffer, "* (skg (node (id y) ") . iter ()
                . any ( |line| line . starts_with (n_under_y) ),
              "n must be write-protected under y:\n{}", buffer );
    let requested : String = requesting_editable_at (&buffer, n_under_y);
    let saved : String = save_cleanly (f, &requested) ? . saved_view;
    let x_subtree : Vec<&str> = subtree_starting (&saved, "* (skg (node (id x) ");
    let y_subtree : Vec<&str> = subtree_starting (&saved, "* (skg (node (id y) ");
    for (subtree, editable) in [ (&x_subtree, false), (&y_subtree, true) ] {
      for node in [ "n", "a" ] {
        let atom : String = format! ("(node (id {}) ", node);
        let line : &str = subtree . iter ()
          . find ( |line| line . contains (&atom) )
          . unwrap_or_else ( || panic! ( "{} must be drawn:\n{}", node, saved ));
        assert_eq! ( ! line . contains ("writeProtected"), editable,
                     "{}:\n{}", node, saved ); }}
    assert! ( y_subtree . contains (&"**** (skg (oldText body))"),
              "a's change must be drawn under y:\n{}", saved );
    assert! ( ! saved . contains ("viewRequests"), "{}", saved );
    let again : String = save_cleanly (f, &saved) ? . saved_view;
    assert_eq! ( again, saved, "the editability moved stays where it is" );
    Ok (( )) }) }

/// In the 'shared' fixture, once editability moved from n under x to
/// n under y, a request on n under x moves it back. The children n
/// kept drawn under x are reconciled, not redrawn: the same members,
/// in the same order, and b, folded there, stays folded. a, a node of
/// U, is editable under x again, its change drawn there, and n and a
/// are write-protected under y. A further save changes nothing.
#[test]
fn an_editable_view_request_moves_editability_back (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("shared", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let moved : String = save_cleanly (
      f, &requesting_editable_at (
        &buffer, "** (skg (node (id n) (repo main) writeProtected " )) ?
      . saved_view;
    let n_under_x : &str = "(node (id n) (repo main) writeProtected ";
    let members_of_n_under_x = |buffer : &str| -> Vec<String> {
      subtree_starting (buffer, "* (skg (node (id x) ") . iter ()
        . filter ( |line| line . starts_with ("*** ") )
        . map ( |line| [ "writeProtected ", "omittedBody ", "bodyFolded ", "folded " ]
                         . iter () . fold ( line . to_string (),
                                            |line, atom| line . replace (atom, "") ))
        . collect () };
    let b_under_x : &str = "(skg (node (id b) (repo main) writeProtected ";
    assert! ( moved . contains (n_under_x) && moved . contains (b_under_x),
              "n and b must be write-protected under x:\n{}", moved );
    let requested : String = requesting_editable_at (
      &moved . replacen ( b_under_x,
                          "(skg folded (node (id b) (repo main) writeProtected ",
                          1 ),
      n_under_x );
    let saved : String = save_cleanly (f, &requested) ? . saved_view;
    let x_subtree : Vec<&str> = subtree_starting (&saved, "* (skg (node (id x) ");
    let y_subtree : Vec<&str> = subtree_starting (&saved, "* (skg (node (id y) ");
    for (subtree, editable) in [ (&x_subtree, true), (&y_subtree, false) ] {
      for node in [ "n", "a" ] {
        let atom : String = format! ("(node (id {}) ", node);
        let line : &str = subtree . iter ()
          . find ( |line| line . contains (&atom) )
          . unwrap_or_else ( || panic! ( "{} must be drawn:\n{}", node, saved ));
        assert_eq! ( ! line . contains ("writeProtected"), editable,
                     "{}:\n{}", node, saved ); }}
    assert_eq! ( members_of_n_under_x (&saved)
                   . iter () . filter ( |line| line . contains ("(node ") )
                   . cloned () . collect::<Vec<String>> (),
                 members_of_n_under_x (&moved)
                   . iter () . filter ( |line| line . contains ("(node ") )
                   . cloned () . collect::<Vec<String>> (),
                 "n's members under x are kept:\n{}", saved );
    assert! ( x_subtree . iter ()
                . any ( |line| line . starts_with ("*** (skg folded ")
                               && line . contains ("(node (id b) ") ),
              "b stays folded under x:\n{}", saved );
    assert! ( x_subtree . contains (&"**** (skg (oldText body))"),
              "a's change must be drawn under x:\n{}", saved );
    assert! ( ! saved . contains ("viewRequests"), "{}", saved );
    let again : String = save_cleanly (f, &saved) ? . saved_view;
    assert_eq! ( again, saved, "the editability moved back stays where it is" );
    Ok (( )) }) }

/// An editable-view request on r, an editable node of U, cascades
/// through its content as in an ordinary view: its members outside U
/// expand until the node budget (2 here) is spent, so w and w1 become
/// editable while w2 and w1a stay write-protected. r and a, in U,
/// are not charged; were either charged, w1 would stay
/// write-protected. A further save changes nothing.
#[test]
fn an_editable_view_request_on_a_node_of_u_unfolds_its_members (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("fresh", &["main"], |f| {
    f . config . initial_node_limit = 2;
    let buffer : String = open_all_changes_buffer (f) ?;
    let requested : String = buffer . replacen (
      "* (skg bodyFolded (node (id r) (repo main) ",
      "* (skg bodyFolded (node (id r) (repo main) (viewRequests editableView) ", 1 );
    assert_ne! ( requested, buffer );
    let saved : String = save_cleanly (f, &requested) ? . saved_view;
    for node in [ "r", "a", "w", "w1" ] {
      assert! ( drawn_editable (&saved, node),
                "{} must be editable:\n{}", node, saved ); }
    for node in [ "w2", "w1a" ] {
      assert! ( ! node_lines (&saved, node) . is_empty ()
                && ! drawn_editable (&saved, node),
                "{} must be write-protected:\n{}", node, saved ); }
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

/// The 'folder_member' fixture (one skgrepo, main): m contains a,
/// whose body changed; c1 and c2, unchanged, each contain m. So m is
/// the buffer's one root, and c1 and c2 are drawn only as members of
/// its container folder. c1 changes after the buffer opened. After
/// the next save it is editable there, with its content list (m)
/// drawn under it and its change shown; no root is appended, and a
/// further save changes nothing.
#[test]
fn a_change_drawn_only_in_a_container_folder_becomes_editable_there (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("folder_member", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    assert! ( ! drawn_editable (&buffer, "c1"), "{}", buffer );
    retitle_in_a_content_view (f, "c1", "c1 retitled") ?;
    let response : SaveResponse = save_cleanly (f, &buffer) ?;
    let saved : String = response . saved_view;
    let c1_subtree : Vec<&str> = subtree_starting (&saved, "*** (skg (node (id c1) ");
    assert! ( ! c1_subtree [0] . contains ("writeProtected"),
              "c1 must be editable in the container folder:\n{}", saved );
    assert! ( c1_subtree . contains (&"**** (skg (oldText title)) c1")
              && c1_subtree . iter () . any ( |line|
                   line . starts_with ("**** (skg (node (id m) ") ),
              "c1's change and content must be drawn under it:\n{}", saved );
    assert_eq! ( root_ids (&saved), vec! [ "m" ], "{}", saved );
    assert_eq! ( response . readiness_summary . unwrap () . unshown_changes, 0 );
    let again : SaveResponse = save_cleanly (f, &saved) ?;
    assert_eq! ( again . saved_view, saved, "c1 stays editable" );
    Ok (( )) }) }

/// Once c1 is editable in m's container folder, a save that undoes
/// its change takes it out of U; it stays editable there, as a
/// lingering node does, after that save and a further one.
#[test]
fn an_editable_container_folder_member_outside_u_stays_editable (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("folder_member", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    retitle_in_a_content_view (f, "c1", "c1 retitled") ?;
    let saved : String = save_cleanly (f, &buffer) ? . saved_view;
    let reverted : String = saved . replacen ("c1 retitled", "c1", 1);
    let after : String = save_cleanly (f, &reverted) ? . saved_view;
    let c1_line : &str = subtree_starting (&after, "*** (skg (node (id c1) ") [0];
    assert! ( c1_line . ends_with (") c1") && ! c1_line . contains ("writeProtected"),
              "c1 must stay editable in the container folder:\n{}", after );
    let again : String = save_cleanly (f, &after) ? . saved_view;
    assert_eq! ( again, after );
    Ok (( )) }) }
