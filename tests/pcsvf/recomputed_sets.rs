// A buffer of the diff view draws by its sets recomputed after
// the save wherever a save draws something new, but only the "show
// unshown changes" request makes occurrences editable or appends roots
// ('TODO/total-diff-view/08_plan_yet-more.org', "The view follows U
// only on command"). The 'fresh' fixture (one skgrepo, main): r
// contains a, whose body changed, and w, unchanged; w contains w1
// and w2; w1 contains w1a and subscribes to w2. s, unchanged,
// contains s1. p and q are new, and contain each other. So the
// buffer's roots are q and r, and w is an unexpanded write-protected
// member.

use crate::pcsvf::common::{
  Fixture, collateral_view_content, node_lines, open_all_changes_buffer,
  requesting_editable_at, rerendered_view_content, save,
  send_stage_and_commit_request, show_unshown_changes, with_fixture};
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
use std::io::Read;
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
/// another buffer would. Returns the "all changes" buffer as its
/// client then holds it: 'buffer', or its collateral rerendering if
/// the save sent one.
fn retitle_in_a_content_view (
  f      : &mut Fixture,
  node   : &str,
  title  : &str,
  buffer : &str,
) -> Result<String, Box<dyn Error>> {
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
  let (mut stream, mut read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response : SaveResponse = block_on (
    update_from_and_rerender_buffer_test (
      &mut stream, &edited, &f . config, &f . tantivy, &f . graph,
      &Ok (view_id), &mut f . views_state )) ?;
  assert! ( response . errors . is_empty (),
            "the content view must save, got {:?}", response . errors );
  drop (stream);
  let mut streamed : String = String::new ();
  read_end . read_to_string (&mut streamed) ?;
  Ok ( collateral_view_content (&streamed)
       . unwrap_or_else ( || buffer . to_string () )) }

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
/// as an unexpanded write-protected member. After the next save it is
/// still one, and its change is counted unshown. The command makes it editable,
/// with its change drawn, and its members, outside U, write-protected.
/// Its origin r is drawn already, so no root is added, and no change
/// is unshown.
#[test]
fn a_write_protected_leaf_that_changes_becomes_editable_by_the_command (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("fresh", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    assert! ( ! drawn_editable (&buffer, "w"), "{}", buffer );
    let buffer : String = retitle_in_a_content_view (f, "w", "w retitled", &buffer) ?;
    let response : SaveResponse = save_cleanly (f, &buffer) ?;
    let saved : &str = &response . saved_view;
    assert! ( ! node_lines (saved, "w") . is_empty ()
              && ! drawn_editable (saved, "w"),
              "w must stay an unexpanded write-protected member:\n{}", saved );
    assert! ( node_lines (saved, "w1") . is_empty (), "{}", saved );
    assert_eq! ( response . readiness_summary . unwrap () . changes_not_editable_here, 1 );
    let (shown, unshown) : (String, usize) = show_unshown_changes (f) ?;
    assert! ( drawn_editable (&shown, "w"), "w must be editable:\n{}", shown );
    assert! ( shown . contains ("*** (skg (oldText title)) w"),
              "w's old title must be drawn:\n{}", shown );
    for member in [ "w1", "w2" ] {
      assert! ( ! node_lines (&shown, member) . is_empty ()
                && ! drawn_editable (&shown, member),
                "{} must be an unexpanded write-protected member:\n{}", member, shown ); }
    assert_eq! ( root_ids (&shown), vec! [ "q", "r" ] );
    assert_eq! ( unshown, 0 );
    Ok (( )) }) }

/// w1a, two levels under the unexpanded write-protected member w,
/// changes. The next save draws nothing of it and counts it unshown. The command
/// makes w, w1 and w1a editable and draws w1a's change under them;
/// w2 stays write-protected.
#[test]
fn a_change_two_levels_under_a_write_protected_leaf_is_drawn_by_the_command (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("fresh", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    assert! ( node_lines (&buffer, "w1a") . is_empty (), "{}", buffer );
    let buffer : String = retitle_in_a_content_view (f, "w1a", "w1a retitled", &buffer) ?;
    let response : SaveResponse = save_cleanly (f, &buffer) ?;
    assert! ( node_lines (&response . saved_view, "w1a") . is_empty (),
              "{}", response . saved_view );
    assert_eq! ( response . readiness_summary . unwrap () . changes_not_editable_here, 1 );
    let (shown, unshown) : (String, usize) = show_unshown_changes (f) ?;
    for node in [ "w", "w1", "w1a" ] {
      assert! ( drawn_editable (&shown, node),
                "{} must be editable:\n{}", node, shown ); }
    assert! ( ! drawn_editable (&shown, "w2"), "{}", shown );
    assert! ( shown . contains ("***** (skg (oldText title)) w1a"),
              "w1a's old title must be drawn under w1:\n{}", shown );
    assert_eq! ( unshown, 0 );
    Ok (( )) }) }

/// s1 changes, under s, an origin the buffer does not draw. The next
/// save appends no root and counts the change unshown. The command
/// appends s as a root, after the standing ones, drawn as a first
/// drawing draws a context root, with s1's change under it; a
/// further save and a further command change nothing.
#[test]
fn a_change_under_a_new_origin_is_appended_as_a_root_by_the_command (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("fresh", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let buffer : String = retitle_in_a_content_view (f, "s1", "s1 retitled", &buffer) ?;
    let response : SaveResponse = save_cleanly (f, &buffer) ?;
    assert_eq! ( root_ids (&response . saved_view), vec! [ "q", "r" ],
                 "{}", response . saved_view );
    assert_eq! ( response . readiness_summary . unwrap () . changes_not_editable_here, 1 );
    let (shown, unshown) : (String, usize) = show_unshown_changes (f) ?;
    assert_eq! ( root_ids (&shown), vec! [ "q", "r", "s" ], "{}", shown );
    let s_subtree : Vec<&str> = subtree_starting (&shown, "* (skg bodyFolded (node (id s) ");
    assert! ( s_subtree . iter () . any ( |line|
                line . starts_with ("** (skg (node (id s1) ")
                && ! line . contains ("writeProtected") )
              && s_subtree . contains (&"*** (skg (oldText title)) s1"),
              "s1's change must be drawn under s:\n{}", shown );
    assert_eq! ( unshown, 0 );
    let again : SaveResponse = save_cleanly (f, &shown) ?;
    assert_eq! ( again . saved_view, shown, "a further save adds nothing" );
    assert_eq! ( show_unshown_changes (f) ? . 0, shown,
                 "a further command adds nothing" );
    Ok (( )) }) }

/// The new cycle of p and q gains a member: q now contains z, which
/// contains p. The cycle's root q stands, so neither the save nor
/// the command adds a second root for the cycle.
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
    let (shown, _) : (String, usize) = show_unshown_changes (f) ?;
    assert_eq! ( root_ids (&shown), vec! [ "q", "r" ], "{}", shown );
    Ok (( )) }) }

/// The user write-protects a, whose body changed, at its only
/// occurrence. It stays write-protected through saves, its change
/// counted unshown, until the command makes it editable again, its
/// change drawn.
#[test]
fn a_node_the_user_write_protected_stays_so_until_the_command (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("fresh", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    assert_eq! ( node_lines (&buffer, "a") . len (), 1, "{}", buffer );
    let requested : String = buffer . replacen (
      "(node (id a) (repo main) ",
      "(node (id a) (repo main) (viewRequests writeProtectedView) ", 1 );
    let saved : SaveResponse = save_cleanly (f, &requested) ?;
    let again : SaveResponse = save_cleanly (f, &saved . saved_view) ?;
    for response in [ &saved, &again ] {
      assert! ( ! drawn_editable (&response . saved_view, "a")
                && ! response . saved_view . contains ("(oldText body)"),
                "a must stay write-protected:\n{}", response . saved_view );
      assert! ( response . readiness_summary . as_ref () . unwrap ()
                . changes_not_editable_here > 0 ); }
    let (shown, unshown) : (String, usize) = show_unshown_changes (f) ?;
    assert! ( drawn_editable (&shown, "a")
              && shown . contains ("*** (skg (oldText body))"),
              "a must be editable, its change drawn:\n{}", shown );
    assert_eq! ( unshown, 0 );
    Ok (( )) }) }

/// On a buffer that shows every change, the command changes nothing.
#[test]
fn the_command_on_a_buffer_with_nothing_unshown_changes_nothing (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("fresh", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let (shown, unshown) : (String, usize) = show_unshown_changes (f) ?;
    assert_eq! ( shown, buffer );
    assert_eq! ( unshown, 0 );
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
/// kept drawn under x, which stopped being members when it was
/// write-protected, are adopted, not redrawn: the same children, in
/// the same order, members again, and b, folded there, stays folded. a, a node of
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
        . map ( |line| [ "writeProtected ", "omittedBody ", "bodyFolded ", "folded ",
                         "(affectsParent false) ", " (birth (contains in 1))" ]
                         . iter () . fold ( line . to_string (),
                                            |line, atom| line . replace (atom, "") ))
        . collect () };
    let b_under_x : &str = "(skg (node (id b) (repo main) (affectsParent false) writeProtected ";
    assert! ( moved . contains (n_under_x) && moved . contains (b_under_x),
              "n and b must be write-protected under x:\n{}", moved );
    let requested : String = requesting_editable_at (
      &moved . replacen ( b_under_x,
                          "(skg folded (node (id b) (repo main) (affectsParent false) writeProtected ",
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
/// the next save it is still write-protected there, its change
/// counted unshown. The command makes it editable there, with its
/// content list (m) drawn under it and its change shown; no root is
/// appended, and a further save changes nothing.
#[test]
fn a_change_drawn_only_in_a_container_folder_becomes_editable_there (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("folder_member", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    assert! ( ! drawn_editable (&buffer, "c1"), "{}", buffer );
    let buffer : String = retitle_in_a_content_view (f, "c1", "c1 retitled", &buffer) ?;
    let response : SaveResponse = save_cleanly (f, &buffer) ?;
    assert! ( ! drawn_editable (&response . saved_view, "c1"),
              "{}", response . saved_view );
    assert_eq! ( response . readiness_summary . unwrap () . changes_not_editable_here, 1 );
    let (shown, unshown) : (String, usize) = show_unshown_changes (f) ?;
    let c1_subtree : Vec<&str> = subtree_starting (&shown, "*** (skg (node (id c1) ");
    assert! ( ! c1_subtree [0] . contains ("writeProtected"),
              "c1 must be editable in the container folder:\n{}", shown );
    assert! ( c1_subtree . contains (&"**** (skg (oldText title)) c1")
              && c1_subtree . iter () . any ( |line|
                   line . starts_with ("**** (skg (node (id m) ") ),
              "c1's change and content must be drawn under it:\n{}", shown );
    assert_eq! ( root_ids (&shown), vec! [ "m" ], "{}", shown );
    assert_eq! ( unshown, 0 );
    let again : SaveResponse = save_cleanly (f, &shown) ?;
    assert_eq! ( again . saved_view, shown, "c1 stays editable" );
    Ok (( )) }) }

/// Once the command made c1 editable in m's container folder, a save
/// that undoes its change takes it out of U; it stays editable there,
/// as a lingering node does, after that save and a further one.
#[test]
fn an_editable_container_folder_member_outside_u_stays_editable (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("folder_member", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let buffer : String = retitle_in_a_content_view (f, "c1", "c1 retitled", &buffer) ?;
    save_cleanly (f, &buffer) ?;
    let (shown, _) : (String, usize) = show_unshown_changes (f) ?;
    let reverted : String = shown . replacen ("c1 retitled", "c1", 1);
    let after : String = save_cleanly (f, &reverted) ? . saved_view;
    let c1_line : &str = subtree_starting (&after, "*** (skg (node (id c1) ") [0];
    assert! ( c1_line . ends_with (") c1") && ! c1_line . contains ("writeProtected"),
              "c1 must stay editable in the container folder:\n{}", after );
    let again : String = save_cleanly (f, &after) ? . saved_view;
    assert_eq! ( again, after );
    Ok (( )) }) }
