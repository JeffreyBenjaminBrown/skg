// A buffer of the diff view is rendered from its sets recomputed after
// the save wherever a save renders something new, but only the "integrate
// unshown changes" request makes occurrences editable or appends roots
// ('docs/diff-view.org', "A save makes nothing editable and appends no
// root"). The 'fresh' fixture (one skgrepo, main): r
// contains a, whose body changed, and w, unchanged; w contains w1
// and w2; w1 contains w1a and subscribes to w2. s, unchanged,
// contains s1. p and q are new, and contain each other. So the
// buffer's roots are q and r, and w is a write-protected member
// without its content.

use skg::metadata_canon::without_vids;
use crate::git_fixture::{graph_straddle, save_cleanly};
use crate::pruned_change_view::common::{
  all_changes_view, integrate_unshown_changes, node_lines,
  open_all_changes_buffer, requesting_editable_at, rerendered_view_content,
  retitle_in_a_content_view, send_stage_and_commit_request, with_fixture};

use skg::graph_straddle::types::GraphStraddle;
use skg::diff_view::pruned_change_view::ensure_editableOccurrences_exist;
use skg::diff_view::readiness_store::ReadinessStore;
use skg::serve::handlers::diff_view::diffView_buffer;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::test_utils::skg_env_from_parts;
use skg::types::env::{RuntimeGeneration, SkgEnv};
use skg::types::misc::ID;
use skg::types::tree::forest::ViewForest;
use skg::types::views_state::DiffViewBufferKind;
use skg::types::viewnode::{Phantom, ViewnodeKind, Vognode};

use ego_tree::NodeId;

use std::error::Error;
use std::sync::Arc;

/// Whether some line of 'buffer' shows 'node' editable.
fn shown_editable (
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

/// w changes after the buffer opened, while the buffer shows it only
/// as a write-protected member without its content. After the next save it is
/// still one, and its change is counted as not editable. The command makes it editable,
/// with its change shown, and its members, outside U, write-protected.
/// Its origin r is shown already, so no root is added, and no change
/// is counted as not editable.
#[test]
fn a_write_protected_leaf_that_changes_becomes_editable_by_the_command (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("fresh", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    assert! ( ! shown_editable (&buffer, "w"), "{}", buffer );
    let buffer : String = retitle_in_a_content_view (f, "w", "w retitled", &buffer) ?;
    let response : SaveResponse = save_cleanly (f, &all_changes_view (), &buffer) ?;
    let saved : &str = &response . saved_view;
    assert! ( ! node_lines (saved, "w") . is_empty ()
              && ! shown_editable (saved, "w"),
              "w must stay a write-protected member without its content:\n{}", saved );
    assert! ( node_lines (saved, "w1") . is_empty (), "{}", saved );
    assert_eq! ( response . readiness_summary . unwrap () . changes_not_editable_here, 1 );
    let (shown, not_editable) : (String, usize) = integrate_unshown_changes (f) ?;
    assert! ( shown_editable (&shown, "w"), "w must be editable:\n{}", shown );
    assert! ( without_vids (&shown) . contains ("*** (skg (oldText title) (diff (leaves 1))) w"),
              "w's old title must be shown:\n{}", shown );
    for member in [ "w1", "w2" ] {
      assert! ( ! node_lines (&shown, member) . is_empty ()
                && ! shown_editable (&shown, member),
                "{} must be a write-protected member without its content:\n{}", member, shown ); }
    assert_eq! ( root_ids (&shown), vec! [ "q", "r" ] );
    assert_eq! ( not_editable, 0 );
    Ok (( )) }) }

/// w1a, two levels under the write-protected member w without its content,
/// changes. The next save shows nothing of it and counts it as not editable. The command
/// makes w, w1 and w1a editable and shows w1a's change under them;
/// w2 stays write-protected.
#[test]
fn a_change_two_levels_under_a_write_protected_leaf_is_shown_by_the_command (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("fresh", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    assert! ( node_lines (&buffer, "w1a") . is_empty (), "{}", buffer );
    let buffer : String = retitle_in_a_content_view (f, "w1a", "w1a retitled", &buffer) ?;
    let response : SaveResponse = save_cleanly (f, &all_changes_view (), &buffer) ?;
    assert! ( node_lines (&response . saved_view, "w1a") . is_empty (),
              "{}", response . saved_view );
    assert_eq! ( response . readiness_summary . unwrap () . changes_not_editable_here, 1 );
    let (shown, not_editable) : (String, usize) = integrate_unshown_changes (f) ?;
    for node in [ "w", "w1", "w1a" ] {
      assert! ( shown_editable (&shown, node),
                "{} must be editable:\n{}", node, shown ); }
    assert! ( ! shown_editable (&shown, "w2"), "{}", shown );
    assert! ( without_vids (&shown) . contains ("***** (skg (oldText title) (diff (leaves 1))) w1a"),
              "w1a's old title must be shown under w1:\n{}", shown );
    assert_eq! ( not_editable, 0 );
    Ok (( )) }) }

/// s1 changes, under s, an origin the buffer does not show. The next
/// save appends no root and counts the change as not editable. The command
/// appends s as a root, after the standing ones, rendered as a first
/// rendering renders a context root, with s1's change under it; a
/// further save and a further command change nothing.
#[test]
fn a_change_under_a_new_origin_is_appended_as_a_root_by_the_command (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("fresh", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let buffer : String = retitle_in_a_content_view (f, "s1", "s1 retitled", &buffer) ?;
    let response : SaveResponse = save_cleanly (f, &all_changes_view (), &buffer) ?;
    assert_eq! ( root_ids (&response . saved_view), vec! [ "q", "r" ],
                 "{}", response . saved_view );
    assert_eq! ( response . readiness_summary . unwrap () . changes_not_editable_here, 1 );
    let (shown, not_editable) : (String, usize) = integrate_unshown_changes (f) ?;
    assert_eq! ( root_ids (&shown), vec! [ "q", "r", "s" ], "{}", shown );
    let s_subtree : Vec<&str> = subtree_starting (&shown, "* (skg bodyFolded (node (id s) ");
    assert! ( s_subtree . iter () . any ( |line|
                line . starts_with ("** (skg (node (id s1) ")
                && ! line . contains ("writeProtected") )
              && s_subtree . iter () . any ( |line| without_vids (line) == "*** (skg (oldText title) (diff (leaves 1))) s1" ),
              "s1's change must be shown under s:\n{}", shown );
    assert_eq! ( not_editable, 0 );
    let again : SaveResponse = save_cleanly (f, &all_changes_view (), &shown) ?;
    assert_eq! ( again . saved_view, shown, "a further save adds nothing" );
    assert_eq! ( integrate_unshown_changes (f) ? . 0, shown,
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
    let response : SaveResponse = save_cleanly (f, &all_changes_view (), &edited) ?;
    let saved : String = response . saved_view;
    assert! ( saved . lines () . any ( |line|
                line . starts_with ("** (skg (node (id ") && line . ends_with (") z") ),
              "z must be shown under q:\n{}", saved );
    assert_eq! ( root_ids (&saved), vec! [ "q", "r" ], "{}", saved );
    let (shown, _) : (String, usize) = integrate_unshown_changes (f) ?;
    assert_eq! ( root_ids (&shown), vec! [ "q", "r" ], "{}", shown );
    Ok (( )) }) }

/// The user write-protects a, whose body changed, at its only
/// occurrence. It stays write-protected through saves, its change
/// counted as not editable, until the command makes it editable again, its
/// change shown.
#[test]
fn a_node_the_user_write_protected_stays_so_until_the_command (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("fresh", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    assert_eq! ( node_lines (&buffer, "a") . len (), 1, "{}", buffer );
    let requested : String = buffer . replacen (
      "(node (id a) (repo main) ",
      "(node (id a) (repo main) (viewRequests writeProtectedView) ", 1 );
    let saved : SaveResponse = save_cleanly (f, &all_changes_view (), &requested) ?;
    let again : SaveResponse = save_cleanly (f, &all_changes_view (), &saved . saved_view) ?;
    for response in [ &saved, &again ] {
      assert! ( ! shown_editable (&response . saved_view, "a")
                && ! without_vids (&response . saved_view) . contains ("(oldText body)"),
                "a must stay write-protected:\n{}", response . saved_view );
      assert! ( response . readiness_summary . as_ref () . unwrap ()
                . changes_not_editable_here > 0 ); }
    let (shown, not_editable) : (String, usize) = integrate_unshown_changes (f) ?;
    assert! ( shown_editable (&shown, "a")
              && without_vids (&shown) . contains ("*** (skg (oldText body) (diff (leaves 1)))"),
              "a must be editable, its change shown:\n{}", shown );
    assert_eq! ( not_editable, 0 );
    Ok (( )) }) }

/// On a buffer that shows every change, the command changes nothing.
#[test]
fn the_command_on_a_buffer_with_every_change_editable_changes_nothing (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("fresh", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let (shown, not_editable) : (String, usize) = integrate_unshown_changes (f) ?;
    assert_eq! ( shown, buffer );
    assert_eq! ( not_editable, 0 );
    Ok (( )) }) }

/// An editable-view request on w, a write-protected member outside
/// U, cascades through its content as in an ordinary view, until the
/// node budget (2 here) is spent: w and w1 extend; w2 and w1a stay
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
    let saved : String = save_cleanly (f, &all_changes_view (), &requested) ? . saved_view;
    for node in [ "w", "w1" ] {
      assert! ( shown_editable (&saved, node),
                "{} must be editable:\n{}", node, saved ); }
    for node in [ "w2", "w1a" ] {
      assert! ( ! node_lines (&saved, node) . is_empty ()
                && ! shown_editable (&saved, node),
                "{} must be write-protected:\n{}", node, saved ); }
    assert! ( ! saved . contains ("subscribeeFolder"),
              "no default folder is added:\n{}", saved );
    assert! ( ! saved . contains ("viewRequests"), "{}", saved );
    let again : String = save_cleanly (f, &all_changes_view (), &saved) ? . saved_view;
    assert_eq! ( again, saved, "the nodes made editable stay editable" );
    Ok (( )) }) }

/// The 'shared' fixture (one skgrepo, main): x and y, whose bodies
/// changed, each contain n, which contains a, whose body changed, and
/// b, unchanged. So x and y are the buffer's roots, and n is shown
/// editable under x and write-protected under y. An editable-view
/// request on n under y makes that occurrence editable and the one
/// under x write-protected, with its content; a, a node of U under
/// it, is editable again under y, its change shown there. A further
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
    let saved : String = save_cleanly (f, &all_changes_view (), &requested) ? . saved_view;
    let x_subtree : Vec<&str> = subtree_starting (&saved, "* (skg (node (id x) ");
    let y_subtree : Vec<&str> = subtree_starting (&saved, "* (skg (node (id y) ");
    for (subtree, editable) in [ (&x_subtree, false), (&y_subtree, true) ] {
      for node in [ "n", "a" ] {
        let atom : String = format! ("(node (id {}) ", node);
        let line : &str = subtree . iter ()
          . find ( |line| line . contains (&atom) )
          . unwrap_or_else ( || panic! ( "{} must be shown:\n{}", node, saved ));
        assert_eq! ( ! line . contains ("writeProtected"), editable,
                     "{}:\n{}", node, saved ); }}
    assert! ( y_subtree . iter () . any ( |line| without_vids (line) == "**** (skg (oldText body) (diff (leaves 1)))" ),
              "a's change must be shown under y:\n{}", saved );
    assert! ( ! without_vids (&saved) . contains ("viewRequests"), "{}", saved );
    let again : String = save_cleanly (f, &all_changes_view (), &saved) ? . saved_view;
    assert_eq! ( again, saved, "the editability moved stays where it is" );
    Ok (( )) }) }

/// In the 'shared' fixture, once editability moved from n under x to
/// n under y, a request on n under x moves it back. The children n
/// kept under x, which stopped being members when it was
/// write-protected, are adopted, not rerendered: the same children, in
/// the same order, members again, and b, folded there, stays folded. a, a node of
/// U, is editable under x again, its change shown there, and n and a
/// are write-protected under y. A further save changes nothing.
#[test]
fn an_editable_view_request_moves_editability_back (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("shared", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let moved : String = save_cleanly (
      f, &all_changes_view (), &requesting_editable_at (
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
    assert! ( without_vids (&moved) . contains (n_under_x) && without_vids (&moved) . contains (b_under_x),
              "n and b must be write-protected under x:\n{}", moved );
    let requested : String = requesting_editable_at (
      &moved . replacen ( b_under_x,
                          "(skg folded (node (id b) (repo main) (affectsParent false) writeProtected ",
                          1 ),
      n_under_x );
    let saved : String = save_cleanly (f, &all_changes_view (), &requested) ? . saved_view;
    let x_subtree : Vec<&str> = subtree_starting (&saved, "* (skg (node (id x) ");
    let y_subtree : Vec<&str> = subtree_starting (&saved, "* (skg (node (id y) ");
    for (subtree, editable) in [ (&x_subtree, true), (&y_subtree, false) ] {
      for node in [ "n", "a" ] {
        let atom : String = format! ("(node (id {}) ", node);
        let line : &str = subtree . iter ()
          . find ( |line| line . contains (&atom) )
          . unwrap_or_else ( || panic! ( "{} must be shown:\n{}", node, saved ));
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
    assert! ( x_subtree . iter () . any ( |line| without_vids (line) == "**** (skg (oldText body) (diff (leaves 1)))" ),
              "a's change must be shown under x:\n{}", saved );
    assert! ( ! without_vids (&saved) . contains ("viewRequests"), "{}", saved );
    let again : String = save_cleanly (f, &all_changes_view (), &saved) ? . saved_view;
    assert_eq! ( again, saved, "the editability moved back stays where it is" );
    Ok (( )) }) }

/// An editable-view request on r, an editable node of U, cascades
/// through its content as in an ordinary view: its members outside U
/// extend until the node budget (2 here) is spent, so w and w1 become
/// editable while w2 and w1a stay write-protected. r and a, in U,
/// are not charged; were either charged, w1 would stay
/// write-protected. A further save changes nothing.
#[test]
fn an_editable_view_request_on_a_node_of_u_extends_its_members (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("fresh", &["main"], |f| {
    f . config . initial_node_limit = 2;
    let buffer : String = open_all_changes_buffer (f) ?;
    let requested : String = buffer . replacen (
      "* (skg bodyFolded (node (id r) (repo main) ",
      "* (skg bodyFolded (node (id r) (repo main) (viewRequests editableView) ", 1 );
    assert_ne! ( requested, buffer );
    let saved : String = save_cleanly (f, &all_changes_view (), &requested) ? . saved_view;
    for node in [ "r", "a", "w", "w1" ] {
      assert! ( shown_editable (&saved, node),
                "{} must be editable:\n{}", node, saved ); }
    for node in [ "w2", "w1a" ] {
      assert! ( ! node_lines (&saved, node) . is_empty ()
                && ! shown_editable (&saved, node),
                "{} must be write-protected:\n{}", node, saved ); }
    assert! ( ! saved . contains ("viewRequests"), "{}", saved );
    let again : String = save_cleanly (f, &all_changes_view (), &saved) ? . saved_view;
    assert_eq! ( again, saved, "the nodes made editable stay editable" );
    Ok (( )) }) }

/// U (a, p, q and r) exceeds the node budget, here 1; the buffer is
/// rendered in full regardless, when opened and after a save.
#[test]
fn a_buffer_whose_u_exceeds_the_node_budget_is_rendered_in_full (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("fresh", &["main"], |f| {
    f . config . initial_node_limit = 1;
    let buffer : String = open_all_changes_buffer (f) ?;
    let saved : String = save_cleanly (f, &all_changes_view (), &buffer) ? . saved_view;
    for rendered in [ &buffer, &saved ] {
      for node in [ "a", "p", "q", "r" ] {
        assert! ( shown_editable (rendered, node),
                  "{} must be editable:\n{}", node, rendered ); }
      assert! ( without_vids (&rendered) . contains ("*** (skg (oldText body) (diff (leaves 1)))"),
                "a's change must be shown:\n{}", rendered ); }
    assert_eq! ( saved, buffer );
    Ok (( )) }) }

/// After a commit, U is empty, but the lingering nodes the buffer showed
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
      assert! ( shown_editable (&content, node),
                "{} must stay editable:\n{}", node, content ); }
    assert! ( ! content . contains ("(diff "), "{}", content );
    Ok (( )) }) }

/// The 'folder_member' fixture (one skgrepo, main): m contains a,
/// whose body changed; c1 and c2, unchanged, each contain m. So m is
/// the buffer's one root, and c1 and c2 are shown only as members of
/// its container folder. c1 changes after the buffer opened. After
/// the next save it is still write-protected there, its change
/// counted as not editable. The command makes it editable there, with its
/// content list (m) under it and its change shown; no root is
/// appended, and a further save changes nothing.
#[test]
fn a_change_shown_only_in_a_container_folder_becomes_editable_there (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("folder_member", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    assert! ( ! shown_editable (&buffer, "c1"), "{}", buffer );
    let buffer : String = retitle_in_a_content_view (f, "c1", "c1 retitled", &buffer) ?;
    let response : SaveResponse = save_cleanly (f, &all_changes_view (), &buffer) ?;
    assert! ( ! shown_editable (&response . saved_view, "c1"),
              "{}", response . saved_view );
    assert_eq! ( response . readiness_summary . unwrap () . changes_not_editable_here, 1 );
    let (shown, not_editable) : (String, usize) = integrate_unshown_changes (f) ?;
    let c1_subtree : Vec<&str> = subtree_starting (&shown, "*** (skg (node (id c1) ");
    assert! ( ! c1_subtree [0] . contains ("writeProtected"),
              "c1 must be editable in the container folder:\n{}", shown );
    assert! ( c1_subtree . iter () . any ( |line| without_vids (line) == "**** (skg (oldText title) (diff (leaves 1))) c1" )
              && c1_subtree . iter () . any ( |line|
                   line . starts_with ("**** (skg (node (id m) ") ),
              "c1's change and content must be shown under it:\n{}", shown );
    assert_eq! ( root_ids (&shown), vec! [ "m" ], "{}", shown );
    assert_eq! ( not_editable, 0 );
    let again : SaveResponse = save_cleanly (f, &all_changes_view (), &shown) ?;
    assert_eq! ( without_vids (&again . saved_view), without_vids (&shown),
                 "c1 stays editable" );
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
    save_cleanly (f, &all_changes_view (), &buffer) ?;
    let (shown, _) : (String, usize) = integrate_unshown_changes (f) ?;
    let reverted : String = shown . replacen ("c1 retitled", "c1", 1);
    let after : String = save_cleanly (f, &all_changes_view (), &reverted) ? . saved_view;
    let c1_line : &str = subtree_starting (&after, "*** (skg (node (id c1) ") [0];
    assert! ( c1_line . ends_with (") c1") && ! c1_line . contains ("writeProtected"),
              "c1 must stay editable in the container folder:\n{}", after );
    let again : String = save_cleanly (f, &all_changes_view (), &after) ? . saved_view;
    assert_eq! ( again, after );
    Ok (( )) }) }

/// The 'historical_content' fixture (one skgrepo, main): at HEAD r
/// contains h and k, and h contains m. Since then h was deleted and m
/// retitled, so the buffer shows h as a historical phantom under r,
/// holding its old content m, a node of U, as an editable removed
/// member.
/// Once m is write-protected there, the walk that finds the
/// occurrences to make editable passes through a historical phantom
/// in content position, so it makes m editable there again.
#[test]
fn the_occurrences_made_editable_include_a_member_under_a_historical_phantom (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("historical_content", &["main"], |f| {
    let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
    let runtime : Arc<RuntimeGeneration> = env . runtime_snapshot ();
    let straddle : GraphStraddle = graph_straddle (f) ?;
    let mut viewforest : ViewForest = diffView_buffer (
      &runtime, &straddle, &ReadinessStore::default (),
      DiffViewBufferKind::AllChanges ) ?
      . expect ("the buffer is not empty") . viewforest;
    let m_under_h = |viewforest : &ViewForest| -> Option<NodeId> {
      viewforest . nodes () . find_map ( |node| match &node . value () . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (t))
          if t . skgid == ID::from ("m")
          && node . parent () . is_some_and ( |parent| matches! (
               &parent . value () . kind,
               ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (h)))
                 if h . skgid == ID::from ("h") )) =>
          Some ( node . id () ),
        _ => None, } ) };
    let m_under_h_is_editable = |viewforest : &ViewForest| -> Option<bool> {
      m_under_h (viewforest) . map ( |m| viewforest . get (m) . unwrap ()
                                           . value () . editable_skgid () . is_some () ) };
    assert_eq! ( m_under_h_is_editable (&viewforest), Some (true),
                 "m is editable under h" );
    let m : NodeId = m_under_h (&viewforest) . unwrap ();
    if let ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =
      &mut viewforest . get_mut (m) . unwrap () . value () . kind {
      t . write_protect (); }
    assert_eq! ( m_under_h_is_editable (&viewforest), Some (false) );
    ensure_editableOccurrences_exist (
      &straddle, DiffViewBufferKind::AllChanges,
      &mut viewforest, &runtime . graph, &runtime . config ) ?;
    assert_eq! ( m_under_h_is_editable (&viewforest), Some (true) );
    Ok (( )) } ) }

/// The same fixture: once the user write-protects m under h, the
/// command makes it editable there again, so every change is
/// editable in the buffer, and m stays editable through a save of it.
#[test]
fn the_command_makes_a_member_under_a_historical_phantom_editable (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("historical_content", &["main"], |f| {
    let m_line : &str = "*** (skg (node (id m) (repo main) (affectsParent false) (diff (presence node~ rel-) (leaves 2 rel all)))) m, retitled";
    let buffer : String = open_all_changes_buffer (f) ?;
    assert! ( without_vids (&buffer) . contains (m_line), "{}", buffer );
    let requested : String = buffer . replacen (
      "(node (id m) (repo main) ",
      "(node (id m) (repo main) (viewRequests writeProtectedView) ", 1 );
    let protected : String =
      save_cleanly (f, &all_changes_view (), &requested) ? . saved_view;
    assert! ( ! shown_editable (&protected, "m"), "{}", protected );
    let (shown, not_editable) : (String, usize) = integrate_unshown_changes (f) ?;
    assert_eq! ( not_editable, 0, "{}", shown );
    assert! ( without_vids (&shown) . contains (m_line), "{}", shown );
    let response : SaveResponse = save_cleanly (f, &all_changes_view (), &shown) ?;
    assert! ( without_vids (&response . saved_view) . contains (m_line),
              "{}", response . saved_view );
    Ok (( )) } ) }

/// The 'removed_partners' fixture (described in 'buffer.rs'): x
/// stands first as b's removed member, editable there. Once the user
/// write-protects it, x has no editable occurrence, and the command
/// makes that removed member editable again, not x's later
/// occurrences under e and c; a save keeps it so.
#[test]
fn the_command_makes_a_removed_member_under_a_live_vognode_editable (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("removed_partners", &["main"], |f| {
    let x_under_b : &str = "** (skg (node (id x) (repo main) (affectsParent false) (rels";
    let buffer : String = open_all_changes_buffer (f) ?;
    assert! ( buffer . contains (x_under_b), "{}", buffer );
    let requested : String = buffer . replacen (
      "(node (id x) (repo main) (affectsParent false) ",
      "(node (id x) (repo main) (affectsParent false) (viewRequests writeProtectedView) ", 1 );
    let protected : String =
      save_cleanly (f, &all_changes_view (), &requested) ? . saved_view;
    assert! ( ! shown_editable (&protected, "x"), "{}", protected );
    let (shown, not_editable) : (String, usize) = integrate_unshown_changes (f) ?;
    assert_eq! ( not_editable, 0, "{}", shown );
    assert! ( shown . contains (x_under_b), "{}", shown );
    assert_eq! ( node_lines (&shown, "x") . iter ()
                 . filter ( |line| ! line . contains ("writeProtected") ) . count (),
                 1, "{}", shown );
    let response : SaveResponse = save_cleanly (f, &all_changes_view (), &shown) ?;
    assert_eq! ( response . saved_view, shown );
    Ok (( )) } ) }
