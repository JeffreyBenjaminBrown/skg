// cargo nextest run --test grouped_repos -E 'test(hidden_nodes::)'
//
// A node is restricted when the skgrepo restriction excludes its home
// skgrepo ('docs/api-and-formats.org', "Restricted-skgrepo nodes").
// Where a record the restriction shows names a restricted node, the
// view shows a Restricted vognode, which says nothing of the node; a
// member whose record is restricted has no headline. A save under a
// restriction keeps what a Restricted vognode stands for while it
// claims, and the members the buffer could not show, which disk
// supplementation owns; a tightened restriction re-kinds an open
// view's occurrence of a restricted node to a Restricted vognode in
// place, over its children.
//
// The fixture, written by the test:
//   public  : r contains a, h, b and subscribes to hs; a; b; s1; hv.
//   private : h ("h secret", aliased "h alias secret") contains s1;
//             hs ("hs secret"); hh ("hh secret").
// r records all of that in its public file. Some tests have r hide hh,
// or hh and hv, from its subscribers there, and some give r a private
// section, which records that r contains hc ("hc secret", private)
// after b and hides hp ("hp secret", private).

use crate::git_fixture::{
  Fixture, OwnedSkgrepos, open_content_view, switch_skgrepo_set,
  switch_skgrepo_set_rerendering, with_git_fixture, write_unchanged_skgrepos};
use crate::git_fixture_helpers::mk_test_tcp_stream_pair;

use skg::from_text::strandeds::StrandedsReview;
use skg::serve::handlers::save_buffer::{
  SaveResponse, update_from_and_rerender_buffer_with_approvals};
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::test_utils::skg_env_from_parts;
use skg::types::env::SkgEnv;
use skg::types::misc::{ID, members_of};
use skg::types::nodes::rust::GraphnodeInRust;
use skg::types::views_state::ViewforestId;

use futures::executor::block_on;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::net::TcpStream;
use std::path::Path;

const VIEW : &str = "view of r";
const VIEW_OF_H : &str = "view of h";

/// Two saves under the restriction "public", each of which writes r:
/// one of a view drawn under it, and one of a view drawn under "all"
/// and then switched to "public". In both, h and hs are Restricted
/// vognodes, which keep r's members h and hs, in place.
#[test]
fn a_save_under_a_restriction_keeps_the_members_its_restricted_vognodes_stand_for (
) -> Result<(), Box<dyn Error>> {
  let case : tempfile::TempDir = tempfile::TempDir::new () ?;
  write_fixture (case . path ()) ?;
  with_git_fixture (
    case . path (), OwnedSkgrepos::Named (&["public", "private"]), &[],
    |f| {
      let public : SkgrepoRestriction =
        SkgrepoRestriction::named (&f . config, "public" . into ()) ?;
      { // drawn under "public"
        let opened : String = open_view_of_r (f, Some (&public)) ?;
        assert_restricted_vognodes_alone_stand_for ("the view drawn under public",
                                                    &opened, &["h", "hs"]);
        let edited : String = opened
          . replacen ( ")) r\n", ")) r edited\n** a typed child\n", 1 );
        assert_ne! ( edited, opened );
        let saved : SaveResponse = save_under (f, &edited, &public) ?;
        assert_restricted_vognodes_alone_stand_for ("the saved view",
                                                    &saved . saved_view, &["h", "hs"]);
        let r : GraphnodeInRust = node_in_graph (f, "r");
        assert_eq! ( r . title, "r edited" );
        let contains : Vec<ID> = members_of (&r . contains);
        assert_eq! ( contains . len (), 4,
                     "r contains the typed child and its three members: {:?}",
                     contains );
        assert_eq! ( contains [1 ..] . to_vec (),
                     vec! [ ID::from ("a"), ID::from ("h"), ID::from ("b") ],
                     "the restricted member h stays where it stood" );
        assert_eq! ( members_of ( r . subscribesTo . or_default () ),
                     vec! [ ID::from ("hs") ],
                     "the restricted subscribee hs stays" ); }
      { // drawn under "all", then switched to "public"
        let mut restriction : SkgrepoRestriction =
          SkgrepoRestriction::named (&f . config, "all" . into ()) ?;
        open_view_of_r (f, None) ?;
        let switched : String = switch (f, &mut restriction, "public") ?;
        assert_restricted_vognodes_alone_stand_for ("the switched view",
                                                    &switched, &["h", "hs"]);
        let edited : String = switched
          . replacen ( ")) r edited\n", ")) r edited again\n", 1 );
        assert_ne! ( edited, switched );
        save_under (f, &edited, &public) ?;
        let r : GraphnodeInRust = node_in_graph (f, "r");
        assert_eq! ( r . title, "r edited again" );
        assert_eq! ( members_of (&r . contains) [1 ..] . to_vec (),
                     vec! [ ID::from ("a"), ID::from ("h"), ID::from ("b") ],
                     "the restricted member h stays where it stood" );
        assert_eq! ( members_of ( r . subscribesTo . or_default () ),
                     vec! [ ID::from ("hs") ],
                     "the restricted subscribee hs stays" ); }
      Ok (( )) } ) }

/// r hides hh (private) and hv (public) from its subscribers in its
/// public file, and hp (private) in its private section, which also
/// records that r contains hc (private). A view of r drawn under "all"
/// and switched to "public" shows Restricted vognodes, not empty
/// folders: hs in the subscribee folder, hh beside
/// hv in the folder of what r hides outside its subscribees; and h in
/// r's content. Of hp and hc, whose records are restricted, it shows
/// nothing. Saved with an edit, it keeps every hide and every member.
#[test]
fn a_save_under_a_restriction_keeps_every_hide_and_member (
) -> Result<(), Box<dyn Error>> {
  let case : tempfile::TempDir = tempfile::TempDir::new () ?;
  write_fixture_where_r_hides (case . path (), &["hh", "hv"], PrivateSectionOfR::Written) ?;
  with_git_fixture (
    case . path (), OwnedSkgrepos::Named (&["public", "private"]), &[],
    |f| {
      let public : SkgrepoRestriction =
        SkgrepoRestriction::named (&f . config, "public" . into ()) ?;
      let switched : String = switched_to_public (f) ?;
      assert_restricted_vognodes_alone_stand_for (
        "the switched view", &switched, &["h", "hs", "hh"]);
      for unseen in ["(gnid hc)", "(gnid hp)"] {
        assert! ( ! switched . contains (unseen),
                  "the switched view shows {}:\n{}", unseen, switched ); }
      assert_eq! ( children_of_the_hidden_outside_folder (&switched),
                   vec! [ "restricted hh" . to_string (), "node hv" . to_string () ],
                   "the folder of r's hides holds hh as a Restricted vognode, \
                    and hv:\n{}", switched );
      let edited : String = switched . replacen ( ")) r\n", ")) r edited\n", 1 );
      assert_ne! ( edited, switched );
      save_under (f, &edited, &public) ?;
      let r : GraphnodeInRust = node_in_graph (f, "r");
      assert_eq! ( r . title, "r edited" );
      assert_eq! ( sorted ( members_of ( r . hidesFromSubs . or_default () )),
                   vec! [ ID::from ("hh"), ID::from ("hp"), ID::from ("hv") ],
                   "the save keeps every hide" );
      assert_r_keeps_its_members (f);
      Ok (( )) } ) }

/// As above, but the line of the Restricted vognode hh is deleted
/// before the save: that one hide goes, and the records the
/// restriction keeps out (r's hide of hp, its containment of hc) stay
/// as they were.
#[test]
fn deleting_a_restricted_vognodes_line_removes_its_record_alone (
) -> Result<(), Box<dyn Error>> {
  let case : tempfile::TempDir = tempfile::TempDir::new () ?;
  write_fixture_where_r_hides (case . path (), &["hh", "hv"], PrivateSectionOfR::Written) ?;
  with_git_fixture (
    case . path (), OwnedSkgrepos::Named (&["public", "private"]), &[],
    |f| {
      let public : SkgrepoRestriction =
        SkgrepoRestriction::named (&f . config, "public" . into ()) ?;
      let switched : String = switched_to_public (f) ?;
      let hh_line : String = switched . lines ()
        . find ( |line| line . contains ("restricted (gnid hh)") )
        . unwrap_or_else ( || panic! ( "hh stands as a Restricted vognode:\n{}", switched ))
        . to_string ();
      let edited : String = switched . replacen ( &format! ("{}\n", hh_line), "", 1 );
      assert_ne! ( edited, switched );
      save_under (f, &edited, &public) ?;
      let r : GraphnodeInRust = node_in_graph (f, "r");
      assert_eq! ( sorted ( members_of ( r . hidesFromSubs . or_default () )),
                   vec! [ ID::from ("hp"), ID::from ("hv") ],
                   "the hide of hh goes; that of hp, recorded in r's private \
                    section, stays" );
      assert_r_keeps_its_members (f);
      Ok (( )) } ) }

/// r contains x, private, and subscribes to e, which contains x too;
/// r records both in its public file. Under "public" x stands in r's
/// content as a Restricted vognode; its line deleted and the view
/// saved, r no longer contains x, and, x being content of a node r
/// subscribes to, r hides x from its subscriptions, as it would an
/// occurrence's removal.
#[test]
fn deleting_a_restricted_vognode_from_a_subscribers_content_hides_it (
) -> Result<(), Box<dyn Error>> {
  let case : tempfile::TempDir = tempfile::TempDir::new () ?;
  write_unchanged_skgrepos (case . path (), &[
    ( "public", &[
      ("r", "pid: r\ntitle: r\ncontains:\n- x\nsubscribesTo:\n- e\n"),
      ("e", "pid: e\ntitle: e\ncontains:\n- x\n") ] ),
    ( "private", &[ ("x", "pid: x\ntitle: x secret\n") ] ) ]) ?;
  with_git_fixture (
    case . path (), OwnedSkgrepos::Named (&["public", "private"]), &[],
    |f| {
      let public : SkgrepoRestriction =
        SkgrepoRestriction::named (&f . config, "public" . into ()) ?;
      let opened : String = open_view_of_r (f, Some (&public)) ?;
      assert_restricted_vognodes_alone_stand_for ("the view", &opened, &["x"]);
      let x_line : String = opened . lines ()
        . find ( |line| line . starts_with ("** ")
                        && line . contains ("restricted (gnid x)") )
        . unwrap_or_else ( || panic! ( "x stands in r's content:\n{}", opened ))
        . to_string ();
      save_under (f, &opened . replacen ( &format! ("{}\n", x_line), "", 1 ),
                  &public) ?;
      let r : GraphnodeInRust = node_in_graph (f, "r");
      assert_eq! ( members_of (&r . contains), Vec::<ID>::new () );
      assert_eq! ( members_of ( r . hidesFromSubs . or_default () ),
                   vec! [ ID::from ("x") ] );
      Ok (( )) } ) }

/// A view of r drawn under "all", switched to "public": h's occurrence
/// is a Restricted vognode, in place, holding s1, and nothing shows h's
/// text. Switched back to "all", it is h's occurrence again, with the
/// same viewnode ID.
#[test]
fn a_tightened_restriction_re_kinds_an_occurrence_of_a_restricted_node_in_place (
) -> Result<(), Box<dyn Error>> {
  let case : tempfile::TempDir = tempfile::TempDir::new () ?;
  write_fixture (case . path ()) ?;
  with_git_fixture (
    case . path (), OwnedSkgrepos::Named (&["public", "private"]), &[],
    |f| {
      let mut restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (&f . config, "all" . into ()) ?;
      let opened : String = open_view_of_r (f, None) ?;
      assert! ( opened . contains ("h secret"),
                "under all the view shows h:\n{}", opened );
      let switched : String = switch (f, &mut restriction, "public") ?;
      assert_restricted_vognodes_alone_stand_for ("the switched view",
                                                  &switched, &["h", "hs"]);
      assert! ( ! switched . contains ("restrictedNode")
                && ! switched . contains (" tombstone"),
                "no retired placeholder, no tombstone:\n{}", switched );
      let headlines : Vec<&str> = switched . lines ()
        . filter ( |line| line . starts_with ('*') ) . collect ();
      let h : usize = headlines . iter ()
        . position ( |line| line . starts_with ("** ")
                            && line . contains (" restricted (gnid h)") )
        . unwrap_or_else ( || panic! (
          "a Restricted vognode stands where h stood:\n{}", switched ));
      assert! ( headlines . get (h + 1) . is_some_and (
                  |line| line . starts_with ("*** ")
                         && line . contains ("(gnid s1)") ),
                "it holds s1:\n{}", switched );
      let back : String = switch (f, &mut restriction, "all") ?;
      assert! ( back . contains ("(vnid 0e) node (gnid h) ")
                && back . contains (") h secret\n"),
                "the switch back draws h where it stood:\n{}", back );
      Ok (( )) } ) }

/// The exact buffers of a view of r switched to "public" and back to
/// "all": narrowed, hs is a Restricted vognode in the subscribee
/// folder, and h's occurrence a Restricted vognode holding s1;
/// widened, each is an occurrence again, where it stood. And of a view
/// rooted at h, which is private: narrowed, its root is a Restricted
/// vognode, showing "restricted" and h's gnid and nothing of h;
/// widened, it is h's editable occurrence again, where it stood, with
/// s1 claiming under it. Its containers folder, which may stand under
/// an occurrence alone, became a tombstone at the narrowing, as under
/// any occurrence re-kinded to a Restricted vognode, and keeps r.
#[test]
fn a_switch_and_its_reverse_draw_these_buffers (
) -> Result<(), Box<dyn Error>> {
  let case : tempfile::TempDir = tempfile::TempDir::new () ?;
  write_fixture (case . path ()) ?;
  with_git_fixture (
    case . path (), OwnedSkgrepos::Named (&["public", "private"]), &[],
    |f| {
      let mut restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (&f . config, "all" . into ()) ?;
      open_view_of_r (f, None) ?;
      let view_of_h : String = open_content_view (f, "h", VIEW_OF_H, None) ?;
      let (narrowed, _) : (Vec<String>, String) = switch_skgrepo_set_rerendering (
        f, &mut restriction, "public", &[VIEW, VIEW_OF_H] ) ?;
      let (widened, _) : (Vec<String>, String) = switch_skgrepo_set_rerendering (
        f, &mut restriction, "all", &[VIEW, VIEW_OF_H] ) ?;
      assert_eq! ( narrowed [0], SWITCHED_TO_PUBLIC );
      assert_eq! ( widened [0], SWITCHED_BACK_TO_ALL );
      assert_eq! ( view_of_h, VIEW_OF_H_UNDER_ALL );
      assert_eq! ( narrowed [1], VIEW_OF_H_SWITCHED_TO_PUBLIC );
      assert_eq! ( widened [1], VIEW_OF_H_SWITCHED_BACK_TO_ALL );
      Ok (( )) } ) }

/// As above, where r also hides hh, private, from its subscribers:
/// narrowed, the subscribee folder holds hs, and the hidden-outside
/// folder in it hh, each as a Restricted vognode; widened, each is an
/// occurrence again.
#[test]
fn a_switch_draws_restricted_vognodes_in_the_folders (
) -> Result<(), Box<dyn Error>> {
  let case : tempfile::TempDir = tempfile::TempDir::new () ?;
  write_fixture_where_r_hides (case . path (), &["hh"], PrivateSectionOfR::NotWritten) ?;
  with_git_fixture (
    case . path (), OwnedSkgrepos::Named (&["public", "private"]), &[],
    |f| {
      let mut restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (&f . config, "all" . into ()) ?;
      let opened : String = open_view_of_r (f, None) ?;
      let narrowed : String = switch (f, &mut restriction, "public") ?;
      let widened : String = switch (f, &mut restriction, "all") ?;
      assert! ( opened . contains ("hiddenOutsideOfSubscribeeFolder"),
                "under all r's subscribee folder holds hs and hh:\n{}", opened );
      assert_eq! ( narrowed, "\
* (skg (vnid 0a) node (gnid r) (skgRepos (nodeRepo public)) (view (rels (contains (out 2))) repoBoundary)) r
** (skg (vnid 0b) subscribeeFolder)
*** (skg (vnid 0c) restricted (gnid hs))
*** (skg (vnid 0d) hiddenOutsideOfSubscribeeFolder)
**** (skg (vnid 0e) folded restricted (gnid hh))
** (skg (vnid 0f) node (gnid a) (skgRepos (nodeRepo public)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) a
** (skg (vnid 0g) restricted (gnid h))
*** (skg (vnid 0h) node (gnid s1) (skgRepos (nodeRepo public))) s1
** (skg (vnid 0i) node (gnid b) (skgRepos (nodeRepo public)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) b
" );
      assert_eq! ( widened, "\
* (skg (vnid 0a) node (gnid r) (skgRepos (nodeRepo public)) (view (rels (contains (out 3)) (subscribesTo (out 1)) (hidesFromSubs (out 1))) repoBoundary)) r
** (skg (vnid 0b) subscribeeFolder)
*** (skg (vnid 0c) node (gnid hs) (skgRepos (nodeRepo private) (relRepo public)) writeProtected (view (rels (contains (out 0 (unintegrated 0))) (subscribesTo (in 1 (ancestors 2)))) (birth (subscribesTo in 2)) repoBoundary)) hs secret
*** (skg (vnid 0d) hiddenOutsideOfSubscribeeFolder)
**** (skg (vnid 0e) folded node (gnid hh) (skgRepos (nodeRepo private)) writeProtected (view (rels (hidesFromSubs (in 1 (ancestors 3)))) (birth (hidesFromSubs in 3)) repoBoundary)) hh secret
** (skg (vnid 0f) node (gnid a) (skgRepos (nodeRepo public)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) a
** (skg (vnid 0g) node (gnid h) (skgRepos (nodeRepo private) (relRepo public)) (view (rels (contains (in 1 (ancestors 1)) (out 1))) (props (aliases 1)) (birth (contains in 1)) repoBoundary)) h secret
*** (skg (vnid 0h) node (gnid s1) (skgRepos (nodeRepo public)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)) repoBoundary)) s1
** (skg (vnid 0i) node (gnid b) (skgRepos (nodeRepo public)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) b
" );
      Ok (( )) } ) }

const SWITCHED_TO_PUBLIC : &str = "\
* (skg (vnid 0a) node (gnid r) (skgRepos (nodeRepo public)) (view (rels (contains (out 2))) repoBoundary)) r
** (skg (vnid 0b) subscribeeFolder)
*** (skg (vnid 0c) restricted (gnid hs))
** (skg (vnid 0d) node (gnid a) (skgRepos (nodeRepo public)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) a
** (skg (vnid 0e) restricted (gnid h))
*** (skg (vnid 0f) node (gnid s1) (skgRepos (nodeRepo public))) s1
** (skg (vnid 0g) node (gnid b) (skgRepos (nodeRepo public)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) b
";

const SWITCHED_BACK_TO_ALL : &str = "\
* (skg (vnid 0a) node (gnid r) (skgRepos (nodeRepo public)) (view (rels (contains (out 3)) (subscribesTo (out 1))) repoBoundary)) r
** (skg (vnid 0b) subscribeeFolder)
*** (skg (vnid 0c) node (gnid hs) (skgRepos (nodeRepo private) (relRepo public)) writeProtected (view (rels (contains (out 0 (unintegrated 0))) (subscribesTo (in 1 (ancestors 2)))) (birth (subscribesTo in 2)) repoBoundary)) hs secret
** (skg (vnid 0d) node (gnid a) (skgRepos (nodeRepo public)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) a
** (skg (vnid 0e) node (gnid h) (skgRepos (nodeRepo private) (relRepo public)) (view (rels (contains (in 1 (ancestors 1)) (out 1))) (props (aliases 1)) (birth (contains in 1)) repoBoundary)) h secret
*** (skg (vnid 0f) node (gnid s1) (skgRepos (nodeRepo public)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)) repoBoundary)) s1
** (skg (vnid 0g) node (gnid b) (skgRepos (nodeRepo public)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) b
";

const VIEW_OF_H_UNDER_ALL : &str = "\
* (skg (vnid 0a) node (gnid h) (skgRepos (nodeRepo private)) (view (rels (contains (in 1) (out 1))) (props (aliases 1)) repoBoundary)) h secret
** (skg (vnid 0b) containerFolder)
*** (skg (vnid 0c) node (gnid r) (skgRepos (nodeRepo public) (relRepo public)) writeProtected (view (rels (contains (out 3 (ancestors 2))) (subscribesTo (out 1))) (birth (contains out 2)) repoBoundary)) r
** (skg (vnid 0d) node (gnid s1) (skgRepos (nodeRepo public)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)) repoBoundary)) s1
";

const VIEW_OF_H_SWITCHED_TO_PUBLIC : &str = "\
* (skg (vnid 0a) restricted (gnid h))
** (skg (vnid 0b) tombstone)
*** (skg (vnid 0c) node (gnid r) (skgRepos (nodeRepo public)) writeProtected (view (rels (contains (out 2))) repoBoundary)) r
** (skg (vnid 0d) node (gnid s1) (skgRepos (nodeRepo public)) (view repoBoundary)) s1
";

const VIEW_OF_H_SWITCHED_BACK_TO_ALL : &str = "\
* (skg (vnid 0a) node (gnid h) (skgRepos (nodeRepo private)) (view (rels (contains (in 1) (out 1))) (props (aliases 1)) repoBoundary)) h secret
** (skg (vnid 0b) tombstone)
*** (skg (vnid 0c) node (gnid r) (skgRepos (nodeRepo public)) writeProtected (view (rels (contains (out 3)) (subscribesTo (out 1))) repoBoundary)) r
** (skg (vnid 0d) node (gnid s1) (skgRepos (nodeRepo public)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)) repoBoundary)) s1
";

/// A switch's rerender reports no buffer corrections: narrowed, the
/// Restricted vognodes are the restriction's doing, and widened, the
/// occurrences drawn again are too ('BufferCorrectionPolicy').
#[test]
fn a_switch_reports_no_buffer_corrections (
) -> Result<(), Box<dyn Error>> {
  let case : tempfile::TempDir = tempfile::TempDir::new () ?;
  write_fixture (case . path ()) ?;
  with_git_fixture (
    case . path (), OwnedSkgrepos::Named (&["public", "private"]), &[],
    |f| {
      let mut restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (&f . config, "all" . into ()) ?;
      open_view_of_r (f, None) ?;
      for name in ["public", "all"] {
        let (view, done) : (String, String) =
          switch_reporting (f, &mut restriction, name) ?;
        assert! ( done . contains ("(corrections ())"),
                  "the switch to {} reports no buffer corrections: {}\n\nview:\n{}",
                  name, done, view ); }
      Ok (( )) } ) }

/// r, public, records the alias "r public alias" in its public file
/// and "r secret alias" in its private section. Under "public" the
/// second, whose record is restricted, is absent, text and count: in a
/// fresh view, and in r's alias folder after a switch. A save under
/// "public" keeps it, even with r's alias folder edited, and with a
/// write-protected occurrence of r showing its folder too. Under "all"
/// it is present again.
#[test]
fn an_alias_recorded_in_a_restricted_section_is_shown_only_under_all (
) -> Result<(), Box<dyn Error>> {
  let case : tempfile::TempDir = tempfile::TempDir::new () ?;
  write_unchanged_skgrepos ( case . path (), &[
    ( "public", &[
      ("r", "pid: r\ntitle: r\naliases:\n- r public alias\ncontains:\n- a\n"),
      ("a", "pid: a\ntitle: a\n") ] ),
    ( "private", &[
      ("r", "pid: r\naliases:\n- r secret alias\n") ] ) ]) ?;
  with_git_fixture (
    case . path (), OwnedSkgrepos::Named (&["public", "private"]), &[],
    |f| {
      let public : SkgrepoRestriction =
        SkgrepoRestriction::named (&f . config, "public" . into ()) ?;
      let mut restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (&f . config, "all" . into ()) ?;
      let fresh_under_public : String =
        open_content_view (f, "r", "fresh view of r", Some (&public)) ?;
      assert! ( fresh_under_public . contains ("(props (aliases 1))")
                && ! fresh_under_public . contains ("secret"),
                "a fresh view under public counts r's public alias alone:\n{}",
                fresh_under_public );
      let opened : String = open_view_of_r (f, None) ?;
      assert! ( opened . contains ("(props (aliases 2))"),
                "under all r counts both aliases:\n{}", opened );
      let with_aliases : String = save_under (
        f, &opened . replacen (
          ") r\n", " (viewRequests (folder aliases))) r\n", 1 ),
        &restriction ) ? . saved_view;
      assert! ( with_aliases . contains ("r secret alias"),
                "under all r's alias folder shows both:\n{}", with_aliases );
      let narrowed : String = switch (f, &mut restriction, "public") ?;
      assert! ( narrowed . contains ("(props (aliases 1))")
                && narrowed . contains (") r public alias\n")
                && ! narrowed . contains ("secret"),
                "narrowed, r shows and counts its public alias alone:\n{}",
                narrowed );
      { // saved under "public", r's alias folder edited, and r shown
        // again write-protected with its folder
        let edited : String = format! (
          "{}* (skg node (gnid r) (skgRepos (nodeRepo public)) writeProtected) r\n\
           ** (skg aliasFolder)\n\
           *** (skg alias) r public alias\n",
          narrowed . replacen (
            ") r public alias\n", ") r public alias\n*** (skg alias) r typed alias\n", 1 ));
        save_under (f, &edited, &public) ?;
        let r : GraphnodeInRust = node_in_graph (f, "r");
        let mut aliases : Vec<String> = members_of ( r . aliases . or_default () );
        aliases . sort ();
        assert_eq! ( aliases,
                     vec! [ "r public alias" . to_string (),
                            "r secret alias" . to_string (),
                            "r typed alias" . to_string () ],
                     "the save keeps the alias whose record is restricted" );
        assert! ( r . aliases . or_default () . iter () . any (
                    |alias| alias . member == "r secret alias"
                            && alias . relRepo . 0 == "private" ),
                  "in its private section: {:?}", r . aliases ); }
      let widened : String = switch (f, &mut restriction, "all") ?;
      assert! ( widened . contains ("(props (aliases 3))")
                && widened . contains ("r secret alias"),
                "widened, r shows and counts every alias:\n{}", widened );
      Ok (( )) } ) }

/// Nothing in 'buffer' shows the text of a private node, and each of
/// 'gnids' stands only as a Restricted vognode.
fn assert_restricted_vognodes_alone_stand_for (
  what   : &str,
  buffer : &str,
  gnids  : &[&str],
) {
  assert! ( ! buffer . contains ("secret"),
            "{} shows a private node's text:\n{}", what, buffer );
  for gnid in gnids {
    let lines : Vec<&str> = buffer . lines ()
      . filter ( |line| line . contains (&format! ("(gnid {})", gnid)) )
      . collect ();
    assert! ( ! lines . is_empty ()
              && lines . iter () . all ( |line|
                   line . contains (&format! ("restricted (gnid {})", gnid)) ),
              "{} shows {} only as a Restricted vognode:\n{}", what, gnid, buffer ); }}

/// What each child of the folder of what r hides outside its
/// subscribees stands as: "KIND GNID".
fn children_of_the_hidden_outside_folder (
  buffer : &str,
) -> Vec<String> {
  let lines : Vec<&str> = buffer . lines () . collect ();
  let folder : usize = lines . iter ()
    . position ( |line| line . contains ("hiddenOutsideOfSubscribeeFolder") )
    . unwrap_or_else ( || panic! ( "no hidden-outside folder:\n{}", buffer ));
  let depth = |line : &str| -> usize { line . chars () . take_while ( |c| *c == '*' ) . count () };
  let folder_depth : usize = depth (lines [folder]);
  lines [folder + 1 ..] . iter ()
    . take_while ( |line| depth (line) > folder_depth )
    . filter ( |line| depth (line) == folder_depth + 1 )
    . map ( |line| {
      let gnid : &str = line . split ("(gnid ") . nth (1) . and_then (
        |rest| rest . split (')') . next () ) . unwrap_or ("?");
      let kind : &str = if line . contains (" restricted (gnid ") { "restricted" }
                        else { "node" };
      format! ("{} {}", kind, gnid) } )
    . collect () }

/// r keeps its members: it contains a, h, b and hc, hc after b as its
/// private section places it, and subscribes to hs.
fn assert_r_keeps_its_members (
  f : &Fixture,
) {
  let r : GraphnodeInRust = node_in_graph (f, "r");
  assert_eq! ( members_of (&r . contains),
               vec! [ ID::from ("a"), ID::from ("h"), ID::from ("b"), ID::from ("hc") ],
               "r keeps its members, hc after b" );
  assert_eq! ( members_of ( r . subscribesTo . or_default () ),
               vec! [ ID::from ("hs") ],
               "r keeps its subscription to hs" ); }

/// A view of r drawn under "all" and switched to "public".
fn switched_to_public (
  f : &mut Fixture,
) -> Result<String, Box<dyn Error>> {
  let mut restriction : SkgrepoRestriction =
    SkgrepoRestriction::named (&f . config, "all" . into ()) ?;
  open_view_of_r (f, None) ?;
  switch (f, &mut restriction, "public") }

fn sorted (
  mut gnids : Vec<ID>,
) -> Vec<ID> {
  gnids . sort ();
  gnids }

//
// The fixture and the operations
//

/// Whether the fixture gives r a private section.
#[derive(Clone, Copy, PartialEq, Eq)]
enum PrivateSectionOfR { Written, NotWritten }

fn write_fixture (
  dir : &Path,
) -> Result<(), Box<dyn Error>> {
  write_fixture_where_r_hides (dir, &[], PrivateSectionOfR::NotWritten) }

/// The fixture, with r hiding 'hides' from its subscribers in its
/// public file, and, when 'private_section' says so, a private section
/// of r, which records that r contains hc after b and hides hp. Beside
/// them stand hh ("hh secret", private) and hv (public), which r may
/// hide.
fn write_fixture_where_r_hides (
  dir             : &Path,
  hides           : &[&str],
  private_section : PrivateSectionOfR,
) -> Result<(), Box<dyn Error>> {
  let hidesFromSubs : String =
    if hides . is_empty () { String::new () }
    else { format! ( "hidesFromSubs:\n{}",
                     hides . iter () . map ( |hide| format! ("- {}\n", hide) )
                     . collect::<String> () ) };
  let r : String = format! (
    "pid: r\ntitle: r\ncontains:\n- a\n- h\n- b\nsubscribesTo:\n- hs\n{}",
    hidesFromSubs );
  let public : &[(&str, &str)] = &[
    ("r",  &r),
    ("a",  "pid: a\ntitle: a\n"),
    ("b",  "pid: b\ntitle: b\n"),
    ("s1", "pid: s1\ntitle: s1\n"),
    ("hv", "pid: hv\ntitle: hv\n"), ];
  let mut private : Vec<(&str, &str)> = vec! [
    ("h",  "pid: h\ntitle: h secret\naliases:\n- h alias secret\ncontains:\n- s1\n"),
    ("hs", "pid: hs\ntitle: hs secret\n"),
    ("hh", "pid: hh\ntitle: hh secret\n"), ];
  if private_section == PrivateSectionOfR::Written {
    private . extend ([
      ("r",  "pid: r\ncontains:\n- anchor: b\n- hc\nhidesFromSubs:\n- hp\n"),
      ("hc", "pid: hc\ntitle: hc secret\n"),
      ("hp", "pid: hp\ntitle: hp secret\n") ]); }
  write_unchanged_skgrepos (dir, &[("public", public), ("private", &private)]) }

/// Draw r, under 'restriction' when one is given, and register the
/// view as VIEW.
fn open_view_of_r (
  f           : &mut Fixture,
  restriction : Option<&SkgrepoRestriction>,
) -> Result<String, Box<dyn Error>> {
  open_content_view (f, "r", VIEW, restriction) }

/// Save 'text' as VIEW under 'restriction'; it must save without
/// errors.
fn save_under (
  f           : &mut Fixture,
  text        : &str,
  restriction : &SkgrepoRestriction,
) -> Result<SaveResponse, Box<dyn Error>> {
  let (mut stream, _read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  let mut env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
  let response : SaveResponse = block_on (
    update_from_and_rerender_buffer_with_approvals (
      &mut stream, text, &mut env,
      &Ok ( ViewforestId::ContentView (VIEW . into ()) ),
      &mut f . views_state, Some (restriction),
      true, &HashMap::new (), &HashSet::new (), &HashSet::new (),
      &StrandedsReview::NotReviewed, &[] )) ?;
  assert! ( response . errors . is_empty (),
            "the buffer must save without errors, got {:?}\n\nbuffer:\n{}",
            response . errors, text );
  Ok (response) }

fn node_in_graph (
  f   : &Fixture,
  pid : &str,
) -> GraphnodeInRust {
  f . graph . load_full () . get (&ID::from (pid)) . cloned ()
    . unwrap_or_else ( || panic! ("the graph has no node {}", pid) ) }

/// Switch to the skgrepo-set 'name'; return the view as rerendered.
fn switch (
  f           : &mut Fixture,
  restriction : &mut SkgrepoRestriction,
  name        : &str,
) -> Result<String, Box<dyn Error>> {
  Ok ( switch_skgrepo_set (f, restriction, name, VIEW) ? . 0 ) }

/// Switch to the skgrepo-set 'name'; return the view as rerendered,
/// and the "rerender-done" message, which carries the buffer
/// corrections.
fn switch_reporting (
  f           : &mut Fixture,
  restriction : &mut SkgrepoRestriction,
  name        : &str,
) -> Result<(String, String), Box<dyn Error>> {
  switch_skgrepo_set (f, restriction, name, VIEW) }
