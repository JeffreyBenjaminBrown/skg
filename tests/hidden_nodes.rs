// cargo nextest run --test grouped_repos -E 'test(hidden_nodes::)'
//
// A node is restricted when the skgrepo restriction excludes its home
// skgrepo ('docs/api-and-formats.org', "Restricted-skgrepo nodes").
// Where a record the skgrepo restriction shows names a restricted
// node, the view shows a Restricted vognode, which says nothing of
// the node; a member whose record is restricted has no headline. A
// save under a skgrepo restriction keeps what a Restricted vognode
// stands for while it claims, and the members the buffer could not
// show, which disk supplementation owns; a tightened skgrepo restriction
// re-kinds an open view's occurrence of a restricted node to a
// Restricted vognode in place, over its children.
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
  Fixture, OwnedSkgrepos, edit_line, open_content_view, switch_skgrepo_set,
  switch_skgrepo_set_rerendering, with_git_fixture, write_unchanged_skgrepos};
use crate::git_fixture::save_under as save_under_restriction;
use crate::pruned_change_view::common::open_all_changes_buffer;

use skg::save::DeleteFrom;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::types::misc::{ID, RelPartner, SkgrepoName, members_of};
use skg::types::nodes::rust::GraphnodeInRust;
use skg::metadata_canon::without_viewnode_ids;
use skg::org_to_text::viewforest_to_string;
use skg::types::errors::SaveError;
use skg::types::save::format_save_error_as_org;
use skg::types::views_state::{DiffViewBufferKind, ViewforestId};

use std::collections::BTreeMap;
use std::error::Error;
use std::path::{Path, PathBuf};

const VIEW : &str = "view of r";
const VIEW_OF_H : &str = "view of h";

/// Two saves under the skgrepo restriction "public", each of which writes r:
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
        let opened : String = open_view_of_r (f, &public) ?;
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
        open_view_of_r (f, &restriction) ?;
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
/// skgrepo restriction keeps out (r's hide of hp, its containment of
/// hc) stay as they were.
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
/// saved, r would hide x from its subscriptions, x being content of a
/// node r subscribes to, as it would on an occurrence's removal. That
/// hide would land in r's private section, the more private of the
/// endpoints' homes, which the skgrepo restriction keeps out: the save
/// is refused, naming the skgrepo, and no file changes.
#[test]
fn deleting_a_restricted_vognode_whose_hide_would_land_restricted_is_refused (
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
      let opened : String = open_view_of_r (f, &public) ?;
      assert_restricted_vognodes_alone_stand_for ("the view", &opened, &["x"]);
      let x_line : String = opened . lines ()
        . find ( |line| line . starts_with ("** ")
                        && line . contains ("restricted (gnid x)") )
        . unwrap_or_else ( || panic! ( "x stands in r's content:\n{}", opened ))
        . to_string ();
      let files_before : BTreeMap<PathBuf, String> = skg_files_on_disk (f) ?;
      assert_refused_for_a_restricted_record (
        save_view_under (f, VIEW, &opened . replacen ( &format! ("{}\n", x_line), "", 1 ),
                         &public),
        "- Relation: hidesFromSubs\n- Member: x\n- The record's repo: private" );
      assert_eq! ( skg_files_on_disk (f) ?, files_before, "no file changed" );
      Ok (( )) } ) }

/// Under "public", h stands in r's content as a restricted headline,
/// recorded in r's public file. A request on its line to record the
/// relationship in 'private' would move the record where the skgrepo
/// restriction keeps it out of sight: the save is refused, naming the
/// skgrepo, and r's record of h stays public.
#[test]
fn a_relRepo_request_on_a_restricted_headline_into_a_restricted_skgrepo_is_refused (
) -> Result<(), Box<dyn Error>> {
  let case : tempfile::TempDir = tempfile::TempDir::new () ?;
  write_fixture (case . path ()) ?;
  with_git_fixture (
    case . path (), OwnedSkgrepos::Named (&["public", "private"]), &[],
    |f| {
      let public : SkgrepoRestriction =
        SkgrepoRestriction::named (&f . config, "public" . into ()) ?;
      let opened : String = open_view_of_r (f, &public) ?;
      let requested : String = opened . replacen (
        "restricted (gnid h)", "restricted (gnid h) (editRequest (relRepo private))", 1 );
      assert_ne! ( requested, opened, "h stands in r's content:\n{}", opened );
      let files_before : BTreeMap<PathBuf, String> = skg_files_on_disk (f) ?;
      assert_refused_for_a_restricted_record (
        save_view_under (f, VIEW, &requested, &public),
        "- Relation: contains\n- Member: h\n- The record's repo: private" );
      assert_eq! ( skg_files_on_disk (f) ?, files_before, "no file changed" );
      let r : GraphnodeInRust = node_in_graph (f, "r");
      let h : &RelPartner<ID> = r . contains . iter ()
        . find ( |partner| partner . member == ID::from ("h") )
        . unwrap_or_else ( || panic! ("r still contains h: {:?}", r . contains) );
      assert_eq! ( h . relRepo, SkgrepoName::from ("public") );
      Ok (( )) } ) }

/// 'saved' is the refusal of a save for a record that would land in a
/// restricted section; the refusal, as the client shows it, contains
/// 'details'.
fn assert_refused_for_a_restricted_record (
  saved   : Result<SaveResponse, Box<dyn Error>>,
  details : &str,
) {
  let refusal : String = match saved {
    Err (e) => match e . downcast_ref::<SaveError> () {
      Some (save_error) => format_save_error_as_org (save_error),
      None => e . to_string (), },
    Ok (response) => panic! ( "the save must be refused; errors {:?}", response . errors ), };
  assert! ( refusal . contains ("Cannot save a record into a restricted repo")
            && refusal . contains (details),
            "the refusal names the record and its skgrepo:\n{}", refusal ); }

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
      let opened : String = open_view_of_r (f, &restriction) ?;
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
/// an occurrence alone, became a tombstone at the skgrepo narrowing,
/// as under any occurrence re-kinded to a Restricted vognode, and
/// keeps r.
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
      open_view_of_r (f, &restriction) ?;
      let view_of_h : String = open_content_view (f, "h", VIEW_OF_H, &restriction) ?;
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
      let opened : String = open_view_of_r (f, &restriction) ?;
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

/// A skgrepo narrowing and the skgrepo widening after it give the
/// view of r back as it was opened: narrowed, the private nodes are
/// Restricted vognodes; widened, they are occurrences again.
#[test]
fn a_narrowing_and_a_widening_give_the_view_back (
) -> Result<(), Box<dyn Error>> {
  let case : tempfile::TempDir = tempfile::TempDir::new () ?;
  write_fixture (case . path ()) ?;
  with_git_fixture (
    case . path (), OwnedSkgrepos::Named (&["public", "private"]), &[],
    |f| {
      let mut restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (&f . config, "all" . into ()) ?;
      let opened : String = open_view_of_r (f, &restriction) ?;
      let (narrowed, _) : (String, String) =
        switch_reporting (f, &mut restriction, "public") ?;
      assert! ( ! narrowed . contains ("secret"), "{}", narrowed );
      let (widened, _) : (String, String) =
        switch_reporting (f, &mut restriction, "all") ?;
      assert_eq! ( widened, opened );
      Ok (( )) } ) }

/// r, public, contains a in its public file and pc, public, in its
/// private section. Narrowed to "public", pc's record is restricted
/// while pc is not: pc, claiming membership of r, which the graph as
/// the skgrepo restriction shows it no longer warrants, vanishes.
#[test]
fn a_narrowing_makes_a_member_whose_record_alone_is_restricted_vanish (
) -> Result<(), Box<dyn Error>> {
  let case : tempfile::TempDir = tempfile::TempDir::new () ?;
  write_fixture_where_r_records_pc_privately (case . path (), PrivateRecordsOfR::Containment) ?;
  with_git_fixture (
    case . path (), OwnedSkgrepos::Named (&["public", "private"]), &[],
    |f| {
      let mut restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (&f . config, "all" . into ()) ?;
      let opened : String = open_view_of_r (f, &restriction) ?;
      assert! ( opened . contains ("node (gnid pc)") && ! opened . contains ("noClaim"),
                "under all pc is r's member:\n{}", opened );
      let narrowed : String = switch (f, &mut restriction, "public") ?;
      assert! ( ! narrowed . contains ("(gnid pc)"),
                "narrowed, pc vanishes:\n{}", narrowed );
      assert! ( narrowed . contains ("node (gnid a)"), "a stays:\n{}", narrowed );
      Ok (( )) } ) }

//
// A skgrepo narrowing, then a skgrepo widening, then a save of the
// view unchanged: r's private record survives. The fixture is
// 'write_fixture_where_r_records_pc_privately'; each test is one of the
// variants the review's probes tried ('probe_noclaim_*').
//

/// The base case: the view of r.
#[test]
fn a_narrowing_and_a_widening_keep_rs_private_record (
) -> Result<(), Box<dyn Error>> {
  round_trip_keeps_the_private_record (
    PrivateRecordsOfR::Containment, false,
    |f| { let unrestricted : SkgrepoRestriction =
            SkgrepoRestriction::unrestricted (&f . config);
          open_view_of_r (f, &unrestricted) },
    VIEW, |_f, _restriction| Ok (( )) ) }

/// The all-changes buffer of the diff view, where r newly contains n
/// since HEAD, so that r and its members are shown.
#[test]
fn a_narrowing_and_a_widening_of_the_diff_view_keep_rs_private_record (
) -> Result<(), Box<dyn Error>> {
  round_trip_keeps_the_private_record (
    PrivateRecordsOfR::Containment, true,
    |f| open_all_changes_buffer (f),
    &format! ( "diff-view: {}", DiffViewBufferKind::AllChanges . wire_name () ),
    |_f, _restriction| Ok (( )) ) }

/// r folded in a view of its container T.
#[test]
fn a_narrowing_and_a_widening_with_r_folded_keep_rs_private_record (
) -> Result<(), Box<dyn Error>> {
  round_trip_keeps_the_private_record (
    PrivateRecordsOfR::Containment, false,
    |f| {
      let unrestricted : SkgrepoRestriction =
        SkgrepoRestriction::unrestricted (&f . config);
      let opened : String = open_content_view (f, "T", VIEW_OF_T, &unrestricted) ?;
      let folded : String = edit_line (
        &opened, "** (skg node (gnid r)",
        |line| line . replacen ("(skg node", "(skg folded node", 1) );
      let all : SkgrepoRestriction =
        SkgrepoRestriction::named (&f . config, "all" . into ()) ?;
      Ok ( save_view_cleanly_under (f, VIEW_OF_T, &folded, &all) ? . saved_view ) },
    VIEW_OF_T, |_f, _restriction| Ok (( )) ) }

/// A save of another view, of o, between the skgrepo narrowing and the
/// skgrepo widening, with the view of r open.
#[test]
fn a_narrowing_a_save_of_another_view_and_a_widening_keep_rs_private_record (
) -> Result<(), Box<dyn Error>> {
  round_trip_keeps_the_private_record (
    PrivateRecordsOfR::Containment, false,
    |f| {
      let unrestricted : SkgrepoRestriction =
        SkgrepoRestriction::unrestricted (&f . config);
      open_content_view (f, "o", VIEW_OF_O, &unrestricted) ?;
      open_view_of_r (f, &unrestricted) },
    VIEW,
    |f, restriction| {
      let o : String = open_content_view (f, "o", VIEW_OF_O, restriction) ?;
      save_view_cleanly_under (
        f, VIEW_OF_O, &o . replace (") o\n", ") o, edited\n"), restriction ) ?;
      Ok (( )) } ) }

/// A save of the view of r unchanged under the skgrepo narrowing,
/// before the skgrepo widening.
#[test]
fn a_save_under_the_narrowing_then_a_widening_keep_rs_private_record (
) -> Result<(), Box<dyn Error>> {
  round_trip_keeps_the_private_record (
    PrivateRecordsOfR::Containment, false,
    |f| { let unrestricted : SkgrepoRestriction =
            SkgrepoRestriction::unrestricted (&f . config);
          open_view_of_r (f, &unrestricted) },
    VIEW,
    |f, restriction| {
      let narrowed : String = rendered_view (f, VIEW) ?;
      let saved : SaveResponse = save_view_cleanly_under (f, VIEW, &narrowed, restriction) ?;
      assert_eq! ( saved . saved_view, narrowed,
                   "saved unchanged under the narrowing, the view comes back as it was" );
      Ok (( )) } ) }

/// r's private section records that r subscribes to s, hides s1 from
/// its subscribers and overrides o, each a public graphnode. The view
/// of r asks for its overrides and hidesFromSubs folders; the
/// subscribee folder is drawn by default.
#[test]
fn a_narrowing_and_a_widening_keep_rs_private_subscription_and_override_records (
) -> Result<(), Box<dyn Error>> {
  round_trip_keeps_the_private_record (
    PrivateRecordsOfR::PartnerRecords, false,
    |f| {
      let unrestricted : SkgrepoRestriction =
        SkgrepoRestriction::unrestricted (&f . config);
      let opened : String = open_view_of_r (f, &unrestricted) ?;
      let asking : String = edit_line (
        &opened, "* (skg node (gnid r)",
        |line| line . replacen (
          "node (gnid r)",
          "node (gnid r) (viewRequests (folder overrides) (folder hidesFromSubs))", 1 ));
      let all : SkgrepoRestriction =
        SkgrepoRestriction::named (&f . config, "all" . into ()) ?;
      let shown : String = save_view_cleanly_under (f, VIEW, &asking, &all) ? . saved_view;
      for member in [ "(gnid s)", "(gnid o)", "(gnid s1)" ] {
        assert! ( shown . contains (member), "under all {} shows:\n{}", member, shown ); }
      Ok (shown) },
    VIEW, |_f, _restriction| Ok (( )) ) }

const VIEW_OF_T : &str = "view of T";
const VIEW_OF_O : &str = "view of o";

/// What r's private section records in
/// 'write_fixture_where_r_records_pc_privately'.
#[derive(Clone, Copy)]
enum PrivateRecordsOfR {
  /// r contains pc.
  Containment,
  /// r subscribes to s, hides s1 from its subscribers and overrides o.
  PartnerRecords,
}

/// public: r ("r") contains a, T contains r, and a, pc, o, s (which
/// contains s1 and s2), s1 and s2; private: r's section, as
/// 'private_records' says.
fn write_fixture_where_r_records_pc_privately (
  dir             : &Path,
  private_records : PrivateRecordsOfR,
) -> Result<(), Box<dyn Error>> {
  let private_r : &str = match private_records {
    PrivateRecordsOfR::Containment => "pid: r\ncontains:\n- pc\n",
    PrivateRecordsOfR::PartnerRecords =>
      "pid: r\nsubscribesTo:\n- s\nhidesFromSubs:\n- s1\noverrides:\n- o\n", };
  write_unchanged_skgrepos ( dir, &[
    ( "public", &[
      ("r",  "pid: r\ntitle: r\ncontains:\n- a\n"),
      ("a",  "pid: a\ntitle: a\n"),
      ("pc", "pid: pc\ntitle: pc\n"),
      ("T",  "pid: T\ntitle: T\ncontains:\n- r\n"),
      ("o",  "pid: o\ntitle: o\n"),
      ("s",  "pid: s\ntitle: s\ncontains:\n- s1\n- s2\n"),
      ("s1", "pid: s1\ntitle: s1\n"),
      ("s2", "pid: s2\ntitle: s2\n") ] ),
    ( "private", &[ ("r", private_r) ] ) ]) }

/// In the worktree, r newly contains n since HEAD.
fn change_r_since_head (
  dir : &Path,
) -> Result<(), Box<dyn Error>> {
  let worktree : PathBuf = dir . join ("public") . join ("worktree");
  std::fs::write ( worktree . join ("r.skg"), "pid: r\ntitle: r\ncontains:\n- a\n- n\n" ) ?;
  std::fs::write ( worktree . join ("n.skg"), "pid: n\ntitle: n\n" ) ?;
  Ok (( )) }

/// Open 'view' ('open'), narrow to "public", do 'between' under the
/// narrowing, widen to "all", and save 'view' unchanged under "all".
/// The widened view is the view as opened (but for viewnode IDs and
/// the edits 'between' made elsewhere), and every file of r is as it
/// was before the narrowing; so is every other file but o's, which
/// 'between' may edit.
fn round_trip_keeps_the_private_record (
  private_records      : PrivateRecordsOfR,
  r_changed_since_head : bool,
  open                 : impl FnOnce (&mut Fixture) -> Result<String, Box<dyn Error>>,
  view                 : &str,
  between              : impl FnOnce (&mut Fixture, &SkgrepoRestriction) -> Result<(), Box<dyn Error>>,
) -> Result<(), Box<dyn Error>> {
  let case : tempfile::TempDir = tempfile::TempDir::new () ?;
  write_fixture_where_r_records_pc_privately (case . path (), private_records) ?;
  if r_changed_since_head { change_r_since_head (case . path ()) ?; }
  with_git_fixture (
    case . path (), OwnedSkgrepos::Named (&["public", "private"]), &[],
    |f| {
      let all : SkgrepoRestriction =
        SkgrepoRestriction::named (&f . config, "all" . into ()) ?;
      let mut restriction : SkgrepoRestriction = all . clone ();
      let opened : String = open (f) ?;
      let files_before : BTreeMap<PathBuf, String> = skg_files_on_disk (f) ?;
      let narrowed : String = switch_skgrepo_set (f, &mut restriction, "public", view) ? . 0;
      // pc, a member of r; s, of its subscribee folder; o, of its
      // overridden folder. (The hidden folder, write-protected, reads
      // no claims: its member s1 stays, unwarranted, with its chain.)
      for member in [ "** (skg node (gnid pc)", "*** (skg node (gnid s)",
                      "*** (skg node (gnid o)" ] {
        assert! ( ! without_viewnode_ids (&narrowed) . lines ()
                    . any ( |line| line . starts_with (member) ),
                  "narrowed, {} vanishes:\n{}", member, narrowed ); }
      between (f, &restriction) ?;
      let widened : String = switch_skgrepo_set (f, &mut restriction, "all", view) ? . 0;
      assert_eq! ( without_viewnode_ids (&widened), without_viewnode_ids (&opened),
                   "widened, the view is as it was opened" );
      let saved : SaveResponse = save_view_cleanly_under (f, view, &widened, &all) ?;
      assert_eq! ( saved . saved_view, widened, "saved unchanged, it comes back as it went" );
      let files_after : BTreeMap<PathBuf, String> = skg_files_on_disk (f) ?;
      let but_o = |files : &BTreeMap<PathBuf, String>| -> BTreeMap<PathBuf, String> {
        files . iter ()
        . filter ( |(path, _)| ! path . ends_with ("o.skg") )
        . map ( |(path, text)| ( path . clone (), text . clone () ))
        . collect () };
      assert_eq! ( but_o (&files_after), but_o (&files_before),
                   "every file of r, and every other but o's, is as it was" );
      Ok (( )) } ) }

/// The text of the open view 'view', as the server holds it.
fn rendered_view (
  f    : &Fixture,
  view : &str,
) -> Result<String, Box<dyn Error>> {
  Ok ( viewforest_to_string (
    f . views_state . open_views
    . viewforest_id_to_view ( &ViewforestId::ContentView (view . into ()) )
    . ok_or ("the view is open") ? ) ? ) }

/// Save 'text' as 'view' under 'restriction'; it must save without
/// errors.
fn save_view_cleanly_under (
  f           : &mut Fixture,
  view        : &str,
  text        : &str,
  restriction : &SkgrepoRestriction,
) -> Result<SaveResponse, Box<dyn Error>> {
  let response : SaveResponse = save_view_under (f, view, text, restriction) ?;
  assert! ( response . errors . is_empty (),
            "the buffer must save without errors, got {:?}\n\nbuffer:\n{}",
            response . errors, text );
  Ok (response) }

/// r hides s1 and hh, private, from its subscribers, recording both in
/// its public file. Under "public", with the folder of what r hides
/// requested, hh stands as a restricted headline there and in the
/// folder of what r hides outside its subscribees. Its line deleted
/// from the second and the view saved, r no longer hides hh, and the
/// first folder's restricted headline goes.
#[test]
fn a_restricted_headline_whose_record_a_save_removed_goes_quietly (
) -> Result<(), Box<dyn Error>> {
  let case : tempfile::TempDir = tempfile::TempDir::new () ?;
  write_unchanged_skgrepos ( case . path (), &[
    ( "public", &[
      ("r",  "pid: r\ntitle: r\ncontains:\n- c1\nsubscribesTo:\n- s\nhidesFromSubs:\n- s1\n- hh\n"),
      ("c1", "pid: c1\ntitle: c1\n"),
      ("s",  "pid: s\ntitle: s\ncontains:\n- s1\n"),
      ("s1", "pid: s1\ntitle: s1\n") ] ),
    ( "private", &[
      ("hh", "pid: hh\ntitle: hh secret\n") ] ) ]) ?;
  with_git_fixture (
    case . path (), OwnedSkgrepos::Named (&["public", "private"]), &[],
    |f| {
      let public : SkgrepoRestriction =
        SkgrepoRestriction::named (&f . config, "public" . into ()) ?;
      let opened : String = open_view_of_r (f, &public) ?;
      let requested : String = save_under (
        f, &opened . replacen (
          "node (gnid r)", "node (gnid r) (viewRequests (folder hidesFromSubs))", 1 ),
        &public ) ? . saved_view;
      let hh_lines : Vec<usize> = requested . lines () . enumerate ()
        . filter ( |(_, line)| line . contains ("restricted (gnid hh)") )
        . map ( |(index, _)| index )
        . collect ();
      assert_eq! ( hh_lines . len (), 2,
                   "hh stands in both folders:\n{}", requested );
      assert_eq! ( children_of_the_hidden_outside_folder (&requested),
                   vec! [ "restricted hh" . to_string () ],
                   "{}", requested );
      let edited : String = requested . lines () . enumerate ()
        . filter ( |(index, _)| *index != hh_lines [1] )
        . map ( |(_, line)| format! ("{}\n", line) )
        . collect ();
      let saved : SaveResponse = save_under (f, &edited, &public) ?;
      assert_eq! ( members_of ( node_in_graph (f, "r") . hidesFromSubs . or_default () ),
                   vec! [ ID::from ("s1") ],
                   "r no longer hides hh" );
      assert! ( ! saved . saved_view . contains ("(gnid hh)"),
                "hh's restricted headline goes:\n{}", saved . saved_view );
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
        open_content_view (f, "r", "fresh view of r", &public) ?;
      assert! ( fresh_under_public . contains ("(props (aliases 1))")
                && ! fresh_under_public . contains ("secret"),
                "a fresh view under public counts r's public alias alone:\n{}",
                fresh_under_public );
      let unrestricted : SkgrepoRestriction =
        SkgrepoRestriction::unrestricted (&f . config);
      let opened : String = open_view_of_r (f, &unrestricted) ?;
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

/// A view rooted at h, which is private, drawn under "all" and
/// switched to "public": its root is a restricted headline. Saved as
/// it is, under "public", it saves, and nothing on disk changes. A
/// headline typed under the tombstone the skgrepo narrowing left in place of
/// h's containers folder has no occurrence above it to take a skgrepo
/// from, so the save refuses it for want of one, and nothing on disk
/// changes either.
#[test]
fn a_save_of_a_view_whose_root_is_a_restricted_headline_changes_nothing (
) -> Result<(), Box<dyn Error>> {
  let case : tempfile::TempDir = tempfile::TempDir::new () ?;
  write_fixture (case . path ()) ?;
  with_git_fixture (
    case . path (), OwnedSkgrepos::Named (&["public", "private"]), &[],
    |f| {
      let mut restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (&f . config, "all" . into ()) ?;
      open_content_view (f, "h", VIEW_OF_H, &restriction) ?;
      let narrowed : String =
        switch_skgrepo_set (f, &mut restriction, "public", VIEW_OF_H) ? . 0;
      assert_eq! ( narrowed, VIEW_OF_H_SWITCHED_TO_PUBLIC );
      let before : BTreeMap<PathBuf, String> = skg_files_on_disk (f) ?;
      let saved : SaveResponse =
        save_view_under (f, VIEW_OF_H, &narrowed, &restriction) ?;
      assert! ( saved . errors . is_empty (), "{:?}", saved . errors );
      assert_eq! ( saved . saved_view, narrowed,
                   "the saved view is the view as it was" );
      assert_eq! ( skg_files_on_disk (f) ?, before,
                   "the save of the restricted root changes nothing on disk" );
      let typed : String = narrowed . replacen (
        ") r\n", ") r\n*** typed under the tombstone\n", 1 );
      assert_ne! ( typed, narrowed );
      let refusal : String =
        match save_view_under (f, VIEW_OF_H, &typed, &restriction) {
          Ok (response) => format! ("{:?}", response . errors),
          Err (error)   => format! ("{:?}", error), };
      assert! ( refusal . contains ("Occurrence must have a repo that exists in the config.")
                && refusal . contains ("typed under the tombstone"),
                "a headline with no occurrence above it is refused for want of a skgrepo: {}",
                refusal );
      assert_eq! ( skg_files_on_disk (f) ?, before,
                   "the refused save changes nothing on disk" );
      Ok (( )) } ) }

/// r (public) subscribes to s and hides s1 and 'nowhere' from its
/// subscribers, a hide of a gnid that resolves to no node, recorded in
/// r's public file. Under "all" and under "public", the folder of what
/// r hides, once requested, holds a reference to the unknown node
/// 'nowhere' beside s1; the buffer saved unchanged saves, and r keeps
/// both hides.
#[test]
fn a_requested_hidden_folder_holding_a_dangling_hide_saves (
) -> Result<(), Box<dyn Error>> {
  let case : tempfile::TempDir = tempfile::TempDir::new () ?;
  write_unchanged_skgrepos ( case . path (), &[
    ( "public", &[
      ("r",  "pid: r\ntitle: r\ncontains:\n- c1\nsubscribesTo:\n- s\nhidesFromSubs:\n- s1\n- nowhere\n"),
      ("c1", "pid: c1\ntitle: c1\n"),
      ("s",  "pid: s\ntitle: s\ncontains:\n- s1\n"),
      ("s1", "pid: s1\ntitle: s1\n") ] ),
    ( "private", &[
      ("hh", "pid: hh\ntitle: hh secret\n") ] ) ]) ?;
  with_git_fixture (
    case . path (), OwnedSkgrepos::Named (&["public", "private"]), &[],
    |f| {
      for name in ["all", "public"] {
        let restriction : SkgrepoRestriction =
          SkgrepoRestriction::named (&f . config, name . into ()) ?;
        let opened : String = open_view_of_r (f, &restriction) ?;
        let requested : String = save_under (
          f, &opened . replacen (
            "node (gnid r)", "node (gnid r) (viewRequests (folder hidesFromSubs))", 1 ),
          &restriction ) ? . saved_view;
        let hidden_folder_children : Vec<&str> = requested . lines ()
          . skip_while ( |line| ! line . contains ("hiddenFolder") )
          . skip (1)
          . take_while ( |line| line . starts_with ("***") )
          . filter ( |line| line . starts_with ("*** ") )
          . collect ();
        assert! ( hidden_folder_children . len () == 2
                  && hidden_folder_children [0] . contains ("node (gnid s1)")
                  && hidden_folder_children [1] . contains (" unknown (gnid nowhere))"),
                  "under {} the hidden folder holds s1 and the unknown 'nowhere':\n{}",
                  name, requested );
        let saved : SaveResponse = save_under (f, &requested, &restriction) ?;
        assert_eq! ( saved . saved_view, requested,
                     "under {} the buffer saves as it was", name );
        assert_eq! ( members_of ( node_in_graph (f, "r") . hidesFromSubs . or_default () ),
                     vec! [ ID::from ("s1"), ID::from ("nowhere") ],
                     "under {} r keeps both hides", name ); }
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
  open_view_of_r (f, &restriction) ?;
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
  restriction : &SkgrepoRestriction,
) -> Result<String, Box<dyn Error>> {
  open_content_view (f, "r", VIEW, restriction) }

/// Save 'text' as VIEW under 'restriction'; it must save without
/// errors.
fn save_under (
  f           : &mut Fixture,
  text        : &str,
  restriction : &SkgrepoRestriction,
) -> Result<SaveResponse, Box<dyn Error>> {
  let response : SaveResponse = save_view_under (f, VIEW, text, restriction) ?;
  assert! ( response . errors . is_empty (),
            "the buffer must save without errors, got {:?}\n\nbuffer:\n{}",
            response . errors, text );
  Ok (response) }

/// Save 'text' as 'view' under 'restriction', whatever the response.
fn save_view_under (
  f           : &mut Fixture,
  view        : &str,
  text        : &str,
  restriction : &SkgrepoRestriction,
) -> Result<SaveResponse, Box<dyn Error>> {
  save_under_restriction (
    f, view, text, restriction, true, DeleteFrom::NotChosen ) }

/// Every '.skg' file of every skgrepo, by path, with its text.
fn skg_files_on_disk (
  f : &Fixture,
) -> Result<BTreeMap<PathBuf, String>, Box<dyn Error>> {
  let mut files : BTreeMap<PathBuf, String> = BTreeMap::new ();
  for skgrepo in f . config . skgrepos . values () {
    for entry in std::fs::read_dir (&skgrepo . path) ? {
      let path : PathBuf = entry ? . path ();
      if path . extension () . is_some_and ( |extension| extension == "skg" ) {
        files . insert ( path . clone (), std::fs::read_to_string (&path) ? ); }}}
  Ok (files) }

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
/// and the "rerender-done" message.
fn switch_reporting (
  f           : &mut Fixture,
  restriction : &mut SkgrepoRestriction,
  name        : &str,
) -> Result<(String, String), Box<dyn Error>> {
  switch_skgrepo_set (f, restriction, name, VIEW) }
