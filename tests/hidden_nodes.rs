// cargo nextest run --test grouped_repos -E 'test(hidden_nodes::)'
//
// A node is hidden when the skgrepo restriction excludes its home
// skgrepo ('docs/api-and-formats.org', "Restricted-skgrepo nodes").
// A save under a restriction keeps the hidden members of the nodes
// it writes, which disk supplementation owns; and a tightened
// restriction leaves no trace of a hidden node in an open view but a
// tombstone over its children.
//
// The fixture, written by the test:
//   public  : r contains a, h, b and subscribes to hs; a; b; s1; hv.
//   private : h ("h secret", aliased "h alias secret") contains s1;
//             hs ("hs secret"); hh ("hh secret").
// Some tests have r hide hh, or hh and hv, from its subscribers.

use crate::git_fixture::{Fixture, OwnedSkgrepos, with_git_fixture};
use crate::git_fixture_helpers::mk_test_tcp_stream_pair;

use skg::from_text::strandeds::StrandedsReview;
use skg::serve::handlers::save_buffer::{
  SaveResponse, update_from_and_rerender_buffer_with_approvals};
use skg::serve::handlers::skgrepo_sets::handle_skgrepo_set_request;
use skg::serve::handlers::text_search::SearchEnrichmentPayload;
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::test_utils::{extract_string_field_from_sexp, read_all_lp_messages, skg_env_from_parts};
use skg::to_org::render::content_view::{multi_root_view, multi_root_view_with_skgrepo_set};
use skg::types::env::SkgEnv;
use skg::types::misc::{ID, members_of};
use skg::types::nodes::rust::GraphnodeInRust;
use skg::types::viewnode::Viewnode;
use skg::types::views_state::ViewforestId;

use ego_tree::Tree;
use futures::executor::block_on;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fs;
use std::io::BufReader;
use std::net::TcpStream;
use std::path::Path;
use std::sync::atomic::AtomicBool;
use std::sync::{Arc, Mutex};

const VIEW : &str = "view of r";

/// Two saves under the restriction "public", each of which writes r:
/// one of a view drawn under it, where the hidden members have no
/// headline at all, and one of a view drawn under "all" and then
/// switched to "public". Both keep r's hidden members, in place.
#[test]
fn a_save_under_a_restriction_keeps_the_hidden_members (
) -> Result<(), Box<dyn Error>> {
  let case : tempfile::TempDir = tempfile::TempDir::new () ?;
  write_fixture (case . path ()) ?;
  with_git_fixture (
    case . path (), OwnedSkgrepos::Named (&["public", "private"]), &[],
    |f| {
      let public : SkgrepoRestriction =
        SkgrepoRestriction::named (&f . config, "public" . into ()) ?;
      { // drawn under "public": no headline for h or hs
        let opened : String = open_view_of_r (f, Some (&public)) ?;
        assert_no_trace_of_the_hidden_nodes ("the view drawn under public", &opened);
        let edited : String = opened
          . replacen ( ")) r\n", ")) r edited\n** a typed child\n", 1 );
        assert_ne! ( edited, opened );
        let saved : SaveResponse = save_under (f, &edited, &public) ?;
        assert_no_trace_of_the_hidden_nodes ("the saved view", &saved . saved_view);
        let r : GraphnodeInRust = node_in_graph (f, "r");
        assert_eq! ( r . title, "r edited" );
        let contains : Vec<ID> = members_of (&r . contains);
        assert_eq! ( contains . len (), 4,
                     "r contains the typed child and its three members: {:?}",
                     contains );
        assert_eq! ( contains [1 ..] . to_vec (),
                     vec! [ ID::from ("a"), ID::from ("h"), ID::from ("b") ],
                     "the hidden member h stays after its anchor a" );
        assert_eq! ( members_of ( r . subscribesTo . or_default () ),
                     vec! [ ID::from ("hs") ],
                     "the hidden subscribee hs stays" ); }
      { // drawn under "all", then switched to "public"
        let mut restriction : SkgrepoRestriction =
          SkgrepoRestriction::named (&f . config, "all" . into ()) ?;
        open_view_of_r (f, None) ?;
        let switched : String = switch (f, &mut restriction, "public") ?;
        assert_no_trace_of_the_hidden_nodes ("the switched view", &switched);
        let edited : String = switched
          . replacen ( ")) r edited\n", ")) r edited again\n", 1 );
        assert_ne! ( edited, switched );
        save_under (f, &edited, &public) ?;
        let r : GraphnodeInRust = node_in_graph (f, "r");
        assert_eq! ( r . title, "r edited again" );
        assert_eq! ( members_of (&r . contains) [1 ..] . to_vec (),
                     vec! [ ID::from ("a"), ID::from ("h"), ID::from ("b") ],
                     "the hidden member h stays after its anchor a" );
        assert_eq! ( members_of ( r . subscribesTo . or_default () ),
                     vec! [ ID::from ("hs") ],
                     "the hidden subscribee hs stays" ); }
      Ok (( )) } ) }

/// r hides hh (private) and hv (public) from its subscribers. A view
/// of r drawn under "all" and switched to "public" keeps the folder of
/// what r hides outside its subscribees, holding hv alone; saved with
/// an edit, it leaves r's hide of hh, which it could not show.
#[test]
fn a_save_under_a_restriction_keeps_the_restricted_hides (
) -> Result<(), Box<dyn Error>> {
  let case : tempfile::TempDir = tempfile::TempDir::new () ?;
  write_fixture_where_r_hides (case . path (), &["hh", "hv"]) ?;
  with_git_fixture (
    case . path (), OwnedSkgrepos::Named (&["public", "private"]), &[],
    |f| {
      let public : SkgrepoRestriction =
        SkgrepoRestriction::named (&f . config, "public" . into ()) ?;
      let mut restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (&f . config, "all" . into ()) ?;
      open_view_of_r (f, None) ?;
      let switched : String = switch (f, &mut restriction, "public") ?;
      assert_no_trace_of_the_hidden_nodes ("the switched view", &switched);
      assert! ( switched . contains ("hiddenOutsideOfSubscribeeFolder")
                && switched . contains ("(gnid hv)"),
                "the folder of r's hides holds hv:\n{}", switched );
      let edited : String = switched . replacen ( ")) r\n", ")) r edited\n", 1 );
      assert_ne! ( edited, switched );
      save_under (f, &edited, &public) ?;
      let r : GraphnodeInRust = node_in_graph (f, "r");
      assert_eq! ( r . title, "r edited" );
      assert_eq! ( members_of ( r . hidesFromSubs . or_default () ),
                   vec! [ ID::from ("hh"), ID::from ("hv") ],
                   "the save keeps the hide of hh, which it could not show" );
      Ok (( )) } ) }

/// A view of r drawn under "all", switched to "public": where h stood
/// is a tombstone holding s1, and nothing names h or shows its text.
/// Switched back to "all", the view draws h again beside the
/// tombstone.
#[test]
fn a_tightened_restriction_leaves_a_tombstone_over_the_hidden_nodes_children (
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
      assert_no_trace_of_the_hidden_nodes ("the switched view", &switched);
      assert! ( ! switched . contains ("restrictedNode"),
                "no restricted placeholder:\n{}", switched );
      let headlines : Vec<&str> = switched . lines ()
        . filter ( |line| line . starts_with ('*') ) . collect ();
      let tombstone : usize = headlines . iter ()
        . position ( |line| line . starts_with ("** ")
                            && line . contains (" tombstone") )
        . unwrap_or_else ( || panic! (
          "a tombstone stands where h stood:\n{}", switched ));
      assert! ( headlines . get (tombstone + 1) . is_some_and (
                  |line| line . starts_with ("*** ")
                         && line . contains ("(gnid s1)") ),
                "the tombstone holds s1:\n{}", switched );
      { // switching back draws h again; the tombstone, which says
        // nothing of what it stood for, stays beside it with s1
        let back : String = switch (f, &mut restriction, "all") ?;
        assert! ( back . contains (") h secret\n"),
                  "the switch back draws h:\n{}", back );
        assert! ( back . contains (" tombstone"),
                  "the tombstone stays:\n{}", back ); }
      Ok (( )) } ) }

/// The exact buffers of a view of r switched to "public" and back to
/// "all": narrowed, the subscribee folder the restriction empties
/// stays, empty, and h's occurrence is a tombstone holding s1;
/// widened, the folder holds hs again and h is drawn again beside the
/// tombstone.
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
      let narrowed : String = switch (f, &mut restriction, "public") ?;
      let widened : String = switch (f, &mut restriction, "all") ?;
      assert_eq! ( narrowed, SWITCHED_TO_PUBLIC );
      assert_eq! ( widened, SWITCHED_BACK_TO_ALL );
      Ok (( )) } ) }

/// As above, where r also hides hh, private, from its subscribers:
/// narrowed, the subscribee folder and the hidden-outside folder in it
/// stay, both emptied; widened, they hold hs and hh again.
#[test]
fn a_switch_keeps_the_folders_the_restriction_empties (
) -> Result<(), Box<dyn Error>> {
  let case : tempfile::TempDir = tempfile::TempDir::new () ?;
  write_fixture_where_r_hides (case . path (), &["hh"]) ?;
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
*** (skg (vnid 0d) hiddenOutsideOfSubscribeeFolder)
** (skg (vnid 0f) node (gnid a) (skgRepos (nodeRepo public)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) a
** (skg (vnid 0g) tombstone)
*** (skg (vnid 0h) node (gnid s1) (skgRepos (nodeRepo public))) s1
** (skg (vnid 0i) node (gnid b) (skgRepos (nodeRepo public)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) b
" );
      assert_eq! ( widened, "\
* (skg (vnid 0a) node (gnid r) (skgRepos (nodeRepo public)) (view (rels (contains (out 3)) (subscribesTo (out 1)) (hidesFromSubs (out 1))) repoBoundary)) r
** (skg (vnid 0b) subscribeeFolder)
*** (skg (vnid 0j) node (gnid hs) (skgRepos (nodeRepo private) (relRepo public)) writeProtected (view (rels (contains (out 0 (unintegrated 0))) (subscribesTo (in 1 (ancestors 2)))) (birth (subscribesTo in 2)) repoBoundary)) hs secret
*** (skg (vnid 0d) hiddenOutsideOfSubscribeeFolder)
**** (skg (vnid 0k) node (gnid hh) (skgRepos (nodeRepo private)) writeProtected (view (rels (hidesFromSubs (in 1 (ancestors 3)))) (birth (hidesFromSubs in 3)) repoBoundary)) hh secret
** (skg (vnid 0f) node (gnid a) (skgRepos (nodeRepo public)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) a
** (skg (vnid 0g) tombstone)
*** (skg (vnid 0h) node (gnid s1) (skgRepos (nodeRepo public)) (view (rels (contains (in 1))))) s1
** (skg (vnid 0l) node (gnid h) (skgRepos (nodeRepo private) (relRepo public)) (view (rels (contains (in 1 (ancestors 1)) (out 1))) (props (aliases 1)) (birth (contains in 1)) repoBoundary)) h secret
*** (skg (vnid 0m) node (gnid s1) (skgRepos (nodeRepo public)) writeProtected (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)) repoBoundary)) s1
** (skg (vnid 0i) node (gnid b) (skgRepos (nodeRepo public)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) b
" );
      Ok (( )) } ) }

const SWITCHED_TO_PUBLIC : &str = "\
* (skg (vnid 0a) node (gnid r) (skgRepos (nodeRepo public)) (view (rels (contains (out 2))) repoBoundary)) r
** (skg (vnid 0b) subscribeeFolder)
** (skg (vnid 0d) node (gnid a) (skgRepos (nodeRepo public)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) a
** (skg (vnid 0e) tombstone)
*** (skg (vnid 0f) node (gnid s1) (skgRepos (nodeRepo public))) s1
** (skg (vnid 0g) node (gnid b) (skgRepos (nodeRepo public)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) b
";

const SWITCHED_BACK_TO_ALL : &str = "\
* (skg (vnid 0a) node (gnid r) (skgRepos (nodeRepo public)) (view (rels (contains (out 3)) (subscribesTo (out 1))) repoBoundary)) r
** (skg (vnid 0b) subscribeeFolder)
*** (skg (vnid 0h) node (gnid hs) (skgRepos (nodeRepo private) (relRepo public)) writeProtected (view (rels (contains (out 0 (unintegrated 0))) (subscribesTo (in 1 (ancestors 2)))) (birth (subscribesTo in 2)) repoBoundary)) hs secret
** (skg (vnid 0d) node (gnid a) (skgRepos (nodeRepo public)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) a
** (skg (vnid 0e) tombstone)
*** (skg (vnid 0f) node (gnid s1) (skgRepos (nodeRepo public)) (view (rels (contains (in 1))))) s1
** (skg (vnid 0i) node (gnid h) (skgRepos (nodeRepo private) (relRepo public)) (view (rels (contains (in 1 (ancestors 1)) (out 1))) (props (aliases 1)) (birth (contains in 1)) repoBoundary)) h secret
*** (skg (vnid 0j) node (gnid s1) (skgRepos (nodeRepo public)) writeProtected (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)) repoBoundary)) s1
** (skg (vnid 0g) node (gnid b) (skgRepos (nodeRepo public)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) b
";

/// A switch's rerender reports no buffer corrections: narrowed, the
/// tombstone over s1 is the restriction's doing, and widened, h drawn
/// again is too ('BufferCorrectionPolicy').
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

fn assert_no_trace_of_the_hidden_nodes (
  what   : &str,
  buffer : &str,
) {
  for trace in ["secret", "(gnid h)", "(gnid hs)"] {
    assert! ( ! buffer . contains (trace),
              "{} shows {:?}:\n{}", what, trace, buffer ); }}

//
// The fixture and the operations
//

fn write_fixture (
  dir : &Path,
) -> Result<(), Box<dyn Error>> {
  write_fixture_where_r_hides (dir, &[]) }

/// The fixture, with r hiding 'hides' from its subscribers. Beside it
/// stand hh ("hh secret", private) and hv (public), which r may hide.
fn write_fixture_where_r_hides (
  dir   : &Path,
  hides : &[&str],
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
  let private : &[(&str, &str)] = &[
    ("h",  "pid: h\ntitle: h secret\naliases:\n- h alias secret\ncontains:\n- s1\n"),
    ("hs", "pid: hs\ntitle: hs secret\n"),
    ("hh", "pid: hh\ntitle: hh secret\n"), ];
  for (skgrepo, files) in [("public", public), ("private", private)] {
    for side in ["head", "worktree"] {
      let folder = dir . join (skgrepo) . join (side);
      fs::create_dir_all (&folder) ?;
      for (name, text) in files {
        fs::write ( folder . join (format! ("{}.skg", name)), text ) ?; }}}
  Ok (( )) }

/// Draw r, under 'restriction' when one is given, and register the
/// view as VIEW.
fn open_view_of_r (
  f           : &mut Fixture,
  restriction : Option<&SkgrepoRestriction>,
) -> Result<String, Box<dyn Error>> {
  let roots : [ID; 1] = [ ID::from ("r") ];
  let (text, pids, tree) : (String, Vec<ID>, Tree<Viewnode>) =
    match restriction {
      Some (restriction) => multi_root_view_with_skgrepo_set (
        &f . config, Some (&f . tantivy), &roots, restriction ) ?,
      None => multi_root_view (&f . config, Some (&f . tantivy), &roots) ?, };
  f . views_state . open_views . register_view (
    &f . graph . load_full (), ViewforestId::ContentView (VIEW . into ()),
    tree, &pids );
  Ok (text) }

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
  Ok ( switch_reporting (f, restriction, name) ? . 0 ) }

/// Switch to the skgrepo-set 'name'; return the view as rerendered,
/// and the "rerender-done" message, which carries the buffer
/// corrections.
fn switch_reporting (
  f           : &mut Fixture,
  restriction : &mut SkgrepoRestriction,
  name        : &str,
) -> Result<(String, String), Box<dyn Error>> {
  let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
  let enrichment_slot : Arc<Mutex<Option<SearchEnrichmentPayload>>> =
    Arc::new (Mutex::new (None));
  let search_cancelled : Arc<AtomicBool> = Arc::new (AtomicBool::new (false));
  let (mut stream, read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  handle_skgrepo_set_request (
    &mut stream,
    &format! ("((request . \"set skgrepo restriction\") (name . \"{}\"))\n", name),
    &env, &mut f . views_state, restriction,
    &enrichment_slot, &search_cancelled );
  drop (stream);
  let messages : Vec<String> = read_all_lp_messages ( &mut BufReader::new (read_end) );
  let rerendered : Option<String> = messages . iter ()
    . find ( |message| message . contains ("rerender-view")
                       && message . contains (&format! ("(view-id \"{}\")", VIEW)) )
    . and_then ( |message| extract_string_field_from_sexp (
      &message [ message . find ('(') ? .. ], "content" ));
  let done : String = messages . iter ()
    . find ( |message| message . contains ("rerender-done") )
    . cloned ()
    . unwrap_or_else ( || panic! ( "the switch to {} ends: {:?}", name, messages ));
  Ok (( rerendered . unwrap_or_else (
          || panic! ( "the switch to {} rerenders the view: {:?}", name, messages )),
        done )) }
