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
//   public  : r contains a, h, b and subscribes to hs; a; b; s1.
//   private : h ("h secret", aliased "h alias secret") contains s1;
//             hs ("hs secret").

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
  let public : &[(&str, &str)] = &[
    ("r",  "pid: r\ntitle: r\ncontains:\n- a\n- h\n- b\nsubscribesTo:\n- hs\n"),
    ("a",  "pid: a\ntitle: a\n"),
    ("b",  "pid: b\ntitle: b\n"),
    ("s1", "pid: s1\ntitle: s1\n"), ];
  let private : &[(&str, &str)] = &[
    ("h",  "pid: h\ntitle: h secret\naliases:\n- h alias secret\ncontains:\n- s1\n"),
    ("hs", "pid: hs\ntitle: hs secret\n"), ];
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
  Ok ( rerendered . unwrap_or_else (
    || panic! ( "the switch to {} rerenders the view: {:?}", name, messages )) ) }
