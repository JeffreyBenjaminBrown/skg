// cargo nextest run --test grouped_saves -E 'test(merge_with_a_note_it_forks)'
//
// A nodeMerge of a note with a note it forks saves, and the merged
// note neither forks nor hides itself: 'acquirer_after_nodeMerge'
// drops the merged note's own IDs from its 'forks' and 'hides', as from
// its contents.
//
// Fixture graph (all owned, at 'main'):
//   a forks b and hides b;
//   b contains x and forks a (a mutual pair of forks).
// The buffer merges b into a.

use indoc::indoc;
use std::error::Error;
use crate::tcp_stream_pair::mk_test_tcp_stream_pair;
use std::net::TcpStream;

use skg::dbs::filesystem::one_node::graphnode_from_pid_and_skgrepo;
use skg::test_utils::{run_with_test_stores, graph_handle_from_config, audit_inrustgraph_or_panic};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::serve::ViewsState;
use skg::types::views_state::OpenViews;

use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::types::config::SkgConfig;
use skg::types::misc::{ID, TantivyIndex, SkgrepoName, members_of};
use skg::types::nodes::complete::Graphnode;

#[test]
fn test_merge_with_a_note_it_forks
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-merge-with-a-note-it-forks",
    "tests/merge/merge_with_a_note_it_forks/fixtures",
    "/tmp/tantivy-test-merge-with-a-note-it-forks",
    |config, tantivy| Box::pin ( async move {
      merge_with_a_note_it_forks_impl (
        config, tantivy ) . await
    } )) }

async fn merge_with_a_note_it_forks_impl (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let input_org_text : &str = indoc! {"
    * (skg node (gnid a) (skgRepos (nodeRepo main)) (editRequest (merge b))) a
  "};
  let graph : InRustGraphHandle =
    graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = ViewsState {
    open_views     : OpenViews::new (),
    graph_straddle : None, };
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response = update_from_and_rerender_buffer (
    &mut stream, input_org_text, config, tantivy, &graph,
    &Err ( String::new () ), &mut views_state ) . await ?;
  assert! ( response . errors . is_empty (),
            "the merge must save: {:?}", response . errors );
  let a : Graphnode = graphnode_from_pid_and_skgrepo (
    config, ID::from ("a"), &SkgrepoName::from ("main") ) ?;
  assert! ( a . extra_ids . contains (&ID::from ("b")),
            "a acquired b's ID: {:?}", a . extra_ids );
  let own_ids : [ID; 2] = [ ID::from ("a"), ID::from ("b") ];
  for (relation, members) in [
    ("forks",         members_of ( a . forks . or_default () )),
    ("hides", members_of ( a . hides . or_default () )),
    ("contains",      members_of ( &a . contains )),
  ] {
    assert! ( ! members . iter () . any ( |m| own_ids . contains (m) ),
              "a's {} names one of its own IDs: {:?}", relation, members ); }
  assert! ( members_of ( &a . contains ) . contains (&ID::from ("x")),
            "a adopted b's content x: {:?}", a . contains );
  audit_inrustgraph_or_panic (&graph) ?;
  Ok (( )) }
