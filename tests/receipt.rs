// cargo nextest run --test grouped_forks -E 'test(receipt::)'
//
// The read receipt ('docs/forks.org', "Read receipts"), through its
// request's handler: one request writes a note under the reader's
// fork of a mailed item, forking it first if need be, and hides the
// item from the reader's fork of its mailbox.
//
// Fixture: the reader owns 'reader-public' and 'reader-private'; the
// sender's 'sender-public' and 'sender-private' are foreign. The
// sender's mailbox 't' holds m1 (with q1), m2, m3, m4, mp (homed in
// 'sender-private') and the reader's own m2y, which the sender forked
// into the reply m2x. The sender's 'u' holds m3 too. The reader's 'f'
// forks 't' and already hides m4; 'g' forks 'u'; 'm2r' forks m2. The
// note n is in no mailbox.

use crate::tcp_stream_pair::mk_test_tcp_stream_pair;
use std::error::Error;
use std::io::BufReader;
use std::net::TcpStream;

use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_skgrepos;
use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::serve::ViewsState;
use skg::serve::handlers::receipt::handle_receipt_request;
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::test_utils::{
  empty_views_state,
  graph_handle_from_config, read_lp_message, run_with_shared_test_stores,
  single_root_view, skg_env_from_parts};
use skg::types::env::SkgEnv;
use skg::types::config::SkgConfig;
use skg::types::misc::{ID, SkgrepoName, TantivyIndex, members_of};
use skg::types::nodes::complete::Graphnode;
use skg::types::views_state::ViewforestId;

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str = "tests/receipt/fixtures";
  run_with_shared_test_stores (
    "skg-test-receipt",
    |s| Box::pin ( async move {
      s . reset ("a_receipt_previews_then_forks_the_item_and_hides_it", fixtures) ?;
      a_receipt_previews_then_forks_the_item_and_hides_it (
        &s . config, &s . tantivy ) ?;
      s . reset ("a_receipt_goes_under_the_fork_the_reader_holds", fixtures) ?;
      a_receipt_goes_under_the_fork_the_reader_holds (&s . config, &s . tantivy) ?;
      s . reset ("an_item_in_two_mailboxes_needs_one_named", fixtures) ?;
      an_item_in_two_mailboxes_needs_one_named (&s . config, &s . tantivy) ?;
      s . reset ("an_item_in_no_mailbox_is_forked_where_the_reader_says", fixtures) ?;
      an_item_in_no_mailbox_is_forked_where_the_reader_says (&s . config, &s . tantivy) ?;
      s . reset ("an_item_already_hidden_still_gets_its_receipt", fixtures) ?;
      an_item_already_hidden_still_gets_its_receipt (&s . config, &s . tantivy) ?;
      s . reset ("an_item_more_private_than_its_mailbox_suggests_no_skgrepo", fixtures) ?;
      an_item_more_private_than_its_mailbox_suggests_no_skgrepo (&s . config, &s . tantivy) ?;
      s . reset ("a_receipt_for_a_reply_hides_the_message_it_forks", fixtures) ?;
      a_receipt_for_a_reply_hides_the_message_it_forks (&s . config, &s . tantivy) ?;
      s . reset ("a_receipt_for_a_note_of_ones_own_goes_under_it", fixtures) ?;
      a_receipt_for_a_note_of_ones_own_goes_under_it (&s . config, &s . tantivy) ?;
      Ok (( )) } )) }

/// The handler's environment and a connection's views.
struct Session {
  env         : SkgEnv,
  views_state : ViewsState,
  restriction : SkgrepoRestriction,
}

fn session (
  config  : &SkgConfig,
  tantivy : &TantivyIndex,
  views   : &[(&str, &str)], // (view ID, root)
) -> Result<Session, Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let restriction : SkgrepoRestriction = SkgrepoRestriction::default_from_config (config) ?;
  let mut views_state : ViewsState = empty_views_state ();
  for (viewforest_id, root) in views {
    let (_text, pids, tree) = single_root_view (config, Some (tantivy), &ID::from (*root)) ?;
    views_state . open_views . register_view (
      &graph . load_full (), ViewforestId::ContentView (viewforest_id . to_string ()),
      tree, &pids, &restriction ); }
  Ok ( Session { env : skg_env_from_parts (config, tantivy, &graph),
                 views_state, restriction } ) }

/// Send the receipt request with these fields; return every message
/// the handler sent.
fn receipt (
  session : &mut Session,
  fields  : &[(&str, &str)],
) -> Vec<String> {
  let request : String = format! (
    "((request . \"receipt\"){})",
    fields . iter ()
      . map ( |(key, value)| format! ( " ({} . \"{}\")", key, value ) )
      . collect::<String> () );
  let (mut server, client) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  std::thread::scope ( |scope| {
    scope . spawn ( || handle_receipt_request (
      &mut server, &request, &mut session . env, &mut session . views_state,
      &session . restriction )); } );
  drop (server);
  let mut reader : BufReader<TcpStream> = BufReader::new (client);
  let mut messages : Vec<String> = Vec::new ();
  while let Ok (message) = read_lp_message (&mut reader) {
    messages . push (message); }
  messages }

fn nodes (
  config : &SkgConfig,
) -> Result<Vec<Graphnode>, Box<dyn Error>> {
  Ok ( read_all_skg_files_from_skgrepos (config) ? ) }

fn node (
  config : &SkgConfig,
  pid    : &str,
) -> Result<Graphnode, Box<dyn Error>> {
  nodes (config) ? . into_iter () . find ( |n| n . pid == ID::from (pid) )
    . ok_or_else ( || format! ("no node {}", pid) . into () ) }

/// The owned notes that fork 'original'.
fn forks_of (
  config   : &SkgConfig,
  original : &str,
) -> Result<Vec<Graphnode>, Box<dyn Error>> {
  Ok ( nodes (config) ? . into_iter ()
       . filter ( |n| config . skgrepo_is_owned (&n . home_skgrepo)
                  && members_of (n . forks . or_default ())
                     . contains (&ID::from (original)) )
       . collect () ) }

fn hides_of (
  config : &SkgConfig,
  pid    : &str,
) -> Result<Vec<ID>, Box<dyn Error>> {
  Ok ( members_of ( node (config, pid) ? . hides . or_default () )) }

/// The new fork of 'original', which the receipt heads.
fn the_receipt_under_the_new_fork_of (
  config   : &SkgConfig,
  original : &str,
) -> Result<(Graphnode, Graphnode), Box<dyn Error>> {
  let forks : Vec<Graphnode> = forks_of (config, original) ?;
  assert_eq! ( forks . len (), 1, "one fork of {}: {:?}", original, forks );
  let fork : Graphnode = forks [0] . clone ();
  let receipt : Graphnode = node (config, &fork . contains [0] . member . 0) ?;
  Ok (( fork, receipt )) }

/// A preview says what it would do and writes nothing; the receipt
/// then forks m1 into the reader's fork of the mailbox's skgrepo
/// (m1 is homed where its mailbox is), heads the fork with the
/// receipt, its text the body, hides m1 in 'f', and redraws the view
/// of 'f' but not the unrelated one.
fn a_receipt_previews_then_forks_the_item_and_hides_it (
  config  : &SkgConfig,
  tantivy : &TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let mut s : Session = session (
    config, tantivy, &[ ("mailbox-fork", "f"), ("unrelated", "p") ]) ?;
  let text : &str = "Done: see [[id:d][maybe done]].";
  let preview : Vec<String> = receipt (
    &mut s, &[ ("item", "m1"), ("text", text), ("preview", "true") ]);
  assert_eq! ( preview . len (), 3, "{:?}", preview );
  assert! ( preview [0] . contains ("receipt") && preview [0] . contains ("(preview true)"),
            "{:?}", preview );
  assert! ( preview [0] . contains ("Forks 'message one' (m1, of 'sender-public') into 'reader-public'"),
            "{:?}", preview );
  assert! ( preview [0] . contains ("Hides 'message one' from 'to reader, as the reader reads it'"),
            "{:?}", preview );
  assert! ( preview [1] . contains ("(lock-views ())"), "{:?}", preview );
  assert! ( forks_of (config, "m1") ? . is_empty (), "a preview writes nothing" );
  assert! ( ! hides_of (config, "f") ? . contains (&ID::from ("m1")) );
  let done : Vec<String> = receipt ( &mut s, &[ ("item", "m1"), ("text", text) ]);
  assert! ( done [0] . contains ("(preview false)"), "{:?}", done );
  assert! ( done [1] . contains ("rerender-lock") && done [1] . contains ("mailbox-fork")
            && ! done [1] . contains ("unrelated"), "{:?}", done );
  assert! ( done . last () . unwrap () . contains ("rerender-done"), "{:?}", done );
  let (fork, receipt_node) = the_receipt_under_the_new_fork_of (config, "m1") ?;
  assert_eq! ( fork . home_skgrepo, SkgrepoName::from ("reader-public") );
  assert_eq! ( members_of (&fork . contains) [1 ..], [ ID::from ("q1") ] );
  assert_eq! ( receipt_node . title, "Read." );
  assert_eq! ( receipt_node . body . as_deref (), Some (text) );
  assert_eq! ( receipt_node . home_skgrepo, SkgrepoName::from ("reader-public") );
  assert! ( done [0] . contains (&receipt_node . pid . 0) && done [0] . contains (&fork . pid . 0),
            "the answer names the receipt and its holder: {:?}", done );
  assert! ( hides_of (config, "f") ? . contains (&ID::from ("m1")) );
  let redrawn : &String = done . iter () . find ( |m| m . contains ("rerender-view") )
    . expect ("the view of f is redrawn");
  let m1_line : &str = redrawn . lines ()
    . find ( |line| line . contains ("(gnid m1)") ) . unwrap_or ("");
  assert! ( m1_line . contains ("(birth (hides in"),
            "f's redrawn forks folder shows m1 among what f hides, not as unread: {}", redrawn );
  Ok (( )) }

/// m2r already forks m2: the receipt becomes its first child, no
/// second fork is made, and m2 is hidden in 'f'.
fn a_receipt_goes_under_the_fork_the_reader_holds (
  config  : &SkgConfig,
  tantivy : &TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let mut s : Session = session (config, tantivy, &[]) ?;
  let done : Vec<String> = receipt (
    &mut s, &[ ("item", "m2"), ("text", "Answered in m2r."), ("title", "Answered.") ]);
  assert! ( done [0] . contains ("your fork of it"), "{:?}", done );
  assert_eq! ( forks_of (config, "m2") ? . len (), 1, "no second fork" );
  let holder : Graphnode = node (config, "m2r") ?;
  assert_eq! ( holder . contains . len (), 1 );
  let receipt_node : Graphnode = node (config, &holder . contains [0] . member . 0) ?;
  assert_eq! ( receipt_node . title, "Answered." );
  assert! ( hides_of (config, "f") ? . contains (&ID::from ("m2")) );
  Ok (( )) }

/// m3 is in 't' and 'u', each of which the reader forks: unnamed, the
/// receipt is refused, listing both, and writes nothing; named by the
/// reader's fork 'g', it hides m3 there and not in 'f'.
fn an_item_in_two_mailboxes_needs_one_named (
  config  : &SkgConfig,
  tantivy : &TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let mut s : Session = session (config, tantivy, &[]) ?;
  let refused : Vec<String> = receipt ( &mut s, &[ ("item", "m3"), ("text", "") ]);
  assert! ( refused [0] . contains ("refusal") && refused [0] . contains ("choose-mailbox")
            && refused [0] . contains ("\"to reader\"")
            && refused [0] . contains ("\"also to reader\""), "{:?}", refused );
  assert! ( forks_of (config, "m3") ? . is_empty (), "a refusal writes nothing" );
  let done : Vec<String> = receipt (
    &mut s, &[ ("item", "m3"), ("text", ""), ("mailbox", "g") ]);
  assert! ( done [0] . contains ("(preview false)"), "{:?}", done );
  assert! ( hides_of (config, "g") ? . contains (&ID::from ("m3")) );
  assert! ( ! hides_of (config, "f") ? . contains (&ID::from ("m3")) );
  let (_fork, receipt_node) = the_receipt_under_the_new_fork_of (config, "m3") ?;
  assert_eq! ( receipt_node . body, None, "an empty text writes no body" );
  Ok (( )) }

/// n is in no mailbox: with two owned skgrepos nothing suggests one,
/// so the receipt asks for it; named, the fork goes there and
/// nothing is hidden.
fn an_item_in_no_mailbox_is_forked_where_the_reader_says (
  config  : &SkgConfig,
  tantivy : &TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let mut s : Session = session (config, tantivy, &[]) ?;
  let refused : Vec<String> = receipt ( &mut s, &[ ("item", "n"), ("text", "Read.") ]);
  assert! ( refused [0] . contains ("choose-skgrepo")
            && refused [0] . contains ("reader-private"), "{:?}", refused );
  assert! ( forks_of (config, "n") ? . is_empty () );
  let done : Vec<String> = receipt (
    &mut s, &[ ("item", "n"), ("text", "Read."), ("fork-into", "reader-private") ]);
  assert! ( done [0] . contains ("Hides nothing") && done [0] . contains ("no mailbox"),
            "{:?}", done );
  let (fork, receipt_node) = the_receipt_under_the_new_fork_of (config, "n") ?;
  assert_eq! ( fork . home_skgrepo, SkgrepoName::from ("reader-private") );
  assert_eq! ( receipt_node . body, None, "a text restating the title writes no body" );
  Ok (( )) }

/// 'f' already hides m4: the receipt is still written, and 'f' is
/// left as it was.
fn an_item_already_hidden_still_gets_its_receipt (
  config  : &SkgConfig,
  tantivy : &TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let mut s : Session = session (config, tantivy, &[]) ?;
  let before : Graphnode = node (config, "f") ?;
  let done : Vec<String> = receipt ( &mut s, &[ ("item", "m4"), ("text", "ok") ]);
  assert! ( done [0] . contains ("already hides"), "{:?}", done );
  the_receipt_under_the_new_fork_of (config, "m4") ?;
  assert_eq! ( node (config, "f") ?, before );
  Ok (( )) }

/// mp is homed in 'sender-private' but mailed in 't', of
/// 'sender-public': its fork is not defaulted to the reader's fork of
/// the mailbox's skgrepo, which may be public.
fn an_item_more_private_than_its_mailbox_suggests_no_skgrepo (
  config  : &SkgConfig,
  tantivy : &TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let mut s : Session = session (config, tantivy, &[]) ?;
  let refused : Vec<String> = receipt ( &mut s, &[ ("item", "mp"), ("text", "") ]);
  assert! ( refused [0] . contains ("choose-skgrepo")
            && refused [0] . contains ("sender-private"), "{:?}", refused );
  assert! ( forks_of (config, "mp") ? . is_empty () );
  assert! ( ! hides_of (config, "f") ? . contains (&ID::from ("mp")) );
  let done : Vec<String> = receipt (
    &mut s, &[ ("item", "mp"), ("text", ""), ("fork-into", "reader-private") ]);
  assert! ( done [0] . contains ("(preview false)"), "{:?}", done );
  assert! ( hides_of (config, "f") ? . contains (&ID::from ("mp")) );
  Ok (( )) }

/// m2x is the sender's reply to the reader's m2y, which 't' holds:
/// the receipt forks the reply, and the hide lands on m2y, the member
/// the mailbox's list names.
fn a_receipt_for_a_reply_hides_the_message_it_forks (
  config  : &SkgConfig,
  tantivy : &TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let mut s : Session = session (config, tantivy, &[]) ?;
  receipt ( &mut s, &[ ("item", "m2x"), ("text", "") ]);
  the_receipt_under_the_new_fork_of (config, "m2x") ?;
  let hides : Vec<ID> = hides_of (config, "f") ?;
  assert! ( hides . contains (&ID::from ("m2y")) && ! hides . contains (&ID::from ("m2x")),
            "{:?}", hides );
  Ok (( )) }

/// d is the reader's own: the receipt goes under it, and no fork is
/// made.
fn a_receipt_for_a_note_of_ones_own_goes_under_it (
  config  : &SkgConfig,
  tantivy : &TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let mut s : Session = session (config, tantivy, &[]) ?;
  let done : Vec<String> = receipt ( &mut s, &[ ("item", "d"), ("text", "") ]);
  assert! ( done [0] . contains ("which is yours"), "{:?}", done );
  assert_eq! ( node (config, "d") ? . contains . len (), 1 );
  assert! ( forks_of (config, "d") ? . is_empty () );
  Ok (( )) }
