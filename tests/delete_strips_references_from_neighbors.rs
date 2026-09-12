// cargo nextest run --test grouped_saves -E 'test(delete_strips_references_from_neighbors::)'
//
// When a node is deleted via a save buffer, every other on-disk
// node that referenced it in an outbound list field should have
// that reference stripped. Without this, deletes leave dangling
// references which surface as PhantomUnknown placeholders -- correct
// rendering, but the inconsistency shouldn't have been left on
// disk in the first place.
//
// Fixture:
//   container.skg     contains: [victim, sibling]
//   subscriber.skg    subscribes_to: [victim, sibling]
//   victim.skg        (the one being deleted)
//   sibling.skg       (left alone)
//
// Save: a buffer that places (editRequest delete) on victim. The
// pipeline should:
//   - delete victim.skg
//   - rewrite container.skg to contains: [sibling]
//   - rewrite subscriber.skg to subscribes_to: [sibling]
//   - leave sibling.skg unchanged.

use indoc::indoc;
use std::error::Error;
use std::fs;
use std::io::BufReader;
use std::net::TcpStream;
use std::path::Path;

use skg::dbs::filesystem::one_node::{
  nodecomplete_from_pid_and_source,
  nodecomplete_from_pid_and_source as load_nc};
use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::save::update_graph_minus_nodeMerges;
use skg::test_utils::{run_with_shared_test_stores, graph_handle_from_config,
                      read_lp_message, extract_string_field_from_sexp,
                      skg_env_from_parts};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::serve::ViewsState;
use skg::serve::handlers::delete_references_to_absent_node::
  handle_delete_references_to_absent_node_request;
use skg::source_sets::ActiveSourceSet;
use skg::to_org::render::content_view::single_root_view;
use skg::types::env::SkgEnv;
use skg::types::views_state::{OpenViews, ViewUri};
use skg::types::misc::{ID, SkgConfig, TantivyIndex, SourceName, members_of, members_msv};
use skg::types::nodes::complete::NodeComplete;
use skg::types::save::{DefineNode, SaveNode, DeleteNode};
use skg::util::path_from_pid_and_source;


#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-delete-strips-references-from-neighbors",
    |s| Box::pin ( async move {
      s . reset ("test_delete_strips_references_from_neighbors",
                 "tests/delete_strips_references_from_neighbors/fixtures") ?;
      test_delete_strips_references_from_neighbors (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_strip_pass_amends_user_supplied_savenode",
                 "tests/delete_strips_references_from_neighbors/fixtures-with-existing-save") ?;
      test_strip_pass_amends_user_supplied_savenode (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_strip_pass_handles_extra_ids",
                 "tests/delete_strips_references_from_neighbors/fixtures-extra-ids") ?;
      test_strip_pass_handles_extra_ids (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("delete_preserves_foreign_referencer",
                 "tests/delete_strips_references_from_neighbors/fixtures-cross-owner") ?;
      delete_preserves_foreign_referencer (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("delete_in_foreign_subscribee_rerenders_as_unknown",
                 "tests/delete_strips_references_from_neighbors/fixtures-cross-owner") ?;
      delete_in_foreign_subscribee_rerenders_as_unknown (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("delete_in_foreign_overridden_rerenders_as_unknown",
                 "tests/delete_strips_references_from_neighbors/fixtures-cross-owner") ?;
      delete_in_foreign_overridden_rerenders_as_unknown (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("delete_in_foreign_hiddenoutside_rerenders_as_unknown",
                 "tests/delete_strips_references_from_neighbors/fixtures-cross-owner") ?;
      delete_in_foreign_hiddenoutside_rerenders_as_unknown (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("absent_reference_cleanup_handler_confirms_then_rewrites",
                 "tests/delete_strips_references_from_neighbors/fixtures-absent-reference-command") ?;
      absent_reference_cleanup_handler_confirms_then_rewrites (
        &s . config, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

/// The command is a two-step protocol when text links would be left alone:
/// preview the exact structural edits, receive an opaque token, then retry
/// with that same token.  This verifies the handler rather than only its
/// pure scanner, including its narrowly-targeted rerender stream.
async fn absent_reference_cleanup_handler_confirms_then_rewrites (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut env : SkgEnv = skg_env_from_parts (
    config, tantivy, &graph );
  let active : ActiveSourceSet = ActiveSourceSet::default_from_config (config) ?;
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false, open_views : OpenViews::new (), };
  for (uri, root) in [("affected", "owner"),
                      ("clean-unrelated", "unrelated"),
                      ("dirty-unrelated", "unrelated-dirty")] {
    let (_text, pids, tree) = single_root_view (
      config, Some (tantivy), &ID::from (root), false ) ?;
    views_state . open_views . register_view (
      &graph . load_full (),
      ViewUri::ContentView (uri . to_string ()), tree, &pids ); }

  let request = |approval : Option<&str>| {
    let mut request = "((request . \"delete references to absent node\") (id . \"gone\"))" . to_string ();
    if let Some (approval) = approval {
      request = format! (
        "((request . \"delete references to absent node\") (id . \"gone\") \
          (approved-preview . \"{}\"))", approval . replace ('\\', "\\\\")
          . replace ('\"', "\\\"") . replace ('\n', "\\n")); }
    request };
  let invoke = |request : &str,
                env : &mut SkgEnv,
                views_state : &mut ViewsState|
   -> Result<Vec<String>, Box<dyn Error>> {
    let listener : std::net::TcpListener =
      std::net::TcpListener::bind ("127.0.0.1:0") ?;
    let client : TcpStream = TcpStream::connect (listener . local_addr () ?) ?;
    let (mut server, _) = listener . accept () ?;
    // The handler synchronously rerenders via `block_on`; run it outside this
    // async test's executor, as production does on its connection thread.
    std::thread::scope (|scope| {
      scope . spawn (|| handle_delete_references_to_absent_node_request (
        &mut server, request, env, views_state, &active )); });
    drop (server);
    let mut reader : BufReader<TcpStream> = BufReader::new (client);
    let mut messages : Vec<String> = Vec::new ();
    while let Ok (message) = read_lp_message (&mut reader) {
      messages . push (message); }
    Ok (messages) };

  let owner_path : String = path_from_pid_and_source (
    config, &SourceName::from ("main"), ID::from ("owner")) ?;
  let owner_before : Vec<u8> = fs::read (&owner_path) ?;
  let confirmation : Vec<String> = invoke (&request (None), &mut env, &mut views_state) ?;
  assert_eq! (confirmation . len (), 3, "{:?}", confirmation);
  assert! (confirmation [0] . contains ("delete-references-confirmation"),
            "{:?}", confirmation);
  assert! (confirmation [0] . contains ("Text links left unchanged"),
            "{:?}", confirmation);
  assert! (confirmation [1] . contains ("(lock-views ())"),
            "the preview stream must unlock every preemptively locked client view: {:?}", confirmation);
  assert! (confirmation [2] . contains ("rerender-done"), "{:?}", confirmation);
  assert_eq! (fs::read (&owner_path) ?, owner_before,
              "preview must not write before approval");
  let approval : String = extract_string_field_from_sexp (
    &confirmation [0], "approved-preview")
    . expect ("confirmation carries an opaque approval token");

  // Change a previewed field through the normal save path.  The old token must
  // not authorize a rewrite against this new graph snapshot.
  let mut changed_owner : NodeComplete = nodecomplete_from_pid_and_source (
    config, ID::from ("owner"), &SourceName::from ("main")) ?;
  changed_owner . title . push_str (" changed after preview");
  let runtime = env . runtime_snapshot ();
  let working_graph = skg::dbs::in_rust_graph::new_handle (
    (*runtime . graph) . clone ());
  update_graph_minus_nodeMerges (
    vec![DefineNode::Save (SaveNode (changed_owner))], &[], config . clone (),
    &runtime . tantivy_index, &working_graph,
    &env . mutation_gate () ) . await ?;
  env . runtime . publish (
    runtime . config . clone (), working_graph . load_full (),
    runtime . tantivy_index . clone ());
  let stale : Vec<String> = invoke (
    &request (Some (&approval)), &mut env, &mut views_state) ?;
  assert! (stale [0] . contains ("Cleanup preview is stale"), "{:?}", stale);
  let unchanged : NodeComplete = nodecomplete_from_pid_and_source (
    config, ID::from ("owner"), &SourceName::from ("main")) ?;
  assert_eq! (members_of (&unchanged . contains), vec![ID::from ("gone"), ID::from ("kept")],
              "a stale approval must not partially rewrite the owner");
  let renewed_confirmation : Vec<String> = invoke (
    &request (None), &mut env, &mut views_state) ?;
  let renewed_approval : String = extract_string_field_from_sexp (
    &renewed_confirmation [0], "approved-preview")
    . expect ("rescan carries a replacement approval token");

  let completed : Vec<String> = invoke (
    &request (Some (&renewed_approval)), &mut env, &mut views_state) ?;
  assert_eq! (completed . len (), 4, "{:?}", completed);
  assert! (completed [0] . contains ("delete-references-result"),
            "{:?}", completed);
  assert! (completed [1] . contains ("rerender-lock"), "{:?}", completed);
  assert! (completed [1] . contains ("affected"), "{:?}", completed);
  assert! (! completed [1] . contains ("clean-unrelated")
            && ! completed [1] . contains ("dirty-unrelated"),
            "only the affected view is locked: {:?}", completed);
  assert! (completed [2] . contains ("rerender-view")
            && completed [2] . contains ("affected"), "{:?}", completed);
  assert! (completed [3] . contains ("rerender-done"), "{:?}", completed);

  let owner : NodeComplete = nodecomplete_from_pid_and_source (
    config, ID::from ("owner"), &SourceName::from ("main")) ?;
  assert_eq! (members_of (&owner . contains), vec! [ID::from ("kept")]);
  assert! (owner . subscribes_to . or_default () . is_empty ());
  assert! (owner . hides_from_its_subscriptions . or_default () . is_empty ());
  assert! (owner . overrides_view_of . or_default () . is_empty ());
  assert! (owner . title . contains ("[[id:gone][text link left alone]]"),
            "cleanup reports text links but does not rewrite them" );
  Ok (( ))
}

async fn delete_preserves_foreign_referencer (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let owned : SourceName = SourceName::from ("owned");
  let foreign : SourceName = SourceName::from ("foreign");
  let foreign_path : String = path_from_pid_and_source (
    config, &foreign, ID::from ("cheese") ) ?;
  let foreign_before : Vec<u8> = fs::read (&foreign_path) ?;
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false,
    open_views        : OpenViews::new (),
  };
  let listener : std::net::TcpListener =
    std::net::TcpListener::bind ("127.0.0.1:0") ?;
  let mut stream : TcpStream = TcpStream::connect (
    listener . local_addr () ? ) ?;
  let input_org_text : &str = indoc! {"
    * (skg (node (id cheese) (source foreign))) cheese
    ** (skg (node (id victim) (source owned) (editRequest delete))) victim
  "};
  let response = update_from_and_rerender_buffer (
    &mut stream, input_org_text, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state ) . await ?;
  assert! (response . saved_view . contains (
    "(unknown (id victim-alt))"),
    "the already-open foreign relationship must immediately retain its raw \
     extra ID as Unknown after deleting the owned primary (with no redundant \
     default-source fact): {}",
    response . saved_view );
  assert_eq! ( fs::read (&foreign_path) ?, foreign_before,
    "deleting an owned target must not rewrite foreign data" );
  let owned_referencer : NodeComplete = nodecomplete_from_pid_and_source (
    config, ID::from ("owned-referencer"), &owned ) ?;
  assert! ( ! members_of (&owned_referencer . contains)
            . contains (&ID::from ("victim-alt")),
    "owned cleanup must remove an extra ID of the deleted node" );
  let graph_after = graph . load_full ();
  let cheese = graph_after . get (&ID::from ("cheese"))
    . expect ("foreign referencer remains in the retained graph");
  assert! ( members_of (&cheese . contains)
            . contains (&ID::from ("victim-alt")),
    "foreign raw membership must survive deletion" );
  let (fresh_view, _, _) = single_root_view (
    config, Some (tantivy), &ID::from ("cheese"), false ) ?;
  assert! (fresh_view . contains ("(unknown (id victim-alt))"),
    "the immediate Unknown must agree with a fresh render: {}", fresh_view);
  Ok (( )) }

async fn delete_in_foreign_subscribee_rerenders_as_unknown (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false, open_views : OpenViews::new (), };
  let listener = std::net::TcpListener::bind ("127.0.0.1:0") ?;
  let mut stream = TcpStream::connect (listener . local_addr () ?) ?;
  let response = update_from_and_rerender_buffer (
    &mut stream, indoc! {"
      * (skg (node (id subscriber) (source foreign))) foreign subscriber
      ** (skg subscribeeCol)
      *** (skg (node (id victim) (source owned) (parentIs independent) (editRequest delete))) victim
    "}, config, tantivy, &graph, false,
    &Err (String::new ()), &mut views_state ) . await ?;
  assert! (response . errors . is_empty (), "{:?}", response . errors);
  assert! (response . saved_view . contains ("(unknown (id victim-alt))"),
    "a retained foreign subscribee must become its raw extra ID immediately: {}",
    response . saved_view);
  let (fresh, _, _) = single_root_view (
    config, Some (tantivy), &ID::from ("subscriber"), false ) ?;
  assert! (fresh . contains ("(unknown (id victim-alt))"),
    "immediate subscribee rerender must match a fresh render: {}", fresh);
  Ok (( ))
}

async fn delete_in_foreign_overridden_rerenders_as_unknown (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false, open_views : OpenViews::new (), };
  let listener = std::net::TcpListener::bind ("127.0.0.1:0") ?;
  let mut stream = TcpStream::connect (listener . local_addr () ?) ?;
  let response = update_from_and_rerender_buffer (
    &mut stream, indoc! {"
      * (skg (node (id overrider) (source foreign))) foreign overrider
      ** (skg overriddenCol)
      *** (skg (node (id victim) (source owned) (editRequest delete))) victim
    "}, config, tantivy, &graph, false,
    &Err (String::new ()), &mut views_state ) . await ?;
  assert! (response . errors . is_empty (), "{:?}", response . errors);
  assert! (response . saved_view . contains ("(unknown (id victim-alt))"),
    "a retained foreign override must become its raw extra ID immediately: {}",
    response . saved_view);
  // Reopen the serialized view with a new graph handle, which exercises the
  // restart path without relying on the content-only default to request this
  // on-demand collection.
  let fresh_graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut restarted_views : ViewsState = ViewsState {
    diff_mode_enabled : false, open_views : OpenViews::new (), };
  let restart_listener = std::net::TcpListener::bind ("127.0.0.1:0") ?;
  let mut restart_stream = TcpStream::connect (restart_listener . local_addr () ?) ?;
  let restarted = update_from_and_rerender_buffer (
    &mut restart_stream, &response . saved_view, config, tantivy,
    &fresh_graph, false, &Err (String::new ()), &mut restarted_views ) . await ?;
  assert! (restarted . saved_view . contains ("(unknown (id victim-alt))"),
    "the restarted overridden view must retain the raw Unknown: {}",
    restarted . saved_view);
  Ok (( ))
}

async fn delete_in_foreign_hiddenoutside_rerenders_as_unknown (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false, open_views : OpenViews::new (), };
  let listener = std::net::TcpListener::bind ("127.0.0.1:0") ?;
  let mut stream = TcpStream::connect (listener . local_addr () ?) ?;
  let response = update_from_and_rerender_buffer (
    &mut stream, indoc! {"
      * (skg (node (id hide-subscriber) (source foreign))) foreign hide subscriber
      ** (skg subscribeeCol)
      *** (skg (node (id hide-subscribee) (source foreign))) foreign hide subscribee
      *** (skg hiddenOutsideOfSubscribeeCol)
      **** (skg (node (id victim) (source owned) (editRequest delete))) victim
    "}, config, tantivy, &graph, false,
    &Err (String::new ()), &mut views_state ) . await ?;
  assert! (response . errors . is_empty (), "{:?}", response . errors);
  assert! (response . saved_view . contains ("(unknown (id victim-alt))"),
    "a retained foreign hide must become its raw extra ID immediately: {}",
    response . saved_view);
  let (fresh, _, _) = single_root_view (
    config, Some (tantivy), &ID::from ("hide-subscriber"), false ) ?;
  assert! (fresh . contains ("(unknown (id victim-alt))"),
    "immediate hidden-outside rerender must match a fresh render: {}", fresh);
  Ok (( ))
}

async fn test_delete_strips_references_from_neighbors (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      delete_strips_references_impl (
        config, tantivy ) . await }

async fn delete_strips_references_impl (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // Single-root content view of victim with editRequest delete.
  let input_org_text : &str = indoc! {"
    * (skg (node (id victim) (source main) (editRequest delete))) victim
  "};

  let graph : InRustGraphHandle =
    graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : false,
        open_views            : OpenViews::new (),
        };
  let listener : std::net::TcpListener =
    std::net::TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let mut stream : TcpStream =
    TcpStream::connect (listener . local_addr () . unwrap ()) . unwrap ();
  let _response = update_from_and_rerender_buffer (
    &mut stream,
    input_org_text, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state ) . await ?;

  let mut failures : Vec<String> = Vec::new ();
  let main : SourceName = SourceName::from ("main");

  // 1. victim.skg deleted.
  let victim_path : String =
    path_from_pid_and_source (
      config, &main, ID::from ("victim") ) ?;
  if Path::new (&victim_path) . exists () {
    failures . push (
      "victim.skg should have been deleted" . to_string ()); }

  // 2. container.skg's contains has only sibling now.
  let container : NodeComplete =
    nodecomplete_from_pid_and_source (
      config, ID::from ("container"), &main ) ?;
  if members_of ( &container . contains ) . contains (&ID::from ("victim")) {
    failures . push ( format! (
      "container.contains still has victim: {:?}",
      container . contains )); }
  if ! members_of ( &container . contains ) . contains (&ID::from ("sibling")) {
    failures . push ( format! (
      "container.contains lost sibling: {:?}",
      container . contains )); }

  // 3. subscriber.skg's subscribes_to has only sibling.
  let subscriber : NodeComplete =
    nodecomplete_from_pid_and_source (
      config, ID::from ("subscriber"), &main ) ?;
  let sub_vec : Vec<ID> =
    members_msv ( &subscriber . subscribes_to ) . into_vec ();
  if sub_vec . contains (&ID::from ("victim")) {
    failures . push ( format! (
      "subscriber.subscribes_to still has victim: {:?}",
      sub_vec )); }
  if ! sub_vec . contains (&ID::from ("sibling")) {
    failures . push ( format! (
      "subscriber.subscribes_to lost sibling: {:?}",
      sub_vec )); }

  // 4. sibling.skg unchanged.
  let sibling : NodeComplete =
    nodecomplete_from_pid_and_source (
      config, ID::from ("sibling"), &main ) ?;
  if sibling . title != "sibling" {
    failures . push ( format! (
      "sibling.skg title changed: {}", sibling . title )); }

  if !failures . is_empty () {
    panic! ("\n{} assertion(s) failed:\n  - {}",
            failures . len (),
            failures . join ("\n  - ") ); }

  Ok (( )) }

// ----------------------------------------------------------------
// Test 2: a hand-constructed batch where a SaveNode for the
// container is present alongside a DeleteNode for victim, and the
// SaveNode still mentions victim in contains. (Buffer-derived
// instructions can't easily produce this configuration -- buffer
// validation rejects it as AmbiguousDeletion -- but the strip pass
// shouldn't depend on buffer-level invariants. This test calls
// update_graph_minus_nodeMerges directly to exercise that.)
//
// With the strip pass, container.contains ends up empty after the
// save even though the user-supplied SaveNode said otherwise.
// ----------------------------------------------------------------

async fn test_strip_pass_amends_user_supplied_savenode (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      strip_pass_amends_user_supplied_savenode_impl (
        config, tantivy ) . await }

async fn strip_pass_amends_user_supplied_savenode_impl (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let main : SourceName = SourceName::from ("main");
  // Build the SaveNode for container by reading the on-disk node
  // verbatim -- contents are still [victim] -- and pair with a
  // DeleteNode for victim.
  let container_nc : NodeComplete =
    load_nc ( config, ID::from ("container"), &main ) ?;
  assert! ( members_of ( &container_nc . contains ) . contains (&ID::from ("victim")),
            "fixture precondition: container should reference victim" );
  let node_defs : Vec<DefineNode> = vec! [
    DefineNode::Save ( SaveNode (container_nc) ),
    DefineNode::Delete ( DeleteNode {
      id: ID::from ("victim"),
      source: main . clone (), } ), ];
  let graph : InRustGraphHandle =
    graph_handle_from_config (config) ?;
  update_graph_minus_nodeMerges (
    node_defs, &[], config . clone (), tantivy, &graph,
    &skg::types::env::new_mutation_gate ()
  ) . await ?;
  let container : NodeComplete =
    load_nc ( config, ID::from ("container"), &main ) ?;
  if members_of ( &container . contains ) . contains (&ID::from ("victim")) {
    panic! ("container.contains still has victim after strip pass: {:?}",
            container . contains ); }
  let victim_path : String =
    path_from_pid_and_source (
      config, &main, ID::from ("victim") ) ?;
  if Path::new (&victim_path) . exists () {
    panic! ("victim.skg should have been deleted"); }
  Ok (( )) }

// ----------------------------------------------------------------
// Test 3: extra_ids of a deleted node also get stripped from
// referencers. Other nodes' on-disk lists may store any of a
// node's ids (primary or extra), so the strip-set must include
// every id of every deleted node.
//
// Fixture:
//   aliased.skg     pid: aliased,    extra_ids: [aliased_alt]
//   referencer.skg  contains: [aliased_alt]   <-- stored under the alias
//
// Buffer puts editRequest delete on `aliased` (by its primary pid).
// Expected: referencer.contains becomes [].
// ----------------------------------------------------------------

async fn test_strip_pass_handles_extra_ids (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      strip_pass_handles_extra_ids_impl (
        config, tantivy ) . await }

async fn strip_pass_handles_extra_ids_impl (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let input_org_text : &str = indoc! {"
    * (skg (node (id aliased) (source main) (editRequest delete))) aliased
  "};
  let graph : InRustGraphHandle =
    graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : false,
        open_views            : OpenViews::new (),
        };
  let listener : std::net::TcpListener =
    std::net::TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let mut stream : TcpStream =
    TcpStream::connect (listener . local_addr () . unwrap ()) . unwrap ();
  let _response = update_from_and_rerender_buffer (
    &mut stream,
    input_org_text, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state ) . await ?;
  let main : SourceName = SourceName::from ("main");
  let referencer : NodeComplete =
    nodecomplete_from_pid_and_source (
      config, ID::from ("referencer"), &main ) ?;
  if members_of ( &referencer . contains ) . contains (&ID::from ("aliased_alt")) {
    panic! ("referencer.contains still has aliased_alt (an extra_id of \
             the deleted node) after strip pass: {:?}",
            referencer . contains ); }
  if members_of ( &referencer . contains ) . contains (&ID::from ("aliased")) {
    panic! ("referencer.contains has aliased (primary pid of deleted node)"); }
  let aliased_path : String =
    path_from_pid_and_source (
      config, &main, ID::from ("aliased") ) ?;
  if Path::new (&aliased_path) . exists () {
    panic! ("aliased.skg should have been deleted"); }
  Ok (( )) }
