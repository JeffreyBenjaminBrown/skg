// cargo nextest run --test grouped_overrides -E 'test(override_substitution::)'
//
// Override substitution ('docs/override.org', "Automatic override
// substitution"):
// when the refresh would draw a viewnode for node N as recursive
// content, and an owned, visible R overrides N, it shows R
// instead, marked '(overridesHere N)' -- and saving such a view
// round-trips to the original IDs, so a container's contains list is
// never silently rewritten from N to R.
//
// Fixture (tests/override_substitution/fixtures, single skgrepo
// "main", owned): Q contains P and P2; P contains N and M;
// R overrides N and contains W; S subscribes to E; E contains N.
//
// The multi-repo fixture (fixtures-multi) adds the ownership and
// visibility gates: FR (skgrepo "foreign", not owned) overrides
// N1; R2 (skgrepo "other", owned) overrides N2; N3 lives in
// "other" while P3 and its overrider R3 live in "main".

use indoc::indoc;
use std::error::Error;
use std::net::TcpStream;

use skg::from_text::buffer_to_validated_saveplan;
use skg::assert_metadata_eq;
use skg::serve::ViewsState;
use skg::skgrepo_sets::{SkgrepoRestriction, SkgrepoSetName};
use skg::test_utils::{
  run_with_shared_test_stores,
  graph_handle_from_config};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::to_org::render::content_view::{
  multi_root_view, multi_root_view_with_skgrepo_set};
use skg::types::errors::{BufferValidationError, SaveError};
use skg::types::misc::{ID, SkgConfig, SkgrepoName, TantivyIndex, members_of};
use skg::types::nodes::complete::Graphnode;
use skg::types::save::{NodeInstruction, SaveNode};
use skg::types::views_state::OpenViews;

use skg::dbs::in_rust_graph::InRustGraphHandle;

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str = "tests/override_substitution/fixtures";
  run_with_shared_test_stores (
    "skg-test-override-substitution",
    |s| Box::pin ( async move {
      s . reset ("de_novo_rendering_shows_the_overrider_marked", fixtures) ?;
      de_novo_rendering_shows_the_overrider_marked (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("save_roundtrips_to_original_and_is_idempotent", fixtures) ?;
      save_roundtrips_to_original_and_is_idempotent (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("extraction_honors_overridesHere", fixtures) ?;
      extraction_honors_overridesHere (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset_from_config ("ownership_and_visibility_gate_substitution",
                             "tests/override_substitution/fixtures-multi/skgconfig.toml") ?;
      ownership_and_visibility_gate_substitution (
        &s . config ) . await ?;
      s . reset_from_config ("chain_half_visible_keeps_the_original",
                             "tests/override_substitution/fixtures-chain/skgconfig.toml") ?;
      chain_half_visible_keeps_the_original (
        &s . config, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

fn marked_lines<'a> (
  buf      : &'a str,
  original : &str,
) -> Vec<&'a str> {
  let fact : String =
    format! ("(overridesHere {})", original);
  buf . lines ()
    . filter ( |l| l . contains (&fact) )
    . collect () }

fn saved_node_by_gnid<'a> (
  instructions : &'a [NodeInstruction],
  gnid           : &str,
) -> &'a Graphnode {
  opt_saved_node_by_gnid (instructions, gnid)
    . unwrap_or_else ( || panic! ("SaveNode not found: {}", gnid) ) }

fn opt_saved_node_by_gnid<'a> (
  instructions : &'a [NodeInstruction],
  gnid           : &str,
) -> Option<&'a Graphnode> {
  for instruction in instructions {
    if let NodeInstruction::Save (SaveNode (node)) = instruction {
      if node . pid == ID::from (gnid) {
        return Some (node); }}}
  None }

async fn node_instructions_from (
  buffer : &str,
  config : &SkgConfig,
) -> Result<Vec<NodeInstruction>, SaveError> {
  Ok ( buffer_to_validated_saveplan (
         buffer, config, None )  ?
       . 1 . node_instructions ) }

async fn save_and_rerender (
  buf     : &str,
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<String, Box<dyn Error>> {
  let graph : InRustGraphHandle =
    graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = ViewsState {
    open_views        : OpenViews::new (),
    graph_straddle    : None,
  };
  let listener : std::net::TcpListener =
    std::net::TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let mut stream : TcpStream =
    TcpStream::connect (listener . local_addr () . unwrap ()) . unwrap ();
  let response = update_from_and_rerender_buffer (
    &mut stream,
    buf, config, tantivy, &graph,
    &Err ( String::new () ), &mut views_state ) . await ?;
  assert! ( response . errors . is_empty (),
    "save must not error; got: {:?}", response . errors );
  Ok ( response . saved_view ) }

fn read_fixture_file (
  config : &SkgConfig,
  pid    : &str,
) -> String {
  let path : std::path::PathBuf =
    config . skgrepos . values () . next () . unwrap ()
    . path . join ( format! ("{}.skg", pid) );
  std::fs::read_to_string (&path)
    . unwrap_or_else ( |e| panic! ("reading {:?}: {}", path, e) ) }

async fn de_novo_rendering_shows_the_overrider_marked (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      (
        graph_handle_from_config (config) ? );
      let (view, _pids, _tree) =
        multi_root_view (
          config, Some (tantivy),
          &[ ID::from ("P") ] ) ?;
      { let marked : Vec<&str> = marked_lines (&view, "N");
        assert_eq! ( marked . len (), 1,
          "exactly one shown substitute for N:\n{}", view );
        assert! ( marked [0] . contains ("(gnid R)"),
          "the substitute is R:\n{}", view ); }
      assert! ( view . lines () . any (
                  |l| l . contains ("(gnid W)") ),
        "R's own content W is shown beneath it:\n{}", view );
      assert! ( view . lines () . any (
                  |l| l . contains ("(gnid M)")
                      && ! l . contains ("overridesHere") ),
        "M, not overridden, is shown raw:\n{}", view );
      Ok (( )) }

async fn save_roundtrips_to_original_and_is_idempotent (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      (
        graph_handle_from_config (config) ? );
      let (de_novo, _pids, _tree) =
        multi_root_view (
          config, Some (tantivy),
          &[ ID::from ("P") ] ) ?;
      let saved : String =
        save_and_rerender (&de_novo, config, tantivy)
        . await ?;
      { let p_file : String = read_fixture_file (config, "P");
        assert! ( p_file . contains ("- N"),
          "P still contains N after the save:\n{}", p_file );
        assert! ( ! p_file . contains ("- R"),
          "P must NOT have been rewritten to contain R:\n{}",
          p_file ); }
      { let r_file : String = read_fixture_file (config, "R");
        assert! ( r_file . contains ("- W"),
          "R's own contains is untouched:\n{}", r_file ); }
      assert_eq! ( marked_lines (&saved, "N") . len (), 1,
        "the rerendered saved view still shows marked R:\n{}",
        saved );
      let saved_again : String =
        save_and_rerender (&saved, config, tantivy)
        . await ?;
      assert_metadata_eq! ( saved, saved_again,
        "a second save is a noop (idempotence through the \
         collected-ID orderkey)" );
      Ok (( )) }

async fn extraction_honors_overridesHere (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      (
        graph_handle_from_config (config) ? );
      { // A marked child collects its original: P's collected
        // contains equals disk ([N, M]), so the noop filter drops
        // P's nodeInstruction entirely -- the round-trip in its
        // strongest form.
        let buffer = indoc! {"
          * (skg node (gnid P) (skgRepos (nodeRepo main))) P
          ** (skg node (gnid R) (skgRepos (nodeRepo main)) writeProtected (overridesHere N)) R
          ** (skg node (gnid M) (skgRepos (nodeRepo main)) writeProtected) M
        "};
        assert! (
          opt_saved_node_by_gnid (
            & node_instructions_from (buffer, config) . await ?,
            "P" ) . is_none (),
          "P's contains is unchanged (N, M), so no instruction \
           touches P" ); }
      { // Deleting the shown child deletes the original member.
        let buffer = indoc! {"
          * (skg node (gnid P) (skgRepos (nodeRepo main))) P
          ** (skg node (gnid M) (skgRepos (nodeRepo main)) writeProtected) M
        "};
        assert_eq! (
          members_of ( & saved_node_by_gnid (
            & node_instructions_from (buffer, config) . await ?,
            "P" ) . contains ),
          vec![ ID::from ("M") ] ); }
      { // Reordering the shown child positions the original.
        let buffer = indoc! {"
          * (skg node (gnid P) (skgRepos (nodeRepo main))) P
          ** (skg node (gnid M) (skgRepos (nodeRepo main)) writeProtected) M
          ** (skg node (gnid R) (skgRepos (nodeRepo main)) writeProtected (overridesHere N)) R
        "};
        assert_eq! (
          members_of ( & saved_node_by_gnid (
            & node_instructions_from (buffer, config) . await ?,
            "P" ) . contains ),
          vec![ ID::from ("M"), ID::from ("N") ] ); }
      { // Moving the shown child to another parent moves the original.
        let buffer = indoc! {"
          * (skg node (gnid Q) (skgRepos (nodeRepo main))) Q
          ** (skg node (gnid P) (skgRepos (nodeRepo main))) P
          *** (skg node (gnid M) (skgRepos (nodeRepo main)) writeProtected) M
          ** (skg node (gnid P2) (skgRepos (nodeRepo main))) P2
          *** (skg node (gnid R) (skgRepos (nodeRepo main)) writeProtected (overridesHere N)) R
        "};
        let instructions : Vec<NodeInstruction> =
          node_instructions_from (buffer, config) . await ?;
        assert_eq! (
          members_of ( & saved_node_by_gnid (&instructions, "P") . contains ),
          vec![ ID::from ("M") ] );
        assert_eq! (
          members_of ( & saved_node_by_gnid (&instructions, "P2") . contains ),
          vec![ ID::from ("N") ] ); }
      { // Edits to the shown R save to R; N is untouched.
        let buffer = indoc! {"
          * (skg node (gnid P) (skgRepos (nodeRepo main))) P
          ** (skg node (gnid R) (skgRepos (nodeRepo main)) (overridesHere N)) R-edited
          *** (skg node (gnid W) (skgRepos (nodeRepo main)) writeProtected) W
          ** (skg node (gnid M) (skgRepos (nodeRepo main)) writeProtected) M
        "};
        let instructions : Vec<NodeInstruction> =
          node_instructions_from (buffer, config) . await ?;
        assert_eq! (
          saved_node_by_gnid (&instructions, "R") . title,
          "R-edited" );
        assert! (
          opt_saved_node_by_gnid (&instructions, "N") . is_none (),
          "no instruction touches N" );
        assert! (
          opt_saved_node_by_gnid (&instructions, "P") . is_none (),
          "P's contains is unchanged, so P noops" ); }
      { // A legacy buffer showing N raw still saves identically
        // (a noop, like the marked equivalent above).
        let buffer = indoc! {"
          * (skg node (gnid P) (skgRepos (nodeRepo main))) P
          ** (skg node (gnid N) (skgRepos (nodeRepo main)) writeProtected) N
          ** (skg node (gnid M) (skgRepos (nodeRepo main)) writeProtected) M
        "};
        assert! (
          opt_saved_node_by_gnid (
            & node_instructions_from (buffer, config) . await ?,
            "P" ) . is_none () ); }
      { // Tamper: a fact the server would not have rendered aborts.
        let buffer = indoc! {"
          * (skg node (gnid P) (skgRepos (nodeRepo main))) P
          ** (skg node (gnid M) (skgRepos (nodeRepo main)) writeProtected (overridesHere W)) M
        "};
        match node_instructions_from (buffer, config) . await {
          Err (SaveError::BufferValidationErrors { errors, .. }) => {
            assert! ( errors . iter () . any ( |e| matches! (
              e, BufferValidationError::OverridesHere_Mismatch (..) )),
              "expected OverridesHere_Mismatch, got: {:?}", errors ); },
          other => panic! (
            "tampered fact must abort the save; got: {:?}",
            other . map ( |v| v . len () ) ), }}
      { // Subscribee-as-such: the visible-content signal speaks of
        // the original, so no spurious hide of N is inferred.
        let buffer = indoc! {"
          * (skg node (gnid S) (skgRepos (nodeRepo main))) S
          ** (skg subscribeeFolder)
          *** (skg node (gnid E) (skgRepos (nodeRepo main))) E
          **** (skg node (gnid R) (skgRepos (nodeRepo main)) writeProtected (overridesHere N)) R
        "};
        let instructions : Vec<NodeInstruction> =
          node_instructions_from (buffer, config) . await ?;
        if let Some (s_node) = opt_saved_node_by_gnid (&instructions, "S") {
          assert! (
            ! s_node . hidesFromSubs . or_default ()
              . iter () . any ( |m| &m . member == &ID::from ("N") ),
            "S must not hide N: the shown R stands for N" ); }}
      Ok (( )) }

async fn ownership_and_visibility_gate_substitution (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
      (
        graph_handle_from_config (config) ? );
      { // A foreign overrider never substitutes.
        let (view, _pids, _tree) =
          multi_root_view (
            config, None,
            &[ ID::from ("P1") ] ) ?;
        assert! ( marked_lines (&view, "N1") . is_empty (),
          "FR is foreign; N1 is shown raw:\n{}", view );
        assert! ( view . contains ("(gnid N1)"), "{}", view ); }
      let restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (
          config, SkgrepoSetName ("main" . to_string ())) ?;
      { // A restricted owned overrider does not substitute.
        let (view, _pids, _tree) =
          multi_root_view_with_skgrepo_set (
            config, None,
            &[ ID::from ("P2") ], &restriction ) ?;
        assert! ( marked_lines (&view, "N2") . is_empty (),
          "R2's repo is restricted; N2 is shown raw:\n{}", view );
        assert! ( view . contains ("(gnid N2)"), "{}", view ); }
      { // The same overrider substitutes when its skgrepo is unrestricted.
        let (view, _pids, _tree) =
          multi_root_view (
            config, None,
            &[ ID::from ("P2") ] ) ?;
        let marked : Vec<&str> = marked_lines (&view, "N2");
        assert_eq! ( marked . len (), 1, "{}", view );
        assert! ( marked [0] . contains ("(gnid R2)"), "{}", view ); }
      { // Omission beats substitution: restricted original, unrestricted
        // overrider -> neither is shown.
        let (view, _pids, _tree) =
          multi_root_view_with_skgrepo_set (
            config, None,
            &[ ID::from ("P3") ], &restriction ) ?;
        assert! ( ! view . contains ("(gnid N3)"),
          "the restricted original is omitted:\n{}", view );
        assert! ( ! view . contains ("(gnid R3)"),
          "its overrider must not be shown in its place (the \
           fact would name a restricted node):\n{}", view ); }
      Ok (( )) }

/// Save 'buf' under a specific skgrepo restriction (the test shims save
/// under 'all'). Asserts no save errors and returns the rerendered
/// view.
async fn save_under_set (
  buf     : &str,
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
  set     : &SkgrepoRestriction,
) -> Result<String, Box<dyn Error>> {
  let graph : InRustGraphHandle =
    graph_handle_from_config (config) ?;
  let mut env : skg::types::env::SkgEnv =
    skg::test_utils::skg_env_from_parts (
      config, tantivy, &graph );
  let mut views_state : ViewsState = ViewsState {
    open_views        : OpenViews::new (),
    graph_straddle    : None, };
  let listener : std::net::TcpListener =
    std::net::TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let mut stream : TcpStream =
    TcpStream::connect (listener . local_addr () . unwrap ()) . unwrap ();
  let response =
    skg::serve::handlers::save_buffer::update_from_and_rerender_buffer (
      &mut stream, buf, &mut env, 
      &Err (String::new ()), &mut views_state,
      Some (set), true,
      &std::collections::HashMap::new () ) . await ?;
  assert! ( response . errors . is_empty (),
    "save must not error; got: {:?}", response . errors );
  Ok ( response . saved_view ) }

/// A half-visible owned chain: D overrides C overrides N, with the
/// end D in skgrepo 'other'. Under 'all' the END D substitutes for N;
/// under 'main' (hiding 'other') the middle C substitutes. Saving the
/// half-visible view accepts the middle carrier (it is on N's chain)
/// and keeps N in P's contains; an off-chain fact is rejected.
async fn chain_half_visible_keeps_the_original (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      (
        graph_handle_from_config (config) ? );
      { // Under 'all', the chain end D is shown in N's place.
        let (view, _p, _t) =
          multi_root_view (
            config, Some (tantivy),
            &[ ID::from ("P") ] ) ?;
        let marked : Vec<&str> = marked_lines (&view, "N");
        assert_eq! ( marked . len (), 1,
          "one substitute for N under all:\n{}", view );
        assert! ( marked [0] . contains ("(gnid D)"),
          "the chain end D is shown under all:\n{}", view ); }
      let main_set : SkgrepoRestriction =
        SkgrepoRestriction::named (
          config, SkgrepoSetName ("main" . to_string ())) ?;
      let view_main : String = {
        // Under 'main', D's skgrepo 'other' is restricted, so the middle
        // C is shown instead.
        let (view, _p, _t) =
          multi_root_view_with_skgrepo_set (
            config, Some (tantivy),
            &[ ID::from ("P") ], &main_set ) ?;
        let marked : Vec<&str> = marked_lines (&view, "N");
        assert_eq! ( marked . len (), 1,
          "one substitute for N under main:\n{}", view );
        assert! ( marked [0] . contains ("(gnid C)"),
          "the chain middle C is shown under main:\n{}", view );
        view };
      { // Saving the half-visible view accepts the middle carrier and
        // keeps N (not C) in P's contains.
        let saved : String =
          save_under_set (&view_main, config, tantivy, &main_set)
          . await ?;
        assert_eq! ( marked_lines (&saved, "N") . len (), 1,
          "the rerendered saved view still shows a marked substitute \
           for N:\n{}", saved );
        let p_file : String = {
          let main_path : &std::path::Path =
            & config . skgrepos
              . get ( &SkgrepoName::from ("main") ) . unwrap () . path;
          std::fs::read_to_string ( main_path . join ("P.skg") )
            . unwrap () };
        assert! ( p_file . contains ("- N"),
          "P still contains N after the half-visible save:\n{}", p_file );
        assert! ( ! p_file . contains ("- C"),
          "P must NOT be rewritten to contain the carrier C:\n{}",
          p_file ); }
      { // A fact on a node not on N's chain is rejected.
        let buffer = indoc! {"
          * (skg node (gnid P) (skgRepos (nodeRepo main))) P
          ** (skg node (gnid D) (skgRepos (nodeRepo other)) writeProtected (overridesHere P)) D
        "};
        match node_instructions_from (buffer, config) . await {
          Err (SaveError::BufferValidationErrors { errors, .. }) =>
            assert! ( errors . iter () . any ( |e| matches! (
              e, BufferValidationError::OverridesHere_Mismatch (..) )),
              "off-chain fact must abort; got: {:?}", errors ),
          other => panic! (
            "off-chain fact must abort the save; got: {:?}",
            other . map ( |v| v . len () ) ), }}
      Ok (( )) }
