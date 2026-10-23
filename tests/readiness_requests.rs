// cargo nextest run --test grouped_repos -E 'test(readiness_requests::)'
//
// A save consumes the readiness requests of the buffer it saves, at
// the fingerprints of the fresh graph straddle, over a git fixture whose
// HEAD and worktree differ: r is retitled and gains the member a, and
// a gains the alias "ay".

use crate::git_fixture::{Fixture, OwnedSkgrepos, save_view, with_git_fixture};

use skg::diff_view::change_leaves::{LeafKey, NodeAspect, RelationshipAspect};
use skg::dbs::in_rust_graph::relation_accessors::{NodeRelation, RelationshipKey};
use skg::diff_view::readiness_store::ReadinessStore;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::types::misc::ID;
use skg::types::views_state::ViewId;

use std::collections::HashSet;
use std::error::Error;
use std::path::Path;

const FIXTURES : &str = "tests/readiness_requests/fixtures";

/// Save 'buffer' as a content view.
fn save (
  f      : &mut Fixture,
  buffer : &str,
) -> Result<SaveResponse, Box<dyn Error>> {
  save_view ( f, &ViewId::ContentView ("readiness" . to_string ()), buffer ) }

fn marked_keys (
  f : &Fixture,
) -> HashSet<LeafKey> {
  ReadinessStore::load (&f . config . data_root) . marks
    . into_keys () . collect () }

fn r_text () -> LeafKey {
  LeafKey::Node { pid : ID::from ("r"), aspect : NodeAspect::Text } }

fn r_contains_a () -> LeafKey {
  LeafKey::Relationship {
    relationship : RelationshipKey {
      relation : NodeRelation::Contains,
      recorder : ID::from ("r"),
      member   : ID::from ("a") },
    aspect       : RelationshipAspect::Existence } }

fn a_alias () -> LeafKey {
  LeafKey::Node { pid : ID::from ("a"),
                  aspect : NodeAspect::Alias ("ay" . to_string ()) } }

#[test]
fn a_save_marks_and_unmarks_the_leaves_its_requests_name (
) -> Result<(), Box<dyn Error>> {
  with_git_fixture ( Path::new (FIXTURES), OwnedSkgrepos::OnlyMain, &[], |f| {
    { // The relationship object of a member, and an alias leaf.
      let response : SaveResponse = save (f, "\
* (skg (node (id r) (repo main))) r, retitled
** (skg (node (id a) (repo main) (diff (readyRequest rel)))) a
*** (skg aliasFolder)
**** (skg alias (diff (readyRequest))) ay
** (skg (node (id b) (repo main))) b
") ?;
      assert_eq! ( marked_keys (f),
                   [ r_contains_a (), a_alias () ] . into_iter () . collect () );
      assert! ( ! response . saved_view . contains ("Request"),
                "the save consumes its requests:\n{}", response . saved_view );
      assert! ( response . warnings . is_empty (), "{:?}", response . warnings );
      { // Each mark holds the fingerprint of the change it approved.
        let straddle = f . views_state . graph_straddle . clone ()
          . expect ("consuming requests built the graph straddle");
        let store : ReadinessStore = ReadinessStore::load (&f . config . data_root);
        for key in [ r_contains_a (), a_alias () ] {
          assert! ( store . is_ready (
            &key, straddle . change_leaves () . leaf_fingerprint (&key) . unwrap () )); }} }
    { // 'all' on a node names every leaf it owns.
      save (f, "\
* (skg (node (id r) (repo main) (diff (readyRequest all)))) r, retitled
** (skg (node (id a) (repo main))) a
** (skg (node (id b) (repo main))) b
") ?;
      assert_eq! ( marked_keys (f),
                   [ r_text (), r_contains_a (), a_alias () ]
                   . into_iter () . collect () ); }
    { // Unmarking.
      save (f, "\
* (skg (node (id r) (repo main) (diff (unreadyRequest all)))) r, retitled
** (skg (node (id a) (repo main))) a
** (skg (node (id b) (repo main))) b
") ?;
      assert_eq! ( marked_keys (f),
                   [ a_alias () ] . into_iter () . collect () ); }
    { // A request that names no change is dropped, with a warning.
      let response : SaveResponse = save (f, "\
* (skg (node (id r) (repo main))) r, retitled
** (skg (node (id a) (repo main))) a
** (skg (node (id b) (repo main) (diff (readyRequest rel))) (vid 0z)) b
") ?;
      assert_eq! ( marked_keys (f),
                   [ a_alias () ] . into_iter () . collect () );
      assert! ( response . warnings . iter ()
                . any ( |w| w . contains (
                  "The readiness request on \"b\" (view id 0z) names no current change" )),
                "the warning names the headline's view id: {:?}",
                response . warnings ); }
    Ok (( )) }) }
