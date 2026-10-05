//! Unit tests for the sticky-else-default relRepo rule
//! ('apply_sticky_relRepos'), its home clamp, the hide floor, and the
//! restricted-set deletion refusal. Each test builds the exact graph fixture
//! it passes to the repo-resolution function.

use super::{apply_sticky_relRepos_in_graph, build_diskSupplemented_nodeInstructions,
            refuse_delete_with_restricted_sections};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::from_text::local_fieldintent_collection::lower::{
  NodeIntent, RequestedRelRepos};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::misc::{
  ID, MSV, RelPartner, SkgConfig, SkgRepo, SkgRepoName,
  SkgRepoSetName};
use crate::types::nodes::complete::{
  Flag, Graphnode, empty_graphnode};
use crate::types::save::{NodeInstruction, SaveNode};

use std::collections::HashMap;
use std::path::PathBuf;

fn config_with_order (
  names : &[&str],
) -> SkgConfig {
  let mut skgrepos : HashMap<SkgRepoName, SkgRepo> =
    HashMap::new ();
  for name in names {
    skgrepos . insert (
      SkgRepoName::from (*name),
      SkgRepo {
        name         : SkgRepoName::from (*name),
        abbreviation : None,
        path         : PathBuf::from ( format! ("owned/{}", name) ),
        owned        : true, } ); }
  let mut config : SkgConfig =
    SkgConfig::dummyFromSkgRepos (skgrepos);
  config . skgrepo_order =
    names . iter () . map ( |n| SkgRepoName::from (*n) ) . collect ();
  config }

fn node_at (
  pid     : &str,
  skgrepo : &str,
) -> Graphnode {
  let mut n : Graphnode = empty_graphnode ();
  n . pid = ID::new (pid);
  n . title = pid . to_string ();
  n . home_skgrepo = SkgRepoName::from (skgrepo);
  n }

fn graph_from (
  nodes : &[Graphnode],
) -> InRustGraph {
  InRustGraph::from_graphnodes (nodes) }

fn pm (
  skgrepo : &str,
  member : &str,
) -> RelPartner<ID> {
  RelPartner::at_relRepo (
    SkgRepoName::from (skgrepo), ID::new (member) ) }

#[test]
fn sticky_preserves_disk_skgrepos_and_default_takes_more_private_home (
) {
  let config : SkgConfig =
    config_with_order ( & ["public", "private"] );
  let old_target : Graphnode = node_at ("old", "public");
  let new_target : Graphnode = node_at ("fresh", "private");
  let recorder   : Graphnode = node_at ("recorder", "public");
  let graph      : InRustGraph = graph_from ( & [ recorder . clone (), old_target, new_target ] );
  let mut disk   : Graphnode = recorder . clone ();
  disk . contains = vec! [
    pm ("private", "old") ]; // privatized on disk
  let mut buffer : Graphnode = recorder;
  buffer . contains = vec! [
    pm ("public", "old"),    // degenerate intent tag
    pm ("public", "fresh") ]; // new relationship to a private-homed target
  let resolved : Graphnode =
    apply_sticky_relRepos_in_graph (
      buffer, &disk, &RequestedRelRepos::default (), &graph, &config) . unwrap ();
  assert_eq! ( resolved . contains, vec! [
    pm ("private", "old"),    // STICKY: the disk's privatization survives
    pm ("private", "fresh") ] ); // DEFAULT: more private of the homes
}

#[test]
fn flag_requests_apply_after_disk_misc_restore_and_survive_skgrepo_moves (
) {
  let config : SkgConfig = config_with_order (&["public", "private"]);
  for (disk_no_search, request, expected) in [
    (false, Some (true), vec![
      Flag::Had_ID_Before_Import,
      Flag::Was_Overloaded,
      Flag::NoSearchMatching]),
    (true, Some (false), vec![
      Flag::Had_ID_Before_Import,
      Flag::Was_Overloaded]),
    (true, None, vec![
      Flag::Had_ID_Before_Import,
      Flag::Was_Overloaded,
      Flag::NoSearchMatching]),
  ] {
    let mut disk : Graphnode = node_at ("node", "public");
    disk . flags = vec![
      Flag::Had_ID_Before_Import,
      Flag::Was_Overloaded];
    if disk_no_search {
      disk . flags . push (Flag::NoSearchMatching); }
    let graph : InRustGraph = graph_from (&[disk . clone ()]);
    let mut buffer = disk;
    buffer . home_skgrepo = SkgRepoName::from ("private");
    buffer . title = "edited" . to_string ();
    let mut intent = NodeIntent::graph_save_from_graphnode (buffer);
    if let NodeIntent::Save (save) = &mut intent {
      // This is the production buffer shape: flags are not textually carried.
      save . flags = Vec::new ();
      save . flag_request = request . map (|value|
        (Flag::NoSearchMatching, value)); }
    let planned = build_diskSupplemented_nodeInstructions (
      vec![intent], &graph, &config, None) . unwrap ();
    let NodeInstruction::Save (SaveNode (saved)) = &planned . instructions [0]
      else { panic! ("expected SaveNode"); };
    assert_eq! (&saved . flags, &expected);
    assert_eq! (planned . skgrepo_moves . len (), 1);
  }
}

#[test]
fn sticky_round_trip_restores_a_resolved_extra_ids_raw_disk_spelling () {
  let config : SkgConfig = config_with_order (&["public"]);
  let mut target : Graphnode = node_at ("target", "public");
  target . extra_ids = vec![ID::new ("target-extra")];
  let recorder : Graphnode = node_at ("recorder", "public");
  let graph : InRustGraph = graph_from (&[recorder . clone (), target]);
  let mut disk : Graphnode = recorder . clone ();
  disk . contains = vec![pm ("public", "target-extra")];
  let mut buffer : Graphnode = recorder;
  // Rendering names the resolved node by PID, but the relationship on disk
  // names its extra ID.  A no-op save must preserve the raw stored value.
  buffer . contains = vec![pm ("public", "target")];
  let resolved = apply_sticky_relRepos_in_graph (
    buffer, &disk, &RequestedRelRepos::default (), &graph, &config ).unwrap ();
  assert_eq! (resolved . contains, vec![pm ("public", "target-extra")]);
}

#[test]
fn home_move_to_more_private_clamps_relRepos_up (
) {
  let config : SkgConfig =
    config_with_order ( & ["public", "private"] );
  let child : Graphnode = node_at ("child", "public");
  let recorder_before : Graphnode = node_at ("recorder", "public");
  let graph           : InRustGraph = graph_from ( & [ recorder_before . clone (), child ] );
  let mut disk        : Graphnode = recorder_before;
  disk . contains = vec! [ pm ("public", "child") ];
  let mut buffer : Graphnode = node_at ("recorder", "private");
  // the buffer moved the node's home to private
  buffer . contains = vec! [ pm ("private", "child") ];
  let resolved : Graphnode =
    apply_sticky_relRepos_in_graph (
      buffer, &disk, &RequestedRelRepos::default (), &graph, &config) . unwrap ();
  assert_eq! ( resolved . contains, vec! [
    pm ("private", "child") ],
    "the sticky public repo rises to the new, more private home" );
}

#[test]
fn hide_floor_is_the_most_public_explaining_subscription (
) {
  let config : SkgConfig =
    config_with_order ( & ["public", "trusted", "private"] );
  let hidden : Graphnode = node_at ("victim", "public");
  let mut container_a : Graphnode = node_at ("expl-a", "public");
  container_a . contains = vec! [ pm ("public", "victim") ];
  let mut container_b : Graphnode = node_at ("expl-b", "public");
  container_b . contains = vec! [ pm ("public", "victim") ];
  let recorder : Graphnode = node_at ("recorder", "public");
  let graph : InRustGraph = graph_from ( & [
    recorder . clone (), hidden, container_a, container_b ] );
  { // Only a PRIVATE subscription explains the hide: the hide must
    // be private, else it leaks the inference that the private
    // subscription exists. The privatized subscription is a DISK
    // fact (sticky preserves it); raising its privacy from the
    // buffer would come through the (relRepo ...) atom (see the
    // explicit_repo_* tests below), landed with render-and-gating.
    let mut disk : Graphnode = recorder . clone ();
    disk . subscribesTo = MSV::Specified ( vec! [
      pm ("private", "expl-a") ] );
    let mut buffer : Graphnode = recorder . clone ();
    buffer . subscribesTo = MSV::Specified ( vec! [
      pm ("public", "expl-a") ] ); // degenerate tag; sticky restores
    buffer . hidesFromSubs = MSV::Specified ( vec! [
      pm ("public", "victim") ] ); // degenerate tag
    let resolved : Graphnode =
      apply_sticky_relRepos_in_graph (
        buffer, &disk, &RequestedRelRepos::default (), &graph, &config) . unwrap ();
    assert_eq! (
      resolved . subscribesTo . or_default (),
      & [ pm ("private", "expl-a") ] );
    assert_eq! (
      resolved . hidesFromSubs . or_default (),
      & [ pm ("private", "victim") ] ); }
  { // A PUBLIC explanation exists too: the inference is innocent,
    // so the hide may stay public.
    let mut disk : Graphnode = recorder . clone ();
    disk . subscribesTo = MSV::Specified ( vec! [
      pm ("private", "expl-a"),
      pm ("public",  "expl-b") ] );
    let mut buffer : Graphnode = recorder;
    buffer . subscribesTo = MSV::Specified ( vec! [
      pm ("public", "expl-a"),
      pm ("public", "expl-b") ] );
    buffer . hidesFromSubs = MSV::Specified ( vec! [
      pm ("public", "victim") ] );
    let resolved : Graphnode =
      apply_sticky_relRepos_in_graph (
        buffer, &disk, &RequestedRelRepos::default (), &graph, &config) . unwrap ();
    assert_eq! (
      resolved . hidesFromSubs . or_default (),
      & [ pm ("public", "victim") ] ); }
}

#[test]
fn explicit_skgrepo_at_or_more_private_than_floor_is_honored (
) {
  // Allowed-side acceptance (render-and-gating, 5_plan.org): an
  // explicit '(editRequest (relRepo ...))' skgrepo that is at least as private as
  // the default floor wins outright.
  let config : SkgConfig =
    config_with_order ( & ["public", "trusted", "private"] );
  let child : Graphnode = node_at ("child", "public");
  let recorder_before : Graphnode = node_at ("recorder", "public");
  let graph           : InRustGraph = graph_from ( & [ recorder_before . clone (), child ] );
  let mut disk        : Graphnode = recorder_before;
  disk . contains = vec! [ pm ("public", "child") ];
  let mut buffer : Graphnode = node_at ("recorder", "public");
  buffer . contains = vec! [ pm ("public", "child") ]; // degenerate tag
  let explicit : RequestedRelRepos = RequestedRelRepos {
    contains : HashMap::from ([
      ( ID::new ("child"), SkgRepoName::from ("trusted") ) ]),
    .. RequestedRelRepos::default () };
  let resolved : Graphnode =
    apply_sticky_relRepos_in_graph (buffer, &disk, &explicit, &graph, &config) . unwrap ();
  assert_eq! ( resolved . contains, vec! [
    pm ("trusted", "child") ],
    "an explicit repo at or more private than the default wins outright" );
}

#[test]
fn explicit_skgrepo_more_public_than_floor_is_rejected (
) {
  // More-public-than-floor rejection (render-and-gating, 5_plan.org): an
  // explicit skgrepo more PUBLIC than the DEFAULT floor is a save
  // error naming the member, the offered skgrepo, and the floor.
  let config : SkgConfig =
    config_with_order ( & ["public", "trusted", "private"] );
  let child : Graphnode = node_at ("child", "private"); // forces the default floor to "private"
  let recorder_before : Graphnode = node_at ("recorder", "public");
  let graph           : InRustGraph = graph_from ( & [ recorder_before . clone (), child ] );
  let disk            : Graphnode = recorder_before; // no sticky entry for "child"
  let mut buffer      : Graphnode = node_at ("recorder", "public");
  buffer . contains = vec! [ pm ("public", "child") ]; // degenerate tag
  let explicit : RequestedRelRepos = RequestedRelRepos {
    contains : HashMap::from ([
      ( ID::new ("child"), SkgRepoName::from ("public") ) ]), // more public than the "private" floor
    .. RequestedRelRepos::default () };
  let err : String =
    apply_sticky_relRepos_in_graph (buffer, &disk, &explicit, &graph, &config)
    . unwrap_err ();
  assert! ( err . contains ("child"),   "names the member: {}", err );
  assert! ( err . contains ("public"),  "names the offered relRepo: {}", err );
  assert! ( err . contains ("private"), "names the floor: {}", err );
}

#[test]
fn explicit_relRepo_moves_a_sticky_relationship_to_its_default (
) {
  // The BUG-and-fix_make-edge-more-public.org fix: an explicit
  // relRepo validates against the DEFAULT floor, not the disk relRepo,
  // so it can lower a stuck relationship's privacy back to the default.
  let config : SkgConfig =
    config_with_order ( & ["public", "trusted", "private"] );
  let child : Graphnode = node_at ("child", "public");
  let recorder_before : Graphnode = node_at ("recorder", "public");
  let graph           : InRustGraph = graph_from ( & [ recorder_before . clone (), child ] );
  let mut disk        : Graphnode = recorder_before;
  disk . contains = vec! [
    pm ("private", "child") ]; // stuck more private than its default
  let mut buffer : Graphnode = node_at ("recorder", "public");
  buffer . contains = vec! [ pm ("public", "child") ]; // degenerate tag
  let explicit : RequestedRelRepos = RequestedRelRepos {
    contains : HashMap::from ([
      ( ID::new ("child"), SkgRepoName::from ("public") ) ]), // = default
    .. RequestedRelRepos::default () };
  let resolved : Graphnode =
    apply_sticky_relRepos_in_graph (buffer, &disk, &explicit, &graph, &config) . unwrap ();
  assert_eq! ( resolved . contains, vec! [
    pm ("public", "child") ],
    "an explicit repo AT the default lowers the sticky relationship's privacy" );
}

#[test]
fn explicit_skgrepo_between_default_and_sticky_is_accepted (
) {
  let config : SkgConfig =
    config_with_order ( & ["public", "trusted", "private"] );
  let child : Graphnode = node_at ("child", "public");
  let recorder_before : Graphnode = node_at ("recorder", "public");
  let graph           : InRustGraph = graph_from ( & [ recorder_before . clone (), child ] );
  let mut disk        : Graphnode = recorder_before;
  disk . contains = vec! [ pm ("private", "child") ];
  let mut buffer : Graphnode = node_at ("recorder", "public");
  buffer . contains = vec! [ pm ("public", "child") ]; // degenerate tag
  let explicit : RequestedRelRepos = RequestedRelRepos {
    contains : HashMap::from ([
      ( ID::new ("child"), SkgRepoName::from ("trusted") ) ]),
    .. RequestedRelRepos::default () };
  let resolved : Graphnode =
    apply_sticky_relRepos_in_graph (buffer, &disk, &explicit, &graph, &config) . unwrap ();
  assert_eq! ( resolved . contains, vec! [
    pm ("trusted", "child") ],
    "moving privacy partway toward the default is accepted" );
}

#[test]
fn explicit_more_public_than_default_is_rejected_and_names_the_default (
) {
  // With the floors split, the error's floor is the DEFAULT, not
  // the (more private) sticky disk relRepo.
  let config : SkgConfig =
    config_with_order ( & ["public", "trusted", "private"] );
  let child : Graphnode = node_at ("child", "trusted"); // default floor: trusted
  let recorder_before : Graphnode = node_at ("recorder", "public");
  let graph           : InRustGraph = graph_from ( & [ recorder_before . clone (), child ] );
  let mut disk        : Graphnode = recorder_before;
  disk . contains = vec! [
    pm ("private", "child") ]; // sticky sits more private than the default
  let mut buffer : Graphnode = node_at ("recorder", "public");
  buffer . contains = vec! [ pm ("public", "child") ]; // degenerate tag
  let explicit : RequestedRelRepos = RequestedRelRepos {
    contains : HashMap::from ([
      ( ID::new ("child"), SkgRepoName::from ("public") ) ]), // more public than default
    .. RequestedRelRepos::default () };
  let err : String =
    apply_sticky_relRepos_in_graph (buffer, &disk, &explicit, &graph, &config)
    . unwrap_err ();
  assert! ( err . contains ("'trusted'"),
            "the floor named is the default: {}", err );
  assert! ( ! err . contains ("'private'"),
            "the sticky repo is not the floor: {}", err );
}

#[test]
fn explicit_at_a_more_public_than_default_disk_skgrepo_round_trips (
) {
  // Legacy or hand-authored data can put an owned-to-owned relationship at a
  // skgrepo more public than its default. Render emits '(relRepo ...)'
  // for every off-default relationship, so that atom must save back unchanged
  // (explicit == disk relRepo), and moving it partway toward the
  // default is fine; moving it still more public is forbidden.
  let config : SkgConfig =
    config_with_order ( & ["public", "trusted", "private"] );
  let child : Graphnode = node_at ("child", "private");
  let recorder_before : Graphnode = node_at ("recorder", "public");
  let graph           : InRustGraph = graph_from ( & [ recorder_before . clone (), child ] );
  let mut disk        : Graphnode = recorder_before;
  disk . contains = vec! [
    pm ("public", "child") ]; // more public than the "private" default
  { // Holding the disk relRepo round-trips.
    let mut buffer : Graphnode = node_at ("recorder", "public");
    buffer . contains = vec! [ pm ("public", "child") ];
    let explicit : RequestedRelRepos = RequestedRelRepos {
      contains : HashMap::from ([
        ( ID::new ("child"), SkgRepoName::from ("public") ) ]),
      .. RequestedRelRepos::default () };
    let resolved : Graphnode =
      apply_sticky_relRepos_in_graph (
        buffer, &disk, &explicit, &graph, &config ) . unwrap ();
    assert_eq! ( resolved . contains, vec! [ pm ("public", "child") ],
      "the rendered atom saves back unchanged" ); }
  { // Moving it partway toward the default is accepted.
    let mut buffer : Graphnode = node_at ("recorder", "public");
    buffer . contains = vec! [ pm ("public", "child") ];
    let explicit : RequestedRelRepos = RequestedRelRepos {
      contains : HashMap::from ([
        ( ID::new ("child"), SkgRepoName::from ("trusted") ) ]),
      .. RequestedRelRepos::default () };
    let resolved : Graphnode =
      apply_sticky_relRepos_in_graph (
        buffer, &disk, &explicit, &graph, &config ) . unwrap ();
    assert_eq! ( resolved . contains, vec! [ pm ("trusted", "child") ],
      "making a legacy more-public relationship more private is accepted" ); }
}

#[test]
fn explicit_lowering_moves_the_relationship_between_section_files (
) {
  // The repro from BUG-and-fix_make-edge-more-public.org, as files:
  // a child once homed in "private" was moved home to "trusted",
  // but the containment relation stayed stuck at "private" (sticky).
  // Explicitly lowering the relationship's privacy to its new default moves
  // the membership line from the recorder's private section file to a
  // trusted one, deleting the emptied private file.
  use crate::dbs::filesystem::one_node::write_graphnode_telescope;
  let tmp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
  let mut config : SkgConfig =
    config_with_order ( & ["public", "trusted", "private"] );
  for name in ["public", "trusted", "private"] {
    let path : PathBuf = tmp . path () . join (name);
    std::fs::create_dir_all (&path) . unwrap ();
    config . skgrepos . get_mut ( &SkgRepoName::from (name) )
      . unwrap () . path = path; }
  let child : Graphnode = node_at ("child", "trusted"); // home already moved
  let recorder : Graphnode = node_at ("recorder", "public");
  let graph    : InRustGraph = graph_from ( & [ recorder . clone (), child ] );
  let mut disk : Graphnode = recorder;
  disk . contains = vec! [ pm ("private", "child") ];
  write_graphnode_telescope ( &disk, &config ) . unwrap ();
  let private_file = tmp . path () . join ("private/recorder.skg");
  let trusted_file = tmp . path () . join ("trusted/recorder.skg");
  let public_file  = tmp . path () . join ("public/recorder.skg");
  assert! ( private_file . is_file (),
            "before: the stuck relationship lives in the private section" );
  assert! ( public_file . is_file (),
            "before: the home section exists" );
  assert! ( ! trusted_file . is_file (),
            "before: no trusted section yet" );
  let mut buffer : Graphnode = node_at ("recorder", "public");
  buffer . contains = vec! [ pm ("public", "child") ]; // degenerate tag
  let explicit : RequestedRelRepos = RequestedRelRepos {
    contains : HashMap::from ([
      ( ID::new ("child"), SkgRepoName::from ("trusted") ) ]), // the new default
    .. RequestedRelRepos::default () };
  let resolved : Graphnode =
    apply_sticky_relRepos_in_graph (buffer, &disk, &explicit, &graph, &config) . unwrap ();
  assert_eq! ( resolved . contains, vec! [ pm ("trusted", "child") ] );
  write_graphnode_telescope ( &resolved, &config ) . unwrap ();
  assert! ( ! private_file . is_file (),
            "after: the emptied private section is deleted" );
  assert! ( trusted_file . is_file (),
            "after: the relationship's new section exists" );
  assert! ( std::fs::read_to_string (&trusted_file) . unwrap ()
            . contains ("child"),
            "after: the trusted section holds the membership" );
  assert! ( ! std::fs::read_to_string (&public_file) . unwrap ()
            . contains ("child"),
            "after: the home section does not name the child" );
}

#[test]
fn same_save_child_home_move_allows_publicizing_its_parent_relationship (
) {
  // A skgrepo move and its parent's explicit relRepo request occur in one
  // save. The parent must calculate the relationship default from the child's NEW
  // home, not the pre-save graph's old home.
  let config : SkgConfig = config_with_order ( & ["public", "pers-p"] );
  let mut parent : Graphnode = node_at ("parent", "public");
  parent . contains = vec! [ pm ("pers-p", "child") ];
  let child : Graphnode = node_at ("child", "pers-p");
  let graph : InRustGraph = graph_from ( & [ parent . clone (), child ] );

  let mut parent_intent : NodeIntent =
    NodeIntent::graph_save_from_graphnode (parent);
  if let NodeIntent::Save (intent) = &mut parent_intent {
    intent . contains = MSV::Specified (vec! [(
      ID::new ("child"), Some (SkgRepoName::from ("public"))) ]); }
  let child_intent : NodeIntent = NodeIntent::graph_save_from_graphnode (
    node_at ("child", "public"));

  let planned = build_diskSupplemented_nodeInstructions (
    vec! [ parent_intent, child_intent ], &graph, &config, None )
    . expect ("the same-save home move makes public the relationship's default");
  let parent = planned . instructions . into_iter ()
    .find_map ( |instruction| match instruction {
      NodeInstruction::Save (SaveNode (node)) if node . pid == ID::new ("parent")
        => Some (node),
      _ => None })
    .expect ("parent save instruction");
  assert_eq! ( parent . contains, vec! [ pm ("public", "child") ] );
  assert_eq! ( planned . skgrepo_moves . len (), 1 );
  assert_eq! ( planned . skgrepo_moves [0] . pid, ID::new ("child") );
}

#[test]
fn same_save_hidden_node_home_move_sets_the_new_hide_skgrepo (
) {
  // Hides have an inferred relRepo rather than an explicit
  // relRepo request, but their endpoint floor must use the same pending
  // home view as the other relationship kinds.
  let config : SkgConfig = config_with_order ( & ["public", "pers-p"] );
  let hider : Graphnode = node_at ("hider", "public");
  let hidden : Graphnode = node_at ("hidden", "pers-p");
  let graph : InRustGraph = graph_from ( & [ hider . clone (), hidden ] );

  let mut hider_intent : NodeIntent =
    NodeIntent::graph_save_from_graphnode (hider);
  if let NodeIntent::Save (intent) = &mut hider_intent {
    intent . hidesFromSubs =
      MSV::Specified (vec! [ID::new ("hidden")]); }
  let hidden_intent : NodeIntent = NodeIntent::graph_save_from_graphnode (
    node_at ("hidden", "public"));

  let planned = build_diskSupplemented_nodeInstructions (
    vec! [ hider_intent, hidden_intent ], &graph, &config, None )
    . expect ("the pending hidden-node home participates in the hide floor");
  let hider = planned . instructions . into_iter ()
    .find_map ( |instruction| match instruction {
      NodeInstruction::Save (SaveNode (node)) if node . pid == ID::new ("hider")
        => Some (node),
      _ => None })
    .expect ("hider save instruction");
  assert_eq! (
    hider . hidesFromSubs,
    MSV::Specified (vec! [pm ("public", "hidden")]) );
}

#[test]
fn new_private_child_in_the_same_save_gets_a_private_relationship (
) {
  // The child is absent from the pre-save graph, so its Save intent is the
  // only available skgrepo of its home. Falling back to the public parent's
  // home here would create exactly the leak shape the endpoint floor forbids.
  let config : SkgConfig = config_with_order ( & ["public", "pers-p"] );
  let parent : Graphnode = node_at ("parent", "public");
  let graph : InRustGraph = graph_from ( & [ parent . clone () ] );

  let mut parent_intent : NodeIntent =
    NodeIntent::graph_save_from_graphnode (parent);
  if let NodeIntent::Save (intent) = &mut parent_intent {
    intent . contains = MSV::Specified (vec! [(
      ID::new ("new-child"), None )]); }
  let child_intent : NodeIntent = NodeIntent::graph_save_from_graphnode (
    node_at ("new-child", "pers-p"));

  let planned = build_diskSupplemented_nodeInstructions (
    vec! [ parent_intent, child_intent ], &graph, &config, None )
    . expect ("same-save new nodes supply their relationship endpoint homes");
  let parent = planned . instructions . into_iter ()
    .find_map ( |instruction| match instruction {
      NodeInstruction::Save (SaveNode (node)) if node . pid == ID::new ("parent")
        => Some (node),
      _ => None })
    .expect ("parent save instruction");
  assert_eq! ( parent . contains, vec! [ pm ("pers-p", "new-child") ] );
}

#[test]
fn owned_to_foreign_new_relationships_default_to_the_recorder_home (
) {
  for relation in ["contains", "subscribesTo", "overrides"] {
    for (recorder_home, member_home) in
        [("public", "private"), ("private", "public")] {
      let mut config : SkgConfig =
        config_with_order (&["public", "private"]);
      config . skgrepos . get_mut (&SkgRepoName::from (member_home))
        . unwrap () . owned = false;
      config . skgrepos . get_mut (&SkgRepoName::from (recorder_home))
        . unwrap () . owned = true;
      let recorder   : Graphnode = node_at ("recorder", recorder_home);
      let member : Graphnode = node_at ("member", member_home);
      let graph      : InRustGraph = graph_from (&[recorder . clone (), member]);
      let disk       : Graphnode = recorder;
      let mut buffer : Graphnode = node_at ("recorder", recorder_home);
      match relation {
        "contains" => buffer . contains = vec! [pm (recorder_home, "member")],
        "subscribesTo" => buffer . subscribesTo =
          MSV::Specified (vec! [pm (recorder_home, "member")]),
        "overrides" => buffer . overrides =
          MSV::Specified (vec! [pm (recorder_home, "member")]),
        _ => unreachable! (), }
      let resolved : Graphnode = apply_sticky_relRepos_in_graph (
        buffer, &disk, &RequestedRelRepos::default (), &graph, &config ) . unwrap ();
      let skgrepo : &SkgRepoName = match relation {
        "contains" => &resolved . contains [0] . relRepo,
        "subscribesTo" =>
          &resolved . subscribesTo . or_default () [0] . relRepo,
        "overrides" =>
          &resolved . overrides . or_default () [0] . relRepo,
        _ => unreachable! (), };
      assert_eq! (skgrepo, &SkgRepoName::from (recorder_home),
                  "relation {}", relation); }}
}

#[test]
fn owned_to_foreign_explicit_owned_skgrepo_is_allowed_and_foreign_refused (
) {
  let mut config : SkgConfig =
    config_with_order (&["public", "foreign", "private"]);
  config . skgrepos . get_mut (&SkgRepoName::from ("foreign"))
    . unwrap () . owned = false;
  let mut recorder : Graphnode = node_at ("recorder", "public");
  let member : Graphnode = node_at ("member", "foreign");
  let graph        : InRustGraph = graph_from (&[recorder . clone (), member]);

  let disk : Graphnode = recorder . clone ();
  recorder . contains = vec! [pm ("public", "member")];
  let allowed : RequestedRelRepos = RequestedRelRepos {
    contains : HashMap::from ([
      (ID::new ("member"), SkgRepoName::from ("private")) ]),
    .. RequestedRelRepos::default () };
  let resolved : Graphnode = apply_sticky_relRepos_in_graph (
    recorder . clone (), &disk, &allowed, &graph, &config ) . unwrap ();
  assert_eq! (resolved . contains [0] . relRepo,
              SkgRepoName::from ("private"));

  let refused : RequestedRelRepos = RequestedRelRepos {
    contains : HashMap::from ([
      (ID::new ("member"), SkgRepoName::from ("foreign")) ]),
    .. RequestedRelRepos::default () };
  let error : String = apply_sticky_relRepos_in_graph (
    recorder, &disk, &refused, &graph, &config ) . unwrap_err ();
  assert! (error . contains ("non-owned repo 'foreign'"), "{}", error);
}

#[test]
fn explicit_alias_relRepo_is_load_bearing_and_validated (
) {
  let config : SkgConfig =
    config_with_order (&["public", "trusted", "private"]);
  let graph : InRustGraph = InRustGraph::new ();
  let disk : Graphnode = node_at ("recorder", "public");
  let mut buffer : Graphnode = disk . clone ();
  buffer . aliases = MSV::Specified (vec! [
    crate::types::misc::RelPartner::at_relRepo (
      SkgRepoName::from ("public"), "nickname" . to_string ()) ]);
  let explicit : RequestedRelRepos = RequestedRelRepos {
    aliases : HashMap::from ([
      ("nickname" . to_string (), SkgRepoName::from ("private")) ]),
    .. RequestedRelRepos::default () };
  let resolved : Graphnode = apply_sticky_relRepos_in_graph (
    buffer, &disk, &explicit, &graph, &config ) . unwrap ();
  assert_eq! (
    resolved . aliases . or_default () [0] . relRepo,
    SkgRepoName::from ("private") );

  let mut foreign_config : SkgConfig = config;
  foreign_config . skgrepos . get_mut (&SkgRepoName::from ("private"))
    . unwrap () . owned = false;
  let mut buffer : Graphnode = disk . clone ();
  buffer . aliases = MSV::Specified (vec! [
    crate::types::misc::RelPartner::at_relRepo (
      SkgRepoName::from ("public"), "nickname" . to_string ()) ]);
  let error : String = apply_sticky_relRepos_in_graph (
    buffer, &disk, &explicit, &graph, &foreign_config ) . unwrap_err ();
  assert! (error . contains ("non-owned relRepo 'private'"), "{}", error);
}

#[test]
fn restricted_delete_refusal_sees_restricted_sections (
) {
  let tmp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
  let mut config : SkgConfig =
    config_with_order ( & ["public", "private"] );
  for (name, dir) in [("public", "public"), ("private", "private")] {
    let path : PathBuf = tmp . path () . join (dir);
    std::fs::create_dir_all (&path) . unwrap ();
    config . skgrepos . get_mut ( &SkgRepoName::from (name) )
      . unwrap () . path = path; }
  std::fs::write (
    tmp . path () . join ("private/n.skg"),
    "pid: n\n" ) . unwrap ();
  let restriction : SkgrepoRestriction = SkgrepoRestriction {
    name    : SkgRepoSetName::from ("public"),
    skgrepos : [ SkgRepoName::from ("public") ]
      . into_iter () . collect (), };
  let refusal : Result<(), String> =
    refuse_delete_with_restricted_sections (
      &config, &restriction, &ID::new ("n") );
  assert! ( refusal . is_err (), "private section must refuse" );
  assert! ( refusal . unwrap_err ()
            . contains ("restricted repos") );
  assert! ( refuse_delete_with_restricted_sections (
    &config, &restriction, &ID::new ("only-public") ) . is_ok (),
    "a node with no restricted sections deletes fine" );
}

#[test]
fn relrepo_fact_and_request_round_trip_separately (
) {
  // The rendered fact and a requested skgrepo are distinct: the fact
  // remains under viewStats, while only editRequest carries write intent.
  use crate::org_to_text::viewnode_to_string;
  use crate::serve::parse_metadata_sexp::parse_metadata_to_viewnodemd;
  use crate::types::viewnode::{
    default_unrestrictedVognode, UnrestrictedVognode, Viewnode, ViewnodeKind, Vognode };

  let mut t : UnrestrictedVognode =
    default_unrestrictedVognode (
      ID::new ("n"), SkgRepoName::from ("public"), "N" . to_string () );
  t . viewStats . relRepo = Some ( SkgRepoName::from ("private") );
  t . relRepo_request = Some ( SkgRepoName::from ("secret") );
  let viewnode : Viewnode = Viewnode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind        : ViewnodeKind::Vognode ( Vognode::Unrestricted (t) ), };
  let config : SkgConfig =
    config_with_order ( & ["public", "private"] );
  let rendered : String =
    viewnode_to_string (&viewnode, &config) . unwrap ();
  assert! ( rendered . contains ("(relRepo private)"),
            "expected a relRepo atom in: {}", rendered );
  assert! ( rendered . contains (
    "(editRequest (relRepo secret))"),
    "expected a relRepo request in: {}", rendered );
  let full_sexp : String = format! ("(skg {})", rendered);
  let parsed = parse_metadata_to_viewnodemd (&full_sexp) . unwrap ();
  assert_eq! ( parsed . viewStats . relRepo,
               Some ( SkgRepoName::from ("private") ),
               "relRepo did not round-trip through render+parse" );
  assert_eq! ( parsed . relRepo_request,
               Some ( SkgRepoName::from ("secret") ),
               "relRepo request did not round-trip" );
}

#[test]
fn relrepo_requests_are_contextual_and_singular (
) {
  use crate::serve::parse_metadata_sexp::parse_metadata_to_viewnodemd;

  assert! ( parse_metadata_to_viewnodemd (
    "(skg alias (relRepo private) (editRequest (relRepo secret)))" )
            . is_ok (),
            "Alias is the only non-vognode which carries a relRepo" );
  for malformed in [
    "(skg (node (id n)) (editRequest (relRepo private)))",
    "(skg id (relRepo private))",
    "(skg (node (id n) (editRequest delete) (editRequest (relRepo private))))",
    "(skg (unknown (id absent) (editRequest delete)))",
    "(skg (unknown (id absent) (viewStats cycle)))",
  ] {
    assert! ( parse_metadata_to_viewnodemd (malformed) . is_err (),
              "malformed relRepo metadata was accepted: {}",
              malformed );
  }
  let (_, error, _) = crate::serve::parse_metadata_sexp::viewnode_from_metadata (
    &parse_metadata_to_viewnodemd (
      "(skg (node (id n) (repo public) writeProtected (editRequest (relRepo private))))" )
    . unwrap (), "N" . to_string (), None );
  assert! ( error . is_some (),
            "write-protected relRepo request must be rejected" );
}

#[test]
fn flag_requests_parse_round_trip_and_reject_write_protected_flags (
) {
  use crate::org_to_text::viewnode_to_string;
  use crate::serve::parse_metadata_sexp::parse_metadata_to_viewnodemd;
  use crate::types::nodes::complete::Flag;
  use crate::types::viewnode::{
    default_unrestrictedVognode, UnrestrictedVognode, NodeEditRequest, Viewnode,
    ViewnodeKind, Vognode};

  for value in [false, true] {
    let mut restriction : UnrestrictedVognode = default_unrestrictedVognode (
      ID::new ("n"), SkgRepoName::from ("public"), "N" . to_string () );
    if let crate::types::viewnode::Editability::Editable {
      edit_request, .. } = &mut restriction . editability
    { *edit_request = Some (NodeEditRequest::SetFlag {
        flag : Flag::NoSearchMatching,
        value, }); }
    else { unreachable! (); }
    let mut node : Viewnode = Viewnode {
      focused     : false,
      folded      : false,
      body_folded : false,
      kind        : ViewnodeKind::Vognode (Vognode::Unrestricted (restriction)), };
    let config : SkgConfig = config_with_order (&["public"]);
    let rendered : String = viewnode_to_string (&node, &config) . unwrap ();
    assert! ( rendered . contains (&format! (
      "(editRequest (flag noSearchMatching {}))", value)) );
    let parsed = parse_metadata_to_viewnodemd (
      &format! ("(skg {})", rendered)) . unwrap ();
    assert_eq! ( parsed . edit_request,
      Some (NodeEditRequest::SetFlag {
        flag : Flag::NoSearchMatching, value }) );
    node . consume_edit_request_after_save ();
    let ViewnodeKind::Vognode (Vognode::Unrestricted (restriction)) = &node . kind
      else { unreachable! (); };
    assert_eq! (restriction . edit_request (), None); }

  for malformed in [
    "(skg (node (id n) (editRequest (flag nope true))))",
    "(skg (node (id n) (editRequest (flag noSearchMatching maybe))))",
    "(skg (node (id n) (editRequest (flag hadId true))))",
    "(skg (node (id n) (editRequest (flag wasOverloaded false))))",
  ] {
    assert! ( parse_metadata_to_viewnodemd (malformed) . is_err (),
              "malformed/write-protected flag request was accepted: {}",
              malformed ); }
}

#[test]
fn unknown_relrepo_fact_and_request_round_trip_separately (
) {
  use crate::org_to_text::viewnode_to_string;
  use crate::serve::parse_metadata_sexp::parse_metadata_to_viewnodemd;
  use crate::types::viewnode::{mk_unknown_viewnode, Phantom, ViewnodeKind, Vognode};

  let mut unknown = mk_unknown_viewnode ( ID::new ("absent-raw") );
  if let ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (u))) = &mut unknown . kind {
    u . relRepo = Some ( SkgRepoName::from ("private") );
    u . relRepo_request = Some ( SkgRepoName::from ("secret") );
  } else { unreachable! (); }
  let config : SkgConfig = config_with_order ( & ["public", "private"] );
  let rendered : String = viewnode_to_string (&unknown, &config) . unwrap ();
  assert! ( rendered . contains (
    "(unknown (id absent-raw) (viewStats (relRepo private)) (editRequest (relRepo secret)))"),
            "Unknown facts and requests must serialize in stable order: {}", rendered );
  let parsed = parse_metadata_to_viewnodemd (
    &format! ("(skg {})", rendered) ) . unwrap ();
  assert_eq! ( parsed . unknown_relRepo,
               Some (SkgRepoName::from ("private")) );
  assert_eq! ( parsed . unknown_relRepo_request,
               Some (SkgRepoName::from ("secret")) );
}
