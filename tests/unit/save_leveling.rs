//! Unit tests for the sticky-else-default relRepo rule
//! ('apply_sticky_relRepos'), its home clamp, the hide floor, and the
//! restricted-set deletion refusal. Each test builds the exact graph fixture
//! it passes to the repo-resolution function.

use super::{apply_sticky_relRepos_in_graph, build_diskSupplemented_defineNodes,
            refuse_delete_with_inactive_sections};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::from_text::local_instruction_collection::lower::{
  NodeIntent, RequestedRelRepos};
use crate::repo_sets::ActiveRepoSet;
use crate::types::misc::{
  ID, MSV, RelPartner, SkgConfig, SkgfileRepo, RepoName,
  RepoSetName};
use crate::types::nodes::complete::{
  Flag, NodeComplete, empty_node_complete};
use crate::types::save::{DefineNode, SaveNode};

use std::collections::HashMap;
use std::path::PathBuf;

fn config_with_order (
  names : &[&str],
) -> SkgConfig {
  let mut repos : HashMap<RepoName, SkgfileRepo> =
    HashMap::new ();
  for name in names {
    repos . insert (
      RepoName::from (*name),
      SkgfileRepo {
        name         : RepoName::from (*name),
        abbreviation : None,
        path         : PathBuf::from ( format! ("owned/{}", name) ),
        user_owns_it : true, } ); }
  let mut config : SkgConfig =
    SkgConfig::dummyFromRepos (repos);
  config . repo_order =
    names . iter () . map ( |n| RepoName::from (*n) ) . collect ();
  config }

fn node_at (
  pid    : &str,
  repo : &str,
) -> NodeComplete {
  let mut n : NodeComplete = empty_node_complete ();
  n . pid = ID::new (pid);
  n . title = pid . to_string ();
  n . home_repo = RepoName::from (repo);
  n }

fn graph_from (
  nodes : &[NodeComplete],
) -> InRustGraph {
  InRustGraph::from_nodecompletes (nodes) }

fn pm (
  repo : &str,
  member : &str,
) -> RelPartner<ID> {
  RelPartner::at_relRepo (
    RepoName::from (repo), ID::new (member) ) }

#[test]
fn sticky_preserves_disk_repos_and_default_takes_more_private_home (
) {
  let config : SkgConfig =
    config_with_order ( & ["public", "private"] );
  let old_target : NodeComplete = node_at ("old", "public");
  let new_target : NodeComplete = node_at ("fresh", "private");
  let owner      : NodeComplete = node_at ("owner", "public");
  let graph : InRustGraph = graph_from ( & [ owner . clone (), old_target, new_target ] );
  let mut disk : NodeComplete = owner . clone ();
  disk . contains = vec! [
    pm ("private", "old") ]; // privatized on disk
  let mut buffer : NodeComplete = owner;
  buffer . contains = vec! [
    pm ("public", "old"),    // degenerate intent tag
    pm ("public", "fresh") ]; // new edge to a private-homed target
  let resolved : NodeComplete =
    apply_sticky_relRepos_in_graph (
      buffer, &disk, &RequestedRelRepos::default (), &graph, &config) . unwrap ();
  assert_eq! ( resolved . contains, vec! [
    pm ("private", "old"),    // STICKY: the disk's privatization survives
    pm ("private", "fresh") ] ); // DEFAULT: more private of the homes
}

#[test]
fn flag_requests_apply_after_disk_misc_restore_and_survive_repo_moves (
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
    let mut disk : NodeComplete = node_at ("node", "public");
    disk . misc = vec![
      Flag::Had_ID_Before_Import,
      Flag::Was_Overloaded];
    if disk_no_search {
      disk . misc . push (Flag::NoSearchMatching); }
    let graph : InRustGraph = graph_from (&[disk . clone ()]);
    let mut buffer = disk;
    buffer . home_repo = RepoName::from ("private");
    buffer . title = "edited" . to_string ();
    let mut intent = NodeIntent::graph_save_from_nodecomplete (buffer);
    if let NodeIntent::Save (save) = &mut intent {
      // This is the production buffer shape: misc is not textually carried.
      save . misc = Vec::new ();
      save . flag_request = request . map (|value|
        (Flag::NoSearchMatching, value)); }
    let planned = build_diskSupplemented_defineNodes (
      vec![intent], &graph, &config, None) . unwrap ();
    let DefineNode::Save (SaveNode (saved)) = &planned . instructions [0]
      else { panic! ("expected SaveNode"); };
    assert_eq! (&saved . misc, &expected);
    assert_eq! (planned . repo_moves . len (), 1);
  }
}

#[test]
fn sticky_round_trip_restores_a_resolved_extra_ids_raw_disk_spelling () {
  let config : SkgConfig = config_with_order (&["public"]);
  let mut target : NodeComplete = node_at ("target", "public");
  target . extra_ids = vec![ID::new ("target-extra")];
  let owner : NodeComplete = node_at ("owner", "public");
  let graph : InRustGraph = graph_from (&[owner . clone (), target]);
  let mut disk : NodeComplete = owner . clone ();
  disk . contains = vec![pm ("public", "target-extra")];
  let mut buffer : NodeComplete = owner;
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
  let child : NodeComplete = node_at ("child", "public");
  let owner_before : NodeComplete = node_at ("owner", "public");
  let graph : InRustGraph = graph_from ( & [ owner_before . clone (), child ] );
  let mut disk : NodeComplete = owner_before;
  disk . contains = vec! [ pm ("public", "child") ];
  let mut buffer : NodeComplete = node_at ("owner", "private");
  // the buffer moved the node's home to private
  buffer . contains = vec! [ pm ("private", "child") ];
  let resolved : NodeComplete =
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
  let hidden : NodeComplete = node_at ("victim", "public");
  let mut container_a : NodeComplete = node_at ("expl-a", "public");
  container_a . contains = vec! [ pm ("public", "victim") ];
  let mut container_b : NodeComplete = node_at ("expl-b", "public");
  container_b . contains = vec! [ pm ("public", "victim") ];
  let owner : NodeComplete = node_at ("owner", "public");
  let graph : InRustGraph = graph_from ( & [
    owner . clone (), hidden, container_a, container_b ] );
  { // Only a PRIVATE subscription explains the hide: the hide must
    // be private, else it leaks the inference that the private
    // subscription exists. The privatized subscription is a DISK
    // fact (sticky preserves it); raising its privacy from the
    // buffer would come through the (relRepo ...) atom (see the
    // explicit_repo_* tests below), landed with render-and-gating.
    let mut disk : NodeComplete = owner . clone ();
    disk . subscribes_to = MSV::Specified ( vec! [
      pm ("private", "expl-a") ] );
    let mut buffer : NodeComplete = owner . clone ();
    buffer . subscribes_to = MSV::Specified ( vec! [
      pm ("public", "expl-a") ] ); // degenerate tag; sticky restores
    buffer . hides_from_its_subscriptions = MSV::Specified ( vec! [
      pm ("public", "victim") ] ); // degenerate tag
    let resolved : NodeComplete =
      apply_sticky_relRepos_in_graph (
        buffer, &disk, &RequestedRelRepos::default (), &graph, &config) . unwrap ();
    assert_eq! (
      resolved . subscribes_to . or_default (),
      & [ pm ("private", "expl-a") ] );
    assert_eq! (
      resolved . hides_from_its_subscriptions . or_default (),
      & [ pm ("private", "victim") ] ); }
  { // A PUBLIC explanation exists too: the inference is innocent,
    // so the hide may stay public.
    let mut disk : NodeComplete = owner . clone ();
    disk . subscribes_to = MSV::Specified ( vec! [
      pm ("private", "expl-a"),
      pm ("public",  "expl-b") ] );
    let mut buffer : NodeComplete = owner;
    buffer . subscribes_to = MSV::Specified ( vec! [
      pm ("public", "expl-a"),
      pm ("public", "expl-b") ] );
    buffer . hides_from_its_subscriptions = MSV::Specified ( vec! [
      pm ("public", "victim") ] );
    let resolved : NodeComplete =
      apply_sticky_relRepos_in_graph (
        buffer, &disk, &RequestedRelRepos::default (), &graph, &config) . unwrap ();
    assert_eq! (
      resolved . hides_from_its_subscriptions . or_default (),
      & [ pm ("public", "victim") ] ); }
}

#[test]
fn explicit_repo_at_or_more_private_than_floor_is_honored (
) {
  // Allowed-side acceptance (render-and-gating, 5_plan.org): an
  // explicit '(editRequest (relRepo ...))' repo that is at least as private as
  // the default floor wins outright.
  let config : SkgConfig =
    config_with_order ( & ["public", "trusted", "private"] );
  let child : NodeComplete = node_at ("child", "public");
  let owner_before : NodeComplete = node_at ("owner", "public");
  let graph : InRustGraph = graph_from ( & [ owner_before . clone (), child ] );
  let mut disk : NodeComplete = owner_before;
  disk . contains = vec! [ pm ("public", "child") ];
  let mut buffer : NodeComplete = node_at ("owner", "public");
  buffer . contains = vec! [ pm ("public", "child") ]; // degenerate tag
  let explicit : RequestedRelRepos = RequestedRelRepos {
    contains : HashMap::from ([
      ( ID::new ("child"), RepoName::from ("trusted") ) ]),
    .. RequestedRelRepos::default () };
  let resolved : NodeComplete =
    apply_sticky_relRepos_in_graph (buffer, &disk, &explicit, &graph, &config) . unwrap ();
  assert_eq! ( resolved . contains, vec! [
    pm ("trusted", "child") ],
    "an explicit repo at or more private than the default wins outright" );
}

#[test]
fn explicit_repo_more_public_than_floor_is_rejected (
) {
  // More-public-than-floor rejection (render-and-gating, 5_plan.org): an
  // explicit repo more PUBLIC than the DEFAULT floor is a save
  // error naming the member, the offered repo, and the floor.
  let config : SkgConfig =
    config_with_order ( & ["public", "trusted", "private"] );
  let child : NodeComplete = node_at ("child", "private"); // forces the default floor to "private"
  let owner_before : NodeComplete = node_at ("owner", "public");
  let graph : InRustGraph = graph_from ( & [ owner_before . clone (), child ] );
  let disk : NodeComplete = owner_before; // no sticky entry for "child"
  let mut buffer : NodeComplete = node_at ("owner", "public");
  buffer . contains = vec! [ pm ("public", "child") ]; // degenerate tag
  let explicit : RequestedRelRepos = RequestedRelRepos {
    contains : HashMap::from ([
      ( ID::new ("child"), RepoName::from ("public") ) ]), // more public than the "private" floor
    .. RequestedRelRepos::default () };
  let err : String =
    apply_sticky_relRepos_in_graph (buffer, &disk, &explicit, &graph, &config)
    . unwrap_err ();
  assert! ( err . contains ("child"),   "names the member: {}", err );
  assert! ( err . contains ("public"),  "names the offered relRepo: {}", err );
  assert! ( err . contains ("private"), "names the floor: {}", err );
}

#[test]
fn explicit_relRepo_moves_a_sticky_edge_to_its_default (
) {
  // The BUG-and-fix_make-edge-more-public.org fix: an explicit
  // relRepo validates against the DEFAULT floor, not the disk relRepo,
  // so it can lower a stuck edge's privacy back to the default.
  let config : SkgConfig =
    config_with_order ( & ["public", "trusted", "private"] );
  let child : NodeComplete = node_at ("child", "public");
  let owner_before : NodeComplete = node_at ("owner", "public");
  let graph : InRustGraph = graph_from ( & [ owner_before . clone (), child ] );
  let mut disk : NodeComplete = owner_before;
  disk . contains = vec! [
    pm ("private", "child") ]; // stuck more private than its default
  let mut buffer : NodeComplete = node_at ("owner", "public");
  buffer . contains = vec! [ pm ("public", "child") ]; // degenerate tag
  let explicit : RequestedRelRepos = RequestedRelRepos {
    contains : HashMap::from ([
      ( ID::new ("child"), RepoName::from ("public") ) ]), // = default
    .. RequestedRelRepos::default () };
  let resolved : NodeComplete =
    apply_sticky_relRepos_in_graph (buffer, &disk, &explicit, &graph, &config) . unwrap ();
  assert_eq! ( resolved . contains, vec! [
    pm ("public", "child") ],
    "an explicit repo AT the default lowers the sticky edge's privacy" );
}

#[test]
fn explicit_repo_between_default_and_sticky_is_accepted (
) {
  let config : SkgConfig =
    config_with_order ( & ["public", "trusted", "private"] );
  let child : NodeComplete = node_at ("child", "public");
  let owner_before : NodeComplete = node_at ("owner", "public");
  let graph : InRustGraph = graph_from ( & [ owner_before . clone (), child ] );
  let mut disk : NodeComplete = owner_before;
  disk . contains = vec! [ pm ("private", "child") ];
  let mut buffer : NodeComplete = node_at ("owner", "public");
  buffer . contains = vec! [ pm ("public", "child") ]; // degenerate tag
  let explicit : RequestedRelRepos = RequestedRelRepos {
    contains : HashMap::from ([
      ( ID::new ("child"), RepoName::from ("trusted") ) ]),
    .. RequestedRelRepos::default () };
  let resolved : NodeComplete =
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
  let child : NodeComplete = node_at ("child", "trusted"); // default floor: trusted
  let owner_before : NodeComplete = node_at ("owner", "public");
  let graph : InRustGraph = graph_from ( & [ owner_before . clone (), child ] );
  let mut disk : NodeComplete = owner_before;
  disk . contains = vec! [
    pm ("private", "child") ]; // sticky sits more private than the default
  let mut buffer : NodeComplete = node_at ("owner", "public");
  buffer . contains = vec! [ pm ("public", "child") ]; // degenerate tag
  let explicit : RequestedRelRepos = RequestedRelRepos {
    contains : HashMap::from ([
      ( ID::new ("child"), RepoName::from ("public") ) ]), // more public than default
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
fn explicit_at_a_more_public_than_default_disk_repo_round_trips (
) {
  // Legacy or hand-authored data can put an owned-to-owned edge at a
  // repo more public than its default. Render emits '(relRepo ...)'
  // for every off-default edge, so that atom must save back unchanged
  // (explicit == disk relRepo), and moving it partway toward the
  // default is fine; moving it still more public is forbidden.
  let config : SkgConfig =
    config_with_order ( & ["public", "trusted", "private"] );
  let child : NodeComplete = node_at ("child", "private");
  let owner_before : NodeComplete = node_at ("owner", "public");
  let graph : InRustGraph = graph_from ( & [ owner_before . clone (), child ] );
  let mut disk : NodeComplete = owner_before;
  disk . contains = vec! [
    pm ("public", "child") ]; // more public than the "private" default
  { // Holding the disk relRepo round-trips.
    let mut buffer : NodeComplete = node_at ("owner", "public");
    buffer . contains = vec! [ pm ("public", "child") ];
    let explicit : RequestedRelRepos = RequestedRelRepos {
      contains : HashMap::from ([
        ( ID::new ("child"), RepoName::from ("public") ) ]),
      .. RequestedRelRepos::default () };
    let resolved : NodeComplete =
      apply_sticky_relRepos_in_graph (
        buffer, &disk, &explicit, &graph, &config ) . unwrap ();
    assert_eq! ( resolved . contains, vec! [ pm ("public", "child") ],
      "the rendered atom saves back unchanged" ); }
  { // Moving it partway toward the default is accepted.
    let mut buffer : NodeComplete = node_at ("owner", "public");
    buffer . contains = vec! [ pm ("public", "child") ];
    let explicit : RequestedRelRepos = RequestedRelRepos {
      contains : HashMap::from ([
        ( ID::new ("child"), RepoName::from ("trusted") ) ]),
      .. RequestedRelRepos::default () };
    let resolved : NodeComplete =
      apply_sticky_relRepos_in_graph (
        buffer, &disk, &explicit, &graph, &config ) . unwrap ();
    assert_eq! ( resolved . contains, vec! [ pm ("trusted", "child") ],
      "making a legacy more-public edge more private is accepted" ); }
}

#[test]
fn explicit_lowering_moves_the_edge_between_section_files (
) {
  // The repro from BUG-and-fix_make-edge-more-public.org, as files:
  // a child once homed in "private" was moved home to "trusted",
  // but the containment edge stayed stuck at "private" (sticky).
  // Explicitly lowering the edge's privacy to its new default moves
  // the membership line from the owner's private section file to a
  // trusted one, deleting the emptied private file.
  use crate::dbs::filesystem::one_node::write_nodecomplete_telescope;
  let tmp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
  let mut config : SkgConfig =
    config_with_order ( & ["public", "trusted", "private"] );
  for name in ["public", "trusted", "private"] {
    let path : PathBuf = tmp . path () . join (name);
    std::fs::create_dir_all (&path) . unwrap ();
    config . repos . get_mut ( &RepoName::from (name) )
      . unwrap () . path = path; }
  let child : NodeComplete = node_at ("child", "trusted"); // home already moved
  let owner : NodeComplete = node_at ("owner", "public");
  let graph : InRustGraph = graph_from ( & [ owner . clone (), child ] );
  let mut disk : NodeComplete = owner;
  disk . contains = vec! [ pm ("private", "child") ];
  write_nodecomplete_telescope ( &disk, &config ) . unwrap ();
  let private_file = tmp . path () . join ("private/owner.skg");
  let trusted_file = tmp . path () . join ("trusted/owner.skg");
  let public_file  = tmp . path () . join ("public/owner.skg");
  assert! ( private_file . is_file (),
            "before: the stuck edge lives in the private section" );
  assert! ( public_file . is_file (),
            "before: the home section exists" );
  assert! ( ! trusted_file . is_file (),
            "before: no trusted section yet" );
  let mut buffer : NodeComplete = node_at ("owner", "public");
  buffer . contains = vec! [ pm ("public", "child") ]; // degenerate tag
  let explicit : RequestedRelRepos = RequestedRelRepos {
    contains : HashMap::from ([
      ( ID::new ("child"), RepoName::from ("trusted") ) ]), // the new default
    .. RequestedRelRepos::default () };
  let resolved : NodeComplete =
    apply_sticky_relRepos_in_graph (buffer, &disk, &explicit, &graph, &config) . unwrap ();
  assert_eq! ( resolved . contains, vec! [ pm ("trusted", "child") ] );
  write_nodecomplete_telescope ( &resolved, &config ) . unwrap ();
  assert! ( ! private_file . is_file (),
            "after: the emptied private section is deleted" );
  assert! ( trusted_file . is_file (),
            "after: the edge's new section exists" );
  assert! ( std::fs::read_to_string (&trusted_file) . unwrap ()
            . contains ("child"),
            "after: the trusted section holds the membership" );
  assert! ( ! std::fs::read_to_string (&public_file) . unwrap ()
            . contains ("child"),
            "after: the home section does not name the child" );
}

#[test]
fn same_save_child_home_move_allows_publicizing_its_parent_edge (
) {
  // A repo move and its parent's explicit relRepo request occur in one
  // save. The parent must calculate the edge default from the child's NEW
  // home, not the pre-save graph's old home.
  let config : SkgConfig = config_with_order ( & ["public", "pers-p"] );
  let mut parent : NodeComplete = node_at ("parent", "public");
  parent . contains = vec! [ pm ("pers-p", "child") ];
  let child : NodeComplete = node_at ("child", "pers-p");
  let graph : InRustGraph = graph_from ( & [ parent . clone (), child ] );

  let mut parent_intent : NodeIntent =
    NodeIntent::graph_save_from_nodecomplete (parent);
  if let NodeIntent::Save (intent) = &mut parent_intent {
    intent . contains = MSV::Specified (vec! [(
      ID::new ("child"), Some (RepoName::from ("public"))) ]); }
  let child_intent : NodeIntent = NodeIntent::graph_save_from_nodecomplete (
    node_at ("child", "public"));

  let planned = build_diskSupplemented_defineNodes (
    vec! [ parent_intent, child_intent ], &graph, &config, None )
    . expect ("the same-save home move makes public the edge's default");
  let parent = planned . instructions . into_iter ()
    .find_map ( |instruction| match instruction {
      DefineNode::Save (SaveNode (node)) if node . pid == ID::new ("parent")
        => Some (node),
      _ => None })
    .expect ("parent save instruction");
  assert_eq! ( parent . contains, vec! [ pm ("public", "child") ] );
  assert_eq! ( planned . repo_moves . len (), 1 );
  assert_eq! ( planned . repo_moves [0] . pid, ID::new ("child") );
}

#[test]
fn same_save_hidden_node_home_move_sets_the_new_hide_repo (
) {
  // Hides have an inferred relRepo rather than an explicit
  // relRepo request, but their endpoint floor must use the same pending
  // home view as the other relationship kinds.
  let config : SkgConfig = config_with_order ( & ["public", "pers-p"] );
  let hider : NodeComplete = node_at ("hider", "public");
  let hidden : NodeComplete = node_at ("hidden", "pers-p");
  let graph : InRustGraph = graph_from ( & [ hider . clone (), hidden ] );

  let mut hider_intent : NodeIntent =
    NodeIntent::graph_save_from_nodecomplete (hider);
  if let NodeIntent::Save (intent) = &mut hider_intent {
    intent . hides_from_its_subscriptions =
      MSV::Specified (vec! [ID::new ("hidden")]); }
  let hidden_intent : NodeIntent = NodeIntent::graph_save_from_nodecomplete (
    node_at ("hidden", "public"));

  let planned = build_diskSupplemented_defineNodes (
    vec! [ hider_intent, hidden_intent ], &graph, &config, None )
    . expect ("the pending hidden-node home participates in the hide floor");
  let hider = planned . instructions . into_iter ()
    .find_map ( |instruction| match instruction {
      DefineNode::Save (SaveNode (node)) if node . pid == ID::new ("hider")
        => Some (node),
      _ => None })
    .expect ("hider save instruction");
  assert_eq! (
    hider . hides_from_its_subscriptions,
    MSV::Specified (vec! [pm ("public", "hidden")]) );
}

#[test]
fn new_private_child_in_the_same_save_gets_a_private_edge (
) {
  // The child is absent from the pre-save graph, so its Save intent is the
  // only available repo of its home. Falling back to the public parent's
  // home here would create exactly the leak shape the endpoint floor forbids.
  let config : SkgConfig = config_with_order ( & ["public", "pers-p"] );
  let parent : NodeComplete = node_at ("parent", "public");
  let graph : InRustGraph = graph_from ( & [ parent . clone () ] );

  let mut parent_intent : NodeIntent =
    NodeIntent::graph_save_from_nodecomplete (parent);
  if let NodeIntent::Save (intent) = &mut parent_intent {
    intent . contains = MSV::Specified (vec! [(
      ID::new ("new-child"), None )]); }
  let child_intent : NodeIntent = NodeIntent::graph_save_from_nodecomplete (
    node_at ("new-child", "pers-p"));

  let planned = build_diskSupplemented_defineNodes (
    vec! [ parent_intent, child_intent ], &graph, &config, None )
    . expect ("same-save new nodes supply their relationship endpoint homes");
  let parent = planned . instructions . into_iter ()
    .find_map ( |instruction| match instruction {
      DefineNode::Save (SaveNode (node)) if node . pid == ID::new ("parent")
        => Some (node),
      _ => None })
    .expect ("parent save instruction");
  assert_eq! ( parent . contains, vec! [ pm ("pers-p", "new-child") ] );
}

#[test]
fn owned_to_foreign_new_edges_default_to_the_owner_home (
) {
  for relation in ["contains", "subscribes_to", "overrides_view_of"] {
    for (owner_home, member_home) in
        [("public", "private"), ("private", "public")] {
      let mut config : SkgConfig =
        config_with_order (&["public", "private"]);
      config . repos . get_mut (&RepoName::from (member_home))
        . unwrap () . user_owns_it = false;
      config . repos . get_mut (&RepoName::from (owner_home))
        . unwrap () . user_owns_it = true;
      let owner : NodeComplete = node_at ("owner", owner_home);
      let member : NodeComplete = node_at ("member", member_home);
      let graph : InRustGraph = graph_from (&[owner . clone (), member]);
      let disk : NodeComplete = owner;
      let mut buffer : NodeComplete = node_at ("owner", owner_home);
      match relation {
        "contains" => buffer . contains = vec! [pm (owner_home, "member")],
        "subscribes_to" => buffer . subscribes_to =
          MSV::Specified (vec! [pm (owner_home, "member")]),
        "overrides_view_of" => buffer . overrides_view_of =
          MSV::Specified (vec! [pm (owner_home, "member")]),
        _ => unreachable! (), }
      let resolved : NodeComplete = apply_sticky_relRepos_in_graph (
        buffer, &disk, &RequestedRelRepos::default (), &graph, &config ) . unwrap ();
      let repo : &RepoName = match relation {
        "contains" => &resolved . contains [0] . relRepo,
        "subscribes_to" =>
          &resolved . subscribes_to . or_default () [0] . relRepo,
        "overrides_view_of" =>
          &resolved . overrides_view_of . or_default () [0] . relRepo,
        _ => unreachable! (), };
      assert_eq! (repo, &RepoName::from (owner_home),
                  "relation {}", relation); }}
}

#[test]
fn owned_to_foreign_explicit_owned_repo_is_allowed_and_foreign_refused (
) {
  let mut config : SkgConfig =
    config_with_order (&["public", "foreign", "private"]);
  config . repos . get_mut (&RepoName::from ("foreign"))
    . unwrap () . user_owns_it = false;
  let mut owner : NodeComplete = node_at ("owner", "public");
  let member : NodeComplete = node_at ("member", "foreign");
  let graph : InRustGraph = graph_from (&[owner . clone (), member]);

  let disk : NodeComplete = owner . clone ();
  owner . contains = vec! [pm ("public", "member")];
  let allowed : RequestedRelRepos = RequestedRelRepos {
    contains : HashMap::from ([
      (ID::new ("member"), RepoName::from ("private")) ]),
    .. RequestedRelRepos::default () };
  let resolved : NodeComplete = apply_sticky_relRepos_in_graph (
    owner . clone (), &disk, &allowed, &graph, &config ) . unwrap ();
  assert_eq! (resolved . contains [0] . relRepo,
              RepoName::from ("private"));

  let refused : RequestedRelRepos = RequestedRelRepos {
    contains : HashMap::from ([
      (ID::new ("member"), RepoName::from ("foreign")) ]),
    .. RequestedRelRepos::default () };
  let error : String = apply_sticky_relRepos_in_graph (
    owner, &disk, &refused, &graph, &config ) . unwrap_err ();
  assert! (error . contains ("non-owned repo 'foreign'"), "{}", error);
}

#[test]
fn explicit_alias_relRepo_is_load_bearing_and_validated (
) {
  let config : SkgConfig =
    config_with_order (&["public", "trusted", "private"]);
  let graph : InRustGraph = InRustGraph::new ();
  let disk : NodeComplete = node_at ("owner", "public");
  let mut buffer : NodeComplete = disk . clone ();
  buffer . aliases = MSV::Specified (vec! [
    crate::types::misc::RelPartner::at_relRepo (
      RepoName::from ("public"), "nickname" . to_string ()) ]);
  let explicit : RequestedRelRepos = RequestedRelRepos {
    aliases : HashMap::from ([
      ("nickname" . to_string (), RepoName::from ("private")) ]),
    .. RequestedRelRepos::default () };
  let resolved : NodeComplete = apply_sticky_relRepos_in_graph (
    buffer, &disk, &explicit, &graph, &config ) . unwrap ();
  assert_eq! (
    resolved . aliases . or_default () [0] . relRepo,
    RepoName::from ("private") );

  let mut foreign_config : SkgConfig = config;
  foreign_config . repos . get_mut (&RepoName::from ("private"))
    . unwrap () . user_owns_it = false;
  let mut buffer : NodeComplete = disk . clone ();
  buffer . aliases = MSV::Specified (vec! [
    crate::types::misc::RelPartner::at_relRepo (
      RepoName::from ("public"), "nickname" . to_string ()) ]);
  let error : String = apply_sticky_relRepos_in_graph (
    buffer, &disk, &explicit, &graph, &foreign_config ) . unwrap_err ();
  assert! (error . contains ("non-owned relRepo 'private'"), "{}", error);
}

#[test]
fn restricted_delete_refusal_sees_inactive_sections (
) {
  let tmp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
  let mut config : SkgConfig =
    config_with_order ( & ["public", "private"] );
  for (name, dir) in [("public", "public"), ("private", "private")] {
    let path : PathBuf = tmp . path () . join (dir);
    std::fs::create_dir_all (&path) . unwrap ();
    config . repos . get_mut ( &RepoName::from (name) )
      . unwrap () . path = path; }
  std::fs::write (
    tmp . path () . join ("private/n.skg"),
    "pid: n\n" ) . unwrap ();
  let active : ActiveRepoSet = ActiveRepoSet {
    name    : RepoSetName::from ("public"),
    repos : [ RepoName::from ("public") ]
      . into_iter () . collect (), };
  let refusal : Result<(), String> =
    refuse_delete_with_inactive_sections (
      &config, &active, &ID::new ("n") );
  assert! ( refusal . is_err (), "private section must refuse" );
  assert! ( refusal . unwrap_err ()
            . contains ("inactive repos") );
  assert! ( refuse_delete_with_inactive_sections (
    &config, &active, &ID::new ("only-public") ) . is_ok (),
    "a node with no inactive sections deletes fine" );
}

#[test]
fn relrepo_fact_and_request_round_trip_separately (
) {
  // The rendered fact and a requested repo are distinct: the fact
  // remains under viewStats, while only editRequest carries write intent.
  use crate::org_to_text::viewnode_to_string;
  use crate::serve::parse_metadata_sexp::parse_metadata_to_viewnodemd;
  use crate::types::viewnode::{
    default_activeNode, ActiveNode, Viewnode, ViewnodeKind, Vognode };

  let mut t : ActiveNode =
    default_activeNode (
      ID::new ("n"), RepoName::from ("public"), "N" . to_string () );
  t . viewStats . relRepo = Some ( RepoName::from ("private") );
  t . relRepo_request = Some ( RepoName::from ("secret") );
  let viewnode : Viewnode = Viewnode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind        : ViewnodeKind::Vognode ( Vognode::Active (t) ), };
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
               Some ( RepoName::from ("private") ),
               "relRepo did not round-trip through render+parse" );
  assert_eq! ( parsed . relRepo_request,
               Some ( RepoName::from ("secret") ),
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
    default_activeNode, ActiveNode, NodeEditRequest, Viewnode,
    ViewnodeKind, Vognode};

  for value in [false, true] {
    let mut active : ActiveNode = default_activeNode (
      ID::new ("n"), RepoName::from ("public"), "N" . to_string () );
    if let crate::types::viewnode::Editability::Definitive {
      edit_request, .. } = &mut active . editability
    { *edit_request = Some (NodeEditRequest::SetFlag {
        flag : Flag::NoSearchMatching,
        value, }); }
    else { unreachable! (); }
    let mut node : Viewnode = Viewnode {
      focused     : false,
      folded      : false,
      body_folded : false,
      kind        : ViewnodeKind::Vognode (Vognode::Active (active)), };
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
    let ViewnodeKind::Vognode (Vognode::Active (active)) = &node . kind
      else { unreachable! (); };
    assert_eq! (active . edit_request (), None); }

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
    u . relRepo = Some ( RepoName::from ("private") );
    u . relRepo_request = Some ( RepoName::from ("secret") );
  } else { unreachable! (); }
  let config : SkgConfig = config_with_order ( & ["public", "private"] );
  let rendered : String = viewnode_to_string (&unknown, &config) . unwrap ();
  assert! ( rendered . contains (
    "(unknown (id absent-raw) (viewStats (relRepo private)) (editRequest (relRepo secret)))"),
            "Unknown facts and requests must serialize in stable order: {}", rendered );
  let parsed = parse_metadata_to_viewnodemd (
    &format! ("(skg {})", rendered) ) . unwrap ();
  assert_eq! ( parsed . unknown_relRepo,
               Some (RepoName::from ("private")) );
  assert_eq! ( parsed . unknown_relRepo_request,
               Some (RepoName::from ("secret")) );
}
