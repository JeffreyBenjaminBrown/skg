//! Unit tests for the sticky-else-default source rule
//! ('apply_sticky_sources'), its home clamp, the hide floor, and the
//! restricted-set deletion refusal. Installs the process-global
//! graph handle, so it assumes per-test process isolation (nextest),
//! like tests/override_substitution.rs.

use super::{apply_sticky_sources, refuse_delete_with_inactive_sections};
use crate::dbs::in_rust_graph::{InRustGraph, install_or_swap_global_handle, new_handle};
use crate::from_text::local_instruction_collection::lower::ExplicitSources;
use crate::source_sets::ActiveSourceSet;
use crate::types::misc::{
  ID, MSV, MemberAtSource, SkgConfig, SkgfileSource, SourceName,
  SourceSetName};
use crate::types::nodes::complete::{NodeComplete, empty_node_complete};

use std::collections::HashMap;
use std::path::PathBuf;

fn config_with_order (
  names : &[&str],
) -> SkgConfig {
  let mut sources : HashMap<SourceName, SkgfileSource> =
    HashMap::new ();
  for name in names {
    sources . insert (
      SourceName::from (*name),
      SkgfileSource {
        name         : SourceName::from (*name),
        abbreviation : None,
        path         : PathBuf::from ( format! ("owned/{}", name) ),
        user_owns_it : true, } ); }
  let mut config : SkgConfig =
    SkgConfig::dummyFromSources (sources);
  config . sources . set_order (
    names . iter () . map ( |n| SourceName::from (*n) ) . collect ());
  config }

fn node_at (
  pid    : &str,
  source : &str,
) -> NodeComplete {
  let mut n : NodeComplete = empty_node_complete ();
  n . pid = ID::new (pid);
  n . title = pid . to_string ();
  n . source = SourceName::from (source);
  n }

fn install_graph (
  nodes : &[NodeComplete],
) {
  install_or_swap_global_handle ( new_handle (
    InRustGraph::from_nodecompletes (nodes) )); }

fn pm (
  source : &str,
  member : &str,
) -> MemberAtSource<ID> {
  MemberAtSource::at_source (
    SourceName::from (source), ID::new (member) ) }

#[test]
fn sticky_preserves_disk_sources_and_default_takes_more_private_home (
) {
  let config : SkgConfig =
    config_with_order ( & ["public", "private"] );
  let old_target : NodeComplete = node_at ("old", "public");
  let new_target : NodeComplete = node_at ("fresh", "private");
  let owner      : NodeComplete = node_at ("owner", "public");
  install_graph ( & [ owner . clone (), old_target, new_target ] );
  let mut disk : NodeComplete = owner . clone ();
  disk . contains = vec! [
    pm ("private", "old") ]; // privatized on disk
  let mut buffer : NodeComplete = owner;
  buffer . contains = vec! [
    pm ("public", "old"),    // degenerate intent tag
    pm ("public", "fresh") ]; // new edge to a private-homed target
  let resolved : NodeComplete =
    apply_sticky_sources (
      buffer, &disk, &ExplicitSources::default (), &config) . unwrap ();
  assert_eq! ( resolved . contains, vec! [
    pm ("private", "old"),    // STICKY: the disk's privatization survives
    pm ("private", "fresh") ] ); // DEFAULT: more private of the homes
}

#[test]
fn home_move_to_more_private_clamps_member_sources_up (
) {
  let config : SkgConfig =
    config_with_order ( & ["public", "private"] );
  let child : NodeComplete = node_at ("child", "public");
  let owner_before : NodeComplete = node_at ("owner", "public");
  install_graph ( & [ owner_before . clone (), child ] );
  let mut disk : NodeComplete = owner_before;
  disk . contains = vec! [ pm ("public", "child") ];
  let mut buffer : NodeComplete = node_at ("owner", "private");
  // the buffer moved the node's home to private
  buffer . contains = vec! [ pm ("private", "child") ];
  let resolved : NodeComplete =
    apply_sticky_sources (
      buffer, &disk, &ExplicitSources::default (), &config) . unwrap ();
  assert_eq! ( resolved . contains, vec! [
    pm ("private", "child") ],
    "the sticky public source rises to the new, more private home" );
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
  install_graph ( & [
    owner . clone (), hidden, container_a, container_b ] );
  { // Only a PRIVATE subscription explains the hide: the hide must
    // be private, else it leaks the inference that the private
    // subscription exists. The privatized subscription is a DISK
    // fact (sticky preserves it); raising its privacy from the
    // buffer would come through the (relSource ...) atom (see the
    // explicit_source_* tests below), landed with render-and-gating.
    let mut disk : NodeComplete = owner . clone ();
    disk . subscribes_to = MSV::Specified ( vec! [
      pm ("private", "expl-a") ] );
    let mut buffer : NodeComplete = owner . clone ();
    buffer . subscribes_to = MSV::Specified ( vec! [
      pm ("public", "expl-a") ] ); // degenerate tag; sticky restores
    buffer . hides_from_its_subscriptions = MSV::Specified ( vec! [
      pm ("public", "victim") ] ); // degenerate tag
    let resolved : NodeComplete =
      apply_sticky_sources (
        buffer, &disk, &ExplicitSources::default (), &config) . unwrap ();
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
      apply_sticky_sources (
        buffer, &disk, &ExplicitSources::default (), &config) . unwrap ();
    assert_eq! (
      resolved . hides_from_its_subscriptions . or_default (),
      & [ pm ("public", "victim") ] ); }
}

#[test]
fn explicit_source_at_or_more_private_than_floor_is_honored (
) {
  // Allowed-side acceptance (render-and-gating, 5_plan.org): an
  // explicit '(relSource ...)' source that is at least as private as
  // the default floor wins outright.
  let config : SkgConfig =
    config_with_order ( & ["public", "trusted", "private"] );
  let child : NodeComplete = node_at ("child", "public");
  let owner_before : NodeComplete = node_at ("owner", "public");
  install_graph ( & [ owner_before . clone (), child ] );
  let mut disk : NodeComplete = owner_before;
  disk . contains = vec! [ pm ("public", "child") ];
  let mut buffer : NodeComplete = node_at ("owner", "public");
  buffer . contains = vec! [ pm ("public", "child") ]; // degenerate tag
  let explicit : ExplicitSources = ExplicitSources {
    contains : HashMap::from ([
      ( ID::new ("child"), SourceName::from ("trusted") ) ]),
    .. ExplicitSources::default () };
  let resolved : NodeComplete =
    apply_sticky_sources (buffer, &disk, &explicit, &config) . unwrap ();
  assert_eq! ( resolved . contains, vec! [
    pm ("trusted", "child") ],
    "an explicit source at or more private than the default wins outright" );
}

#[test]
fn explicit_source_more_public_than_floor_is_rejected (
) {
  // More-public-than-floor rejection (render-and-gating, 5_plan.org): an
  // explicit source more PUBLIC than the DEFAULT floor is a save
  // error naming the member, the offered source, and the floor.
  let config : SkgConfig =
    config_with_order ( & ["public", "trusted", "private"] );
  let child : NodeComplete = node_at ("child", "private"); // forces the default floor to "private"
  let owner_before : NodeComplete = node_at ("owner", "public");
  install_graph ( & [ owner_before . clone (), child ] );
  let disk : NodeComplete = owner_before; // no sticky entry for "child"
  let mut buffer : NodeComplete = node_at ("owner", "public");
  buffer . contains = vec! [ pm ("public", "child") ]; // degenerate tag
  let explicit : ExplicitSources = ExplicitSources {
    contains : HashMap::from ([
      ( ID::new ("child"), SourceName::from ("public") ) ]), // more public than the "private" floor
    .. ExplicitSources::default () };
  let err : String =
    apply_sticky_sources (buffer, &disk, &explicit, &config)
    . unwrap_err ();
  assert! ( err . contains ("child"),   "names the member: {}", err );
  assert! ( err . contains ("public"),  "names the offered source: {}", err );
  assert! ( err . contains ("private"), "names the floor: {}", err );
}

#[test]
fn explicit_source_moves_a_sticky_edge_to_its_default (
) {
  // The BUG-and-fix_make-edge-more-public.org fix: an explicit
  // source validates against the DEFAULT floor, not the disk source,
  // so it can lower a stuck edge's privacy back to the default.
  let config : SkgConfig =
    config_with_order ( & ["public", "trusted", "private"] );
  let child : NodeComplete = node_at ("child", "public");
  let owner_before : NodeComplete = node_at ("owner", "public");
  install_graph ( & [ owner_before . clone (), child ] );
  let mut disk : NodeComplete = owner_before;
  disk . contains = vec! [
    pm ("private", "child") ]; // stuck more private than its default
  let mut buffer : NodeComplete = node_at ("owner", "public");
  buffer . contains = vec! [ pm ("public", "child") ]; // degenerate tag
  let explicit : ExplicitSources = ExplicitSources {
    contains : HashMap::from ([
      ( ID::new ("child"), SourceName::from ("public") ) ]), // = default
    .. ExplicitSources::default () };
  let resolved : NodeComplete =
    apply_sticky_sources (buffer, &disk, &explicit, &config) . unwrap ();
  assert_eq! ( resolved . contains, vec! [
    pm ("public", "child") ],
    "an explicit source AT the default lowers the sticky edge's privacy" );
}

#[test]
fn explicit_source_between_default_and_sticky_is_accepted (
) {
  let config : SkgConfig =
    config_with_order ( & ["public", "trusted", "private"] );
  let child : NodeComplete = node_at ("child", "public");
  let owner_before : NodeComplete = node_at ("owner", "public");
  install_graph ( & [ owner_before . clone (), child ] );
  let mut disk : NodeComplete = owner_before;
  disk . contains = vec! [ pm ("private", "child") ];
  let mut buffer : NodeComplete = node_at ("owner", "public");
  buffer . contains = vec! [ pm ("public", "child") ]; // degenerate tag
  let explicit : ExplicitSources = ExplicitSources {
    contains : HashMap::from ([
      ( ID::new ("child"), SourceName::from ("trusted") ) ]),
    .. ExplicitSources::default () };
  let resolved : NodeComplete =
    apply_sticky_sources (buffer, &disk, &explicit, &config) . unwrap ();
  assert_eq! ( resolved . contains, vec! [
    pm ("trusted", "child") ],
    "moving privacy partway toward the default is accepted" );
}

#[test]
fn explicit_more_public_than_default_is_rejected_and_names_the_default (
) {
  // With the floors split, the error's floor is the DEFAULT, not
  // the (more private) sticky disk source.
  let config : SkgConfig =
    config_with_order ( & ["public", "trusted", "private"] );
  let child : NodeComplete = node_at ("child", "trusted"); // default floor: trusted
  let owner_before : NodeComplete = node_at ("owner", "public");
  install_graph ( & [ owner_before . clone (), child ] );
  let mut disk : NodeComplete = owner_before;
  disk . contains = vec! [
    pm ("private", "child") ]; // sticky sits more private than the default
  let mut buffer : NodeComplete = node_at ("owner", "public");
  buffer . contains = vec! [ pm ("public", "child") ]; // degenerate tag
  let explicit : ExplicitSources = ExplicitSources {
    contains : HashMap::from ([
      ( ID::new ("child"), SourceName::from ("public") ) ]), // more public than default
    .. ExplicitSources::default () };
  let err : String =
    apply_sticky_sources (buffer, &disk, &explicit, &config)
    . unwrap_err ();
  assert! ( err . contains ("'trusted'"),
            "the floor named is the default: {}", err );
  assert! ( ! err . contains ("'private'"),
            "the sticky source is not the floor: {}", err );
}

#[test]
fn explicit_at_a_more_public_than_default_disk_source_round_trips (
) {
  // Legacy or hand-authored data can put an owned-to-owned edge at a
  // source more public than its default. Render emits '(relSource ...)'
  // for every off-default edge, so that atom must save back unchanged
  // (explicit == disk source), and moving it partway toward the
  // default is fine; moving it still more public is forbidden.
  let config : SkgConfig =
    config_with_order ( & ["public", "trusted", "private"] );
  let child : NodeComplete = node_at ("child", "private");
  let owner_before : NodeComplete = node_at ("owner", "public");
  install_graph ( & [ owner_before . clone (), child ] );
  let mut disk : NodeComplete = owner_before;
  disk . contains = vec! [
    pm ("public", "child") ]; // more public than the "private" default
  { // Holding the disk source round-trips.
    let mut buffer : NodeComplete = node_at ("owner", "public");
    buffer . contains = vec! [ pm ("public", "child") ];
    let explicit : ExplicitSources = ExplicitSources {
      contains : HashMap::from ([
        ( ID::new ("child"), SourceName::from ("public") ) ]),
      .. ExplicitSources::default () };
    let resolved : NodeComplete =
      apply_sticky_sources (
        buffer, &disk, &explicit, &config ) . unwrap ();
    assert_eq! ( resolved . contains, vec! [ pm ("public", "child") ],
      "the rendered atom saves back unchanged" ); }
  { // Moving it partway toward the default is accepted.
    let mut buffer : NodeComplete = node_at ("owner", "public");
    buffer . contains = vec! [ pm ("public", "child") ];
    let explicit : ExplicitSources = ExplicitSources {
      contains : HashMap::from ([
        ( ID::new ("child"), SourceName::from ("trusted") ) ]),
      .. ExplicitSources::default () };
    let resolved : NodeComplete =
      apply_sticky_sources (
        buffer, &disk, &explicit, &config ) . unwrap ();
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
    config . sources . get_mut ( &SourceName::from (name) )
      . unwrap () . path = path; }
  let child : NodeComplete = node_at ("child", "trusted"); // home already moved
  let owner : NodeComplete = node_at ("owner", "public");
  install_graph ( & [ owner . clone (), child ] );
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
  let explicit : ExplicitSources = ExplicitSources {
    contains : HashMap::from ([
      ( ID::new ("child"), SourceName::from ("trusted") ) ]), // the new default
    .. ExplicitSources::default () };
  let resolved : NodeComplete =
    apply_sticky_sources (buffer, &disk, &explicit, &config) . unwrap ();
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
fn owned_to_foreign_new_edges_default_to_the_owner_home (
) {
  for relation in ["contains", "subscribes_to", "overrides_view_of"] {
    for (owner_home, member_home) in
        [("public", "private"), ("private", "public")] {
      let mut config : SkgConfig =
        config_with_order (&["public", "private"]);
      config . sources . get_mut (&SourceName::from (member_home))
        . unwrap () . user_owns_it = false;
      config . sources . get_mut (&SourceName::from (owner_home))
        . unwrap () . user_owns_it = true;
      let owner : NodeComplete = node_at ("owner", owner_home);
      let member : NodeComplete = node_at ("member", member_home);
      install_graph (&[owner . clone (), member]);
      let disk : NodeComplete = owner;
      let mut buffer : NodeComplete = node_at ("owner", owner_home);
      match relation {
        "contains" => buffer . contains = vec! [pm (owner_home, "member")],
        "subscribes_to" => buffer . subscribes_to =
          MSV::Specified (vec! [pm (owner_home, "member")]),
        "overrides_view_of" => buffer . overrides_view_of =
          MSV::Specified (vec! [pm (owner_home, "member")]),
        _ => unreachable! (), }
      let resolved : NodeComplete = apply_sticky_sources (
        buffer, &disk, &ExplicitSources::default (), &config ) . unwrap ();
      let source : &SourceName = match relation {
        "contains" => &resolved . contains [0] . source,
        "subscribes_to" =>
          &resolved . subscribes_to . or_default () [0] . source,
        "overrides_view_of" =>
          &resolved . overrides_view_of . or_default () [0] . source,
        _ => unreachable! (), };
      assert_eq! (source, &SourceName::from (owner_home),
                  "relation {}", relation); }}
}

#[test]
fn owned_to_foreign_explicit_owned_source_is_allowed_and_foreign_refused (
) {
  let mut config : SkgConfig =
    config_with_order (&["public", "foreign", "private"]);
  config . sources . get_mut (&SourceName::from ("foreign"))
    . unwrap () . user_owns_it = false;
  let mut owner : NodeComplete = node_at ("owner", "public");
  let member : NodeComplete = node_at ("member", "foreign");
  install_graph (&[owner . clone (), member]);

  let disk : NodeComplete = owner . clone ();
  owner . contains = vec! [pm ("public", "member")];
  let allowed : ExplicitSources = ExplicitSources {
    contains : HashMap::from ([
      (ID::new ("member"), SourceName::from ("private")) ]),
    .. ExplicitSources::default () };
  let resolved : NodeComplete = apply_sticky_sources (
    owner . clone (), &disk, &allowed, &config ) . unwrap ();
  assert_eq! (resolved . contains [0] . source,
              SourceName::from ("private"));

  let refused : ExplicitSources = ExplicitSources {
    contains : HashMap::from ([
      (ID::new ("member"), SourceName::from ("foreign")) ]),
    .. ExplicitSources::default () };
  let error : String = apply_sticky_sources (
    owner, &disk, &refused, &config ) . unwrap_err ();
  assert! (error . contains ("non-owned source 'foreign'"), "{}", error);
}

#[test]
fn explicit_alias_source_is_load_bearing_and_validated (
) {
  let config : SkgConfig =
    config_with_order (&["public", "trusted", "private"]);
  let disk : NodeComplete = node_at ("owner", "public");
  let mut buffer : NodeComplete = disk . clone ();
  buffer . aliases = MSV::Specified (vec! [
    crate::types::misc::MemberAtSource::at_source (
      SourceName::from ("public"), "nickname" . to_string ()) ]);
  let explicit : ExplicitSources = ExplicitSources {
    aliases : HashMap::from ([
      ("nickname" . to_string (), SourceName::from ("private")) ]),
    .. ExplicitSources::default () };
  let resolved : NodeComplete = apply_sticky_sources (
    buffer, &disk, &explicit, &config ) . unwrap ();
  assert_eq! (
    resolved . aliases . or_default () [0] . source,
    SourceName::from ("private") );

  let mut foreign_config : SkgConfig = config;
  foreign_config . sources . get_mut (&SourceName::from ("private"))
    . unwrap () . user_owns_it = false;
  let mut buffer : NodeComplete = disk . clone ();
  buffer . aliases = MSV::Specified (vec! [
    crate::types::misc::MemberAtSource::at_source (
      SourceName::from ("public"), "nickname" . to_string ()) ]);
  let error : String = apply_sticky_sources (
    buffer, &disk, &explicit, &foreign_config ) . unwrap_err ();
  assert! (error . contains ("non-owned source 'private'"), "{}", error);
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
    config . sources . get_mut ( &SourceName::from (name) )
      . unwrap () . path = path; }
  std::fs::write (
    tmp . path () . join ("private/n.skg"),
    "pid: n\n" ) . unwrap ();
  let active : ActiveSourceSet = ActiveSourceSet {
    name    : SourceSetName::from ("public"),
    sources : [ SourceName::from ("public") ]
      . into_iter () . collect (), };
  let refusal : Result<(), String> =
    refuse_delete_with_inactive_sections (
      &config, &active, &ID::new ("n") );
  assert! ( refusal . is_err (), "private section must refuse" );
  assert! ( refusal . unwrap_err ()
            . contains ("inactive sources") );
  assert! ( refuse_delete_with_inactive_sections (
    &config, &active, &ID::new ("only-public") ) . is_ok (),
    "a node with no inactive sections deletes fine" );
}

#[test]
fn relsource_atom_round_trips_through_render_and_parse (
) {
  // Atom parse round-trip (render-and-gating, 5_plan.org): a
  // viewnode carrying an explicit rel_source renders
  // '(relSource NAME)' inside its viewStats, and parsing that text
  // back recovers the same value (see 'org_to_text.rs' /
  // 'parse_metadata_sexp.rs').
  use crate::org_to_text::viewnode_to_string;
  use crate::serve::parse_metadata_sexp::parse_metadata_to_viewnodemd;
  use crate::types::viewnode::{
    default_activeNode, ActiveNode, ViewNode, ViewNodeKind, Vognode };

  let mut t : ActiveNode =
    default_activeNode (
      ID::new ("n"), SourceName::from ("public"), "N" . to_string () );
  t . viewStats . rel_source = Some ( SourceName::from ("private") );
  let viewnode : ViewNode = ViewNode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind        : ViewNodeKind::Vognode ( Vognode::Active (t) ), };
  let config : SkgConfig =
    config_with_order ( & ["public", "private"] );
  let rendered : String =
    viewnode_to_string (&viewnode, &config) . unwrap ();
  assert! ( rendered . contains ("(relSource private)"),
            "expected a relSource atom in: {}", rendered );
  let full_sexp : String = format! ("(skg {})", rendered);
  let parsed = parse_metadata_to_viewnodemd (&full_sexp) . unwrap ();
  assert_eq! ( parsed . viewStats . rel_source,
               Some ( SourceName::from ("private") ),
               "relSource did not round-trip through render+parse" );
}
