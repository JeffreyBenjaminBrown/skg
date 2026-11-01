//! OTHER_SKGREPO_DEPENDENCIES.toml: publisher generation (byte-stable,
//! owned skgrepos only, each the ladder through itself, no foreign
//! skgrepo listed) and the receiver's foreign fold order (matched by
//! path, else final component; a pair warned about only when a
//! telescope spans it and the manifests do not settle its order).

use super::{
  foreign_section_warnings, read_foreign_manifest_orders,
  write_dependencies_manifests};
use crate::dbs::filesystem::one_node::graphnode_from_pid_and_skgrepo;
use crate::types::misc::{
  ForeignManifestOrder, ID, SkgConfig, Skgrepo, SkgrepoName};

use std::collections::HashMap;
use std::path::PathBuf;

fn config_at (
  data_root : &std::path::Path,
  entries   : &[(&str, &str, bool)], // (name, relative path, owned)
) -> SkgConfig {
  let mut skgrepos : HashMap<SkgrepoName, Skgrepo> =
    HashMap::new ();
  for (name, rel, owned) in entries {
    let path : PathBuf = data_root . join (rel);
    std::fs::create_dir_all (&path) . unwrap ();
    skgrepos . insert (
      SkgrepoName::from (*name),
      Skgrepo {
        name         : SkgrepoName::from (*name),
        abbreviation : None,
        path,
        owned : *owned, } ); }
  let mut config : SkgConfig =
    SkgConfig::dummyFromSkgrepos (skgrepos);
  config . data_root = data_root . to_path_buf ();
  config . skgrepo_order =
    entries . iter ()
    . map ( |(name, _, _)| SkgrepoName::from (*name) )
    . collect ();
  config }

#[test]
fn manifests_list_prefixes_for_owned_skgrepos_only (
) {
  let tmp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
  let config : SkgConfig = config_at (
    tmp . path (),
    & [ ("public",  "owned/public",  true),
        ("eggs",    "eggman/eggs",   false),
        ("private", "owned/private", true) ] );
  let written : Vec<SkgrepoName> =
    write_dependencies_manifests (&config) . unwrap ();
  assert_eq! ( written . len (), 2, "owned repos only" );
  let private_manifest : String =
    std::fs::read_to_string (
      tmp . path () . join ("owned/private/OTHER_SKGREPO_DEPENDENCIES.toml") )
    . unwrap ();
  assert! ( private_manifest . contains ("\"owned/public\"") );
  assert! ( ! private_manifest . contains ("eggman/eggs"),
            "a manifest lists no foreign repo, wherever the config puts it" );
  assert! ( private_manifest . contains ("\"owned/private\"") );
  assert! ( ! tmp . path ()
            . join ("eggman/eggs/OTHER_SKGREPO_DEPENDENCIES.toml") . exists (),
            "foreign repos get no manifest" );
  let public_manifest : String =
    std::fs::read_to_string (
      tmp . path () . join ("owned/public/OTHER_SKGREPO_DEPENDENCIES.toml") )
    . unwrap ();
  assert! ( ! public_manifest . contains ("private"),
            "a manifest lists only repos at least as public" );
  { // byte-stability: rewriting changes nothing
    let mtime_before =
      std::fs::metadata (
        tmp . path () . join ("owned/public/OTHER_SKGREPO_DEPENDENCIES.toml") )
      . unwrap () . modified () . unwrap ();
    write_dependencies_manifests (&config) . unwrap ();
    let mtime_after =
      std::fs::metadata (
        tmp . path () . join ("owned/public/OTHER_SKGREPO_DEPENDENCIES.toml") )
      . unwrap () . modified () . unwrap ();
    assert_eq! (mtime_before, mtime_after); }
}

#[test]
fn dependency_pair_records_origin_remote_without_naming_it (
) {
  let tmp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
  let config : SkgConfig = config_at (
    tmp . path (),
    & [ ("public", "owned/public", true) ] );
  { // Make the owned skgrepo its own gitrepo with an `origin`.
    let gitrepo : git2::Repository =
      git2::Repository::init (
        tmp . path () . join ("owned/public") ) . unwrap ();
    gitrepo . remote (
      "origin",
      "git@github.com:me/public-notes.git" ) . unwrap (); }
  write_dependencies_manifests (&config) . unwrap ();
  let manifest : String =
    std::fs::read_to_string (
      tmp . path () . join ("owned/public/OTHER_SKGREPO_DEPENDENCIES.toml") )
    . unwrap ();
  assert! (
    manifest . contains (
      "{ path = \"owned/public\", \
       git-remote = \"git@github.com:me/public-notes.git\" }" ),
    "the pair carries path and git-remote: {manifest}" );
  assert! (
    ! manifest . contains ("git-remote-name ="),
    "does not name the remote when it is the conventional origin: \
     {manifest}" );
}

#[test]
fn dependency_pair_names_a_non_origin_remote (
) {
  let tmp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
  let config : SkgConfig = config_at (
    tmp . path (),
    & [ ("public", "owned/public", true) ] );
  { // A skgrepo whose only remote is not called `origin`.
    let gitrepo : git2::Repository =
      git2::Repository::init (
        tmp . path () . join ("owned/public") ) . unwrap ();
    gitrepo . remote (
      "upstream",
      "https://example.com/notes.git" ) . unwrap (); }
  write_dependencies_manifests (&config) . unwrap ();
  let manifest : String =
    std::fs::read_to_string (
      tmp . path () . join ("owned/public/OTHER_SKGREPO_DEPENDENCIES.toml") )
    . unwrap ();
  assert! (
    manifest . contains (
      "git-remote = \"https://example.com/notes.git\", \
       git-remote-name = \"upstream\"" ),
    "records the fallback remote's URL and its name: {manifest}" );
}

#[test]
fn dependency_pair_omits_git_remote_when_repo_is_not_a_gitrepo (
) {
  let tmp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
  let config : SkgConfig = config_at (
    tmp . path (),
    & [ ("public", "owned/public", true) ] );
  write_dependencies_manifests (&config) . unwrap ();
  let manifest : String =
    std::fs::read_to_string (
      tmp . path () . join ("owned/public/OTHER_SKGREPO_DEPENDENCIES.toml") )
    . unwrap ();
  assert! (
    manifest . contains ("{ path = \"owned/public\" }"),
    "a path-only pair when the Skg repo is not a Git repo: {manifest}" );
  assert! (
    ! manifest . contains ("git-remote ="),
    "no git-remote field when there is no remote: {manifest}" );
}

/// 'config' with what its foreign manifests say read in, as the
/// config loaders do.
fn with_foreign_manifests_read (
  mut config : SkgConfig,
) -> SkgConfig {
  let orders = read_foreign_manifest_orders (&config);
  config . set_foreign_manifest_orders (orders);
  config }

fn names (
  names : &[&str],
) -> Vec<SkgrepoName> {
  names . iter () . map ( |n| SkgrepoName::from (*n) ) . collect () }

#[test]
fn a_manifest_entry_names_a_foreign_skgrepo_by_path_else_by_its_one_final_component (
) {
  let tmp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
  let config : SkgConfig = config_at (
    tmp . path (),
    & [ ("fred-pub",  "fred/public",   false),
        ("mine",      "me/public",     true),
        ("fred-priv", "elsewhere/private", false),
        ("sue-pub",   "sue/public",    false) ] );
  std::fs::write (
    tmp . path () . join ("elsewhere/private/OTHER_SKGREPO_DEPENDENCIES.toml"),
    // 'fred/public' matches by path; 'fred/private' only by its final
    // component; 'fred/notes' matches nothing; 'zed/public' matches
    // two foreign skgrepos by final component, so it is skipped.
    "dependencies = [\n  \
     { path = \"fred/public\" },\n  \
     { path = \"zed/public\" },\n  \
     { path = \"fred/notes\" },\n  \
     { path = \"fred/private\" },\n]\n" ) . unwrap ();
  assert_eq! (
    read_foreign_manifest_orders (&config),
    vec! [ ForeignManifestOrder {
             manifest_skgrepo : SkgrepoName::from ("fred-priv"),
             listed : names (&["fred-pub", "fred-priv"]), } ],
    "the owned 'me/public' is never matched" ); }

#[test]
fn two_foreign_skgrepos_a_manifest_lists_fold_in_its_order_whatever_the_config_says (
) {
  let tmp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
  let config : SkgConfig = config_at (
    tmp . path (),
    & [ ("her-extra", "colleague/extra", false),
        ("mine",      "owned/mine",      true),
        ("her-work",  "colleague/work",  false) ] );
  std::fs::write (
    tmp . path () . join ("colleague/extra/OTHER_SKGREPO_DEPENDENCIES.toml"),
    "dependencies = [\n  \
     { path = \"colleague/work\" },\n  \
     { path = \"colleague/extra\" },\n]\n" ) . unwrap ();
  std::fs::write ( tmp . path () . join ("colleague/work/S.skg"),
                   "title: from work\npid: S\n" ) . unwrap ();
  std::fs::write ( tmp . path () . join ("colleague/extra/S.skg"),
                   "title: from extra\npid: S\n" ) . unwrap ();
  { // Unread, the manifests order nothing: config-file order.
    assert_eq! ( config . ordered_skgrepos (),
                 names (&["her-extra", "mine", "her-work"]) );
    let node = graphnode_from_pid_and_skgrepo (
      &config, ID::from ("S"), &SkgrepoName::from ("her-extra") )
      . unwrap ();
    assert_eq! ( node . home_skgrepo, SkgrepoName::from ("her-extra") ); }
  let config : SkgConfig = with_foreign_manifests_read (config);
  assert_eq! ( config . ordered_skgrepos (),
               names (&["her-work", "mine", "her-extra"]),
               "the foreign entries swap places; the owned one stays" );
  let node = graphnode_from_pid_and_skgrepo (
    &config, ID::from ("S"), &SkgrepoName::from ("her-work") ) . unwrap ();
  assert_eq! ( node . home_skgrepo, SkgrepoName::from ("her-work") );
  assert_eq! ( node . title, "from work" );
  assert! ( foreign_section_warnings (&config) . is_empty (),
            "the manifest settles the order" ); }

#[test]
fn sections_of_one_node_in_two_foreign_owners_skgrepos_are_an_illegal_edit (
) {
  // No manifest chains 'fred' and 'sue': two owners. S and T have a
  // section in each; the home is 'fred', declared first, and sue's
  // sections are illegal edits, ignored.
  let tmp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
  let config : SkgConfig = config_at (
    tmp . path (),
    & [ ("fred",  "fred/public", false),
        ("mine",  "owned/mine",  true),
        ("sue",   "sue/public",  false) ] );
  std::fs::write ( tmp . path () . join ("fred/public/F.skg"),
                   "title: only fred\npid: F\n" ) . unwrap ();
  assert! ( foreign_section_warnings (
              &with_foreign_manifests_read (config . clone ()) ) . is_empty (),
            "no node spans two owners" );
  for pid in ["S", "T"] {
    std::fs::write ( tmp . path () . join (format! ("fred/public/{}.skg", pid)),
                     format! ("title: fred's {}\npid: {}\n", pid, pid) ) . unwrap ();
    std::fs::write ( tmp . path () . join (format! ("sue/public/{}.skg", pid)),
                     format! ("title: sue's {}\npid: {}\n", pid, pid) ) . unwrap (); }
  let config : SkgConfig = with_foreign_manifests_read (config);
  assert_ne! ( config . skgrepo_owner (&SkgrepoName::from ("fred")),
               config . skgrepo_owner (&SkgrepoName::from ("sue")) );
  assert_eq! ( foreign_section_warnings (&config), vec! [
    "Node 'S' ('fred's S') (and 1 more node) has a section in foreign repo 'sue', but its home 'fred' has another owner: an illegal edit to a foreign node. Skg ignores that section." . to_string () ] );
  let node = graphnode_from_pid_and_skgrepo (
    &config, ID::from ("S"), &SkgrepoName::from ("fred") ) . unwrap ();
  assert_eq! ( node . title, "fred's S", "sue's section takes no part" );
  assert_eq! ( crate::types::disk_home::home_from_disk (&ID::from ("S"), &config),
               Some ( SkgrepoName::from ("fred") )); }

#[test]
fn one_owners_disputed_pair_folds_as_the_more_private_ones_manifest_says (
) {
  // Fred's 'a', 'b' and 'c', declared b, a, c. b's own manifest lists
  // a before it, so b is the more private and a folds first; c's
  // manifest says b, a, c, and is overruled.
  let tmp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
  let config : SkgConfig = config_at (
    tmp . path (),
    & [ ("b", "fred/b", false),
        ("a", "fred/a", false),
        ("c", "fred/c", false),
        ("mine", "owned/mine", true) ] );
  std::fs::write ( tmp . path () . join ("fred/b/OTHER_SKGREPO_DEPENDENCIES.toml"),
                   "dependencies = [\n  { path = \"fred/a\" },\n  { path = \"fred/b\" },\n]\n" )
    . unwrap ();
  std::fs::write ( tmp . path () . join ("fred/c/OTHER_SKGREPO_DEPENDENCIES.toml"),
                   "dependencies = [\n  { path = \"fred/b\" },\n  { path = \"fred/a\" },\n  { path = \"fred/c\" },\n]\n" )
    . unwrap ();
  std::fs::write ( tmp . path () . join ("fred/a/S.skg"),
                   "title: from a\npid: S\n" ) . unwrap ();
  std::fs::write ( tmp . path () . join ("fred/b/S.skg"),
                   "pid: S\ncontains:\n- x\n" ) . unwrap ();
  let config : SkgConfig = with_foreign_manifests_read (config);
  assert_eq! ( config . skgrepo_owner (&SkgrepoName::from ("a")),
               config . skgrepo_owner (&SkgrepoName::from ("c")),
               "the manifests chain all three: one owner" );
  assert_eq! ( config . ordered_skgrepos (),
               names (&["a", "b", "c", "mine"]) );
  let node = graphnode_from_pid_and_skgrepo (
    &config, ID::from ("S"), &SkgrepoName::from ("a") ) . unwrap ();
  assert_eq! ( node . home_skgrepo, SkgrepoName::from ("a") );
  assert_eq! ( node . title, "from a" );
  let warnings : Vec<String> = foreign_section_warnings (&config);
  assert_eq! ( warnings . len (), 1, "{:?}", warnings );
  assert! ( warnings [0] . starts_with (
              "Node 'S' ('from a') has sections in foreign repos 'a' and 'b'. The OTHER_SKGREPO_DEPENDENCIES.toml of foreign repo 'b' lists 'a' first, but that of 'c' lists 'b' first, so this server folds 'a' first" ),
            "{}", warnings [0] ); }
