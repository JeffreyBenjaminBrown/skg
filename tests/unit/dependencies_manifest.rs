//! OTHER_SKGREPO_DEPENDENCIES.toml: publisher generation (byte-stable, owned
//! Skgrepos only, prefix-in-order) and receiver order warnings
//! (matched by skgrepo name, a contradiction warned about only when a
//! telescope spans the pair, no false alarm on agreement).

use super::{foreign_manifest_order_warnings, write_dependencies_manifests};
use crate::types::misc::{SkgConfig, Skgrepo, SkgrepoName};

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
  assert! ( private_manifest . contains ("\"eggman/eggs\"") );
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

#[test]
fn receiver_warns_on_contradicted_order_only_when_a_telescope_spans_the_pair (
) {
  let tmp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
  let config : SkgConfig = config_at (
    tmp . path (),
    & [ ("her-work",   "colleague/work",   false),
        ("her-extra",  "colleague/extra",  false),
        ("mine",       "owned/mine",       true) ] );
  let manifest_path : PathBuf =
    tmp . path () . join ("colleague/work/OTHER_SKGREPO_DEPENDENCIES.toml");
  let contradicting_manifest : &str =
    // extra more public than work; this config orders them the other way
    "dependencies = [\n  \
     { path = \"owned/extra\" },\n  \
     { path = \"owned/work\" },\n]\n";
  std::fs::write (&manifest_path, contradicting_manifest) . unwrap ();
  { // A contradiction no telescope spans: silent.
    std::fs::write ( tmp . path () . join ("colleague/work/W.skg"),
                     "title: only in work\npid: W\n" ) . unwrap ();
    std::fs::write ( tmp . path () . join ("colleague/extra/E.skg"),
                     "title: only in extra\npid: E\n" ) . unwrap ();
    assert! ( foreign_manifest_order_warnings (&config) . is_empty () ); }
  { // Node S has a section in each: the warning names the node and
    // both skgrepos.
    std::fs::write ( tmp . path () . join ("colleague/work/S.skg"),
                     "title: spanning node\npid: S\n" ) . unwrap ();
    std::fs::write ( tmp . path () . join ("colleague/extra/S.skg"),
                     "pid: S\n" ) . unwrap ();
    let warnings : Vec<String> =
      foreign_manifest_order_warnings (&config);
    assert_eq! ( warnings . len (), 1, "{:?}", warnings );
    assert! ( warnings [0] . starts_with (
                "Node 'S' ('spanning node') has sections in both 'her-work' and 'her-extra'. This config orders 'her-work' more public than 'her-extra'" ),
              "{}", warnings [0] ); }
  { // Agreement: no warning, though S spans the pair.
    std::fs::write (
      &manifest_path,
      "dependencies = [\n  \
       { path = \"owned/work\" },\n  \
       { path = \"owned/extra\" },\n]\n"
    ) . unwrap ();
    assert! ( foreign_manifest_order_warnings (&config)
              . is_empty () ); }
  { // The bare-string manifest shape is read too.
    std::fs::write (
      &manifest_path,
      "dependencies = [\n  \"owned/extra\",\n  \"owned/work\",\n]\n"
    ) . unwrap ();
    let warnings : Vec<String> =
      foreign_manifest_order_warnings (&config);
    assert_eq! ( warnings . len (), 1, "{:?}", warnings ); }
  { // An owned section of S makes this server ignore its foreign
    // sections, so no telescope spans the pair: silent.
    std::fs::write ( tmp . path () . join ("owned/mine/S.skg"),
                     "title: spanning node\npid: S\n" ) . unwrap ();
    assert! ( foreign_manifest_order_warnings (&config)
              . is_empty () ); }
}

#[test]
fn receiver_checks_every_manifest_pair_not_only_adjacent_ones (
) {
  // The manifest orders a, b, c; this config orders b, c, a. Only the
  // non-adjacent pair (a, c) is spanned, by node T.
  let tmp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
  let config : SkgConfig = config_at (
    tmp . path (),
    & [ ("b", "them/b", false),
        ("c", "them/c", false),
        ("a", "them/a", false),
        ("mine", "owned/mine", true) ] );
  std::fs::write (
    tmp . path () . join ("them/c/OTHER_SKGREPO_DEPENDENCIES.toml"),
    "dependencies = [\n  \
     { path = \"them/a\" },\n  \
     { path = \"them/b\" },\n  \
     { path = \"them/c\" },\n]\n"
  ) . unwrap ();
  std::fs::write ( tmp . path () . join ("them/c/T.skg"),
                   "title: t\npid: T\n" ) . unwrap ();
  std::fs::write ( tmp . path () . join ("them/a/T.skg"),
                   "pid: T\n" ) . unwrap ();
  let warnings : Vec<String> =
    foreign_manifest_order_warnings (&config);
  assert_eq! ( warnings . len (), 1, "{:?}", warnings );
  assert! ( warnings [0] . contains (
              "has sections in both 'c' and 'a'" ),
            "{}", warnings [0] );
}
