//! DEPENDENCIES.toml: publisher generation (byte-stable, owned
//! Skgrepos only, prefix-in-order) and receiver order warnings
//! (matched by skgrepo name, contradiction detected, no false alarm
//! on agreement).

use super::{foreign_manifest_order_warnings, write_dependencies_manifests};
use crate::types::misc::{SkgConfig, SkgRepo, SkgRepoName};

use std::collections::HashMap;
use std::path::PathBuf;

fn config_at (
  data_root : &std::path::Path,
  entries   : &[(&str, &str, bool)], // (name, relative path, owned)
) -> SkgConfig {
  let mut skgrepos : HashMap<SkgRepoName, SkgRepo> =
    HashMap::new ();
  for (name, rel, owned) in entries {
    let path : PathBuf = data_root . join (rel);
    std::fs::create_dir_all (&path) . unwrap ();
    skgrepos . insert (
      SkgRepoName::from (*name),
      SkgRepo {
        name         : SkgRepoName::from (*name),
        abbreviation : None,
        path,
        owned : *owned, } ); }
  let mut config : SkgConfig =
    SkgConfig::dummyFromSkgRepos (skgrepos);
  config . data_root = data_root . to_path_buf ();
  config . skgrepo_order =
    entries . iter ()
    . map ( |(name, _, _)| SkgRepoName::from (*name) )
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
  let written : Vec<SkgRepoName> =
    write_dependencies_manifests (&config) . unwrap ();
  assert_eq! ( written . len (), 2, "owned repos only" );
  let private_manifest : String =
    std::fs::read_to_string (
      tmp . path () . join ("owned/private/DEPENDENCIES.toml") )
    . unwrap ();
  assert! ( private_manifest . contains ("\"owned/public\"") );
  assert! ( private_manifest . contains ("\"eggman/eggs\"") );
  assert! ( private_manifest . contains ("\"owned/private\"") );
  assert! ( ! tmp . path ()
            . join ("eggman/eggs/DEPENDENCIES.toml") . exists (),
            "foreign repos get no manifest" );
  let public_manifest : String =
    std::fs::read_to_string (
      tmp . path () . join ("owned/public/DEPENDENCIES.toml") )
    . unwrap ();
  assert! ( ! public_manifest . contains ("private"),
            "a manifest lists only repos at least as public" );
  { // byte-stability: rewriting changes nothing
    let mtime_before =
      std::fs::metadata (
        tmp . path () . join ("owned/public/DEPENDENCIES.toml") )
      . unwrap () . modified () . unwrap ();
    write_dependencies_manifests (&config) . unwrap ();
    let mtime_after =
      std::fs::metadata (
        tmp . path () . join ("owned/public/DEPENDENCIES.toml") )
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
      tmp . path () . join ("owned/public/DEPENDENCIES.toml") )
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
      tmp . path () . join ("owned/public/DEPENDENCIES.toml") )
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
      tmp . path () . join ("owned/public/DEPENDENCIES.toml") )
    . unwrap ();
  assert! (
    manifest . contains ("{ path = \"owned/public\" }"),
    "a path-only pair when the Skg repo is not a Git repo: {manifest}" );
  assert! (
    ! manifest . contains ("git-remote ="),
    "no git-remote field when there is no remote: {manifest}" );
}

#[test]
fn receiver_warns_on_contradicted_order_and_not_on_agreement (
) {
  let tmp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
  let config : SkgConfig = config_at (
    tmp . path (),
    & [ ("mine",       "owned/mine",       true),
        ("her-work",   "colleague/work",   false),
        ("her-extra",  "colleague/extra",  false) ] );
  { // The colleague's manifest says extra is MORE PUBLIC than work;
    // this config orders them the other way.
    std::fs::write (
      tmp . path () . join ("colleague/work/DEPENDENCIES.toml"),
      "dependencies = [\n  \
       { path = \"owned/extra\" },\n  \
       { path = \"owned/work\" },\n]\n"
    ) . unwrap ();
    let warnings : Vec<String> =
      foreign_manifest_order_warnings (&config);
    assert_eq! ( warnings . len (), 1, "{:?}", warnings );
    assert! ( warnings [0] . contains ("her-work") ); }
  { // Agreement: no warning.
    std::fs::write (
      tmp . path () . join ("colleague/work/DEPENDENCIES.toml"),
      "dependencies = [\n  \
       { path = \"owned/work\" },\n  \
       { path = \"owned/extra\" },\n]\n"
    ) . unwrap ();
    assert! ( foreign_manifest_order_warnings (&config)
              . is_empty () ); }
  { // Back-compat: a foreign manifest still in the old bare-string
    // shape is parsed too, so its order is still checked.
    std::fs::write (
      tmp . path () . join ("colleague/work/DEPENDENCIES.toml"),
      "dependencies = [\n  \"owned/extra\",\n  \"owned/work\",\n]\n"
    ) . unwrap ();
    let warnings : Vec<String> =
      foreign_manifest_order_warnings (&config);
    assert_eq! ( warnings . len (), 1, "{:?}", warnings ); }
}
