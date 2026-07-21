//! DEPENDENCIES.toml: publisher generation (byte-stable, owned
//! sources only, prefix-in-order) and receiver order warnings
//! (matched by repo name, contradiction detected, no false alarm
//! on agreement).

use super::{foreign_manifest_order_warnings, write_dependencies_manifests};
use crate::types::misc::{SkgConfig, SkgfileSource, SourceName};

use std::collections::HashMap;
use std::path::PathBuf;

fn config_at (
  data_root : &std::path::Path,
  entries   : &[(&str, &str, bool)], // (name, relative path, owned)
) -> SkgConfig {
  let mut sources : HashMap<SourceName, SkgfileSource> =
    HashMap::new ();
  for (name, rel, owned) in entries {
    let path : PathBuf = data_root . join (rel);
    std::fs::create_dir_all (&path) . unwrap ();
    sources . insert (
      SourceName::from (*name),
      SkgfileSource {
        name         : SourceName::from (*name),
        abbreviation : None,
        path,
        user_owns_it : *owned, } ); }
  let mut config : SkgConfig =
    SkgConfig::dummyFromSources (sources);
  config . data_root = data_root . to_path_buf ();
  config . source_order =
    entries . iter ()
    . map ( |(name, _, _)| SourceName::from (*name) )
    . collect ();
  config }

#[test]
fn manifests_list_prefixes_for_owned_sources_only (
) {
  let tmp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
  let config : SkgConfig = config_at (
    tmp . path (),
    & [ ("public",  "owned/public",  true),
        ("eggs",    "eggman/eggs",   false),
        ("private", "owned/private", true) ] );
  let written : Vec<SourceName> =
    write_dependencies_manifests (&config) . unwrap ();
  assert_eq! ( written . len (), 2, "owned sources only" );
  let private_manifest : String =
    std::fs::read_to_string (
      tmp . path () . join ("owned/private/DEPENDENCIES.toml") )
    . unwrap ();
  assert! ( private_manifest . contains ("\"owned/public\"") );
  assert! ( private_manifest . contains ("\"eggman/eggs\"") );
  assert! ( private_manifest . contains ("\"owned/private\"") );
  assert! ( ! tmp . path ()
            . join ("eggman/eggs/DEPENDENCIES.toml") . exists (),
            "foreign sources get no manifest" );
  let public_manifest : String =
    std::fs::read_to_string (
      tmp . path () . join ("owned/public/DEPENDENCIES.toml") )
    . unwrap ();
  assert! ( ! public_manifest . contains ("private"),
            "a manifest lists only sources at least as public" );
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
  { // Make the owned source its own git repo with an `origin`.
    let repo : git2::Repository =
      git2::Repository::init (
        tmp . path () . join ("owned/public") ) . unwrap ();
    repo . remote (
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
  { // A repo whose only remote is not called `origin`.
    let repo : git2::Repository =
      git2::Repository::init (
        tmp . path () . join ("owned/public") ) . unwrap ();
    repo . remote (
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
fn dependency_pair_omits_git_remote_when_source_is_not_a_git_repo (
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
    "a path-only pair when the source is not a git repo: {manifest}" );
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
