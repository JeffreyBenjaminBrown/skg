use super::*;
use std::collections::HashMap;
use super::super::misc::Skgrepo;

fn skgrepo_name (s: &str) -> SkgrepoName { SkgrepoName ( s . to_string () ) }
fn gnid        (s: &str) -> ID         { ID ( s . to_string () ) }

/// A node with sections in several skgrepos has exactly one home: the
/// most public section of its home's owner. The privacy order here is
/// declaration order (zed, alpha), deliberately not alphabetical. A
/// node with a foreign section and owned ones is the foreign owner's:
/// the owned sections are illegal edits to it. Rebuilding the config
/// for every assertion also gives its HashMap a fresh randomized state,
/// catching any accidental return to raw map iteration.
#[test]
fn home_from_disk_is_the_most_public_section () {
  let dir : tempfile::TempDir = tempfile::tempdir () . unwrap ();
  let foreign_path : PathBuf = dir . path () . join ("foreign");
  let public_path  : PathBuf = dir . path () . join ("zed");
  let private_path : PathBuf = dir . path () . join ("alpha");
  std::fs::create_dir_all (&foreign_path) . unwrap ();
  std::fs::create_dir_all (&public_path)  . unwrap ();
  std::fs::create_dir_all (&private_path) . unwrap ();
  let make_config = || -> SkgConfig {
    let mut skgrepos : HashMap<SkgrepoName, Skgrepo> =
      HashMap::new ();
    for (name, path, owned) in
      [ ("foreign", foreign_path . clone (), false),
        ("zed",     public_path  . clone (), true),
        ("alpha",   private_path . clone (), true) ] {
      skgrepos . insert ( skgrepo_name (name), Skgrepo {
        name         : skgrepo_name (name),
        abbreviation : None,
        path,
        owned, } ); }
    let mut config : SkgConfig =
      SkgConfig::dummyFromSkgrepos (skgrepos);
    config . skgrepo_order = // most public first
      vec! [ skgrepo_name ("foreign"),
             skgrepo_name ("zed"),
             skgrepo_name ("alpha") ];
    config };
  { // The foreign section is the home; the owned ones are illegal edits.
    std::fs::write ( foreign_path . join ("N.skg"),
                     "pid: N\ntitle: unrelated foreign N\n" ) . unwrap ();
    std::fs::write ( public_path  . join ("N.skg"),
                     "pid: N\ntitle: N\n" ) . unwrap ();
    std::fs::write ( private_path . join ("N.skg"),
                     "pid: N\ncontains:\n- C\n" ) . unwrap ();
    for _ in 0 .. 20 {
      let config : SkgConfig = make_config ();
      assert_eq! ( home_from_disk ( &gnid ("N"), &config ),
                   Some ( skgrepo_name ("foreign") ) ); }}
  { // Owned sections alone: the most public one is the home.
    std::fs::write ( public_path  . join ("O.skg"),
                     "pid: O\ntitle: O\n" ) . unwrap ();
    std::fs::write ( private_path . join ("O.skg"),
                     "pid: O\ncontains:\n- C\n" ) . unwrap ();
    for _ in 0 .. 20 {
      let config : SkgConfig = make_config ();
      assert_eq! ( home_from_disk ( &gnid ("O"), &config ),
                   Some ( skgrepo_name ("zed") ) ); }}
  { // A section in only the more private skgrepo: that is the home.
    std::fs::write ( private_path . join ("P.skg"),
                     "pid: P\ntitle: P\n" ) . unwrap ();
    let config : SkgConfig = make_config ();
    assert_eq! ( home_from_disk ( &gnid ("P"), &config ),
                 Some ( skgrepo_name ("alpha") ) ); }
  let config : SkgConfig = make_config ();
  assert_eq! ( home_from_disk ( &gnid ("absent"), &config ), None );
}
