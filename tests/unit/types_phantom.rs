use super::*;
use std::collections::HashMap;
use super::super::misc::Skgrepo;

fn skgrepo_name (s: &str) -> SkgrepoName { SkgrepoName ( s . to_string () ) }
fn skgid        (s: &str) -> ID         { ID ( s . to_string () ) }

/// A node with sections in several skgrepos has exactly one home: the
/// most public RETAINED section. The privacy order here is declaration
/// order (foreign, zed, alpha), deliberately NOT alphabetical. The
/// foreign section is discarded by the owned-pid collision rule, so
/// "zed" wins over the retained "alpha" section. Rebuilding the config
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
  { // The foreign collision is ignored; most-public retained wins.
    std::fs::write ( foreign_path . join ("N.skg"),
                     "pid: N\ntitle: unrelated foreign N\n" ) . unwrap ();
    std::fs::write ( public_path  . join ("N.skg"),
                     "pid: N\ntitle: N\n" ) . unwrap ();
    std::fs::write ( private_path . join ("N.skg"),
                     "pid: N\ncontains:\n- C\n" ) . unwrap ();
    for _ in 0 .. 20 {
      let config : SkgConfig = make_config ();
      assert_eq! ( home_from_disk ( &skgid ("N"), &config ),
                   Some ( skgrepo_name ("zed") ) ); }}
  { // A section in only the more private skgrepo: that is the home.
    std::fs::write ( private_path . join ("P.skg"),
                     "pid: P\ntitle: P\n" ) . unwrap ();
    let config : SkgConfig = make_config ();
    assert_eq! ( home_from_disk ( &skgid ("P"), &config ),
                 Some ( skgrepo_name ("alpha") ) ); }
  let config : SkgConfig = make_config ();
  assert_eq! ( home_from_disk ( &skgid ("absent"), &config ), None );
}
