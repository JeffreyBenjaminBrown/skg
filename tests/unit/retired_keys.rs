use super::*;

#[test]
fn a_file_without_retired_keys_holds_none () {
  assert_eq! ( retired_keys_held ("pid: a\ntitle: a\nforks:\n- b\nhides:\n- c\n"),
               Vec::<&str>::new () ); }

#[test]
fn every_retired_top_level_key_is_found_in_order () {
  assert_eq! (
    retired_keys_held (
      "pid: a\noverrides:\n- b\nsubscribesTo:\n- b\nhidesFromSubs:\n- c\n" ),
    vec! [ "subscribesTo", "overrides", "hidesFromSubs" ] );
  assert_eq! (
    retired_keys_held ("pid: a\nsubscribes_to:\n- b\noverrides_view_of:\n- b\n"),
    vec! [ "subscribes_to", "overrides_view_of" ] ); }

#[test]
fn a_key_named_in_a_title_or_body_is_no_key () {
  assert_eq! (
    retired_keys_held (
      "pid: a\ntitle: 'overrides: a note'\nbody: |-\n  subscribesTo:\n  - b\n" ),
    Vec::<&str>::new () ); }

#[test]
fn the_error_names_each_file_with_its_keys () {
  let error : std::io::Error = RetiredKeysInFiles {
    files : vec! [
      ( PathBuf::from ("repo/b.skg"), vec! [ "overrides" ] ),
      ( PathBuf::from ("repo/a.skg"), vec! [ "subscribesTo", "hidesFromSubs" ] ) ] }
    . into_io_error ();
  let carried : &RetiredKeysInFiles =
    RetiredKeysInFiles::carried_by (&error) . expect ("it carries the files");
  assert_eq! ( carried . files [0] . 0, PathBuf::from ("repo/a.skg") );
  let message : String = error . to_string ();
  assert! ( message . starts_with (
    "repo/a.skg (holds 'subscribesTo', 'hidesFromSubs'); repo/b.skg (holds 'overrides'); " ),
    "{}", message );
  assert! ( message . contains (FORKS_MIGRATION_TOOL), "{}", message ); }
