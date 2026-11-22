use super::*;

use crate::types::nodes::fs::GraphnodeOnDisk;

/// The cases 'tests/shared/remote-hash-cases.txt' lists, which the
/// migration tool's tests read too.
fn remote_hash_cases () -> Vec<(String, String, String)> {
  let text : String = std::fs::read_to_string (
    concat! ( env! ("CARGO_MANIFEST_DIR"), "/tests/shared/remote-hash-cases.txt" ))
    . expect ("the shared cases file");
  text . lines ()
    . filter ( |line| ! line . starts_with ('#') && ! line . trim () . is_empty () )
    . map ( |line| {
      let fields : Vec<&str> = line . split ('\t') . collect ();
      ( fields [0] . to_string (), fields [1] . to_string (), fields [2] . to_string () ) } )
    . collect () }

#[test]
fn each_remote_url_normalises_and_hashes_as_the_shared_cases_say () {
  let cases = remote_hash_cases ();
  assert! ( cases . len () >= 5 );
  for (url, normalised, hash) in cases {
    assert_eq! ( normalised_remote_url (&url), normalised, "normalising {}", url );
    assert_eq! ( remote_hash_of_url (&url), hash, "hashing {}", url ); }}

/// The form the migration tool appends to a home section, and the
/// form the server writes: one and the same, so that a file the tool
/// wrote, saved again by the server, does not change.
const HOME_SECTION_WITH_ATTRIBUTION : &str = "\
title: a reply
pid: 6f1c2b2e-0d7d-4f0e-9a51-1c2d3e4f5a6b
forks:
- 0a1b2c3d-1111-2222-3333-444455556666
attribution:
  original: 0a1b2c3d-1111-2222-3333-444455556666
  skgrepo: claude-public
  remote_hash: d1b916a3aef4c6f971a1a0306e9a03529823e5e7
  commit: 9f8e7d6c5b4a39281706f5e4d3c2b1a098765432
  uncommitted: true
";

#[test]
fn an_attribution_round_trips_through_a_home_section_byte_for_byte () {
  let node_fs : GraphnodeOnDisk =
    yaml_serde::from_str (HOME_SECTION_WITH_ATTRIBUTION) . unwrap ();
  let attribution : &Attribution = node_fs . attribution . as_ref () . unwrap ();
  assert_eq! ( attribution . skgrepo, SkgrepoName::from ("claude-public") );
  assert! ( attribution . uncommitted );
  assert_eq! ( node_fs . to_yaml () . unwrap (), HOME_SECTION_WITH_ATTRIBUTION ); }

#[test]
fn the_optional_parts_of_an_attribution_are_left_out_when_absent () {
  let text : &str = "\
title: a reply
pid: b
attribution:
  original: a
  skgrepo: claude-public
";
  let node_fs : GraphnodeOnDisk = yaml_serde::from_str (text) . unwrap ();
  let attribution : &Attribution = node_fs . attribution . as_ref () . unwrap ();
  assert_eq! ( attribution . remote_hash, None );
  assert_eq! ( attribution . commit, None );
  assert! ( ! attribution . uncommitted );
  assert_eq! ( node_fs . to_yaml () . unwrap (), text ); }

/// What an older skg binary does with the field: its 'GraphnodeOnDisk'
/// denies no unknown field, so it reads the file and ignores the
/// attribution, as this struct, lacking a field, ignores one.
#[test]
fn a_reader_without_the_field_reads_the_file_and_ignores_it () {
  #[derive(serde::Deserialize)]
  #[allow(dead_code)]
  struct OlderGraphnodeOnDisk { title : Option<String>, pid : ID }
  let older : OlderGraphnodeOnDisk =
    yaml_serde::from_str (HOME_SECTION_WITH_ATTRIBUTION) . unwrap ();
  assert_eq! ( older . pid, ID::new ("6f1c2b2e-0d7d-4f0e-9a51-1c2d3e4f5a6b") ); }

// ------------------------------------------------------------
// The git facts, on a gitrepo made here
// ------------------------------------------------------------

use crate::types::config::Skgrepo;
use std::collections::HashMap;

const THEIR_URL : &str = "git@github.com:Ed/notes.git";

/// A gitrepo 'theirs', with a remote, holding N.skg in one commit, and
/// an owned skgrepo 'mine' beside it; the config names them so.
struct Fixture {
  _root   : tempfile::TempDir,
  theirs  : std::path::PathBuf,
  mine    : std::path::PathBuf,
  commit  : String,
}

fn fixture () -> Fixture {
  let root : tempfile::TempDir = tempfile::tempdir () . unwrap ();
  let theirs = root . path () . join ("theirs");
  let mine   = root . path () . join ("mine");
  std::fs::create_dir_all (&theirs) . unwrap ();
  std::fs::create_dir_all (&mine) . unwrap ();
  std::fs::write ( theirs . join ("N.skg"), "title: N as forked\npid: N\n" ) . unwrap ();
  let gitrepo = git2::Repository::init (&theirs) . unwrap ();
  gitrepo . remote ("origin", THEIR_URL) . unwrap ();
  let mut index = gitrepo . index () . unwrap ();
  index . add_path ( std::path::Path::new ("N.skg") ) . unwrap ();
  index . write () . unwrap ();
  let tree = gitrepo . find_tree ( index . write_tree () . unwrap () ) . unwrap ();
  let signature = git2::Signature::now ("test", "test@example.org") . unwrap ();
  let commit = gitrepo . commit (
    Some ("HEAD"), &signature, &signature, "N", &tree, &[] ) . unwrap ();
  Fixture { _root : root, theirs, mine, commit : commit . to_string () } }

fn config_naming (
  names : &[(&str, &std::path::Path, bool)], // name, path, owned
) -> SkgConfig {
  let skgrepos : HashMap<SkgrepoName, Skgrepo> = names . iter ()
    . map ( |(name, path, owned)| ( SkgrepoName::from (*name), Skgrepo {
      name : SkgrepoName::from (*name), abbreviation : None,
      path : path . to_path_buf (), owned : *owned } ))
    . collect ();
  let mut config : SkgConfig = SkgConfig::dummyFromSkgrepos (skgrepos);
  config . skgrepo_order = names . iter () . map ( |(name, _, _)| SkgrepoName::from (*name) ) . collect ();
  config }

#[test]
fn a_fork_records_the_remote_the_commit_and_whether_the_file_was_committed () {
  let f : Fixture = fixture ();
  let config : SkgConfig = config_naming (&[ ("mine", &f . mine, true), ("theirs", &f . theirs, false) ]);
  let attribution : Attribution = attribution_at_fork (
    &config, &ID::new ("N"), &SkgrepoName::from ("theirs"), &SkgrepoName::from ("mine") )
    . unwrap ();
  assert_eq! ( attribution, Attribution {
    original    : ID::new ("N"),
    skgrepo     : SkgrepoName::from ("theirs"),
    remote_hash : Some ( remote_hash_of_url (THEIR_URL) ),
    commit      : Some ( f . commit . clone () ),
    uncommitted : false } );
  std::fs::write ( f . theirs . join ("N.skg"), "title: N edited since\npid: N\n" ) . unwrap ();
  assert! ( attribution_at_fork (
    &config, &ID::new ("N"), &SkgrepoName::from ("theirs"), &SkgrepoName::from ("mine") )
    . unwrap () . uncommitted );
  assert_eq! ( attribution_at_fork (
    &config, &ID::new ("N"), &SkgrepoName::from ("mine"), &SkgrepoName::from ("mine") ),
    None, "a fork within one skgrepo records nothing" ); }

#[test]
fn the_herald_names_the_skgrepo_with_the_recorded_remote_by_its_current_name () {
  let f : Fixture = fixture ();
  let recorded : Attribution = Attribution {
    original : ID::new ("N"), skgrepo : SkgrepoName::from ("theirs"),
    remote_hash : Some ( remote_hash_of_url (THEIR_URL) ), commit : None, uncommitted : false };
  let renamed : SkgConfig = config_naming (&[ ("mine", &f . mine, true), ("eds-notes", &f . theirs, false) ]);
  assert_eq! ( attributed_skgrepo (&renamed, &recorded),
               AttributedSkgrepo::Loaded ( SkgrepoName::from ("eds-notes") ));
  let without : SkgConfig = config_naming (&[ ("mine", &f . mine, true) ]);
  assert_eq! ( attributed_skgrepo (&without, &recorded),
               AttributedSkgrepo::Unverified ( SkgrepoName::from ("theirs") ));
  let no_remote : Attribution = Attribution { remote_hash : None, .. recorded . clone () };
  let mine_named_theirs : SkgConfig = config_naming (&[ ("theirs", &f . mine, true) ]);
  assert_eq! ( attributed_skgrepo (&mine_named_theirs, &no_remote),
               AttributedSkgrepo::Loaded ( SkgrepoName::from ("theirs") ),
               "with no remote recorded, a skgrepo of the name with no remote is taken" );
  assert_eq! ( attributed_skgrepo (&renamed, &Attribution {
                 skgrepo : SkgrepoName::from ("eds-notes"), .. no_remote . clone () }),
               AttributedSkgrepo::Unverified ( SkgrepoName::from ("eds-notes") ),
               "a name recorded without a remote, now naming a skgrepo with one, is unverified" ); }

#[test]
fn doubtful_attributions_are_reported_and_rewritten () {
  let f : Fixture = fixture ();
  let config : SkgConfig = config_naming (&[ ("mine", &f . mine, true), ("eds-notes", &f . theirs, false) ]);
  let lost : Attribution = Attribution {
    original : ID::new ("N"), skgrepo : SkgrepoName::from ("theirs"),
    remote_hash : None, commit : Some ( f . commit . clone () ), uncommitted : false };
  let problems : Vec<String> = attribution_problems ( &config, [ &lost, &lost ] );
  assert_eq! ( problems . len (), 1, "{:?}", problems );
  assert! ( problems [0] . starts_with ("2 attributions of forks in your skgrepos: they name 'theirs', which the config lacks"),
            "{}", problems [0] );
  assert! ( problems [0] . ends_with ( &format! (
              " If it is loaded under another name, run, with that name for NAME: {}",
              skg_command_for_user (&[ "fix-attributions", "", "theirs", "NAME" ]) )),
            "the advice ends in a command that runs as pasted: {}", problems [0] );
  let fine : Attribution = Attribution {
    skgrepo : SkgrepoName::from ("eds-notes"),
    remote_hash : Some ( remote_hash_of_url (THEIR_URL) ), .. lost . clone () };
  assert! ( attribution_problems ( &config, [ &fine ] ) . is_empty () );

  let fork_text : String = format! (
    "title: my reply\npid: R\nforks:\n- N\nattribution:\n  original: N\n  skgrepo: theirs\n  commit: {}\n",
    f . commit );
  std::fs::write ( f . mine . join ("R.skg"), &fork_text ) . unwrap ();
  let rewritten = rewrite_attributions ( &config, "theirs", &SkgrepoName::from ("eds-notes") ) . unwrap ();
  assert_eq! ( rewritten, vec! [ f . mine . join ("R.skg") ] );
  assert_eq! ( std::fs::read_to_string ( f . mine . join ("R.skg") ) . unwrap (),
    format! ( "title: my reply\npid: R\nforks:\n- N\nattribution:\n  original: N\n  skgrepo: eds-notes\n  remote_hash: {}\n  commit: {}\n",
              remote_hash_of_url (THEIR_URL), f . commit ));
  assert! ( rewrite_attributions ( &config, "theirs", &SkgrepoName::from ("eds-notes") )
            . unwrap () . is_empty (), "a second rewrite finds nothing" );

  // What the original said when forked, though it has changed since.
  std::fs::write ( f . theirs . join ("N.skg"), "title: N edited since\npid: N\n" ) . unwrap ();
  let then : String = forked_original_at_fork_time (&config, &ID::new ("R")) . unwrap ();
  assert! ( then . ends_with ("title: N as forked\npid: N\n"), "{}", then );
  assert! ( then . contains (&f . commit), "{}", then ); }
