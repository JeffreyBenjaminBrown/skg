//! Unit tests for server/obsidian/import.rs: the round trip of
//! 'TODO/export-obsidian/plan.org' (part 2). For a skgrepo S,
//! import(export(S)) is S's files in normal form (each parsed and
//! written again by 'to_yaml'), byte for byte, and no others; and
//! export(import(V)) = V for the vault V that export wrote. The corpora
//! are the fixture of obsidian_fixture.rs, 'skg-system/', and random
//! skgrepos.

use super::*;
use crate::obsidian::export::{RenderedVault, SkgrepoGitFacts, render_vault, write_vault};

#[path = "obsidian_fixture.rs"]
mod fixture;
use fixture::*;

use proptest::prelude::*;
use tempfile::TempDir;

/// Each section as skg writes it, parsed from what skg writes: the
/// normal form.
fn normal_form (
  sections : &[GraphnodeOnDisk],
) -> BTreeMap<ID, String> {
  sections . iter ()
    . map ( |section| {
      let reread : GraphnodeOnDisk =
        yaml_serde::from_str (& section . to_yaml () . unwrap ()) . unwrap ();
      (reread . pid . clone (), reread . to_yaml () . unwrap ()) } )
    . collect () }

/// Both halves of the round trip, in memory; Err says what differs.
fn round_trip (
  sections : &[GraphnodeOnDisk],
) -> Result<(), String> {
  let normal : BTreeMap<ID, String> = normal_form (sections);
  let normal_sections : Vec<GraphnodeOnDisk> = normal . values ()
    . map ( |yaml| yaml_serde::from_str (yaml) . unwrap () ) . collect ();
  let facts : SkgrepoGitFacts = SkgrepoGitFacts {
    remote_hash : Some ("r" . into ()), commit : Some ("c" . into ()) };
  let skgrepo : SkgrepoName = SkgrepoName::from ("main");
  let vault : RenderedVault = render_vault (&skgrepo, &normal_sections, &facts)
    . map_err ( |e| e . to_string () ) ?;
  let imported : ImportedVault = read_vault_files (&vault . files)
    . map_err ( |e| format! ("import failed: {}", e) ) ?;
  let imported_sections : &[GraphnodeOnDisk] = imported . get ("main")
    . map ( |v| v . as_slice () ) . unwrap_or (&[]);
  let imported_yaml : BTreeMap<ID, String> = imported_sections . iter ()
    . map ( |s| (s . pid . clone (), s . to_yaml () . unwrap ()) ) . collect ();
  if imported_yaml != normal {
    let differing : Vec<String> = normal . iter ()
      . filter ( |(pid, yaml)| imported_yaml . get (*pid) != Some (*yaml) )
      . map ( |(pid, yaml)| format! ("--- {}\nwant:\n{}got:\n{}", pid . as_str (), yaml,
                                      imported_yaml . get (pid) . map_or ("(none)\n", |s| s . as_str ())) )
      . chain ( imported_yaml . keys () . filter ( |pid| ! normal . contains_key (*pid) )
                . map ( |pid| format! ("--- {}: imported, but not exported", pid . as_str ()) ) )
      . collect ();
    return Err ( format! ("import(export(S)) differs:\n{}", differing . join ("\n")) ); }
  let re_export : RenderedVault = render_vault (&skgrepo, imported_sections, &facts)
    . map_err ( |e| e . to_string () ) ?;
  if re_export . files != vault . files {
    let differing : Vec<&String> = vault . files . keys ()
      . filter ( |path| re_export . files . get (*path) != vault . files . get (*path) ) . collect ();
    return Err ( format! ("export(import(V)) differs in {:?}", differing) ); }
  Ok (( )) }

#[test]
fn the_fixture_round_trips () {
  round_trip (&fixture_sections ()) . unwrap_or_else ( |e| panic! ("{}", e) ); }

#[test]
fn the_fixture_round_trips_through_the_disk () {
  let sections : Vec<GraphnodeOnDisk> = fixture_sections ();
  let mut vault : RenderedVault = render_vault (
    &SkgrepoName::from ("main"), &sections, &SkgrepoGitFacts::default () ) . unwrap ();
  let dir : TempDir = TempDir::new () . unwrap ();
  write_vault (&mut vault, & dir . path () . join ("vault")) . unwrap ();
  let imported : ImportedVault = read_vault (& dir . path () . join ("vault")) . unwrap ();
  assert_eq! (imported . keys () . collect::<Vec<_>> (), vec! [ "main" ]);
  let written : usize = write_imported_vault (&imported, & dir . path () . join ("out")) . unwrap ();
  assert_eq! (written, sections . len ());
  for (pid, yaml) in normal_form (&sections) {
    let file : PathBuf = dir . path () . join ( format! ("out/main/{}.skg", pid . as_str ()) );
    assert_eq! (fs::read_to_string (&file) . unwrap (), yaml, "{}", file . display ()); }
  assert! (write_imported_vault (&imported, & dir . path () . join ("out")) . is_err (),
           "a second import into the same folder is refused"); }

/// skg's own documentation: 1,267 notes of real text.
#[test]
fn skg_system_round_trips () {
  let folder : PathBuf = Path::new (env! ("CARGO_MANIFEST_DIR")) . join ("skg-system");
  let mut sections : Vec<GraphnodeOnDisk> = Vec::new ();
  for entry in fs::read_dir (&folder) . unwrap () {
    let path : PathBuf = entry . unwrap () . path ();
    if path . extension () . is_some_and ( |e| e == "skg" ) {
      sections . push ( yaml_serde::from_str (& fs::read_to_string (&path) . unwrap ()) . unwrap () ); }}
  assert! (sections . len () > 1000);
  round_trip (&sections) . unwrap_or_else ( |e| panic! ("{}", e) ); }

#[test]
fn a_hand_edit_the_dialect_cannot_place_is_named () {
  let mut vault : RenderedVault = render_vault (
    &SkgrepoName::from ("main"), &fixture_sections (), &SkgrepoGitFacts::default () ) . unwrap ();
  let path : String = vault . files . keys ()
    . find ( |p| p . starts_with ("main/Threads") ) . unwrap () . clone ();
  let edited : String = vault . files [&path] . replacen ("\n- Second child", "\n   stray\n- Second child", 1);
  vault . files . insert (path, edited);
  let error : String = read_vault_files (&vault . files) . err () . unwrap () . to_string ();
  assert! (error . contains ("main/Threads"), "{}", error); }

#[test]
fn the_import_reads_the_folders_the_readme_lists_in_its_format () {
  let mut vault : RenderedVault = render_vault (
    &SkgrepoName::from ("main"), &fixture_sections (), &SkgrepoGitFacts::default () ) . unwrap ();
  let some_file : String = vault . files . keys () . find ( |p| p . starts_with ("main/") ) . unwrap () . clone ();
  let copy : String = vault . files [&some_file] . clone ();
  vault . files . insert ( some_file . replacen ("main/", "mine/", 1), copy );
  let imported : ImportedVault = read_vault_files (&vault . files) . unwrap ();
  assert_eq! (imported . keys () . collect::<Vec<_>> (), vec! [ "main" ], "mine/ is not listed");
  let readme : String = vault . files ["README.md"] . replace (VAULT_FORMAT, "skg-obsidian 0");
  vault . files . insert ("README.md" . to_string (), readme);
  let error : String = read_vault_files (&vault . files) . err () . unwrap () . to_string ();
  assert! (error . contains ("skg-obsidian 0"), "{}", error); }

//
// Random skgrepos
//

const PIDS : &[&str] = &[ "a1", "b-2", "C3", "d\u{201c}4", "skgid-41", "f6" ];
const EXTRA : &str = "x-extra";

fn arb_plain (
  one_line : bool,
) -> impl Strategy<Value = String> {
  let class : &'static str =
    if one_line { "[a-z \\[\\]%^#$!`~<>+*.()\\\\\\-|=:/\t]{0,12}" }
    else { "[a-z \\[\\]%^#$!`~<>+*.()\\\\\\-|=:/\t\n]{0,24}" };
  let org_markup : Vec<&'static str> = vec! [
    " *b* ", " /i/ ", " =v= ", " ~c~ ", " *a /b/ c* ", " =a`b= ", "`code`", "**", "*",
    "[[https://x.org/a(b)][l]]", "[[https://y]]", "[[file:z][f]]", "<https://q>", "[a](b)",
    "\n#+begin_src sh\necho *x* ~~~\n#+end_src\n", "\n#+BEGIN_EXAMPLE\n~~~\n#+END_EXAMPLE\n",
    "\n  #+begin_src\nx\n  #+end_src\n", "\n#+begin_quote\nq\n#+end_quote\n",
    "\n#+begin_src a\nunclosed\n", "\n: fixed\n:\n: \n", "\n| a | b |\n|---+---|\n| c | d |\n",
    "\n|:-|\n", "\n| a |\n|---|\n" ];
  prop_oneof! [
    4 => class . prop_map ( |s| s ),
    1 => Just ("\n```\ncode ^x [[y]]\n```\n" . to_string ()),
    1 => Just ("```\n" . to_string ()),
    1 => Just ("\n- item ^id-like\n" . to_string ()),
    1 => Just ("%%skg section%%" . to_string ()),
    4 => prop::sample::select (org_markup) . prop_map ( |s| s . to_string () ),
  ] . prop_map ( move |s| if one_line { s . replace ('\n', " ") } else { s } ) }

fn arb_text (
  one_line : bool,
) -> impl Strategy<Value = String> {
  let link = ( prop::sample::select (vec! [ "a1", "C3", "d\u{201c}4", "x-extra", "gone", "f6" ]),
               "[a-z |\\]`%]{0,6}" )
    . prop_map ( |(pid, label)| format! ("[[id:{}][{}]]", pid, label) );
  prop::collection::vec ( prop_oneof! [ 2 => arb_plain (one_line), 1 => link ], 0 .. 5 )
    . prop_map ( |parts| parts . concat () ) }

fn arb_member () -> impl Strategy<Value = ListItem> {
  prop_oneof! [
    6 => prop::sample::select ( PIDS . to_vec () ) . prop_map ( |p| ListItem::Member (ID::from (p)) ),
    1 => Just (ListItem::Member (ID::from (EXTRA))),
    1 => Just (ListItem::Member (ID::from ("gone"))),
    1 => Just (ListItem::Anchor { anchor : ID::from ("a1") }) ] }

prop_compose! {
  fn arb_section (pid : &'static str) (
    title in prop::option::weighted (0.9, arb_text (true) . prop_filter ("nonempty", |t| ! t . is_empty ())),
    aliases in prop::collection::vec (arb_plain (true), 0 .. 2),
    body in prop::option::of (arb_text (false)),
    contains in prop::collection::vec (arb_member (), 0 .. 4),
    forks in prop::collection::vec (arb_member (), 0 .. 2),
    hides in prop::collection::vec (prop::sample::select (vec! [ "a1", "gone", EXTRA ]), 0 .. 2),
    flags in prop::sample::subsequence (Flag::ALL . to_vec (), 0 ..= 2),
    attribution in prop::option::weighted (0.2, (any::<bool> (), any::<bool> ())),
  ) -> GraphnodeOnDisk {
    GraphnodeOnDisk {
      title, aliases, pid : ID::from (pid), extra_ids : Vec::new (), body, contains, forks,
      hides : hides . into_iter () . map (ID::from) . collect (), flags,
      attribution : attribution . map ( |(has_hash, uncommitted)| Attribution {
        original : ID::from ("gone"), skgrepo : SkgrepoName::from ("elsewhere"),
        remote_hash : if has_hash { Some ("h" . into ()) } else { None },
        commit : None, uncommitted } ), }}
}

fn arb_skgrepo () -> impl Strategy<Value = Vec<GraphnodeOnDisk>> {
  ( 1 .. PIDS . len () + 1, any::<bool> () )
    . prop_flat_map ( |(n, with_extra)| {
      let sections : Vec<_> = PIDS [.. n] . iter () . map ( |pid| arb_section (pid) ) . collect ();
      ( sections, Just (with_extra) ) } )
    . prop_map ( |(mut sections, with_extra) : (Vec<GraphnodeOnDisk>, bool)| {
      if with_extra { sections [0] . extra_ids = vec! [ ID::from (EXTRA) ]; }
      sections } ) }

proptest! {
  #![proptest_config (ProptestConfig::with_cases (300))]
  #[test]
  fn random_skgrepos_round_trip (sections in arb_skgrepo ()) {
    if let Err (e) = round_trip (&sections) { prop_assert! (false, "{}", e); }}
}
