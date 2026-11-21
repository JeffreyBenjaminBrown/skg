//! Unit tests for server/obsidian/export.rs, over the fixture in
//! obsidian_fixture.rs. Each exported vault is also run through the
//! checker (server/obsidian/vault_check.rs).

use super::*;
use crate::obsidian::vault_check::{VaultCheckReport, check_vault};
use crate::types::misc::{SkgConfig, Skgrepo};

#[path = "obsidian_fixture.rs"]
mod fixture;
use fixture::*;

use std::collections::HashMap;
use tempfile::TempDir;

fn render_fixture () -> RenderedVault {
  render_vault (
    &SkgrepoName::from ("main"), &fixture_sections (),
    &SkgrepoGitFacts { remote_hash : Some ("r".into ()), commit : Some ("c".into ()) } )
  . unwrap () }

/// The file whose name begins with 'prefix', in the folder 'main/'.
fn file_starting<'a> (
  vault  : &'a RenderedVault,
  prefix : &str,
) -> (&'a str, &'a str) {
  let mut found = vault . files . iter ()
    . filter ( |(path, _)| path . starts_with (& format! ("main/{}", prefix)) );
  let (path, text) = found . next ()
    . unwrap_or_else ( || panic! ("no file main/{}*; files: {:?}", prefix, vault . files . keys () . collect::<Vec<_>> ()) );
  assert! (found . next () . is_none (), "two files start with {}", prefix);
  (path, text) }

#[test]
fn the_fixture_vault_passes_the_checker () {
  let mut vault : RenderedVault = render_fixture ();
  let dir : TempDir = TempDir::new () . unwrap ();
  write_vault (&mut vault, dir . path ()) . unwrap ();
  let report : VaultCheckReport = check_vault (dir . path ()) . unwrap ();
  assert! (report . problems . is_empty (), "{}", report . summary ());
  assert_eq! (report . manifest_notes, fixture_sections () . len ());
  assert! (report . embeds >= 4, "{}", report . summary ()); }

#[test]
fn two_exports_are_identical () {
  assert_eq! (render_fixture () . files, render_fixture () . files); }

#[test]
fn the_report_counts_what_could_not_be_written_plainly () {
  let report : ObsidianExportReport = render_fixture () . report;
  assert_eq! (report . notes, fixture_sections () . len ());
  assert_eq! (report . title_fallbacks, 1, "the title with leading blanks");
  assert_eq! (report . body_fallbacks, 2, "the carriage return and the leading blank lines");
  assert_eq! (report . absent_members, 1);
  assert_eq! (report . links_left_as_text, 1, "the label with a bar"); }

#[test]
fn a_note_with_two_containers_is_written_once_and_embedded_elsewhere () {
  let vault : RenderedVault = render_fixture ();
  let (_, threads) = file_starting (&vault, "Threads (00000001)");
  let (_, other) = file_starting (&vault, "Other document");
  assert! (threads . contains (& format! ("- Shared note ^{}\n", id (SHARED) . as_str ())));
  assert! (other . contains (& format! (
    "- ![[Threads (00000001)#^{}]]\n", id (SHARED) . as_str ())), "{}", other);
  assert! (other . contains ("- ![[Forker (00000007)]]\n"), "{}", other); }

#[test]
fn a_cycle_is_closed_by_an_embed () {
  let vault : RenderedVault = render_fixture ();
  let (_, threads) = file_starting (&vault, "Threads (00000001)");
  assert! (threads . contains (& format! (
    "    - ![[Threads (00000001)#^{}]]\n", id (FIRST) . as_str ())), "{}", threads);
  let (_, cycle) = file_starting (&vault, "Cycle one");
  assert! (cycle . contains ("- Cycle two ^"));
  assert! (cycle . contains ("  - ![[Cycle one (00000009)]]\n"), "{}", cycle); }

#[test]
fn a_fork_has_its_own_file_with_its_metadata_as_properties () {
  let vault : RenderedVault = render_fixture ();
  let (_, forker) = file_starting (&vault, "Forker (00000007)");
  let expected_head : String = format! (
"---
skg-id: \"{forker}\"
skg-forks:
  - \"{outside}\"
  - \"[[Threads (00000001)#^{shared}]]\"
  - \"skg-anchor {shared}\"
skg-attribution-original: \"{outside}\"
skg-attribution-skgrepo: \"elsewhere\"
skg-attribution-remote-hash: \"abc123\"
skg-attribution-commit: \"def456\"
skg-attribution-uncommitted: true
---
# Forker

- Forker's child ^{kid}
",
    forker = id (FORKER) . as_str (), outside = id (OUTSIDE + 3) . as_str (),
    shared = id (SHARED) . as_str (), kid = id (FORKER_KID) . as_str () );
  assert_eq! (forker, expected_head); }

#[test]
fn a_bullet_carries_its_metadata_in_one_comment () {
  let vault : RenderedVault = render_fixture ();
  let (_, threads) = file_starting (&vault, "Threads (00000001)");
  let expected : String = format! (
"- Second child, with a [[Threads (00000001)#^{first}|link]] ^{second}

  %%skg {{\"aliases\":[\"x\",\"y: z\"],\"skg-flags\":[\"NoSearchMatching\"],\"skg-hides\":[\"{hidden}\"]}}%%
",
    first = id (FIRST) . as_str (), second = id (SECOND) . as_str (),
    hidden = id (OUTSIDE + 2) . as_str () );
  assert! (threads . contains (&expected), "{}", threads);
  assert! (threads . contains (& format! (
    "- [[README#Links to notes outside this vault|(a note outside this vault)]] \
     %%skg {{\"skg-absent\":\"{}\"}}%%\n", id (OUTSIDE) . as_str () )));
  assert! (threads . contains (& format! (
    "- %%skg {{\"skg-anchor\":\"{}\"}}%%\n", id (OUTSIDE + 1) . as_str () )));
  assert! (threads . contains (& format! (
    "- Third, listed by an extra ID ^{}\n\n  %%skg {{\"skg-extra-ids\":[\"{}\"],\"skg-ref\":\"{}\"}}%%\n",
    id (BY_EXTRA) . as_str (), EXTRA_ID, EXTRA_ID )), "{}", threads); }

#[test]
fn links_say_their_id_exactly () {
  let vault : RenderedVault = render_fixture ();
  let (_, threads) = file_starting (&vault, "Threads (00000001)");
  assert! (threads . contains (& format! (
    "[[Threads (00000001)#^{}|extra]]%%skg-id {}%%", id (BY_EXTRA) . as_str (), EXTRA_ID )));
  assert! (threads . contains (& format! (
    "[[README#Links to notes outside this vault|gone]]%%skg-id {}%%", id (OUTSIDE) . as_str () )));
  assert! (threads . contains ("a bar: \\[\\[id:"), "a label with a bar stays text");
  assert! (threads . contains (& format! (
    "\\\\[[Threads (00000001)#^{}|after a backslash]] and \\![[", id (SECOND) . as_str () )), "{}", threads);
  let weird : String = block_id_of_pid (&ID::from (WEIRD_ID));
  assert! (threads . contains (& format! ("- A weird ID ^{}\n", weird)));
  assert! (threads . contains (& format! ("[[Threads (00000001)#^{}|me]]", weird))); }

#[test]
fn code_is_copied_and_everything_else_escaped () {
  let vault : RenderedVault = render_fixture ();
  let (_, threads) = file_starting (&vault, "Threads (00000001)");
  for expected in [
    "\\# an Org comment line", "Prices: \\$5, 50\\%\\% off, x\\^2 and a trailing \\^word",
    "A \\#tag, \\[\\[https://example.com][a web link]]",
    "```text\n- x ^notanid\n[[id:", // a fence: copied
    "after a blank line, \\%\\% and \\[\\[not a link]] and !\\[\\[nor an embed]]" ] {
    assert! (threads . contains (expected), "missing {:?} in\n{}", expected, threads); }
  let (_, markdownish) = file_starting (&vault, "Markdown-ish body");
  for expected in [ "\\```\nunclosed fence", "\\<!-- an unclosed", "\\<div>",
                    "- item \\^looks-like-an-id", "\\$\\$" ] {
    assert! (markdownish . contains (expected), "missing {:?} in\n{}", expected, markdownish); }}

#[test]
fn bodies_the_escapes_cannot_carry_go_in_a_verbatim_fence () {
  let vault : RenderedVault = render_fixture ();
  let (_, carriage) = file_starting (&vault, "Body with a carriage return");
  assert! (carriage . contains ("```skg-verbatim-json\n\"line one\\r\\nline two\"\n```\n"), "{}", carriage);
  let (_, blanks) = file_starting (&vault, "Body with leading blank lines");
  assert! (blanks . contains ("```skg-verbatim\n\n\nafter blanks\n\n```\n"), "{}", blanks); }

#[test]
fn odd_titles_get_safe_unique_filenames () {
  let vault : RenderedVault = render_fixture ();
  let names : Vec<&String> = vault . files . keys () . collect ();
  for expected in [
    "main/a b c d e f g h i q 100 (00000064).md",
    "main/see also (00000066).md", "main/see also (00000067).md",
    "main/Case (00000068).md", "main/case (00000069).md",
    "main/First notes (0000006a).md", "main/hidden-looking (00000072).md",
    "main/back tick title (0000006f).md", "main/section (000000c8).md" ] {
    assert! (names . iter () . any ( |n| n . as_str () == expected ), "no {}: {:?}", expected, names); }
  let long : &&String = names . iter () . find ( |n| n . starts_with ("main/word word") ) . unwrap ();
  assert! (long . chars () . count () < 100, "{}", long); }

#[test]
fn filename_base_cases () {
  assert_eq! (filename_base ("a/b: c"), "a b c");
  assert_eq! (filename_base ("...dots"), "dots");
  assert_eq! (filename_base ("ends."), "ends");
  assert_eq! (filename_base ("[[id:x][Label]] rest"), "Label rest");
  assert_eq! (filename_base ("?"), "untitled");
  let long : String = format! ("{} tail", "x" . repeat (100));
  assert_eq! (filename_base (&long) . chars () . count (), 80); }

#[test]
fn a_titles_rendering_is_distinguished_from_a_titleless_section () {
  let vault : RenderedVault = render_fixture ();
  let (_, section) = file_starting (&vault, "section (000000c8)");
  assert! (section . contains ("\n# %%skg section%%\n"));
  let (_, lookalike) = file_starting (&vault, "skg section (00000074)");
  assert! (lookalike . contains ("\n# \\%\\%skg section\\%\\%\n"), "{}", lookalike); }

#[test]
fn a_document_reads_as_an_outline () {
  let sections : Vec<GraphnodeOnDisk> = vec! [
    { let mut s : GraphnodeOnDisk = section (1, "Topic");
      s . body = Some ("About the topic." . to_string ());
      s . contains = vec! [ ListItem::Member (id (2)), ListItem::Member (id (4)) ];
      s },
    { let mut s : GraphnodeOnDisk = section (2, "A point");
      s . body = Some ("Its detail,\nover two lines." . to_string ());
      s . contains = vec! [ ListItem::Member (id (3)) ];
      s },
    section (3, "A sub-point"),
    section (4, "Another point") ];
  let vault : RenderedVault = render_vault (
    &SkgrepoName::from ("main"), &sections, &SkgrepoGitFacts::default () ) . unwrap ();
  assert_eq! (vault . files ["main/Topic (00000001).md"], format! (
"---
skg-id: \"{one}\"
---
# Topic

About the topic.

- A point ^{two}

  Its detail,
  over two lines.

  - A sub-point ^{three}
- Another point ^{four}
",
    one = id (1) . as_str (), two = id (2) . as_str (),
    three = id (3) . as_str (), four = id (4) . as_str () )); }

//
// Writing, and privacy
//

fn config_of (
  skgrepos : &[(&str, &Path, bool)], // name, folder, owned
) -> SkgConfig {
  let map : HashMap<SkgrepoName, Skgrepo> = skgrepos . iter ()
    . map ( |(name, path, owned)| (
      SkgrepoName::from (*name),
      Skgrepo { name : SkgrepoName::from (*name), abbreviation : None,
                path : path . to_path_buf (), owned : *owned } ) )
    . collect ();
  SkgConfig::dummyFromSkgrepos (map) }

fn write_sections (
  folder   : &Path,
  sections : &[GraphnodeOnDisk],
) {
  fs::create_dir_all (folder) . unwrap ();
  for section in sections {
    fs::write ( folder . join ( format! ("{}.skg", section . pid . as_str ()) ),
                section . to_yaml () . unwrap () ) . unwrap (); }}

#[test]
fn no_text_of_another_skgrepo_reaches_the_vault () {
  let dir : TempDir = TempDir::new () . unwrap ();
  let public : PathBuf = dir . path () . join ("pub");
  let private : PathBuf = dir . path () . join ("priv");
  let mut public_note : GraphnodeOnDisk = section (1, "Public note");
  public_note . body = Some ( format! ("See [[id:{}][a private note]].", id (2) . as_str ()) );
  public_note . contains = vec! [ ListItem::Member (id (2)) ];
  let mut private_note : GraphnodeOnDisk = section (2, "SECRET-MARKER title");
  private_note . body = Some ("SECRET-MARKER body" . to_string ());
  private_note . aliases = vec! [ "SECRET-MARKER alias" . to_string () ];
  let mut private_section : GraphnodeOnDisk = section (1, "unused");
  private_section . title = None;
  private_section . aliases = vec! [ "SECRET-MARKER section alias" . to_string () ];
  write_sections (&public, &[public_note]);
  write_sections (&private, &[private_note, private_section]);
  let config : SkgConfig = config_of (&[ ("pub", &public, true), ("priv", &private, true) ]);
  let vault_dir : PathBuf = dir . path () . join ("vault");
  export_skgrepo_to_vault (&config, &SkgrepoName::from ("pub"), &vault_dir) . unwrap ();
  for entry in walkdir::WalkDir::new (&vault_dir) {
    let entry = entry . unwrap ();
    if ! entry . file_type () . is_file () { continue; }
    let text : String = fs::read_to_string (entry . path ()) . unwrap ();
    assert! (! text . contains ("SECRET-MARKER"), "{} leaks: {}", entry . path () . display (), text); }
  assert! (check_vault (&vault_dir) . unwrap () . problems . is_empty ()); }

#[test]
fn a_foreign_skgrepo_is_refused () {
  let dir : TempDir = TempDir::new () . unwrap ();
  write_sections (& dir . path () . join ("f"), &[ section (1, "A note") ]);
  let config : SkgConfig = config_of (&[ ("f", & dir . path () . join ("f"), false) ]);
  let result = export_skgrepo_to_vault (&config, &SkgrepoName::from ("f"), & dir . path () . join ("v"));
  assert! (result . is_err ()); }

#[test]
fn a_re_export_removes_files_it_no_longer_writes_and_nothing_else () {
  let dir : TempDir = TempDir::new () . unwrap ();
  let vault_dir : PathBuf = dir . path () . join ("vault");
  let render = |title : &str| -> RenderedVault {
    render_vault (&SkgrepoName::from ("main"), &[ section (1, title) ],
                  &SkgrepoGitFacts::default ()) . unwrap () };
  write_vault (&mut render ("Old name"), &vault_dir) . unwrap ();
  fs::create_dir_all (vault_dir . join (".obsidian")) . unwrap ();
  fs::write (vault_dir . join ("main/mine.md"), "a responder's note") . unwrap ();
  let mut renamed : RenderedVault = render ("New name");
  write_vault (&mut renamed, &vault_dir) . unwrap ();
  assert_eq! (renamed . report . files_removed, vec! [ "main/Old name (00000001).md" . to_string () ]);
  assert! (vault_dir . join ("main/New name (00000001).md") . exists ());
  assert! (vault_dir . join ("main/mine.md") . exists ());
  { // a manifest naming a path outside the vault removes nothing there
    let manifest : PathBuf = vault_dir . join (MANIFEST_FILENAME);
    let text : String = fs::read_to_string (&manifest) . unwrap ();
    fs::write (dir . path () . join ("outside.md"), "x") . unwrap ();
    fs::write (&manifest, text . replace ("\"main/New name (00000001).md\"", "\"../outside.md\"")) . unwrap ();
    write_vault (&mut render ("New name"), &vault_dir) . unwrap ();
    assert! (dir . path () . join ("outside.md") . exists ()); }
  let elsewhere : PathBuf = dir . path () . join ("not-a-vault");
  fs::create_dir_all (&elsewhere) . unwrap ();
  fs::write (elsewhere . join ("x.txt"), "x") . unwrap ();
  assert! (write_vault (&mut render ("A"), &elsewhere) . is_err ()); }

