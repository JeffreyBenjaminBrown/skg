//! Unit tests for server/obsidian/dialect.rs.
//! Included via `#[path = "../../tests/unit/obsidian_dialect.rs"] mod tests;`.

use super::*;
use crate::obsidian::read_text::unrender_text;

use proptest::prelude::*;

fn escape_text (
  text     : &str,
  is_title : bool,
) -> String {
  escape_pieces (&[Piece::Text (text)], is_title) }

#[test]
fn a_uuid_is_its_own_block_id () {
  let pid : ID = ID::from ("0a1b2c3d-4e5f-6789-abcd-ef0123456789");
  assert_eq! (block_id_of_pid (&pid), pid . as_str ());
  assert_eq! (pid_of_block_id (pid . as_str ()), Some (pid)); }

#[test]
fn other_ids_are_written_in_hex_and_read_back () {
  for raw in [ "21c9d917-f75a-4983-bc0e\u{201c}-a95d31384856", "Abc", "a_b",
               "skgid-41", "with space" ] {
    let pid : ID = ID::from (raw);
    let block_id : String = block_id_of_pid (&pid);
    assert! (block_id . starts_with ("skgid-"), "{}", block_id);
    assert! (is_written_block_id (&block_id));
    assert_eq! (pid_of_block_id (&block_id), Some (pid)); }}

#[test]
fn a_trailing_block_id_splits_off () {
  assert_eq! (split_trailing_block_id ("- Title ^ab-12"), Some (("- Title", "ab-12")));
  assert_eq! (split_trailing_block_id ("- Title \\^ab"), None);
  assert_eq! (split_trailing_block_id ("- Title ^Ab"), None); }

#[test]
fn structural_characters_are_escaped () {
  assert_eq! (escape_text ("[[x]]", false), "\\[\\[x]]");
  assert_eq! (escape_text ("a [b] c", false), "a [b] c");
  assert_eq! (escape_text ("50%% off", false), "50\\%\\% off");
  assert_eq! (escape_text ("5% off", false), "5% off");
  assert_eq! (escape_text ("x ^word", false), "x \\^word");
  assert_eq! (escape_text ("$5 and #tag", false), "\\$5 and \\#tag");
  assert_eq! (escape_text ("# comment\nC# code", false), "\\# comment\nC# code");
  assert_eq! (escape_text ("```\nunclosed", false), "\\`\\`\\`\nunclosed");
  assert_eq! (escape_text ("~~~\nunclosed", false), "\\~~~\nunclosed");
  assert_eq! (escape_text ("<!-- x\n<div> and <hash>, a < b", false),
              "\\<!-- x\n\\<div> and \\<hash>, a < b");
  assert_eq! (escape_text ("a \\ b \\# c", false), "a \\ b \\\\# c"); }

/// What the inverse of the conversions would read as Markdown made
/// from Org is escaped when the text itself holds it.
#[test]
fn what_looks_converted_is_escaped () {
  assert_eq! (escape_text ("`code` and **bold** and 2*3", false),
              "\\`code\\` and \\*\\*bold\\*\\* and 2\\*3");
  assert_eq! (escape_text ("* a list item\n  * nested", false), "* a list item\n  * nested");
  assert_eq! (escape_text ("* not in a title", true), "\\* not in a title");
  assert_eq! (escape_text ("[a](b) but [a] (b) and [ ] x", false), "\\[a](b) but [a] (b) and [ ] x");
  assert_eq! (escape_text ("|:-|:--|\n| a |:-|", false), "\\|:-|:--|\n| a |:-|");
  assert_eq! (escape_text ("|---|---|", false), "|---|---|"); }

#[test]
fn table_rules_are_written_with_colons_and_read_back () {
  assert_eq! (table_rule_written_for ("|-----+----|", 2), Some ("|:----|:---|" . to_string ()));
  assert_eq! (table_rule_written_for ("|-----+----|", 3), None);
  assert_eq! (table_rule_written_for ("|---|", 1), Some ("|:--|" . to_string ()));
  assert_eq! (table_rule_written_for ("|-+--|", 2), None, "a one-dash cell");
  assert! (is_table_rule_written ("|:----|:---|"));
  assert! (! is_table_rule_written ("|----|----|"));
  assert_eq! (org_table_rule_of ("|:----|:---|"), "|-----+----|"); }

#[test]
fn org_blocks_round_trip_through_fence_info () {
  for block in [
    OrgBlock::Src { arguments : "rust".into (), uppercase : false },
    OrgBlock::Src { arguments : "sh :results output".into (), uppercase : true },
    OrgBlock::Src { arguments : String::new (), uppercase : false },
    OrgBlock::Src { arguments : String::new (), uppercase : true },
    OrgBlock::Src { arguments : " two spaces".into (), uppercase : false },
    OrgBlock::Example { uppercase : false },
    OrgBlock::Example { uppercase : true },
    OrgBlock::FixedWidth ] {
    let info : String = fence_info_of_org_block (&block) . unwrap ();
    assert_eq! (org_block_of_fence_info (&info), block, "{:?}", info); }
  assert_eq! (fence_info_of_org_block (&OrgBlock::Example { uppercase : false }), Some (String::new ()));
  assert_eq! (fence_info_of_org_block (&OrgBlock::Src { arguments : "skg-upper".into (), uppercase : false }), None);
  assert_eq! (org_block_delimiters (&OrgBlock::Src { arguments : "rust".into (), uppercase : true }),
              Some (("#+BEGIN_SRC rust" . into (), "#+END_SRC" . into ())));
  assert_eq! (tilde_fence_for (&[ "a", "  ~~~~ b" ]), "~~~~~"); }

#[test]
fn a_title_escapes_a_leading_list_marker () {
  assert_eq! (escape_text ("- dash", true), "\\- dash");
  assert_eq! (escape_text ("1. Intro", true), "1\\. Intro");
  assert_eq! (escape_text ("> quoted", true), "\\> quoted");
  assert_eq! (escape_text ("- dash", false), "- dash");
  assert_eq! (escape_text ("a - b", true), "a - b"); }

#[test]
fn text_next_to_a_wikilink_is_escaped_so_the_link_stays_whole () {
  let pieces : Vec<Piece> = vec! [
    Piece::Text ("see ["), Piece::Produced ("[[F#^a|x]]%%skg-id b%%" . to_string ()),
    Piece::Text ("% and !\\"), Piece::Produced ("[[G|y]]" . to_string ()) ];
  assert_eq! (escape_pieces (&pieces, false),
              "see \\[[[F#^a|x]]%%skg-id b%%\\% and !\\\\[[G|y]]");
  let embed_like : Vec<Piece> = vec! [
    Piece::Text ("!"), Piece::Produced ("[[G|y]]" . to_string ()) ];
  assert_eq! (escape_pieces (&embed_like, false), "\\![[G|y]]"); }

#[test]
fn balanced_fences_are_found_and_unbalanced_ones_are_not () {
  let text : &str = "a\n```sh\nx [[y]]\n```\nb\n~~~\nopen";
  let ranges : Vec<Range<usize>> = balanced_fence_ranges (text);
  assert_eq! (ranges . len (), 1);
  assert_eq! (&text [ranges [0] . clone ()], "```sh\nx [[y]]\n```\n");
  assert_eq! (fence_for ("a ```` b"), "`````");
  assert_eq! (fence_for ("plain"), "```"); }

#[test]
fn a_meta_comment_cannot_end_early () {
  let mut object : serde_json::Map<String, serde_json::Value> = serde_json::Map::new ();
  object . insert ("aliases" . into (), serde_json::json! (["100%%", "b"]));
  let comment : String = meta_comment (&object);
  assert_eq! (comment . matches ("%%") . count (), 2);
  assert_eq! (parse_meta_comment (&comment), Some (object)); }

proptest! {
  /// Escaping is undone exactly, for titles and bodies, whatever the
  /// mix of structural characters.
  #[test]
  fn unescape_inverts_escape (
    text in "[\\[\\]%^#$!`~<>+*.)(|:=/\\\\ \\-\n\ta1é]{0,40}",
    is_title in any::<bool> (),
  ) {
    let escaped : String = escape_text (&text, is_title);
    prop_assert_eq! (unescape_text (&escaped), text . clone ());
    let text = if is_title { text . replace ('\n', " ") } else { text };
    let escaped : String = escape_text (&text, is_title);
    prop_assert_eq! (unrender_text (&escaped, &|_| None), Some (text), "{:?}", escaped);
    prop_assert! (! has_unescaped (&escaped, "[["), "{:?}", escaped);
    prop_assert! (! has_unescaped (&escaped, "%%"), "{:?}", escaped); }
}

/// Whether 'pattern' occurs in 'escaped' at a position no backslash
/// escapes.
fn has_unescaped (
  escaped : &str,
  pattern : &str,
) -> bool {
  let chars : Vec<char> = escaped . chars () . collect ();
  let mut i : usize = 0;
  while i < chars . len () {
    if chars [i] == '\\' { i += 2; continue; }
    if chars [i ..] . iter () . collect::<String> () . starts_with (pattern) { return true; }
    i += 1; }
  false }
