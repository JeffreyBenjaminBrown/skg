//! Format-aware, source-preserving document parsing for interactive import.

use pulldown_cmark::{Event, LinkType, Options, Parser, Tag, TagEnd};
use serde_yaml::Value;
use std::ops::Range;
use std::path::{Path, PathBuf};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DocumentFormat { Markdown, Org }

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Diagnostic {
  pub range : Range<usize>,
  pub message : String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParsedSection {
  pub level : usize,
  pub title : String,
  pub heading : Range<usize>,
  pub body : Range<usize>,
  pub explicit_id : Option<String>,
  pub custom_id : Option<String>,
  pub aliases : Vec<String>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParsedDocument {
  pub path : PathBuf,
  pub format : DocumentFormat,
  pub text : String,
  pub sections : Vec<ParsedSection>,
  pub diagnostics : Vec<Diagnostic>,
  /// Replacements are located in the original text, before sections split it.
  pub source_edits : Vec<SourceEdit>,
  pub links : Vec<ParsedLink>,
  pub literal_ranges : Vec<Range<usize>>,
  pub footnote_definitions : Vec<ParsedFootnoteDefinition>,
  pub footnote_references : Vec<ParsedFootnoteReference>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParsedFootnoteDefinition {
  pub name : String,
  pub range : Range<usize>,
  pub ambiguous : bool,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParsedFootnoteReference {
  pub name : String,
  pub range : Range<usize>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LinkSyntax { Markdown, Org }

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParsedLink {
  pub range : Range<usize>,
  pub destination : String,
  pub label : String,
  pub syntax : LinkSyntax,
  pub reference_id : Option<String>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SourceEdit {
  pub range : Range<usize>,
  pub replacement : String,
}

pub fn parse_document (
  path : &Path,
  text : String,
) -> ParsedDocument {
  let format : DocumentFormat =
    if path . extension () . and_then (|s| s . to_str ()) == Some ("md") {
      DocumentFormat::Markdown
    } else { DocumentFormat::Org };
  let mut document : ParsedDocument = ParsedDocument {
    path : path . to_path_buf (),
    format,
    text,
    sections : Vec::new (),
    diagnostics : Vec::new (),
    source_edits : Vec::new (),
    links : Vec::new (),
    literal_ranges : Vec::new (),
    footnote_definitions : Vec::new (),
    footnote_references : Vec::new (),
  };
  let fallback_title : String = path . file_stem ()
    . map (|s| s . to_string_lossy () . into_owned ())
    . unwrap_or_default ();
  let (title, aliases, explicit_id, custom_id) :
    (String, Vec<String>, Option<String>, Option<String>) =
    match format {
      DocumentFormat::Markdown => {
        let (title, aliases) : (Option<String>, Vec<String>) =
          parse_markdown_front_matter (&mut document);
        (title . unwrap_or (fallback_title), aliases, None, None) }
      DocumentFormat::Org =>
        (fallback_title, Vec::new (), None, None),
    };
  document . sections . push (ParsedSection {
    level : 0,
    title,
    heading : 0..0,
    body : 0..document . text . len (),
    explicit_id,
    custom_id,
    aliases,
  });
  match format {
    DocumentFormat::Markdown => parse_markdown_headings (&mut document),
    DocumentFormat::Org => {
      parse_org_headings (&mut document);
      parse_org_inline_literals (&mut document);
      let (title, aliases, id, custom_id) =
        parse_org_file_metadata (&document);
      let root : &mut ParsedSection = &mut document . sections [0];
      if let Some (title) = title { root . title = title; }
      root . aliases = aliases;
      root . explicit_id = id;
      root . custom_id = custom_id; }
  }
  parse_markdown_links (&mut document);
  if format == DocumentFormat::Markdown {
    parse_markdown_footnotes (&mut document);
    warn_markdown_wiki_links (&mut document);
  }
  if format == DocumentFormat::Org { parse_org_links (&mut document); }
  set_body_ranges (&mut document);
  document
}

fn parse_markdown_footnotes (
  document : &mut ParsedDocument,
) {
  let mut protected : Vec<Range<usize>> = document . literal_ranges . clone ();
  for (event, range) in Parser::new_ext (&document . text, markdown_options ())
    . into_offset_iter () {
    match event {
      Event::Code (_) => protected . push (range),
      Event::Start (Tag::FootnoteDefinition (name)) => {
        document . footnote_definitions . push (ParsedFootnoteDefinition {
          name : name . to_string (), range, ambiguous : false }); }
      Event::FootnoteReference (name) => {
        document . footnote_references . push (ParsedFootnoteReference {
          name : name . to_string (), range }); }
      _ => (), } }
  scan_footnote_references (&document . text, &protected,
    &mut document . footnote_references);
  let mut definition_counts : std::collections::HashMap<String, usize> =
    std::collections::HashMap::new ();
  for (offset, line) in lines_with_offsets (&document . text) {
    let indentation : usize = line . bytes () . take_while (|byte| *byte == b' ') . count ();
    if indentation > 3 { continue; }
    let candidate : &str = &line [indentation..];
    if ! candidate . starts_with ("[^") { continue; }
    let Some (close) = candidate . find ("]:") else { continue; };
    if document . literal_ranges . iter () . any (|literal|
      literal . start <= offset && offset < literal . end) { continue; }
    let name : &str = &candidate [2..close];
    if ! name . is_empty () {
      *definition_counts . entry (name . to_string ()) . or_default () += 1; } }
  for definition in &mut document . footnote_definitions {
    if definition_counts . get (&definition . name) . copied () . unwrap_or (0) > 1 {
      definition . ambiguous = true;
      document . diagnostics . push (Diagnostic {
        range : definition . range . clone (),
        message : format! ("Duplicate footnote definition {:?}; left in place",
          definition . name), }); } }
  for reference in &document . footnote_references {
    match definition_counts . get (&reference . name) . copied () . unwrap_or (0) {
      0 => document . diagnostics . push (Diagnostic {
        range : reference . range . clone (),
        message : format! ("Missing footnote definition {:?}", reference . name), }),
      count if count > 1 => document . diagnostics . push (Diagnostic {
        range : reference . range . clone (),
        message : format! ("Ambiguous footnote definition {:?}", reference . name), }),
      _ => (), } }
}

fn scan_footnote_references (
  text : &str,
  protected : &[Range<usize>],
  references : &mut Vec<ParsedFootnoteReference>,
) {
  let mut cursor : usize = 0;
  while let Some (relative) = text [cursor..] . find ("[^") {
    let start : usize = cursor + relative;
    let Some (closing) = text [start + 2..] . find (']') else { break; };
    let end : usize = start + 2 + closing + 1;
    cursor = end;
    if text [end..] . starts_with (':') ||
      protected . iter () . any (|span| ranges_overlap (&(start..end), span)) ||
      references . iter () . any (|reference| reference . range == (start..end)) {
      continue; }
    let escaped : bool = text [..start] . bytes () . rev ()
      .take_while (|byte| *byte == b'\\') . count () % 2 == 1;
    if escaped { continue; }
    let name : &str = &text [start + 2..end - 1];
    if ! name . is_empty () {
      references . push (ParsedFootnoteReference {
        name : name . to_string (), range : start..end }); } }
}

fn markdown_options () -> Options {
  let mut options : Options = Options::empty ();
  options . insert (Options::ENABLE_YAML_STYLE_METADATA_BLOCKS);
  options . insert (Options::ENABLE_FOOTNOTES);
  options
}

fn parse_markdown_links (
  document : &mut ParsedDocument,
) {
  for (event, range) in Parser::new_ext (&document . text, markdown_options ())
    . into_offset_iter () {
    let Event::Start (Tag::Link { dest_url, link_type, id, .. }) = event
      else { continue; };
    if document . literal_ranges . iter () . any (|literal|
      ranges_overlap (&range, literal)) { continue; }
    let syntax : &str = &document . text [range . clone ()];
    if syntax . starts_with ("[[") { continue; }
    let label : String = markdown_link_label (syntax)
      . unwrap_or_else (|| dest_url . to_string ());
    document . links . push (ParsedLink {
      range,
      destination : dest_url . to_string (),
      label,
      syntax : LinkSyntax::Markdown,
      reference_id : if matches! (link_type,
        LinkType::Reference | LinkType::Collapsed | LinkType::Shortcut) {
        Some (id . to_string ()) } else { None },
    }); }
}

fn markdown_link_label (
  syntax : &str,
) -> Option<String> {
  if ! syntax . starts_with ('[') { return None; }
  let mut escaped : bool = false;
  for (index, ch) in syntax . char_indices () . skip (1) {
    if escaped { escaped = false; continue; }
    if ch == '\\' { escaped = true; continue; }
    if ch == ']' { return Some (syntax [1..index] . to_string ()); } }
  None
}

fn parse_org_links (
  document : &mut ParsedDocument,
) {
  let text : &str = &document . text;
  let mut cursor : usize = 0;
  while let Some (relative_start) = text [cursor..] . find ("[[") {
    let start : usize = cursor + relative_start;
    let Some (relative_end) = text [start + 2..] . find ("]]" ) else { break; };
    let end : usize = start + 2 + relative_end + 2;
    cursor = end;
    if document . literal_ranges . iter () . any (|literal|
      ranges_overlap (&(start..end), literal)) { continue; }
    let inner : &str = &text [start + 2..end - 2];
    let (destination, label) : (&str, &str) =
      inner . split_once ("][") . unwrap_or ((inner, inner));
    if destination . is_empty () { continue; }
    document . links . retain (|link| ! ranges_overlap (&link . range, &(start..end)));
    document . links . push (ParsedLink {
      range : start..end,
      destination : destination . to_string (),
      label : label . to_string (),
      syntax : LinkSyntax::Org,
      reference_id : None,
    }); }
  document . links . sort_by_key (|link| link . range . start);
}

fn parse_org_inline_literals (
  document : &mut ParsedDocument,
) {
  let text : &str = &document . text;
  let mut spans : Vec<Range<usize>> = Vec::new ();
  for (offset, line) in lines_with_offsets (text) {
    if document . literal_ranges . iter () . any (|range|
      range . start <= offset && offset < range . end) { continue; }
    let mut cursor : usize = 0;
    while cursor < line . len () {
      let Some (ch) = line [cursor..] . chars () . next () else { break; };
      if ch != '~' && ch != '=' && ch != '`' {
        cursor += ch . len_utf8 ();
        continue; }
      let delimiter : usize = ch . len_utf8 ();
      let after : usize = cursor + delimiter;
      if line [after..] . starts_with (char::is_whitespace) {
        cursor = after; continue; }
      let Some (closing) = line [after..] . find (ch) else {
        cursor = after; continue; };
      let end : usize = after + closing + delimiter;
      if closing > 0 {
        spans . push (offset + cursor..offset + end);
        cursor = end;
      } else { cursor = after; } } }
  document . literal_ranges . extend (spans);
}

fn warn_markdown_wiki_links (
  document : &mut ParsedDocument,
) {
  let mut protected : Vec<Range<usize>> = document . literal_ranges . clone ();
  for (event, range) in Parser::new_ext (&document . text, markdown_options ())
    . into_offset_iter () {
    if matches! (event, Event::Code (_)) { protected . push (range); } }
  let mut cursor : usize = 0;
  while let Some (relative) = document . text [cursor..] . find ("[[") {
    let start : usize = cursor + relative;
    let Some (closing) = document . text [start + 2..] . find ("]]" )
      else { break; };
    let end : usize = start + 2 + closing + 2;
    cursor = end;
    if protected . iter () . any (|range| ranges_overlap (&(start..end), range)) {
      continue; }
    document . diagnostics . push (Diagnostic {
      range : start..end,
      message : "Wiki/Org-style link syntax in Markdown is unsupported; left as text"
        . to_string (), }); }
}

fn ranges_overlap (
  left : &Range<usize>,
  right : &Range<usize>,
) -> bool {
  left . start < right . end && right . start < left . end
}

/// Apply non-overlapping edits using original byte positions. The source
/// remains intact for address resolution and change evidence.
pub fn rendered_range (
  document : &ParsedDocument,
  range : Range<usize>,
) -> String {
  let mut out : String = String::new ();
  let mut cursor : usize = range . start;
  for edit in document . source_edits . iter () . filter (|edit|
    edit . range . start >= range . start && edit . range . end <= range . end) {
    assert! (edit . range . start >= cursor, "overlapping source edits");
    out . push_str (&document . text [cursor..edit . range . start]);
    out . push_str (&edit . replacement);
    cursor = edit . range . end; }
  out . push_str (&document . text [cursor..range . end]);
  out
}

fn parse_markdown_front_matter (
  document : &mut ParsedDocument,
) -> (Option<String>, Vec<String>) {
  let text : &str = &document . text;
  if ! text . starts_with ("---\n") { return (None, Vec::new ()); }
  let Some (end) : Option<usize> = text [4..] . find ("\n---")
    . map (|offset| offset + 4) else {
      document . diagnostics . push (Diagnostic {
        range : 0..text . len () . min (3),
        message : "Unclosed YAML front matter" . to_string (), });
      return (None, Vec::new ()); };
  let yaml : &str = &text [4..end];
  let parsed : Value = match serde_yaml::from_str (yaml) {
    Ok (value) => value,
    Err (error) => {
      document . diagnostics . push (Diagnostic {
        range : 0..end + 4,
        message : format! ("Malformed YAML front matter: {}", error), });
      return (None, Vec::new ()); } };
  let Some (map) = parsed . as_mapping () else {
    document . diagnostics . push (Diagnostic {
      range : 0..end + 4,
      message : "YAML front matter must be a mapping" . to_string (), });
    return (None, Vec::new ()); };
  let title : Option<String> = match map . get ("title") {
    None => None,
    Some (Value::String (value)) => Some (value . clone ()),
    Some (_) => {
      document . diagnostics . push (Diagnostic {
        range : 0..end + 4,
        message : "YAML title must be a string" . to_string (), });
      None } };
  let aliases : Vec<String> = match map . get ("aliases") {
    None => Vec::new (),
    Some (Value::Sequence (items)) if
      items . iter () . all (|item| matches! (item, Value::String (_))) =>
      items . iter () . filter_map (|item| item . as_str () . map (str::to_string))
        . collect (),
    Some (_) => {
      document . diagnostics . push (Diagnostic {
        range : 0..end + 4,
        message : "YAML aliases must be a list of strings" . to_string (), });
      Vec::new () } };
  (title, aliases)
}

fn parse_markdown_headings (
  document : &mut ParsedDocument,
) {
  let mut current : Option<(usize, usize)> = None;
  for (event, range) in
    Parser::new_ext (&document . text, markdown_options ()) . into_offset_iter () {
    match event {
      Event::Start (Tag::Heading { level, .. }) => {
        current = Some ((level as usize, range . start)); }
      Event::Start (Tag::CodeBlock (_)) => {
        document . literal_ranges . push (range); }
      Event::End (TagEnd::Heading (_)) => {
        if let Some ((level, start)) = current . take () {
          let end : usize = range . end;
          let title : String = markdown_heading_title (
            &document . text [start..end]);
          document . sections . push (ParsedSection {
            level,
            title,
            heading : start..end,
            body : end..end,
            explicit_id : None,
            custom_id : None,
            aliases : Vec::new (),
          }); } }
      Event::HardBreak => {
        document . source_edits . push (SourceEdit {
          range,
          replacement : "\\\\\n" . to_string (),
        }); }
      _ => () } }
}

fn markdown_heading_title (
  heading : &str,
) -> String {
  let first_line : &str = heading . lines () . next () . unwrap_or ("");
  let trimmed_start : &str = first_line . trim_start ();
  let hashes : usize = trimmed_start . bytes () . take_while (|b| *b == b'#') . count ();
  if hashes > 0 && hashes <= 6 &&
    trimmed_start [hashes..] . starts_with (char::is_whitespace) {
    trimmed_start [hashes..] . trim ()
      . trim_end_matches ('#') . trim_end () . to_string ()
  } else { first_line . trim () . to_string () }
}

fn parse_org_file_metadata (
  document : &ParsedDocument,
) -> (Option<String>, Vec<String>, Option<String>, Option<String>) {
  let lines : Vec<(usize, &str)> = lines_with_offsets (&document . text);
  let mut title : Option<String> = None;
  let mut aliases : Vec<String> = Vec::new ();
  let mut id : Option<String> = None;
  let mut custom_id : Option<String> = None;
  let first_heading : usize = document . sections . get (1)
    . map (|section| section . heading . start)
    . unwrap_or (document . text . len ());
  for (index, (offset, line)) in lines . iter () . enumerate () {
    if *offset >= first_heading { break; }
    if document . literal_ranges . iter () . any (|range|
      range . start <= *offset && *offset < range . end) { continue; }
    let trimmed : &str = line . trim ();
    if trimmed . to_ascii_lowercase () . starts_with ("#+title:") {
      title = Some (trimmed [8..] . trim () . to_string ()); }
    if trimmed . eq_ignore_ascii_case (":PROPERTIES:") &&
      lines [..index] . iter () . all (|(_, earlier)| {
        let earlier : &str = earlier . trim ();
        earlier . is_empty () || earlier . starts_with ("#+") }) {
      let metadata : (Option<String>, Option<String>, Vec<String>) =
        org_drawer_metadata (&lines, index);
      id = metadata . 0;
      custom_id = metadata . 1;
      aliases = metadata . 2; } }
  (title, aliases, id, custom_id)
}

fn parse_org_headings (
  document : &mut ParsedDocument,
) {
  let lines : Vec<(usize, &str)> = lines_with_offsets (&document . text);
  let mut literal_end : Option<&str> = None;
  let mut literal_start : Option<usize> = None;
  let mut fence : Option<(char, usize)> = None;
  for (index, (start, line)) in lines . iter () . enumerate () {
    let trimmed : &str = line . trim_start ();
    if let Some (end_directive) = literal_end {
      if trimmed . to_ascii_lowercase () . starts_with (end_directive) {
        document . literal_ranges . push (
          literal_start . take () . unwrap ()..*start + line . len ());
        literal_end = None; }
      continue; }
    if let Some ((delimiter, minimum)) = fence {
      if markdown_fence (trimmed) . is_some_and (|(found, width)|
        found == delimiter && width >= minimum) {
        document . literal_ranges . push (
          literal_start . take () . unwrap ()..*start + line . len ());
        fence = None; }
      continue; }
    if trimmed . to_ascii_lowercase () . starts_with ("#+begin_src") {
      literal_end = Some ("#+end_src"); literal_start = Some (*start); continue; }
    if trimmed . to_ascii_lowercase () . starts_with ("#+begin_example") {
      literal_end = Some ("#+end_example"); literal_start = Some (*start); continue; }
    if let Some (marker) = markdown_fence (trimmed) {
      fence = Some (marker); literal_start = Some (*start); continue; }
    let Some ((level, title)) : Option<(usize, String)> =
      org_heading (line) else { continue; };
    let line_end : usize = *start + line . len ();
    let (id, custom_id, aliases) :
      (Option<String>, Option<String>, Vec<String>) =
      if lines . get (index + 1) . is_some_and (|(_, next)|
        next . trim () . eq_ignore_ascii_case (":PROPERTIES:")) {
        org_drawer_metadata (&lines, index + 1)
      } else { (None, None, Vec::new ()) };
    document . sections . push (ParsedSection {
      level,
      title,
      heading : *start..line_end,
      body : line_end..line_end,
      explicit_id : id,
      custom_id,
      aliases,
    }); }
  if literal_end . is_some () || fence . is_some () {
    document . literal_ranges . push (
      literal_start . unwrap_or (document . text . len ())..document . text . len ());
    document . diagnostics . push (Diagnostic {
      range : document . text . len ()..document . text . len (),
      message : "Unclosed literal block or Markdown fence" . to_string (), }); }
}

fn org_heading (
  line : &str,
) -> Option<(usize, String)> {
  let level : usize = line . bytes () . take_while (|byte| *byte == b'*') . count ();
  if level == 0 || ! line [level..] . starts_with (' ') { return None; }
  Some ((level, line [level + 1..] . trim_end () . to_string ()))
}

fn markdown_fence (
  line : &str,
) -> Option<(char, usize)> {
  let indent : usize = line . len () - line . trim_start_matches (' ') . len ();
  if indent > 3 { return None; }
  let line : &str = &line [indent..];
  let delimiter : char = line . chars () . next ()?;
  if delimiter != '`' && delimiter != '~' { return None; }
  let width : usize = line . chars () . take_while (|ch| *ch == delimiter) . count ();
  if width < 3 { None } else { Some ((delimiter, width)) }
}

fn org_drawer_metadata (
  lines : &[(usize, &str)],
  start : usize,
) -> (Option<String>, Option<String>, Vec<String>) {
  let mut id : Option<String> = None;
  let mut custom_id : Option<String> = None;
  let mut aliases : Vec<String> = Vec::new ();
  for (_, line) in lines . iter () . skip (start + 1) {
    let trimmed : &str = line . trim ();
    if trimmed . eq_ignore_ascii_case (":END:") { break; }
    if let Some (value) = org_property (trimmed, ":ID:") {
      id = Some (value . trim () . to_string ()); }
    if let Some (value) = org_property (trimmed, ":CUSTOM_ID:") {
      custom_id = Some (value . trim () . to_string ()); }
    if let Some (value) = org_property (trimmed, ":ROAM_ALIASES:") {
      aliases = crate::import_org_roam::parse::parse_roam_aliases (value . trim ()); } }
  (id, custom_id, aliases)
}

fn org_property <'a> (
  line : &'a str,
  name : &str,
) -> Option<&'a str> {
  line . get (..name . len ())
    . filter (|prefix| prefix . eq_ignore_ascii_case (name))
    . map (|_| &line [name . len ()..])
}

fn lines_with_offsets (
  text : &str,
) -> Vec<(usize, &str)> {
  let mut offset : usize = 0;
  text . split_inclusive ('\n') . map (|line| {
    let start : usize = offset;
    offset += line . len ();
    (start, line . trim_end_matches ('\n') . trim_end_matches ('\r'))
  }) . collect ()
}

fn set_body_ranges (
  document : &mut ParsedDocument,
) {
  let length : usize = document . text . len ();
  for index in 0..document . sections . len () {
    let start : usize = document . sections [index] . heading . end;
    let end : usize = document . sections . get (index + 1)
      . map (|section| section . heading . start)
      . unwrap_or (length);
    document . sections [index] . body = start..end; }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn markdown_structure_preserves_original_spans () {
    let text : String = "---\ntitle: Graph\naliases: [energy, \"machine learning\"]\n---\nPreface\n# Graph\nbody\n```org\n# fake\n```\nSecond\n======\n" . to_string ();
    let document : ParsedDocument = parse_document (Path::new ("notes.md"), text . clone ());
    assert_eq! (document . sections . len (), 3);
    assert_eq! (document . sections [0] . title, "Graph");
    assert_eq! (document . sections [0] . aliases,
      vec!["energy", "machine learning"]);
    assert_eq! (document . sections [1] . title, "Graph");
    assert_eq! (document . sections [2] . title, "Second");
    assert! (text [document . sections [1] . body . clone ()]
      . contains ("# fake"));
    assert! (text [document . sections [0] . body . clone ()]
      . contains ("Preface"));
  }

  #[test]
  fn org_structure_ignores_literal_headings_and_reads_real_drawers () {
    let text : String = "#+title: Space\n:PROPERTIES:\n:ID: root\n:END:\nBefore\n* One\n:PROPERTIES:\n:ID: one\n:ROAM_ALIASES: \"first one\" first\n:END:\n#+begin_src org\n* fake\n#+end_src\n```org\n* also fake\n```\n** Two\n" . to_string ();
    let document : ParsedDocument = parse_document (Path::new ("notes.org"), text);
    assert_eq! (document . sections . len (), 3);
    assert_eq! (document . sections [0] . explicit_id . as_deref (), Some ("root"));
    assert_eq! (document . sections [1] . explicit_id . as_deref (), Some ("one"));
    assert_eq! (document . sections [1] . aliases,
      vec!["first one", "first"]);
    assert_eq! (document . sections [2] . title, "Two");
  }

  #[test]
  fn org_metadata_and_links_inside_literal_content_are_not_interpreted () {
    let text : String = "#+begin_src org\n#+title: False\n:PROPERTIES:\n:ID: false\n:END:\n#+end_src\n#+title: True\n* Actual\n~[[file:other.org]]~ =[[id:absent]]= `[[file:no.md]]`\n" . to_string ();
    let document : ParsedDocument = parse_document (Path::new ("note.org"), text);
    assert_eq! (document . sections [0] . title, "True");
    assert_eq! (document . sections [0] . explicit_id, None);
    assert_eq! (document . links . len (), 0);
  }

  #[test]
  fn markdown_link_events_cover_original_syntax () {
    let text : &str = "[inline](other.md#one) [label][ref] [ref]\n\n[ref]: other.md#two\n";
    let events : Vec<(String, String)> = Parser::new (text)
      . into_offset_iter ()
      .filter_map (|(event, range)| match event {
        Event::Start (Tag::Link { .. }) =>
          Some ((format! ("{:?}", event), text [range] . to_string ())),
        _ => None,
      }) . collect ();
    assert_eq! (events . len (), 3);
    assert_eq! (events [0] . 1, "[inline](other.md#one)");
    assert_eq! (events [1] . 1, "[label][ref]");
    assert_eq! (events [2] . 1, "[ref]");
  }

  #[test]
  fn markdown_hard_break_event_identifies_spaces_before_newline () {
    let text : &str = "First line  \nSecond line\n\nCode:\n```\nkeep  \n```\n";
    let breaks : Vec<&str> = Parser::new (text)
      . into_offset_iter ()
      .filter_map (|(event, range)| match event {
        Event::HardBreak => Some (&text [range]),
        _ => None,
      }) . collect ();
    assert_eq! (breaks, vec!["  \n"]);
  }

  #[test]
  fn markdown_footnote_definition_has_source_extent () {
    let text : &str = "Text[^n]\n\n[^n]: first line\n    second line\n\nAfter\n";
    let events : Vec<(String, String)> = Parser::new_ext (
      text, markdown_options ()) . into_offset_iter ()
      .filter_map (|(event, range)| match event {
        Event::Start (Tag::FootnoteDefinition (name)) =>
          Some ((name . to_string (), text [range] . to_string ())),
        Event::FootnoteReference (name) =>
          Some ((name . to_string (), text [range] . to_string ())),
        _ => None,
      }) . collect ();
    assert_eq! (events [0], ("n" . to_string (), "[^n]" . to_string ()));
    assert_eq! (events [1] . 0, "n");
    assert! (events [1] . 1 . starts_with ("[^n]: first line"));
    assert! (events [1] . 1 . contains ("second line"));
  }

  #[test]
  fn markdown_missing_footnote_is_not_silently_resolved () {
    let document : ParsedDocument = parse_document (
      Path::new ("missing.md"), "Missing[^absent]\n" . to_string ());
    assert_eq! (document . footnote_references . len (), 1);
    assert! (document . diagnostics . iter () . any (|diagnostic|
      diagnostic . message . contains ("Missing footnote")));
  }

  #[test]
  fn duplicate_footnote_definitions_stay_ambiguous () {
    let document : ParsedDocument = parse_document (
      Path::new ("duplicate.md"),
      "Ref[^a]\n\n[^a]: first\n\n[^a]: second\n" . to_string ());
    assert! (document . footnote_definitions . iter () . all (|definition|
      definition . ambiguous));
    assert! (document . diagnostics . iter () . any (|diagnostic|
      diagnostic . message . contains ("Ambiguous footnote")));
  }

  #[test]
  fn unsupported_wiki_links_warn_outside_code_only () {
    let document : ParsedDocument = parse_document (
      Path::new ("wiki.md"),
      "[[Page]] `[[Inline]]`\n```\n[[Block]]\n```\n" . to_string ());
    let warnings : Vec<&Diagnostic> = document . diagnostics . iter ()
      .filter (|diagnostic| diagnostic . message . contains ("Wiki"))
      .collect ();
    assert_eq! (warnings . len (), 1);
    assert_eq! (&document . text [warnings [0] . range . clone ()], "[[Page]]");
  }
}
