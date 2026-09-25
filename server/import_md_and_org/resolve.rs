//! Resolve original document addresses after IDs are assigned, before writes.

use super::build::BuiltDocument;
use super::parse::{
  Diagnostic, LinkSyntax, ParsedDocument, ParsedLink,
  SourceEdit, rendered_range,
};
use crate::types::misc::ID;
use crate::types::nodes::complete::normalize_body;
use pulldown_cmark::{Event, Parser};
use std::collections::HashMap;
use std::path::{Component, Path, PathBuf};

#[derive(Default)]
struct AddressIndex {
  files : HashMap<PathBuf, usize>,
  roots : HashMap<usize, ID>,
  markdown_fragments : HashMap<(usize, String), ID>,
  org_headings : HashMap<(usize, String), Vec<ID>>,
  custom_ids : HashMap<(usize, String), Vec<ID>>,
  named_targets : HashMap<(usize, String), Vec<ID>>,
  global_ids : HashMap<String, ID>,
}

enum LinkTarget {
  Node (ID, Option<String>),
  External,
  ExternalFile (String, String),
  Unresolved (String),
}

/// Returns true only for filesystem links whose path is absolute. Call this
/// before asking the client for a host/container mapping.
pub fn contains_absolute_file_link (
  documents : &[ParsedDocument],
) -> bool {
  documents . iter () . any (|document|
    document . links . iter () . any (|link|
      file_part_of_link (link) . is_some_and (|path| {
        let path : String = if link . syntax == LinkSyntax::Markdown {
          percent_decode (path) . unwrap_or_else (|_| path . to_string ())
        } else { org_unescape (path) };
        Path::new (&path) . is_absolute () })))
}

/// Apply internal-link replacements to every original section. Unresolved
/// links remain untouched, with diagnostics at their source byte positions.
pub fn resolve_document_links (
  documents : &mut [ParsedDocument],
  built : &mut [BuiltDocument],
  input_directory : &Path,
  host_root : Option<&Path>,
  existing_ids : &HashMap<String, ID>,
) {
  let index : AddressIndex =
    build_address_index (documents, built, existing_ids);
  for (document_index, document) in documents . iter_mut () . enumerate () {
    let links : Vec<ParsedLink> = document . links . clone ();
    for link in links {
      match resolve_link (
        document_index, &link, document, &index,
        input_directory, host_root) {
        LinkTarget::Node (id, warning) => {
          if let Some (warning) = warning {
            document . diagnostics . push (Diagnostic {
              range : link . range . clone (), message : warning }); }
          if document . source_edits . iter () . any (|edit|
            edit . range . start < link . range . end &&
            link . range . start < edit . range . end) {
            document . diagnostics . push (Diagnostic {
              range : link . range . clone (),
              message : "Link overlaps another source conversion; left unchanged"
                . to_string (), });
            continue; }
          document . source_edits . push (SourceEdit {
            range : link . range . clone (),
            replacement : format! ("[[id:{}][{}]]", id, link . label),
          }); }
        LinkTarget::External => {
          if let Some (reference_id) = &link . reference_id {
            if let Some (definition_offset) =
              markdown_reference_definition_offset (document, reference_id) {
              if section_at_offset (document, definition_offset) !=
                section_at_offset (document, link . range . start) {
                document . diagnostics . push (Diagnostic {
                  range : link . range . clone (),
                  message : format! (
                    "External reference {:?} stays in Markdown syntax, but its definition is in another imported node",
                    reference_id), }); } } } }
        LinkTarget::ExternalFile (replacement, warning) => {
          document . diagnostics . push (Diagnostic {
            range : link . range . clone (), message : warning });
          if ! document . source_edits . iter () . any (|edit|
            edit . range . start < link . range . end &&
            link . range . start < edit . range . end) {
            document . source_edits . push (SourceEdit {
              range : link . range . clone (), replacement }); } }
        LinkTarget::Unresolved (message) => {
          document . diagnostics . push (Diagnostic {
            range : link . range . clone (), message }); } } }
    for reference in &document . footnote_references {
      let Some (node_index) = built [document_index] . footnote_node_indices
        . get (&reference . name) else { continue; };
      if document . source_edits . iter () . any (|edit|
        edit . range . start < reference . range . end &&
        reference . range . start < edit . range . end) {
        document . diagnostics . push (Diagnostic {
          range : reference . range . clone (),
          message : "Footnote reference overlaps another conversion; left unchanged"
            . to_string (), });
        continue; }
      let id : &ID = &built [document_index] . nodes [*node_index] . pid;
      document . source_edits . push (SourceEdit {
        range : reference . range . clone (),
        replacement : format! ("[[id:{}][Footnote {}]]", id, reference . name),
      }); }
    document . source_edits . sort_by_key (|edit| edit . range . start);
    for definition in &document . footnote_definitions {
      let Some (node_index) = built [document_index] . footnote_node_indices
        . get (&definition . name) else { continue; };
      built [document_index] . nodes [*node_index] . body = normalize_body (
        Some (rendered_range (document, definition . range . clone ()))); }
    document . source_edits . retain (|edit|
      ! document . footnote_definitions . iter () . any (|definition|
        ! definition . ambiguous &&
        edit . range . start >= definition . range . start &&
        edit . range . end <= definition . range . end));
    for definition in &document . footnote_definitions {
      if ! definition . ambiguous {
        document . source_edits . push (SourceEdit {
          range : definition . range . clone (), replacement : String::new (),
        }); } }
    document . source_edits . sort_by_key (|edit| edit . range . start);
    for (section_index, section) in document . sections . iter () . enumerate () {
      built [document_index] . nodes [section_index] . body =
        normalize_body (Some (rendered_range (document, section . body . clone ())));
      if section_index > 0 {
        let heading : &str = &document . text [section . heading . clone ()];
        if let Some (relative) = heading . find (&section . title) {
          let start : usize = section . heading . start + relative;
          let end : usize = start + section . title . len ();
          built [document_index] . nodes [section_index] . title =
            rendered_range (document, start..end); } } } }
}

fn section_at_offset (
  document : &ParsedDocument,
  offset : usize,
) -> usize {
  document . sections . iter () . enumerate ()
    .filter (|(_, section)| section . heading . start <= offset)
    .map (|(index, _)| index) . last () . unwrap_or (0)
}

fn markdown_reference_definition_offset (
  document : &ParsedDocument,
  reference_id : &str,
) -> Option<usize> {
  let mut offset : usize = 0;
  for line in document . text . split_inclusive ('\n') {
    let leading : usize = line . bytes ()
      .take_while (|byte| *byte == b' ') . count ();
    if leading <= 3 && ! document . literal_ranges . iter () . any (|range|
      range . start <= offset && offset < range . end) {
      let candidate : &str = &line [leading..];
      if let Some (close) = candidate . find ("]:" ) {
        if candidate . starts_with ('[') &&
          candidate [1..close] . eq_ignore_ascii_case (reference_id) {
          return Some (offset + leading); } } }
    offset += line . len (); }
  None
}

fn build_address_index (
  documents : &[ParsedDocument],
  built : &[BuiltDocument],
  existing_ids : &HashMap<String, ID>,
) -> AddressIndex {
  let mut index : AddressIndex = AddressIndex {
    global_ids : existing_ids . clone (), ..Default::default () };
  for (document_index, document) in documents . iter () . enumerate () {
    index . files . insert (document . path . clone (), document_index);
    index . roots . insert (document_index, built [document_index] . root_id . clone ());
    let mut slug_counts : HashMap<String, usize> = HashMap::new ();
    let mut used_slugs : std::collections::HashSet<String> =
      std::collections::HashSet::new ();
    for (section_index, section) in document . sections . iter () . enumerate () {
      let id : ID = built [document_index] . nodes [section_index] . pid . clone ();
      if let Some (explicit) = &section . explicit_id {
        index . global_ids . insert (explicit . clone (), id . clone ()); }
      if let Some (custom) = &section . custom_id {
        index . custom_ids . entry ((document_index, custom . clone ()))
          . or_default () . push (id . clone ()); }
      if section_index == 0 { continue; }
      index . org_headings . entry ((document_index, section . title . clone ()))
        . or_default () . push (id . clone ());
      let base : String = github_slug (&section . title);
      let count : &mut usize = slug_counts . entry (base . clone ()) . or_default ();
      let mut slug : String = if *count == 0 { base . clone () }
        else { format! ("{}-{}", base, count) };
      while used_slugs . contains (&slug) {
        *count += 1;
        slug = format! ("{}-{}", base, count); }
      *count += 1;
      used_slugs . insert (slug . clone ());
      index . markdown_fragments . insert ((document_index, slug), id); }
    for (name, offset) in named_targets (document) {
      let section_index : usize = document . sections . iter () . enumerate ()
        .filter (|(_, section)| section . heading . start <= offset)
        .map (|(index, _)| index) . last () . unwrap_or (0);
      let id : ID = built [document_index] . nodes [section_index] . pid . clone ();
      index . named_targets . entry ((document_index, name))
        . or_default () . push (id); } }
  index
}

fn github_slug (
  title : &str,
) -> String {
  let mut plain : String = String::new ();
  for event in Parser::new (title) {
    match event {
      Event::Text (text) | Event::Code (text) => plain . push_str (&text),
      Event::SoftBreak | Event::HardBreak => plain . push (' '),
      _ => (), } }
  plain . to_lowercase () . chars ()
    . filter_map (|ch| {
      if ch == ' ' { Some ('-') }
      else if ch == '-' || ch == '_' || ch . is_alphanumeric () { Some (ch) }
      else { None } })
    . collect ()
}

fn named_targets (
  document : &ParsedDocument,
) -> Vec<(String, usize)> {
  let mut targets : Vec<(String, usize)> = Vec::new ();
  let mut cursor : usize = 0;
  while let Some (start_offset) = document . text [cursor..] . find ("<<") {
    let start : usize = cursor + start_offset;
    let Some (end_offset) = document . text [start + 2..] . find (">>") else { break; };
    let end : usize = start + 2 + end_offset + 2;
    cursor = end;
    if document . literal_ranges . iter () . any (|range|
      range . start < end && start < range . end) { continue; }
    let name : &str = &document . text [start + 2..end - 2];
    if ! name . is_empty () { targets . push ((name . to_string (), start)); } }
  targets
}

fn resolve_link (
  document_index : usize,
  link : &ParsedLink,
  document : &ParsedDocument,
  index : &AddressIndex,
  input_directory : &Path,
  host_root : Option<&Path>,
) -> LinkTarget {
  let destination : &str = &link . destination;
  if let Some (id) = destination . strip_prefix ("id:") {
    return index . global_ids . get (id) . cloned ()
      . map (|id| LinkTarget::Node (id, None))
      . unwrap_or_else (|| LinkTarget::Unresolved (
        format! ("ID target {:?} is absent", id))); }
  if destination . starts_with ("http://") ||
    destination . starts_with ("https://") ||
    destination . starts_with ("mailto:") ||
    destination . starts_with ("//") {
    return LinkTarget::External; }
  if destination . contains ("://") ||
    destination . starts_with ("data:") {
    return LinkTarget::External; }
  let target : &str = destination . strip_prefix ("file:")
    . unwrap_or (destination);
  let (path, search) : (&str, Option<&str>) = match link . syntax {
    LinkSyntax::Markdown => {
      let (path, fragment) = target . split_once ('#')
        . map (|(path, fragment)| (path, Some (fragment)))
        . unwrap_or ((target, None));
      (path, fragment) }
    LinkSyntax::Org => {
      let (path, search) = target . split_once ("::")
        . map (|(path, search)| (path, Some (search)))
        . unwrap_or ((target, None));
      (path, search) } };
  let path : String = match link . syntax {
    LinkSyntax::Markdown => match percent_decode (path) {
      Ok (decoded) => decoded,
      Err (error) => return LinkTarget::Unresolved (error), },
    LinkSyntax::Org => org_unescape (path), };
  if path . starts_with ('~') { return LinkTarget::Unresolved (
    "Home-relative paths are unsupported; use a relative path or the configured host root"
      . to_string ()); }
  let target_document : usize = if path . is_empty () {
    document_index
  } else {
    let Some (relative) = normalized_input_path (
      &document . path, &path, input_directory, host_root) else {
      return LinkTarget::Unresolved (format! (
        "Path {:?} is outside the imported input tree or host mapping", path)); };
    let Some (target) = index . files . get (&relative) else {
      let attachment : PathBuf = input_directory . join (&relative);
      if attachment . is_file () &&
        ! matches! (relative . extension () . and_then (|ext| ext . to_str ()),
          Some ("md" | "org")) && search . is_none () {
        let usable_path : String = host_root
          . map (|host| host . join (&relative) . display () . to_string ())
          .unwrap_or_else (|| path . clone ());
        let warning : String = if host_root . is_some () {
          format! ("Attachment {:?} remains at its original host path; it is not copied",
            path)
        } else {
          format! ("Attachment {:?} is not copied; its file link may be unusable from a different export directory or host",
            path) };
        return LinkTarget::ExternalFile (
          format! ("[[file:{}][{}]]", usable_path, link . label), warning); }
      return LinkTarget::Unresolved (format! (
        "Path {:?} does not name an imported document or readable attachment", path)); };
    *target };
  let root : ID = index . roots . get (&target_document) . unwrap () . clone ();
  if search . is_none () || search == Some ("") {
    return LinkTarget::Node (root, None); }
  let search : &str = search . unwrap ();
  let key : String = if link . syntax == LinkSyntax::Markdown {
    match percent_decode (search) {
      Ok (decoded) => decoded,
      Err (error) => return LinkTarget::Unresolved (error), }
  } else { org_unescape (search) };
  if link . syntax == LinkSyntax::Org &&
    (key . starts_with ('/') || key . chars () . all (|ch| ch . is_ascii_digit ())) {
    return LinkTarget::Unresolved (format! (
      "Org regexp/line search {:?} is unsupported; left unchanged", key)); }
  let candidate : Option<&ID> = match link . syntax {
    LinkSyntax::Markdown =>
      index . markdown_fragments . get (&(target_document, key . clone ())),
    LinkSyntax::Org if key . starts_with ('*') =>
      unique_address (&index . org_headings, target_document, &key [1..]),
    LinkSyntax::Org if key . starts_with ('#') =>
      unique_address (&index . custom_ids, target_document, &key [1..]),
    LinkSyntax::Org =>
      unique_address (&index . named_targets, target_document, &key), };
  match candidate {
    Some (id) => LinkTarget::Node (id . clone (),
      if link . syntax == LinkSyntax::Org && ! key . starts_with ('*') &&
        ! key . starts_with ('#') {
        Some (format! (
          "Named target {:?} resolves to its containing node; position is lost",
          key))
      } else { None }),
    None => LinkTarget::Unresolved (format! (
      "Unresolved or ambiguous address {:?} in {:?}; HTML anchors are not interpreted",
      key, path)), }
}

fn unique_address <'a> (
  addresses : &'a HashMap<(usize, String), Vec<ID>>,
  document_index : usize,
  key : &str,
) -> Option<&'a ID> {
  let ids : &Vec<ID> = addresses . get (&(document_index, key . to_string ()))?;
  if ids . len () == 1 { ids . first () } else { None }
}

fn normalized_input_path (
  containing_document : &Path,
  path : &str,
  input_directory : &Path,
  host_root : Option<&Path>,
) -> Option<PathBuf> {
  let candidate : &Path = Path::new (path);
  let relative : PathBuf = if candidate . is_absolute () {
    if let Ok (inside) = candidate . strip_prefix (input_directory) {
      inside . to_path_buf ()
    } else {
      candidate . strip_prefix (host_root?) . ok ()? . to_path_buf () }
  } else {
    containing_document . parent () . unwrap_or (Path::new ("")) . join (candidate) };
  let mut components : Vec<std::ffi::OsString> = Vec::new ();
  for component in relative . components () {
    match component {
      Component::CurDir => (),
      Component::ParentDir => { components . pop ()?; },
      Component::Normal (part) => components . push (part . to_os_string ()),
      _ => return None, } }
  Some (components . iter () . collect ())
}

fn percent_decode (
  input : &str,
) -> Result<String, String> {
  let bytes : &[u8] = input . as_bytes ();
  let mut out : Vec<u8> = Vec::new ();
  let mut index : usize = 0;
  while index < bytes . len () {
    if bytes [index] == b'%' {
      if index + 2 >= bytes . len () { return Err (
        format! ("Incomplete percent escape in {:?}", input)); }
      let high : u8 = hex_value (bytes [index + 1]) . ok_or_else (||
        format! ("Invalid percent escape in {:?}", input))?;
      let low : u8 = hex_value (bytes [index + 2]) . ok_or_else (||
        format! ("Invalid percent escape in {:?}", input))?;
      out . push (high * 16 + low);
      index += 3;
    } else { out . push (bytes [index]); index += 1; } }
  String::from_utf8 (out) . map_err (|_| format! (
    "Percent escapes in {:?} are not UTF-8", input))
}

fn hex_value (byte : u8) -> Option<u8> {
  match byte {
    b'0'..=b'9' => Some (byte - b'0'),
    b'a'..=b'f' => Some (byte - b'a' + 10),
    b'A'..=b'F' => Some (byte - b'A' + 10),
    _ => None, }
}

fn org_unescape (
  input : &str,
) -> String {
  let mut out : String = String::new ();
  let mut chars = input . chars ();
  while let Some (ch) = chars . next () {
    if ch == '\\' {
      match chars . next () {
        Some (next) => out . push (next),
        None => out . push (ch), }
    } else { out . push (ch); } }
  out
}

fn file_part_of_link (
  link : &ParsedLink,
) -> Option<&str> {
  let destination : &str = &link . destination;
  if destination . starts_with ("id:") ||
    destination . starts_with ("http://") ||
    destination . starts_with ("https://") ||
    destination . starts_with ("mailto:") ||
    destination . starts_with ("//") ||
    destination . contains ("://") { return None; }
  let target : &str = destination . strip_prefix ("file:")
    . unwrap_or (destination);
  let path : &str = match link . syntax {
    LinkSyntax::Markdown => target . split_once ('#') . map (|pair| pair . 0)
      . unwrap_or (target),
    LinkSyntax::Org => target . split_once ("::") . map (|pair| pair . 0)
      . unwrap_or (target), };
  Some (path)
}

#[cfg(test)]
mod tests {
  use super::*;
  use super::super::build::build_document;
  use super::super::parse::parse_document;
  use crate::types::misc::SourceName;
  use std::fs;

  #[test]
  fn resolves_cross_format_paths_slugs_org_searches_and_reference_links () {
    let mut documents : Vec<ParsedDocument> = vec![
      parse_document (Path::new ("notes/a.md"),
        "# Same\n# Same\n[org](../b.org#section) [later][ref] [site][web]\n\n[ref]: ../b.org\n[web]: https://example.org\n" . to_string ()),
      parse_document (Path::new ("b.org"),
        "* Section\n[[file:notes/a.md::*Same][ambiguous]]\n[[file:notes/a.md][root]]\n"
          . to_string ()),
    ];
    let source : SourceName = SourceName::from ("owned");
    let mut counter : usize = 0;
    let mut next = || { counter += 1; ID::new (&format! ("generated-{}", counter)) };
    let mut built : Vec<BuiltDocument> = documents . iter ()
      . map (|doc| build_document (doc, &source, &mut next) . unwrap ())
      . collect ();
    resolve_document_links (&mut documents, &mut built,
      Path::new ("/container/import"), None, &HashMap::new ());
    let a_second_body : &str = built [0] . nodes [2] . body . as_deref () . unwrap ();
    assert! (a_second_body . contains (&format! (
      "[[id:{}][org]]", built [1] . nodes [1] . pid)));
    assert! (a_second_body . contains (&format! (
      "[[id:{}][later]]", built [1] . root_id)));
    assert! (a_second_body . contains ("[site][web]"));
    let b_body : &str = built [1] . nodes [1] . body . as_deref () . unwrap ();
    assert! (b_body . contains (&format! (
      "[[id:{}][root]]", built [0] . root_id)));
    assert! (b_body . contains ("[[file:notes/a.md::*Same][ambiguous]]"));
    assert! (documents [1] . diagnostics . iter () . any (|warning|
      warning . message . contains ("ambiguous")));
  }

  #[test]
  fn maps_absolute_paths_by_whole_components () {
    let original : PathBuf = normalized_input_path (
      Path::new ("folder/source.md"), "/host/notes/folder/other.org",
      Path::new ("/container/notes"), Some (Path::new ("/host/notes")))
      .unwrap ();
    assert_eq! (original, Path::new ("folder/other.org"));
    assert! (normalized_input_path (
      Path::new ("folder/source.md"), "/host/notes-old/other.org",
      Path::new ("/container/notes"), Some (Path::new ("/host/notes")))
      .is_none ());
  }

  #[test]
  fn duplicate_markdown_slugs_do_not_steal_an_explicit_suffix () {
    let mut documents : Vec<ParsedDocument> = vec![parse_document (
      Path::new ("notes.md"),
      "# A\n# A-1\n# A\n[third](#a-2)\n" . to_string ())];
    let source : SourceName = SourceName::from ("owned");
    let mut counter : usize = 0;
    let mut next = || { counter += 1; ID::new (&format! ("generated-{}", counter)) };
    let mut built : Vec<BuiltDocument> = documents . iter ()
      .map (|document| build_document (document, &source, &mut next) . unwrap ())
      .collect ();
    let expected : ID = built [0] . nodes [3] . pid . clone ();
    resolve_document_links (&mut documents, &mut built,
      Path::new ("/input"), None, &HashMap::new ());
    assert! (built [0] . nodes [3] . body . as_deref () . unwrap ()
      . contains (&format! ("[[id:{}][third]]", expected)));
  }

  #[test]
  fn relocates_named_footnotes_and_rewrites_repeated_references () {
    let mut documents : Vec<ParsedDocument> = vec![parse_document (
      Path::new ("notes.md"),
      "# Topic\nOne[^a] and again[^a].\n\n[^a]: See [other](other.org).\n    More detail.\n"
        .to_string ())];
    let source : SourceName = SourceName::from ("owned");
    let mut counter : usize = 0;
    let mut next = || { counter += 1; ID::new (&format! ("generated-{}", counter)) };
    let mut built : Vec<BuiltDocument> = documents . iter ()
      .map (|doc| build_document (doc, &source, &mut next) . unwrap ())
      .collect ();
    let footnote_index : usize = built [0] . footnote_node_indices ["a"];
    let footnote_id : ID = built [0] . nodes [footnote_index] . pid . clone ();
    resolve_document_links (&mut documents, &mut built,
      Path::new ("/container/import"), None, &HashMap::new ());
    let body : &str = built [0] . nodes [1] . body . as_deref () . unwrap ();
    assert_eq! (body . matches (&format! ("[[id:{}][Footnote a]]", footnote_id))
      .count (), 2);
    assert! (! body . contains ("[^a]:"));
    assert! (built [0] . nodes [footnote_index] . body . as_deref () . unwrap ()
      .contains ("More detail."));
    assert! (built [0] . nodes [0] . contains . last () . is_some_and (|child|
      built [0] . nodes . iter () . any (|node|
        node . pid == child . member && node . title == "Footnotes")));
  }

  #[test]
  fn attachment_links_use_host_path_when_available_and_warn_without_copying () {
    let temp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
    fs::create_dir (temp . path () . join ("assets")) . unwrap ();
    fs::write (temp . path () . join ("assets/chart.png"), "data") . unwrap ();
    let mut documents : Vec<ParsedDocument> = vec![parse_document (
      Path::new ("readme.md"),
      "See [chart](assets/chart.png)." . to_string ())];
    let source : SourceName = SourceName::from ("owned");
    let mut next = || ID::new (&uuid::Uuid::new_v4 () . to_string ());
    let mut built : Vec<BuiltDocument> = documents . iter ()
      .map (|document| build_document (document, &source, &mut next) . unwrap ())
      .collect ();
    resolve_document_links (&mut documents, &mut built,
      temp . path (), Some (Path::new ("/host/notes")), &HashMap::new ());
    assert! (built [0] . nodes [0] . body . as_deref () . unwrap ()
      . contains ("[[file:/host/notes/assets/chart.png][chart]]"));
    assert! (documents [0] . diagnostics . iter () . any (|diagnostic|
      diagnostic . message . contains ("not copied")));
  }

  #[test]
  fn external_reference_separated_from_definition_warns_without_rewriting () {
    let mut documents : Vec<ParsedDocument> = vec![parse_document (
      Path::new ("refs.md"),
      "# Link\n[site][web]\n# Definitions\n[web]: https://example.org\n"
        .to_string ())];
    let source : SourceName = SourceName::from ("owned");
    let mut next = || ID::new (&uuid::Uuid::new_v4 () . to_string ());
    let mut built : Vec<BuiltDocument> = documents . iter ()
      .map (|document| build_document (document, &source, &mut next) . unwrap ())
      .collect ();
    resolve_document_links (&mut documents, &mut built,
      Path::new ("/input"), None, &HashMap::new ());
    assert! (built [0] . nodes [1] . body . as_deref () . unwrap ()
      . contains ("[site][web]"));
    assert! (documents [0] . diagnostics . iter () . any (|diagnostic|
      diagnostic . message . contains ("definition is in another")));
  }
}
