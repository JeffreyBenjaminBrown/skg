pub mod parse;
pub mod build;
pub mod resolve;
pub mod publish;
pub mod batch;

use parse::{ParsedDocument, parse_document};
use std::fs;
use std::path::{Path, PathBuf};
use walkdir::{DirEntry, WalkDir};

/// Read the exact import set before assigning any IDs. Every discovered file
/// contributes a document, including files with no text or headings.
pub fn discover_documents (
  input_directory : &Path,
) -> Result<Vec<ParsedDocument>, String> {
  let metadata : fs::Metadata = fs::symlink_metadata (input_directory)
    . map_err (|error| format! ("{}: {}", input_directory . display (), error))?;
  if ! metadata . is_dir () || metadata . file_type () . is_symlink () {
    return Err (format! ("{} is not a real directory",
      input_directory . display ())); }
  let mut paths : Vec<PathBuf> = Vec::new ();
  for entry in WalkDir::new (input_directory)
    . follow_links (false)
    . into_iter ()
    . filter_entry (should_visit) {
    let entry : DirEntry = entry . map_err (|error| error . to_string ())?;
    if entry . file_type () . is_symlink () { continue; }
    if entry . file_type () . is_file () &&
      matches! (entry . path () . extension () . and_then (|s| s . to_str ()),
        Some ("md" | "org")) {
      paths . push (entry . into_path ()); } }
  paths . sort_by (|left, right| {
    left . strip_prefix (input_directory) . unwrap ()
      . cmp (right . strip_prefix (input_directory) . unwrap ()) });
  paths . into_iter () . map (|path| {
    let text : String = fs::read_to_string (&path)
      . map_err (|error| format! ("{}: {}", path . display (), error))?;
    let relative : &Path = path . strip_prefix (input_directory)
      . map_err (|error| error . to_string ())?;
    Ok (parse_document (relative, text))
  }) . collect ()
}

fn should_visit (
  entry : &DirEntry,
) -> bool {
  entry . depth () == 0 ||
    (entry . file_name () != ".git" && ! entry . file_type () . is_symlink ())
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::misc::{ID, SourceName};
  use std::collections::HashMap;

  #[test]
  fn discovery_orders_paths_and_includes_empty_documents () {
    let directory : tempfile::TempDir = tempfile::tempdir () . unwrap ();
    fs::create_dir (directory . path () . join ("nested")) . unwrap ();
    fs::create_dir (directory . path () . join (".git")) . unwrap ();
    fs::write (directory . path () . join ("nested/b.md"), "") . unwrap ();
    fs::write (directory . path () . join ("a.org"), "* Heading\n") . unwrap ();
    fs::write (directory . path () . join (".git/hidden.md"), "hidden") . unwrap ();
    let documents : Vec<ParsedDocument> = discover_documents (directory . path ()) . unwrap ();
    assert_eq! (documents . iter () . map (|doc| doc . path . as_path ())
      . collect::<Vec<_>> (), vec![Path::new ("a.org"), Path::new ("nested/b.md")]);
    assert_eq! (documents [1] . sections . len (), 1);
    assert_eq! (documents [1] . sections [0] . title, "b");
  }

  #[test]
  fn repository_document_samples_keep_mixed_syntax_and_warn_on_stale_paths () {
    let root : &Path = Path::new (env! ("CARGO_MANIFEST_DIR"));
    let names : [&str; 4] = [
      "docs/setup.org", "docs/README-old.org",
      "docs/data-model_friendly.org",
      "docs/integrate-public-and-private.md",
    ];
    let mut documents : Vec<ParsedDocument> = names . iter () . map (|name| {
      let path : &Path = Path::new (name);
      parse_document (path,
        fs::read_to_string (root . join (path)) . unwrap ())
    }) . collect ();
    let mut next = || ID::new (&uuid::Uuid::new_v4 () . to_string ());
    let source : SourceName = SourceName::from ("owned");
    let mut built : Vec<build::BuiltDocument> = documents . iter ()
      .map (|document| build::build_document (document, &source, &mut next)
        .unwrap ()) . collect ();
    resolve::resolve_document_links (&mut documents, &mut built,
      root, None, &HashMap::new ());
    assert! (built [0] . nodes . iter () . any (|node|
      node . body . as_deref () . unwrap_or ("") . contains ("```elisp")));
    assert! (documents [1] . diagnostics . iter () . any (|diagnostic|
      diagnostic . message . contains ("docs/data-model-trees-with-links.md")));
    assert! (built [3] . nodes . iter () . any (|node|
      node . body . as_deref () . unwrap_or ("") . contains (
        "[Semantic Synchrony](https://github.com/synchrony/smsn)")));
  }
}
