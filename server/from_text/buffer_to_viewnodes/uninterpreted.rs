/// DANGER: This was vibe-coded without much oversight.
/// I *believe* the tests constrain its behavior sufficiently.

use crate::types::sexp::find_sexp_end;
use crate::types::errors::{BufferValidationError, HeadlineName, with_org_subheadlines};
use crate::serve::parse_metadata_sexp::{ parse_metadata_to_viewnodemd, default_metadata, viewnode_from_metadata, ViewnodeMetadata };
use crate::types::maybe_placed_viewnode::{MpViewnode, MpViewnodeKind};
use crate::types::misc::ID;
use crate::types::tree::forest::MpViewForest;
use crate::types::viewnode::ViewnodeId;

use ego_tree::{Tree, NodeId, NodeMut};
use regex::Regex;
use sexp::{Atom, Sexp};
use std::collections::HashSet;
use std::sync::LazyLock;

/// Type alias for headline information: (level, metadata, title)
pub type HeadlineInfo = (usize, Option<ViewnodeMetadata>, String);

/// Result type for headline parsing with specific error messages
type HeadlineResult = Result<HeadlineInfo, String>;

/// Represents a parsed org node with its headline and body lines
#[derive(Debug, Clone)]
struct ViewnodeLineCol {
  headline: HeadlineInfo,
  body: Vec<String>,
}

/// Parse org text into a view forest.
/// Each level-1 headline becomes a forest root.
/// The result takes the org-buffer at face value,
/// without changing the tree (e.g. without absorbing aliases
/// into the relevant ancestor) and without supplementing information
/// (e.g. missing IDs).
///
/// Returns (tree, parsing_errors).
///
/// Shares responsibility for error detection
/// with 'find_buffer_errors_for_saving', which runs later.
/// That function detects the majority of possible errors,
/// but it can't detect them all because it uses a tree of MpViewnodes,
/// which permit fewer kinds of invalid state than the raw text.
/// (For instance, Alias and AliasFolder cannot have bodies,
/// but they can in the raw text, and that's an error.)
pub fn org_to_uninterpreted_nodes(
  input: &str
) -> Result < ( Tree<MpViewnode>,
                Vec<BufferValidationError> ),
              String > {
  let (viewforest, parsing_errors, _warnings)
    : (MpViewForest, Vec<BufferValidationError>, Vec<String>) =
    org_to_uninterpreted_viewforest (input)?;
  Ok ( ( viewforest . into_internal_tree (), parsing_errors ) ) }

/// The third element of the result is nonfatal parse warnings
/// (e.g. discarded headline text on a folder), destined for
/// 'SaveResponse.warnings'.
pub fn org_to_uninterpreted_viewforest(
  input: &str
) -> Result < ( MpViewForest,
                Vec<BufferValidationError>,
                Vec<String> ),
              String > {
  let mut viewforest: MpViewForest =
    MpViewForest::new();
  let mut parsing_errors: Vec<BufferValidationError> = Vec::new();
  let mut parsing_warnings: Vec<String> = Vec::new();
  let mut viewnode_ids_seen: HashSet<ViewnodeId> = HashSet::new();
  // treeid_stack[0] is the internal forest root,
  // treeid_stack[1] is the current view root, etc.
  let mut treeid_stack: Vec<NodeId> = vec![ {
    let viewforest_root_treeid: NodeId =
      viewforest . internal_root_skgid ();
    viewforest_root_treeid } ];
  for viewnode_line_col in &mut {
    let viewnode_line_cols: Vec<ViewnodeLineCol> =
      divide_into_viewNodeLineCols (input)?;
    viewnode_line_cols } {
    if let Some (metadata) = &mut viewnode_line_col . headline . 1 {
      if let Some (viewnode_id) = &metadata . viewnode_id {
        // Of two headlines with one viewnode ID, the first in buffer order
        // keeps it; the second is a copy, and will get a fresh one.
        // (Dropped before the headline becomes a viewnode, so that no
        // message names the copy by the original's viewnode ID.)
        // One the view's record does not know is dropped after the
        // parse ('drop_unknown_viewnode_ids_in_parsed_viewforest').
        if ! viewnode_ids_seen . insert (viewnode_id . clone ()) {
          metadata . viewnode_id = None; }}}
    let (level, viewnode, error_opt, warning_opt)
      : (usize, MpViewnode, Option<BufferValidationError>, Option<String>)
      = linecol_to_viewnode (viewnode_line_col)?;
    if let Some (error) = error_opt {
      parsing_errors . push (error); }
    if let Some (warning) = warning_opt {
      parsing_warnings . push (warning); }
    // Adjust treeid_stack to proper level
    // (the internal forest root is level 0; view roots are level 1).
    while treeid_stack . len() > level {
      treeid_stack . pop(); }
    // Check for orphaned headlines (e.g., ** without preceding *)
    // treeid_stack.len() should equal level
    // because the internal forest root is at level 0.
    if treeid_stack . len() < level {
      return Err ( with_org_subheadlines (
        &format! (
          "Node \"{}\" at level {} jumps too far between levels (no valid parent at level {}).",
          viewnode . error_label(), level, level - 1),
        &[ viewnode . headline_name () ] )); }
    treeid_stack . push( {
      let parent_treeid: NodeId = *treeid_stack . last() . unwrap();
      let new_treeid: NodeId = {
        let mut parent_mut : NodeMut<MpViewnode> =
          viewforest . get_mut (parent_treeid) . unwrap();
        parent_mut . append (viewnode) . id() };
      new_treeid } ); }
  Ok ( ( viewforest, parsing_errors, parsing_warnings ) ) }

/// Parse input text into org node line collections.
/// Each org node consists of a headline and its following body lines.
/// Returns an error if any headline has invalid metadata.
fn divide_into_viewNodeLineCols (
  input: &str
) -> Result<Vec<ViewnodeLineCol>, String> {
  let lines: Vec<&str> = // Trailing whitespace is stripped from every line at save. (Titles are trimmed again downstream; this is what strips body lines.)
    input . lines() . map ( str::trim_end ) . collect();
  let mut result: Vec<ViewnodeLineCol> = Vec::new();
  let mut i: usize = 0;
  while i < lines . len() {
    match headline_to_triple( lines[i] ) {
      Ok (headline_info) => {
        // Found a headline. Now collect its body lines.
        let mut body_lines: Vec<String> = Vec::new();
        i += 1; // Move past the headline
        while i < lines . len() {
          match headline_to_triple( lines[i] ) {
            Ok (_) => break, // Found another headline
            Err (e) if e == "__NOT_A_HEADLINE__" => {
              // Not a headline, it's a body line
              body_lines . push (
                crate::body_view_escape::decode_body_line_from_view (lines[i]));
              i += 1;
            },
            Err (e) => return Err (e), // Invalid metadata
          }
        }
        result . push ( ViewnodeLineCol {
          headline: headline_info,
          body: body_lines, } );
      },
      Err (e) if e == "__NOT_A_HEADLINE__" => {
        // Skip non-headline lines at the beginning of the document
        i += 1;
      },
      Err (e) => return Err (e), // Invalid metadata
    }
  }
  Ok (result) }

#[cfg(test)]
mod imported_body_tests {
  use super::*;

  #[test]
  fn literal_headings_drawers_and_mixed_fences_remain_one_body_on_save () {
    let body : &str = ":PROPERTIES:\n:ID: example-not-a-node\n:END:\n* literal heading\n,* existing comma heading\n```org\n** fenced heading\n```\n#+begin_src org\n* repo heading\n#+end_src";
    let rendered : String = format! ("* File root\n{}\n",
      crate::body_view_escape::encode_body_for_view (body));
    let parsed : Vec<ViewnodeLineCol> =
      divide_into_viewNodeLineCols (&rendered) . unwrap ();
    assert_eq! (parsed . len (), 1);
    assert_eq! (parsed [0] . body . join ("\n"), body);
  }
}

/// Create an MpViewnode from an ViewnodeLineCol.
/// This helper extracts the node creation logic from the main parsing function.
/// Returns (level, MpViewnode, error, warning).
fn linecol_to_viewnode(
  viewnode_line_col: &ViewnodeLineCol
) -> Result < ( usize, MpViewnode,
                Option<BufferValidationError>,
                Option<String> ),
              String > {
  let (level, metadata_option, title): HeadlineInfo =
    viewnode_line_col . headline . clone();
  let body_lines: &[String] = &viewnode_line_col . body;
  let body_text: Option<String> =
    if body_lines . is_empty() { None
    } else { Some(body_lines . join ("\n")) };
  let metadata : ViewnodeMetadata =
    if let Some (parsed_metadata) = metadata_option {
      parsed_metadata
    } else { // No metadata, so use defaults.
      default_metadata () };
  let body_on_writeProtected : bool =
    metadata . writeProtected
    && body_text . as_ref () . is_some_and (
      |body| ! body . trim () . is_empty () );
  let ( viewnode, error_opt, warning_opt )
    : ( MpViewnode, Option<BufferValidationError>, Option<String> )
    = viewnode_from_metadata ( &metadata, title . clone (), body_text );
  if matches! (
    &viewnode . kind,
    MpViewnodeKind::BufferRoot)
  { return Err (
      "forestRoot metadata is internal and cannot appear in buffer text"
      . to_string () ); }
  let error_opt : Option<BufferValidationError> =
    error_opt . or_else ( || if body_on_writeProtected {
      Some (match metadata . skgid . clone () {
        Some (skgid) => BufferValidationError::EditedWriteProtectedOccurrence {
          skgid    : skgid,
          title    : title . clone (),
          changes  : vec!["added body text" . to_string ()],
          headline : viewnode . headline_name (), },
        None => BufferValidationError::Other ( with_org_subheadlines (
          "A write-protected node has body text, which saving would discard. Add an ID and edit a editable occurrence instead.",
          &[ viewnode . headline_name () ] )), })
    } else { None } );
  Ok ( ( level, viewnode, error_opt, warning_opt ) ) }

/// Check if a line is a valid headline and extract level, metadata, and title.
/// Returns Ok(HeadlineInfo) if it's a valid headline with valid metadata.
/// Returns Ok with None metadata if it's a valid headline without metadata.
/// Returns Err with specific error message if metadata is invalid.
/// Returns Ok for non-headlines (they are valid non-headline lines).
pub fn headline_to_triple (
  headline: &str
) -> HeadlineResult {
  // Simple regex to capture just asterisks and remainder
  static HEADLINE_REGEX
    : LazyLock<Regex>
    = LazyLock::new(|| {
      Regex::new(r"^(\*+)\s+(.+)") . unwrap()
    });
  if let Some (captures) = HEADLINE_REGEX . captures (headline) {
    let asterisks: &str = captures . get (1) . unwrap() . as_str();
    let level: usize = asterisks . len();
    let remainder: &str = captures . get (2) . unwrap() . as_str() . trim();

    // Check if remainder starts with metadata
    if remainder . starts_with ("(skg") {
      // Find the end of the s-expression
      if let Some (sexp_end) = find_sexp_end (remainder) {
        // Extract the s-expression substring
        let sexp_str: &str = &remainder[..sexp_end];

        // The title is everything after the s-expression
        let title: String =
          remainder[sexp_end..] . trim() . to_string();

        // Verify it's valid by attempting to parse it
        let parsed_sexp : Sexp = match sexp::parse (sexp_str) {
          Ok (parsed_sexp) => parsed_sexp,
          Err (e) => return Err ( with_org_subheadlines (
            &format! ("Invalid s-expression syntax: {}", e),
            &[ HeadlineName::of ( None, None, &title ) ] )), };

        // Parse the metadata from the s-expression
        let metadata: Option<ViewnodeMetadata> =
          match parse_metadata_to_viewnodemd (sexp_str) {
            Ok (parsed_metadata) => Some (parsed_metadata),
            Err (e) => // Invalid metadata with specific error
              return Err ( with_org_subheadlines (
                &e,
                &[ HeadlineName::of (
                     viewnode_id_written_in_metadata (&parsed_sexp) . as_ref (),
                     graphnode_id_written_in_metadata (&parsed_sexp) . as_ref (),
                     &title ) ] )),
          };

        return Ok((level, metadata, title));
      } else {
        return Err("Unclosed metadata parentheses" . to_string());
      }
    }

    // No metadata, just title
    let title: String = remainder . to_string();
    Ok((level, None, title))
  } else {
    // Not a headline - this is OK (it's a valid non-headline line)
    // We use a special error to signal "not a headline"
    // This is a bit of a hack, but maintains backward compatibility
    Err("__NOT_A_HEADLINE__" . to_string())
  }}

/// The graphnode ID a headline's '(skg ...)' sexp carries in its
/// '(node ...)' or '(unknown ...)' form as '(id X)', or in its
/// '(ghost ...)' form as '(gnid X)', read without parsing the rest of
/// it, so that an error in the rest can name the headline. None if it
/// carries none.
fn graphnode_id_written_in_metadata (
  metadata_sexp : &Sexp,
) -> Option<ID> {
  let Sexp::List (elements) = metadata_sexp else { return None; };
  elements . iter () . find_map ( |element| {
    let Sexp::List (form) = element else { return None; };
    let [ Sexp::Atom (Atom::S (head)), fields @ .. ] = form . as_slice ()
      else { return None; };
    let id_key : &str = match head . as_str () {
      "node" | "unknown" => "id",
      "ghost"            => "gnid",
      _                  => return None, };
    fields . iter () . find_map ( |field| match field {
      Sexp::List (pair) => match pair . as_slice () {
        [ Sexp::Atom (Atom::S (key)), Sexp::Atom (Atom::S (value)) ]
          if key == id_key => Some ( ID::from ( value . as_str () )),
        _ => None },
      _ => None } ) } ) }

/// The viewnode ID a headline's '(skg ...)' sexp carries, read without
/// parsing the rest of it, so that an error in the rest can name the
/// headline. None if it carries none, or one that is malformed.
fn viewnode_id_written_in_metadata (
  metadata_sexp : &Sexp,
) -> Option<ViewnodeId> {
  let Sexp::List (elements) = metadata_sexp else { return None; };
  elements . iter () . find_map ( |element| match element {
    Sexp::List (pair) => match pair . as_slice () {
      [ Sexp::Atom (Atom::S (key)), Sexp::Atom (Atom::S (value)) ]
        if key == "vnid" => ViewnodeId::parse (value) . ok (),
      _ => None },
    _ => None } ) }
