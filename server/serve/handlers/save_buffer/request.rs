//! The save request's fields: its envelope ('parse_save_request_envelope':
//! the saved buffer, its baseline and the other open views) and the
//! '(KEY . VALUE)' fields of its request line (point position, approvals,
//! the confirmations' answers, the diff view buffer and the perspective).
//! Reading them refuses nothing but a malformed envelope or a name no
//! buffer or perspective has; the handler ('save_buffer.rs') decides
//! what each means for the save.

use super::{ClientViewSnapshot, SavePointPosition, SaveRequestEnvelope};
use crate::dbs::in_rust_graph::relation_accessors::{NodeRelation, RelationshipKey};
use crate::from_text::strandeds::StrandedsReview;
use crate::save::DeleteFrom;
use crate::serve::util::value_from_request_sexp;
use crate::types::config::SkgConfig;
use crate::types::misc::{ID, SkgrepoName};
use crate::types::views_state::{DiffViewBufferKind, Perspective, ViewforestId};

use sexp::{Sexp, Atom};
use std::collections::{HashMap, HashSet};
use std::error::Error;

pub fn parse_save_request_envelope (
  content   : &str,
  saved_viewforest_id : Option<&ViewforestId>,
) -> Result<SaveRequestEnvelope, Box<dyn Error>> {
  let parsed : Sexp = sexp::parse (content)
    .map_err (|error| format! ("Malformed save envelope: {}", error)) ?;
  let fields : &[Sexp] = sexp_list (&parsed, "save envelope") ?;
  let saved_buffer_value : &Sexp = unique_field (
    fields, "saved-buffer", "save envelope") ?;
  let saved_buffer : String = sexp_atom_string (
    saved_buffer_value, "saved-buffer") ?;
  let baseline : String = sexp_atom_string (
    unique_field (fields, "baseline", "save envelope") ?,
    "baseline") ?;
  let other_views_value : &Sexp = unique_field (
    fields, "other-views", "save envelope") ?;
  let other_view_entries : &[Sexp] = sexp_list (
    other_views_value, "other-views") ?;
  let mut other_views : Vec<ClientViewSnapshot> = Vec::new ();
  let mut seen_viewforest_ids : HashSet<ViewforestId> = HashSet::new ();
  for entry in other_view_entries {
    let entry_fields : &[Sexp] = sexp_list (entry, "other view") ?;
    let viewforest_id : ViewforestId = ViewforestId::from_client_string (sexp_atom_string (
      unique_field (entry_fields, "view-id", "other view") ?,
      "other view-id") ?);
    if saved_viewforest_id == Some (&viewforest_id) {
      return Err (format! (
        "Malformed save envelope: saved view ID {} appears in other-views",
        viewforest_id . repr_in_client ()) . into ()); }
    if ! seen_viewforest_ids . insert (viewforest_id . clone ()) {
      return Err (format! (
        "Malformed save envelope: duplicate other view ID {}",
        viewforest_id . repr_in_client ()) . into ()); }
    let dirty_text : String = sexp_atom_string (
      unique_field (entry_fields, "dirty", "other view") ?, "dirty") ?;
    let dirty : bool = match dirty_text . as_str () {
      "true"  => true,
      "false" => false,
      _ => return Err (format! (
        "Malformed save envelope: dirty must be true or false for {}",
        viewforest_id . repr_in_client ()) . into ()), };
    let baseline_field : Option<&Sexp> = optional_unique_field (
      entry_fields, "baseline", "other view") ?;
    let current_field : Option<&Sexp> = optional_unique_field (
      entry_fields, "current", "other view") ?;
    let (baseline, current) : (Option<String>, Option<String>) = if dirty {
      let baseline : Option<String> = parse_baseline (
        baseline_field . ok_or_else (|| format! (
          "Malformed save envelope: dirty view {} has no baseline field",
          viewforest_id . repr_in_client ())) ?) ?;
      let current : String = sexp_atom_string (
        current_field . ok_or_else (|| format! (
          "Malformed save envelope: dirty view {} has no current text",
          viewforest_id . repr_in_client ())) ?, "current") ?;
      (baseline, Some (current))
    } else {
      if baseline_field . is_some () || current_field . is_some () {
        return Err (format! (
          "Malformed save envelope: clean view {} carries dirty snapshots",
          viewforest_id . repr_in_client ()) . into ()); }
      (None, None)
    };
    for field in entry_fields {
      let (key, _) : (&str, &Sexp) = field_pair (field, "other view") ?;
      if ! ["view-id", "dirty", "baseline", "current"] . contains (&key) {
        return Err (format! (
          "Malformed save envelope: unknown other-view field {}", key)
          . into ()); }}
    other_views . push (ClientViewSnapshot {
      viewforest_id, dirty, baseline, current, }); }
  for field in fields {
    let (key, _) : (&str, &Sexp) = field_pair (field, "save envelope") ?;
    if ! ["saved-buffer", "baseline", "other-views"] . contains (&key) {
      return Err (format! (
        "Malformed save envelope: unknown field {}", key) . into ()); }}
  Ok (SaveRequestEnvelope { saved_buffer, baseline, other_views })
}

fn parse_baseline (
  value : &Sexp,
) -> Result<Option<String>, Box<dyn Error>> {
  match value {
    Sexp::Atom (Atom::S (status)) if status == "unavailable" => Ok (None),
    Sexp::List (items) if items . len () == 2 => {
      let status : String = sexp_atom_string (&items [0], "baseline status") ?;
      if status != "present" {
        return Err ("Malformed save envelope: baseline wrapper must begin with present" . into ()); }
      Ok (Some (sexp_atom_string (&items [1], "baseline text") ?)) }
    _ => Err ("Malformed save envelope: baseline must be unavailable or (present TEXT)" . into ()), }
}

fn unique_field<'a> (
  fields  : &'a [Sexp],
  key     : &str,
  context : &str,
) -> Result<&'a Sexp, Box<dyn Error>> {
  optional_unique_field (fields, key, context) ? . ok_or_else (||
    format! ("Malformed {}: missing {}", context, key) . into ())
}

fn optional_unique_field<'a> (
  fields  : &'a [Sexp],
  key     : &str,
  context : &str,
) -> Result<Option<&'a Sexp>, Box<dyn Error>> {
  let mut matches : Vec<&Sexp> = Vec::new ();
  for field in fields {
    let (candidate, value) : (&str, &Sexp) = field_pair (field, context) ?;
    if candidate == key { matches . push (value); }}
  match matches . as_slice () {
    []      => Ok (None),
    [value] => Ok (Some (*value)),
    _       => Err (format! (
      "Malformed {}: duplicate {}", context, key) . into ()), }
}

fn field_pair<'a> (
  value   : &'a Sexp,
  context : &str,
) -> Result<(&'a str, &'a Sexp), Box<dyn Error>> {
  let items : &[Sexp] = sexp_list (value, context) ?;
  if items . len () != 2 {
    return Err (format! (
      "Malformed {}: each field must contain a key and value", context)
      . into ()); }
  let key : &str = match &items [0] {
    Sexp::Atom (Atom::S (key)) => key,
    _ => return Err (format! (
      "Malformed {}: field key must be an atom", context) . into ()), };
  Ok ((key, &items [1]))
}

fn sexp_list<'a> (
  value   : &'a Sexp,
  context : &str,
) -> Result<&'a [Sexp], Box<dyn Error>> {
  match value {
    Sexp::List (items) => Ok (items),
    _ => Err (format! ("Malformed {}: expected a list", context) . into ()), }
}

fn sexp_atom_string (
  value   : &Sexp,
  context : &str,
) -> Result<String, Box<dyn Error>> {
  match value {
    Sexp::Atom (Atom::S (text)) => Ok (text . clone ()),
    _ => Err (format! ("Malformed {}: expected text", context) . into ()), }
}

/// The request's '(diff-view-buffer . "NAME")', naming the buffer of
/// the diff view the save is of ('DiffViewBufferKind::wire_name'); None
/// without one. A name no buffer has refuses the save.
pub(super) fn named_diffView_buffer_kind_from_request (
  request : &str,
) -> Result<Option<DiffViewBufferKind>, String> {
  match value_from_request_sexp ("diff-view-buffer", request) {
    Err (_) => Ok (None),
    Ok (name) => DiffViewBufferKind::from_wire_name (&name)
      . map (Some)
      . ok_or_else ( || format! (
        "The save request names the diff view buffer {:?}, which the diff view does not have. Nothing was saved.",
        name )), }}

/// The request's '(perspective . "NAME")': the perspective the
/// saved buffer was drawn from ('Perspective::wire_name');
/// its root homes without one. A name no choice has refuses the
/// save.
pub(super) fn perspective_from_save_request (
  request : &str,
  config  : &SkgConfig,
) -> Result<Perspective, String> {
  match value_from_request_sexp ("perspective", request) {
    Err (_) => Ok (Perspective::RootHomes),
    Ok (name) => Perspective::from_wire_name (&name, config)
      . ok_or_else ( || format! (
        "The save request says the buffer was drawn from the perspective {:?}, which names none. Nothing was saved.",
        name )), }}

pub(super) fn save_point_position_from_request (
  request : &str,
) -> Option<SavePointPosition> {
  let point_lines_below_focused_headline : usize =
    nat_from_request (
      request,
      "point-lines-below-focused-headline" ) ?;
  let point_column : usize =
    nat_from_request (
      request,
      "point-column" ) ?;
  let point_screen_lines_below_window_start : usize =
    nat_from_request (
      request,
      "point-screen-lines-below-window-start" ) ?;
  Some ( SavePointPosition {
    point_lines_below_focused_headline,
    point_column,
    point_screen_lines_below_window_start, } ) }

fn nat_from_request (
  request : &str,
  key     : &str,
) -> Option<usize> {
  value_from_request_sexp (key, request) . ok () ? . parse () . ok () }

/// The request's review of the relationships a home skgrepo move strands:
/// '(strandeds-reviewed . "true")' marks a re-save after a
/// strandeds-confirmation, and '(reset-strandeds (RELATION RECORDER
/// MEMBER) ...)' names the ones the user chose to reset to default. Without
/// the first, not reviewed.
pub(super) fn strandeds_review_from_request (
  request : &str,
) -> StrandedsReview {
  let reviewed : bool =
    value_from_request_sexp ("strandeds-reviewed", request)
    . map ( |v| v == "true" )
    . unwrap_or (false);
  if ! reviewed { return StrandedsReview::NotReviewed; }
  let mut chosen : HashSet<RelationshipKey> = HashSet::new ();
  let Ok (Sexp::List (items)) = sexp::parse (request)
    else { return StrandedsReview::Reviewed (chosen); };
  for item in &items {
    let Sexp::List (field) = item else { continue; };
    let Some ( Sexp::Atom ( Atom::S (key) )) = field . first ()
      else { continue; };
    if key != "reset-strandeds" { continue; }
    for entry in &field [1..] {
      if let Sexp::List (triple) = entry {
        if let [ Sexp::Atom ( Atom::S (relation) ),
                 Sexp::Atom ( Atom::S (recorder) ),
                 Sexp::Atom ( Atom::S (member) ) ] = & triple [..]
        { if let Some (relation) = NodeRelation::from_relation_name (relation) {
            chosen . insert ( RelationshipKey {
              relation,
              recorder : ID::from (recorder . as_str ()),
              member   : ID::from (member . as_str ()), } ); }} }} }
  StrandedsReview::Reviewed (chosen) }

/// The request's choice of where its deletes may strip the deleted
/// IDs: '(delete-from . "visible")' or '(delete-from . "all")', present
/// only on a retry after a 'delete-from-confirmation'. Absent or any
/// other value, not chosen.
pub(super) fn delete_from_from_request (
  request : &str,
) -> DeleteFrom {
  match value_from_request_sexp ("delete-from", request) . as_deref () {
    Ok ("visible") => DeleteFrom::VisibleRecords,
    Ok ("all")     => DeleteFrom::AllOwnedRecords,
    _              => DeleteFrom::NotChosen, }}

/// Whether a save request carries '(preview . "true")': the client
/// asks only what the save would hide and stop hiding, and the server
/// answers 'save-preview' once the plan is built, writing nothing.
pub(super) fn preview_from_request (
  request : &str,
) -> bool {
  value_from_request_sexp ("preview", request)
    . map ( |v| v == "true" )
    . unwrap_or (false) }

/// Whether this save request carries '(approved-forks . "true")', set by
/// the client when the user approves a fork-confirmation. Absent or any
/// other value means not approved.
pub(super) fn fork_approved_from_request (
  request : &str,
) -> bool {
  value_from_request_sexp ("approved-forks", request)
    . map ( |v| v == "true" )
    . unwrap_or (false) }

/// Parse the optional '(fork-repos ((N . REPO) ...))' field of a
/// save request into a map from each forked node's id N to the owned
/// skgrepo the user chose for its clone (rotated, or left at the default,
/// in the fork-confirmation buffer). Absent on an ordinary save and on
/// the first (unapproved) save -- then every clone skgrepo resolves by
/// inference-else-default. The chosen skgrepo is validated owned + unrestricted
/// downstream in 'validate_fork_specs'.
pub(super) fn fork_skgrepos_from_request (
  request : &str,
) -> HashMap<ID, SkgrepoName> {
  let mut map : HashMap<ID, SkgrepoName> = HashMap::new ();
  let Ok (sexp) = sexp::parse (request) else { return map; };
  let Sexp::List (items) = sexp else { return map; };
  for item in & items {
    let Sexp::List (pair) = item else { continue; };
    let Some ( Sexp::Atom ( Atom::S (k) )) = pair . first ()
      else { continue; };
    if k != "fork-repos" { continue; }
    let Some ( Sexp::List (entries) ) = pair . get (1)
      else { continue; };
    for entry in entries {
      // Each entry is a dotted pair (N . REPO), which the sexp crate
      // parses as the 3-element list [N, ".", REPO].
      if let Sexp::List (kv) = entry {
        if let [ Sexp::Atom ( Atom::S (gnid) ),
                 Sexp::Atom ( Atom::S (dot) ),
                 Sexp::Atom ( Atom::S (skgrepo) ) ] = & kv [..]
        { if dot == "." {
            map . insert ( ID ( gnid . clone () ),
                           SkgrepoName::from ( skgrepo . as_str () )); }} }} }
  map }

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::config::Skgrepo;
  use std::path::PathBuf;

  #[test]
  fn a_save_names_its_buffers_perspective_and_a_bad_name_refuses_it () {
    let mut config : SkgConfig = SkgConfig::dummyFromSkgrepos (HashMap::new ());
    for (name, owned) in [ ("main", true), ("tracy", false) ] {
      config . skgrepos . insert (
        SkgrepoName::from (name),
        Skgrepo { name : SkgrepoName::from (name), abbreviation : None,
                  path : PathBuf::from (name), owned } ); }
    let named = |sexp : &str| perspective_from_save_request (
      &format! ("((request . \"save buffer\") (view-id . \"v\"){})", sexp), &config );
    assert_eq! ( named (""), Ok (Perspective::RootHomes) );
    assert_eq! ( named (" (perspective . \"root homes\")"),
                 Ok (Perspective::RootHomes) );
    for skgrepo in [ "main", "tracy" ] {
      assert_eq! ( named ( &format! (" (perspective . \"skgrepo {}\")", skgrepo )),
                   Ok ( Perspective::Skgrepo ( SkgrepoName::from (skgrepo) )),
                   "an owned or a foreign skgrepo" ); }
    for unknown in [ "mine", "skgrepo nobody", "user", "root authors", "owner of tracy" ] {
      assert! ( named ( &format! (" (perspective . \"{}\")", unknown )) . is_err (),
                "{:?} names no perspective", unknown ); }}
}
