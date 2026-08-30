pub mod parse;

use crate::types::misc::{
  ID, MSV, MemberAtSource, SkgConfig, SkgfileSource, SourceName,
  members_msv, members_at_source_msv};
use crate::telescope::unfold::{
  UnfoldInput, UnfoldedTelescope, unfold_node,
};
use crate::types::nodes::fs::NodeFS;
use crate::types::nodes::complete::{FileProperty, NodeComplete};

use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

//
// Types
//

pub struct ImportStats {
  pub files_read    : usize,
  pub nodes_written : usize,
  pub errors        : Vec<String>,
}

impl fmt::Display for ImportStats {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>,
  ) -> fmt::Result {
    write! (f, "Files read: {}, nodes written: {}, errors: {}",
            self . files_read,
            self . nodes_written,
            self . errors . len() ) }}

//
// Public API
//

pub fn import_org_roam_directory (
  org_dir    : &Path,
  output_dir : &Path,
  source     : &SourceName,
) -> Result<ImportStats, Box<dyn Error>> {
  fs::create_dir_all (output_dir)?;
  for entry in fs::read_dir (output_dir)? { // Wipe existing .skg files to avoid orphans from previous runs.
    let entry : fs::DirEntry = entry?;
    let path : PathBuf = entry . path();
    if path . extension() . map_or (false, |e| e == "skg") {
      fs::remove_file (&path)?; }}
  let mut stats : ImportStats = ImportStats {
    files_read    : 0,
    nodes_written : 0,
    errors        : Vec::new(), };
  // Collect all nodes into a map keyed by primary ID.
  // When multiple org files define the same ID,
  // merge their children rather than clobbering.
  let mut node_map : HashMap<ID, NodeComplete> = HashMap::new();
  for entry in WalkDir::new (org_dir)
    . into_iter()
    . filter_entry (|e| // Never descend into a repo's .git folder.
      e . file_name() != ".git" )
    . filter_map (|e| e . ok() )
    . filter (|e| {
      e . path() . extension()
        . map_or (false, |ext| ext == "org") }) {
    let path : &Path = entry . path();
    stats . files_read += 1;
    let nodes : Vec<NodeComplete> =
      parse::parse_org_file (path);
    for mut node in nodes {
      node . source = source . clone();
      { // Re-tag the parse-time placeholder sources with the real
        // source, so the sources are honest even before the FS
        // boundary drops them (see MemberAtSource's INTERIM note).
        for m in node . contains . iter_mut () {
          m . source = source . clone (); }
        node . aliases = members_at_source_msv (
          &source, members_msv ( &node . aliases )); }
      { let pid : ID = node . pid . clone();
        if let Some (existing) = node_map . get_mut (&pid) {
          merge_into_existing (existing, &node);
          tracing::warn! (
            id = %pid,
            "Overloaded ID — multiple org-roam nodes use this ID. \
             Merging their content."); }
        else {
          node_map . insert (pid, node); }} }}
  for (_pid, node) in &node_map {
    match write_nodecomplete_to_dir (node, output_dir) {
      Ok (()) => { stats . nodes_written += 1; }
      Err (e) => {
        let msg : String = format! (
          "Error writing node '{}': {}",
          node . title, e);
        stats . errors . push (msg); }} }
  Ok (stats) }

//
// File writing
//

/// Merge a duplicate node into an existing one with the same ID.
/// Appends children, body text, and aliases from the newcomer.
/// Tags the result with Was_Overloaded.
fn merge_into_existing (
  existing : &mut NodeComplete,
  newcomer : &NodeComplete,
) {
  if ! existing . misc . contains (&FileProperty::Was_Overloaded) {
    existing . misc . push (FileProperty::Was_Overloaded); }
  { // Merge contents. New members are tagged with the owning
    // (existing) node's source; DEGENERATE (see MemberAtSource).
    for child in &newcomer . contains {
      let child_id : &ID = & child . member;
      if ! existing . contains . iter ()
           . any ( |m| &m . member == child_id ) {
        existing . contains . push ( MemberAtSource::at_source (
          existing . source . clone (), child_id . clone () )); }} }
  { // Append the newcomer's title and body into the existing body,
    // separated by an informative marker.
    let separator : &str =
      "\n\n====== imported from a distinct org-roam node with the same ID ======";
    let mut appendage : String = String::new();
    appendage . push_str (separator);
    appendage . push_str (&format! ("\ntitle: {}", newcomer . title));
    if let Some (new_body) = &newcomer . body {
      appendage . push_str (&format! ("\nbody: {}", new_body)); }
    let body : &mut String =
      existing . body . get_or_insert_with (String::new);
    body . push_str (&appendage); }
  { // Merge aliases. New members are tagged with the owning
    // (existing) node's source; DEGENERATE (see MemberAtSource).
    let newcomer_aliases : MSV<String> = members_msv (&newcomer . aliases);
    let new_aliases : &[String] = newcomer_aliases . or_default();
    if ! new_aliases . is_empty() {
      let source : SourceName = existing . source . clone();
      let merged : &mut Vec<MemberAtSource<String>> =
        existing . aliases . ensure_specified();
      for alias in new_aliases {
        if ! merged . iter () . any ( |m| &m . member == alias ) {
          merged . push ( MemberAtSource::at_source (
            source . clone (), alias . clone () )); }} } }
  if newcomer . misc . contains (&FileProperty::Had_ID_Before_Import)
    && ! existing . misc . contains (&FileProperty::Had_ID_Before_Import)
    { // Preserve Had_ID_Before_Import from either side.
      existing . misc . push (FileProperty::Had_ID_Before_Import); }}

fn write_nodecomplete_to_dir (
  node       : &NodeComplete,
  output_dir : &Path,
) -> Result<(), Box<dyn Error>> {
  let pid : &ID = &node . pid;
  let filename : String =
    format! ("{}.skg", &pid . 0);
  let path : std::path::PathBuf =
    output_dir . join (&filename);
  let node_fs : NodeFS = {
    // An imported node is single-section by construction (every
    // member source == its home), so the unfold yields exactly one
    // section. A one-source config lets the importer use the same
    // checked boundary as the ordinary filesystem writer.
    let source_name : SourceName = node . source . clone ();
    let config : SkgConfig = SkgConfig::dummyFromSources (
      [ ( source_name . clone (), SkgfileSource {
            name         : source_name . clone (),
            abbreviation : None,
            path         : output_dir . to_path_buf (),
            user_owns_it : true, } ) ]
      . into_iter () . collect () );
    let unfolded : UnfoldedTelescope =
      unfold_node (
        & UnfoldInput {
          pid      : & node . pid,
          extra_ids : & node . extra_ids,
          misc      : & node . misc,
          title    : Some ( & node . title ),
          body     : node . body . as_deref (),
          home     : & node . source,
          aliases  : node . aliases . or_default (),
          contains : & node . contains,
          subscribes_to :
            node . subscribes_to . or_default (),
          hides_from_its_subscriptions :
            node . hides_from_its_subscriptions . or_default (),
          overrides_view_of :
            node . overrides_view_of . or_default (), },
        &config ) ?;
    let (_, node_fs) : (SourceName, NodeFS) =
      unfolded . into_sections () . into_iter () . next ()
      . expect ("an imported node has a home section");
    node_fs };
  let yaml    : String = node_fs . to_yaml ()?;
  fs::write (&path, &yaml)?;
  Ok (( )) }
