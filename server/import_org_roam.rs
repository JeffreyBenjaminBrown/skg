pub mod parse;

use crate::types::misc::{
  ID, MSV, RelPartner, SkgConfig, SkgfileRepo, RepoName,
  members_msv, rel_partners_at_relRepo_msv};
use crate::telescope::unfold::{
  UnfoldInput, UnfoldedTelescope, unfold_node,
};
use crate::types::nodes::fs::NodeFS;
use crate::types::nodes::complete::{Flag, NodeComplete};
use crate::types::links::org_literal_ranges::{
  HEADLINES_INSIDE_BLOCKS_EXPLANATION, headlines_inside_blocks};

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
  repo     : &RepoName,
) -> Result<ImportStats, Box<dyn Error>> {
  let org_files : Vec<PathBuf> = org_files_in (org_dir);
  refuse_headlines_inside_blocks (&org_files)?; // before wiping anything
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
  for path in &org_files {
    stats . files_read += 1;
    let nodes : Vec<NodeComplete> =
      parse::parse_org_file (path);
    for mut node in nodes {
      node . home_repo = repo . clone();
      { // Re-tag the parse-time placeholder repos with the real
        // repo, so the repos are honest even before the FS
        // boundary drops them (see RelPartner's INTERIM note).
        for m in node . contains . iter_mut () {
          m . relRepo = repo . clone (); }
        node . aliases = rel_partners_at_relRepo_msv (
          &repo, members_msv ( &node . aliases )); }
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

fn org_files_in (
  org_dir : &Path,
) -> Vec<PathBuf> {
  WalkDir::new (org_dir)
    . into_iter()
    . filter_entry (|e| // Never descend into a repo's .git folder.
      e . file_name() != ".git" )
    . filter_map (|e| e . ok() )
    . filter (|e| {
      e . path() . extension()
        . map_or (false, |ext| ext == "org") })
    . map (|e| e . path() . to_path_buf() )
    . collect() }

/// A headline-like line inside a block is ambiguous (Org would end the
/// block there), so no file is imported, and nothing is wiped.
fn refuse_headlines_inside_blocks (
  org_files : &[PathBuf],
) -> Result<(), Box<dyn Error>> {
  let mut problems : Vec<String> = Vec::new();
  for path in org_files {
    let Ok (text) : Result<String, std::io::Error> =
      fs::read_to_string (path) else { continue; }; // parse skips it too
    for inside in headlines_inside_blocks (&text) {
      problems . push ( format! (
        "  {}:{}: {}", path . display(),
        1 + text [.. inside . headline . start] . matches ('\n') . count(),
        inside )); }}
  if problems . is_empty() { return Ok (( )); }
  Err ( format! ("{}\n{}",
                 HEADLINES_INSIDE_BLOCKS_EXPLANATION,
                 problems . join ("\n")) . into() ) }

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
  if ! existing . misc . contains (&Flag::Was_Overloaded) {
    existing . misc . push (Flag::Was_Overloaded); }
  { // Merge contents. New members are tagged with the owning
    // (existing) node's repo; DEGENERATE (see RelPartner).
    for child in &newcomer . contains {
      let child_id : &ID = & child . member;
      if ! existing . contains . iter ()
           . any ( |m| &m . member == child_id ) {
        existing . contains . push ( RelPartner::at_relRepo (
          existing . home_repo . clone (), child_id . clone () )); }} }
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
    // (existing) node's repo; DEGENERATE (see RelPartner).
    let newcomer_aliases : MSV<String> = members_msv (&newcomer . aliases);
    let new_aliases : &[String] = newcomer_aliases . or_default();
    if ! new_aliases . is_empty() {
      let repo : RepoName = existing . home_repo . clone();
      let merged : &mut Vec<RelPartner<String>> =
        existing . aliases . ensure_specified();
      for alias in new_aliases {
        if ! merged . iter () . any ( |m| &m . member == alias ) {
          merged . push ( RelPartner::at_relRepo (
            repo . clone (), alias . clone () )); }} } }
  if newcomer . misc . contains (&Flag::Had_ID_Before_Import)
    && ! existing . misc . contains (&Flag::Had_ID_Before_Import)
    { // Preserve Had_ID_Before_Import from either side.
      existing . misc . push (Flag::Had_ID_Before_Import); }}

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
    // member relRepo == its home), so the unfold yields exactly one
    // section. A one-repo config lets the importer use the same
    // checked boundary as the ordinary filesystem writer.
    let repo_name : RepoName = node . home_repo . clone ();
    let mut config : SkgConfig = SkgConfig::dummyFromRepos (
      [ ( repo_name . clone (), SkgfileRepo {
            name         : repo_name . clone (),
            abbreviation : None,
            path         : output_dir . to_path_buf (),
            user_owns_it : true, } ) ]
      . into_iter () . collect () );
    config . repo_order = vec! [repo_name];
    let unfolded : UnfoldedTelescope =
      unfold_node (
        & UnfoldInput {
          pid      : & node . pid,
          extra_ids : & node . extra_ids,
          misc      : & node . misc,
          title    : Some ( & node . title ),
          body     : node . body . as_deref (),
          home     : & node . home_repo,
          aliases  : node . aliases . or_default (),
          contains : & node . contains,
          subscribes_to :
            node . subscribes_to . or_default (),
          hides_from_its_subscriptions :
            node . hides_from_its_subscriptions . or_default (),
          overrides_view_of :
            node . overrides_view_of . or_default (), },
        &config ) ?;
    let (_, node_fs) : (RepoName, NodeFS) =
      unfolded . into_sections () . into_iter () . next ()
      . expect ("an imported node has a home section");
    node_fs };
  let yaml    : String = node_fs . to_yaml ()?;
  fs::write (&path, &yaml)?;
  Ok (( )) }
