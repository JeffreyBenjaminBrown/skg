//! Read-only preparation and under-gate revalidation of one import.

use super::{discover_documents, build::{BuiltDocument, build_document},
  parse::ParsedDocument, publish::prepare_import_publication,
  resolve::{contains_absolute_file_link, resolve_document_links}};
use crate::dbs::filesystem::multiple_nodes::{
  read_all_skg_files_from_sources_read_only, read_skg_sections_from_folder};
use crate::dbs::in_rust_graph::{InRustGraph,
  complete_validation::complete_from_rust};
use crate::export_org::claimed_export_targets;
use crate::types::env::SkgEnv;
use crate::types::misc::{ID, MSV, SkgConfig, SourceName, rel_partners_at_relSource};
use crate::types::nodes::complete::{NodeComplete, empty_node_complete};
use std::collections::HashMap;
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use time::{OffsetDateTime, format_description::well_known::Rfc3339};
use uuid::Uuid;

pub struct PreparedImportBatch {
  pub input_directory : PathBuf,
  pub host_root : Option<PathBuf>,
  pub destination_source : SourceName,
  pub documents : Vec<ParsedDocument>,
  pub nodes : Vec<NodeComplete>,
  pub record_id : Option<ID>,
  pub export_targets : Vec<(PathBuf, String)>,
  config : Arc<SkgConfig>,
  destination_evidence : BTreeMap<PathBuf, Vec<u8>>,
}

pub enum ImportPreparation {
  HostMappingNeeded,
  Prepared (PreparedImportBatch),
}

pub fn prepare_import_batch (
  input_directory : &Path,
  destination_source : &SourceName,
  host_root : Option<&Path>,
  host_mapping_answered : bool,
  env : &SkgEnv,
) -> Result<ImportPreparation, String> {
  let mut new_id = || ID::new (&Uuid::new_v4 () . to_string ());
  prepare_import_batch_with (
    input_directory, destination_source, host_root,
    host_mapping_answered, env, &mut new_id)
}

pub fn prepare_import_batch_with (
  input_directory : &Path,
  destination_source : &SourceName,
  host_root : Option<&Path>,
  host_mapping_answered : bool,
  env : &SkgEnv,
  new_id : &mut impl FnMut () -> ID,
) -> Result<ImportPreparation, String> {
  if ! input_directory . is_absolute () {
    return Err ("Input directory must be an absolute server path" . to_string ()); }
  if let Some (host) = host_root {
    if ! host . is_absolute () {
      return Err ("Host root must be an absolute path" . to_string ()); } }
  let runtime = env . runtime_snapshot ();
  if ! runtime . config . user_owns_source (destination_source) {
    return Err (format! ("Destination source {} is absent or not owned",
      destination_source)); }
  let mut documents : Vec<ParsedDocument> = discover_documents (input_directory)?;
  if ! host_mapping_answered && contains_absolute_file_link (&documents) {
    return Ok (ImportPreparation::HostMappingNeeded); }
  let existing_ids : HashMap<String, ID> =
    configured_identity_map (env)?;
  let destination_evidence : BTreeMap<PathBuf, Vec<u8>> =
    source_file_evidence (&runtime . config)?;
  let mut built : Vec<BuiltDocument> = documents . iter ()
    .map (|document| build_document (document, destination_source, new_id))
    .collect::<Result<_, _>> ()?;
  resolve_document_links (&mut documents, &mut built,
    input_directory, host_root, &existing_ids);
  let export_targets : Vec<(PathBuf, String)> = documents . iter ()
    .zip (built . iter ())
    .map (|(document, built)|
      (document . path . clone (), built . export_target . clone ()))
    .collect ();
  let existing : Vec<NodeComplete> =
    existing_authoritative_nodes (&runtime . config)?;
  ensure_runtime_matches_disk (&existing, &runtime . graph)?;
  check_export_target_conflicts (
    &export_targets, &runtime . config, &existing)?;
  let root_ids : Vec<ID> = built . iter ()
    .map (|document| document . root_id . clone ()) . collect ();
  let mut nodes : Vec<NodeComplete> = built . into_iter ()
    .flat_map (|document| document . nodes) . collect ();
  let record_id : Option<ID> = if documents . is_empty () { None } else {
    let record_id : ID = new_id ();
    nodes . push (import_record (
      record_id . clone (), &root_ids, input_directory,
      host_root, destination_source, "on confirmation"));
    Some (record_id) };
  if ! nodes . is_empty () {
    let _ = prepare_import_publication (&nodes, env)?; }
  Ok (ImportPreparation::Prepared (PreparedImportBatch {
    input_directory : input_directory . to_path_buf (),
    host_root : host_root . map (Path::to_path_buf),
    destination_source : destination_source . clone (),
    documents, nodes, record_id, export_targets,
    config : runtime . config . clone (),
    destination_evidence,
  }))
}

impl PreparedImportBatch {
  pub fn preview_report (
    &self,
  ) -> String {
    let warnings : Vec<String> = self . documents . iter ()
      .flat_map (|document| document . diagnostics . iter ()
        .map (|diagnostic| format! (
          "{}:{}: {}", document . path . display (),
          line_at (&document . text, diagnostic . range . start),
          diagnostic . message)))
      .collect ();
    let mut out : String = format! (
      "Import directory: {}\nDestination source: {} (determines privacy)\nHost root: {}\nDocuments: {}\nNew nodes: {}\n",
      self . input_directory . display (), self . destination_source,
      self . host_root . as_ref () . map (|path| path . display () . to_string ())
        . unwrap_or_else (|| "none" . to_string ()),
      self . documents . len (), self . nodes . len ());
    for (source, target) in &self . export_targets {
      out . push_str (&format! (
        "  {} -> {}.org\n", source . display (), target)); }
    if ! warnings . is_empty () {
      out . push_str (&format! ("Warnings ({}):\n", warnings . len ()));
      for warning in warnings { out . push_str (&format! ("  {}\n", warning)); } }
    out
  }

  /// Caller holds the shared mutation gate. A changed input set, body,
  /// configuration, identity claim, or export claim requires a new preview.
  pub(crate) fn apply_under_mutation_gate (
    self,
    env : &SkgEnv,
  ) -> Result<(usize, ID), String> {
    let runtime = env . runtime_snapshot ();
    if *runtime . config != *self . config ||
      ! runtime . config . user_owns_source (&self . destination_source) {
      return Err ("Configuration or source ownership changed; preview again"
        . to_string ()); }
    let current : Vec<ParsedDocument> = discover_documents (&self . input_directory)?;
    if current . len () != self . documents . len () ||
      current . iter () . zip (self . documents . iter ())
        .any (|(now, before)| now . path != before . path ||
          now . text != before . text) {
      return Err ("Input files changed; preview again" . to_string ()); }
    if source_file_evidence (&runtime . config)? != self . destination_evidence {
      return Err ("Configured source files changed; preview again" . to_string ()); }
    let existing : Vec<NodeComplete> =
      existing_authoritative_nodes (&runtime . config)?;
    ensure_runtime_matches_disk (&existing, &runtime . graph)?;
    check_export_target_conflicts (
      &self . export_targets, &runtime . config, &existing)?;
    let record_id : ID = self . record_id . ok_or_else (||
      "An empty import has no approval token or record" . to_string ())?;
    let mut nodes : Vec<NodeComplete> = self . nodes;
    let execution_time : String = OffsetDateTime::now_utc ()
      .format (&Rfc3339) . map_err (|error| error . to_string ())?;
    let record : &mut NodeComplete = nodes . iter_mut ()
      .find (|node| node . pid == record_id) .unwrap ();
    record . body = Some (import_record_body (
      &self . input_directory, self . host_root . as_deref (),
      &self . destination_source, &execution_time));
    let prepared = prepare_import_publication (&nodes, env)?;
    let created : usize = prepared . apply_under_mutation_gate (env)?;
    Ok ((created, record_id))
  }
}

fn configured_identity_map (
  env : &SkgEnv,
) -> Result<HashMap<String, ID>, String> {
  let runtime = env . runtime_snapshot ();
  let mut ids : HashMap<String, ID> = HashMap::new ();
  for node in runtime . graph . nodes . values () {
    ids . insert (node . pid . 0 . clone (), node . pid . clone ());
    for extra in &node . extra_ids {
      ids . insert (extra . 0 . clone (), node . pid . clone ()); } }
  for source in runtime . config . ordered_sources () {
    let sections = read_skg_sections_from_folder (&source, &runtime . config)
      .map_err (|error| format! ("Reading source {}: {}", source, error))?;
    for (_, section) in sections {
      ids . insert (section . pid . 0 . clone (), section . pid . clone ());
      for extra in &section . extra_ids {
        ids . insert (extra . 0 . clone (), section . pid . clone ()); } } }
  Ok (ids)
}

fn existing_authoritative_nodes (
  config : &SkgConfig,
) -> Result<Vec<NodeComplete>, String> {
  read_all_skg_files_from_sources_read_only (config)
    . map_err (|error| format! ("Reading configured sources: {}", error))
}

fn ensure_runtime_matches_disk (
  on_disk : &[NodeComplete],
  graph : &InRustGraph,
) -> Result<(), String> {
  let disk : HashMap<ID, NodeComplete> = on_disk . iter ()
    .map (|node| (node . pid . clone (),
      normalized_for_runtime_comparison (node . clone ()))) . collect ();
  let runtime : HashMap<ID, NodeComplete> = graph . nodes . iter ()
    .map (|(id, node)| (id . clone (),
      normalized_for_runtime_comparison (complete_from_rust (node))))
    .collect ();
  if disk != runtime {
    return Err ("Configured source files differ from the runtime graph; rebuild and preview again"
      . to_string ()); }
  Ok (())
}

fn normalized_for_runtime_comparison (
  mut node : NodeComplete,
) -> NodeComplete {
  // Folding omits an empty home alias field; the runtime graph stores the
  // same empty set as Specified([]). Neither makes an identity claim.
  if matches! (&node . aliases, MSV::Specified (values) if values . is_empty ()) {
    node . aliases = MSV::Unspecified; }
  node
}

fn source_file_evidence (
  config : &SkgConfig,
) -> Result<BTreeMap<PathBuf, Vec<u8>>, String> {
  let mut files : BTreeMap<PathBuf, Vec<u8>> = BTreeMap::new ();
  for source_name in config . ordered_sources () {
    let source = config . sources . get (&source_name)
      . ok_or_else (|| format! ("Configured source {} disappeared", source_name))?;
    let entries = fs::read_dir (&source . path)
      .map_err (|error| format! ("Reading source {}: {}", source_name, error))?;
    for entry in entries {
      let entry = entry . map_err (|error| error . to_string ())?;
      let path : PathBuf = entry . path ();
      if path . extension () . and_then (|ext| ext . to_str ()) != Some ("skg") {
        continue; }
      let bytes : Vec<u8> = fs::read (&path)
        .map_err (|error| format! ("Reading {}: {}", path . display (), error))?;
      files . insert (path, bytes); } }
  Ok (files)
}

fn check_export_target_conflicts (
  proposed : &[(PathBuf, String)],
  config : &SkgConfig,
  existing : &[NodeComplete],
) -> Result<(), String> {
  let mut targets : Vec<(String, PathBuf)> = claimed_export_targets (existing, config)?
    .into_iter () . map (|(id, target)|
      (format! ("existing root {}", id), PathBuf::from (format! ("{}.org", target))))
    .collect ();
  for (source_path, target) in proposed {
    let output : PathBuf = PathBuf::from (format! ("{}.org", target));
    for (owner, prior) in &targets {
      if output . starts_with (prior) || prior . starts_with (&output) {
        return Err (format! (
          "Export path conflict: {} -> {} and {} -> {}",
          source_path . display (), output . display (),
          owner, prior . display ())); } }
    targets . push ((source_path . display () . to_string (), output)); }
  Ok (())
}

fn import_record (
  id : ID,
  roots : &[ID],
  input_directory : &Path,
  host_root : Option<&Path>,
  source : &SourceName,
  time : &str,
) -> NodeComplete {
  let mut node : NodeComplete = empty_node_complete ();
  node . pid = id;
  node . title = format! ("Imported Markdown and Org from {}",
    input_directory . display ());
  node . source = source . clone ();
  node . body = Some (import_record_body (
    input_directory, host_root, source, time));
  node . contains = rel_partners_at_relSource (source, roots . to_vec ());
  node
}

fn import_record_body (
  input_directory : &Path,
  host_root : Option<&Path>,
  source : &SourceName,
  time : &str,
) -> String {
  format! (
    "Input directory: {}\nHost root: {}\nDestination source: {}\nUTC execution time: {}",
    input_directory . display (),
    host_root . map (|path| path . display () . to_string ())
      .unwrap_or_else (|| "none" . to_string ()),
    source, time)
}

fn line_at (
  text : &str,
  offset : usize,
) -> usize {
  1 + text [..offset . min (text . len ())] . bytes ()
    .filter (|byte| *byte == b'\n') . count ()
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::dbs::init::empty_in_ram_tantivy_index;
  use crate::dbs::in_rust_graph::InRustGraph;
  use crate::dbs::tantivy::background_writer::wait_for_tantivy_writes_idle;
  use crate::export_org::export_to_org;
  use crate::source_sets::{ActiveSourceSet, SourceSetName};
  use crate::types::misc::SkgfileSource;
  use std::fs;

  fn environment (
    source_directory : &Path,
    owned : bool,
  ) -> SkgEnv {
    let name : SourceName = SourceName::from ("notes");
    let config : SkgConfig = SkgConfig::dummyFromSources (HashMap::from ([
      (name . clone (), SkgfileSource {
        name,
        abbreviation : None,
        path : source_directory . to_path_buf (),
        user_owns_it : owned,
      }),
    ]));
    SkgEnv::new (config, Arc::new (InRustGraph::new ()),
      empty_in_ram_tantivy_index () . unwrap ())
  }

  #[test]
  fn imports_empty_and_nonempty_documents_as_one_additive_batch () {
    let temp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
    let input : PathBuf = temp . path () . join ("input");
    let source : PathBuf = temp . path () . join ("source");
    fs::create_dir (&input) . unwrap ();
    fs::create_dir (&source) . unwrap ();
    fs::write (input . join ("empty.md"), "") . unwrap ();
    fs::write (input . join ("notes.org"),
      "#+title: Notes\n:PROPERTIES:\n:ID: org-root\n:ROAM_ALIASES: first \"second alias\"\n:END:\n* Heading\nText\n")
      .unwrap ();
    let env : SkgEnv = environment (&source, true);
    let prepared : PreparedImportBatch = match prepare_import_batch (
      &input, &SourceName::from ("notes"), None, false, &env).unwrap () {
      ImportPreparation::Prepared (prepared) => prepared,
      ImportPreparation::HostMappingNeeded => panic! ("unexpected host prompt"),
    };
    assert_eq! (prepared . documents . len (), 2);
    let record_id : ID = prepared . record_id . clone () . unwrap ();
    let node_count : usize = prepared . nodes . len ();
    assert! (prepared . preview_report () . contains ("empty.md -> empty.org"));
    let gate = env . mutation_gate ();
    let _guard = futures::executor::block_on (gate . lock ());
    let (created, reported_id) = prepared . apply_under_mutation_gate (&env) .unwrap ();
    assert_eq! (created, node_count);
    assert_eq! (reported_id, record_id);
    assert_eq! (env . runtime_snapshot () . graph . len (), node_count);
    assert! (source . join (format! ("{}.skg", record_id)) . exists ());
    assert_eq! (fs::read_to_string (input . join ("empty.md")) . unwrap (), "");
    let runtime = env . runtime_snapshot ();
    let disk : Vec<NodeComplete> =
      existing_authoritative_nodes (&runtime . config) . unwrap ();
    ensure_runtime_matches_disk (&disk, &runtime . graph) . unwrap ();
    wait_for_tantivy_writes_idle ();
  }

  #[test]
  fn changed_input_refuses_approval_without_writes () {
    let temp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
    let input : PathBuf = temp . path () . join ("input");
    let source : PathBuf = temp . path () . join ("source");
    fs::create_dir (&input) . unwrap ();
    fs::create_dir (&source) . unwrap ();
    fs::write (input . join ("a.md"), "original") . unwrap ();
    let env : SkgEnv = environment (&source, true);
    let prepared : PreparedImportBatch = match prepare_import_batch (
      &input, &SourceName::from ("notes"), None, false, &env).unwrap () {
      ImportPreparation::Prepared (prepared) => prepared,
      ImportPreparation::HostMappingNeeded => panic! ("unexpected host prompt"),
    };
    fs::write (input . join ("added.org"), "added") . unwrap ();
    let gate = env . mutation_gate ();
    let _guard = futures::executor::block_on (gate . lock ());
    assert! (prepared . apply_under_mutation_gate (&env) .unwrap_err ()
      .contains ("Input files changed"));
    assert_eq! (fs::read_dir (&source) . unwrap () . count (), 0);
    assert_eq! (env . runtime_snapshot () . graph . len (), 0);
  }

  #[test]
  fn added_destination_file_makes_preview_stale () {
    let temp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
    let input : PathBuf = temp . path () . join ("input");
    let source : PathBuf = temp . path () . join ("source");
    fs::create_dir (&input) . unwrap ();
    fs::create_dir (&source) . unwrap ();
    fs::write (input . join ("a.md"), "original") . unwrap ();
    let env : SkgEnv = environment (&source, true);
    let prepared : PreparedImportBatch = match prepare_import_batch (
      &input, &SourceName::from ("notes"), None, false, &env).unwrap () {
      ImportPreparation::Prepared (prepared) => prepared,
      ImportPreparation::HostMappingNeeded => panic! ("unexpected host prompt"),
    };
    let foreign : PathBuf = source . join ("unrelated.skg");
    fs::write (&foreign, "external change") . unwrap ();
    let gate = env . mutation_gate ();
    let _guard = futures::executor::block_on (gate . lock ());
    assert! (prepared . apply_under_mutation_gate (&env) . unwrap_err ()
      . contains ("Configured source files changed"));
    assert_eq! (fs::read_to_string (&foreign) . unwrap (), "external change");
    assert_eq! (env . runtime_snapshot () . graph . len (), 0);
  }

  #[test]
  fn export_path_collision_and_foreign_destination_refuse_preflight () {
    let temp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
    let input : PathBuf = temp . path () . join ("input");
    let source : PathBuf = temp . path () . join ("source");
    fs::create_dir (&input) . unwrap ();
    fs::create_dir (&source) . unwrap ();
    fs::write (input . join ("guide.md"), "Markdown") . unwrap ();
    fs::write (input . join ("guide.org"), "Org") . unwrap ();
    let owned : SkgEnv = environment (&source, true);
    assert! (prepare_import_batch (
      &input, &SourceName::from ("notes"), None, false, &owned)
      .err () .unwrap () .contains ("Export path conflict"));
    let foreign : SkgEnv = environment (&source, false);
    assert! (prepare_import_batch (
      &input, &SourceName::from ("notes"), None, false, &foreign)
      .err () .unwrap () .contains ("not owned"));
    assert_eq! (fs::read_dir (&source) . unwrap () . count (), 0);
  }

  #[test]
  fn real_export_after_mixed_import_keeps_originals_and_emits_org_paths () {
    let temp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
    let input : PathBuf = temp . path () . join ("input");
    let source : PathBuf = temp . path () . join ("source");
    let output : PathBuf = temp . path () . join ("output");
    fs::create_dir (&input) . unwrap ();
    fs::create_dir (&source) . unwrap ();
    fs::create_dir (input . join ("nested")) . unwrap ();
    let markdown : &str = "# Introduction\nSee [Org](../details.org) and note[^a].\n\n[^a]: Footnote text.\n";
    let org : &str = "#+title: Details\n* Section\n[[file:nested/guide.md][Guide]]\n";
    fs::write (input . join ("nested/guide.md"), markdown) . unwrap ();
    fs::write (input . join ("details.org"), org) . unwrap ();
    let env : SkgEnv = environment (&source, true);
    let prepared : PreparedImportBatch = match prepare_import_batch (
      &input, &SourceName::from ("notes"), None, false, &env).unwrap () {
      ImportPreparation::Prepared (prepared) => prepared,
      ImportPreparation::HostMappingNeeded => panic! ("unexpected host prompt"),
    };
    assert! (! output . exists ());
    let gate = env . mutation_gate ();
    let _guard = futures::executor::block_on (gate . lock ());
    prepared . apply_under_mutation_gate (&env) . unwrap ();
    drop (_guard);
    let config = env . runtime_snapshot () . config . clone ();
    let nodes : Vec<NodeComplete> =
      read_all_skg_files_from_sources_read_only (&config) . unwrap ();
    let active : ActiveSourceSet = ActiveSourceSet::named (
      &config, SourceSetName::from ("all")) . unwrap ();
    export_to_org (&active, &nodes, &output) . unwrap ();
    let exported_markdown : String =
      fs::read_to_string (output . join ("nested/guide.org")) . unwrap ();
    let exported_org : String =
      fs::read_to_string (output . join ("details.org")) . unwrap ();
    assert! (exported_markdown . contains ("Footnote text"));
    assert! (exported_markdown . contains ("details.org"));
    assert! (! exported_markdown . contains ("target_filepath"));
    assert! (exported_org . contains ("nested/guide.org"));
    assert_eq! (fs::read_to_string (input . join ("nested/guide.md")) . unwrap (),
      markdown);
    assert_eq! (fs::read_to_string (input . join ("details.org")) . unwrap (), org);
    wait_for_tantivy_writes_idle ();
  }
}
