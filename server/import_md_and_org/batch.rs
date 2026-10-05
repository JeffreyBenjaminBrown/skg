//! Read-only preparation and under-gate revalidation of one import.

use super::{discover_documents, build::{BuiltDocument, build_document},
  parse::{ParsedDocument, line_at}, publish::prepare_import_publication,
  resolve::{contains_absolute_file_link, resolve_document_links}};
use crate::dbs::filesystem::multiple_nodes::{
  read_all_skg_files_from_skgrepos_read_only, read_skg_sections_from_folder};
use crate::dbs::in_rust_graph::{InRustGraph,
  complete_validation::complete_from_rust};
use crate::export_org::claimed_export_targets;
use crate::types::env::SkgEnv;
use crate::types::links::org_literal_ranges::HEADLINES_INSIDE_BLOCKS_EXPLANATION;
use crate::types::misc::{ID, MSV, SkgConfig, SkgRepoName};
use crate::types::nodes::complete::{Graphnode, empty_graphnode};
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
  pub destination_skgrepo : SkgRepoName,
  pub documents : Vec<ParsedDocument>,
  pub nodes : Vec<Graphnode>,
  pub record_id : Option<ID>,
  pub export_targets : Vec<(PathBuf, String)>,
  config : Arc<SkgConfig>,
  destination_evidence : BTreeMap<PathBuf, Vec<u8>>,
  /// Each document's path and the ID of its file root, which the import
  /// record links to.
  record_documents : Vec<(PathBuf, ID)>,
}

pub enum ImportPreparation {
  HostMappingNeeded,
  Prepared (PreparedImportBatch),
}

pub fn prepare_import_batch (
  input_directory : &Path,
  destination_skgrepo : &SkgRepoName,
  host_root : Option<&Path>,
  host_mapping_answered : bool,
  env : &SkgEnv,
) -> Result<ImportPreparation, String> {
  let mut new_id = || ID::new (&Uuid::new_v4 () . to_string ());
  prepare_import_batch_with (
    input_directory, destination_skgrepo, host_root,
    host_mapping_answered, env, &mut new_id)
}

pub fn prepare_import_batch_with (
  input_directory : &Path,
  destination_skgrepo : &SkgRepoName,
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
  if ! runtime . config . skgrepo_is_owned (destination_skgrepo) {
    return Err (format! ("Destination repo {} is absent or not owned",
      destination_skgrepo)); }
  let mut documents : Vec<ParsedDocument> = discover_documents (input_directory)?;
  refuse_documents_with_errors (&documents)?;
  if ! host_mapping_answered && contains_absolute_file_link (&documents) {
    return Ok (ImportPreparation::HostMappingNeeded); }
  let existing_skgids : HashMap<String, ID> =
    configured_identity_map (env)?;
  let destination_evidence : BTreeMap<PathBuf, Vec<u8>> =
    skgrepo_file_evidence (&runtime . config)?;
  let mut built : Vec<BuiltDocument> = documents . iter ()
    .map (|document| build_document (document, destination_skgrepo, new_id))
    .collect::<Result<_, _>> ()?;
  resolve_document_links (&mut documents, &mut built,
    input_directory, host_root, &existing_skgids);
  let export_targets : Vec<(PathBuf, String)> = documents . iter ()
    .zip (built . iter ())
    .map (|(document, built)|
      (document . path . clone (), built . export_target . clone ()))
    .collect ();
  let existing : Vec<Graphnode> =
    existing_authoritative_nodes (&runtime . config)?;
  ensure_runtime_matches_disk (&existing, &runtime . graph)?;
  check_export_target_conflicts (
    &export_targets, &runtime . config, &existing)?;
  let record_documents : Vec<(PathBuf, ID)> = documents . iter ()
    . zip (built . iter ())
    . map (|(document, built)| (document . path . clone (), built . root_skgid . clone ()))
    . collect ();
  let mut nodes : Vec<Graphnode> = built . into_iter ()
    .flat_map (|document| document . nodes) . collect ();
  let record_id : Option<ID> = if documents . is_empty () { None } else {
    let record_id : ID = new_id ();
    nodes . push (import_record (
      record_id . clone (), &record_documents, input_directory,
      host_root, destination_skgrepo, "on confirmation"));
    Some (record_id) };
  if ! nodes . is_empty () {
    let _ = prepare_import_publication (&nodes, env)?; }
  Ok (ImportPreparation::Prepared (PreparedImportBatch {
    input_directory : input_directory . to_path_buf (),
    host_root : host_root . map (Path::to_path_buf),
    destination_skgrepo : destination_skgrepo . clone (),
    documents, nodes, record_id, export_targets,
    config : runtime . config . clone (),
    destination_evidence,
    record_documents,
  }))
}

impl PreparedImportBatch {
  /// An Org document: a summary, warnings grouped by file, then the
  /// imported documents, Markdown ones first, since their exports will
  /// be named differently.
  pub fn preview_report (
    &self,
  ) -> String {
    let mut out : String = format! (
      "* Import preview\nImport directory: {}\nDestination repo: {} (determines privacy)\nHost root: {}\n",
      self . input_directory . display (), self . destination_skgrepo,
      self . host_root . as_ref () . map (|path| path . display () . to_string ())
        . unwrap_or_else (|| "none" . to_string ()));
    let warning_count : usize = self . documents . iter ()
      . map (|document| document . diagnostics . len ()) . sum ();
    if warning_count == 0 {
      out . push_str ("* No warnings\n");
    } else {
      out . push_str (&format! ("* Warnings ({})\n", warning_count));
      for document in self . documents . iter ()
        . filter (|document| ! document . diagnostics . is_empty ()) {
        out . push_str (&format! ("** {}\n", document . path . display ()));
        for diagnostic in &document . diagnostics {
          out . push_str (&format! ("- line {}: {}\n",
            line_at (&document . text, diagnostic . range . start),
            diagnostic . message)); }}}
    out . push_str (&format! ("* New nodes ({}, from {} documents)\n",
      self . nodes . len (), self . documents . len ()));
    let (markdown, org) : (Vec<&PathBuf>, Vec<&PathBuf>) =
      self . documents . iter () . map (|document| &document . path)
      . partition (|path|
        path . extension () . is_some_and (|extension| extension == "md"));
    out . push_str ("** Markdown documents (they will export as .org)\n");
    for path in markdown {
      out . push_str (&format! ("*** {}\n", path . display ())); }
    out . push_str ("** Org documents\n");
    for path in org {
      out . push_str (&format! ("*** {}\n", path . display ())); }
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
      ! runtime . config . skgrepo_is_owned (&self . destination_skgrepo) {
      return Err ("Configuration or repo ownership changed; preview again"
        . to_string ()); }
    let current : Vec<ParsedDocument> = discover_documents (&self . input_directory)?;
    if current . len () != self . documents . len () ||
      current . iter () . zip (self . documents . iter ())
        .any (|(now, before)| now . path != before . path ||
          now . text != before . text) {
      return Err ("Input files changed; preview again" . to_string ()); }
    if skgrepo_file_evidence (&runtime . config)? != self . destination_evidence {
      return Err ("Configured repo files changed; preview again" . to_string ()); }
    let existing : Vec<Graphnode> =
      existing_authoritative_nodes (&runtime . config)?;
    ensure_runtime_matches_disk (&existing, &runtime . graph)?;
    check_export_target_conflicts (
      &self . export_targets, &runtime . config, &existing)?;
    let record_id : ID = self . record_id . ok_or_else (||
      "An empty import has no approval token or record" . to_string ())?;
    let mut nodes : Vec<Graphnode> = self . nodes;
    let execution_time : String = OffsetDateTime::now_utc ()
      .format (&Rfc3339) . map_err (|error| error . to_string ())?;
    let record : &mut Graphnode = nodes . iter_mut ()
      .find (|node| node . pid == record_id) .unwrap ();
    record . body = Some (import_record_body (
      &self . record_documents, &self . input_directory,
      self . host_root . as_deref (), &self . destination_skgrepo,
      &execution_time));
    let prepared = prepare_import_publication (&nodes, env)?;
    let created : usize = prepared . apply_under_mutation_gate (env)?;
    Ok ((created, record_id))
  }
}

/// Some input is ambiguous enough that no import should proceed.
fn refuse_documents_with_errors (
  documents : &[ParsedDocument],
) -> Result<(), String> {
  let errors : Vec<String> = documents . iter ()
    . flat_map (|document| document . errors . iter () . map (|error|
      format! ("  {}:{}: {}", document . path . display (),
               line_at (&document . text, error . range . start),
               error . message)))
    . collect ();
  if errors . is_empty () { return Ok (()); }
  Err (format! ("{}\n{}",
    HEADLINES_INSIDE_BLOCKS_EXPLANATION, errors . join ("\n")))
}

fn configured_identity_map (
  env : &SkgEnv,
) -> Result<HashMap<String, ID>, String> {
  let runtime = env . runtime_snapshot ();
  let mut skgids : HashMap<String, ID> = HashMap::new ();
  for node in runtime . graph . nodes . values () {
    skgids . insert (node . pid . 0 . clone (), node . pid . clone ());
    for extra in &node . extra_ids {
      skgids . insert (extra . 0 . clone (), node . pid . clone ()); } }
  for skgrepo in runtime . config . ordered_skgrepos () {
    let sections = read_skg_sections_from_folder (&skgrepo, &runtime . config)
      .map_err (|error| format! ("Reading repo {}: {}", skgrepo, error))?;
    for (_, section) in sections {
      skgids . insert (section . pid . 0 . clone (), section . pid . clone ());
      for extra in &section . extra_ids {
        skgids . insert (extra . 0 . clone (), section . pid . clone ()); } } }
  Ok (skgids)
}

fn existing_authoritative_nodes (
  config : &SkgConfig,
) -> Result<Vec<Graphnode>, String> {
  read_all_skg_files_from_skgrepos_read_only (config)
    . map_err (|error| format! ("Reading configured repos: {}", error))
}

fn ensure_runtime_matches_disk (
  on_disk : &[Graphnode],
  graph : &InRustGraph,
) -> Result<(), String> {
  let disk : HashMap<ID, Graphnode> = on_disk . iter ()
    .map (|node| (node . pid . clone (),
      normalized_for_runtime_comparison (node . clone ()))) . collect ();
  let runtime : HashMap<ID, Graphnode> = graph . nodes . iter ()
    .map (|(skgid, node)| (skgid . clone (),
      normalized_for_runtime_comparison (complete_from_rust (node))))
    .collect ();
  if disk != runtime {
    return Err ("Configured repo files differ from the runtime graph; rebuild and preview again"
      . to_string ()); }
  Ok (())
}

fn normalized_for_runtime_comparison (
  mut node : Graphnode,
) -> Graphnode {
  // Folding omits an empty home alias field; the runtime graph stores the
  // same empty set as Specified([]). Neither makes an identity claim.
  if matches! (&node . aliases, MSV::Specified (values) if values . is_empty ()) {
    node . aliases = MSV::Unspecified; }
  node
}

fn skgrepo_file_evidence (
  config : &SkgConfig,
) -> Result<BTreeMap<PathBuf, Vec<u8>>, String> {
  let mut files : BTreeMap<PathBuf, Vec<u8>> = BTreeMap::new ();
  for skgrepo_name in config . ordered_skgrepos () {
    let skgrepo = config . skgrepos . get (&skgrepo_name)
      . ok_or_else (|| format! ("Configured repo {} disappeared", skgrepo_name))?;
    let entries = fs::read_dir (&skgrepo . path)
      .map_err (|error| format! ("Reading repo {}: {}", skgrepo_name, error))?;
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
  existing : &[Graphnode],
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

/// The import record links to each document's file root rather than
/// containing it, so viewing the record does not expand every document.
fn import_record (
  skgid : ID,
  documents : &[(PathBuf, ID)],
  input_directory : &Path,
  host_root : Option<&Path>,
  skgrepo : &SkgRepoName,
  time : &str,
) -> Graphnode {
  let mut node : Graphnode = empty_graphnode ();
  node . pid = skgid;
  node . title = format! ("Imported Markdown and Org from {}",
    input_directory . display ());
  node . home_skgrepo = skgrepo . clone ();
  node . body = Some (import_record_body (
    documents, input_directory, host_root, skgrepo, time));
  node
}

/// Labels are paths, not titles: a title can contain a link, which a
/// link label cannot.
fn import_record_body (
  documents : &[(PathBuf, ID)],
  input_directory : &Path,
  host_root : Option<&Path>,
  skgrepo : &SkgRepoName,
  time : &str,
) -> String {
  let mut body : String = format! (
    "Input directory: {}\nHost root: {}\nDestination repo: {}\nUTC execution time: {}\n\nImported documents:",
    input_directory . display (),
    host_root . map (|path| path . display () . to_string ())
      .unwrap_or_else (|| "none" . to_string ()),
    skgrepo, time);
  for (path, root) in documents {
    body . push_str (&format! ("\n- [[id:{}][{}]]", root, path . display ())); }
  body
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::dbs::init::empty_in_ram_tantivy_index;
  use crate::dbs::in_rust_graph::InRustGraph;
  use crate::dbs::tantivy::background_writer::wait_for_tantivy_writes_idle;
  use crate::dbs::tantivy::search::{SearchOptions, search_index};
  use crate::export_org::export_to_org;
  use crate::skgrepo_sets::{ActiveSkgRepoSet, SkgRepoSetName};
  use crate::types::misc::SkgRepo;
  use std::fs;

  fn environment (
    skgrepo_directory : &Path,
    owned : bool,
  ) -> SkgEnv {
    let name : SkgRepoName = SkgRepoName::from ("notes");
    let config : SkgConfig = SkgConfig::dummyFromSkgRepos (HashMap::from ([
      (name . clone (), SkgRepo {
        name,
        abbreviation : None,
        path         : skgrepo_directory . to_path_buf (),
        owned        : owned,
      }),
    ]));
    SkgEnv::new (config, Arc::new (InRustGraph::new ()),
      empty_in_ram_tantivy_index () . unwrap ())
  }

  #[test]
  fn imports_empty_and_nonempty_documents_as_one_additive_batch () {
    let temp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
    let input : PathBuf = temp . path () . join ("input");
    let skgrepo : PathBuf = temp . path () . join ("repo");
    fs::create_dir (&input) . unwrap ();
    fs::create_dir (&skgrepo) . unwrap ();
    fs::write (input . join ("empty.md"), "") . unwrap ();
    fs::write (input . join ("notes.org"),
      "#+title: Notes\n:PROPERTIES:\n:ID: org-root\n:ROAM_ALIASES: first \"second alias\"\n:END:\n* Heading\nText\n")
      .unwrap ();
    let env      : SkgEnv = environment (&skgrepo, true);
    let prepared : PreparedImportBatch = match prepare_import_batch (
      &input, &SkgRepoName::from ("notes"), None, false, &env).unwrap () {
      ImportPreparation::Prepared (prepared) => prepared,
      ImportPreparation::HostMappingNeeded => panic! ("unexpected host prompt"),
    };
    assert_eq! (prepared . documents . len (), 2);
    let record_id : ID = prepared . record_id . clone () . unwrap ();
    { // The record links to each document's file root; it contains none.
      let record : &Graphnode = prepared . nodes . iter ()
        . find (|node| node . pid == record_id) . unwrap ();
      let body : &str = record . body . as_deref () . unwrap ();
      assert! (record . contains . is_empty ());
      assert! (body . contains ("][empty.md]]") && body . contains ("][notes.org]]"),
               "{}", body); }
    let node_count : usize = prepared . nodes . len ();
    let report : String = prepared . preview_report ();
    assert! (report . starts_with ("* Import preview\n"), "{}", report);
    assert! (report . contains (
      "* No warnings\n* New nodes (6, from 2 documents)\n** Markdown documents (they will export as .org)\n*** empty.md\n** Org documents\n*** notes.org\n"),
      "{}", report);
    let gate = env . mutation_gate ();
    let _guard = futures::executor::block_on (gate . lock ());
    let (created, reported_id) = prepared . apply_under_mutation_gate (&env) .unwrap ();
    assert_eq! (created, node_count);
    assert_eq! (reported_id, record_id);
    assert_eq! (env . runtime_snapshot () . graph . len (), node_count);
    assert! (skgrepo . join (format! ("{}.skg", record_id)) . exists ());
    assert_eq! (fs::read_to_string (input . join ("empty.md")) . unwrap (), "");
    let runtime = env . runtime_snapshot ();
    let disk : Vec<Graphnode> =
      existing_authoritative_nodes (&runtime . config) . unwrap ();
    ensure_runtime_matches_disk (&disk, &runtime . graph) . unwrap ();
    wait_for_tantivy_writes_idle ();
    let (hits, _) = search_index (
      &env . runtime_snapshot () . tantivy_index,
      "Notes", &SearchOptions::default ()) . unwrap ();
    assert! (! hits . is_empty (), "imported title is searchable after search-index drain");
  }

  #[test]
  fn heading_inside_a_block_refuses_the_preview () {
    let temp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
    let input : PathBuf = temp . path () . join ("input");
    let skgrepo : PathBuf = temp . path () . join ("repo");
    fs::create_dir (&input) . unwrap ();
    fs::create_dir (&skgrepo) . unwrap ();
    fs::write (input . join ("bad.org"),
      "* Top\n#+begin_src\n* inside\n#+end_src\n") . unwrap ();
    let env   : SkgEnv = environment (&skgrepo, true);
    let error : String = prepare_import_batch (
      &input, &SkgRepoName::from ("notes"), None, false, &env)
      . err () . unwrap ();
    assert! (error . starts_with ("Nothing was imported."), "{}", error);
    assert! (error . contains ("bad.org:3: \"* inside\""), "{}", error);
  }

  #[test]
  fn changed_input_refuses_approval_without_writes () {
    let temp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
    let input : PathBuf = temp . path () . join ("input");
    let skgrepo : PathBuf = temp . path () . join ("repo");
    fs::create_dir (&input) . unwrap ();
    fs::create_dir (&skgrepo) . unwrap ();
    fs::write (input . join ("a.md"), "original") . unwrap ();
    let env      : SkgEnv = environment (&skgrepo, true);
    let prepared : PreparedImportBatch = match prepare_import_batch (
      &input, &SkgRepoName::from ("notes"), None, false, &env).unwrap () {
      ImportPreparation::Prepared (prepared) => prepared,
      ImportPreparation::HostMappingNeeded => panic! ("unexpected host prompt"),
    };
    fs::write (input . join ("added.org"), "added") . unwrap ();
    let gate = env . mutation_gate ();
    let _guard = futures::executor::block_on (gate . lock ());
    assert! (prepared . apply_under_mutation_gate (&env) .unwrap_err ()
      .contains ("Input files changed"));
    assert_eq! (fs::read_dir (&skgrepo) . unwrap () . count (), 0);
    assert_eq! (env . runtime_snapshot () . graph . len (), 0);
  }

  #[test]
  fn added_destination_file_makes_preview_stale () {
    let temp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
    let input : PathBuf = temp . path () . join ("input");
    let skgrepo : PathBuf = temp . path () . join ("repo");
    fs::create_dir (&input) . unwrap ();
    fs::create_dir (&skgrepo) . unwrap ();
    fs::write (input . join ("a.md"), "original") . unwrap ();
    let env      : SkgEnv = environment (&skgrepo, true);
    let prepared : PreparedImportBatch = match prepare_import_batch (
      &input, &SkgRepoName::from ("notes"), None, false, &env).unwrap () {
      ImportPreparation::Prepared (prepared) => prepared,
      ImportPreparation::HostMappingNeeded => panic! ("unexpected host prompt"),
    };
    let foreign : PathBuf = skgrepo . join ("unrelated.skg");
    fs::write (&foreign, "external change") . unwrap ();
    let gate = env . mutation_gate ();
    let _guard = futures::executor::block_on (gate . lock ());
    assert! (prepared . apply_under_mutation_gate (&env) . unwrap_err ()
      . contains ("Configured repo files changed"));
    assert_eq! (fs::read_to_string (&foreign) . unwrap (), "external change");
    assert_eq! (env . runtime_snapshot () . graph . len (), 0);
  }

  #[test]
  fn export_path_collision_and_foreign_destination_refuse_preflight () {
    let temp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
    let input : PathBuf = temp . path () . join ("input");
    let skgrepo : PathBuf = temp . path () . join ("repo");
    fs::create_dir (&input) . unwrap ();
    fs::create_dir (&skgrepo) . unwrap ();
    fs::write (input . join ("guide.md"), "Markdown") . unwrap ();
    fs::write (input . join ("guide.org"), "Org") . unwrap ();
    let owned : SkgEnv = environment (&skgrepo, true);
    assert! (prepare_import_batch (
      &input, &SkgRepoName::from ("notes"), None, false, &owned)
      .err () .unwrap () .contains ("Export path conflict"));
    let foreign : SkgEnv = environment (&skgrepo, false);
    assert! (prepare_import_batch (
      &input, &SkgRepoName::from ("notes"), None, false, &foreign)
      .err () .unwrap () .contains ("not owned"));
    assert_eq! (fs::read_dir (&skgrepo) . unwrap () . count (), 0);
  }

  #[test]
  fn real_export_after_mixed_import_keeps_originals_and_emits_org_paths () {
    let temp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
    let input : PathBuf = temp . path () . join ("input");
    let skgrepo : PathBuf = temp . path () . join ("repo");
    let output : PathBuf = temp . path () . join ("output");
    fs::create_dir (&input) . unwrap ();
    fs::create_dir (&skgrepo) . unwrap ();
    fs::create_dir (input . join ("nested")) . unwrap ();
    let markdown : &str = "# Introduction\nSee [Org](../details.org) and note[^a].\n\n[^a]: Footnote text.\n";
    let org : &str = "#+title: Details\n* Section\n[[file:nested/guide.md][Guide]]\n";
    fs::write (input . join ("nested/guide.md"), markdown) . unwrap ();
    fs::write (input . join ("details.org"), org) . unwrap ();
    let env      : SkgEnv = environment (&skgrepo, true);
    let prepared : PreparedImportBatch = match prepare_import_batch (
      &input, &SkgRepoName::from ("notes"), None, false, &env).unwrap () {
      ImportPreparation::Prepared (prepared) => prepared,
      ImportPreparation::HostMappingNeeded => panic! ("unexpected host prompt"),
    };
    assert! (! output . exists ());
    let gate = env . mutation_gate ();
    let _guard = futures::executor::block_on (gate . lock ());
    prepared . apply_under_mutation_gate (&env) . unwrap ();
    drop (_guard);
    let config = env . runtime_snapshot () . config . clone ();
    let nodes : Vec<Graphnode> =
      read_all_skg_files_from_skgrepos_read_only (&config) . unwrap ();
    let active : ActiveSkgRepoSet = ActiveSkgRepoSet::named (
      &config, SkgRepoSetName::from ("all")) . unwrap ();
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
