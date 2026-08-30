//! Durable evidence and Git-plumbing recovery for fatal reload incidents.

use crate::git_ops::read_repo::open_repo;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  send_response_with_length_prefix,
  tag_terminal_sexp_response,
  tag_terminal_text_response,
  value_from_request_sexp,
};
use crate::types::misc::{ID, SkgConfig, SourceName};

use futures::executor::block_on;
use git2::{
  IndexEntry, IndexTime, ObjectType, Oid, Repository, RepositoryState,
  Signature,
};
use serde::{Deserialize, Serialize};
use sexp::{Atom, Sexp};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fs;
use std::io;
use std::net::TcpStream;
use std::path::{Path, PathBuf};
use std::sync::{Mutex, OnceLock};
use walkdir::{DirEntry, WalkDir};

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct SnapshotPath {
  pub absolute      : PathBuf,
  pub repo_relative : PathBuf,
  pub source        : Option<SourceName>,
  pub owned         : bool,
  pub bytes         : Option<Vec<u8>>,
  pub digest        : Option<String>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct RepositorySnapshot {
  pub root          : PathBuf,
  pub git_directory : Option<PathBuf>,
  pub base_commit   : Option<String>,
  pub head_symbolic : Option<String>,
  pub state         : String,
  pub detached      : bool,
  pub has_owned_source : bool,
  pub paths         : Vec<SnapshotPath>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct IncidentDiskSnapshot {
  pub repositories : Vec<RepositorySnapshot>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RecoveryDraft {
  pub fatal          : Vec<(ID, String)>,
  pub touched_pids   : Vec<ID>,
  pub pre_manifest   : BTreeMap<PathBuf, Option<Vec<u8>>>,
  pub legal_manifest : BTreeMap<PathBuf, Option<Vec<u8>>>,
  pub disk_snapshot  : IncidentDiskSnapshot,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PendingRecoveryIncident {
  pub incident_id : String,
  pub draft       : RecoveryDraft,
  pub journal     : PathBuf,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RecoveredRepository {
  pub root        : PathBuf,
  pub pre_ref     : String,
  pub legal_ref   : String,
  pub incident_ref : String,
  pub pre_commit  : String,
  pub legal_commit : String,
  pub incident_commit : String,
  pub initialized : bool,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RecoveryReport {
  pub incident_id : String,
  pub sequence    : usize,
  pub repositories : Vec<RecoveredRepository>,
  pub restored_paths : Vec<PathBuf>,
  pub warnings    : Vec<String>,
}

pub fn handle_reload_recovery_request (
  stream  : &mut TcpStream,
  request : &str,
  config  : &SkgConfig,
) {
  let incident_id = match value_from_request_sexp ("incident-id", request) {
    Ok (incident) => incident,
    Err (error) => {
      send_response_with_length_prefix (stream, &tag_terminal_text_response (
        TcpToClient::ReloadRecovery, "failed", &error));
      return; }};
  if value_from_request_sexp ("approved", request) . as_deref () != Ok ("true") {
    send_response_with_length_prefix (stream, &tag_terminal_text_response (
      TcpToClient::ReloadRecovery, "failed",
      "Recovery requires explicit client approval"));
    return; }
  let _write_guard = block_on (
    crate::write_lock::acquire_graph_write_lock ());
  match recover_incident (config, &incident_id) {
    Ok (report) => {
      let payload = recovery_report_sexp (&report) . to_string ();
      send_response_with_length_prefix (stream, &tag_terminal_sexp_response (
        TcpToClient::ReloadRecovery, "complete", &payload)); }
    Err (error) => send_response_with_length_prefix (
      stream, &tag_terminal_text_response (
        TcpToClient::ReloadRecovery, "failed", &error)),
  }
}

fn recovery_report_sexp (report : &RecoveryReport) -> Sexp {
  let atom = |value : &str| Sexp::Atom (Atom::S (value . into ()));
  let field = |name : &str, value : Sexp| Sexp::List (vec![atom (name), value]);
  let repositories = report . repositories . iter () . map (|repo| Sexp::List (
    vec![
      field ("root", atom (&repo . root . to_string_lossy ())),
      field ("pre-ref", atom (&repo . pre_ref)),
      field ("legal-ref", atom (&repo . legal_ref)),
      field ("incident-ref", atom (&repo . incident_ref)),
      field ("pre-commit", atom (&repo . pre_commit)),
      field ("legal-commit", atom (&repo . legal_commit)),
      field ("incident-commit", atom (&repo . incident_commit)),
      field ("initialized", atom (if repo . initialized { "true" } else { "false" })),
    ])) . collect ();
  Sexp::List (vec![
    field ("content", atom (&format! (
      "Recovered fatal reload incident {} into oops-{} refs and restored {} owned path(s).",
      report . incident_id, report . sequence, report . restored_paths . len ()))),
    field ("repositories", Sexp::List (repositories)),
    field ("restored-paths", Sexp::List (report . restored_paths . iter ()
      . map (|path| atom (&path . to_string_lossy ())) . collect ())),
    field ("warnings", Sexp::List (report . warnings . iter ()
      . map (|warning| atom (warning)) . collect ())),
  ])
}

static INCIDENTS : OnceLock<Mutex<HashMap<String, PendingRecoveryIncident>>> =
  OnceLock::new ();

fn incidents () -> &'static Mutex<HashMap<String, PendingRecoveryIncident>> {
  INCIDENTS . get_or_init (|| Mutex::new (HashMap::new ()))
}

impl IncidentDiskSnapshot {
  pub fn capture (
    config       : &SkgConfig,
    touched_pids : &[ID],
  ) -> Result<Self, String> {
    let mut repositories : BTreeMap<PathBuf, RepoCapture> = BTreeMap::new ();
    for source_name in config . ordered_sources () {
      let source = config . sources . get (&source_name)
        . expect ("ordered source exists");
      let repo = open_repo (&source . path);
      let (root, git_directory) = match repo . as_ref () {
        Some (repo) => (
          repo . workdir () . ok_or_else (|| format! (
            "Source repository {} has no worktree", source . path . display ()))?
            . to_path_buf (),
          Some (repo . path () . to_path_buf ())),
        None => (source . path . clone (), None),
      };
      let root = root . canonicalize () . unwrap_or (root);
      let capture = repositories . entry (root . clone ())
        . or_insert_with (|| RepoCapture {
          root,
          git_directory,
          sources: Vec::new (),
        });
      capture . sources . push ((
        source_name,
        source . path . clone (),
        source . user_owns_it));
    }
    let mut snapshots = Vec::new ();
    for capture in repositories . values () {
      snapshots . push (capture_repository (capture, touched_pids)?); }
    snapshots . sort_by (|a, b| a . root . cmp (&b . root));
    Ok (Self { repositories: snapshots })
  }

  /// Exact-byte comparison, including newly created or deleted `.skg` paths.
  pub fn revalidate (
    &self,
    config       : &SkgConfig,
    touched_pids : &[ID],
  ) -> Result<(), String> {
    let actual = Self::capture (config, touched_pids)?;
    if &actual != self {
      return Err (changed_snapshot_summary (self, &actual)); }
    Ok (( ))
  }
}

struct RepoCapture {
  root          : PathBuf,
  git_directory : Option<PathBuf>,
  sources       : Vec<(SourceName, PathBuf, bool)>,
}

fn capture_repository (
  capture      : &RepoCapture,
  touched_pids : &[ID],
) -> Result<RepositorySnapshot, String> {
  let repo = capture . git_directory . as_ref ()
    . and_then (|directory| Repository::open (directory) . ok ());
  let base_commit = repo . as_ref () . and_then (|repo|
    repo . head () . ok () . and_then (|head|
      head . peel_to_commit () . ok () . map (|commit| commit . id () . to_string ()))) ;
  // `Repository::head` fails for an unborn branch. Reading HEAD as a
  // reference preserves that branch name so recovery can create its root
  // commit without silently switching it to the user's Git default.
  let head_symbolic = repo . as_ref () . and_then (|repo|
    repo . find_reference ("HEAD") . ok () . and_then (|head|
      head . symbolic_target () . map (str::to_string)));
  let state = repo . as_ref ()
    . map (|repo| format! ("{:?}", repo . state ()))
    . unwrap_or_else (|| "NotARepository" . into ());
  let detached = repo . as_ref ()
    . and_then (|repo| repo . head_detached () . ok ())
    . unwrap_or (false);
  let mut relative_paths = worktree_skg_paths (&capture . root)?;
  if let Some (repo) = &repo {
    relative_paths . extend (head_skg_paths (repo)?); }
  for (_, source_path, _) in &capture . sources {
    let source_relative = source_path . strip_prefix (&capture . root)
      . map_err (|_| format! (
        "Source {} is outside repository {}",
        source_path . display (), capture . root . display ()))?;
    for pid in touched_pids {
      relative_paths . insert (
        source_relative . join (format! ("{}.skg", pid)) ); }
  }
  let mut paths = Vec::new ();
  for relative in relative_paths {
    let absolute = capture . root . join (&relative);
    let bytes = read_optional_regular_file (&absolute)
      . map_err (|error| format! ("{}: {}", absolute . display (), error))?;
    let (source, owned) = capture . sources . iter () . find_map (
      |(name, directory, owned)| {
        (absolute . parent () == Some (directory . as_path ()))
          . then (|| (Some (name . clone ()), *owned))
      }) . unwrap_or ((None, false));
    let digest = bytes . as_ref ()
      . map (|bytes| blake3::hash (bytes) . to_hex () . to_string ());
    paths . push (SnapshotPath {
      absolute, repo_relative: relative, source, owned, bytes, digest });
  }
  paths . sort_by (|a, b| a . repo_relative . cmp (&b . repo_relative));
  Ok (RepositorySnapshot {
    root: capture . root . clone (),
    git_directory: capture . git_directory . clone (),
    base_commit,
    head_symbolic,
    state,
    detached,
    has_owned_source: capture . sources . iter () . any (|(_, _, owned)| *owned),
    paths,
  })
}

fn worktree_skg_paths (root : &Path) -> Result<BTreeSet<PathBuf>, String> {
  let mut result = BTreeSet::new ();
  let walker = WalkDir::new (root) . follow_links (false) . into_iter ()
    . filter_entry (|entry| descend_into (root, entry));
  for entry in walker {
    let entry = entry . map_err (|error| error . to_string ())?;
    if !entry . file_type () . is_file ()
       || entry . path () . extension () . and_then (|part| part . to_str ())
          != Some ("skg")
    { continue; }
    result . insert (entry . path () . strip_prefix (root)
      . expect ("walked entry is below root") . to_path_buf ());
  }
  Ok (result)
}

fn descend_into (root : &Path, entry : &DirEntry) -> bool {
  if entry . path () == root { return true; }
  if entry . file_name () == ".git" { return false; }
  if entry . file_type () . is_dir ()
     && entry . path () . join (".git") . exists ()
  { return false; }
  true
}

fn head_skg_paths (repo : &Repository) -> Result<BTreeSet<PathBuf>, String> {
  let mut result = BTreeSet::new ();
  let Ok (head) = repo . head () else { return Ok (result); };
  let tree = head . peel_to_tree () . map_err (|error| error . to_string ())?;
  tree . walk (git2::TreeWalkMode::PreOrder, |prefix, entry| {
    if entry . kind () == Some (ObjectType::Commit) {
      return git2::TreeWalkResult::Skip; }
    if entry . kind () == Some (ObjectType::Blob) {
      if let Some (name) = entry . name () {
        let path = PathBuf::from (prefix) . join (name);
        if path . extension () . and_then (|part| part . to_str ())
           == Some ("skg")
        { result . insert (path); }
      }
    }
    git2::TreeWalkResult::Ok
  }) . map_err (|error| error . to_string ())?;
  Ok (result)
}

fn read_optional_regular_file (path : &Path) -> io::Result<Option<Vec<u8>>> {
  match fs::symlink_metadata (path) {
    Ok (metadata) if metadata . file_type () . is_file () =>
      fs::read (path) . map (Some),
    Ok (_) => Ok (None),
    Err (error) if error . kind () == io::ErrorKind::NotFound => Ok (None),
    Err (error) => Err (error),
  }
}

fn changed_snapshot_summary (
  expected : &IncidentDiskSnapshot,
  actual   : &IncidentDiskSnapshot,
) -> String {
  let flatten = |snapshot : &IncidentDiskSnapshot| snapshot . repositories
    . iter () . flat_map (|repo| repo . paths . iter ())
    . map (|path| (path . absolute . clone (), path . digest . clone ()))
    . collect::<BTreeMap<_, _>> ();
  let expected = flatten (expected);
  let actual = flatten (actual);
  let paths : BTreeSet<PathBuf> = expected . keys () . cloned ()
    . chain (actual . keys () . cloned ()) . collect ();
  let changed = paths . into_iter () . filter (|path|
    expected . get (path) != actual . get (path))
    . map (|path| path . display () . to_string ()) . collect::<Vec<_>> ();
  format! ("Recovery stopped because disk changed after the incident: {}",
           changed . join (", "))
}

struct RecoveryRepositoryPlan {
  snapshot    : RepositorySnapshot,
  repo        : Repository,
  initialized : bool,
  branch_ref  : Option<String>,
  pre_ref     : String,
  legal_ref   : String,
  incident_ref : String,
  pre_commit  : Oid,
  legal_commit : Oid,
  incident_commit : Oid,
}

/// Materialize one confirmed fatal incident without checkout or index writes.
/// Object construction is harmless to the worktree; refs are published only
/// after object construction succeeds in every participating repository.
pub fn recover_incident (
  config      : &SkgConfig,
  incident_id : &str,
) -> Result<RecoveryReport, String> {
  let pending = pending_incident (incident_id) . ok_or_else (|| format! (
    "No unresolved recovery incident named '{}'", incident_id)) ?;
  pending . draft . disk_snapshot . revalidate (
    config, &pending . draft . touched_pids) ?;

  let mut warnings = Vec::new ();
  let mut repositories : Vec<(RepositorySnapshot, Repository, bool, Option<String>)> =
    Vec::new ();
  for snapshot in &pending . draft . disk_snapshot . repositories {
    if !snapshot . has_owned_source {
      warnings . push (format! (
        "{} participates only through non-owned sources; no refs or files were changed there",
        snapshot . root . display ()));
      continue; }
    if snapshot . git_directory . is_some ()
       && !repository_is_safe_to_recover (snapshot)
    {
      warnings . push (format! (
        "{} was detached or in Git state {}; it was preserved unchanged",
        snapshot . root . display (), snapshot . state));
      continue; }
    let (repo, initialized) = match &snapshot . git_directory {
      Some (directory) => (
        Repository::open (directory) . map_err (|error| format! (
          "Could not reopen {}: {}", snapshot . root . display (), error)) ?,
        false),
      None => {
        let repo = Repository::init (&snapshot . root) . map_err (|error| format! (
          "Could not initialize recovery repository {}: {}",
          snapshot . root . display (), error)) ?;
        repo . set_head ("refs/heads/main") . map_err (|error| format! (
          "Could not give new repository {} symbolic HEAD main: {}",
          snapshot . root . display (), error)) ?;
        (repo, true)
      }};
    let branch_ref = if snapshot . base_commit . is_none () {
      snapshot . head_symbolic . clone ()
        . or_else (|| initialized . then (|| "refs/heads/main" . into ()))
    } else { None };
    repositories . push ((snapshot . clone (), repo, initialized, branch_ref));
  }
  if repositories . is_empty () {
    return Err (format! (
      "Incident {} has no owned repository in a safe state; nothing was changed. {}",
      incident_id, warnings . join (" ")));
  }

  let sequence = first_common_unused_sequence (&repositories) ?;
  let (pre_ref, legal_ref, incident_ref) = recovery_ref_names (sequence);
  let mut plans = Vec::new ();
  for (snapshot, repo, initialized, branch_ref) in repositories {
    let pre_commit = recovery_commit (
      &repo, &snapshot, &pending . draft . pre_manifest,
      None, "Skg recovery: pre-incident graph state") ?;
    let legal_commit = recovery_commit (
      &repo, &snapshot, &pending . draft . legal_manifest,
      Some (pre_commit), "Skg recovery: legal changes from incident") ?;
    let incident_commit = recovery_commit (
      &repo, &snapshot, &BTreeMap::new (),
      Some (pre_commit), "Skg recovery: all changes from incident") ?;
    plans . push (RecoveryRepositoryPlan {
      snapshot, repo, initialized, branch_ref,
      pre_ref: pre_ref . clone (), legal_ref: legal_ref . clone (),
      incident_ref: incident_ref . clone (),
      pre_commit, legal_commit, incident_commit,
    });
  }

  // Object construction can be appreciable. Recheck the complete overlay at
  // its last mutation-free boundary; if it moved, no ref or file is changed.
  revalidate_overlay (
    &pending . draft . disk_snapshot, config,
    &pending . draft . touched_pids) ?;
  publish_recovery_refs (&plans) ?;

  // The topology is deliberately two branches from P, not a linear history:
  //
  // base commit C
  //       |
  //       P  oops-N_1-pre-incident
  //      / \
  //     L   A
  //     |   |
  //     |   oops-N_3-with-all-changes-from-incident
  //     |
  //     oops-N_2-with-only-legal-changes-from-incident
  //
  // Therefore P..L is the accepted reload, P..A is the complete incident,
  // and L..A is the part which could not safely enter Skg.
  revalidate_overlay (
    &pending . draft . disk_snapshot, config,
    &pending . draft . touched_pids) . map_err (|error| format! (
      "Recovery refs were preserved, but files were not restored because {}",
      error)) ?;
  let participating_roots : Vec<PathBuf> = plans . iter ()
    . map (|plan| plan . snapshot . root . clone ()) . collect ();
  let restored_paths = restore_owned_fatal_paths (
    config, &pending . draft, &participating_roots) ?;

  let recovered = plans . iter () . map (|plan| RecoveredRepository {
    root: plan . snapshot . root . clone (),
    pre_ref: plan . pre_ref . clone (),
    legal_ref: plan . legal_ref . clone (),
    incident_ref: plan . incident_ref . clone (),
    pre_commit: plan . pre_commit . to_string (),
    legal_commit: plan . legal_commit . to_string (),
    incident_commit: plan . incident_commit . to_string (),
    initialized: plan . initialized,
  }) . collect ();
  retire_incident (incident_id) ?;
  Ok (RecoveryReport {
    incident_id: incident_id . into (), sequence,
    repositories: recovered, restored_paths, warnings,
  })
}

/// Recheck only the exact `.skg` overlay after recovery has itself created
/// objects, refs, or a repository. The initial check above compares repository
/// identity/state too; later checks must not reject Skg's own Git metadata.
fn revalidate_overlay (
  expected     : &IncidentDiskSnapshot,
  config       : &SkgConfig,
  touched_pids : &[ID],
) -> Result<(), String> {
  let actual = IncidentDiskSnapshot::capture (config, touched_pids)?;
  let flatten = |snapshot : &IncidentDiskSnapshot| snapshot . repositories
    . iter () . flat_map (|repo| repo . paths . iter ())
    . map (|path| (path . absolute . clone (), path . bytes . clone ()))
    . collect::<BTreeMap<_, _>> ();
  if flatten (expected) != flatten (&actual) {
    return Err (changed_snapshot_summary (expected, &actual)); }
  Ok (( ))
}

fn recovery_ref_names (sequence : usize) -> (String, String, String) {
  (
    format! ("refs/heads/oops-{}_1-pre-incident", sequence),
    format! ("refs/heads/oops-{}_2-with-only-legal-changes-from-incident",
             sequence),
    format! ("refs/heads/oops-{}_3-with-all-changes-from-incident", sequence),
  )
}

fn first_common_unused_sequence (
  repositories : &[(RepositorySnapshot, Repository, bool, Option<String>)],
) -> Result<usize, String> {
  for sequence in 1..=usize::MAX {
    let names = recovery_ref_names (sequence);
    if repositories . iter () . all (|(_, repo, _, _)|
      [&names . 0, &names . 1, &names . 2] . into_iter ()
        . all (|name| repo . find_reference (name) . is_err ()))
    { return Ok (sequence); }
  }
  Err ("No common unused recovery-ref number exists" . into ())
}

fn recovery_commit (
  repo      : &Repository,
  snapshot  : &RepositorySnapshot,
  overrides : &BTreeMap<PathBuf, Option<Vec<u8>>>,
  parent    : Option<Oid>,
  message   : &str,
) -> Result<Oid, String> {
  let tree_id = recovery_tree (repo, snapshot, overrides) ?;
  let tree = repo . find_tree (tree_id) . map_err (|error| error . to_string ())?;
  let signature = repo . signature () . or_else (|_|
    Signature::now ("Skg recovery", "skg-recovery@localhost"))
    . map_err (|error| error . to_string ())?;
  let parent_id = parent . or_else (|| snapshot . base_commit . as_ref ()
    . and_then (|id| Oid::from_str (id) . ok ()));
  let parent_commit = parent_id . map (|id| repo . find_commit (id))
    . transpose () . map_err (|error| format! (
      "Could not find recovery parent in {}: {}",
      snapshot . root . display (), error))?;
  let parents : Vec<&git2::Commit<'_>> = parent_commit . iter () . collect ();
  repo . commit (None, &signature, &signature, message, &tree, &parents)
    . map_err (|error| format! (
      "Could not construct recovery commit in {}: {}",
      snapshot . root . display (), error))
}

fn recovery_tree (
  repo      : &Repository,
  snapshot  : &RepositorySnapshot,
  overrides : &BTreeMap<PathBuf, Option<Vec<u8>>>,
) -> Result<Oid, String> {
  // `repo.index()` is only an in-memory staging area here. We intentionally
  // never call Index::write, so the user's real index remains byte-for-byte
  // untouched; write_tree_to stores only tree/blob objects.
  let mut index = repo . index () . map_err (|error| error . to_string ())?;
  match &snapshot . base_commit {
    Some (base) => {
      let commit = repo . find_commit (Oid::from_str (base)
        . map_err (|error| error . to_string ())?)
        . map_err (|error| error . to_string ())?;
      let tree = commit . tree () . map_err (|error| error . to_string ())?;
      index . read_tree (&tree) . map_err (|error| error . to_string ())?; }
    None => index . clear () . map_err (|error| error . to_string ())?,
  }
  let mut overlay : BTreeMap<PathBuf, Option<Vec<u8>>> = snapshot . paths
    . iter () . map (|path| (
      path . repo_relative . clone (),
      overrides . get (&path . absolute) . cloned ()
        . unwrap_or_else (|| path . bytes . clone ())))
    . collect ();
  for (absolute, bytes) in overrides {
    if let Ok (relative) = absolute . strip_prefix (&snapshot . root) {
      overlay . insert (relative . to_path_buf (), bytes . clone ()); }
  }
  for (path, bytes) in overlay {
    match bytes {
      Some (bytes) => {
        let entry = plain_blob_index_entry (&path);
        index . add_frombuffer (&entry, &bytes)
          . map_err (|error| format! (
            "Could not add {} to recovery tree: {}", path . display (), error))?; }
      None => {
        if index . get_path (&path, 0) . is_some () {
          index . remove_path (&path) . map_err (|error| format! (
            "Could not remove {} from recovery tree: {}",
            path . display (), error))?; }} }
  }
  index . write_tree_to (repo) . map_err (|error| error . to_string ())
}

fn plain_blob_index_entry (path : &Path) -> IndexEntry {
  let path = path . to_string_lossy () . replace ('\\', "/") . into_bytes ();
  IndexEntry {
    ctime: IndexTime::new (0, 0), mtime: IndexTime::new (0, 0),
    dev: 0, ino: 0, mode: 0o100644, uid: 0, gid: 0,
    file_size: 0, id: Oid::zero (), flags: 0, flags_extended: 0,
    path,
  }
}

fn publish_recovery_refs (plans : &[RecoveryRepositoryPlan]) -> Result<(), String> {
  let mut published : Vec<(usize, String)> = Vec::new ();
  let result = (|| -> Result<(), String> {
    for (index, plan) in plans . iter () . enumerate () {
      for (name, target) in [
        (&plan . pre_ref, plan . pre_commit),
        (&plan . legal_ref, plan . legal_commit),
        (&plan . incident_ref, plan . incident_commit),
      ] {
        plan . repo . reference (name, target, false, "Skg fatal reload recovery")
          . map_err (|error| format! (
            "Could not publish {} in {}: {}",
            name, plan . snapshot . root . display (), error))?;
        published . push ((index, name . clone ())); }
      if let Some (branch) = &plan . branch_ref {
        plan . repo . reference (
          branch, plan . pre_commit, false, "Skg recovery root")
          . map_err (|error| format! (
            "Could not publish unborn branch {} in {}: {}",
            branch, plan . snapshot . root . display (), error))?;
        published . push ((index, branch . clone ())); }
    }
    Ok (( ))
  })();
  if let Err (error) = result {
    let mut rollback_errors = Vec::new ();
    for (index, name) in published . into_iter () . rev () {
      if let Err (rollback) = plans[index] . repo . find_reference (&name)
        . and_then (|mut reference| reference . delete ())
      {
        rollback_errors . push (format! ("{}: {}", name, rollback)); }
    }
    return Err (if rollback_errors . is_empty () { error } else { format! (
      "{}; additionally failed to remove newly published refs: {}",
      error, rollback_errors . join (", ")) });
  }
  Ok (( ))
}

fn restore_owned_fatal_paths (
  config              : &SkgConfig,
  draft               : &RecoveryDraft,
  participating_roots : &[PathBuf],
) -> Result<Vec<PathBuf>, String> {
  let fatal_pids : BTreeSet<ID> = draft . fatal . iter ()
    . map (|(pid, _)| pid . clone ()) . collect ();
  let mut restore : Vec<(PathBuf, Option<Vec<u8>>)> = draft . legal_manifest
    . iter () . filter (|(path, _)| {
      config . sources . source_and_pid_for_direct_path (path)
        .map (|(source, pid)| config . user_owns_source (&source)
          && fatal_pids . contains (&pid)
          && participating_roots . iter () . any (|root| path . starts_with (root)))
        . unwrap_or (false)
    }) . map (|(path, bytes)| (path . clone (), bytes . clone ())) . collect ();
  restore . sort_by (|a, b| a . 0 . cmp (&b . 0));

  let mut prepared = Vec::new ();
  for (path, bytes) in &restore {
    if let Some (bytes) = bytes {
      let parent = path . parent () . ok_or_else (|| format! (
        "Recovery path {} has no parent", path . display ()))?;
      fs::create_dir_all (parent) . map_err (|error| error . to_string ())?;
      let temporary = parent . join (format! (
        ".skg-recovery-{}.tmp", uuid::Uuid::new_v4 ()));
      fs::write (&temporary, bytes) . map_err (|error| format! (
        "Could not prepare {}: {}", path . display (), error))?;
      prepared . push ((path . clone (), temporary)); }
  }
  for (path, bytes) in &restore {
    match bytes {
      Some (_) => {
        let position = prepared . iter () . position (|(target, _)| target == path)
          . expect ("prepared every restoration write");
        let (_, temporary) = prepared . remove (position);
        fs::rename (&temporary, path) . map_err (|error| format! (
          "Could not restore {}: {}", path . display (), error))?; }
      None => match fs::remove_file (path) {
        Ok (( )) => {}
        Err (error) if error . kind () == io::ErrorKind::NotFound => {}
        Err (error) => return Err (format! (
          "Could not restore tombstone {}: {}", path . display (), error)),
      }
    }
  }
  Ok (restore . into_iter () . map (|(path, _)| path) . collect ())
}

pub fn register_incident (
  config      : &SkgConfig,
  incident_id : &str,
  draft       : RecoveryDraft,
) -> Result<(), String> {
  let directory = journal_directory (config);
  fs::create_dir_all (&directory) . map_err (|error| error . to_string ())?;
  set_private_permissions (&directory, true)?;
  // Incident IDs originate at clients. Hash the opaque value rather than
  // admitting it into a pathname (where separators or `..` would be unsafe).
  let journal_key = blake3::hash (incident_id . as_bytes ())
    . to_hex () . to_string ();
  let journal = directory . join (format! ("{}.yaml", journal_key));
  let pending = PendingRecoveryIncident {
    incident_id: incident_id . to_string (), draft, journal: journal . clone () };
  let bytes = serde_yaml::to_string (&pending)
    . map_err (|error| error . to_string ())? . into_bytes ();
  let temporary = directory . join (format! (
    ".{}.{}.tmp", journal_key, uuid::Uuid::new_v4 ()));
  fs::write (&temporary, bytes) . map_err (|error| error . to_string ())?;
  set_private_permissions (&temporary, false)?;
  fs::rename (&temporary, &journal) . map_err (|error| error . to_string ())?;
  incidents () . lock () . unwrap ()
    . insert (incident_id . to_string (), pending);
  Ok (( ))
}

pub fn pending_incident (incident_id : &str) -> Option<PendingRecoveryIncident> {
  incidents () . lock () . unwrap () . get (incident_id) . cloned ()
}

pub fn pending_incidents_for_config (
  config : &SkgConfig,
) -> Vec<PendingRecoveryIncident> {
  let directory = journal_directory (config);
  let mut pending : Vec<_> = incidents () . lock () . unwrap () . values ()
    . filter (|incident| incident . journal . parent () == Some (&directory))
    . cloned () . collect ();
  pending . sort_by (|a, b| a . incident_id . cmp (&b . incident_id));
  pending
}

/// Rehydrate unresolved incidents before accepting clients. Journals are
/// evidence, so malformed or old entries are reported and retained rather
/// than deleted automatically.
pub fn load_recovery_journals (config : &SkgConfig) -> Result<usize, String> {
  let directory = journal_directory (config);
  let entries = match fs::read_dir (&directory) {
    Ok (entries) => entries,
    Err (error) if error . kind () == io::ErrorKind::NotFound => return Ok (0),
    Err (error) => return Err (error . to_string ()),
  };
  let mut loaded = 0;
  let mut errors = Vec::new ();
  for entry in entries {
    let entry = match entry {
      Ok (entry) => entry,
      Err (error) => { errors . push (error . to_string ()); continue; }};
    let path = entry . path ();
    if path . extension () . and_then (|extension| extension . to_str ())
       != Some ("yaml")
    { continue; }
    match fs::read (&path) . map_err (|error| error . to_string ())
      . and_then (|bytes| serde_yaml::from_slice::<PendingRecoveryIncident> (&bytes)
        . map_err (|error| error . to_string ()))
    {
      Ok (mut pending) => {
        pending . journal = path;
        incidents () . lock () . unwrap ()
          . insert (pending . incident_id . clone (), pending);
        loaded += 1; }
      Err (error) => errors . push (format! ("{}: {}", path . display (), error)),
    }
  }
  if loaded > 64 {
    tracing::warn! (
      count = loaded,
      "More than 64 unresolved recovery journals are retained; Skg never evicts recovery evidence silently"); }
  if errors . is_empty () { Ok (loaded) }
  else { Err (format! (
    "Loaded {} recovery journal(s), but could not read: {}",
    loaded, errors . join ("; "))) }
}

pub fn retire_incident (incident_id : &str) -> Result<(), String> {
  let pending = incidents () . lock () . unwrap () . remove (incident_id);
  if let Some (pending) = pending {
    match fs::remove_file (&pending . journal) {
      Ok (( )) => {}
      Err (error) if error . kind () == io::ErrorKind::NotFound => {}
      Err (error) => return Err (error . to_string ()),
    }
  }
  Ok (( ))
}

fn journal_directory (config : &SkgConfig) -> PathBuf {
  let base = std::env::var_os ("XDG_STATE_HOME") . map (PathBuf::from)
    . or_else (|| std::env::var_os ("HOME")
      . map (|home| PathBuf::from (home) . join (".local/state")))
    . unwrap_or_else (|| std::env::temp_dir () . join ("skg-state"));
  let identity = config . config_path . canonicalize ()
    . unwrap_or_else (|_| config . config_path . clone ());
  let key = blake3::hash (identity . to_string_lossy () . as_bytes ())
    . to_hex () . to_string ();
  base . join ("skg/recovery") . join (&key[..16])
}

#[cfg(unix)]
fn set_private_permissions (path : &Path, directory : bool) -> Result<(), String> {
  use std::os::unix::fs::PermissionsExt;
  fs::set_permissions (path, fs::Permissions::from_mode (
    if directory { 0o700 } else { 0o600 }))
    . map_err (|error| error . to_string ())
}

#[cfg(not(unix))]
fn set_private_permissions (_path : &Path, _directory : bool) -> Result<(), String> {
  Ok (( ))
}

pub fn repository_is_safe_to_recover (snapshot : &RepositorySnapshot) -> bool {
  snapshot . state == format! ("{:?}", RepositoryState::Clean)
    && !snapshot . detached
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::misc::SkgfileSource;
  use git2::IndexAddOption;
  use std::collections::HashMap;
  use tempfile::{TempDir, tempdir};

  fn test_config (temp : &TempDir, source : &Path) -> SkgConfig {
    let name = SourceName::from ("owned");
    let mut sources = HashMap::new ();
    sources . insert (name . clone (), SkgfileSource {
      name: name . clone (), abbreviation: None,
      path: source . to_path_buf (), user_owns_it: true,
    });
    let mut config = SkgConfig::dummyFromSources (sources);
    config . config_path = temp . path () . join ("config.toml");
    config . data_root = temp . path () . to_path_buf ();
    let order = vec![name];
    config . sources . set_order (order);
    config
  }

  fn commit_all (repo : &Repository, message : &str) -> Oid {
    let mut index = repo . index () . unwrap ();
    index . add_all (["*"], IndexAddOption::DEFAULT, None) . unwrap ();
    index . write () . unwrap ();
    let tree_id = index . write_tree () . unwrap ();
    let tree = repo . find_tree (tree_id) . unwrap ();
    let signature = Signature::now ("Skg test", "skg@example.invalid") . unwrap ();
    let parent = repo . head () . ok ()
      . and_then (|head| head . peel_to_commit () . ok ());
    let parents : Vec<&git2::Commit<'_>> = parent . iter () . collect ();
    repo . commit (Some ("HEAD"), &signature, &signature,
                   message, &tree, &parents) . unwrap ()
  }

  fn blob (repo : &Repository, commit : Oid, path : &str) -> Vec<u8> {
    repo . find_commit (commit) . unwrap () . tree () . unwrap ()
      . get_path (Path::new (path)) . unwrap ()
      . to_object (repo) . unwrap () . peel_to_blob () . unwrap ()
      . content () . to_vec ()
  }

  #[test]
  fn snapshot_captures_overlay_tombstones_and_non_utf8_but_not_nested_repo () {
    let temp = tempdir () . unwrap ();
    let root = temp . path () . join ("repo");
    let source = root . join ("nodes");
    fs::create_dir_all (&source) . unwrap ();
    let repo = Repository::init (&root) . unwrap ();
    fs::write (source . join ("base-only.skg"), b"pid: base-only\n") . unwrap ();
    fs::write (root . join ("ordinary.txt"), b"base") . unwrap ();
    commit_all (&repo, "base");
    fs::remove_file (source . join ("base-only.skg")) . unwrap ();
    fs::write (source . join ("A.skg"), [0xff, 0x00, b'A']) . unwrap ();
    fs::write (root . join ("unrelated.skg"), b"untracked") . unwrap ();
    let nested = root . join ("nested");
    fs::create_dir_all (&nested) . unwrap ();
    Repository::init (&nested) . unwrap ();
    fs::write (nested . join ("hidden.skg"), b"nested") . unwrap ();
    let config = test_config (&temp, &source);

    let snapshot = IncidentDiskSnapshot::capture (
      &config, &[ID::from ("A"), ID::from ("missing")]) . unwrap ();
    assert_eq! (snapshot . repositories . len (), 1);
    let paths = &snapshot . repositories[0] . paths;
    let find = |suffix : &str| paths . iter () . find (|path|
      path . absolute . ends_with (suffix)) . unwrap ();
    assert_eq! (find ("nodes/A.skg") . bytes, Some (vec![0xff, 0x00, b'A']));
    assert_eq! (find ("nodes/base-only.skg") . bytes, None);
    assert_eq! (find ("nodes/missing.skg") . bytes, None);
    assert_eq! (find ("unrelated.skg") . bytes, Some (b"untracked" . to_vec ()));
    assert! (!paths . iter () . any (|path|
      path . absolute . ends_with ("nested/hidden.skg")));
  }

  #[test]
  fn recovery_builds_two_branches_preserves_index_and_restores_fatal_path () {
    let temp = tempdir () . unwrap ();
    let root = temp . path () . join ("repo");
    let source = root . join ("nodes");
    fs::create_dir_all (&source) . unwrap ();
    let repo = Repository::init (&root) . unwrap ();
    let path = source . join ("A.skg");
    fs::write (&path, b"pid: A\ntitle: old\n") . unwrap ();
    fs::write (root . join ("keep.txt"), b"base") . unwrap ();
    let base = commit_all (&repo, "base");
    fs::write (&path, b"not: [valid") . unwrap ();
    fs::write (root . join ("unrelated.skg"), b"dirty overlay") . unwrap ();
    let index_before = fs::read (repo . path () . join ("index")) . unwrap ();
    let head_before = repo . head () . unwrap () . target () . unwrap ();
    let config = test_config (&temp, &source);
    let snapshot = IncidentDiskSnapshot::capture (
      &config, &[ID::from ("A")]) . unwrap ();
    let journal = temp . path () . join ("incident.yaml");
    fs::write (&journal, b"journal") . unwrap ();
    incidents () . lock () . unwrap () . insert ("test-recovery" . into (),
      PendingRecoveryIncident {
        incident_id: "test-recovery" . into (), journal: journal . clone (),
        draft: RecoveryDraft {
          fatal: vec![(ID::from ("A"), "bad yaml" . into ())],
          touched_pids: vec![ID::from ("A")],
          pre_manifest: BTreeMap::from ([
            (path . clone (), Some (b"pid: A\ntitle: old\n" . to_vec ())),
          ]),
          legal_manifest: BTreeMap::from ([
            (path . clone (), Some (b"pid: A\ntitle: old\n" . to_vec ())),
          ]),
          disk_snapshot: snapshot,
        },
      });

    let report = recover_incident (&config, "test-recovery") . unwrap ();
    assert_eq! (report . repositories . len (), 1);
    let recovered = &report . repositories[0];
    let p = Oid::from_str (&recovered . pre_commit) . unwrap ();
    let l = Oid::from_str (&recovered . legal_commit) . unwrap ();
    let a = Oid::from_str (&recovered . incident_commit) . unwrap ();
    assert_eq! (repo . find_commit (p) . unwrap () . parent_id (0) . unwrap (), base);
    assert_eq! (repo . find_commit (l) . unwrap () . parent_id (0) . unwrap (), p);
    assert_eq! (repo . find_commit (a) . unwrap () . parent_id (0) . unwrap (), p);
    assert_eq! (blob (&repo, p, "nodes/A.skg"), b"pid: A\ntitle: old\n");
    assert_eq! (blob (&repo, l, "nodes/A.skg"), b"pid: A\ntitle: old\n");
    assert_eq! (blob (&repo, a, "nodes/A.skg"), b"not: [valid");
    assert_eq! (blob (&repo, a, "unrelated.skg"), b"dirty overlay");
    assert_eq! (fs::read (&path) . unwrap (), b"pid: A\ntitle: old\n");
    assert_eq! (fs::read (repo . path () . join ("index")) . unwrap (), index_before);
    assert_eq! (repo . head () . unwrap () . target () . unwrap (), head_before);
    assert! (!journal . exists ());
  }

  #[test]
  fn recovery_refuses_changed_overlay_before_creating_refs () {
    let temp = tempdir () . unwrap ();
    let root = temp . path () . join ("repo");
    let source = root . join ("nodes");
    fs::create_dir_all (&source) . unwrap ();
    let repo = Repository::init (&root) . unwrap ();
    let path = source . join ("A.skg");
    fs::write (&path, b"bad") . unwrap ();
    commit_all (&repo, "base");
    let config = test_config (&temp, &source);
    let snapshot = IncidentDiskSnapshot::capture (
      &config, &[ID::from ("A")]) . unwrap ();
    let journal = temp . path () . join ("changed.yaml");
    fs::write (&journal, b"journal") . unwrap ();
    incidents () . lock () . unwrap () . insert ("test-changed" . into (),
      PendingRecoveryIncident {
        incident_id: "test-changed" . into (), journal,
        draft: RecoveryDraft {
          fatal: vec![(ID::from ("A"), "bad" . into ())],
          touched_pids: vec![ID::from ("A")],
          pre_manifest: BTreeMap::new (), legal_manifest: BTreeMap::new (),
          disk_snapshot: snapshot,
        },
      });
    fs::write (&path, b"changed after incident") . unwrap ();
    assert! (recover_incident (&config, "test-changed") . is_err ());
    assert! (repo . find_reference ("refs/heads/oops-1_1-pre-incident")
      . is_err ());
    incidents () . lock () . unwrap () . remove ("test-changed");
  }

  #[test]
  fn confirmed_recovery_initializes_owned_non_git_source_at_pre_incident () {
    let temp = tempdir () . unwrap ();
    let source = temp . path () . join ("plain-source");
    fs::create_dir_all (&source) . unwrap ();
    let path = source . join ("A.skg");
    fs::write (&path, b"not: [valid") . unwrap ();
    let config = test_config (&temp, &source);
    let snapshot = IncidentDiskSnapshot::capture (
      &config, &[ID::from ("A")]) . unwrap ();
    assert! (snapshot . repositories[0] . git_directory . is_none ());
    let journal = temp . path () . join ("non-git.yaml");
    fs::write (&journal, b"journal") . unwrap ();
    incidents () . lock () . unwrap () . insert ("test-non-git" . into (),
      PendingRecoveryIncident {
        incident_id: "test-non-git" . into (), journal,
        draft: RecoveryDraft {
          fatal: vec![(ID::from ("A"), "bad yaml" . into ())],
          touched_pids: vec![ID::from ("A")],
          pre_manifest: BTreeMap::from ([
            (path . clone (), Some (b"pid: A\ntitle: old\n" . to_vec ())),
          ]),
          legal_manifest: BTreeMap::from ([
            (path . clone (), Some (b"pid: A\ntitle: old\n" . to_vec ())),
          ]),
          disk_snapshot: snapshot,
        },
      });

    let report = recover_incident (&config, "test-non-git") . unwrap ();
    assert! (report . repositories[0] . initialized);
    let repo = Repository::open (&source) . unwrap ();
    assert_eq! (repo . find_reference ("HEAD") . unwrap () . symbolic_target (),
                Some ("refs/heads/main"));
    let p = Oid::from_str (&report . repositories[0] . pre_commit) . unwrap ();
    assert_eq! (repo . head () . unwrap () . target (), Some (p));
    assert_eq! (repo . find_commit (p) . unwrap () . parent_count (), 0);
    assert_eq! (fs::read (&path) . unwrap (), b"pid: A\ntitle: old\n");
  }
}
