//! Verification of the client-owned initial recovery archive.
//!
//! `ARCHIVE-READY` is only a client claim.  Before maintenance may cross its
//! point of no return, the server opens the server-visible incident without
//! following archive-contained symlinks and independently checks the marker,
//! manifest identity, complete file inventory, modes, lengths and SHA-256s.

use super::types::{ActiveMaintenance, BufferKind};
use crate::types::misc::ID;
use crate::types::views_state::ViewUri;

use sexp::{Atom, Sexp};
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet};
use std::fs::{self, File, OpenOptions};
use std::io::{BufReader, Read};
use std::path::{Component, Path, PathBuf};

#[cfg(unix)]
use std::ffi::CString;
#[cfg(unix)]
use std::os::unix::ffi::OsStrExt;
#[cfg(unix)]
use std::os::unix::fs::{MetadataExt, OpenOptionsExt, PermissionsExt};
#[cfg(unix)]
use std::os::unix::io::{AsRawFd, FromRawFd};

const ARCHIVE_FORMAT_VERSION : u64 = 1;
const MAX_MANIFEST_BYTES : u64 = 64 * 1024 * 1024;
const MAX_MARKER_BYTES : u64 = 64 * 1024;
const MAX_BUFFER_TEXT_BYTES : u64 = 256 * 1024 * 1024;

pub struct InitialArchiveExpectation<'a> {
  pub archive_root       : &'a Path,
  pub active             : &'a ActiveMaintenance,
  pub manifest_sha256    : &'a str,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct VerifiedInitialArchive {
  pub path             : PathBuf,
  pub manifest_sha256  : String,
  pub artifact_count   : usize,
  pub total_file_bytes : u64,
  pub buffers          : Vec<VerifiedBufferSnapshot>,
}

/// The exact dirty-buffer facts recovered from the independently verified
/// archive.  Current text comes from the checksummed artifact, never from an
/// unverified follow-up client message.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct VerifiedBufferSnapshot {
  pub buffer_key             : String,
  pub buffer_id              : String,
  pub kind                   : BufferKind,
  pub name                   : String,
  pub view_uri               : Option<ViewUri>,
  pub root_ids               : Vec<ID>,
  pub recipe                 : String,
  pub graph_generation       : u64,
  pub presentation_generation : u64,
  pub server_revision        : u64,
  pub application_token      : u64,
  pub last_fetched_text      : String,
  pub current_text           : String,
}

#[derive(Clone, Debug)]
struct InspectedFile {
  bytes  : u64,
  sha256 : String,
}

#[derive(Debug)]
struct InspectedTree {
  files          : BTreeMap<String, InspectedFile>,
  directories    : BTreeSet<String>,
  manifest_bytes : Vec<u8>,
  marker_bytes   : Vec<u8>,
  captured_artifacts : BTreeMap<String, Vec<u8>>,
  total_bytes    : u64,
}

#[derive(Debug)]
struct ArtifactClaim {
  path   : String,
  bytes  : u64,
  sha256 : String,
}

pub fn verify_initial_archive (
  expected : InitialArchiveExpectation<'_>,
) -> Result<VerifiedInitialArchive, String> {
  validate_archive_directory_name (
    &expected . active . archive_directory_name,
    expected . active . incident_id . as_str ())?;
  validate_sha256 (expected . manifest_sha256, "archive-ready ACK")?;
  if expected . active . archive_owner_session_id . is_empty () {
    return Err ("active maintenance has no archive-owning client" . into ()); }
  if expected . active . archive_owner_client_kind != "emacs"
  && expected . active . archive_owner_client_kind != "neovim" {
    return Err (format! (
      "unsupported archive client kind '{}'",
      expected . active . archive_owner_client_kind)); }

  let tree = inspect_tree (
    expected . archive_root,
    &expected . active . archive_directory_name)?;
  let manifest_file = tree . files . get ("manifest.initial.sexp")
    . ok_or_else (|| "archive has no manifest.initial.sexp" . to_string ())?;
  if manifest_file . sha256 != expected . manifest_sha256 {
    return Err (format! (
      "initial manifest checksum is {}; ACK claimed {}",
      manifest_file . sha256, expected . manifest_sha256)); }
  let marker_file = tree . files . get ("ARCHIVE-READY")
    . ok_or_else (|| "archive has no ARCHIVE-READY marker" . to_string ())?;
  if marker_file . bytes == 0 {
    return Err ("ARCHIVE-READY is an empty filename, not authority" . into ()); }
  if tree . files . contains_key ("FINALIZED")
  || tree . files . contains_key ("manifest.final.sexp")
  {
    return Err ("initial archive was prematurely marked final" . into ()); }

  let marker_text = std::str::from_utf8 (&tree . marker_bytes)
    . map_err (|_| "ARCHIVE-READY is not UTF-8" . to_string ())?;
  let marker = sexp::parse (marker_text)
    . map_err (|error| format! ("invalid ARCHIVE-READY: {}", error))?;
  let marker_fields = alist (&marker, "ARCHIVE-READY")?;
  require_exact_keys (&marker_fields, &[
    "archive-format-version", "incident-id", "manifest-sha256",
  ], "ARCHIVE-READY")?;
  require_u64 (&marker_fields, "archive-format-version", "ARCHIVE-READY")?
    . eq (&ARCHIVE_FORMAT_VERSION) . then_some (()) . ok_or_else (||
      "ARCHIVE-READY has an unsupported archive format" . to_string ())?;
  require_equal_text (&marker_fields, "incident-id",
    expected . active . incident_id . as_str (), "ARCHIVE-READY")?;
  require_equal_text (&marker_fields, "manifest-sha256",
    expected . manifest_sha256, "ARCHIVE-READY")?;

  let manifest_text = std::str::from_utf8 (&tree . manifest_bytes)
    . map_err (|_| "manifest.initial.sexp is not UTF-8" . to_string ())?;
  let manifest = sexp::parse (manifest_text)
    . map_err (|error| format! ("invalid initial manifest: {}", error))?;
  let fields = alist (&manifest, "initial manifest")?;
  require_exact_keys (&fields, &[
    "archive-format-version", "manifest-kind", "incident-id",
    "maintenance-epoch", "origin", "started-at-utc",
    "archive-directory-name", "client-kind", "client-version",
    "client-session-id", "client-archive-identity",
    "server-archive-identity", "source-set", "g0-graph-generation",
    "g0-manifest-revision", "directory-sync", "artifacts", "buffers",
    "initial-status",
  ], "initial manifest")?;
  if require_u64 (&fields, "archive-format-version", "initial manifest")?
     != ARCHIVE_FORMAT_VERSION
  {
    return Err ("initial manifest has an unsupported archive format" . into ()); }
  require_equal_text (&fields, "manifest-kind", "initial", "initial manifest")?;
  require_equal_text (&fields, "incident-id",
    expected . active . incident_id . as_str (), "initial manifest")?;
  if require_u64 (&fields, "maintenance-epoch", "initial manifest")?
     != expected . active . epoch . get ()
  {
    return Err ("initial manifest maintenance epoch does not match" . into ()); }
  require_equal_text (&fields, "origin",
    expected . active . origin . label (), "initial manifest")?;
  require_equal_text (&fields, "started-at-utc",
    &expected . active . started_at_utc, "initial manifest")?;
  require_equal_text (&fields, "archive-directory-name",
    &expected . active . archive_directory_name, "initial manifest")?;
  require_equal_text (&fields, "client-kind",
    &expected . active . archive_owner_client_kind, "initial manifest")?;
  require_nonempty_text (&fields, "client-version", "initial manifest")?;
  require_equal_text (&fields, "client-session-id",
    &expected . active . archive_owner_session_id, "initial manifest")?;
  require_nonempty_text (
    &fields, "client-archive-identity", "initial manifest")?;
  require_equal_text (&fields, "server-archive-identity",
    &expected . archive_root . to_string_lossy (), "initial manifest")?;
  require_equal_text (
    &fields, "source-set", &expected . active . source_set, "initial manifest")?;
  if require_u64 (&fields, "g0-graph-generation", "initial manifest")?
     != expected . active . g0_graph_generation . get ()
  {
    return Err ("initial manifest G0 graph generation does not match" . into ()); }
  if require_u64 (&fields, "g0-manifest-revision", "initial manifest")?
     != expected . active . g0_manifest_revision . get ()
  {
    return Err ("initial manifest G0 manifest revision does not match" . into ()); }
  require_nonempty_text (&fields, "directory-sync", "initial manifest")?;
  require_equal_text (&fields, "initial-status",
    "prepared-for-publication", "initial manifest")?;

  let root_claims = artifact_claims (
    require_list (&fields, "artifacts", "initial manifest")?,
    "initial manifest root artifacts")?;
  let root_paths : BTreeSet<_> = root_claims . iter ()
    . map (|claim| claim . path . clone ()) . collect ();
  let expected_root_paths : BTreeSet<String> = [
    "incident.org" . into (),
    "interrupted-buffers/README.org" . into (),
  ] . into_iter () . collect ();
  if root_paths != expected_root_paths {
    return Err (format! (
      "initial root artifacts are {:?}, expected {:?}",
      root_paths, expected_root_paths)); }

  let buffers = require_list (&fields, "buffers", "initial manifest")?;
  let mut buffer_ids = BTreeSet::new ();
  let mut buffer_keys = BTreeSet::new ();
  let mut used_waivers = BTreeMap::new ();
  let mut claims = root_claims;
  let mut verified_buffers = Vec::new ();
  let mut expected_directories : BTreeSet<String> = [
    "buffer-snapshots" . into (),
    "interrupted-buffers" . into (),
  ] . into_iter () . collect ();
  for (index, buffer) in buffers . iter () . enumerate () {
    let context = format! ("initial manifest buffer {}", index + 1);
    let buffer_fields = alist (buffer, &context)?;
    require_exact_keys (&buffer_fields, &[
      "buffer-key", "buffer-id", "kind", "name", "view-uri", "root-ids",
      "recipe", "graph-generation", "presentation-generation",
      "server-revision", "application-token", "undo", "artifacts",
      "initial-disposition",
    ], &context)?;
    let key = require_nonempty_text (&buffer_fields, "buffer-key", &context)?;
    validate_buffer_key (&key)?;
    if !buffer_keys . insert (key . clone ()) {
      return Err (format! ("duplicate archive buffer key '{}'", key)); }
    let buffer_id = require_nonempty_text (
      &buffer_fields, "buffer-id", &context)?;
    if !buffer_ids . insert (buffer_id . clone ()) {
      return Err (format! ("duplicate archive buffer ID '{}'", buffer_id)); }
    let kind = BufferKind::parse (&require_nonempty_text (
      &buffer_fields, "kind", &context)?)?;
    let name = require_text (&buffer_fields, "name", &context)?;
    let view_uri_text = require_text (&buffer_fields, "view-uri", &context)?;
    let view_uri = if view_uri_text == "none" { None }
      else { Some (ViewUri::from_client_string (view_uri_text)) };
    let root_ids = require_atom_list_text (
      &buffer_fields, "root-ids", &context)? . into_iter ()
      . map (ID::from) . collect ();
    let recipe = require_text (&buffer_fields, "recipe", &context)?;
    let graph_generation = require_u64 (
      &buffer_fields, "graph-generation", &context)?;
    let presentation_generation = require_u64 (
      &buffer_fields, "presentation-generation", &context)?;
    let server_revision = require_u64 (
      &buffer_fields, "server-revision", &context)?;
    let application_token = require_u64 (
      &buffer_fields, "application-token", &context)?;
    require_equal_text (&buffer_fields, "initial-disposition",
      "pending-classification", &context)?;

    let buffer_claims = artifact_claims (
      require_list (&buffer_fields, "artifacts", &context)?,
      &format! ("{} artifacts", context))?;
    let base = format! ("buffer-snapshots/{}", key);
    expected_directories . insert (base . clone ());
    let mut actual_buffer_paths = BTreeSet::new ();
    for claim in &buffer_claims {
      if !claim . path . starts_with (&format! ("{}/", base)) {
        return Err (format! (
          "artifact '{}' is outside buffer key '{}'", claim . path, key)); }
      actual_buffer_paths . insert (claim . path . clone ());
    }
    let mut required_buffer_paths : BTreeSet<String> = [
      "README.org", "metadata.sexp", "last-fetched.org",
      "unsaved-changes.org", "diff.txt",
    ] . into_iter () . map (|name| format! ("{}/{}", base, name))
      . collect ();

    let undo = field_alist (&buffer_fields, "undo", &context)?;
    require_allowed_keys (&undo, &[
      "status", "kind", "version", "header-identity", "validation",
      "bytes", "sha256", "reason",
    ], &format! ("{} undo", context))?;
    let undo_status = require_nonempty_text (
      &undo, "status", &format! ("{} undo", context))?;
    let undo_kind = require_nonempty_text (
      &undo, "kind", &format! ("{} undo", context))?;
    require_nonempty_text (
      &undo, "version", &format! ("{} undo", context))?;
    let sidecar = match expected . active . archive_owner_client_kind . as_str () {
      "emacs" => {
        if undo_kind != "undo-fu-session" {
          return Err (format! ("{} has non-Emacs undo kind '{}'",
            context, undo_kind)); }
        format! ("{}/undo.emacs.gz", base) }
      "neovim" => {
        if undo_kind != "nvim-wundo" {
          return Err (format! ("{} has non-Neovim undo kind '{}'",
            context, undo_kind)); }
        format! ("{}/undo.nvim", base) }
      _ => unreachable! (),
    };
    match undo_status . as_str () {
      "archived" => {
        required_buffer_paths . insert (sidecar);
        require_u64 (&undo, "bytes", &format! ("{} undo", context))?;
        let undo_sha = require_nonempty_text (
          &undo, "sha256", &format! ("{} undo", context))?;
        validate_sha256 (&undo_sha, &format! ("{} undo", context))?;
      }
      "empty" => {
        if actual_buffer_paths . contains (&sidecar) {
          return Err (format! ("{} declares a sidecar for empty undo", context)); }
      }
      "undo-unavailable-approved" => {
        let reason = require_nonempty_text (
          &undo, "reason", &format! ("{} undo", context))?;
        match expected . active . undo_waivers . get (&key) {
          Some (approved) if approved == &reason => {
            used_waivers . insert (key . clone (), reason); }
          _ => return Err (format! (
            "{} carries no exact server-approved undo waiver", context)),
        }
        if actual_buffer_paths . contains (&sidecar) {
          return Err (format! (
            "{} declares both an undo waiver and a sidecar", context)); }
      }
      other => return Err (format! (
        "{} has unsupported undo status '{}'", context, other)),
    }
    if actual_buffer_paths != required_buffer_paths {
      return Err (format! (
        "{} artifact set is {:?}, expected {:?}",
        context, actual_buffer_paths, required_buffer_paths)); }
    let last_path = format! ("{}/last-fetched.org", base);
    let current_path = format! ("{}/unsaved-changes.org", base);
    let last_fetched_text = captured_utf8_artifact (
      &tree, &last_path, &context)?;
    let current_text = captured_utf8_artifact (
      &tree, &current_path, &context)?;
    let frozen = expected . active . buffer_census . get (&buffer_id)
      . ok_or_else (|| format! (
        "{} is absent from the frozen maintenance census", context))?;
    if !frozen . dirty
    || frozen . kind != kind
    || frozen . view_uri != view_uri . as_ref () . map (ViewUri::repr_in_client)
    || frozen . graph_generation != graph_generation
    || frozen . presentation_generation != presentation_generation
    || frozen . server_revision != server_revision
    || frozen . application_token != application_token
    {
      return Err (format! (
        "{} authority does not match the frozen maintenance census", context)); }
    let last_sha = tree . files . get (&last_path)
      . expect ("required captured last-fetched artifact exists") . sha256 . clone ();
    let current_sha = tree . files . get (&current_path)
      . expect ("required captured current artifact exists") . sha256 . clone ();
    if frozen . last_fetched_sha256 != last_sha
    || frozen . current_sha256 != current_sha
    {
      return Err (format! (
        "{} text checksums do not match the frozen maintenance census", context)); }
    verified_buffers . push (VerifiedBufferSnapshot {
      buffer_key: key,
      buffer_id,
      kind,
      name,
      view_uri,
      root_ids,
      recipe,
      graph_generation,
      presentation_generation,
      server_revision,
      application_token,
      last_fetched_text,
      current_text,
    });
    claims . extend (buffer_claims);
  }
  let expected_dirty : BTreeSet<String> = expected . active . dirty_buffer_ids
    . iter () . cloned () . collect ();
  if buffer_ids != expected_dirty {
    return Err (format! (
      "archive dirty buffer IDs are {:?}, expected {:?}",
      buffer_ids, expected_dirty)); }
  if used_waivers != expected . active . undo_waivers {
    return Err ("archive does not account for every approved undo waiver" . into ()); }

  let mut declared = BTreeSet::new ();
  for claim in &claims {
    if !declared . insert (claim . path . clone ()) {
      return Err (format! ("artifact '{}' is declared twice", claim . path)); }
    let actual = tree . files . get (&claim . path) . ok_or_else (|| format! (
      "declared artifact '{}' is missing", claim . path))?;
    if actual . bytes != claim . bytes {
      return Err (format! (
        "artifact '{}' has {} bytes, manifest claims {}",
        claim . path, actual . bytes, claim . bytes)); }
    if actual . sha256 != claim . sha256 {
      return Err (format! (
        "artifact '{}' checksum mismatch", claim . path)); }
    add_parent_directories (&claim . path, &mut expected_directories);
  }
  let actual_artifacts : BTreeSet<String> = tree . files . keys ()
    . filter (|path| path . as_str () != "manifest.initial.sexp"
      && path . as_str () != "ARCHIVE-READY")
    . cloned () . collect ();
  if actual_artifacts != declared {
    return Err (format! (
      "archive file inventory is {:?}, manifest declares {:?}",
      actual_artifacts, declared)); }
  if tree . directories != expected_directories {
    return Err (format! (
      "archive directory inventory is {:?}, expected {:?}",
      tree . directories, expected_directories)); }

  Ok (VerifiedInitialArchive {
    path: expected . archive_root
      . join (&expected . active . archive_directory_name),
    manifest_sha256: expected . manifest_sha256 . into (),
    artifact_count: claims . len (),
    total_file_bytes: tree . total_bytes,
    buffers: verified_buffers,
  })
}

fn captured_utf8_artifact (
  tree    : &InspectedTree,
  path    : &str,
  context : &str,
) -> Result<String, String> {
  let bytes = tree . captured_artifacts . get (path) . ok_or_else (|| format! (
    "{} exact text artifact '{}' was not retained", context, path))?;
  String::from_utf8 (bytes . clone ()) . map_err (|_| format! (
    "{} exact text artifact '{}' is not UTF-8", context, path))
}

fn validate_archive_directory_name (
  name        : &str,
  incident_id : &str,
) -> Result<(), String> {
  let bytes = name . as_bytes ();
  let fixed = bytes . len () == 60
    && bytes . get (8) == Some (&b'T')
    && bytes . get (15) == Some (&b'.')
    && bytes . get (22) == Some (&b'Z')
    && bytes . get (23) == Some (&b'_')
    && bytes[..8] . iter () . all (u8::is_ascii_digit)
    && bytes[9..15] . iter () . all (u8::is_ascii_digit)
    && bytes[16..22] . iter () . all (u8::is_ascii_digit);
  if !fixed || &name[24..] != incident_id {
    return Err (format! (
      "invalid server-owned archive directory name '{}'", name)); }
  Ok (( ))
}

fn validate_buffer_key (key : &str) -> Result<(), String> {
  if key == "." || key == ".." || key . len () > 160
  || !key . bytes () . all (|byte|
       byte . is_ascii_alphanumeric () || matches! (byte, b'.' | b'_' | b'-'))
  {
    return Err (format! ("unsafe archive buffer key '{}'", key)); }
  Ok (( ))
}

fn validate_relative_artifact_path (path : &str) -> Result<(), String> {
  if path . is_empty () || path . contains ('\\')
  || !path . bytes () . all (|byte| byte . is_ascii_alphanumeric ()
       || matches! (byte, b'/' | b'.' | b'_' | b'-'))
  {
    return Err (format! ("unsafe artifact path '{}'", path)); }
  let parsed = Path::new (path);
  if parsed . is_absolute () || parsed . components () . any (|component|
    !matches! (component, Component::Normal (_)))
  {
    return Err (format! ("unsafe artifact path '{}'", path)); }
  Ok (( ))
}

fn validate_sha256 (value : &str, context : &str) -> Result<(), String> {
  if value . len () != 64
  || !value . bytes () . all (|byte|
       byte . is_ascii_digit () || (b'a'..=b'f') . contains (&byte))
  {
    return Err (format! ("{} has invalid lowercase SHA-256", context)); }
  Ok (( ))
}

fn alist<'a> (
  value   : &'a Sexp,
  context : &str,
) -> Result<BTreeMap<String, &'a Sexp>, String> {
  let Sexp::List (entries) = value else {
    return Err (format! ("{} must be a proper list", context)); };
  let mut result = BTreeMap::new ();
  for entry in entries {
    let Sexp::List (pair) = entry else {
      return Err (format! ("{} contains a non-field", context)); };
    if pair . len () != 2 {
      return Err (format! ("{} field is not a two-element list", context)); }
    let Sexp::Atom (Atom::S (key)) = &pair[0] else {
      return Err (format! ("{} field has a non-symbol key", context)); };
    if result . insert (key . clone (), &pair[1]) . is_some () {
      return Err (format! ("{} repeats field '{}'", context, key)); }
  }
  Ok (result)
}

fn field_alist<'a> (
  fields  : &BTreeMap<String, &'a Sexp>,
  key     : &str,
  context : &str,
) -> Result<BTreeMap<String, &'a Sexp>, String> {
  let value = fields . get (key) . ok_or_else (|| format! (
    "{} has no '{}' field", context, key))?;
  alist (value, &format! ("{} {}", context, key))
}

fn require_list<'a> (
  fields  : &BTreeMap<String, &'a Sexp>,
  key     : &str,
  context : &str,
) -> Result<&'a [Sexp], String> {
  match fields . get (key) {
    Some (Sexp::List (values)) => Ok (values),
    Some (_) => Err (format! ("{} field '{}' must be a list", context, key)),
    None => Err (format! ("{} has no '{}' field", context, key)),
  }
}

fn require_atom_list_text (
  fields  : &BTreeMap<String, &Sexp>,
  key     : &str,
  context : &str,
) -> Result<Vec<String>, String> {
  require_list (fields, key, context)? . iter () . map (|value| match value {
    Sexp::Atom (Atom::S (text)) => Ok (text . clone ()),
    Sexp::Atom (Atom::I (number)) => Ok (number . to_string ()),
    _ => Err (format! (
      "{} field '{}' contains a non-atom", context, key)),
  }) . collect ()
}

fn require_text (
  fields  : &BTreeMap<String, &Sexp>,
  key     : &str,
  context : &str,
) -> Result<String, String> {
  match fields . get (key) {
    Some (Sexp::Atom (Atom::S (value))) => Ok (value . clone ()),
    Some (_) => Err (format! ("{} field '{}' must be text", context, key)),
    None => Err (format! ("{} has no '{}' field", context, key)),
  }
}

fn require_nonempty_text (
  fields  : &BTreeMap<String, &Sexp>,
  key     : &str,
  context : &str,
) -> Result<String, String> {
  let value = require_text (fields, key, context)?;
  if value . is_empty () {
    return Err (format! ("{} field '{}' may not be empty", context, key)); }
  Ok (value)
}

fn require_equal_text (
  fields   : &BTreeMap<String, &Sexp>,
  key      : &str,
  expected : &str,
  context  : &str,
) -> Result<(), String> {
  let actual = require_text (fields, key, context)?;
  if actual != expected {
    return Err (format! (
      "{} field '{}' is '{}', expected '{}'",
      context, key, actual, expected)); }
  Ok (( ))
}

fn require_u64 (
  fields  : &BTreeMap<String, &Sexp>,
  key     : &str,
  context : &str,
) -> Result<u64, String> {
  match fields . get (key) {
    Some (Sexp::Atom (Atom::I (value))) if *value >= 0 => Ok (*value as u64),
    Some (_) => Err (format! (
      "{} field '{}' must be a nonnegative integer", context, key)),
    None => Err (format! ("{} has no '{}' field", context, key)),
  }
}

fn require_exact_keys (
  fields   : &BTreeMap<String, &Sexp>,
  expected : &[&str],
  context  : &str,
) -> Result<(), String> {
  let expected : BTreeSet<&str> = expected . iter () . copied () . collect ();
  let actual : BTreeSet<&str> = fields . keys () . map (String::as_str) . collect ();
  if actual != expected {
    return Err (format! (
      "{} fields are {:?}, expected {:?}", context, actual, expected)); }
  Ok (( ))
}

fn require_allowed_keys (
  fields  : &BTreeMap<String, &Sexp>,
  allowed : &[&str],
  context : &str,
) -> Result<(), String> {
  let allowed : BTreeSet<&str> = allowed . iter () . copied () . collect ();
  let unexpected : Vec<&str> = fields . keys () . map (String::as_str)
    . filter (|key| !allowed . contains (key)) . collect ();
  if !unexpected . is_empty () {
    return Err (format! ("{} has unexpected fields {:?}", context, unexpected)); }
  Ok (( ))
}

fn artifact_claims (
  values  : &[Sexp],
  context : &str,
) -> Result<Vec<ArtifactClaim>, String> {
  values . iter () . enumerate () . map (|(index, value)| {
    let item_context = format! ("{} record {}", context, index + 1);
    let fields = alist (value, &item_context)?;
    require_exact_keys (&fields, &["path", "bytes", "sha256"], &item_context)?;
    let path = require_nonempty_text (&fields, "path", &item_context)?;
    validate_relative_artifact_path (&path)?;
    let bytes = require_u64 (&fields, "bytes", &item_context)?;
    let sha256 = require_nonempty_text (&fields, "sha256", &item_context)?;
    validate_sha256 (&sha256, &item_context)?;
    Ok (ArtifactClaim { path, bytes, sha256 })
  }) . collect ()
}

fn add_parent_directories (path : &str, directories : &mut BTreeSet<String>) {
  let mut parent = Path::new (path) . parent ();
  while let Some (path) = parent {
    if path . as_os_str () . is_empty () { break; }
    directories . insert (path . to_string_lossy () . replace ('\\', "/"));
    parent = path . parent ();
  }
}

fn hash_file (
  file         : File,
  capture_limit : Option<u64>,
  description  : &str,
) -> Result<(InspectedFile, Vec<u8>), String> {
  let before = file . metadata () . map_err (|error| format! (
    "cannot stat {}: {}", description, error))?;
  if !before . file_type () . is_file () {
    return Err (format! ("{} is not a regular file", description)); }
  #[cfg(unix)]
  {
    if before . permissions () . mode () & 0o777 != 0o600 {
      return Err (format! ("{} is not mode 0600", description)); }
    if before . nlink () != 1 {
      return Err (format! ("{} has {} hard links", description, before . nlink ())); }
  }
  if let Some (limit) = capture_limit {
    if before . len () > limit {
      return Err (format! ("{} is larger than {} bytes", description, limit)); }
  }
  let mut reader = BufReader::new (file);
  let mut digest = Sha256::new ();
  let mut captured = Vec::new ();
  let mut buffer = [0u8; 64 * 1024];
  let mut total = 0u64;
  loop {
    let count = reader . read (&mut buffer) . map_err (|error| format! (
      "cannot read {}: {}", description, error))?;
    if count == 0 { break; }
    total = total . checked_add (count as u64)
      . ok_or_else (|| format! ("{} is impossibly large", description))?;
    digest . update (&buffer[..count]);
    if capture_limit . is_some () {
      captured . extend_from_slice (&buffer[..count]); }
  }
  let after = reader . get_ref () . metadata () . map_err (|error| format! (
    "cannot restat {}: {}", description, error))?;
  if total != before . len () || after . len () != before . len () {
    return Err (format! ("{} changed while being verified", description)); }
  Ok ((InspectedFile {
    bytes: total,
    sha256: format! ("{:x}", digest . finalize ()),
  }, captured))
}

#[cfg(unix)]
fn inspect_tree (root : &Path, final_name : &str) -> Result<InspectedTree, String> {
  let root_directory = OpenOptions::new () . read (true)
    . custom_flags (libc::O_DIRECTORY | libc::O_NOFOLLOW | libc::O_CLOEXEC)
    . open (root) . map_err (|error| format! (
      "cannot open archive root '{}': {}", root . display (), error))?;
  require_private_directory (&root_directory, "archive root")?;
  let final_os = std::ffi::OsStr::new (final_name);
  let final_directory = openat (
    &root_directory, final_os,
    libc::O_RDONLY | libc::O_DIRECTORY | libc::O_NOFOLLOW | libc::O_CLOEXEC)
    . map_err (|error| format! (
      "cannot open final incident '{}': {}", final_name, error))?;
  require_private_directory (&final_directory, "final incident directory")?;
  let mut tree = InspectedTree {
    files: BTreeMap::new (),
    directories: BTreeSet::new (),
    manifest_bytes: Vec::new (),
    marker_bytes: Vec::new (),
    captured_artifacts: BTreeMap::new (),
    total_bytes: 0,
  };
  inspect_directory (&final_directory, "", &mut tree)?;
  Ok (tree)
}

#[cfg(unix)]
fn require_private_directory (file : &File, description : &str) -> Result<(), String> {
  let metadata = file . metadata () . map_err (|error| format! (
    "cannot stat {}: {}", description, error))?;
  if !metadata . file_type () . is_dir () {
    return Err (format! ("{} is not a directory", description)); }
  if metadata . permissions () . mode () & 0o777 != 0o700 {
    return Err (format! ("{} is not mode 0700", description)); }
  Ok (( ))
}

#[cfg(unix)]
fn openat (directory : &File, name : &std::ffi::OsStr, flags : i32)
  -> Result<File, std::io::Error>
{
  let name = CString::new (name . as_bytes ())
    . map_err (|_| std::io::Error::new (
      std::io::ErrorKind::InvalidInput, "filename contains NUL"))?;
  let descriptor = unsafe { libc::openat (
    directory . as_raw_fd (), name . as_ptr (), flags) };
  if descriptor < 0 { return Err (std::io::Error::last_os_error ()); }
  Ok (unsafe { File::from_raw_fd (descriptor) })
}

#[cfg(unix)]
fn lstatat (directory : &File, name : &std::ffi::OsStr)
  -> Result<libc::stat, std::io::Error>
{
  let name = CString::new (name . as_bytes ())
    . map_err (|_| std::io::Error::new (
      std::io::ErrorKind::InvalidInput, "filename contains NUL"))?;
  let mut stat = std::mem::MaybeUninit::<libc::stat>::uninit ();
  let result = unsafe { libc::fstatat (
    directory . as_raw_fd (), name . as_ptr (), stat . as_mut_ptr (),
    libc::AT_SYMLINK_NOFOLLOW) };
  if result != 0 { return Err (std::io::Error::last_os_error ()); }
  Ok (unsafe { stat . assume_init () })
}

#[cfg(unix)]
fn inspect_directory (
  directory : &File,
  relative  : &str,
  tree      : &mut InspectedTree,
) -> Result<(), String> {
  let descriptor_path = if cfg! (target_os = "linux") {
    PathBuf::from (format! ("/proc/self/fd/{}", directory . as_raw_fd ()))
  } else {
    PathBuf::from (format! ("/dev/fd/{}", directory . as_raw_fd ()))
  };
  let mut names : Vec<_> = fs::read_dir (&descriptor_path)
    . map_err (|error| format! (
      "cannot list archive directory '{}': {}", relative, error))?
    . map (|entry| entry . map (|entry| entry . file_name ()))
    . collect::<Result<_, _>> ()
    . map_err (|error| format! (
      "cannot list archive directory '{}': {}", relative, error))?;
  names . sort ();
  for name in names {
    let name_text = name . to_str () . ok_or_else (||
      "archive contains a non-UTF-8 filename" . to_string ())?;
    if name_text == "." || name_text == ".." || name_text . contains ('/') {
      return Err (format! ("unsafe archive entry name '{}'", name_text)); }
    let path = if relative . is_empty () { name_text . to_string () }
      else { format! ("{}/{}", relative, name_text) };
    let stat = lstatat (directory, &name) . map_err (|error| format! (
      "archive entry '{}' changed during traversal: {}", path, error))?;
    let kind = stat . st_mode & libc::S_IFMT;
    if kind == libc::S_IFLNK {
      return Err (format! ("archive contains a symlink: {}", path));
    } else if kind == libc::S_IFDIR {
      let child = openat (directory, &name,
        libc::O_RDONLY | libc::O_DIRECTORY | libc::O_NOFOLLOW | libc::O_CLOEXEC)
        . map_err (|error| format! (
          "cannot safely open archive directory '{}': {}", path, error))?;
      require_private_directory (&child, &format! ("archive directory '{}'", path))?;
      tree . directories . insert (path . clone ());
      inspect_directory (&child, &path, tree)?;
    } else if kind == libc::S_IFREG {
      let child = openat (directory, &name,
        libc::O_RDONLY | libc::O_NOFOLLOW | libc::O_CLOEXEC | libc::O_NONBLOCK)
        . map_err (|error| format! (
          "cannot safely open archive file '{}': {}", path, error))?;
      let capture = match path . as_str () {
        "manifest.initial.sexp" => Some (MAX_MANIFEST_BYTES),
        "ARCHIVE-READY" => Some (MAX_MARKER_BYTES),
        _ if is_buffer_text_artifact (&path) => Some (MAX_BUFFER_TEXT_BYTES),
        _ => None,
      };
      let (inspected, bytes) = hash_file (
        child, capture, &format! ("archive file '{}'", path))?;
      tree . total_bytes = tree . total_bytes . checked_add (inspected . bytes)
        . ok_or_else (|| "archive size overflow" . to_string ())?;
      if path == "manifest.initial.sexp" { tree . manifest_bytes = bytes; }
      else if path == "ARCHIVE-READY" { tree . marker_bytes = bytes; }
      else if is_buffer_text_artifact (&path) {
        tree . captured_artifacts . insert (path . clone (), bytes); }
      if tree . files . insert (path . clone (), inspected) . is_some () {
        return Err (format! ("archive entry '{}' appeared twice", path)); }
    } else {
      return Err (format! ("archive contains a special entry: {}", path));
    }
  }
  Ok (( ))
}

#[cfg(not(unix))]
fn inspect_tree (root : &Path, final_name : &str) -> Result<InspectedTree, String> {
  tracing::warn! (
    "cannot enforce POSIX archive permission and no-follow descriptor checks on this platform");
  let final_path = root . join (final_name);
  let root_metadata = fs::symlink_metadata (root)
    . map_err (|error| error . to_string ())?;
  let final_metadata = fs::symlink_metadata (&final_path)
    . map_err (|error| error . to_string ())?;
  if !root_metadata . is_dir () || !final_metadata . is_dir () {
    return Err ("archive root or incident is not a directory" . into ()); }
  let mut tree = InspectedTree {
    files: BTreeMap::new (), directories: BTreeSet::new (),
    manifest_bytes: Vec::new (), marker_bytes: Vec::new (), total_bytes: 0,
    captured_artifacts: BTreeMap::new (),
  };
  inspect_directory_portable (&final_path, "", &mut tree)?;
  Ok (tree)
}

#[cfg(not(unix))]
fn inspect_directory_portable (
  directory : &Path,
  relative  : &str,
  tree      : &mut InspectedTree,
) -> Result<(), String> {
  let mut entries : Vec<_> = fs::read_dir (directory)
    . map_err (|error| error . to_string ())?
    . collect::<Result<_, _>> () . map_err (|error| error . to_string ())?;
  entries . sort_by_key (|entry| entry . file_name ());
  for entry in entries {
    let name = entry . file_name () . into_string ()
      . map_err (|_| "archive contains a non-UTF-8 filename" . to_string ())?;
    let path = if relative . is_empty () { name . clone () }
      else { format! ("{}/{}", relative, name) };
    let metadata = fs::symlink_metadata (entry . path ())
      . map_err (|error| error . to_string ())?;
    if metadata . file_type () . is_symlink () {
      return Err (format! ("archive contains a symlink: {}", path));
    } else if metadata . is_dir () {
      tree . directories . insert (path . clone ());
      inspect_directory_portable (&entry . path (), &path, tree)?;
    } else if metadata . is_file () {
      let capture = match path . as_str () {
        "manifest.initial.sexp" => Some (MAX_MANIFEST_BYTES),
        "ARCHIVE-READY" => Some (MAX_MARKER_BYTES),
        _ if is_buffer_text_artifact (&path) => Some (MAX_BUFFER_TEXT_BYTES),
        _ => None,
      };
      let file = File::open (entry . path ()) . map_err (|error| error . to_string ())?;
      let (inspected, bytes) = hash_file (file, capture, &path)?;
      tree . total_bytes += inspected . bytes;
      if path == "manifest.initial.sexp" { tree . manifest_bytes = bytes; }
      else if path == "ARCHIVE-READY" { tree . marker_bytes = bytes; }
      else if is_buffer_text_artifact (&path) {
        tree . captured_artifacts . insert (path . clone (), bytes); }
      tree . files . insert (path, inspected);
    } else {
      return Err (format! ("archive contains a special entry: {}", path));
    }
  }
  Ok (( ))
}

fn is_buffer_text_artifact (path : &str) -> bool {
  path . starts_with ("buffer-snapshots/")
  && (path . ends_with ("/last-fetched.org")
      || path . ends_with ("/unsaved-changes.org"))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::maintenance::coordinator::MaintenanceCoordinator;
  use crate::maintenance::MaintenanceOrigin;
  use std::io::Write;

  #[cfg(unix)]
  use std::os::unix::fs::{PermissionsExt, symlink};

  struct Fixture {
    _temporary      : tempfile::TempDir,
    root            : PathBuf,
    active          : ActiveMaintenance,
    manifest_sha256 : String,
  }

  fn sha256 (bytes : &[u8]) -> String {
    format! ("{:x}", Sha256::digest (bytes))
  }

  fn quoted (value : &str) -> String {
    format! ("\"{}\"", value . replace ('\\', "\\\\")
      . replace ('\"', "\\\"") . replace ('\n', "\\n")
      . replace ('\t', "\\t"))
  }

  fn artifact_record (path : &str, bytes : &[u8]) -> String {
    format! ("((path {}) (bytes {}) (sha256 {}))",
      quoted (path), bytes . len (), quoted (&sha256 (bytes)))
  }

  fn create_private_directory (path : &Path) {
    fs::create_dir (path) . unwrap ();
    #[cfg(unix)]
    fs::set_permissions (path, fs::Permissions::from_mode (0o700)) . unwrap ();
  }

  fn write_private (path : &Path, bytes : &[u8]) {
    let mut options = OpenOptions::new ();
    options . write (true) . create_new (true);
    #[cfg(unix)]
    options . mode (0o600);
    let mut file = options . open (path) . unwrap ();
    file . write_all (bytes) . unwrap ();
    file . sync_all () . unwrap ();
  }

  fn fixture () -> Fixture {
    let temporary = tempfile::tempdir () . unwrap ();
    let root = temporary . path () . to_path_buf ();
    #[cfg(unix)]
    fs::set_permissions (&root, fs::Permissions::from_mode (0o700)) . unwrap ();
    let mut coordinator = MaintenanceCoordinator::new ();
    let active = coordinator . begin_for_client (
      MaintenanceOrigin::ExplicitPartialReload,
      None, "client-session" . into ()) . unwrap ();
    let final_path = root . join (&active . archive_directory_name);
    create_private_directory (&final_path);
    create_private_directory (&final_path . join ("buffer-snapshots"));
    create_private_directory (&final_path . join ("interrupted-buffers"));
    let incident = b"* test incident\n";
    let interrupted = b"* Interrupted buffers\n";
    write_private (&final_path . join ("incident.org"), incident);
    write_private (
      &final_path . join ("interrupted-buffers/README.org"), interrupted);
    let manifest = format! (
      concat! (
        "((archive-format-version 1) (manifest-kind \"initial\") ",
        "(incident-id {}) (maintenance-epoch {}) ",
        "(origin {}) (started-at-utc {}) ",
        "(archive-directory-name {}) (client-kind \"emacs\") ",
        "(client-version \"test\") (client-session-id \"client-session\") ",
        "(client-archive-identity {}) (server-archive-identity {}) ",
        "(source-set \"all\") (g0-graph-generation 1) ",
        "(g0-manifest-revision 1) (directory-sync \"test\") ",
        "(artifacts ({} {})) (buffers ()) ",
        "(initial-status \"prepared-for-publication\"))\n"),
      quoted (active . incident_id . as_str ()), active . epoch . get (),
      quoted (active . origin . label ()), quoted (&active . started_at_utc),
      quoted (&active . archive_directory_name),
      quoted (&root . to_string_lossy ()), quoted (&root . to_string_lossy ()),
      artifact_record ("incident.org", incident),
      artifact_record ("interrupted-buffers/README.org", interrupted));
    let manifest_sha256 = sha256 (manifest . as_bytes ());
    write_private (
      &final_path . join ("manifest.initial.sexp"), manifest . as_bytes ());
    let marker = format! (
      "((archive-format-version 1) (incident-id {}) (manifest-sha256 {}))\n",
      quoted (active . incident_id . as_str ()), quoted (&manifest_sha256));
    write_private (&final_path . join ("ARCHIVE-READY"), marker . as_bytes ());
    Fixture {
      _temporary: temporary,
      root,
      active,
      manifest_sha256,
    }
  }

  fn dirty_fixture () -> Fixture {
    let temporary = tempfile::tempdir () . unwrap ();
    let root = temporary . path () . to_path_buf ();
    #[cfg(unix)]
    fs::set_permissions (&root, fs::Permissions::from_mode (0o700)) . unwrap ();
    let last = "* (skg (node (id old) (source main))) café\n" . as_bytes ();
    let current = "* (skg (node (id new) (source main))) café\n" . as_bytes ();
    let mut coordinator = MaintenanceCoordinator::new ();
    let active = coordinator . begin_with_archive_contract_and_targets (
      MaintenanceOrigin::ExplicitPartialReload, None,
      "client-session" . into (), "emacs" . into (), "all" . into (),
      crate::types::store_state::GraphGeneration::INITIAL,
      crate::types::store_state::ManifestRevision::INITIAL,
      vec![crate::maintenance::FrozenBufferRecord {
        buffer_id: "buffer-1" . into (),
        kind: BufferKind::ContentView,
        view_uri: Some ("view-uri" . into ()),
        graph_generation: 1,
        presentation_generation: 2,
        server_revision: 3,
        application_token: 4,
        dirty: true,
        undo_required: false,
        last_fetched_sha256: sha256 (last),
        current_sha256: sha256 (current),
      }], crate::maintenance::MaintenanceTargets {
        paths: Vec::new (), ids: vec!["node" . into ()],
      }) . unwrap ();
    let final_path = root . join (&active . archive_directory_name);
    create_private_directory (&final_path);
    create_private_directory (&final_path . join ("buffer-snapshots"));
    create_private_directory (&final_path . join ("interrupted-buffers"));
    let base = "buffer-snapshots/root_deadbeef";
    create_private_directory (&final_path . join (base));
    let files : Vec<(String, Vec<u8>)> = vec![
      ("incident.org" . into (), b"* incident\n" . to_vec ()),
      ("interrupted-buffers/README.org" . into (), b"* pending\n" . to_vec ()),
      (format! ("{}/README.org", base), b"* buffer\n" . to_vec ()),
      (format! ("{}/metadata.sexp", base), b"((kind content-view))\n" . to_vec ()),
      (format! ("{}/last-fetched.org", base), last . to_vec ()),
      (format! ("{}/unsaved-changes.org", base), current . to_vec ()),
      (format! ("{}/diff.txt", base), b"--- old\n+++ new\n" . to_vec ()),
    ];
    for (path, bytes) in &files {
      write_private (&final_path . join (path), bytes); }
    let root_artifacts = files[..2] . iter ()
      . map (|(path, bytes)| artifact_record (path, bytes))
      . collect::<Vec<_>> () . join (" ");
    let buffer_artifacts = files[2..] . iter ()
      . map (|(path, bytes)| artifact_record (path, bytes))
      . collect::<Vec<_>> () . join (" ");
    let manifest = format! (concat! (
      "((archive-format-version 1) (manifest-kind \"initial\") ",
      "(incident-id {}) (maintenance-epoch {}) (origin {}) ",
      "(started-at-utc {}) (archive-directory-name {}) ",
      "(client-kind \"emacs\") (client-version \"test\") ",
      "(client-session-id \"client-session\") ",
      "(client-archive-identity {}) (server-archive-identity {}) ",
      "(source-set \"all\") (g0-graph-generation 1) ",
      "(g0-manifest-revision 1) (directory-sync \"test\") ",
      "(artifacts ({})) (buffers (((buffer-key \"root_deadbeef\") ",
      "(buffer-id \"buffer-1\") (kind \"content-view\") ",
      "(name \"View\") (view-uri \"view-uri\") (root-ids (\"old\")) ",
      "(recipe \"single-root:old\") (graph-generation 1) ",
      "(presentation-generation 2) (server-revision 3) ",
      "(application-token 4) (undo ((status \"empty\") ",
      "(kind \"undo-fu-session\") (version \"0.8\"))) ",
      "(artifacts ({})) (initial-disposition \"pending-classification\")))) ",
      "(initial-status \"prepared-for-publication\"))\n"),
      quoted (active . incident_id . as_str ()), active . epoch . get (),
      quoted (active . origin . label ()), quoted (&active . started_at_utc),
      quoted (&active . archive_directory_name),
      quoted (&root . to_string_lossy ()), quoted (&root . to_string_lossy ()),
      root_artifacts, buffer_artifacts);
    let manifest_sha256 = sha256 (manifest . as_bytes ());
    write_private (&final_path . join ("manifest.initial.sexp"), manifest . as_bytes ());
    let marker = format! (
      "((archive-format-version 1) (incident-id {}) (manifest-sha256 {}))\n",
      quoted (active . incident_id . as_str ()), quoted (&manifest_sha256));
    write_private (&final_path . join ("ARCHIVE-READY"), marker . as_bytes ());
    Fixture { _temporary: temporary, root, active, manifest_sha256 }
  }

  fn expectation (fixture : &Fixture) -> InitialArchiveExpectation<'_> {
    InitialArchiveExpectation {
      archive_root: &fixture . root,
      active: &fixture . active,
      manifest_sha256: &fixture . manifest_sha256,
    }
  }

  #[test]
  fn accepts_an_exact_private_zero_buffer_archive () {
    let fixture = fixture ();
    let verified = verify_initial_archive (expectation (&fixture)) . unwrap ();
    assert_eq! (verified . artifact_count, 2);
    assert_eq! (verified . manifest_sha256, fixture . manifest_sha256);
    assert! (verified . total_file_bytes > 0);
  }

  #[test]
  fn captures_exact_utf8_dirty_text_bound_to_the_frozen_census () {
    let fixture = dirty_fixture ();
    let verified = verify_initial_archive (expectation (&fixture)) . unwrap ();
    assert_eq! (verified . buffers . len (), 1);
    assert_eq! (verified . buffers[0] . buffer_id, "buffer-1");
    assert_eq! (verified . buffers[0] . root_ids, vec![ID::from ("old")]);
    assert_eq! (verified . buffers[0] . current_text,
      "* (skg (node (id new) (source main))) café\n");
  }

  #[test]
  fn rejects_an_artifact_checksum_mismatch () {
    let fixture = fixture ();
    fs::write (
      fixture . root . join (&fixture . active . archive_directory_name)
        . join ("incident.org"), b"* evil incident\n") . unwrap ();
    let error = verify_initial_archive (expectation (&fixture)) . unwrap_err ();
    assert! (error . contains ("checksum mismatch"), "{}", error);
  }

  #[test]
  fn rejects_an_undeclared_file () {
    let fixture = fixture ();
    write_private (
      &fixture . root . join (&fixture . active . archive_directory_name)
        . join ("rogue"), b"not declared");
    let error = verify_initial_archive (expectation (&fixture)) . unwrap_err ();
    assert! (error . contains ("inventory"), "{}", error);
  }

  #[cfg(unix)]
  #[test]
  fn rejects_an_archive_contained_symlink () {
    let fixture = fixture ();
    symlink ("incident.org",
      fixture . root . join (&fixture . active . archive_directory_name)
        . join ("link")) . unwrap ();
    let error = verify_initial_archive (expectation (&fixture)) . unwrap_err ();
    assert! (error . contains ("symlink"), "{}", error);
  }

  #[cfg(unix)]
  #[test]
  fn rejects_nonprivate_mode_bits () {
    let fixture = fixture ();
    fs::set_permissions (
      fixture . root . join (&fixture . active . archive_directory_name)
        . join ("incident.org"), fs::Permissions::from_mode (0o644)) . unwrap ();
    let error = verify_initial_archive (expectation (&fixture)) . unwrap_err ();
    assert! (error . contains ("0600"), "{}", error);
  }
}
