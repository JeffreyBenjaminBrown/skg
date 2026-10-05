//! GraphnodeOnDisk: the on-disk shape of one telescope SECTION.
//!
//! One node = one ID = one privacy telescope: same-ID .skg files, at
//! most one per skgrepo, each an "telescope section" holding the
//! slice of the node recorded at that privacy level. The most public
//! section (the HOME) alone carries title and body. Ordered
//! relations are ONE flat sequence of 'ListItem's -- members render
//! as bare "- ID" lines in every section and anchors as
//! "- anchor: ID" lines, so a membership moving between sections
//! diffs as a clean one-line delete/add pair. Unordered relations
//! and aliases are plain lists. A field a section omits is a field
//! that section has no opinion about. Today's single-section files
//! parse unchanged (title present, no anchors).
//!
//! Conversions:
//! - 'GraphnodeOnDisk::into_section_slices' feeds the composition
//!   ('server/telescope/compose.rs'), which is how Graphnode values
//!   are born; 'graphnode_on_disk_from_section' is the decomposition-side inverse.
//! - 'GraphnodeOnDisk::into_complete_as_single_section (repo)' treats ONE
//!   section as a whole node. Used only where a single FILE is the
//!   honest unit: historical blobs (the vanished-node search) and
//!   diff MODE's per-file reads (safe because diff mode requires
//!   the skgrepo restriction "all", so no composition is hidden).

use serde::{Serialize, Deserialize};

use crate::telescope::types::{ListItem, SectionSlices};
use crate::types::misc::{ID, MSV, SkgRepoName, rel_partners_at_relRepo, rel_partners_at_relRepo_msv};
use crate::types::nodes::complete::Graphnode;
use crate::types::nodes::complete::Flag;

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct GraphnodeOnDisk {
  // The aliases read the keys used before October 2026, so old files
  // and old git blobs still load; writes use the new keys.
  // tools/migrate-skg-keys-2026-10/ rewrites old files.
  // Field order fixes YAML output order. When anything here changes,
  // update 'Graphnode' (see [[./complete.rs][server/types/nodes/complete.rs]]) and vice versa.

  #[serde(default, skip_serializing_if = "Option::is_none")]
  pub title: Option<String>, // Some in the home section only.

  #[serde(default, skip_serializing_if = "Vec::is_empty")]
  pub aliases: Vec<String>,

  pub pid: ID,

  #[serde(default, skip_serializing_if = "Vec::is_empty")]
  pub extra_ids: Vec<ID>, // home section only, by convention

  #[serde(default, skip_serializing_if = "Option::is_none")]
  pub body: Option<String>, // home section only, like title

  #[serde(default, skip_serializing_if = "Vec::is_empty")]
  pub contains: Vec<ListItem>,

  #[serde(default, skip_serializing_if = "Vec::is_empty", alias = "subscribes_to")]
  pub subscribesTo: Vec<ListItem>,

  #[serde(default, skip_serializing_if = "Vec::is_empty", alias = "hides_from_its_subscriptions")]
  pub hidesFromSubs: Vec<ID>,

  #[serde(default, skip_serializing_if = "Vec::is_empty", alias = "overrides_view_of")]
  pub overrides: Vec<ID>,

  #[serde(default, skip_serializing_if = "Vec::is_empty", alias = "misc")]
  pub flags: Vec<Flag>,
}

impl GraphnodeOnDisk {
  /// This section's contribution to the composition.
  pub fn into_section_slices (
    self,
  ) -> SectionSlices {
    let nonempty_items = |v : Vec<ListItem>| -> Option<Vec<ListItem>> {
      if v . is_empty () { None } else { Some (v) }};
    let nonempty_skgids = |v : Vec<ID>| -> Option<Vec<ID>> {
      if v . is_empty () { None } else { Some (v) }};
    SectionSlices {
      title                        : self . title,
      body                         : self . body,
      aliases                      :
        if self . aliases . is_empty () { None }
        else { Some ( self . aliases ) },
      contains                     :
        nonempty_items ( self . contains ),
      subscribesTo                 :
        nonempty_items ( self . subscribesTo ),
      hidesFromSubs                :
        nonempty_skgids ( self . hidesFromSubs ),
      overrides                    :
        nonempty_skgids ( self . overrides ), }}

  /// Treat ONE section as a whole node: exact for single-section
  /// telescopes; the per-file git/diff paths use it as interim
  /// behavior (see the module comment). A titleless section gets an
  /// empty title rather than erroring, since blob-parsers must be
  /// total; the telescope validators, not this conversion, own
  /// missing-title reporting.
  pub fn into_complete_as_single_section (
    self,
    skgrepo : SkgRepoName,
  ) -> Graphnode {
    let members_only = |items : Vec<ListItem>| -> Vec<ID> {
      items . into_iter ()
        . filter_map ( |i| match i {
          ListItem::Member (skgid) => Some (skgid),
          ListItem::Anchor { .. } => None } )
        . collect () };
    let msv_skgids = |skgids : Vec<ID>| -> MSV<ID> {
      if skgids . is_empty () { MSV::Unspecified }
      else { MSV::Specified (skgids) }};
    Graphnode {
      title                        :
        self . title . unwrap_or_default (),
      overPrivateText_telescope               : false,
      aliases                      : rel_partners_at_relRepo_msv (
        &skgrepo,
        if self . aliases . is_empty () { MSV::Unspecified }
        else { MSV::Specified ( self . aliases ) } ),
      pid                          : self . pid,
      extra_ids                    : self . extra_ids,
      body                         : self . body,
      contains                     : rel_partners_at_relRepo (
        &skgrepo, members_only ( self . contains ) ),
      subscribesTo                 : rel_partners_at_relRepo_msv (
        &skgrepo, msv_skgids ( members_only ( self . subscribesTo ) ) ),
      hidesFromSubs                : rel_partners_at_relRepo_msv (
        &skgrepo,
        msv_skgids ( self . hidesFromSubs ) ),
      overrides                    : rel_partners_at_relRepo_msv (
        &skgrepo, msv_skgids ( self . overrides ) ),
      flags                         : self . flags,
      home_skgrepo: skgrepo,
    }}

  /// Serialize to YAML, emitting the 'body' field as a block literal
  /// (`body: |2-` / `body: |2`) so multi-line bodies are readable in
  /// version control rather than collapsed to a single-line
  /// double-quoted string with `\n` escapes.
  ///
  /// Field order matches the struct declaration, matching what a
  /// plain `serde_yaml::to_string(self)` would produce. Reads
  /// (`serde_yaml::from_str`) already accept every scalar style, so
  /// existing .skg files need no migration.
  ///
  /// Falls back to the default `serde_yaml` rendering for body strings
  /// that cannot be losslessly represented in block form (e.g. ones
  /// containing carriage returns or NUL bytes).
  pub fn to_yaml (
    &self,
  ) -> Result<String, serde_yaml::Error> {
    let value : serde_yaml::Value =
      serde_yaml::to_value (self)?;
    let mapping : &serde_yaml::Mapping =
      value . as_mapping ()
      . expect ("GraphnodeOnDisk serializes as a YAML mapping");
    let mut out : String = String::new();
    for (key, val) in mapping . iter () {
      let is_body : bool =
        key . as_str() == Some ("body");
      let block : Option<String> =
        if is_body {
          val . as_str()
            . and_then (block_scalar_for_body) }
        else { None };
      match block {
        Some (rendered) => out . push_str (&rendered),
        None => {
          // Singleton mapping, so 'serde_yaml::to_string'
          // renders just this one key-value pair.
          let singleton : serde_yaml::Mapping =
            [(key . clone (), val . clone ())]
            . into_iter () . collect ();
          out . push_str (
            & serde_yaml::to_string (&singleton)?); } } }
    Ok (out) }
}

/// The unfold-side inverse of 'into_section_slices': one section's
/// on-disk shape. 'pid' names the telescope; 'is_home' decides
/// whether extra_ids ride along (they live in the home only).
pub fn graphnode_on_disk_from_section (
  pid       : &ID,
  extra_ids : &[ID],
  flags     : &[Flag],
  is_home   : bool,
  slices    : SectionSlices,
) -> GraphnodeOnDisk {
  GraphnodeOnDisk {
    title                        : slices . title,
    aliases                      :
      slices . aliases . unwrap_or_default (),
    pid                          : pid . clone (),
    extra_ids                    :
      if is_home { extra_ids . to_vec () } else { Vec::new () },
    body                         :
      crate::types::nodes::complete::normalize_body (
        slices . body ),
    contains                     :
      slices . contains . unwrap_or_default (),
    subscribesTo                 :
      slices . subscribesTo . unwrap_or_default (),
    hidesFromSubs                :
      slices . hidesFromSubs . unwrap_or_default (),
    overrides                    :
      slices . overrides . unwrap_or_default (),
    flags                        :
      if is_home { flags . to_vec () } else { Vec::new () }, }}

/// Render BODY as a YAML block-literal key-value of the form
/// `body: |2…\n  line\n  line\n`. Returns Some only when the result
/// round-trips exactly through `serde_yaml::from_str`; otherwise None,
/// signalling the caller to fall back to the default rendering.
fn block_scalar_for_body (
  body : &str,
) -> Option<String> {
  let ends_nl : bool = body . ends_with ('\n');
  let inner : &str =
    // Block form's final newline is implied by `|2`; `|2-` strips it.
    if ends_nl { & body [.. body . len() - 1] }
    else       { body };
  let chomp : &str = if ends_nl { "" } else { "-" };
  // Indentation indicator first, then chomping indicator -- matches
  // serde_yaml's own emitter style (e.g. `|2-`).
  let mut out : String =
    format! ("body: |2{}\n", chomp);
  for line in inner . split ('\n') {
    if line . is_empty () { out . push ('\n'); }
    else {
      out . push_str ("  ");
      out . push_str (line);
      out . push ('\n'); } }
  // Round-trip guard: parse what we emitted and accept only if the
  // decoded body is bit-for-bit identical to the input.
  let parsed : Result<serde_yaml::Value, _> =
    serde_yaml::from_str (&out);
  match parsed {
    Ok (v) => {
      let got : Option<&str> =
        v . get ("body") . and_then (|b| b . as_str ());
      if got == Some (body) { Some (out) }
      else { None } }
    Err (_) => None, } }
