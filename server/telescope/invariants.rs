//! The telescope invariant validator: ONE shared primitive
//! ('telescope_violations_of') consulted at both gates -- init /
//! rebuild (whole graph, aggregated report) and save (affected owners)
//! -- per the override-invariants lesson in TODO/problems.org (two
//! divergent validators nearly let bad data through).
//!
//! ENFORCEMENT STRENGTH (decided, 4_discussion "Migration and the
//! fate of today's validations"): cross-file violations are
//! WARNINGS with repair guidance, never load refusals -- they can
//! arise from two perfectly correct saves on different machines, so
//! a pull must never brick a source. Only single-file malformations
//! (unparseable YAML, empty-string title, pid/filename mismatch,
//! anchors in unordered relations -- unrepresentable in the format)
//! hard-error, and those live in the parser, not here.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::telescope::types::FoldWarning;
use crate::types::misc::{ID, MSV, MemberAtSource, SkgConfig, SourceName};
use crate::types::nodes::rust::NodeRust;

use std::collections::HashSet;
use std::fmt;
use std::io;
use std::path::Path;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TelescopeViolation {
  /// THE leak shape: a relationship instance recorded at a source
  /// more public than its target's home, so the (more public) file
  /// names an ID whose node is more private -- exactly what the
  /// telescope exists to prevent. Repair: move the membership's source
  /// to the target's home or beyond ('skg-set-relationship-source',
  /// C-c s r). NOTE the git caveat: the leaking file's
  /// history already contains the ID; repair only stops the
  /// bleeding.
  LeakShapedMember {
    relation    : &'static str,
    source      : SourceName,
    member      : ID,
    member_home : SourceName,
  },
  /// A dangling relationship recorded more publicly than its extant owner.
  /// With no target home to consult, the owner's home is the conservative
  /// privacy ceiling.
  AbsentTargetLeakShapedMember {
    relation   : &'static str,
    source     : SourceName,
    member     : ID,
    owner_home : SourceName,
  },
  /// An edge whose source names no configured source: its section
  /// could never be written. Arises only from junk or a config
  /// that lost a source.
  UnconfiguredSource {
    relation : &'static str,
    source   : SourceName,
    member   : ID,
  },
  /// Non-owned sections used the same pid as at least one owned
  /// section. The owned telescope won and these sources were
  /// ignored before folding or id-claim collection.
  IgnoredForeignPidCollision {
    ignored_sources : Vec<SourceName>,
  },
  /// Anything the FOLD noticed while combining a node's sections
  /// (a dangling anchor, a title below the home, a stray second
  /// title, ...). These were logged and dropped before; they
  /// belong in the report with the rest.
  Fold ( FoldWarning ),
}

impl fmt::Display for TelescopeViolation {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>,
  ) -> fmt::Result {
    match self {
      TelescopeViolation::LeakShapedMember {
        relation, source, member, member_home } =>
        write! ( f,
          "leak-shaped {} member: edge at source '{}' names '{}', whose home '{}' is more private. Move the membership with skg-set-relationship-source (C-c s r). The leaking file's git history already contains the ID.",
          relation, source, member, member_home ),
      TelescopeViolation::UnconfiguredSource {
        relation, source, member } =>
        write! ( f,
          "{} member '{}' carries source '{}', which is not configured",
          relation, member, source ),
      TelescopeViolation::AbsentTargetLeakShapedMember {
        relation, source, member, owner_home } =>
        write! ( f,
          "leak-shaped {} member with absent target: edge at source '{}' names '{}'; because the target is absent, privacy is judged against the extant owner's home '{}'. Move the membership with skg-set-relationship-source (C-c s r).",
          relation, source, member, owner_home ),
      TelescopeViolation::IgnoredForeignPidCollision {
        ignored_sources } =>
        write! ( f,
          "non-owned source(s) [{}] use the same pid as one or more of your files. Skg kept your owned telescope, ignored those non-owned files, and left them untouched. Their contents are unreachable within Skg; inspect the raw .skg files if you need them.",
          ignored_sources . iter ()
            . map ( |source| format! ("'{}'", source) )
            . collect::<Vec<String>> () . join (", ") ),
      TelescopeViolation::Fold (w) =>
        write! ( f, "{}", w ), }}}

/// THE PRIMITIVE both gates call: one node's telescope violations,
/// judged against the whole graph (targets' homes) and the config
/// (the privacy order).
pub fn telescope_violations_of (
  config : &SkgConfig,
  graph  : &InRustGraph,
  pid    : &ID,
) -> Vec<TelescopeViolation> {
  let Some (node) : Option<&NodeRust> =
    graph . nodes . get (pid) else { return Vec::new (); };
  let mut violations : Vec<TelescopeViolation> = Vec::new ();
  let mut check = |relation : &'static str,
                   members  : &[MemberAtSource<ID>]| {
    for m in members {
      if config . source_position ( &m . source ) . is_none () {
        violations . push ( TelescopeViolation::UnconfiguredSource {
          relation,
          source : m . source . clone (),
          member : m . member . clone (), } );
        continue; }
      let target_home : Option<SourceName> =
        graph . pid_of ( &m . member )
        . and_then ( |p| graph . nodes . get (&p) )
        . map ( |n| n . source . clone () );
      let privacy_ceiling : &SourceName = target_home . as_ref ()
        . unwrap_or (&node . source);
      if config . is_strictly_more_public ( &m . source, privacy_ceiling ) {
        match target_home {
          Some (home) =>
          violations . push ( TelescopeViolation::LeakShapedMember {
            relation,
            source      : m . source . clone (),
            member      : m . member . clone (),
            member_home : home, } ),
          None =>
            violations . push (
              TelescopeViolation::AbsentTargetLeakShapedMember {
                relation,
                source     : m . source . clone (),
                member     : m . member . clone (),
                owner_home : node . source . clone (),
              }), }} }};
  check ("contains", &node . contains);
  let msv = |m : &MSV<MemberAtSource<ID>>| -> Vec<MemberAtSource<ID>> {
    m . or_default () . to_vec () };
  check ("subscribes_to",
         & msv ( &node . subscribes_to ));
  check ("hides_from_its_subscriptions",
         & msv ( &node . hides_from_its_subscriptions ));
  check ("overrides_view_of",
         & msv ( &node . overrides_view_of ));
  violations }

/// Owners whose telescope-warning truth may differ between two valid
/// snapshots. Saved owners are always included; untouched inbound owners are
/// included when a target's existence, canonical PID, or home changed.
pub fn derive_affected_telescope_owners (
  base         : &InRustGraph,
  candidate    : &InRustGraph,
  saved_pids   : &HashSet<ID>,
  affected_ids : &HashSet<ID>,
) -> HashSet<ID> {
  let mut owners : HashSet<ID> = saved_pids . clone ();
  for raw in affected_ids {
    let old_pid : Option<ID> = base . pid_of (raw);
    let final_pid : Option<ID> = candidate . pid_of (raw);
    let old_home : Option<SourceName> = old_pid . as_ref ()
      .and_then (|pid| base . nodes . get (pid))
      .map (|node| node . source . clone ());
    let final_home : Option<SourceName> = final_pid . as_ref ()
      .and_then (|pid| candidate . nodes . get (pid))
      .map (|node| node . source . clone ());
    if old_pid == final_pid && old_home == final_home { continue; }
    let old_key : &ID = old_pid . as_ref () . unwrap_or (raw);
    let final_key : &ID = final_pid . as_ref () . unwrap_or (raw);
    for (graph, key) in [
      (base, old_key), (base, final_key),
      (candidate, old_key), (candidate, final_key),
    ] {
      for index in [
        &graph . contained_by,
        &graph . subscribers_of,
        &graph . hiders_of,
        &graph . overriders_of,
      ] {
        if let Some (inbound) = index . get (key) {
          owners . extend (inbound . iter () . cloned ()); }}} }
  owners
}

pub fn affected_telescope_warnings (
  config       : &SkgConfig,
  base         : &InRustGraph,
  candidate    : &InRustGraph,
  saved_pids   : &HashSet<ID>,
  affected_ids : &HashSet<ID>,
) -> Vec<(ID, TelescopeViolation)> {
  let _span : tracing::span::EnteredSpan =
    tracing::info_span! ("affected_telescope_warnings") . entered ();
  let mut owners : Vec<ID> = derive_affected_telescope_owners (
    base, candidate, saved_pids, affected_ids) . into_iter () . collect ();
  owners . sort ();
  tracing::info! (
    "incremental telescope work: telescope_owners_checked={}",
    owners . len ());
  let mut warnings : Vec<(ID, TelescopeViolation)> = Vec::new ();
  for owner in owners {
    for warning in telescope_violations_of (config, candidate, &owner) {
      warnings . push ((owner . clone (), warning)); }}
  warnings . sort_by (|(pid_a, warning_a), (pid_b, warning_b)|
    pid_a . cmp (pid_b) . then_with (||
      warning_a . to_string () . cmp (&warning_b . to_string ())));
  warnings
}

/// The init/rebuild gate: every node, aggregated. Returns the
/// violations paired with their nodes; the caller decides
/// presentation (report file + logs at init/rebuild).
pub fn validate_all_telescopes (
  config : &SkgConfig,
  graph  : &InRustGraph,
) -> Vec<(ID, TelescopeViolation)> {
  let mut all : Vec<(ID, TelescopeViolation)> = Vec::new ();
  for pid in graph . nodes . keys () {
    for v in telescope_violations_of (config, graph, pid) {
      all . push (( pid . clone (), v )); }}
  all . sort_by ( |a, b| a . 0 . cmp ( &b . 0 ));
  all }

/// The whole init/rebuild report: what the graph shows
/// ('validate_all_telescopes') plus what the LOAD saw that the
/// graph cannot show -- fold complaints and ignored foreign pid
/// collisions, which
/// need a node's section list rather than its fold. Reporting
/// failures are logged, not propagated: a report we could not write
/// is no reason to refuse to start.
pub fn report_all_telescope_violations (
  config           : &SkgConfig,
  graph            : &InRustGraph,
  load_violations  : Vec<(ID, TelescopeViolation)>,
) {
  let all : Vec<(ID, TelescopeViolation)> = {
    let mut all : Vec<(ID, TelescopeViolation)> =
      validate_all_telescopes (config, graph);
    all . extend (load_violations);
    all . sort_by ( |a, b| a . 0 . cmp ( &b . 0 ));
    all };
  if let Err (e) = report_telescope_violations (
    &all, &config . data_root ) {
    tracing::warn! ( error = %e,
                     "could not write the telescope report" ); }}

/// Write the aggregated init report (one line per violation,
/// grouped by node; a count up top) to
/// DATA_ROOT/telescope-warnings.org, and log a summary. Removes a
/// stale report when there is nothing to say, so the file's
/// presence is meaningful.
pub fn report_telescope_violations (
  violations : &[(ID, TelescopeViolation)],
  data_root  : &Path,
) -> io::Result<()> {
  let report_path : std::path::PathBuf =
    data_root . join ("telescope-warnings.org");
  if violations . is_empty () {
    match std::fs::remove_file (&report_path) {
      Ok (( ))                                          => {},
      Err (e) if e . kind () == io::ErrorKind::NotFound => {},
      Err (e)                                           =>
        return Err (e), }
    return Ok (( )); }
  let mut content : String = String::new ();
  content . push_str ("#+title: Telescope warnings\n");
  content . push_str ("#+date: <generated at initialization>\n\n");
  content . push_str ( & format! (
    "{} telescope warning(s). These are WARNINGS, not errors: the data loads, but the shapes below should be repaired -- each line says how.\n\n",
    violations . len () ));
  let mut current : Option<&ID> = None;
  for (pid, v) in violations {
    if current != Some (pid) {
      content . push_str ( & format! ("* {}\n", pid ));
      current = Some (pid); }
    content . push_str ( & format! ("** {}\n", v )); }
  std::fs::write (&report_path, content) ?;
  tracing::warn! (
    count = violations . len (),
    report = %report_path . display (),
    "telescope warnings found; see report" );
  Ok (( )) }

#[cfg(test)]
#[path = "../../tests/unit/telescope_invariants.rs"]
mod tests;
