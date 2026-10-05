//! The telescope invariant validator: ONE shared primitive
//! ('telescope_violations_of') consulted at both gates -- init /
//! rebuild (whole graph, aggregated report) and save (affected recorders)
//! -- per the override-invariants lesson in TODO/problems.org (two
//! divergent validators nearly let bad data through).
//!
//! ENFORCEMENT STRENGTH (decided, 4_discussion "Migration and the
//! fate of today's validations"): cross-file violations are
//! WARNINGS with repair guidance, never load refusals -- they can
//! arise from two perfectly correct saves on different machines, so
//! a pull must never brick a skgrepo. Only single-file malformations
//! (unparseable YAML, empty-string title, pid/filename mismatch,
//! anchors in unordered relations -- unrepresentable in the format)
//! hard-error, and those live in the parser, not here.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::telescope::types::CompositionWarning;
use crate::types::misc::{ID, MSV, RelPartner, SkgConfig, SkgRepoName};
use crate::types::nodes::rust::GraphnodeInRust;

use std::collections::HashSet;
use std::fmt;
use std::io;
use std::path::Path;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TelescopeViolation {
  /// THE leak shape: a relationship instance whose relRepo is
  /// more public than its target's home, so the (more public) file
  /// names an ID whose node is more private -- exactly what the
  /// telescope exists to prevent. Repair: move the membership's relRepo
  /// to the target's home or beyond ('skg-set-relRepo',
  /// C-c s r). NOTE the git caveat: the leaking file's
  /// history already contains the ID; repair only stops the
  /// bleeding.
  LeakShapedMember {
    relation    : &'static str,
    relRepo     : SkgRepoName,
    member      : ID,
    member_home : SkgRepoName,
  },
  /// A dangling relationship recorded more publicly than its extant recorder.
  /// With no target home to consult, the recorder's home is the conservative
  /// privacy ceiling.
  AbsentTargetLeakShapedMember {
    relation      : &'static str,
    relRepo       : SkgRepoName,
    member        : ID,
    recorder_home : SkgRepoName,
  },
  /// A relationship whose relRepo names no configured skgrepo: its section
  /// could never be written. Arises only from junk or a config
  /// that lost a skgrepo.
  UnconfiguredRelRepo {
    relation : &'static str,
    relRepo: SkgRepoName,
    member   : ID,
  },
  /// Non-owned sections used the same pid as at least one owned
  /// section. The owned telescope won and these skgrepos were
  /// ignored before composition or id-claim collection.
  IgnoredForeignPidFolderlision {
    ignored_skgrepos : Vec<SkgRepoName>,
  },
  /// Anything the COMPOSITION noticed while combining a node's sections
  /// (a dangling anchor, a title below the home, a stray second
  /// title, ...). These were logged and dropped before; they
  /// belong in the report with the rest.
  Composition ( CompositionWarning ),
}

impl fmt::Display for TelescopeViolation {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>,
  ) -> fmt::Result {
    match self {
      TelescopeViolation::LeakShapedMember {
        relation, relRepo, member, member_home } =>
        write! ( f,
          "leak-shaped {} member: relationship at relRepo '{}' names '{}', whose home '{}' is more private. Move the membership with skg-set-relRepo (C-c s r). The leaking file's git history already contains the ID.",
          relation, relRepo, member, member_home ),
      TelescopeViolation::UnconfiguredRelRepo {
        relation, relRepo, member } =>
        write! ( f,
          "{} member '{}' carries relRepo '{}', which is not configured",
          relation, member, relRepo ),
      TelescopeViolation::AbsentTargetLeakShapedMember {
        relation, relRepo, member, recorder_home } =>
        write! ( f,
          "leak-shaped {} member with absent target: relationship at relRepo '{}' names '{}'; because the target is absent, privacy is judged against the extant recorder's home '{}'. Move the membership with skg-set-relRepo (C-c s r).",
          relation, relRepo, member, recorder_home ),
      TelescopeViolation::IgnoredForeignPidFolderlision {
        ignored_skgrepos } =>
        write! ( f,
          "non-owned repo(s) [{}] use the same pid as one or more of your files. Skg kept your owned telescope, ignored those non-owned files, and left them untouched. Their contents are unreachable within Skg; inspect the raw .skg files if you need them.",
          ignored_skgrepos . iter ()
            . map ( |skgrepo| format! ("'{}'", skgrepo) )
            . collect::<Vec<String>> () . join (", ") ),
      TelescopeViolation::Composition (w) =>
        write! ( f, "{}", w ), }}}

/// THE PRIMITIVE both gates call: one node's telescope violations,
/// judged against the whole graph (targets' homes) and the config
/// (the privacy order).
pub fn telescope_violations_of (
  config : &SkgConfig,
  graph  : &InRustGraph,
  pid    : &ID,
) -> Vec<TelescopeViolation> {
  let Some (node) : Option<&GraphnodeInRust> =
    graph . nodes . get (pid) else { return Vec::new (); };
  let mut violations : Vec<TelescopeViolation> = Vec::new ();
  let mut check = |relation : &'static str,
                   members  : &[RelPartner<ID>]| {
    for m in members {
      if config . skgrepo_position ( &m . relRepo ) . is_none () {
        violations . push ( TelescopeViolation::UnconfiguredRelRepo {
          relation,
          relRepo : m . relRepo . clone (),
          member : m . member . clone (), } );
        continue; }
      let target_home : Option<SkgRepoName> =
        graph . pid_of ( &m . member )
        . and_then ( |p| graph . nodes . get (&p) )
        . map ( |n| n . home_skgrepo . clone () );
      let privacy_ceiling : &SkgRepoName = target_home . as_ref ()
        . unwrap_or (&node . home_skgrepo);
      if config . is_strictly_more_public ( &m . relRepo, privacy_ceiling ) {
        match target_home {
          Some (home) =>
          violations . push ( TelescopeViolation::LeakShapedMember {
            relation,
            relRepo   : m . relRepo . clone (),
            member      : m . member . clone (),
            member_home : home, } ),
          None =>
            violations . push (
              TelescopeViolation::AbsentTargetLeakShapedMember {
                relation,
                relRepo  : m . relRepo . clone (),
                member     : m . member . clone (),
                recorder_home : node . home_skgrepo . clone (),
              }), }} }};
  check ("contains", &node . contains);
  let msv = |m : &MSV<RelPartner<ID>>| -> Vec<RelPartner<ID>> {
    m . or_default () . to_vec () };
  check ("subscribesTo",
         & msv ( &node . subscribesTo ));
  check ("hidesFromSubs",
         & msv ( &node . hidesFromSubs ));
  check ("overrides",
         & msv ( &node . overrides ));
  violations }

/// Recorders whose telescope-warning truth may differ between two valid
/// graph snapshots. Saved recorders are always included; untouched inbound recorders are
/// included when a target's existence, canonical PID, or home changed.
pub fn derive_affected_telescope_recorders (
  base            : &InRustGraph,
  candidate       : &InRustGraph,
  saved_pids      : &HashSet<ID>,
  affected_skgids : &HashSet<ID>,
) -> HashSet<ID> {
  let mut recorders : HashSet<ID> = saved_pids . clone ();
  for raw in affected_skgids {
    let old_pid : Option<ID> = base . pid_of (raw);
    let final_pid : Option<ID> = candidate . pid_of (raw);
    let old_home : Option<SkgRepoName> = old_pid . as_ref ()
      .and_then (|pid| base . nodes . get (pid))
      .map (|node| node . home_skgrepo . clone ());
    let final_home : Option<SkgRepoName> = final_pid . as_ref ()
      .and_then (|pid| candidate . nodes . get (pid))
      .map (|node| node . home_skgrepo . clone ());
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
          recorders . extend (inbound . iter () . cloned ()); }}} }
  recorders
}

pub fn affected_telescope_warnings (
  config          : &SkgConfig,
  base            : &InRustGraph,
  candidate       : &InRustGraph,
  saved_pids      : &HashSet<ID>,
  affected_skgids : &HashSet<ID>,
) -> Vec<(ID, TelescopeViolation)> {
  let _span : tracing::span::EnteredSpan =
    tracing::info_span! ("affected_telescope_warnings") . entered ();
  let mut recorders : Vec<ID> = derive_affected_telescope_recorders (
    base, candidate, saved_pids, affected_skgids) . into_iter () . collect ();
  recorders . sort ();
  tracing::info! (
    "incremental telescope work: telescope_recorders_checked={}",
    recorders . len ());
  let mut warnings : Vec<(ID, TelescopeViolation)> = Vec::new ();
  for recorder in recorders {
    for warning in telescope_violations_of (config, candidate, &recorder) {
      warnings . push ((recorder . clone (), warning)); }}
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
/// graph cannot show -- compose complaints and ignored foreign pid
/// collisions, which
/// need a node's section list rather than its composition. Reporting
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
