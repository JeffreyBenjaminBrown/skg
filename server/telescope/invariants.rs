//! The telescope invariant validator: ONE shared primitive
//! ('telescope_violations_of') consulted at both gates -- init /
//! rebuild (whole graph, aggregated report) and save (touched pids)
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
    winning_sources : Vec<SourceName>,
    winning_paths   : Vec<std::path::PathBuf>,
    ignored_sources : Vec<SourceName>,
    ignored_paths   : Vec<std::path::PathBuf>,
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
      TelescopeViolation::IgnoredForeignPidCollision {
        ignored_sources, .. } =>
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
      if let Some (home) = target_home {
        // A dangling member (no node) is a different, pre-existing
        // problem (TODO/problems.org, the dangling-reference audit
        // gap); not this validator's to report.
        if config . is_strictly_more_public ( &m . source, &home ) {
          violations . push ( TelescopeViolation::LeakShapedMember {
            relation,
            source      : m . source . clone (),
            member      : m . member . clone (),
            member_home : home, } ); }} }};
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

/// The init/rebuild gate: every node, aggregated. Returns the
/// violations paired with their nodes; the caller decides
/// presentation (report file + logs at init; save warnings at
/// save).
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
