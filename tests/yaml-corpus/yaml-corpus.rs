//! The '.skg' corpus check for replacing 'serde_yaml'.
//!
//! For every '.skg' file under 'tests/' and under each path given on
//! the command line (read only), parse with 'serde_yaml' and
//! re-serialize the way 'GraphnodeOnDisk::to_yaml' does with
//! 'serde_yaml'; that is the reference. Then do the same with each
//! candidate library, and with the server's own 'to_yaml', and report
//! every candidate output that differs from the reference byte for
//! byte, and every file a candidate parses differently.
//!
//! Usage: 'cargo run -p yaml-corpus -- [PATH ...]'

use serde::Deserialize;
use skg::types::nodes::fs::GraphnodeOnDisk;
use std::collections::BTreeMap;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

/// Parses a file's text, then renders it as 'to_yaml' would.
struct Candidate {
  name   : &'static str,
  parse  : fn (&str) -> Result<GraphnodeOnDisk, String>,
  render : fn (&GraphnodeOnDisk) -> Result<String, String>,
}

#[derive(Default)]
struct CandidateTally {
  identical        : usize,
  differing        : Vec<PathBuf>,
  parse_failures   : Vec<(PathBuf, String)>,
  parsed_otherwise : Vec<PathBuf>,
  render_failures  : Vec<(PathBuf, String)>,
}

/// Show at most this many diffs per candidate on stdout.
const DIFFS_SHOWN_PER_CANDIDATE : usize = 20;

fn main () {
  let corpus_roots : Vec<PathBuf> = {
    let tests_folder : PathBuf =
      Path::new (env!("CARGO_MANIFEST_DIR")) . join ("..");
    std::iter::once (tests_folder)
      . chain ( env::args () . skip (1) . map (PathBuf::from) )
      . collect () };
  let files : Vec<PathBuf> = skg_files_under (&corpus_roots);
  let candidates : Vec<Candidate> = candidates ();
  let mut tallies : BTreeMap<&'static str, CandidateTally> =
    candidates . iter ()
    . map ( |c| (c . name, CandidateTally::default ()) )
    . collect ();
  let mut reference_parse_failures : usize = 0;
  for file in &files {
    let text : String = match fs::read_to_string (file) {
      Ok (text) => text,
      Err (_) => { reference_parse_failures += 1; continue; } };
    let reference_node : GraphnodeOnDisk =
      match serde_yaml::from_str (&text) {
        Ok (node) => node,
        Err (_) => { reference_parse_failures += 1; continue; } };
    let reference_yaml : String =
      to_yaml_with_serde_yaml (&reference_node)
      . expect ("serde_yaml renders what it parsed");
    for candidate in &candidates {
      let tally : &mut CandidateTally =
        tallies . get_mut (candidate . name) . unwrap ();
      compare_candidate (
        candidate, file, &text, &reference_node, &reference_yaml,
        tally ); } }
  println! ("files: {}; serde_yaml could not read {} of them",
            files . len (), reference_parse_failures);
  for candidate in &candidates {
    report_candidate (candidate . name,
                      &tallies [candidate . name]); } }

fn compare_candidate (
  candidate      : &Candidate,
  file           : &Path,
  text           : &str,
  reference_node : &GraphnodeOnDisk,
  reference_yaml : &str,
  tally          : &mut CandidateTally,
) {
  let node : GraphnodeOnDisk = match (candidate . parse) (text) {
    Ok (node) => node,
    Err (e) => {
      tally . parse_failures . push ((file . to_path_buf (), e));
      return; } };
  if &node != reference_node {
    tally . parsed_otherwise . push (file . to_path_buf ()); }
  match (candidate . render) (reference_node) {
    Err (e) =>
      tally . render_failures . push ((file . to_path_buf (), e)),
    Ok (yaml) if yaml == reference_yaml =>
      tally . identical += 1,
    Ok (yaml) => {
      if tally . differing . len () < DIFFS_SHOWN_PER_CANDIDATE {
        println! ("--- {} differs on {}\n{}",
                  candidate . name, file . display (),
                  line_diff (reference_yaml, &yaml)); }
      tally . differing . push (file . to_path_buf ()); } } }

fn report_candidate (
  name  : &str,
  tally : &CandidateTally,
) {
  println! (
    "== {}: identical {}, differing {}, parse failures {}, parsed otherwise {}, render failures {}",
    name, tally . identical, tally . differing . len (),
    tally . parse_failures . len (), tally . parsed_otherwise . len (),
    tally . render_failures . len ());
  for (file, e) in tally . parse_failures . iter () . take (5) {
    println! ("   parse failure {}: {}", file . display (), e); }
  for file in tally . parsed_otherwise . iter () . take (5) {
    println! ("   parsed otherwise {}", file . display ()); }
  for (file, e) in tally . render_failures . iter () . take (5) {
    println! ("   render failure {}: {}", file . display (), e); } }

/// The lines of each text that the other lacks, in order.
fn line_diff (
  reference : &str,
  candidate : &str,
) -> String {
  let reference_lines : Vec<&str> = reference . lines () . collect ();
  let candidate_lines : Vec<&str> = candidate . lines () . collect ();
  let mut out : String = String::new ();
  for line in &reference_lines {
    if ! candidate_lines . contains (line) {
      out . push_str (&format! ("  serde_yaml: {:?}\n", line)); } }
  for line in &candidate_lines {
    if ! reference_lines . contains (line) {
      out . push_str (&format! ("  candidate:  {:?}\n", line)); } }
  if out . is_empty () {
    out = format! ("  same lines, other bytes:\n  serde_yaml: {:?}\n  candidate:  {:?}\n",
                   reference, candidate); }
  out }

fn skg_files_under (
  roots : &[PathBuf],
) -> Vec<PathBuf> {
  let mut files : Vec<PathBuf> =
    roots . iter ()
    . flat_map ( |root| WalkDir::new (root) . into_iter ()
                 . filter_map ( |entry| entry . ok () ) )
    . filter ( |entry| entry . file_type () . is_file ()
               && entry . path () . extension ()
                  . is_some_and ( |e| e == "skg" ) )
    . map ( |entry| entry . into_path () )
    . filter ( |path| ! path . components ()
               . any ( |c| c . as_os_str () == "target" ) )
    . collect ();
  files . sort ();
  files . dedup ();
  files }

fn candidates () -> Vec<Candidate> {
  vec! [
    Candidate { name   : "server's to_yaml",
                parse  : parse_with_serde_yaml,
                render : |node| node . to_yaml ()
                                . map_err ( |e| e . to_string () ) },
    Candidate { name   : "yaml_serde",
                parse  : parse_with_yaml_serde,
                render : to_yaml_with_yaml_serde },
    Candidate { name   : "serde_yaml_ng",
                parse  : parse_with_serde_yaml_ng,
                render : to_yaml_with_serde_yaml_ng },
    Candidate { name   : "serde_yaml_bw",
                parse  : parse_with_serde_yaml_bw,
                render : to_yaml_with_serde_yaml_bw },
    Candidate { name   : "serde-saphyr",
                parse  : parse_with_serde_saphyr,
                render : to_yaml_with_serde_saphyr }, ] }

/// A parser and a 'to_yaml' replica for a library with serde_yaml's
/// 'Value' and 'Mapping' API.
macro_rules! value_api_library {
  ($parse:ident, $render:ident, $library:ident) => {
    fn $parse (
      text : &str,
    ) -> Result<GraphnodeOnDisk, String> {
      $library::from_str (text) . map_err ( |e| e . to_string () ) }

    fn $render (
      node : &GraphnodeOnDisk,
    ) -> Result<String, String> {
      let value : $library::Value =
        $library::to_value (node) . map_err ( |e| e . to_string () ) ?;
      let mapping : &$library::Mapping =
        value . as_mapping () . ok_or ("not a mapping") ?;
      let mut out : String = String::new ();
      for (key, val) in mapping . iter () {
        let block : Option<String> =
          if key . as_str () == Some ("body") {
            val . as_str () . and_then ( |body| block_scalar_for_body (
              body,
              |yaml| $library::from_str::<$library::Value> (yaml) . ok ()
                     . and_then ( |v| v . get ("body")
                                  . and_then ( |b| b . as_str () )
                                  . map (str::to_string) ) )) }
          else { None };
        match block {
          Some (rendered) => out . push_str (&rendered),
          None => {
            let singleton : $library::Mapping =
              [(key . clone (), val . clone ())] . into_iter () . collect ();
            out . push_str ( & $library::to_string (&singleton)
                             . map_err ( |e| e . to_string () ) ? ); } } }
      Ok (out) } } }

value_api_library! (parse_with_serde_yaml, to_yaml_with_serde_yaml,
                    serde_yaml);
value_api_library! (parse_with_yaml_serde, to_yaml_with_yaml_serde,
                    yaml_serde);
value_api_library! (parse_with_serde_yaml_ng, to_yaml_with_serde_yaml_ng,
                    serde_yaml_ng);
value_api_library! (parse_with_serde_yaml_bw, to_yaml_with_serde_yaml_bw,
                    serde_yaml_bw);

fn parse_with_serde_saphyr (
  text : &str,
) -> Result<GraphnodeOnDisk, String> {
  serde_saphyr::from_str (text) . map_err ( |e| e . to_string () ) }

/// serde-saphyr has no 'Value', so serde_yaml's splits the node into
/// its fields, and serde-saphyr renders each one.
fn to_yaml_with_serde_saphyr (
  node : &GraphnodeOnDisk,
) -> Result<String, String> {
  #[derive(Deserialize)]
  struct BodyOnly { body : String }
  let value : serde_yaml::Value =
    serde_yaml::to_value (node) . map_err ( |e| e . to_string () ) ?;
  let mapping : &serde_yaml::Mapping =
    value . as_mapping () . ok_or ("not a mapping") ?;
  let mut out : String = String::new ();
  for (key, val) in mapping . iter () {
    let block : Option<String> =
      if key . as_str () == Some ("body") {
        val . as_str () . and_then ( |body| block_scalar_for_body (
          body,
          |yaml| serde_saphyr::from_str::<BodyOnly> (yaml) . ok ()
                 . map ( |b| b . body ) )) }
      else { None };
    match block {
      Some (rendered) => out . push_str (&rendered),
      None => {
        let singleton : serde_yaml::Mapping =
          [(key . clone (), val . clone ())] . into_iter () . collect ();
        out . push_str ( & serde_saphyr::to_string (&singleton)
                         . map_err ( |e| e . to_string () ) ? ); } } }
  Ok (out) }

/// A copy of the private 'block_scalar_for_body' in
/// 'server/types/nodes/fs.rs', with the round-trip parse supplied.
fn block_scalar_for_body (
  body       : &str,
  parse_body : impl Fn (&str) -> Option<String>,
) -> Option<String> {
  let ends_nl : bool = body . ends_with ('\n');
  let inner : &str =
    if ends_nl { & body [.. body . len () - 1] }
    else       { body };
  let chomp : &str = if ends_nl { "" } else { "-" };
  let mut out : String =
    format! ("body: |2{}\n", chomp);
  for line in inner . split ('\n') {
    if line . is_empty () { out . push ('\n'); }
    else {
      out . push_str ("  ");
      out . push_str (line);
      out . push ('\n'); } }
  if parse_body (&out) . as_deref () == Some (body) { Some (out) }
  else { None } }
