//! Temporary guardrail test: NodeFS serializes bit-exact with today's SkgNode.
//!
//! Run once during Plan A step 2 to verify that round-tripping every
//! .skg file in data/public/ through NodeFS (instead of SkgNode)
//! produces identical YAML.
//!
//! Delete this test after Plan A completes.

use skg::types::nodes::fs::NodeFS;
use skg::types::skgnode::SkgNode;
use std::fs;
use std::path::PathBuf;

#[test]
fn nodefs_serializes_bit_exact_with_skgnode () {
  let dir : PathBuf = PathBuf::from ("data/public");
  if ! dir . is_dir () {
    eprintln! (
      "skipping: {} not a directory (probably running in isolated context)",
      dir . display ());
    return; }
  let entries : Vec < PathBuf > =
    fs::read_dir (&dir) . unwrap ()
    . filter_map ( |e| e . ok () . map ( |e| e . path () ))
    . filter ( |p| p . extension ()
              . map ( |x| x == "skg" )
              . unwrap_or (false) )
    . collect ();
  let mut checked      : usize = 0;
  let mut mismatched   : Vec < (PathBuf, String, String) > = Vec::new ();
  let mut parse_failed : Vec < (PathBuf, String) > = Vec::new ();
  for path in &entries {
    let contents : String =
      match fs::read_to_string (path) {
        Ok (s)  => s,
        Err (_) => continue, };
    let as_skgnode : SkgNode =
      match serde_yaml::from_str (&contents) {
        Ok (n)  => n,
        Err (e) => {
          parse_failed . push ((path . clone (), e . to_string ()));
          continue; }};
    let as_nodefs : NodeFS =
      match serde_yaml::from_str (&contents) {
        Ok (n)  => n,
        Err (e) => {
          parse_failed . push ((path . clone (), e . to_string ()));
          continue; }};
    let yaml_skgnode : String =
      serde_yaml::to_string (&as_skgnode) . unwrap ();
    let yaml_nodefs  : String =
      serde_yaml::to_string (&as_nodefs) . unwrap ();
    if yaml_skgnode != yaml_nodefs {
      mismatched . push ((
        path . clone (),
        yaml_skgnode,
        yaml_nodefs )); }
    checked += 1; }
  println! ("checked {} files, {} mismatched, {} parse-failed",
            checked, mismatched . len (), parse_failed . len ());
  for (path, a, b) in mismatched . iter () . take (3) {
    println! ("--- {:?} ---", path);
    println! ("SkgNode:\n{}", a);
    println! ("NodeFS:\n{}",  b); }
  for (path, e) in parse_failed . iter () . take (3) {
    println! ("parse-failed {:?}: {}", path, e); }
  assert! (mismatched   . is_empty ());
  assert! (parse_failed . is_empty ()); }
